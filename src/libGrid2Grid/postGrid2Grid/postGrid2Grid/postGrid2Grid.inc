subroutine initializePostG2G(this, inputFileName)
    use mfpGlobal, only : CHAR_LEN
    implicit none
    class(typPostGrid2Grid), intent(inout) :: this
    character(len=*), intent(in)           :: inputFileName
    real(rp)    :: zMinus
    Character(len = CHAR_LEN) :: hosTypeName
    !------------------------------------------------------------------

    if (this%isInitialized_) return

    !! Read Post Program Input File
    ! Call readPostG2GInputFile(this, inputFileName)

    !- Initialize HOS Vol2Vol Class
    ! Call this%hosVol2Vol_%initialize(this%hosSolver_, &
    !                                  this%hosFileName_, &
    !                                  this%rectLGrid_%zMin_, &
    !                                  this%rectLGrid_%zMax_, &
    !                                  this%rectLGrid_%nZmin_, &
    !                                  this%rectLGrid_%nZmax_)

    ! ReadPost Program Input File (Dictionary Format)
    Call readPostG2GInputFileDict(this, inputFileName)

    ! initialize HOS Vol2Vol with Dictionary
    hosTypeName = this%dict_%getChar("Grid2Grid")
    Call this%hosVol2Vol_%initialize(this%dict_%subDict(hosTypeName))

    !! Check Input Parameters
    Call checkPostG2GParameter(this)

    !! Build 3D Mesh
    Call this%rectLGrid_%initialize(this%isBuildAirMesh_, this%zflag_)

    !! Delete VTK Folder and re-make
    CALL System("rm -r "//trim(postG2GVTK)//"*.vtk")
    CALL System("mkdir -p "//trim(postG2GVTK))

    !! Make Wave Probe Result File
    CALL System("rm "//trim(this%waveFile_%name))
    this%waveFile_%unit = callFileUnit()

    open(unit = this%waveFile_%unit,&
         file = this%waveFile_%name,&
         status = 'replace')

    this%isInitialized_ = .TRUE.

end subroutine

subroutine destroyPostG2G(this)
    implicit none
    class(typPostGrid2Grid), intent(inout) :: this

    Call this%hosVol2Vol_%destroy

    Call this%rectLGrid_%destroy

    if (allocated(this%waveProbe_)) deallocate(this%waveProbe_)

end subroutine

subroutine finalPostG2G(this)
    implicit none
    type(typPostGrid2Grid), intent(inout) :: this
    Call this%destroy
end subroutine

SUBROUTINE doPostProcessing(this)
    implicit none
    class(typPostGrid2Grid), intent(inout) :: this
    real(RP) :: globalStart, localStart, finish, gCompTime, lCompTime, leftTime
    real(RP) :: simulTime

    simulTime = this%startTime_

    call cpu_time(globalStart)
    do
        if (simulTime.gt.this%endTime_ + this%dt_ / 2.0) exit

        call cpu_time(localStart)
        Call this%correct(simulTime)
        call cpu_time(finish)

        gCompTime = finish - globalStart
        lCompTime = finish - localStart

        leftTime = gCompTime / (simulTime + this%dt_) * (this%endTime_ + this%dt_) - gCompTime

        write(*,1001) " Write t = ", simulTime, &
                      " ,  cpu time = ", lCompTime, &
                      " ,  total time = ", gCompTime, &
                      " ,  left time = ", leftTime

        !! Update Time
        simulTime = simulTime + this%dt_

    end do

    1001 format (a, f12.3, a, f12.3, a, f12.3, a, f12.3)

END SUBROUTINE

subroutine correctPostG2G(this, simulTime)
    implicit none
    class(typPostGrid2Grid), intent(inout) :: this
    real(RP),intent(in) :: simulTime
    real(RP) :: x, y, z, eta, u, v, w, pd
    real(rp),allocatable :: tempZArr(:)
    integer  :: ix, iy, iz, iProbe
    integer  :: iTime
    ! integer  :: iflag

    Call this%hosVol2Vol_%correct(simulTime)

    ! iflag = 0                                       ! dimensional Coordinates
    ! if (this%isBuildAirMesh_.EQV..FALSE.) iflag = 1  ! non-dimensional Coordinates

    if (this%isWriteVTK_) then
        !! Get Flow information on Grid
        do ix = 1, this%rectLGrid_%nX_
        x = this%rectLGrid_%X_(ix)
        do iy = 1, this%rectLGrid_%nY_
        y = this%rectLGrid_%Y_(iy)

            if (this%isBuildAirMesh_.EQV..FALSE.) then

                eta = this%hosVol2Vol_%getEta(x, y, simulTime)
                Call buildZmesh(this%rectLGrid_%zMin_, eta, &
                                this%rectLGrid_%nZmin_, 0, &
                                tempZArr, &
                                this%zflag_, &
                                this%rectLGrid_%zMinRatio_,&
                                this%rectLGrid_%zMaxRatio_)

                this%rectLGrid_%movingZ_(ix, iy, : ) = tempZArr

            end if

            do iz = 1, this%rectLGrid_%nZ_

                if (this%isBuildAirMesh_) then
                    z = this%rectLGrid_%Z_(iz)
                else
                    z = this%rectLGrid_%movingZ_(ix, iy, iz)
                end if

                !! Get interpolated value
                Call this%hosVol2Vol_%getFlow(x, y, z, simulTime, eta, u, v, w, pd)

                this%rectLGrid_%eta_(ix, iy, iz) = eta
                this%rectLGrid_%u_(ix, iy, iz)   = u
                this%rectLGrid_%v_(ix, iy, iz)   = v
                this%rectLGrid_%w_(ix, iy, iz)   = w
                this%rectLGrid_%pd_(ix, iy, iz)  = pd
            enddo

        enddo
        enddo

        iTime = int(simulTime / this%dt_)

        Call writeVTK(this, iTime)

    end if

    if (this%isWriteWaveProbe_) then
        !! Get Eta Information (Wave probe)
        do iProbe = 1, this%nWaveProbe_
            x = this%waveProbe_(iProbe)%xPos_
            y = this%waveProbe_(iProbe)%yPos_
            this%waveProbe_(iProbe)%eta_ = this%hosVol2Vol_%getEta(x ,y , simulTime)
        enddo

        Call writeWaveProbe(this,simulTime)

    end if
end subroutine

SUBROUTINE writeVTK(this, iTime)
    implicit none
    class(typPostGrid2Grid), intent(inout) :: this
    integer, intent(in) :: iTime

    if (this%isBuildAirMesh_) then
        Call this%writeVTKtotalASCII(iTime)
    else
        Call this%writeVTKnoAirASCII(iTime)
    endif

END SUBROUTINE

SUBROUTINE writeWaveProbe(this, simulTime)
    implicit none
    class(typPostGrid2Grid), intent(inout) :: this
    real(rp), intent(in)  :: simulTime

    integer               :: i
    Character(len=100000) :: tLine

    if (this%isWaveProbeOutHeader_) then
        !! Write File Header
        write(this%waveFile_%unit,1001) "#"
        write(this%waveFile_%unit,1001) "#    HOS Grid2Grid Post Program Result File"
        write(this%waveFile_%unit,1001) "#"
        write(this%waveFile_%unit,1001) "#        Wave elevation time series"
        write(this%waveFile_%unit,1001) "#"
        write(this%waveFile_%unit,1002) "#        Post processing input file name : ", &
                                        adjustl(trim(this%postInputFile_%name))
        write(this%waveFile_%unit,1001) "#"
        write(this%waveFile_%unit,1001) "#---------------------------------------------------------"
        write(this%waveFile_%unit,1001) "#  Wave Gauges   idx       name        xPos           yPos"
        write(this%waveFile_%unit,1001) "#---------------------------------------------------------"
        do i = 1, this%nWaveProbe_
        write(this%waveFile_%unit,1201) "#", i, "     "//adjustl(trim(this%waveProbe_(i)%name_)), &
                                            this%waveProbe_(i)%xPos_, this%waveProbe_(i)%yPos_
        end do
        write(this%waveFile_%unit,1001) "#---------------------------------------------------------"
        write(this%waveFile_%unit,1001) "#"

        !! Write Wave Probe variables
        tLine = "var=       time "
        do i = 1, this%nWaveProbe_
            tLine = trim(tLine)//" "//adjustl(trim(this%waveProbe_(i)%name_))
        enddo

        write(this%waveFile_%unit,1001) trim(tLine)

        tLine = "#           [s] "
        do i = 1, this%nWaveProbe_
            tLine = trim(tLine)//" "//adjustl(trim("[m]"))
        enddo
        write(this%waveFile_%unit,1001) trim(tLine)

        this%isWaveProbeOutHeader_ = .FALSE.
    end if

    !! Write Wave Elevation Data

    write(this%waveFile_%unit,1101) simulTime, (this%waveProbe_(i)%eta_, i=1, this%nWaveProbe_)

    Call flush(this%waveFile_%unit)

    !! Close File

    1001 format(a)
    1002 format(2(a))
    1201 format(a,i19,a,2(f15.6))

    1101 format(5000(e15.7))

END SUBROUTINE

subroutine readPostG2GInputFileDict(this, inputFilePath)
    use mfpGlobal, only : CHAR_LEN
    implicit none
    class(typPostGrid2Grid), intent(inout)  :: this
    character(len=*), intent(in)            :: inputFilePath
    !!!... Local Variables
    Character(len=CHAR_LEN)                 :: fileDir
    Character(len=CHAR_LEN)                 :: fileName
    Character(len=CHAR_LEN)                 :: fileExt

    Character(len=CHAR_LEN)  :: hosTypeName
    Type(typDictionaryPtr)   :: dict, HOSdict, &
                                simulDict, vtkDict
    Character(len=CHAR_LEN)  :: zMeshType
    Type(typDictionaryPtr)   :: waveProbeDict, probesDict, probeDict
    integer :: iprobe

    !!!... Subroutine Start

    this%postInputFile_%name = inputFilePath

    Call separateFilePath(inputFilePath, fileDir, fileName, fileExt)

    Call this%dict_%initialize(fileDir, fileName, fileExt)

    dict = this%dict_
    hosTypeName = dict%getChar("Grid2Grid")

    !!... HOS Dictionary -------------------------------------------------------

    HOSdict     = dict%subDict(hosTypeName)
    this%hosSolver_ = HOSdict%getChar("type")

    if ( (.not.(isStringEqual(this%hosSolver_, "nwt"))).and.&
         (.not.(isStringEqual(this%hosSolver_, "ocean"))) ) then
        write(*,*) "    [ERROR] Grid2Grid, readPostG2GInputFile(fileName) "
        write(*,*) "        Wrong solver is given when HOS post initilize"
        write(*,*) "        given solver : ", this%hosSolver_
        stop
    end if

    !!... HOS Re-Construction Information
    this%hosFileName_ = HOSdict%getChar("filePath")

    !!... Simulation  Dictionary -----------------------------------------------

    simulDict   = dict%subDict("simulation")

    this%startTime_ = simulDict%getReal("startTime")
    if (this%startTime_.lt.0.0) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        startTime should be positive"
        write(*,*) "        given startTime :", this%startTime_
        stop
    end if

    this%endTime_   = simulDict%getReal("endTime")
    if (this%endTime_.lt.0.0) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        endTime should be positive"
        write(*,*) "        given endTime :", this%endTime_
        stop
    end if

    this%dt_        = simulDict%getReal("dt")
    if (this%dt_.le.0.0) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        dt should be positive"
        write(*,*) "        given dt :", this%dt_
        stop
    end if

    !!... VTK Mesh Dictionary --------------------------------------------------

    this%isWriteVTK_  = dict%getLogical("writeVTK")

    if (this%isWriteVTK_) then
        vtkDict = dict%subDict("vtkMesh")
    end if

    this%isBuildAirMesh_ = vtkDict%getLogicalOrDefault("airMesh", .false.)

    this%rectLGrid_%xMin_ = vtkDict%getReal("xMin")
    if (this%rectLGrid_%xMin_.lt.0.0) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        Wrong xMin is given, xMin should be positive or zero"
        write(*,*) "        given origin xMin :", this%rectLGrid_%xMin_
        stop
    end if

    this%rectLGrid_%xMax_ = vtkDict%getReal("xMax")
    if (this%rectLGrid_%xMax_.lt.0.0) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        Wrong xMax is given, xMax should be positive or zero"
        write(*,*) "        given origin xMax :", this%rectLGrid_%xMax_
        stop
    end if

    this%rectLGrid_%yMin_ = vtkDict%getReal("yMin")
    if (this%rectLGrid_%yMin_.lt.0.0) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        Wrong yMin is given, yMin should be positive or zero"
        write(*,*) "        given origin xMin :", this%rectLGrid_%yMin_
        stop
    end if

    this%rectLGrid_%yMax_ = vtkDict%getReal("yMax")
    if (this%rectLGrid_%yMax_.lt.0.0) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        Wrong yMax is given, yMax should be positive or zero"
        write(*,*) "        given origin yMax :", this%rectLGrid_%yMax_
        stop
    end if

    this%rectLGrid_%zMin_ = vtkDict%getReal("zMin")
    if (this%rectLGrid_%zMin_.ge.0.0) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        Wrong zMin is given, zMin should be negative"
        write(*,*) "        given origin zMin :", this%rectLGrid_%zMin_
        stop
    end if

    this%rectLGrid_%zMax_ = vtkDict%getReal("zMax")
    if (this%rectLGrid_%zMax_.lt.0.0) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        Wrong zMax is given, zMax should be positive"
        write(*,*) "        given origin zMax :", this%rectLGrid_%zMax_
        stop
    end if

    this%rectLGrid_%nX_    = vtkDict%getInt("nX")
    if (this%rectLGrid_%nX_.le.1) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        Wrong nX is given, nX should be larger than 1"
        write(*,*) "        given origin nX :", this%rectLGrid_%nX_
        stop
    end if

    this%rectLGrid_%nY_    = vtkDict%getInt("nY")
    if (this%rectLGrid_%nY_.le.1) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        Wrong nY is given, nY should be larger than 1"
        write(*,*) "        given origin nY :", this%rectLGrid_%nY_
        stop
    end if

    this%rectLGrid_%nZmin_ = vtkDict%getInt("nZMin")
    if (this%rectLGrid_%nZmin_.le.3) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        Wrong nZmin is given, nZ should be larger than 3"
        write(*,*) "        given origin nZmin :", this%rectLGrid_%nZmin_
        stop
    end if

    this%rectLGrid_%nZmax_ = vtkDict%getInt("nZMax")
    if (this%rectLGrid_%nZmax_.le.3) then
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        Wrong nZmax is given, nZ should be larger than 3"
        write(*,*) "        given origin nZmax :", this%rectLGrid_%nZmax_
        stop
    end if

    zMeshType = vtkDict%getChar("zMeshType")

    if (isStringEqual(zMeshType, "uniform")) then
        this%zflag_ = INDEX_UNIFORM_MESH    ! Uniform Mesh
    else if (isStringEqual(zMeshType, "sine")) then
        this%zflag_ = INDEX_SINE_MESH       ! Sine Distributed Mesh
    else if (isStringEqual(zMeshType, "meshratio")) then
        this%zflag_ = INDEX_GEOMETRICRATIO_MESH   ! GEOMETRIC RATIO Mesh

        this%rectLGrid_%zMinRatio_ = vtkDict%getRealOrDefault("zMinRatio", 1.0_RP)
        this%rectLGrid_%zMaxRatio_ = vtkDict%getRealOrDefault("zMaxRatio", 1.0_RP)

        if (this%rectLGrid_%zMinRatio_.le.0.0) then
            write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
            write(*,*) "        zMinRatio should be larger than 0.0"
            write(*,*) "        zMesh   meshRatio   zMinRatio   zMaxRatio"
            stop
        end if

        if (this%rectLGrid_%zMaxRatio_.le.0.0) then
            write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
            write(*,*) "        zMaxRatio should be larger than 0.0"
            write(*,*) "        zMesh   meshRatio   zMinRatio   zMaxRatio"
            stop
        end if
    else
        write(*,*) "    [Error] readPostG2GInputFileDict(filePath)"
        write(*,*) "        Wrong input file format is given. Input file should have following form :"
        write(*,*) "        zMesh   meshRatio   zMinRatio   zMaxRatio"
        write(*,*) "        Default mesh configuration will be used."
        this%zflag_ = INDEX_UNIFORM_MESH
    end if

    !!... Wave Probe Dictionary ------------------------------------------------

    this%isWriteWaveProbe_ = dict%getLogicalOrDefault("writeWaveProbe",.false.)

    if (this%isWriteWaveProbe_) then
        waveProbeDict = dict%subDict("waveProbe")
        this%waveFile_%name = waveProbeDict%getCharOrDefault("waveProbeFile", "waveElevation.dat")

        probesDict = waveProbeDict%subDict("waveProbes")
        this%nWaveProbe_ = probesDict%nSubDict()

        if (this%nWaveProbe_.ge.1) then
            allocate(this%waveProbe_(this%nWaveProbe_))
            do iprobe = 1, this%nWaveProbe_
                this%waveProbe_(iprobe)%name_ = probesDict%getSubDictName(iprobe)
                probeDict = probesDict%subDict(this%waveProbe_(iprobe)%name_)

                this%waveProbe_(iprobe)%xPos_ = probeDict%getReal("position", 1)
                this%waveProbe_(iprobe)%yPos_ = probeDict%getReal("position", 2)
            enddo
        else
            write(*,*) "    [WARNING] readPostG2GInputFileDict(filePath)"
            write(*,*) "        Given nWaveProbe is less than 1"
            write(*,*) "        No wave gauge will be used "
            this%nWaveProbe_ = 0
        end if
    else
        this%nWaveProbe_ = 0
    end if

end Subroutine

subroutine readPostG2GInputFile(this, inputFileName)
    implicit none
    class(typPostGrid2Grid), intent(inout) :: this
    character(len=*), intent(in)        :: inputFileName
    character(len=nCharFileLength) tLine
    integer :: io, i, nArg, j
    logical :: iflag
    character(len = StringLength), allocatable :: cArg(:), scArg(:), lcArg(:)
    integer,allocatable ::iArg(:)
    real(RP),allocatable ::rArg(:)
    logical,allocatable :: numFlag(:)

    integer :: iUnit
    !------------------------------------------------------------------

    !! - Set file name and file unit
    this%postInputFile_%name = inputFileName
    this%postInputFile_%unit = callFileUnit()

    iUnit = this%postInputFile_%unit

    this%waveFile_%name = "waveElevation.dat"

    open(unit = iUnit, &
         file = this%postInputFile_%name, &
         status='OLD')

    do
        read(iUnit, "(a)" , IOSTAT=io) tLine
        if (io<0) exit

        !!... Split Line to Argument
        Call splitLine(tLine, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, iflag)

        !!... Read Line if there is character
        if (iflag) then

            !!... Select Keyword to read
            select case(trim(scArg(1)))

            !! HOS Solver
            case('solver')
                if (nArg.ge.2) then
                    this%hosSolver_ = scArg(2)
                    !!- Solver Check (HOS Ocean or HOS NWT)
                    if ((this%hosSolver_.ne."nwt").and.(this%hosSolver_.ne."ocean")) then
                        write(*,*) "    [ERROR] Grid2Grid, readPostG2GInputFile(fileName) "
                        write(*,*) "        Wrong solver is given when HOS post initilize"
                        write(*,*) "        given solver : ", this%hosSolver_
                        stop
                    end if
                    if (this%hosSolver_.eq."nwt") this%hosSolver_ = "NWT"
                    if (this%hosSolver_.eq."ocean") this%hosSolver_ = "Ocean"
                endif

            !! HOS Result File Name
            case('hosfile')
                if (nArg.ge.2) then
                    this%hosFileName_ = cArg(2)
                endif

            !! 3D VTK file Out
            case('writevtk')
                if (nArg.ge.2) then
                    if ((scArg(2).eq."false").or.(scArg(2).eq."f").or.&
                        (scArg(2).eq."no").or.(scArg(2).eq."n")) then
                        this%isWriteVTK_ = .FALSE.
                    end if
                end if

            !! Wave Probe Output
            case('writewaveprobe')
                if (nArg.ge.2) then
                    if ((scArg(2).eq."false").or.(scArg(2).eq."f").or.&
                        (scArg(2).eq."no").or.(scArg(2).eq."n")) then
                        this%isWriteWaveProbe_ = .FALSE.
                    end if
                end if

            !! HOS Post Process Start Time
            case('starttime')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%startTime_ = rArg(2)
                    if (this%startTime_.lt.0.0) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        startTime should be positive"
                        write(*,*) "        given startTime :", this%startTime_
                        stop
                    end if
                endif
            !! HOS Post Process End Time
            case('endtime')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%endTime_  = rArg(2)
                    if (this%endTime_.lt.0.0) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        endTime should be positive"
                        write(*,*) "        given endTime :", this%endTime_
                        stop
                    end if
                endif

            !! HOS Post Process time difference
            case('dt')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%dt_ = rArg(2)
                    if (this%dt_.le.0.0) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        dt should be positive"
                        write(*,*) "        given dt :", this%dt_
                        stop
                    end if
                endif

            !! Mesh build option for air region
            case('airmesh')
                if (nArg.ge.2) then
                    if ((scArg(2).eq.'yes').or.(scArg(2).eq.'y').or. &
                        (scArg(2).eq.'true').or.(scArg(2).eq.'t')) then
                        this%isBuildAirMesh_ = .TRUE.
                    end if
                endif

            !! HOS Post x-minimum
            case('xmin')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%rectLGrid_%xMin_ = rArg(2)
                    if (this%rectLGrid_%xMin_.lt.0.0) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        Wrong xMin is given, xMin should be positive or zero"
                        write(*,*) "        given origin xMin :", this%rectLGrid_%xMin_
                        stop
                    end if
                endif

            !! HOS Post y-minimum
            case('ymin')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%rectLGrid_%yMin_ = rArg(2)
                    if (this%rectLGrid_%yMin_.lt.0.0) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        Wrong yMin is given, yMin should be positive or zero"
                        write(*,*) "        given origin xMin :", this%rectLGrid_%yMin_
                        stop
                    end if
                endif

            !! HOS Post z-minimum
            case('zmin')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%rectLGrid_%zMin_ = rArg(2)
                    if (this%rectLGrid_%zMin_.ge.0.0) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        Wrong zMin is given, zMin should be negative"
                        write(*,*) "        given origin zMin :", this%rectLGrid_%zMin_
                        stop
                    end if
                endif

            !! HOS Post x-max
            case('xmax')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%rectLGrid_%xMax_ = rArg(2)
                    if (this%rectLGrid_%xMax_.lt.0.0) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        Wrong xMax is given, xMax should be positive or zero"
                        write(*,*) "        given origin xMax :", this%rectLGrid_%xMax_
                        stop
                    end if
                endif

            !! HOS Post y-max
            case('ymax')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%rectLGrid_%yMax_ = rArg(2)
                    if (this%rectLGrid_%yMax_.lt.0.0) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        Wrong yMax is given, yMax should be positive or zero"
                        write(*,*) "        given origin yMax :", this%rectLGrid_%yMax_
                        stop
                    end if
                endif

            !! HOS Post z-max
            case('zmax')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%rectLGrid_%zMax_ = rArg(2)
                    if (this%rectLGrid_%zMax_.lt.0.0) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        Wrong zMax is given, zMax should be positive"
                        write(*,*) "        given origin zMax :", this%rectLGrid_%zMax_
                        stop
                    end if
                endif

            !! HOS Post nX
            case('nx')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%rectLGrid_%nX_ = iArg(2)
                    if (this%rectLGrid_%nX_.le.1) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        Wrong nX is given, nX should be larger than 1"
                        write(*,*) "        given origin nX :", this%rectLGrid_%nX_
                        stop
                    end if
                endif

            !! HOS Post nY
            case('ny')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%rectLGrid_%nY_ = iArg(2)
                    if (this%rectLGrid_%nY_.le.1) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        Wrong nY is given, nY should be larger than 1"
                        write(*,*) "        given origin nY :", this%rectLGrid_%nY_
                        stop
                    end if
                endif

            !! HOS Post nZ
            case('nzmin')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%rectLGrid_%nZmin_ = iArg(2)
                    if (this%rectLGrid_%nZmin_.le.3) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        Wrong nZmin is given, nZ should be larger than 3"
                        write(*,*) "        given origin nZmin :", this%rectLGrid_%nZmin_
                        stop
                    end if
                endif

            !! HOS Post nZ
            case('nzmax')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%rectLGrid_%nZmax_ = iArg(2)
                    if (this%rectLGrid_%nZmax_.le.3) then
                        write(*,*) "    [Error] readPostG2GInputFile(fileName)"
                        write(*,*) "        Wrong nZmax is given, nZ should be larger than 3"
                        write(*,*) "        given origin nZmax :", this%rectLGrid_%nZmax_
                        stop
                    end if
                endif

            !! z meshing option
            case('zmesh')
                if (nArg.ge.2) then
                    if (scArg(2).eq."uniform") then
                        this%zflag_ = INDEX_UNIFORM_MESH    ! Uniform Mesh
                    elseif (scArg(2).eq."sine") then
                        this%zflag_ = INDEX_SINE_MESH       ! Sine Distributed Mesh
                    elseif (scArg(2).eq."meshratio") then
                        if ((nArg.ge.4).and.isAllTrue(numFlag(3:4))) then
                            this%zflag_ = INDEX_GEOMETRICRATIO_MESH   ! GEOMETRIC RATIO Mesh
                            this%rectLGrid_%zMinRatio_ = rArg(3)
                            this%rectLGrid_%zMaxRatio_ = rArg(4)
                            if (this%rectLGrid_%zMinRatio_.le.0.0) then
                                write(*,*) "    [ERROR] readPostG2GInputFile(fileName)"
                                write(*,*) "        zMinRatio should be larger than 0.0"
                                write(*,*) "        zMesh   meshRatio   zMinRatio   zMaxRatio"
                                stop
                            end if
                            if (this%rectLGrid_%zMaxRatio_.le.0.0) then
                                write(*,*) "    [ERROR] readPostG2GInputFile(fileName)"
                                write(*,*) "        zMaxRatio should be larger than 0.0"
                                write(*,*) "        zMesh   meshRatio   zMinRatio   zMaxRatio"
                                stop
                            end if
                        else
                            write(*,*) "    [WARNING] readPostG2GInputFile(fileName)"
                            write(*,*) "        Wrong input file format is given. Input file should have following form :"
                            write(*,*) "        zMesh   meshRatio   zMinRatio   zMaxRatio"
                            write(*,*) "        Default mesh configuration will be used."
                            this%zflag_ = INDEX_UNIFORM_MESH                ! Uniform Mesh
                        end if
                    else    ! Default
                        this%zflag_ = INDEX_UNIFORM_MESH
                    endif
                endif

            !! Number of wave gauge
            case('nwaveprobe')
                if ((nArg.ge.2).and.numFlag(2)) then
                    this%nWaveProbe_ = iArg(2)
                    if (this%nWaveProbe_.le.0) then
                        this%isWriteWaveProbe_ = .FALSE.
                        write(*,*) "    [WARNING] readPostG2GInputFile(fileName)"
                        write(*,*) "        Given nWaveProbe is less than 1"
                        write(*,*) "        No wave gauge will be used "
                    else
                        allocate(this%waveProbe_(this%nWaveProbe_))
                        j = 0
                        do i = 1, this%nWaveProbe_
                            read(this%postinputfile_%unit, "(a)" , IOSTAT=io) tLine
                            if (io<0) exit

                            !!... Split Line to Argument
                            Call splitLine(tLine, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, iflag)

                            if (iflag.EQV..FALSE.) exit

                            if (nArg.ge.3) then
                                if ( isAllTrue(numFlag(2:3)) ) then
                                    j = j + 1
                                    this%waveProbe_(j)%name_ = cArg(1)
                                    this%waveProbe_(j)%xPos_ = rArg(2)
                                    this%waveProbe_(j)%yPos_ = rArg(3)
                                end if
                            else if (nArg.ge.2) then
                                if ( isAllTrue(numFlag(1:2)) ) then
                                    j = j + 1
                                    this%waveProbe_(j)%name_ = "waveProbe"//adjustl(int2str(j))
                                    this%waveProbe_(j)%xPos_ = rArg(1)
                                    this%waveProbe_(j)%yPos_ = rArg(2)
                                end if
                            end if

                        end do
                        this%nWaveProbe_ = j
                    end if
                endif

            !! Number of wave gauge
            case('waveprobefile')
                if ((nArg.ge.2)) then
                    this%waveFile_%name = cArg(2)
                endif

            end select

        end if
    enddo

    close(this%postInputFile_%unit)

end subroutine

subroutine checkPostG2GParameter(this)
    implicit none
    class(typPostGrid2Grid), intent(inout) :: this
    integer :: iflag, jflag = 1
    integer :: i

    iflag = 0

    !! Solver Check
    if ((this%hosSolver_.ne."NWT").and.(this%hosSolver_.ne."Ocean")) then
        write(*,*) "    [ERROR] Grid2Grid, checkPostG2GParameter"
        write(*,*) "        Wrong solver is given when HOS initilize"
        write(*,*) "        given solver : ", this%hosSolver_
        iflag = 1
    end if

    !! Start Time and End Time Check
    if (this%endTime_.le.this%startTime_) then
        write(*,*) "    [ERROR] Grid2Grid, checkPostG2GParameter"
        write(*,*) "        endTime is less or equal than startTime"
        write(*,*) "        given startTime : ", this%startTime_
        write(*,*) "        given endTime   : ", this%endTime_
        iflag = 1
    end if

    !! End Time Check
    if (this%endTime_.gt.this%hosVol2Vol_%endTime_) then
        write(*,*) "    [ERROR] Grid2Grid, checkPostG2GParameter"
        write(*,*) "        input endTime is larger than simulation time of HOS result file"
        write(*,*) "        given endTime : ", this%endTime_
        write(*,*) "        HOS   endTime : ", this%hosVol2Vol_%endTime_
        iflag = 1
    end if

    if(this%isWriteVTK_) then

    !! Domain Origin Check
    if ( (this%rectLGrid_%xMin_.lt.0.0).or.(this%rectLGrid_%xMin_.gt.this%hosVol2Vol_%Lx_) ) then
        write(*,*) "    [ERROR] Grid2Grid, checkPostG2GParameter"
        write(*,*) "        xMin should be inside [0.0  HOS Lx]"
        write(*,*) "        given xMin : ", this%rectLGrid_%xMin_
        write(*,*) "        HOS   Lx   : ", this%hosVol2Vol_%Lx_
        iflag = 1
    end if

    if (this%hosVol2Vol_%isHOS2D_.EQV..FALSE.) then
    if ( (this%rectLGrid_%yMin_.lt.0.0).or.(this%rectLGrid_%yMin_.gt.this%hosVol2Vol_%Ly_) ) then
        write(*,*) "    [ERROR] Grid2Grid, checkPostG2GParameter"
        write(*,*) "        yMin should be inside [0.0  HOS Lx]"
        write(*,*) "        given yMin : ", this%rectLGrid_%yMin_
        write(*,*) "        HOS   Ly   : ", this%hosVol2Vol_%Lx_
        iflag = 1
    end if
    end if

    if ( (this%rectLGrid_%xMax_.le.this%rectLGrid_%xMin_) .or. &
         (this%rectLGrid_%xMax_.gt.this%hosVol2Vol_%Lx_) ) then
        write(*,*) "    [ERROR] Grid2Grid, checkPostG2GParameter"
        write(*,*) "        xMax should be inside (xMin  HOS Lx]"
        write(*,*) "        given xMax : ", this%rectLGrid_%xMax_
        write(*,*) "        given xMin : ", this%rectLGrid_%xMin_
        write(*,*) "        HOS   Lx   : ", this%hosVol2Vol_%Lx_
        iflag = 1
    end if

    if (this%hosVol2Vol_%isHOS2D_.EQV..FALSE.) then
    if ( (this%rectLGrid_%yMax_.le.this%rectLGrid_%yMin_) .or. &
         (this%rectLGrid_%yMax_.gt.this%hosVol2Vol_%Ly_) ) then
        write(*,*) "    [ERROR] Grid2Grid, checkPostG2GParameter"
        write(*,*) "        yMax should be inside (yMin  HOS Ly]"
        write(*,*) "        given yMax : ", this%rectLGrid_%yMax_
        write(*,*) "        given yMin : ", this%rectLGrid_%yMin_
        write(*,*) "        HOS   Ly   : ", this%hosVol2Vol_%Ly_
        iflag = 1
    end if
    end if

    if (abs(this%rectLGrid_%zMin_ + this%hosVol2Vol_%waterDepth_).le.convErr) then
        this%rectLGrid_%zMin_ = - this%hosVol2Vol_%waterDepth_
    endif

    if ( (this%rectLGrid_%zMin_.lt.-this%hosVol2Vol_%waterDepth_) ) then
        write(*,*) "    [Warning] Grid2Grid, checkPostG2GParameter"
        write(*,*) "        zMin should be larger than water depth"
        write(*,*) "        given zMin : ", this%rectLGrid_%zMin_
        write(*,*) "        HOS  Depth : ", this%hosVol2Vol_%waterDepth_
        write(*,*) " "
        write(*,*) "        HOS depth will be used for zMin"
        this%rectLGrid_%zMin_ = -this%hosVol2Vol_%waterDepth_
    end if

    if ( (this%rectLGrid_%zMax_.le.this%rectLGrid_%zMin_) ) then
        write(*,*) "    [ERROR] Grid2Grid, checkPostG2GParameter"
        write(*,*) "        zMax should be larger than zMin"
        write(*,*) "        given zMax : ", this%rectLGrid_%zMax_
        write(*,*) "        given zMin : ", this%rectLGrid_%zMin_
        iflag = 1
    end if

    end if

    if (this%isWriteWaveProbe_) then

    if (this%nWaveProbe_.eq.0) then
        this%isWriteWaveProbe_ = .FALSE.
    end if

    do i = 1,this%nWaveProbe_
        if ((this%waveProbe_(i)%xPos_.lt.0.0).or.&
            (this%waveProbe_(i)%xPos_.gt.this%hosVol2Vol_%Lx_)) then
            write(*,*) "    [ERROR] Grid2Grid, checkPostG2GParameter"
            write(*,*) "        Input wave probe x position is wrong"
            write(*,*) "        Wave Gauge Name : ", this%waveProbe_(i)%name_
            write(*,*) "        Wave Gauge xPos : ", this%waveProbe_(i)%xPos_
            write(*,*) "        HOS    X-Length : ", this%hosVol2Vol_%Lx_
            iflag = 1
        end if

        if (this%hosVol2Vol_%isHOS2D_.EQV..FALSE.) then
        if ((this%waveProbe_(i)%yPos_.lt.0.0).or.&
            (this%waveProbe_(i)%yPos_.gt.this%hosVol2Vol_%Ly_ )) then
            write(*,*) "    [ERROR] Grid2Grid, checkPostG2GParameter"
            write(*,*) "        Input wave probe y position is wrong"
            write(*,*) "        Wave Gauge Name : ", this%waveProbe_(i)%name_
            write(*,*) "        Wave Gauge yPos : ", this%waveProbe_(i)%yPos_
            write(*,*) "        HOS    Y-Length : ", this%hosVol2Vol_%Ly_
            iflag = 1
        end if
        else
            this%waveProbe_(i)%yPos_ = 0.0_RP
        endif
    end do

    end if

    if (jflag.eq.1) then
        write(*,1001) ""
        write(*,1001) "#-------------------------------------------------------------------#"
        write(*,1001) "#                                                                   #"
        write(*,1001) "#              HOS GRID2GRID POST PROCESSING PROGRAM                #"
        write(*,1001) "#                                                                   #"
        write(*,1001) "#                        VERSION 1.0.0                              #"
        write(*,1001) "#                                                                   #"
        write(*,1001) "#                  ECOLE CENTRALE DE NANTES                         #"
        write(*,1001) "#                                                                   #"
        write(*,1001) "#-------------------------------------------------------------------#"
        write(*,1001) ""
        write(*,1001) " HOS Grid2Grid Post Input File : ", trim(this%postinputfile_%name)
        write(*,1001) " HOS Solver Type               : ", trim(this%hosSolver_)
        write(*,1001) " HOS Result         Input File : ", trim(this%hosFileName_)
        write(*,1001) ""
        write(*,1001) " Post Processing Parameters"
        write(*,1001) ""
        write(*,3001) "     - startTime : ", this%startTime_
        write(*,3001) "     - endTime   : ", this%endTime_
        write(*,3001) "     - dt        : ", this%dt_
        write(*,1001) ""

        !! - 3D VTK File
        if (this%isWriteVTK_) then
        write(*,1001) "     Write 3D VTK File "
        write(*,1001) ""
        write(*,3001) "     - xMin , xMax  : ", this%rectLGrid_%xMin_, this%rectLGrid_%xMax_
        write(*,3001) "     - yMin , zMax  : ", this%rectLGrid_%yMin_, this%rectLGrid_%yMax_
        write(*,3001) "     - zMin , zMax  : ", this%rectLGrid_%zMin_, this%rectLGrid_%zMax_
        write(*,1001) ""
        write(*,2001) "     - nX, nY       : ", this%rectLGrid_%nX_, this%rectLGrid_%nY_
        write(*,2001) "     - nZmin, nZmax : ", this%rectLGrid_%nZmin_, this%rectLGrid_%nZmax_
        if (this%isBuildAirMesh_) then
        write(*,1001) "     - airMesh   : true"
        else
        write(*,1001) "     - airMesh   : false"
        end if
        else
        write(*,1001) "     No Write 3D VTK File "
        end if
        write(*,1001) ""

        !! - Wave Probe Time Series
        if (this%isWriteWaveProbe_) then
        write(*,1001) "     Write Wave Elevation Time Series "
        write(*,1001) ""
        write(*,2001) "     - Number of Wave Probe : ", this%nWaveProbe_
        else
        write(*,1001) "     No Write Wave Elevation Time Series "
        end if
        write(*,1001) ""

    endif

    if (iflag.ne.0) stop

1001 format(10(a))
2001 format(1(a), 3(i15))
3001 format(1(a), 3(f15.6))

end subroutine

subroutine writeVTKtotalASCII(this, iTime)
    implicit none
    class(typPostGrid2Grid), intent(inout) :: this
    integer, intent(in) :: iTime
    type(typFileIO) :: vtkFile
    integer :: iUnit, ix, iy, iz
    real(RP) :: alpha

    !! Make Output Path
    vtkFile%name = trim(postG2GVTK)//"hosResult_"//adjustl(int2str(iTime))
    vtkFile%name = trim(vtkFile%name)//".vtk"
    vtkFile%unit = callFileUnit()

    iUnit = vtkFile%unit

    open(vtkFile%unit, &
         file = trim(vtkFile%name), &
         status='replace')

    write(vtkFile%unit,1001) "# vtk DataFile Version 3.0"
    write(vtkFile%unit,1001) "vtk output"
    write(vtkFile%unit,1001) "ASCII"
    write(vtkFile%unit,1001) "DATASET RECTILINEAR_GRID"
    write(vtkFile%unit,1002) "DIMENSIONS", this%rectLGrid_%nX_, this%rectLGrid_%nY_, this%rectLGrid_%nZ_

    write(vtkFile%unit,1003) "X_COORDINATES", this%rectLGrid_%nX_, " float"
    write(vtkFile%unit,*) ( this%rectLGrid_%X_(ix), ix =1, this%rectLGrid_%nX_ )

    write(vtkFile%unit,1003) "Y_COORDINATES", this%rectLGrid_%nY_, " float"
    write(vtkFile%unit,*) ( this%rectLGrid_%Y_(iy), iy =1, this%rectLGrid_%nY_ )

    write(vtkFile%unit,1003) "Z_COORDINATES", this%rectLGrid_%nZ_, " float"
    write(vtkFile%unit,*) ( this%rectLGrid_%Z_(iz), iz =1, this%rectLGrid_%nZ_ )

    write(vtkFile%unit,1002) "POINT_DATA", this%rectLGrid_%nX_ * this%rectLGrid_%nY_ * this%rectLGrid_%nZ_

    write(vtkFile%unit,1001) "SCALARS alpha float 1"
    write(vtkFile%unit,1001) "LOOKUP_TABLE default"

    do iz = 1, this%rectLGrid_%nZ_
        do iy = 1, this%rectLGrid_%nY_
            do ix = 1, this%rectLGrid_%nX_
                alpha = 1.0_RP
                if (this%rectLGrid_%Z_(iz).gt.this%rectLGrid_%eta_(ix, iy, iz)) alpha = 0.0_RP
                write(vtkFile%unit,*) alpha
            enddo
        end do
    enddo

    write(vtkFile%unit,1001) "SCALARS pd float 1"
    write(vtkFile%unit,1001) "LOOKUP_TABLE default"

    do iz = 1, this%rectLGrid_%nZ_
        do iy = 1, this%rectLGrid_%nY_
            write(vtkFile%unit,*) (this%rectLGrid_%pd_(ix, iy, iz), ix = 1, this%rectLGrid_%nX_)
        end do
    enddo

    write(vtkFile%unit,1001) "VECTORS Uwave float"

    do iz = 1, this%rectLGrid_%nZ_
        do iy = 1, this%rectLGrid_%nY_
            write(vtkFile%unit,*) (this%rectLGrid_%u_(ix, iy, iz), &
                                   this%rectLGrid_%v_(ix, iy, iz), &
                                   this%rectLGrid_%w_(ix, iy, iz), &
                                   ix = 1, this%rectLGrid_%nX_)
        end do
    enddo

    !! Close File
    close(vtkFile%unit)
    vtkFile%unit = RemoveFileUnit()

    if (iUnit.ne.vtkFile%unit) then
        write(*,*) "    [ERROR] Grid2Grid, writeVTKtotalASCII"
        write(*,*) "        file unit is wrong !!!"
        stop
    endif

    1001 format(a)
    1002 format(a,3(i15))
    1003 format(a,i15,a)
    1004 format(e15.6)

end subroutine

subroutine writeVTKnoAirASCII(this, iTime)
    implicit none
    class(typPostGrid2Grid), intent(inout) :: this
    integer, intent(in) :: iTime
    type(typFileIO) :: vtkFile
    character(len=StringLength) ::cTime
    integer :: iUnit, ix, iy, iz

    !! Make Output Path
    vtkFile%name = trim(postG2GVTK)//"/hosResult_"//adjustl(int2str(iTime))
    vtkFile%name = trim(vtkFile%name)//".vtk"
    vtkFile%unit = callFileUnit()

    iUnit = vtkFile%unit

    open(vtkFile%unit, &
         file = trim(vtkFile%name), &
         status='replace')

    write(vtkFile%unit,1001) "# vtk DataFile Version 3.0"
    write(vtkFile%unit,1001) "vtk output"
    write(vtkFile%unit,1001) "ASCII"
    write(vtkFile%unit,1001) "DATASET STRUCTURED_GRID"
    write(vtkFile%unit,1002) "DIMENSIONS", this%rectLGrid_%nX_, this%rectLGrid_%nY_, this%rectLGrid_%nZ_
    write(vtkFile%unit,1003) "POINTS", this%rectLGrid_%nX_ * this%rectLGrid_%nY_ * this%rectLGrid_%nZ_, " float"
    do iz = 1, this%rectLGrid_%nZ_
        do iy = 1, this%rectLGrid_%nY_
            do ix = 1, this%rectLGrid_%nX_
            write(vtkFile%unit,*) this%rectLGrid_%X_(ix), this%rectLGrid_%Y_(iy), &
                                  this%rectLGrid_%movingZ_(ix, iy, iz)
            enddo
        enddo
    enddo

    write(vtkFile%unit,1002) "POINT_DATA", this%rectLGrid_%nX_ * this%rectLGrid_%nY_ * this%rectLGrid_%nZ_
    write(vtkFile%unit,1001) "SCALARS eta float 1"
    write(vtkFile%unit,1001) "LOOKUP_TABLE default"
    do iz = 1, this%rectLGrid_%nZ_
        do iy = 1, this%rectLGrid_%nY_
            write(vtkFile%unit,*) (this%rectLGrid_%eta_(ix, iy, iz), ix = 1, this%rectLGrid_%nX_)
        end do
    enddo

    write(vtkFile%unit,1001) "SCALARS pd float 1"
    write(vtkFile%unit,1001) "LOOKUP_TABLE default"

    do iz = 1, this%rectLGrid_%nZ_
        do iy = 1, this%rectLGrid_%nY_
            write(vtkFile%unit,*) (this%rectLGrid_%pd_(ix, iy, iz), ix = 1, this%rectLGrid_%nX_)
        end do
    enddo

    write(vtkFile%unit,1001) "VECTORS Uwave float"

    do iz = 1, this%rectLGrid_%nZ_
        do iy = 1, this%rectLGrid_%nY_
            write(vtkFile%unit,*) (this%rectLGrid_%u_(ix, iy, iz), &
                                   this%rectLGrid_%v_(ix, iy, iz), &
                                   this%rectLGrid_%w_(ix, iy, iz), &
                                   ix = 1, this%rectLGrid_%nX_)
        end do
    enddo

    !! Close File
    close(vtkFile%unit)
    vtkFile%unit = RemoveFileUnit()

    if (iUnit.ne.vtkFile%unit) then
        write(*,*) "    [ERROR] Grid2Grid, writeVTKnoAirASCII"
        write(*,*) "        file unit is wrong !!!"
        stop
    endif

    1001 format(a)
    1002 format(a,3(i15))
    1003 format(a,i15,a)
    1004 format(e15.6)

end subroutine

! subroutine writeVTKtotalBINARY(this, iTime)
!     implicit none
!     class(typPostGrid2Grid), intent(inout) :: this
!     integer, intent(in) :: iTime
!     type(typFileIO) :: vtkFile
!     integer :: iUnit, ix, iy, iz
!     real(RP) :: alpha
!     character(len=StringLength) :: cbuffer
!
!     !! Make Output Path
!     vtkFile%name = trim(vtkDirectory)//"/hosResult_"//adjustl(int2str(iTime))
!     vtkFile%name = trim(vtkFile%name)//".vtk"
!     vtkFile%unit = callFileUnit()
!
!     iUnit = vtkFile%unit
!
!     ! open(unit  = vtkFile%unit,&
!     !  file = trim(vtkFile%name),&
!     !  form   = 'unformatted',&
!     !  action = 'write',&
!     !  convert='BIG_ENDIAN',&
!     !  access='sequential')
!
!     open(unit = vtkFile%unit,&
!          file = trim(vtkFile%name),&
!          form = 'unformatted',&
!          action = 'write',&
!          convert='BIG_ENDIAN',&
!          access='stream',&
!          status='replace')
!
!     ! open(unit = vtkFile%unit,&
!     !      file = trim(vtkFile%name),&
!     !      form = 'unformatted',&
!     !      convert='BIG_ENDIAN')
!
!     !!!... Basic .vtk File Format (Header)
!     write(vtkFile%unit) '# vtk DataFile Version 3.0'
!     write(vtkFile%unit) 'vtk output'
!     write(vtkFile%unit) 'BINARY'
!     write(vtkFile%unit) 'DATASET RECTILINEAR_GRID'
!
!     write(cbuffer,'("DIMENSIONS",3(i10,1x))') this%rectLGrid_%nX, this%rectLGrid_%nY, this%rectLGrid_%nZ
!     write(vtkFile%unit) trim(cbuffer)
!
!     write(cbuffer,'("X_COORDINATES",1(i10,1x),"float")') this%rectLGrid_%nX
!     write(vtkFile%unit) trim(cbuffer)
!     do  ix =1, this%rectLGrid_%nX
!         write(vtkFile%unit) real(this%rectLGrid_%X(ix))
!     end do
!
!     write(cbuffer,'("Y_COORDINATES",1(i10,1x),"float")') this%rectLGrid_%nY
!     write(vtkFile%unit) trim(cbuffer)
!     do  iy =1, this%rectLGrid_%nY
!         write(vtkFile%unit) real(this%rectLGrid_%Y(iy))
!     end do
!
!     write(cbuffer,'("Z_COORDINATES",1(i10,1x),"float")') this%rectLGrid_%nZ
!     write(vtkFile%unit) trim(cbuffer)
!     do  iz =1, this%rectLGrid_%nZ
!         write(vtkFile%unit) real(this%rectLGrid_%Z(iz))
!     end do
!
!     ! write(cbuffer,'("POINT_DATA",1(i10,1x),"float")') this%rectLGrid_%nX * this%rectLGrid_%nY * this%rectLGrid_%nZ
!     ! write(vtkFile%unit) trim(cbuffer)
!     !
!     ! cbuffer='SCALARS alpha float 1'; write(vtkFile%unit) trim(cbuffer)
!     ! cbuffer='LOOKUP_TABLE default'; write(vtkFile%unit) trim(cbuffer)
!     !
!     ! do iz = 1, this%rectLGrid_%nZ
!     !      do iy = 1, this%rectLGrid_%nY
!     !          do ix = 1, this%rectLGrid_%nX
!     !              alpha = 1.0_RP
!     !              if (this%rectLGrid_%Z(iz).gt.this%rectLGrid_%eta(ix, iy, iz)) alpha = 0.0_RP
!     !              write(vtkFile%unit) real(alpha)
!     !          enddo
!     !      end do
!     ! enddo
!     !
!     ! cbuffer='SCALARS pd float 1'; write(vtkFile%unit) trim(cbuffer)
!     ! cbuffer='LOOKUP_TABLE default'; write(vtkFile%unit) trim(cbuffer)
!     !
!     ! do iz = 1, this%rectLGrid_%nZ
!     !     do iy = 1, this%rectLGrid_%nY
!     !         write(vtkFile%unit) (real(this%rectLGrid_%pd(ix, iy, iz)), ix = 1, this%rectLGrid_%nX)
!     !     end do
!     ! enddo
!     !
!     ! cbuffer='VECTORS Uwave float'; write(vtkFile%unit) trim(cbuffer)
!     !
!     !
!     ! do iz = 1, this%rectLGrid_%nZ
!     !     do iy = 1, this%rectLGrid_%nY
!     !         write(vtkFile%unit) (real(this%rectLGrid_%u(ix, iy, iz)), &
!     !                              real(this%rectLGrid_%v(ix, iy, iz)), &
!     !                              real(this%rectLGrid_%w(ix, iy, iz)), &
!     !                                ix = 1, this%rectLGrid_%nX)
!     !     end do
!     ! enddo
!
!     ! !! Close File
!     close(vtkFile%unit)
!     vtkFile%unit = RemoveFileUnit()
!
!     if (iUnit.ne.vtkFile%unit) then
!         write(*,*) "    [ERROR] Grid2Grid, writeVTKtotalASCII"
!         write(*,*) "        file unit is wrong !!!"
!         stop
!     endif
!
!     1001 format(a)
!     1002 format(a,3(i15))
!     1003 format(a,i15,a)
!     1004 format(e15.6)
!
! end subroutine
