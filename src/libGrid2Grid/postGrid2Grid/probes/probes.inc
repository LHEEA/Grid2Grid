Subroutine initialize_typProbes( this, dict, variableType )
    Implicit None
    class(typProbes), intent(inout)  :: this
    type(typDictionaryPtr)           :: dict
    Type(typHOSVol2Vol)              :: hosVol2VolPtr
    Character(len=*), intent(in)     :: variableType
    !!
    type(typDictionaryPtr) :: probeListDict, probeDict
    Real(RP) :: x, y, z
    integer :: iProbe, nPositionVector
    Real(RP), allocatable :: positionVector(:)

    this%dict_ = dict
    this%isActive = .FALSE.

    !!... Check Variable Type & Set Enumerator for probe type
    this%ENUM_PROBE = - 1
    if ( isStringEqual(variableType, "waveElevation") ) then

        this%variableType = "waveElevation"
        this%ENUM_PROBE   = ENUM_PROBE_WAVE_ELEVATION

    else if ( isStringEqual(variableType, "velocity") ) then

        this%variableType = "velocity"
        this%ENUM_PROBE   = ENUM_PROBE_VELOCITY

    else if ( isStringEqual(variableType, "pressure") ) then

        this%variableType = "pressure"
        this%ENUM_PROBE   = ENUM_PROBE_PRESSURE

    else

        write(*,"(a)") "    [ERROR] initialize_typProbes"
        write(*,"(a)") ""
        write(*,"(a)") "        Wrong Variable Type is given."
        write(*,"(a)") ""
        write(*,"(a)") "        variableType :", trim(variableType)
        write(*,"(a)") ""
        stop

    end if

    !!... Initialize the probes

    probeListDict = this%dict_%subDict("probeList")
    this%nProbe_  = probeListDict%nSubDict()

    this%isActive = .FALSE.
    if ( this%nProbe_.GE.1 ) then
        this%isActive = .TRUE.
    else
        write(*,*) "    [WARNING] initialize_typProbes(dict, variableType)"
        write(*,*) " "
        write(*,*) "        The number of probes is zero. No Gauges will be used"
        write(*,*) " "
        write(*,*) "        variableType: ", trim(variableType)
        write(*,*) " "
        this%isActive = .FALSE.
    end if

    if ( this%isActive.EQV..TRUE. ) then

        allocate( this%probes_( this%nProbe_ ) )

        do iprobe = 1, this%nProbe_

            !!... Get Sub Dict Name & Set Probe Name
            this%probes_(iprobe)%name_ = probeListDict%getSubDictName(iprobe)

            !!... Get Probe Dict
            probeDict = probeListDict%subDict( this%probes_(iprobe)%name_ )

            !!... Get Probe Position
            if ( allocated(positionVector) ) deallocate( positionVector )
            positionVector = probeDict%getRealArray( "position" )

            !!... Get the size of probe position
            nPositionVector = size( positionVector )

            !!... Set Probe Position
            if ( this%ENUM_PROBE == ENUM_PROBE_WAVE_ELEVATION ) then

                !!... Wave Probe: X, Y
                if ( nPositionVector.le.0 ) then
                    write(*,*) "    [WARNING] initialize_typProbes(dict, variableType)"
                    write(*,*) " "
                    write(*,*) "        No Position is given."
                    write(*,*) " "
                    write(*,*) "        probe: ", trim(this%probes_(iprobe)%name_)
                    write(*,*) " "
                    x = 0.0_RP;  y = 0.0_RP; z = 0.0_RP
                else if ( nPositionVector.le.1 ) then
                    x = positionVector(1)
                    y = 0.0_RP
                    z = 0.0_RP
                else
                    x = positionVector(1)
                    y = positionVector(2)
                    z = 0.0_RP
                end if

            else if (  ( this%ENUM_PROBE == ENUM_PROBE_VELOCITY ).OR. &
                       ( this%ENUM_PROBE == ENUM_PROBE_PRESSURE ) ) then

               !!... 3D Probe: X, Y, Z (Velocity, Pressure)

               !!... X, Y, Z
               if ( nPositionVector.le.1 ) then
                   write(*,*) "    [WARNING] initialize_typProbes(dict, variableType)"
                   write(*,*) " "
                   write(*,*) "        No Position is given."
                   write(*,*) " "
                   write(*,*) "        probe: ", trim(this%probes_(iprobe)%name_)
                   write(*,*) " "
                   x = 0.0_RP;  y = 0.0_RP; z = 0.0_RP
               else if ( nPositionVector.le.2 ) then
                   !!... NOTE y position is zero
                   x = positionVector(1)
                   y = 0.0_RP
                   z = positionVector(2)
               else
                   x = positionVector(1)
                   y = positionVector(2)
                   z = positionVector(3)
               end if

           end if

           this%probes_(iprobe)%xPos_ = x
           this%probes_(iprobe)%yPos_ = y
           this%probes_(iprobe)%zPos_ = z

        enddo

    end if

    !!... Open Output File & Write Header
    if ( this%isActive.EQV..TRUE. ) then

        !!... Initialize output file
        this%outFile%name = this%dict_%getChar("outputFile")

        !!... Remove original file if exist
        CALL System( "rm "//trim(this%outFile%name) )

        !!... Get File Unit
        this%outFile%unit = callFileUnit()

        !!... Open File
        open(unit = this%outFile%unit, &
             file = this%outFile%name, &
             status = 'replace')

        !!... Write header
        Call this%writeHeaderOutput( this%outFile%unit )

    end if

End Subroutine

Subroutine correct_typProbes(this, hosVol2Vol, simulTime)
    Implicit None
    class(typProbes), intent(inout) :: this
    Type(typHOSVol2Vol)             :: hosVol2Vol
    Real(RP), intent(in)            :: simulTime
    !!... Local
    Real(RP) :: x, y, z, u, v, w
    integer :: iProbe

    !!... Get Flow Information at the probe position
    if ( this%ENUM_PROBE == ENUM_PROBE_WAVE_ELEVATION ) then

        z = 0.0_RP
        do iProbe = 1, this%nProbe_
            x = this%probes_(iProbe)%xPos_
            y = this%probes_(iProbe)%yPos_
            this%probes_(iProbe)%scalar_ = hosVol2Vol%getEta(x ,y, simulTime)
        enddo

    else if ( this%ENUM_PROBE == ENUM_PROBE_VELOCITY ) then

        do iProbe = 1, this%nProbe_

            x = this%probes_(iProbe)%xPos_
            y = this%probes_(iProbe)%yPos_
            z = this%probes_(iProbe)%zPos_

            Call hosVol2Vol%getU(x ,y, z, simulTime, u, v, w)

            this%probes_(iProbe)%vector_(1) = u
            this%probes_(iProbe)%vector_(2) = v
            this%probes_(iProbe)%vector_(3) = w

        enddo

    else if ( this%ENUM_PROBE == ENUM_PROBE_PRESSURE ) then

        do iProbe = 1, this%nProbe_

            x = this%probes_(iProbe)%xPos_
            y = this%probes_(iProbe)%yPos_
            z = this%probes_(iProbe)%zPos_

            this%probes_(iProbe)%scalar_ = hosVol2Vol%getPd(x ,y, z, simulTime)
        enddo

    end if

    !!... Write the probe info
    if ( this%ENUM_PROBE == ENUM_PROBE_WAVE_ELEVATION ) then

        Call this%writeScalarValue( this%outFile%unit, simulTime )

    else if ( this%ENUM_PROBE == ENUM_PROBE_VELOCITY ) then

        Call this%writeVectorValue( this%outFile%unit, simulTime )

    else if ( this%ENUM_PROBE == ENUM_PROBE_PRESSURE ) then

        Call this%writeScalarValue( this%outFile%unit, simulTime )

    end if

End Subroutine

Subroutine writeHeaderOutput_typProbes(this, fid)
    Implicit None
    class(typProbes), intent(inout) :: this
    Integer, intent(in)             :: fid
    integer :: iProbe

    write(fid,1001) "# HOS Grid2Grid Post Program Result File"
    write(fid,1001) "#"

    if ( this%ENUM_PROBE == ENUM_PROBE_WAVE_ELEVATION ) then
        write(fid,1001) "#    variable : waveElevation"
    else if ( this%ENUM_PROBE == ENUM_PROBE_VELOCITY ) then
        write(fid,1001) "#    variable : velocity"
    else if ( this%ENUM_PROBE == ENUM_PROBE_PRESSURE ) then
        write(fid,1001) "#    variable : pressure"
    end if

    write(fid,1001) "#"

    if ( this%ENUM_PROBE == ENUM_PROBE_WAVE_ELEVATION ) then

        write(fid,1001) "#-----------------------------------------------------------------"
        write(fid,1001) "# ProbeIndex                    name           xPos           yPos"
        write(fid,1001) "#-----------------------------------------------------------------"

        do iProbe = 1, this%nProbe_
            write(fid,1201) iProbe, &
                adjustl(trim(this%probes_(iProbe)%name_)), &
                this%probes_(iProbe)%xPos_, &
                this%probes_(iProbe)%yPos_
        enddo

        write(fid,1001) "#-----------------------------------------------------------------"
        write(fid,1001) "# Time [s]   Probe1   Probe2 ... "
        write(fid,1001) "#-----------------------------------------------------------------"

    else if ( (this%ENUM_PROBE == ENUM_PROBE_VELOCITY).OR. &
              (this%ENUM_PROBE == ENUM_PROBE_PRESSURE) ) then

        write(fid,1001) "#--------------------------------------------------------------------------------"
        write(fid,1001) "# ProbeIndex                    name           xPos           yPos           zPos"
        write(fid,1001) "#--------------------------------------------------------------------------------"

        do iProbe = 1, this%nProbe_
            write(fid,1201) iProbe, &
                adjustl(trim(this%probes_(iProbe)%name_)), &
                this%probes_(iProbe)%xPos_, &
                this%probes_(iProbe)%yPos_, &
                this%probes_(iProbe)%zPos_
        enddo

        write(fid,1001) "#--------------------------------------------------------------------------------"
        if ( this%ENUM_PROBE == ENUM_PROBE_VELOCITY) then
            write(fid,1001) "# Time [s]   Probe1 {Vx Vy Vz}   Probe2 {Vx Vy Vz} ... "
        else if (this%ENUM_PROBE == ENUM_PROBE_PRESSURE) then
            write(fid,1001) "# Time [s]   Probe1   Probe2 ... "
        end if
        write(fid,1001) "#--------------------------------------------------------------------------------"

    endif

    1001 format(a)
    1002 format(2(a))
    1201 format("# ",i10,1x,a23, 10000(f15.3))

    1101 format(10000(e15.7))

End Subroutine

Subroutine writeScalarValue_typProbes(this, fid, simulTime)
    Implicit None
    class(typProbes), intent(inout) :: this
    Integer, intent(in)             :: fid
    Real(RP), intent(in)            :: simulTime
    !!... Local variables
    Integer :: iprob

    write(fid, 1101) simulTime, (this%probes_(iprob)%scalar_, iprob = 1 , this%nProbe_)

    1101 format(10000(e15.7))

End Subroutine

Subroutine writeVectorValue_typProbes(this, fid, simulTime)
    Implicit None
    class(typProbes), intent(inout) :: this
    Integer, intent(in)             :: fid
    Real(RP), intent(in)            :: simulTime
    !!... Local variables
    Integer :: iprob, ivar

    write(fid, 1101) simulTime,                             &
        ( (this%probes_(iprob)%vector_(ivar), ivar=1,3),    &
        iprob = 1, this%nProbe_)

    1101 format(10000(e15.7))

End Subroutine

Subroutine destroy_typProbes(this)
    Implicit None
    class(typProbes), intent(inout) :: this

    close(this%outFile%unit)
    if (allocated(this%probes_)) deallocate(this%probes_)

End Subroutine
