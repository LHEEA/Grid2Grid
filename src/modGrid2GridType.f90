Module modGrid2GridType
! definition de symboles pour les types reels (RP) et complexes (CP)

! les reels, simple ou double precision
INTEGER, PARAMETER :: SP = KIND(1.0)
INTEGER, PARAMETER :: DP = KIND(1.0D0)

! les complexes, simple ou double precision
INTEGER, PARAMETER :: SPC = KIND(1.0)
INTEGER, PARAMETER :: DPC = KIND(1.0D0)

! Les types courants
INTEGER, PARAMETER :: RP = DP
INTEGER, PARAMETER :: CP = DPC

! les constantes mathematiques usuelles i, pi, 2pi, pi/2, racine de 2.
COMPLEX(CP), PARAMETER :: iMaginary = CMPLX(0.0_rp, 1.0_rp, KIND=CP)
REAL(RP), PARAMETER    :: PI    = 3.141592653589793238462643383279502888419_rp
REAL(RP), PARAMETER    :: g     = 9.81_rp
REAL(RP), PARAMETER    :: PIO2  = 1.570796326794896619231321691639751442098_rp
REAL(RP), PARAMETER    :: TWOPI = 6.283185307179586476925286766559005768394_rp
REAL(RP), PARAMETER    :: SQ2   = 1.414213562373095048801688724209698078569_rp

! For comparison of real numbers
REAL(RP), PARAMETER    :: tiny    = epsilon(1.0_rp)
REAL(RP), PARAMETER    :: convErr = epsilon(1.0) * 10

! String length
INTEGER, PARAMETER :: StringLength = 300

! Input File Character Length
INTEGER, PARAMETER :: nCharFileLength = 5000

! surf2Vol VTK file Out
LOGICAL :: isSurf2VolVTKWrite = .FALSE.

! VTK file Out Directory
Character(len=StringLength),parameter :: vtkDirectory = "./VTK/"
Character(len=StringLength),parameter :: surf2VolVTK  = trim(vtkDirectory)//"Grid2Grid/"
Character(len=StringLength),parameter :: postG2GVTK   = trim(vtkDirectory)//"G2G_3DResult/"

! Building Z mesh Index
integer, parameter :: INDEX_UNIFORM_MESH        = 0
integer, parameter :: INDEX_SINE_MESH           = 1
integer, parameter :: INDEX_GEOMETRICRATIO_MESH = 2

! Function Z Value Criterion
REAL(RP),PARAMETER :: FNZ_VALUE = 1000.0_RP

! File IO Setting
integer :: fidUnit = 101
type typFileIO
    character(len=StringLength) :: name
    integer                     :: unit
end type

!!- HOS Mesh Abstract type
type, public :: typHOSMesh
    integer   :: Nx, Ny, Nz
    integer   :: nZmin, nZmax
    real(rp)  :: zMinRatio = 3.0_RP, zMaxRatio = 3.0_RP

    !!- nondimensionalized values (HOS Domain)
    real(rp) :: nonDimLx, nonDimLy, nonDimZMin, nonDimZMax
    real(rp) :: nonDimDx, nonDimDy
    real(rp),allocatable :: nonDimX(:), nonDimY(:), nonDimZ(:)

    real(rp),allocatable :: nonDimEta(:,:)
    real(rp),allocatable :: nonDimPhiX(:,:,:)
    real(rp),allocatable :: nonDimPhiY(:,:,:)
    real(rp),allocatable :: nonDimPhiZ(:,:,:)
    real(rp),allocatable :: nonDimPhit(:,:,:)

    real(rp),allocatable :: nonDimDuDt(:,:,:)
    real(rp),allocatable :: nonDimDvDt(:,:,:)
    real(rp),allocatable :: nonDimDwDt(:,:,:)

    !!- Dimensionalized values (HOS Domain)

    real(rp) :: dimL, dimT

    real(rp) :: Lx, Ly, zMin, zMax

    real(rp),allocatable :: eta(:,:)
    real(rp),allocatable :: u(:,:,:)
    real(rp),allocatable :: v(:,:,:)
    real(rp),allocatable :: w(:,:,:)
    real(rp),allocatable :: pd(:,:,:)

    real(rp),allocatable :: dudt(:,:,:)
    real(rp),allocatable :: dvdt(:,:,:)
    real(rp),allocatable :: dwdt(:,:,:)

    contains

    !!- Allocate HOS Array
    procedure, pass      :: allocateHOSArray

    !!- Calcul dimensional flow quantities
    procedure, pass      :: calculDimValue

    !!- Deallocate Dynamic Array of HOS Mesh
    procedure,pass       :: destroyHOSMesh

    !!- Destroyer
    final                :: final_HOSMesh

end type

!!! functions and subroutines -------------------------------------------------

contains

    !!!- Converts an integer to a string
    CHARACTER(LEN=12) FUNCTION  int2str(int)
        !CHARACTER(LEN=*) :: int2str
        INTEGER          :: int
        WRITE(int2str,*) int
        int2str = TRIM(ADJUSTL(int2str))
    END FUNCTION int2str

    !!!- Converts a string to an integer
    FUNCTION strtoint(lstr)
        CHARACTER*(*) :: lstr
        INTEGER       :: result,leng,i,strtoint
        leng   = len(lstr)
        result = 0
        DO i=1,leng
            IF (ichar(lstr(i:i))<58 .and.ichar(lstr(i:i))>=48) THEN
                result = 10*result+(ichar(lstr(i:i))-48)
            ENDIF
        ENDDO
        strtoint = result
    END FUNCTION strtoint

    !!!- Auto file unit indexing for Grid2Grid
    integer function callFileUnit()
        callFileUnit = fidUnit
        fidUnit = fidUnit + 1
    end function

    !!!- Special care !!!!!
    integer function RemoveFileUnit()
        fidUnit = fidUnit - 1
        RemoveFileUnit = fidUnit
    end function

    !!!- Allocate HOS Mesh Array
    subroutine allocateHOSArray(this, nX, nY, nZmin, nZmax)
        implicit none
        class(typHOSMesh) :: this
        integer, intent(in) :: nX, nY, nZmin, nZmax

        this%nX = nX
        this%nY = nY
        this%nZmin = nZmin
        this%nZmax = nZmax

        this%nZ = this%nZmin + this%nZmax

        !!- Allocate array
        allocate( this%nonDimX(this%nX) )
        allocate( this%nonDimY(this%nY) )
        allocate( this%nonDimZ(this%nZ) )

        !!- Allocate flow quantity array -----

        !... flow quantities in real domain
        allocate( this%nonDimEta(this%nX, this%nY) )
        allocate( this%nonDimPhiX(this%nX, this%nY, this%nZ) )
        allocate( this%nonDimPhiY(this%nX, this%nY, this%nZ) )
        allocate( this%nonDimPhiZ(this%nX, this%nY, this%nZ) )
        allocate( this%nonDimPhit(this%nX, this%nY, this%nZ) )

        allocate( this%nonDimDuDt(this%nX, this%nY, this%nZ) )
        allocate( this%nonDimDvDt(this%nX, this%nY, this%nZ) )
        allocate( this%nonDimDwDt(this%nX, this%nY, this%nZ) )

        allocate( this%eta(this%nX, this%nY) )
        allocate( this%u(this%nX, this%nY, this%nZ) )
        allocate( this%v(this%nX, this%nY, this%nZ) )
        allocate( this%w(this%nX, this%nY, this%nZ) )
        allocate( this%pd(this%nX, this%nY, this%nZ) )

        allocate( this%dudt(this%nX, this%nY, this%nZ) )
        allocate( this%dvdt(this%nX, this%nY, this%nZ) )
        allocate( this%dwdt(this%nX, this%nY, this%nZ) )

    end subroutine

    subroutine destroyHOSMesh(this)
        implicit none
        class(typHOSMesh),intent(inout) :: this
        if (allocated(this%nonDimX)) deallocate( this%nonDimX )
        if (allocated(this%nonDimY)) deallocate( this%nonDimY )
        if (allocated(this%nonDimZ)) deallocate( this%nonDimZ )

        if (allocated(this%nonDimEta)) deallocate( this%nonDimEta)
        if (allocated(this%nonDimPhiX)) deallocate( this%nonDimPhiX )
        if (allocated(this%nonDimPhiY)) deallocate( this%nonDimPhiY )
        if (allocated(this%nonDimPhiZ)) deallocate( this%nonDimPhiZ )

        if (allocated(this%nonDimPhit)) deallocate( this%nonDimPhit )
        if (allocated(this%nonDimDuDt)) deallocate( this%nonDimDuDt )
        if (allocated(this%nonDimDvDt)) deallocate( this%nonDimDvDt )
        if (allocated(this%nonDimDwDt)) deallocate( this%nonDimDwDt )

        if (allocated(this%eta)) deallocate( this%eta )
        if (allocated(this%u))   deallocate( this%u )
        if (allocated(this%v))   deallocate( this%v )
        if (allocated(this%w))   deallocate( this%w )
        if (allocated(this%pd))  deallocate( this%pd )

        if (allocated(this%dudt)) deallocate( this%dudt )
        if (allocated(this%dvdt)) deallocate( this%dvdt )
        if (allocated(this%dwdt)) deallocate( this%dwdt )
    end subroutine

    subroutine final_HOSMesh(this)
        implicit none
        type(typHOSMesh),intent(inout) :: this
        Call this%destroyHOSMesh
    end subroutine

    !!!- Calcul dimensional values
    subroutine calculDimValue(this)
        implicit none
        class(typHOSMesh) :: this

        this%eta = this%nonDimEta * this%dimL
        this%u   = this%nonDimPhiX * this%dimL / this%dimT
        this%v   = this%nonDimPhiY * this%dimL / this%dimT
        this%w   = this%nonDimPhiZ * this%dimL / this%dimT

        this%dudt = this%nonDimDuDt * this%dimL / this%dimT / this%dimT
        this%dvdt = this%nonDimDvDt * this%dimL / this%dimT / this%dimT
        this%dwdt = this%nonDimDwDt * this%dimL / this%dimT / this%dimT

        this%pd = -0.5_RP * (  this%nonDimPhiX * this%nonDimPhiX &
                             + this%nonDimPhiY * this%nonDimPhiY &
                             + this%nonDimPhiZ * this%nonDimPhiZ ) &
                  - this%nonDimPhit

        this%pd = this%pd * this%dimL**2.d0 / this%dimT**2.d0

    end subroutine

    subroutine splitLine(str, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, iflag)
    !!----------------------------------------------------------------------------------
    !!
    !!  Split character line to character, integer, real argument by space (' ')
    !!
    !!  YoungMyung Choi, Ecole Centrale de Nantes
    !!
    !!----------------------------------------------------------------------------------
    !!  Input
    !!      tLine : Character Line
    !!
    !!  Output
    !!      nArg     : Number of Argument (if nArg = 0, no Argument)
    !!      cArg     : Character argument array, cArg(nArg)
    !!      scArg    : Lower character argument array, cArg(nArg)
    !!      lcArg    : Upper character argument array, cArg(nArg)
    !!      iArg     : Integer   argument array, iArg(nArg)
    !!      rArg     : Real      argument array, rArg(nArg)
    !!      numFlag  : Logical numeric data argument array, numFlag(nArg)
    !!                 if given argument if numeric data, numFlag is .TRUE.
    !!      iflag    : Check given tLine is blank line (if blank, iflag = .FALSE.)
    !!----------------------------------------------------------------------------------
        implicit none
        character(len=*), intent(inout) :: str
        integer, intent(inout) :: nArg
        character(len = StringLength), allocatable, intent(inout) :: cArg(:), scArg(:), lcArg(:)
        integer, allocatable, intent(inout)                       :: iArg(:)
        real(RP), allocatable, intent(inout)                      :: rArg(:)
        logical,allocatable, intent(inout)                        :: numFlag(:)
        logical, intent(out) :: iflag
        !----------------------------------------------------
        integer :: i, io
        character(len=StringLength), allocatable :: args(:)
        character(len=StringLength)              :: dummyStr
        real :: dummyReal
        iflag = .TRUE.
        call parse(str, ' ', args, nArg)
        if (nArg.eq.0) then
            nArg  = 0
            iflag = .FALSE.
            return
        end if
        if (allocated(cArg))    deallocate(cArg)
        if (allocated(scArg))   deallocate(scArg)
        if (allocated(lcArg))   deallocate(lcArg)
        if (allocated(iArg))    deallocate(iArg)
        if (allocated(rArg))    deallocate(rArg)
        if (allocated(numFlag)) deallocate(numFlag)
        allocate(cArg(nArg), scArg(nArg), lcArg(nArg), iArg(nArg), rArg(nArg), numFlag(nArg))
        iArg    = 0
        rArg    = 0.0
        numFlag = .FALSE.
        do i = 1, nArg
            dummyStr = trim(args(i))
            cArg(i)  = dummyStr
            scArg(i) = lower(dummyStr)
            lcArg(i) = upper(dummyStr)
            read(dummyStr, *, iostat = io) dummyReal
            rArg(i) = dummyReal
            iArg(i) = int(rArg(i))
            if (io.eq.0) numFlag(i) = .TRUE.
        end do
    end subroutine

    !**********************************************************************

    subroutine parse(str,delims,args,nargs)
        !
        !  Dr. George Benthiem's string subroutine
        !
        !       See. http://gbenthien.net/about.html
        !
        ! Parses the string 'str' into arguments args(1), ..., args(nargs) based on
        ! the delimiters contained in the string 'delims'. Preceding a delimiter in
        ! 'str' by a backslash (\) makes this particular instance not a delimiter.
        ! The integer output variable nargs contains the number of arguments found.

        character(len=*) :: str,delims
        character(len=len_trim(str)) :: strsav
        character(len=*),allocatable :: args(:)
        character(len=len_trim(str)),allocatable :: tempArgs(:)
        integer :: i
        character(len=1) :: dummy

        strsav=str
        call compact(str)
        na = sizeof(str) / sizeof(dummy)
        if (na.le.0) then
            nargs = 0
            return
        endif
        allocate(args(na), tempArgs(na))
        !na=size(args)
        do i=1,na
            args(i)=' '
        end do
        nargs=0
        lenstr=len_trim(str)
        if(lenstr==0) return
        k=0

        do
            if(len_trim(str) == 0) exit
            nargs=nargs+1
            call split(str,delims,args(nargs))
            call removebksl(args(nargs))
            call compact(args(nargs))
        end do
        tempArgs = args
        deallocate(args)
        allocate(args(nargs))
        do i = 1, nargs
            args(i) = tempArgs(i)
        enddo
        deallocate(tempArgs)
        str=strsav

    end subroutine parse

    !**********************************************************************

    subroutine compact(str)
        !
        !  Dr. George Benthiem's string subroutine
        !
        !       See. http://gbenthien.net/about.html
        !
        ! Converts multiple spaces and tabs to single spaces; deletes control characters;
        ! removes initial spaces.
        character(len=*):: str
        character(len=1):: ch
        character(len=len_trim(str)):: outstr
        str=adjustl(str)
        lenstr=len_trim(str)
        outstr=' '
        isp=0
        k=0
        do i=1,lenstr
            ch=str(i:i)
            ich=iachar(ch)
            select case(ich)
            case(9,32)     ! space or tab character
                if(isp==0) then
                    k=k+1
                    outstr(k:k)=' '
                end if
                isp=1
            case(33:)      ! not a space, quote, or control character
                k=k+1
                outstr(k:k)=ch
                isp=0
            end select
        end do
        str=adjustl(outstr)
    end subroutine compact

!**********************************************************************

    subroutine split(str,delims,before,sep)
        !
        !  Dr. George Benthiem's string subroutine
        !
        !       See. http://gbenthien.net/about.html
        !
        ! Routine finds the first instance of a character from 'delims' in the
        ! the string 'str'. The characters before the found delimiter are
        ! output in 'before'. The characters after the found delimiter are
        ! output in 'str'. The optional output character 'sep' contains the
        ! found delimiter. A delimiter in 'str' is treated like an ordinary
        ! character if it is preceded by a backslash (\). If the backslash
        ! character is desired in 'str', then precede it with another backslash.

        character(len=*) :: str,delims,before
        character,optional :: sep
        logical :: pres
        character :: ch,cha

        pres=present(sep)
        str=adjustl(str)
        call compact(str)
        lenstr=len_trim(str)
        if(lenstr == 0) return        ! string str is empty
        k=0
        ibsl=0                        ! backslash initially inactive
        before=' '
        do i=1,lenstr
            ch=str(i:i)
            if(ibsl == 1) then          ! backslash active
                k=k+1
                before(k:k)=ch
                ibsl=0
                cycle
            end if
            if(ch == '\') then          ! backslash with backslash inactive
                k=k+1
                before(k:k)=ch
                ibsl=1
                cycle
            end if
            ipos=index(delims,ch)
            if(ipos == 0) then          ! character is not a delimiter
                k=k+1
                before(k:k)=ch
                cycle
            end if
            if(ch /= ' ') then          ! character is a delimiter that is not a space
                str=str(i+1:)
                if(pres) sep=ch
                exit
            end if
            cha=str(i+1:i+1)            ! character is a space delimiter
            iposa=index(delims,cha)
            if(iposa > 0) then          ! next character is a delimiter
                str=str(i+2:)
                if(pres) sep=cha
                exit
            else
                str=str(i+1:)
                if(pres) sep=ch
                exit
            end if
        end do
        if(i >= lenstr) str=''
        str=adjustl(str)              ! remove initial spaces
        return

    end subroutine split

    !**********************************************************************

    subroutine removebksl(str)
        !
        !  Dr. George Benthiem's string subroutine
        !
        !       See. http://gbenthien.net/about.html
        !
        ! Removes backslash (\) characters. Double backslashes (\\) are replaced
        ! by a single backslash.

        character(len=*):: str
        character(len=1):: ch
        character(len=len_trim(str))::outstr

        str=adjustl(str)
        lenstr=len_trim(str)
        outstr=' '
        k=0
        ibsl=0                        ! backslash initially inactive

        do i=1,lenstr
            ch=str(i:i)
            if(ibsl == 1) then          ! backslash active
                k=k+1
                outstr(k:k)=ch
                ibsl=0
                cycle
            end if
            if(ch == '\') then          ! backslash with backslash inactive
                ibsl=1
                cycle
            end if
            k=k+1
            outstr(k:k)=ch              ! non-backslash with backslash inactive
        end do

        str=adjustl(outstr)

    end subroutine removebksl

    !**********************************************************************

    FUNCTION Upper(s1)  RESULT (s2)
        CHARACTER(*)       :: s1
        CHARACTER(LEN(s1)) :: s2
        CHARACTER          :: ch
        INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
        INTEGER            :: i

        DO i = 1,LEN(s1)
            ch = s1(i:i)
            IF (ch >= 'a'.AND.ch <= 'z') ch = CHAR(ICHAR(ch)+DUC)
            s2(i:i) = ch
        END DO
    END FUNCTION Upper

    FUNCTION Lower(s1)  RESULT (s2)
        CHARACTER(*)       :: s1
        CHARACTER(LEN(s1)) :: s2
        CHARACTER          :: ch
        INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
        INTEGER            :: i

        DO i = 1,LEN(s1)
            ch = s1(i:i)
            IF (ch >= 'A'.AND.ch <= 'Z') ch = CHAR(ICHAR(ch)-DUC)
            s2(i:i) = ch
        END DO
    END FUNCTION Lower

    Logical Function isAllTrue(logicalArray)
        implicit none
        logical, dimension(:), intent(in) :: logicalArray
        integer :: nn, i
        logical :: dummy
        isAllTrue = .TRUE.
        nn = sizeof(logicalArray) / sizeof(dummy)
        if (nn.eq.0) then
            isAllTrue = .FALSE.
            return
        end if
        do i = 1, nn
            if(logicalArray(i).EQV..FALSE.) then
                isAllTrue = .FALSE.
                return
            end if
        enddo
    end function

    subroutine buildZmesh(zMin, zMax, nZmin, nZmax, Z, iflag, ratio1, ratio2)
        implicit none
        real(rp),intent(in)                :: zMin, zMax
        integer,intent(in)                 :: nZmin, nZmax
        real(rp),allocatable,intent(inout) :: Z(:)
        integer, optional                  :: iflag
        real(rp),optional                  :: ratio1, ratio2
        !-----------------------------------------------------------------
        logical  :: buildAbove = .FALSE.
        real(rp) :: dz, Lz1, Lz2, commonRatio
        integer  :: nZ, iz, jz
        real(rp) :: dummyR

        nZ = nZmin + nZmax

        buildAbove = .FALSE.
        if (nZmax.gt.0) buildAbove = .TRUE.

        if (nZ.le.2) then
            write(*,*) "    [Error] buildZmesh(zMin, zMax, nZmin, nZmax, Z, iflag, ratio1, ratio2)"
            write(*,*) "        Number of Zmesh should be larger than 2"
            stop
        end if

        if (allocated(Z)) then
            if (sizeof(Z) / sizeof(dummyR).ne.nZ) then
                write(*,*) "    [Error] buildZmesh(zMin, zMax, nZmin, nZmax, Z, iflag, ratio1, ratio2)"
                write(*,*) "        Number of Zmesh and given Z(:) array size is different"
                stop
            endif
        else
            allocate(Z(nZ))
        endif

        if (present(iflag).and.(iflag.eq.INDEX_SINE_MESH)) then
            if (buildAbove) then
                Lz1 = abs(zMin)
            else
                Lz1 = zMax - zMin
            endif

            do iz = 1, nZmin
                Z(iz) = zMin + Lz1 * sin(PIO2 * real(iz - 1, RP) / real(nZmin-1,RP))
            enddo

            if (nZmax.ge.1.0) then
                Lz2 = zMax - Z(nZmin)
                do jz = 1, nZmax
                    iz = nZmin + jz
                    Z(iz) = Z(nZmin) &
                           + Lz2 * (1.0_RP + sin(3.0_RP*PIO2 + PIO2*real(jz , RP)/ real(nZmax,RP)))
                end do
            endif

        else if (present(iflag).and.(iflag.eq.INDEX_GEOMETRICRATIO_MESH)) then
            if (present(ratio1).EQV..FALSE.) ratio1 = 1.0_RP
            if (present(ratio2).EQV..FALSE.) ratio2 = 1.0_RP

            if (buildAbove) then
                Lz1 = abs(zMin)
                if (nZmin.le.2) then
                    write(*,*) "    [Error] buildZmesh(zMin, zMax, nZmin, nZmax, Z, iflag, ratio1, ratio2)"
                    write(*,*) "        nZmin should be larger than 2"
                    stop
                end if
                if (nZmax.le.2) then
                    write(*,*) "    [Error] buildZmesh(zMin, zMax, nZmin, nZmax, Z, iflag, ratio1, ratio2)"
                    write(*,*) "        nZmax should be larger than 2"
                    stop
                end if
            else
                Lz1 = zMax - zMin
            end if

            commonRatio = ratio1**( 1.0_RP / (nZmin - 1.0_RP) )
            dz = Lz1 * (1.0_RP - commonRatio) / (1.0_RP - commonRatio**(nZmin - 1.0_RP))
            dz = dz * commonRatio**(nZmin - 2.0_RP)

            Z(1) = zMin
            do iz = 2, nZmin
                Z(iz) = Z(iz- 1) + dz
                dz = dz / commonRatio
            enddo

            if (nZmax.ge.1.0) then
                Lz2 = zMax
                commonRatio = ratio2**( 1.0_RP/ (nZmax - 1.0_RP) )
                dz = Lz2 * (1.0_RP - commonRatio) / (1.0_RP - commonRatio**(nZmax))
                do iz = nZmin + 1, nZmin + nZmax
                    Z(iz) = Z(iz- 1) + dz
                    dz = dz * commonRatio
                enddo
            endif

        else
            if (buildAbove) then
                dz = abs(Zmin) / (nZmin - 1.0_RP)
                do iz = 1, nZmin
                    Z(iz) = Zmin + dz * (iz- 1.0_RP)
                enddo

                dz = abs(Zmax) / real(nZmax,rp)
                do iz = 1, nZmax
                    jz = nZmin + iz
                    Z(jz) = real(dz * iz,rp)
                enddo
            else
                dz = abs(Zmax - Zmin) / (nZ - 1.0_RP)
                do iz = 1, nZ
                    Z(iz) = Zmin + dz * (iz- 1.0_RP)
                enddo
            end if
        end if

    end subroutine

EndModule
