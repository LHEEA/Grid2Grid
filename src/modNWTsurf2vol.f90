!-----------------------------------------------------------------------
Module  modNWTsurf2vol
!-----------------------------------------------------------------------
!
!   Wrapper module of HOS NWT (surf2vol)
!       It construct HOS NWT global mesh and calculate flow quantities
!       on HOS NWT global mesh. To calculate quantities of arbitrary
!       position, vol2vol module is needed.
!
!-----------------------------------------------------------------------
!   This program is part of the Grid2Grid project
!   Copyright (C) 2017 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598
!
!   This program based on Post Processing Part of HOS.
!
!-----------------------------------------------------------------------
!
!       Program intialized by Maite Gouin.
!       Port to modulized format by YoungMyung Choi.
!
!-----------------------------------------------------------------------
use modGrid2GridType
use modFourier_r2c_FFTW3_NWT
use iso_fortran_env, only : error_unit
use modHDF5interface
use hdf5

Implicit none
!!! module variables
    private

    !!- HOS NWT mode array type
    type,private :: typHOSNWTmode

        integer :: nXMode, nYMode, nAddMode

        !!- Mode amplitudes
        real(rp),allocatable :: modeX(:,:), modeY(:,:), modeZ(:,:)
        real(rp),allocatable :: modet(:,:), modeFS(:,:), modeFSt(:,:)
        real(rp),allocatable :: modeAdd(:,:), modeAddt(:,:)

        !!- Wave Numbers
        real(rp),allocatable :: kx(:), ky(:), kxy(:,:), ktheta(:,:)
        real(rp),allocatable :: kxAdd(:)

        contains

            procedure, pass :: allocNWTMode

            !!- Deallocate Dynamic Array of HOS Ocean Mode
            procedure, pass :: destroy => destroyNWTMode

            !!- Destroyer
            final           :: finalHOSNWTMode

    end type

    !!- HOS NWT Mesh type (derived from typHOSMesh)
    type,extends(typHOSMesh), public :: typHOSNWTMesh
        !!- Added Mode Flow Quantities
        real(rp),allocatable :: nonDimAddPhiX(:,:,:)
        real(rp),allocatable :: nonDimAddPhiY(:,:,:)
        real(rp),allocatable :: nonDimAddPhiZ(:,:,:)
        real(rp),allocatable :: nonDimAddPhit(:,:,:)

        real(rp),allocatable :: nonDimAddDuDt(:,:,:)
        real(rp),allocatable :: nonDimAddDvDt(:,:,:)
        real(rp),allocatable :: nonDimAddDwDt(:,:,:)

        !!- Added Mode Z-function
        real(rp),allocatable,private :: csh_add_x(:,:,:)
        real(rp),allocatable,private :: k_add_sh_add_x(:,:,:)
        real(rp),allocatable,private :: kycsh_add_x(:,:,:)
        real(rp),allocatable,private :: kx_add_csh_add_x(:,:,:)

        !!- general mode flow quantities in fourier domain (fixed z)
        real(rp),allocatable,private :: aVitX(:,:), aVitY(:,:), aVitZ(:,:), aPhit(:,:)
        real(rp),allocatable,private :: aDuDt(:,:), aDvDt(:,:), aDwDt(:,:)

        !!- added mode flow quantites in fourier domain
        real(rp),allocatable,private :: lVitX(:),lVitY(:),lVitZ(:),lPhit(:)
        real(rp),allocatable,private :: lDuDt(:),lDvDt(:),lDwDt(:)

        contains

        !!- Build HOS mesh
        procedure, pass, private :: buildHOSNWTMesh

        !!- Deallocate Dynamic Array
        procedure, pass, private :: destroyHOSNWTMesh

        !!- Destroyer
        Final                    :: finalHOSNWTMesh

    end type

    !!- HOS NWT Type
    type, public :: typHOSNWT

        !!! Private -------------------------------------------------------
        private

        !!- HOS NWT file IO
        type(typFileIO) :: hosFile_

        !!- Initialization logical Value
        Logical :: isInitialized_ = .FALSE.

        !!- Simulation Parameters of HOS NWT
        integer         :: nXmode_, nYmode_, nAddmode_
        real(rp),public :: dtOut_, Tstop_
        real(rp),public :: depth_
        real(rp),public :: nonDimDepth_

        real(rp)        :: dimL_, dimT_

        integer         :: nHOSTime_

        real(rp)        :: xLen_, yLen_
        real(rp)        :: zMin_, zMax_

        real(rp)        :: nonDimxLen_, nonDimyLen_
        real(rp)        :: nonDimzMin_, nonDimzMax_

        !!- Added Mode Computation Bool
        logical  :: isAddedMode_ = .true.

        !! - Is HDF5 format
        logical :: isHDF5Format_ = .false.

        !!- Correct time index
        integer  :: iReadTimeIndex_

        !!- HOS NWT mode array (updated by calling read_mod subroutine)
        type(typHOSNWTmode) :: hosMode_

        !!- HOS Ocean FFTW Class
        type(typFFFWHosNWT) :: hosNWTFFTW_

        !!! Public --------------------------------------------------------

        !!- HOS NWT Global Grid
        type(typHOSNWTMesh),public :: hosMesh_

        !!! ***************************************************************
        !!! HOS NWT subroutines
        !!! ***************************************************************

        contains

        !!! Private -------------------------------------------------------

        !!- initialize simulation parameter
        procedure, pass, private :: init_read_mod
        procedure, pass, private :: init_ascii_read_mod
        procedure, pass, private :: init_hdf5_read_mod

        !!- read HOS NWT mode
        procedure, pass, private :: read_mod
        procedure, pass, private :: read_ascii_mod
        procedure, pass, private :: read_hdf5_mod

        !!- build global mesh and wave numbers
        procedure, pass, private :: buildGlobalMesh

        !!- reconstruct flow quantities (gradient, time derivatives of potential)
        procedure, pass, private :: reconstructionFFTs

        !!! Public  -------------------------------------------------------

        !!- initialize HOS NWT Wrapper
        procedure, public :: initialize => initHOSNWT

        !!- read and compute flow
        procedure, public :: correct => correctHOSNWT

        !!- Destroy HOS NWT surf2vol
        procedure, public :: destroy => destroyHOSNWT

    end type typHOSNWT

!-----------------------------------------------------------------------
contains

    !!!- initialize HOS NWT reading Module
    SUBROUTINE initHOSNWT(this, fileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio, isAddedMode)
        !---------------------------------------------------------
        !   Input :
        !       fileName : file path of HOS NWT mode file
        !       nZ       : number of HOS z-directional mesh number
        !---------------------------------------------------------
        implicit none
        class(typHOSNWT), intent(inout) :: this
        character(len=*),intent(in) :: fileName
        real(rp),intent(in) :: zMin, zMax
        integer, intent(in) :: nZmin, nZmax
        real(rp), optional, intent(in) :: zMinRatio, zMaxRatio
        logical,optional, intent(in) :: isAddedMode
        !!!......................................

        if (this%isInitialized_) return

        !! - Set file name and file unit
        this%hosFile_%name = fileName
        this%hosFile_%unit = callFileUnit()

        !! - Set Added Mode Computation Bool Type
        if (present(isAddedMode)) then
            this%isAddedMode_ = isAddedMode
        else
            this%isAddedMode_ = .true.
        endif

        !! - Set output format
        if (index(trim(this%hosFile_%name), ".dat", .true.) /= 0) then
          this%isHDF5Format_ = .false.

        else if (index(trim(this%hosFile_%name), ".h5", .true.) /= 0 .or. &
          index(trim(this%hosFile_%name), ".hdf5", .true.) /= 0 ) then
          this%isHDF5Format_ = .true.
        else
          write(error_unit, '(3A)') "Error in init_read_mod: Unknown file extension for '",&
            trim(this%hosFile_%name), "' (should be .dat, .h5, or .hdf5)"
          stop
        endif

        ! Read and initalize HOS simulation parameter depending on file extension
        Call init_read_mod(this)

        ! Build HOS NWT mesh and Calculate wave number
        Call buildGlobalMesh(this, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio)

        ! Initialize Fourier transforms (FFTW)
        Call this%hosNWTFFTW_%initialize(this%nXmode_, this%nYmode_, this%nAddmode_)

        ! Read HOS NWT mode for first time step
        Call correctHOSNWT(this, 0)

        this%isInitialized_ = .TRUE.

    END SUBROUTINE

    SUBROUTINE correctHOSNWT(this, iTime)
        Implicit None
        class(typHOSNWT), intent(inout) :: this
        Integer, intent(in) :: iTime

        !! - Check time index
        if (iTime.eq.this%iReadTimeIndex_ ) return

        !! - Update time index
        this%iReadTimeIndex_ = iTime

        !! - Read HOS NWT mode for given time index
        Call read_mod(this,iTime)

        !! - Construct General Mode Flow quantities by FFT
        Call reconstructionFFTs(this)

        !! - Added Mode Computation
        if (this%isAddedMode_) then
            !! - Construct Added Mode Flow quantities by FFT
            Call reconstructionAddFFTs(this)

            !! - Add Added mode components
            this%hosMesh_%nonDimPhiX = this%hosMesh_%nonDimPhiX + this%hosMesh_%nonDimAddPhiX
            this%hosMesh_%nonDimPhiY = this%hosMesh_%nonDimPhiY + this%hosMesh_%nonDimAddPhiY
            this%hosMesh_%nonDimPhiZ = this%hosMesh_%nonDimPhiZ + this%hosMesh_%nonDimAddPhiZ
            this%hosMesh_%nonDimPhit = this%hosMesh_%nonDimPhit + this%hosMesh_%nonDimAddPhit

            ! this%hosMesh_%nonDimDuDt = this%hosMesh_%nonDimDuDt + this%hosMesh_%nonDimAddDuDt
            ! this%hosMesh_%nonDimDvDt = this%hosMesh_%nonDimDvDt + this%hosMesh_%nonDimAddDvDt
            ! this%hosMesh_%nonDimDwDt = this%hosMesh_%nonDimDwDt + this%hosMesh_%nonDimAddDwDt
        end if

        Call this%hosMesh_%calculDimValue()

    END SUBROUTINE correctHOSNWT

    Subroutine destroyHOSNWT(this)
        Implicit None
        class(typHOSNWT), intent(inout) :: this

        this%isInitialized_ = .FALSE.

        Call this%hosMode_%destroy

        Call this%hosNWTFFTW_%destroy

        Call this%hosMesh_%destroyHOSNWTMesh

    end subroutine

    !!!- Initialize data from volumic mode description generated by HOS-NWT
    subroutine init_read_mod(this)
        implicit none
        class(typHOSNWT), intent(inout) :: this
        REAL(RP) :: x1, x2, x3
!!!......................................

        if (this%isHDF5Format_) then
          Call init_hdf5_read_mod(this, x1, x2, x3)
        else
          Call init_ascii_read_mod(this, x1, x2, x3)
        endif

        ! Set Simulation Parameters
        this%nXmode_   = nint(x1)
        this%nYmode_   = nint(x2)
        this%nAddmode_ = nint(x3)
        this%nHOSTime_ = nint(this%Tstop_ / this%dtOut_)

        this%depth_ = this%nonDimdepth_
        this%dimL_  = this%nonDimdepth_
        this%dimT_  = real(dsqrt(this%depth_ / g ),RP)

        this%Tstop_ = this%Tstop_ * this%dimT_
        this%dtOut_ = this%dtOut_ * this%dimT_

        this%xLen_ = this%nonDimxLen_ * this%dimL_
        this%yLen_ = this%nonDimyLen_ * this%dimL_

        ! Reading Time Index
        this%iReadTimeIndex_ = -1

        ! set size of HOS NWT mode array
        Call this%hosMode_%allocNWTMode(this%nXmode_, this%nYmode_, this%nAddmode_)

      end subroutine init_read_mod

   subroutine init_ascii_read_mod(this, x1, x2, x3)
        implicit none
        class(typHOSNWT), intent(inout) :: this
        REAL(RP), intent(inout) :: x1, x2, x3
        REAL(RP) :: dummyIndex
        !!!......................................

        ! We will look at first eight variables written on 18 characters
        OPEN(unit = this%hosFile_%unit, &
             file = this%hosFile_%name, &
             status='OLD', &
             form='FORMATTED', &
             access='DIRECT', &
             recl=18*8)
        ! Read HOS NWT simulation parameters
        READ(this%hosFile_%unit,'(8(ES17.10,1X))',rec=1) x1, x3, x2, &
                                                    this%dtOut_, this%Tstop_, &
                                                    this%nonDimxLen_, this%nonDimyLen_, this%nonDimdepth_, &
                                                    dummyIndex

        CLOSE(this%hosFile_%unit) ! close HOS NWT file

        IF ( abs(dummyIndex-0.0_RP) > tiny ) then
            write(*,*) "[ERROR] typHOSNWT::init_read_mod()"
            write(*,*) " "
            write(*,*) "    Input file is not HOS NWT Result File."
            write(*,*) " "
            stop
        ENDIF

    end subroutine init_ascii_read_mod

    subroutine init_hdf5_read_mod(this, x1, x2, x3)
        implicit none
        class(typHOSNWT), intent(inout) :: this
        REAL(RP), intent(inout) :: x1, x2, x3
        !!!......................................

        CHARACTER(len=6), parameter :: HEADER_DSET_NAME = "header"
        INTEGER, parameter :: HEADER_SIZE = 8
        INTEGER(hsize_t), dimension(1), parameter :: HEADER_DATA_DIMS = (/HEADER_SIZE/)

        REAL(DP), dimension(HEADER_SIZE) :: header_data
        INTEGER(hid_t) :: file_id
        INTEGER(hid_t) :: header_dset_id
        INTEGER :: error

        ! Open FORTRAN interface
        Call h5open_f(error)

        ! ! Open the file
        Call h5fopen_f(this%hosFile_%name, H5F_ACC_RDONLY_F, file_id, error)

        ! ! ! Open the header dataset
        Call h5dopen_f(file_id, HEADER_DSET_NAME, header_dset_id, error)

        ! ! ! ! Read the header dataset
        Call h5dread_f(header_dset_id, H5T_NATIVE_DOUBLE, header_data, HEADER_DATA_DIMS, error)

        ! ! ! ! Unpack header_data
        x1 = header_data(1)
        x3 = header_data(2)
        x2 = header_data(3)
        this%dtOut_ = header_data(4)
        this%Tstop_ = header_data(5)
        this%nonDimxLen_ = header_data(6)
        this%nonDimyLen_ = header_data(7)
        this%nonDimdepth_ = header_data(8)

        ! ! ! Close the header dataset
        Call h5dclose_f(header_dset_id, error)

        ! ! Close the file
        Call h5fclose_f(file_id, error)

        ! Close FORTRAN interface
        Call h5close_f(error)

    end subroutine init_hdf5_read_mod

    Subroutine allocNWTMode(this, nXmode, nYmode, nAddmode)
        implicit none
        class(typHOSNWTmode), intent(inout) :: this
        integer, intent(in) :: nXmode, nYmode, nAddmode

        this%nXMode   = nXmode
        this%nYMode   = nYmode
        this%nAddMode = nAddmode

        allocate( this%modeX(nXmode, nYmode) )
        allocate( this%modeY(nXmode, nYmode) )
        allocate( this%modeZ(nXmode, nYmode) )
        allocate( this%modet(nXmode, nYmode) )
        allocate( this%modeFS(nXmode, nYmode) )
        allocate( this%modeFSt(nXmode, nYmode) )

        allocate( this%modeAdd(nAddmode, nYmode) )
        allocate( this%modeAddt(nAddmode, nYmode) )

        allocate( this%kx(nXmode) )
        allocate( this%ky(nXmode) )
        allocate( this%kxy(nXmode, nYmode) )
        allocate( this%ktheta(nXmode, nYmode) )
        allocate( this%kxAdd(nAddmode) )

    end subroutine

    Subroutine destroyNWTMode(this)
        implicit none
        class(typHOSNWTmode), intent(inout) :: this
        if (allocated(this%modeX))    deallocate(this%modeX)
        if (allocated(this%modeY))    deallocate(this%modeY)
        if (allocated(this%modeZ))    deallocate(this%modeZ)
        if (allocated(this%modet))    deallocate(this%modet)

        if (allocated(this%modeFS))   deallocate(this%modeFS)
        if (allocated(this%modeFSt))  deallocate(this%modeFSt)
        if (allocated(this%modeAdd))  deallocate(this%modeAdd)
        if (allocated(this%modeAddt)) deallocate(this%modeAddt)
        if (allocated(this%kx))       deallocate(this%kx)
        if (allocated(this%ky))       deallocate(this%ky)
        if (allocated(this%kxy))      deallocate(this%kxy)
        if (allocated(this%ktheta))   deallocate(this%ktheta)
        if (allocated(this%kxAdd))    deallocate(this%kxAdd)
    End Subroutine

    subroutine finalHOSNWTMode(this)
        implicit none
        type(typHOSNWTmode), intent(inout) :: this
        Call this%destroy()
    end subroutine

    !!!- read HOS NWT mode array from HOS_NWT results file
    subroutine read_mod(this, iTime)
        ! --------------------------------------------------
        !   Input
        !       iTime : time index
        !           - iTime = 0;            time = 0
        !           - iTime = nHOSTime_;    time = T_stop_
        ! --------------------------------------------------
        implicit none
        class(typHOSNWT), intent(inout) :: this
        integer,intent(in) :: iTime
        !!!......................................

        ! Time index check
        if (iTime < 0 .or. iTime > this%nHOSTime_) then
            write(*,*) "[ERROR] modGrid2GridType::read_mod(iTime)"
            write(*,*) " "
            write(*,*) "    iTime exceeds total number of simulation step"
            write(*,*) " "
            stop
        endif

        if (this%isHDF5Format_) then
          Call read_hdf5_mod(this, iTime)
        else
          Call read_ascii_mod(this, iTime)
        endif

    end subroutine read_mod

    subroutine read_ascii_mod(this, iTime)
        implicit none
        class(typHOSNWT), intent(inout) :: this
        integer,intent(in) :: iTime
        integer :: i1,i2, NRECL
        !!!......................................

        ! HOS NWT file open
        OPEN(unit = this%hosFile_%unit, &
             file = this%hosFile_%name, &
             status='OLD', &
             form='FORMATTED', &
             access='DIRECT', &
             recl=18*this%nXmode_)

        ! HOS NWT mode file read
        NRECL = (iTime + 1)*this%nYmode_*8
        do i2=1,this%nYmode_
            READ(this%hosFile_%unit,1001,REC=(NRECL+1+8*(i2-1))) (this%hosMode_%modeX(i1,i2), i1=1,this%nXmode_)
            READ(this%hosFile_%unit,1001,REC=(NRECL+2+8*(i2-1))) (this%hosMode_%modeY(i1,i2), i1=1,this%nXmode_)
            READ(this%hosFile_%unit,1001,REC=(NRECL+3+8*(i2-1))) (this%hosMode_%modeZ(i1,i2), i1=1,this%nXmode_)
            READ(this%hosFile_%unit,1001,REC=(NRECL+4+8*(i2-1))) (this%hosMode_%modet(i1,i2), i1=1,this%nXmode_)
            READ(this%hosFile_%unit,1001,REC=(NRECL+5+8*(i2-1))) (this%hosMode_%modeFS(i1,i2), i1=1,this%nXmode_)
            READ(this%hosFile_%unit,1001,REC=(NRECL+6+8*(i2-1))) (this%hosMode_%modeFSt(i1,i2), i1=1,this%nXmode_)
            READ(this%hosFile_%unit,1001,REC=(NRECL+7+8*(i2-1))) (this%hosMode_%modeAdd(i1,i2), i1=1,this%nAddmode_)
            READ(this%hosFile_%unit,1001,REC=(NRECL+8+8*(i2-1))) (this%hosMode_%modeAddt(i1,i2), i1=1,this%nAddmode_)
        end do

        CLOSE(this%hosFile_%unit)    ! file close

        ! file reading format for HOS NWT
        1001 format((5000(ES17.10,1X)))
    end subroutine read_ascii_mod

    subroutine read_hdf5_mod(this, iTime)
        implicit none
        class(typHOSNWT), intent(inout) :: this
        integer,intent(in) :: iTime

        ! Time group format
        CHARACTER(len=6), parameter :: TIME_C_FORMAT = '(I0.6)' ! Increase .6) if necessary
        CHARACTER(len=6) :: time_c ! Increase len=6 if necessary

        INTEGER :: n
        INTEGER(hid_t) :: file_id, time_group_id
        INTEGER :: error

        ! Dimensions of NWT mode
        INTEGER(hsize_t), dimension(2) :: nwt_mode_data_dims

        ! Open FORTRAN interface
        Call h5open_f(error)

        ! ! Open the file
        Call h5fopen_f(this%hosFile_%name, H5F_ACC_RDONLY_F, file_id, error)

        ! ! ! Open the iTime group
        if (iTime > 999999) then
          write(error_unit, '(2A)') "Implementation error in read_hdf5_mod: " // &
            "Unexpected iTime value > 999999 ! Modify 'time_c' variable size and 'TIME_C_FORMAT'"
          stop
        endif
        write(time_c,TIME_C_FORMAT) iTime
        Call h5gopen_f(file_id, "/time_"//trim(time_c), time_group_id, error)

        ! ! ! ! Read datasets
        ! ! ! ! nXmode_ x nYmode_
        nwt_mode_data_dims(1) = this%nXmode_
        nwt_mode_data_dims(2) = this%nYmode_
        Call read_hdf5_dataset_mod(time_group_id, "modeX", nwt_mode_data_dims, this%hosMode_%modeX)
        Call read_hdf5_dataset_mod(time_group_id, "modeY", nwt_mode_data_dims, this%hosMode_%modeY)
        Call read_hdf5_dataset_mod(time_group_id, "modeZ", nwt_mode_data_dims, this%hosMode_%modeZ)
        Call read_hdf5_dataset_mod(time_group_id, "modet", nwt_mode_data_dims, this%hosMode_%modet)
        Call read_hdf5_dataset_mod(time_group_id, "modeFS", nwt_mode_data_dims, this%hosMode_%modeFS)
        Call read_hdf5_dataset_mod(time_group_id, "modeFSt", nwt_mode_data_dims, this%hosMode_%modeFSt)

        ! ! ! ! nAddmode_ x nYmode_
        nwt_mode_data_dims(1) = this%nAddmode_
        Call read_hdf5_dataset_mod(time_group_id, "modeAdd", nwt_mode_data_dims, this%hosMode_%modeAdd)
        Call read_hdf5_dataset_mod(time_group_id, "modeAddt", nwt_mode_data_dims, this%hosMode_%modeAddt)

        ! ! ! Close iTime group
        Call h5gclose_f(time_group_id, error)

        ! ! Close the file
        Call h5fclose_f(file_id, error)

        ! Close FORTRAN interface
        Call h5close_f(error)

    end subroutine read_hdf5_mod


    !!- Build HOS NWT Global Mesh & calculate Wave numbers and additional mode part
    subroutine buildGlobalMesh(this, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio)
        implicit none
        class(typHOSNWT), intent(inout) :: this
        real(rp),intent(in) :: zMin
        real(rp),intent(in) :: zMax
        integer, intent(in) :: nZmin, nZmax
        real(rp), optional  :: zMinRatio, zMaxRatio
        !! z-function criterion
        real(rp), parameter :: kAddXMax = 700.0_rp
        !! dummy variables
        integer  :: ix, iy, iAdd
        !! local variables for wave number
        integer  :: lAdd
        real(rp) :: xLenAdd
        real(rp) :: piXLen, piYLen, piXAddLen
        real(rp) :: kx2, ky2, kxAdd2
        !! local variables for additional mode part
        real(rp) :: csAdd, sAdd, kAddsAdd
        real(rp) :: kxyAddLx, kxyAdd, kxyAddTwo
        real(rp) :: expon1, expon2, expon3, expon12
        real(rp) :: kAddx1, kAddx2
        real(rp) :: cshAdd, shAdd
        !!!......................................

        lAdd = 2
        xLenAdd = 2.d0 * real(lAdd)

        !!- Build global mesh -------------------

        this%zMin_ = zMin
        this%zMax_ = zMax

        !.. Truncation Error for zMin
        if (abs(this%zMin_ + this%depth_).le.convErr) then
            this%zMin_ = -this%depth_
        endif

        !-- Check z domain length
        if (-this%zMin_.gt.this%depth_) then
            write(*,*) "    [Error] NWTSurf2Vol::buildGlobalMesh(zMin, zMax, nZmin, nZmax)"
            write(*,*) "        zMin is over than water depth !"
            write(*,*) "        zMin      : ", -this%zMin_
            write(*,*) "        HOS depth : ", this%depth_
            stop
        endif

        ! nondimensionalization of z length
        this%nonDimzMin_ = this%zMin_ / this%dimL_
        this%nonDimzMax_ = this%zMax_ / this%dimL_

        ! build HOS global mesh
        this%hosMesh_%dimL = this%dimL_
        this%hosMesh_%dimT = this%dimT_

        Call this%hosMesh_%buildHOSNWTMesh(this%nonDimxLen_, this%nonDimyLen_, &
                                           this%nonDimzMin_, this%nonDimzMax_, &
                                           this%nXmode_, this%nYmode_, &
                                           nZmin, nZmax, zMinRatio, zMaxRatio)

        !... added mode flow quantities in real domain
        allocate( this%hosMesh_%nonDimAddPhiX(this%hosMesh_%nX, this%hosMesh_%nY, this%hosMesh_%nZ) )
        allocate( this%hosMesh_%nonDimAddPhiY(this%hosMesh_%nX, this%hosMesh_%nY, this%hosMesh_%nZ) )
        allocate( this%hosMesh_%nonDimAddPhiZ(this%hosMesh_%nX, this%hosMesh_%nY, this%hosMesh_%nZ) )
        allocate( this%hosMesh_%nonDimAddPhit(this%hosMesh_%nX, this%hosMesh_%nY, this%hosMesh_%nZ) )

        allocate( this%hosMesh_%nonDimAddDuDt(this%hosMesh_%nX, this%hosMesh_%nY, this%hosMesh_%nZ) )
        allocate( this%hosMesh_%nonDimAddDvDt(this%hosMesh_%nX, this%hosMesh_%nY, this%hosMesh_%nZ) )
        allocate( this%hosMesh_%nonDimAddDwDt(this%hosMesh_%nX, this%hosMesh_%nY, this%hosMesh_%nZ) )

        !... general mode flow quantities in Fourier Domain
        allocate( this%hosMesh_%aVitX(this%nXmode_, this%nYmode_) )
        allocate( this%hosMesh_%aVitY(this%nXmode_, this%nYmode_) )
        allocate( this%hosMesh_%aVitZ(this%nXmode_, this%nYmode_) )
        allocate( this%hosMesh_%aPhit(this%nXmode_, this%nYmode_) )
        allocate( this%hosMesh_%aDuDt(this%nXmode_, this%nYmode_) )
        allocate( this%hosMesh_%aDvDt(this%nXmode_, this%nYmode_) )
        allocate( this%hosMesh_%aDwDt(this%nXmode_, this%nYmode_) )

        !... added mode flow quantities in Fourier Domain
        allocate( this%hosMesh_%lVitX(this%nYmode_))
        allocate( this%hosMesh_%lVitY(this%nYmode_))
        allocate( this%hosMesh_%lVitZ(this%nYmode_))
        allocate( this%hosMesh_%lPhit(this%nYmode_))
        allocate( this%hosMesh_%lDuDt(this%nYmode_))
        allocate( this%hosMesh_%lDvDt(this%nYmode_))
        allocate( this%hosMesh_%lDwDt(this%nYmode_))

        !!- build wave number -----------------
        allocate( this%hosMesh_%csh_add_x(this%nXmode_, this%nYmode_, this%nAddmode_) )
        allocate( this%hosMesh_%k_add_sh_add_x(this%nXmode_, this%nYmode_, this%nAddmode_) )
        allocate( this%hosMesh_%kycsh_add_x(this%nXmode_, this%nYmode_, this%nAddmode_) )
        allocate( this%hosMesh_%kx_add_csh_add_x(this%nXmode_, this%nYmode_, this%nAddmode_) )

        !! X, Y General mode wave number
        piXLen = PI / this%hosMesh_%nonDimLx
        if (this%nYmode_ == 1) then
            piYLen = 0.d0
        else
            piYLen = PI / this%hosMesh_%nonDimLy
        end if

        do iy = 1, this%nYmode_
            this%hosMode_%ky(iy) = (iy - 1.d0) * piYLen
        end do

        do ix = 1, this%nXmode_
            this%hosMode_%kx(ix) = (ix - 1.d0) * piXLen
        end do

        do ix = 1, this%nXmode_
            kx2 = this%hosMode_%kx(ix) * this%hosMode_%kx(ix)
            do iy = 1, this%nYmode_
                ky2 = this%hosMode_%ky(iy) * this%hosMode_%ky(iy)
                this%hosMode_%kxy(ix,iy)    = sqrt(kx2 + ky2)
                this%hosMode_%ktheta(ix,iy) = this%hosMode_%kxy(ix,iy) &
                                                *tanh(this%hosMode_%kxy(ix,iy))
            end do
        end do

        ! Wave maker added mode wave number
        piXAddLen = PI / xLenAdd

        do iAdd = 1, this%nAddmode_
            this%hosMode_%kxAdd(iAdd) = (2.d0 * iAdd - 1.d0) * piXAddLen
        enddo

        ! wave maker mode part
        do iAdd = 1, this%nAddmode_
            kxAdd2 = this%hosMode_%kxAdd(iAdd) * this%hosMode_%kxAdd(iAdd)
            csAdd    = cos(this%hosMode_%kxAdd(iAdd))
            sAdd     = sin(this%hosMode_%kxAdd(iAdd))
            kAddsAdd = this%hosMode_%kxAdd(iAdd) * sAdd

            do iy = 1, this%nYmode_
                ky2 = this%hosMode_%ky(iy) * this%hosMode_%ky(iy)
                kxyAdd  = sqrt(kxAdd2 + ky2)
                kxyAddTwo = 2.d0 * kxyAdd
                kxyAddLx  = kxyAddTwo * this%hosMesh_%nonDimLx

                expon3 = 0.0_rp
                if ( kxyAddLx.le.kAddXMax ) expon3 = exp(-kxyAddLx)

                do ix = 1, this%nXmode_

                    kAddx1 = kxyAdd  * this%hosMesh_%nonDimX(ix)
                    expon1 = 0.0_rp
                    if (kAddx1.le.kAddXMax) expon1 = exp(-kAddx1) / (expon3 + 1.d0)

                    kAddx2 = kxyAddTwo * (this%hosMesh_%nonDimLx - this%hosMesh_%nonDimX(ix))
                    expon2 = 0.0_rp
                    if (kAddx2.le.kAddXMax) expon2 = exp(-kAddx2)

                    expon12 = 0.0_rp
                    if ((kAddx1 + kAddx2).le.kAddXMax) expon12 = expon1 * expon2

                    cshAdd = expon1 + expon12
                    shAdd  = expon1 - expon12

                    this%hosMesh_%csh_add_x(ix,iy,iAdd)        = cshAdd
                    this%hosMesh_%k_add_sh_add_x(ix,iy,iAdd)   = shAdd  * kxyAdd
                    this%hosMesh_%kycsh_add_x(ix,iy,iAdd)      = cshAdd * this%hosMode_%ky(iy)
                    this%hosMesh_%kx_add_csh_add_x(ix,iy,iAdd) = cshAdd * this%hosMode_%kxAdd(iAdd)

                enddo
            enddo
        enddo

    end subroutine

    subroutine destroyHOSNWTMesh(this)
        implicit none
        class(typHOSNWTMesh), intent(inout) :: this
        Call this%destroyHOSMesh        !! Destroy Base Type

        if ( allocated(this%nonDimAddPhiX) ) deallocate( this%nonDimAddPhiX )
        if ( allocated(this%nonDimAddPhiY) ) deallocate( this%nonDimAddPhiY )
        if ( allocated(this%nonDimAddPhiZ) ) deallocate( this%nonDimAddPhiZ )
        if ( allocated(this%nonDimAddPhit) ) deallocate( this%nonDimAddPhit )

        if ( allocated(this%nonDimAddDuDt) ) deallocate( this%nonDimAddDuDt )
        if ( allocated(this%nonDimAddDvDt) ) deallocate( this%nonDimAddDvDt )
        if ( allocated(this%nonDimAddDwDt) ) deallocate( this%nonDimAddDwDt )

        if ( allocated(this%csh_add_x) )        deallocate( this%csh_add_x )
        if ( allocated(this%k_add_sh_add_x) )   deallocate( this%k_add_sh_add_x )
        if ( allocated(this%kycsh_add_x) )      deallocate( this%kycsh_add_x )
        if ( allocated(this%kx_add_csh_add_x) ) deallocate( this%kx_add_csh_add_x )

        if ( allocated(this%aVitX) ) deallocate( this%aVitX )
        if ( allocated(this%aVitY) ) deallocate( this%aVitY )
        if ( allocated(this%aVitZ) ) deallocate( this%aVitZ )
        if ( allocated(this%aPhit) ) deallocate( this%aPhit )

        if ( allocated(this%aDuDt) ) deallocate( this%aDuDt )
        if ( allocated(this%aDvDt) ) deallocate( this%aDvDt )
        if ( allocated(this%aDwDt) ) deallocate( this%aDwDt )

        if ( allocated(this%lVitX) ) deallocate( this%lVitX )
        if ( allocated(this%lVitY) ) deallocate( this%lVitY )
        if ( allocated(this%lVitZ) ) deallocate( this%lVitZ )
        if ( allocated(this%lPhit) ) deallocate( this%lPhit )

        if ( allocated(this%lDuDt) ) deallocate( this%lDuDt )
        if ( allocated(this%lDvDt) ) deallocate( this%lDvDt )
        if ( allocated(this%lDwDt) ) deallocate( this%lDwDt )
    end subroutine

    subroutine finalHOSNWTMesh(this)
        implicit none
        type(typHOSNWTMesh), intent(inout) :: this
        Call this%destroyHOSNWTMesh
    end subroutine

    subroutine reconstructionFFTs(this)
        implicit none
        class(typHOSNWT), intent(inout) :: this
        integer :: ix, iy, iz
        real(rp) :: hosZ, k, kZ
        real(rp) :: coeff, coeff2
        !!!......................................

        !! - Compute nondimensionalized wave elevation
        this%hosMesh_%nonDimEta = this%hosNWTFFTW_%fourier_2_space(this%hosMode_%modeFS,'cos','cos')

        do iz = 1, this%hosMesh_%nZ
            hosZ = this%hosMesh_%nonDimZ(iz)

            ix = 1
            iy = 1
            this%hosMesh_%aVitX(ix, iy) =  this%hosMode_%modeX(ix, iy)
            this%hosMesh_%aVitY(ix, iy) =  this%hosMode_%modeY(ix, iy)
            this%hosMesh_%aVitZ(ix, iy) =  this%hosMode_%modeZ(ix, iy)
            this%hosMesh_%aPhit(ix, iy) =  this%hosMode_%modet(ix, iy)
            ! this%hosMesh_%aDuDt(ix, iy) = -this%hosMode_%kx(ix) * this%hosMode_%modet(ix, iy)
            ! this%hosMesh_%aDvDt(ix, iy) = -this%hosMode_%ky(iy) * this%hosMode_%modet(ix, iy)
            ! this%hosMesh_%aDwDt(ix, iy) =  this%hosMode_%ktheta(ix,iy) * this%hosMode_%modet(ix, iy)

            do iy = 2, this%nYmode_
                k = this%hosMode_%kxy(ix,iy)
                kZ = k * (hosZ + 1.0_rp)
                if ((kZ.lt.50.0).and.(k.lt.50.0)) then
                    ! coeff  = cosh(kZ) / cosh(k)
                    ! coeff2 = sinh(kZ) / sinh(k)
                    coeff  = exp(k * hosZ) * (1.0_rp + exp(-2.0_rp*kZ)) / (1.0_rp + exp(-2.0_rp * k))
                    coeff2 = exp(k * hosZ) * (1.0_rp - exp(-2.0_rp*kZ)) / (1.0_rp - exp(-2.0_rp * k))
                else
                    coeff  = exp(kZ)
                    coeff2 = coeff
                endif

                if (coeff.ge.FNZ_VALUE) then
                    coeff = FNZ_VALUE
                endif

                if (coeff2.ge.FNZ_VALUE) then
                    coeff2 = FNZ_VALUE
                endif

                this%hosMesh_%aVitX(ix, iy) =  this%hosMode_%modeX(ix, iy) * coeff
                this%hosMesh_%aVitY(ix, iy) =  this%hosMode_%modeY(ix, iy) * coeff
                this%hosMesh_%aVitZ(ix, iy) =  this%hosMode_%modeZ(ix, iy) * coeff2
                this%hosMesh_%aPhit(ix, iy) =  this%hosMode_%modet(ix, iy) * coeff
                ! this%hosMesh_%aDuDt(ix, iy) = -this%hosMode_%kx(ix) * this%hosMesh_%aPhit(ix, iy)
                ! this%hosMesh_%aDvDt(ix, iy) = -this%hosMode_%ky(iy) * this%hosMesh_%aPhit(ix, iy)
                ! this%hosMesh_%aDwDt(ix, iy) =  this%hosMode_%ktheta(ix,iy) * this%hosMode_%modet(ix, iy) * coeff2
            enddo

            do ix = 2, this%nXmode_
                do iy = 1, this%nYmode_
                    k = this%hosMode_%kxy(ix,iy)
                    kZ = k * (hosZ + 1.0_rp)
                    if ((kZ.lt.50.0).and.(k.lt.50.0)) then
                        coeff  = cosh(kZ) / cosh(k)
                        coeff2 = sinh(kZ) / sinh(k)
                    else
                        coeff  = exp(k * hosZ)
                        coeff2 = coeff
                    endif

                    if (coeff.ge.FNZ_VALUE) then
                        coeff = FNZ_VALUE
                    endif

                    if (coeff2.ge.FNZ_VALUE) then
                        coeff2 = FNZ_VALUE
                    endif

                    this%hosMesh_%aVitX(ix, iy) =  this%hosMode_%modeX(ix, iy) * coeff
                    this%hosMesh_%aVitY(ix, iy) =  this%hosMode_%modeY(ix, iy) * coeff
                    this%hosMesh_%aVitZ(ix, iy) =  this%hosMode_%modeZ(ix, iy) * coeff2
                    this%hosMesh_%aPhit(ix, iy) =  this%hosMode_%modet(ix, iy) * coeff
                    ! this%hosMesh_%aDuDt(ix, iy) = -this%hosMode_%kx(ix) * this%hosMesh_%aPhit(ix, iy)
                    ! this%hosMesh_%aDvDt(ix, iy) = -this%hosMode_%ky(iy) * this%hosMesh_%aPhit(ix, iy)
                    ! this%hosMesh_%aDwDt(ix, iy) =  this%hosMode_%ktheta(ix,iy) * this%hosMode_%modet(ix, iy) * coeff2
                enddo
            enddo

            ! Inverse FFTs
            this%hosMesh_%nonDimPhiX(:,:, iz) = this%hosNWTFFTW_%fourier_2_space(this%hosMesh_%aVitX,'sin','cos')
            if (this%nYmode_.NE.1) then
                this%hosMesh_%nonDimPhiY(:,:, iz) = this%hosNWTFFTW_%fourier_2_space(this%hosMesh_%aVitY,'cos','sin')
                ! this%hosMesh_%nonDimDvDt(:,:, iz) = this%hosNWTFFTW_%fourier_2_space(this%hosMesh_%aDvDt,'cos','sin')
            else
                this%hosMesh_%nonDimPhiY(:,:, iz) = 0.0_rp
                ! this%hosMesh_%nonDimDvDt(:,:, iz) = 0.0_rp
            end if
            this%hosMesh_%nonDimPhiZ(:,:, iz) = this%hosNWTFFTW_%fourier_2_space(this%hosMesh_%aVitZ,'cos','cos')
            this%hosMesh_%nonDimPhit(:,:, iz) = this%hosNWTFFTW_%fourier_2_space(this%hosMesh_%aPhit,'cos','cos')
            ! this%hosMesh_%nonDimDuDt(:,:, iz) = this%hosNWTFFTW_%fourier_2_space(this%hosMesh_%aDuDt,'cos','cos')
            ! this%hosMesh_%nonDimDwDt(:,:, iz) = this%hosNWTFFTW_%fourier_2_space(this%hosMesh_%aDwDt,'cos','cos')

        enddo

    end subroutine

    subroutine reconstructionAddFFTs(this)
        implicit none
        class(typHOSNWT), intent(inout) :: this
        integer  :: ix, iy, iz, iAdd
        REAL(RP) :: coskx_add,sinkx_add,coeff1,coeff2,coeff3,coeff4
        REAL(RP) :: hosZ

        do iz = 1, this%hosMesh_%nZ
            hosZ = this%hosMesh_%nonDimZ(iz)
            do ix = 1, this%hosMesh_%nX
                do iy = 1, this%hosMesh_%nY

                    this%hosMesh_%lVitX(iy) = 0.0_rp
                    this%hosMesh_%lVitY(iy) = 0.0_rp
                    this%hosMesh_%lVitZ(iy) = 0.0_rp
                    this%hosMesh_%lPhit(iy) = 0.0_rp

                    ! this%hosMesh_%lDuDt(iy) = 0.0_rp
                    ! this%hosMesh_%lDvDt(iy) = 0.0_rp
                    ! this%hosMesh_%lDwDt(iy) = 0.0_rp

                    do iAdd = 1, this%nAddmode_
                        coskx_add = cos(this%hosMode_%kxAdd(iAdd) * (hosZ + 1.0_rp))
                        sinkx_add = sin(this%hosMode_%kxAdd(iAdd) * (hosZ + 1.0_rp))

                        coeff1 = coskx_add * this%hosMesh_%csh_add_x(ix,iy,iAdd)
                        coeff2 = sinkx_add * this%hosMesh_%kx_add_csh_add_x(ix,iy,iAdd)
                        coeff3 = coskx_add * this%hosMesh_%k_add_sh_add_x(ix,iy,iAdd)
                        coeff4 = coskx_add * this%hosMesh_%kycsh_add_x(ix,iy,iAdd)

                        this%hosMesh_%lVitX(iy) =  this%hosMesh_%lVitX(iy) &
                                                    - this%hosMode_%modeAdd(iAdd, iy) * coeff3
                        this%hosMesh_%lVitY(iy) =  this%hosMesh_%lVitY(iy) &
                                                    - this%hosMode_%modeAdd(iAdd, iy) * coeff4
                        this%hosMesh_%lVitZ(iy) =  this%hosMesh_%lVitZ(iy) &
                                                    - this%hosMode_%modeAdd(iAdd, iy) * coeff2
                        this%hosMesh_%lPhit(iy) =  this%hosMesh_%lPhit(iy) &
                                                    + this%hosMode_%modeAddt(iAdd, iy) * coeff1

                        ! this%hosMesh_%lDuDt(iy) =  this%hosMesh_%lDuDt(iy) &
                        !                             - this%hosMode_%modeAddt(iAdd, iy) * coeff3
                        ! this%hosMesh_%lDvDt(iy) =  this%hosMesh_%lDvDt(iy) &
                        !                             - this%hosMode_%modeAddt(iAdd, iy) * coeff4
                        ! this%hosMesh_%lDwDt(iy) =  this%hosMesh_%lDwDt(iy) &
                        !                             - this%hosMode_%modeAddt(iAdd, iy) * coeff2
                    enddo
                enddo

                this%hosMesh_%nonDimAddPhiX(ix, :, iz) = this%hosNWTFFTW_%fourier_2_space_y(this%hosMesh_%lVitX,'cos')
                this%hosMesh_%nonDimAddPhiY(ix, :, iz) = this%hosNWTFFTW_%fourier_2_space_y(this%hosMesh_%lVitY,'sin')
                this%hosMesh_%nonDimAddPhiZ(ix, :, iz) = this%hosNWTFFTW_%fourier_2_space_y(this%hosMesh_%lVitZ,'cos')
                this%hosMesh_%nonDimAddPhit(ix, :, iz) = this%hosNWTFFTW_%fourier_2_space_y(this%hosMesh_%lPhit,'cos')

                ! this%hosMesh_%nonDimAddDuDt(ix, :, iz) = this%hosNWTFFTW_%fourier_2_space_y(this%hosMesh_%lDuDt,'cos')
                ! this%hosMesh_%nonDimAddDvDt(ix, :, iz) = this%hosNWTFFTW_%fourier_2_space_y(this%hosMesh_%lDvDt,'sin')
                ! this%hosMesh_%nonDimAddDwDt(ix, :, iz) = this%hosNWTFFTW_%fourier_2_space_y(this%hosMesh_%lDwDt,'cos')
            enddo

        enddo

    end subroutine

    !!!- Build HOS Mesh
    subroutine buildHOSNWTMesh(this, nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax, zMinRatio, zMaxRatio)
        implicit none
        class(typHOSNWTMesh), intent(inout) :: this
        real(rp),intent(in)      :: nonXLen, nonYLen, nonDimzMin, nonDimzMax
        integer,intent(in)       :: nX, nY, nZmin, nZmax
        real(rp),optional        :: zMinRatio, zMaxRatio
        integer :: ix, iy, iz, jz
        !----------------------------------

        !-- Allocate HOS Mesh Array
        Call this%allocateHOSArray(nX, nY, nZmin, nZmax)

        this%nonDimLx = nonXLen
        this%nonDimLy = nonYLen

        this%nonDimZMin = nonDimzMin
        this%nonDimZMax = nonDimzMax

        this%Lx = this%nonDimLx * this%dimL
        this%Ly = this%nonDimLy * this%dimL

        this%zMin = this%nonDimZMin * this%dimL
        this%zMax = this%nonDimZMax * this%dimL

        this%zMinRatio = 3.0_RP
        if (present(zMinRatio).and.(zMinRatio.gt.0.0)) then
            this%zMinRatio = zMinRatio
        end if

        this%zMaxRatio = 3.0_RP
        if (present(zMaxRatio).and.(zMaxRatio.gt.0.0)) then
            this%zMaxRatio = zMaxRatio
        end if

        !-- Check Parameters
        if ( (Nx.le.0).or.(Ny.le.0)) then
            write(*,*) "    [Error] buildHOSNWTMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
            write(*,*) "        nX or nY should be larger than 0 !"
            stop
        end if

        if ( (nZmin.le.1).or.(nZmax.le.1)) then
            write(*,*) "    [Error] buildHOSNWTMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
            write(*,*) "        nZmin or nZmax should be larger than 0 !"
            stop
        end if

        if ( (nonXLen.le.0).or.(nonYLen.le.0)) then
            write(*,*) "    [Error] buildHOSNWTMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
            write(*,*) "        nonXLen or nonYLen should be larger than 0 !"
            stop
        end if

        if ( (-nonDimzMin.le.0).or.(nonDimzMax.le.0)) then
            write(*,*) "    [Error] buildHOSNWTMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
            write(*,*) "        -nonDimzMin or nonDimzMax should be larger than 0 !"
            stop
        end if

        !!- delta x
        this%nonDimDx = this%nonDimLx / (this%nX - 1.d0)

        !! x node position
        do ix = 1,this%nX
            this%nonDimX(ix) = (ix - 1.d0) * this%nonDimDx
        enddo

        !!- delta y
        if (this%nY == 1) then
            this%nonDimDy = 0.d0
        else
            this%nonDimDy = this%nonDimLy / (this%nY - 1.d0)
        endif

        !! y node position
        do iy = 1,this%nY
            this%nonDimY(iy) = (iy - 1.d0) * this%nonDimDy
        enddo

        !! z node position
        Call buildZmesh(this%nonDimZMin, &
                        this%nonDimZMax, &
                        this%nZmin, &
                        this%nZmax, &
                        this%nonDimZ, &
                        INDEX_GEOMETRICRATIO_MESH, &
                        this%zMinRatio, &
                        this%zMaxRatio)

    end subroutine

!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
