!-----------------------------------------------------------------------
Module  modOceansurf2vol
!-----------------------------------------------------------------------
!
!   Wrapper module of HOS Ocean (surf2vol)
!       It construct HOS Ocean global mesh and calculate flow quantities
!       on HOS Ocean global mesh. To calculate quantities of arbitrary
!       position, vol2vol module is needed.
!
!-----------------------------------------------------------------------
!
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
use modFourier_r2c_FFTW3_ocean
Implicit none
!!! module variables

private

    !!- HOS Ocean mode array type
    type,private :: typHOSOceanMode

        integer :: nXo2p1, nYmode

        !!- Mode amplitudes
        complex(cp),allocatable :: modeX(:,:), modeY(:,:), modeZ(:,:)
        complex(cp),allocatable :: modet(:,:), modeFS(:,:), modeFSt(:,:)

        !!- Wave Numbers
        real(rp), allocatable :: kx(:), kyN2(:), ktheta(:,:), kxy(:,:)
        complex(cp), allocatable :: ikx(:,:), iky(:,:)

        contains

            !!- Allocate Dynamic Array
            procedure, pass :: allocOceanMode

            !!- Deallocate Dynamic Array of HOS Ocean Mode
            procedure, pass :: destroy => destroyOceanMode

            !!- Destroyer
            final           :: finalHOSOceanMode

    end type

    type,extends(typHOSMesh), public :: typHOSOceanMesh
        private

            !!- general mode flow quantities in fourier domain (fixed z)
            complex(cp),allocatable, private :: aVitX(:,:), aVitY(:,:), aVitZ(:,:), aPhit(:,:)
            complex(cp),allocatable, private :: aDuDt(:,:), aDvDt(:,:), aDwDt(:,:)

        contains

            !!- Build HOS Ocean Mesh
            procedure, pass, private :: buildHOSOceanMesh

            !!- Deallocate Dynamic Array of HOS Ocean Mesh
            procedure, pass          :: destroyOceanMesh

            !!- Destroyer
            final                    :: finalHOSOceanMesh

    end type

    !!- HOS Ocean Type
    type, public :: typHOSOcean

    !!! Private -------------------------------------------------------
    private

        !!- HOS Ocean file IO
        type(typFileIO) :: hosFile_

        !!- Initialization logical Value
        Logical :: isInitialized_ = .FALSE.

        !!- Simulation Parameters of HOS Ocean
        integer  :: nXmode_, nYmode_
        integer  :: nXo2p1_, nYo2p1_

        logical  :: isXeven_,isYeven_

        real(rp),public :: dtOut_, Tstop_

        real(rp)        :: zMin_, zMax_
        real(rp)        :: xLen_, yLen_

        real(rp)        :: nonDimxLen_, nonDimyLen_
        real(rp)        :: nonDimzMin_, nonDimzMax_

        real(rp),public :: depth_
        real(rp),public :: nonDimDepth_
        integer         :: nHOSTime_
        real(rp)        :: gravi, dimL_, dimT_

        !!- Correct time index
        integer  :: iReadTimeIndex_

        !!- HOS Ocean mode array (updated by calling read_mod subroutine)
        type(typHOSOceanMode) :: hosMode_

        !!- HOS Ocean FFTW Class
        type(typFFFWHosOcean) :: hosOceanFFTW_

        !!! Public --------------------------------------------------------

        !!- HOS Ocean Global Grid
        type(typHOSOceanMesh),public :: hosMesh_

    contains

        !!! Private -------------------------------------------------------

        !!- initialize simulation parameter
        procedure, pass, private :: init_read_mod

        !!- read HOS Ocean mode
        procedure, pass, private :: read_mod

        !!- build global mesh and wave numbers
        procedure, pass, private :: buildGlobalMesh

        !!- reconstruct flow quantities (gradient, time derivatives of potential)
        procedure, pass, private :: reconstructionFFTs

        !!! Public  -------------------------------------------------------

        !!- initialize HOS NWT Wrapper
        procedure :: initialize => initHOSOcean

        !!- read and compute flow
        procedure :: correct    => correctHOSOcean

        !!- Destroy HOS Ocean Surf2Vol
        procedure :: destroy    => destroyHOSOcean

    end type

!!! ***************************************************************
!!! HOS Ocean subroutines
!!! ***************************************************************

contains

    !!!- initialize HOS Ocean reading Module
    SUBROUTINE initHOSOcean(this, fileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio)
        !---------------------------------------------------------
        !   Input :
        !       fileName : file path of HOS Ocean mode file
        !       zMin     : Z min
        !       zMax     : Z max
        !       nZmin    : number of HOS z-directional mesh ( z < 0 )
        !       nZmax    : number of HOS z-directional mesh ( z > 0 )
        !---------------------------------------------------------
        implicit none
        class(typHOSOcean), intent(inout) :: this
        character(len=*),intent(in) :: fileName
        real(rp),intent(in) :: zMin, zMax
        integer, intent(in) :: nZmin, nZmax
        real(rp), optional  :: zMinRatio, zMaxRatio
        !!!......................................

        if (this%isInitialized_) return

        !! - Set file name and file unit
        this%hosFile_%name = fileName
        this%hosFile_%unit = callFileUnit()

        ! Read and initalize HOS simulation parameter
        Call init_read_mod(this)

        ! Build Mesh and Calculate Wave Numbers
        Call buildGlobalMesh(this, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio)

        ! Initialize Fourier transforms (FFTW)
        Call this%hosOceanFFTW_%initialize(this%nXmode_, this%nYmode_)

        ! Read HOS Ocean mode for first time step
        Call correctHOSOcean(this,0)

        this%isInitialized_ = .TRUE.

    END SUBROUTINE

    SUBROUTINE correctHOSOcean(this, iTime)
        Implicit none

        class(typHOSOcean), intent(inout) :: this
        Integer, intent(in) :: iTime

        !! - Check time index
        if (iTime.eq.this%iReadTimeIndex_ ) return

        !! - Update time index
        this%iReadTimeIndex_ = iTime

        !! - Read HOS NWT mode for given time index
        Call read_mod(this,iTime)

        !! - Construct General Mode Flow quantities by FFT
        Call reconstructionFFTs(this)

        Call this%hosMesh_%calculDimValue()

    END SUBROUTINE

    SUBROUTINE destroyHOSOcean(this)
        Implicit none
        class(typHOSOcean), intent(inout) :: this
        this%isInitialized_ = .FALSE.
        Call this%hosMode_%destroy()
        Call this%hosMesh_%destroyOceanMesh()
        Call this%hosOceanFFTW_%destroy()
    END SUBROUTINE

    !!!- Initialize data from volumic mode description generated by HOS-Ocean
    subroutine init_read_mod(this)
        implicit none
        class(typHOSOcean), intent(inout) :: this
        REAL(RP) :: x1, x2
        !!!......................................
        ! We will look at first ten variables written on 18 characters
        OPEN(unit = this%hosFile_%unit, &
             file = this%hosFile_%name, &
             status='OLD', &
             form='FORMATTED', &
             access='DIRECT', &
             recl=18*10)

        ! Read HOS Ocean simulation parameters
        READ(this%hosFile_%unit,'(10(ES17.10,1X))',rec=1) x1, x2, &
                                                    this%dtOut_, this%Tstop_, &
                                                    this%nonDimxLen_, this%nonDimyLen_, this%nonDimDepth_, &
                                                    this%gravi, this%dimL_, this%dimT_

        CLOSE(this%hosFile_%unit) ! close HOS Ocean file

        IF ( abs(this%dimT_ - 0.0_RP) < tiny ) then
            write(*,*) "[ERROR] typHOSOcean::init_read_mod()"
            write(*,*) " "
            write(*,*) "    Input file is not HOS Ocean Result File."
            write(*,*) " "
            stop
        ENDIF

        ! Set Simulation Parameters
        this%nXmode_   = nint(x1)
        this%nYmode_   = nint(x2)
        this%nHOSTime_ = nint(this%Tstop_ / this%dtOut_)

        this%Tstop_    = this%Tstop_ * this%dimT_
        this%dtOut_    = this%dtOut_ * this%dimT_

        this%nXo2p1_   = this%nXmode_ / 2 + 1
        this%nYo2p1_   = this%nYmode_ / 2 + 1

        this%isXeven_ = (MOD(this%nXmode_,2) == 0)
        this%isYeven_ = (MOD(this%nYmode_,2) == 0)

        this%xLen_  = this%nonDimxLen_ * this%dimL_
        this%yLen_  = this%nonDimyLen_ * this%dimL_
        this%depth_ = this%nonDimDepth_ * this%dimL_

        ! Reading Time Index
        this%iReadTimeIndex_ = -1

        ! set size of HOS Ocean mode array
        Call this%hosMode_%allocOceanMode(this%nXo2p1_, this%nYmode_)

    end subroutine

    subroutine allocOceanMode(this, nXo2p1, nYmode)
        class(typHOSOceanMode), intent(inout) :: this
        integer, intent(in) :: nXo2p1, nYmode

        this%nXo2p1 = nXo2p1
        this%nYmode = nYmode

        allocate(this%modeX(nXo2p1, nYmode),&
                 this%modeY(nXo2p1, nYmode),&
                 this%modeZ(nXo2p1, nYmode),&
                 this%modet(nXo2p1, nYmode),&
                 this%modeFS(nXo2p1, nYmode),&
                 this%modeFSt(nXo2p1, nYmode) )

        !-- Allocate wave number array
        allocate( this%kx(nXo2p1) )
        allocate( this%kyN2(nYmode) )
        allocate( this%kxy(nXo2p1, nYmode) )
        allocate( this%ktheta(nXo2p1, nYmode) )

        allocate( this%ikx(nXo2p1, nYmode) )
        allocate( this%iky(nXo2p1, nYmode) )

    end subroutine

    subroutine destroyOceanMode(this)
        class(typHOSOceanMode), intent(inout) :: this
        if (allocated(this%modeX)) deallocate( this%modeX )
        if (allocated(this%modeY)) deallocate( this%modeY )
        if (allocated(this%modeZ)) deallocate( this%modeZ )

        if (allocated(this%modet))   deallocate( this%modet )
        if (allocated(this%modeFS))  deallocate( this%modeFS )
        if (allocated(this%modeFSt)) deallocate( this%modeFSt )

        if (allocated(this%kx))     deallocate( this%kx )
        if (allocated(this%kyN2))   deallocate( this%kyN2 )
        if (allocated(this%ktheta)) deallocate( this%ktheta )
        if (allocated(this%kxy))    deallocate( this%kxy )

        if (allocated(this%ikx)) deallocate( this%ikx )
        if (allocated(this%iky)) deallocate( this%iky )
    end subroutine

    subroutine finalHOSOceanMode(this)
        type(typHOSOceanMode), intent(inout) :: this
        Call this%destroy
    end subroutine

    !!!- read HOS Ocean mode array from HOS_Ocean results file
    subroutine read_mod(this, iTime)
        ! --------------------------------------------------
        !   Input
        !       iTime : time index
        !           - iTime = 0;            time = 0
        !           - iTime = nHOSTime_;    time = T_stop_
        ! --------------------------------------------------
        implicit none
        class(typHOSOcean), intent(inout) :: this
        integer,intent(in) :: iTime
        integer :: i1,i2, NRECL
        !!!......................................

        ! Time index check
        if (iTime < 0 .or. iTime > this%nHOSTime_) then
            write(*,*) "[ERROR] modGrid2GridType::read_mod(iTime)"
            write(*,*) " "
            write(*,*) "    iTime exceeds total number of simulation step"
            write(*,*) " "
            stop
        endif

        ! HOS Ocean file open
        OPEN(unit = this%hosFile_%unit, &
             file = this%hosFile_%name, &
             status='OLD', &
             form='FORMATTED', &
             access='DIRECT', &
             recl=18*(2 * this%nXo2p1_))

        ! HOS Ocean mode file read
        NRECL = (iTime + 1)*this%nYmode_*6
        do i2=1,this%nYmode_
            READ(this%hosFile_%unit,1001,REC=NRECL+1+6*(i2-1)) (this%hosMode_%modeX(i1,i2), i1=1,this%nXo2p1_)
            READ(this%hosFile_%unit,1001,REC=NRECL+2+6*(i2-1)) (this%hosMode_%modeY(i1,i2), i1=1,this%nXo2p1_)
            READ(this%hosFile_%unit,1001,REC=NRECL+3+6*(i2-1)) (this%hosMode_%modeZ(i1,i2), i1=1,this%nXo2p1_)
            READ(this%hosFile_%unit,1001,REC=NRECL+4+6*(i2-1)) (this%hosMode_%modet(i1,i2), i1=1,this%nXo2p1_)
            READ(this%hosFile_%unit,1001,REC=NRECL+5+6*(i2-1)) (this%hosMode_%modeFS(i1,i2), i1=1,this%nXo2p1_)
            READ(this%hosFile_%unit,1001,REC=NRECL+6+6*(i2-1)) (this%hosMode_%modeFSt(i1,i2), i1=1,this%nXo2p1_)
        end do

        CLOSE(this%hosFile_%unit)    ! file close

        ! file reading format for HOS NWT
        1001 format((5000(ES17.10,1X)))
    end subroutine read_mod

    subroutine buildGlobalMesh(this, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio)
        Implicit none
        class(typHOSOcean), intent(inout) :: this
        real(rp),intent(in) :: zMin,  zMax
        integer, intent(in) :: nZmin, nZmax
        real(rp), optional  :: zMinRatio, zMaxRatio
        Real(RP) :: twoPiXLen, twoPiYLen
        integer  :: nDer(2)
        integer  :: ix, iy
        integer  :: nxFou, nyFou
        real(rp) :: kXY
        !!- Build global mesh -------------------

        this%zMin_ = zMin
        this%zMax_ = zMax

        !.. Truncation Error for zMin
        if (abs(this%zMin_ + this%depth_).le.convErr) then
            this%zMin_ = -this%depth_
        endif

        !-- Check z domain length
        if (-this%zMin_.gt.this%depth_) then
            write(*,*) "    [Error] oceanSurf2Vol::buildGlobalMesh(zMin, zMax, nZmin, nZmax)"
            write(*,*) "        zMin is over than water depth !"
            write(*,*) "        zMin      : ", -this%zMin_
            write(*,*) "        HOS depth : ", this%depth_
            stop
        endif

        this%nonDimzMin_ = this%zMin_ / this%dimL_
        this%nonDimzMax_ = this%zMax_ / this%dimL_

        !-- Build Mesh
        this%hosMesh_%dimL = this%dimL_
        this%hosMesh_%dimT = this%dimT_

        Call this%hosMesh_%buildHOSOceanMesh(this%nonDimxLen_, this%nonDimyLen_, &
                                             this%nonDimzMin_, this%nonDimzMax_, &
                                             this%nXmode_, this%nYmode_, &
                                             nZmin, nZmax, zMinRatio, zMaxRatio)

        !... general mode flow quantities in Fourier Domain
        allocate( this%hosMesh_%aVitX(this%nXo2p1_, this%nYmode_) )
        allocate( this%hosMesh_%aVitY(this%nXo2p1_, this%nYmode_) )
        allocate( this%hosMesh_%aVitZ(this%nXo2p1_, this%nYmode_) )
        allocate( this%hosMesh_%aPhit(this%nXo2p1_, this%nYmode_) )
        allocate( this%hosMesh_%aDuDt(this%nXo2p1_, this%nYmode_) )
        allocate( this%hosMesh_%aDvDt(this%nXo2p1_, this%nYmode_) )
        allocate( this%hosMesh_%aDwDt(this%nXo2p1_, this%nYmode_) )

        !-- Build Wave Number
        ! Specify temporary number of points
        nDer(1) = this%nXo2p1_
        nDer(2) = this%nYo2p1_

        twoPiXLen = TWOPI / this%nonDimxLen_

        if (this%nYmode_.eq.1) then
            twoPiYLen = 0.0_RP
        else
            twoPiYLen = TWOPI / this%nonDimyLen_
        endif

        do ix = 1, this%nXo2p1_
            this%hosMode_%kx(ix) = real(ix - 1,RP) * twoPiXLen
        enddo

        do iy = 1, this%nYo2p1_
            this%hosMode_%kyN2(iy) = real(iy - 1,RP) * twoPiYLen
        enddo

        do iy = 2, this%nYo2p1_
            this%hosMode_%kyN2(this%nYmode_ - iy + 2) = -real(iy - 1,RP) * twoPiYLen
        enddo

        if (this%isYeven_) this%hosMode_%kyN2(this%nYo2p1_) = real(this%nYo2p1_ - 1,RP) * twoPiYLen

        nxFou = MIN(nDer(1), this%nXo2p1_)
        nyFou = MIN(nDer(2), this%nYo2p1_)

        this%hosMode_%ikx = 0.0_CP

        !  x-derivative on n1 points (i.e. n1o2p1 modes)
        do iy = 1, this%nYo2p1_
            this%hosMode_%ikx(1:nxFou, iy ) = iMaginary * this%hosMode_%kx(1:nxFou)
        enddo

        ! Last mode contains cos information only and must not be part of the differentiation.
        if (this%isXeven_) this%hosMode_%ikx(this%nXo2p1_, :) = 0.0_CP

        ! negative kx
        do iy = 2, this%nYo2p1_
            this%hosMode_%ikx(1:nxFou, this%nYmode_ - iy + 2) = iMaginary * this%hosMode_%kx(1:nxFou)
        enddo

        this%hosMode_%iky = 0.0_CP

        ! y-derivative on n1 points (i.e. n1o2p1 modes)
        do ix = 1, this%nXo2p1_
            this%hosMode_%iky(ix, 1:nyFou) = iMaginary * this%hosMode_%kyN2(1:nyFou)
            ! negative ky
            do iy = 2, nyFou
                this%hosMode_%iky(ix, this%nYmode_ - iy + 2) = -iMaginary * this%hosMode_%kyN2(iy)
            enddo
            if (this%isYeven_.and.(nDer(2).ge.this%nYo2p1_)) this%hosMode_%iky(ix, this%nYo2p1_) = 0.0_CP
        enddo

        do iy = 1, this%nYmode_
            do ix = 1, this%nXo2p1_
                kXY = sqrt(this%hosMode_%kx(ix) * this%hosMode_%kx(ix) &
                          +this%hosMode_%kyN2(iy) * this%hosMode_%kyN2(iy))

                this%hosMode_%kXY(ix,iy) = kXY
                this%hosMode_%ktheta(ix,iy) = kXY * tanh(kXY * this%nonDimDepth_)
            enddo
        enddo

    end subroutine

    subroutine buildHOSOceanMesh(this, nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax, zMinRatio, zMaxRatio)
        implicit none
        class(typHOSOceanMesh), intent(inout) :: this
        real(rp),intent(in)  :: nonXLen, nonYLen, nonDimzMin, nonDimzMax
        integer,intent(in)   :: nX, nY, nZmin, nZmax
        real(rp), optional   :: zMinRatio, zMaxRatio
        integer :: ix, iy, iz, jz

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
            write(*,*) "    [Error] buildHOSOceanMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
            write(*,*) "        nX or nY should be larger than 0 !"
            stop
        end if

        if ( (nZmin.le.1).or.(nZmax.le.1)) then
            write(*,*) "    [Error] buildHOSOceanMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
            write(*,*) "        nZmin or nZmax should be larger than 0 !"
            stop
        end if

        if ( (nonXLen.le.0).or.(nonYLen.le.0)) then
            write(*,*) "    [Error] buildHOSOceanMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
            write(*,*) "        nonXLen or nonYLen should be larger than 0 !"
            stop
        end if

        if ( (-nonDimzMin.le.0).or.(nonDimzMax.le.0)) then
            write(*,*) "    [Error] buildHOSOceanMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
            write(*,*) "        -nonDimzMin or nonDimzMax should be larger than 0 !"
            stop
        end if

        !!- delta x
        this%nonDimDx = this%nonDimLx / this%nX

        !! x node position
        do ix = 1,this%nX
            this%nonDimX(ix) = (ix - 1.d0) * this%nonDimDx
        enddo

        !!- delta y
        if (this%nY == 1) then
            this%nonDimDy = 0.0_RP
        else
            this%nonDimDy = this%nonDimLy / this%nY
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

    subroutine destroyOceanMesh(this)
        implicit none
        class(typHOSOceanMesh), intent(inout) :: this
        Call this%destroyHOSMesh
        if (allocated(this%aVitX)) deallocate( this%aVitX )
        if (allocated(this%aVitY)) deallocate( this%aVitY )
        if (allocated(this%aVitZ)) deallocate( this%aVitZ )
        if (allocated(this%aPhit)) deallocate( this%aPhit )

        if (allocated(this%aDuDt)) deallocate( this%aDuDt )
        if (allocated(this%aDvDt)) deallocate( this%aDvDt )
        if (allocated(this%aDwDt)) deallocate( this%aDwDt )
    end subroutine

    subroutine finalHOSOceanMesh(this)
        implicit none
        type(typHOSOceanMesh), intent(inout) :: this
        Call this%destroyOceanMesh
    end subroutine

    subroutine reconstructionFFTs(this)
        implicit none
        class(typHOSOcean), intent(inout) :: this
        integer  :: ix, iy, iz
        real(rp) :: hosZ, eta
        real(rp) :: kZ, kD, coeff, coeff2, k
        !!!......................................

        !! - Compute nondimensionalized wave elevation
        Call this%hosOceanFFTW_%fourier_2_space(this%hosMode_%modeFS,this%hosMesh_%nonDimEta)

        do iz = 1, this%hosMesh_%nZ
            hosZ = this%hosMesh_%nonDimZ(iz)

            ix = 1
            iy = 1

            this%hosMesh_%aVitX(ix, iy) = this%hosMode_%modeX(ix, iy)
            this%hosMesh_%aVitY(ix, iy) = this%hosMode_%modeY(ix, iy)
            this%hosMesh_%aVitZ(ix, iy) = this%hosMode_%modeZ(ix, iy)
            this%hosMesh_%aPhit(ix, iy) = this%hosMode_%modet(ix, iy)

            ! this%hosMesh_%aDuDt(ix, iy) = this%hosMode_%ikx(ix, iy) * this%hosMesh_%aPhit(ix, iy)
            ! this%hosMesh_%aDvDt(ix, iy) = this%hosMode_%iky(ix, iy) * this%hosMesh_%aPhit(ix, iy)
            ! this%hosMesh_%aDwDt(ix, iy) = this%hosMode_%ktheta(ix, iy) * this%hosMode_%modet(ix, iy)

            do iy = 2, this%nYmode_
                k  = this%hosMode_%kXY(ix, iy)
                kZ = k * (hosZ + this%nonDimDepth_ )
                kD = k * this%nonDimDepth_
                if ( (kZ.lt.50.0).and.(kD.le.50.0) ) then
                    ! coeff  = cosh(kZ) / cosh(kD)
                    ! coeff2 = sinh(kZ) / sinh(kD)
                    coeff  = exp(k * hosZ) * (1.0_rp + exp(-2.0_rp*kZ)) / (1.0_rp + exp(-2.0_rp * kD))
                    coeff2 = exp(k * hosZ) * (1.0_rp - exp(-2.0_rp*kZ)) / (1.0_rp - exp(-2.0_rp * kD))
                else
                    coeff  = exp(this%hosMode_%kXY(ix, iy) * hosZ)
                    coeff2 = coeff
                endif

                if (coeff.ge.FNZ_VALUE) then
                    coeff = FNZ_VALUE
                endif

                if (coeff2.ge.FNZ_VALUE) then
                    coeff2 = FNZ_VALUE
                endif

                this%hosMesh_%aVitX(ix, iy) = this%hosMode_%modeX(ix, iy) * coeff
                this%hosMesh_%aVitY(ix, iy) = this%hosMode_%modeY(ix, iy) * coeff
                this%hosMesh_%aVitZ(ix, iy) = this%hosMode_%modeZ(ix, iy) * coeff2
                this%hosMesh_%aPhit(ix, iy) = this%hosMode_%modet(ix, iy) * coeff

                ! this%hosMesh_%aDuDt(ix, iy) = this%hosMode_%ikx(ix, iy) * this%hosMesh_%aPhit(ix, iy)
                ! this%hosMesh_%aDvDt(ix, iy) = this%hosMode_%iky(ix, iy) * this%hosMesh_%aPhit(ix, iy)
                ! this%hosMesh_%aDwDt(ix, iy) = this%hosMode_%ktheta(ix, iy) * this%hosMode_%modet(ix, iy) * coeff2
            enddo

            do ix = 2, this%nXo2p1_
                do iy = 1, this%nYmode_
                    kZ = this%hosMode_%kXY(ix, iy) * (hosZ + this%nonDimDepth_ )
                    kD = this%hosMode_%kXY(ix, iy) * this%nonDimDepth_

                    if ( (kZ.lt.50.0).and.(kD.le.50.0) ) then
                        coeff  = cosh(kZ) / cosh(kD)
                        coeff2 = sinh(kZ) / sinh(kD)
                    else
                        coeff  = exp(this%hosMode_%kXY(ix, iy) * hosZ)
                        coeff2 = coeff
                    endif

                    if (coeff.ge.1000.0_RP) then
                        coeff = 1000.0_RP
                    endif

                    if (coeff2.ge.1000.0_RP) then
                        coeff2 = 1000.0_RP
                    endif

                    this%hosMesh_%aVitX(ix, iy) = this%hosMode_%modeX(ix, iy) * coeff
                    this%hosMesh_%aVitY(ix, iy) = this%hosMode_%modeY(ix, iy) * coeff
                    this%hosMesh_%aVitZ(ix, iy) = this%hosMode_%modeZ(ix, iy) * coeff2
                    this%hosMesh_%aPhit(ix, iy) = this%hosMode_%modet(ix, iy) * coeff

                    ! this%hosMesh_%aDuDt(ix, iy) = this%hosMode_%ikx(ix, iy) * this%hosMesh_%aPhit(ix, iy)
                    ! this%hosMesh_%aDvDt(ix, iy) = this%hosMode_%iky(ix, iy) * this%hosMesh_%aPhit(ix, iy)
                    ! this%hosMesh_%aDwDt(ix, iy) = this%hosMode_%ktheta(ix, iy) * this%hosMode_%modet(ix, iy) * coeff2
                enddo
            enddo

            Call this%hosOceanFFTW_%fourier_2_space(this%hosMesh_%aVitX, this%hosMesh_%nonDimPhiX(:,:, iz) )

            if (this%nYmode_.NE.1) then
                Call this%hosOceanFFTW_%fourier_2_space(this%hosMesh_%aVitY, this%hosMesh_%nonDimPhiY(:,:, iz) )
                ! Call this%hosOceanFFTW_%fourier_2_space( this%hosMesh_%aDvDt, this%hosMesh_%nonDimDvDt(:,:, iz) )
            else
                this%hosMesh_%nonDimPhiY(:,:, iz) = 0.0_rp
                ! this%hosMesh_%nonDimDvDt(:,:, iz) = 0.0_rp
            end if

            Call this%hosOceanFFTW_%fourier_2_space(this%hosMesh_%aVitZ, this%hosMesh_%nonDimPhiZ(:,:, iz) )
            Call this%hosOceanFFTW_%fourier_2_space(this%hosMesh_%aPhit, this%hosMesh_%nonDimPhit(:,:, iz) )

            ! Call this%hosOceanFFTW_%fourier_2_space( this%hosMesh_%aDuDt, this%hosMesh_%nonDimDuDt(:,:, iz) )
            ! Call this%hosOceanFFTW_%fourier_2_space( this%hosMesh_%aDwDt, this%hosMesh_%nonDimDwDt(:,:, iz) )

        enddo

    end subroutine

!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
