!-----------------------------------------------------------------------
Module  modVol2Vol
!-----------------------------------------------------------------------
!
!   Wrapper module of HOS (vol2vol)
!
!-----------------------------------------------------------------------
!
!   Copyright (C) 2017 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598
!
!   This program based on Post Processing Part of HOS.
!
!-----------------------------------------------------------------------
!
!       vol2vol module written by YoungMyung Choi.
!
!-----------------------------------------------------------------------
use modGrid2GridType
use modSurf2vol
use modV2VSplineInterp

Implicit none

    type :: typHOSVol2Vol

        private

            !!- Solver Name (NWT or Ocean)
            character(len=StringLength) :: HOSSolver_

            !!- Initialization logical Value
            Logical, public :: isInitialized_ = .FALSE.

            !!- Interpolation Order (2 : Linear, 3 : Quadratic , 4 : Cubic, ... )
            integer :: nInterp_ = 4

            !!- Save n-time step for intepolation ( nSaveT >= nInterp_ + 1 )
            integer :: nSaveT_  = 6

            !!- Band of interpolation
            integer :: nBand_

            !!- HOS Surf2Vol variables
            type(typHOSSurf2Vol),allocatable :: HOSs2v_(:)

            !!- HOS correct Index
            integer,allocatable              :: hosCorrectIdx_(:)

            !!- HOS Pointer Index
            integer,allocatable              :: hosPointerIdx_(:)

            !!- HOS Update Logical
            logical,allocatable              :: hosUpdate_(:)

            !!- Correction Time
            real(rp) :: correctTime_

            !!- Correct Time Index
            integer  :: correctIdx_

            !!- HOS 2D bool
            logical,public  :: isHOS2D_

            !!- HOS delta t and end Time (dimensionalzed)
            real(rp),public :: dt_, endTime_

            !!- end Time Step of HOS
            integer  :: nTime_

            !!- nonDimensional parameter of HOS
            real(rp),public :: dimL_

            integer         :: nX_, nY_

            !!- Domain Size
            real(rp),public :: Lx_, Ly_

            !!- Domain Size
            real(rp),public :: nonDimLx_, nonDimLy_

            !!- Domain Size
            real(rp),public :: zMin_, zMax_

            !!- Domain Size
            real(rp),public :: nonDimZMin_, nonDimZMax_

            !!- Water depth
            real(rp),public :: waterDepth_

            !!- 2D interpolation Class
            type(typV2VInterp2D) :: itp2D_

            !!- 3D interpolation Class
            type(typV2VInterp3D) :: itp3D_

    contains

        !!- Update HOS Surf2Vol and Index
        procedure, pass, private :: updateHOS

        !!- Construct Spline Interpolation Data
        procedure, pass, private :: constructSpline

        !!- Check non-dimensional X, Y position
        procedure, pass, private :: checkHOSXY

        !!- Check non-dimensional Z position
        procedure, pass, private :: checkHOSZ

        !!- Initialize HOS Vol2Vol
        procedure, pass, public :: initialize

        !!- Correct HOS Vol2Vol
        procedure, pass, public :: correct

        !!- Get interpolated Eta
        procedure, pass, public :: getEta

        !!- Get interpolated U
        procedure, pass, public :: getU

        !!- Get interpolated Pd
        procedure, pass, public :: getPd

        !!- Get interpolated Flow Information (eta, u, pd)
        procedure, pass, public :: getFlow

    end type

contains

    SUBROUTINE initialize(this, solver, fileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio, iflag)
        implicit none
        class(typHOSVol2Vol), intent(inout) :: this
        character(len=*), intent(in)     :: solver, fileName
        real(RP),intent(in)              :: zMin, zMax
        integer,intent(in)               :: nZmin, nZmax
        real(rp),optional, intent(in)    :: zMinRatio, zMaxRatio
        logical,optional, intent(in)     :: iflag
        !! Local Variables
        integer :: it
        real(RP),allocatable :: interpX(:), interpY(:), interpZ(:)
        !!!--------------------------------------------------------------------

        if (this%isInitialized_) return

        allocate(this%HOSs2v_(this%nSaveT_))
        allocate(this%hosCorrectIdx_(this%nSaveT_))
        allocate(this%hosPointerIdx_(this%nSaveT_))
        allocate(this%hosUpdate_(this%nSaveT_))

        this%HOSSolver_    = solver
        this%hosCorrectIdx_ = -1
        this%hosPointerIdx_ = -1
        this%hosUpdate_     = .TRUE.

        !!- Solver Check (HOS Ocean or HOS NWT)
        if ((trim(solver).ne."NWT").and.(trim(solver).ne."Ocean")) then
            write(*,*) "    [ERROR] Grid2Grid, typHOSVol2Vol::initialize(solver, fileName, zMin, zMax, nZmin, nZmax)"
            write(*,*) "        Wrong solver is given when HOS vol2Vol initialize"
            write(*,*) "        given solver : ", solver
        end if

        do it = 1, this%nSaveT_
            Call this%HOSs2v_(it)%initialize(solver, fileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio)
            this%hosCorrectIdx_(it) = this%HOSs2v_(it)%correctIdx_
        enddo

        this%correctTime_ = -1e6
        this%correctIdx_  = -1
        this%dt_          = this%HOSs2v_(1)%dt
        this%endTime_     = this%HOSs2v_(1)%endTime
        this%nTime_       = int(this%endTime_ / this%dt_) + 1
        this%waterDepth_  = this%HOSs2v_(1)%waterDepth_
        this%nBand_       = floor(this%nSaveT_ / 2.0)
        this%dimL_        = this%HOSs2v_(1)%ptrHOSMesh_%dimL

        this%nX_   = this%HOSs2v_(1)%ptrHOSMesh_%nX
        this%nY_   = this%HOSs2v_(1)%ptrHOSMesh_%nY

        this%Lx_   = this%HOSs2v_(1)%ptrHOSMesh_%Lx
        this%Ly_   = this%HOSs2v_(1)%ptrHOSMesh_%Ly

        this%nonDimLx_   = this%HOSs2v_(1)%ptrHOSMesh_%nonDimLx
        this%nonDimLy_   = this%HOSs2v_(1)%ptrHOSMesh_%nonDimLy

        this%zMin_ = zMin
        this%zMax_ = zMax

        this%nonDimZMin_ = this%HOSs2v_(1)%ptrHOSMesh_%nonDimZmin
        this%nonDimZMax_ = this%HOSs2v_(1)%ptrHOSMesh_%nonDimZmax

        if (this%HOSs2v_(1)%ptrHOSMesh_%nY.eq.1) then
            this%isHOS2D_ = .TRUE.
        else
            this%isHOS2D_ = .FALSE.
        endif

        if(this%HOSSolver_.eq."Ocean") then
            !! Allocate Interpolation Data Array
            if (this%isHOS2D_) then
                allocate(interpX(this%HOSs2v_(1)%ptrHOSMesh_%nX + 1))
                interpX(1:this%HOSs2v_(1)%ptrHOSMesh_%nX) = this%HOSs2v_(1)%ptrHOSMesh_%nonDimX
                interpX(1+this%HOSs2v_(1)%ptrHOSMesh_%nX) = this%nonDimLx_

                Call this%itp2D_%allocArray(this%HOSs2v_(1)%ptrHOSMesh_%nX + 1, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nZ, &
                                            this%nSaveT_, &
                                            interpX, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nonDimZ)
            else
                allocate(interpX(this%HOSs2v_(1)%ptrHOSMesh_%nX + 1))
                allocate(interpY(this%HOSs2v_(1)%ptrHOSMesh_%nY + 1))
                interpX(1:this%HOSs2v_(1)%ptrHOSMesh_%nY) = this%HOSs2v_(1)%ptrHOSMesh_%nonDimX
                interpX(1+this%HOSs2v_(1)%ptrHOSMesh_%nX) = this%nonDimLx_

                interpY(1:this%HOSs2v_(1)%ptrHOSMesh_%nY) = this%HOSs2v_(1)%ptrHOSMesh_%nonDimY
                interpY(1+this%HOSs2v_(1)%ptrHOSMesh_%nY) = this%nonDimLy_

                Call this%itp3D_%allocArray(this%HOSs2v_(1)%ptrHOSMesh_%nX + 1, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nY + 1, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nZ, &
                                            this%nSaveT_, &
                                            interpX, &
                                            interpY, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nonDimZ)
            end if
        else
            !! Allocate Interpolation Data Array
            if (this%isHOS2D_) then
                Call this%itp2D_%allocArray(this%HOSs2v_(1)%ptrHOSMesh_%nX, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nZ, &
                                            this%nSaveT_, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nonDimX, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nonDimZ)
            else
                Call this%itp3D_%allocArray(this%HOSs2v_(1)%ptrHOSMesh_%nX, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nY, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nZ, &
                                            this%nSaveT_, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nonDimX, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nonDimY, &
                                            this%HOSs2v_(1)%ptrHOSMesh_%nonDimZ)
            end if
        end if

        Call this%correct(0.0_RP)

        if (present(iflag).and.iflag) then
            write(*,"(a)") ""
            write(*,"(a)") "HOS Wrapper Program GRID2GRID Vol2Vol is initialized."
            write(*,"(a)") ""
            write(*,"(a,a)") " HOS Solver      : ", trim(this%HOSSolver_)
            write(*,"(a,a)") " HOS Result File : ", trim(fileName)
            write(*,"(a)") ""
            write(*,"(a,2f15.6)") " HOS Grid   zMin , zMax        : ", zMin, zMax
            write(*,"(a,2i15)") " HOS Grid   nZmin, nZmax       : ", nZmin, nZmax
            write(*,"(a,2f15.6)") " HOS Grid zMinRatio, zMaxRatio : ", this%HOSs2v_(1)%ptrHOSMesh_%zMinRatio, &
                                                                     this%HOSs2v_(1)%ptrHOSMesh_%zMaxRatio
            write(*,"(a)") ""
        end if

        this%isInitialized_ = .TRUE.

    END SUBROUTINE

    SUBROUTINE correct(this, simulTime)
        implicit none
        class(typHOSVol2Vol), intent(inout) :: this
        real(RP),intent(in)                 :: simulTime
        integer :: iTime

        !!!-----------------------------------------------------------------

        if (simulTime > this%endTime_) then
            write(*,*) "    [ERROR] Grid2Grid  typHOSVol2Vol::correct(simulTime)"
            write(*,*) "        simulTime is over than endTime, simulTime : ", simulTime
            write(*,*) "                                          endTime : ", this%endTime_
            stop
        end if

        !!- Check Simulation Time
        if (abs(this%correctTime_ - simulTime) < convErr) return
        this%correctTime_ = simulTime

        !!- Check Correct and Interpolation Time
        iTime = ceiling(simulTime / this%dt_)
        if (this%correctIdx_.eq.iTime) return

        !!- Set Correct Index not to update again in same time index
        this%correctIdx_ = iTime

        !!- Update HOS surf2vol and Interpolation order
        Call updateHOS(this)

        !!- Construct Spline Interpolation Data Structure
        Call constructSpline(this)

    END SUBROUTINE

    REAL(RP) FUNCTION getEta(this, x, y, t, iflag)
        implicit none
        class(typHOSVol2Vol), intent(inout) :: this
        real(RP),intent(in) :: x, y, t
        real(RP)            :: nonDimX, nonDimY
        integer,optional    :: iflag
        !------------------------------------------------------
        if(present(iflag).and.(iflag.eq.1)) then
            nonDimX = x
            nonDimY = y
            if (this%isHOS2D_) nonDimY = 0.0_RP
        else
            nonDimX = x / this%dimL_
            nonDimY = y / this%dimL_
            if (this%isHOS2D_) nonDimY = 0.0_RP
        endif

        !!! Coordinate Check
        Call checkHOSXY(this, nonDimX, nonDimY)

        if (this%isHOS2D_) then
            getEta = this%itp2D_%interpEta(nonDimX, t)
        else
            getEta = this%itp3D_%interpEta(nonDimX, nonDimY, t)
        end if

    END FUNCTION

    SUBROUTINE getU(this, x, y, z, t, u, v, w, iflag)
        implicit none
        class(typHOSVol2Vol), intent(inout) :: this
        real(RP),intent(in) :: x, y, z, t
        real(RP),intent(out) :: u, v, w
        real(RP) :: nonDimX, nonDimY, nonDimZ
        real(RP) :: etaVal
        integer,optional    :: iflag
        !------------------------------------------------------
        if(present(iflag).and.(iflag.eq.1)) then
            nonDimX = x
            nonDimY = y
            nonDimZ = z
            if (this%isHOS2D_) nonDimY = 0.0_RP
        else
            nonDimX = x / this%dimL_
            nonDimY = y / this%dimL_
            nonDimZ = z / this%dimL_
            if (this%isHOS2D_) nonDimY = 0.0_RP
        endif

        !!! Coordinate Check
        Call checkHOSXY(this, nonDimX, nonDimY)

        if (this%isHOS2D_) then
            etaVal = this%itp2D_%interpEta(nonDimX,t)
        else
            etaVal = this%itp3D_%interpEta(nonDimX, nonDimY,t)
        end if

        if(present(iflag).and.(iflag.eq.1)) then
            if (abs (nonDimZ * this%dimL_ - etaVal).le.convErr) nonDimZ = etaVal / this%dimL_
            if (nonDimZ.gt.etaVal / this%dimL_) then
                u = 0.0_RP
                v = 0.0_RP
                w = 0.0_RP
            else
                Call checkHOSZ(this, nonDimZ)
                if (this%isHOS2D_) then
                    v = 0.0_RP
                    Call this%itp2D_%interpU(nonDimX, nonDimZ, t, u, w)
                else
                    Call this%itp3D_%interpU(nonDimX, nonDimY, nonDimZ, t, u, v, w)
                end if

            endif
        else
            if (abs (nonDimZ * this%dimL_ - etaVal).le.convErr) nonDimZ = etaVal / this%dimL_
            if (nonDimZ.gt.etaVal / this%dimL_) then
                u = 0.0_RP
                v = 0.0_RP
                w = 0.0_RP
            else
                Call checkHOSZ(this, nonDimZ)
                if (this%isHOS2D_) then
                    v = 0.0_RP
                    Call this%itp2D_%interpU(nonDimX, nonDimZ, t, u, w)
                else
                    Call this%itp3D_%interpU(nonDimX, nonDimY, nonDimZ, t, u, v, w)
                end if
            endif
        end if

    END SUBROUTINE

    REAL(RP) FUNCTION getPd(this, x, y, z, t, iflag)
        implicit none
        class(typHOSVol2Vol), intent(inout) :: this
        real(RP), intent(in) :: x, y, z, t
        real(RP) :: nonDimX, nonDimY, nonDimZ
        real(RP) :: etaVal
        integer,optional    :: iflag
        !! Nondimensional HOS Coordinates
        if(present(iflag).and.(iflag.eq.1)) then
            nonDimX = x
            nonDimY = y
            nonDimZ = z
            if (this%isHOS2D_) nonDimY = 0.0_RP
        else
            nonDimX = x / this%dimL_
            nonDimY = y / this%dimL_
            nonDimZ = z / this%dimL_
            if (this%isHOS2D_) nonDimY = 0.0_RP
        endif

        !!! Coordinate Check
        Call checkHOSXY(this, nonDimX, nonDimY)

        if (this%isHOS2D_) then
            etaVal = this%itp2D_%interpEta(nonDimX,t)
        else
            etaVal = this%itp3D_%interpEta(nonDimX, nonDimY,t)
        end if

        if(present(iflag).and.(iflag.eq.1)) then
            if (abs (nonDimZ * this%dimL_ - etaVal).le.convErr) nonDimZ = etaVal / this%dimL_
            if (nonDimZ.gt.etaVal / this%dimL_) then
                getPd = 0.0_RP
            else
                Call checkHOSZ(this, nonDimZ)
                if (this%isHOS2D_) then
                    getPd = this%itp2D_%interpPd(nonDimX, nonDimZ, t)
                else
                    getPd = this%itp3D_%interpPd(nonDimX, nonDimY, nonDimZ, t)
                end if

            endif
        else
            if (abs (nonDimZ * this%dimL_ - etaVal).le.convErr) nonDimZ = etaVal / this%dimL_
            if (nonDimZ.gt.etaVal / this%dimL_) then
                getPd = 0.0_RP
            else
                Call checkHOSZ(this, nonDimZ)
                if (this%isHOS2D_) then
                    getPd = this%itp2D_%interpPd(nonDimX, nonDimZ, t)
                else
                    getPd = this%itp3D_%interpPd(nonDimX, nonDimY, nonDimZ, t)
                end if
            endif
        end if

    END FUNCTION

    SUBROUTINE getFlow(this, x, y, z, t, eta, u, v, w, pd, iflag)
        implicit none
        class(typHOSVol2Vol), intent(inout) :: this
        real(RP),intent(in) :: x, y, z, t
        real(RP),intent(out) :: eta, u, v, w, pd
        real(RP) :: nonDimX, nonDimY, nonDimZ
        integer,optional    :: iflag
        real(rp) :: etaVal
        !! Nondimensional HOS Coordinates
        if(present(iflag).and.(iflag.eq.1)) then
            nonDimX = x
            nonDimY = y
            nonDimZ = z
            if (this%isHOS2D_) nonDimY = 0.0_RP
        else
            nonDimX = x / this%dimL_
            nonDimY = y / this%dimL_
            nonDimZ = z / this%dimL_
            if (this%isHOS2D_) nonDimY = 0.0_RP
        endif

        !!! Coordinate Check
        Call checkHOSXY(this, nonDimX, nonDimY)

        if (this%isHOS2D_) then
            etaVal = this%itp2D_%interpEta(nonDimX,t)
        else
            etaVal = this%itp3D_%interpEta(nonDimX, nonDimY,t)
        end if
        eta = etaVal

        if(present(iflag).and.(iflag.eq.1)) then
            if (abs (nonDimZ * this%dimL_ - etaVal).le.convErr) nonDimZ = etaVal / this%dimL_
            if (nonDimZ.gt.etaVal / this%dimL_) then
                u = 0.0_RP
                v = 0.0_RP
                w = 0.0_RP
                pd = 0.0_RP
            else
                Call checkHOSZ(this, nonDimZ)
                if (this%isHOS2D_) then
                    v = 0.0_RP
                    Call this%itp2D_%interpU(nonDimX, nonDimZ, t, u, w)
                    pd = this%itp2D_%interpPd(nonDimX, nonDimZ, t)
                else
                    Call this%itp3D_%interpU(nonDimX, nonDimY, nonDimZ, t, u, v, w)
                    pd = this%itp3D_%interpPd(nonDimX, nonDimY, nonDimZ, t)
                end if
            endif
        else
            if (abs (nonDimZ * this%dimL_ - etaVal).le.convErr) nonDimZ = etaVal / this%dimL_
            if (nonDimZ.gt.etaVal / this%dimL_) then
                u = 0.0_RP
                v = 0.0_RP
                w = 0.0_RP
                pd = 0.0_RP
            else
                Call checkHOSZ(this, nonDimZ)
                if (this%isHOS2D_) then
                    v = 0.0_RP
                    Call this%itp2D_%interpU(nonDimX, nonDimZ, t, u, w)
                    pd = this%itp2D_%interpPd(nonDimX, nonDimZ, t)
                else
                    Call this%itp3D_%interpU(nonDimX, nonDimY, nonDimZ, t, u, v, w)
                    pd = this%itp3D_%interpPd(nonDimX, nonDimY, nonDimZ, t)
                end if
            endif
        end if

    END SUBROUTINE

    subroutine constructSpline(this)
        implicit none
        class(typHOSVol2Vol), intent(inout) :: this
        integer :: it, idx
        integer :: iflag

        !! Destroy Spline Data
        if (this%isHOS2D_) then
            Call this%itp2D_%destroy()
        else
            Call this%itp3D_%destroy()
        end if

        !! Set Inteprolation Data Structure
        do it = 1,this%nSaveT_
            idx = this%hosPointerIdx_(it)

            if(this%HOSSolver_.eq."Ocean") then
                if (this%isHOS2D_) then
                    this%itp2D_%etaValue(1:this%nX_, it) = this%HOSs2v_(idx)%ptrHOSMesh_%eta(:, 1)
                    this%itp2D_%etaValue(1+this%nX_, it) = this%HOSs2v_(idx)%ptrHOSMesh_%eta(1, 1)

                    this%itp2D_%uValue(1:this%nX_,:, it) = this%HOSs2v_(idx)%ptrHOSMesh_%u(:, 1,  :)
                    this%itp2D_%uValue(1+this%nX_,:, it) = this%HOSs2v_(idx)%ptrHOSMesh_%u(1, 1,  :)

                    this%itp2D_%wValue(1:this%nX_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%w(:, 1,  :)
                    this%itp2D_%wValue(1+this%nX_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%w(1, 1,  :)

                    this%itp2D_%pdValue(1:this%nX_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%pd(:, 1,  :)
                    this%itp2D_%pdValue(1+this%nX_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%pd(1, 1,  :)
                else
                    this%itp3D_%etaValue(1:this%nX_, 1:this%nY_, it)  = this%HOSs2v_(idx)%ptrHOSMesh_%eta
                    this%itp3D_%etaValue(1+this%nX_, : , it)          = this%HOSs2v_(idx)%ptrHOSMesh_%eta(1, :)
                    this%itp3D_%etaValue(:, 1+this%nY_ , it)          = this%HOSs2v_(idx)%ptrHOSMesh_%eta(:, 1)
                    this%itp3D_%etaValue(1+this%nX_, 1+this%nY_, it)  = this%HOSs2v_(idx)%ptrHOSMesh_%eta(1, 1)

                    this%itp3D_%uValue(1:this%nX_, 1:this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%u
                    this%itp3D_%uValue(1+this%nX_, 1:this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%u(1,:,:)
                    this%itp3D_%uValue(1:this%nX_, 1+this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%u(:,1,:)
                    this%itp3D_%uValue(1+this%nX_, 1+this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%u(1,1,:)

                    this%itp3D_%vValue(1:this%nX_, 1:this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%v
                    this%itp3D_%vValue(1+this%nX_, 1:this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%v(1,:,:)
                    this%itp3D_%vValue(1:this%nX_, 1+this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%v(:,1,:)
                    this%itp3D_%vValue(1+this%nX_, 1+this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%v(1,1,:)

                    this%itp3D_%wValue(1:this%nX_, 1:this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%w
                    this%itp3D_%wValue(1+this%nX_, 1:this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%w(1,:,:)
                    this%itp3D_%wValue(1:this%nX_, 1+this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%w(:,1,:)
                    this%itp3D_%wValue(1+this%nX_, 1+this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%w(1,1,:)

                    this%itp3D_%pdValue(1:this%nX_, 1:this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%pd
                    this%itp3D_%pdValue(1+this%nX_, 1:this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%pd(1,:,:)
                    this%itp3D_%pdValue(1:this%nX_, 1+this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%pd(:,1,:)
                    this%itp3D_%pdValue(1+this%nX_, 1+this%nY_, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%pd(1,1,:)
                end if
            else
                if (this%isHOS2D_) then
                    this%itp2D_%etaValue(:, it)   = this%HOSs2v_(idx)%ptrHOSMesh_%eta(:, 1)
                    this%itp2D_%uValue(:, :, it)  = this%HOSs2v_(idx)%ptrHOSMesh_%u(:, 1,  :)
                    this%itp2D_%wValue(:, :, it)  = this%HOSs2v_(idx)%ptrHOSMesh_%w(:, 1,  :)
                    this%itp2D_%pdValue(:, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%pd(:, 1,  :)
                else
                    this%itp3D_%etaValue(:, :, it)  = this%HOSs2v_(idx)%ptrHOSMesh_%eta
                    this%itp3D_%uValue(:, :, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%u
                    this%itp3D_%vValue(:, :, :, it) = this%HOSs2v_(idx)%ptrHOSMesh_%v
                    this%itp3D_%wValue(:, :, :,it)  = this%HOSs2v_(idx)%ptrHOSMesh_%w
                    this%itp3D_%pdValue(:, :, :,it) = this%HOSs2v_(idx)%ptrHOSMesh_%pd
                end if
            end if

        enddo

        if (this%isHOS2D_) then
            Call this%itp2D_%initialize(this%nInterp_, this%nInterp_, this%nInterp_)
        else
            Call this%itp3D_%initialize(this%nInterp_, this%nInterp_, this%nInterp_ , this%nInterp_)
        end if

    end subroutine

    subroutine updateHOS(this)
        implicit none
        class(typHOSVol2Vol), intent(inout) :: this
        integer :: iTimeLeft
        integer :: it, idxLeft, jt

        !! Interpolation Time Index
        idxLeft = 0
        this%hosUpdate_ = .FALSE.

        iTimeLeft = this%correctIdx_ - this%nBand_

        !!- Set variables for left ends of time spline (start of simulation)
        if (iTimeLeft.le.0) then
            iTimeLeft = 0
        end if

        !!- Set variables for right ends of time spline (end of simulation)
        if (iTimeLeft.ge.this%nTime_ - this%nSaveT_ + 1) then
            iTimeLeft = this%nTime_ - this%nSaveT_ + 1
        end if

        !!- Search time index in saved data structure
        do it = 1, this%nSaveT_
            if (this%hosCorrectIdx_(it).eq.iTimeLeft) then
                idxLeft = it           ! Revoling Left Index
                exit
            endif
        enddo

        !! Revolving
        if (idxLeft.eq.0) then
            do it = 1, this%nSaveT_
                this%hosCorrectIdx_(it) = iTimeLeft - 1 + it
                this%hosUpdate_(it)     = .TRUE.
                this%hosPointerIdx_(it) = it
                if (this%isHOS2D_) then
                    this%itp2D_%tArg(it) = this%hosCorrectIdx_(it) * this%dt_
                else
                    this%itp3D_%tArg(it) = this%hosCorrectIdx_(it) * this%dt_
                end if
            enddo
        else
            do it = 1,this%nSaveT_
                jt = idxLeft - 1 + it
                if (jt.gt.this%nSaveT_) then
                      jt = jt - this%nSaveT_
                end if
                if (this%hosCorrectIdx_(jt).ne.iTimeLeft - 1 + it) then
                    this%hosCorrectIdx_(jt) = iTimeLeft - 1 + it
                    this%hosUpdate_(jt)     = .TRUE.
                end if
                this%hosPointerIdx_(it) = jt
            enddo

            do it = 1, this%nSaveT_
                if (this%isHOS2D_) then
                    this%itp2D_%tArg(it) = (iTimeLeft - 1 + it) * this%dt_
                else
                    this%itp3D_%tArg(it) = (iTimeLeft - 1 + it) * this%dt_
                end if
            enddo

        end if

        !! Correct surf2vol
        do it = 1, this%nSaveT_
            if (this%hosUpdate_(it)) then
                Call this%HOSs2v_(it)%correct( this%hosCorrectIdx_(it) )
            end if
            !!.. Just to check Index is correct or not
            this%hosCorrectIdx_(it) = this%HOSs2v_(it)%correctIdx_
        enddo

    end subroutine

    !! Check HOS nondimensional position
    subroutine checkHOSXY(this, nonDimX, nonDimY)
        implicit none
        class(typHOSVol2Vol), intent(inout) :: this
        real(RP),intent(in)  :: nonDimX, nonDimY

        !! X - check
        if ( (nonDimX.lt.0.0).or.(nonDimX.gt.this%nonDimLx_) )then
            write(*,*) "    [Error] typHOSVol2Vol::checkHOSXY(x, y) "
            write(*,*) "        given x - position is not in HOS mesh "
            write(*,*) "        given (x, y) : ", nonDimX * this%dimL_, &
                                                  nonDimY * this%dimL_
            write(*,*) "        Lx, Ly       : ", this%Lx_, this%Ly_
        end if

        !! Y - check
        if ( (nonDimY.lt.0.0).or.(nonDimY.gt.this%nonDimLy_) )then
            write(*,*) "    [Error] typHOSVol2Vol::checkHOSXY(x, y) "
            write(*,*) "        given y - position is not in HOS mesh "
            write(*,*) "        given (x, y) : ", nonDimX * this%dimL_, &
                                                     nonDimY * this%dimL_
            write(*,*) "        Lx, Ly       : ", this%Lx_, this%Ly_
        end if

    end subroutine

    subroutine checkHOSZ(this,  nonDimZ)
        implicit none
        class(typHOSVol2Vol), intent(inout) :: this
        real(RP),intent(inout)  :: nonDimZ

        if (abs(nonDimZ - this%nonDimZMin_).le.convErr ) then
            nonDimZ = this%nonDimZMin_
        end if

        if (abs(nonDimZ - this%nonDimZMax_).le.convErr ) then
            nonDimZ = this%nonDimZMax_
        end if

        !! Z - check
        if ( (nonDimZ.lt.this%nonDimZMin_).or.(nonDimZ.gt.this%nonDimZMax_))then
            write(*,*) "    [Error] typHOSVol2Vol::checkHOSZ(z) "
            write(*,*) "        given z - position is not in HOS mesh "
            write(*,*) "        given z    : ", nonDimZ * this%dimL_
            write(*,*) "        zMin, zMax : ", this%zMin_, this%zMax_
        end if

    end subroutine

!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
