type, public :: typHOSVol2Vol

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

    !!- Destroy Vol2Vol
    procedure, pass, public :: destroy => destroyVol2Vol

    !!- Destroyer (Disabled. gfortran is unstable. YM. 2018/03/16)
    !final                   :: finalVol2Vol

end type
