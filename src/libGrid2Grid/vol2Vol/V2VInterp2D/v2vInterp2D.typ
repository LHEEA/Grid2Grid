type,public :: typV2VInterp2D

    !!- Intepolation Node
    real(wp),allocatable :: xArg(:), zArg(:), tArg(:)

    !!- Interpolation Node Value
    real(wp),allocatable :: etaValue(:,:)
    real(wp),allocatable :: uValue(:,:,:)
    real(wp),allocatable :: wValue(:,:,:)
    real(wp),allocatable :: pdValue(:,:,:)

    !!- 2D Spline Type (2D Case)
    type(bspline_2d)     :: spl2eta

    !!- 3D Spline Type (2D Case)
    type(bspline_3d)     :: spl3u, spl3v, spl3w, spl3pd

    contains

    procedure, public  :: allocArray => allocSpl2DArray ! Allocate Array
    procedure, public  :: destroySplMod => destroySpl2DModule  ! Destroy Spline type
    procedure, public  :: initialize => initialize2D    ! initialize Spline type

    procedure, public  :: interpEta  => interpEta2D     ! interpolate eta
    procedure, public  :: interpU    => interpU2D       ! interpolate velocity
    procedure, public  :: interpPd   => interpPd2D      ! interpolate dynmic pressure

    ! Destroy and Deallocate interp3D
    procedure, public  :: destroy    => destroySpl2D    ! Destroy Spline type

    ! Destroyer
    final              :: finalSpl2D

end type
