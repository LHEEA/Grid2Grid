type,public :: typV2VInterp3D

    !!- Intepolation Node
    real(wp),allocatable :: xArg(:), yArg(:), zArg(:), tArg(:)

    !!- Interpolation Node Value
    real(wp),allocatable :: etaValue(:,:,:)
    real(wp),allocatable :: uValue(:,:,:,:)
    real(wp),allocatable :: vValue(:,:,:,:)
    real(wp),allocatable :: wValue(:,:,:,:)
    real(wp),allocatable :: pdValue(:,:,:,:)

    !!- 3D Spline Type
    type(bspline_3d)     :: spl3eta

    !!- 4D Spline Type
    type(bspline_4d)     :: spl4u, spl4v, spl4w, spl4pd

contains

    procedure, public  :: allocArray    => allocSpl3DArray     ! Allocate Array
    procedure, public  :: destroySplMod => destroySpl3DModule  ! Destroy Spline type
    procedure, public  :: initialize    => initialize3D        ! initialize Spline type

    procedure, public  :: interpEta  => interpEta3D         ! interpolate eta
    procedure, public  :: interpU    => interpU3D           ! interpolate velocity
    procedure, public  :: interpPd   => interpPd3D          ! interpolate dynmic pressure

    ! Destroy and Deallocate interp3D
    procedure, public  :: destroy => destroySpl3D

    ! Destroyer
    final              :: finalSpl3D

end type
