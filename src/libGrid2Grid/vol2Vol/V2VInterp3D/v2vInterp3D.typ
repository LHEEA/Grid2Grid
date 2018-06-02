type,public :: typV2VInterp3D

    !!- Intepolation Node
    real(wp),allocatable :: xArg(:), yArg(:), zArg(:), tArg(:)

    !!- Interpolation Node Value
    real(wp),allocatable :: etaValue(:,:,:)
    real(wp),allocatable :: detadxValue(:,:,:)
    real(wp),allocatable :: detadyValue(:,:,:)
    real(wp),allocatable :: detadtValue(:,:,:)
    real(wp),allocatable :: uValue(:,:,:,:)
    real(wp),allocatable :: vValue(:,:,:,:)
    real(wp),allocatable :: wValue(:,:,:,:)
    real(wp),allocatable :: pdValue(:,:,:,:)
    real(wp),allocatable :: dudxValue(:,:,:,:)
    real(wp),allocatable :: dvdyValue(:,:,:,:)
    real(wp),allocatable :: dudyValue(:,:,:,:)
    real(wp),allocatable :: dudzValue(:,:,:,:)
    real(wp),allocatable :: dvdzValue(:,:,:,:)

    !!- 3D Spline Type
    type(bspline_3d)     :: spl3eta, spl3detadx, spl3detady, spl3detadt

    !!- 4D Spline Type
    type(bspline_4d)     :: spl4u, spl4v, spl4w, spl4pd, spl4dudx, spl4dvdy, spl4dudy, spl4dudz, spl4dvdz

contains

    procedure, public  :: allocArray    => allocSpl3DArray     ! Allocate Array
    procedure, public  :: destroySplMod => destroySpl3DModule  ! Destroy Spline type
    procedure, public  :: initialize    => initialize3D        ! initialize Spline type

    procedure, public  :: interpEta  => interpEta3D           ! interpolate eta
    procedure, public  :: interpdEta => interpdEta3D          ! interpolate eta derivative
    procedure, public  :: interpU    => interpU3D             ! interpolate velocity
    procedure, public  :: interpdU   => interpdU3D            ! interpolate velocity derivative
    procedure, public  :: interpPd   => interpPd3D            ! interpolate dynmic pressure

    ! Destroy and Deallocate interp3D
    procedure, public  :: destroy => destroySpl3D

    ! Destroyer
    final              :: finalSpl3D

end type
