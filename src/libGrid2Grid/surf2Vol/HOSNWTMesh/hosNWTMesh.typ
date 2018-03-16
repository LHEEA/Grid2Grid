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
