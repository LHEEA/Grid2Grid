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