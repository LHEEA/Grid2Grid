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
