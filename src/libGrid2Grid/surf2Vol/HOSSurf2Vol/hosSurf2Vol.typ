type, public :: typHOSSurf2Vol

    !!! - Variables

    !!- Solver Name (NWT or Ocean)
    character(len=StringLength) :: HOSSolver_

    !!- Initialization logical Value
    Logical :: isInitialized_ = .FALSE.

    !!- HOS NWT surf2Vol Type
    type(typHOSNWT)             :: hosNWTSurf2Vol_

    !!- HOS Ocean surf2Vol Type
    type(typHOSOcean)           :: hosOceanSurf2Vol_

    !!- HOS Mesh Pointer
    class(typHOSMesh),allocatable :: ptrHOSMesh_

    !!- HOS delta t and end Time (dimensionalzed)
    real(rp) :: dt, endTime

    !!- Water Depth
    real(rp) :: waterDepth_

    !!- HOS correct Index
    integer :: correctIdx_ = -1

    contains

    !!! - Subroutine

    !!- Initialize HOS surf2Vol
    procedure, public :: initialize => initHOS

    !!- Correct HOS surf2vol and Point HOS Mesh
    procedure, public :: correct => correctHOS

    !!- Destroy HOS surf2vol
    procedure, public :: destroy => destroySurfVol

    final :: finalSurf2Vol

end type
