type, public :: typHOSSurf2Vol

    !!! - Variables

    !!- HOS Input Dictionary
    Type(typDictionaryPtr)      :: dict_

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

    !!- Initialize HOS surf2Vol with arguments (v.1.0)
    procedure, pass, public :: initHOS

    !!- Initialize HOS surf2Vol with dictionary (v.1.0)
    procedure, pass, public :: initHOSSurf2VolDict

    !!- Correct HOS surf2vol and Point HOS Mesh
    procedure, pass, public :: correct => correctHOS

    !!- Destroy HOS surf2vol
    procedure, pass, public :: destroy => destroySurfVol

    !!- Initializer
    generic, public :: initialize => initHOS, initHOSSurf2VolDict

    !!- Destructor
    final :: finalSurf2Vol

end type
