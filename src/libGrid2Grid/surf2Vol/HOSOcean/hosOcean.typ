!!- HOS Ocean Type
type, public :: typHOSOcean

!!! Private -------------------------------------------------------
private

    !!- HOS Input Dictionary
    Type(typDictionaryPtr)      :: dict_

    !!- HOS Ocean file IO
    type(typFileIO) :: hosFile_

    !!- HOS volume procedure
    character(len=StringLength) :: hosProcedure_

    !!- Initialization logical Value
    Logical :: isInitialized_ = .FALSE.

    !!- Simulation Parameters of HOS Ocean

    integer  :: nXmodeOrg_, nYmodeOrg_

    integer  :: nXmode_, nYmode_

    integer  :: nXo2p1Org_, nYo2p1Org_

    integer  :: nXo2p1_, nYo2p1_

    logical  :: isXeven_,isYeven_

    real(rp),public :: dtOut_, Tstop_

    real(rp)        :: zMin_, zMax_
    real(rp)        :: xLen_, yLen_

    real(rp)        :: nonDimxLen_, nonDimyLen_
    real(rp)        :: nonDimzMin_, nonDimzMax_

    real(rp),public :: depth_
    real(rp),public :: nonDimDepth_
    integer         :: nHOSTime_
    real(rp)        :: gravi, dimL_, dimT_


    !! - Is HDF5 format
    logical :: isHDF5Format_ = .false.

    !!- Correct time index
    integer  :: iReadTimeIndex_

    !!- HOS Ocean mode array (updated by calling read_mod subroutine)
    type(typHOSOceanMode) :: hosMode_

    !!- HOS Ocean FFTW Class
    type(typFFFWHosOcean) :: hosOceanFFTW_

    !!! Public --------------------------------------------------------

    !!- HOS Ocean Global Grid
    type(typHOSOceanMesh),public :: hosMesh_

contains

    !!! Private -------------------------------------------------------

    !!- initialize simulation parameter
    procedure, pass, private :: init_read_mod => init_read_mod_HOSOcean
    procedure, pass, private :: init_ascii_read_mod => init_ascii_read_mod_HOSOcean

#ifdef ENABLE_HDF5
    procedure, pass, private :: init_hdf5_read_mod => init_hdf5_read_mod_mod_HOSOcean
#endif
    !!- read HOS Ocean mode
    procedure, pass, private :: read_mod => read_mod_HOSOcean
    procedure, pass, private :: read_ascii_mod => read_ascii_mod_HOSOcean

#ifdef ENABLE_HDF5
    procedure, pass, private :: read_hdf5_mod => read_hdf5_mod_HOSOcean
#endif

    !!- build global mesh and wave numbers
    procedure, pass, private :: buildGlobalMesh => buildGlobalMesh_HOSOcean

    !!- reconstruct flow quantities (gradient, time derivatives of potential)
    procedure, pass, private :: reconstructionFFTs => reconstructionFFTs_HOSOcean

    !!! Public  -------------------------------------------------------

    !!- initialize HOS Ocean Wrapper with argument (v.1.0)
    procedure, pass, public :: initHOSOcean

    !!- initialize HOS Ocean Wrapper with dictionary (v.2.0)
    procedure, pass, public :: initHOSOceanSurf2VolDict

    !!- read and compute flow
    procedure :: correct    => correctHOSOcean

    !!- Destroy HOS Ocean Surf2Vol
    procedure :: destroy    => destroyHOSOcean

    !!- Initializer
    generic, public :: initialize => initHOSOcean, initHOSOceanSurf2VolDict

end type
