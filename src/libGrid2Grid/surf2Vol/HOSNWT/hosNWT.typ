!!- HOS NWT Type
type, public :: typHOSNWT

!!! Private -------------------------------------------------------
private

    !!- HOS Input Dictionary
    Type(typDictionaryPtr)      :: dict_

    !!- HOS NWT file IO
    type(typFileIO) :: hosFile_

    !!- HOS volume procedure
    character(len=StringLength) :: hosProcedure_

    !!- Initialization logical Value
    Logical :: isInitialized_ = .FALSE.

    !!- Simulation Parameters of HOS NWT
    integer         :: nXmode_, nYmode_, nAddmode_

    integer         :: nXmodeOrg_, nYmodeOrg_

    real(rp),public :: dtOut_, Tstop_
    real(rp),public :: depth_
    real(rp),public :: nonDimDepth_

    real(rp)        :: dimL_, dimT_

    integer         :: nHOSTime_

    real(rp)        :: xLen_, yLen_
    real(rp)        :: zMin_, zMax_

    real(rp)        :: nonDimxLen_, nonDimyLen_
    real(rp)        :: nonDimzMin_, nonDimzMax_

    !!- Number of extra interpolation points between HOS node
    integer :: extraInterpolationNumber

    !!- Added Mode Computation Bool
    logical  :: isAddedMode_ = .true.

    !! - Is HDF5 format
    logical :: isHDF5Format_ = .false.

    !!- Correct time index
    integer  :: iReadTimeIndex_

    !!- HOS NWT mode array (updated by calling read_mod subroutine)
    type(typHOSNWTmode) :: hosMode_

    !!- HOS Ocean FFTW Class
    type(typFFFWHosNWT) :: hosNWTFFTW_

    !!- HOS Amp Weight SWITCH
    Logical :: isHOSAmpWeight

    !!- HOS Amp Weight function
    type(typHOSModeWeightFunc) :: HOSWeightFunc

    !!! Public --------------------------------------------------------

    !!- HOS NWT Global Grid
    type(typHOSNWTMesh),public :: hosMesh_

!!! ***************************************************************
!!! HOS NWT subroutines
!!! ***************************************************************

contains

    !!! Private -------------------------------------------------------

    !!- initialize simulation parameter
    procedure, pass, private :: init_read_mod => init_read_mod_HOSNWT
    procedure, pass, private :: init_ascii_read_mod => init_ascii_read_mod_HOSNWT

#ifdef ENABLE_HDF5
    procedure, pass, private :: init_hdf5_read_mod => init_hdf5_read_mod_HOSNWT
#endif

    !!- read HOS NWT mode
    procedure, pass, private :: read_mod => read_mod_HOSNWT
    procedure, pass, private :: read_ascii_mod => read_ascii_mod_HOSNWT

#ifdef ENABLE_HDF5
    procedure, pass, private :: read_hdf5_mod => read_hdf5_mod_HOSNWT
#endif

    !!- build global mesh and wave numbers
    procedure, pass, private :: buildGlobalMesh => buildGlobalMesh_HOSNWT

    !!- reconstruct flow quantities (gradient, time derivatives of potential)
    procedure, pass, private :: reconstructionFFTs => reconstructionFFTs_HOSNWT

    !!- reconstruct flow quantities (gradient, time derivatives of potential)
    procedure, pass, private :: reconstructionAddFFTs => reconstructionAddFFTs_HOSNWT

    !!! Public  -------------------------------------------------------

    !!- initialize HOS NWT Wrapper with arguments (v.1.0)
    procedure, pass, public :: initHOSNWT

    !!- initialize HOS Ocean Wrapper with dictionary (v.2.0)
    procedure, pass, public :: initHOSNWTSurf2VolDict

    !!- read and compute flow
    procedure, public :: correct => correctHOSNWT

    !!- Destroy HOS NWT surf2vol
    procedure, public :: destroy => destroyHOSNWT

    !!- Initializer
    generic, public :: initialize => initHOSNWT, initHOSNWTSurf2VolDict

end type typHOSNWT
