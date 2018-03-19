Type, public :: typPostGrid2Grid

    private

        !!- HOS NWT file IO
        type(typFileIO) :: postInputFile_

        type(typDictionaryPtr)      :: dict_

        !!- Initialization logical Value
        Logical :: isInitialized_ = .FALSE.

        !!- Solver Name
        Character(len=StringLength) :: hosSolver_

        !!- HOS Result File Name
        Character(len=StringLength) :: hosFileName_

        !!- Output Option
        logical                     :: isWriteVTK_       = .TRUE.

        !!- Output Option
        logical                     :: isWriteWaveProbe_ = .TRUE.

        !!- Grid Construction
        logical                     :: isBuildAirMesh_ = .FALSE.

        !!- Start & End Time
        Real(RP)                    :: startTime_, endTime_, dt_

        !!- Z mesh Flag
        Integer                     :: zflag_ = INDEX_UNIFORM_MESH
        !   zFlag = 0 : Unifrom Mesh (default)
        !         = 1 : Sine distributed mesh
        !         = 2 : Distributed mesh with Geometic ratio (common ratio)

        !!- Vol2Vol Class
        Type(typHOSVol2Vol)            :: hosVol2Vol_

        !!- rectilinearGrid
        Type(typRectilinearGrid)       :: rectLGrid_


        !!- Number of Wave probe
        integer                        :: nWaveProbe_

        !!- wave probe
        Type(typWaveProbe),allocatable :: waveProbe_(:)

        !!- Logical value for wave probe file Header
        logical                        :: isWaveProbeOutHeader_ = .TRUE.

        !!- Wave Elevation Result File IO
        type(typFileIO)                :: waveFile_

    contains

        !!- Read Grid2Grid Post Processing Input Program
        procedure, pass, private :: readPostG2GInputFile

        !!- Check Input Parameters
        procedure, pass, private :: checkPostG2GParameter

        !!- Write VTK file when air mesh is used
        procedure, pass, private :: writeVTKtotalASCII

        !!- Write VTK file when air mesh is not used (stretched to wave elevation)
        procedure, pass, private :: writeVTKnoAirASCII

        !!- (Developing VTK output formating)
        ! procedure, pass, private :: writeVTKtotalBINARY

        !!- Write VTK
        procedure, pass, public :: writeVTK

        !!- Inialize Grid2Grid Post Processing
        procedure, public :: initialize => initializePostG2G

        !!- Correct Post Processing Program for given time index
        procedure, public :: correct => correctPostG2G

        !!- Do Post Processing
        procedure, public :: doPostProcessing

        !!- Destroy postGrid2Grid
        procedure, public :: destroy => destroyPostG2G

        !!- Destroyer
        !final             :: finalPostG2G

End Type
