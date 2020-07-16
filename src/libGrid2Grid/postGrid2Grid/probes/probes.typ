ENUM, BIND(C)

    ENUMERATOR :: ENUM_PROBE_WAVE_ELEVATION = 101

    ENUMERATOR :: ENUM_PROBE_VELOCITY

    ENUMERATOR :: ENUM_PROBE_PRESSURE

END ENUM

Type :: typProbes

    !!... Dictionary
    Type(typDictionaryPtr) :: dict_

    !!... is Active
    Logical :: isActive

    !!... Variable Type (Wave Elevation, Velocity, Pressure)
    Character(len=100) :: variableType

    !!... Output File
    Character(len=1000) :: outputFile

    !!... ENUMERATOR
    Integer, private :: ENUM_PROBE

    !!... Probe Out File
    type(typFileIO) :: outFile

    !!... Number of Probbes
    Integer :: nProbe_

    !!... Probes
    Type(typProbe), allocatable :: probes_(:)

Contains

    !!... Inialize the probe
    Procedure :: initialize => initialize_typProbes

    !!... Correct & Write
    Procedure :: correct => correct_typProbes

    !!... Destroy the class
    Procedure :: destroy => destroy_typProbes

    !!... Write Header File
    Procedure, private :: writeHeaderOutput => writeHeaderOutput_typProbes

    !!... Write Scalar Value (Wave Elevation)
    Procedure, private :: writeScalarValue => writeScalarValue_typProbes

    !!... Write VEctor Value (Velocity)
    Procedure, private :: writeVectorValue => writeVectorValue_typProbes

End Type
