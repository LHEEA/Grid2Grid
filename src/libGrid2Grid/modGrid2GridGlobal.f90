Module modGrid2GridGlobal
! This program is part of the Grid2Grid project
!   Copyright (C) 2017 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598

! definition de symboles pour les types reels (RP) et complexes (CP)
! les reels, simple ou double precision
    INTEGER, PARAMETER :: SP = KIND(1.0)
    INTEGER, PARAMETER :: DP = KIND(1.0D0)

    ! les complexes, simple ou double precision
    INTEGER, PARAMETER :: SPC = KIND(1.0)
    INTEGER, PARAMETER :: DPC = KIND(1.0D0)

    ! Les types courants
    INTEGER, PARAMETER :: RP = DP
    INTEGER, PARAMETER :: CP = DPC

    ! les constantes mathematiques usuelles i, pi, 2pi, pi/2, racine de 2.
    COMPLEX(CP), PARAMETER :: iMaginary = CMPLX(0.0_rp, 1.0_rp, KIND=CP)
    REAL(RP), PARAMETER    :: PI    = 3.141592653589793238462643383279502888419_rp
    REAL(RP), PARAMETER    :: g     = 9.81_rp
    REAL(RP), PARAMETER    :: PIO2  = 1.570796326794896619231321691639751442098_rp
    REAL(RP), PARAMETER    :: TWOPI = 6.283185307179586476925286766559005768394_rp
    REAL(RP), PARAMETER    :: SQ2   = 1.414213562373095048801688724209698078569_rp

    ! For comparison of real numbers
    REAL(RP), PARAMETER    :: tiny    = epsilon(1.0_rp)
    REAL(RP), PARAMETER    :: convErr = epsilon(1.0) * 10

    ! String length
    INTEGER, PARAMETER :: StringLength = 300

    ! Input File Character Length
    INTEGER, PARAMETER :: nCharFileLength = 5000

    ! surf2Vol VTK file Out
    LOGICAL :: isSurf2VolVTKWrite = .FALSE.

    ! VTK file Out Directory
    Character(len=StringLength),parameter :: vtkDirectory = "./VTK/"
    Character(len=StringLength),parameter :: surf2VolVTK  = trim(vtkDirectory)//"Grid2Grid/"
    Character(len=StringLength),parameter :: postG2GVTK   = trim(vtkDirectory)//"G2G_3DResult/"

    ! Building Z mesh Index
    integer, parameter :: INDEX_UNIFORM_MESH        = 0
    integer, parameter :: INDEX_SINE_MESH           = 1
    integer, parameter :: INDEX_GEOMETRICRATIO_MESH = 2

    ! Function Z Value Criterion
    REAL(RP),PARAMETER :: FNZ_VALUE = 10.0_RP

    Include "auxiliary/fileIO.typ"

Contains

    Include "auxiliary/fileIO.inc"

    Include "auxiliary/string.inc"

EndModule
