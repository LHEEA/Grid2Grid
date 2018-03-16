!!*------------------------------------------------------------------*!!
!!  Project : mfpDataStructure
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Module
!!      General
!!
!!  Description
!!      FyMC Pack General Module
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!  Module : FyMcPack Module General
!! ------------------------------------------------------------------ !!
Module  mfpGeneral

!!  Dependency ------------------------------------------------------ !!

    use mfpGlobal

!!  Variable Declaration -------------------------------------------- !!

Implicit None

private

!!  Module Variable ------------------------------------------------- !!

    integer, parameter :: strMaxLength = CHAR_LEN

    integer, parameter :: sLTableDefaultSize = 50

    logical, parameter :: sLTableSearchErrrorStop = .TRUE.

    integer, parameter :: fileLineLength = 2000

!!  Header Files ---------------------------------------------------- !!

    Include "string/stringI.typ"

    Include "string/string.typ"

    Include "fileIO/fileIO.typ"

    Include "dataArray/dataArray.typ"

    Include "sLookUpTable/sLookUpTable.typ"

    Include "dictionary/dictionary.typ"

    Include "dictionary/dictionaryPtr.typ"

    Include "dictionary/dictDataBase.typ"

    public :: testDataArray, testString, testSLTable, testDictionary

Contains

!!  Procedure Files ------------------------------------------------- !!

    Include "string/stringI.inc"

    Include "string/string.inc"

    Include "fileIO/fileIO.inc"

    Include "dataArray/dataArray.inc"

    Include "sLookUpTable/sLookUpTable.inc"

    Include "auxiliary/auxiliary.inc"

    Include "dictionary/dictionary.inc"

    Include "dictionary/dictionaryPtr.inc"

    Include "dictionary/dictDataBase.inc"

!!  Subroutine Test Script ------------------------------------------ !!

    Include "string/testString.inc"

    Include "dataArray/testDataArray.inc"

    Include "sLookUpTable/testSLTable.inc"

    Include "dictionary/testDictionary.inc"

End Module
