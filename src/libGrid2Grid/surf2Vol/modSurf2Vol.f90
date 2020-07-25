!-----------------------------------------------------------------------
!   Grid2Grid
!-----------------------------------------------------------------------
!
! This module contains all necessary features for the use of FFTW 3.3.4 in HOS-ocean
! with comprehensive interface and normalization
!
!-----------------------------------------------------------------------
!
!    Copyright (C) 2014 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598
!
!    This program is part of the Grid2Grid project and is
!    based on HOS-ocean, which is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!-----------------------------------------------------------------------
!
!   HOS Surf2Vol Module
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
Module  modSurf2vol
!-----------------------------------------------------------------------

    !!... Modules to use
    use modGrid2GridGlobal
    use modFourier_r2c_FFTW3

    use iso_fortran_env, only : error_unit
    use modHDF5interface

#ifdef ENABLE_HDF5
    use hdf5
#endif

    use mfpGlobal, only : CHAR_LEN
    use mfpGeneral, only : typDictionaryPtr, isStringEqual

!-----------------------------------------------------------------------
Implicit none

Private

!!... Module Parameter

!!... Module Data

!! ----------------------------------------------------------------
!! Add HOS Weight Class definition in the below
!! ----------------------------------------------------------------

!!... Base(Abstract) Class fot HOS Weight
#include "HOSModeWeight/hosWeightBaseFunc/hosWeightBaseFunc.typ"

!!... HOS Weight: expoential function
#include "HOSModeWeight/hosWeightExpFunc/hosWeightExpFunc.typ"

!!... HOS Weight: step function
#include "HOSModeWeight/hosWeightStepFunc/hosWeightStepFunc.typ"

!!... HOS Weight: poly function
#include "HOSModeWeight/hosWeightPolyFunc/hosWeightPolyFunc.typ"

!!... HOS Mode Weight Wrapper Class
#include "HOSModeWeight/hosModeWeight.typ"

!! ----------------------------------------------------------------

    !!... HOS Surf2Vol Base Mesh Class
#include "HOSMesh/hosMesh.typ"

    !!... HOS NWT Surf2Vol Mesh Class
#include "HOSNWTMesh/hosNWTMesh.typ"

    !!... HOS NWT Surf2Vol Mode Class
#include "HOSNWTMode/hosNWTMode.typ"

    !!... HOS NWT Surf2Vol Class
#include "HOSNWT/hosNWT.typ"

    !!... HOS Ocean Surf2Vol Mesh Class
#include "HOSOceanMesh/hosOceanMesh.typ"

    !!... HOS Ocean Surf2Vol Mode Class
#include "HOSOceanMode/hosOceanMode.typ"

    !!... HOS Ocean Surf2Vol Class
#include "HOSOcean/hosOcean.typ"

    !!... HOS Surf2Vol Class
#include "HOSSurf2Vol/hosSurf2Vol.typ"

!-----------------------------------------------------------------------
contains

!!... Module Function

!! ----------------------------------------------------------------
!! Add HOS Weight Class functions in the below
!! ----------------------------------------------------------------

!!... HOS Weight: expoential function
#include "HOSModeWeight/hosWeightExpFunc/hosWeightExpFunc.inc"

!!... HOS Weight: step function
#include "HOSModeWeight/hosWeightStepFunc/hosWeightStepFunc.inc"

!!... HOS Weight: poly function
#include "HOSModeWeight/hosWeightPolyFunc/hosWeightPolyFunc.inc"

!!... HOS Mode Weight Wrapper
#include "HOSModeWeight/hosModeWeight.inc"

!! ----------------------------------------------------------------

    !!... HOS Surf2Vol Base Mesh Class Member Function
#include "HOSMesh/hosMesh.inc"

    !!... HOS NWT Surf2Vol Mesh Class Member Function
#include "HOSNWTMesh/hosNWTMesh.inc"

    !!... HOS NWT Surf2Vol Mode Class Member Function
#include "HOSNWTMode/hosNWTMode.inc"

    !!... HOS NWT Surf2Vol Class Member Function
#include "HOSNWT/hosNWT.inc"

    !!... HOS Ocean Surf2Vol Mesh Class Member Function
#include "HOSOceanMesh/hosOceanMesh.inc"

    !!... HOS Ocean Surf2Vol Mode Class Member Function
#include "HOSOceanMode/hosOceanMode.inc"

    !!... HOS Ocean Surf2Vol Class Member Function
#include "HOSOcean/hosOcean.inc"

    !!... HOS Surf2Vol Class Member Function
#include "HOSSurf2Vol/hosSurf2Vol.inc"

!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
