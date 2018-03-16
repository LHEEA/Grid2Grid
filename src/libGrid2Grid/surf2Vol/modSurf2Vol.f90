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
    use hdf5

!-----------------------------------------------------------------------
Implicit none

Private

!!... Module Parameter

!!... Module Data

    !!... HOS Surf2Vol Base Mesh Class
    Include "HOSMesh/hosMesh.typ"

    !!... HOS NWT Surf2Vol Mesh Class
    Include "HOSNWTMesh/hosNWTMesh.typ"

    !!... HOS NWT Surf2Vol Mode Class
    Include "HOSNWTMode/hosNWTMode.typ"

    !!... HOS NWT Surf2Vol Class
    Include "HOSNWT/hosNWT.typ"

    !!... HOS Ocean Surf2Vol Mesh Class
    Include "HOSOceanMesh/hosOceanMesh.typ"

    !!... HOS Ocean Surf2Vol Mode Class
    Include "HOSOceanMode/hosOceanMode.typ"

    !!... HOS Ocean Surf2Vol Class
    Include "HOSOcean/hosOcean.typ"

    !!... HOS Surf2Vol Class
    Include "HOSSurf2Vol/hosSurf2Vol.typ"

!-----------------------------------------------------------------------
contains

!!... Module Function    

    !!... HOS Surf2Vol Base Mesh Class Member Function
    Include "HOSMesh/hosMesh.inc"

    !!... HOS NWT Surf2Vol Mesh Class Member Function
    Include "HOSNWTMesh/hosNWTMesh.inc"

    !!... HOS NWT Surf2Vol Mode Class Member Function
    Include "HOSNWTMode/hosNWTMode.inc"

    !!... HOS NWT Surf2Vol Class Member Function
    Include "HOSNWT/hosNWT.inc"

    !!... HOS Ocean Surf2Vol Mesh Class Member Function
    Include "HOSOceanMesh/hosOceanMesh.inc"

    !!... HOS Ocean Surf2Vol Mode Class Member Function
    Include "HOSOceanMode/hosOceanMode.inc"

    !!... HOS Ocean Surf2Vol Class Member Function
    Include "HOSOcean/hosOcean.inc"

    !!... HOS Surf2Vol Class Member Function
    Include "HOSSurf2Vol/hosSurf2Vol.inc"

!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
