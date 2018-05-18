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
!   HOS-FFTW3 Library Interface Module
!
!-----------------------------------------------------------------------

!!------------------------------------------------------------------------------
MODULE modFourier_r2c_FFTW3
!!------------------------------------------------------------------------------

!!... Modules to use

    USE modGrid2GridGlobal

    USE, INTRINSIC :: iso_c_binding

!!------------------------------------------------------------------------------
IMPLICIT NONE

Private

    !!... FFTW module procedure for original and dealiased domains
    INCLUDE 'fftw3.f03'

!!... Module Parameter
    Integer :: G_FFTW_OCEAN_INDEX = 3
    Integer :: G_FFTW_NWT_INDEX   = 103

!!... Module Data

    !!... FFTW Ocean Class(Type) Header
    Include "FFTW_Ocean/fftwHOSOcean.typ"

    !!... FFTW NWT Class(Type) Header
    Include "FFTW_NWT/fftwHOSNWT.typ"

!!------------------------------------------------------------------------------
CONTAINS

!!... Module Function

    !!... FFTW Ocean Class(Type) Function
    Include "FFTW_Ocean/fftwHOSOcean.inc"

    !!... FFTW NWT Class(Type) Function
    Include "FFTW_NWT/fftwHOSNWT.inc"


!!------------------------------------------------------------------------------
END MODULE modFourier_r2c_FFTW3
!!------------------------------------------------------------------------------
