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
!   Grid2Grid Vol2Vol Module
!
!-----------------------------------------------------------------------
!
!   YoungMyung Choi
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
Module  modVol2Vol
!-----------------------------------------------------------------------

!!... Modules to use
    use modGrid2GridGlobal
    use modSurf2vol

    Use bspline_module
    use,intrinsic :: iso_fortran_env, only: wp => real64

    use mfpGlobal, only : CHAR_LEN
    use mfpGeneral, only : typDictionaryPtr

Implicit none

Private

!!... Module Data

    !!... HOS 2D Simulation Interpolation Class
    Include "V2VInterp2D/v2vInterp2D.typ"

    !!... HOS 3D Simulation Interpolation Class
    Include "V2VInterp3D/v2vInterp3D.typ"

    !!... HOS Vol2Vol Class
    Include "HOSVol2Vol/hosVol2Vol.typ"

contains

!!... Module Function

    !!... HOS 2D Simulation Interpolation Class Member Function
    Include "V2VInterp2D/v2vInterp2D.inc"

    !!... HOS 3D Simulation Interpolation Class Member Function
    Include "V2VInterp3D/v2vInterp3D.inc"

    !!... HOS Vol2Vol Class Member Class
    Include "HOSVol2Vol/hosVol2Vol.inc"

!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
