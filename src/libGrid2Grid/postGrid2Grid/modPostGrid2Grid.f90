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
!   Grid2Grid Post Processing Module
!
!-----------------------------------------------------------------------
!
!   YoungMyung Choi
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
Module modPostGrid2Grid
!-----------------------------------------------------------------------

!!... Modules to use

    use modGrid2GridGlobal
    use modVol2Vol

!-----------------------------------------------------------------------
Implicit None

    Private

!!... Module Parameter

!!... Module Data

    !!... Rectilinear grid class to construct 3D HOS Mesh
    Include "rectilinearGrid/rectilinearGrid.typ"

    !!... Wave Probe Data Class
    Include "waveProbe/waveProbe.typ"

    !!... Post Processing Class
    Include "postGrid2Grid/postGrid2Grid.typ"

contains

!!... Module Function

    !! Rectilinear grid class member function
    Include "rectilinearGrid/rectilinearGrid.inc"

    !! Include "waveProbe/waveProbe.inc"

    !!... Post Processing class member function
    Include "postGrid2Grid/postGrid2Grid.inc"

!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
