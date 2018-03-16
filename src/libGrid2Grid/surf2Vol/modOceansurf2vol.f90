!-----------------------------------------------------------------------
Module  modOceansurf2vol
!-----------------------------------------------------------------------
!
!   Wrapper module of HOS Ocean (surf2vol)
!       It construct HOS Ocean global mesh and calculate flow quantities
!       on HOS Ocean global mesh. To calculate quantities of arbitrary
!       position, vol2vol module is needed.
!
!-----------------------------------------------------------------------
!
!   Copyright (C) 2017 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598
!
!   This program based on Post Processing Part of HOS.
!
!-----------------------------------------------------------------------
!
!       Program intialized by Maite Gouin.
!       Port to modulized format by YoungMyung Choi.
!
!-----------------------------------------------------------------------
use modGrid2GridType
use modFourier_r2c_FFTW3_ocean
use iso_fortran_env, only : error_unit
use modHDF5interface
use hdf5

Implicit none
!!! module variables

private




!!! ***************************************************************
!!! HOS Ocean subroutines
!!! ***************************************************************

contains







!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
