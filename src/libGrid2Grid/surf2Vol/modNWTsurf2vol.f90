!-----------------------------------------------------------------------
Module  modNWTsurf2vol
!-----------------------------------------------------------------------
!
!   Wrapper module of HOS NWT (surf2vol)
!       It construct HOS NWT global mesh and calculate flow quantities
!       on HOS NWT global mesh. To calculate quantities of arbitrary
!       position, vol2vol module is needed.
!
!-----------------------------------------------------------------------
!   This program is part of the Grid2Grid project
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
use modFourier_r2c_FFTW3_NWT
use iso_fortran_env, only : error_unit
use modHDF5interface
use hdf5

Implicit none
!!! module variables
    private

!-----------------------------------------------------------------------
contains







!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
