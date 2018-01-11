!-----------------------------------------------------------------------
Module  modHDF5interface
!-----------------------------------------------------------------------
!
!   Interface module of HDF5
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
use iso_fortran_env, only : error_unit
use hdf5

Implicit none

contains

  !!- Read a mode from a HDF5 file using its group id and its name
  subroutine read_hdf5_dataset_mod(time_group_id, mode_name, mode_data_dims, mode)
    implicit none
    INTEGER(hid_t), intent(in) :: time_group_id
    CHARACTER(len=*), intent(in) :: mode_name
    INTEGER(hsize_t), dimension(2), intent(in) :: mode_data_dims
    DOUBLE PRECISION, dimension(:,:), intent(inout) :: mode
    !!!.............................................

    ! Size of one single NWT mode
    INTEGER(hid_t) :: mode_dset_id
    INTEGER :: error

    ! Open data set
    Call h5dopen_f(time_group_id, trim(mode_name), mode_dset_id, error)

    ! Read data set
    Call h5dread_f(mode_dset_id, H5T_NATIVE_DOUBLE, mode, mode_data_dims, error)

    ! Close data set
    Call h5dclose_f(mode_dset_id, error)

  end subroutine read_hdf5_dataset_mod

!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
