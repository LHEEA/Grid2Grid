!-----------------------------------------------------------------------
!   Grid2Grid
!-----------------------------------------------------------------------
!   Copyright (C) 2017 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598
!
!   This program is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with this program.  If not, see <https://www.gnu.org/licenses/>.
!-----------------------------------------------------------------------
!
!   Interface module of HDF5

!       This program based on Post Processing Part of HOS.
!
!-----------------------------------------------------------------------
!
!       Program intialized by Maite Gouin.
!       Port to modulized format by YoungMyung Choi.
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
Module  modHDF5interface
!-----------------------------------------------------------------------

    use iso_fortran_env, only : error_unit
    use modGrid2GridGlobal

#ifdef ENABLE_HDF5
    use hdf5
#endif

Implicit none

  INTEGER(8), parameter :: HOS_TYPE_NAME_SIZE = 10

#ifdef ENABLE_HDF5
  interface read_hdf5_dataset_mod
    module procedure read_hdf5_double_dataset_mod, read_hdf5_complex_dataset_mod
  end interface
#endif

contains

#ifdef ENABLE_HDF5
  !! - Read the name of the HOS result file type (HOS_NWT or HOS_Ocean)
  function read_hos_type_name_mod(file_id) result(name)
    implicit none
    INTEGER(hid_t), intent(in) :: file_id
    CHARACTER(len=HOS_TYPE_NAME_SIZE) :: name

    CHARACTER(len=20), parameter :: HOS_TYPE_NAME_DSET_NAME = "HOS_result_file_type"
    INTEGER(hsize_t), dimension(1), parameter :: CHARACTER_DATA_DIMS = (/ 1 /)
    INTEGER(hid_t) :: character_type_id
    INTEGER(hid_t) :: dset_id
    INTEGER :: error
    !!!.................................

    Call h5tcopy_f(H5T_NATIVE_CHARACTER, character_type_id, error)
    Call h5tset_size_f(character_type_id, HOS_TYPE_NAME_SIZE, error)

    Call h5dopen_f(file_id, HOS_TYPE_NAME_DSET_NAME,  dset_id, error)

    Call h5dread_f(dset_id, character_type_id, name, CHARACTER_DATA_DIMS, error)

    Call h5dclose_f(dset_id, error)

  end function read_hos_type_name_mod

  !!- Read a double mode from a HDF5 file using its group id and its name
  subroutine read_hdf5_double_dataset_mod(time_group_id, mode_name, mode_data_dims, mode)
    implicit none
    INTEGER(hid_t), intent(in) :: time_group_id
    CHARACTER(len=*), intent(in) :: mode_name
    INTEGER(hsize_t), dimension(2), intent(in) :: mode_data_dims
    REAL(DP), dimension(:,:), intent(inout) :: mode
    !!!.............................................

    INTEGER(hid_t) :: mode_dset_id
    INTEGER :: error

    ! Open data set
    Call h5dopen_f(time_group_id, trim(mode_name), mode_dset_id, error)

    ! Read data set
    Call h5dread_f(mode_dset_id, H5T_NATIVE_DOUBLE, mode, mode_data_dims, error)

    ! Close data set
    Call h5dclose_f(mode_dset_id, error)

  end subroutine read_hdf5_double_dataset_mod

  !!- Read a complex mode from a HDF5 file using its group id and its name
  subroutine read_hdf5_complex_dataset_mod(time_group_id, mode_name, mode_data_dims, mode)
    implicit none
    INTEGER(hid_t), intent(in) :: time_group_id
    CHARACTER(len=*), intent(in) :: mode_name
    INTEGER(hsize_t), dimension(2), intent(in) :: mode_data_dims
    COMPLEX(CP), dimension(:,:), intent(inout) :: mode
    !!!.............................................

    INTEGER(hid_t) :: mode_dset_id
    INTEGER :: error

    ! Temporary array to pack complex type
    REAL(DP), allocatable, dimension(:,:) :: tmpRe, tmpIm
    INTEGER :: sizeX, sizeY
    sizeX = size(mode,1)
    sizeY = size(mode,2)

    allocate(tmpRe(sizeX,sizeY))
    allocate(tmpIm(sizeX,sizeY))

    ! Read each component
    call read_hdf5_double_dataset_mod(time_group_id, trim(mode_name)//".re", mode_data_dims, tmpRe)
    call read_hdf5_double_dataset_mod(time_group_id, trim(mode_name)//".im", mode_data_dims, tmpIm)

    ! Pack into a complex
    mode = cmplx(tmpRe, tmpIm)

    deallocate(tmpRe)
    deallocate(tmpIm)

  end subroutine read_hdf5_complex_dataset_mod
#endif

!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
