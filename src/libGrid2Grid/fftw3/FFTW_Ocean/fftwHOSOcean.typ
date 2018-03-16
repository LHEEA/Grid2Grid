Type, public :: typFFFWHosOcean

    integer :: fftw_index
    integer :: fftw_flag

    logical :: isCptrAllocated = .FALSE.

    TYPE(C_PTR)                              :: plan_R2C, plan_C2R
    REAL(RP), ALLOCATABLE, DIMENSION(:,:)    :: s_2_f, f_2_s
    REAL(RP), ALLOCATABLE, DIMENSION(:,:)    :: in
    COMPLEX(CP), ALLOCATABLE, DIMENSION(:,:) :: out

    TYPE(C_PTR)                              :: plan_R2C_big, plan_C2R_big
    REAL(RP), ALLOCATABLE, DIMENSION(:,:)    :: s_2_f_big, f_2_s_big
    REAL(RP), ALLOCATABLE, DIMENSION(:,:)    :: in_big
    COMPLEX(CP), ALLOCATABLE, DIMENSION(:,:) :: out_big

    REAL(RP) :: twoon1, twooNd1, twoon2, twooNd2, oneon1, oneon2, oneoNd1, oneoNd2

    integer :: n1,n2,n1o2p1
    integer :: m1, m2, nd1, nd2, Nd1o2p1
    integer :: m1o2p1, md1o2p1, md1, md2

contains

    procedure, public :: initialize => initFFTW_HOS_OCEAN

    ! procedure, public :: terminate => endFFTW_HOS_OCEAN

    procedure, public :: SPACE_2_FOURIER => FFTW_R2C

    procedure, public :: FOURIER_2_SPACE => FFTW_C2R

    procedure, public :: SPACE_2_FOURIER_big => FFTW_R2C_big

    procedure, public :: FOURIER_2_SPACE_big => FFTW_C2R_big

    !! Deallocate Dynamic Array
    procedure, public :: destroy => destroyFFTWOcean

    !! Destroyer
    final             :: endFFTW_HOS_OCEAN

end type
