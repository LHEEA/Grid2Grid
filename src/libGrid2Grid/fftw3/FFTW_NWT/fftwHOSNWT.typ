type,public :: typFFFWHosNWT

    integer :: fftw_index
    integer :: flag

    logical :: isCptrAllocated = .FALSE.

    type(C_PTR)                           :: plan_CC, plan_SC, plan_CS, plan_SS
    REAL(RP), ALLOCATABLE, DIMENSION(:,:) :: s_2_f, f_2_s
    REAL(RP), ALLOCATABLE, DIMENSION(:,:) :: in, out, in_sin, out_sin

    type(C_PTR)                           :: plan_Cy, plan_Sy
    REAL(RP), ALLOCATABLE, DIMENSION(:)   :: s_2_f_y, f_2_s_y
    REAL(RP), ALLOCATABLE, DIMENSION(:)   :: in_y, out_y

    type(C_PTR)                           :: plan_CC_add, plan_SC_add, plan_CS_add, plan_SS_add
    type(C_PTR)                           :: plan_CC_add_I, plan_SC_add_I, plan_CS_add_I, plan_SS_add_I
    REAL(RP), ALLOCATABLE, DIMENSION(:,:) :: s_2_f_add, f_2_s_add
    REAL(RP), ALLOCATABLE, DIMENSION(:,:) :: in_add, out_add

    ! Fourier transforms on 'dealiased' domain i.e. extended
    type(C_PTR)                           :: plan_CC_big, plan_SC_big, plan_CS_big, plan_SS_big
    REAL(RP), ALLOCATABLE, DIMENSION(:,:) :: s_2_f_big, f_2_s_big
    REAL(RP), ALLOCATABLE, DIMENSION(:,:) :: in_big, out_big, in_sin_big, out_sin_big

    integer :: m1, m2, m3_add
    integer :: Nd1, Nd2, md1, md2
    integer :: nX, nY, nAdd

    contains        

        procedure, public :: initialize => initFFTW_HOS_NWT

        procedure, public :: SPACE_2_FOURIER => DCT_FFTW

        procedure, public :: FOURIER_2_SPACE => IDCT_FFTW

        procedure, public :: FOURIER_2_SPACE_y => IDCT_FFTW_y

        procedure, public :: SPACE_2_FOURIER_add => DCT_FFTW_add

        procedure, public :: FOURIER_2_SPACE_add => IDCT_FFTW_add

        procedure, public :: SPACE_2_FOURIER_BIG => DCT_FFTW_BIG

        procedure, public :: FOURIER_2_SPACE_BIG => IDCT_FFTW_BIG

        !! Deallocate Dynamic Array
        procedure, public :: destroy => destroyFFTWNWT

        !! Destroyer
        final             :: endFFTW_HOS_NWT

end type
