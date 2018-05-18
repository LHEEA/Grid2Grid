SUBROUTINE initFFTW_HOS_OCEAN(this, nX, nY)
    !
    ! Initializes the Fourier Transforms
    ! default library: FFTW-3.3 (library=3)
    !
    IMPLICIT NONE
    !
    ! Input variables
    class(typFFFWHosOcean),intent(inout) :: this
    INTEGER, INTENT(in) :: nX, nY

    this%fftw_index    = G_FFTW_OCEAN_INDEX
    G_FFTW_OCEAN_INDEX = G_FFTW_OCEAN_INDEX + 1

    this%n1 = nX
    this%n2 = nY
    this%n1o2p1 = nX / 2 + 1

    this%m1      = this%n1
    this%m2      = this%n2
    this%Nd1     = 1
    this%Nd2     = 1
    this%Nd1o2p1 = 1
    this%m1o2p1  = this%n1o2p1
    this%md1o2p1 = 1
    this%md1     = 1
    this%md2     = 1
    !
    ! Memory allocation
    ALLOCATE(this%in(this%n1,this%n2), this%out(this%n1o2p1,this%n2))
    ALLOCATE(this%s_2_f(this%n1o2p1,this%n2), this%f_2_s(this%n1o2p1,this%n2))
    ALLOCATE(this%in_big(this%Nd1,this%Nd2))
    ALLOCATE(this%out_big(this%Nd1o2p1,this%Nd2))
    ALLOCATE(this%s_2_f_big(this%Nd1o2p1,this%Nd2))
    ALLOCATE(this%f_2_s_big(this%Nd1o2p1,this%Nd2))

    ! Initializing the FFT library
    ! FFTW-3.3 library
    ! FFTW_ESTIMATE is deterministic (useful for debug/testing) but not optimal
    this%fftw_flag = FFTW_ESTIMATE !FFTW_PATIENT ! FFTW_MEASURE ! FFTW_EXHAUSTIVE
    IF (this%n2 == 1) THEN
        ! 1D FFTs
        CALL dfftw_plan_dft_r2c_1d(this%plan_R2C, this%n1, this%in, this%out, this%fftw_flag)
        CALL dfftw_plan_dft_c2r_1d(this%plan_C2R, this%n1, this%out,this%in, this%fftw_flag)
        !
        CALL dfftw_plan_dft_r2c_1d(this%plan_R2C_big, this%Nd1, this%in_big, this%out_big, this%fftw_flag)
        CALL dfftw_plan_dft_c2r_1d(this%plan_C2R_big, this%Nd1, this%out_big, this%in_big, this%fftw_flag)
    ELSE
        ! 2D FFTs
        CALL dfftw_plan_dft_r2c_2d(this%plan_R2C, this%n1, this%n2, this%in, this%out, this%fftw_flag)
        CALL dfftw_plan_dft_c2r_2d(this%plan_C2R, this%n1, this%n2, this%out, this%in, this%fftw_flag)
        !
        CALL dfftw_plan_dft_r2c_2d(this%plan_R2C_big, this%Nd1, this%Nd2, this%in_big, this%out_big, this%fftw_flag)
        CALL dfftw_plan_dft_c2r_2d(this%plan_C2R_big, this%Nd1, this%Nd2, this%out_big, this%in_big, this%fftw_flag)
    ENDIF

    ! Evaluating conversion coefficients
    ! when computing a space to Fourier Transform
    ! on the free surface
    this%s_2_f(1,1)             = 1.0_rp / REAL(this%n1*this%n2,RP)
    this%s_2_f(2:this%n1o2p1,1) = 2.0_rp / REAL(this%n1*this%n2,RP)
    !
    this%s_2_f(1,2:this%n2)             = 1.0_rp / REAL(this%n1*this%n2,RP)
    this%s_2_f(2:this%n1o2p1,2:this%n2) = 2.0_rp / REAL(this%n1*this%n2,RP)
    !
    IF(iseven(this%n1)) THEN
        this%s_2_f(this%n1o2p1,1:this%n2) = 1.0_rp / REAL(this%n1*this%n2,RP)
    ENDIF
    !
    ! for 'dealiased' domain
    !
    this%s_2_f_big(1,1)              = 1.0_rp / REAL(this%Nd1*this%Nd2,RP)
    this%s_2_f_big(2:this%Nd1o2p1,1) = 2.0_rp / REAL(this%Nd1*this%Nd2,RP)
    !
    this%s_2_f_big(1,2:this%Nd2)              = 1.0_rp / REAL(this%Nd1*this%Nd2,RP)
    this%s_2_f_big(2:this%Nd1o2p1,2:this%Nd2) = 2.0_rp / REAL(this%Nd1*this%Nd2,RP)
    !
    IF(iseven(this%Nd1)) THEN
        this%s_2_f_big(this%Nd1o2p1,1:this%Nd2) = 1.0_rp / REAL(this%Nd1*this%Nd2,RP)
    ENDIF
    !
    !  when computing a Fourier to space Transform
    !  on the free surface
    this%f_2_s(1,1)                = 1.0_rp
    this%f_2_s(2:this%n1o2p1,1)    = 0.5_rp
    !
    this%f_2_s(1,2:this%n2)        = 1.0_rp
    this%f_2_s(2:this%n1o2p1,2:this%n2) = 0.5_rp
    !
    IF(iseven(this%n1)) THEN
        this%f_2_s(this%n1o2p1,1:this%n2) = 1.0_rp
    ENDIF
    !
    ! for 'dealiased' domain
    !
    this%f_2_s_big(1,1)              = 1.0_rp
    this%f_2_s_big(2:this%Nd1o2p1,1) = 0.5_rp
    !
    this%f_2_s_big(1,2:this%Nd2)              = 1.0_rp
    this%f_2_s_big(2:this%Nd1o2p1,2:this%Nd2) = 0.5_rp
    !
    IF(iseven(this%Nd1)) THEN
        this%f_2_s_big(this%Nd1o2p1,1:this%Nd2) = 1.0_rp
    ENDIF

    this%isCptrAllocated = .TRUE.

END SUBROUTINE initFFTW_HOS_OCEAN

SUBROUTINE destroyFFTWOcean(this)
    Implicit None
    class(typFFFWHosOcean),intent(inout) :: this

    ! FFTW-3.3 library
    if (this%isCptrAllocated) then
        CALL dfftw_destroy_plan(this%plan_R2C)
        CALL dfftw_destroy_plan(this%plan_C2R)
        CALL dfftw_destroy_plan(this%plan_R2C_big)
        CALL dfftw_destroy_plan(this%plan_C2R_big)
        this%isCptrAllocated = .FALSE.
    end if

    ! CALL fftw_cleanup         !! Need to Ask

    ! Memory allocation
    if (allocated(this%in)) DEALLOCATE(this%in)
    if (allocated(this%out)) DEALLOCATE(this%out)
    if (allocated(this%s_2_f)) DEALLOCATE(this%s_2_f)
    if (allocated(this%f_2_s)) DEALLOCATE(this%f_2_s)

    if (allocated(this%in_big)) DEALLOCATE(this%in_big)
    if (allocated(this%out_big)) DEALLOCATE(this%out_big)
    if (allocated(this%s_2_f_big)) DEALLOCATE(this%s_2_f_big)
    if (allocated(this%f_2_s_big)) DEALLOCATE(this%f_2_s_big)
END SUBROUTINE

SUBROUTINE endFFTW_HOS_OCEAN(this)
    IMPLICIT NONE
    type(typFFFWHosOcean), intent(inout) :: this
    Call this%destroy
END SUBROUTINE endFFTW_HOS_OCEAN

! Define the effective transormations
SUBROUTINE FFTW_R2C(this, x, y)
    IMPLICIT NONE
    ! Input variables, output variable ---------------------------
    class(typFFFWHosOcean),intent(inout) :: this
    REAL(RP), DIMENSION(this%m1, this%m2), INTENT(IN)         :: x
    COMPLEX(CP), DIMENSION(this%m1o2p1, this%m2), INTENT(OUT) :: y
    !!!-----------------------------------------------------------
    this%in   = x(1:this%n1,1:this%n2)
    CALL dfftw_execute_dft_r2c(this%plan_R2C, this%in, this%out)
    y(1:this%n1o2p1,1:this%n2) = this%out * this%s_2_f
    !!!-----------------------------------------------------------
END SUBROUTINE FFTW_R2C

SUBROUTINE FFTW_C2R(this, y, x)
    IMPLICIT NONE
    ! Input variables, output variable ---------------------------
    class(typFFFWHosOcean),intent(inout) :: this
    REAL(RP), DIMENSION(this%m1, this%m2), INTENT(OUT) :: x
    COMPLEX(CP), DIMENSION(this%m1o2p1, this%m2), INTENT(IN) :: y
    !!!-----------------------------------------------------------
    this%out   = y(1:this%n1o2p1,1:this%n2) * this%f_2_s
    CALL dfftw_execute_dft_c2r(this%plan_C2R, this%out, this%in)
    x(1:this%n1,1:this%n2) = this%in
END SUBROUTINE FFTW_C2R

! Dealiased domains
SUBROUTINE FFTW_R2C_big(this, x, y)
    IMPLICIT NONE
    ! Input variables, output variable ---------------------------
    class(typFFFWHosOcean),intent(inout) :: this
    REAL(RP), DIMENSION(this%md1, this%md2), INTENT(IN)         :: x
    COMPLEX(CP), DIMENSION(this%md1o2p1, this%md2), INTENT(OUT) :: y
    !!!-----------------------------------------------------------
    this%in_big   = x(1:this%Nd1,1:this%Nd2)
    CALL dfftw_execute_dft_r2c(this%plan_R2C_big, this%in_big, this%out_big)
    y(1:this%Nd1o2p1,1:this%Nd2) = this%out_big * this%s_2_f_big
END SUBROUTINE FFTW_R2C_big

SUBROUTINE FFTW_C2R_big(this, y, x)
    IMPLICIT NONE
    ! Input variables, output variable ---------------------------
    class(typFFFWHosOcean),intent(inout) :: this
    REAL(RP), DIMENSION(this%md1, this%md2), INTENT(OUT)       :: x
    COMPLEX(CP), DIMENSION(this%md1o2p1, this%md2), INTENT(IN) :: y
    !!!-----------------------------------------------------------
    this%out_big = y(1:this%Nd1o2p1, 1:this%Nd2) * this%f_2_s_big
    CALL dfftw_execute_dft_c2r(this%plan_C2R_big, this%out_big, this%in_big)
    x(1:this%Nd1,1:this%Nd2) = this%in_big
END SUBROUTINE FFTW_C2R_big


LOGICAL FUNCTION iseven(n)
    IMPLICIT NONE
    INTEGER,intent(in) :: n
    iseven = (MOD(n,2) == 0)
END FUNCTION iseven
