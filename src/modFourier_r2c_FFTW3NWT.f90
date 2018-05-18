MODULE modFourier_r2c_FFTW3_NWT
!
! This module contains all necessary features for the use of FFTW 3.3.4 in HOS-NWT
! with comprehensive interface and normalization
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    Copyright (C) 2014 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598
!
!    This program is part of the Grid2Grid project and is
!    based on HOS-NWT, which is free software: you can redistribute it and/or modify
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    USE modGrid2GridType
    USE, INTRINSIC :: iso_c_binding

    IMPLICIT NONE

    include 'fftw3.f03'

    Integer :: G_FFTW_NWT_INDEX = 103

    type typFFFWHosNWT

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

CONTAINS

    SUBROUTINE initFFTW_HOS_NWT(this, modenX, modenY, modenAdd)
        ! Initializes the Fourier Transforms
        ! default library: FFTW-3.x (library=3)
        IMPLICIT NONE
        ! Input variables --------------------------------------------------------
        class(typFFFWHosNWT), intent(inout) :: this
        integer, intent(in)                 :: modenX, modenY, modenAdd
        !-------------------------------------------------------------------------

        this%fftw_index  = G_FFTW_NWT_INDEX
        G_FFTW_NWT_INDEX = G_FFTW_NWT_INDEX + 1

        this%nX   = modenX
        this%nY   = modenY
        this%nAdd = modenAdd

        this%m1     = this%nX
        this%m2     = this%nY
        this%m3_add = this%nAdd

        this%Nd1     = 1
        this%Nd2     = 1
        this%md1     = 1
        this%md2     = 1

        ! Memory allocation
        ALLOCATE(this%in(this%nX, this%nY), this%out(this%nX, this%nY))
        ALLOCATE(this%s_2_f(this%nX, this%nY), this%f_2_s(this%nX, this%nY))
        ALLOCATE(this%in_sin(this%nX-2, this%nY), this%out_sin(this%nX-2, this%nY))
        ALLOCATE(this%in_y(this%nY), this%out_y(this%nY), this%s_2_f_y(this%nY), this%f_2_s_y(this%nY))

        ALLOCATE(this%in_add(this%nAdd, this%nY), this%out_add(this%nAdd, this%nY))
        ALLOCATE(this%s_2_f_add(this%nAdd, this%nY), this%f_2_s_add(this%nAdd, this%nY))

        ! Fourier transforms on 'dealiased' domain i.e. extended
        ALLOCATE(this%in_big(this%nd1, this%nd2), this%out_big(this%nd1, this%nd2))
        ALLOCATE(this%s_2_f_big(this%nd1, this%nd2), this%f_2_s_big(this%nd1, this%nd2))
        ALLOCATE(this%in_sin_big(this%nd1-2, this%nd2), this%out_sin_big(this%nd1-2, this%nd2))

        ! Initializing the FFT library
        ! FFTW-3.x library
        this%flag = FFTW_PATIENT ! FFTW_MEASURE ! FFTW_PATIENT ! FFTW_EXHAUSTIVE
        IF (this%nY == 1) THEN
            ! 1D FFTs
            CALL dfftw_plan_r2r_1d( this%plan_CC, this%nX,   this%in(1,1),     this%out(1,1),     FFTW_REDFT00, this%flag)
            CALL dfftw_plan_r2r_1d( this%plan_SC, this%nX-2, this%in_sin(1,1), this%out_sin(1,1), FFTW_RODFT00, this%flag)

            CALL dfftw_plan_r2r_1d(this%plan_CC_add,   this%nAdd, this%in_add(1,1), this%out_add(1,1), FFTW_REDFT01, this%flag)
            CALL dfftw_plan_r2r_1d(this%plan_CC_add_I, this%nAdd, this%in_add(1,1), this%out_add(1,1), FFTW_REDFT10, this%flag)
            CALL dfftw_plan_r2r_1d(this%plan_SC_add,   this%nAdd, this%in_add(1,1), this%out_add(1,1), FFTW_RODFT01, this%flag)
            CALL dfftw_plan_r2r_1d(this%plan_SC_add_I, this%nAdd, this%in_add(1,1), this%out_add(1,1), FFTW_RODFT10, this%flag)

            ! Fourier transforms on 'dealiased' domain i.e. extended
            IF (this%nd1 > 2) THEN
                CALL dfftw_plan_r2r_1d(this%plan_CC_big, this%nd1,   this%in_big(1,1),     &
                                       this%out_big(1,1),     FFTW_REDFT00, this%flag)
                CALL dfftw_plan_r2r_1d(this%plan_SC_big, this%nd1-2, this%in_sin_big(1,1), &
                                       this%out_sin_big(1,1), FFTW_RODFT00, this%flag)
            ENDIF
        ELSE
            ! 2D FFTs
            CALL dfftw_plan_r2r_2d(this%plan_CC, this%nX,   this%nY,   this%in(1,1),     this%out(1,1),     &
                                   FFTW_REDFT00, FFTW_REDFT00, this%flag)
            CALL dfftw_plan_r2r_2d(this%plan_SC, this%nX-2, this%nY,   this%in_sin(1,1), this%out_sin(1,1), &
                                   FFTW_RODFT00, FFTW_REDFT00, this%flag)
            CALL dfftw_plan_r2r_2d(this%plan_CS, this%nX,   this%nY-2, this%in(1,2),     this%out(1,2),     &
                                   FFTW_REDFT00, FFTW_RODFT00, this%flag)
            CALL dfftw_plan_r2r_2d(this%plan_SS, this%nX-2, this%nY-2, this%in_sin(1,2), this%out_sin(1,2), &
                                   FFTW_RODFT00, FFTW_RODFT00, this%flag)

            ! 2D FFTs y-direction only
            CALL dfftw_plan_r2r_1d(this%plan_Cy, this%nY,   this%in_y(1), this%out_y(1), FFTW_REDFT00, this%flag)
            CALL dfftw_plan_r2r_1d(this%plan_Sy, this%nY-2, this%in_y(2), this%out_y(2), FFTW_RODFT00, this%flag)

            ! 2D FFTs additionnal potential
            CALL dfftw_plan_r2r_2d(this%plan_CC_add,   this%nAdd, this%nY,   this%in_add(1,1), this%out_add(1,1), &
                                   FFTW_REDFT01, FFTW_REDFT00, this%flag)
            CALL dfftw_plan_r2r_2d(this%plan_CC_add_I, this%nAdd, this%nY,   this%in_add(1,1), this%out_add(1,1), &
                                   FFTW_REDFT10, FFTW_REDFT00, this%flag)
            CALL dfftw_plan_r2r_2d(this%plan_SC_add,   this%nAdd, this%nY,   this%in_add(1,1), this%out_add(1,1), &
                                   FFTW_RODFT01, FFTW_REDFT00, this%flag)
            CALL dfftw_plan_r2r_2d(this%plan_SC_add_I, this%nAdd, this%nY,   this%in_add(1,1), this%out_add(1,1), &
                                   FFTW_RODFT10, FFTW_REDFT00, this%flag)
            CALL dfftw_plan_r2r_2d(this%plan_CS_add,   this%nAdd, this%nY-2, this%in_add(1,2), this%out_add(1,2), &
                                   FFTW_REDFT01, FFTW_RODFT00, this%flag)
            CALL dfftw_plan_r2r_2d(this%plan_CS_add_I, this%nAdd, this%nY-2, this%in_add(1,2), this%out_add(1,2), &
                                   FFTW_REDFT10, FFTW_RODFT00, this%flag)
            CALL dfftw_plan_r2r_2d(this%plan_SS_add,   this%nAdd, this%nY-2, this%in_add(1,2), this%out_add(1,2), &
                                   FFTW_RODFT01, FFTW_RODFT00, this%flag)
            CALL dfftw_plan_r2r_2d(this%plan_SS_add_I, this%nAdd, this%nY-2, this%in_add(1,2), this%out_add(1,2), &
                                   FFTW_RODFT10, FFTW_RODFT00, this%flag)

            ! 2D FFTs in 'dealiased' domain (i.e. extended)
            IF ((this%nd1 > 2).AND.(this%nd2>2)) THEN
                CALL dfftw_plan_r2r_2d(this%plan_CC_big, this%nd1,   this%nd2,   this%in_big(1,1),     this%out_big(1,1),     &
                                       FFTW_REDFT00, FFTW_REDFT00, this%flag)
                CALL dfftw_plan_r2r_2d(this%plan_SC_big, this%nd1-2, this%nd2,   this%in_sin_big(1,1), this%out_sin_big(1,1), &
                                       FFTW_RODFT00, FFTW_REDFT00, this%flag)
                CALL dfftw_plan_r2r_2d(this%plan_CS_big, this%nd1,   this%nd2-2, this%in_big(1,2),     this%out_big(1,2),     &
                                       FFTW_REDFT00, FFTW_RODFT00, this%flag)
                CALL dfftw_plan_r2r_2d(this%plan_SS_big, this%nd1-2, this%nd2-2, this%in_sin_big(1,2), this%out_sin_big(1,2), &
                                       FFTW_RODFT00, FFTW_RODFT00, this%flag)
            ENDIF
        END IF

        ! Evaluating conversion coefficients
        !  when computing a space to Fourier Transform
        !  on the free surface
        this%s_2_f                = 1.0_rp / REAL(MAX(1,2*(this%nX-1)), RP)
        this%s_2_f(2:this%nX-1,:) = 2.0_rp * this%s_2_f(2:this%nX-1,:)
        IF (this%nY /= 1) THEN
            this%s_2_f                = this%s_2_f / REAL(MAX(1,2*(this%nY-1)), RP)
            this%s_2_f(:,2:this%nY-1) = 2.0_rp * this%s_2_f(:,2:this%nY-1)
        END IF

        IF (this%nY /= 1) THEN
            this%s_2_f_y              = 1.0_rp / REAL(MAX(1,2*(this%nY-1)), RP)
            this%s_2_f_y(2:this%nY-1) = 2.0_rp * this%s_2_f_y(2:this%nY-1)
        END IF

        !  on the wavemaker
        this%s_2_f_add           = 1.0_rp / REAL(this%nAdd, RP) ! it is 2/(2*n)=2/(2*nAdd)=1/nAdd in fact
        IF (this%nY /= 1) THEN
            this%s_2_f_add                = this%s_2_f_add / REAL(MAX(1,2*(this%nY-1)), RP)
            this%s_2_f_add(:,2:this%nY-1) = 2.0_rp * this%s_2_f_add(:,2:this%nY-1)
        END IF

        ! on the free surface 'dealiased' domain
        this%s_2_f_big                 = 1.0_rp / REAL(MAX(1,2*(this%nd1-1)), RP)
        this%s_2_f_big(2:this%nd1-1,:) = 2.0_rp * this%s_2_f_big(2:this%nd1-1,:)
        IF (this%nY /= 1) THEN
            this%s_2_f_big                 = this%s_2_f_big / REAL(MAX(1,2*(this%nd2-1)), RP)
            this%s_2_f_big(:,2:this%nd2-1) = 2.0_rp * this%s_2_f_big(:,2:this%nd2-1)
        END IF

        !  when computing a Fourier to space Transform
        !  on the free surface
        this%f_2_s                = 1.0_rp
        this%f_2_s(2:this%nX-1,:) = 0.5_rp * this%f_2_s(2:this%nX-1,:)
        IF (this%nY /= 1) THEN
            this%f_2_s(:,2:this%nY-1) = 0.5_rp * this%f_2_s(:,2:this%nY-1)
        END IF

        IF (this%nY /= 1) THEN
            this%f_2_s_y              = 1.0_rp
            this%f_2_s_y(2:this%nY-1) = 0.5_rp * this%f_2_s_y(2:this%nY-1)
        END IF

        !  on the wavemaker
        this%f_2_s_add = 0.5_rp
        IF (this%nY /= 1) THEN
            this%f_2_s_add(:,2:this%nY-1) = 0.5_rp * this%f_2_s_add(:,2:this%nY-1)
        END IF

        ! on the free surface 'dealiased' domain
        this%f_2_s_big                     = 1.0_rp
        this%f_2_s_big(2:this%nd1-1,:)     = 0.5_rp * this%f_2_s_big(2:this%nd1-1,:)
        IF (this%nY /= 1) THEN
            this%f_2_s_big(:,2:this%nd2-1) = 0.5_rp * this%f_2_s_big(:,2:this%nd2-1)
        END IF

        this%isCptrAllocated = .TRUE.

    END SUBROUTINE initFFTW_HOS_NWT

    SUBROUTINE destroyFFTWNWT(this)
        Implicit None
        class(typFFFWHosNWT),intent(inout) :: this
        ! FFTW-3.3 library

        if (this%isCptrAllocated) then
            CALL dfftw_destroy_plan(this%plan_CC)
            CALL dfftw_destroy_plan(this%plan_SC)
            CALL dfftw_destroy_plan(this%plan_CS)
            CALL dfftw_destroy_plan(this%plan_SS)

            CALL dfftw_destroy_plan(this%plan_Cy)
            CALL dfftw_destroy_plan(this%plan_Sy)

            CALL dfftw_destroy_plan(this%plan_CC_add)
            CALL dfftw_destroy_plan(this%plan_SC_add)
            CALL dfftw_destroy_plan(this%plan_CS_add)
            CALL dfftw_destroy_plan(this%plan_SS_add)

            CALL dfftw_destroy_plan(this%plan_CC_add_I)
            CALL dfftw_destroy_plan(this%plan_SC_add_I)
            CALL dfftw_destroy_plan(this%plan_CS_add_I)
            CALL dfftw_destroy_plan(this%plan_SS_add_I)

            CALL dfftw_destroy_plan(this%plan_CC_big)
            CALL dfftw_destroy_plan(this%plan_SC_big)
            CALL dfftw_destroy_plan(this%plan_CS_big)
            CALL dfftw_destroy_plan(this%plan_SS_big)
            this%isCptrAllocated = .FALSE.
        end if

        ! CALL fftw_cleanup         !! Need to Ask

        ! Memory allocation
        if (allocated(this%s_2_f))   DEALLOCATE(this%s_2_f)
        if (allocated(this%f_2_s))   DEALLOCATE(this%f_2_s)
        if (allocated(this%in))      DEALLOCATE(this%in)
        if (allocated(this%out))     DEALLOCATE(this%out)
        if (allocated(this%in_sin))  DEALLOCATE(this%in_sin)
        if (allocated(this%out_sin)) DEALLOCATE(this%out_sin)

        if (allocated(this%s_2_f_y)) DEALLOCATE(this%s_2_f_y)
        if (allocated(this%f_2_s_y)) DEALLOCATE(this%f_2_s_y)
        if (allocated(this%in_y))    DEALLOCATE(this%in_y)
        if (allocated(this%out_y))   DEALLOCATE(this%out_y)

        if (allocated(this%s_2_f_add)) DEALLOCATE(this%s_2_f_add)
        if (allocated(this%f_2_s_add)) DEALLOCATE(this%f_2_s_add)
        if (allocated(this%in_add))    DEALLOCATE(this%in_add)
        if (allocated(this%out_add))   DEALLOCATE(this%out_add)

        if (allocated(this%s_2_f_big)) DEALLOCATE(this%s_2_f_big)
        if (allocated(this%f_2_s_big)) DEALLOCATE(this%f_2_s_big)
        if (allocated(this%in_big))    DEALLOCATE(this%in_big)
        if (allocated(this%out_big))   DEALLOCATE(this%out_big)

        if (allocated(this%in_sin_big))  DEALLOCATE(this%in_sin_big)
        if (allocated(this%out_sin_big)) DEALLOCATE(this%out_sin_big)

    END SUBROUTINE

    Subroutine endFFTW_HOS_NWT(this)
        Implicit None
        type(typFFFWHosNWT),intent(inout) :: this
        Call this%destroy
    End subroutine

    FUNCTION DCT_FFTW(this, f, CS_x, CS_y) result(result_DCT_FFTW)
        IMPLICIT NONE
        ! Input variables, output variable --------------------------------------
        class(typFFFWHosNWT), intent(inout)               :: this
        REAL(RP), DIMENSION(this%m1, this%m2),intent(in)  :: f
        CHARACTER(LEN=*), intent(in)                      :: CS_x, CS_y
        REAL(RP), DIMENSION(this%m1, this%m2)             :: result_DCT_FFTW
        !-------------------------------------------------------------------------
        this%in   = f(1:this%nX,1:this%nY)
        CALL execute(this, CS_x, CS_y)
        result_DCT_FFTW(1:this%nX,1:this%nY) = this%out * this%s_2_f
    END FUNCTION DCT_FFTW

    FUNCTION IDCT_FFTW(this, f, CS_x, CS_y) result(result_IDCT_FFTW)
        IMPLICIT NONE
        ! Input variables, output variable ---------------------------------------
        class(typFFFWHosNWT), intent(inout)               :: this
        REAL(RP), DIMENSION(this%m1, this%m2),intent(in)  :: f
        CHARACTER(LEN=*),intent(in)                       :: CS_x, CS_y
        REAL(RP), DIMENSION(this%m1, this%m2)             :: result_IDCT_FFTW
        !-------------------------------------------------------------------------
        this%in   = f(1:this%nX,1:this%nY) * this%f_2_s
        CALL execute(this, CS_x, CS_y)
        result_IDCT_FFTW(1:this%nX,1:this%nY) = this%out
    END FUNCTION IDCT_FFTW

    SUBROUTINE execute(this, CS_x, CS_y)
        IMPLICIT NONE
        ! Input variables --------------------------------------------------------
        class(typFFFWHosNWT), intent(inout)      :: this
        CHARACTER(LEN=*), intent(in)             :: CS_x, CS_y
        ! Local variables
        type(C_PTR)                              :: plan
        !-------------------------------------------------------------------------
        plan = select_plan(this, CS_x, CS_y)

        IF ((this%nY /= 1) .AND. (CS_y == 'sin')) THEN
            this%in(:,1)       = 0.0_rp
            this%in(:,this%nY) = 0.0_rp
        END IF

        IF (CS_x == 'sin') this%in_sin = this%in(2:this%nX-1,:)
        call dfftw_execute(plan)

        IF (CS_x == 'sin') THEN
            this%out(1,:)           = 0.0_rp
            this%out(2:this%nX-1,:) = this%out_sin
            this%out(this%nX,:)     = 0.0_rp
        END IF

        IF ((this%nY /= 1) .AND. (CS_y == 'sin')) THEN
            this%out(:,1)       = 0.0_rp
            this%out(:,this%nY) = 0.0_rp
        END IF
    END SUBROUTINE execute

    FUNCTION select_plan(this, CS_x, CS_y) result(result_select_plan)
        IMPLICIT NONE
        ! Input variables --------------------------------------------------------
        class(typFFFWHosNWT), intent(inout)      :: this
        CHARACTER(LEN=*),intent(in)              :: CS_x, CS_y
        ! Output variable
        type(C_PTR)                              :: result_select_plan
        !-------------------------------------------------------------------------
        IF ((CS_x /= 'cos') .AND. (CS_x /= 'sin')) STOP 'Unknown x-transform in Fourier_FFTW:select_plan'
        IF ((CS_y /= 'cos') .AND. (CS_y /= 'sin')) STOP 'Unknown y-transform in Fourier_FFTW:select_plan'

        IF (this%nY == 1) THEN
            IF (CS_x == 'cos') THEN
                result_select_plan = this%plan_CC
            ELSE IF (CS_x == 'sin') THEN
                result_select_plan = this%plan_SC
            ELSE
                STOP 'Unknown x-transform in Fourier_FFTW:select_plan'
                result_select_plan = this%plan_CC
            END IF
        ELSE
            IF ((CS_x == 'cos') .AND. (CS_y == 'cos')) THEN
                result_select_plan = this%plan_CC
            ELSE IF ((CS_x == 'sin') .AND. (CS_y == 'cos')) THEN
                result_select_plan = this%plan_SC
            ELSE IF ((CS_x == 'cos') .AND. (CS_y == 'sin')) THEN
                result_select_plan = this%plan_CS
            ELSE IF ((CS_x == 'sin') .AND. (CS_y == 'sin')) THEN
                result_select_plan = this%plan_SS
            ELSE
                STOP 'Unknown x,y-transform in Fourier_FFTW:select_plan'
                result_select_plan = this%plan_CC
            END IF
        END IF

    END FUNCTION select_plan

    FUNCTION DCT_FFTW_y(this, f, CS_y) result (result_DCT_FFTW_y)
        IMPLICIT NONE
        ! Input variables, output variable --------------------------------------
        class(typFFFWHosNWT), intent(inout)      :: this
        REAL(RP), DIMENSION(this%m2),intent(in)  :: f
        CHARACTER(LEN=*),intent(in)              :: CS_y
        REAL(RP), DIMENSION(this%m2)             :: result_DCT_FFTW_y
        !-------------------------------------------------------------------------
        IF (this%nY /= 1) THEN
            this%in_y = f(1:this%nY)
            CALL execute_y(this, CS_y)
            result_DCT_FFTW_y(1:this%nY) = this%out_y * this%s_2_f_y
        ELSE
            result_DCT_FFTW_y = f
        END IF
    END FUNCTION DCT_FFTW_y

    FUNCTION IDCT_FFTW_y(this, f, CS_y) result(result_IDCT_FFTW_y)
        IMPLICIT NONE
        ! Input variables, output variable ---------------------------------------
        class(typFFFWHosNWT), intent(inout)      :: this
        REAL(RP), DIMENSION(this%m2),intent(in)  :: f
        CHARACTER(LEN=*), intent(in)             :: CS_y
        REAL(RP), DIMENSION(this%m2)             :: result_IDCT_FFTW_y
        !-------------------------------------------------------------------------
        IF (this%nY /= 1) THEN
            this%in_y = f(1:this%nY) * this%f_2_s_y
            CALL execute_y(this, CS_y)
            result_IDCT_FFTW_y(1:this%nY) = this%out_y
        ELSE
            result_IDCT_FFTW_y = f
        END IF

    END FUNCTION IDCT_FFTW_y

    SUBROUTINE execute_y(this, CS_y)
        IMPLICIT NONE
        ! Input variables --------------------------------------------------------
        class(typFFFWHosNWT), intent(inout) :: this
        CHARACTER(LEN=*),intent(in)         :: CS_y
        ! Local variables
        type(C_PTR)                         :: plan
        !-------------------------------------------------------------------------
        plan = select_plan_y(this, CS_y)

        IF (CS_y == 'sin') THEN
            this%in_y(1)       = 0.0_rp
            this%in_y(this%nY) = 0.0_rp
        END IF

        CALL dfftw_execute(plan)

        IF (CS_y == 'sin') THEN
            this%out_y(1)       = 0.0_rp
            this%out_y(this%nY) = 0.0_rp
        END IF
    END SUBROUTINE execute_y

    FUNCTION select_plan_y(this, CS_y) result(result_select_plan_y)
        IMPLICIT NONE
        ! Input variables --------------------------------------------------------
        class(typFFFWHosNWT), intent(inout) :: this
        CHARACTER(LEN=*),intent(in)         :: CS_y
        ! Output variable
        type(C_PTR)                         :: result_select_plan_y
        !-------------------------------------------------------------------------
        IF ((CS_y /= 'cos') .AND. (CS_y /= 'sin')) STOP 'Unknown y-transform in Fourier_FFTW:select_plan'

        IF (CS_y == 'cos') THEN
            result_select_plan_y = this%plan_Cy
        ELSE IF (CS_y == 'sin') THEN
            result_select_plan_y = this%plan_Sy
        END IF

    END FUNCTION select_plan_y

    FUNCTION DCT_FFTW_add(this, f, CS_z, CS_y) result(result_DCT_FFTW_add)
        IMPLICIT NONE
        ! Input variables, output variable ---------------------------------------
        class(typFFFWHosNWT), intent(inout)                 :: this
        REAL(RP), DIMENSION(this%m3_add,this%m2),intent(in) :: f
        CHARACTER(LEN=*),intent(in)                         :: CS_z, CS_y
        REAL(RP), DIMENSION(this%m3_add,this%m2)            :: result_DCT_FFTW_add
        !-------------------------------------------------------------------------
        this%in_add = f(1:this%nAdd,1:this%nY)
        CALL execute_add(this, CS_z, CS_y, 1)
        result_DCT_FFTW_add(1:this%nAdd,1:this%nY) = this%out_add * this%s_2_f_add
    END FUNCTION DCT_FFTW_add

    FUNCTION IDCT_FFTW_add(this, f, CS_z, CS_y) result(result_IDCT_FFTW_add)
        IMPLICIT NONE
        ! Input variables, output variable -----------------------------------------
        class(typFFFWHosNWT), intent(inout)                  :: this
        REAL(RP), DIMENSION(this%m3_add, this%m2),intent(in) :: f
        CHARACTER(LEN=*),intent(in)                          :: CS_z, CS_y
        REAL(RP), DIMENSION(this%m3_add, this%m2)            :: result_IDCT_FFTW_add
        !---------------------------------------------------------------------------
        this%in_add = f(1:this%nAdd,1:this%nY) * this%f_2_s_add
        CALL execute_add(this, CS_z, CS_y, -1)
        result_IDCT_FFTW_add(1:this%nAdd,1:this%nY) = this%out_add
    END FUNCTION IDCT_FFTW_add

    SUBROUTINE execute_add(this, CS_z, CS_y, FB)
        IMPLICIT NONE
        ! Input variables --------------------------------------------------------
        class(typFFFWHosNWT), intent(inout) :: this
        CHARACTER(LEN=*),intent(in)         :: CS_z, CS_y
        INTEGER,intent(in)                  :: FB
        ! Local variables
        type(C_PTR)                         :: plan
        !-------------------------------------------------------------------------
        plan = select_plan_add(this, CS_z, CS_y, FB)

        IF ((this%nY /= 1) .AND. (CS_y == 'sin')) THEN
            this%in_add(:,1)       = 0.0_rp
            this%in_add(:,this%nY) = 0.0_rp
        END IF

        CALL dfftw_execute(plan)

        IF ((this%nY /= 1) .AND. (CS_y == 'sin')) THEN
            this%out_add(:,1)       = 0.0_rp
            this%out_add(:,this%nY) = 0.0_rp
        END IF

    END SUBROUTINE execute_add

    FUNCTION select_plan_add(this, CS_z, CS_y, FB) result(result_select_plan_add)
        IMPLICIT NONE
        ! Input variables --------------------------------------------------------
        class(typFFFWHosNWT), intent(inout) :: this
        CHARACTER(LEN=*),intent(in)         :: CS_z, CS_y
        INTEGER,intent(in)                  :: FB
        ! Output variable
        type(C_PTR)                         :: result_select_plan_add
        !-------------------------------------------------------------------------
        IF ((CS_z /= 'cos') .AND. (CS_z /= 'sin')) STOP 'Unknown z-transform in Fourier_FFTW:select_plan_add'
        IF ((CS_y /= 'cos') .AND. (CS_y /= 'sin')) STOP 'Unknown y-transform in Fourier_FFTW:select_plan_add'
        IF ((FB   /= 1)     .AND. (FB   /= -1))    STOP 'Unknown direction in Fourier_FFTW:select_plan_add'

        IF (this%nY == 1) THEN
            IF (FB == 1) THEN
                IF (CS_z == 'cos') THEN
                    result_select_plan_add = this%plan_CC_add
                ELSE IF (CS_z == 'sin') THEN
                    result_select_plan_add = this%plan_SC_add
                ELSE
                    STOP 'Unknown z-transform in Fourier_FFTW:select_plan_add'
                    result_select_plan_add = this%plan_CC_add
                END IF
            ELSE IF (FB == -1) THEN
                IF (CS_z == 'cos') THEN
                    result_select_plan_add = this%plan_CC_add_I
                ELSE IF (CS_z == 'sin') THEN
                    result_select_plan_add = this%plan_SC_add_I
                ELSE
                    STOP 'Unknown z-transform in Fourier_FFTW:select_plan_add'
                    result_select_plan_add = this%plan_CC_add
                END IF
            ELSE
                STOP 'Unknown direction in Fourier_FFTW:select_plan_add'
                result_select_plan_add = this%plan_CC_add
            END IF
        ELSE
            IF (FB == 1) THEN
                IF ((CS_z == 'cos') .AND. (CS_y == 'cos')) THEN
                    result_select_plan_add = this%plan_CC_add
                ELSE IF ((CS_z == 'sin') .AND. (CS_y == 'cos')) THEN
                    result_select_plan_add = this%plan_SC_add
                ELSE IF ((CS_z == 'cos') .AND. (CS_y == 'sin')) THEN
                    result_select_plan_add = this%plan_CS_add
                ELSE IF ((CS_z == 'sin') .AND. (CS_y == 'sin')) THEN
                    result_select_plan_add = this%plan_SS_add
                ELSE
                    STOP 'Unknown y,z-transform in Fourier_FFTW:select_plan_add'
                    result_select_plan_add = this%plan_CC_add
                END IF
            ELSE IF (FB == -1) THEN
                IF ((CS_z == 'cos') .AND. (CS_y == 'cos')) THEN
                    result_select_plan_add = this%plan_CC_add_I
                ELSE IF ((CS_z == 'sin') .AND. (CS_y == 'cos')) THEN
                    result_select_plan_add = this%plan_SC_add_I
                ELSE IF ((CS_z == 'cos') .AND. (CS_y == 'sin')) THEN
                    result_select_plan_add = this%plan_CS_add_I
                ELSE IF ((CS_z == 'sin') .AND. (CS_y == 'sin')) THEN
                    result_select_plan_add = this%plan_SS_add_I
                ELSE
                    STOP 'Unknown y,z-transform in Fourier_FFTW:select_plan_add'
                    result_select_plan_add = this%plan_CC_add
                END IF
            ELSE
                STOP 'Unknown y,z-transform and Direction in Fourier_FFTW:select_plan_add'
                result_select_plan_add = this%plan_CC_add
            END IF
        END IF
    END FUNCTION select_plan_add

    ! Fourier transforms on 'dealiased' domain i.e. extended
    FUNCTION DCT_FFTW_big(this, f, CS_x, CS_y) result(RESULT_DCT_FFTW_big)
        IMPLICIT NONE
        ! Input variables, output variable ---------------------------------------
        Class(typFFFWHosNWT), intent(inout) :: this
        REAL(RP), DIMENSION(this%md1,this%md2), intent(in) :: f
        CHARACTER(LEN=*), intent(in) :: CS_x, CS_y
        REAL(RP), DIMENSION(this%md1,this%md2)             :: RESULT_DCT_FFTW_big
        !-------------------------------------------------------------------------
        this%in_big   = f(1:this%nd1,1:this%nd2)
        CALL execute_big(this, CS_x, CS_y)
        RESULT_DCT_FFTW_big(1:this%nd1,1:this%nd2) = this%out_big * this%s_2_f_big
    END FUNCTION DCT_FFTW_big

    FUNCTION IDCT_FFTW_big(this, f, CS_x, CS_y) result(RESULT_IDCT_FFTW_big)
        IMPLICIT NONE
        ! Input variables, output variable ---------------------------------------
        Class(typFFFWHosNWT), intent(inout) :: this
        REAL(RP), DIMENSION(this%md1,this%md2), intent(in) :: f
        CHARACTER(LEN=*),intent(in) :: CS_x, CS_y
        REAL(RP), DIMENSION(this%md1,this%md2)             :: RESULT_IDCT_FFTW_big
        !-------------------------------------------------------------------------
        this%in_big   = f(1:this%nd1,1:this%nd2) * this%f_2_s_big
        CALL execute_big(this, CS_x, CS_y)
        RESULT_IDCT_FFTW_big(1:this%nd1,1:this%nd2) = this%out_big
    END FUNCTION IDCT_FFTW_big

    SUBROUTINE execute_big(this, CS_x, CS_y)
        IMPLICIT NONE
        ! Input variables --------------------------------------------------------
        Class(typFFFWHosNWT), intent(inout) :: this
        CHARACTER(LEN=*), intent(in)        :: CS_x, CS_y
        ! Local variables
        type(C_PTR)                         :: plan
        !-------------------------------------------------------------------------
        plan = select_plan_big(this, CS_x, CS_y)
        IF ((this%nY /= 1) .AND. (CS_y == 'sin')) THEN
            this%in_big(:,1)        = 0.0_rp
            this%in_big(:,this%nd2) = 0.0_rp
        END IF

        IF (CS_x == 'sin') this%in_sin_big = this%in_big(2:this%nd1-1,:)
        call dfftw_execute(plan)
        IF (CS_x == 'sin') THEN
            this%out_big(1,:)            = 0.0_rp
            this%out_big(2:this%nd1-1,:) = this%out_sin_big
            this%out_big(this%nd1,:)     = 0.0_rp
        END IF

        IF ((this%nY /= 1) .AND. (CS_y == 'sin')) THEN
            this%out_big(:,1)        = 0.0_rp
            this%out_big(:,this%nd2) = 0.0_rp
        END IF
    END SUBROUTINE execute_big

    FUNCTION select_plan_big(this, CS_x, CS_y) result(result_select_plan_big)
        IMPLICIT NONE
        ! Input variables --------------------------------------------------------
        Class(typFFFWHosNWT), intent(inout) :: this
        CHARACTER(LEN=*),intent(in)         :: CS_x, CS_y
        ! Output variable
        type(C_PTR)                         :: result_select_plan_big
        !-------------------------------------------------------------------------
        IF ((CS_x /= 'cos') .AND. (CS_x /= 'sin')) STOP 'Unknown x-transform in Fourier_FFTW:select_plan'
        IF ((CS_y /= 'cos') .AND. (CS_y /= 'sin')) STOP 'Unknown y-transform in Fourier_FFTW:select_plan'

        IF (this%nY == 1) THEN
            IF (CS_x == 'cos') THEN
                result_select_plan_big = this%plan_CC_big
            ELSE IF (CS_x == 'sin') THEN
                result_select_plan_big = this%plan_SC_big
            ELSE
                STOP 'Unknown x-transform in Fourier_FFTW:select_plan'
                result_select_plan_big = this%plan_CC_big
            END IF
        ELSE
            IF ((CS_x == 'cos') .AND. (CS_y == 'cos')) THEN
                result_select_plan_big = this%plan_CC_big
            ELSE IF ((CS_x == 'sin') .AND. (CS_y == 'cos')) THEN
                result_select_plan_big = this%plan_SC_big
            ELSE IF ((CS_x == 'cos') .AND. (CS_y == 'sin')) THEN
                result_select_plan_big = this%plan_CS_big
            ELSE IF ((CS_x == 'sin') .AND. (CS_y == 'sin')) THEN
                result_select_plan_big = this%plan_SS_big
            ELSE
                STOP 'Unknown x,y-transform in Fourier_FFTW:select_plan'
                result_select_plan_big = this%plan_CC_big
            END IF
        END IF

    END FUNCTION select_plan_big

END MODULE modFourier_r2c_FFTW3_NWT
