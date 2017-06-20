MODULE modPost_processing_ocean
!
! This is the main program for post-processing HOS-ocean computations
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    Copyright (C) 2014 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598
!
!    This program is part of HOS-ocean
!
!    HOS-ocean is free software: you can redistribute it and/or modify
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
!
USE modType
USE modVariables_3d_ocean
USE modInput_post_process_ocean
USE modRead_files_ocean
USE modAnalysis_wavefield_ocean
USE modOutput_post_process_ocean
USE modReconstruction_ocean
USE modFourier_r2c_FFTW3_ocean
!
IMPLICIT NONE
!
!
CONTAINS
!    
!
SUBROUTINE Post_processing_ocean
!
!
REAL(RP) :: dt_out, time, time_prev
REAL(RP), ALLOCATABLE, DIMENSION(:) :: H_up, L_up, H_down, L_down, crest, trough
INTEGER, ALLOCATABLE, DIMENSION(:)  :: idx_up, idx_down, idx_crest, idx_trough
! Transverse
REAL(RP), ALLOCATABLE, DIMENSION(:) :: L_up_t
INTEGER, ALLOCATABLE, DIMENSION(:)  :: idx_crest_t,idx_up_t
INTEGER  :: i_unit,n_waves
REAL(RP) :: H_1_3rd_up, H_1_3rd_down, H_lim
!
! For moments
REAL(RP) :: ave_eta,adev_eta,sdev_eta,var_eta,skew_eta,kurt_eta
REAL(RP), ALLOCATABLE, DIMENSION(:) :: data
!
! For freak waves
INTEGER :: n_freak
REAL(RP), ALLOCATABLE, DIMENSION(:) :: H_freak,L_freak,x_freak
INTEGER,ALLOCATABLE, DIMENSION(:)   :: idx_freak
! Transverse
REAL(RP), ALLOCATABLE, DIMENSION(:) :: L_freak_t,y_freak
INTEGER,ALLOCATABLE, DIMENSION(:)   :: idx_freak_t
!
! For SWENSE-type outputs + velocity/pressure cards
!
!INTEGER :: nz
COMPLEX(CP), ALLOCATABLE, DIMENSION(:,:) :: modesspecx,modesspecy,modesspecz,modesspect,modesFS,modesFSt
REAL(RP), ALLOCATABLE, DIMENSION(:)      :: xvect,yvect,zvect
REAL(RP), ALLOCATABLE, DIMENSION(:,:)    :: vitx,vity,vitz,phit,dudt,dvdt,dwdt,zlocal
!
REAL(RP) :: dt_out_star,T_stop_star,xlen_star,ylen_star,depth_star,g_star,L,T
INTEGER  :: imin,imax,jmin,jmax,i_xvect,i_yvect
INTEGER  :: i1,i2,i3,i_test
REAL(RP) :: tiny_sp
!
! tiny_sp is single precision: useful for inequalities check with values read from files
tiny_sp = epsilon(1.0)
!
!
! Initialize outputs
CALL init_output_post_process(i_ana,i_card)
!
!
! Velocities and pressure inside domain
IF (i_card /= 0) THEN
    !
    i_unit = 201
    !
    ! Initialize computations of volumic informations
    ! Everything is non-dimensional in file_mod
    !
    CALL recons_HOS_init(file_mod,i_unit,n1,n2,dt_out_star,T_stop_star,xlen_star,ylen_star,depth_star,g_star,L,T, &
        modesspecx,modesspecy,modesspecz,modesspect,modesFS,modesFSt)
    !
    n1o2p1 = n1/2+1
    n2o2p1 = n2/2+1
    !
    ! Initialize Fourier (global variables have to be defined)
    m1      = n1
    m2      = n2
    Nd1     = 1
    Nd2     = 1
    Nd1o2p1 = 1
    m1o2p1  = n1o2p1
    md1o2p1 = 1
    md1     = 1
    md2     = 1
    !
    ! Initialize Fourier transforms (FFTW)
    CALL fourier_ini(3)
    !
    ! Check (x_min, x_max, y_min, y_max) w.r.t. domain size
    ! + time window (t_min, t_max)
    CALL check_sizes(n2,x_min,x_max,y_min,y_max,z_min,T_start,T_stop,xlen_star,ylen_star,depth_star,T_stop_star,L,T)
    !
    ALLOCATE(x(n1),y(n2),kx(n1o2p1),ky_n2(n2),ikx(n1o2p1,n2),iky(n1o2p1,n2),kth(n1o2p1,n2),eta(n1,n2))
    !
    ! Initialize mesh in physical and modal space (whole domain in HOS-ocean)
    CALL build_mesh_global(xlen_star,ylen_star,depth_star,n1,n2,x,y,kx,ky_n2,ikx,iky,kth)
    !
    ! Define local meshes for zone of study
    CALL build_mesh_local(x_min,x_max,y_min,y_max,z_min,z_max,xlen_star,ylen_star,L,n1,n2,i_zvect, &
        xvect,yvect,zvect,imin,imax,jmin,jmax)
    !
    ! Reconstruction of fields
    ! First ALLOCATE the matrices
    i_xvect = imax-imin+1
    i_yvect = jmax-jmin+1
    !
    ALLOCATE(zlocal(i_xvect, i_yvect), vitx(i_xvect, i_yvect), vity(i_xvect, i_yvect), vitz(i_xvect, i_yvect), &
        phit(i_xvect, i_yvect), dudt(i_xvect, i_yvect), dvdt(i_xvect, i_yvect), dwdt(i_xvect, i_yvect))
    !
    ! Define first time as the closest to T_start (input file)
    time      = NINT(T_start/T/dt_out_star)*dt_out_star
    time_prev = 0.0_rp
    !
    DO WHILE (time*T <= T_stop+tiny_sp)
        !
        WRITE(*,'(A,ES8.1)') 'time = ',time*T
        !
        ! It reads the corresponding time in file_mod (closest to time)
        IF (time >= dt_out_star/2) THEN
            CALL read_mod(file_mod,i_unit,time,dt_out_star,n1o2p1,n2, &
                modesspecx,modesspecy,modesspecz,modesspect,modesFS,modesFSt)
        ENDIF
        !
		! For outputs
		CALL fourier_2_space(modesFS,eta)
        !
        ! Make a loop over all the elements in z
        DO i3 = 1, i_zvect
            IF (i_card == 1) THEN
                ! Construct the field at each zvect...
                CALL reconstruction_FFTs(modesspecx,modesspecy,modesspecz,modesspect,modesFS, &
                    imin,imax,jmin,jmax,zvect(i3),depth_star,vitx,vity,vitz,phit,dudt,dvdt,dwdt)
                ! Necessary for output
                DO i1=1, imax-imin+1
                    DO i2=1, jmax-jmin+1
                        zlocal(i1,i2) = zvect(i3)
                    ENDDO
                ENDDO
            ELSEIF (i_card == 2) THEN
                ! Construct the field at each zvect...
                CALL reconstruction_direct(modesspecx,modesspecy,modesspecz,modesspect,modesFS, &
                    imin,imax,jmin,jmax,z_min/L,i3,i_zvect,depth_star,vitx,vity,vitz,phit,dudt,dvdt,dwdt,zlocal)
            ENDIF
            ! Test to know if it is first z-element to write corresponding header
            IF (i3 == 1) THEN
                i_test = 1
            ELSE
                i_test = 0
            ENDIF
            ! Output of time-step
            CALL output_time_step_card(i_card,tecplot,time,dt_out_star,zlocal,z_min,z_max,T_start,g_star,L,T,i_test,&
                imin,imax,jmin,jmax,i_zvect,vitx,vity,vitz,phit,eta)
        ENDDO
        !
        ! next time-step
        time_prev = time
        time      = time + dt_out_star
        !
    ENDDO
    !
    ! Close all files (including those open in output...
    CLOSE(i_unit)
    CLOSE(30)
    CLOSE(31)
    CLOSE(32)
ENDIF
! End of main program
!
END SUBROUTINE Post_processing_ocean
!
!
!
END MODULE modPost_processing_ocean
