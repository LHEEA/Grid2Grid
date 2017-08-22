!-----------------------------------------------------------------------
Module modGrid2Grid
!-----------------------------------------------------------------------
!
!   Grid2Grid Module to connect OpenFOAM (C++ code)
!
!-----------------------------------------------------------------------
!
!   Copyright (C) 2017 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598
!
!-----------------------------------------------------------------------
use iso_c_binding
use modGrid2GridType
use modVol2Vol
!-----------------------------------------------------------------------
Implicit None
!-----------------------------------------------------------------------

Type(typHOSVol2Vol) :: hosVol2Vol_

contains

    subroutine initializeGrid2Grid(hosSolver, hosFileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio) &
        bind(c, name='__modgrid2grid_MOD_initializegrid2grid')
        implicit None
        integer, parameter :: nChar = 300
        character(kind=c_char, len=1), dimension(nChar),intent(in) :: hosSolver
        character(kind=c_char, len=1), dimension(nChar),intent(in) :: hosFileName
        Double precision, intent(in)   :: zMin, zMax
        integer, intent(in)            :: nZmin, nZmax
        Double precision, intent(in)   :: zMinRatio, zMaxRatio
        !! --------------------------------------------------------------------------
        character(len=nChar)           :: charF_hosSolver
        character(len=nChar)           :: charF_hosFileName
        integer :: i

        !! C Character and Fortran Character is Different !!!
        charF_hosSolver = ''
        do i = 1, nChar
            if (hosSolver(i) == c_null_char ) then
                exit
            else
                charF_hosSolver(i:i) = hosSolver(i)
            end if
        enddo

        charF_hosFileName = ''
        do i = 1, nChar
            if (hosFileName(i) == c_null_char ) then
                exit
            else
                charF_hosFileName(i:i) = hosFileName(i)
            end if
        enddo

        !! Call HOS Vol2VOL initialize
        Call hosVol2Vol_%initialize(charF_hosSolver, charF_hosFileName, &
                                    real(zMin, RP), real(zMax, RP), &
                                    nZmin, nZmax, &
                                    real(zMinRatio, RP), real(zMaxRatio, RP))

    end subroutine

    subroutine correctGrid2Grid(simulTime) &
        bind(c, name='__modgrid2grid_MOD_correctgrid2grid')
        implicit None
        Double precision, intent(in) :: simulTime
        Call hosVol2Vol_%correct(real(simulTime,RP))
    end subroutine

    subroutine getHOSeta(x, y, t, eta) &
        bind(c, name='__modgrid2grid_MOD_gethoseta')
        implicit none
        Double precision, intent(in) :: x, y, t
        Double precision, intent(out) ::  eta
        real(rp)         :: tmp_eta
        tmp_eta = hosVol2Vol_%getEta(real(x,rp), real(y, rp), real(t, rp))
        eta = dble(tmp_eta)
    end subroutine

    subroutine getHOSU(x, y, z, t, u, v, w) &
        bind(c, name='__modgrid2grid_MOD_gethosu')
        implicit none
        Double precision, intent(in)  :: x, y, z, t
        Double precision, intent(out) :: u, v, w
        real(rp)          :: tmp_u, tmp_v, tmp_w
        Call hosVol2Vol_%getU(real(x,rp), real(y, rp), real(z, rp), real(t, rp), tmp_u, tmp_v, tmp_w)
        u = dble(tmp_u)
        v = dble(tmp_v)
        w = dble(tmp_w)
    end subroutine

    subroutine getHOSPd(x, y, z, t, pd) &
        bind(c, name='__modgrid2grid_MOD_gethospd')
        implicit none
        double precision, intent(in)  :: x, y, z, t
        double precision, intent(out) :: pd
        real(rp) :: tmp_pd
        tmp_pd = hosVol2Vol_%getPd(real(x,rp), real(y, rp), real(z, rp), real(t, rp))
        pd = dble(tmp_pd)
    end subroutine

    subroutine getHOSFlow(x, y, z, t, eta, u, v, w, pd) &
        bind(c, name='__modgrid2grid_MOD_gethosflow')
        implicit none
        double precision, intent(in)  :: x, y, z, t
        double precision, intent(out) :: eta, u, v, w, pd
        real(rp)          :: tmp_eta, tmp_u, tmp_v, tmp_w, tmp_pd
        Call hosVol2Vol_%getFlow(real(x,rp), real(y, rp), real(z, rp), real(t, rp), &
                                 tmp_eta, tmp_u, tmp_v, tmp_w, tmp_pd)
        eta = dble(tmp_eta)
        u = dble(tmp_u)
        v = dble(tmp_v)
        w = dble(tmp_w)
        pd = dble(tmp_pd)
    end subroutine

    subroutine getHOSEndTime(endTime) &
        bind(c, name='__modgrid2grid_MOD_gethosendtime')
        implicit none
        double precision, intent(out) :: endTime
        if (hosVol2Vol_%isInitialized_) then
            endTime = dble(hosVol2Vol_%endTime_)
        else
            write(*,*) "    [Warning] Grid2Grid is not initialized, Grid2Grid::getEndTime(endTime)"
        end if
    end subroutine

    subroutine getHOSWaterDepth(waterDepth) &
        bind(c, name='__modgrid2grid_MOD_gethoswaterdepth')
        implicit none
        double precision, intent(out) :: waterDepth
        if (hosVol2Vol_%isInitialized_) then
            waterDepth = dble(hosVol2Vol_%waterDepth_)
        else
            write(*,*) "    [Warning] Grid2Grid is not initialized, Grid2Grid::getWaterDepth(waterDepth)"
        end if
    end subroutine

    subroutine isGrid2GridInitialized(isG2Initialized) &
        bind(c, name='__modgrid2grid_MOD_isgrid2gridinitialized')
        implicit none
        logical, intent(out) :: isG2Initialized
        isG2Initialized = hosVol2Vol_%isInitialized_
    end subroutine

!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
