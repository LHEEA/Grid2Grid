Program main
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

    Implicit None
    Character(Len =100) :: inputFileName
    inputFileName = "postGrid2Grid.inp"

    ! Call testPost(inputFileName)

    ! Call testSurf2Vol()

    ! Call testVol2Vol()

    Call testDict()

End Program

Subroutine testDict()

    use mfpGeneral

    Implicit None

    type(typDictionaryPtr)  :: dict

    Call dict%initialize("/home/cyoungmy/OpenFOAM/FoamStar/ThirdParty/Grid2Grid/auxiliary/FyMcPack/", "testDict","")

End Subroutine


!! - Test Post Processing
Subroutine testPost(inputFileName)
    use modPostGrid2Grid
    implicit none
    Character(Len = * ),intent(in) :: inputFileName
    Type(typPostGrid2Grid)         :: postG2G

    !! Post Processing Inialize
    Call postG2G%initialize(inputFileName)

    !! Do Post Processing
    Call postG2G%doPostProcessing()

    Call postG2G%destroy

end Subroutine

!! - Test HOS Vol2VOL
Subroutine testVol2Vol
    Use modGrid2GridType
    Use modVol2Vol
    Implicit None
    Type(typHOSVol2Vol) :: hosV2V
    character(len=100)  :: fileName1, fileName2
    integer             :: nZmin, nZmax
    real(RP)            :: zMin, zMax
    real(RP)            :: dt,t
    real(RP)            :: x, y, z
    real(RP)            :: eta, u, v, w, pd
    integer             :: it
    ! -------------------------------------------------------------------------

    !! - HOS Surf2Vol z-directional mesh Information
    zMin = -0.6
    zMax =  0.6

    nZmin = 50
    nZmax = 50

    !! Time Information
    dt = 0.1_RP

    !! Given Point
    x = 0.5_RP
    y = 0.5_RP
    z = -0.5_RP

    !!! ----------------------------------------------------------------------
    write(*,*) ""
    write(*,*) "HOS Ocean Vol2Vol ..."
    write(*,*) ""

    !! - HOS Ocean Intialize
    fileName1 = "data-modes_HOS_SWENSE.dat/hosOcean_2DIrr_modes_HOS_SWENSE.dat"
    Call hosV2V%initialize('Ocean',fileName1, zMin, zMax, nZmin, nZmax)

    t  = 0.0_RP
    !! Time Loop
    do it = 1,10

        !! Correct HOS Vol2VOl for given time
        Call hosV2V%correct(t)

        !! Get Wave Elevation
        eta = hosV2V%getEta(x, y ,t)

        !! Get Flow Velocity
        Call hosV2V%getU(x, y, z, t, u, v ,w)

        !! Get Dynamic Pressure
        pd = hosV2V%getpd(x, y, z, t)

        !! Write Flow Information
        write(*,*) t, eta, u, v, w, pd

        !! Time Update
        t = t + dt
    enddo

    Call hosV2V%destroy

    !!! ----------------------------------------------------------------------
    write(*,*) ""
    write(*,*) "HOS NWT Vol2Vol ..."
    write(*,*) ""

    !! - HOS NWT Intialize

    fileName2 = "data-modes_HOS_SWENSE.dat/hosNWT_2DIrr_modes_HOS_SWENSE.dat"
    Call hosV2V%initialize('NWT',fileName2, zMin, zMax, nZmin, nZmax)

    t  = 0.0_RP
    !! Time Loop
    do it = 1,10

        !! Correct HOS Vol2VOl for given time
        Call hosV2V%correct(t)

        !! Get Wave Elevation
        eta = hosV2V%getEta(x, y ,t)

        !! Get Flow Velocity
        Call hosV2V%getU(x, y, z, t, u, v ,w)

        !! Get Dynamic Pressure
        pd = hosV2V%getpd(x, y, z, t)

        !! Write Flow Information
        write(*,*) t, eta, u, v, w, pd

        !! Time Update
        t = t + dt
    enddo

    Call hosV2V%destroy

    write(*,*) ""

End Subroutine

!! - Test HOS surf2vol
subroutine testSurf2Vol
    use modSurf2vol
    Implicit none
    character(len=100)   :: fileName1, fileName2    !! HOS Result File Name
    integer              :: nZmin, nZmax            !! HOS Surf2Vol Z-Domain
    real(RP)             :: zMin, zMax              !! HOS Number of Z-Mesh

    type(typHOSSurf2Vol) :: hosS2V                  !! HOS Surf2Vol Class
    integer              :: iTime, ix, iy, iz       !! time, space index of HOS surf2vol mesh
    real(RP)             :: eta, u, v, w, pd        !! Flow information

    !! -----------------------------------------------------------------------
    !!! - HOS Ocean or NWT
    !! -----------------------------------------------------------------------

    !! - HOS Surf2Vol z-directional mesh Information

    nZmin = 50
    nZmax = 50
    zMin = -0.6
    zMax = 0.6

    !!! ----------------------------------------------------------------------
    write(*,*) ""
    write(*,*) "HOS Ocean Surf2Vol ..."
    write(*,*) ""

    !! - Initialize HOS Ocean Surf2Vol
    fileName1 = "data-modes_HOS_SWENSE.dat/hosOcean_2DIrr_modes_HOS_SWENSE.dat"

    call hosS2V%initialize('Ocean',fileName1, zMin, zMax, nZmin, nZmax)

    do iTime = 0, 15        !! HOS time Index

        !! - Correct time index
        call hosS2V%correct(iTime)

        ix = 20     ! HOS Surf2Vol Mesh x - Index
        iy = 1      ! HOS Surf2Vol Mesh y - Index
        iz = 8      ! HOS Surf2Vol Mesh z - Index (1 : Sea Bottom, nZ : Top)

        !! - Get HOS Surf2Vol Flow Information for given mesh and time index
        eta = hosS2V%ptrHOSMesh_%eta(ix, iy)
        u   = hosS2V%ptrHOSMesh_%u(ix, iy, iz)
        v   = hosS2V%ptrHOSMesh_%v(ix, iy, iz)
        w   = hosS2V%ptrHOSMesh_%w(ix, iy, iz)
        pd  = hosS2V%ptrHOSMesh_%pd(ix, iy, iz)

        write(*,*) iTime, eta, u, v, w, pd

    enddo

    Call hosS2V%destroy

    !!! ----------------------------------------------------------------------
    write(*,*) ""
    write(*,*) "HOS NWT Surf2Vol ..."
    write(*,*) ""

    ! - Initialize HOS NWT Surf2Vol
    fileName2 = "data-modes_HOS_SWENSE.dat/hosNWT_3DReg_modes_HOS_SWENSE.dat"

    call hosS2V%initialize('NWT',fileName2, zMin, zMax, nZmin, nZmax)

    do iTime = 0, 2        !! HOS time Index

        !! - Correct time index
        call hosS2V%correct(iTime)

        ix = 20     ! HOS Surf2Vol Mesh x - Index
        iy = 1      ! HOS Surf2Vol Mesh y - Index
        iz = 8      ! HOS Surf2Vol Mesh z - Index (1 : Sea Bottom, nZ : Top)

        !! - Get HOS Surf2Vol Flow Information for given mesh and time index
        eta = hosS2V%ptrHOSMesh_%eta(ix, iy)
        u   = hosS2V%ptrHOSMesh_%u(ix, iy, iz)
        v   = hosS2V%ptrHOSMesh_%v(ix, iy, iz)
        w   = hosS2V%ptrHOSMesh_%w(ix, iy, iz)
        pd  = hosS2V%ptrHOSMesh_%pd(ix, iy, iz)

        write(*,*) iTime, eta, u, v, w, pd

    enddo

    Call hosS2V%destroy

    write(*,*) ""

    !!! ----------------------------------------------------------------------

end subroutine
