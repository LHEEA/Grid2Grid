    Program Main
        use iso_c_binding
        use modCommG2G
        Implicit None

        Integer,Parameter      :: nChar = 300
        integer                :: hosIndex
        Character(len = nChar) :: hosSolver
        Character(len = nChar) :: hosFileName
        Character(len = nChar) :: grid2gridPath
        Double precision       :: zMin, zMax
        integer                :: nZmin, nZmax
        Double precision       :: zMinRatio, zMaxRatio

        Double precision       :: t, dt
        Double precision       :: x, y, z
        Double precision       :: eta, u, v, w, pd
        integer                :: it

        write(*,*) "Test program (Intel Fortran) to use Grid2Grid shared library"

        grid2gridPath = "../../obj/libGrid2Grid.so"
        Call callGrid2Grid(grid2gridPath)

        hosIndex = -1

        hosSolver = "Ocean"
        hosFileName = "../../data-modes_HOS_SWENSE.dat/hosOcean_2DIrr_modes_HOS_SWENSE.dat"

        zMin = -0.6d0
        zMax =  0.6d0

        nZmin = 50
        nZmax = 50

        zMinRatio = 3.d0
        zMaxRatio = 3.d0

        Call initializeGrid2Grid(hosSolver, hosFileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio, hosIndex)

        !! Time Information
        t  = 0.0d0
        dt = 0.1d0

        !! Given Point
        x = 0.5d0
        y = 0.5d0
        z = -0.5d0

        !! Time Loop
        do it = 1,10

            !! Correct HOS Vol2VOl for given time
            Call correctGrid2Grid(hosIndex, t)

            !! Get Wave Elevation
            Call getHOSeta(hosIndex, x, y , t, eta)

            !! Get Flow Velocity
            Call getHOSU(hosIndex, x, y, z, t, u, v ,w)

            !! Get Dynamic Pressure
            Call getHOSPd(hosIndex, x, y, z, t, pd)

            !! Write Flow Information
            write(*,*) t, eta, u, v, w, pd

            !! Time Update
            t = t + dt
        enddo

        write(*,*) "Test program (Intel Fortran) is done ..."

    End Program
