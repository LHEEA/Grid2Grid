    Module modCommG2G
        use iso_c_binding
        Implicit None

        !! Dummy Character Length
        Integer,parameter :: procNChar = 1000

        !! Shared Object Path
        Character(len=procNChar) :: libGrid2GridPath = "../../lib/libGrid2Grid.so"

        !! Is Shared Library is loaded
        logical :: isGrid2GridLoad = .FALSE.

        !! Fortran Subroutine C Header
        Character(len=procNChar),parameter :: headerG2G = "__modgrid2grid_MOD_"

        !! Communication Type
        type commSub
            type(c_funptr)           :: proc_addr
            Character(len=procNChar) :: proc_name
        end type

        type(commSub) :: comm_init
        type(commSub) :: comm_correct
        type(commSub) :: comm_getEta
        type(commSub) :: comm_getU
        type(commSub) :: comm_getPd
        type(commSub) :: comm_getFlow
        type(commSub) :: comm_getEndTime
        type(commSub) :: comm_getWaveDepth
        type(commSub) :: comm_isInit

        ! interface to linux API
        integer(c_int), parameter :: rtld_lazy=1 ! value extracte from the C header file
        integer(c_int), parameter :: rtld_now=2  ! value extracte from the C header file

        interface
            function dlopen(filename, mode) bind(c,name="dlopen")
            ! void *dlopen(const char *filename, int mode);
            use iso_c_binding
            implicit none
            type(c_ptr) :: dlopen
            character(c_char), intent(in) :: filename(*)
            integer(c_int), value :: mode
            end function

            function dlsym(handle, name) bind(c,name="dlsym")
            ! void *dlsym(void *handle, const char *name);
            use iso_c_binding
            implicit none
            type(c_funptr) :: dlsym
            type(c_ptr), value :: handle
            character(c_char), intent(in) :: name(*)
            end function

            function dlclose(handle) bind(c,name="dlclose")
            ! int dlclose(void *handle);
            use iso_c_binding
            implicit none
            integer(c_int) :: dlclose
            type(c_ptr), value :: handle
            end function
        end interface

        ! Define interface of call-back routine.
        abstract interface
            subroutine proc_init(hosSolver, hosFileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio, hosIndex) &
                bind(c)
                use, intrinsic :: iso_c_binding
                implicit none
                integer, parameter :: nChar = 300
                character(kind=c_char, len=1), dimension(nChar),intent(in) :: hosSolver
                character(kind=c_char, len=1), dimension(nChar),intent(in) :: hosFileName
                Double precision, intent(in)   :: zMin, zMax
                integer, intent(in)            :: nZmin, nZmax
                Double precision, intent(in)   :: zMinRatio, zMaxRatio
                integer, intent(out)           :: hosIndex
            end subroutine proc_init

            subroutine proc_correct(hosIndex, simulTime) bind(c)
                use, intrinsic :: iso_c_binding
                implicit none
                integer, intent(in)          :: hosIndex
                Double precision, intent(in) :: simulTime
            end subroutine

            subroutine proc_getHOSEta(hosIndex, x, y, t, eta) bind(c)
                use, intrinsic :: iso_c_binding
                implicit none
                integer, intent(in)           :: hosIndex
                Double precision, intent(in)  :: x, y, t
                Double precision, intent(out) ::  eta
            end subroutine

            subroutine proc_getHOSU(hosIndex, x, y, z, t, u, v, w) bind(c)
                use, intrinsic :: iso_c_binding
                implicit none
                integer, intent(in)           :: hosIndex
                Double precision, intent(in)  :: x, y, z, t
                Double precision, intent(out) :: u, v, w
            end subroutine

            subroutine proc_getHOSPd(hosIndex, x, y, z, t, pd) bind(c)
                use, intrinsic :: iso_c_binding
                implicit none
                integer, intent(in)           :: hosIndex
                Double precision, intent(in)  :: x, y, z, t
                double precision, intent(out) :: pd
            end subroutine

            subroutine proc_getHOSFlow(hosIndex, x, y, z, t, eta, u, v, w, pd) bind(c)
                use, intrinsic :: iso_c_binding
                implicit none
                integer, intent(in)           :: hosIndex
                double precision, intent(in)  :: x, y, z, t
                double precision, intent(out) :: eta, u, v, w, pd
            end subroutine

            subroutine proc_getHOSEndTime(hosIndex, endTime) bind(c)
                use, intrinsic :: iso_c_binding
                implicit none
                integer, intent(in)           :: hosIndex
                double precision, intent(out) :: endTime
            end subroutine

            subroutine proc_getHOSWaterDepth(hosIndex, waterDepth) bind(c)
                use, intrinsic :: iso_c_binding
                implicit none
                integer, intent(in)           :: hosIndex
                double precision, intent(out) :: waterDepth
            end subroutine

            subroutine proc_isGrid2GridInitialized(hosIndex, isG2Initialized) bind(c)
                use, intrinsic :: iso_c_binding
                implicit none
                integer, intent(in)  :: hosIndex
                logical, intent(out) :: isG2Initialized
            end subroutine

        end interface

        type(c_ptr) :: handle
        procedure(proc_init), bind(c), pointer       :: subG2G_init
        procedure(proc_correct), bind(c), pointer    :: subG2G_correct
        procedure(proc_getHOSEta), bind(c), pointer  :: subG2G_getHOSEta
        procedure(proc_getHOSU), bind(c), pointer    :: subG2G_getHOSU
        procedure(proc_getHOSPd), bind(c), pointer   :: subG2G_getHOSPd
        procedure(proc_getHOSFlow), bind(c), pointer :: subG2G_getHOSFlow
        procedure(proc_getHOSEndTime), bind(c), pointer :: subG2G_getHOSEndTime
        procedure(proc_getHOSWaterDepth), bind(c), pointer :: subG2G_getHOSWaterDepth
        procedure(proc_isGrid2GridInitialized), bind(c), pointer :: subG2G_isGrid2GridInitialized

    contains

        subroutine callGrid2Grid(grid2gridPath)
            implicit none
            character(len=*), intent(in) :: grid2gridPath
            character(len=procNChar)   :: subroutineName

            libGrid2GridPath = ''
            libGrid2GridPath = grid2gridPath

            handle=dlopen(trim(libGrid2GridPath)//c_null_char, RTLD_LAZY)
            if (.not. c_associated(handle))then
                write(*,*), 'Unable to load shared library : ', trim(libGrid2GridPath)
                isGrid2GridLoad = .FALSE.
                stop
            else
                write(*,*), 'Load shared library : ', trim(libGrid2GridPath)
                isGrid2GridLoad = .TRUE.

                subroutineName = trim(headerG2G)//"initializegrid2grid"
                Call linkG2GSubroutine(comm_init, subroutineName)
                call c_f_procpointer(comm_init%proc_addr, subG2G_init)

                subroutineName = trim(headerG2G)//"correctgrid2grid"
                Call linkG2GSubroutine(comm_correct, subroutineName)
                call c_f_procpointer(comm_correct%proc_addr, subG2G_correct)

                subroutineName = trim(headerG2G)//"gethoseta"
                Call linkG2GSubroutine(comm_getEta, subroutineName)
                call c_f_procpointer(comm_getEta%proc_addr, subG2G_getHOSEta)

                subroutineName = trim(headerG2G)//"gethosu"
                Call linkG2GSubroutine(comm_getU, subroutineName)
                call c_f_procpointer(comm_getU%proc_addr, subG2G_getHOSU)

                subroutineName = trim(headerG2G)//"gethospd"
                Call linkG2GSubroutine(comm_getPd, subroutineName)
                call c_f_procpointer(comm_getPd%proc_addr, subG2G_getHOSPd)

                subroutineName = trim(headerG2G)//"gethosflow"
                Call linkG2GSubroutine(comm_getFlow, subroutineName)
                call c_f_procpointer(comm_getFlow%proc_addr, subG2G_getHOSFlow)

                subroutineName = trim(headerG2G)//"gethosendtime"
                Call linkG2GSubroutine(comm_getEndTime, subroutineName)
                call c_f_procpointer(comm_getEndTime%proc_addr, subG2G_getHOSEndTime)

                subroutineName = trim(headerG2G)//"gethoswaterdepth"
                Call linkG2GSubroutine(comm_getWaveDepth, subroutineName)
                call c_f_procpointer(comm_getWaveDepth%proc_addr, subG2G_getHOSWaterDepth)

                subroutineName = trim(headerG2G)//"isgrid2gridinitialized"
                Call linkG2GSubroutine(comm_isInit, subroutineName)
                call c_f_procpointer(comm_isInit%proc_addr, subG2G_isGrid2GridInitialized)

            end if

        end subroutine

        subroutine linkG2GSubroutine(this, subName)
            Implicit None
            type(commSub),intent(inout)         :: this
            Character(len=procNChar),intent(in) :: subName
            this%proc_name = trim(subName)
            if (isGrid2GridLoad) then
                this%proc_addr=dlsym(handle, trim(subName)//c_null_char)
                if (.not. c_associated(this%proc_addr))then
                    write(*,*) 'Unable to load the procedure', trim(subName)
                    stop
                end if
            else
                write(*,*) "libGrid2Grid.so is not loaded. Please run callGrid2Grid first"
                stop
            end if
        End Subroutine

        ! Subroutine
        subroutine initializeGrid2Grid(hosSolver, hosFileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio, hosIndex)
            implicit None
            integer, parameter              :: nChar = 300
            character(len=nChar),intent(in) :: hosSolver
            character(len=nChar),intent(in) :: hosFileName
            Double precision, intent(in)    :: zMin, zMax
            integer, intent(in)             :: nZmin, nZmax
            Double precision, intent(in)    :: zMinRatio, zMaxRatio
            integer, intent(out)            :: hosIndex
            !! ----------------------------------------------
            character(kind=c_char, len=1), dimension(nChar) :: charC_hosSolver
            character(kind=c_char, len=1), dimension(nChar) :: charC_hosFileName
            integer :: i
            !! C Character and Fortran Character is Different !!!
            charC_hosSolver = ''
            do i = 1, nChar
                if (hosSolver(i:i) == '' ) then
                    exit
                else
                    charC_hosSolver(i) = hosSolver(i:i)
                end if
            enddo
            charC_hosFileName = ''
            do i = 1, nChar
                if (hosFileName(i:i) == '' ) then
                    exit
                else
                    charC_hosFileName(i) = hosFileName(i:i)
                end if
            enddo
            call subG2G_init(charC_hosSolver, charC_hosFileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio, hosIndex)
        end subroutine

        subroutine correctGrid2Grid(hosIndex, simulTime)
            implicit None
            integer, intent(in)          :: hosIndex
            Double precision, intent(in) :: simulTime
            Call subG2G_correct(hosIndex, simulTime)
        end subroutine

        subroutine getHOSeta(hosIndex, x, y, t, eta)
            implicit none
            integer, intent(in)           :: hosIndex
            Double precision, intent(in)  :: x, y, t
            Double precision, intent(out) ::  eta
            Call subG2G_getHOSEta(hosIndex, x, y, t, eta)
        end subroutine

        subroutine getHOSU(hosIndex, x, y, z, t, u, v, w)
            implicit none
            integer, intent(in)           :: hosIndex
            Double precision, intent(in)  :: x, y, z, t
            Double precision, intent(out) :: u, v, w
            Call subG2G_getHOSU(hosIndex, x, y, z, t, u, v, w)
        end subroutine

        subroutine getHOSPd(hosIndex, x, y, z, t, pd)
            implicit none
            integer, intent(in)           :: hosIndex
            double precision, intent(in)  :: x, y, z, t
            double precision, intent(out) :: pd
            Call subG2G_getHOSPd(hosIndex, x, y, z, t, pd)
        end subroutine

        subroutine getHOSFlow(hosIndex, x, y, z, t, eta, u, v, w, pd)
            implicit none
            integer, intent(in)           :: hosIndex
            double precision, intent(in)  :: x, y, z, t
            double precision, intent(out) :: eta, u, v, w, pd
            Call subG2G_getHOSFlow(hosIndex, x, y, z, t, eta, u, v, w, pd)
        end subroutine

        subroutine getHOSEndTime(hosIndex, endTime)
            implicit none
            integer, intent(in)           :: hosIndex
            double precision, intent(out) :: endTime
            Call subG2G_getHOSEndTime(hosIndex, endTime)
        end subroutine

        subroutine getHOSWaterDepth(hosIndex, waterDepth)
            implicit none
            integer, intent(in)           :: hosIndex
            double precision, intent(out) :: waterDepth
            Call subG2G_getHOSWaterDepth(hosIndex, waterDepth)
        end subroutine

        subroutine isGrid2GridInitialized(hosIndex, isG2Initialized)
            implicit none
            integer, intent(in)  :: hosIndex
            logical, intent(out) :: isG2Initialized
            Call subG2G_isGrid2GridInitialized(hosIndex, isG2Initialized)
        end subroutine

    End Module
