!-----------------------------------------------------------------------
Module  modSurf2vol
!-----------------------------------------------------------------------
!
!   Wrapper module of HOS (surf2vol)
!
!-----------------------------------------------------------------------
!
!   Copyright (C) 2017 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598
!
!   This program based on Post Processing Part of HOS.
!
!-----------------------------------------------------------------------
!
!       Program intialized by Maite Gouin.
!       Port to modulized format by YoungMyung Choi.
!
!-----------------------------------------------------------------------
use modGrid2GridType
use modNWTsurf2vol
use modOceansurf2vol

Implicit none

    type typHOSSurf2Vol

        !!! - Variables

        !!- Solver Name (NWT or Ocean)
        character(len=StringLength) :: HOSSolver_

        !!- Initialization logical Value
        Logical :: isInitialized_ = .FALSE.

        !!- HOS NWT surf2Vol Type
        type(typHOSNWT)             :: hosNWTSurf2Vol_

        !!- HOS Ocean surf2Vol Type
        type(typHOSOcean)           :: hosOceanSurf2Vol_

        !!- HOS Mesh Pointer
        class(typHOSMesh),allocatable :: ptrHOSMesh_

        !!- HOS delta t and end Time (dimensionalzed)
        real(rp) :: dt, endTime

        !!- Water Depth
        real(rp) :: waterDepth_

        !!- HOS correct Index
        integer :: correctIdx_ = -1

        contains

        !!! - Subroutine

        !!- Initialize HOS surf2Vol
        procedure, public :: initialize => initHOS

        !!- Correct HOS surf2vol and Point HOS Mesh
        procedure, public :: correct => correctHOS

        !!- Destroy HOS surf2vol
        procedure, public :: destroy => destroySurfVol

        final :: finalSurf2Vol

    end type

    contains

        !!- Initialize HOS surf2Vol
        subroutine initHOS(this, solver, fileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio)
            implicit none
            class(typHOSSurf2Vol),intent(inout) :: this
            character(len=*),intent(in) :: solver
            character(len=*),intent(in) :: fileName
            real(RP), intent(in)        :: zMin, zMax
            integer, intent(in)         :: nZmin, nZmax
            real(rp),optional, intent(in) :: zMinRatio, zMaxRatio

            if (this%isInitialized_) return

            !!- Solver Check (HOS Ocean or HOS NWT)
            if ((solver.ne."NWT").and.(solver.ne."Ocean")) then
                write(*,*) "    [ERROR] Grid2Grid, typHOSSurf2Vol::initHOS(solver, fileName, zMin, zMax, nZmin, nZmax) "
                write(*,*) "        Wrong solver is given when HOS surf2Vol initilize"
                write(*,*) "        given solver : ", solver
                stop
            end if

            !!- Set Solver Name
            this%HOSSolver_ = solver

            if (zMin.ge.0.0_RP) then
                write(*,*) "    [ERROR] Grid2Grid, typHOSSurf2Vol::initHOS(solver, fileName, zMin, zMax, nZmin, nZmax) "
                write(*,*) "        zMin should be negative !"
                write(*,*) "        given zMin : ", zMin
                stop
            end if

            if (zMax.le.0.0_RP) then
                write(*,*) "    [ERROR] Grid2Grid, typHOSSurf2Vol::initHOS(solver, fileName, zMin, zMax, nZmin, nZmax) "
                write(*,*) "        zMax should be positive !"
                write(*,*) "        given zMax : ", zMax
                stop
            end if

            !!- Initialize Solver Class
            if (this%HOSSolver_.eq."NWT") then
                Call this%hosNWTSurf2Vol_%initialize(fileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio)
            else if (this%HOSSolver_.eq."Ocean") then
                Call this%hosOceanSurf2Vol_%initialize(fileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio)
            endif

            !!- Set Simulation Time Variables
            if (this%HOSSolver_.eq."NWT") then
                this%dt      = this%hosNWTSurf2Vol_%dtOut_
                this%endTime = this%hosNWTSurf2Vol_%Tstop_
                this%waterDepth_ = this%hosNWTSurf2Vol_%depth_
            else if (this%HOSSolver_.eq."Ocean") then
                this%dt      = this%hosOceanSurf2Vol_%dtOut_
                this%endTime = this%hosOceanSurf2Vol_%Tstop_
                this%waterDepth_ = this%hosOceanSurf2Vol_%depth_
            end if

            Call correctHOS(this, 0)

            this%isInitialized_ = .TRUE.

            Call writeSurf2VolMeshVTK(this)

        end subroutine

        !!- Correct HOS surf2Vol Class and Point HOS Mesh
        subroutine correctHOS(this, iTime)
            implicit none
            class(typHOSSurf2Vol),intent(inout) :: this
            integer,intent(in)                  :: iTime

            !!- Correct HOS NWT or Ocean
            if (this%HOSSolver_.eq."NWT") then
                Call this%hosNWTSurf2Vol_%correct(iTime)
            else if (this%HOSSolver_.eq."Ocean") then
                Call this%hosOceanSurf2Vol_%correct(iTime)
            endif

            !!- Point HOS mesh quantities to access flow quantities
            if (allocated(this%ptrHOSMesh_)) then
                deallocate(this%ptrHOSMesh_)
            end if

            if (this%HOSSolver_.eq."NWT") then
                allocate(this%ptrHOSMesh_, source =this%hosNWTSurf2Vol_%hosMesh_)
            else if (this%HOSSolver_.eq."Ocean") then
                allocate(this%ptrHOSMesh_, source =this%hosOceanSurf2Vol_%hosMesh_)
            endif

            this%correctIdx_ = iTime

        end subroutine

        subroutine destroySurfVol(this)
            implicit none
            class(typHOSSurf2Vol),intent(inout) :: this
            this%isInitialized_ = .FALSE.
            !! If HOS mesh pointer is allocated
            if (allocated(this%ptrHOSMesh_)) then
                deallocate(this%ptrHOSMesh_)
            end if
            !! Destroy HOS Surf2Vol
            if (this%HOSSolver_.eq."NWT") then
                Call this%hosNWTSurf2Vol_%destroy
            else if (this%HOSSolver_.eq."Ocean") then
                Call this%hosOceanSurf2Vol_%destroy
            endif
        end subroutine

        subroutine finalSurf2Vol(this)
            implicit none
            type(typHOSSurf2Vol),intent(inout) :: this
            Call this%destroy
        end subroutine

        subroutine writeSurf2VolMeshVTK(this)
            implicit none
            class(typHOSSurf2Vol),intent(inout) :: this
            type(typFileIO) :: vtkFile
            integer :: iUnit, ix, iy, iz

            if (isSurf2VolVTKWrite.EQV..FALSE.) then

            isSurf2VolVTKWrite = .TRUE.

            !! Make Output Path
            vtkFile%name = trim(surf2VolVTK)
            CALL System("mkdir -p "//trim(vtkFile%name))
            vtkFile%name = trim(vtkFile%name)//"surf2volMesh.vtk"
            vtkFile%unit = callFileUnit()

            iUnit = vtkFile%unit

            open(vtkFile%unit, &
                 file = trim(vtkFile%name), &
                 status='replace')

            write(vtkFile%unit,1001) "# vtk DataFile Version 3.0"
            write(vtkFile%unit,1001) "vtk output"
            write(vtkFile%unit,1001) "ASCII"
            write(vtkFile%unit,1001) "DATASET RECTILINEAR_GRID"
            write(vtkFile%unit,1002) "DIMENSIONS", this%ptrHOSMesh_%nX, this%ptrHOSMesh_%nY, this%ptrHOSMesh_%nZ

            write(vtkFile%unit,1003) "X_COORDINATES", this%ptrHOSMesh_%nX, " float"
            write(vtkFile%unit,*) ( this%ptrHOSMesh_%dimL * this%ptrHOSMesh_%nonDimX(ix), ix =1, this%ptrHOSMesh_%nX )

            write(vtkFile%unit,1003) "Y_COORDINATES", this%ptrHOSMesh_%nY, " float"
            write(vtkFile%unit,*) ( this%ptrHOSMesh_%dimL * this%ptrHOSMesh_%nonDimY(iy), iy =1, this%ptrHOSMesh_%nY )

            write(vtkFile%unit,1003) "Z_COORDINATES", this%ptrHOSMesh_%nZ, " float"
            write(vtkFile%unit,*) ( this%ptrHOSMesh_%dimL * this%ptrHOSMesh_%nonDimZ(iz), iz =1, this%ptrHOSMesh_%nZ )

            !! Close File
            close(vtkFile%unit)
            vtkFile%unit = RemoveFileUnit()

            endif

            1001 format(a)
            1002 format(a,3(i15))
            1003 format(a,i15,a)

        end Subroutine

!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
