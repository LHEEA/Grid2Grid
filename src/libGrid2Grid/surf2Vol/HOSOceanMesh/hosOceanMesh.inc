subroutine buildHOSOceanMesh(this, nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax, zMinRatio, zMaxRatio)
    implicit none
    class(typHOSOceanMesh), intent(inout) :: this
    real(rp),intent(in)  :: nonXLen, nonYLen, nonDimzMin, nonDimzMax
    integer,intent(in)   :: nX, nY, nZmin, nZmax
    real(rp), optional   :: zMinRatio, zMaxRatio
    integer :: ix, iy, iz, jz

    !-- Allocate HOS Mesh Array
    Call this%allocateHOSArray(nX, nY, nZmin, nZmax)

    this%nonDimLx = nonXLen
    this%nonDimLy = nonYLen

    this%nonDimZMin = nonDimzMin
    this%nonDimZMax = nonDimzMax

    this%Lx = this%nonDimLx * this%dimL
    this%Ly = this%nonDimLy * this%dimL

    this%zMin = this%nonDimZMin * this%dimL
    this%zMax = this%nonDimZMax * this%dimL

    this%zMinRatio = 3.0_RP
    if (present(zMinRatio).and.(zMinRatio.gt.0.0)) then
        this%zMinRatio = zMinRatio
    end if

    this%zMaxRatio = 3.0_RP
    if (present(zMaxRatio).and.(zMaxRatio.gt.0.0)) then
        this%zMaxRatio = zMaxRatio
    end if

    !-- Check Parameters
    if ( (Nx.le.0).or.(Ny.le.0)) then
        write(*,*) "    [Error] buildHOSOceanMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
        write(*,*) "        nX or nY should be larger than 0 !"
        stop
    end if

    if ( (nZmin.le.1).or.(nZmax.le.1)) then
        write(*,*) "    [Error] buildHOSOceanMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
        write(*,*) "        nZmin or nZmax should be larger than 0 !"
        stop
    end if

    if ( (nonXLen.le.0).or.(nonYLen.le.0)) then
        write(*,*) "    [Error] buildHOSOceanMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
        write(*,*) "        nonXLen or nonYLen should be larger than 0 !"
        stop
    end if

    if ( (-nonDimzMin.le.0).or.(nonDimzMax.le.0)) then
        write(*,*) "    [Error] buildHOSOceanMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
        write(*,*) "        -nonDimzMin or nonDimzMax should be larger than 0 !"
        stop
    end if

    !!- delta x
    this%nonDimDx = this%nonDimLx / this%nX

    !! x node position
    do ix = 1,this%nX
        this%nonDimX(ix) = (ix - 1.d0) * this%nonDimDx
    enddo

    !!- delta y
    if (this%nY == 1) then
        this%nonDimDy = 0.0_RP
    else
        this%nonDimDy = this%nonDimLy / this%nY
    endif

    !! y node position
    do iy = 1,this%nY
        this%nonDimY(iy) = (iy - 1.d0) * this%nonDimDy
    enddo

    !! z node position
    Call buildZmesh(this%nonDimZMin, &
                    this%nonDimZMax, &
                    this%nZmin, &
                    this%nZmax, &
                    this%nonDimZ, &
                    INDEX_GEOMETRICRATIO_MESH, &
                    this%zMinRatio, &
                    this%zMaxRatio)

end subroutine

subroutine destroyOceanMesh(this)
    implicit none
    class(typHOSOceanMesh), intent(inout) :: this
    Call this%destroyHOSMesh
    if (allocated(this%aVitX)) deallocate( this%aVitX )
    if (allocated(this%aVitY)) deallocate( this%aVitY )
    if (allocated(this%aVitZ)) deallocate( this%aVitZ )
    if (allocated(this%aPhit)) deallocate( this%aPhit )

    if (allocated(this%aDuDt)) deallocate( this%aDuDt )
    if (allocated(this%aDvDt)) deallocate( this%aDvDt )
    if (allocated(this%aDwDt)) deallocate( this%aDwDt )
end subroutine

subroutine finalHOSOceanMesh(this)
    implicit none
    type(typHOSOceanMesh), intent(inout) :: this
    Call this%destroyOceanMesh
end subroutine
