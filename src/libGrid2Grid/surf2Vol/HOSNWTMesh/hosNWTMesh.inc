!!!- Build HOS Mesh
subroutine buildHOSNWTMesh(this, nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax, zMinRatio, zMaxRatio)
    implicit none
    class(typHOSNWTMesh), intent(inout) :: this
    real(rp),intent(in)      :: nonXLen, nonYLen, nonDimzMin, nonDimzMax
    integer,intent(in)       :: nX, nY, nZmin, nZmax
    real(rp),optional        :: zMinRatio, zMaxRatio
    integer :: ix, iy, iz, jz
    !----------------------------------

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
        write(*,*) "    [Error] buildHOSNWTMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
        write(*,*) "        nX or nY should be larger than 0 !"
        stop
    end if

    if ( (nZmin.le.1).or.(nZmax.le.1)) then
        write(*,*) "    [Error] buildHOSNWTMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
        write(*,*) "        nZmin or nZmax should be larger than 0 !"
        stop
    end if

    if ( (nonXLen.le.0).or.(nonYLen.le.0)) then
        write(*,*) "    [Error] buildHOSNWTMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
        write(*,*) "        nonXLen or nonYLen should be larger than 0 !"
        stop
    end if

    if ( (-nonDimzMin.le.0).or.(nonDimzMax.le.0)) then
        write(*,*) "    [Error] buildHOSNWTMesh(nonXLen, nonYLen, nonDimzMin, nonDimzMax, nX, nY, nZmin, nZmax)"
        write(*,*) "        -nonDimzMin or nonDimzMax should be larger than 0 !"
        stop
    end if

    !!- delta x
    this%nonDimDx = this%nonDimLx / (this%nX - 1.d0)

    !! x node position
    do ix = 1,this%nX
        this%nonDimX(ix) = (ix - 1.d0) * this%nonDimDx
    enddo

    !!- delta y
    if (this%nY == 1) then
        this%nonDimDy = 0.d0
    else
        this%nonDimDy = this%nonDimLy / (this%nY - 1.d0)
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

subroutine destroyHOSNWTMesh(this)
    implicit none
    class(typHOSNWTMesh), intent(inout) :: this
    Call this%destroyHOSMesh        !! Destroy Base Type

    if ( allocated(this%nonDimAddPhiX) ) deallocate( this%nonDimAddPhiX )
    if ( allocated(this%nonDimAddPhiY) ) deallocate( this%nonDimAddPhiY )
    if ( allocated(this%nonDimAddPhiZ) ) deallocate( this%nonDimAddPhiZ )
    if ( allocated(this%nonDimAddPhit) ) deallocate( this%nonDimAddPhit )

    if ( allocated(this%nonDimAdddvitXdx) ) deallocate( this%nonDimAdddvitXdx )
    if ( allocated(this%nonDimAdddvitYdY) ) deallocate( this%nonDimAdddvitYdY )
    if ( allocated(this%nonDimAdddvitXdZ) ) deallocate( this%nonDimAdddvitXdZ )
    if ( allocated(this%nonDimAdddvitYdZ) ) deallocate( this%nonDimAdddvitYdZ )
    if ( allocated(this%nonDimAdddvitXdY) ) deallocate( this%nonDimAdddvitXdY )

    if ( allocated(this%nonDimAddDuDt) ) deallocate( this%nonDimAddDuDt )
    if ( allocated(this%nonDimAddDvDt) ) deallocate( this%nonDimAddDvDt )
    if ( allocated(this%nonDimAddDwDt) ) deallocate( this%nonDimAddDwDt )

    if ( allocated(this%csh_add_x) )        deallocate( this%csh_add_x )
    if ( allocated(this%k_add_sh_add_x) )   deallocate( this%k_add_sh_add_x )
    if ( allocated(this%kycsh_add_x) )      deallocate( this%kycsh_add_x )
    if ( allocated(this%kx_add_csh_add_x) ) deallocate( this%kx_add_csh_add_x )

    if ( allocated(this%aVitX) ) deallocate( this%aVitX )
    if ( allocated(this%aVitY) ) deallocate( this%aVitY )
    if ( allocated(this%aVitZ) ) deallocate( this%aVitZ )
    if ( allocated(this%aPhit) ) deallocate( this%aPhit )

    if ( allocated(this%adVitXdx) ) deallocate( this%adVitXdx )
    if ( allocated(this%adVitYdy) ) deallocate( this%adVitYdy )
    if ( allocated(this%adVitXdy) ) deallocate( this%adVitXdy )
    if ( allocated(this%adVitXdz) ) deallocate( this%adVitXdz )
    if ( allocated(this%adVitYdz) ) deallocate( this%adVitYdz )

    if ( allocated(this%aDuDt) ) deallocate( this%aDuDt )
    if ( allocated(this%aDvDt) ) deallocate( this%aDvDt )
    if ( allocated(this%aDwDt) ) deallocate( this%aDwDt )

    if ( allocated(this%lVitX) ) deallocate( this%lVitX )
    if ( allocated(this%lVitY) ) deallocate( this%lVitY )
    if ( allocated(this%lVitZ) ) deallocate( this%lVitZ )
    if ( allocated(this%lPhit) ) deallocate( this%lPhit )

    if ( allocated(this%ldVitXdx) ) deallocate( this%ldVitXdx )
    if ( allocated(this%ldVitYdy) ) deallocate( this%ldVitYdy )
    if ( allocated(this%ldVitXdy) ) deallocate( this%ldVitXdy )
    if ( allocated(this%ldVitXdz) ) deallocate( this%ldVitXdz )
    if ( allocated(this%ldVitYdz) ) deallocate( this%ldVitYdz )

    if ( allocated(this%lDuDt) ) deallocate( this%lDuDt )
    if ( allocated(this%lDvDt) ) deallocate( this%lDvDt )
    if ( allocated(this%lDwDt) ) deallocate( this%lDwDt )
end subroutine

subroutine finalHOSNWTMesh(this)
    implicit none
    type(typHOSNWTMesh), intent(inout) :: this
    Call this%destroyHOSNWTMesh
end subroutine
