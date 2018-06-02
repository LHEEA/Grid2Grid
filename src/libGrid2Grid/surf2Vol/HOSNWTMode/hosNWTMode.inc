Subroutine allocNWTMode(this, nXmode, nYmode, nAddmode)
    implicit none
    class(typHOSNWTmode), intent(inout) :: this
    integer, intent(in) :: nXmode, nYmode, nAddmode

    this%nXMode   = nXmode
    this%nYMode   = nYmode
    this%nAddMode = nAddmode

    allocate( this%modeX(nXmode, nYmode) )
    allocate( this%modeY(nXmode, nYmode) )
    allocate( this%modeZ(nXmode, nYmode) )
    allocate( this%modet(nXmode, nYmode) )
    allocate( this%modeFS(nXmode, nYmode) )
    allocate( this%modeFSt(nXmode, nYmode) )
    allocate( this%modeFSx(nXmode, nYmode) )
    allocate( this%modeFSy(nXmode, nYmode) )

    allocate( this%modeAdd(nAddmode, nYmode) )
    allocate( this%modeAddt(nAddmode, nYmode) )

    allocate( this%kx(nXmode) )
    allocate( this%ky(nXmode) )
    allocate( this%kxy(nXmode, nYmode) )
    allocate( this%ktheta(nXmode, nYmode) )
    allocate( this%kxAdd(nAddmode) )

end subroutine

Subroutine destroyNWTMode(this)
    implicit none
    class(typHOSNWTmode), intent(inout) :: this
    if (allocated(this%modeX))    deallocate(this%modeX)
    if (allocated(this%modeY))    deallocate(this%modeY)
    if (allocated(this%modeZ))    deallocate(this%modeZ)
    if (allocated(this%modet))    deallocate(this%modet)

    if (allocated(this%modeFS))   deallocate(this%modeFS)
    if (allocated(this%modeFSt))  deallocate(this%modeFSt)
    if (allocated(this%modeFSx))  deallocate(this%modeFSx)
    if (allocated(this%modeFSy))  deallocate(this%modeFSy)
    if (allocated(this%modeAdd))  deallocate(this%modeAdd)
    if (allocated(this%modeAddt)) deallocate(this%modeAddt)
    if (allocated(this%kx))       deallocate(this%kx)
    if (allocated(this%ky))       deallocate(this%ky)
    if (allocated(this%kxy))      deallocate(this%kxy)
    if (allocated(this%ktheta))   deallocate(this%ktheta)
    if (allocated(this%kxAdd))    deallocate(this%kxAdd)
End Subroutine

subroutine finalHOSNWTMode(this)
    implicit none
    type(typHOSNWTmode), intent(inout) :: this
    Call this%destroy()
end subroutine
