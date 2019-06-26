Subroutine allocNWTMode(this, nXmode, nYmode, nAddmode, procedure)
    implicit none
    class(typHOSNWTmode), intent(inout) :: this
    integer, intent(in) :: nXmode, nYmode, nAddmode
    character(len=*), optional :: procedure
    !...
    character(len=300) :: reconstProcedure

    this%nXMode   = nXmode
    this%nYMode   = nYmode
    this%nAddMode = nAddmode

    reconstProcedure = "velocity"

    if ( present(procedure) ) then
        reconstProcedure = procedure
    end if

    if (reconstProcedure=='velocity') then
      allocate( this%modeX(nXmode, nYmode) )
      allocate( this%modeY(nXmode, nYmode) )
      allocate( this%modeZ(nXmode, nYmode) )
      this%modeX = 0.0_RP
      this%modeY = 0.0_RP
      this%modeZ = 0.0_RP
    elseif (reconstProcedure=='potential') then
      allocate( this%mode(nXmode, nYmode) )
      this%mode = 0.0_RP
    endif

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


    this%modet = 0.0_RP
    this%modeFS = 0.0_RP
    this%modeFSx = 0.0_RP
    this%modeFSy = 0.0_RP
    this%modeFSt = 0.0_RP

end subroutine

Subroutine destroyNWTMode(this)
    implicit none
    class(typHOSNWTmode), intent(inout) :: this
    if (allocated(this%mode))     deallocate(this%mode)
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
