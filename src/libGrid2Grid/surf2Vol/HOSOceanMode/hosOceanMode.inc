subroutine allocOceanMode(this, nXo2p1, nYmode)
    class(typHOSOceanMode), intent(inout) :: this
    integer, intent(in) :: nXo2p1, nYmode

    this%nXo2p1 = nXo2p1
    this%nYmode = nYmode

    allocate(this%modeX(nXo2p1, nYmode),&
             this%modeY(nXo2p1, nYmode),&
             this%modeZ(nXo2p1, nYmode),&
             this%modet(nXo2p1, nYmode),&
             this%modeFS(nXo2p1, nYmode),&
             this%modeFSt(nXo2p1, nYmode) )

    !-- Allocate wave number array
    allocate( this%kx(nXo2p1) )
    allocate( this%kyN2(nYmode) )
    allocate( this%kxy(nXo2p1, nYmode) )
    allocate( this%ktheta(nXo2p1, nYmode) )

    allocate( this%ikx(nXo2p1, nYmode) )
    allocate( this%iky(nXo2p1, nYmode) )

end subroutine

subroutine destroyOceanMode(this)
    class(typHOSOceanMode), intent(inout) :: this
    if (allocated(this%modeX)) deallocate( this%modeX )
    if (allocated(this%modeY)) deallocate( this%modeY )
    if (allocated(this%modeZ)) deallocate( this%modeZ )

    if (allocated(this%modet))   deallocate( this%modet )
    if (allocated(this%modeFS))  deallocate( this%modeFS )
    if (allocated(this%modeFSt)) deallocate( this%modeFSt )

    if (allocated(this%kx))     deallocate( this%kx )
    if (allocated(this%kyN2))   deallocate( this%kyN2 )
    if (allocated(this%ktheta)) deallocate( this%ktheta )
    if (allocated(this%kxy))    deallocate( this%kxy )

    if (allocated(this%ikx)) deallocate( this%ikx )
    if (allocated(this%iky)) deallocate( this%iky )
end subroutine

subroutine finalHOSOceanMode(this)
    type(typHOSOceanMode), intent(inout) :: this
    Call this%destroy
end subroutine
