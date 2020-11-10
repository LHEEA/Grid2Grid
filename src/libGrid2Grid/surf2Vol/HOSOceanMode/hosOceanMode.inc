subroutine allocOceanMode(this, nXo2p1, nYmode, procedure)
    class(typHOSOceanMode), intent(inout) :: this
    integer, intent(in) :: nXo2p1, nYmode
    character(len=*), optional :: procedure
    !!...
    character(len=300) :: reconstProcedure

    this%nXo2p1 = nXo2p1
    this%nYmode = nYmode

    reconstProcedure = "velocity"

    if ( present(procedure) ) then
        reconstProcedure = procedure
    end if

    if (reconstProcedure=='velocity') then
        allocate(this%modeX(nXo2p1, nYmode),&
                 this%modeY(nXo2p1, nYmode),&
                 this%modeZ(nXo2p1, nYmode))
        this%modeX = 0.0_RP
        this%modeY = 0.0_RP
        this%modeZ = 0.0_RP
    elseif (reconstProcedure=='potential') then
        allocate(this%mode(nXo2p1, nYmode))
        this%mode = 0.0_RP
    endif

    allocate(this%modet(nXo2p1, nYmode),&
             this%modeFS(nXo2p1, nYmode),&
             this%modeFSx(nXo2p1, nYmode),&
             this%modeFSy(nXo2p1, nYmode),&
             this%modeFSt(nXo2p1, nYmode) )

    !-- Allocate wave number array
    allocate( this%kx(nXo2p1) )
    allocate( this%kyN2(nYmode) )
    allocate( this%kxy(nXo2p1, nYmode) )
    allocate( this%ktheta(nXo2p1, nYmode) )

    allocate( this%ikx(nXo2p1, nYmode) )
    allocate( this%iky(nXo2p1, nYmode) )

    allocate( this%omega(nXo2p1, nYmode) )

    allocate( this%weight(nXo2p1, nYmode) )

    this%modet = 0.0_RP
    this%modeFS = 0.0_RP
    this%modeFSx = 0.0_RP
    this%modeFSy = 0.0_RP
    this%modeFSt = 0.0_RP

    this%weight(:, :) = 1.0_RP

end subroutine

subroutine destroyOceanMode(this)
    class(typHOSOceanMode), intent(inout) :: this
    if (allocated(this%mode))  deallocate( this%mode  )
    if (allocated(this%modeX)) deallocate( this%modeX )
    if (allocated(this%modeY)) deallocate( this%modeY )
    if (allocated(this%modeZ)) deallocate( this%modeZ )

    if (allocated(this%modet))   deallocate( this%modet )
    if (allocated(this%modeFS))  deallocate( this%modeFS )
    if (allocated(this%modeFSt)) deallocate( this%modeFSt )
    if (allocated(this%modeFSx)) deallocate( this%modeFSx )
    if (allocated(this%modeFSy)) deallocate( this%modeFSy )

    if (allocated(this%kx))     deallocate( this%kx )
    if (allocated(this%kyN2))   deallocate( this%kyN2 )
    if (allocated(this%ktheta)) deallocate( this%ktheta )
    if (allocated(this%kxy))    deallocate( this%kxy )

    if (allocated(this%ikx)) deallocate( this%ikx )
    if (allocated(this%iky)) deallocate( this%iky )

    if (allocated(this%omega)) deallocate( this%omega )
    if (allocated(this%weight)) deallocate( this%weight )
end subroutine

subroutine finalHOSOceanMode(this)
    type(typHOSOceanMode), intent(inout) :: this
    Call this%destroy
end subroutine
