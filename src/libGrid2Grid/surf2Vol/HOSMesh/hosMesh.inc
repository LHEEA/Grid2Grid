!!!- Allocate HOS Mesh Array
subroutine allocateHOSArray(this, nX, nY, nZmin, nZmax)
    implicit none
    class(typHOSMesh) :: this
    integer, intent(in) :: nX, nY, nZmin, nZmax

    this%nX = nX
    this%nY = nY
    this%nZmin = nZmin
    this%nZmax = nZmax

    this%nZ = this%nZmin + this%nZmax

    !!- Allocate array
    allocate( this%nonDimX(this%nX) )
    allocate( this%nonDimY(this%nY) )
    allocate( this%nonDimZ(this%nZ) )

    !!- Allocate flow quantity array -----

    !... flow quantities in real domain
    allocate( this%nonDimEta(this%nX, this%nY) )
    allocate( this%nonDimPhiX(this%nX, this%nY, this%nZ) )
    allocate( this%nonDimPhiY(this%nX, this%nY, this%nZ) )
    allocate( this%nonDimPhiZ(this%nX, this%nY, this%nZ) )
    allocate( this%nonDimPhit(this%nX, this%nY, this%nZ) )

    allocate( this%nonDimDuDt(this%nX, this%nY, this%nZ) )
    allocate( this%nonDimDvDt(this%nX, this%nY, this%nZ) )
    allocate( this%nonDimDwDt(this%nX, this%nY, this%nZ) )

    allocate( this%eta(this%nX, this%nY) )
    allocate( this%u(this%nX, this%nY, this%nZ) )
    allocate( this%v(this%nX, this%nY, this%nZ) )
    allocate( this%w(this%nX, this%nY, this%nZ) )
    allocate( this%pd(this%nX, this%nY, this%nZ) )

    allocate( this%dudt(this%nX, this%nY, this%nZ) )
    allocate( this%dvdt(this%nX, this%nY, this%nZ) )
    allocate( this%dwdt(this%nX, this%nY, this%nZ) )

end subroutine

subroutine destroyHOSMesh(this)
    implicit none
    class(typHOSMesh),intent(inout) :: this
    if (allocated(this%nonDimX)) deallocate( this%nonDimX )
    if (allocated(this%nonDimY)) deallocate( this%nonDimY )
    if (allocated(this%nonDimZ)) deallocate( this%nonDimZ )

    if (allocated(this%nonDimEta)) deallocate( this%nonDimEta)
    if (allocated(this%nonDimPhiX)) deallocate( this%nonDimPhiX )
    if (allocated(this%nonDimPhiY)) deallocate( this%nonDimPhiY )
    if (allocated(this%nonDimPhiZ)) deallocate( this%nonDimPhiZ )

    if (allocated(this%nonDimPhit)) deallocate( this%nonDimPhit )
    if (allocated(this%nonDimDuDt)) deallocate( this%nonDimDuDt )
    if (allocated(this%nonDimDvDt)) deallocate( this%nonDimDvDt )
    if (allocated(this%nonDimDwDt)) deallocate( this%nonDimDwDt )

    if (allocated(this%eta)) deallocate( this%eta )
    if (allocated(this%u))   deallocate( this%u )
    if (allocated(this%v))   deallocate( this%v )
    if (allocated(this%w))   deallocate( this%w )
    if (allocated(this%pd))  deallocate( this%pd )

    if (allocated(this%dudt)) deallocate( this%dudt )
    if (allocated(this%dvdt)) deallocate( this%dvdt )
    if (allocated(this%dwdt)) deallocate( this%dwdt )
end subroutine

subroutine final_HOSMesh(this)
    implicit none
    type(typHOSMesh),intent(inout) :: this
    Call this%destroyHOSMesh
end subroutine

!!!- Calcul dimensional values
subroutine calculDimValue(this)
    implicit none
    class(typHOSMesh) :: this

    this%eta = this%nonDimEta * this%dimL
    this%u   = this%nonDimPhiX * this%dimL / this%dimT
    this%v   = this%nonDimPhiY * this%dimL / this%dimT
    this%w   = this%nonDimPhiZ * this%dimL / this%dimT

    this%dudt = this%nonDimDuDt * this%dimL / this%dimT / this%dimT
    this%dvdt = this%nonDimDvDt * this%dimL / this%dimT / this%dimT
    this%dwdt = this%nonDimDwDt * this%dimL / this%dimT / this%dimT

    this%pd = -0.5_RP * (  this%nonDimPhiX * this%nonDimPhiX &
			  + this%nonDimPhiY * this%nonDimPhiY &
			  + this%nonDimPhiZ * this%nonDimPhiZ ) &
	      - this%nonDimPhit

    this%pd = this%pd * this%dimL**2.d0 / this%dimT**2.d0

end subroutine