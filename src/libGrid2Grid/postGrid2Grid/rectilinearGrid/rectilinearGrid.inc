subroutine buildRectLinearMesh(this, isBuildAirMesh,  zflag)
    implicit none
    class(typRectilinearGrid), intent(inout) :: this
    logical, intent(in) :: isBuildAirMesh
    integer,optional    :: zflag
    REAL(RP) :: dx, dy
    integer  :: i

    if (isBuildAirMesh.EQV..FALSE.) this%nZmax_ = 0.0

    this%nZ_ = this%nZmin_ + this%nZmax_

    if ((this%nX_.le.1).or.(this%nY_.le.1)) then
        write(*,*) "    [ERROR] Grid2Grid, buildRectLinearMesh"
        write(*,*) "        nX, nY Should be larger than 2"
        write(*,*) "        given nX, nY : ", this%nX_, this%nY_
        stop
    end if

    if (isBuildAirMesh.EQV..FALSE.) then
        if ((this%nZmin_.le.1)) then
            write(*,*) "    [ERROR] Grid2Grid, buildRectLinearMesh"
            write(*,*) "        nZmin should be larger than 2"
            write(*,*) "        given nZmin : ", this%nZmin_
            stop
        end if
    else
        if ((this%nZmin_.le.1).or.(this%nZmax_.le.1)) then
            write(*,*) "    [ERROR] Grid2Grid, buildRectLinearMesh"
            write(*,*) "        nZmin or nZmax should be larger than 2"
            write(*,*) "        given nZmin, nZmax : ", this%nZmin_, this%nZmax_
            stop
        end if
    endif

    this%Lx_ = this%xMax_ - this%xMin_
    this%Ly_ = this%yMax_ - this%yMin_

    allocate( this%X_(this%nX_), this%Y_(this%nY_))

    allocate( this%movingZ_(this%nX_, this%nY_, this%nZ_) )
    allocate( this%eta_(this%nX_, this%nY_, this%nZ_) )
    allocate( this%u_(this%nX_, this%nY_, this%nZ_), &
              this%v_(this%nX_, this%nY_, this%nZ_), &
              this%w_(this%nX_, this%nY_, this%nZ_) )
    allocate( this%pd_(this%nX_, this%nY_, this%nZ_) )

    dx = this%Lx_ / (this%nX_ - 1.0_RP)
    dy = this%Ly_ / (this%nY_ - 1.0_RP)

    do i = 1, this%nX_
        this%X_(i) = this%xMin_ + (i - 1.0_RP) * dx
    enddo

    do i = 1, this%nY_
        this%Y_(i) = this%yMin_ + (i - 1.0_RP) * dy
    enddo

    Call buildZmesh(this%zMin_, this%zMax_, this%nZmin_, this%nZmax_, &
                    this%Z_, zflag, this%zMinRatio_, this%zMaxRatio_)

end subroutine

Subroutine destroyRectLGrid(this)
    implicit none
    class(typRectilinearGrid), intent(inout) :: this

    if ( allocated( this%X_ ) ) deallocate( this%X_ )
    if ( allocated( this%Y_ ) ) deallocate( this%Y_ )
    if ( allocated( this%Z_ ) ) deallocate( this%Z_ )

    if ( allocated( this%movingZ_ ) ) deallocate( this%movingZ_ )

    if ( allocated( this%eta_ ) ) deallocate( this%eta_ )
    if ( allocated( this%u_ ) ) deallocate( this%u_ )
    if ( allocated( this%v_ ) ) deallocate( this%v_ )
    if ( allocated( this%w_ ) ) deallocate( this%w_ )
    if ( allocated( this%pd_ ) ) deallocate( this%pd_ )

    this%zMinRatio_ = 1.0
    this%zMaxRatio_ = 1.0

    this%nX_ = 0
    this%nY_ = 0
    this%nZ_ = 0

End Subroutine

Subroutine finalRectLGrid(this)
    implicit none
    type(typRectilinearGrid), intent(inout) :: this
    Call this%destroy
End Subroutine
