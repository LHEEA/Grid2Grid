!!- Intialize Spline Type Data
subroutine initialize3D(this,kX, kY, kZ, kT)
    implicit none
    class(typV2VInterp3D), intent(inout) :: this
    integer, intent(in) :: kX, kY, kZ, kT
    integer :: iflag

    !! Initialize eta spline type
    Call this%spl3eta%initialize(this%xArg, this%yArg, this%tArg, this%etaValue, &
                                 kX, kY, kZ, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 3D Eta spline: '//get_status_message(iflag)
    end if

    !! Initialize u spline type
    Call this%spl4u%initialize(this%xArg, this%yArg, this%zArg, this%tArg, this%uValue, &
                               kX, kY, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 4D U spline: '//get_status_message(iflag)
    end if

    !! Initialize v spline type
    Call this%spl4v%initialize(this%xArg, this%yArg, this%zArg, this%tArg, this%vValue, &
                               kX, kY, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 4D V spline: '//get_status_message(iflag)
    end if

    !! Initialize w spline type
    Call this%spl4w%initialize(this%xArg, this%yArg, this%zArg, this%tArg, this%wValue, &
                               kX, kY, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 4D W spline: '//get_status_message(iflag)
    end if

    !! Initialize dynamic pressure spline type
    Call this%spl4pd%initialize(this%xArg, this%yArg, this%zArg, this%tArg, this%pdValue, &
                                kX, kY, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 4D pd spline: '//get_status_message(iflag)
    end if

end subroutine

!!- Allocate Data Array for Spline Interpolation
subroutine allocSpl3DArray(this, nX, nY, nZ, nT, x, y, z)
    implicit none
    class(typV2VInterp3D), intent(inout) :: this
    integer, intent(in)    :: nX, nY, nZ, nT
    real(rp),dimension(:)  :: x, y, z

    allocate(this%xArg(nX))
    allocate(this%yArg(nY))
    allocate(this%zArg(nZ))
    allocate(this%tArg(nT))

    this%xArg = x
    this%yArg = y
    this%zArg = z

    allocate(this%etaValue(nX, nY, nT))
    allocate(this%uValue(nX, nY, nZ, nT))
    allocate(this%vValue(nX, nY, nZ, nT))
    allocate(this%wValue(nX, nY, nZ, nT))
    allocate(this%pdValue(nX, nY, nZ, nT))

    this%etaValue = real(0.0,RP)
    this%uValue   = real(0.0,RP)
    this%vValue   = real(0.0,RP)
    this%wValue   = real(0.0,RP)
    this%pdValue  = real(0.0,RP)
end subroutine

!!- interpolate wave elevation for given HOS position
real(rp) function interpEta3D(this, nonDimX, nonDimY, t)
    implicit none
    class(typV2VInterp3D), intent(inout) :: this
    real(rp),intent(in) :: nonDimX, nonDimY, t
    real(wp) :: val
    integer  :: iflag
    Call this%spl3eta%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(t,wp), &
                               0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 3D eta spline: '//get_status_message(iflag)
    end if
    interpEta3D = real(val, rp)
end function

!!- interpolate veloicity for given HOS position
subroutine interpU3D(this, nonDimX, nonDimY, nonDimZ, t, u, v, w)
    implicit none
    class(typV2VInterp3D), intent(inout) :: this
    real(rp),intent(in)  :: nonDimX, nonDimY, nonDimZ, t
    real(rp),intent(out) :: u, v, w
    real(wp) :: val
    integer  :: iflag
    Call this%spl4u%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(nonDimZ,wp), real(t,wp), &
                             0, 0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 4D U spline: '//get_status_message(iflag)
    end if
    u = real(val, rp)
    Call this%spl4v%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(nonDimZ,wp), real(t,wp), &
                             0, 0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 4D V spline: '//get_status_message(iflag)
    end if
    v = real(val, rp)
    Call this%spl4w%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(nonDimZ,wp), real(t,wp), &
                             0, 0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 4D W spline: '//get_status_message(iflag)
    end if
    w = real(val, rp)
end subroutine

!!- interpolate dynamic pressure for given HOS position
real(rp) function interpPd3D(this, nonDimX, nonDimY, nonDimZ, t)
    implicit none
    class(typV2VInterp3D), intent(inout) :: this
    real(rp),intent(in)  :: nonDimX, nonDimY, nonDimZ, t
    real(wp) :: val
    integer  :: iflag
    Call this%spl4pd%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(nonDimZ,wp), real(t,wp), &
                              0, 0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 3D pd spline: '//get_status_message(iflag)
    end if
    interpPd3D = real(val, rp)
end function

!!- Destroy Spline Data Type
subroutine destroySpl3DModule(this)
    implicit none
    class(typV2VInterp3D), intent(inout) :: this
    Call this%spl3eta%destroy
    Call this%spl4u%destroy
    Call this%spl4v%destroy
    Call this%spl4w%destroy
    Call this%spl4pd%destroy
end subroutine

subroutine destroySpl3D(this)
    implicit none
    class(typV2VInterp3D), intent(inout) :: this
    Call this%destroySplMod
    if (allocated( this%xArg )) deallocate(this%xArg)
    if (allocated( this%yArg )) deallocate(this%yArg)
    if (allocated( this%zArg )) deallocate(this%zArg)
    if (allocated( this%tArg )) deallocate(this%tArg)

    if (allocated( this%etaValue )) deallocate(this%etaValue)
    if (allocated( this%uValue )) deallocate(this%uValue)
    if (allocated( this%vValue )) deallocate(this%vValue)
    if (allocated( this%wValue )) deallocate(this%wValue)
    if (allocated( this%pdValue )) deallocate(this%pdValue)
end subroutine

subroutine finalSpl3D(this)
    type(typV2VInterp3D), intent(inout) :: this
    Call this%destroy
end subroutine
