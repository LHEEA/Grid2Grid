!!- Intialize Spline Type Data
subroutine initialize2D(this,kX, kZ, kT)
    implicit none
    class(typV2VInterp2D), intent(inout) :: this
    integer, intent(in) :: kX, kZ, kT
    integer :: iflag

    !! Initialize eta spline type
    Call this%spl2eta%initialize(this%xArg, this%tArg, this%etaValue, &
                                 kX, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 2D Eta spline: '//get_status_message(iflag)
    end if

    !! Initialize u spline type
    Call this%spl3u%initialize(this%xArg, this%zArg, this%tArg, this%uValue, &
                               kX, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 3D U spline: '//get_status_message(iflag)
    end if

    !! Initialize w spline type
    Call this%spl3w%initialize(this%xArg, this%zArg, this%tArg, this%wValue, &
                               kX, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 3D W spline: '//get_status_message(iflag)
    end if

    !! Initialize dynamic pressure spline type
    Call this%spl3pd%initialize(this%xArg, this%zArg, this%tArg, this%pdValue, &
                                kX, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 3D pd spline: '//get_status_message(iflag)
    end if

    !! Initialize detadx spline type
    Call this%spl2detadx%initialize(this%xArg, this%tArg, this%detadxValue, &
                                 kX, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 2D dEtadx spline: '//get_status_message(iflag)
    end if

    !! Initialize detadt spline type
    Call this%spl2detadt%initialize(this%xArg, this%tArg, this%detadtValue, &
                                 kX, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 2D dEtadt spline: '//get_status_message(iflag)
    end if

    !! Initialize dudx spline type
    Call this%spl3dudx%initialize(this%xArg, this%zArg, this%tArg, this%dudxValue, &
                               kX, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 3D dudx spline: '//get_status_message(iflag)
    end if

    !! Initialize dudz spline type
    Call this%spl3dudz%initialize(this%xArg, this%zArg, this%tArg, this%dudzValue, &
                               kX, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 3D dudz spline: '//get_status_message(iflag)
    end if

end subroutine

!!- Allocate Data Array for Spline Interpolation
subroutine allocSpl2DArray(this, nX, nZ, nT, x, z)
    implicit none
    class(typV2VInterp2D), intent(inout) :: this
    integer, intent(in)    :: nX, nZ, nT
    real(rp),dimension(:)  :: x, z

    allocate(this%xArg(nX))
    allocate(this%zArg(nZ))
    allocate(this%tArg(nT))

    this%xArg = x
    this%zArg = z

    allocate(this%etaValue(nX, nT))
    allocate(this%detadxValue(nX, nT))
    allocate(this%detadtValue(nX, nT))
    allocate(this%uValue(nX, nZ, nT))
    allocate(this%wValue(nX, nZ, nT))
    allocate(this%dudxValue(nX, nZ, nT))
    allocate(this%dudzValue(nX, nZ, nT))
    allocate(this%pdValue(nX, nZ, nT))

    this%etaValue = real(0.0,RP)
    this%detadxValue = real(0.0,RP)
    this%detadtValue = real(0.0,RP)
    this%uValue   = real(0.0,RP)
    this%wValue   = real(0.0,RP)
    this%pdValue  = real(0.0,RP)
    this%dudxValue  = real(0.0,RP)
    this%dudzValue  = real(0.0,RP)

end subroutine

!!- interpolate wave elevation for given HOS position
real(rp) function interpEta2D(this, nonDimX, t)
    implicit none
    class(typV2VInterp2D), intent(inout) :: this
    real(rp),intent(in) :: nonDimX, t
    real(wp) :: val
    integer  :: iflag
    Call this%spl2eta%evaluate(real(nonDimX,wp), real(t,wp), 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 2D eta spline: '//get_status_message(iflag)
    end if
    interpEta2D = real(val, rp)
end function

!!- interpolate wave elevation derivative for given HOS position
subroutine interpdEta2D(this, nonDimX, t, detadx, detadt)
    implicit none
    class(typV2VInterp2D), intent(inout) :: this
    real(rp),intent(in)  :: nonDimX, t
    real(rp),intent(out) :: detadx, detadt
    real(wp) :: val
    integer  :: iflag
    Call this%spl2detadx%evaluate(real(nonDimX,wp), real(t,wp), 0,  0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 2D dEtadx spline: '//get_status_message(iflag)
    end if
    detadx = real(val, rp)
    Call this%spl2detadt%evaluate(real(nonDimX,wp), real(t,wp), 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 2D dEtadt spline: '//get_status_message(iflag)
    end if
    detadt = real(val, rp)
end subroutine

!!- interpolate velocity for given HOS position
subroutine interpU2D(this, nonDimX, nonDimZ, t, u, w)
    implicit none
    class(typV2VInterp2D), intent(inout) :: this
    real(rp),intent(in)  :: nonDimX, nonDimZ, t
    real(rp),intent(out) :: u, w
    real(wp) :: val
    integer  :: iflag
    Call this%spl3u%evaluate(real(nonDimX,wp), real(nonDimZ,wp), real(t,wp), &
                             0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 3D U spline: '//get_status_message(iflag)
    end if
    u = real(val, rp)
    Call this%spl3w%evaluate(real(nonDimX,wp), real(nonDimZ,wp), real(t,wp), &
                             0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 3D W spline: '//get_status_message(iflag)
    end if
    w = real(val, rp)
end subroutine

!!- interpolate velocity derivative for given HOS position
subroutine interpdU2D(this, nonDimX, nonDimZ, t, dudx, dudz)
    implicit none
    class(typV2VInterp2D), intent(inout) :: this
    real(rp),intent(in)  :: nonDimX, nonDimZ, t
    real(rp),intent(out) :: dudx, dudz
    real(wp) :: val
    integer  :: iflag
    Call this%spl3dudx%evaluate(real(nonDimX,wp), real(nonDimZ,wp), real(t,wp), &
                             0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 3D dudx spline: '//get_status_message(iflag)
    end if
    dudx = real(val, rp)
    Call this%spl3dudz%evaluate(real(nonDimX,wp), real(nonDimZ,wp), real(t,wp), &
                             0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 3D dudz spline: '//get_status_message(iflag)
    end if
    dudz = real(val, rp)
end subroutine

!!- interpolate dynamic pressure for given HOS position
real(rp) function interpPd2D(this, nonDimX, nonDimZ, t)
    implicit none
    class(typV2VInterp2D), intent(inout) :: this
    real(rp),intent(in)  :: nonDimX, nonDimZ, t
    real(wp) :: val
    integer  :: iflag
    Call this%spl3pd%evaluate(real(nonDimX,wp), real(nonDimZ,wp), real(t,wp), &
                              0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 3D pd spline: '//get_status_message(iflag)
    end if
    interpPd2D = real(val, rp)
end function

!!- Destroy Spline Data Type
subroutine destroySpl2DModule(this)
    implicit none
    class(typV2VInterp2D), intent(inout) :: this
    Call this%spl2eta%destroy
    Call this%spl2detadx%destroy
    Call this%spl2detadt%destroy
    Call this%spl3u%destroy
    Call this%spl3v%destroy
    Call this%spl3w%destroy
    Call this%spl3pd%destroy
    Call this%spl3dudx%destroy
    Call this%spl3dudz%destroy
end subroutine

subroutine destroySpl2D(this)
    implicit none
    class(typV2VInterp2D), intent(inout) :: this
    Call this%destroySplMod
    if (allocated( this%xArg )) deallocate(this%xArg)
    if (allocated( this%zArg )) deallocate(this%zArg)
    if (allocated( this%tArg )) deallocate(this%tArg)

    if (allocated( this%etaValue )) deallocate(this%etaValue)
    if (allocated( this%detadxValue )) deallocate(this%detadxValue)
    if (allocated( this%detadtValue )) deallocate(this%detadtValue)
    if (allocated( this%uValue )) deallocate(this%uValue)
    if (allocated( this%wValue )) deallocate(this%wValue)
    if (allocated( this%pdValue )) deallocate(this%pdValue)
    if (allocated( this%dudxValue )) deallocate(this%dudxValue)
    if (allocated( this%dudzValue )) deallocate(this%dudzValue)
end subroutine

subroutine finalSpl2D(this)
    type(typV2VInterp2D), intent(inout) :: this
    Call this%destroy
end subroutine
