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

    !! Initialize detadx spline type
    Call this%spl3detadx%initialize(this%xArg, this%yArg, this%tArg, this%detadxValue, &
                                 kX, kY, kZ, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 3D dEtadx spline: '//get_status_message(iflag)
    end if
    !! Initialize detady spline type
    Call this%spl3detady%initialize(this%xArg, this%yArg, this%tArg, this%detadyValue, &
                                 kX, kY, kZ, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 3D dEtady spline: '//get_status_message(iflag)
    end if
    !! Initialize detadt spline type
    Call this%spl3detadt%initialize(this%xArg, this%yArg, this%tArg, this%detadtValue, &
                                 kX, kY, kZ, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 3D dEtadt spline: '//get_status_message(iflag)
    end if

    !! Initialize dudx spline type
    Call this%spl4dudx%initialize(this%xArg, this%yArg, this%zArg, this%tArg, this%dudxValue, &
                               kX, kY, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 4D dudx spline: '//get_status_message(iflag)
    end if

    !! Initialize dvdy spline type
    Call this%spl4dvdy%initialize(this%xArg, this%yArg, this%zArg, this%tArg, this%dvdyValue, &
                               kX, kY, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 4D dvdy spline: '//get_status_message(iflag)
    end if

    !! Initialize dudy spline type
    Call this%spl4dudy%initialize(this%xArg, this%yArg, this%zArg, this%tArg, this%dudyValue, &
                               kX, kY, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 4D dudy spline: '//get_status_message(iflag)
    end if

    !! Initialize dudz spline type
    Call this%spl4dudz%initialize(this%xArg, this%yArg, this%zArg, this%tArg, this%dudzValue, &
                               kX, kY, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 4D dudz spline: '//get_status_message(iflag)
    end if

    !! Initialize dvdz spline type
    Call this%spl4dvdz%initialize(this%xArg, this%yArg, this%zArg, this%tArg, this%dvdzValue, &
                               kX, kY, kZ, kT, iflag)
    if (iflag/=0) then
        write(*,*) 'Error initializing 4D dvdz spline: '//get_status_message(iflag)
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
    allocate(this%detadxValue(nX, nY, nT))
    allocate(this%detadyValue(nX, nY, nT))
    allocate(this%detadtValue(nX, nY, nT))
    allocate(this%uValue(nX, nY, nZ, nT))
    allocate(this%vValue(nX, nY, nZ, nT))
    allocate(this%wValue(nX, nY, nZ, nT))
    allocate(this%pdValue(nX, nY, nZ, nT))
    allocate(this%dudxValue(nX, nY, nZ, nT))
    allocate(this%dvdyValue(nX, nY, nZ, nT))
    allocate(this%dudyValue(nX, nY, nZ, nT))
    allocate(this%dudzValue(nX, nY, nZ, nT))
    allocate(this%dvdzValue(nX, nY, nZ, nT))

    this%etaValue = real(0.0,RP)
    this%detadxValue = real(0.0,RP)
    this%detadyValue = real(0.0,RP)
    this%detadtValue = real(0.0,RP)
    this%uValue   = real(0.0,RP)
    this%vValue   = real(0.0,RP)
    this%wValue   = real(0.0,RP)
    this%dudxValue   = real(0.0,RP)
    this%dvdyValue   = real(0.0,RP)
    this%dudyValue   = real(0.0,RP)
    this%dudzValue   = real(0.0,RP)
    this%dvdzValue   = real(0.0,RP)
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

!!- interpolate wave elevation for given HOS position
subroutine interpdEta3D(this, nonDimX, nonDimY, t, dEtadx, dEtady, dEtadt)
    implicit none
    class(typV2VInterp3D), intent(inout) :: this
    real(rp),intent(in)  :: nonDimX, nonDimY, t
    real(rp),intent(out) :: dEtadx, dEtady, dEtadt
    real(wp) :: val
    integer  :: iflag
    Call this%spl3dEtadx%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(t,wp), &
                              0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 3D dEtadx spline: '//get_status_message(iflag)
    end if
    dEtadx = real(val, rp)
    Call this%spl3dEtady%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(t,wp), &
                              0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 3D dEtady spline: '//get_status_message(iflag)
    end if
    dEtady = real(val, rp)
    Call this%spl3dEtadt%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(t,wp), &
                             0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 3D dEtadt spline: '//get_status_message(iflag)
    end if
    dEtadt = real(val, rp)
end subroutine

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

!!- interpolate velocity derivative  for given HOS position
subroutine interpdU3D(this, nonDimX, nonDimY, nonDimZ, t, dudx, dvdy, dudy, dudz, dvdz)
    implicit none
    class(typV2VInterp3D), intent(inout) :: this
    real(rp),intent(in)  :: nonDimX, nonDimY, nonDimZ, t
    real(rp),intent(out) :: dudx, dvdy, dudy, dudz, dvdz
    real(wp) :: val
    integer  :: iflag
    Call this%spl4dudx%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(nonDimZ,wp), real(t,wp), &
                             0, 0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 4D dUdx spline: '//get_status_message(iflag)
    end if
    dudx = real(val, rp)
    Call this%spl4dvdy%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(nonDimZ,wp), real(t,wp), &
                             0, 0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 4D dVdy spline: '//get_status_message(iflag)
    end if
    dvdy = real(val, rp)
    Call this%spl4dudy%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(nonDimZ,wp), real(t,wp), &
                             0, 0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 4D dudy spline: '//get_status_message(iflag)
    end if
    dudy = real(val, rp)

    Call this%spl4dudz%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(nonDimZ,wp), real(t,wp), &
                             0, 0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 4D dudz spline: '//get_status_message(iflag)
    end if
    dudz = real(val, rp)
    Call this%spl4dvdz%evaluate(real(nonDimX,wp), real(nonDimY,wp), real(nonDimZ,wp), real(t,wp), &
                             0, 0, 0, 0, val, iflag)
    if (iflag/=0) then
        write(*,*) 'Error evaluating 4D dVdz spline: '//get_status_message(iflag)
    end if
    dvdz = real(val, rp)
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
    Call this%spl3detadx%destroy
    Call this%spl3detady%destroy
    Call this%spl3detadt%destroy
    Call this%spl4u%destroy
    Call this%spl4v%destroy
    Call this%spl4w%destroy
    Call this%spl4pd%destroy
    Call this%spl4dudx%destroy
    Call this%spl4dvdy%destroy
    Call this%spl4dudy%destroy
    Call this%spl4dudz%destroy
    Call this%spl4dvdz%destroy
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
    if (allocated( this%detadxValue )) deallocate(this%detadxValue)
    if (allocated( this%detadyValue )) deallocate(this%detadyValue)
    if (allocated( this%detadtValue )) deallocate(this%detadtValue)
    if (allocated( this%uValue )) deallocate(this%uValue)
    if (allocated( this%vValue )) deallocate(this%vValue)
    if (allocated( this%wValue )) deallocate(this%wValue)
    if (allocated( this%pdValue )) deallocate(this%pdValue)
    if (allocated( this%dudxValue )) deallocate(this%dudxValue)
    if (allocated( this%dvdyValue )) deallocate(this%dvdyValue)
    if (allocated( this%dudyValue )) deallocate(this%dudyValue)
    if (allocated( this%dudzValue )) deallocate(this%dudzValue)
    if (allocated( this%dvdzValue )) deallocate(this%dvdzValue)
end subroutine

subroutine finalSpl3D(this)
    type(typV2VInterp3D), intent(inout) :: this
    Call this%destroy
end subroutine
