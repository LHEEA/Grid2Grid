!-----------------------------------------------------------------------
Module  modV2VSplineInterp
!-----------------------------------------------------------------------
!
!   Spline Interpolation module of HOS (vol2vol)
!
!-----------------------------------------------------------------------
!
!   Copyright (C) 2017 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598
!
!   This program based on Post Processing Part of HOS.
!
!-----------------------------------------------------------------------
!
!       Written by YoungMyung Choi.
!
!-----------------------------------------------------------------------
use modGrid2GridType
Use bspline_module
use,intrinsic :: iso_fortran_env, only: wp => real64

type,public :: typV2VInterp2D

    !!- Intepolation Node
    real(wp),allocatable :: xArg(:), zArg(:), tArg(:)

    !!- Interpolation Node Value
    real(wp),allocatable :: etaValue(:,:)
    real(wp),allocatable :: uValue(:,:,:)
    real(wp),allocatable :: wValue(:,:,:)
    real(wp),allocatable :: pdValue(:,:,:)

    !!- 2D Spline Type (2D Case)
    type(bspline_2d)     :: spl2eta

    !!- 3D Spline Type (2D Case)
    type(bspline_3d)     :: spl3u, spl3v, spl3w, spl3pd

    contains

    procedure, public  :: allocArray => allocSpl2DArray ! Allocate Array
    procedure, public  :: destroy    => destroySpl2D    ! Destroy Spline type
    procedure, public  :: initialize => initialize2D    ! initialize Spline type

    procedure, public  :: interpEta  => interpEta2D     ! interpolate eta
    procedure, public  :: interpU    => interpU2D       ! interpolate velocity
    procedure, public  :: interpPd   => interpPd2D      ! interpolate dynmic pressure

end type

type,public :: typV2VInterp3D

    !!- Intepolation Node
    real(wp),allocatable :: xArg(:), yArg(:), zArg(:), tArg(:)

    !!- Interpolation Node Value
    real(wp),allocatable :: etaValue(:,:,:)
    real(wp),allocatable :: uValue(:,:,:,:)
    real(wp),allocatable :: vValue(:,:,:,:)
    real(wp),allocatable :: wValue(:,:,:,:)
    real(wp),allocatable :: pdValue(:,:,:,:)

    !!- 3D Spline Type
    type(bspline_3d)     :: spl3eta

    !!- 4D Spline Type
    type(bspline_4d)     :: spl4u, spl4v, spl4w, spl4pd

contains

    procedure, public  :: allocArray => allocSpl3DArray ! Allocate Array
    procedure, public  :: destroy    => destroySpl3D    ! Destroy Spline type
    procedure, public  :: initialize => initialize3D    ! initialize Spline type

    procedure, public  :: interpEta  => interpEta3D     ! interpolate eta
    procedure, public  :: interpU    => interpU3D       ! interpolate velocity
    procedure, public  :: interpPd   => interpPd3D      ! interpolate dynmic pressure

end type

contains

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
    allocate(this%uValue(nX, nZ, nT))
    allocate(this%wValue(nX, nZ, nT))
    allocate(this%pdValue(nX, nZ, nT))

    this%etaValue = real(0.0,RP)
    this%uValue   = real(0.0,RP)
    this%wValue   = real(0.0,RP)
    this%pdValue  = real(0.0,RP)

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

!!- Destroy Spline Data Type
subroutine destroySpl2D(this)
    implicit none
    class(typV2VInterp2D), intent(inout) :: this
    Call this%spl2eta%destroy
    Call this%spl3u%destroy
    Call this%spl3v%destroy
    Call this%spl3w%destroy
    Call this%spl3pd%destroy
end subroutine

!!- Destroy Spline Data Type
subroutine destroySpl3D(this)
    implicit none
    class(typV2VInterp3D), intent(inout) :: this
    Call this%spl3eta%destroy
    Call this%spl4u%destroy
    Call this%spl4v%destroy
    Call this%spl4w%destroy
    Call this%spl4pd%destroy
end subroutine

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

end subroutine

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



!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
