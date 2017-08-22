Program progTestSurf2Vol
    Implicit none

    Call test1DSpline

    ! Call test2DSpline

End Program

subroutine test2DSpline

    Use bspline_module
    use,intrinsic :: iso_fortran_env, only: wp => real64
    Implicit None

    type(bspline_2d) :: spl2

    integer, parameter :: kx  = 4, ky = 4
    integer, parameter :: nX  = 31, nY = 6
    integer, parameter :: nXX = 301, nYY = 201
    real :: xmin, xmax, ymin, ymax, dx, dy
    real(wp) :: x(nX), y(nY), valueZ(nX, nY)
    real(wp) :: xx(nXX), yy(nYY), val(nXX, nYY)
    real(wp) :: xin(nX, nY), yin(nX, nY)
    real(wp) :: xout(nXX, nYY), yout(nXX, nYY)
    integer :: ix, iy
    integer :: iflag

    open(unit=301, file = "checkInterp/input2DX.dat")
    open(unit=302, file = "checkInterp/input2DY.dat")
    open(unit=303, file = "checkInterp/input2DV.dat")

    open(unit=311, file = "checkInterp/result2DX.dat")
    open(unit=312, file = "checkInterp/result2DY.dat")
    open(unit=313, file = "checkInterp/result2DV.dat")

    xmin = -10.d0
    xmax =  10.d0

    ymin =  0.d0
    ymax =  1.d0

    dx = (xmax - xmin) / (nX - 1.0)
    x(1) = xmin
    do ix = 2, nX
        x(ix) = x(ix-1) + dx
    enddo

    dy = (ymax - ymin) / (nY - 1.0)
    y(1) = ymin
    do iy = 2, nY
        y(iy) = y(iy-1) + dy
    enddo

    do ix = 1,nX
        do iy =1, nY
            xin(ix, iy) = x(ix)
            yin(ix, iy) = y(iy)
            valueZ(ix, iy) = sin(3.0 * x(ix)) + 2.0 &
                           + cos(5.0 * y(iy)) * cos(0.5 * x(ix))
        enddo
    enddo

    Call spl2%initialize(x, y, valueZ, kx, ky, iflag)

    dx = (xmax - xmin) / (nXX - 1.0)
    xx(1) = xmin
    do ix = 2, nXX
        xx(ix) = xx(ix-1) + dx
    enddo

    dy = (ymax - ymin) / (nYY - 1.0)
    yy(1) = ymin
    do iy = 2, nYY
        yy(iy) = yy(iy-1) + dy
    enddo

    do ix = 1,nXX
        do iy =1, nYY
            xout(ix, iy) = xx(ix)
            yout(ix, iy) = yy(iy)
            Call spl2%evaluate(xx(ix), yy(iy), 0, 0, val(ix, iy), iflag)
        enddo
    enddo

    do ix = 1,nX
        write(301,*) (xin(ix,iy), iy=1, nY)
        write(302,*) (yin(ix,iy), iy=1, nY)
        write(303,*) (valueZ(ix,iy), iy=1, nY)
    enddo

    do ix = 1,nXX
        write(311,*) (xout(ix,iy), iy=1, nYY)
        write(312,*) (yout(ix,iy), iy=1, nYY)
        write(313,*) (val(ix,iy), iy=1, nYY)
    enddo

    close(301)
    close(302)
    close(303)

    close(311)
    close(312)
    close(313)

end subroutine

subroutine test1DSpline
    Use bspline_module
    use,intrinsic :: iso_fortran_env, only: wp => real64
    Implicit None

    type(bspline_1d) :: spl1

    integer, parameter  :: kx  = 4
    integer, parameter  :: nX  = 31, nXX = 301
    real :: xmin, xmax, dx
    real(wp), dimension(nX) :: x, y
    real(wp), dimension(nXX) :: xx,yy
    integer ::ix, iflag

    open(unit=300, file = "checkInterp/input1D.dat")
    open(unit=301, file = "checkInterp/result1D.dat")

    xmin = -3.d0
    xmax =  3.d0

    dx = (xmax - xmin) / (nX - 1.0)
    x(1) = xmin
    do ix = 2,nX
        x(ix) = x(ix-1) + dx
    enddo

    do ix = 1,nX
        !y(ix) = 3.0 * x(ix) * x(ix) + sin(10.0 * x(ix)) + 2.0
        if (ix.le.(nX-1)/2) then
            y(ix) = x(ix)
        else
            y(ix) = x((nX-1)/2)
        endif

        write(300,*) x(ix), y(ix)
    enddo

    Call spl1%initialize(x, y, kx, iflag)

    dx = (xmax - xmin) / (nXX - 1.0)
    xx(1) = xmin
    do ix = 2,nXX
        xx(ix) = xx(ix-1)+dx
    enddo

    do ix = 1,nXX
        Call spl1%evaluate(xx(ix), 0, yy(ix), iflag)
        write(301,*) xx(ix), yy(ix)
    enddo

    close(300)
    close(301)
end subroutine
