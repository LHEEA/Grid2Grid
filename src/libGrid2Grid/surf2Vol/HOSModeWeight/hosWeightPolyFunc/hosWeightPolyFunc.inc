Subroutine initialize_typHOSWeightPolyFunc(this, dict)
    Implicit None
    Class(typHOSWeightPolyFunc), intent(inout) :: this
    Type(typDictionaryPtr)                    :: dict

    this%dict                 = dict
    this%ENUM_HOS_WEIGHT_FUNC = ENUM_HOS_WEIGHT_FUNC_POLY

    this%omegaMin     = dict%getReal("omegaMin")
    this%omegaMax     = dict%getReal("omegaMax")

    this%omegaMaxMin  = this%omegaMax - this%omegaMin

    if ( this%omegaMaxMin.le.1.D-12 ) then
        write(*,"(a)") "    [ERROR] initialize_typHOSWeightPolyFunc"
        write(*,"(a)") ""
        write(*,"(a)") "    omegaMax is very closed or less than omegaMin"
        write(*,"(a)") ""
        write(*,"(a,e20.7)") "    omegaMax : ", this%omegaMax
        write(*,"(a,e20.7)") "    omegaMin : ", this%omegaMin
        write(*,"(a)") ""
        stop
    end if

End Subroutine

Subroutine getHOSWeight_typHOSWeightPolyFunc(this, omega, weight)
    Implicit None
    Class(typHOSWeightPolyFunc), intent(in) :: this
    Real(RP), intent(in)                    :: omega
    Real(RP), intent(out)                   :: weight
    Real(RP) :: xi, xi2

    if ( dabs(omega).le.this%omegaMin ) then
        weight = 1.0_RP
    else if ( dabs(omega).ge.this%omegaMax ) then
        weight = 0.0_RP
    else
        xi = (omega - this%omegaMin) / this%omegaMaxMin
        xi2 = xi *xi
        weight = 2.0_RP * xi2*xi - 3.0_RP * xi2 + 1.0_RP
    end if

End Subroutine

Subroutine print_typHOSWeightPolyFunc(this)
    Implicit None
    Class(typHOSWeightPolyFunc), intent(in) :: this

    write(*,1001) " HOS Weight: Polynomial Function"
    write(*,1001) ""
    write(*,1002) "     - omegaMin  : ", this%omegaMin
    write(*,1002) "     - omegaMax  : ", this%omegaMax

    1001 format(a)
    1002 format(a,e16.7)
    1003 format(a,i16)

End Subroutine
