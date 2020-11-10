Subroutine initialize_typHOSWeightExpFunc(this, dict)
    Implicit None
    Class(typHOSWeightExpFunc), intent(inout) :: this
    Type(typDictionaryPtr)                    :: dict

    this%dict                 = dict
    this%ENUM_HOS_WEIGHT_FUNC = ENUM_HOS_WEIGHT_FUNC_EXP

    this%omegaMin     = dict%getReal("omegaMin")
    this%alpha        = dict%getReal("alpha")
    this%twoNExpPower = 2 * dict%getIntOrDefault("nExpPower", 1)

End Subroutine

Subroutine getHOSWeight_typHOSWeightExpFunc(this, omega, weight)
    Implicit None
    Class(typHOSWeightExpFunc), intent(in) :: this
    Real(RP), intent(in)                   :: omega
    Real(RP), intent(out)                  :: weight
    Real(RP) :: omegaArg

    weight = 1.0_RP
    if ( dabs(omega).le.this%omegaMin ) return

    omegaArg = dabs(omega)/this%omegaMin - 1.0_RP
    weight = dexp( -this%alpha * omegaArg**this%twoNExpPower )

End Subroutine

Subroutine print_typHOSWeightExpFunc(this)
    Implicit None
    Class(typHOSWeightExpFunc), intent(in) :: this

    write(*,1001) " HOS Weight: Exponential Function"
    write(*,1001) ""
    write(*,1002) "     - omegaMin  : ", this%omegaMin
    write(*,1002) "     - alpha     : ", this%alpha
    write(*,1003) "     - nExpPower : ", this%twoNExpPower/2

    1001 format(a)
    1002 format(a,e16.7)
    1003 format(a,i16)

End Subroutine
