Subroutine initialize_typHOSWeightStepFunc(this, dict)
    Implicit None
    Class(typHOSWeightStepFunc), intent(inout) :: this
    Type(typDictionaryPtr)                     :: dict

    this%dict                 = dict
    this%ENUM_HOS_WEIGHT_FUNC = ENUM_HOS_WEIGHT_FUNC_STEP

    this%omegaMin     = dict%getReal("omegaMin")

End Subroutine

Subroutine getHOSWeight_typHOSWeightStepFunc(this, omega, weight)
    Implicit None
    Class(typHOSWeightStepFunc), intent(in) :: this
    Real(RP), intent(in)                    :: omega
    Real(RP), intent(out)                   :: weight
    Real(RP) :: omegaArg

    weight = 1.0_RP
    if ( dabs(omega).le.this%omegaMin ) return
    weight = 0.0_RP

End Subroutine

Subroutine print_typHOSWeightStepFunc(this)
    Implicit None
    Class(typHOSWeightStepFunc), intent(in) :: this

    write(*,1001) " HOS Weight: Step Function"
    write(*,1001) ""
    write(*,1002) "     - omegaMin  : ", this%omegaMin

    1001 format(a)
    1002 format(a,e16.7)
    1003 format(a,i16)

End Subroutine
