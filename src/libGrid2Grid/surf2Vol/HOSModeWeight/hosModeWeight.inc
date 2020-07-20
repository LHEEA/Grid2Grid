!! ----------------------------------------------------------------
!! Add HOS Weight Class functions in the below
!! ----------------------------------------------------------------

!!... HOS Weight: expoential function
#include "hosWeightExpFunc/hosWeightExpFunc.inc"

!!... HOS Weight: step function
#include "hosWeightStepFunc/hosWeightStepFunc.inc"

!!... HOS Weight: poly function
#include "hosWeightPolyFunc/hosWeightPolyFunc.inc"

!! ----------------------------------------------------------------
!! Wrapper of HOS Weight function
!! ----------------------------------------------------------------

Subroutine initialize_typHOSModeWeightFunc(this, dict)
    Implicit None
    Class(typHOSModeWeightFunc), intent(inout) :: this
    Type(typDictionaryPtr)                     :: dict
    !!... Local variagbles

    this%dict = dict

    !!... Get the weight type
    this%HOSWeightType = this%dict%getChar("HOSAmpWeightType")

    !!... Deallocate the pointer
    if ( allocated(this%ptrHOSWeightFunc) ) then
        deallocate( this%ptrHOSWeightFunc  )
    end if

    !!... Allocate the pointer

    if ( isStringEqual(this%HOSWeightType, "expWeight") ) then

        !!... Exponential Weight
        allocate( this%ptrHOSWeightFunc, &
                  source = this%HOSWeightExpFunc )

    elseif ( isStringEqual(this%HOSWeightType, "stepWeight") ) then

        !!... Step Weight
        allocate( this%ptrHOSWeightFunc, &
                  source = this%HOSWeightStepFunc )

    elseif ( isStringEqual(this%HOSWeightType, "polyWeight") ) then

        !!... Poly Weight
        allocate( this%ptrHOSWeightFunc, &
                  source = this%HOSWeightPolyFunc )

    else

        write(*,"(a)") "    [ERROR] initialize_typHOSModeWeightFunc"
        write(*,"(a)") " "
        write(*,"(a)") "        No corresponding HOSAmpWeightType"
        write(*,"(a)") " "
        write(*,"(a,a)") "         HOSAmpWeightType : ", trim(this%HOSWeightType)
        write(*,"(a)") " "
        stop

    end if

    !!... Initialize the class
    Call this%ptrHOSWeightFunc%initialize( &
        this%dict%subDict( trim(this%HOSWeightType)//"Coeffs") )

    !!... Print the class
    Call this%ptrHOSWeightFunc%print()

End Subroutine

Subroutine getHOSWeight_typHOSModeWeightFunc(this, omega, weight)
    Implicit None
    Class(typHOSModeWeightFunc), intent(inout) :: this
    Real(RP), intent(in)                       :: omega
    Real(RP), intent(inout)                    :: weight

    Call this%ptrHOSWeightFunc%getHOSWeight(omega, weight)

    if (weight.le.0.0_RP)  weight = 0.0_RP
    if (weight.ge.1.0_RP)  weight = 1.0_RP

End Subroutine

Subroutine getHOSWeightArray_typHOSModeWeightFunc(this, omega, weight)
    Implicit None
    Class(typHOSModeWeightFunc), intent(inout) :: this
    Real(RP), allocatable, intent(in)          :: omega(:)
    Real(RP), allocatable, intent(inout)       :: weight(:)
    !!
    integer :: nOmega, iomega

    nOmega = size(omega)
    if ( nOmega.le.0 ) then

        write(*,"(a)") "    [ERROR] getHOSWeightArray_typHOSModeWeightFunc"
        write(*,"(a)") " "
        write(*,"(a)") "    The number of omega array is less than 0."
        write(*,"(a)") " "
        stop

    else

        if (allocated(weight)) deallocate(weight)
        allocate(weight(nOmega)); weight = 1.0_RP

        do iomega = 1, nOmega
            Call this%getHOSWeight( omega(iomega), weight(iomega))
        end do

    end if

End Subroutine

#include "testHOSModeWeight.inc"
