!!... Enumerator of HOS Weight function
Enum, bind(c)

    ENUMERATOR :: ENUM_HOS_WEIGHT_FUNC_EXP  = 1

    ENUMERATOR :: ENUM_HOS_WEIGHT_FUNC_STEP = 2

    ENUMERATOR :: ENUM_HOS_WEIGHT_FUNC_POLY = 3

End Enum

!!... Base class of HOS Weight function
Type, abstract :: typHOSWeightBaseFunc

    !!... Input dictionary
    Type(typDictionaryPtr) :: dict

    !!... Enumerator
    Integer :: ENUM_HOS_WEIGHT_FUNC

Contains

    !!... initialize the class with dict
    procedure(proc_initialize_HOSWeight), deferred :: initialize

    !!... get HOS Weight
    procedure(proc_getHOSWeight), deferred :: getHOSWeight

    !!... print the class
    procedure(proc_print_HOSWeight), deferred :: print

End Type

!!... All derived class has a subroutine given in:
abstract interface

   subroutine proc_initialize_HOSWeight(this, dict)
       import typHOSWeightBaseFunc, typDictionaryPtr
       class(typHOSWeightBaseFunc), intent(inout) :: this
       Type(typDictionaryPtr)                     :: dict
   end subroutine

   subroutine proc_getHOSWeight(this, omega, weight)
       import typHOSWeightBaseFunc, RP
       class(typHOSWeightBaseFunc), intent(in) :: this
       Real(RP), intent(in)                    :: omega
       Real(RP), intent(out)                   :: weight
   end subroutine

   subroutine proc_print_HOSWeight(this)
       import typHOSWeightBaseFunc
       class(typHOSWeightBaseFunc), intent(in) :: this
   end subroutine

end interface
