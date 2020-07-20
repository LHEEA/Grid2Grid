!! ----------------------------------------------------------------
!! Add HOS Weight Class definition in the below
!! ----------------------------------------------------------------

!!... Base(Abstract) Class fot HOS Weight
#include "hosWeightBaseFunc/hosWeightBaseFunc.typ"

!!... HOS Weight: expoential function
#include "hosWeightExpFunc/hosWeightExpFunc.typ"

!!... HOS Weight: step function
#include "hosWeightStepFunc/hosWeightStepFunc.typ"

!!... HOS Weight: poly function
#include "hosWeightPolyFunc/hosWeightPolyFunc.typ"

!! ----------------------------------------------------------------
!! Wrapper of HOS Weight function
!! ----------------------------------------------------------------

Type :: typHOSModeWeightFunc

    !!... Input dictionary
    Type(typDictionaryPtr) :: dict

    !!... HOS Weight Type
    Character(len=StringLength) :: HOSWeightType

    !!... Pointer: HOS Weight Func
    class(typHOSWeightBaseFunc), allocatable :: ptrHOSWeightFunc

    !!... HOS Weight: Exponential Function
    Type(typHOSWeightExpFunc) :: HOSWeightExpFunc

    !!... HOS Weight: Step Function
    Type(typHOSWeightStepFunc) :: HOSWeightStepFunc

    !!... HOS Weight: Poly Function
    Type(typHOSWeightPolyFunc) :: HOSWeightPolyFunc

Contains

    !!... Initialize the class
    Procedure :: initialize => initialize_typHOSModeWeightFunc

    !!... Get HOS Weight for single omega
    Procedure :: getHOSWeightSingle => getHOSWeight_typHOSModeWeightFunc

    !!... Get HOS Weight for omega array
    Procedure :: getHOSWeightArray => getHOSWeightArray_typHOSModeWeightFunc

    !!... Generic Procedure
    Generic :: getHOSWeight => getHOSWeightSingle, getHOSWeightArray

End Type

Public :: testHOSModeWeight
