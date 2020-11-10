!!  Step Weight
!!  w = { 1    \omega < \omegaMin
!!      { 0    \omega \ge \omegaMin

Type, extends(typHOSWeightBaseFunc) :: typHOSWeightStepFunc

    !!... Omega Min
    Real(RP) :: omegaMin

Contains

    !!... initialize the class
    Procedure :: initialize => initialize_typHOSWeightStepFunc

    !!... get HOS Weight
    Procedure :: getHOSWeight => getHOSWeight_typHOSWeightStepFunc

    !!... print the class
    Procedure :: print => print_typHOSWeightStepFunc

End Type
