!!  Exponential Weight
!!  w = { 1                                             \omega < \omegaMin
!!      { exp[ -\alpha*(\omega/\omegaMin - 1)^{2n} ]    \omega \ge \omegaMin

Type, extends(typHOSWeightBaseFunc) :: typHOSWeightExpFunc

    !!... Omega Min
    Real(RP) :: omegaMin

    !!... Alpha
    Real(RP) :: alpha

    !!... nPowerExp
    Integer  :: twoNExpPower

Contains

    !!... initialize the class
    Procedure :: initialize => initialize_typHOSWeightExpFunc

    !!... get HOS Weight
    Procedure :: getHOSWeight => getHOSWeight_typHOSWeightExpFunc

    !!... print the class
    Procedure :: print => print_typHOSWeightExpFunc

End Type
