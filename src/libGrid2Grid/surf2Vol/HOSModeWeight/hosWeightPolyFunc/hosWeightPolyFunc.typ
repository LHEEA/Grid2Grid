!!  Exponential Weight
!!  w = { 1                             \omega    < \omegaMin
!!      { 2*\xi**3 - 3*\xi**2 + 1       \omegaMin \le \omega \le \omegaMin
!!      { 0                             \omega    > \omegaMax
!!
!!      \xi = (\omega - \omegaMin) / (  \omegaMax - \omegaMin ) \in [0, 1]
!!

Type, extends(typHOSWeightBaseFunc) :: typHOSWeightPolyFunc

    !!... Omega Min
    Real(RP) :: omegaMin

    !!... Omega Max
    Real(RP) :: omegaMax

    !!... Omega Max - Omega Min
    Real(RP) :: omegaMaxMin

Contains

    !!... initialize the class
    Procedure :: initialize => initialize_typHOSWeightPolyFunc

    !!... get HOS Weight
    Procedure :: getHOSWeight => getHOSWeight_typHOSWeightPolyFunc

    !!... print the class
    Procedure :: print => print_typHOSWeightPolyFunc

End Type
