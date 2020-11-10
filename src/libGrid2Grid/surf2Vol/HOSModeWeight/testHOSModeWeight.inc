Subroutine testHOSModeWeight()

    use mfpGeneral, only : typDictionaryPtr

Implicit None

    type(typDictionaryPtr)  :: fileDict
    type(typDictionaryPtr)  :: HOSDict, weightDict
    Type(typHOSModeWeightFunc) :: hosModeWeightFunc

    Real(RP)              :: omegaMin, omegaMax, domega
    Real(RP), allocatable :: omega(:), weight(:)
    integer :: nOmega, iomega

    Call fileDict%initialize("", "postGrid2Grid", '.dict')

    HOSDict = fileDict%subDict("HOSOcean")

    weightDict = HOSDict%subDict("HOSAmpWeight")

    Call hosModeWeightFunc%initialize(weightDict)

    nOmega = 500
    allocate( omega(nOmega), weight(nOmega) )
    omegaMin = 0.0_RP
    omegaMax = 5.0_RP

    domega = ( omegaMax - omegaMin ) / (nOmega - 1.0_RP)

    do iomega = 1, nOmega
        omega(iomega) =  (iomega - 1.0_RP) * domega
    enddo

    Call hosModeWeightFunc%getHOSWeight(omega, weight)

    open(unit = 301, file = "HOSWeight.dat", status= "replace")
    do iomega = 1, nOmega
        write(301, 1001) omega(iomega), weight(iomega)
    enddo
    close(301)

    1001 format(99e20.7)

End Subroutine
