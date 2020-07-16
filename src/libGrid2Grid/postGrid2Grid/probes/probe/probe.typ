Type :: typProbe

    private

        !!- Wave Gauge Name
        Character(len=StringLength) :: name_

        !!- Wave Probe Position
        Real(RP)            :: xPos_, yPos_, zPos_

        !!... Scalar Value
        Real(RP),public     :: scalar_

        !!... Vector Value
        Real(RP), dimension(3), public :: vector_

end Type
