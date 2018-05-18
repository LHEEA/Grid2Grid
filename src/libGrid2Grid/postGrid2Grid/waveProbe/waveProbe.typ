Type :: typWaveProbe

    private

        !!- Wave Gauge Name
        Character(len=StringLength) :: name_

        !!- Wave Probe Position
        Real(RP)            :: xPos_, yPos_

        !!- wave elevation
        Real(RP),public     :: eta_

end Type
