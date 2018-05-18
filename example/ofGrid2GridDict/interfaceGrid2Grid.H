/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2011-2015 OpenFOAM Foundation
     \\/     M anipulation  |
-------------------------------------------------------------------------------
License
    This file is part of OpenFOAM.

    OpenFOAM is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    OpenFOAM is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with OpenFOAM.  If not, see <http://www.gnu.org/licenses/>.

Application
    testGrid2Grid

Description
    Example program hot to use Grid2Grid (HOS Wrapper) library in OpenFOAM.
    It needs shared library "libGrid2Grid.so".

Author
    YoungMyung Choi (Ecole Centrale de Nantes)

\*---------------------------------------------------------------------------*/

#include "interfaceGrid2GridFwd.H"
#include "fvCFD.H"

namespace Foam
{
    label initializeGrid2Grid
    (
        const dictionary& Grid2GridDict,
        const fvMesh* meshPtr
    )
    {
        const word Grid2GridDictName("Grid2Grid.dict");

        const IOdictionary dictOut
        (
            IOobject
            (
                Grid2GridDictName,
                meshPtr->time().constant(),
                meshPtr->thisDb(),
                IOobject::NO_READ,
                IOobject::NO_WRITE
            ),
            Grid2GridDict
        );

        dictOut.writeObject
        (
            IOstream::ASCII,
            IOstream::originalVersion,
            IOstream::UNCOMPRESSED,
            true
        );

        string strHOSDictFilePath = meshPtr->time().constant()/string(Grid2GridDictName);
        const char *HOSDictFilaPath = strHOSDictFilePath.c_str();
        int indexHOS(-1);

        Foam::Grid2Grid::__modgrid2grid_MOD_initializegrid2griddict(HOSDictFilaPath, &indexHOS);

        return label(indexHOS);
    }

    void correctGrid2Grid
    (
        const label& hosLabel,
        const scalar& hosSimult
    )
    {
        int HOSindex(hosLabel);
        double HOSsimulTime(hosSimult);
        Foam::Grid2Grid::__modgrid2grid_MOD_correctgrid2grid(&HOSindex, &HOSsimulTime);
    }

    scalar getHOSWaveElevation
    (
        const label& hosLabel,
        const point& hosPosition,
        const scalar& hosSimult
    )
    {
        int HOSindex(hosLabel);
        double HOSsimulTime(hosSimult);

        double hosPositionX(hosPosition.x());
        double hosPositionY(hosPosition.y());

        double hosEta;

        Foam::Grid2Grid::__modgrid2grid_MOD_gethoseta(&HOSindex,
                                                      &hosPositionX,
                                                      &hosPositionY,
                                                      &HOSsimulTime,
                                                      &hosEta);

        scalar eta(hosEta);
        return eta;
    }

    vector getHOSWaveVelocity
    (
        const label& hosLabel,
        const point& hosPosition,
        const scalar& hosSimult
    )

    {
        int HOSindex(hosLabel);
        double HOSsimulTime(hosSimult);

        double hosPositionX(hosPosition.x());
        double hosPositionY(hosPosition.y());
        double hosPositionZ(hosPosition.z());

        double hosU, hosV, hosW;

        Foam::Grid2Grid::__modgrid2grid_MOD_gethosu(&HOSindex,
                                                    &hosPositionX,
                                                    &hosPositionY,
                                                    &hosPositionZ,
                                                    &HOSsimulTime,
                                                    &hosU,
                                                    &hosV,
                                                    &hosW);

        vector hosVelocity(hosU, hosV, hosW);
        return hosVelocity;
    }

    scalar getHOSWaveDynamicPressure
    (
        const label& hosLabel,
        const point& hosPosition,
        const scalar& hosSimult
    )
    {
        int HOSindex(hosLabel);
        double HOSsimulTime(hosSimult);

        double hosPositionX(hosPosition.x());
        double hosPositionY(hosPosition.y());
        double hosPositionZ(hosPosition.z());

        double pd;

        Foam::Grid2Grid::__modgrid2grid_MOD_gethospd(&HOSindex,
                                                     &hosPositionX,
                                                     &hosPositionY,
                                                     &hosPositionZ,
                                                     &HOSsimulTime,
                                                     &pd);

        scalar hosPd(pd);
        return hosPd;
    }

    void getHOSFlow
    (
        const label& hosLabel,
        const point& hosPosition,
        const scalar& hosSimult,
        scalar& hosEta,
        vector& hosU,
        scalar& hosPd
    )
    {
        int HOSindex(hosLabel);
        double HOSsimulTime(hosSimult);

        double hosPositionX(hosPosition.x());
        double hosPositionY(hosPosition.y());
        double hosPositionZ(hosPosition.z());

        double eta;
        double u, v, w;
        double pd;

        Foam::Grid2Grid::__modgrid2grid_MOD_gethosflow(&HOSindex,
                                                     &hosPositionX,
                                                     &hosPositionY,
                                                     &hosPositionZ,
                                                     &HOSsimulTime,
                                                     &eta,
                                                     &u,
                                                     &v,
                                                     &w,
                                                     &pd);

        hosEta = scalar(eta);
        hosU   = vector(u, v, w);
        hosPd  = scalar(pd);

    }


}
