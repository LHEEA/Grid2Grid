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

#include "fvCFD.H"
#include "interfaceGrid2Grid.H"

int main(int argc, char *argv[])
{
	#include "setRootCase.H"

    Info << "OpenFOAM Program Example to Call Grid2Grid (HOS Wrapper) in OpenFOAM" << endl;

    #include "createTime.H"
    #include "createMesh.H"

	//... Make Input Dictionary ------------------------------------------------

	dictionary HOSDict;
    HOSDict.add(keyType("type"), word("NWT"));

    HOSDict.add(keyType("fileType"), word("ASCII"));

    HOSDict.add(keyType("filePath"), word("../modes_HOS_SWENSE.dat") );

    HOSDict.add(keyType("interpolationOrder"), scalar(3));

    HOSDict.add(keyType("zMeshType"), word("meshRatio"));
	HOSDict.add(keyType("zMin"), scalar(-0.6));
    HOSDict.add(keyType("zMax"), scalar(0.6));
	HOSDict.add(keyType("nZMin"), label(50));
	HOSDict.add(keyType("nZMax"), label(50));

    HOSDict.add(keyType("zMinRatio"), scalar(3.0));
    HOSDict.add(keyType("zMaxRatio"), scalar(3.0));

	dictionary Grid2GridDict;

    const word HOSDictName = word("HOSNWT");

    Grid2GridDict.add(keyType("Grid2Grid"), HOSDictName);
    Grid2GridDict.add(keyType(HOSDictName), HOSDict);

	Info << "Grid2Grid Dictionary : " << nl << Grid2GridDict << endl;

	//... Intialize Grid2Grid with Dictionary ----------------------------------

    const label hosLabel = initializeGrid2Grid(&Grid2GridDict, &mesh);

	Info << "HOS Label : " << hosLabel << endl;

	// Set Position
	const point hosPosition(0.5, 0.0, -0.2);

    // Define Flow Quantities

	Info<< "\nStarting time loop\n" << endl;
	while (runTime.run())
    {
		//... Print RunTime
		const scalar timeValue(runTime.timeOutputValue());
		Info << " sumulTime : " << timeValue << endl;

		//... Correct Grid2Grid
		correctGrid2Grid(hosLabel, timeValue);

		//... Get HOS Wave Elevation
		const scalar eta(getHOSWaveElevation(hosLabel, hosPosition, timeValue));

		//... Get HOS Wave Velocity
		const vector U(getHOSWaveVelocity(hosLabel, hosPosition, timeValue));

		//... Get HOS Wave Velocity
		const scalar pd(getHOSWaveDynamicPressure(hosLabel, hosPosition, timeValue));

		scalar eta1, pd1;
		vector U1;

		getHOSFlow(hosLabel, hosPosition, timeValue, eta1, U1, pd1);

		Info << "   eta     : " << eta << "  Check : " << eta1 << endl;
		Info << "   U       : " << U   << "  Check : " << U1   << endl;
		Info << "   pd      : " << pd  << "  Check : " << pd1  << nl << endl;

		runTime++;

	}

	return 0;
}
