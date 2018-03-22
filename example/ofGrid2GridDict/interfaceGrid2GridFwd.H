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

namespace Foam
{
namespace Grid2Grid
{
    //- Grid2Grid Initial Character Length
    const int nCharGridGrid(300);

    //- Initialize Grid2Grid Class in Fortran with dictionary file
    extern "C" void __modgrid2grid_MOD_initializegrid2griddict
    (
        const char[nCharGridGrid],  // hosDictionary File Path [Input]
        int*                        // hosIndex (Output) HOS Vol2Vol Index
    );

    //- Initialize Grid2Grid Class in Fortran with Arguments
    extern "C" void __modgrid2grid_MOD_initializegrid2grid
    (
        const char[nCharGridGrid],  // hosSolver [Input : "NWT" or "Ocean"]
        const char[nCharGridGrid],  // hosFileName [Input : filePath of HOS mode result file]
        const double*,              // zMin     [Input : HOS grid zMin, zMax]
        const double*,              // zMax
        const int*,                 // nZMin    [Input : HOS number of z grid]
        const int*,                 // nZMax
        const double*,              // zMinRatio [Input : HOS z grid max/min ratio]
        const double*,              // zMaxRatio
        int*                        // hosIndex (Output) HOS Vol2Vol Index
    );

    //- Correct Grid2Grid for given simulation Time
    extern "C" void __modgrid2grid_MOD_correctgrid2grid
    (
        const int *,            // hosIndex   : HOS Vol2Vol Index
        const double *          // simulTime  : Simulation Time
    );

    //- Get HOS Wave Elevation
    extern "C" void __modgrid2grid_MOD_gethoseta
    (
        const int *,        // hosIndex [Input : HOS Vol2Vol Index]
        const double *,     // x [Input : HOS x position]
        const double *,     // y [Input : HOS y position]
        const double *,     // t [Input : HOS Simulation Time]
        double *            // eta [Output : Wave Elevation]
    );

    //- Get HOS Flow Velocity
    extern "C" void __modgrid2grid_MOD_gethosu
    (
        const int *,        // hosIndex [Input : HOS Vol2Vol Index]
        const double *,     // x [Input : HOS x position]
        const double *,     // y [Input : HOS y position]
        const double *,     // z [Input : HOS z position]
        const double *,     // t [Input : HOS Simulation Time]
        double *,           // u [Output x-directional fluid velocity]
        double *,           // v [Output y-directional fluid velocity]
        double *            // w [Output z-directional fluid velocity]
    );

    //- Get HOS Dynamic Pressure
    extern "C" void __modgrid2grid_MOD_gethospd
    (
        const int *,        // hosIndex [Input : HOS Vol2Vol Index]
        const double *,     // x [Input : HOS x position]
        const double *,     // y [Input : HOS y position]
        const double *,     // z [Input : HOS z position]
        const double *,     // t [Input : HOS Simulation Time]
        double *            // pd [Output dynamic pressure]
    );

    //- Get HOS Wave Elevation, Flow Velocity and Dynamic Pressure
    extern "C" void __modgrid2grid_MOD_gethosflow
    (
        const int *,        // hosIndex [Input : HOS Vol2Vol Index]
        const double *,     // x [Input : HOS x position]
        const double *,     // y [Input : HOS y position]
        const double *,     // z [Input : HOS z position]
        const double *,     // t [Input : HOS Simulation Time]
        double *,           // eta [Output : Wave Elevation]
        double *,           // u [Output x-directional fluid velocity]
        double *,           // v [Output y-directional fluid velocity]
        double *,           // w [Output z-directional fluid velocity]
        double *            // pd [Output dynamic pressure]
    );
}
}
