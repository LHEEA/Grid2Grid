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

namespace Foam
{
    //- Grid2Grid Initial Character Length
    const int nCharGridGrid(300);

    //- Initialize Grid2Grid Class in Fortran
    //
    //  __modgrid2grid_MOD_initializegrid2grid
    //  (
    //      hosSolver,
    //      hosFileName,
    //      zMin,
    //      zMax,
    //      nZmin,
    //      nZmax,
    //      zMinRatio,
    //      zMaxRatio,
    //      hosIndex
    //  )
    //
    //    Input
    //      hosSolver            : "NWT" or "Ocean"
    //      hosFileName          : filePath of HOS mode result file
    //      zMin, zMax           : HOS grid zMin and zMax
    //      nZmin, nZmax         : HOS number of z grid
    //      zMinRatio, zMaxRatio : HOS z grid max/min ratio
    //
    //    Output
    //      hosIndex             : HOS Vol2Vol Index
    //
    extern "C" void __modgrid2grid_MOD_initializegrid2grid
    (
        const char[nCharGridGrid],
        const char[nCharGridGrid],
        const double*,
        const double*,
        const int*,
        const int*,
        const double*,
        const double*,
        int*
    );

    //- Correct Grid2Grid for given simulation Time
    //
    //  __modgrid2grid_MOD_correctgrid2grid(hosIndex, simulTime)
    //
    //    Input
    //      hosIndex   : HOS Vol2Vol Index
    //      simulTime  : Simulation Time
    //
    extern "C" void __modgrid2grid_MOD_correctgrid2grid(const int *, const double *);

    //- Get HOS Wave Elevation
    //
    //  __modgrid2grid_MOD_gethoseta(hosIndex, x, y, t, eta)
    //
    //    Input
    //      hosIndex : HOS Vol2Vol Index
    //      x, y, t  : (x and y) position and simulation Time (t)
    //
    //    Output
    //      eta      : wave elevation
    //
    extern "C" void __modgrid2grid_MOD_gethoseta
    (
        const int *,
        const double *,
        const double *,
        const double *,
        double *
    );

    //- Get HOS Flow Velocity
    //
    //  __modgrid2grid_MOD_gethosu(hosIndex, x, y, z, t, u, v, w)
    //
    //    Input
    //      hosIndex    : HOS Vol2Vol Index
    //      x, y, z, t  : (x, y, z) position and simulation Time (t)
    //
    //    Output
    //      u, v, w     : (x, y, z) - directional flow velocity
    //
    extern "C" void __modgrid2grid_MOD_gethosu
    (
        const int *,
        const double *,
        const double *,
        const double *,
        const double *,
        double *,
        double *,
        double *
    );

    //- Get HOS Dynamic Pressure
    //
    //  __modgrid2grid_MOD_gethospd(hosIndex, x, y, z, t, pd)
    //
    //    Input
    //      hosIndex    : HOS Vol2Vol Index
    //      x, y, z, t  : (x, y, z) position and simulation Time (t)
    //
    //    Output
    //      pd          : Dynamic Pressure p = -rho * d(phi)/dt - 0.5 * rho * |U * U|
    //
    extern "C" void __modgrid2grid_MOD_gethospd
    (
        const int *,
        const double *,
        const double *,
        const double * ,
        const double *,
        double *
    );

    //- Get HOS Wave Elevation, Flow Velocity and Dynamic Pressure
    //
    //  __modgrid2grid_MOD_gethosflow(hosIndex, x, y, z, t, eta, u, v, w, pd)
    //
    //    Input
    //      hosIndex    : HOS Vol2Vol Index
    //      x, y, z, t  : (x, y, z) position and simulation Time (t)
    //
    //    Output
    //      eta         : wave elevation
    //      u, v, w     : (x, y, z) - directional flow velocity
    //      pd          : Dynamic Pressure p = -rho * d(phi)/dt - 0.5 * rho * |U * U|
    //
    extern "C" void __modgrid2grid_MOD_gethosflow
    (
        const int *,
        const double *,
        const double *,
        const double * ,
        const double *,
        double *,
        double *,
        double * ,
        double *,
        double *
    );

}

int main(int argc, char *argv[])
{
	Info << "OpenFOAM Program Example to Call Grid2Grid (HOS Wrapper) in OpenFOAM" << endl;

    // Set HOS Solver Type
    const word HOSsolver_("NWT");
    const word HOSFileName_("../modes_HOS_SWENSE.dat");

    // Set File Name
    string strHOSSolver = string(HOSsolver_);
    string strHOSFileName = string(HOSFileName_);

    // Set HOS Solver Type
    const char *HOSsolver = strHOSSolver.c_str();

    // Set HOS Mode Result File Path
    const char *HOSfileName = strHOSFileName.c_str();

    // Set HOS Z Grid Information
    int indexHOS(-1);

    double zMin(-0.6), zMax(0.6);
    int nZmin(50), nZMax(50);

    double zMinRatio(3.0), zMaxRatio(3.0);

    // Initialize Grid2Grid
    __modgrid2grid_MOD_initializegrid2grid(HOSsolver, HOSfileName,
                                          &zMin, &zMax,
                                          &nZmin, &nZMax,
                                          &zMinRatio, &zMaxRatio, &indexHOS);

    Info << "HOS Label : " << indexHOS << endl;

    // Set Position
    double x(0.5), y(0.0), z(-0.5);

    // Define Flow Quantities
    double eta, u, v, w, pd;

    // Set Simulation Time and Time Difference
    double simulTime(0.0);
    double dt(0.1);

    // Time Loop
    for (int it = 0; it < 11; it++)
    {
        // Correct Grid2Grid
        __modgrid2grid_MOD_correctgrid2grid(&indexHOS, &simulTime);

        // Get Wave Eta
        __modgrid2grid_MOD_gethoseta(&indexHOS, &x, &y, &simulTime, &eta);

        // Get Flow Velocity
        __modgrid2grid_MOD_gethosu(&indexHOS, &x, &y, &z, &simulTime, &u, &v, &w);

        // Get Dynamic Pressure
        __modgrid2grid_MOD_gethospd(&indexHOS, &x, &y, &z, &simulTime, &pd);

        Info << " sumulTime : " << simulTime << endl;
        Info << "   eta     : " << eta << endl;
        Info << "   u, v, w : " << u << " " << v << " " << w << endl;
        Info << "   pd      : " << pd << nl << endl;

        // Get whole Information

        __modgrid2grid_MOD_gethosflow(&indexHOS, &x, &y, &z, &simulTime, &eta, &u, &v, &w, &pd);
        Info << "   eta     : " << eta << endl;
        Info << "   u, v, w : " << u << " " << v << " " << w << endl;
        Info << "   pd      : " << pd << nl << endl;

        // Time Update
        simulTime+=dt;
    }


	return 0;
}
