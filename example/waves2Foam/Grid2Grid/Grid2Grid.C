/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright held by original author
     \\/     M anipulation  |
-------------------------------------------------------------------------------
License
    This file is part of OpenFOAM.

    OpenFOAM is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 2 of the License, or (at your
    option) any later version.

    OpenFOAM is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with OpenFOAM; if not, write to the Free Software Foundation,
    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

\*---------------------------------------------------------------------------*/

#include "Grid2Grid.H"
#include "addToRunTimeSelectionTable.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace waveTheories
{

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

defineTypeNameAndDebug(Grid2Grid, 0);

addToRunTimeSelectionTable
(
    externalWaveForcing,
    Grid2Grid,
    externalWaveForcing
);

// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //


Grid2Grid::Grid2Grid
(
    IOobject io,
    Time& rT,
    const fvMesh& mesh
)
:
    externalWaveForcing(io, rT, mesh),

    waveProps_(io.db().lookupObject<IOdictionary>("waveProperties")),
    coeffDict_(waveProps_.subDict("Grid2GridCoeffs")),
    seaLevel_(readScalar(waveProps_.lookup("seaLevel"))),
    g_(vector::zero),
    rhoWater_(1000),
    Tsoft_(coeffDict_.lookupOrDefault<scalar>("Tsoft", 0.0)),
    referenceTime_
    (
        coeffDict_.lookupOrDefault<scalar>("referenceTime", 0.0)
    ),
    referencePosition_
    (
        coeffDict_.lookupOrDefault<vector>("referencePosition", vector::zero )
    ),
    referenceThetaRad_
    (
        coeffDict_.lookupOrDefault<scalar>("referenceTheta",0.0) * M_PI / 180.0
    ),
    Grid2GridInputFileName_(coeffDict_.lookup("Grid2GridInputFileName")),
    transMatrixToOF_(tensor::zero),
    transMatrixFromOF_(tensor::zero)
{
    // Make the mapping tensors
	mappingTensors( referenceThetaRad_ );

    IOdictionary transProp
    (
        IOobject
        (
            "transportProperties",
            "constant",
            mesh_,
            IOobject::MUST_READ,
            IOobject::NO_WRITE
        )
    );

    dictionary sD(transProp.subDict(Foam::waves2Foam::waterPhase()));
    rhoWater_ = dimensionedScalar(sD.lookup("rho")).value();

    // Set Grid2Grid Input File Path
    string strHOSDictFilePath = mesh_.time().constant()/string(Grid2GridInputFileName_);
    const char *HOSDictFilaPath = strHOSDictFilePath.c_str();

    indexGrid2Grid_ = -1;

    // Initialize the Grid2Grid
    Foam::Grid2Grid::__modgrid2grid_MOD_initializegrid2griddict
    (
        HOSDictFilaPath, &indexGrid2Grid_
    );

    Info << "transMatrixToOF_ "   << transMatrixToOF_   << endl;
    Info << "transMatrixFromOF_ " << transMatrixFromOF_ << endl;

    std::cin.get();
}

// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //

scalar Grid2Grid::factor(const scalar& time) const
{
    scalar factor(1.0);
    if (Tsoft_ > 0.0)
    {
        factor = Foam::sin( 2*M_PI/(4.0*Tsoft_)*Foam::min(Tsoft_, time) );
    }
    return factor;
}

void Grid2Grid::mappingTensors( const scalar& thetaRadian )
{
	// Get a unit vector along the direction of gravity.
#if OFPLUSBRANCH==1
    #if OFVERSION<1812
        vector g(uniformDimensionedVectorField
            (
                mesh_.thisDb().lookupObject<uniformDimensionedVectorField>("g")
            ).value());
    #else
        vector g(Foam::meshObjects::gravity::New(mesh_.thisDb().time()).value());
    #endif
#else
        vector g(uniformDimensionedVectorField
            (
                mesh_.thisDb().lookupObject<uniformDimensionedVectorField>("g")
            ).value());
#endif

//	vector g = uniformDimensionedVectorField
//        (
//	        mesh_.thisDb().lookupObject<uniformDimensionedVectorField>("g")
//	    ).value();

    g_ = g;

    g /= Foam::mag(g);
    if ( abs( (g.z() + 1.0) ) > SMALL)
    {
        FatalErrorIn("void Grid2Grid::mappingTensors()")
			<< "The gravitational vector should be given in negative z.  \n"
			<< endl << exit(FatalError);
    }

    transMatrixFromOF_.xx() =   Foam::cos(thetaRadian);
    transMatrixFromOF_.xy() = - Foam::sin(thetaRadian);
    transMatrixFromOF_.yx() =   Foam::sin(thetaRadian);
    transMatrixFromOF_.yy() =   Foam::cos(thetaRadian);
    transMatrixFromOF_.zz() =   1;

	// Create the inverse map
	transMatrixToOF_ = Foam::inv(transMatrixFromOF_);
}

// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

double Grid2Grid::getGrid2GridTIme(const scalar& time)  const
{

    // Set Grid2Grid Simulation time by reference time
    double timeGri2Grid( time + referenceTime_ );

    return timeGri2Grid;
}

void Grid2Grid::getGrid2GridPosition
(
    const point& x,
    double& Grid2GridX,
    double& Grid2GridY,
    double& Grid2GridZ
) const
{
    vector xx = transMatrixFromOF_ & (x + referencePosition_);

    Grid2GridX = xx.x();
    Grid2GridY = xx.y();
    Grid2GridZ = xx.z() - seaLevel_;
    return;
}

void Grid2Grid::step()
{
    // Get OpenFOAM Simulation Time
    double timeGri2Grid( mesh_.time().value() );

    Info << "Grid2Grid Time : " << timeGri2Grid << endl;

    // Update Grid2Grid
    Foam::Grid2Grid::__modgrid2grid_MOD_correctgrid2grid
    (
        &indexGrid2Grid_,
        &timeGri2Grid
    );
}

void Grid2Grid::close()
{
	return;
}

scalar Grid2Grid::eta
(
    const point& x,
    const scalar& time
) const
{
    // Get Grid2Grid Position
    double Grid2GridX, Grid2GridY, Grid2GridZ;
	getGrid2GridPosition(x, Grid2GridX, Grid2GridY, Grid2GridZ);

    // Get Grid2Grid Time
    double Grid2GridTime( getGrid2GridTIme(time) );

	// Declare double variable
    double Grid2GridEta;

    // Get Grid2Grid Eta
    Foam::Grid2Grid::__modgrid2grid_MOD_gethoseta
    (
        &indexGrid2Grid_,
        &Grid2GridX,
        &Grid2GridY,
        &Grid2GridTime,
        &Grid2GridEta
    );

    scalar eta(Grid2GridEta);

    // Multiply by the ramping factor
    eta *= factor(time);
    eta += seaLevel_;

    // Return data
    return eta;
}


scalar Grid2Grid::pExcess
(
    const point& x,
    const scalar& time
) const
{
    // Get Grid2Grid Position
    double Grid2GridX, Grid2GridY, Grid2GridZ;
	getGrid2GridPosition(x, Grid2GridX, Grid2GridY, Grid2GridZ);

    // Get Grid2Grid Time
    double Grid2GridTime( getGrid2GridTIme(time) );

    // Declare double variable
    double Grid2GridPd;

    // Get Grid2Grid Pd
    Foam::Grid2Grid::__modgrid2grid_MOD_gethospd
    (
        &indexGrid2Grid_,
        &Grid2GridX,
        &Grid2GridY,
        &Grid2GridZ,
        &Grid2GridTime,
        &Grid2GridPd
    );

    scalar pd( Grid2GridPd );

    pd *= factor(time);
    pd += rhoWater_ * Foam::mag(g_) * (seaLevel_);

    return pd;
}


vector Grid2Grid::U
(
    const point& x,
    const scalar& time
) const
{
	// Get Grid2Grid Position
    double Grid2GridX, Grid2GridY, Grid2GridZ;
	getGrid2GridPosition(x, Grid2GridX, Grid2GridY, Grid2GridZ);

    Info << "OpenFOAM Position : " << x << endl;
    Info << "Grid2Grid Position : "
         << Grid2GridX << " "
         << Grid2GridY << " "
         << Grid2GridZ << " "
         << endl;

    std::cin.get();

    // Get Grid2Grid Time
    double Grid2GridTime( getGrid2GridTIme(time) );

    // Make the return variables
    double Grid2GridUx, Grid2GridUy, Grid2GridUz;

    // Evaluate the velocity in the given point
    Foam::Grid2Grid::__modgrid2grid_MOD_gethosu
    (
        &indexGrid2Grid_,
        &Grid2GridX,
        &Grid2GridY,
        &Grid2GridZ,
        &Grid2GridTime,
        &Grid2GridUx,
        &Grid2GridUy,
        &Grid2GridUz
    );

    // Map the solution of OF-format but still in rotated form
    vector U(vector::zero);

    U.x() = Grid2GridUx;
    U.y() = Grid2GridUy;
    U.z() = Grid2GridUz;

    // Multiply by the ramping factor
    U *= factor(time);

    // Rotate the solution back to the defined coordinate system in OpenFoam
    U = transMatrixToOF_ & U;

    // Return the solution
    return U;
}

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace waveTheories
} // End namespace Foam

// ************************************************************************* //
