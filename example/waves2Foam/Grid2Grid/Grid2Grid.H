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

Class
    Foam::waveTheories::Grid2Grid

Description


SourceFiles
    Grid2Grid.C

Author
    Niels Gjoel Jacobsen, Deltares.  All rights reserved.
    Young-Myung Choi. Bureau, Veritas.

\*---------------------------------------------------------------------------*/

#ifndef Grid2Grid_H
#define Grid2Grid_H

#include "externalWaveForcing.H"
#include "relaxationShape.H"

#include "crossVersionCompatibility.H"
#include "setWaveField.H"

#include "Grid2GridFwd.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace waveTheories
{

/*---------------------------------------------------------------------------*\
                           Class Grid2Grid Declaration
\*---------------------------------------------------------------------------*/

class Grid2Grid
:
    public externalWaveForcing
{

protected:

    // Protected data

    	// Wave property dictionary
        const IOdictionary& waveProps_;

        // Grid2GridCoeffs Dictionary
        const dictionary& coeffDict_;

        // Sea level
        const scalar seaLevel_;

        // Gravity
        vector g_;

        // Water Density
        scalar rhoWater_;

        // Ramp Time
        const scalar Tsoft_;

        // Reference time (Grid2Grid initial time in OpenFOAM time coordinate)
        const scalar referenceTime_;

        // Reference Position (Grid2Gridd Origin position in OpenFOAM space coordinates)
        const vector referencePosition_;

        // Rotational angle in radian from OpenFOAM Coordinates to Grid2Grid Coordinates
        const scalar referenceThetaRad_;

        // Grid2Grid Input File Name
        const word Grid2GridInputFileName_;

        // Rotational matrix from OpenFOAM to Grid2Grid
        tensor transMatrixToOF_;

        // Rotational matrix from Grid2Grid to OpenFOAM
        tensor transMatrixFromOF_;

        // Grid2Grid Wave Inddex
        int  indexGrid2Grid_;

    // Protected member functions
        scalar factor(const scalar&) const;

        //- Construct mapping tensors, if vertical coordinate does not match
        void mappingTensors( const scalar& );

        // Compute Grid2Grid Time
        double getGrid2GridTIme( const scalar& ) const;

        // Compute Grid2Gridd Position
        void getGrid2GridPosition( const point& , double&, double&, double& ) const;

public:

    //- Runtime type information
    TypeName("Grid2Grid");

    // Constructors

        //- from components
        Grid2Grid
        (
            IOobject,
            Time&,
            const fvMesh&
        );


    // Destructor

        ~Grid2Grid()
        {}


    // Member Functions

        // Update Gri2Grid
        virtual void step();

        // Close Gri2Grid. FIXME. Check with Grid2Grid
        virtual void close();

        // Get wave elevation
        virtual scalar eta
        (
            const point&,
            const scalar&
        ) const;

        // Get wave velocity
        virtual vector U
        (
            const point&,
            const scalar&
        ) const;

        // Get wave pressure + hydrostatic
        virtual scalar pExcess
        (
            const point&,
            const scalar&
        ) const;

        bool implementPressure() const
        {
            return true;
        };

        virtual bool writeData(Foam::Ostream&) const
        {
            return true;
        };

};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace waveTheories
} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
