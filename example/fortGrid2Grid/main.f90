!! Program Start ----------------------------------------------------
	Program Main
	!! ------------------------------------------------------------------
	use modCommG2G			!! Use Communication Module
	!! ------------------------------------------------------------------
	Implicit None
	!! Variables --------------------------------------------------------
	Integer,Parameter      :: nChar = 300			!! Default Character Length
	Character(len = nChar) :: grid2gridPath		!! libGrid2Grid.so Path

	integer                :: hosIndex				!! HOS Index
	Character(len = nChar) :: hosSolver				!! HOS Solver (Ocean or NWT)
	Character(len = nChar) :: hosFileName			!! HOS Result File Path

	Character(len = nChar) :: dictFilePath   		!! HOS Solver (Ocean or NWT)

	Double precision       :: zMin, zMax			!! Surf2Vol Domain
	integer                :: nZmin, nZmax		!! Number of vertical grid
	Double precision       :: zMinRatio, zMaxRatio	!! Grading ratio (=3)

	Double precision       :: t, dt			!! Simulation Time, dt
	Double precision       :: x, y, z		!! Computation Point
	Double precision       :: eta, u, v, w, pd	!! HOS Wave Information
	!! Dummy variables --------------------------------------------------
	integer                :: it				!! Dummy time loop integer

	!! Program Body -----------------------------------------------------

	!!!... Write Program Start
	write(*,*) "Test program (Connect to Fortran) to use Grid2Grid shared library"

	!!!... Set libGrid2Grid.so path.
	!!!    It is recommended to use absolute path
	! grid2gridPath = "/usr/lib/libGrid2Grid.so"	(if soft link is made)
	grid2gridPath = "../../lib/libGrid2Grid.so"

	!!!... Load libGrid2Grid.so and connect subroutines
	Call callGrid2Grid(grid2gridPath)

	!!!... Declare HOS Index
	hosIndex = -1

	!!!... Set HOS Type (Ocean or NWT)
	hosSolver = "NWT"

	!!!... Set HOS Result file Path
	hosFileName = "../modes_HOS_SWENSE.dat"
	!hosFileName = "../modes_HOS_SWENSE.hdf5"

	dictFilePath = "Grid2Grid.dict"

	!!!... Set HOS Surf2Vol Domain and Vertical Grid
	zMin = -0.6d0; 				zMax =  0.6d0
	nZmin = 50; 					nZmax = 50
	zMinRatio = 3.d0; 		zMaxRatio = 3.d0

	!!... Initialize Grid2Grid and Get HOS Index
	! Call initializeGrid2Grid(hosSolver, hosFileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio, hosIndex)

	Call initializeGrid2GridDict(dictFilePath, hosIndex)

	!! Time Information
	t  = 0.0d0; 		dt = 0.1d0

	!! Given Point
	x = 0.5d0; 			y = 0.5d0; 			z = -0.5d0

	!! Time Loop
	do it = 1,10

		!! Correct HOS Vol2VOl for given time
		Call correctGrid2Grid(hosIndex, t)

		!! Get Wave Elevation
		Call getHOSeta(hosIndex, x, y , t, eta)

		!! Get Flow Velocity
		Call getHOSU(hosIndex, x, y, z, t, u, v ,w)

		!! Get Dynamic Pressure
		Call getHOSPd(hosIndex, x, y, z, t, pd)

		!! Write Flow Information
		write(*,*) t, eta, u, v, w, pd

		!! Time Update
		t = t + dt
	enddo

	!! Write End of Program
	write(*,*) "Test program (Connect to Fortran) is done ..."
	!! ------------------------------------------------------------------
	End Program
	!! ------------------------------------------------------------------
