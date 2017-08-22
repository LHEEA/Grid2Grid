# Grid2Grid

ECN HOS wrapper program. It generates following flow information from result file of HOS.

     - Wave Elevation
     - Flow Velocity
     - Dynamic Pressure
     
It needs FFTW third party library. You can download FFTW library from 

http://www.fftw.org/download.html 

How to compile FFTW library 

     1. Download FFTW library and Extract 
     
     2. ./configure --prefix=/DOWNLOAD_PATH
     
     3. make CFLAGS="-fPIC"
     
     4. make install
     
     5. ln -s /DOWNLOAD_PATH/lib/fftw3.a /usr/local/lib/fftw3.a
        ln -s /DOWNLOAD_PATH/lib/fftw3.la /usr/local/lib/fftw3.la
        
Install Grid2Grid

     1. Path to Grid2Grid and run following command which you want.
     
      make
	: compile main.f90, it generates post processing program of Grid2Grid.
	  And main.f90 contains some subroutine how to connect Grid2Grid to Fortran Program.
     
      make createlib
        : make shared library(libGrid2Grid.so) in /obj directory.
     
      make createOFlib
        : Within OpenFOAM Environments, it makes shared library(libGrid2Grid.so) 
          in $FOAM_USER_LIBBIN
          
          
          


