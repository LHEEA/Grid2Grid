# Grid2Grid

ECN HOS wrapper program. It generates following flow information from result file of HOS.

     - Wave Elevation
     - Flow Velocity
     - Dynamic Pressure

It needs FFTW third party library. You can download FFTW library from

http://www.fftw.org/download.html

It requires HDF5 third party library (version >= 1.8.16). You can:

* either download HDF5 library from
  https://support.hdfgroup.org/HDF5/

* or install package libhdf5-dev on Ubuntu 16.04.1

How to compile FFTW library

     1. Download FFTW library and Extract

     2. ./configure --prefix=/DOWNLOAD_PATH

     3. make CFLAGS="-fPIC"

     4. make install

     5. ln -s /DOWNLOAD_PATH/lib/fftw3.a /usr/local/lib/fftw3.a
        ln -s /DOWNLOAD_PATH/lib/fftw3.la /usr/local/lib/fftw3.la

How to compile HDF5 library 

     1. Download HDF5 library with Cmake and extract

        https://support.hdfgroup.org/HDF5/release/cmakebuild.html 

        download with Unix version    

     2. Add following lines in a file "HDF5options.cmake"

        #set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_NO_PACKAGES:BOOL=ON")
        ### Create install package with external libraries (szip, zlib)

        set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_PACKAGE_EXTLIBS:BOOL=ON")
        set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_FORTRAN:BOOL=ON")
        set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_SHARED_LIBS:BOOL=ON")
        set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_CPP_LIB:BOOL=ON")

     3. Compile with the following command
 
        ctest -S HDF5config.cmake,BUILD_GENERATOR=Unix -C Release -V -O hdf5.log

     4. Check the library exists in a followng path 

         ls build/bin/libhdf5.a build/bin/libhdf5_fortran.a build/bin/libszip.a build/bin/libz.a

     5. make soft link 

        ln -s ..../Cmake-hdf5-x /usr/local/lib/hdf5

     6. Set HDF5_Library path in a "CMakeLists.txt" in Grid2Grid 

        set(HDF5_LIB_PATH /usr/local/lib/hdf5/build/bin)

Install Grid2Grid

    There are two ways to compile Grid2Grid. 
       
      1. gnu make 
      2. cmake

    It is recommended to use cmake instead of gnu make. Makefile will be deleted in a following update. 

    Installation using CMake

      - Instllation without HDF5 library
    
        cmake -H. -Bbuild 
      
        cmake --build build

      - Installation with HDF5 library (Add the flag: -DHDF_LIBRARY:STRIN="ON")
 
        cmake -H. -Bbuild -DHDF_LIBRARY:STRIN="ON"

        cmake --build build

      - Installation libGrid2Grid on the path $FOAM_USER_LIBBIN (Add the flag -DBUILD_OF_LIB=ON)

        cmake -H. -BbuildOF -DBUILD_OF_LIB=ON -DHDF_LIBRARY:STRING="ON"

        cmake --build buildOF

    Installation using GNU make

      - Path to Grid2Grid and run following command which you want.

      make
        : compile main.f90, it generates post processing program of Grid2Grid.
          And main.f90 contains some subroutine how to connect Grid2Grid to Fortran Program.

      make createlib
        : make shared library(libGrid2Grid.so) in /obj directory.
