# Implementation of Grid2Grid in waves2Foam

* Author: Young-Myung Choi, Bureau Veritas, Paris, France

* Date: 17/10/2020

* Document version: 1.0
* Mail to: youngmyung.choi@bureauveritas.com or benjamin.bouscasse@ec-nantes.fr

## Remarks

As OpenFOAM has divergent version and the waves2Foam has many macros to support many OpenFOAM version, the provided example may not exactly fit into the version what user installed. Therefore, the user has a responsibility to install and implement the Grid2Grid Class. 

The author believes that the user can manage these bugs by referring the following procedure and we hope the users report the bugs/difficulties what they faces during the installation/applications for future development. 

## How to install

The author assumes that the user's linux terminal is under the <u>OpenFOAM environment and has a environmental variable **$WAVES_DIR**</u> where it is defined in "waves2Foam/bin/bashrc".

* Compile the Grid2Grid  (See cmake.org) 

  ```cmake
  cmake -H. -Bbuild -DBUILD_OF_LIB=ON
  cmake --build build
  ```

  Then, check the "libGrid2Grid.so" exists in $FOAM_USER_LIBBIN

  ```bash
  ls $FOAM_USER_LIBBIN/libGrid2Grid.so
  ```

  

1. Copy the folder "Grid2Grid" into wave2Foam library

   ```bash
   cp -r Grid2Gridd $WAVES_DIR/src/waves2Foam/waveTheories/externalWaveForcing/
   ```

   

2. Copy the foldder "Grid2GridProperties" into wave2FoamProcessing library

   ```bash
   cp -r Grid2GridProperties $WAVES_DIR/src/waves2FoamProcessing/preProcessing/setWaveProperties/externalWaveForcing
   ```

3. Change wmake setting of wave2Foam (For example),

   * **File: $WAVES_DIR/src/waves2Foam/Make/files**

   ```makefile
   /* ADD THIS LINE*/
   $(waveTheories)/$(extWF)/Grid2Grid/Grid2Grid.C
   ```

   ​	**For example**

   ```makefile
   ...
   
   /* External wave source */
   extWF=externalWaveForcing
   $(waveTheories)/$(extWF)/externalSource.C
   $(waveTheories)/$(extWF)/externalWaveForcing.C
   $(waveTheories)/$(extWF)/emptyExternal/emptyExternal.C
   $(waveTheories)/$(extWF)/irregularFast/irregularFast.C
   $(waveTheories)/$(extWF)/Grid2Grid/Grid2Grid.C
   
   ...
   ```

   

   * **File: $WAVES_DIR/src/waves2Foam/Make/options** 

     ```
     /* ADD THIS LINE for EXE_INC*/ 
     -I/home/yomy/Codes/Grid2Grid/lib
     
     /* ADD THIS LINE for LIB_LIBS */
     -L/home/yomy/Codes/Grid2Grid/lib \
         -lGrid2Grid
     
     ```

     **For example**

     ```make
     EXE_INC = \
         -DOFVERSION=$(WM_PROJECT_VERSION_NUMBER) \
         ...
         -I$(WAVES_GSL_INCLUDE) \
         -I${Grid2Grid_PATH}/lib
     
     LIB_LIBS = \
         -lfiniteVolume \
         ...
         -lgfortran \
         ...
         -lGrid2Grid
     
     ```

     

4. Change wmake setting of wave2FoamProcessing

   * **File: $WAVES_DIR/src/waves2FoamProcessing/Make/files**

     ```makefile
     /* ADD THIS LINE*/
     $(waveProp)/$(external)/Grid2GridProperties/Grid2GridProperties.C
     ```

     <u>For example</u>

     ```makefile
     ...
     /* External wave forcing */
     external=externalWaveForcing
     $(waveProp)/$(external)/externalSourceProperties/externalSourceProperties.C
     $(waveProp)/$(external)/oceanWave3DProperties/oceanWave3DProperties.C
     $(waveProp)/$(external)/Grid2GridProperties/Grid2GridProperties.C
     ...
     ```

5. Compile **waves2Foam**

   ```bash
   cd $WAVES_DIR
   ./Allwmake
   ```

6. Fix the bugs related to the OpenFOAM version.



## Caution

* The gravitational vector should be a negative z always. (Be careful when you generate the mesh). 

  

## Input Format (constant/waveProperties)

* For example (waves2Properties.input)

  ```c
  externalForcing     Grid2Grid;
  
  Grid2GridCoeffs
  {
      waveType            Grid2Grid;
  
      Tsoft               1;
  
      referenceTime       -20;
  
      referencePosition   (-10 -10 0);
  
      referenceTheta      0;
  
      Grid2GridInputFileName Grid2Grid.dict;
  }
  ```

  

