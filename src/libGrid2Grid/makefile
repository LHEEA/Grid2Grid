#!/usr/bin/make

### Compiler and Compiling Rule ------------------------------------------------

#... Get Current makefile path
DIR_CURRENT:= $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

#... Extract Project directory path
PROJECT_NAME=Grid2Grid
PROJECT_DIR:=$(shell echo $(DIR_CURRENT) | awk '{sub(/$(PROJECT_NAME).*/,x)}1')$(PROJECT_NAME)/
#... Project Configuration directory
PROJECT_CONFIG_DIR:=$(PROJECT_DIR)config/

#... Manual setting for directory path
# ifndef $(PROJECT_DIR)
# 	PROJECT_CONFIG_DIR = $(DIR_CURRENT)../../config/
# endif

#... Read make configuration setting
include $(PROJECT_CONFIG_DIR)config.mk

### Compile Order --------------------------------------------------------------

createObj: cleanObj cleanlib
createObj: $(DIR_OBJ)modGrid2GridGlobal.o
createObj: $(DIR_OBJ)modFourier_r2c_FFTW3.o
createObj: $(DIR_OBJ)modHDF5Interface.o
createObj: $(DIR_OBJ)modSurf2Vol.o
createObj: $(DIR_OBJ)modVol2Vol.o
createObj: $(DIR_OBJ)modPostGrid2Grid.o
createObj: $(DIR_OBJ)modGrid2Grid.o

createLib: createObj
createLib: $(DIR_LIB)libGrid2Grid.so

### Object Compiling Rules -----------------------------------------------------

$(DIR_OBJ)modGrid2GridGlobal.o: $(DIR_CURRENT)modGrid2GridGlobal.f90
	@echo $(OBJPRINT)
	$(COMPILE_OBJECT_RULE) $< -o $@

$(DIR_OBJ)modFourier_r2c_FFTW3.o: $(DIR_CURRENT)fftw3/modFourier_r2c_FFTW3.f90
	@echo $(OBJPRINT)
	$(COMPILE_OBJECT_RULE) $< -o $@

$(DIR_OBJ)modHDF5Interface.o: $(DIR_CURRENT)HDF5/modHDF5Interface.f90
	@echo $(OBJPRINT)
	$(COMPILE_OBJECT_RULE) $< -o $@ -I$(HDF5_INCLUDE)

$(DIR_OBJ)modSurf2Vol.o: $(DIR_CURRENT)surf2Vol/modSurf2Vol.f90
	@echo $(OBJPRINT)
	$(COMPILE_OBJECT_RULE) $< -o $@ -I$(HDF5_INCLUDE)

$(DIR_OBJ)modVol2Vol.o: $(DIR_CURRENT)vol2Vol/modVol2Vol.f90
	@echo $(OBJPRINT)
	$(COMPILE_OBJECT_RULE) $< -o $@

$(DIR_OBJ)modPostGrid2Grid.o: $(DIR_CURRENT)postGrid2Grid/modPostGrid2Grid.f90
	@echo $(OBJPRINT)
	$(COMPILE_OBJECT_RULE) $< -o $@ $(FYMC)

$(DIR_OBJ)modGrid2Grid.o: $(DIR_CURRENT)modGrid2Grid.f90
	@echo $(OBJPRINT)
	$(COMPILE_OBJECT_RULE) $< -o $@

$(DIR_LIB)libGrid2Grid.so:
	@echo $(LIBPRINT)
	$(COMPILE_SHARED_LIB_RULE) $(DIR_LIB)libGrid2Grid.so $(DIR_OBJ)*.o $(FFTW_LIB)libfftw3.a

### Clean Rule -----------------------------------------------------------------

#... delete object file
.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)modGrid2GridGlobal.o
	@rm -rf $(DIR_OBJ)modFourier_r2c_FFTW3.o
	@rm -rf $(DIR_OBJ)modHDF5Interface.o
	@rm -rf $(DIR_OBJ)modSurf2Vol.o
	@rm -rf $(DIR_OBJ)modVol2Vol.o
	@rm -rf $(DIR_OBJ)modPostGrid2Grid.o
	@rm -rf $(DIR_OBJ)modGrid2Grid.o

#... delete library and headers
.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)modgrid2gridglobal.mod
	@rm -rf $(DIR_LIB)modfourier_r2c_fftw3.mod
	@rm -rf $(DIR_LIB)modhdf5interface.mod
	@rm -rf $(DIR_LIB)modsurf2vol.mod
	@rm -rf $(DIR_LIB)modvol2vol.mod
	@rm -rf $(DIR_LIB)modpostgrid2grid.mod
	@rm -rf $(DIR_LIB)modgrid2grid.mod
	@rm -rf $(DIR_LIB)libGrid2Grid.so

#... Read recursive clean setting
include $(PROJECT_CONFIG_DIR)cleanTool.mk
