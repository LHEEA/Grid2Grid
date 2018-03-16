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
# 	PROJECT_CONFIG_DIR = $(DIR_CURRENT)../../../config/
# endif

#... Read make configuration setting
include $(PROJECT_CONFIG_DIR)config.mk

### Compile Order --------------------------------------------------------------

createObj: $(DIR_OBJ)modHDF5Interface.o

### Compiling Rules ------------------------------------------------------------

$(DIR_OBJ)modHDF5Interface.o: $(DIR_CURRENT)modHDF5Interface.f90
	$(COMPILE_OBJECT_RULE) $< -o $@ $(HDF5_LDFLAG)

### Clean Rule -----------------------------------------------------------------

#... delete object file
.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)modHDF5Interface.o

#... delete library and headers
.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)modhdf5interface.mod

#... Read recursive clean setting
include $(PROJECT_DIR)config/cleanTool.mk
