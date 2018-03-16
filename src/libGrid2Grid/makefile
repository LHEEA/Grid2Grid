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

#... Set sub directory to run sub makefile
SUB_CREATEOBJ_LIST:= $(DIR_CURRENT)fftw3 	\
					 $(DIR_CURRENT)HDF5 	\
					 $(DIR_CURRENT)surf2Vol \
					 $(DIR_CURRENT)vol2Vol 	\
					 $(DIR_CURRENT)postGrid2Grid

createObj: $(DIR_OBJ)modGrid2GridGlobal.o
createObj: createSub
createObj: $(DIR_OBJ)modGrid2Grid.o
createObj: createLib

createSub:
	$(foreach subMakeObj, $(SUB_CREATEOBJ_LIST), 		\
		$(MAKE_SUB_CREATEOBJ) $(subMakeObj)/makefile;)

createLib: $(DIR_LIB)libGrid2Grid.so

### Object Compiling Rules -----------------------------------------------------

$(DIR_LIB)libGrid2Grid.so:
	$(COMPILE_SHARED_LIB_RULE) $(DIR_LIB)libGrid2Grid.so $(DIR_OBJ)*.o $(FFTW_LIB)libfftw3.a

$(DIR_OBJ)modGrid2GridGlobal.o: $(DIR_CURRENT)modGrid2GridGlobal.f90
	$(COMPILE_OBJECT_RULE) $< -o $@

$(DIR_OBJ)modGrid2Grid.o: $(DIR_CURRENT)modGrid2Grid.f90
	$(COMPILE_OBJECT_RULE) $< -o $@

### Clean Rule -----------------------------------------------------------------

#... delete object file
.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)modGrid2GridGlobal.o
	@rm -rf $(DIR_OBJ)modGrid2Grid.o

#... delete library and headers
.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)modgrid2gridglobal.mod
	@rm -rf $(DIR_LIB)modgrid2grid.mod
	@rm -rf $(DIR_LIB)libGrid2Grid.so

#... Read recursive clean setting
include $(PROJECT_CONFIG_DIR)cleanTool.mk
