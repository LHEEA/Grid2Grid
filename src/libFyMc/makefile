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

DIR_CURRENT_SRC=$(DIR_CURRENT)src/

#... Set sub directory to run sub makefile
SUB_CREATEOBJ_LIST= $(DIR_CURRENT_SRC)general \
				    $(DIR_CURRENT_SRC)geometry

createObj: $(DIR_OBJ)mfpGlobal.o
createObj: createSub
createObj: createLib

#... Run sub-make file
createSub:
	$(foreach subMakeObj, $(SUB_CREATEOBJ_LIST), 		\
		$(MAKE_SUB_CREATEOBJ) $(subMakeObj)/makefile;)

createLib: $(DIR_LIB)libfymc.a

### compiling rules ------------------------------------------------------------

$(DIR_LIB)libfymc.a:
	@ar cr $(DIR_LIB)libfymc.a $(DIR_OBJ)*.o

$(DIR_OBJ)mfpGlobal.o: $(DIR_CURRENT_SRC)mfpGlobal.f90
	$(COMPILE_OBJECT_RULE) $< -o $@

### Clean Rule -----------------------------------------------------------------

#... delete object file
.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)mfpGlobal.o

#... delete library and headers
.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)mfpglobal.mod
	@rm -rf $(DIR_LIB)libfymc.a

#... Read recursive clean setting
include $(PROJECT_CONFIG_DIR)cleanTool.mk
