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
createObj: $(DIR_OBJ)mfpGlobal.o
createObj: $(DIR_OBJ)mfpGeneral.o
createObj: $(DIR_OBJ)mfpGeometry.o

createLib: createObj
createLib: $(DIR_LIB)libfymc.a

createExe: createObj
createExe: $(DIR_EXE)fymcMain

### compiling rules ------------------------------------------------------------

$(DIR_EXE)fymcMain:$(DIR_CURRENT)src/main.f90
	@echo $(EXEPRINT)
	@$(FC) -o $@ $(DIR_OBJ)mfpGlobal.o $(DIR_OBJ)mfpGeneral.o $(DIR_OBJ)mfpGeometry.o

$(DIR_LIB)libfymc.a: 
	@echo $(LIBPRINT)
	@ar cr $(DIR_LIB)libfymc.a $(DIR_OBJ)*.o

$(DIR_OBJ)mfpGlobal.o: $(DIR_CURRENT)src/mfpGlobal.f90
	@echo $(OBJPRINT)
	$(COMPILE_OBJECT_RULE) $< -o $@

$(DIR_OBJ)mfpGeneral.o: $(DIR_CURRENT)src/general/mfpGeneral.f90
	@echo $(OBJPRINT)
	$(COMPILE_OBJECT_RULE) $< -o $@

$(DIR_OBJ)mfpGeometry.o: $(DIR_CURRENT)src/geometry/mfpGeometry.f90
	@echo $(OBJPRINT)
	$(COMPILE_OBJECT_RULE) $< -o $@

### Clean Rule -----------------------------------------------------------------

#... delete object file
.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)mfpGlobal.o
	@rm -rf $(DIR_OBJ)mfpGeneral.o
	@rm -rf $(DIR_OBJ)mfpGeometry.o

#... delete library and headers
.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)mfpglobal.mod
	@rm -rf $(DIR_LIB)mfpgeneral.mod
	@rm -rf $(DIR_LIB)mfpgeometry.mod
	@rm -rf $(DIR_LIB)libfymc.a

#... Read recursive clean setting
include $(PROJECT_CONFIG_DIR)cleanTool.mk
