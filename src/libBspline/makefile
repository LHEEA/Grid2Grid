#!/usr/bin/make

### Compiler and Compiling Rule ------------------------------------------------

#... Get Current makefile path
DIR_CURRENT=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

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

createObj: $(DIR_OBJ)bspline_module.o

### Compiling Rules ------------------------------------------------------------

$(DIR_OBJ)bspline_module.o: $(DIR_CURRENT)src/bspline_module.f90 \
	$(DIR_OBJ)bspline_oo_module.o \
	$(DIR_OBJ)bspline_sub_module.o
	$(COMPILE_OBJECT_RULE) $< -o $@

$(DIR_OBJ)bspline_oo_module.o: $(DIR_CURRENT)src/bspline_oo_module.f90 \
	$(DIR_OBJ)bspline_sub_module.o
	$(COMPILE_OBJECT_RULE) $< -o $@

$(DIR_OBJ)bspline_sub_module.o: $(DIR_CURRENT)src/bspline_sub_module.f90
	$(COMPILE_OBJECT_RULE) $< -o $@

### Clean Rule -----------------------------------------------------------------

#... delete object file
.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)bspline_module.o
	@rm -rf $(DIR_OBJ)bspline_oo_module.o
	@rm -rf $(DIR_OBJ)bspline_sub_module.o

#... delete library and headers
.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)bspline_module.so
	@rm -rf $(DIR_LIB)bspline_module.mod
	@rm -rf $(DIR_LIB)bspline_oo_module.mod
	@rm -rf $(DIR_LIB)bspline_sub_module.mod

#... Read recursive clean setting
include $(PROJECT_CONFIG_DIR)cleanTool.mk
