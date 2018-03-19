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
# 	PROJECT_CONFIG_DIR = $(DIR_CURRENT)config/
# endif

#... Read make configuration setting
include $(PROJECT_CONFIG_DIR)config.mk

### Compile Order --------------------------------------------------------------

DIR_CURRENT_SRC=$(DIR_CURRENT)src/

SUB_CREATEOBJ_LIST= $(DIR_CURRENT_SRC)libBspline 	\
				    $(DIR_CURRENT_SRC)libFyMc 		\
				    $(DIR_CURRENT_SRC)libGrid2Grid

Release: createObj
Release: postG2G

createLib: cleanObj cleanlib
createLib:
	$(MAKE_SUB_CREATEOBJ) $(DIR_CURRENT_SRC)libBspline/makefile
	$(MAKE_SUB_CREATELIB) $(DIR_CURRENT_SRC)libFyMc/makefile
	$(MAKE_SUB_CREATELIB) $(DIR_CURRENT_SRC)libGrid2Grid/makefile

createObj: cleanObj cleanlib
createObj:
	$(foreach subMakeObj, $(SUB_CREATEOBJ_LIST), 		\
		$(MAKE_SUB_CREATEOBJ) $(subMakeObj)/makefile;)

postG2G: $(DIR_OBJ)main.o
	@echo $(EXEPRINT)
	@$(FC) -o $@ $(DIR_OBJ)*.o $(LIBRARY_LINK)
EXES :=$(EXES) postG2G

### Compile Rule ---------------------------------------------------------------

$(DIR_OBJ)main.o: $(DIR_SRC)main.f90
	$(COMPILE_OBJECT_RULE) $< -o $@

### Clean Rule -----------------------------------------------------------------

#... delete object file
.PHONY : cleanObj
cleanObj:
	@rm -rf obj

#... delete library and headers
.PHONY : cleanlib
cleanlib:
	@rm -rf lib
	@rm -rf postG2G

#... Read recursive clean setting
include $(PROJECT_CONFIG_DIR)cleanTool.mk
