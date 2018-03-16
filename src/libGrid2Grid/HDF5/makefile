#!/usr/bin/make

### Compiler and Compiling Rule ------------------------------------------------

DIR_CURRENT:= $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

ifndef $(PROJECT_DIR)
	#... Read Configuration File
	include $(DIR_CURRENT)../../../config/config.mk
endif

### Compiling Rules ------------------------------------------------------------

createObj: \
	$(DIR_OBJ)modHDF5Interface.o

$(DIR_OBJ)modHDF5Interface.o: $(DIR_CURRENT)modHDF5Interface.f90
	$(COMPILE_OBJECT_RULE) $< -o $@ $(HDF5_LDFLAG)

### Clean Rule -----------------------------------------------------------------

.PHONY : cleanSub
cleanSub:

.PHONY : cleanSubAll
cleanSubAll:

.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)modHDF5Interface.o

.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)modhdf5interface.mod

.PHONY : clean
clean: \
	cleanSub \
	cleanObj

.PHONY : cleanall
cleanall: \
	cleanSubAll \
	cleanObj \
	cleanlib
