#!/usr/bin/make

### Compiler and Compiling Rule ------------------------------------------------

DIR_CURRENT:= $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

ifndef $(PROJECT_DIR)
	#... Read Configuration File
	include $(DIR_CURRENT)../../config/config.mk
endif

### Compile Order --------------------------------------------------------------

createObj: $(DIR_OBJ)modGrid2GridGlobal.o

createObj: createSub

createSub:
	$(MAKE) createObj -f $(DIR_CURRENT)fftw3/makefile
	$(MAKE) createObj -f $(DIR_CURRENT)HDF5/makefile
	$(MAKE) createObj -f $(DIR_CURRENT)surf2Vol/makefile
	$(MAKE) createObj -f $(DIR_CURRENT)vol2Vol/makefile
	$(MAKE) createObj -f $(DIR_CURRENT)postGrid2Grid/makefile

createObj: $(DIR_OBJ)modGrid2Grid.o

createObj: createLib

createLib: $(DIR_LIB)libGrid2Grid.so

### Object Compiling Rules -----------------------------------------------------

$(DIR_LIB)libGrid2Grid.so:
	$(COMPILE_SHARED_LIB_RULE) $(DIR_LIB)libGrid2Grid.so $(DIR_OBJ)*.o $(LIBRARY_LINK)

$(DIR_OBJ)modGrid2GridGlobal.o: $(DIR_CURRENT)modGrid2GridGlobal.f90
	$(COMPILE_OBJECT_RULE) $< -o $@

$(DIR_OBJ)modGrid2Grid.o: $(DIR_CURRENT)modGrid2Grid.f90
	$(COMPILE_OBJECT_RULE) $< -o $@

### Clean Rule -----------------------------------------------------------------

.PHONY : cleanSub
cleanSub:
	$(MAKE) clean -f $(DIR_CURRENT)fftw3/makefile
	$(MAKE) clean -f $(DIR_CURRENT)HDF5/makefile
	$(MAKE) clean -f $(DIR_CURRENT)surf2Vol/makefile
	$(MAKE) clean -f $(DIR_CURRENT)vol2Vol/makefile
	$(MAKE) clean -f $(DIR_CURRENT)postGrid2Grid/makefile

.PHONY : cleanSubAll
cleanSubAll:
	$(MAKE) cleanall -f $(DIR_CURRENT)fftw3/makefile
	$(MAKE) cleanall -f $(DIR_CURRENT)HDF5/makefile
	$(MAKE) cleanall -f $(DIR_CURRENT)surf2Vol/makefile
	$(MAKE) cleanall -f $(DIR_CURRENT)vol2Vol/makefile
	$(MAKE) cleanall -f $(DIR_CURRENT)postGrid2Grid/makefile

.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)modGrid2GridGlobal.o
	@rm -rf $(DIR_OBJ)modGrid2Grid.o

.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)modgrid2gridglobal.mod
	@rm -rf $(DIR_LIB)modgrid2grid.mod
	@rm -rf $(DIR_LIB)libGrid2Grid.so

.PHONY : clean
clean: \
	cleanSub \
	cleanObj

.PHONY : cleanall
cleanall: \
	cleanSubAll \
	cleanObj \
	cleanlib
