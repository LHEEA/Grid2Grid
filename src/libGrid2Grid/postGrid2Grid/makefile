#!/usr/bin/make

### Compiler and Compiling Rule ------------------------------------------------

DIR_CURRENT:= $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

ifndef $(PROJECT_DIR)
	#... Read Configuration File
	include $(DIR_CURRENT)../../../config/config.mk
endif

### Compiling Rules ------------------------------------------------------------

createObj: \
	$(DIR_OBJ)modPostGrid2Grid.o

$(DIR_OBJ)modPostGrid2Grid.o: $(DIR_CURRENT)modPostGrid2Grid.f90
	$(COMPILE_OBJECT_RULE) $< -o $@


### Clean Rule -----------------------------------------------------------------

.PHONY : cleanSub
cleanSub:

.PHONY : cleanSubAll
cleanSubAll:

.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)modPostGrid2Grid.o

.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)modpostgrid2grid.mod

.PHONY : clean
clean: \
	cleanSub \
	cleanObj

.PHONY : cleanall
cleanall: \
	cleanSubAll \
	cleanObj \
	cleanlib
