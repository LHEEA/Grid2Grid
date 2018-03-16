#!/usr/bin/make

### Compiler and Compiling Rule ------------------------------------------------

DIR_CURRENT:= $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

ifndef $(PROJECT_DIR)
	#... Read Configuration File
	include $(DIR_CURRENT)../../../config/config.mk
endif

### Compiling Rules ------------------------------------------------------------

createObj: \
	$(DIR_OBJ)modVol2Vol.o

$(DIR_OBJ)modVol2Vol.o: $(DIR_CURRENT)modVol2Vol.f90
	$(COMPILE_OBJECT_RULE) $< -o $@

### Clean Rule -----------------------------------------------------------------

.PHONY : cleanSub
cleanSub:

.PHONY : cleanSubAll
cleanSubAll:

.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)modVol2Vol.o

.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)modvol2vol.mod

.PHONY : clean
clean: \
	cleanSub \
	cleanObj

.PHONY : cleanall
cleanall: \
	cleanSubAll \
	cleanObj \
	cleanlib
