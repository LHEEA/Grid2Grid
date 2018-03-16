#!/usr/bin/make

### Compiler and Compiling Rule ------------------------------------------------

DIR_CURRENT:= $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

ifndef $(PROJECT_DIR)
	#... Read Configuration File
	include $(DIR_CURRENT)../../../../config/config.mk
endif

### Compile Order --------------------------------------------------------------

createObj: \
	$(DIR_OBJ)mfpGeneral.o

### compiling rules ------------------------------------------------------------

$(DIR_OBJ)mfpGeneral.o: $(DIR_CURRENT)mfpGeneral.f90
	$(COMPILE_OBJECT_RULE) $< -o $@

### Clean Rule -----------------------------------------------------------------

.PHONY : cleanSub
cleanSub:

.PHONY : cleanSubAll
cleanSubAll:

.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)mfpGeneral.o

.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)mfpgeneral.mod

.PHONY : clean
clean: \
	cleanSub \
	cleanObj

.PHONY : cleanall
cleanall: \
	cleanSubAll \
	cleanObj \
	cleanlib
