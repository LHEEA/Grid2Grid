#!/usr/bin/make

### Compiler and Compiling Rule ------------------------------------------------

DIR_CURRENT:= $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
DIR_CURRENT_SRC=$(DIR_CURRENT)src/

ifndef $(PROJECT_DIR)
	#... Read Configuration File
	include $(DIR_CURRENT)../../config/config.mk
endif

### Compile Order --------------------------------------------------------------

createObj: \
	$(DIR_OBJ)mfpGlobal.o

createObj:
	$(MAKE) createObj -f $(DIR_CURRENT_SRC)general/makefile
	$(MAKE) createObj -f $(DIR_CURRENT_SRC)geometry/makefile

createObj:createLib

createLib: \
	$(DIR_LIB)libfymc.a

### compiling rules ------------------------------------------------------------

$(DIR_LIB)libfymc.a:
	@ar cr $(DIR_LIB)libfymc.a $(DIR_OBJ)*.o

$(DIR_OBJ)mfpGlobal.o: $(DIR_CURRENT_SRC)mfpGlobal.f90
	@echo $(COTEXT)
	$(COMPILE_OBJECT_RULE) $< -o $@

### Clean Rule -----------------------------------------------------------------

.PHONY : cleanSub
cleanSub:
	$(MAKE) clean -f $(DIR_CURRENT_SRC)general/makefile
	$(MAKE) clean -f $(DIR_CURRENT_SRC)geometry/makefile

.PHONY : cleanSubAll
cleanSubAll:
	$(MAKE) cleanall -f $(DIR_CURRENT_SRC)general/makefile
	$(MAKE) cleanall -f $(DIR_CURRENT_SRC)geometry/makefile

.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)mfpGlobal.o

.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)mfpglobal.mod
	@rm -rf $(DIR_LIB)libfymc.a

.PHONY : clean
clean: \
	cleanSub \
	cleanObj

.PHONY : cleanall
cleanall: \
	cleanSubAll \
	cleanObj \
	cleanlib
