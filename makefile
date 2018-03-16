#!/usr/bin/make

### Compiler and Compiling Rule ------------------------------------------------

DIR_CURRENT:= $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
DIR_CURRENT_SRC=$(DIR_CURRENT)src/

ifndef $(PROJECT_DIR)
	#... Read Configuration File
	include $(DIR_CURRENT)config/config.mk
endif

### Compile Order --------------------------------------------------------------

Release: createlib

createlib:
	$(MAKE) createObj -f $(DIR_CURRENT_SRC)libBspline/makefile
	$(MAKE) createObj -f $(DIR_CURRENT_SRC)libFyMc/makefile
	$(MAKE) createObj -f $(DIR_CURRENT_SRC)libGrid2Grid/makefile

#createOFlib:

### Clean Rule -----------------------------------------------------------------

.PHONY : cleanSub
cleanSub:
	$(MAKE) clean -f $(DIR_CURRENT_SRC)libBspline/makefile
	$(MAKE) clean -f $(DIR_CURRENT_SRC)libFyMc/makefile
	$(MAKE) clean -f $(DIR_CURRENT_SRC)libGrid2Grid/makefile

.PHONY : cleanSubAll
cleanSubAll:
	$(MAKE) cleanall -f $(DIR_CURRENT_SRC)libBspline/makefile
	$(MAKE) cleanall -f $(DIR_CURRENT_SRC)libFyMc/makefile
	$(MAKE) cleanall -f $(DIR_CURRENT_SRC)libGrid2Grid/makefile

.PHONY : cleanObj
cleanObj:
	@rm -rf obj

.PHONY : cleanlib
cleanlib:
	@rm -rf lib

.PHONY : clean
clean: \
	cleanSub \
	cleanObj

.PHONY : cleanall
cleanall: \
	cleanSubAll \
	cleanObj \
	cleanlib
