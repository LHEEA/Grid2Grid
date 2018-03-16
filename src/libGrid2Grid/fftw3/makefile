#!/usr/bin/make

### Compiler and Compiling Rule ------------------------------------------------

DIR_CURRENT:= $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

ifndef $(PROJECT_DIR)
	#... Read Configuration File
	include $(DIR_CURRENT)../../../config/config.mk
endif

### Compiling Rules ------------------------------------------------------------

createObj: \
	$(DIR_OBJ)modFourier_r2c_FFTW3.o

$(DIR_OBJ)modFourier_r2c_FFTW3.o: $(DIR_CURRENT)modFourier_r2c_FFTW3.f90
	$(COMPILE_OBJECT_RULE) $< -o $@

### Clean Rule -----------------------------------------------------------------

.PHONY : cleanSub
cleanSub:

.PHONY : cleanSubAll
cleanSubAll:

.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)modFourier_r2c_FFTW3.o

.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)modfourier_r2c_fftw3.mod

.PHONY : clean
clean: \
	cleanSub \
	cleanObj

.PHONY : cleanall
cleanall: \
	cleanSubAll \
	cleanObj \
	cleanlib
