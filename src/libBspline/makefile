#!/usr/bin/make

### Compiler and Compiling Rule ------------------------------------------------

DIR_CURRENT:= $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

ifndef $(PROJECT_DIR)
	#... Read Configuration File
	include $(DIR_CURRENT)../../config/config.mk
endif

### Compile Order --------------------------------------------------------------

createObj: \
	$(DIR_OBJ)bspline_module.o

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

.PHONY : cleanObj
cleanObj:
	@rm -rf $(DIR_OBJ)bspline_module.o
	@rm -rf $(DIR_OBJ)bspline_oo_module.o
	@rm -rf $(DIR_OBJ)bspline_sub_module.o

.PHONY : cleanlib
cleanlib:
	@rm -rf $(DIR_LIB)bspline_module.so
	@rm -rf $(DIR_LIB)bspline_module.mod
	@rm -rf $(DIR_LIB)bspline_oo_module.mod
	@rm -rf $(DIR_LIB)bspline_sub_module.mod

.PHONY : clean
clean: cleanObj

.PHONY : cleanall
cleanall: clean cleanlib
