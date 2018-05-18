#!/usr/bin/make

DSRC=./auxiliary/bspline-fortran/src/
DOBJ=obj/

# DSRC=src/
# DOBJ=../../obj/

#compiler flags:
FC=gfortran

CFLAGS = -fPIC -O2 -g # C flags
LDFLAGS = -shared     # linking flags

FLAGMOD1= -J $(DOBJ)
OPTSC0  = -c $(FLAGMOD1)
OPTSC = $(OPTSC0)

createlib: libspline

### auxiliary variables --------------------------------------------------------
COTEXT  = "Compiling $(<F)"
LITEXT  = "Assembling $@"

libspline: $(DOBJ)bspline_module.o
	@echo $(LITEXT)
EXES := bspline_module

$(DOBJ)bspline_module.o: $(DSRC)bspline_module.f90 \
	$(DOBJ)bspline_oo_module.o \
	$(DOBJ)bspline_sub_module.o
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(DOBJ)bspline_oo_module.o: $(DSRC)bspline_oo_module.f90 \
	$(DOBJ)bspline_sub_module.o
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(DOBJ)bspline_sub_module.o: $(DSRC)bspline_sub_module.f90
	@echo $(FC) $(CFLAGS) $(OPTSC) $< -o $@
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

clean:
	@rm -rf $(DOBJ)bspline_module.so $(DOBJ)bspline_module.o $(DOBJ)bspline_oo_module.o $(DOBJ)bspline_sub_module.o
