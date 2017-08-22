#!/usr/bin/make

### Path ----------------------a-------------------------------------------------

DSRC=src/
DOBJ=obj/
DEXE=./

### FFT library (later it should be fixed.)
DTHRDLIB=/usr/local/lib/

### compiler (gfortran) --------------------------------------------------------
FC=gfortran

#CFLAGS = -fPIC -Wall -Wextra -O2 -g # C flags
CFLAGS = -fPIC -O2 -g # C flags

LDFLAGS = -shared  					# linking flags

FLAGMOD1= -J $(DOBJ) #Flag for writing modules in $(OBJ)
FLAGMOD2= -I $(DOBJ) #Flag for reading modules in $(OBJ)
OPTSC0  = -c $(FLAGMOD1)
OPTSL0  =  $(FLAGMOD2)
MKDIRS  = $(DOBJ)

### Targets for compilation ----------------------------------------------------

Release: OPTSC = $(OPTSC0)
Release: OPTSL = $(OPTSL0)
Release: $(MKDIRS)
Release: postG2G

testspline: OPTSC = $(OPTSC0)
testspline: OPTSL = $(OPTSL0)
testspline: $(MKDIRS)
testspline: testspline

createlib: OPTSC = $(OPTSC0)
createlib: OPTSL = $(OPTSL0)
createlib: $(MKDIRS)
createlib: libGrid2Grid

createOFlib: OPTSC = $(OPTSC0)
createOFlib: OPTSL = $(OPTSL0)
createOFlib: $(MKDIRS)
createOFlib: libOFGrid2Grid

### auxiliary variables --------------------------------------------------------
COTEXT  = "Compiling $(<F)"
LITEXT  = "Assembling $@"

postG2G: $(DOBJ)main.o
	@echo $(COTEXT)
	@$(FC) -o $@ $(DOBJ)*.o $(DTHRDLIB)libfftw3.a
EXES :=$(EXES) postG2G

testspline: $(DOBJ)testspline.o
	@echo $(COTEXT)
	@$(FC) -o $@ $(DOBJ)*.o $(DTHRDLIB)libfftw3.a
EXES :=$(EXES) testspline

libGrid2Grid: $(DOBJ)libGrid2Grid.so
	@echo $(LITEXT)
EXES :=$(EXES) libGrid2Grid

libOFGrid2Grid: $(FOAM_USER_LIBBIN)/libGrid2Grid.so
	@echo $(LITEXT)
EXES :=$(EXES) libGrid2Grid

### compiling rules ------------------------------------------------------------

$(DOBJ)main.o: $(DSRC)main.f90 \
	$(DOBJ)modGrid2Grid.o \
    $(DOBJ)modPostGrid2Grid.o
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(DOBJ)testspline.o: $(DSRC)test/testSpline.f90 \
	$(DOBJ)modGrid2Grid.o \
    $(DOBJ)modPostGrid2Grid.o
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(FOAM_USER_LIBBIN)/libGrid2Grid.so: \
	$(DOBJ)modVol2Vol.o \
	$(DOBJ)modSurf2Vol.o \
	$(DOBJ)modGrid2Grid.o \
	$(DOBJ)modPostGrid2Grid.o
	@$(FC) $(LDFLAGS) -o $(FOAM_USER_LIBBIN)/libGrid2Grid.so $(DOBJ)*.o $(DTHRDLIB)libfftw3.a

$(DOBJ)libGrid2Grid.so: \
	$(DOBJ)modVol2Vol.o \
	$(DOBJ)modSurf2Vol.o \
	$(DOBJ)modGrid2Grid.o \
	$(DOBJ)modPostGrid2Grid.o
	@$(FC) $(LDFLAGS) -o $(DOBJ)libGrid2Grid.so $(DOBJ)*.o $(DTHRDLIB)libfftw3.a

$(DOBJ)modGrid2Grid.o: $(DSRC)modGrid2Grid.f90 \
	$(DOBJ)modGrid2GridType.o \
	$(DOBJ)modVol2Vol.o
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(DOBJ)modPostGrid2Grid.o: $(DSRC)modPostGrid2Grid.f90 \
	$(DOBJ)modGrid2GridType.o \
	$(DOBJ)modVol2Vol.o
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(DOBJ)modVol2Vol.o: $(DSRC)modVol2Vol.f90 \
	$(DOBJ)modSurf2Vol.o \
	$(DOBJ)modV2VSplineInterp.o \
	$(DOBJ)modGrid2GridType.o
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(DOBJ)modSurf2Vol.o: $(DSRC)modSurf2Vol.f90 \
	$(DOBJ)modNWTsurf2vol.o \
	$(DOBJ)modOceansurf2vol.o
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(DOBJ)modV2VSplineInterp.o: $(DSRC)modV2VSplineInterp.f90 \
	$(DOBJ)modGrid2GridType.o
	@echo $(COTEXT)
	$(MAKE) createlib -f ./auxiliary/bspline-fortran/makefile
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(DOBJ)modNWTsurf2vol.o: $(DSRC)modNWTsurf2vol.f90 \
	$(DOBJ)modFourier_r2c_FFTW3NWT.o \
	$(DOBJ)modGrid2GridType.o
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(DOBJ)modOceansurf2vol.o: $(DSRC)modOceansurf2vol.f90 \
	$(DOBJ)modFourier_r2c_FFTW3_ocean.o \
	$(DOBJ)modGrid2GridType.o
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(DOBJ)modFourier_r2c_FFTW3NWT.o: $(DSRC)modFourier_r2c_FFTW3NWT.f90 \
	$(DOBJ)modGrid2GridType.o
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(DOBJ)modFourier_r2c_FFTW3_ocean.o: $(DSRC)modFourier_r2c_FFTW3_ocean.f90 \
	$(DOBJ)modGrid2GridType.o
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@

$(DOBJ)modGrid2GridType.o: $(DSRC)modGrid2GridType.f90
	@echo $(COTEXT)
	@$(FC) $(CFLAGS) $(OPTSC) $< -o $@


### phony auxiliary rules ------------------------------------------------------
.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@
.PHONY : cleanobj
cleanobj:
	@echo deleting objects
	@rm -fr $(DOBJ) @rm *.so *.o
.PHONY : cleanmod
cleanmod:
	@echo deleting mods
	@rm -fr $(DMOD)
.PHONY : cleanexe
cleanexe:
	@echo deleting exes
	@rm -f $(addprefix $(DEXE),$(EXES))
.PHONY : clean
clean: cleanobj cleanmod
.PHONY : cleanall
cleanall: clean cleanexe
