#!/usr/bin/make

#main building variables
DSRC    = .
DOBJ    = obj/
DMOD    = mod/
DEXE    = ./
LIBDIR    = ../lib/
LINKLIB = $(LIBDIR)libfftw3.a $(LIBDIR)liblapack.a $(LIBDIR)librefblas.a
# ifort compiler
FC      = ifort
OPTSC0  = -c -fpp -module mod
OPTSL0  =  -module mod
OPTFLAGS= -O3 -ip -ipo -vec-report0 -fp-model fast=2
DBFLAGS = -O0 -CA -CU -CB -fpe0 -debug full -traceback
# gfortran compiler
#FC      = gfortran
#OPTFLAGS= -O3 -march=corei7 -msse2 -funroll-loops -fno-protect-parens -ffast-math
#DBFLAGS = -O0 -Wline-truncation -fpack-derived -finit-integer=inf -finit-real=nan -fbacktrace -ffpe-trap=zero,overflow -ffpe-summary=all -fimplicit-none -fcheck=all -Wall -Wtabs -Wextra -Wunderflow  -Wno-zerotrip
#FLAGMOD1= -J $(DOBJ) #Flag for writing modules in $(OBJ)
#FLAGMOD2= -I $(DOBJ) #Flag for reading modules in $(OBJ)
#OPTSC0  = -c $(FLAGMOD1)
#OPTSL0  =  $(FLAGMOD2)
#
# top-level rule, to compile everything.
all:cleanall Release
all_Debug:cleanall Debug
#
# Targets for compilation
Release: OPTSC = $(OPTFLAGS) $(OPTSC0)
Release: OPTSL = $(OPTFLAGS) $(OPTSL0)
Release: $(DEXE)MAIN_HOS #$(DSRC)/*.f90

Debug: OPTSC = $(DBFLAGS) $(OPTSC0)
Debug: OPTSL = $(DBFLAGS) $(OPTSL0)
Debug: $(DEXE)MAIN_HOS #$(DSRC)/*.f90

createlib_Debug: OPTSC = $(DBFLAGS) $(OPTSC0)
createlib_Debug: OPTSL = $(DBFLAGS) $(OPTSL0)
createlib_Debug: $(DEXE)libHOSSurf2Vol #$(DSRC)/*.f90

createlib_Release: OPTSC = $(OPTFLAGS) $(OPTSC0)
createlib_Release: OPTSL = $(OPTFLAGS) $(OPTSL0)
createlib_Release: $(DEXE)libHOSSurf2Vol #$(DSRC)/*.f90
	
#
VPATH   = $(DSRC) $(DOBJ) $(DMOD)
MKDIRS  = $(DOBJ) $(DMOD) $(DEXE)
LCEXES  = $(shell echo $(EXES) | tr '[:upper:]' '[:lower:]')
EXESPO  = $(addsuffix .o,$(LCEXES))
EXESOBJ = $(addprefix $(DOBJ),$(EXESPO))

#auxiliary variables
COTEXT  = "Compiling $(<F)"
LITEXT  = "Assembling $@"

#building rules
$(DEXE)MAIN_HOS: $(MKDIRS) $(DOBJ)main_hos.o
	@rm -f $(filter-out $(DOBJ)main_hos.o,$(EXESOBJ))
	@echo $(LITEXT)
	@$(FC) $(OPTSL) $(DOBJ)*.o $(LINKLIB) -o $@
EXES := $(EXES) MAIN_HOS

$(DEXE)libHOSSurf2Vol: $(MKDIRS) $(DOBJ)libHOSSurf2Vol.o
	@echo $(LITEXT)
	@ar rc $(DOBJ)libHOSSurf2Vol.a $(DOBJ)*.o
EXES := $(EXES) libHOSSurf2Vol
	
#compiling rules

$(DOBJ)main_hos.o: $(DSRC)/Main.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modPost_processing_ocean.o \
	$(DOBJ)modPost_processing_NWT.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)libHOSSurf2Vol.o: $(DSRC)/libHOSSurf2Vol.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modPost_processing_ocean.o \
	$(DOBJ)modPost_processing_NWT.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)modType.o: $(DSRC)/modType.f90
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@	

$(DOBJ)modPost_processing_ocean.o: $(DSRC)/modPost_processing_ocean.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_ocean.o \
	$(DOBJ)modInput_post_process_ocean.o \
	$(DOBJ)modRead_files_ocean.o \
	$(DOBJ)modAnalysis_wavefield_ocean.o \
	$(DOBJ)modOutput_post_process_ocean.o \
	$(DOBJ)modReconstruction_ocean.o \
	$(DOBJ)modFourier_r2c_FFTW3_ocean.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)modVariables_3d_ocean.o: $(DSRC)/modVariables_3d_ocean.f90 \
	$(DOBJ)modType.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)modInput_post_process_ocean.o: $(DSRC)/modInput_post_process_ocean.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_ocean.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@
	
$(DOBJ)modRead_files_ocean.o: $(DSRC)/modRead_files_ocean.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_ocean.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)modAnalysis_wavefield_ocean.o: $(DSRC)/modAnalysis_wavefield_ocean.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_ocean.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)modOutput_post_process_ocean.o: $(DSRC)/modOutput_post_process_ocean.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_ocean.o 
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)modReconstruction_ocean.o: $(DSRC)/modReconstruction_ocean.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_ocean.o \
	$(DOBJ)modFourier_r2c_FFTW3_ocean.o \
	$(DOBJ)modRead_files_ocean.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@
	
$(DOBJ)modFourier_r2c_FFTW3_ocean.o: $(DSRC)/modFourier_r2c_FFTW3_ocean.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_ocean.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)modPost_processing_NWT.o: $(DSRC)/modPost_processing_NWT.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_NWT.o \
	$(DOBJ)modInput_post_process_NWT.o \
	$(DOBJ)modRead_files_NWT.o \
	$(DOBJ)modOutput_post_process_NWT.o \
	$(DOBJ)modReconstruction_NWT.o \
	$(DOBJ)modFourier_r2c_FFTW3_NWT.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)modVariables_3d_NWT.o: $(DSRC)/modVariables_3d_NWT.f90 \
	$(DOBJ)modType.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)modInput_post_process_NWT.o: $(DSRC)/modInput_post_process_NWT.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_NWT.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@
	
$(DOBJ)modRead_files_NWT.o: $(DSRC)/modRead_files_NWT.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_NWT.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)modOutput_post_process_NWT.o: $(DSRC)/modOutput_post_process_NWT.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_NWT.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)modReconstruction_NWT.o: $(DSRC)/modReconstruction_NWT.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_NWT.o \
	$(DOBJ)modFourier_r2c_FFTW3_NWT.o \
	$(DOBJ)modRead_files_NWT.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@
	
$(DOBJ)modFourier_r2c_FFTW3_NWT.o: $(DSRC)/modFourier_r2c_FFTW3_NWT.f90 \
	$(DOBJ)modType.o \
	$(DOBJ)modVariables_3d_NWT.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@	
#phony auxiliary rules
.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@
.PHONY : cleanobj
cleanobj:
	@echo deleting objects
	@rm -fr $(DOBJ)
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
