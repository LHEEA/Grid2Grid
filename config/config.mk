### Path -----------------------------------------------------------------------

export PROJECT_NAME=Grid2Grid

export PROJECT_DIR:= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))../

export DIR_SRC=$(PROJECT_DIR)src/
export DIR_OBJ=$(PROJECT_DIR)obj/
export DIR_LIB=$(PROJECT_DIR)lib/
export DIR_EXE=$(PROJECT_DIR)

export DIR_SRCTEST=$(DIR_SRC)testCode/

export EXES=$(PROJECT_DIR)main

MKDIRS = $(DIR_OBJ) $(DIR_LIB)

OBJPRINT = "Compiling object ... $@ $(<F)"
LIBPRINT = "Making library   ... $@"
EXEPRINT = "Making execution ... $@"

### Third Party Library ---------------------------------------------------

export FFTW_LIB=/usr/local/lib/
THIRD_LIB_LINK+=$(FFTW_LIB)libfftw3.a

export HDF5_LIB=/usr/local/lib/hdf5/build/bin/
export HDF5_INCLUDE=$(HDF5_LIB)static/

THIRD_INCLUDE+=-I$(HDF5_INCLUDE)
THIRD_LIB_LINK+=$(HDF5_LIB)libhdf5_fortran.a
THIRD_LIB_LINK+=$(HDF5_LIB)libhdf5_f90cstub.a
THIRD_LIB_LINK+=$(HDF5_LIB)libhdf5.a -ldl -pthread
THIRD_LIB_LINK+=$(HDF5_LIB)libszip.a
THIRD_LIB_LINK+=$(HDF5_LIB)libz.a


export FYMC_LIB=$(DIR_LIB)
export FYMC_INCLUDE=$(DIR_LIB)

THIRD_INCLUDE+=-I$(FYMC_INCLUDE)
THIRD_LIB_LINK+=$(FYMC_LIB)libfymc.a

### compiling Rule --------------------------------------------------------

FC=gfortran
#FC=/usr/local/biogfortran

CFLAG1 = -fPIC -O2 -g -c

ifeq ($(FC),ifort)
	# Array Bound Check
	CFLAG2     = -CB
	# .mod output path
	FLAGMODOUT = -module
else
	# Array Bound Check
	CFLAG2     = -fbounds-check
	# .mod output path
	FLAGMODOUT = -J
endif

# C flags
CFLAGS = $(CFLAG1) $(CFLAG2)

# linking flags
LDFLAGS = -shared

#Flag for writing modules in $(DIR_LIB)
FLAGMOD1= $(FLAGMODOUT) $(DIR_LIB)

#Flag for reading modules in $(OBJ) - I : include .mod file in given dir
FLAGMOD2= -I$(DIR_LIB)

OPTSC = $(FLAGMOD1) $(FLAGMOD2)
OPTSL = $(THIRD_INCLUDE) $(THIRD_LIB)

export COMPILE_OBJECT_RULE=@$(FC) $(CFLAGS) $(OPTSC)
export COMPILE_SHARED_LIB_RULE=@$(FC) $(LDFLAGS) -o

export LIBRARY_LINK=$(THIRD_LIB_LINK)

### Make Folder -----------------------------------------------------------

cmd = $(shell mkdir -p $(DIR_OBJ) $(DIR_LIB) )
$(info ${cmd})

### Auto Compile Rule

export MAKE_SUB_CREATEOBJ = $(MAKE) createObj -f
export MAKE_SUB_CREATELIB = $(MAKE) createLib -f
export MAKE_SUB_CLEAN 	  = $(MAKE) clean -f
export MAKE_SUB_CLEANALL  = $(MAKE) cleanall -f
