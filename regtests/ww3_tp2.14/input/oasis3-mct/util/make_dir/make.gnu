###############################################################################
# On caparmor cluster, you must load the following modules :                  #
# module purge                                                                #
# module load gfortran mpif90                                                 #
###############################################################################
#
# CHAN	: communication technique used in OASIS3 (MPI1/MPI2)
CHAN            = MPI1
#
# Paths for libraries, object files and binaries
#
# COUPLE	: path for oasis3-mct main directory
COUPLE          = $(WWATCH3_DIR)/../regtests/ww3_tp2.14/input/oasis3-mct
#
# ARCHDIR       : directory created when compiling
ARCHDIR         = $(WWATCH3_DIR)/../regtests/ww3_tp2.14/work_oasis3-mct
#
# NetCDF library
NETCDF_INCLUDE  = $(shell $(NETCDF_CONFIG) --includedir) 
NETCDF_LIBRARY  = $(shell $(NETCDF_CONFIG) --flibs) 
#
# Compilers and options
MAKE        = gmake
F90         = mpif90
F           = $(F90)
f90         = $(F90)
f           = $(F90)
CC          = gcc
AR          = ar
ARFLAGS     = -ruv
F90COMP     = -g -O3 -ffree-line-length-0 -fbacktrace -Wall -fconvert=big-endian
F90LINK     =  -O2
#
# MPI library
MPI         = $(shell which gfortran)
MPIDIR      = $(shell dirname $(shell dirname $(MPI)))
MPIBIN      = $(MPIDIR)/bin
MPI_INCLUDE = $(MPIDIR)/include
#
# CPP keys and compiler options
CPPDEF    = -Duse_netCDF -Duse_comm_$(CHAN) -D__VERBOSE -DTREAT_OVERLAY -Duse_realtype_single
#
# Flags 
F90FLAGS_1  = "-g"
f90FLAGS_1  = $(F90FLAGS_1)
FFLAGS_1    = $(F90FLAGS_1)
fFLAGS_1    = $(F90FLAGS_1)
CCFLAGS_1   = $(F90FLAGS_1) 
LDFLAGS     = 
#
###################
#
# Additional definitions that should not be changed
#
#FLIBS		: netcdf library and more
FLIBS		= $(NETCDF_LIBRARY) $(LCPP) $(LIBXML)
# BINDIR        : directory for executables
BINDIR          = $(ARCHDIR)/bin
# LIBBUILD      : contains a directory for each library
LIBBUILD        = $(ARCHDIR)/build/lib
# INCPSMILE     : includes all *o and *mod for each library
INCPSMILE       = -I$(LIBBUILD)/psmile.$(CHAN) -I$(LIBBUILD)/mct -I$(LIBBUILD)/scrip

F90FLAGS  = $(F90FLAGS_1) $(INCPSMILE) $(CPPDEF) -I$(NETCDF_INCLUDE)
f90FLAGS  = $(f90FLAGS_1) $(INCPSMILE) $(CPPDEF) -I$(NETCDF_INCLUDE)
FFLAGS    = $(FFLAGS_1) $(INCPSMILE) $(CPPDEF) -I$(NETCDF_INCLUDE)
fFLAGS    = $(fFLAGS_1) $(INCPSMILE) $(CPPDEF) -I$(NETCDF_INCLUDE)
CCFLAGS   = $(CCFLAGS_1) $(INCPSMILE) $(CPPDEF) -I$(NETCDF_INCLUDE)
#
#############################################################################
