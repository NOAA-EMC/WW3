#
include $(OASIS_INPUT_PATH)/util/make_dir/make.inc
#
LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libscrip.a
#
##### User configurable options #####
#
# CPP keys for toy model
# type of decomposition :
# DECOMP_APPLE for 1D decomposition
# DECOMP_BOX for 2D decomposition
CPPKEYDECOMP=DECOMP_APPLE
#
# Precision for REAL: USE_DOUBLE_PRECISION or NO_USE_DOUBLE_PRECISION
CPPKEYDP=NO_USE_DOUBLE_PRECISION
##############################
#
CPPLOCAL = -D${CPPKEYDECOMP} -D${CPPKEYDP}
#
OBJ =  routine_hdlerr.o read_dimgrid.o  \
       read_grid.o read_forcing.o read_namelist.o decomp_def.o \
       function_sent.o
#
#-------------------------------------------------------------------------------
# General rules
#-------------------------------------------------------------------------------
#
default: all
#
all: toy_model
#
# Compile and link libaries
oasis3_psmile:
	(cd $(COUPLE)/util/make_dir ; $(MAKE) oasis3_psmile -f TopMakefileOasis3)
#
#
#-------------------------------------------------------------------------------
# Rules for executables
#-------------------------------------------------------------------------------
#
toy_model: $(OBJ) toy_model.o $(LIBPSMILE) Makefile
	$(F90) $(F90LINK) $(LDFLAGS) -o $@ $(OBJ) toy_model.o $(LIBPSMILE) $(FLIBS)
#
#-------------------------------------------------------------------------------
# Rules for compilation
#-------------------------------------------------------------------------------
#
routine_hdlerr.o :	routine_hdlerr.F90
				$(F90) $(INCPSMILE) $(CPPDEF) $(NETCDF_INCLUDE) $(F90COMP) -c routine_hdlerr.F90
read_dimgrid.o :	read_dimgrid.F90
				$(F90) $(INCPSMILE) $(CPPDEF) $(NETCDF_INCLUDE) $(F90COMP) -c read_dimgrid.F90
read_grid.o :		read_grid.F90
				$(F90) $(INCPSMILE) $(CPPDEF) $(NETCDF_INCLUDE) $(F90COMP) -c read_grid.F90
read_forcing.o :	read_forcing.F90
				$(F90) $(INCPSMILE) $(CPPDEF) $(NETCDF_INCLUDE) $(F90COMP) -c read_forcing.F90
read_namelist.o :       read_namelist.F90
				$(F90) $(INCPSMILE) $(CPPDEF) $(NETCDF_INCLUDE) $(F90COMP) -c read_namelist.F90
decomp_def.o : 		decomp_def.F90
				$(F90) $(F90COMP) $(INCPSMILE) $(CPPDEF) $(NETCDF_INCLUDE) $(CPPLOCAL) -c decomp_def.F90
function_sent.o :	function_sent.F90
				$(F90) $(F90COMP) $(INCPSMILE) $(CPPDEF) $(NETCDF_INCLUDE) $(CPPLOCAL) -c function_sent.F90
toy_model.o :   	toy_model.F90 Makefile
				$(F90) $(F90COMP) $(INCPSMILE) $(CPPDEF) $(NETCDF_INCLUDE)  $(CPPLOCAL) -c toy_model.F90
#
#-------------------------------------------------------------------------------
# Utilities
#-------------------------------------------------------------------------------
#
help:
	more Make.help
#
# Clean directory
#
clean:
	-rm -f *.o *.mod *.MOD
	-rm -f i.*.F90 *.L
	-rm -f core core.* 
#
# Make sure to recompile everything
#
realclean: clean remove
remove:
	(cd $(COUPLE)/util/make_dir ; $(MAKE) realclean -f TopMakefileOasis3)
#
#-------------------------------------------------------------------------------
