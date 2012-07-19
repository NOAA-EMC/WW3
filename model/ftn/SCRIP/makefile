#
# Makefile for interpolation code
#
# Notes: netcdf path for LIB and INC will usually need to be 
#        edited by user
# Notes: User should be careful that compiler is that same as that
#        used by WW3 (see /bin/comp)
# Notes: LIB needs to be included in compile of any executable, 
#        but isn't needed for other compiles.
#        INC is needed for several of the other compiles
#
# Reminder for developer ("TO DO") :
# ...COMPILE, FLAGS are hardwired now
# ......rather, they should use compile method consistent with comp file
# ......How can we grab that information from comp and use it here?

# Notes: 
# SCRIP is not accelerated by MPI.
# One may need to have these lines invoked to get MPI to work:
# SCRIP_driver.F90:   call SCRIP_CommInitMessageEnvironment
# SCRIP_driver.F90:   call SCRIP_CommExitMessageEnvironment
# ..and "COMPILE = mpif90"

COMPILE = pgf90
LIB = -L/common/netcdf/pgi/lib -lnetcdff -lnetcdf
INC = -I/common/netcdf/pgi/include
FLAGS = -fastsse
# or
# FLAGS = -C
# or
#FLAGS = -fastsse -mp

#COMPILE = gfortran
#LIB = -L/usr/lib64 -lnetcdff -lnetcdf
#FLAGS = -O3
# or
#FLAGS = 

SRCDIR  = .
OUTPUTDIR  = .
OBJSET  = \
	scrip_kindsmod.o \
	scrip_constants.o \
	scrip_iounitsmod.o \
	scrip_netcdfmod.o \
	scrip_grids.o \
	scrip_remap_vars.o \
	scrip_remap_conservative.o \
	scrip_timers.o \
	scrip_remap_write.o \
	scrip_interface.o \

all: $(OUTPUTDIR)/$(OBJSET)

scrip_kindsmod.o: $(SRCDIR)/scrip_kindsmod.f90
	$(COMPILE) $(FLAGS) -c $(SRCDIR)/scrip_kindsmod.f90

scrip_constants.o: $(SRCDIR)/scrip_constants.f scrip_kindsmod.o
	$(COMPILE) $(FLAGS) -c $(SRCDIR)/scrip_constants.f

scrip_iounitsmod.o: $(SRCDIR)/scrip_iounitsmod.f90 scrip_kindsmod.o
	$(COMPILE) $(FLAGS) -c $(SRCDIR)/scrip_iounitsmod.f90

scrip_errormod.o: $(SRCDIR)/scrip_errormod.f90 scrip_kindsmod.o scrip_iounitsmod.o
	$(COMPILE) $(FLAGS) -c $(SRCDIR)/scrip_errormod.f90

scrip_netcdfmod.o: $(SRCDIR)/scrip_netcdfmod.f90 scrip_kindsmod.o scrip_errormod.o
	$(COMPILE) $(FLAGS) $(INC) -c $(SRCDIR)/scrip_netcdfmod.f90

scrip_grids.o: $(SRCDIR)/scrip_grids.f scrip_kindsmod.o scrip_constants.o scrip_iounitsmod.o \
                scrip_netcdfmod.o scrip_errormod.o
	$(COMPILE) $(FLAGS) $(INC) -c $(SRCDIR)/scrip_grids.f

scrip_remap_vars.o: $(SRCDIR)/scrip_remap_vars.f scrip_kindsmod.o scrip_constants.o scrip_grids.o \
                scrip_errormod.o scrip_iounitsmod.o scrip_netcdfmod.o
	$(COMPILE) $(FLAGS) $(INC) -c $(SRCDIR)/scrip_remap_vars.f

scrip_remap_conservative.o: $(SRCDIR)/scrip_remap_conservative.f scrip_kindsmod.o scrip_constants.o \
		scrip_timers.o scrip_remap_vars.o scrip_grids.o scrip_errormod.o scrip_iounitsmod.o \
                scrip_netcdfmod.o
	$(COMPILE) $(FLAGS) $(INC) -c $(SRCDIR)/scrip_remap_conservative.f

scrip_timers.o: $(SRCDIR)/scrip_timers.f scrip_kindsmod.o
	$(COMPILE) $(FLAGS) -c $(SRCDIR)/scrip_timers.f

scrip_remap_write.o: $(SRCDIR)/scrip_remap_write.f scrip_kindsmod.o scrip_constants.o \
		scrip_netcdfmod.o scrip_remap_vars.o scrip_grids.o scrip_errormod.o \
                scrip_iounitsmod.o
	$(COMPILE) $(FLAGS) $(INC) -c $(SRCDIR)/scrip_remap_write.f

scrip_interface.o: $(SRCDIR)/scrip_interface.f90 scrip_kindsmod.o scrip_constants.o \
		scrip_timers.o scrip_remap_vars.o scrip_grids.o scrip_remap_conservative.o \
		scrip_remap_write.o scrip_iounitsmod.o scrip_errormod.o scrip_netcdfmod.o
	$(COMPILE) $(FLAGS) $(INC) -c $(SRCDIR)/scrip_interface.f90

clean: 
	/bin/rm *.o *.mod

