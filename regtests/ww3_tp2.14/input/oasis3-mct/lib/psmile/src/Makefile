
SRCS1	= $(shell ls $(COUPLE)/lib/psmile/src/*.F90)
OBJS1	= $(patsubst $(COUPLE)/lib/psmile/src/%.F90, %.o, $(SRCS1))

VPATH 	= .:$(COUPLE)/lib/psmile/src:\
	     $(COUPLE)/lib/psmile/include:\
	     $(NETCDF_INCLUDE):$(MPI_INCLUDE)

LIBRARY = $(ARCHDIR)/lib/libpsmile.${CHAN}.a

clean:
	rm -f i.* *.o *.mod

all:	$(LIBRARY)

$(LIBRARY): $(OBJS1) 
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS1) 

INCLS = -I$(COUPLE)/lib/psmile/include \
        -I$(ARCHDIR)/build/lib/mct \
        -I$(ARCHDIR)/build/lib/scrip \
        -I$(ARCHDIR)/build/lib/pio

INCLSC = -I$(COUPLE)/lib/psmile/include
.SUFFIXES:
.SUFFIXES: .o .F90 .c

%.o: %.F90
	$(F90) $(F90FLAGS) $(INCLS)  -c   $<

%.o: %.c
	$(CC) $(CCFLAGS) $(INCLSC) -c   $<

mod_psmile_io.o: mod_psmile_io.F90
	$(F90) $(F90FLAGS) $(INCLS)  -c   $<

#
# ALL dependencies ...
#

mod_oasis_kinds.o: 
mod_oasis_parameters.o: mod_oasis_kinds.o
mod_oasis_data.o: mod_oasis_kinds.o 
mod_oasis_sys.o: mod_oasis_kinds.o mod_oasis_data.o
mod_oasis_mpi.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o
mod_oasis_timer.o: mod_oasis_kinds.o mod_oasis_sys.o mod_oasis_mpi.o mod_oasis_data.o
mod_oasis_string.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o
mod_oasis_namcouple.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_mpi.o mod_oasis_string.o
mod_oasis_var.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o
mod_oasis_part.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_timer.o
mod_oasis_ioshr.o:  mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_mpi.o mod_oasis_string.o
mod_oasis_io.o:  mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_ioshr.o mod_oasis_mpi.o
mod_oasis_grid.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_io.o
mod_oasis_coupler.o: mod_oasis_kinds.o  mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_mpi.o mod_oasis_var.o mod_oasis_part.o  \
        mod_oasis_string.o mod_oasis_namcouple.o mod_oasis_timer.o mod_oasis_io.o
mod_oasis_advance.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_var.o mod_oasis_part.o mod_oasis_mpi.o \
        mod_oasis_coupler.o mod_oasis_timer.o mod_oasis_io.o
mod_oasis_method.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_coupler.o mod_oasis_namcouple.o \
        mod_oasis_timer.o mod_oasis_ioshr.o mod_oasis_advance.o mod_oasis_grid.o \
        mod_oasis_mpi.o
mod_oasis_getput_interface.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_var.o mod_oasis_advance.o
mod_prism.o: mod_oasis_kinds.o mod_oasis_part.o mod_oasis_sys.o \
	mod_oasis_getput_interface.o mod_oasis_parameters.o \
	mod_oasis_grid.o mod_oasis_method.o mod_oasis_var.o
mod_oasis.o: mod_oasis_kinds.o mod_oasis_part.o mod_oasis_sys.o \
	mod_oasis_getput_interface.o mod_oasis_parameters.o \
	mod_oasis_grid.o mod_oasis_method.o mod_oasis_var.o

