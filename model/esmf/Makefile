# GNU Makefile for WAVEWATCH III Implementation using NEMS at NCEP

################################################################################

WW3_DIR  := $(shell cd .. 1>/dev/null 2>&1 && pwd)
WW3_BASEDIR  := $(shell cd ../.. 1>/dev/null 2>&1 && pwd)
WW3_BINDIR := $(WW3_DIR)/bin
WW3_TMPDIR := $(WW3_DIR)/tmp
WW3_EXEDIR := $(WW3_DIR)/exe
WW3_COMP ?= gnu
WW3_F90 := gfortran
SWITCHES := $(shell cat switch)

WWATCH3_ENV := $(WW3_BINDIR)/wwatch3.env
NETCDF_CONFIG := $(shell which nc-config)
export WWATCH3_ENV NETCDF_CONFIG

EXE := $(WW3_EXEDIR)/ww3_multi_esmf

################################################################################

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)

################################################################################

# pgi
ifeq ($(WW3_COMP),Portland)
 ESMF_F90COMPILEOPTS := $(ESMF_F90COMPILEOPTS) -byteswapio
else ifeq ("$(WW3_COMP)",$(filter "$(WW3_COMP)","pgi" "datarmor_pgi" "datarmor_pgi_debug"))
 ESMF_F90COMPILEOPTS := $(ESMF_F90COMPILEOPTS) -byteswapio
# intel
else ifeq ("$(WW3_COMP)",$(filter "$(WW3_COMP)","Intel" "hera.intel" "orion.intel" "jet.intel" "s4.intel"))
 ESMF_F90COMPILEOPTS := $(ESMF_F90COMPILEOPTS) -convert big_endian
else ifeq ("$(WW3_COMP)",$(filter "$(WW3_COMP)", "cheyenne.intel" "stampede.intel" "expanse.intel"))
 ESMF_F90COMPILEOPTS := $(ESMF_F90COMPILEOPTS) -convert big_endian
else ifeq ("$(WW3_COMP)",$(filter "$(WW3_COMP)","wcoss_cray" "wcoss_dell_p3" "gaea.intel"))
 ESMF_F90COMPILEOPTS := $(ESMF_F90COMPILEOPTS) -convert big_endian
else ifeq ("$(WW3_COMP)",$(filter "$(WW3_COMP)","wcoss2"))
 ESMF_F90COMPILEOPTS := $(ESMF_F90COMPILEOPTS) -convert big_endian
 WW3_CC=cc
 WW3_F90=ftn
else ifeq ("$(WW3_COMP)",$(filter "$(WW3_COMP)","intel" "datarmor_intel" "datarmor_intel_debug"))
 ESMF_F90COMPILEOPTS := $(ESMF_F90COMPILEOPTS) -convert big_endian
# mpt
else ifeq ("$(WW3_COMP)",$(filter "$(WW3_COMP)","mpt" "datarmor_mpt" "datarmor_mpt_debug"))
 ESMF_F90COMPILEOPTS := $(ESMF_F90COMPILEOPTS) -convert big_endian
# gnu
else ifeq ("$(WW3_COMP)",$(filter "$(WW3_COMP)","gnu" "datarmor_gnu" "datarmor_gnu_debug"))
 ESMF_F90COMPILEOPTS := $(ESMF_F90COMPILEOPTS) -fconvert=big-endian
else ifeq ("$(WW3_COMP)",$(filter "$(WW3_COMP)","hera.gnu" "cheyenne.gnu"))
 ESMF_F90COMPILEOPTS := $(ESMF_F90COMPILEOPTS) -fconvert=big-endian
# error
else
 $(error Unsupported WW3_COMP: $(WW3_COMP))
endif

################################################################################

DEP_FRONTS    := -DFRONT_ESM=ESM
DEP_FRONTS    += -DFRONT_ATM=MDL
DEP_FRONTS    += -DFRONT_OCN=MDL
DEP_FRONTS    += -DFRONT_ICE=MDL
DEP_CMPL_OBJS := utl.o concomp.o mdl.o dum.o
DEP_LINK_OBJS := esm.o utl.o concomp.o mdl.o dum.o

ifeq ($(MAKECMDGOALS),ww3_multi_esmf)
include $(WW3_DIR)/nuopc.mk
DEP_FRONTS    += -DFRONT_WAV=$(ESMF_DEP_FRONT)
DEP_INCS      += $(addprefix -I, $(ESMF_DEP_INCPATH))
DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
endif

################################################################################
################################################################################

.SUFFIXES: .F90

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(DEP_FRONTS) $(DEP_INCS) \
		$(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) \
		$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<


# -----------------------------------------------------------------------------
# ------------------------------------------------------------------------------
default: env setup gout switch
	$(WW3_BINDIR)/w3_make ww3_multi_esmf
	$(MAKE) ww3_multi_esmf

ww3_nems: env setup gout switch
	$(WW3_BINDIR)/w3_make ww3_multi_esmf
	$(WW3_BINDIR)/w3_make ww3_multi
	\cp -f $(WW3_EXEDIR)/ww3_multi $(WW3_BASEDIR)/exec

ww3_nemslibonly: env setup switch
	$(WW3_BINDIR)/w3_make ww3_multi_esmf

ww3_multi_esmf: esmApp.o $(DEP_LINK_OBJS)
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) -o $(EXE) $^ \
		$(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(ESMF_F90ESMFLINKLIBS)

# module dependencies:
esmApp.o: macros.h utl.o esm.o
esm.o: macros.h $(DEP_CMPL_OBJS)
utl.o: macros.h
concomp.o: macros.h utl.o
mdl.o: macros.h utl.o
dum.o: macros.h

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: env setup switch dust clean
dust:
	\rm -f PET*.ESMF_LogFile
clean:
	\rm -f $(EXE) *.o *.mod
distclean: env clean
	$(WW3_BINDIR)/w3_clean -c
	\rm -fr $(WW3_TMPDIR)
	\rm -f $(WWATCH3_ENV)


# ------------------------------------------------------------------------------
# Setup wwatch3.env file for WW3 build or clean
# ------------------------------------------------------------------------------
env:
	@rm -f $(WWATCH3_ENV)
	@touch $(WWATCH3_ENV)
	@echo '#'                                              >> $(WWATCH3_ENV)
	@echo '# ---------------------------------------'      >> $(WWATCH3_ENV)
	@echo '# Environment variables for wavewatch III'      >> $(WWATCH3_ENV)
	@echo '# ---------------------------------------'      >> $(WWATCH3_ENV)
	@echo '#'                                              >> $(WWATCH3_ENV)
	@echo 'WWATCH3_F90      $(WW3_F90)'                    >> $(WWATCH3_ENV)
	@echo 'WWATCH3_DIR      $(WW3_DIR)'                    >> $(WWATCH3_ENV)
	@echo 'WWATCH3_TMP      $(WW3_TMPDIR)'                 >> $(WWATCH3_ENV)
	@echo 'WWATCH3_SOURCE   yes'                           >> $(WWATCH3_ENV)
	@echo 'WWATCH3_LIST     yes'                           >> $(WWATCH3_ENV)
	@echo ''                                               >> $(WWATCH3_ENV)
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Setup comp & link files and auxiliary programs (this is done using w3_setup)
# ------------------------------------------------------------------------------
setup: env
	@$(WW3_BINDIR)/w3_setup -q -c $(WW3_COMP) $(WW3_DIR)
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Setup switch file
# ------------------------------------------------------------------------------
switch:
	@echo "$(SWITCHES)" > $(WW3_BINDIR)/switch
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Setup switch file and compile serial routines ww3_grid, ww3_outf and ww3_outp
# ------------------------------------------------------------------------------
gout:
	@echo "$(SWITCHES)" > $(WW3_BINDIR)/tempswitch
	@sed -e "s/DIST/SHRD/g"\
	     -e "s/OMPG/ /g"\
	     -e "s/OMPH/ /g"\
	     -e "s/MPIT/ /g"\
             -e "s/MPI/ /g"\
             -e "s/PDLIB/ /g"\
                  $(WW3_BINDIR)/tempswitch > $(WW3_BINDIR)/switch
	\rm -f $(WW3_BINDIR)/tempswitch
	$(WW3_BINDIR)/w3_make ww3_grid
	$(WW3_BINDIR)/w3_make ww3_outf
	$(WW3_BINDIR)/w3_make ww3_outp
	$(WW3_BINDIR)/w3_make ww3_prep
	$(WW3_BINDIR)/w3_make ww3_gint
	$(WW3_BINDIR)/w3_make ww3_prnc
	$(WW3_BINDIR)/w3_make ww3_ounf
	$(WW3_BINDIR)/w3_make ww3_ounp
	@echo "$(SWITCHES)" > $(WW3_BINDIR)/tempswitch
	@sed -e "s/DIST/SHRD/g"\
	     -e "s/OMPG/ /g"\
	     -e "s/OMPH/ /g"\
             -e "s/MPIT/ /g"\
             -e "s/MPI/ /g"\
             -e "s/PDLIB/ /g"\
             -e "s/NOGRB/NCEP2 NCO/g"\
                  $(WW3_BINDIR)/tempswitch > $(WW3_BINDIR)/switch
	\rm -f $(WW3_BINDIR)/tempswitch
	$(WW3_BINDIR)/w3_make ww3_grib
	\rm -rf $(WW3_BASEDIR)/exec
	\mkdir -p $(WW3_BASEDIR)/exec
	\cp -f $(WW3_EXEDIR)/ww3_* $(WW3_BASEDIR)/exec

# ------------------------------------------------------------------------------
