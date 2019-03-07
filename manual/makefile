# Set default shell
SHELL = /bin/sh

# Target document
TARGET = manual

# External programs required (assumed to be in user path)
LATEX = latex
BIBTEX = bibtex
DVIPDF = dvipdf

# Make sure external programs are found
ifneq ($(MAKECMDGOALS),clean)
  ifeq ($(shell if [ -z "`which $(LATEX) 2>/dev/null`" ]; then echo 1; fi),1)
    $(error $(LATEX): command not found.  The $(LATEX) command is required)
  endif
  ifeq ($(shell if [ -z "`which $(BIBTEX) 2>/dev/null`" ]; then echo 1; fi),1)
    $(error $(BIBTEX): command not found.  The $(BIBTEX) command is required)
  endif
  ifeq ($(shell if [ -z "`which $(DVIPDF) 2>/dev/null`" ]; then echo 1; fi),1)
    $(error $(DVIPDF): command not found.  The $(DVIPDF) command is required)
  endif
endif

# Set base WAVEWATCH III directory used her
  WW3 = ../.
# WW3 = ~/wwatch3/.

# INPDIR: location of ww3_*.inp input files used for documentation.
# The default can be overridden by specifying INPDIR on the make
# command-line or in the environment.
INPDIR ?= ${WW3}/model/inp
NMLDIR ?= ${WW3}/model/nml

# Make sure INPDIR exists (if not clean)
ifneq ($(MAKECMDGOALS),clean)
  ifeq ($(shell if [ ! -e $(INPDIR) ]; then echo 1; fi),1)
    INPDIR = ${WW3}/inp
  endif
  ifeq ($(shell if [ ! -e $(INPDIR) ]; then echo 1; fi),1)
    $(error INPDIR=$(INPDIR) does not exist.  INPDIR must point to the \
      location of the ww3_*.inp input files used for documentation)
  endif
  ifeq ($(shell if [ ! -e $(NMLDIR) ]; then echo 1; fi),1)
    NMLDIR = ${WW3}/nml
  endif
  ifeq ($(shell if [ ! -e $(NMLDIR) ]; then echo 1; fi),1)
    $(error INPDIR=$(NMLDIR) does not exist.  INPDIR must point to the \
      location of the ww3_*.nml input files used for documentation)
  endif
endif

# Make sure ww3_gspl.sh script is there
GSPL ?= $(WW3)/bin/ww3_gspl.sh

ifneq ($(MAKECMDGOALS),clean)
  ifeq ($(shell if [ ! -e $(GSPL) ]; then echo 1; fi),1)
    GSPL = $(WW3)/model/bin/ww3_gspl.sh
  endif
  ifeq ($(shell if [ ! -e $(GSPL) ]; then echo 1; fi),1)
    $(error GSPL=$(GSPL) does not exist.  GSPL must point to the \
      location of the ww3_gspl.sh used for documentation)
  endif
endif

# Make sure run_test script is there
RUNTEST ?= $(WW3)/regtests/bin/run_test

ifneq ($(MAKECMDGOALS),clean)
  ifeq ($(shell if [ ! -e $(RUNTEST) ]; then echo 1; fi),1)
    RUNTEST = $(WW3)/regtests/bin/run_test
  endif
  ifeq ($(shell if [ ! -e $(RUNTEST) ]; then echo 1; fi),1)
    $(error RUNTEST=$(RUNTEST) does not exist.  RUNTEST must point to the \
      location of the run_test used for documentation)
  endif
endif

# TeX files in this directory
TEXFILES = \
defs.tex \
manual.tex \
start.tex \
intro.tex \
intro/about.tex \
intro/license.tex \
intro/copyright.tex \
intro/group.tex \
intro/ack.tex \
eqs.tex \
eqs/intro.tex \
eqs/prop.tex \
eqs/source.tex \
eqs/sourceice.tex \
eqs/NL1.tex \
eqs/NL2.tex \
eqs/NL1.tex \
eqs/NL3.tex \
eqs/NLS.tex \
eqs/ST1.tex \
eqs/ST2.tex \
eqs/ST3.tex \
eqs/ST4.tex \
eqs/ST5.tex \
eqs/ST6.tex \
eqs/LN1.tex \
eqs/BT1.tex \
eqs/BT2.tex \
eqs/BT3.tex \
eqs/BT4.tex \
eqs/BT8.tex \
eqs/BT9.tex \
eqs/DB1.tex \
eqs/TR1.tex \
eqs/BS1.tex \
eqs/ICE1.tex \
eqs/ICE2.tex \
eqs/ICE3.tex \
eqs/ICE4.tex \
eqs/ICE5.tex \
eqs/IS1.tex \
eqs/IS2.tex \
eqs/REF1.tex \
eqs/IG1.tex \
eqs/XXn.tex \
eqs/output.tex \
num.tex \
num/basics.tex \
num/depth.tex \
num/space.tex \
num/space_trad.tex \
num/space_trad_1.tex \
num/space_trad_2.tex \
num/space_trad_3.tex \
num/space_curv.tex \
num/space_tri.tex \
num/space_SMC.tex \
num/GSE.tex \
num/GSE_null.tex \
num/GSE_BH.tex \
num/GSE_avg.tex \
num/obst.tex \
num/move.tex \
num/rotagrid.tex \
num/spec.tex \
num/spec_1up.tex \
num/spec_uno.tex \
num/spec_uq.tex \
num/source.tex \
num/w_c.tex \
num/tide.tex \
num/space_time_ext.tex \
num/ice.tex \
num/part.tex \
num/track.tex \
num/nest.tex \
num/fig_nest1.tex \
num/fig_nest2.tex \
num/fig_nest3.tex \
run.tex \
run/design.tex \
run/fig_flow.tex \
run/core.tex \
run/fig_log.tex \
run/fig_gspl.tex \
run/das.tex \
run/aux.tex \
run/ww3_grid.tex \
run/ww3_strt.tex \
run/ww3_prep.tex \
run/ww3_prnc.tex \
run/ww3_bound.tex \
run/ww3_bounc.tex \
run/ww3_prtide.tex \
run/ww3_gspl.tex \
run/ww3_shel.tex \
run/ww3_multi.tex \
run/ww3_outf.tex \
run/ww3_ounf.tex \
run/gx_outf.tex \
run/ww3_grib.tex \
run/ww3_outp.tex \
run/ww3_ounp.tex \
run/ww3_gint.tex \
run/gx_outp.tex \
run/ww3_trck.tex \
run/ww3_trnc.tex \
run/ww3_systrk.tex \
run/ww3_uprstr.tex \
impl.tex \
impl/intro.tex \
impl/install.tex \
impl/compile.tex \
impl/fig_make.tex \
impl/switch.tex \
impl/modify.tex \
impl/test_it.tex \
sys.tex \
sys/w3adc.tex \
sys/fig_ad3.tex \
sys/files.tex \
sys/files_w3.tex \
sys/fig_w3init.tex \
sys/fig_w3wave.tex \
sys/files_wm.tex \
sys/das.tex \
sys/aux.tex \
sys/fig_prep.tex \
sys/fig_shel.tex \
sys/fig_post.tex \
sys/optim.tex \
sys/store.tex \
sys/store_grid.tex \
sys/fig_grd1.tex \
sys/fig_grd2.tex \
sys/fig_grd3.tex \
sys/store_MPI.tex \
sys/fig_transpose.tex \
sys/store_multi.tex \
sys/fig_struc1.tex \
sys/fig_struc2.tex \
sys/vars.tex \
sys/modules.tex \
sys/structures.tex \
app/tstep.tex \
app/nest.tex \
app/mpi.tex \
app/scrip.tex \
app/to_do.tex \
app/oasis.tex \
app/nuopc.tex \
app/config.tex \
test.tex

# TeX files generated from $(INPDIR) files
INPFILES = \
ww3_grid.tex \
ww3_grid.tel \
ww3_strt.tex \
ww3_bounc.tex \
ww3_bounc.tel \
ww3_bound.tex \
ww3_prep.tex \
ww3_prtide.tex \
ww3_prnc.tex \
ww3_prnc.tel \
ww3_gspl.tex \
ww3_shel.tex \
ww3_shel.tel \
ww3_multi.tex \
ww3_multi.tel \
ww3_trck.tex \
ww3_trnc.tex \
ww3_trnc.tel \
ww3_outf.tex \
ww3_outp.tex \
ww3_ounf.tex \
ww3_ounf.tel \
ww3_ounp.tex \
ww3_ounp.tel \
ww3_systrk.tex \
ww3_grib.tex \
ww3_gint.tex \
ww3_uprstr.tex \
gx_outf.tex \
gx_outp.tex

# Bibliography inputs
BIBFILES = manual.bib
#BSTFILES = jas.bst
BSTFILES = agufull08.bst

# Style inputs
STYFILES = svn.sty

# Graphics inputs
GFXFILES = \
num/GSE_1.eps \
num/GSE_2.eps \
num/obstr.eps \
num/partition.eps

# All dependencies
DEPFILES = $(TEXFILES) $(INPFILES) ww3_gspl.sh.out run_test.out $(BIBFILES) $(BSTFILES) $(STYFILES) #$(GFXFILES)

#------------------------------------------------------------------------------#
# Make targets
#------------------------------------------------------------------------------#

$(TARGET): $(TARGET).dvi $(TARGET).pdf

inp : $(INPFILES) ww3_gspl.sh.out run_test.out

clean:
	\rm -f $(INPFILES)
	\rm -f *.log */*.aux *.aux *.bbl *.blg *.toc *.out *~ */*~
	\rm -f *.dvi *.pdf

%.tel : $(NMLDIR)/%.nml
	-@echo "Processing $<"
	-@if [ ! -e $< ]; then echo "ERROR: $< not found"; exit 1; fi
	-@\rm -f $@
	-@touch $@
	-@printf '%s\n' \\begin\{footnotesize\} >> $@
	-@printf '%s\n' \\begin\{verbatim\} >> $@
	-@cat $< | cut -c -72 >> $@
	-@printf '%s\n' \\end\{verbatim\} >> $@
	-@printf '%s\n' \\end\{footnotesize\} >> $@

%.tex : $(INPDIR)/%.inp
	-@echo "Processing $<"
	-@if [ ! -e $< ]; then echo "ERROR: $< not found"; exit 1; fi
	-@\rm -f $@
	-@touch $@
	-@printf '%s\n' \\begin\{footnotesize\} >> $@
	-@printf '%s\n' \\begin\{verbatim\} >> $@
	-@cat $< | cut -c -72 >> $@
	-@printf '%s\n' \\end\{verbatim\} >> $@
	-@printf '%s\n' \\end\{footnotesize\} >> $@



ww3_gspl.sh.out : $(GSPL)
	-@echo "Processing $<"
	-@if [ ! -e $< ]; then echo "ERROR: $< not found"; exit 1; fi
	-@\rm -f $@
	-@touch $@
	-@printf '%s\n' \\begin\{verbatim\} >> $@
	-@ $< -h >> $@
	-@printf '%s\n' \\end\{verbatim\} >> $@
	-@echo "   *** NOTE: Error 1 expected ***"

run_test.out : $(RUNTEST)
	-@echo "Processing $<"
	-@if [ ! -e $< ]; then echo "ERROR: $< not found"; exit 1; fi
	-@\rm -f $@
	-@touch $@
	-@printf '%s\n' \\begin\{verbatim\} >> $@
	-@ $< -h >> $@
	-@printf '%s\n' \\end\{verbatim\} >> $@
	-@echo "   *** NOTE: Error 1 expected ***"

$(TARGET).dvi: $(DEPFILES)
	$(LATEX) $(TARGET)
	$(BIBTEX) $(TARGET)
	-@sh bib_strip.sh $(TARGET)
	$(LATEX) $(TARGET)
	$(BIBTEX) $(TARGET)
	-@sh bib_strip.sh $(TARGET)
	$(LATEX) $(TARGET)
	$(BIBTEX) $(TARGET)
	-@sh bib_strip.sh $(TARGET)
	$(LATEX) $(TARGET)

$(TARGET).pdf: $(TARGET).dvi
	$(DVIPDF) $(TARGET)
