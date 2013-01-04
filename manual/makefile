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

# INPDIR: location of ww3_*.inp input files used for documentation.
# The default can be overridden by specifying INPDIR on the make
# command-line or in the environment.
INPDIR ?= ../inp

# Make sure INPDIR exists (if not clean)
ifneq ($(MAKECMDGOALS),clean)
  ifeq ($(shell if [ ! -e $(INPDIR) ]; then echo 1; fi),1)
    $(error INPDIR=$(INPDIR) does not exist.  INPDIR must point to the \
      location of the ww3_*.inp input files used for documentation)
  endif
endif

# TeX files in this directory
TEXFILES = \
defs.tex \
eqs.tex \
fig_ad3.tex \
fig_flow.tex \
fig_grd1.tex \
fig_grd2.tex \
fig_grd3.tex \
fig_log.tex \
fig_make.tex \
fig_nest1.tex \
fig_nest2.tex \
fig_nest3.tex \
fig_post.tex \
fig_prep.tex \
fig_shel.tex \
fig_struc1.tex \
fig_struc2.tex \
fig_transpose.tex \
fig_w3init.tex \
fig_w3wave.tex \
impl.tex \
intro.tex \
manual.tex \
more.tex \
move.tex \
mpi.tex \
nest.tex \
num.tex \
run.tex \
start.tex \
sys.tex \
test.tex \
tstep.tex

# TeX files generated from $(INPDIR) files
INPFILES = \
ww3_grid.tex \
ww3_strt.tex \
ww3_prep.tex \
ww3_bound.tex \
ww3_shel.tex \
ww3_multi.tex \
ww3_outf.tex \
ww3_outp.tex \
ww3_ounf.tex \
ww3_ounp.tex \
ww3_systrk.tex \
ww3_grib.tex \
gx_outf.tex \
gx_outp.tex

# Bibliography inputs
BIBFILES = manual.bib
BSTFILES = jas.bst

# Style inputs
STYFILES = svn.sty

# Graphics inputs
GFXFILES = \
GSE_1.eps \
GSE_2.eps \
obstr.eps \
partition.eps

# All dependencies
DEPFILES = $(TEXFILES) $(INPFILES) $(BIBFILES) $(BSTFILES) $(STYFILES) $(GFXFILES)

#------------------------------------------------------------------------------#
# Make targets
#------------------------------------------------------------------------------#

$(TARGET): $(TARGET).dvi $(TARGET).pdf

inp : $(INPFILES)

clean:
	\rm -f $(INPFILES)
	\rm -f *.log *.aux *.bbl *.blg *.toc *.out
	\rm -f *.dvi *.pdf

%.tex : $(INPDIR)/%.inp
	-@echo "Processing $<"
	-@if [ ! -e $< ]; then echo "ERROR: $< not found"; exit 1; fi
	-@\rm -f $@
	-@touch $@
	-@printf '%s\n' \\begin\{verbatim\} >> $@
	-@cat $< >> $@
	-@printf '%s\n' \\end\{verbatim\} >> $@

$(TARGET).dvi: $(DEPFILES)
	$(LATEX) $(TARGET)
	$(BIBTEX) $(TARGET)
	$(LATEX) $(TARGET)
	$(BIBTEX) $(TARGET)
	$(LATEX) $(TARGET)

$(TARGET).pdf: $(TARGET).dvi
	$(DVIPDF) $(TARGET)
