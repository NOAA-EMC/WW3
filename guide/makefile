# Set default shell
SHELL = /bin/sh

# Target document
TARGET = guide

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
# WW3 = ~/wwatch3

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
TEXFILES = guide.tex docu_subr.tex docu_mod.tex

# Bibliography inputs
BIBFILES = guide.bib
BSTFILES = jas.bst

# Style inputs
STYFILES = psfig.sty svn.sty

# All dependencies
DEPFILES = $(TEXFILES) run_test.out $(BIBFILES) $(BSTFILES) $(STYFILES)

#------------------------------------------------------------------------------#
# Make targets
#------------------------------------------------------------------------------#

$(TARGET): $(TARGET).dvi $(TARGET).pdf

clean:
	\rm -f *.log *.aux *.bbl *.blg *.toc *.out *~
	\rm -f *.dvi *.pdf

run_test.out : $(RUNTEST)
	-@echo "Processing $<"
	-@if [ ! -e $< ]; then echo "ERROR: $< not found"; exit 1; fi
	-@\rm -f $@
	-@touch $@
	-@printf '%s\n' \\begin\{verbatim\} >> $@
	-@ $< -h >> $@
	-@printf '%s\n' \\end\{verbatim\} >> $@
	-@echo "   *** NOTE: Error 1 expected ..."

$(TARGET).dvi: $(DEPFILES)
	$(LATEX) $(TARGET)
	$(BIBTEX) $(TARGET)
	-@sh bib_strip.sh $(TARGET)
	$(LATEX) $(TARGET)
	$(BIBTEX) $(TARGET)
	-@sh bib_strip.sh $(TARGET)
	$(LATEX) $(TARGET)

$(TARGET).pdf: $(TARGET).dvi
	$(DVIPDF) $(TARGET)
