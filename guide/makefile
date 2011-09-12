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

# TeX files in this directory
TEXFILES = report.tex

# Bibliography inputs
BIBFILES = report.bib
BSTFILES = jas.bst

# Style inputs
STYFILES = psfig.sty svn.sty

# All dependencies
DEPFILES = $(TEXFILES) $(BIBFILES) $(BSTFILES) $(STYFILES)

#------------------------------------------------------------------------------#
# Make targets
#------------------------------------------------------------------------------#

$(TARGET): $(TARGET).dvi $(TARGET).pdf

clean:
	\rm -f *.log *.aux *.bbl *.blg *.toc *.out
	\rm -f *.dvi *.pdf

$(TARGET).dvi: $(DEPFILES)
	$(LATEX) $(TARGET)
	$(BIBTEX) $(TARGET)
	$(LATEX) $(TARGET)
	$(BIBTEX) $(TARGET)
	$(LATEX) $(TARGET)

$(TARGET).pdf: $(TARGET).dvi
	$(DVIPDF) $(TARGET)
