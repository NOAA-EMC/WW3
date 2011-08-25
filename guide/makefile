# Latex commands (assumed to be in user path)
LATEX = latex
BIBTEX = bibtex
DVIPDF = dvipdf

# Target document
TARGET = report

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
