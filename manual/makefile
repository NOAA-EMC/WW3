# Wavewatch III model directory
# The default can be overridden by specifying WWATCH3_DIR on the make
# command-line or in the environment.
WWATCH3_DIR ?= ../model

# Latex commands (assumed to be in user path)
LATEX = latex
BIBTEX = bibtex
DVIPDF = dvipdf

# Target document
TARGET = manual

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

# TeX files generated from $(WWATCH3_DIR)/inp files
INPFILES = \
ww3_grid.tex \
ww3_strt.tex \
ww3_prep.tex \
ww3_shel.tex \
ww3_multi.tex \
ww3_outf.tex \
ww3_outp.tex \
ww3_ounf.tex \
ww3_ounp.tex \
ww3_grib.tex \
gx_outf.tex \
gx_outp.tex

# Bibliography inputs
BIBFILES = manual.bib
BSTFILES = jas.bst

# Style inputs
STYFILES =

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

clean:
	\rm -f $(INPFILES)
	\rm -f *.log *.aux *.bbl *.blg *.toc
	\rm -f *.dvi *.pdf

%.tex : $(WWATCH3_DIR)/inp/%.inp
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
