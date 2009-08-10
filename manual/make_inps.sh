#!/bin/sh
# --------------------------------------------------------------------------- #
# make_inps.sh : Convert WAVEWATCH III input files to tex files for inclusion #
#                in teh manual (run.tex).                                     #
#                                                                             #
# use     : make_inps.sh                                                      #
#           Reset files to be processed in sections below.                    #
#                                                                             #
# error codes : None.                                                         #
# programs used : None.                                                       #
#                                                                             #
# remarks :                                                                   #
#  - Run from LaTeX directory for manual as the script will touch manual.tex. #
#  - WAVEWACTH III directory name hardwired.                                  #
#                                                                             #
#                                                      Hendrik L. Tolman      #
#                                                      July 2009              #
#                                                                             #
# --------------------------------------------------------------------------- #

# 1. Input files for ww3_* programs

  files='grid strt prep shel multi outp outf grib'

  for file in $files
  do
    echo "processing ww3_$file.inp ..."
    rm -f inp_$file.tex

    data=~/wwatch3/inp/ww3_$file.inp

    echo '\begin{verbatim}'     > inp_$file.tex
    cat $data                  >> inp_$file.tex
#   echo 'will be included'    >> inp_$file.tex
    echo '\end{verbatim}'      >> inp_$file.tex

  done

# 2. Input files for gx* programs (GrADS preprocessors)

  files='outp outf'

  for file in $files
  do
    echo "processing gx_$file.inp ..."
    rm -f inpg_$file.tex

    data=~/wwatch3/inp/gx_$file.inp

    echo '\begin{verbatim}'     > inpg_$file.tex
    cat $data                  >> inpg_$file.tex
#   echo 'will be included'    >> inpg_$file.tex
    echo '\end{verbatim}'      >> inpg_$file.tex

  done

# 3. Force updating of latex by touching manin manual file

  touch manual.tex

# - end of make_inps.sh ----------------------------------------------------- #
