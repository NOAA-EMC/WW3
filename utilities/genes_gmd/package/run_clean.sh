#!/bin/sh
# ---------------------------------------------------------------------------- #
# run_clean.sh : Clean up work and other directories.                          #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 27-Oct-2008 : Origination.                                                   #
# 06-Nov-2008 : Add progs and exe directory.                                   #
# 12-Nov-2008 : Add all_data.ww3.gz files.                                     #
# 11-Dec-2008 : Add matlab *.m~ files.                                         #
# 30-Dec-2008 : Disable section 7 (all_data files).
# 29-Sep-2010 : Add clean up for GrADS maps             
#                                                                              #
#    Copyright 2008-2010 National Weather Service (NWS),                       #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization

  set -e

  echo ' '
  echo 'run_clean.sh :'
  echo '--------------'
  echo '   Clean up work and other directories for GMD optimization.'

  setup='.genes.env'
  test='no'

# ---------------------------------------------------------------------------- #
# 2. Test setup file

  if [ -f ~/$setup ]
  then
    echo "   Setup file $setup found."
    . ~/$setup
  else
    echo "   Setup file $setup NOT found (abort)."
    exit 1
  fi

  echo ' '

# ---------------------------------------------------------------------------- #
# 3. Clean up main directory

  echo "   Cleaning up $genes_main ..."

  cd $genes_main
  rm -f *.inp
  rm -f *.out
  rm -f *.ww3
  rm -f *.ww3.gz
  rm -f fort.*
  rm -f ww3.*
  rm -f *.ctl
  rm -f plot.grads*
  rm -f *.eps
  rm -f quad*.bqf
  rm -f xnl4v5.*
  rm -f *1.data
  rm -f *2.data
  rm -f errors.*
  rm -f log.*
  rm -f mod_def.*
  rm -f wind.wind
  rm -f restart*.*
  rm -f test.*
  rm -f test???.*
  rm -f control.status
  rm -f matlab/*.m~
  rm -f matlab/util/*.m~
  rm -f matlab/temp_?.sh
  rm -f matlab/temp_?.out
  rm -f matlab/*.eps

  if [ "$test" = 'yes' ]
  then
    pwd
    ls -l
  fi

# ---------------------------------------------------------------------------- #
# 4. Clean up work directory

  echo "   Cleaning up $genes_work ..."

  mkdir -p $genes_work
  cd $genes_work
  rm -f *.inp
  rm -f *.out
  rm -f *.ww3.gz
  rm -f *.ww3
  rm -f *.ctl
  rm -f fort.*
  rm -f ww3.*
  rm -f plot.grads*
  rm -f *.eps
  rm -f quad*.bqf
  rm -f xnl4v5.*
  rm -f *1.data
  rm -f *2.data
  rm -f errors.*
  rm -f log.*
  rm -f mod_def.*
  rm -f wind.wind
  rm -f restart*.*
  rm -f test.*
  rm -f test???.*

  if [ "$test" = 'yes' ]
  then
    pwd
    ls -l
  fi

# ---------------------------------------------------------------------------- #
# 5. Clean up progs directory

  echo -n "   Cleaning up $genes_main/progs [y/-] ? : "
  read OK

  if [ "$OK" = 'y' ] || [ "$OK" = 'y' ]
  then
    cd $genes_main/progs

    rm -f *.lst
    rm -f *.mod
    rm -f *.o

    if [ "$test" = 'yes' ]
    then
      pwd
      ls -l
    fi
  fi

# ---------------------------------------------------------------------------- #
# 6. Clean up exe directory

  echo -n "   Cleaning up $genes_main/exe [y/-] ? : "
  read OK

  if [ "$OK" = 'y' ] || [ "$OK" = 'y' ]
  then
    cd $genes_main/exe
    rm -f *.x

    if [ "$test" = 'yes' ]
    then
      pwd
      ls -l
    fi
  fi

# ---------------------------------------------------------------------------- #
# 7. Clean up all_data.ww3 files

# echo -n "   Cleaning up $genes_data/*/test_??/all_data.ww3[.gz] [y/-] ? : "
# read OK

# if [ "$OK" = 'y' ] || [ "$OK" = 'y' ]
# then
#   cd $genes_data/
#   for dir in `ls -d *`
#   do
#     rm -f $dir/test_??/all_data.ww3*
#   done
# fi

# ---------------------------------------------------------------------------- #
# 8. End of script

  echo ' '
  echo 'End of run_clean.sh' 

# End of run_clean.sh -------------------------------------------------------- #
