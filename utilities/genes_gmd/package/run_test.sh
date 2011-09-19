#!/bin/sh
# ---------------------------------------------------------------------------- #
# run_test.sh  : Run a single test case and put the results in the main        #
#                directory for inspection.                                     #
#                                                                              #
# usage: run_test.sh test_ID                                                   #
#                                                                              #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 27-Oct-2008 : Origination.                                                   #
# 01-Dec-2009 : Move test file to output directory if exists.                  #
# 28-Sep-2010 : Add GrADS maps to files to be copied.                          #
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
  echo 'run_test.sh :'
  echo '-------------'

  if [ "$#" -lt "1" ] ; then
    echo "   No test case defined (abort)" ; exit 1 ; fi
  testID=$1

  echo "   Run test $testID and place result in main directory."

  setup='.genes.env'

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

  if [ ! -f $genes_main/tests/$testID ] ; then
    echo "   Test case $testID not found (abort)" ; exit 2 ; fi

# ---------------------------------------------------------------------------- #
# 3. Running the test

  cd $genes_work

  $genes_main/tests/$testID


# ---------------------------------------------------------------------------- #
# 4. Moving the relevant data

  echo ' '

  echo "   Moving output files to $genes_main ..."
  mv *.out $genes_main/.

  echo "   Moving log file(s) to $genes_main ..."
  mv log*.* $genes_main/.

  echo "   Moving spectral file to $genes_main ..."
  mv spec.ww3 $genes_main/.

  echo "   Moving source term file to $genes_main ..."
  mv source.ww3 $genes_main/.
  echo "   Leaving other .ww3 files in $genes_work ..."

  echo "   Moving partitioning file to $genes_main ..."
  mv part.ww3 $genes_main/.

  if [ -f test.ww3 ]
  then
    echo "   Moving test file to $genes_main ..."
    mv test.ww3 $genes_main/.
  fi

  echo "   Deleting remaining .ww3 files ..."
  rm -f mod_def.ww3 out_pnt.ww3

  echo "   Moving GrADS files to $genes_main ..."
  mv *.ctl $genes_main/.
  mv ww3.* $genes_main/.

# ---------------------------------------------------------------------------- #
# 5. End of script

  echo ' '
  echo 'End of run_test.sh' 

# End of run_test.sh --------------------------------------------------------- #
