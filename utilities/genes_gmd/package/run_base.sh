#!/bin/sh
# ---------------------------------------------------------------------------- #
# run_base.sh  : Run a single test case and put the results in the main        #
#                directory for inspection.                                     #
#                                                                              #
# usage: run_base.sh base_ID [save_grads]                                      #
#                                                                              #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 27-Oct-2008 : Origination.                                                   #
# 03-Nov-2008 : Add partitioned output.                                        #
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
  echo 'run_base.sh :'
  echo '-------------'

  if [ "$#" -lt "1" ] ; then
    echo "   No base_ID defined (abort)" ; exit 1 ; fi

  baseID=$1

  if [ "$#" -gt "1" ] ; then
    save_grads=$2 ; fi

  if [ "$save_grads" != 'yes' ] ; then
    save_grads='no' ; fi
    
  echo -n "   Model ID for this baseline $baseID [y/-] ? : "
  read OK
  if [ "$OK" != 'y' ] && [ "$OK" != 'Y' ] ; then
    exit 0 ; fi

  setup='.genes.env'

# ---------------------------------------------------------------------------- #
# 2. Get setting for running script

  if [ -f ~/$setup ]
  then
    echo "   Setup file $setup found."
    . ~/$setup
  else
    echo "   Setup file $setup NOT found (abort)."
    exit 1
  fi

  all_cases=`$genes_main/ush/get_cases.sh`

  echo "   Save directory  : $genes_data/$baseID"
  mkdir -p $genes_data/$baseID
  echo "   Save GrADS data : $save_grads"
  echo "   Cases to run    : $all_cases"
  echo ' '

# ---------------------------------------------------------------------------- #
# 3.  Running the tests
# 3.a Set up

  cd $genes_work

  for test in $all_cases
  do
    echo "+---------+"
    echo "| $test |"
    echo "+---------+"
    echo ' '

# 3.b Run the test case

    if [ ! -f $genes_main/tests/$test ] ; then
      echo "   Test case $test not found (abort)" ; exit 2 ; fi

# 3.c Save the data

    echo "   Output routed to $genes_main/$test.out"
    $genes_main/tests/$test # > $genes_main/$test.out

    out_dir=$genes_data/$baseID/$test
    mkdir -p $out_dir

    echo "   Removing output files ..."
    rm -f *.out

    echo "   Move log.ww3 to $out_dir ..."
    mv log*.* $out_dir

    echo "   Move spec.ww3 to $out_dir ..."
    mv spec.ww3 $out_dir

    echo "   Move source.ww3 to $out_dir ..."
    mv source.ww3 $out_dir

    echo "   Move part.ww3 to $out_dir ..."
    mv part.ww3 $out_dir

    rm -f *.ww3

    if [ "$save_grads" = 'yes' ]
    then
      echo "   Move GrADS files to $out_dir ..."
      mv *.ctl $out_dir
      mv ww3.* $out_dir
    else
      rm -f ww3.* *.ctl
    fi

    echo ' '

  done

# ---------------------------------------------------------------------------- #
# 4. End of script

  echo ' '
  echo 'End of run_base.sh' 

# End of run_base.sh --------------------------------------------------------- #
