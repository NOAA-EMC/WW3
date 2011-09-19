#!/bin/sh
# ---------------------------------------------------------------------------- #
# get_err_test.sh ; Get combined error measure for a given test case.          #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 26-Nov-2008 : Origination.                                                   #
#                                                                              #
#    Copyright 2008-2010 National Weather Service (NWS),                       #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization

  set -e
  setup='.genes.env'
  cases='genes.cases.env'

# ---------------------------------------------------------------------------- #
# 2. Test setup files and script input

  if [ "$#" -lt '3' ] ; then
    echo "usage: get_err_test.sh test first second" ; exit 1 ; fi

  if [ -f ~/$setup ]
  then
    . ~/$setup
  else
    echo "   Setup file $setup NOT found (abort)."
    exit 2
  fi

  test=$1
  case1=$2
  case2=$3
  weight=`echo $test | sed 's/test/w/g' | awk '{ print $1}'`

  file1=$genes_main/genes.$weight.env
  if [ ! -f $file1 ]
  then
    file1="$genes_main/genes.weights.env"
    if [ ! -f $file1 ]
    then
      echo "    Weight file or alternate not found." ; exit 3
    fi
  fi

  file2=errors.$test.$case1.$case2
  if [ ! -f $file2 ]
  then
    echo "    Error file not found." ; exit 4
  fi

# ---------------------------------------------------------------------------- #
# 3. Compute error
    
# back=`pwd`
# cd $genes_main/progs
# rm -f err_test.o
# pgf90 err_test.f90 -byteswapio -o err_test.x -Mlist *.o
# rm -f err_test.o
# mv err_test.x $genes_main/exe/.
# cd $back      

  set +e
  sed -n '/^\$.*/!p' $file1 $file2 | cut -c10- | \
                             $genes_main/exe/err_test.x  > err_test.out
  OK=$?
  set -e

  if [ "$OK" != '0' ]
  then
    cat err_test.out
    exit 5
  fi

# ---------------------------------------------------------------------------- #
# 5 End of script

  tail -1 err_test.out
  rm -f err_test.out

# End of get_err_test.sh ----------------------------------------------------- #
