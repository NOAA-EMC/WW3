#!/bin/sh
# ---------------------------------------------------------------------------- #
# get_err_tot.sh ; Get combined error measure for a given test case.           #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# Remarks: Using get_err_test.sh.                                              #
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

  if [ "$#" -lt '2' ] ; then
    echo "usage: get_err_tot.sh first second" ; exit 1 ; fi

  if [ -f ~/$setup ]
  then
    . ~/$setup
  else
    echo "   Setup file $setup NOT found (abort)."
    exit 2
  fi

  if [ ! -f $genes_main/$cases ]
  then
    echo "   Setup file $cases NOT found (abort)."
    exit 3
  fi

  case1=$1
  case2=$2
  tests=`$genes_main/ush/get_cases.sh`

# ---------------------------------------------------------------------------- #
# 3. Make file with error per case

  rm -f error.sum

  for test in $tests
  do
    err=`$genes_main/ush/get_err_test.sh $test $case1 $case2`
    wgh=`grep $test $genes_main/$cases | awk '{ print $3}'`
    echo "$err $wgh '$test'" >> error.sum
  done

# ---------------------------------------------------------------------------- #
# 4. Compute error
    
# back=`pwd`
# cd $genes_main/progs
# rm -f err_tot.o
# pgf90 err_tot.f90 -byteswapio -o err_tot.x -Mlist *.o
# rm -f err_tot.o
# mv err_tot.x $genes_main/exe/.
# cd $back      

  set +e
  cat error.sum | $genes_main/exe/err_tot.x  > err_tot.out
  OK=$?
  set -e

  rm -f error.sum

  if [ "$OK" != '0' ]
  then
    cat err_tot.out
    exit 5
  fi

# ---------------------------------------------------------------------------- #
# 5 End of script

  tail -1 err_tot.out
  rm -f err_tot.out

# End of get_err_tot.sh ------------------------------------------------------ #
