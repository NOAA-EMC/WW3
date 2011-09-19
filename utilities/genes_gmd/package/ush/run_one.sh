#!/bin/sh
# ---------------------------------------------------------------------------- #
# run_one.sh : Running single member within run_thread.sh.                     #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 22-Dec-2008 : Origination.                                                   #
# 31-Aug-2009 : Add filtering for error of tests 0X or 01.                     #
# 03-Sep-2009 : Add filtering for error of tests 0X and 01 and 11.             #
#               Removing minor bugs for running shallw water tests.            #
#                                                                              #
#    Copyright 2008-2010 National Weather Service (NWS),                       #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization
# 1.a Check command line

  set -e

  key=$1
  echo "run_one.sh $key :"
  echo "----------------------"

  tests=`$genes_main/ush/get_cases.sh`
  errfile=`echo "$key" | sed 's/snl/err/g'` 
  echo "   test to run: $tests"

  if [ -f ../genes.terr.env ]
  then
    . ../genes.terr.env
  fi

  filt_test=`echo $tests | awk '{print $1}'`
  if [ "$filt_test" != 'test_0X' ] && [ "$filt_test" != 'test_01' ] \
                                   && [ "$filt_test" != 'test_11' ]
  then 
    filt_test=$NULL
  fi

  if [ -n "$TERR" ] && [ "$TERR" != '999.999' ] && [ -n "$filt_test" ]
  then
    echo "   filter error level set to $TERR, applied to $filt_test"
  else
    echo "   no filtering on first test errors."
    filt_test=$NULL
  fi

  all="test_01 test_02 test_03 test_04 test_05 test_06 test_11 test_12 test_13"
  allerr=$all

# ---------------------------------------------------------------------------- #
# 2. Loop over tests

  rm -f errors.test_??.*.*
  rm -f t??.test
  test_pass='yes'

  for test in $tests
  do

# 2.a Check for test passing

    if [ "$test_pass" = 'yes' ]
    then

# 2.b Run test case

      outfile="`echo $test | sed 's/test_/t/g'`.test"
      echo ' '
      echo "   Running test $test [$outfile] ..."
      $genes_main/tests/$test > $outfile

      rm -f gx_*
      rm -f ww3.*
      rm -f mod_def.ww3 out_pnt.ww3 log.* *.wind
      echo ' '          >> $outfile
      cat ww3_grid*.out >> $outfile
      rm -f ww3_*.out

# 2.c Compute errors for this case

      echo "   Compute errors for $test [$test.out] ..."
      $genes_main/run_comp.sh $test local $genes_base >> $outfile

      rm -f outfile
      rm -f *.ww3

# 2.d Composite error for this test case

      loc_err=`$genes_main/ush/get_err_test.sh $test local $genes_base | \
                  awk '{ print $1}'`
      echo "      Error for test case is $loc_err %"

      allerr=`echo $allerr | sed "s/$test/$loc_err/g"`

# 2.e Perform test on error level

      if [ "$test" = "$filt_test" ]
      then
        echo "      This is the error filter test case."
        err1=`echo $loc_err | sed 's/\.//g'`
        err2=`echo $TERR | sed 's/\.//g'`
        if [ "$err1" -gt "$err2" ]
        then
          echo "         Failed filter test."
          test_pass='no'
        else
          echo "         Passed filter test."
          test_pass='yes'
          if [ "$filt_test" = 'test_0X' ]
          then
            filt_test='test_01'
          fi
        fi
      fi

# 2.f Filter test failed, copy errors from test case

    else

      outfile="`echo $test | sed 's/test_/t/g'`.test"
      echo ' '
      echo "   Copy test $test [$outfile] from $filt_test ..."

      echo "Data copied from $filt_test" > $outfile
      cp errors.$filt_test.local.WRT errors.$test.local.WRT

    fi
  done

# ---------------------------------------------------------------------------- #
# 3. Get overall error

  tot_err=`$genes_main/ush/get_err_tot.sh local WRT | awk '{ print $1}'`
  echo ' '
  echo "   Total error is $tot_err [$errfile]"

  for test in $all
  do
    allerr=`echo $allerr | sed "s/$test/-1.00/g"`
  done

  echo "$tot_err $allerr" > ../$errfile

  par_err=`$genes_main/ush/get_err_par.sh local WRT`
  echo "   Error per parameter computed"

  echo "$tot_err $par_err" >> ../$errfile

  rm -f errors.test_??.*.*
  rm -f t??.test

# ---------------------------------------------------------------------------- #
# 4. End of script

  echo ' '
  echo "End of run_one.sh $key"

# End of run_one.sh ---------------------------------------------------------- #
