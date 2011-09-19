#!/bin/sh
# ---------------------------------------------------------------------------- #
# get_err_par.sh ; Combine error per parameter from sevearl cases.             #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# Remarks: Error files axpected to be there.                                   #
#                                                                              #
# 24-Dec-2008 : Origination.                                                   #
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
    echo "usage: get_err_par.sh first second" ; exit 1 ; fi

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
# 3. Make input file

  rm -f input

  for test in $tests
  do
    weight=`echo $test | sed 's/test/w/g' | awk '{ print $1}'`

    file1=$genes_main/genes.$weight.env
    if [ ! -f $file1 ]
    then
      file1="$genes_main/genes.weights.env"
      if [ ! -f $file1 ]
      then
        echo "    Weight file or alternate not found." ; exit 4
      fi
    fi
  
    file2=errors.$test.$case1.$case2
    if [ ! -f $file2 ]
    then
      echo "    Error file not found." ; exit 5
    fi

    cat $genes_main/$cases | grep $test | awk '{print $3}'      >> input
    sed -n '/^\$.*/!p' $file1 $file2 | cut -c10-                >> input
  done

# ---------------------------------------------------------------------------- #
# 4. Compute error
    
# back=`pwd`
# cd $genes_main/progs
# rm -f err_par.o
# pgf90 err_par.f90 -byteswapio -o err_par.x -Mlist *.o
# rm -f err_par.o
# mv err_par.x $genes_main/exe/.
# cd $back      

  set +e
  cat input | $genes_main/exe/err_par.x  > err_par.out
  OK=$?
  set -e

  rm -f input

  if [ "$OK" != '0' ]
  then
    cat err_par.out
    exit 6
  fi

# ---------------------------------------------------------------------------- #
# 5 End of script

  tail -1 err_par.out
  rm -f err_par.out

# End of get_err_par.sh ------------------------------------------------------ #
