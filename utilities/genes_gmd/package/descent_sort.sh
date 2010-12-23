#!/bin/sh
# ---------------------------------------------------------------------------- #
# descent_sort.sh : Sort the results of a steepest descent method.             #
#                                                                              #
# usage: descent_sort.sh igen ipop/ID                                          #
#                                                                              #
# remarks:                                                                     #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 12-Sep-2010 : Origination.                                                   #
#                                                                              #
#    Copyright 2010 National Weather Service (NWS),                            #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization

  set -e

  echo ' '
  echo 'descent_sort.sh : '`date`
  echo '-----------------------------------------------------'
  echo '   sort steepest descent results'
  echo ' '

  if [ "$#" -lt "2" ] ; then
    echo "   usage: descent_sort.sh igen ipop/ID" ; exit 1 ; fi

  setup='.genes.env'

# ---------------------------------------------------------------------------- #
# 2. Get base setup for the runs

  if [ -f ~/$setup ]
  then
    echo "   Setup file $setup found."
    . ~/$setup
  else
    echo "   Setup file $setup NOT found (abort)."
    exit 2
  fi
  all_cases=`$genes_main/ush/get_cases.sh`
  igen=$1
  ipop=$2

  set +e
  test=`expr $ipop + 1 2> /dev/null`
  OK=$?
  set -e

  if [ "$OK" = '0' ]
  then
    pop_is_nr='yes'
  else
    pop_is_nr='no'
  fi

  echo "   Save directory  : $genes_data/$genes_exp1/$genes_exp2"
  echo "   Cases to run    : $all_cases"
  echo "   Generation      : $1"
  if [ "$pop_is_nr" = 'yes' ]
  then
    echo "   Member          : $2"
  else
    echo "   Case            : $2"
  fi
  echo ' '

# ---------------------------------------------------------------------------- #
# 3.  Checking directories and files.
# 3.a General data directory

  if [ ! -d "$genes_data" ]
  then
    echo "   Main data directory $genes_data not found (abort) *** "
    exit 3 
  fi

  cd $genes_data

# 3.b First level experiment directory

  if [ ! -d "$genes_exp1" ]
  then
    echo "   Experiment directory $genes_exp1 not found (abort)"
    exit 4 
  fi

  cd $genes_exp1

# 3.c Second level experiment directory

  if [ ! -d "$genes_exp2" ]
  then
    echo "   Experiment directory $genes_exp2 not found (abort)"
    exit 5
  fi

  cd $genes_exp2

# 3.d Second level experiment directory

  if [ ! -f "genes.expdef.env" ]
  then
    echo "   File genes.expdef.env not found (abort)"
    exit 6
  fi

  . genes.expdef.env

# 3.e Generational directory

  if [ "$igen" -lt '10' ]
  then
    igen="000$igen"
  else
    if [ "$igen" -lt '100' ]
    then
      igen="00$igen"
    else
      if [ "$igen" -lt '1000' ]
      then
        igen="0$igen"
      fi
    fi
  fi
  gen=gen$igen

  if [ ! -d "$gen" ]
  then
    echo "   Generation directory $gen not found (abort)"
    exit 7
  fi

  cd $gen

# 3.f Population check

  if [ "$pop_is_nr" = 'yes' ] && [ ! -f "population" ]
  then
    echo "   Population file not found (abort)"
    exit 8
  fi

# 3.g Member check

  if [ "$pop_is_nr" = 'yes' ]
  then

    mpop=`wc -l population | awk '{ print $1}'`
    mpop=`expr $mpop / $genes_nq`

    if [ "$ipop" -lt '1' ] || [ "$ipop" -gt "$mpop" ]
    then
      echo "   Population member $ipop [$mpop] not found (abort)"
      exit 9
    fi

    if [ "$ipop" -lt '10' ]
    then
      ipop="000$ipop"
    else
      if [ "$ipop" -lt '100' ]
      then
        ipop="00$ipop"
      else
        if [ "$ipop" -lt '1000' ]
        then
          ipop="0$ipop"
        fi
      fi
    fi

  fi

  if [ ! -f descent.$ipop ]
  then
    echo "   File descent.$ipop not found (abort)"
    exit 9
  fi

# ---------------------------------------------------------------------------- #
# 4.  Sort the descent results

  mkdir -p work
  cd work

  cp ../descent.$ipop population
  mdesc=`wc -l population | awk '{ print $1}'`
  mdesc=`expr $mdesc / $genes_nq`

  echo "$genes_nq $mdesc" | $genes_main/exe/sortgen.x > sortgen.out

  head -$genes_nq pop_clean > ../descent_sort.$ipop

  cd ..
  rm -rf work

  echo "   best sorted descent result :"
  echo '-----------------------------------------------------------------'
  cat descent_sort.$ipop
  echo '-----------------------------------------------------------------'

# ---------------------------------------------------------------------------- #
# 5. End of script

  echo ' '
  echo 'Out of descent_sort.sh '`date`

# End of descent_sort.sh ----------------------------------------------------- #
