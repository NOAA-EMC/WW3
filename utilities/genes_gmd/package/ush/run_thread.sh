#!/bin/sh
# ---------------------------------------------------------------------------- #
# run_thread.sh : Running and managing a single thread of the engine.          #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 22-Dec-2008 : Origination.                                                   #
#                                                                              #
#    Copyright 2008-2010 National Weather Service (NWS),                       #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization
# 1.a Check command line

  sleep_1=5    # Sleep waiting for inital data file setup
  sleep_2=5    # Sleep waiting for next assignment

  set -e

  if [ "$#" != '1' ]
  then
    echo "usage: run_thread.sh caseNR" ; exit 1
  fi

  run_ID=$1

  echo "run_thread.sh $run_ID :"
  echo "------------------"

  . ~/.genes.env

# 1.b Make work directory and move there

  work=thread_$run_ID
  if [ -d $work ] 
  then
    echo "   CLeaning up old work directory ..."
    rm -rf $work
  fi

  echo "   Making work directory $work ..."
  mkdir $work
  cd $work

# 1.c Check data file and wait if necessary

  data=../tdata.$run_ID
  echo "   Data file is $data"

  if [ ! -f $data ]
  then
    echo "      Waiting for data file to be generated"
    until [ -f $data ]
    do
      sleep $sleep_1
    done
  fi

  key=`head -1 $data | awk '{print $1}'`
  if [ "$key" != 'starting' ]
  then
    echo "      Waiting for data file to be ready"
    until [ "$key" = 'starting' ]
    do
      key=`head -1 $data | awk '{print $1}'`
      sleep $sleep_1
    done
  fi

# 1.d Signal 'ready to go'

  echo 'ready to go' > $data
  echo '   Ready to go'

# ---------------------------------------------------------------------------- #
# 2. Processing based on key info

  key=`head -1 $data | awk '{print $1}'`

  echo ' '
  echo '   Starting processing loop ...'

# 2.a Loop for getting keys

  until [ "$key" = 'done' ]
  do
    key=`head -1 $data | awk '{print $1}'`
    keytop=`echo $key | cut -c1-3`

# 2.b Snl assigned, go compute the errors.

    if [ "$keytop" = 'snl' ]
    then
      echo "      Processing $key ..."
      echo "         Output to ../$key.out"

      rm -f genes.snl.env
      mv ../$key genes.snl.env
      $genes_main/ush/run_one.sh $key > ../$key.out

      echo 'ready to go' > $data
    fi

# 2.c Wait always needed to sync with control.sh

    if [ "$key" != 'done' ]
    then
#     echo "      Waiting ..."
      sleep $sleep_2
    fi

  done

# ---------------------------------------------------------------------------- #
# 3. End of script

  cd ..
  rm -rf $work

  echo ' '
  echo "End of run_thread.sh $run_ID"

# End of run_thread.sh ------------------------------------------------------- #
