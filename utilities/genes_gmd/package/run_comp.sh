#!/bin/sh
# ---------------------------------------------------------------------------- #
# run_comp.sh  : Process output files of one or two model runs. In case of     #
#                two model runs produce model errors.                          #
#                                                                              #
# usage: run_comp.sh testID runID [compID]                                     #
#                                                                              #
# remarks: Runs in local directory.                                            #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 13-Nov-2008 : Origination.                                                   #
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
  echo 'run_comp.sh :'
  echo '-------------'

  if [ "$#" -lt "2" ] ; then
    echo "   run_comp.sh test_ID run_ID [comp_ID]" ; exit 1 ; fi

  testID=$1
  echo "   Test case  : $testID"
   runID=$2
  echo "   First run  : $runID"

  if [ "$#" -ge "3" ]
  then 
    compID=$3
    echo "   Second run : $compID"
    nf=2
  else
    compID=
    echo "   Second run : N/A"
    nf=1
  fi

  setup='.genes.env'

# ---------------------------------------------------------------------------- #
# 2. Test setup file

  echo ' '

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
# 2. Get raw data files

  echo ' '
  echo '   Getting data files'
  echo '      Cleaning up old files ....'

  rm -f spec1.data spec2.data srce1.data srce2.data part1.data part2.data

  if [ "$runID" = 'local' ]
  then
    dir='./'
    echo "      First set of files from local directory ..."
  else
    dir="$genes_data/$runID/$testID"
    echo "      First set of files from $dir ..."
  fi

  if [ ! -d $dir ] ; then
    echo "      Directory not found (abort)" ; exit 3 ; fi

  for file in spec source part
  do
    if [ -f $dir/$file.ww3 ]
    then
      echo "         Copy $file.ww3 as ${file}1.data ..."
      cp $dir/$file.ww3 ${file}1.data
    else
      echo "         File $file.ww3 not found (abort)" ; exit 4
    fi
  done

  if [ -n "$compID" ]
  then

    if [ "$compID" = 'local' ]
    then
      dir='./'
      echo "      Second set of files from local directory ..."
    else
      dir="$genes_data/$compID/$testID"
      echo "      Second set of files from $dir ..."
    fi

    if [ ! -d $dir ] ; then
      echo "      Directory not found (abort)" ; exit 3 ; fi

    for file in spec source part
    do
      if [ -f $dir/$file.ww3 ]
      then
        echo "         Copy $file.ww3 as ${file}2.data ..."
        cp $dir/$file.ww3 ${file}2.data
      else
        echo "         File $file.ww3 not found (abort)" ; exit 4
      fi
    done

  fi

# ---------------------------------------------------------------------------- #
# 3. Run processing program

  echo ' '
  echo '   Process data files ....'

  rm -f all_data.ww3* errors.ww3

# back=`pwd`
# cd $genes_main/progs
# rm -f process.o
# pgf90 process.f90 -byteswapio -o process.x -Mlist *.o
# rm -f process.o
# mv process.x $genes_main/exe/.
# cd $back

  $genes_main/exe/process.x > process.out

  rm -f spec?.data source?.data part?.data
  rm -f process.out

# ---------------------------------------------------------------------------- #
# 4. Process output files

  if [ -f all_data.ww3 ]
  then
#   echo '      Compressing all_data.ww3'
#   gzip all_data.ww3
    if [ "$dir" != './' ]
    then
      echo "      Store all_data.ww3.gz in $dir"
      cp all_data.ww3* $dir/.
    fi
    if [ -f all_data.ww3.gz ] ; then
      gunzip all_data.ww3.gz ; fi
  fi

  if [ -f errors.ww3 ]
  then
    fname="errors.$testID.$runID.$compID"
#   fname="errors.$testID"
    echo "      Saving errors.ww3 as $fname"
    mv errors.ww3 $fname
  fi

# ---------------------------------------------------------------------------- #
# 5. End of script

  echo ' '
  echo 'End of run_comp.sh' 

# End of run_comp.sh --------------------------------------------------------- #
