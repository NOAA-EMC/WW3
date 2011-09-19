#!/bin/sh
# ---------------------------------------------------------------------------- #
# pop_extract.sh :  process pop_clean file to get counts for nq, npop, ngen    #
#                   to be read by matlab scripts.                              #
#                                                                              #
# usage: pop_extract.sh dir_1 dir_2                                            #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 04-Nov-2009 : Origination.                                                   #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization

  set -e

  echo ' '
  echo 'pop_extract.sh :'
  echo '----------------'

  if [ "$#" -lt "2" ] ; then
    echo "   No directories defined (abort)" ; exit 1 ; fi

  dir_1=$1
  dir_2=$2

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

  if [ ! -d $genes_data/$dir_1/$dir_2 ] ; then
    echo "   Directory $genes_data/$dir_1/$dir_2 not found (abort)" 
    exit 2 ; fi

  data_dir=$genes_data/$dir_1/$dir_2
  echo "   Data directory has been found :"
  echo "      $data_dir"

  cd $data_dir

  if [ -f genes.expdef.env ]
  then
    . genes.expdef.env
  else
    echo "   File genes.expdef.env.not found (abort)" ; exit 3
  fi

  echo ' '

# ---------------------------------------------------------------------------- #
# 3.  Make count file

  echo "$genes_nq $genes_npop $genes_ngen" > pop.count

# ---------------------------------------------------------------------------- #
# 4. End of script

  echo ' '
  echo 'End of pop_extract.sh' 

# End of pop_extract.sh ------------------------------------------------------ #
