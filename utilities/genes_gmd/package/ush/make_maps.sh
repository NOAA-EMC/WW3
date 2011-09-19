#!/bin/sh
# ---------------------------------------------------------------------------- #
# make_maps.sh  : Generate population for error mapping.                       #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 14-Jan-2009 : Origination.                                                   #
#                                                                              #
#    Copyright 2009-2010 National Weather Service (NWS),                       #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization

  echo 'make_maps.sh :'
  echo '--------------'
  set -e
  setup='.genes.env'
  expdef='genes.expdef.env'

# ---------------------------------------------------------------------------- #
# 2. Test setup files

  if [ -f ~/$setup ]
  then
    . ~/$setup
  else
    echo "   Setup file $setup NOT found (abort)."
    exit 1
  fi

  cd $genes_data/$genes_exp1/$genes_exp2

  if [ -f $expdef ]
  then
    . $expdef
  else
    echo "   Setup file $expdef NOT found (abort)."
    exit 2
  fi

# ---------------------------------------------------------------------------- #
# 2. Set initial population

  echo ' '
  echo 'Make mapping population ...'

  echo "  $genes_nq"                                     > input
  sed -n '/^\$.*/!p'  ./genes.mask.env                  >> input
  sed -n '/^\$.*/!p'  ./genes.maps.env                  >> input

# echo '---------------------------------------------------------------'
# cat input
# echo '---------------------------------------------------------------'
# back=`pwd`
# cd $genes_main/progs
# rm -f qtoolsmd.o
# pgf90 -c qtoolsmd.f90 -Mlist qstoolmd.o
# rm -f mapsgen.o
# pgf90 mapsgen.f90 -byteswapio -o mapsgen.x -Mlist *.o
# rm -f mapsgen.o
# mv mapsgen.x $genes_main/exe/.
# cd $back

  $genes_main/exe/mapsgen.x

  rm -f input

# ---------------------------------------------------------------------------- #
# 3. End of script

  echo ' '
  echo 'End of make_maps.sh'

# End of make_maps.sh -------------------------------------------------------- #
