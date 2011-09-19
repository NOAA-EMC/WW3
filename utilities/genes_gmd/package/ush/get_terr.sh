#!/bin/sh
# ---------------------------------------------------------------------------- #
# get_terr.sh   : Get representative test error from population.               #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# Remarks: Error testing switched off by setting min_err to 999.999            #
#                                                                              #
# 31-Aug-2009 : Origination.                                                   #
#                                                                              #
#    Copyright 2009-2010 National Weather Service (NWS),                       #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization

  set -e
  setup='.genes.env'

  min_err='45.000'
# min_err='999.999'
  max_err='125.000'
  err_fac='2.5'

# ---------------------------------------------------------------------------- #
# 2. Test setup file

  if [ -f ~/$setup ]
  then
    . ~/$setup
  else
    echo "   Setup file $setup NOT found (abort)."
    exit 1
  fi

# ---------------------------------------------------------------------------- #
# 3. Check for population

  if [ ! -f population ]
  then
    echo "   population file NOT found (abort)."
    exit 2
  fi

# ---------------------------------------------------------------------------- #
# 4. Get test error

  min_err_pop=`head -1 population | awk '{ print $1 }'`

# back=`pwd`
# cd $genes_main/progs
# rm -f testerr.o
# pgf90 testerr.f90 -byteswapio -o testerr.x -Mlist
# rm -f testerr.o
# mv testerr.x $genes_main/exe/.
# cd $back

  echo "$min_err_pop $min_err $max_err $err_fac" |  \
      $genes_main/exe/testerr.x > testerr.out

  terr=`grep 'test error' testerr.out | awk '{ print $4}'`

  if [ -n "$terr" ]
  then
    echo "#!/bin/sh"                   > genes.terr.env
    echo "export TERR=$terr"          >> genes.terr.env
  fi

  rm -f testerr.out

# ---------------------------------------------------------------------------- #
# 5. End of script

# End of get_terr.sh --------------------------------------------------------- #
