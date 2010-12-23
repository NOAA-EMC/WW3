#!/bin/sh
# ---------------------------------------------------------------------------- #
# make_init.sh  : Generate the initial population.                             #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 20-Dec-2008 : Origination.                                                   #
#                                                                              #
#    Copyright 2008-2010 National Weather Service (NWS),                       #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization

  echo 'make_init.sh :'
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

  cd gen0001

# ---------------------------------------------------------------------------- #
# 2. Set initial population

  echo ' '
  echo 'Make initial population ...'

  echo "  $genes_nq $genes_npop0 $genes_seed"            > input
  sed -n '/^\$.*/!p'  ../genes.mask.env                 >> input
  sed -n '/^\$.*/!p'  ../genes.stats.env                >> input

# echo '---------------------------------------------------------------'
# cat input
# echo '---------------------------------------------------------------'
# back=`pwd`
# cd $genes_main/progs
# rm -f qtoolsmd.o
# pgf90 -c qtoolsmd.f90 -Mlist qstoolmd.o
# rm -f initgen.o
# pgf90 initgen.f90 -byteswapio -o initgen.x -Mlist *.o
# rm -f initgen.o
# mv initgen.x $genes_main/exe/.
# cd $back

  $genes_main/exe/initgen.x

  rm -f input
# mv population.ieee population
# mv population.text population
# rm -f population.????

# ---------------------------------------------------------------------------- #
# 3. Set seed for this generation

  echo ' '
  echo "   Make new seed file ..."

  new_seed=`echo $genes_seed | $genes_main/exe/reseed.x | awk '{ print $1}'`

  echo "#!/bin/sh"                            > seed.env
  echo ' '                                   >> seed.env
  echo " export genes_seed=$new_seed"        >> seed.env

# ---------------------------------------------------------------------------- #
# 4. End of script

  echo ' '
  echo 'End of make_init.sh'

# End of make_init.sh -------------------------------------------------------- #
