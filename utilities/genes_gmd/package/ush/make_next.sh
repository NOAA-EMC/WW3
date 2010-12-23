#!/bin/sh
# ---------------------------------------------------------------------------- #
# make_next.sh  : Generate the next population.                                #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 02-Jan-2009 : Origination.                                                   #
# 21-Jan-2009 : Upgrade child generation.                                      #
# 01-Nov-2009 : Generate error files for completed members.                    #
#                                                                              #
#    Copyright 2009-2010 National Weather Service (NWS),                       #
#       National Oceanic and Atmospheric Administration.  All rights           #
#       reserved.  Distributed as part of WAVEWATCH III. WAVEWATCH III is a    #
#       trademark of the NWS. No unauthorized use without permission.          #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization

  echo 'make_next.sh :'
  echo '--------------'

  old=$1
  new=$2

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

  echo "   Previous generation : $old"
  if [ ! -d $old ]
  then
    echo "      Directory $old not found"
    echo '      *** Abort ***'
    exit 2
  fi

  echo "   Next generation     : $new"
  if [ ! -d $new ]
  then
    echo "      Generating directory $new"
    mkdir $new
  fi

  cd $new
  rm -f input previous

  if [ -f ../$old/population ]
  then
    echo "      Linking to old population ..."
    ln -sf ../$old/population previous
  else
    echo "      Old population not found"
    echo '      *** Abort ***'
    exit 3
  fi

# ---------------------------------------------------------------------------- #
# 2. Set next population

  echo ' '
  echo 'Make next population ...'

  echo "  $genes_nq $genes_npop $genes_seed"             > input
  sed -n '/^\$.*/!p'  ../genes.mask.env                 >> input
  sed -n '/^\$.*/!p'  ../genes.stats.env                >> input

  echo '---------------------------------------------------------------'
  cat input
  echo '---------------------------------------------------------------'
# back=`pwd`
# cd $genes_main/progs
# rm -f qtoolsmd.o qtoolsmd.mod
# pgf90 -c qtoolsmd.f90 -byteswapio
# rm -f nextgen.o
# pgf90 nextgen.f90 -byteswapio -o nextgen.x -Mlist *.o
# rm -f nextgen.o
# mv nextgen.x $genes_main/exe/.
# cd $back

  $genes_main/exe/nextgen.x

  size_new=`wc -l next      | awk '{print $1}'`
  size_new=`expr $size_new / $genes_nq`

  if [ "$size_new" != "$genes_npop" ]
  then
    echo "   Unexpected population size $size_new [$genes_npop]"
    echo "   *** abort ***" ; exit 4
  else
    rm -f input previous
    mv next population
  fi

# ---------------------------------------------------------------------------- #
# 3. Set seed for this generation

  echo ' '
  echo "   Make new seed file ..."

  new_seed=`echo $genes_seed | $genes_main/exe/reseed.x | awk '{ print $1}'`

  echo "#!/bin/sh"                            > seed.env
  echo ' '                                   >> seed.env
  echo " export genes_seed=$new_seed"        >> seed.env

# ---------------------------------------------------------------------------- #
# 4. Set up error files of completed members

  echo ' '
  echo "   Make error files ..."

  set +e
  nold=`ls old.???? 2> /dev/null | wc -l | awk '{ print $1}'`
  set -e

  echo "      $nold files to be generated ..."
  i=1

  if [ "$nold" -gt '0' ] 
  then
    for file in `ls old.????`
    do
      err_file=`echo $file | sed 's/old/err/g'`
      count=`echo $file | sed 's/\./ /g' | awk '{print $2}'`
      count=`head -1 $file | awk '{print $1}'`
      echo "      Errors for file $err_file found [$count]."
      sed -n "$count,${count}p" ../$old/errors.test   > $err_file
      sed -n "$count,${count}p" ../$old/errors.pars  >> $err_file
      rm -f $file
    done
  fi

# ---------------------------------------------------------------------------- #
# 4. End of script

  echo ' '
  echo 'End of make_next.sh'

# End of make_next.sh -------------------------------------------------------- #
