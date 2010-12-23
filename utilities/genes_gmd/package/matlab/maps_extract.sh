#!/bin/sh
# ---------------------------------------------------------------------------- #
# maps_extract.sh : process raw mapping data files for easy reading by         #
#                   matlab scripts.                                            #
#                                                                              #
# usage: maps_extract.sh dir_1 dir_2                                           #
#                                                                              #
#                                      Author      : Hendrik L. Tolman         #
#                                                                              #
# 06-Oct-2009 : Origination.                                                   #
# 07-Oct-2009 : Repair Cd and Cs extraction.                                   #
# 23-Oct-2009 : Repair n extraction.                                           #
# 03-Nov-2009 : Adding NQ > 1 options.                                         #
#                                                                              #
# ---------------------------------------------------------------------------- #
# 1. Initialization

  set -e

  echo ' '
  echo 'maps_extract.sh :'
  echo '-----------------'

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

  if [ ! -f mapping ] ; then
    echo "   File mapping not found (abort)" ; exit 3 ; fi

  if [ ! -f mapping.pars ] ; then
    echo "   File mapping.pars not found (abort)" ; exit 4 ; fi

  if [ ! -f mapping.test ] ; then
    echo "   File mapping.test not found (abort)" ; exit 5 ; fi

  if [ -f genes.expdef.env ]
  then
    . genes.expdef.env
  else
    echo "   File genes.expdef.env.not found (abort)" ; exit 6
  fi

  echo ' '

  if  [ "$genes_nq" = '1' ]
  then
    mapping=mapping.reduced
    cat mapping | cut -c 10-63 > mapping.reduced
  else
    mapping=mapping.reduced
    cat mapping | awk ' NF == 7 { printf " %7s%7s%7s%11s%11s%6s%6s\n" , \
                   $1 , $2 , $3 , $4 , $5 , $6 , $7 }' > mapping.reduced
  fi

# ---------------------------------------------------------------------------- #
# 3.  Extract data
# 3.a lambda

  cat $mapping | awk '{ print $1}' | sort -u > maps.lambda
  nr_lambda=`wc -l maps.lambda | awk '{ print $1}'`
  echo "   Number of lambda found : $nr_lambda"

# 3.b mu

  cat $mapping | awk '{ print $2}' | sort -u > maps.mu
  nr_mu=`wc -l maps.mu | awk '{ print $1}'`
  echo "   Number of mu found     : $nr_mu"

# 3.c Dtheta

  cat $mapping | awk '{ print $3}' | sort -u > maps.Dtheta
  nr_Dtheta=`wc -l maps.Dtheta | awk '{ print $1}'`
  echo "   Number of Dtheta found : $nr_Dtheta"

# 3.d Cd

  cat $mapping | awk '{ print $4}' > temp.Cd
  head -1 $mapping  | awk '{ print $4}'  > maps.Cd

  for value in `cat temp.Cd`
  do
    set +e
    test=`grep $value maps.Cd`
    set -e
    if [ -z "$test" ]
    then
      echo "$value" >> maps.Cd
    fi
  done

  rm -f temp.Cd
  nr_Cd=`wc -l maps.Cd | awk '{ print $1}'`
  echo "   Number of Cd found     : $nr_Cd"

# 3.e Cs

  cat $mapping | awk '{ print $5}' > temp.Cs
  head -1 $mapping  | awk '{ print $5}'  > maps.Cs

  for value in `cat temp.Cs`
  do
    set +e
    test=`grep $value maps.Cs`
    set -e
    if [ -z "$test" ]
    then
      echo "$value" >> maps.Cs
    fi
  done

  rm -f temp.Cs
  nr_Cs=`wc -l maps.Cs | awk '{ print $1}'`
  echo "   Number of Cs found     : $nr_Cs"

# 3.f m

  cat $mapping | awk '{ print $6}' | sort -u > maps.m
  nr_m=`wc -l maps.m | awk '{ print $1}'`
  echo "   Number of m found      : $nr_m"

# 3.g n

  cat $mapping | awk '{ print $7}' > temp.n
  head -1 $mapping  | awk '{ print $7}'  > maps.n

  for value in `cat temp.n`
  do
    last=`tail -1 maps.n | awk '{ print $1}'`
    if [ "$last" != "$value" ]
    then
      echo "$value" >> maps.n
    fi
  done

  rm -f temp.n
  nr_n=`wc -l maps.n | awk '{ print $1}'`
  echo "   Number of n found      : $nr_n"

  rm -f mapping.reduced

# ---------------------------------------------------------------------------- #
# 4.  File with counts

  echo "$nr_lambda $nr_mu $nr_Dtheta $nr_Cd $nr_Cs $nr_m $nr_n $genes_nq" > maps.count

# ---------------------------------------------------------------------------- #
# 5. End of script

  echo ' '
  echo 'End of maps_extract.sh' 

# End of maps_extract.sh ----------------------------------------------------- #
