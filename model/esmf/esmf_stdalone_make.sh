#!/bin/bash

# Temporary script to compile esmf codes for testing modifications

 NEMSdir=${NEMSdir:-$1} # Requires to enter location of NEMS dir
 WW3dir=${WW3dir:-$1} # Requires to enter location of WW3 model codes

 if [ -z $NEMSdir ] || [ -z $WW3dir ]
 then
  echo " Variable NEMSdir not set "
  echo " Usage: ./esmf_stdalone_make.sh NEMSdir WW3dir"  
  exit
 fi

 hostl=`hostname | cut -c1`
 if [ "$hostl" = "v" ] || [ "$hostl" = "m" ]
 then
   machn="wcoss_dell_p3"
 fi

 module use ${NEMSdir}/modulefiles/${machn}
 module load fv3ww3              ; module list   
 ulimit -S -s $stack ; set -x ; #cd `pwd`
 export COMP_SRCDIR="${WW3dir}/model" COMP_BINDIR="${WW3dir}/model/bin" WW3_COMP="intel"
 set -e
 exec make -j 1 WW3_COMP="intel" ww3_nems 1> make.out 2>&1

