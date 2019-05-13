#!/bin/bash

# Temporary script to compile esmf codes for testing modifications

 NEMSdir=$1 # Requires FV3-WW3 coupled app installed in machine

 if [ -z $NEMSdir ]
 then
  echo " Variable NEMSdir not set "
  echo " Usage: ./esmf_stdalone_make.sh NEMSdir"  
  exit
 fi
 module use ${NEMSdir}/NEMS/src/conf 
 module load modules.nems              ; module list   
 ulimit -S -s $stack ; set -x ; #cd `pwd`
 export COMP_SRCDIR="/scratch4/NCEPDEV/ocean/noscrub/Henrique.Alves/EMC_FV3-WW3/WW3/model" COMP_BINDIR="/scratch4/NCEPDEV/ocean/noscrub/Henrique.Alves/EMC_FV3-WW3/WW3/WW3_INSTALL" WW3_COMP="theia"
 set -e
 exec make -j 1 WW3_COMP="theia" ww3_nems 1> make.out 2>&1

