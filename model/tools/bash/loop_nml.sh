#!/bin/bash -e

#############################################################################
# how to run it over regtests : 
#
# for dir in $(find $PWD/../../../regtests/ -mindepth 1 -maxdepth 1 -type d -name "*t*"); do echo $dir; qsub -l walltime=04:00:00 -l mem=1G -v rpath="$dir",prog="ww3_grid",comment="header" -N grid loop_nml.sh; done
#############################################################################

#shell argument
if [ $# -eq 3 ]
then
  rpath="$1"
  prog="$2"
  comment="$3"
fi

if [ -z "$(echo $rpath)" ] || [ -z "$(echo $prog)" ] || [ -z "$(echo $comment)" ]
then
  echo "[ERROR] need 3 arguments :"
  echo '$1 : root path where to find all inp files [~/WW3/regtests]'
  echo '$2 : prog to which convert inp files [ww3_shel]'
  echo '$3 : include header or full comments [header|full]'
  echo 'or qsub -v rpath=XXX,prog=XXX,comment=XXX loop_nml.sh'
  exit 1
fi

# bash environment
if [ ! -z $(echo $BASH_SOURCE[0]) ] ; then
  cd "$( dirname "${BASH_SOURCE[0]}" )"
fi

# pbs environment
if [ ! -z $(echo $PBS_O_WORKDIR) ] ; then
  cd $PBS_O_WORKDIR
fi

path_bash="$( cd $PWD && pwd )"


for file in $(find $rpath -name "${prog}*.inp*" ! -name "${prog}_clean.inp" )
do
  echo 'file : '$file
  $path_bash/${prog}_inp2nml.sh $file $comment
done

echo ''
echo '****** end of loop ******'
