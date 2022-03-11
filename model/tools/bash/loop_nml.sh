#!/bin/bash -e

#input argument
if [ $# -ne 3 ]
then
  echo "need 3 arguments :"
  echo '$1 : root path where to find all inp files [~/WW3/regtests]'
  echo '$2 : prog to which convert inp files [ww3_shel]'
  echo '$3 : include header or full comments [header|full]'
  exit 1
fi

rpath="$1"
prog="$2"
comment="$3"

path_bash="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

for file in $(find $rpath -name "${prog}*.inp*" ! -name "${prog}_clean.inp" )
do
  echo 'file : '$file
  $path_bash/${prog}_inp2nml.sh $file $comment
done

echo ''
echo '****** end of loop ******'
