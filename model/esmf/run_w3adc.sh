#!/bin/bash
set -eux

if [ $# -ne 2 ]; then
  echo "Error in run_w3adc.sh"
  exit 1
fi

ftn_dir=$1
filename=$2

switches=$(cat ${ftn_dir}/../esmf/switch | tr '\n' ' ')

extension="${filename##*.}"
basename="${filename%.*}"
inputname="${filename//\//_}.input"

mkdir -p $( dirname ${filename} )

if [[ $extension == "ftn" ]]; then
   echo "0 0" > ${inputname}
   echo "'${ftn_dir}/${filename}' '${basename}.F90'" >> ${inputname}
   echo "'${switches}'" >> ${inputname}
   ./w3adc < ${inputname}
else
   cp ${ftn_dir}/${filename} ${filename}
fi

echo "Done running w3adc for ${filename}"
