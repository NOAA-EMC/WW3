#!/bin/bash
# --------------------------------------------------------------------------- #
# matrix_divider simply divides the main matrix into subsets with a given     #
# number of test (i.e. maxlist=100)                                           #
#                                                                             #
# Remarks:                                                                    #
# - Once the matrix is generated, this script can be execute. The user should #
#   define the maxlist and this script divide the matrix into matrix? and     #
#   submit the job using sbatch.                                              #
#                                                                             #
#                                                      Ali Abdolali           #
#                                                      August 2018            #
#                                                      March 2021             #
#                                                                             #
#    Copyright 2013 National Weather Service (NWS),                           #
#       National Oceanic and Atmospheric Administration.  All rights          #
#       reserved.  WAVEWATCH III is a trademark of the NWS.                   #
#       No unauthorized use without permission.                               #
#                                                                             #
# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #
# 1.  clean up and definitions                                                #
# --------------------------------------------------------------------------- #
rm -r ../model?
rm -r ../model??
#Put the job requirement/spec in "before"
sed -e "/run_test/,\$d" matrix > before
#Put the list of tests in "list"
command egrep 'run_test|ww3_tp2.14' matrix | cat >> list
#Count the number of lines in "list"
listn=$(wc -l < list)
count=0
countT=0
# Define the number of test in each subset "maxlist"
maxlist=100
matrixno=1
# --------------------------------------------------------------------------- #
# 2.  Divide and dump in subsets                                              #
# --------------------------------------------------------------------------- #
while read line
do
if [[ count -eq 0 ]]
then 
cat before >> matrix$matrixno
fi
  if [[ count -le maxlist ]]
     then
     echo $line >> matrix$matrixno
     (( count = count + 1 ))
     (( countT = countT + 1 ))
#     echo $count
#     echo $countT
 else
 count=0
 (( matrixno = matrixno + 1 ))
  fi
done < list

  for i in `seq 1 1 "$((matrixno))"`; do
#Replace matrix.out > matrix?.out, model > model?
  cp -r ../model ../model$i
  sed -i 's/'matrix.out'/'matrix${i}.out'/gI' matrix$i
  sed -i 's/'model'/'model${i}'/gI' matrix$i
  echo "  echo ' '"                                                                     >> matrix$i
  echo "  echo '     **************************************************************'"   >> matrix$i
  echo "  echo '     *  end of WAVEWATCH III matrix$i of regression tests         *'"   >> matrix$i
  echo "  echo '     **************************************************************'"   >> matrix$i
  echo "  echo ' '"                                                                     >> matrix$i
done

rm before
rm list

  echo "file matrix is divided into $i subsets ...."

# --------------------------------------------------------------------------- #
# End to matrix_divider                                                       #
# --------------------------------------------------------------------------- #
