#!/bin/bash
# --------------------------------------------------------------------------- #
# matrix_divider simply divides the main matrix into subsets with a given     #
# number of test (i.e. maxlist1 for mpi tests and maxlist2 for serial tests)  #
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

cp matrix matrix.tmp

maxlist1=50
maxlist2=100

#Put the job requirement/spec in "before"
sed -e "/run_test/,\$d" matrix.tmp > before
#Put the list of tests in "list"
command egrep 'ww3_tp2.14' matrix.tmp | cat >> list_2.14
awk '!/ww3_tp2.14/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
command egrep 'mpirun|mpiexec|MPI_LAUNCH' matrix.tmp | cat >> list_mpi
awk '!/mpirun|mpiexec|MPI_LAUNCH/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
split -dl $maxlist1 list_mpi list_mpi_
rm list_mpi
matrixno1=$(ls list_mpi* | wc -l)
echo "Total nummber of matrix with parallel tests = $(($matrixno1 + 1)); each includes $maxlist1 tests"
command egrep 'run_test' matrix.tmp | cat >> list_serial
split -dl $maxlist2 list_serial list_serial_
rm list_serial
matrixno2=$(ls list_serial* | wc -l)
echo "Total nummber of matrix with serial test = $matrixno2; each includes $maxlist2 tests"
rm matrix.tmp

# --------------------------------------------------------------------------- #
# 2.  Divide and dump in subsets                                              #
# --------------------------------------------------------------------------- #
# parallel jobs
count=0
  for i in `seq -f '%02g' 0 "$((matrixno1 - 1))"`; do
#echo $i
#Replace matrix.out > matrix?.out, model > model?
  (( count = count + 1 ))
  cat before >> matrix$count
  cat list_mpi_$i >> matrix$count
  cp -r ../model ../model$count
  sed -i 's/'matrix.out'/'matrix${count}.out'/gI' matrix$count
  sed -i 's/'model'/'model${count}'/gI' matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo '     *  end of WAVEWATCH III matrix$count of regression tests     *'"   >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "rm ../model$count"                                                              >> matrix$count
  echo " matrix$count prepared"
 done

#serial jobs
 for i in `seq -f '%02g' 0 "$((matrixno2 - 1))"`; do
#echo $i
#Replace matrix.out > matrix?.out, model > model?
  (( count = count + 1 ))
  cat before >> matrix$count
  cat list_serial_$i >> matrix$count
  cp -r ../model ../model$count
  sed -i 's/'matrix.out'/'matrix${count}.out'/gI' matrix$count
  sed -i 's/'model'/'model${count}'/gI' matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo '     *  end of WAVEWATCH III matrix$count of regression tests     *'"   >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "rm ../model$count"                                                              >> matrix$count
  echo " matrix$count prepared"
 done

#ww3_tp2.14 is separated, as it has dependency. 
  (( count = count + 1 ))
  cat before >> matrix$count
  cat list_2.14 >> matrix$count
  cp -r ../model ../model$count
  sed -i 's/'matrix.out'/'matrix${count}.out'/gI' matrix$count
  sed -i 's/'model'/'model${count}'/gI' matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo '     *  end of WAVEWATCH III matrix$count of regression tests     *'"   >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "rm ../model$count"                                                              >> matrix$count
  echo " matrix$count prepared"



rm before
rm list*

  echo "file matrix is divided into $count subsets ...."

# --------------------------------------------------------------------------- #
# End to matrix_divider                                                       #
# --------------------------------------------------------------------------- #

