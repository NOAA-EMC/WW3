#!/bin/bash
# --------------------------------------------------------------------------- #
# matrix_divider simply divides the main matrix into subsets with a given     #
# number of test (i.e. maxlist1 for mpi tests, maxlist3 for OMP/OMPH tests    #
# and maxlist3 for serial tests)                                              #
#                                                                             #
# Remarks:                                                                    #
# - Once the matrix is generated, this script can be execute. The user should #
#   define the maxlist and this script divide the matrix into matrix? and     #
#   submit the job using sbatch.                                              #
#                                                                             #
#                                                      Ali Abdolali           #
#                                                      August 2018            #
#                                              Updated:  May 2021             #
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
cp matrix matrix.tmp
HOME=${PWD%/*}

maxlist1=48
maxlist2=52
maxlist3=104
#Put the job requirement/spec in "before"
sed -e "/run_test/,\$d" matrix.tmp > before
#Put the list of tests in "list"
command egrep 'ww3_ufs' matrix.tmp | cat >> list_ufs
awk '!/ww3_ufs/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
command egrep 'ww3_tp2.14|ww3_tp2.15|ww3_tp2.17|ww3_tp2.21' matrix.tmp | cat >> list_heavy
awk '!/ww3_tp2.14/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
awk '!/ww3_tp2.15/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
awk '!/ww3_tp2.17/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
awk '!/ww3_tp2.21/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
command egrep 'mpirun|mpiexec|MPI_LAUNCH|srun|\-O' matrix.tmp | cat >> list_mpi
awk '!/mpirun|mpiexec|MPI_LAUNCH|srun|\-O/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
#Separate the OMP/OMPH tests from MPI tests
command egrep '\-t|\-O' list_mpi | cat >> list_omp
awk '!/\-t|\-O/' list_mpi > tmpfile && mv tmpfile list_mpi

split -dl $maxlist1 list_mpi list_mpi_
rm list_mpi
matrixno1=$(ls list_mpi_* | wc -l)
echo "Total nummber of matrix with parallel tests = $(($matrixno1 + 1)); each includes $maxlist1 tests"

split -dl $maxlist2 list_omp list_omp_
rm list_omp
matrixno2=$(ls list_omp_* | wc -l)
echo "Total nummber of matrix with parallel tests with OMP/OMPH = $matrixno2; each includes $maxlist2 tests"
#serial jobs
command egrep 'run_test' matrix.tmp | cat >> list_serial
split -dl $maxlist3 list_serial list_serial_
rm list_serial
matrixno3=$(ls list_serial_* | wc -l)
echo "Total nummber of matrix with serial test = $matrixno3; each includes $maxlist3 tests"
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
  if [ -f "matrix${count}" ]; then rm -f matrix${count}; fi
  cat before >> matrix$count
  cat list_mpi_$i >> matrix$count
  sed -i 's/'matrix.out'/'matrix${count}.out'/gI' matrix$count
  sed -i 's/'model'/'model${count}'/gI' matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo '     *  end of WAVEWATCH III matrix$count of regression tests     *'"   >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "rm -rf ${HOME}/model${count}"                                                   >> matrix$count
#make sure ../model$count does not exist and copy a fresh copy
  awk '1;/cd/ && !x {print "  cp -r ../model ../model'$count'"; x=1;}' matrix$count > tmpfile && mv tmpfile matrix$count
  awk '1;/cd/ && !x {print "  if [ -d ../model'${count}' ]; then rm -rf ../model'${count}'; fi"; x=1;}' matrix$count > tmpfile && mv tmpfile matrix$count

  echo " matrix$count prepared"
 done

# --------------------------------------------------------------------------- #

# parallel jobs +OMP/OMPH
  for i in `seq -f '%02g' 0 "$((matrixno2 - 1))"`; do
#echo $i
#Replace matrix.out > matrix?.out, model > model?
  (( count = count + 1 ))
  if [ -f "matrix${count}" ]; then rm -f matrix${count}; fi
  cat before >> matrix$count
  cat list_omp_$i >> matrix$count
  sed -i 's/'matrix.out'/'matrix${count}.out'/gI' matrix$count
  sed -i 's/'model'/'model${count}'/gI' matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo '     *  end of WAVEWATCH III matrix$count of regression tests     *'"   >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "rm -rf ${HOME}/model${count}"                                                   >> matrix$count
#make sure ../model$count does not exist and copy a fresh copy
  awk '1;/cd/ && !x {print "  cp -r ../model ../model'$count'"; x=1;}' matrix$count > tmpfile && mv tmpfile matrix$count
  awk '1;/cd/ && !x {print "  if [ -d ../model'${count}' ]; then rm -rf ../model'${count}'; fi"; x=1;}' matrix$count > tmpfile && mv tmpfile matrix$count

  echo " matrix$count prepared"
 done

# --------------------------------------------------------------------------- #

#serial jobs
 for i in `seq -f '%02g' 0 "$((matrixno3 - 1))"`; do
#echo $i
#Replace matrix.out > matrix?.out, model > model?
  (( count = count + 1 ))
  if [ -f "matrix${count}" ]; then rm -f matrix${count}; fi
  cat before >> matrix$count
  cat list_serial_$i >> matrix$count
  sed -i 's/'matrix.out'/'matrix${count}.out'/gI' matrix$count
  sed -i 's/'model'/'model${count}'/gI' matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo '     *  end of WAVEWATCH III matrix$count of regression tests     *'"   >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "rm -rf ${HOME}/model${count}"                                                   >> matrix$count
#make sure ../model$count does not exist and copy a fresh copy
  awk '1;/cd/ && !x {print "  cp -r ../model ../model'$count'"; x=1;}' matrix$count > tmpfile && mv tmpfile matrix$count
  awk '1;/cd/ && !x {print "  if [ -d ../model'${count}' ]; then rm -rf ../model'${count}'; fi"; x=1;}' matrix$count > tmpfile && mv tmpfile matrix$count

  echo " matrix$count prepared"
 done

# --------------------------------------------------------------------------- #

#ww3_tp2.14 is separated, as it has dependency. 
#ww3_tp2.17 and ww3_tp2.21 is separated, as it takes a long time to finish
  (( count = count + 1 ))
  if [ -f "matrix${count}" ]; then rm -f matrix${count}; fi
  cat before >> matrix$count
  cat list_heavy >> matrix$count
  sed -i 's/'matrix.out'/'matrix${count}.out'/gI' matrix$count
  sed -i 's/'model'/'model${count}'/gI' matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo '     *  end of WAVEWATCH III matrix$count of regression tests     *'"   >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "rm -rf ${HOME}/model${count}"                                                   >> matrix$count
#make sure ../model$count does not exist and copy a fresh copy
  awk '1;/cd/ && !x {print "  cp -r ../model ../model'$count'"; x=1;}' matrix$count > tmpfile && mv tmpfile matrix$count
  awk '1;/cd/ && !x {print "  if [ -d ../model'${count}' ]; then rm -rf ../model'${count}'; fi"; x=1;}' matrix$count > tmpfile && mv tmpfile matrix$count

  echo " matrix$count prepared"

# --------------------------------------------------------------------------- #

#ncep operational tests including ww3_ufs and gfsv16 which require a large number of processor/esmf coupler and the ones for grib test are separated
  (( count = count + 1 ))
  if [ -f "matrix${count}" ]; then rm -f matrix${count}; fi
  cat before >> matrix$count
  sed -i 's/'n\ 24'/'n\ 140'/gI' matrix$count
  cat list_ufs >> matrix$count
  sed -i 's/'matrix.out'/'matrix${count}.out'/gI' matrix$count
  sed -i 's/'model'/'model${count}'/gI' matrix$count
  sed -i 's/'##SBATCH'/'#SBATCH'/gI' matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo '     *  end of WAVEWATCH III matrix$count of regression tests     *'"   >> matrix$count
  echo "  echo '     **************************************************************'"   >> matrix$count
  echo "  echo ' '"                                                                     >> matrix$count
  echo "rm -rf ${HOME}/model${count}"                                                   >> matrix$count
#make sure ../model$count does not exist and copy a fresh copy
  awk '1;/cd/ && !x {print "  cp -r ../model ../model'$count'"; x=1;}' matrix$count > tmpfile && mv tmpfile matrix$count
  awk '1;/cd/ && !x {print "  if [ -d ../model'${count}' ]; then rm -rf ../model'${count}'; fi"; x=1;}' matrix$count > tmpfile && mv tmpfile matrix$count

 echo " matrix$count prepared"

# --------------------------------------------------------------------------- #

rm before
rm list*

  echo "file matrix is divided into $count subsets ...."

# --------------------------------------------------------------------------- #
# End to matrix_divider                                                       #
# --------------------------------------------------------------------------- #

