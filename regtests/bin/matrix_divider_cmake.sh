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
maxlist2=35
maxlist3=104
#Put the job requirement/spec in "before"
sed -e "/run_cmake_test/,\$d" matrix.tmp > before
#Put the list of tests in "list"
command egrep 'ww3_ufs1.2|ww3_ufs1.3' matrix.tmp | cat >> list_ufs
command egrep 'ww3_tp2.14|ww3_tp2.15|ww3_tp2.17|ww3_tp2.21|ww3_ufs1.1' matrix.tmp | cat >> list_heavy
awk '!/ww3_tp2.14/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
awk '!/ww3_tp2.15/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
awk '!/ww3_tp2.17/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
awk '!/ww3_tp2.21/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
awk '!/ww3_ufs/' matrix.tmp > tmpfile && mv tmpfile matrix.tmp
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
command egrep 'run_cmake_test' matrix.tmp | cat >> list_serial
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
  (( count = count + 1 ))
  countf=$(printf %02i $count)
  if [ -f "matrix${countf}" ]; then rm -f matrix${countf}; fi
  cat before >> matrix${countf}
  cat list_mpi_$i >> matrix${countf}
  sed -i 's/'matrix.out'/'matrix${countf}.out'/gI' matrix${countf}
  sed -i 's/'buildmatrix'/'buildmatrix${countf}'/gI' matrix${countf}
  sed -i 's/'ww3_regtest'/'ww3_regtest_${countf}'/gI' matrix${countf}
  echo '  [[ -d ${path_build_root} ]] && rm -rf ${path_build_root}*'                    >> matrix${countf}
  echo "  echo ' '"                                                                     >> matrix${countf}
  echo "  echo '     **************************************************************'"   >> matrix${countf}
  echo "  echo '     *  end of WAVEWATCH III matrix${countf} of regression tests  *'"   >> matrix${countf}
  echo "  echo '     **************************************************************'"   >> matrix${countf}
  echo "  echo ' '"                                                                     >> matrix${countf}
  echo " matrix${countf} prepared"
 done

# --------------------------------------------------------------------------- #

# parallel jobs +OMP/OMPH
  for i in `seq -f '%02g' 0 "$((matrixno2 - 1))"`; do
  (( count = count + 1 ))
  countf=$(printf %02i $count)
  if [ -f "matrix${countf}" ]; then rm -f matrix${countf}; fi
  cat before >> matrix${countf}
  cat list_omp_$i >> matrix${countf}
  sed -i 's/'matrix.out'/'matrix${countf}.out'/gI' matrix${countf}
  sed -i 's/'buildmatrix'/'buildmatrix${countf}'/gI' matrix${countf}
  sed -i 's/'ww3_regtest'/'ww3_regtest_${countf}'/gI' matrix${countf}
  echo '  [[ -d ${path_build_root} ]] && rm -rf ${path_build_root}*'                    >> matrix${countf}
  echo "  echo ' '"                                                                     >> matrix${countf}
  echo "  echo '     **************************************************************'"   >> matrix${countf}
  echo "  echo '     *  end of WAVEWATCH III matrix${countf} of regression tests  *'"   >> matrix${countf}
  echo "  echo '     **************************************************************'"   >> matrix${countf}
  echo "  echo ' '"                                                                     >> matrix${countf}
  echo " matrix${countf} prepared"
 done

# --------------------------------------------------------------------------- #

#serial jobs
 for i in `seq -f '%02g' 0 "$((matrixno3 - 1))"`; do
  (( count = count + 1 ))
  countf=$(printf %02i $count)
  if [ -f "matrix${countf}" ]; then rm -f matrix${countf}; fi
  cat before >> matrix${countf}
  cat list_serial_$i >> matrix${countf}
  sed -i 's/'matrix.out'/'matrix${countf}.out'/gI' matrix${countf}
  sed -i 's/'buildmatrix'/'buildmatrix${countf}'/gI' matrix${countf}
  sed -i 's/'ww3_regtest'/'ww3_regtest_${countf}'/gI' matrix${countf}
  echo '  [[ -d ${path_build_root} ]] && rm -rf ${path_build_root}*'                    >> matrix${countf}
  echo "  echo ' '"                                                                     >> matrix${countf}
  echo "  echo '     **************************************************************'"   >> matrix${countf}
  echo "  echo '     *  end of WAVEWATCH III matrix${countf} of regression tests  *'"   >> matrix${countf}
  echo "  echo '     **************************************************************'"   >> matrix${countf}
  echo "  echo ' '"                                                                     >> matrix${countf}
  echo " matrix${countf} prepared"
 done

# --------------------------------------------------------------------------- #

#ww3_tp2.14 is separated, as it has dependency. 
#ww3_tp2.17 and ww3_tp2.21 is separated, as it takes a long time to finish
  (( count = count + 1 ))
  countf=$(printf %02i $count)
  if [ -f "matrix${countf}" ]; then rm -f matrix${countf}; fi
  cat before >> matrix${countf}
  cat list_heavy >> matrix${countf}
  sed -i 's/'matrix.out'/'matrix${countf}.out'/gI' matrix${countf}
  sed -i 's/'buildmatrix'/'buildmatrix${countf}'/gI' matrix${countf}
  sed -i 's/'ww3_regtest'/'ww3_regtest_${countf}'/gI' matrix${countf}
  echo '  [[ -d ${path_build_root} ]] && rm -rf ${path_build_root}*'                    >> matrix${countf}
  echo "  echo ' '"                                                                     >> matrix${countf}
  echo "  echo '     **************************************************************'"   >> matrix${countf}
  echo "  echo '     *  end of WAVEWATCH III matrix${countf} of regression tests  *'"   >> matrix${countf}
  echo "  echo '     **************************************************************'"   >> matrix${countf}
  echo "  echo ' '"                                                                     >> matrix${countf}
  echo " matrix${countf} prepared"

# --------------------------------------------------------------------------- #

#ncep operational tests including ww3_ufs and gfsv16 which require a large number of processor/esmf coupler and the ones for grib test are separated
  (( count = count + 1 ))
  countf=$(printf %02i $count)
  if [ -f "matrix${countf}" ]; then rm -f matrix${countf}; fi
  cat before >> matrix${countf}
  sed -i 's/'n\ 24'/'n\ 140'/gI' matrix${countf}
  cat list_ufs >> matrix${countf}
  sed -i 's/'matrix.out'/'matrix${countf}.out'/gI' matrix${countf}
  sed -i 's/'##SBATCH'/'#SBATCH'/gI' matrix${countf}
  sed -i 's/'buildmatrix'/'buildmatrix${countf}'/gI' matrix${countf}
  sed -i 's/'ww3_regtest'/'ww3_regtest_${countf}'/gI' matrix${countf}
  echo '  [[ -d ${path_build_root} ]] && rm -rf ${path_build_root}*'                    >> matrix${countf}
  echo "  echo ' '"                                                                     >> matrix${countf}
  echo "  echo '     **************************************************************'"   >> matrix${countf}
  echo "  echo '     *  end of WAVEWATCH III matrix${countf} of regression tests  *'"   >> matrix${countf}
  echo "  echo '     **************************************************************'"   >> matrix${countf}
  echo "  echo ' '"                                                                     >> matrix${countf}

 echo " matrix${countf} prepared"

# --------------------------------------------------------------------------- #

rm before
rm list*

  echo "file matrix is divided into ${count} subsets ...."

# --------------------------------------------------------------------------- #
# End to matrix_divider                                                       #
# --------------------------------------------------------------------------- #

