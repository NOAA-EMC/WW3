
 
#SBATCH -n 24
##SBATCH --cpus-per-task=6
#SBATCH -q batch
#SBATCH -t 01:00:00
#SBATCH -A marine-cpu
#SBATCH -J mtest_bugfix
#SBATCH -o results_mtest_bugfix.out
#SBATCH -p orion#!/bin/sh
#SBATCH --exclusive
##SBATCH --mail-user=edwin.rainville@noaa.gov
##SBATCH --mail-type=END,FAIL 

module purge	
module use /apps/contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.1.0/install/modulefiles/Core/
module use /apps/contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.1.0/install/modulefiles/intel-oneapi-mpi/2021.13-li242lf/gcc/12.2.0
module load stack-oneapi/2024.2.1
module load stack-intel-oneapi-mpi/2021.13
module load cmake/3.27.9
module load libpng/1.6.37
module load zlib/1.2.13
module load jasper/2.0.32
module load hdf5/1.14.3
module load netcdf-c/4.9.2
module load netcdf-fortran/4.6.1
module load bacio/2.4.1
module load g2/3.5.1
module load w3emc/2.10.0
module load esmf/8.8.0
module load scotch/7.0.4
export METIS_PATH=/work/noaa/marine/Ming.Chen/apps/parmetis/parmetis_intel/orion/parmetis-4.0.3/install
export path_build_root=/work/noaa/marine/erainville/ww3_bugfix1615/regtests/buildmatrix
[[ -d ${path_build_root} ]] && rm -rf ${path_build_root}

module list

echo "Modules loaded sucessfully, starting cmake portion"
# ------------- ww3_ufs1.1 test case -----------------
# Base test case is work_unstr_a
# Run a test for UFS1.1 - Use INP files and ruun full test case
./bin/run_cmake_test -b slurm -o all -S -T -s MPI -s PDLIB -i input_unstr -w work_unstr_a -g a -f -p srun -n 24 ../model ww3_ufs1.1 

# Goal Command: Run full with -N flag 
#./bin/run_cmake_test -b slurm -o all -S -T -s MPI -s PDLIB -i input_unstr -w work_unstr_a_nml -g a -N -f -p srun -n 24 ../model ww3_ufs1.1 

# Difference between the inp and nml directories
diff -qr /work/noaa/marine/erainville/ww3_bugfix1615/regtests/ww3_ufs1.1/work_unstr_a /work/noaa/marine/erainville/ww3_bugfix1615/regtests/ww3_ufs1.1/work_unstr_a_nml > nml_test_diff.txt