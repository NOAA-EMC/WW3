#!/bin/bash
# --------------------------------------------------------------------------- #
# matrix_datarmor: Run matrix of regression tests on target machine.          #
#                                                                             #
# Remarks:                                                                    #
# - This version is set up for automatic w3_setenv script and for the         #
#   IFREMER HPC 'datarmor'. When using this for your own setup and            #
#   computer, please copy rather than modify.                                 #
#                                                                             #
#                                                      Hendrik L. Tolman      #
#                                                      Mickael Accensi        #
#                                                                             #
#                                                      August 2013            #
#                                                      December 2013          #
#                                                      April 2018             #
#                                                      October 2021           #
#                                                      Feb 2022               #
#                                                                             #
#    Copyright 2013 National Weather Service (NWS),                           #
#       National Oceanic and Atmospheric Administration.  All rights          #
#       reserved.  WAVEWATCH III is a trademark of the NWS.                   #
#       No unauthorized use without permission.                               #
#                                                                             #
# --------------------------------------------------------------------------- #

usage ()
{
cat 2>&1 << EOF

Usage: $myname model_dir 
Required:
  model_dir : path to model dir of WW3 source
EOF
}


# 0.e Get required arguments
if [ ! $# = 0 ]
then
  main_dir="$1" ; shift
else
  usage
  exit 1
fi

#convert main_dir to absolute path
main_dir="`cd $main_dir 1>/dev/null 2>&1 && pwd`"

  cmplr=datarmor_intel



# 1. Set up
   
  export np='28'      #number of mpi tasks
  export npl='28'    #number of mpi tasks for ufs applications and large setups
  export npl1='20'   #number of mpi tasks for ufs/large setups (b4b check)
  export nr='4'       #number of mpi tasks for hybrid
  export nth='7'      #number of threads
  export nth1='6'     #number of threads (b4b check)

# 1.a Computer/ user dependent set up

  echo '#!/bin/bash'                                        > matrix.head
  echo ' '                                                  >> matrix.head
  echo '#PBS -q mpi_1'                                      >> matrix.head
  echo "#PBS -l select=1:ncpus=28:mpiprocs=$nr:ompthreads=$nth:mem=20G"  >> matrix.head
  echo '#PBS -l walltime=12:00:00'                          >> matrix.head
  echo "#PBS -N $(basename $(dirname $(dirname $PWD)))"     >> matrix.head
  echo '#PBS -j oe'                                         >> matrix.head
  echo '#PBS -o matrix.out'                                 >> matrix.head
  echo ' '                                                  >> matrix.head

  echo "  cd $(dirname $main_dir)/regtests"                 >> matrix.head
  echo ' '                                                  >> matrix.head

# Netcdf, grib and Parmetis modules

  echo '  source /usr/share/Modules/3.2.10/init/bash'       >> matrix.head
  echo '  module purge'                                     >> matrix.head

  if [ $cmplr = "datarmor_intel_debug" ] || [ $cmplr = "datarmor_intel" ]
  then
    COMP='INTEL'
    echo '  module load intel-comp/18'                      >> matrix.head
    echo '  module load impi/2018.1.163'                    >> matrix.head
    echo '  export CC=mpiicc'                               >> matrix.head
    echo '  export FC=mpiifort'                             >> matrix.head
  elif [ $cmplr = "datarmor_mpt_debug" ] || [ $cmplr = "datarmor_mpt" ]
  then
    COMP='MPT'
    echo '  module load intel-comp/18'                      >> matrix.head
    echo '  module load mpt/2.18'                           >> matrix.head
    echo "  export CC='icc -lmpi'"                          >> matrix.head
    echo "  export FC='ifort -lmpi'"                        >> matrix.head
  elif [ $cmplr = "datarmor_gnu_debug" ] | [ $cmplr = "datarmor_gnu" ]
  then
    COMP='GNU'
    echo '  module load impi/2018.1.163'                    >> matrix.head
    echo '  export CC=mpigcc'                               >> matrix.head
    echo '  export FC=mpif90'                               >> matrix.head
  elif [ $cmplr = "datarmor_pgi_debug" ] || [ $cmplr = "datarmor_pgi" ]
  then
    COMP='PGI'
    echo '  module load pgi/17.10'                          >> matrix.head
  fi

  echo "  export PATH=\${PATH}:/home/datawork-wave/CMAKE2022/cmake-3.22.1/bin"             >> matrix.head
  echo "  export PATH=\${PATH}:/home/datawork-wave/NETCDF2019/${COMP}/bin"             >> matrix.head
  echo "  export CPATH=\${CPATH}:/home/datawork-wave/NETCDF2019/${COMP}/include"       >> matrix.head
  echo "  export LD_LIBRARY_PATH=\${LD_LIBRARY_PATH}:/home/datawork-wave/NETCDF2019/${COMP}/lib"  >> matrix.head
  echo "  export NETCDF_CONFIG=/home/datawork-wave/NETCDF2019/${COMP}/bin/nc-config"  >> matrix.head
  echo "  export NetCDF_ROOT=/home/datawork-wave/NETCDF2019/${COMP}"  >> matrix.head
  echo "  export METIS_PATH=/home/datawork-wave/PARMETIS2019/${COMP}"                 >> matrix.head
  echo "  export SCOTCH_PATH=/home/datawork-wave/LIB/SCOTCH/v7.0.3/${COMP}"                 >> matrix.head
  echo "  export WW3_PARCOMPN=4"                                                      >> matrix.head
  echo "  export G2_LIB4=/home/datawork-wave/NCEPLIBS/${COMP}/g2-3.4.5/lib64/libg2_4.a"            >> matrix.head
  echo "  export BACIO_LIB4=/home/datawork-wave/NCEPLIBS/${COMP}/bacio-2.4.1/lib/libbacio_4.a"      >> matrix.head
  echo "  export JASPER_LIB=/home/datawork-wave/NETCDF2019/${COMP}/lib/libjasper.so"  >> matrix.head
  echo "  export PNG_LIB=/home/datawork-wave/NETCDF2019/${COMP}/lib/libpng.so"        >> matrix.head
  echo "  export Z_LIB=/lib64/libz.so.1"                                              >> matrix.head
  echo "  # cmake dependencies"      >> matrix.head
  echo "  export CMAKE_PREFIX_PATH=/home/datawork-wave/NETCDF2019/${COMP}"      >> matrix.head
  echo "  export g2_DIR=/home/datawork-wave/NCEPLIBS/${COMP}/g2-3.4.5/lib64/cmake/g2"      >> matrix.head
  echo "  export bacio_DIR=/home/datawork-wave/NCEPLIBS/${COMP}/bacio-2.4.1/lib/cmake/bacio"      >> matrix.head
  echo "  export w3emc_DIR=/home/datawork-wave/NCEPLIBS/${COMP}/w3emc-2.9.2/lib/cmake/w3emc"      >> matrix.head
  echo ' '

  export mpi='$MPI_LAUNCH'

# Compile option
  opt="-f -N -S -T -o both"

# Base run_test command line
  export rtst="./bin/run_cmake_test $opt"

  export  ww3='../model'

# 1.b Flags to do course selection - - - - - - - - - - - - - - - - - - - - - -
#     Addition selection by commenting out lines as below

  export       shrd='y' # Do shared architecture tests
  export       dist='y' # Do distributed architecture (MPI) tests
  export        omp='y' # Threaded (OpenMP) tests
  export       hybd='y' # Hybrid options

  export     prop1D='y' # 1-D propagation tests (ww3_tp1.X)
  export     prop2D='y' # 2-D propagation tests (ww3_tp2.X)
  export       time='y' # time linmited growth
  export      fetch='y' # fetch linmited growth
  export     hur1mg='y' # Hurricane with one moving grid
  export      shwtr='y' # shallow water tests
  export      unstr='y' # unstructured grid tests
  export      pdlib='y' # unstr with pdlib for domain decomposition and implicit solver
  export      smcgr='y' # SMC grid test
  export        rtd='y' # Rotated pole test
  export     mudice='y' # Mud/Ice and wave interaction tests 
  export     infgrv='y' # Second harmonic generation tests
  export       uost='y' # ww3_ts4 Unresolved Obstacles Source Term (UOST)
  export      assim='y' # Restart spectra update
  export      oasis='y' # Atmosphere, ocean, and ice coupling using OASIS
  export   calendar='y' # Calendar type
  export   confignc='y' # Configurable netCDF meta data (ww3_ounf)

  export    multi01='y' # mww3_test_01 (wetting and drying)
  export    multi02='y' # mww3_test_02 (basic two-way nesting test))
  export    multi03='y' # mww3_test_03 (three high and three low res grids).
  export    multi04='y' # mww3_test_04 (swell on sea mount and/or current)
  export    multi05='y' # mww3_test_05 (three-grid moving hurricane)
  export    multi06='y' # mww3_test_06 (curvilinear grid tests)
  export    multi07='y' # mww3_test_07 (unstructured grid tests)
  export    multi08='y' # mww3_test_08 (wind and ice tests)
  export    multi09='y' # mww3_test_09 (SMC multi grid test)
  export        ufs='n' # The Unified Forecast System
  export  ufscoarse='n' # Option for small PCs
  export       grib='y' # grib file field output
  export  rstrt_b4b='y' # Restart Reproducibility
  export    npl_b4b='y' # MPI task Reproducibility
  export    nth_b4b='y' # Thread Reproducibility
  export       esmf='n' # ESMF coupling
# export   filter='PR3 ST2 UQ'
                      # The filter does a set of consecutive greps on the 
                      # command lines generated by filter.base with the above
                      # selected options.

# --------------------------------------------------------------------------- #
# 2.  Execute matrix.base ...                                                 #
# --------------------------------------------------------------------------- #
             
  $main_dir/../regtests/bin/matrix.base

  $main_dir/../regtests/bin/matrix_divider_cmake.sh 

# --------------------------------------------------------------------------- #
# End to the matrix                                                           #
# --------------------------------------------------------------------------- #
