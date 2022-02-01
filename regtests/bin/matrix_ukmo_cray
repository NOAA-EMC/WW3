#!/bin/bash
# --------------------------------------------------------------------------- #
# matrix_ukmo_cray: Run matrix of regression tests on Cray XC architecture    #
#   as currently used at the UK Met Office.                                   #
#                                                                             #
# Remarks:                                                                    #
#   Currently, programs using the PDLIB switch (METIS library) crash when     #
#   compiled with the Cray CCE compiler. For those regtests that use PDLIB    #
#   it is required to use the GNU compiler.                                   #
#                                                                             #
#                                                      Chris Bunney           #
#                                                      April 2019             #
#                                                                             #
#                                                      Hendrik L. Tolman      #
#                                                      August 2013            #
#                                                      December 2013          #
#                                                      April 2018             #
#                                                                             #
#    Copyright 2013 National Weather Service (NWS),                           #
#       National Oceanic and Atmospheric Administration.  All rights          #
#       reserved.  WAVEWATCH III is a trademark of the NWS.                   #
#       No unauthorized use without permission.                               #
#                                                                             #
# --------------------------------------------------------------------------- #
# 0. Environment file

  source $(dirname $0)/../../model/bin/w3_setenv
  main_dir=$WWATCH3_DIR
  temp_dir=$WWATCH3_TMP
  source=$WWATCH3_SOURCE
  list=$WWATCH3_LIST

  echo "Main directory    : $main_dir"
  echo "Scratch directory : $temp_dir"
  echo "Save source codes : $source"
  echo "Save listings     : $list"

  # Set Cray compiler variant: CCE (Cray Compiler Environment) or GNU.
  #  - ukmo_cray[_debug]       : Cray Fortan (Cray Compiler Environment)
  #  - ukmo_cray_gnu[_dubug]   : GNU Fortran using cray wrapper
  #  - ukmo_cray_intel[_dubug] : Ifort using cray wrapper
  # Note: currently, Cray Fortran fails with some PDLIB regrests
  cmplr="ukmo_cray_gnu"

# 1. Set up for compilation environemnt on Cray XC (broadwell processors)
  echo '#!/bin/bash'                                         > matrix.head
  echo ' '                                                  >> matrix.head

# Run everything on single node in shared queue (compilation on compute
# nodes via parallel queue is inefficient and wasteful of resources):
  echo '#PBS -l ncpus=16'                                   >> matrix.head
  echo '#PBS -l mem=16GB'                                   >> matrix.head
  echo '#PBS -q shared'                                     >> matrix.head
  echo '#PBS -l walltime=04:00:00'                          >> matrix.head
  echo '#PBS -N ww3_regtest'                                >> matrix.head
  echo '#PBS -j oe'                                         >> matrix.head
  echo '#PBS -o matrix.out'                                 >> matrix.head
  echo ' '                                                  >> matrix.head

  echo "  cd $(dirname $main_dir)/regtests"                 >> matrix.head
  echo ' '                                                  >> matrix.head

if [[ $cmplr == "ukmo_cray" ]] || [[ $cmplr == "ukmo_cray_debug" ]]; then
  # Load targetted versions of Cray Development Tools (bug in Fortran StreamIO
  # for older versions) and netCDF/HDF5 modules:
    echo "module load  cdt/18.12"                         >> matrix.head
    echo "module load cray-netcdf/4.6.1.3"                >> matrix.head
    echo "module load cray-hdf5/1.10.2.0"                 >> matrix.head
    echo "export METIS_PATH=/home/d02/frey/WW3/ParMETIS"  >> matrix.head

elif [[ $cmplr == ukmo_cray_gnu* ]]; then
    # ParMETIS library not currently working with Cray compiler.
    # Use GNU compiler for programs that use PDLIB.
    echo "module switch PrgEnv-cray PrgEnv-gnu/5.2.82"    >> matrix.head
    echo "module load cray-netcdf"                        >> matrix.head
    echo "export METIS_PATH=/home/d02/frey/WW3/ParMETIS_GNU" >> matrix.head

elif [[ $cmplr == ukmo_cray_intel* ]]; then
    echo "module switch PrgEnv-cray PrgEnv-intel"         >> matrix.head
    echo "module swap intel/15.0.0.090 intel/18.0.5.274"  >> matrix.head
    echo "module load cdt/18.12"                          >> matrix.head
    echo "module load cray-netcdf/4.6.1.3"                >> matrix.head
    echo "module load cray-hdf5/1.10.2.0"                 >> matrix.head

else
    echo "Unknown compiler for UKMO regression tests: $cmplr"
    exit 1
fi

# SNP Launcher 7.7.4 allows -np switch:
  echo "  module load cray-snplauncher/7.7.4"               >> matrix.head

  echo "  export NETCDF_CONFIG=\$(which nc-config)"         >> matrix.head

# Compiler option. Set cmplOption to
# y if using for the first time or using a different compiler

  export cmplOption='y'

  export  mpi='mpiexec'
  export   np='16'
  export   nr='4'
  export  nth='4'

  if [ "$cmplOption" = 'y' ]
  then
     export rtst="./bin/run_test -c $cmplr -S"
  else
     export rtst="./bin/run_test -S"
  fi

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
  export      oasis='y' # Atmosphere, ocean, and ice coupling using oasis
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
  export       grib='n' # grib file field output
  export  rstrt_b4b='y' # Restart Reproducibility
  export    npl_b4b='y' # MPI task Reproducibility
  export    nth_b4b='y' # Thread Reproducibility
  export       esmf='n' # ESMF coupling
# export   filter='PR3 ST2 UQ'
                      # The filter does a set of consecutinve greps on the 
                      # command lines generated by filter.base with the above
                      # selected options.

# --------------------------------------------------------------------------- #
# 2.  Execute matrix.base ...                                                 #
# --------------------------------------------------------------------------- #
             
  $main_dir/../regtests/bin/matrix.base

# --------------------------------------------------------------------------- #
# End to the matrix                                                           #
# --------------------------------------------------------------------------- #
