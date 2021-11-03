#!/bin/bash -e

if [ $# -ne 4 ]
then
  echo "need four arguments:"
  echo '$1 : path_i'
  echo '$2 : path_w'
  echo '$3 : complr'
  echo '$4 : switch'
  exit 1
fi

path_i=$1
path_w=$2
cmplr=$3
swtstr=$4

echo ''
echo '   setup coupling environment'
export WWATCH3_DIR=`grep WWATCH3_DIR $WWATCH3_ENV | awk -F' ' '{print $2}' `
export WWATCH3_CC=`grep WWATCH3_CC $WWATCH3_ENV | awk -F' ' '{print $2}' `


echo '   copy oasis and toy in $path_w'
cp -r $path_i/../input/toy $path_w/
cp -r $path_i/../input/oasis3-mct $path_w/


echo '   compile oasis coupler'
cd $path_w/oasis3-mct/util/make_dir
if [ $cmplr ]
then
  echo ' '
  echo '   Setup cmplr file'
  if [ "$cmplr" == "mpt" ] || [ "$cmplr" == "mpt_debug" ]                         || \
     [ "$cmplr" == "datarmor_mpt" ] || [ "$cmplr" == "datarmor_mpt_debug" ]       || \
     [ "$cmplr" == "intel" ] || [ "$cmplr" == "intel_debug" ]                     || \
     [ "$cmplr" == "so_intel" ] || [ "$cmplr" == "so_intel_debug" ]               || \
     [ "$cmplr" == "datarmor_intel" ] || [ "$cmplr" == "datarmor_intel_debug" ]   || \
     [ "$cmplr" == "gnu" ] || [ "$cmplr" == "gnu_debug" ]                         || \
     [ "$cmplr" == "hera.intel" ] || [ "$cmplr" == "orion.intel" ]                || \
     [ "$cmplr" == "hera.gnu" ]   || [ "$cmplr" == "jet.intel" ]                  || \
     [ "$cmplr" == "stampede.intel" ] || [ "$cmplr" == "gaea.intel" ]             || \
     [ "$cmplr" == "cheyenne.intel" ] || [ "$cmplr" == "cheyenne.gnu" ]           || \
     [ "$cmplr" == "s4.intel" ] || \
     [ "$cmplr" == "wcoss_cray" ] || [ "$cmplr" == "wcoss_dell_p3" ]              || \
     [ "$cmplr" == "datarmor_gnu" ] || [ "$cmplr" == "datarmor_gnu_debug" ]       || \
     [ "$cmplr" == "pgi" ] || [ "$cmplr" == "pgi_debug" ]                         || \
     [ "$cmplr" == "datarmor_pgi" ] || [ "$cmplr" == "datarmor_pgi_debug" ]       || \
     [ "$cmplr" == "ukmo_cray" ] || [ "$cmplr" == "ukmo_cray_debug" ]             || \
     [ "$cmplr" == "ukmo_cray_gnu" ] || [ "$cmplr" == "ukmo_cray_gnu_debug" ]; then
     source $WWATCH3_DIR/bin/cmplr.env
     # shortlist optl
     alloptl=( $optl )
     for ioptl in $(seq 2 ${#alloptl[@]}); do
       optls="${optls}${alloptl[$ioptl]} "
     done
     # shortlist optc
     alloptc=( $optc )
     for ioptc in $(seq 3 ${#alloptc[@]}); do
       optcs="${optcs}${alloptc[$ioptc]} "
     done
     # shorten comp_mpi
     comp_mpi_exe="$(echo $comp_mpi | awk -F' ' '{print $1}')"
     # sed cmplr.tmpl
     sed -e "s/<optc_short>/$optcs/" -e "s/<optl_short>/$optls/" -e "s/<comp_mpi>/$comp_mpi/" -e "s/<wwatch3_cc>/$WWATCH3_CC/" -e "s/<comp_mpi_exe>/$comp_mpi_exe/" cmplr.tmpl > cmplr
    echo "      sed cmplr.tmpl => cmplr"
  else
    echo "ERROR: cmplr.$cmplr not found" 2>&1
    exit 1
  fi
  chmod 775 cmplr
fi

make realclean -f TopMakefileOasis3 > $path_w/oasis_clean.out
make -f TopMakefileOasis3 > $path_w/oasis_make.out

echo '   compile toy model'
cd $path_w/toy
make clean > $path_w/toy_clean.out
make > $path_w/toy_make.out

echo '   copy oasis coupler inputs'
cp $path_i/namcouple $path_w/namcouple

echo '   copy toy model inputs'
if [ -f $path_w/toy/r-toy.nc.$swtstr ]; then
  cp $path_w/toy/r-toy.nc.$swtstr $path_w/r-toy.nc
else
  echo "WARNING: model input $path_w/toy/r-toy.nc.$swtstr does not exist"
fi

cp $path_w/toy/grid_toy_model.nc $path_w/
cp $path_w/toy/toy_coupled_field.nc.$swtstr $path_w/toy_coupled_field.nc
cp $path_w/toy/toy_model $path_w/

cp $path_i/TOYNAMELIST.nam $path_w/

echo '   copy ww3 model inputs'
cd $path_w
if [ -f $path_i/r-ww3.nc ]; then
  cp $path_i/r-ww3.nc $path_w/r-ww3.nc
else
  echo "WARNING: model input $path_i/toy/r-ww3.nc does not exist"
fi



echo ''
