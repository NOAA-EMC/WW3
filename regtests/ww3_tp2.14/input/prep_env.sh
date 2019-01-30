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

echo '   compile oasis coupler'
cd $path_i/oasis3-mct/util/make_dir
if [ $cmplr ]
then
  echo ' '
  echo '   Setup cmplr file'
  if [ "$cmplr" == "mpt" ] || [ "$cmplr" == "mpt_debug" ]                         || \
     [ "$cmplr" == "zeus_mpt" ] || [ "$cmplr" == "zeus_mpt_debug" ]               || \
     [ "$cmplr" == "datarmor_mpt" ] || [ "$cmplr" == "datarmor_mpt_debug" ]       || \
     [ "$cmplr" == "intel" ] || [ "$cmplr" == "intel_debug" ]                     || \
     [ "$cmplr" == "zeus_intel" ] || [ "$cmplr" == "zeus_intel_debug" ]           || \
     [ "$cmplr" == "datarmor_intel" ] || [ "$cmplr" == "datarmor_intel_debug" ]   || \
     [ "$cmplr" == "gnu" ] || [ "$cmplr" == "gnu_debug" ]                         || \
     [ "$cmplr" == "zeus_gnu" ] || [ "$cmplr" == "zeus_gnu_debug" ]               || \
     [ "$cmplr" == "datarmor_gnu" ] || [ "$cmplr" == "datarmor_gnu_debug" ]       || \
     [ "$cmplr" == "pgi" ] || [ "$cmplr" == "pgi_debug" ]                         || \
     [ "$cmplr" == "zeus_pgi" ] || [ "$cmplr" == "zeus_pgi_debug" ]               || \
     [ "$cmplr" == "datarmor_pgi" ] || [ "$cmplr" == "datarmor_pgi_debug" ] ; then
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
     sed -e "s/<optc_short>/$optcs/" -e "s/<optl_short>/$optls/" -e "s/<comp_mpi>/$comp_mpi/" -e "s/<comp_mpi_exe>/$comp_mpi_exe/" cmplr.tmpl > cmplr
    echo "      sed cmplr.tmpl => cmplr"
  else
    errmsg "cmplr.$cmplr not found"
    exit 1
  fi
  chmod 775 cmplr
fi

make realclean -f TopMakefileOasis3 > $path_w/oasis_clean.out
make -f TopMakefileOasis3 > $path_w/oasis_make.out

echo '   compile toy model'
cd $path_i/toy
make clean > $path_w/toy_clean.out
make > $path_w/toy_make.out

echo '   copy oasis coupler inputs'
cd $path_w
ln -sf ../input/namcouple.$swtstr namcouple

echo '   copy toy model inputs'
cd $path_w
cp ../input/toy/r-toy.nc.$swtstr r-toy.nc
ln -sf ../input/toy/grid_toy_model.nc .
ln -sf ../input/toy/toy_coupled_field.nc.$swtstr toy_coupled_field.nc
ln -sf ../input/TOYNAMELIST.nam.$swtstr TOYNAMELIST.nam
ln -sf ../input/toy/toy_model .

echo '   copy ww3 model inputs'
cd $path_w
cp ../input/r-ww3.nc.$swtstr r-ww3.nc
cp ../input/ww3_shel_${swtstr}.inp ../input/ww3_shel.inp
cp ../input/ww3_shel_${swtstr}.nml ../input/ww3_shel.nml
echo ''
