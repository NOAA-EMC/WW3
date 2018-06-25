#!/bin/sh -e

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
ln -sf make.$cmplr make
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

echo ''
