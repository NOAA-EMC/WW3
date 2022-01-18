#!/bin/bash -e

if [ $# -ne 5 ]
then
  echo "need five arguments:"
  echo '$1 : path_i'
  echo '$2 : path_w'
  echo '$3 : complr'
  echo '$4 : switch'
  echo '$5 : ww3_dir'
  exit 1
fi

path_i=$1
path_w=$2
cmplr=$3
swtstr=$4
ww3_dir=$5

echo ''
echo '   setup coupling environment'

echo '   compile oasis coupler'
cd $path_i/oasis3-mct/util/make_dir

export WWATCH3_DIR=${ww3_dir}/model

# Build OASIS with CMake wrapper
rm -rf Makefile CMakeCache.txt  CMakeFiles  cmake_install.cmake cmplr src tmp
cmake .
make

echo '   compile toy model'
cd $path_i/toy
make clean > $path_w/toy_clean.out
make > $path_w/toy_make.out

echo '   copy oasis coupler inputs'
cd $path_w
ln -sf ../input/namcouple.$swtstr namcouple

echo '   copy toy model inputs'
cd $path_w
if [ -f ../input/toy/r-toy.nc.$swtstr ]; then
  cp ../input/toy/r-toy.nc.$swtstr r-toy.nc
else
  echo "WARNING: model input ../input/toy/r-toy.nc.$swtstr does not exist"
fi
ln -sf ../input/toy/grid_toy_model.nc .
ln -sf ../input/toy/toy_coupled_field.nc.$swtstr toy_coupled_field.nc
ln -sf ../input/TOYNAMELIST.nam.$swtstr TOYNAMELIST.nam
ln -sf ../input/toy/toy_model .

echo '   copy ww3 model inputs'
cd $path_w
if [ -f ../input/r-ww3.nc.$swtstr ]; then
  cp ../input/r-ww3.nc.$swtstr r-ww3.nc
else
  echo "WARNING: model input ../input/toy/r-ww3.nc.$swtstr does not exist"
fi
cp ../input/ww3_shel_${swtstr}.inp ../input/ww3_shel.inp
cp ../input/ww3_shel_${swtstr}.nml ../input/ww3_shel.nml
echo ''
