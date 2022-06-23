#!/bin/bash -e

if [ $# -ne 4 ]
then
  echo "need four arguments:"
  echo '$1 : path_i'
  echo '$2 : path_w'
  echo '$3 : switch'
  echo '$4 : ww3_dir'
  exit 1
fi

path_i=$1
path_w=$2
swtstr=$3
ww3_dir=$4

echo ''
echo '   setup coupling environment'

export WWATCH3_DIR=${ww3_dir}/model
export OASIS_INPUT_PATH=$path_w/oasis3-mct
export OASIS_WORK_PATH=$path_w/work_oasis3-mct


echo '   copy oasis and toy in $path_w'
cp -r $path_i/../input/toy $path_w/
cp -r $path_i/../input/oasis3-mct $path_w/


echo '   Setup oasis cmplr file'
cd $path_w/oasis3-mct/util/make_dir

echo '   setup oasis make.inc file'

echo '   compile oasis coupler'
# Build OASIS with CMake wrapper
rm -rf Makefile CMakeCache.txt  CMakeFiles  cmake_install.cmake cmplr src tmp
cmake . ${CMAKE_OPTIONS:-}
make

echo '   setup toy Makefile'
cd $path_w/toy

echo '   compile toy model'
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

cp $path_i/TOYNAMELIST.nam $path_w/TOYNAMELIST.nam


echo '   copy ww3 model inputs'
cd $path_w
if [ -f $path_i/r-ww3.nc ]; then
  cp $path_i/r-ww3.nc $path_w/r-ww3.nc
else
  echo "WARNING: model input $path_i/toy/r-ww3.nc does not exist"
fi



echo ''
