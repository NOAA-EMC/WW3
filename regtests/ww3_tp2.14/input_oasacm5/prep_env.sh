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


echo '   Setup oasis cmplr file'
cd $path_w/oasis3-mct/util/make_dir
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
sed -e "s:<oasis_input_path>:$path_w/oasis3-mct:" \
    -e "s:<oasis_work_path>:$path_w/work_oasis3-mct:" \
    -e "s/<optc_short>/$optcs/" -e "s/<optl_short>/$optls/" \
    -e "s/<comp_mpi>/$comp_mpi/" -e "s/<comp_mpi_exe>/$comp_mpi_exe/" \
    -e "s/<wwatch3_cc>/$WWATCH3_CC/"  cmplr.tmpl > cmplr
echo "      sed cmplr.tmpl => cmplr"
chmod 775 cmplr


echo '   setup oasis make.inc file'
sed -e "s:<oasis_input_path>:$path_w/oasis3-mct:" make.inc.tmpl > make.inc


echo '   compile oasis coupler'
make realclean -f TopMakefileOasis3 > $path_w/oasis_clean.out
make -f TopMakefileOasis3 > $path_w/oasis_make.out


echo '   setup toy Makefile'
cd $path_w/toy
sed -e "s:<oasis_input_path>:$path_w/oasis3-mct:" Makefile.tmpl > Makefile


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
