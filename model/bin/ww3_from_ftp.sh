#!/bin/bash
# --------------------------------------------------------------------------- #
#                                                                             #
# Script for downloading data from ftp                                        #
#                                                    Created 0ct 10, 2017     #
# --------------------------------------------------------------------------- #

curr_dir=`pwd`

# Set WW3 code version
ww3ver=v7.12.6

#Get top level directory of ww3 from user: 
echo -e "\n\n This script will download data from the ftp for WAVEWATCH III "
echo -e "Enter the relative path to the main/top level directory, this would "
echo -e "be '../../' if in the model/bin directory or '.' if already in the "
echo -e "top/main directory:"
read ww3dir 

#Move to top level directory of ww3: 
cd $ww3dir 

#Download from ftp and uptar: 
echo -e "Downloading and untaring file from ftp:" 
wget https://ftp.emc.ncep.noaa.gov/static_files/public/WW3/ww3_from_ftp.${ww3ver}.tar.gz
tar -xvzf ww3_from_ftp.${ww3ver}.tar.gz

#Move regtest info from data_regtests to regtests:
echo -e "Moving data from data_regtests to regtests"  
cp -r data_regtests/ww3_tp2.18/input/*.nc  regtests/ww3_tp2.18/input/
cp -r data_regtests/ww3_tp2.15/input/wind.nc  regtests/ww3_tp2.15/input/
cp -r data_regtests/ww3_tp2.15/input/*.nc  regtests/ww3_tp2.15/input_rho/
cp -r data_regtests/ww3_tp2.13/*.png       regtests/ww3_tp2.13/
cp -r data_regtests/ww3_tic1.4/input/*.nc  regtests/ww3_tic1.4/input/
cp -r data_regtests/ww3_tp2.8/input/*.nc   regtests/ww3_tp2.8/input/
cp -r data_regtests/ww3_tp2.12/input/*     regtests/ww3_tp2.12/input/
cp -r data_regtests/ww3_tp2.12/input_be/*  regtests/ww3_tp2.12/input_be/
cp -r data_regtests/ww3_tp2.12/input_le/*  regtests/ww3_tp2.12/input_le/
cp -r data_regtests/ww3_tp2.14/input/r-ww3.nc.OAS*CM regtests/ww3_tp2.14/input/
cp -r data_regtests/ww3_tp2.14/input/r-ww3.nc.OASACM regtests/ww3_tp2.14/input/r-ww3.nc.OASACM2
cp -r data_regtests/ww3_tp2.14/input/rmp_* regtests/ww3_tp2.14/input/
if [ ! -d regtests/ww3_tp2.14/input/oasis3-mct/doc ]
then
  mkdir regtests/ww3_tp2.14/input/oasis3-mct/doc
fi
cp -r data_regtests/ww3_tp2.14/input/oasis3-mct/doc/* regtests/ww3_tp2.14/input/oasis3-mct/doc/ 
cp -r data_regtests/ww3_tp2.14/input/toy/*.nc.OAS*CM regtests/ww3_tp2.14/input/toy/
cp -r data_regtests/ww3_tp2.14/input/toy/r-toy.nc.OASACM regtests/ww3_tp2.14/input/toy/r-toy.nc.OASACM2
cp -r data_regtests/ww3_tp2.14/input/toy/toy_coupled_field.nc.OASACM regtests/ww3_tp2.14/input/toy/toy_coupled_field.nc.OASACM2
cp -r data_regtests/ww3_tp2.14/input/toy/toy_coupled_field.nc.OASACM regtests/ww3_tp2.14/input/toy/toy_coupled_field.nc.OASACM4
cp -r data_regtests/ww3_tp2.14/input/toy/toy_coupled_field.nc.OASACM regtests/ww3_tp2.14/input/toy/toy_coupled_field.nc.OASACM5
cp -r data_regtests/ww3_tp2.14/input/toy/toy_coupled_field.nc.OASACM regtests/ww3_tp2.14/input/toy/toy_coupled_field.nc.OASACM6
cp -r data_regtests/ww3_tp2.14/input/toy/*.nc regtests/ww3_tp2.14/input/toy/
cp -r data_regtests/ww3_tp2.17/input/*     regtests/ww3_tp2.17/input/
cp -r data_regtests/ww3_tp2.21/input/*     regtests/ww3_tp2.21/input/
cp -r data_regtests/mww3_test_09/input/*   regtests/mww3_test_09/input/
cp -r data_regtests/ww3_ufs1.1/input/*     regtests/ww3_ufs1.1/input/
cp -r data_regtests/ww3_ufs1.1/input/*.nc  regtests/ww3_ufs1.2/input/
cp -r data_regtests/ww3_ufs1.2/input/*     regtests/ww3_ufs1.2/input/
cp -r data_regtests/ww3_ufs1.3/input/*nc   regtests/ww3_ufs1.3/input/
#Do you want to clean up (aka delete tar file, delete the data_regtests directory) 
echo -e "\n\n Do you want to delete the tar file ww3_from_ftp.${ww3ver}.tar.gz [y|n]: "
read wnew
if [ "${wnew}" = "Y" ] || [ "${wnew}" = "y" ]
then
  echo -e '\n Deleting tar file ww3_from_ftp.${ww3ver}.tar.gz'
  rm ww3_from_ftp.${ww3ver}.tar.gz
else
  echo -e ' Not deleting tar file.' 
fi

echo -e "\n\n Files were copied from the data_regtests to the regtests folder."
echo -e "Do you want to delete the data_regtests folder? [y|n]: "
read wnew2
if [ "${wnew2}" = "Y" ] || [ "${wnew2}" = "y" ]
then
  echo -e '\n Deleting the data_regtests folder'
  rm -rf data_regtests
else
  echo -e ' Not deleting the data_regtests folder.' 
fi

#move back to original directory: 
cd $curr_dir
echo -e "End of ww3_from_ftp.sh"
