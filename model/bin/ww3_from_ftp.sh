#!/bin/bash -e
# --------------------------------------------------------------------------- #
#                                                                             #
# Script for downloading data from ftp                                        #
#                                                    Created 0ct 10, 2017     #
# --------------------------------------------------------------------------- #

usage()
{
    echo ''
    echo ' Usage : ./ww3_from_ftp.sh [options]'
    echo ''
    echo ' Options : '
    echo '           -h : print usage'
    echo '           -i : interactive mode'
    echo '           -k : keep tar files'
    echo ''
}

curr_dir=`pwd`

# Set WW3 code version
ww3ver=v7.14.1

interactive='n'
keep='n'
if [ $# -eq 1 ] ; then
  if [ "$1" = "-h" ] ; then
    usage
    exit 0
  elif [ "$1" = "-i" ] ; then
    interactive='y'
  elif [ "$1" = "-k" ] ; then
    keep='y'
  else
    echo '[ERROR] input argument not recognized'
    usage
    exit 1
  fi
elif [ $# -gt 1 ] ; then
  echo '[ERROR] only one input argument accepted'
  usage
  exit 1
fi

dir0=$(cd $(dirname $0) > /dev/null && pwd -P)
ww3dir=$(dirname $(dirname $dir0))

#Get top level directory of ww3 from user:
echo -e "\n\n This script will download data from the ftp for WAVEWATCH III "
if [ "$interactive" = "n" ]
then
  echo $ww3dir
else
echo -e "Enter the absolute or relative path to the main/top directory, "
echo -e "this would be '../../' if in the model/bin directory "
echo -e "or './' if already in the top/main directory:"
  read ww3dir
fi

#Move to top level directory of ww3:
cd $ww3dir

#Download from ftp (if not already present) and uptar:
echo -e "Downloading (or finding) and untaring file from ftp:"
if ! test -f ww3_from_ftp.${ww3ver}.tar.gz; then
  wget --no-check-certificate https://ftp.emc.ncep.noaa.gov/static_files/public/WW3/ww3_from_ftp.${ww3ver}.tar.gz
fi
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
cp -r data_regtests/ww3_tp2.14/input/r-ww3.nc.OASOCM regtests/ww3_tp2.14/input_oasocm/r-ww3.nc
cp -r data_regtests/ww3_tp2.14/input/r-ww3.nc.OASICM regtests/ww3_tp2.14/input_oasicm/r-ww3.nc
cp -r data_regtests/ww3_tp2.14/input/r-ww3.nc.OASACM regtests/ww3_tp2.14/input_oasacm/r-ww3.nc
cp -r data_regtests/ww3_tp2.14/input/r-ww3.nc.OASACM regtests/ww3_tp2.14/input_oasacm2/r-ww3.nc
cp -r data_regtests/ww3_tp2.14/input/density.nc  regtests/ww3_tp2.14/input_oasacm6/
cp -r data_regtests/ww3_tp2.14/input/rmp_* regtests/ww3_tp2.14/input_oasocm/
cp -r data_regtests/ww3_tp2.14/input/rmp_* regtests/ww3_tp2.14/input_oasicm/
cp -r data_regtests/ww3_tp2.14/input/rmp_* regtests/ww3_tp2.14/input_oasacm/
cp -r data_regtests/ww3_tp2.14/input/rmp_* regtests/ww3_tp2.14/input_oasacm2/
cp -r data_regtests/ww3_tp2.14/input/rmp_* regtests/ww3_tp2.14/input_oasacm4/
cp -r data_regtests/ww3_tp2.14/input/rmp_* regtests/ww3_tp2.14/input_oasacm5/
cp -r data_regtests/ww3_tp2.14/input/rmp_* regtests/ww3_tp2.14/input_oasacm6/
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
cp -r data_regtests/ww3_tp2.19/input/*     regtests/ww3_tp2.19/input/
cp -r data_regtests/ww3_tp2.21/input/*     regtests/ww3_tp2.21/input/
cp -r data_regtests/mww3_test_09/input/*   regtests/mww3_test_09/input/
cp -r data_regtests/ww3_ufs1.1/input/*     regtests/ww3_ufs1.1/input/
cp -r data_regtests/ww3_ufs1.1/input_unstr/*     regtests/ww3_ufs1.1/input_unstr/
cp -r data_regtests/ww3_ufs1.1/input/*.nc  regtests/ww3_ufs1.2/input/
cp -r data_regtests/ww3_ufs1.2/input/*     regtests/ww3_ufs1.2/input/
cp -r data_regtests/ww3_ufs1.3/input/*nc   regtests/ww3_ufs1.3/input/
#Do you want to clean up (aka delete tar file, delete the data_regtests directory)
echo -e "\n\n Do you want to delete the tar file ww3_from_ftp.${ww3ver}.tar.gz [y|n]: "
if [ "$interactive" = "n" ]
then
  echo $keep
else
  read keep
fi
if [ "${keep}" = "N" ] || [ "${keep}" = "n" ]
then
  echo -e "\n Deleting tar file ww3_from_ftp.${ww3ver}.tar.gz"
  rm ww3_from_ftp.${ww3ver}.tar.gz
else
  echo -e ' Not deleting tar file.'
fi

echo -e "\n\n Files were copied from the data_regtests to the regtests folder."
echo -e "Do you want to delete the data_regtests folder? [y|n]: "
if [ "$interactive" = "n" ]
then
  echo $keep
else
  read keep
fi
if [ "${keep}" = "N" ] || [ "${keep}" = "n" ]
then
  echo -e '\n Deleting the data_regtests folder'
  rm -rf data_regtests
else
  echo -e ' Not deleting the data_regtests folder.'
fi

#move back to original directory:
cd $curr_dir
echo -e "End of ww3_from_ftp.sh"
