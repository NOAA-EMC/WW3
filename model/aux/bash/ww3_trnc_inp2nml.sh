#!/bin/bash -e


if [ $# -ne 1 ]
then
  echo '  [ERROR] need ww3_trnc input filename in argument [ww3_trnc.inp]'
  exit 1
fi
inp=$1
cur_dir=$(dirname $1)


version=$(bash --version | awk -F' ' '{print $4}')
version4=$(echo $version | cut -d '.' -f1)
echo "bash version : " $version4
if [ "$version4" != "4" ]
then
  echo '  [ERROR] need a version of bash at least 4'
  exit 1
fi


#------------------------------
# clean up inp file from all $ lines

cleaninp="$cur_dir/ww3_trnc_clean.inp"
rm -f $cleaninp

cat $inp | while read line
do

  if [ "$(echo $line | cut -c1)" = "$" ]
  then
    continue
  fi

  cleanline="$(echo $line | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"  
  if [ -z "$cleanline" ]
  then
    continue
  fi

  echo $line >> $cleaninp

done

#------------------------------
# get all values from clean inp file


readarray -t lines < "$cleaninp"
il=0

timestart[1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
timestart[2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo ${timestart[@]}
timestride="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
echo $timestride
timecount="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
echo $timecount

il=$(($il+1))
netcdf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $netcdf

il=$(($il+1))
prefix="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $prefix

il=$(($il+1))
timesplit="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $timesplit


#------------------------------
# write value in a new nml file

nmlfile=$cur_dir/$(basename $inp .inp).nml

# header
cat > $nmlfile << EOF
! -------------------------------------------------------------------- !
! WAVEWATCH III ww3_trnc.nml - Track output post-processing            !
! -------------------------------------------------------------------- !

EOF

# field namelist
cat >> $nmlfile << EOF
! -------------------------------------------------------------------- !
! Define the output fields to postprocess via TRACK_NML namelist
!
! * namelist must be terminated with /
! * definitions & defaults:
!     TRACK%TIMESTART            = '19000101 000000'  ! Stop date for the output field
!     TRACK%TIMESTRIDE           = '0'                ! Time stride for the output field
!     TRACK%TIMECOUNT            = '1000000000'       ! Number of time steps
!     TRACK%TIMESPLIT            = 6                  ! [4(yearly),6(monthly),8(daily),10(hourly)]
! -------------------------------------------------------------------- !
&TRACK_NML
EOF

if [ "${timestart[*]}" != "19000101 000000" ];  then  echo "  TRACK%TIMESTART        =  '${timestart[@]}'" >> $nmlfile; fi
if [ "$timestride" != "0" ];  then  echo "  TRACK%TIMESTRIDE       =  '$timestride'" >> $nmlfile; fi
if [ $timecount -lt 10000 ];  then  echo "  TRACK%TIMECOUNT        =  '$timecount'" >> $nmlfile; fi
if [ "$timesplit" != 6 ];  then  echo "  TRACK%TIMESPLIT        =  $timesplit" >> $nmlfile; fi


# file namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the content of the input file via FILE_NML namelist
!
! * namelist must be terminated with /
! * definitions & defaults:
!     FILE%PREFIX        = 'ww3.'            ! Prefix for output file name
!     FILE%NETCDF        = 3                 ! Netcdf version [3|4]
! -------------------------------------------------------------------- !
&FILE_NML
EOF

if [ "$prefix" != "ww3." ];  then  echo "  FILE%PREFIX        = '$prefix'" >> $nmlfile; fi
if [ "$netcdf" != 3 ];  then  echo "  FILE%NETCDF        = $netcdf" >> $nmlfile; fi



cat >> $nmlfile << EOF
/


! -------------------------------------------------------------------- !
! WAVEWATCH III - end of namelist                                      !
! -------------------------------------------------------------------- !
EOF

rm -f $cleaninp
#------------------------------




