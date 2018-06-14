#!/bin/bash -e


if [ $# -ne 1 ]
then
  echo '  [ERROR] need ww3_ounf input filename in argument [ww3_ounf.inp]'
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

cleaninp="$cur_dir/ww3_ounf_clean.inp"
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

# fields
il=$(($il+1))
namelist="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $namelist
if [ "$namelist" != "N" ]
then
  echo '[ERROR] not in namelist format'
  echo 'please update your inp file first'
  exit 1
fi
il=$(($il+1))
list="$(echo ${lines[$il]} | cut -d \" -f2  | cut -d \' -f2)"
echo $list

il=$(($il+1))
netcdf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $netcdf
type="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo $type

il=$(($il+1))
partition="$(echo ${lines[$il]} | cut -d \" -f2  | cut -d \' -f2)"
echo $partition

il=$(($il+1))
samefile="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $samefile

il=$(($il+1))
prefix="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $prefix

il=$(($il+1))
timesplit="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $timesplit

il=$(($il+1))
ix0="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
ixn="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
iy0="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
iyn="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
echo $ix0 $ixn $iy0 $iyn


#------------------------------
# write value in a new nml file

nmlfile=$cur_dir/$(basename $inp .inp).nml

# header
cat > $nmlfile << EOF
! -------------------------------------------------------------------- !
! WAVEWATCH III ww3_ounf.nml - Grid output post-processing             !
! -------------------------------------------------------------------- !

EOF

# field namelist
cat >> $nmlfile << EOF
! -------------------------------------------------------------------- !
! Define the output fields to postprocess via FIELD_NML namelist
!
! * the full list of field names FIELD%LIST is : 
!  DPT CUR WND AST WLV ICE IBG D50 IC1 IC5 HS LM T02 T0M1 T01 FP DIR SPR
!  DP HIG EF TH1M STH1M TH2M STH2M WN PHS PTP PLP PDIR PSPR PWS TWS PNR
!  UST CHA CGE FAW TAW TWA WCC WCF WCH WCM SXY TWO BHD FOC TUS USS P2S
!  USF P2L TWI FIC ABR UBR BED FBB TBB MSS MSC DTD FC CFX CFD CFK U1 U2 
!
! * namelist must be terminated with /
! * definitions & defaults:
!     FIELD%TIMESTART            = '19000101 000000'  ! Stop date for the output field
!     FIELD%TIMESTRIDE           = '0'                ! Time stride for the output field
!     FIELD%TIMESTOP             = '29001231 000000'  ! Stop date for the output field
!     FIELD%TIMECOUNT            = '1000000000'       ! Number of time steps
!     FIELD%TIMESPLIT            = 6                  ! [4(yearly),6(monthly),8(daily),10(hourly)]
!     FIELD%LIST                 = 'unset'            ! List of output fields
!     FIELD%PARTITION            = '0 1 2 3'          ! List of wave partitions ['0 1 2 3 4 5']
!     FIELD%SAMEFILE             = T                  ! All the variables in the same file [T|F]
!     FIELD%TYPE                 = 3                  ! [2 = SHORT, 3 = it depends , 4 = REAL]
! -------------------------------------------------------------------- !
&FIELD_NML
EOF

if [ "${timestart[*]}" != "19000101 000000" ];  then  echo "  FIELD%TIMESTART        =  '${timestart[@]}'" >> $nmlfile; fi
if [ "$timestride" != "0" ];  then  echo "  FIELD%TIMESTRIDE       =  '$timestride'" >> $nmlfile; fi
if [ $timecount -lt 10000 ];  then  echo "  FIELD%TIMECOUNT        =  '$timecount'" >> $nmlfile; fi
if [ "$timesplit" != 6 ];  then  echo "  FIELD%TIMESPLIT        =  $timesplit" >> $nmlfile; fi
if [ "$timestride" != "0" ];  then  echo "  FIELD%LIST             =  '$list'" >> $nmlfile; fi
if [ "$partition" != "0 1 2 3" ];  then  echo "  FIELD%PARTITION        =  '$partition'" >> $nmlfile; fi
if [ "$samefile" != T ];  then  echo "  FIELD%SAMEFILE         =  $samefile" >> $nmlfile; fi
if [ "$type" != 3 ];  then  echo "  FIELD%TYPE             =  $type" >> $nmlfile; fi


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
!     FILE%IX0           = 1                 ! First X-axis or node index
!     FILE%IXN           = 1000000000        ! Last X-axis or node index
!     FILE%IY0           = 1                 ! First Y-axis index
!     FILE%IYN           = 1000000000        ! Last Y-axis index
! -------------------------------------------------------------------- !
&FILE_NML
EOF

if [ "$prefix" != "ww3." ];  then  echo "  FILE%PREFIX        = '$prefix'" >> $nmlfile; fi
if [ "$netcdf" != 3 ];  then  echo "  FILE%NETCDF        = $netcdf" >> $nmlfile; fi
if [ "$ix0" != 1 ];  then  echo "  FILE%IX0           = $ix0" >> $nmlfile; fi
if [ "$ixn" != 1000000000 ] && [ "$ixn" != 1000000 ];  then  echo "  FILE%IXN           = $ixn" >> $nmlfile; fi
if [ "$iy0" != 1 ];  then  echo "  FILE%IY0           = $iy0" >> $nmlfile; fi
if [ "$iyn" != 1000000000 ] && [ "$iyn" != 1000000 ];  then  echo "  FILE%IYN           = $iyn" >> $nmlfile; fi


cat >> $nmlfile << EOF
/


! -------------------------------------------------------------------- !
! WAVEWATCH III - end of namelist                                      !
! -------------------------------------------------------------------- !
EOF

rm -f $cleaninp
#------------------------------




