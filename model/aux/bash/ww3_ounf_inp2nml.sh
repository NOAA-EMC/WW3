#!/bin/bash -e

prog="ww3_ounf"

if [ $# -ne 1 ]
then
  echo "  [ERROR] need ${prog} input filename in argument [${prog}.inp]"
  exit 1
fi

inp="$( cd "$( dirname "$1" )" && pwd )/$(basename $1)"

# check filename extension
ext=$(echo $inp | awk -F '.' '{print $NF}')
if [ "$(echo $ext)" != 'inp' ] ; then
  echo "[ERROR] input file has no .inp extension. Please rename it before conversion"  
  exit 1
fi

# commented because it is not working in all cases
# link to temporary inp with regtest format
#ext=$(echo $inp | awk -F"${prog}.inp." '{print $2}' || awk -F"${prog}.inp_" '{print $2}')
#base=$(echo $inp | awk -F"${prog}\\..inp\\.." '{print $1}' | awk -F".inp.$ext" '{print $1}' || awk -F"${prog}\\..inp_" '{print $1}' | awk -F".inp_$ext" '{print $1}')
#if [ ! -z $(echo $ext) ] ; then
# new_inp=${base}_${ext}.inp
# echo "link $inp to $new_inp"
# ln -sfn $inp $new_inp
# old_inp=$inp
# inp=$new_inp
#fi

cd $( dirname $inp)
cur_dir="../$(basename $(dirname $inp))"


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

cleaninp="$cur_dir/${prog}_clean.inp"
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

  echo "$line" >> $cleaninp

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
nf=$(echo ${lines[$il]} | awk -F' ' '{print NF}' )
if [ $nf -gt 1 ]
then
  ix0="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  ixn="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  iy0="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  iyn="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $ix0 $ixn $iy0 $iyn
  smctype=1
  sx0=-999.9
  sy0=-999.9
  ex0=-999.9
  ey0=-999.9
  celfac=1
  noval=-999.9
else
  smctype="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $smctype
  il=$(($il+1))
  sx0="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  sy0="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  ex0="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  ey0="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
  celfac=1
  echo $sx0 $sy0 $ex0 $e0
  if [ $smctype -eq 2 ]
  then
    celfac="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
    echo $celfac
  fi
  noval=-999.9
  echo $noval
  ix0=1
  ixn=1000000
  iy0=1
  iyn=1000000
fi


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
! * the detailed list of field names FIELD%LIST is given in ww3_shel.nml
!  DPT CUR WND AST WLV ICE IBG D50 IC1 IC5
!  HS LM T02 T0M1 T01 FP DIR SPR DP HIG
!  EF TH1M STH1M TH2M STH2M WN
!  PHS PTP PLP PDIR PSPR PWS PDP PQP PPE PGW PSW PTM10 PT01 PT02 PEP TWS PNR
!  UST CHA CGE FAW TAW TWA WCC WCF WCH WCM FWS
!  SXY TWO BHD FOC TUS USS P2S USF P2L TWI FIC
!  ABR UBR BED FBB TBB
!  MSS MSC WL02 AXT AYT AXY
!  DTD FC CFX CFD CFK
!  U1 U2
!
! * namelist must be terminated with /
! * definitions & defaults:
!     FIELD%TIMESTART            = '19000101 000000'  ! Stop date for the output field
!     FIELD%TIMESTRIDE           = '0'                ! Time stride for the output field
!     FIELD%TIMESTOP             = '29001231 000000'  ! Stop date for the output field
!     FIELD%TIMECOUNT            = '1000000000'       ! Number of time steps
!     FIELD%TIMESPLIT            = 6                  ! [0(nodate),4(yearly),6(monthly),8(daily),10(hourly)]
!     FIELD%LIST                 = 'unset'            ! List of output fields
!     FIELD%PARTITION            = '0 1 2 3'          ! List of wave partitions ['0 1 2 3 4 5']
!     FIELD%SAMEFILE             = T                  ! All the variables in the same file [T|F]
!     FIELD%TYPE                 = 3                  ! [2 = SHORT, 3 = it depends , 4 = REAL]
! -------------------------------------------------------------------- !
&FIELD_NML
EOF

if [ "${timestart[*]}" != "19000101 000000" ];  then  echo "  FIELD%TIMESTART        =  '${timestart[@]}'" >> $nmlfile; fi
if [ "$timestride" != "0" ];                    then  echo "  FIELD%TIMESTRIDE       =  '$timestride'" >> $nmlfile; fi
if [ $timecount -lt 10000 ];                    then  echo "  FIELD%TIMECOUNT        =  '$timecount'" >> $nmlfile; fi
if [ "$timesplit" != 6 ];                       then  echo "  FIELD%TIMESPLIT        =  $timesplit" >> $nmlfile; fi
if [ "$timestride" != "0" ];                    then  echo "  FIELD%LIST             =  '$list'" >> $nmlfile; fi
if [ "$partition" != "0 1 2 3" ];               then  echo "  FIELD%PARTITION        =  '$partition'" >> $nmlfile; fi
if [ "$samefile" != T ];                        then  echo "  FIELD%SAMEFILE         =  $samefile" >> $nmlfile; fi
if [ "$type" != 3 ];                            then  echo "  FIELD%TYPE             =  $type" >> $nmlfile; fi


# file namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the content of the output file via FILE_NML namelist
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

if [ "$prefix" != "ww3." ];                            then  echo "  FILE%PREFIX        = '$prefix'" >> $nmlfile; fi
if [ "$netcdf" != 3 ];                                 then  echo "  FILE%NETCDF        = $netcdf" >> $nmlfile; fi
if [ "$ix0" != 1 ];                                    then  echo "  FILE%IX0           = $ix0" >> $nmlfile; fi
if [ "$ixn" != 1000000000 ] && [ "$ixn" != 1000000 ];  then  echo "  FILE%IXN           = $ixn" >> $nmlfile; fi
if [ "$iy0" != 1 ];                                    then  echo "  FILE%IY0           = $iy0" >> $nmlfile; fi
if [ "$iyn" != 1000000000 ] && [ "$iyn" != 1000000 ];  then  echo "  FILE%IYN           = $iyn" >> $nmlfile; fi


# smc grid
if [ $nf -le 1 ]
then

cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the content of the output file via SMC_NML namelist
!
! * For SMC grids, IX0, IXN, IY0 and IYN from FILE_NML are not used.
!   Two types of output are available: 
! *   TYPE=1: Flat 1D "seapoint" array of grid cells.
! *   TYPE=2: Re-gridded regular grid with cell sizes being an integer
! *           multiple of the smallest SMC grid cells size.
!
! * Note that the first/last longitudes and latitudes will be adjusted
!  to snap to the underlying SMC grid edges. CELFAC is only used for
!  type 2 output and defines the output cell sizes as an integer
!  multiple of the smallest SMC Grid cell size. CELFAC should be a
!  power of 2, e.g: 1,2,4,8,16, etc...
!
! * namelist must be terminated with /
! * definitions & defaults:
!     SMC%TYPE          = 1              ! SMC Grid type (1 or 2)
!     SMC%SXO           = -999.9         ! First longitude
!     SMC%EXO           = -999.9         ! Last longitude
!     SMC%SYO           = -999.9         ! First latitude
!     SMC%EYO           = -999.9         ! Last latitude
!     SMC%CELFAC        = 1              ! Cell size factor (SMCTYPE=2 only)
!     SMC%NOVAL         = UNDEF          ! Fill value for wet cells with no data
! -------------------------------------------------------------------- !
&SMC_NML
EOF

if [ "$smctype" != 1 ];    then  echo "  SMC%TYPE        = $smctype" >> $nmlfile; fi
if [ "$sx0" != -999.9 ];   then  echo "  SMC%SX0         = $sx0" >> $nmlfile; fi
if [ "$sy0" != -999.9 ];   then  echo "  SMC%SY0         = $sy0" >> $nmlfile; fi
if [ "$ex0" != -999.9 ];   then  echo "  SMC%EX0         = $ex0" >> $nmlfile; fi
if [ "$ey0" != -999.9 ];   then  echo "  SMC%EY0         = $ey0" >> $nmlfile; fi
if [ "$celfac" != 1 ];     then  echo "  SMC%CELFAC      = $celfac" >> $nmlfile; fi
if [ "$noval" != -999.9 ]; then  echo "  SMC%NOVAL       = $noval" >> $nmlfile; fi

fi

cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! WAVEWATCH III - end of namelist                                      !
! -------------------------------------------------------------------- !
EOF
echo "DONE : $( cd "$( dirname "$nmlfile" )" && pwd )/$(basename $nmlfile)"
rm -f $cleaninp

# commented because it is not working in all cases
#if [ ! -z $(echo $ext) ] ; then
#  unlink $new_inp
#  addon="$(echo $(basename $nmlfile) | awk -F"${prog}_" '{print $2}' | awk -F'.nml' '{print $1}'  )"
#  new_nmlfile="${prog}.nml.$addon"
#  mv $( cd "$( dirname "$nmlfile" )" && pwd )/$(basename $nmlfile) $( cd "$( dirname "$nmlfile" )" && pwd )/$(basename $new_nmlfile)
#  echo "RENAMED  : $( cd "$( dirname "$nmlfile" )" && pwd )/$(basename $new_nmlfile)"
#fi
#------------------------------


