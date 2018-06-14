#!/bin/bash -e


if [ $# -ne 1 ]
then
  echo '  [ERROR] need ww3_ounp input filename in argument [ww3_ounp.inp]'
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

cleaninp="$cur_dir/ww3_ounp_clean.inp"
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

# points
il=$(($il+1))
list="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $list
pointlist=''
while [ "$list" != "-1" ]
do
  pointlist="$pointlist $list"
  il=$(($il+1))
  list="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $list
done
if [ "$pointlist" = '' ]
then
  pointlist='all'
fi
pointlist="$(echo $pointlist | cut -d \" -f2  | cut -d \' -f2)"

il=$(($il+1))
prefix="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $prefix

il=$(($il+1))
timesplit="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $timesplit

il=$(($il+1))
netcdf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $netcdf

il=$(($il+1))
samefile="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $samefile
buffer="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo $buffer

il=$(($il+1))
type="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $type

il=$(($il+1))
globatt="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $globatt

il=$(($il+1))
dimorder="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $dimorder


if [ "$type" = 1 ]
then
  il=$(($il+1))
  output="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $output
  scale_fac="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $scale_fac
  output_fac="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $output_fac
elif [ "$type" = 2 ]
then
  il=$(($il+1))
  output="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $output
elif [ "$type" = 3 ]
then
  il=$(($il+1))
  output="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $output
  scale_fac="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $scale_fac
  output_fac="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $output_fac
  spectrum="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $spectrum
  input="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $input
  interactions="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $interactions
  dissipation="$(echo ${lines[$il]} | awk -F' ' '{print $7}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $dissipation
  bottom="$(echo ${lines[$il]} | awk -F' ' '{print $8}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $bottom
  ice="$(echo ${lines[$il]} | awk -F' ' '{print $9}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $ice
  total="$(echo ${lines[$il]} | awk -F' ' '{print $10}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $total
  table_fac="$(echo ${lines[$il]} | awk -F' ' '{print $11}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $table_fac
  if [ "$table_fac" = '' ]
  then
    echo "[ERROR] need to update source term flags before converting into namelist"
    echo "        example : 4  0  0  T T T T T T T  0"
    exit 1
  fi
fi




#------------------------------
# write value in a new nml file

nmlfile=$cur_dir/$(basename $inp .inp).nml

# header
cat > $nmlfile << EOF
! -------------------------------------------------------------------- !
! WAVEWATCH III ww3_ounp.nml - Point output post-processing            !
! -------------------------------------------------------------------- !

EOF

# field namelist
cat >> $nmlfile << EOF
! -------------------------------------------------------------------- !
! Define the output fields to postprocess via POINT_NML namelist
!
!
! * namelist must be terminated with /
! * definitions & defaults:
!     POINT%TIMESTART            = '19000101 000000'  ! Stop date for the output field
!     POINT%TIMESTRIDE           = '0'                ! Time stride for the output field
!     POINT%TIMECOUNT            = '1000000000'       ! Number of time steps
!     POINT%TIMESPLIT            = 6                  ! [4(yearly),6(monthly),8(daily),10(hourly)]
!     POINT%LIST                 = 'all'              ! List of points index ['all'|'1 2 3']
!     POINT%SAMEFILE             = T                  ! All the points in the same file
!     POINT%BUFFER               = 150                ! Number of points to process per pass
!     POINT%TYPE                 = 1                  ! [0=inventory | 1=spectra | 2=mean param | 3=source terms]
!     POINT%DIMORDER             = T                  ! [time,station=T | station,time=F]
! -------------------------------------------------------------------- !
&POINT_NML
EOF

if [ "${timestart[*]}" != "19000101 000000" ];  then  echo "  POINT%TIMESTART        =  '${timestart[@]}'" >> $nmlfile; fi
if [ "$timestride" != "0" ];  then  echo "  POINT%TIMESTRIDE       =  '$timestride'" >> $nmlfile; fi
if [ $timecount -lt 10000 ];  then  echo "  POINT%TIMECOUNT        =  '$timecount'" >> $nmlfile; fi
if [ "$timesplit" != 6 ];  then  echo "  POINT%TIMESPLIT        =  $timesplit" >> $nmlfile; fi
if [ "$pointlist" != "all" ];  then  echo "  POINT%LIST             =  '$pointlist'" >> $nmlfile; fi
if [ "$samefile" != T ];  then  echo "  POINT%SAMEFILE         =  $samefile" >> $nmlfile; fi
if [ "$buffer" != 150 ];  then  echo "  POINT%BUFFER           =  $buffer" >> $nmlfile; fi
if [ "$type" != 1 ];  then  echo "  POINT%TYPE             =  $type" >> $nmlfile; fi
if [ "$dimorder" != T ];  then  echo "  POINT%DIMORDER         =  $dimorder" >> $nmlfile; fi


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
! -------------------------------------------------------------------- !
&FILE_NML
EOF

if [ "$prefix" != "ww3." ];  then  echo "  FILE%PREFIX        = '$prefix'" >> $nmlfile; fi
if [ "$netcdf" != 3 ];  then  echo "  FILE%NETCDF        = $netcdf" >> $nmlfile; fi




# inventory namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the type 0, inventory of file
!
! * namelist must be terminated with /
! * definitions & defaults:
!            No additional input, the above time range is ignored.
! -------------------------------------------------------------------- !


EOF

# spectra namelist
cat >> $nmlfile << EOF
! -------------------------------------------------------------------- !
! Define the type 1, spectra via SPECTRA_NML namelist
!
! Transfert file content :
!   - time, station id, station name, longitude, latitude 
!   - frequency : unit Hz, center band frequency - linear log scale (XFR factor)
!   - frequency1 : unit Hz, lower band frequency
!   - frequency2 : unit Hz, upper band frequency
!   - direction : unit degree, convention to, origin East, trigonometric order
!   - efth(time,station,frequency,direction) : 2D spectral density
!   - dpt, wnd, wnddir, cur, curdir : mean parameters
!
!
! * namelist must be terminated with /
! * definitions & defaults:
!     SPECTRA%OUTPUT        = 3            ! 1: Print plots
!                                          ! 2: Table of 1-D spectra
!                                          ! 3: Transfer file
!                                          ! 4: Spectral partitioning
!     SPECTRA%SCALE_FAC     = 1            ! Scale factor (-1=disabled)
!     SPECTRA%OUTPUT_FAC    = 0            ! Output factor (0=normalized)
! -------------------------------------------------------------------- !
&SPECTRA_NML
EOF

if [ "$type" = 1 ]
then
  if [ "$output" != 3 ];  then  echo "  SPECTRA%OUTPUT        = $output" >> $nmlfile; fi
  if [ "$scale_fac" != 1 ];  then  echo "  SPECTRA%SCALE_FAC     = $scale_fac" >> $nmlfile; fi
  if [ "$output_fac" != 0 ];  then  echo "  SPECTRA%OUTPUT_FAC    = $output_fac" >> $nmlfile; fi
fi


# param namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the type 2, mean parameter via PARAM_NML namelist
!
!
! * namelist must be terminated with /
! * definitions & defaults:
!     PARAM%OUTPUT      = 4                ! 1: Depth, current, wind
!                                          ! 2: Mean wave parameters
!                                          ! 3: Nondimensional pars. (U*)
!                                          ! 4: Nondimensional pars. (U10)
!                                          ! 5: Validation table
!                                          ! 6: WMO standard output
! -------------------------------------------------------------------- !
&PARAM_NML
EOF

if [ "$type" = 2 ]
then
  if [ "$output" != 4 ];  then  echo "  PARAM%OUTPUT        = $output" >> $nmlfile; fi
fi


# source namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the type 3, source terms via SOURCE_NML namelist
!
! Transfert file content :
!   - time, station id, station name, longitude, latitude 
!   - frequency : unit Hz, center band frequency - linear log scale (XFR factor)
!   - frequency1 : unit Hz, lower band frequency
!   - frequency2 : unit Hz, upper band frequency
!   - direction : unit degree, convention to, origin East, trigonometric order
!   - efth(frequency,direction) : 2D spectral density
!   - Sin(frequency,direction)  : input source term
!   - Snl(frequency,direction)  : non linear interactions source term
!   - Sds(frequency,direction)  : dissipation source term
!   - Sbt(frequency,direction)  : bottom source term
!   - Sice(frequency,direction) : ice source term
!   - Stot(frequency,direction) : total source term
!
! * namelist must be terminated with /
! * definitions & defaults:
!     SOURCE%OUTPUT         = 4            ! 1: Print plots
!                                          ! 2: Table of 1-D S(f)
!                                          ! 3: Table of 1-D inverse time scales (1/T = S/F)
!                                          ! 4: Transfer file
!     SOURCE%SCALE_FAC     = 0             ! Scale factor (-1=disabled)
!     SOURCE%OUTPUT_FAC    = 0             ! Output factor (0=normalized)
!     SOURCE%TABLE_FAC     = 0             ! Table factor 
!                                                          0 : Dimensional.
!                                                          1 : Nondimensional in terms of U10
!                                                          2 : Nondimensional in terms of U*
!                                                          3-5: like 0-2 with f normalized with fp.
!     SOURCE%SPECTRUM      = T             ! [T|F]
!     SOURCE%INPUT         = T             ! [T|F]
!     SOURCE%INTERACTIONS  = T             ! [T|F]
!     SOURCE%DISSIPATION   = T             ! [T|F]
!     SOURCE%BOTTOM        = T             ! [T|F]
!     SOURCE%ICE           = T             ! [T|F]
!     SOURCE%TOTAL         = T             ! [T|F]
! -------------------------------------------------------------------- !
&SOURCE_NML
EOF

if [ "$type" = 3 ]
then
  if [ "$output" != 4 ];  then          echo "  SOURCE%OUTPUT        = $output" >> $nmlfile; fi
  if [ "$scale_fac" != 0 ];  then       echo "  SOURCE%SCALE_FAC     = $scale_fac" >> $nmlfile; fi
  if [ "$output_fac" != 0 ];  then      echo "  SOURCE%OUTPUT_FAC    = $output_fac" >> $nmlfile; fi
  if [ "$table_fac" != 0 ];  then       echo "  SOURCE%TABLE_FAC     = $table_fac" >> $nmlfile; fi
  if [ "$spectrum" != T ];  then        echo "  SOURCE%SPECTRUM      = $spectrum" >> $nmlfile; fi
  if [ "$input" != T ];  then           echo "  SOURCE%INPUT         = $input" >> $nmlfile; fi
  if [ "$interactions" != T ];  then    echo "  SOURCE%INTERACTIONS  = $interactions" >> $nmlfile; fi
  if [ "$dissipation" != T ];  then     echo "  SOURCE%DISSIPATION   = $dissipation" >> $nmlfile; fi
  if [ "$bottom" != T ];  then          echo "  SOURCE%BOTTOM        = $bottom" >> $nmlfile; fi
  if [ "$ice" != T ];  then             echo "  SOURCE%ICE           = $ice" >> $nmlfile; fi
  if [ "$total" != T ];  then           echo "  SOURCE%TOTAL         = $total" >> $nmlfile; fi
fi




cat >> $nmlfile << EOF
/


! -------------------------------------------------------------------- !
! WAVEWATCH III - end of namelist                                      !
! -------------------------------------------------------------------- !
EOF

rm -f $cleaninp
#------------------------------




