#!/bin/bash -e

prog="ww3_prnc"

if [ $# -ne 2 ]
then
  echo '[ERROR] need 2 arguments : '
  echo "\$1 : ${prog} input filename in argument [${prog}.inp]"
  echo '$2 : include header or full comments [header|full]' 
  exit 1
fi

inp="$( cd "$( dirname "$1" )" && pwd )/$(basename $1)"
comment="$2"

# check filename extension
ext=$(echo $inp | awk -F '.' '{print $NF}')
if [ "$(echo $ext)" != 'inp' ] ; then
  echo "[ERROR] input file has no .inp extension. Please rename it before conversion"  
  exit 1
fi

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


declare -A var

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

  cleanline="$(echo $line | awk -F' ' '{print $1}' | sed -e "s/\*//g" -e "s/\"//g" -e "s/\'//g")"  
  if [ -z "$cleanline" ]
  then
    continue
  fi

  echo "$line" >> $cleaninp

done



#------------------------------
# get all values from clean inp file

readarray -t lines < "$cleaninp"
numlines=${#lines[@]}

# remove carriage return characters
for il in $(seq 0 $numlines)
do
  lines[$il]=$(echo "$(echo ${lines[$il]} | sed 's/\r$//')")
done
il=0

field="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $field
format="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo $format
tflag="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
echo $tflag
hflag="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
echo $hflag

if [ "$format" = "AT" ] 
then
  il=$(($il+1))
  tide="$(echo ${lines[$il]})"
  echo $tide
fi

il=$(($il+1))
lon="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $lon
lat="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo $lat

il=$(($il+1))
var[1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
i=1
while [ ! -z "$(echo ${var[$i]})" ] && [ "${var[$i]}" != "${var[$(($i - 1))]}" ]
do
  i=$(($i + 1))
  var[$i]="$(echo ${lines[$il]} | cut -d ' ' -f$i | cut -d \" -f2  | cut -d \' -f2)"
done
if [ $i -gt 1 ]
then
  i=$(($i - 1))
fi
for it in $(seq 1 $i)
do
  echo ${var[$it]}
done

if [ "$tflag" = F ]
then
  il=$(($il+1))
  timestart[1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  timestart[2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  echo ${timestart[@]}
fi

il=$(($il+1))
filename="$(echo ${lines[$il]})"
echo $filename


#------------------------------
# write value in a new nml file

nmlfile=$cur_dir/$(basename $inp .inp).nml

# header
cat > $nmlfile << EOF
! -------------------------------------------------------------------- !
! WAVEWATCH III ww3_prnc.nml - Field preprocessor                      !
! -------------------------------------------------------------------- !


EOF

# forcing namelist
if [ "$comment" = "full" ]; then
cat >> $nmlfile << EOF
! -------------------------------------------------------------------- !
! Define the forcing fields to preprocess via FORCING_NML namelist
!
! * only one FORCING%FIELD can be set at true
! * only one FORCING%grid can be set at true
! * tidal constituents FORCING%tidal is only available on grid%asis with FIELD%level or FIELD%current
!
! * namelist must be terminated with /
! * definitions & defaults:
!     FORCING%TIMESTART            = '19000101 000000'  ! Start date for the forcing field
!     FORCING%TIMESTOP             = '29001231 000000'  ! Stop date for the forcing field
!
!     FORCING%FIELD%ICE_PARAM1     = F           ! Ice thickness                      (1-component)
!     FORCING%FIELD%ICE_PARAM2     = F           ! Ice viscosity                      (1-component)
!     FORCING%FIELD%ICE_PARAM3     = F           ! Ice density                        (1-component)
!     FORCING%FIELD%ICE_PARAM4     = F           ! Ice modulus                        (1-component)
!     FORCING%FIELD%ICE_PARAM5     = F           ! Ice floe mean diameter             (1-component)
!     FORCING%FIELD%MUD_DENSITY    = F           ! Mud density                        (1-component)
!     FORCING%FIELD%MUD_THICKNESS  = F           ! Mud thickness                      (1-component)
!     FORCING%FIELD%MUD_VISCOSITY  = F           ! Mud viscosity                      (1-component)
!     FORCING%FIELD%WATER_LEVELS   = F           ! Level                              (1-component)
!     FORCING%FIELD%CURRENTS       = F           ! Current                            (2-components)
!     FORCING%FIELD%WINDS          = F           ! Wind                               (2-components)
!     FORCING%FIELD%WIND_AST       = F           ! Wind and air-sea temp. dif.        (3-components)
!     INPUT%FORCING%ATM_MOMENTUM   = f           ! Atmospheric momentum               (2-components)
!     INPUT%FORCING%AIR_DENSITY    = f           ! Air density                        (1-component)
!     FORCING%FIELD%ICE_CONC       = F           ! Ice concentration                  (1-component)
!     FORCING%FIELD%ICE_BERG       = F           ! Icebergs and sea ice concentration (2-components)
!     FORCING%FIELD%DATA_ASSIM     = F           ! Data for assimilation              (1-component)
!
!     FORCING%GRID%ASIS            = F           ! Transfert field 'as is' on the model grid
!     FORCING%GRID%LATLON          = F           ! Define field on regular lat/lon or cartesian grid
!
!     FORCING%TIDAL                = 'unset'     ! Set the tidal constituents [FAST | VFAST | 'M2 S2 N2']
! -------------------------------------------------------------------- !
&FORCING_NML
EOF
else
cat >> $nmlfile << EOF
! -------------------------------------------------------------------- !
! Define the forcing fields to preprocess via FORCING_NML namelist
! -------------------------------------------------------------------- !
&FORCING_NML
EOF
fi

if [ "$field" = "IC1" ]; then echo "  FORCING%FIELD%ICE_PARAM1     = T" >> $nmlfile; fi
if [ "$field" = "IC2" ]; then echo "  FORCING%FIELD%ICE_PARAM2     = T" >> $nmlfile; fi
if [ "$field" = "IC3" ]; then echo "  FORCING%FIELD%ICE_PARAM3     = T" >> $nmlfile; fi
if [ "$field" = "IC4" ]; then echo "  FORCING%FIELD%ICE_PARAM4     = T" >> $nmlfile; fi
if [ "$field" = "IC5" ]; then echo "  FORCING%FIELD%ICE_PARAM5     = T" >> $nmlfile; fi
if [ "$field" = "MDN" ]; then echo "  FORCING%FIELD%MUD_DENSITY    = T" >> $nmlfile; fi
if [ "$field" = "MTH" ]; then echo "  FORCING%FIELD%MUD_THICKNESS  = T" >> $nmlfile; fi
if [ "$field" = "MVS" ]; then echo "  FORCING%FIELD%MUD_VISCOSITY  = T" >> $nmlfile; fi
if [ "$field" = "LEV" ]; then echo "  FORCING%FIELD%WATER_LEVELS   = T" >> $nmlfile; fi
if [ "$field" = "CUR" ]; then echo "  FORCING%FIELD%CURRENTS       = T" >> $nmlfile; fi
if [ "$field" = "WND" ]; then echo "  FORCING%FIELD%WINDS          = T" >> $nmlfile; fi
if [ "$field" = "WNS" ]; then echo "  FORCING%FIELD%WIND_AST       = T" >> $nmlfile; fi
if [ "$field" = "TAU" ]; then echo "  FORCING%FIELD%ATM_MOMENTUM   = T" >> $nmlfile; fi
if [ "$field" = "RHO" ]; then echo "  FORCING%FIELD%AIR_DENSITY    = T" >> $nmlfile; fi
if [ "$field" = "ICE" ]; then echo "  FORCING%FIELD%ICE_CONC       = T" >> $nmlfile; fi
if [ "$field" = "ISI" ]; then echo "  FORCING%FIELD%ICE_BERG       = T" >> $nmlfile; fi
if [ "$field" = "DAT" ]; then echo "  FORCING%FIELD%DATA_ASSIM     = T" >> $nmlfile; fi

if [ "$format" = "AI" ]; then echo "  FORCING%GRID%ASIS            = T" >> $nmlfile; fi
if [ "$format" = "LL" ]; then echo "  FORCING%GRID%LATLON          = T" >> $nmlfile; fi

if [ "$format" = "AT" ]
then
  if [ "$format" = "AT" ]; then echo "  FORCING%GRID%ASIS            = T" >> $nmlfile; fi
  if [ "$field" = "CUR" ] || [ "$field" = "LEV" ] ; then echo "  FORCING%TIDAL                = '$tide'" >> $nmlfile; fi
fi





# file namelist
if [ "$comment" = "full" ]; then
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the content of the input file via FILE_NML namelist
!
! * input file must respect netCDF format and CF conventions
! * input file must contain :
!      -dimension : time, name expected to be called time
!      -dimension : longitude/latitude, names can defined in the namelist
!      -variable : time defined along time dimension
!      -attribute : time with attributes units written as ISO8601 convention
!      -attribute : time with attributes calendar set to standard as CF convention
!      -variable : longitude defined along longitude dimension
!      -variable : latitude defined along latitude dimension
!      -variable : field defined along time,latitude,longitude dimensions
! * FILE%VAR(I) must be set for each field component
!
! * namelist must be terminated with /
! * definitions & defaults:
!     FILE%FILENAME      = 'unset'           ! relative path input file name
!     FILE%LONGITUDE     = 'unset'           ! longitude/x dimension name
!     FILE%LATITUDE      = 'unset'           ! latitude/y dimension name
!     FILE%VAR(I)        = 'unset'           ! field component
!     FILE%TIMESHIFT     = '00000000 000000' ! shift the time value to 'YYYYMMDD HHMMSS'
! -------------------------------------------------------------------- !
&FILE_NML
EOF
else
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the content of the input file via FILE_NML namelist
! -------------------------------------------------------------------- !
&FILE_NML
EOF
fi

if [ "$filename" != "unset" ];  then  echo "  FILE%FILENAME      = $filename" >> $nmlfile; fi
if [ "$lon" != "unset" ];  then  echo "  FILE%LONGITUDE     = '$lon'" >> $nmlfile; fi
if [ "$lat" != "unset" ];  then  echo "  FILE%LATITUDE      = '$lat'" >> $nmlfile; fi

for it in $(seq 1 $i)
do
  echo "  FILE%VAR($it)        = '${var[$it]}'" >> $nmlfile
done


cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! WAVEWATCH III - end of namelist                                      !
! -------------------------------------------------------------------- !
EOF
echo "DONE : $( cd "$( dirname "$nmlfile" )" && pwd )/$(basename $nmlfile)"
rm -f $cleaninp

#------------------------------


