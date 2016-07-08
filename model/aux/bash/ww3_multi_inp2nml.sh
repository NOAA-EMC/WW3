#!/bin/bash -e


if [ $# -ne 1 ]
then
  echo '  [ERROR] need ww3_multi input filename in argument [ww3_multi.inp]'
  exit 1
fi
inp=$1


version=$(bash --version | cut -d ' ' -f4)
version4=$(echo $version | cut -d '.' -f1)
echo $version4
if [ "$version4" != "4" ]
then
  echo '  [ERROR] need a version of bash at least 4'
  exit 1
fi


declare -A flginp
declare -A flggrd
declare -A fielddates
declare -A fieldlists
declare -A pointdates
declare -A pointfiles
declare -A trackdates
declare -A trackflags
declare -A restartdates
declare -A boundarydates
declare -A partitiondates
declare -A partfields
declare -A datafields


#------------------------------
# clean up inp file from all $ lines

rm -f ww3_multi_clean.inp

cat $inp | while read line
do
  
  if [ "$(echo $line | cut -c1)" = "$" ] 
  then
    continue
  fi

  echo $line >> ww3_multi_clean.inp

done

#------------------------------
# get all values from clean inp file

cat ww3_multi_clean.inp | while read line
do
  
echo $line

# model definition  
nrgrd="$(echo $line | cut -d ' ' -f1 | cut -d \" -f2  | cut -d \' -f2)"
nrinp="$(echo $line | cut -d ' ' -f2 | cut -d \" -f2  | cut -d \' -f2)"
unipts="$(echo $line | cut -d ' ' -f3 | cut -d \" -f2  | cut -d \' -f2)"
iostyp="$(echo $line | cut -d ' ' -f4 | cut -d \" -f2  | cut -d \' -f2)"
upproc="$(echo $line | cut -d ' ' -f5 | cut -d \" -f2  | cut -d \' -f2)"
pshare="$(echo $line | cut -d ' ' -f6 | cut -d \" -f2  | cut -d \' -f2)"


# input grids
for irinp in $(seq 1 $nrinp)
do
  read line
  inpid[$irinp]="$(echo $line | cut -d ' ' -f1 | cut -d \" -f2  | cut -d \' -f2)"
  numvar="$(echo $line | wc -w)"
  for iflag in $(seq 2  $numvar)
  do
    ind=$(expr $iflag - 1)
    flginp[$irinp,$ind]="$(echo $line | cut -d ' ' -f${iflag} | cut -d \" -f2  | cut -d \' -f2)"
    echo ${flginp[$irinp,$ind]}
  done
done

# unified point
if [ "$unipts" = "T" ]
then
  read line
  pointname="$(echo $line | cut -d \" -f2  | cut -d \' -f2)"
  echo $pointname
else
  pointname='points'
fi

# model grids
for irgrd in $(seq 1 $nrgrd)
do
  read line
  echo $line
  modid[$irgrd]="$(echo $line | cut -d ' ' -f1 | cut -d \" -f2  | cut -d \' -f2)"
  numvar=8
  for iflag in $(seq 2 $numvar)
  do
    ind=$(expr $iflag - 1)
    flggrd[$irgrd,$ind]="$(echo $line | cut -d ' ' -f${iflag} | cut -d \" -f2  | cut -d \' -f2)"
    echo ${flggrd[$irgrd,$ind]}
  done
  rank[$irgrd]="$(echo $line | cut -d ' ' -f$(expr $numvar + 1 ) | cut -d \" -f2  | cut -d \' -f2)"
  echo ${rank[$irgrd]}
  group[$irgrd]="$(echo $line | cut -d ' ' -f$(expr $numvar + 2 ) | cut -d \" -f2  | cut -d \' -f2)"
  echo ${group[$irgrd]}
  comm0[$irgrd]="$(echo $line | cut -d ' ' -f$(expr $numvar + 3 ) | cut -d \" -f2  | cut -d \' -f2)"
  echo ${comm0[$irgrd]}
  comm1[$irgrd]="$(echo $line | cut -d ' ' -f$(expr $numvar + 4 ) | cut -d \" -f2  | cut -d \' -f2)"
  echo ${comm1[$irgrd]}
  bound[$irgrd]="$(echo $line | cut -d ' ' -f$(expr $numvar + 5 ) | cut -d \" -f2  | cut -d \' -f2)"
  echo ${bound[$irgrd]}
done


# start stop dates
read line
start[1]="$(echo $line | cut -d ' ' -f1 | cut -d \" -f2  | cut -d \' -f2)"
start[2]="$(echo $line | cut -d ' ' -f2 | cut -d \" -f2  | cut -d \' -f2)"
stop[1]="$(echo $line | cut -d ' ' -f3 | cut -d \" -f2  | cut -d \' -f2)"
stop[2]="$(echo $line | cut -d ' ' -f4 | cut -d \" -f2  | cut -d \' -f2)"
echo ${start[@]}
echo ${stop[@]}

# masking flags
read line
flghg1="$(echo $line | cut -d ' ' -f1 | cut -d \" -f2  | cut -d \' -f2)"
flghg2="$(echo $line | cut -d ' ' -f2 | cut -d \" -f2  | cut -d \' -f2)"


# field date
echo 'field date'
read line
for i in $(seq 1 5)
do
  fielddate[$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${fielddate[@]}

# field list
if [ ${fielddate[3]} -ne 0 ]
then
  read line
  fieldformat="$(echo $line)"

  if [ "${fieldformat}" = "N" ]
  then
    read line
    fieldlist="$(echo $line)"
  else
    echo "OLD FORMAT ! please use name format with N"
    exit 1
  fi
fi


# point date
echo 'point date'
read line
for i in $(seq 1 5)
do
  pointdate[$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${pointdate[@]}

# point list
if [ ${pointdate[3]} -ne 0 ]
then
  pointfile="${pointname}.list"
  rm -f $pointfile
  read line
  tmpname="$(echo $line | cut -d ' ' -f3 | cut -d \" -f2  | cut -d \' -f2)"
  echo ${tmpname}
  while [ "${tmpname}" != "STOPSTRING" ]
  do
    echo $line >> $pointfile
    read line
    tmpname="$(echo $line | cut -d ' ' -f3 | cut -d \" -f2  | cut -d \' -f2)"
  done
fi


# track date
echo 'track date'
read line
for i in $(seq 1 5)
do
  trackdate[$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${trackdate[@]}

# track flag
if [ ${trackdate[3]} -ne 0 ]
then
  read line
  trackflag="$(echo $line)"
fi


# restart date
echo 'restart date'
read line
for i in $(seq 1 5)
do
  restartdate[$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${restartdate[@]}


# boundary date
echo 'boundary date'
read line
for i in $(seq 1 5)
do
  boundarydate[$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${boundarydate[@]}


# partition date
echo 'partition date'
read line
for i in $(seq 1 5)
do
  partitiondate[$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${partitiondate[@]}

# partition info
if [ ${partitiondate[3]} -ne 0 ]
then
  read line
  for i in $(seq 1 7)
  do
    partfield[$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
  done
fi


# fill all model grids outputs
for j in $(seq 1 $nrgrd)
do
  for key in "${!fielddate[@]}"
  do
    fielddates[$j,$key]=${fielddate[$key]}
    pointdates[$j,$key]=${pointdate[$key]}
    trackdates[$j,$key]=${trackdate[$key]}
    restartdates[$j,$key]=${restartdate[$key]}
    boundarydates[$j,$key]=${boundarydate[$key]}
    partitiondates[$j,$key]=${partitiondate[$key]}
  done
  pointfiles[$j]=$pointfile
  fieldformats[$j]=$fieldformat
  fieldlists[$j]=$fieldlist
  trackflags[$j]=$trackflag
  for key in "${!partfield[@]}"
  do
    partfields[$j,$key]=${partfield[$key]}
  done
done


# output per model grid
read line
grdname="$(echo $line | cut -d ' ' -f1 | cut -d \" -f2  | cut -d \' -f2)"
grdnum="$(echo $line | cut -d ' ' -f2 | cut -d \" -f2  | cut -d \' -f2)"
while [ "$grdname" != 'the_end' ]
do
  for irgrd in $(seq 1 $nrgrd)
  do
    if [ "${modid[$irgrd]}" = "$grdname" ]
    then
      # field output
      if [ "${grdnum}" = "1" ]
      then
        read line
        for i in $(seq 1 5)
        do
          fielddates[$irgrd,$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
        done
        if [ ${fielddates[$irgrd,3]} != 0 ]
        then
          read line
          fieldformats[$irgrd]="$(echo $line)"

          if [ "${fieldformats[$irgrd]}" = "N" ]
          then
            read line
            fieldlists[$irgrd]="$(echo $line)"
          else
            echo "OLD FORMAT ! please use name format with N"
            exit 1
          fi
        fi

      # point output
      elif [ "${grdnum}" = "2" ] 
      then
        read line
        for i in $(seq 1 5)
        do
          pointdates[$irgrd,$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
        done
        if [ ${pointdates[$irgrd,3]} != 0 ]
        then
          pointfiles[$irgrd]="points_${irgrd}.list"
          rm -f ${pointfiles[$irgrd]}
          read line
          tmpname="$(echo $line | cut -d ' ' -f3 | cut -d \" -f2  | cut -d \' -f2)"
          while [ "$tmpname" != "STOPSTRING" ]
          do
            echo $line >> ${pointfiles[$irgrd]}
            read line
            tmpname="$(echo $line | cut -d ' ' -f3 | cut -d \" -f2  | cut -d \' -f2)"
          done
        fi

      # track output
      elif [ "${grdnum}" = "3" ] 
      then
        read line
        for i in $(seq 1 5)
        do
          trackdates[$irgrd,$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
        done
        if [ ${trackdates[$irgrd,3]} != 0 ]
        then
          read line
          trackflags[$irgrd]="$(echo $line)"
        fi

      # restart output
      elif [ "${grdnum}" = "4" ] 
      then
        read line
        for i in $(seq 1 5)
        do
          restartdates[$irgrd,$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
        done

      # boundary output
      elif [ "${grdnum}" = "5" ] 
      then
        read line
        for i in $(seq 1 5)
        do
          boundarydates[$irgrd,$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
        done

      # partition output
      elif [ "${grdnum}" = "6" ] 
      then
        read line
        for i in $(seq 1 5)
        do
          partitiondates[$irgrd,$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
        done
        if [ ${partitiondates[$irgrd,3]} != 0 ]
        then
          read line
          for i in $(seq 1 7)
          do
            partfields[$irgrd,$i]="$(echo $line | cut -d ' ' -f${i} | cut -d \" -f2  | cut -d \' -f2)"
          done
        fi
      fi
    fi
  done
  echo $grdname
  read line
  grdname="$(echo $line | cut -d ' ' -f1 | cut -d \" -f2  | cut -d \' -f2)"
  grdnum="$(echo $line | cut -d ' ' -f2 | cut -d \" -f2  | cut -d \' -f2)"
done
echo $grdname


# moving grid data
read line
dataname="$(echo $line | cut -d ' ' -f1 | cut -d \" -f2  | cut -d \' -f2)"
imove=0
while [ "$dataname" != 'STP' ]
do
  imove=$(expr $imove + 1 )
  echo $dataname
  datafields[$imove,1]="$(echo $line | cut -d ' ' -f1 | cut -d \" -f2  | cut -d \' -f2)"
  datafields[$imove,2]="$(echo $line | cut -d ' ' -f2 | cut -d \" -f2  | cut -d \' -f2)"
  datafields[$imove,3]="$(echo $line | cut -d ' ' -f3 | cut -d \" -f2  | cut -d \' -f2)"
  datafields[$imove,4]="$(echo $line | cut -d ' ' -f4 | cut -d \" -f2  | cut -d \' -f2)"
  datafields[$imove,5]="$(echo $line | cut -d ' ' -f5 | cut -d \" -f2  | cut -d \' -f2)"
  read line
  dataname="$(echo $line | cut -d ' ' -f1 | cut -d \" -f2  | cut -d \' -f2)"
done
nmove=$imove
echo $dataname




#------------------------------
# write value in a new nml file

nmlfile=$(basename $inp .inp).nml



# domain def namelist
cat > $nmlfile << EOF
! -------------------------------------------------------------------- !
! Define top-level model parameters via domain_def_nml namelist
! * namelist must be terminated with /
! * definitions & defaults:
!     domain%nrinp  =  0  ! Number of grids defining input fields.
!     domain%nrgrd  =  1  ! Number of wave model grids.
!     domain%nmove  =  0  ! Number of moving grid inputs.
!     domain%unipts =  F  ! Flag for using unified point output file.
!     domain%iostyp =  1  ! Output server type as in ww3_shel.nml
!     domain%upproc =  F  ! Flag for dedicated process for unified point output.
!     domain%pshare =  F  ! Flag for grids sharing dedicated output processes.
!     domain%flghg1 =  T  ! Flag for masking computation in two-way nesting
!     domain%flghg2 =  T  ! Flag for masking at printout time
!     domain%start  = '19680606 000000'  ! Start date for the entire model 
!     domain%stop   = '19680607 000000'  ! Stop date for the entire model
! -------------------------------------------------------------------- !
&domain_def_nml
EOF

if [ "$nrinp" != 0 ];                       then  echo "  domain%nrinp  = $nrinp" >> $nmlfile; fi
if [ "$nrgrd" != 1 ];                       then  echo "  domain%nrgrd  = $nrgrd" >> $nmlfile; fi
if [ "$nmove" != 0 ];                       then  echo "  domain%nmove  = $nmove" >> $nmlfile; fi
if [ "$unipts" != "F" ];                    then  echo "  domain%unipts = $unipts" >> $nmlfile; fi
if [ "$iostyp" != 1 ];                      then  echo "  domain%iostyp = $iostyp" >> $nmlfile; fi
if [ "$upproc" != "F" ];                    then  echo "  domain%upproc = $upproc" >> $nmlfile; fi
if [ "$pshare" != "F" ];                    then  echo "  domain%pshare = $pshare" >> $nmlfile; fi
if [ "$flghg1" != "T" ];                    then  echo "  domain%flghg1 = $flghg1" >> $nmlfile; fi
if [ "$flghg2" != "T" ];                    then  echo "  domain%flghg2 = $flghg2" >> $nmlfile; fi
if [ "${start[*]}" != "19680606 000000" ];  then  echo "  domain%start  = '${start[@]}'" >> $nmlfile; fi
if [ "${stop[*]}" != "19680607 000000" ];   then  echo "  domain%stop   = '${stop[@]}'" >> $nmlfile; fi




# input grid namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define each input grid via the input_grid_nml namelist
! * namelist must be terminated with /
! * input(i)%name must be set for each active input grid i
! * definitions & defaults:
!     input(i)%name                  = 'unset'
!     input(i)%forcing%water_levels  = F
!     input(i)%forcing%currents      = F
!     input(i)%forcing%winds         = F
!     input(i)%forcing%ice_conc      = F
!     input(i)%forcing%ice_param1    = F
!     input(i)%forcing%ice_param2    = F
!     input(i)%forcing%ice_param3    = F
!     input(i)%forcing%ice_param4    = F
!     input(i)%forcing%ice_param5    = F
!     input(i)%forcing%mud_density   = F
!     input(i)%forcing%mud_thickness = F
!     input(i)%forcing%mud_viscosity = F
!     input(i)%assim%mean            = F
!     input(i)%assim%spec1d          = F
!     input(i)%assim%spec2d          = F
! -------------------------------------------------------------------- !
&input_grid_nml
EOF

for irinp in $(seq 1 $nrinp)
do
  if [ "${inpid[$irinp]}" != 'unset' ] 
  then
                                              echo "  input($irinp)%name                  = '${inpid[$irinp]}'" >> $nmlfile
    if [ "${flginp[$irinp,1]}" != 'F' ];then  echo "  input($irinp)%forcing%water_levels  = ${flginp[$irinp,1]}" >> $nmlfile; fi
    if [ "${flginp[$irinp,2]}" != 'F' ];then  echo "  input($irinp)%forcing%currents      = ${flginp[$irinp,2]}" >> $nmlfile; fi
    if [ "${flginp[$irinp,3]}" != 'F' ];then  echo "  input($irinp)%forcing%winds         = ${flginp[$irinp,3]}" >> $nmlfile; fi
    if [ "${flginp[$irinp,4]}" != 'F' ];then  echo "  input($irinp)%forcing%ice_conc      = ${flginp[$irinp,4]}" >> $nmlfile; fi
    if [ "${flginp[$irinp,5]}" != 'F' ];then  echo "  input($irinp)%assim%mean            = ${flginp[$irinp,5]}" >> $nmlfile; fi
    if [ "${flginp[$irinp,6]}" != 'F' ];then  echo "  input($irinp)%assim%spec1d          = ${flginp[$irinp,6]}" >> $nmlfile; fi
    if [ "${flginp[$irinp,7]}" != 'F' ];then  echo "  input($irinp)%assim%spec2d          = ${flginp[$irinp,7]}" >> $nmlfile; fi
  fi
done


# model grid namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define each model grid via the model_grid_nml namelist
! * namelist must be terminated with /
! * model(i)%name must be set for each active model grid i
! * definitions & defaults:
!     model(i)%name                  = 'unset'
!     model(i)%forcing%water_levels  = 'no'
!     model(i)%forcing%currents      = 'no'
!     model(i)%forcing%winds         = 'no'
!     model(i)%forcing%ice_conc      = 'no'
!     model(i)%forcing%ice_param1    = 'no'
!     model(i)%forcing%ice_param2    = 'no'
!     model(i)%forcing%ice_param3    = 'no'
!     model(i)%forcing%ice_param4    = 'no'
!     model(i)%forcing%ice_param5    = 'no'
!     model(i)%forcing%mud_density   = 'no'
!     model(i)%forcing%mud_thickness = 'no'
!     model(i)%forcing%mud_viscosity = 'no'
!     model(i)%assim%mean            = 'no'
!     model(i)%assim%spec1d          = 'no'
!     model(i)%assim%spec2d          = 'no'
!     model(i)%resource%rank_id      = i
!     model(i)%resource%group_id     = 1
!     model(i)%resource%sibling_id   = 0
!     model(i)%resource%comm_frac    = 0.00,1.00
!     model(i)%resource%bound_flag   = F
!
!     model(i)%forcing = 'no' 'no' 'no' 'no' 'no' 'no'
!
!     model(i)%resource = i 1 0 0.00 1.00 f
! -------------------------------------------------------------------- !
&model_grid_nml
EOF

for irgrd in $(seq 1 $nrgrd)
do
  if [ "${modid[$irgrd]}" != 'unset' ] 
  then
                                                echo "  model($irgrd)%name                  = '${modid[$irgrd]}'" >> $nmlfile
    if [ "${flggrd[$irgrd,1]}" != 'no' ]; then  echo "  model($irgrd)%forcing%water_levels  = '${flggrd[$irgrd,1]}'" >> $nmlfile; fi
    if [ "${flggrd[$irgrd,2]}" != 'no' ]; then  echo "  model($irgrd)%forcing%currents      = '${flggrd[$irgrd,2]}'" >> $nmlfile; fi
    if [ "${flggrd[$irgrd,3]}" != 'no' ]; then  echo "  model($irgrd)%forcing%winds         = '${flggrd[$irgrd,3]}'" >> $nmlfile; fi
    if [ "${flggrd[$irgrd,4]}" != 'no' ]; then  echo "  model($irgrd)%forcing%ice_conc      = '${flggrd[$irgrd,4]}'" >> $nmlfile; fi
    if [ "${flggrd[$irgrd,5]}" != 'no' ]; then  echo "  model($irgrd)%assim%mean            = '${flggrd[$irgrd,5]}'" >> $nmlfile; fi
    if [ "${flggrd[$irgrd,6]}" != 'no' ]; then  echo "  model($irgrd)%assim%spec1d          = '${flggrd[$irgrd,6]}'" >> $nmlfile; fi
    if [ "${flggrd[$irgrd,7]}" != 'no' ]; then  echo "  model($irgrd)%assim%spec2d          = '${flggrd[$irgrd,7]}'" >> $nmlfile; fi
    if [ "${rank[$irgrd]}" != "$irgrd" ]; then  echo "  model($irgrd)%resource%rank_id      = ${rank[$irgrd]}" >> $nmlfile; fi
    if [ "${group[$irgrd]}" != 1 ];       then  echo "  model($irgrd)%resource%group_id     = ${group[$irgrd]}" >> $nmlfile; fi
    if [ "${comm0[$irgrd]},${comm1[$irgrd]}" != '0.00,1.00' ];then  
                                                echo "  model($irgrd)%resource%comm_frac    = ${comm0[$irgrd]},${comm1[$irgrd]}" >> $nmlfile; fi
    if [ "${bound[$irgrd]}" != 'F' ];     then  echo "  model($irgrd)%resource%bound_flag   = ${bound[$irgrd]}" >> $nmlfile; fi
  fi
done


# output type namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the output types point parameters via output_type_nml namelist
! * namelist must be terminated with /
! * alltype will apply the output types for all the model grids
! * itype(i) will apply the output types for the model grid number i
! * the full list of field names is : 
!  DPT CUR WND AST WLV ICE IBG D50 IC1 IC5 HS LM T02 T0M1 T01 FP DIR SPR
!  DP HIG EF TH1M STH1M TH2M STH2M WN PHS PTP PLP PDIR PSPR PWS TWS PNR
!  UST CHA CGE FAW TAW TWA WCC WCF WCH WCM SXY TWO BHD FOC TUS USS P2S
!  USF P2L TWI FIC ABR UBR BED FBB TBB MSS MSC DTD FC CFX CFD CFK U1 U2 
! * need domain%unipts equal true to use a unified point output file
! * the point file is a space separated values per line : lon lat 'name'
! * output track file formatted (t) or unformated (f)
! * definitions & defaults:
!     alltype%field%list         =  'unset'
!     alltype%point%name         =  'unset'
!     alltype%point%file         =  'unset'
!     alltype%track%format       =  T
!     alltype%partition%x0       =  0
!     alltype%partition%xn       =  0
!     alltype%partition%nx       =  0
!     alltype%partition%y0       =  0
!     alltype%partition%yn       =  0
!     alltype%partition%ny       =  0
!     alltype%partition%format   =  T
!
!     itype(i)%track%format      =  T
!
!     alltype%partition          =  0 0 0 0 0 0 T
! -------------------------------------------------------------------- !
&output_type_nml
EOF

if [ "${fielddate[3]}" != 0 ];    then  echo "  alltype%field%list       = '$fieldlist'" >> $nmlfile; fi

if [ "${pointdate[3]}" != 0 ]
then
  if [ "$pointname" != 'unset' ] && [ "$unipts" = "T" ]; then  
                                        echo "  alltype%point%name       = '$pointname'" >> $nmlfile; fi
  if [ "$pointfile" != 'unset' ]; then  echo "  alltype%point%file       = '$pointfile'" >> $nmlfile; fi
fi

if [ "${trackdate[3]}" != 0 ] && [ "$trackflag" != T ];    then  
                                        echo "  alltype%track%format     = $trackflag" >> $nmlfile; fi
if [ "${partitiondate[3]}" != 0 ]
then
#  if [ "${partfield[1]}" != 0 ];  then  echo "  alltype%partition%x0     = ${partfield[1]}" >> $nmlfile; fi
#  if [ "${partfield[2]}" != 0 ];  then  echo "  alltype%partition%xn     = ${partfield[2]}" >> $nmlfile; fi
#  if [ "${partfield[3]}" != 0 ];  then  echo "  alltype%partition%nx     = ${partfield[3]}" >> $nmlfile; fi
#  if [ "${partfield[4]}" != 0 ];  then  echo "  alltype%partition%y0     = ${partfield[4]}" >> $nmlfile; fi
#  if [ "${partfield[5]}" != 0 ];  then  echo "  alltype%partition%yn     = ${partfield[5]}" >> $nmlfile; fi
#  if [ "${partfield[6]}" != 0 ];  then  echo "  alltype%partition%ny     = ${partfield[6]}" >> $nmlfile; fi
#  if [ "${partfield[7]}" != T ];  then  echo "  alltype%partition%format = ${partfield[7]}" >> $nmlfile; fi
  if [ "${partfield[1]}" != 0 ] || [ "${partfield[2]}" != 0 ] || [ "${partfield[3]}" != 0 ] || [ "${partfield[4]}" != 0 ] || \
     [ "${partfield[5]}" != 0 ] || [ "${partfield[6]}" != 0 ] || [ "${partfield[7]}" != T ];  then
  echo "  alltype%partition        = ${partfield[1]} ${partfield[2]} ${partfield[3]} ${partfield[4]} ${partfield[5]} ${partfield[6]} ${partfield[7]}" >> $nmlfile; fi
fi

for irgrd in $(seq 1 $nrgrd)
do
  if [ "${fieldlist}" != "${fieldlists[$irgrd]}" ]; then
    echo "  itype(${irgrd})%field%list      = ${fieldlists[$irgrd]}" >> $nmlfile; fi

  if [ "${pointfile}" != "${pointfiles[$irgrd]}" ]; then
    echo "  itype(${irgrd})%point%file      = '${pointfiles[$irgrd]}'" >> $nmlfile; fi

  if [ "${trackflag}" != "${trackflags[$irgrd]}" ]; then
    echo "  itype(${irgrd})%track%format    = ${trackflags[$irgrd]}" >> $nmlfile; fi

#  if [ "${partfield[1]}" != "${partfields[$irgrd,1]}" ]; then
#    echo "  type(${irgrd})%partition%x0     = ${partfields[$irgrd,1]}" >> $nmlfile; fi
#  if [ "${partfield[2]}" != "${partfields[$irgrd,2]}" ]; then
#    echo "  type(${irgrd})%partition%xn     = ${partfields[$irgrd,2]}" >> $nmlfile; fi
#  if [ "${partfield[3]}" != "${partfields[$irgrd,3]}" ]; then
#    echo "  type(${irgrd})%partition%nx     = ${partfields[$irgrd,3]}" >> $nmlfile; fi
#  if [ "${partfield[4]}" != "${partfields[$irgrd,4]}" ]; then
#    echo "  type(${irgrd})%partition%y0     = ${partfields[$irgrd,4]}" >> $nmlfile; fi
#  if [ "${partfield[5]}" != "${partfields[$irgrd,5]}" ]; then
#    echo "  type(${irgrd})%partition%yn     = ${partfields[$irgrd,5]}" >> $nmlfile; fi
#  if [ "${partfield[6]}" != "${partfields[$irgrd,6]}" ]; then
#    echo "  type(${irgrd})%partition%ny     = ${partfields[$irgrd,6]}" >> $nmlfile; fi
#  if [ "${partfield[7]}" != "${partfields[$irgrd,7]}" ]; then
#    echo "  type(${irgrd})%partition%format = ${partfields[$irgrd,7]}" >> $nmlfile; fi
  if [ "${partfield[1]}" != "${partfields[$irgrd,1]}" ] || [ "${partfield[2]}" != "${partfields[$irgrd,2]}" ] || \
     [ "${partfield[3]}" != "${partfields[$irgrd,3]}" ] || [ "${partfield[4]}" != "${partfields[$irgrd,4]}" ] || \
     [ "${partfield[5]}" != "${partfields[$irgrd,5]}" ] || [ "${partfield[6]}" != "${partfields[$irgrd,6]}" ] || \
     [ "${partfield[7]}" != "${partfields[$irgrd,7]}" ]; then
  echo "  itype(${irgrd})%partition       = ${partfields[$irgrd,1]} ${partfields[$irgrd,2]} ${partfields[$irgrd,3]} ${partfields[$irgrd,4]} ${partfields[$irgrd,5]} ${partfields[$irgrd,6]} ${partfields[$irgrd,7]}" >> $nmlfile; fi

done

# output date namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define output dates via output_date_nml namelist
! * namelist must be terminated with /
! * alldate will apply the output dates for all the model grids
! * idate(i) will apply the output dates for the model grid number i
! * start and stop times are with format 'yyyymmdd hhmmss'
! * if time stride is equal '0', then output is disabled
! * time stride is given in seconds
! * it is possible to overwrite a global output date for a given grid
! * definitions & defaults:
!     alldate%field%start          =  '19680606 000000'
!     alldate%field%stride         =  '0'
!     alldate%field%stop           =  '19680607 000000'
!     alldate%point%start          =  '19680606 000000'
!     alldate%point%stride         =  '0'
!     alldate%point%stop           =  '19680607 000000'
!     alldate%track%start          =  '19680606 000000'
!     alldate%track%stride         =  '0'
!     alldate%track%stop           =  '19680607 000000'
!     alldate%restart%start        =  '19680606 000000'
!     alldate%restart%stride       =  '0'
!     alldate%restart%stop         =  '19680607 000000'
!     alldate%boundary%start       =  '19680606 000000'
!     alldate%boundary%stride      =  '0'
!     alldate%boundary%stop        =  '19680607 000000'
!     alldate%partition%start      =  '19680606 000000'
!     alldate%partition%stride     =  '0'
!     alldate%partition%stop       =  '19680607 000000'
!     
!     idate(i)%partition%startdate =  '19680606 000000' 
!
!     alldate%restart              =  '19680606 000000' '0' '19680607 000000'
! -------------------------------------------------------------------- !
&output_date_nml
EOF

if [ "${fielddate[1]}" != '19680606' ] || [ "${fielddate[2]}" != '000000' ] || [ "${fielddate[3]}" != '0' ] || \
   [ "${fielddate[4]}" != '19680607' ] || [ "${fielddate[5]}" != '000000' ]; then  
      echo "  alldate%field          = '${fielddate[1]} ${fielddate[2]}' '${fielddate[3]}' '${fielddate[4]} ${fielddate[5]}'" >> $nmlfile; fi

if [ "${pointdate[1]}" != '19680606' ] || [ "${pointdate[2]}" != '000000' ] || [ "${pointdate[3]}" != '0' ] || \
   [ "${pointdate[4]}" != '19680607' ] || [ "${pointdate[5]}" != '000000' ]; then  
      echo "  alldate%point          = '${pointdate[1]} ${pointdate[2]}' '${pointdate[3]}' '${pointdate[4]} ${pointdate[5]}'" >> $nmlfile; fi

if [ "${trackdate[1]}" != '19680606' ] || [ "${trackdate[2]}" != '000000' ] || [ "${trackdate[3]}" != '0' ] || \
   [ "${trackdate[4]}" != '19680607' ] || [ "${trackdate[5]}" != '000000' ]; then  
      echo "  alldate%track          = '${trackdate[1]} ${trackdate[2]}' '${trackdate[3]}' '${trackdate[4]} ${trackdate[5]}'" >> $nmlfile; fi

if [ "${restartdate[1]}" != '19680606' ] || [ "${restartdate[2]}" != '000000' ] || [ "${restartdate[3]}" != '0' ] || \
   [ "${restartdate[4]}" != '19680607' ] || [ "${restartdate[5]}" != '000000' ]; then  
      echo "  alldate%restart        = '${restartdate[1]} ${restartdate[2]}' '${restartdate[3]}' '${restartdate[4]} ${restartdate[5]}'" >> $nmlfile; fi

if [ "${boundarydate[1]}" != '19680606' ] || [ "${boundarydate[2]}" != '000000' ] || [ "${boundarydate[3]}" != '0' ] || \
   [ "${boundarydate[4]}" != '19680607' ] || [ "${boundarydate[5]}" != '000000' ]; then  
      echo "  alldate%boundary       = '${boundarydate[1]} ${boundarydate[2]}' '${boundarydate[3]}' '${boundarydate[4]} ${boundarydate[5]}'" >> $nmlfile; fi

if [ "${partitiondate[1]}" != '19680606' ] || [ "${partitiondate[2]}" != '000000' ] || [ "${partitiondate[3]}" != '0' ] || \
   [ "${partitiondate[4]}" != '19680607' ] || [ "${partitiondate[5]}" != '000000' ]; then  
      echo "  alldate%partition      = '${partitiondate[1]} ${partitiondate[2]}' '${partitiondate[3]}' '${partitiondate[4]} ${partitiondate[5]}'" >> $nmlfile; fi


for irgrd in $(seq 1 $nrgrd)
do
  if [ "${fielddate[1]}" != "${fielddates[$irgrd,1]}" ] || [ "${fielddate[2]}" != "${fielddates[$irgrd,2]}" ] || \
     [ "${fielddate[3]}" != "${fielddates[$irgrd,3]}" ] || [ "${fielddate[4]}" != "${fielddates[$irgrd,4]}" ] || \
     [ "${fielddate[5]}" != "${fielddates[$irgrd,5]}" ]; then  
        echo "  idate($irgrd)%field         = '${fielddates[$irgrd,1]} ${fielddates[$irgrd,2]}' '${fielddates[$irgrd,3]}' '${fielddates[$irgrd,4]} ${fielddates[$irgrd,5]}'" >> $nmlfile; fi

  if [ "${pointdate[1]}" != "${pointdates[$irgrd,1]}" ] || [ "${pointdate[2]}" != "${pointdates[$irgrd,2]}" ] || \
     [ "${pointdate[3]}" != "${pointdates[$irgrd,3]}" ] || [ "${pointdate[4]}" != "${pointdates[$irgrd,4]}" ] || \
     [ "${pointdate[5]}" != "${pointdates[$irgrd,5]}" ]; then  
        echo "  idate($irgrd)%point         = '${pointdates[$irgrd,1]} ${pointdates[$irgrd,2]}' '${pointdates[$irgrd,3]}' '${pointdates[$irgrd,4]} ${pointdates[$irgrd,5]}'" >> $nmlfile; fi

  if [ "${trackdate[1]}" != "${trackdates[$irgrd,1]}" ] || [ "${trackdate[2]}" != "${trackdates[$irgrd,2]}" ] || \
     [ "${trackdate[3]}" != "${trackdates[$irgrd,3]}" ] || [ "${trackdate[4]}" != "${trackdates[$irgrd,4]}" ] || \
     [ "${trackdate[5]}" != "${trackdates[$irgrd,5]}" ]; then  
        echo "  idate($irgrd)%track         = '${trackdates[$irgrd,1]} ${trackdates[$irgrd,2]}' '${trackdates[$irgrd,3]}' '${trackdates[$irgrd,4]} ${trackdates[$irgrd,5]}'" >> $nmlfile; fi

  if [ "${restartdate[1]}" != "${restartdates[$irgrd,1]}" ] || [ "${restartdate[2]}" != "${restartdates[$irgrd,2]}" ] || \
     [ "${restartdate[3]}" != "${restartdates[$irgrd,3]}" ] || [ "${restartdate[4]}" != "${restartdates[$irgrd,4]}" ] || \
     [ "${restartdate[5]}" != "${restartdates[$irgrd,5]}" ]; then   
        echo "  idate($irgrd)%restart       = '${restartdates[$irgrd,1]} ${restartdates[$irgrd,2]}' '${restartdates[$irgrd,3]}' '${restartdates[$irgrd,4]} ${restartdates[$irgrd,5]}'" >> $nmlfile; fi

  if [ "${boundarydate[1]}" != "${boundarydates[$irgrd,1]}" ] || [ "${boundarydate[2]}" != "${boundarydates[$irgrd,2]}" ] || \
     [ "${boundarydate[3]}" != "${boundarydates[$irgrd,3]}" ] || [ "${boundarydate[4]}" != "${boundarydates[$irgrd,4]}" ] || \
     [ "${boundarydate[5]}" != "${boundarydates[$irgrd,5]}" ]; then  
        echo "  idate($irgrd)%boundary      = '${boundarydates[$irgrd,1]} ${boundarydates[$irgrd,2]}' '${boundarydates[$irgrd,3]}' '${boundarydates[$irgrd,4]} ${boundarydates[$irgrd,5]}'" >> $nmlfile; fi

  if [ "${partitiondate[1]}" != "${partitiondates[$irgrd,1]}" ] || [ "${partitiondate[2]}" != "${partitiondates[$irgrd,2]}" ] || \
     [ "${partitiondate[3]}" != "${partitiondates[$irgrd,3]}" ] || [ "${partitiondate[4]}" != "${partitiondates[$irgrd,4]}" ] || \
     [ "${partitiondate[5]}" != "${partitiondates[$irgrd,5]}" ]; then  
        echo "  idate($irgrd)%partition     = '${partitiondates[$irgrd,1]} ${partitiondates[$irgrd,2]}' '${partitiondates[$irgrd,3]}' '${partitiondates[$irgrd,4]} ${partitiondates[$irgrd,5]}'" >> $nmlfile; fi
done

# homogeneous input namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define homogeneous input via homonegenous_input_nml namelist
! * namelist must be terminated with /
! * the number of moving grid inputs is defined by domain_def%nmove
! * each homogeneous input must start from index imove = 1 to nmove
! * if speed is equal 0, then the moving grid is desactivated
! * definitions & defaults:
!     homogeneous(imove)%moving%start      =  '19680606 000000'
!     homogeneous(imove)%moving%speed      =  0
!     homogeneous(imove)%moving%direction  =  0
! -------------------------------------------------------------------- !
&homogeneous_input_nml
EOF

for imove in $(seq 1 $nmove)
do
  if [ "${datafields[$imove,2]} ${datafields[$imove,3]}" != '19680606 000000' ]; then
  echo "  homogeneous($imove)%moving%start         = '${datafields[$imove,2]} ${datafields[$imove,3]}'" >> $nmlfile; fi
  if [ "${datafields[$imove,4]}" != '0' ]; then
  echo "  homogeneous($imove)%moving%speed         = ${datafields[$imove,4]}" >> $nmlfile; fi
  if [ "${datafields[$imove,5]}" != '0' ]; then
  echo "  homogeneous($imove)%moving%direction     = ${datafields[$imove,5]}" >> $nmlfile; fi
done

cat >> $nmlfile << EOF
/

EOF

done # must be closed here to keep the variables values locally

rm -f ww3_multi_clean.inp
#------------------------------



