#!/bin/bash -e

prog="ww3_multi"

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
declare -A homogmov


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

# model definition  
nrgrd="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $nrgrd
nrinp="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo $nrinp
unipts="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
echo $unipts
iostyp="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
echo $iostyp
upproc="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
echo $upproc
pshare="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
echo $pshare


# input grids
for irinp in $(seq 1 $nrinp)
do
  il=$(($il+1))
  inpid[$irinp]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  numvar="$(echo ${lines[$il]} | wc -w)"
  for iflag in $(seq 2  $numvar)
  do
    ind=$(($iflag - 1))
    flginp[$irinp,$ind]="$(echo ${lines[$il]} | awk -F' ' "{print \$$iflag}" | cut -d \" -f2  | cut -d \' -f2)"
    echo ${flginp[$irinp,$ind]}
  done
done

# unified point
if [ "$unipts" = "T" ]
then
  il=$(($il+1))
  pointname="$(echo ${lines[$il]} | cut -d \" -f2  | cut -d \' -f2)"
  echo $pointname
else
  pointname='points'
fi

# model grids
for irgrd in $(seq 1 $nrgrd)
do
  il=$(($il+1))
  echo ${lines[$il]}
  modid[$irgrd]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  numvar=8
  for iflag in $(seq 2 $numvar)
  do
    ind=$(($iflag - 1))
    flggrd[$irgrd,$ind]="$(echo ${lines[$il]} | awk -F' ' "{print \$$iflag}" | cut -d \" -f2  | cut -d \' -f2)"
    echo ${flggrd[$irgrd,$ind]}
  done
  rank[$irgrd]="$(echo ${lines[$il]} | awk -F' ' "{print \$$(($numvar + 1))}" | cut -d \" -f2  | cut -d \' -f2)"
  echo ${rank[$irgrd]}
  group[$irgrd]="$(echo ${lines[$il]} | awk -F' ' "{print \$$(($numvar + 2))}" | cut -d \" -f2  | cut -d \' -f2)"
  echo ${group[$irgrd]}
  comm0[$irgrd]="$(echo ${lines[$il]} | awk -F' ' "{print \$$(($numvar + 3))}" | cut -d \" -f2  | cut -d \' -f2)"
  echo ${comm0[$irgrd]}
  comm1[$irgrd]="$(echo ${lines[$il]} | awk -F' ' "{print \$$(($numvar + 4))}" | cut -d \" -f2  | cut -d \' -f2)"
  echo ${comm1[$irgrd]}
  bound[$irgrd]="$(echo ${lines[$il]} | awk -F' ' "{print \$$(($numvar + 5))}" | cut -d \" -f2  | cut -d \' -f2)"
  echo ${bound[$irgrd]}
done


# start stop dates
il=$(($il+1))
start[1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
start[2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
stop[1]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
stop[2]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
echo ${start[@]}
echo ${stop[@]}

# masking flags
il=$(($il+1))
flghg1="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
flghg2="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"


# field date
echo 'field date'
il=$(($il+1))
for i in $(seq 1 5)
do
  fielddate[$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${fielddate[@]}

# field list
if [ ${fielddate[3]} -ne 0 ]
then
  il=$(($il+1))
  fieldformat="$(echo ${lines[$il]} | cut -d \" -f2  | cut -d \' -f2)"

  if [ "${fieldformat}" = "N" ]
  then
    il=$(($il+1))
    fieldlist="$(echo ${lines[$il]} | cut -d \" -f2  | cut -d \' -f2)"
  else
    echo "OLD FORMAT ! please use name format with N"
    exit 1
  fi
fi


# point date
echo 'point date'
il=$(($il+1))
for i in $(seq 1 5)
do
  pointdate[$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${pointdate[@]}

# point list
if [ ${pointdate[3]} -ne 0 ]
then
  foripoint="$cur_dir/${pointname}.list"
  fpoint="$cur_dir/${pointname}.list.new"
  point_filename=$foripoint
  rm -f $fpoint
  il=$(($il+1))
  tmpname="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  echo ${tmpname}
  while [ "${tmpname}" != "STOPSTRING" ]
  do
    echo ${lines[$il]} >> $fpoint
    il=$(($il+1))
    tmpname="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  done
  if [ -f $foripoint ]
  then
    if [ -z "$(diff $foripoint $fpoint)" ]
    then
      echo $foripoint ' and ' $fpoint 'are same.'
      echo 'delete ' $fpoint
      rm $fpoint
    else
      echo 'diff between :' $foripoint ' and new file : ' $fpoint
      echo 'inp2nml conversion stopped'
      exit 1
    fi
  else
    echo 'mv '$fpoint ' to ' $foripoint
    mv $fpoint $foripoint
  fi
fi

# track date
echo 'track date'
il=$(($il+1))
for i in $(seq 1 5)
do
  trackdate[$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${trackdate[@]}

# track flag
if [ ${trackdate[3]} -ne 0 ]
then
  il=$(($il+1))
  trackflag="$(echo ${lines[$il]})"
fi


# restart date
echo 'restart date'
il=$(($il+1))
for i in $(seq 1 6)
do
  restartdate[$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${restartdate[@]}

if [ "${restartdate[6]}" = 'T' ]
then
# restart date2
echo 'restart date 2'
il=$(($il+1))
for i in $(seq 1 5)
do
  restartdate2[$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${restartdate2[@]}
fi

# boundary date
echo 'boundary date'
il=$(($il+1))
for i in $(seq 1 5)
do
  boundarydate[$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${boundarydate[@]}


# partition date
echo 'partition date'
il=$(($il+1))
for i in $(seq 1 5)
do
  partitiondate[$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${partitiondate[@]}

# partition info
if [ ${partitiondate[3]} -ne 0 ]
then
  il=$(($il+1))
  for i in $(seq 1 7)
  do
    partfield[$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
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
    restartdates2[$j,$key]=${restartdate2[$key]}
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
il=$(($il+1))
grdname="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
grdnum="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
while [ "$grdname" != 'the_end' ]
do
  for irgrd in $(seq 1 $nrgrd)
  do
    if [ "${modid[$irgrd]}" = "$grdname" ]
    then
      # field output
      if [ "${grdnum}" = "1" ]
      then
        il=$(($il+1))
        for i in $(seq 1 5)
        do
          fielddates[$irgrd,$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
        done
        if [ ${fielddates[$irgrd,3]} != 0 ]
        then
          il=$(($il+1))
          fieldformats[$irgrd]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"

          if [ "${fieldformats[$irgrd]}" = "N" ]
          then
            il=$(($il+1))
            fieldlists[$irgrd]="$(echo ${lines[$il]} | cut -d \" -f2  | cut -d \' -f2)"
          else
            echo "OLD FORMAT ! please use name format with N"
            exit 1
          fi
        fi

      # point output
      elif [ "${grdnum}" = "2" ] 
      then
        il=$(($il+1))
        for i in $(seq 1 5)
        do
          pointdates[$irgrd,$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
        done
        if [ ${pointdates[$irgrd,3]} != 0 ]
        then
          pointfiles[$irgrd]="$cur_dir/points_${irgrd}.list"
          rm -f ${pointfiles[$irgrd]}
          il=$(($il+1))
          tmpname="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
          while [ "$tmpname" != "STOPSTRING" ]
          do
            echo ${lines[$il]} >> ${pointfiles[$irgrd]}
            il=$(($il+1))
            tmpname="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
          done
        fi

      # track output
      elif [ "${grdnum}" = "3" ] 
      then
        il=$(($il+1))
        for i in $(seq 1 5)
        do
          trackdates[$irgrd,$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
        done
        if [ ${trackdates[$irgrd,3]} != 0 ]
        then
          il=$(($il+1))
          trackflags[$irgrd]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
        fi

      # restart output
      elif [ "${grdnum}" = "4" ] 
      then
        il=$(($il+1))
        for i in $(seq 1 5)
        do
          restartdates[$irgrd,$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
        done

      # boundary output
      elif [ "${grdnum}" = "5" ] 
      then
        il=$(($il+1))
        for i in $(seq 1 5)
        do
          boundarydates[$irgrd,$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
        done

      # partition output
      elif [ "${grdnum}" = "6" ] 
      then
        il=$(($il+1))
        for i in $(seq 1 5)
        do
          partitiondates[$irgrd,$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
        done
        if [ ${partitiondates[$irgrd,3]} != 0 ]
        then
          il=$(($il+1))
          for i in $(seq 1 7)
          do
            partfields[$irgrd,$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
          done
        fi
      fi
    fi
  done
  echo $grdname
  il=$(($il+1))
  grdname="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  grdnum="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
done
echo $grdname


# moving grid data
il=$(($il+1))
dataname="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
imov=0
while [ "$dataname" != 'STP' ]
do
  imov=$(($imov + 1))
  echo $dataname
  homogmov[$imov,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  homogmov[$imov,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  homogmov[$imov,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  homogmov[$imov,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
  homogmov[$imov,5]="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
  il=$(($il+1))
  dataname="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
done
nmov=$imov
ntot=$nmov
echo $dataname




#------------------------------
# write value in a new nml file

nmlfile=$cur_dir/$(basename $inp .inp).nml

# header
cat > $nmlfile << EOF
! -------------------------------------------------------------------- !
! WAVEWATCH III ww3_multi.nml - multi-grid model                       !
! -------------------------------------------------------------------- !


EOF

# domain def namelist
cat >> $nmlfile << EOF
! -------------------------------------------------------------------- !
! Define top-level model parameters via DOMAIN_NML namelist
!
! * namelist must be terminated with /
! * definitions & defaults:
!     DOMAIN%NRINP  =  0  ! Number of grids defining input fields.
!     DOMAIN%NRGRD  =  1  ! Number of wave model grids.
!     DOMAIN%UNIPTS =  F  ! Flag for using unified point output file.
!     DOMAIN%IOSTYP =  1  ! Output server type as in ww3_shel.nml
!     DOMAIN%UPPROC =  F  ! Flag for dedicated process for unified point output.
!     DOMAIN%PSHARE =  F  ! Flag for grids sharing dedicated output processes.
!     DOMAIN%FLGHG1 =  F  ! Flag for masking computation in two-way nesting
!     DOMAIN%FLGHG2 =  F  ! Flag for masking at printout time
!     DOMAIN%START  = '19680606 000000'  ! Start date for the entire model 
!     DOMAIN%STOP   = '19680607 000000'  ! Stop date for the entire model
! -------------------------------------------------------------------- !
&DOMAIN_NML
EOF

if [ "$nrinp" != 0 ];                       then  echo "  DOMAIN%NRINP  = $nrinp" >> $nmlfile; fi
if [ "$nrgrd" != 1 ];                       then  echo "  DOMAIN%NRGRD	= $nrgrd" >> $nmlfile; fi
if [ "$unipts" != "F" ];                    then  echo "  DOMAIN%UNIPTS = $unipts" >> $nmlfile; fi
if [ "$iostyp" != 1 ];                      then  echo "  DOMAIN%IOSTYP = $iostyp" >> $nmlfile; fi
if [ "$upproc" != "F" ];                    then  echo "  DOMAIN%UPPROC = $upproc" >> $nmlfile; fi
if [ "$pshare" != "F" ];                    then  echo "  DOMAIN%PSHARE = $pshare" >> $nmlfile; fi
if [ "$flghg1" != "F" ];                    then  echo "  DOMAIN%FLGHG1 = $flghg1" >> $nmlfile; fi
if [ "$flghg2" != "F" ];                    then  echo "  DOMAIN%FLGHG2 = $flghg2" >> $nmlfile; fi
if [ "${start[*]}" != "19680606 000000" ];  then  echo "  DOMAIN%START  = '${start[@]}'" >> $nmlfile; fi
if [ "${stop[*]}" != "19680607 000000" ];   then  echo "  DOMAIN%STOP   = '${stop[@]}'" >> $nmlfile; fi




# input grid namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define each input grid via the INPUT_GRID_NML namelist
!
! * index I must match indexes from 1 to DOMAIN%NRINP
! * INPUT(I)%NAME must be set for each active input grid I
!
! * namelist must be terminated with /
! * definitions & defaults:
!     INPUT(I)%NAME                  = 'unset'
!     INPUT(I)%FORCING%WATER_LEVELS  = F
!     INPUT(I)%FORCING%CURRENTS      = F
!     INPUT(I)%FORCING%WINDS         = F
!     INPUT(I)%FORCING%ICE_CONC      = F
!     INPUT(I)%FORCING%ICE_PARAM1    = F
!     INPUT(I)%FORCING%ICE_PARAM2    = F
!     INPUT(I)%FORCING%ICE_PARAM3    = F
!     INPUT(I)%FORCING%ICE_PARAM4    = F
!     INPUT(I)%FORCING%ICE_PARAM5    = F
!     INPUT(I)%FORCING%MUD_DENSITY   = F
!     INPUT(I)%FORCING%MUD_THICKNESS = F
!     INPUT(I)%FORCING%MUD_VISCOSITY = F
!     INPUT(I)%ASSIM%MEAN            = F
!     INPUT(I)%ASSIM%SPEC1D          = F
!     INPUT(I)%ASSIM%SPEC2D          = F
! -------------------------------------------------------------------- !
&INPUT_GRID_NML
EOF

for irinp in $(seq 1 $nrinp)
do
  if [ "${inpid[$irinp]}" != 'unset' ] 
  then
                                              echo "  INPUT($irinp)%NAME                  = '${inpid[$irinp]}'" >> $nmlfile
    if [ "${flginp[$irinp,1]}" != 'F' ];then  echo "  INPUT($irinp)%FORCING%WATER_LEVELS  = ${flginp[$irinp,1]}" >> $nmlfile; fi
    if [ "${flginp[$irinp,2]}" != 'F' ];then  echo "  INPUT($irinp)%FORCING%CURRENTS      = ${flginp[$irinp,2]}" >> $nmlfile; fi
    if [ "${flginp[$irinp,3]}" != 'F' ];then  echo "  INPUT($irinp)%FORCING%WINDS         = ${flginp[$irinp,3]}" >> $nmlfile; fi
    if [ "${flginp[$irinp,4]}" != 'F' ];then  echo "  INPUT($irinp)%FORCING%ICE_CONC      = ${flginp[$irinp,4]}" >> $nmlfile; fi
    if [ "${flginp[$irinp,5]}" != 'F' ];then  echo "  INPUT($irinp)%ASSIM%MEAN            = ${flginp[$irinp,5]}" >> $nmlfile; fi
    if [ "${flginp[$irinp,6]}" != 'F' ];then  echo "  INPUT($irinp)%ASSIM%SPEC1D          = ${flginp[$irinp,6]}" >> $nmlfile; fi
    if [ "${flginp[$irinp,7]}" != 'F' ];then  echo "  INPUT($irinp)%ASSIM%SPEC2D          = ${flginp[$irinp,7]}" >> $nmlfile; fi
  fi
done


# model grid namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define each model grid via the MODEL_GRID_NML namelist
!
! * index I must match indexes from 1 to DOMAIN%NRGRD
! * MODEL(I)%NAME must be set for each active model grid I
! * FORCING can be set as :
!    - 'no'          : This input is not used.
!    - 'native'      : This grid has its own input files, e.g. grid
!                      grdX (mod_def.grdX) uses ice.grdX.
!    - 'INPUT%NAME'  : Take input from the grid identified by
!                      INPUT%NAME.
! * RESOURCE%RANK_ID : Rank number of grid (internally sorted and reassigned).
! * RESOURCE%GROUP_ID : Group number (internally reassigned so that different
!                                     ranks result in different group numbers).
! * RESOURCE%COMM_FRAC : Fraction of communicator (processes) used for this grid.
! * RESOURCE%BOUND_FLAG : Flag identifying dumping of boundary data used by this
!                         grid. If true, the file nest.MODID is generated.
!
! * Limitations relevant to irregular (curvilinear) grids:
!   1) Equal rank is not supported when one or more is an irregular
!       grid. Use non-equal rank instead. (see wmgridmd.ftn)
!   2) Non-native input grids: feature is not supported when either
!      an input grid or computational grids is irregular.
!      (see wmupdtmd.ftn)
!   3) Irregular grids with unified point output: This is supported
!      but the feature has not been verified for accuracy.
!      (see wmiopomd.ftn)
!
! * namelist must be terminated with /
! * definitions & defaults:
!     MODEL(I)%NAME                  = 'unset'
!     MODEL(I)%FORCING%WATER_LEVELS  = 'no'
!     MODEL(I)%FORCING%CURRENTS      = 'no'
!     MODEL(I)%FORCING%WINDS         = 'no'
!     MODEL(I)%FORCING%ICE_CONC      = 'no'
!     MODEL(I)%FORCING%ICE_PARAM1    = 'no'
!     MODEL(I)%FORCING%ICE_PARAM2    = 'no'
!     MODEL(I)%FORCING%ICE_PARAM3    = 'no'
!     MODEL(I)%FORCING%ICE_PARAM4    = 'no'
!     MODEL(I)%FORCING%ICE_PARAM5    = 'no'
!     MODEL(I)%FORCING%MUD_DENSITY   = 'no'
!     MODEL(I)%FORCING%MUD_THICKNESS = 'no'
!     MODEL(I)%FORCING%MUD_VISCOSITY = 'no'
!     MODEL(I)%ASSIM%MEAN            = 'no'
!     MODEL(I)%ASSIM%SPEC1d          = 'no'
!     MODEL(I)%ASSIM%SPEC2d          = 'no'
!     MODEL(I)%RESOURCE%RANK_ID      = I
!     MODEL(I)%RESOURCE%GROUP_ID     = 1
!     MODEL(I)%RESOURCE%COMM_FRAC    = 0.00,1.00
!     MODEL(I)%RESOURCE%BOUND_FLAG   = F
!
!     MODEL(4)%FORCING = 'no' 'no' 'no' 'no' 'no' 'no'
!
!     MODEL(2)%RESOURCE = 1 1 0.00 1.00 F
! -------------------------------------------------------------------- !
&MODEL_GRID_NML
EOF

for irgrd in $(seq 1 $nrgrd)
do
  if [ "${modid[$irgrd]}" != 'unset' ] 
  then
                                                echo "  MODEL($irgrd)%NAME                  = '${modid[$irgrd]}'" >> $nmlfile
    if [ "${flggrd[$irgrd,1]}" != 'no' ]; then  echo "  MODEL($irgrd)%FORCING%WATER_LEVELS  = '${flggrd[$irgrd,1]}'" >> $nmlfile; fi
    if [ "${flggrd[$irgrd,2]}" != 'no' ]; then  echo "  MODEL($irgrd)%FORCING%CURRENTS      = '${flggrd[$irgrd,2]}'" >> $nmlfile; fi
    if [ "${flggrd[$irgrd,3]}" != 'no' ]; then  echo "  MODEL($irgrd)%FORCING%WINDS         = '${flggrd[$irgrd,3]}'" >> $nmlfile; fi
    if [ "${flggrd[$irgrd,4]}" != 'no' ]; then  echo "  MODEL($irgrd)%FORCING%ICE_CONC      = '${flggrd[$irgrd,4]}'" >> $nmlfile; fi
    if [ "${flggrd[$irgrd,5]}" != 'no' ]; then  echo "  MODEL($irgrd)%ASSIM%MEAN            = '${flggrd[$irgrd,5]}'" >> $nmlfile; fi
    if [ "${flggrd[$irgrd,6]}" != 'no' ]; then  echo "  MODEL($irgrd)%ASSIM%SPEC1D          = '${flggrd[$irgrd,6]}'" >> $nmlfile; fi
    if [ "${flggrd[$irgrd,7]}" != 'no' ]; then  echo "  MODEL($irgrd)%ASSIM%SPEC2D          = '${flggrd[$irgrd,7]}'" >> $nmlfile; fi
    if [ "${rank[$irgrd]}" != "$irgrd" ]; then  echo "  MODEL($irgrd)%RESOURCE%RANK_ID      = ${rank[$irgrd]}" >> $nmlfile; fi
    if [ "${group[$irgrd]}" != 1 ];       then  echo "  MODEL($irgrd)%RESOURCE%GROUP_ID     = ${group[$irgrd]}" >> $nmlfile; fi
    if [ "${comm0[$irgrd]},${comm1[$irgrd]}" != '0.00,1.00' ];then  
                                                echo "  MODEL($irgrd)%RESOURCE%COMM_FRAC    = ${comm0[$irgrd]},${comm1[$irgrd]}" >> $nmlfile; fi
    if [ "${bound[$irgrd]}" != 'F' ];     then  echo "  MODEL($irgrd)%RESOURCE%BOUND_FLAG   = ${bound[$irgrd]}" >> $nmlfile; fi
  fi
done


# output type namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the output types point parameters via OUTPUT_TYPE_NML namelist
!
! * index I must match indexes from 1 to DOMAIN%NRGRD
!
! * ALLTYPE will apply the output types for all the model grids
!
! * ITYPE(I) will apply the output types for the model grid number I
!
! * need DOMAIN%UNIPTS equal true to use a unified point output file
!
! * the point file is a space separated values per line :
!   longitude latitude 'name' (C*40)
!
! * the detailed list of field names is given in model/nml/ww3_shel.nml :
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
! * output track file formatted (T) or unformated (F)
!
! * namelist must be terminated with /
! * definitions & defaults:
!     ALLTYPE%FIELD%LIST         =  'unset'
!     ALLTYPE%POINT%NAME         =  'unset'
!     ALLTYPE%POINT%FILE         =  'points.list'
!     ALLTYPE%TRACK%FORMAT       =  T
!     ALLTYPE%PARTITION%X0       =  0
!     ALLTYPE%PARTITION%XN       =  0
!     ALLTYPE%PARTITION%NX       =  0
!     ALLTYPE%PARTITION%Y0       =  0
!     ALLTYPE%PARTITION%YN       =  0
!     ALLTYPE%PARTITION%NY       =  0
!     ALLTYPE%PARTITION%FORMAT   =  T
!
!     ITYPE(3)%TRACK%FORMAT      =  F
! -------------------------------------------------------------------- !
&OUTPUT_TYPE_NML
EOF

if [ "${fielddate[3]}" != 0 ];    then  echo "  ALLTYPE%FIELD%LIST       = '$fieldlist'" >> $nmlfile; fi

if [ "${pointdate[3]}" != 0 ]
then
  if [ "$pointname" != 'unset' ] && [ "$unipts" = "T" ]; then  
                                        echo "  ALLTYPE%POINT%NAME       = '$pointname'" >> $nmlfile; fi
  if [ "$point_filename" != 'points.list' ]; then  echo "  ALLTYPE%POINT%FILE       = '$point_filename'" >> $nmlfile; fi
fi

if [ "${trackdate[3]}" != 0 ] && [ "$trackflag" != T ];    then  
                                        echo "  ALLTYPE%TRACK%FORMAT     = $trackflag" >> $nmlfile; fi
if [ "${partitiondate[3]}" != 0 ]
then
#  if [ "${partfield[1]}" != 0 ];  then  echo "  ALLTYPE%PARTITION%X0     = ${partfield[1]}" >> $nmlfile; fi
#  if [ "${partfield[2]}" != 0 ];  then  echo "  ALLTYPE%PARTITION%XN     = ${partfield[2]}" >> $nmlfile; fi
#  if [ "${partfield[3]}" != 0 ];  then  echo "  ALLTYPE%PARTITION%NX     = ${partfield[3]}" >> $nmlfile; fi
#  if [ "${partfield[4]}" != 0 ];  then  echo "  ALLTYPE%PARTITION%Y0     = ${partfield[4]}" >> $nmlfile; fi
#  if [ "${partfield[5]}" != 0 ];  then  echo "  ALLTYPE%PARTITION%YN     = ${partfield[5]}" >> $nmlfile; fi
#  if [ "${partfield[6]}" != 0 ];  then  echo "  ALLTYPE%PARTITION%NY     = ${partfield[6]}" >> $nmlfile; fi
#  if [ "${partfield[7]}" != T ];  then  echo "  ALLTYPE%PARTITION%FORMAT = ${partfield[7]}" >> $nmlfile; fi
  if [ "${partfield[1]}" != 0 ] || [ "${partfield[2]}" != 0 ] || [ "${partfield[3]}" != 0 ] || [ "${partfield[4]}" != 0 ] || \
     [ "${partfield[5]}" != 0 ] || [ "${partfield[6]}" != 0 ] || [ "${partfield[7]}" != T ];  then
  echo "  ALLTYPE%PARTITION        = ${partfield[1]} ${partfield[2]} ${partfield[3]} ${partfield[4]} ${partfield[5]} ${partfield[6]} ${partfield[7]}" >> $nmlfile; fi
fi

for irgrd in $(seq 1 $nrgrd)
do
  if [ "${fieldlist}" != "${fieldlists[$irgrd]}" ]; then
    echo "  ITYPE(${irgrd})%FIELD%LIST      = ${fieldlists[$irgrd]}" >> $nmlfile; fi

  if [ "${pointfile}" != "${pointfiles[$irgrd]}" ]; then
    echo "  ITYPE(${irgrd})%POINT%FILE      = '${pointfiles[$irgrd]}'" >> $nmlfile; fi

  if [ "${trackflag}" != "${trackflags[$irgrd]}" ]; then
    echo "  ITYPE(${irgrd})%TRACK%FORMAT    = ${trackflags[$irgrd]}" >> $nmlfile; fi

#  if [ "${partfield[1]}" != "${partfields[$irgrd,1]}" ]; then
#    echo "  ITYPE(${irgrd})%PARTITION%X0     = ${partfields[$irgrd,1]}" >> $nmlfile; fi
#  if [ "${partfield[2]}" != "${partfields[$irgrd,2]}" ]; then
#    echo "  ITYPE(${irgrd})%PARTITION%XN     = ${partfields[$irgrd,2]}" >> $nmlfile; fi
#  if [ "${partfield[3]}" != "${partfields[$irgrd,3]}" ]; then
#    echo "  ITYPE(${irgrd})%PARTITION%NX     = ${partfields[$irgrd,3]}" >> $nmlfile; fi
#  if [ "${partfield[4]}" != "${partfields[$irgrd,4]}" ]; then
#    echo "  ITYPE(${irgrd})%PARTITION%Y0     = ${partfields[$irgrd,4]}" >> $nmlfile; fi
#  if [ "${partfield[5]}" != "${partfields[$irgrd,5]}" ]; then
#    echo "  ITYPE(${irgrd})%PARTITION%YN     = ${partfields[$irgrd,5]}" >> $nmlfile; fi
#  if [ "${partfield[6]}" != "${partfields[$irgrd,6]}" ]; then
#    echo "  ITYPE(${irgrd})%PARTITION%NY     = ${partfields[$irgrd,6]}" >> $nmlfile; fi
#  if [ "${partfield[7]}" != "${partfields[$irgrd,7]}" ]; then
#    echo "  ITYPE(${irgrd})%PARTITION%FORMAT = ${partfields[$irgrd,7]}" >> $nmlfile; fi
  if [ "${partfield[1]}" != "${partfields[$irgrd,1]}" ] || [ "${partfield[2]}" != "${partfields[$irgrd,2]}" ] || \
     [ "${partfield[3]}" != "${partfields[$irgrd,3]}" ] || [ "${partfield[4]}" != "${partfields[$irgrd,4]}" ] || \
     [ "${partfield[5]}" != "${partfields[$irgrd,5]}" ] || [ "${partfield[6]}" != "${partfields[$irgrd,6]}" ] || \
     [ "${partfield[7]}" != "${partfields[$irgrd,7]}" ]; then
  echo "  ITYPE(${irgrd})%PARTITION       = ${partfields[$irgrd,1]} ${partfields[$irgrd,2]} ${partfields[$irgrd,3]} ${partfields[$irgrd,4]} ${partfields[$irgrd,5]} ${partfields[$irgrd,6]} ${partfields[$irgrd,7]}" >> $nmlfile; fi

done

# output date namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define output dates via OUTPUT_DATE_NML namelist
!
! * index I must match indexes from 1 to DOMAIN%NRGRD
! * ALLDATE will apply the output dates for all the model grids
! * IDATE(I) will apply the output dates for the model grid number i
! * start and stop times are with format 'yyyymmdd hhmmss'
! * if time stride is equal '0', then output is disabled
! * time stride is given in seconds
! * it is possible to overwrite a global output date for a given grid
!
! * namelist must be terminated with /
! * definitions & defaults:
!     ALLDATE%FIELD%START         =  '19680606 000000'
!     ALLDATE%FIELD%STRIDE        =  '0'
!     ALLDATE%FIELD%STOP          =  '19680607 000000'
!     ALLDATE%POINT%START         =  '19680606 000000'
!     ALLDATE%POINT%STRIDE        =  '0'
!     ALLDATE%POINT%STOP          =  '19680607 000000'
!     ALLDATE%TRACK%START         =  '19680606 000000'
!     ALLDATE%TRACK%STRIDE        =  '0'
!     ALLDATE%TRACK%STOP          =  '19680607 000000'
!     ALLDATE%RESTART%START       =  '19680606 000000'
!     ALLDATE%RESTART%STRIDE      =  '0'
!     ALLDATE%RESTART%STOP        =  '19680607 000000'
!     ALLDATE%RESTART2%START      =  '19680606 000000'
!     ALLDATE%RESTART2%STRIDE     =  '0'
!     ALLDATE%RESTART2%STOP       =  '19680607 000000'
!     ALLDATE%BOUNDARY%START      =  '19680606 000000'
!     ALLDATE%BOUNDARY%STRIDE     =  '0'
!     ALLDATE%BOUNDARY%STOP       =  '19680607 000000'
!     ALLDATE%PARTITION%START     =  '19680606 000000'
!     ALLDATE%PARTITION%STRIDE    =  '0'
!     ALLDATE%PARTITION%STOP      =  '19680607 000000'
!     
!     ALLDATE%RESTART             =  '19680606 000000' '0' '19680607 000000'
!
!     IDATE(3)%PARTITION%START    =  '19680606 000000' 
! -------------------------------------------------------------------- !
&OUTPUT_DATE_NML
EOF

if [ "${fielddate[3]}" != '0' ]; then  
      echo "  ALLDATE%FIELD          = '${fielddate[1]} ${fielddate[2]}' '${fielddate[3]}' '${fielddate[4]} ${fielddate[5]}'" >> $nmlfile; fi

if [ "${pointdate[3]}" != '0' ]; then  
      echo "  ALLDATE%POINT          = '${pointdate[1]} ${pointdate[2]}' '${pointdate[3]}' '${pointdate[4]} ${pointdate[5]}'" >> $nmlfile; fi

if [ "${trackdate[3]}" != '0' ]; then  
      echo "  ALLDATE%TRACK          = '${trackdate[1]} ${trackdate[2]}' '${trackdate[3]}' '${trackdate[4]} ${trackdate[5]}'" >> $nmlfile; fi

if [ "${restartdate[3]}" != '0' ]; then  
      echo "  ALLDATE%RESTART        = '${restartdate[1]} ${restartdate[2]}' '${restartdate[3]}' '${restartdate[4]} ${restartdate[5]}'" >> $nmlfile; fi

if [ "${restartdate2[3]}" != '0' ]; then  
      echo "  ALLDATE%RESTART2       = '${restartdate2[1]} ${restartdate2[2]}' '${restartdate2[3]}' '${restartdate[4]} ${restartdate[5]}'" >> $nmlfile; fi

if [ "${boundarydate[3]}" != '0' ]; then  
      echo "  ALLDATE%BOUNDARY       = '${boundarydate[1]} ${boundarydate[2]}' '${boundarydate[3]}' '${boundarydate[4]} ${boundarydate[5]}'" >> $nmlfile; fi

if [ "${partitiondate[3]}" != '0' ]; then  
      echo "  ALLDATE%PARTITION      = '${partitiondate[1]} ${partitiondate[2]}' '${partitiondate[3]}' '${partitiondate[4]} ${partitiondate[5]}'" >> $nmlfile; fi


for irgrd in $(seq 1 $nrgrd)
do
  if [ "${fielddate[1]}" != "${fielddates[$irgrd,1]}" ] || [ "${fielddate[2]}" != "${fielddates[$irgrd,2]}" ] || \
     [ "${fielddate[3]}" != "${fielddates[$irgrd,3]}" ] || [ "${fielddate[4]}" != "${fielddates[$irgrd,4]}" ] || \
     [ "${fielddate[5]}" != "${fielddates[$irgrd,5]}" ]; then  
        echo "  IDATE($irgrd)%FIELD         = '${fielddates[$irgrd,1]} ${fielddates[$irgrd,2]}' '${fielddates[$irgrd,3]}' '${fielddates[$irgrd,4]} ${fielddates[$irgrd,5]}'" >> $nmlfile; fi

  if [ "${pointdate[1]}" != "${pointdates[$irgrd,1]}" ] || [ "${pointdate[2]}" != "${pointdates[$irgrd,2]}" ] || \
     [ "${pointdate[3]}" != "${pointdates[$irgrd,3]}" ] || [ "${pointdate[4]}" != "${pointdates[$irgrd,4]}" ] || \
     [ "${pointdate[5]}" != "${pointdates[$irgrd,5]}" ]; then  
        echo "  IDATE($irgrd)%POINT         = '${pointdates[$irgrd,1]} ${pointdates[$irgrd,2]}' '${pointdates[$irgrd,3]}' '${pointdates[$irgrd,4]} ${pointdates[$irgrd,5]}'" >> $nmlfile; fi

  if [ "${trackdate[1]}" != "${trackdates[$irgrd,1]}" ] || [ "${trackdate[2]}" != "${trackdates[$irgrd,2]}" ] || \
     [ "${trackdate[3]}" != "${trackdates[$irgrd,3]}" ] || [ "${trackdate[4]}" != "${trackdates[$irgrd,4]}" ] || \
     [ "${trackdate[5]}" != "${trackdates[$irgrd,5]}" ]; then  
        echo "  IDATE($irgrd)%TRACK         = '${trackdates[$irgrd,1]} ${trackdates[$irgrd,2]}' '${trackdates[$irgrd,3]}' '${trackdates[$irgrd,4]} ${trackdates[$irgrd,5]}'" >> $nmlfile; fi

  if [ "${restartdate[1]}" != "${restartdates[$irgrd,1]}" ] || [ "${restartdate[2]}" != "${restartdates[$irgrd,2]}" ] || \
     [ "${restartdate[3]}" != "${restartdates[$irgrd,3]}" ] || [ "${restartdate[4]}" != "${restartdates[$irgrd,4]}" ] || \
     [ "${restartdate[5]}" != "${restartdates[$irgrd,5]}" ]; then   
        echo "  IDATE($irgrd)%RESTART       = '${restartdates[$irgrd,1]} ${restartdates[$irgrd,2]}' '${restartdates[$irgrd,3]}' '${restartdates[$irgrd,4]} ${restartdates[$irgrd,5]}'" >> $nmlfile; fi

  if [ "${restartdate2[1]}" != "${restartdates2[$irgrd,1]}" ] || [ "${restartdate2[2]}" != "${restartdates2[$irgrd,2]}" ] || \
     [ "${restartdate2[3]}" != "${restartdates2[$irgrd,3]}" ] || [ "${restartdate2[4]}" != "${restartdates2[$irgrd,4]}" ] || \
     [ "${restartdate2[5]}" != "${restartdates2[$irgrd,5]}" ]; then   
        echo "  IDATE($irgrd)%RESTART2      = '${restartdates2[$irgrd,1]} ${restartdates2[$irgrd,2]}' '${restartdates2[$irgrd,3]}' '${restartdates2[$irgrd,4]} ${restartdates2[$irgrd,5]}'" >> $nmlfile; fi

  if [ "${boundarydate[1]}" != "${boundarydates[$irgrd,1]}" ] || [ "${boundarydate[2]}" != "${boundarydates[$irgrd,2]}" ] || \
     [ "${boundarydate[3]}" != "${boundarydates[$irgrd,3]}" ] || [ "${boundarydate[4]}" != "${boundarydates[$irgrd,4]}" ] || \
     [ "${boundarydate[5]}" != "${boundarydates[$irgrd,5]}" ]; then  
        echo "  IDATE($irgrd)%BOUNDARY      = '${boundarydates[$irgrd,1]} ${boundarydates[$irgrd,2]}' '${boundarydates[$irgrd,3]}' '${boundarydates[$irgrd,4]} ${boundarydates[$irgrd,5]}'" >> $nmlfile; fi

  if [ "${partitiondate[1]}" != "${partitiondates[$irgrd,1]}" ] || [ "${partitiondate[2]}" != "${partitiondates[$irgrd,2]}" ] || \
     [ "${partitiondate[3]}" != "${partitiondates[$irgrd,3]}" ] || [ "${partitiondate[4]}" != "${partitiondates[$irgrd,4]}" ] || \
     [ "${partitiondate[5]}" != "${partitiondates[$irgrd,5]}" ]; then  
        echo "  IDATE($irgrd)%PARTITION     = '${partitiondates[$irgrd,1]} ${partitiondates[$irgrd,2]}' '${partitiondates[$irgrd,3]}' '${partitiondates[$irgrd,4]} ${partitiondates[$irgrd,5]}'" >> $nmlfile; fi
done

# homogeneous input namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define homogeneous input via HOMOG_COUNT_NML and HOMOG_INPUT_NML namelist
!
! * the number of each homogeneous input is defined by HOMOG_COUNT
! * the total number of homogeneous input is automatically calculated
! * the homogeneous input must start from index 1 to N
! * if VALUE1 is equal 0, then the homogeneous input is desactivated
! * NAME can only be MOV
! * each homogeneous input is defined over a maximum of 3 values detailled below :
!     - MOV is defined by speed and direction
!
! * namelist must be terminated with /
! * definitions & defaults:
!     HOMOG_COUNT%N_MOV            =  0
!
!     HOMOG_INPUT(I)%NAME           =  'unset'
!     HOMOG_INPUT(I)%DATE           =  '19680606 000000'
!     HOMOG_INPUT(I)%VALUE1         =  0
!     HOMOG_INPUT(I)%VALUE2         =  0
!     HOMOG_INPUT(I)%VALUE3         =  0
! -------------------------------------------------------------------- !
&HOMOG_COUNT_NML
EOF

if [ $ntot -gt 0 ] ; then

  if [ $nmov -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_MOV                =  $nmov" >> $nmlfile; fi
fi

cat >> $nmlfile << EOF
/

&HOMOG_INPUT_NML
EOF

itot=0
if [ $ntot -gt 0 ] ; then

  if [ $nmov -gt 0 ] ; then
    for imov in $(seq 1 $nmov)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'MOV'" >> $nmlfile
      if [ "${homogmov[$imov,2]} ${homogmov[$imov,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homogmov[$imov,2]} ${homogmov[$imov,3]}'" >> $nmlfile; fi
      if [ "${homogmov[$imov,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homogmov[$imov,4]}" >> $nmlfile; fi
      if [ "${homogmov[$imov,5]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE2      = ${homogmov[$imov,5]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

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


