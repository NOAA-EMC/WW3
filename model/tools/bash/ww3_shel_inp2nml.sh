#!/bin/bash -e

prog="ww3_shel"

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


declare -A forc
declare -A flginp
declare -A flggrd
declare -A fielddate
declare -A fieldlist
declare -A pointdate
declare -A pointfile
declare -A trackdate
declare -A trackflag
declare -A restartdate
declare -A restart2date
declare -A boundarydate
declare -A partitiondate
declare -A partfield
declare -A homogic1
declare -A homogic2
declare -A homogic3
declare -A homogic4
declare -A homogic5
declare -A homogmdn
declare -A homogmth
declare -A homogmvs
declare -A homogcur
declare -A homoglev
declare -A homogwnd
declare -A homogice
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

# forcing
forc[1,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
forc[1,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo ${forc[1,1]} ${forc[1,2]}
il=$(($il+1))
forc[2,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
forc[2,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo ${forc[2,1]} ${forc[2,2]}
il=$(($il+1))
forc[3,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
forc[3,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo ${forc[3,1]} ${forc[3,2]}
il=$(($il+1))
forc[4,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
forc[4,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo ${forc[4,1]} ${forc[4,2]}
il=$(($il+1))
forc[5,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
forc[5,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo ${forc[5,1]} ${forc[5,2]}
il=$(($il+1))
forc[6,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
forc[6,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo ${forc[6,1]} ${forc[6,2]}
il=$(($il+1))
forc[7,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
forc[7,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo ${forc[7,1]} ${forc[7,2]}
if [ "${forc[7,2]}" = "T" ] || [ "${forc[7,2]}" = "F" ] || [ "${forc[7,2]}" = "C" ]
then
  mudice=T
  echo 'mudice'
  il=$(($il+1))
  forc[8,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  forc[8,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  echo ${forc[8,1]} ${forc[8,2]}
  il=$(($il+1))
  forc[9,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  forc[9,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  echo ${forc[9,1]} ${forc[9,2]}
  il=$(($il+1))
  forc[10,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  forc[10,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  echo ${forc[10,1]} ${forc[10,2]}
  il=$(($il+1))
  forc[11,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  forc[11,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  echo ${forc[11,1]} ${forc[11,2]}
  il=$(($il+1))
  forc[12,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  forc[12,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  echo ${forc[12,1]} ${forc[12,2]}
  il=$(($il+1))
  forc[13,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  forc[13,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  echo ${forc[13,1]} ${forc[13,2]}
  il=$(($il+1))
  forc[14,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  forc[14,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  echo ${forc[14,1]} ${forc[14,2]}
  il=$(($il+1))
  forc[15,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  forc[15,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  echo ${forc[15,1]} ${forc[15,2]}
  if [ "${forc[13,2]}" = "T" ] || [ "${forc[13,2]}" = "F" ] || [ "${forc[13,2]}" = "C" ]
  then
    atmos=T
    echo 'atmos'
    il=$(($il+1))
    forc[16,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    forc[16,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${forc[16,1]} ${forc[16,2]}
    il=$(($il+1))
    forc[17,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    forc[17,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${forc[17,1]} ${forc[17,2]}
  else
    atmos=F
    echo 'no atmos'
  fi
else
  mudice=F
  echo 'no mudice'
  if [ "${forc[5,2]}" = "T" ] || [ "${forc[5,2]}" = "F" ] || [ "${forc[5,2]}" = "C" ]
  then
    atmos=T
    echo 'atmos'
    il=$(($il+1))
    forc[8,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    forc[8,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${forc[8,1]} ${forc[8,2]}
    il=$(($il+1))
    forc[9,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    forc[9,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${forc[9,1]} ${forc[9,2]}
  else
    atmos=F
    echo 'no atmos'
  fi
fi



# model time
echo 'model time'
il=$(($il+1))
timestart[1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
timestart[2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo ${timestart[@]}
il=$(($il+1))
timestop[1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
timestop[2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
echo ${timestop[@]}

# iostyp
il=$(($il+1))
iostyp="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo "iostyp : $iostyp"


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
  fieldformat="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"

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
  foripoint="$cur_dir/points.list"
  fpoint="$cur_dir/points.list.new"
  point_filename=$foripoint
  rm -f $fpoint
  il=$(($il+1))
  tmpname="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  while [ "${tmpname}" != "STOPSTRING" ]
  do
    echo ${lines[$il]} >> $fpoint
    il=$(($il+1))
    tmpname="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  done
  echo ${tmpname}
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
  trackflag="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
fi


# restart date
echo 'restart date'
il=$(($il+1))
for i in $(seq 1 5)
do
  restartdate[$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
done
echo ${restartdate[@]}
restart2="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
echo $restart2
extra="$(echo ${lines[$il]} | awk -F' ' '{print $7}' | cut -d \" -f2  | cut -d \' -f2)"
echo $extra
if [ "$restart2" = 'T' ]; then
  il=$(($il+1))
  for i in $(seq 1 5)
  do
    restart2date[$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
  done
  echo ${restart2date[@]}
fi
if [ "$extra" = 'T' ]; then
  il=$(($il+1))
  extrafields="$(echo ${lines[$il]} | cut -d \" -f2  | cut -d \' -f2)"
  echo $extrafields
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

# coupling date
il=$(($il+1))
yyyymmdd=$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)
if [ ${#yyyymmdd} -eq 8 ]
then
  coupling=T
  echo 'coupling date'
  for i in $(seq 1 5)
  do
    couplingdate[$i]="$(echo ${lines[$il]} | awk -F' ' "{print \$$i}" | cut -d \" -f2  | cut -d \' -f2)"
  done
  echo ${couplingdate[@]}
  couplet0="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
  echo 'couplet0 : ' $couplet0
  il=$(($il+1))
  nml="$(echo ${lines[$il]} | cut -d \" -f2  | cut -d \' -f2)"
  il=$(($il+1))
  sent="$(echo ${lines[$il]} | cut -d \" -f2  | cut -d \' -f2)"
  il=$(($il+1))
  received="$(echo ${lines[$il]} | cut -d \" -f2  | cut -d \' -f2)"
  il=$(($il+1))
else
  coupling=F
  echo 'no coupling'
fi



# homogeneous data
echo "homogeneous"
#il=$(($il+1)) - <already done for coupling test>
if [ -z "${lines[$il]}" ]
then
  echo "[WARNING] missing 'STP' for homogeneous input"
  lines[$il]='STP'
fi
dataname="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"

iic1=0
iic2=0
iic3=0
iic4=0
iic5=0
imdn=0
imth=0
imvs=0
icur=0
ilev=0
iwnd=0
iice=0
imov=0
while [ "$dataname" != 'STP' ]
do
  if [ "$dataname" = "IC1" ]
  then
    iic1=$(( $iic1 + 1 ))
    homogic1[$iic1,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic1[$iic1,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic1[$iic1,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic1[$iic1,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homogic1[$iic1,1]} ${homogic1[$iic1,2]} ${homogic1[$iic1,3]} ${homogic1[$iic1,4]}

  elif [ "$dataname" = "IC2" ]
  then
    iic2=$(( $iic2 + 1 ))
    homogic2[$iic2,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic2[$iic2,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic2[$iic2,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic2[$iic2,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homogic2[$iic2,1]} ${homogic2[$iic2,2]} ${homogic2[$iic2,3]} ${homogic2[$iic2,4]}

  elif [ "$dataname" = "IC3" ]
  then
    iic3=$(( $iic3 + 1 ))
    homogic3[$iic3,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic3[$iic3,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic3[$iic3,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic3[$iic3,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homogic3[$iic3,1]} ${homogic3[$iic3,2]} ${homogic3[$iic3,3]} ${homogic3[$iic3,4]}

  elif [ "$dataname" = "IC4" ]
  then
    iic4=$(( $iic4 + 1 ))
    homogic4[$iic4,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic4[$iic4,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic4[$iic4,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic4[$iic4,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homogic4[$iic4,1]} ${homogic4[$iic4,2]} ${homogic4[$iic4,3]} ${homogic4[$iic4,4]}

  elif [ "$dataname" = "IC5" ]
  then
    iic5=$(( $iic5 + 1 ))
    homogic5[$iic5,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic5[$iic5,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic5[$iic5,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homogic5[$iic5,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homogic5[$iic5,1]} ${homogic5[$iic5,2]} ${homogic5[$iic5,3]} ${homogic5[$iic5,4]}

  elif [ "$dataname" = "MDN" ]
  then
    imdn=$(( $imdn + 1 ))
    homogmdn[$imdn,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmdn[$imdn,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmdn[$imdn,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmdn[$imdn,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homogmdn[$imdn,1]} ${homogmdn[$imdn,2]} ${homogmdn[$imdn,3]} ${homogmdn[$imdn,4]}

  elif [ "$dataname" = "MTH" ]
  then
    imth=$(( $imth + 1 ))
    homogmth[$imth,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmth[$imth,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmth[$imth,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmth[$imth,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homogmth[$imth,1]} ${homogmth[$imth,2]} ${homogmth[$imth,3]} ${homogmth[$imth,4]}

  elif [ "$dataname" = "MVS" ]
  then
    imvs=$(( $imvs + 1 ))
    homogmvs[$imvs,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmvs[$imvs,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmvs[$imvs,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmvs[$imvs,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homogmvs[$imvs,1]} ${homogmvs[$imvs,2]} ${homogmvs[$imvs,3]} ${homogmvs[$imvs,4]}

  elif [ "$dataname" = "CUR" ]
  then
    icur=$(( $icur + 1 ))
    homogcur[$icur,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homogcur[$icur,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homogcur[$icur,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homogcur[$icur,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    homogcur[$icur,5]="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homogcur[$icur,1]} ${homogcur[$icur,2]} ${homogcur[$icur,3]} ${homogcur[$icur,4]} ${homogcur[$icur,5]}

  elif [ "$dataname" = "LEV" ]
  then
    ilev=$(( $ilev + 1 ))
    homoglev[$ilev,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homoglev[$ilev,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homoglev[$ilev,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homoglev[$ilev,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homoglev[$ilev,1]} ${homoglev[$ilev,2]} ${homoglev[$ilev,3]} ${homoglev[$ilev,4]}

  elif [ "$dataname" = "WND" ]
  then
    iwnd=$(( $iwnd + 1 ))
    homogwnd[$iwnd,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homogwnd[$iwnd,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homogwnd[$iwnd,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homogwnd[$iwnd,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    homogwnd[$iwnd,5]="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
    homogwnd[$iwnd,6]="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homogwnd[$iwnd,1]} ${homogwnd[$iwnd,2]} ${homogwnd[$iwnd,3]} ${homogwnd[$iwnd,4]} ${homogwnd[$iwnd,5]} ${homogwnd[$iwnd,6]}

  elif [ "$dataname" = "ICE" ]
  then
    iice=$(( $iice + 1 ))
    homogice[$iice,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homogice[$iice,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homogice[$iice,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homogice[$iice,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homogice[$iice,1]} ${homogice[$iice,2]} ${homogice[$iice,3]} ${homogice[$iice,4]}

  elif [ "$dataname" = "MOV" ]
  then
    imov=$(( $imov + 1 ))
    homogmov[$imov,1]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmov[$imov,2]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmov[$imov,3]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmov[$imov,4]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    homogmov[$imov,5]="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
    echo ${homogmov[$imov,1]} ${homogmov[$imov,2]} ${homogmov[$imov,3]} ${homogmov[$imov,4]} ${homogmov[$imov,5]}
  fi

  il=$(($il+1))
  dataname="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
done

nic1=$iic1
nic2=$iic2
nic3=$iic3
nic4=$iic4
nic5=$iic5
nmdn=$imdn
nmth=$imth
nmvs=$imvs
ncur=$icur
nlev=$ilev
nwnd=$iwnd
nice=$iice
nmov=$imov
ntot=$(( $nic1 + $nic2 + $nic3 + $nic4 + $nic5 + $nmdn + $nmth + $nmvs + $ncur + $nlev + $nwnd + $nice + $nmov ))
echo $dataname





#------------------------------
# write value in a new nml file

nmlfile=$cur_dir/$(basename $inp .inp).nml

# header
cat > $nmlfile << EOF
! -------------------------------------------------------------------- !
! WAVEWATCH III ww3_shel.nml - single-grid model                       !
! -------------------------------------------------------------------- !


EOF

# domain namelist
if [ "$comment" = "full" ]; then
cat >> $nmlfile << EOF
! -------------------------------------------------------------------- !
! Define top-level model parameters via DOMAIN_NML namelist
!
! * IOSTYP defines the output server mode for parallel implementation.
!             0 : No data server processes, direct access output from
!                 each process (requires true parallel file system).
!             1 : No data server process. All output for each type 
!                 performed by process that performs computations too.
!             2 : Last process is reserved for all output, and does no
!                 computing.
!             3 : Multiple dedicated output processes.
!
! * namelist must be terminated with /
! * definitions & defaults:
!     DOMAIN%IOSTYP =  1                 ! Output server type
!     DOMAIN%START  = '19680606 000000'  ! Start date for the entire model 
!     DOMAIN%STOP   = '19680607 000000'  ! Stop date for the entire model
! -------------------------------------------------------------------- !
&DOMAIN_NML
EOF
else
cat >> $nmlfile << EOF
! -------------------------------------------------------------------- !
! Define top-level model parameters via DOMAIN_NML namelist
! -------------------------------------------------------------------- !
&DOMAIN_NML
EOF
fi

if [ "$iostyp" != 1 ];                          then  echo "  DOMAIN%IOSTYP  = $iostyp" >> $nmlfile; fi
if [ "${timestart[*]}" != "19680606 000000" ];  then  echo "  DOMAIN%START   = '${timestart[@]}'" >> $nmlfile; fi
if [ "${timestop[*]}" != "19680607 000000" ];   then  echo "  DOMAIN%STOP    = '${timestop[@]}'" >> $nmlfile; fi




# forcing namelist
if [ "$comment" = "full" ]; then
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define each forcing via the INPUT_NML namelist
!
! * The FORCING flag can be  : 'F' for "no forcing"
!                              'T' for "external forcing file"
!                              'H' for "homogeneous forcing input"
!                              'C' for "coupled forcing field"
!
! * homogeneous forcing is not available for ICE_CONC
!
! * The ASSIM flag can :  'F' for "no forcing"
!                         'T' for "external forcing file"
!
! * namelist must be terminated with /
! * definitions & defaults:
!     INPUT%FORCING%WATER_LEVELS  = 'F'
!     INPUT%FORCING%CURRENTS      = 'F'
!     INPUT%FORCING%WINDS         = 'F'
!     INPUT%FORCING%ATM_MOMENTUM  = 'F'
!     INPUT%FORCING%AIR_DENSITY   = 'F'
!     INPUT%FORCING%ICE_CONC      = 'F'
!     INPUT%FORCING%ICE_PARAM1    = 'F'
!     INPUT%FORCING%ICE_PARAM2    = 'F'
!     INPUT%FORCING%ICE_PARAM3    = 'F'
!     INPUT%FORCING%ICE_PARAM4    = 'F'
!     INPUT%FORCING%ICE_PARAM5    = 'F'
!     INPUT%FORCING%MUD_DENSITY   = 'F'
!     INPUT%FORCING%MUD_THICKNESS = 'F'
!     INPUT%FORCING%MUD_VISCOSITY = 'F'
!     INPUT%ASSIM%MEAN            = 'F'
!     INPUT%ASSIM%SPEC1D          = 'F'
!     INPUT%ASSIM%SPEC2D          = 'F'
! -------------------------------------------------------------------- !
&INPUT_NML
EOF
else
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define each forcing via the INPUT_NML namelist
! -------------------------------------------------------------------- !
&INPUT_NML
EOF
fi

if [ "$mudice" = "T" ]
then

# ice param1
  if [ "${forc[1,1]}" = "T" ] ; then
    if [ "${forc[1,2]}" = "T" ] ; then    echo "  INPUT%FORCING%ICE_PARAM1    = 'H'" >> $nmlfile
    elif [ "${forc[1,2]}" = "F" ] ; then  echo "  INPUT%FORCING%ICE_PARAM1    = 'T'" >> $nmlfile; fi
  elif [ "${forc[1,1]}" = "C" ] ; then    echo "  INPUT%FORCING%ICE_PARAM1    = 'C'" >> $nmlfile
  fi
  
# ice param2
  if [ "${forc[2,1]}" = "T" ] ; then
    if [ "${forc[2,2]}" = "T" ] ; then    echo "  INPUT%FORCING%ICE_PARAM2    = 'H'" >> $nmlfile
    elif [ "${forc[2,2]}" = "F" ] ; then  echo "  INPUT%FORCING%ICE_PARAM2    = 'T'" >> $nmlfile; fi
  elif [ "${forc[2,1]}" = "C" ] ; then    echo "  INPUT%FORCING%ICE_PARAM2    = 'C'" >> $nmlfile
  fi
# ice param3
  if [ "${forc[3,1]}" = "T" ] ; then
    if [ "${forc[3,2]}" = "T" ] ; then    echo "  INPUT%FORCING%ICE_PARAM3    = 'H'" >> $nmlfile
    elif [ "${forc[3,2]}" = "F" ] ; then  echo "  INPUT%FORCING%ICE_PARAM3    = 'T'" >> $nmlfile; fi
  elif [ "${forc[3,1]}" = "C" ] ; then    echo "  INPUT%FORCING%ICE_PARAM3    = 'C'" >> $nmlfile
  fi
# ice param4
  if [ "${forc[4,1]}" = "T" ] ; then
    if [ "${forc[4,2]}" = "T" ] ; then    echo "  INPUT%FORCING%ICE_PARAM4    = 'H'" >> $nmlfile
    elif [ "${forc[4,2]}" = "F" ] ; then  echo "  INPUT%FORCING%ICE_PARAM4    = 'T'" >> $nmlfile; fi
  elif [ "${forc[4,1]}" = "C" ] ; then    echo "  INPUT%FORCING%ICE_PARAM4    = 'C'" >> $nmlfile
  fi
# ice param5
  if [ "${forc[5,1]}" = "T" ] ; then
    if [ "${forc[5,2]}" = "T" ] ; then    echo "  INPUT%FORCING%ICE_PARAM5    = 'H'" >> $nmlfile
    elif [ "${forc[5,2]}" = "F" ] ; then  echo "  INPUT%FORCING%ICE_PARAM5    = 'T'" >> $nmlfile; fi
  elif [ "${forc[5,1]}" = "C" ] ; then    echo "  INPUT%FORCING%ICE_PARAM5    = 'C'" >> $nmlfile
  fi
# mud density
  if [ "${forc[6,1]}" = "T" ] ; then
    if [ "${forc[6,2]}" = "T" ] ; then    echo "  INPUT%FORCING%MUD_DENSITY   = 'H'" >> $nmlfile
    elif [ "${forc[6,2]}" = "F" ] ; then  echo "  INPUT%FORCING%MUD_DENSITY   = 'T'" >> $nmlfile; fi
  elif [ "${forc[6,1]}" = "C" ] ; then    echo "  INPUT%FORCING%MUD_DENSITY   = 'C'" >> $nmlfile
  fi
# mud thickness
  if [ "${forc[7,1]}" = "T" ] ; then
    if [ "${forc[7,2]}" = "T" ] ; then    echo "  INPUT%FORCING%MUD_THICKNESS = 'H'" >> $nmlfile
    elif [ "${forc[7,2]}" = "F" ] ; then  echo "  INPUT%FORCING%MUD_THICKNESS = 'T'" >> $nmlfile; fi
  elif [ "${forc[7,1]}" = "C" ] ; then    echo "  INPUT%FORCING%MUD_THICKNESS = 'C'" >> $nmlfile
  fi
# mud viscosity
  if [ "${forc[8,1]}" = "T" ] ; then
    if [ "${forc[8,2]}" = "T" ] ; then    echo "  INPUT%FORCING%MUD_VISCOSITY = 'H'" >> $nmlfile
    elif [ "${forc[8,2]}" = "F" ] ; then  echo "  INPUT%FORCING%MUD_VISCOSITY = 'T'" >> $nmlfile; fi
  elif [ "${forc[8,1]}" = "C" ] ; then    echo "  INPUT%FORCING%MUD_VISCOSITY = 'C'" >> $nmlfile
  fi
# water levels
  if [ "${forc[9,1]}" = "T" ] ; then
    if [ "${forc[9,2]}" = "T" ] ; then    echo "  INPUT%FORCING%WATER_LEVELS  = 'H'" >> $nmlfile
    elif [ "${forc[9,2]}" = "F" ] ; then  echo "  INPUT%FORCING%WATER_LEVELS  = 'T'" >> $nmlfile; fi
  elif [ "${forc[9,1]}" = "C" ] ; then    echo "  INPUT%FORCING%WATER_LEVELS  = 'C'" >> $nmlfile
  fi
# currents
  if [ "${forc[10,1]}" = "T" ] ; then
    if [ "${forc[10,2]}" = "T" ] ; then    echo "  INPUT%FORCING%CURRENTS      = 'H'" >> $nmlfile
    elif [ "${forc[10,2]}" = "F" ] ; then  echo "  INPUT%FORCING%CURRENTS      = 'T'" >> $nmlfile; fi
  elif [ "${forc[10,1]}" = "C" ] ; then    echo "  INPUT%FORCING%CURRENTS      = 'C'" >> $nmlfile
  fi
# winds
  if [ "${forc[11,1]}" = "T" ] ; then
    if [ "${forc[11,2]}" = "T" ] ; then    echo "  INPUT%FORCING%WINDS         = 'H'" >> $nmlfile
    elif [ "${forc[11,2]}" = "F" ] ; then  echo "  INPUT%FORCING%WINDS         = 'T'" >> $nmlfile; fi
  elif [ "${forc[11,1]}" = "C" ] ; then    echo "  INPUT%FORCING%WINDS         = 'C'" >> $nmlfile
  fi
  if [ "$atmos" = "T" ]
  then
# atm momentum
    if [ "${forc[13,1]}" = "T" ] ; then
      if [ "${forc[13,2]}" = "T" ] ; then    echo "  INPUT%FORCING%ATM_MOMENTUM  = 'H'" >> $nmlfile
      elif [ "${forc[13,2]}" = "F" ] ; then  echo "  INPUT%FORCING%ATM_MOMENTUM  = 'T'" >> $nmlfile; fi
    elif [ "${forc[13,1]}" = "C" ] ; then    echo "  INPUT%FORCING%ATM_MOMENTUM  = 'C'" >> $nmlfile
    fi
# atm density
    if [ "${forc[14,1]}" = "T" ] ; then
      if [ "${forc[14,2]}" = "T" ] ; then    echo "  INPUT%FORCING%AIR_DENSITY   = 'H'" >> $nmlfile
      elif [ "${forc[14,2]}" = "F" ] ; then  echo "  INPUT%FORCING%AIR_DENSITY   = 'T'" >> $nmlfile; fi
    elif [ "${forc[14,1]}" = "C" ] ; then    echo "  INPUT%FORCING%AIR_DENSITY   = 'C'" >> $nmlfile
    fi
# ice
    if [ "${forc[12,1]}" = "T" ] ; then      echo "  INPUT%FORCING%ICE_CONC      = 'T'" >> $nmlfile; fi
# mean
    if [ "${forc[15,1]}" = "T" ] ; then      echo "  INPUT%ASSIM%MEAN            = 'T'" >> $nmlfile; fi
# spec1d
    if [ "${forc[16,1]}" = "T" ] ; then      echo "  INPUT%ASSIM%SPEC1D          = 'T'" >> $nmlfile; fi
# spec2d
    if [ "${forc[17,1]}" = "T" ] ; then      echo "  INPUT%ASSIM%SPEC2D          = 'T'" >> $nmlfile; fi
  elif [ "$atmos" = 'F' ]
  then
# ice
    if [ "${forc[12,1]}" = "T" ] ; then      echo "  INPUT%FORCING%ICE_CONC      = 'T'" >> $nmlfile; fi
# mean
    if [ "${forc[13,1]}" = "T" ] ; then      echo "  INPUT%ASSIM%MEAN            = 'T'" >> $nmlfile; fi
# spec1d
    if [ "${forc[14,1]}" = "T" ] ; then      echo "  INPUT%ASSIM%SPEC1D          = 'T'" >> $nmlfile; fi
# spec2d
    if [ "${forc[15,1]}" = "T" ] ; then      echo "  INPUT%ASSIM%SPEC2D          = 'T'" >> $nmlfile; fi
  fi

elif [ "$mudice" = "F" ]
then

# water levels
  if [ "${forc[1,1]}" = "T" ] ; then
    if [ "${forc[1,2]}" = "T" ] ; then    echo "  INPUT%FORCING%WATER_LEVELS  = 'H'" >> $nmlfile
    elif [ "${forc[1,2]}" = "F" ] ; then  echo "  INPUT%FORCING%WATER_LEVELS  = 'T'" >> $nmlfile
    elif [ "${forc[1,2]}" = "C" ] ; then  echo "  INPUT%FORCING%WATER_LEVELS  = 'C'" >> $nmlfile; fi
  fi
# currents
  if [ "${forc[2,1]}" = "T" ] ; then
    if [ "${forc[2,2]}" = "T" ] ; then    echo "  INPUT%FORCING%CURRENTS      = 'H'" >> $nmlfile
    elif [ "${forc[2,2]}" = "F" ] ; then  echo "  INPUT%FORCING%CURRENTS      = 'T'" >> $nmlfile
    elif [ "${forc[2,2]}" = "C" ] ; then  echo "  INPUT%FORCING%CURRENTS      = 'C'" >> $nmlfile; fi
  fi
# winds
  if [ "${forc[3,1]}" = "T" ] ; then
    if [ "${forc[3,2]}" = "T" ] ; then    echo "  INPUT%FORCING%WINDS         = 'H'" >> $nmlfile
    elif [ "${forc[3,2]}" = "F" ] ; then  echo "  INPUT%FORCING%WINDS         = 'T'" >> $nmlfile
    elif [ "${forc[3,2]}" = "C" ] ; then  echo "  INPUT%FORCING%WINDS         = 'C'" >> $nmlfile; fi
  fi
  if [ "$atmos" = "T" ]
  then
# atm momentum
    if [ "${forc[5,1]}" = "T" ] ; then
      if [ "${forc[5,2]}" = "T" ] ; then    echo "  INPUT%FORCING%ATM_MOMENTUM  = 'H'" >> $nmlfile
      elif [ "${forc[5,2]}" = "F" ] ; then  echo "  INPUT%FORCING%ATM_MOMENTUM  = 'T'" >> $nmlfile; fi
    elif [ "${forc[5,1]}" = "C" ] ; then    echo "  INPUT%FORCING%ATM_MOMENTUM  = 'C'" >> $nmlfile
    fi
# atm density
    if [ "${forc[6,1]}" = "T" ] ; then
      if [ "${forc[6,2]}" = "T" ] ; then    echo "  INPUT%FORCING%AIR_DENSITY   = 'H'" >> $nmlfile
      elif [ "${forc[6,2]}" = "F" ] ; then  echo "  INPUT%FORCING%AIR_DENSITY   = 'T'" >> $nmlfile; fi
    elif [ "${forc[6,1]}" = "C" ] ; then    echo "  INPUT%FORCING%AIR_DENSITY   = 'C'" >> $nmlfile
    fi
# ice
    if [ "${forc[4,1]}" = "T" ] ; then      echo "  INPUT%FORCING%ICE_CONC      = 'T'" >> $nmlfile; fi
# mean
    if [ "${forc[7,1]}" = "T" ] ; then      echo "  INPUT%ASSIM%MEAN            = 'T'" >> $nmlfile; fi
# spec1d
    if [ "${forc[8,1]}" = "T" ] ; then      echo "  INPUT%ASSIM%SPEC1D          = 'T'" >> $nmlfile; fi
# spec2d
    if [ "${forc[9,1]}" = "T" ] ; then      echo "  INPUT%ASSIM%SPEC2D          = 'T'" >> $nmlfile; fi
  else
# ice
    if [ "${forc[4,1]}" = "T" ] ; then      echo "  INPUT%FORCING%ICE_CONC      = 'T'" >> $nmlfile; fi
# mean
    if [ "${forc[5,1]}" = "T" ] ; then      echo "  INPUT%ASSIM%MEAN            = 'T'" >> $nmlfile; fi
# spec1d
    if [ "${forc[6,1]}" = "T" ] ; then      echo "  INPUT%ASSIM%SPEC1D          = 'T'" >> $nmlfile; fi
# spec2d
    if [ "${forc[7,1]}" = "T" ] ; then      echo "  INPUT%ASSIM%SPEC2D          = 'T'" >> $nmlfile; fi
  fi
fi



# output type namelist
if [ "$comment" = "full" ]; then
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the output types point parameters via OUTPUT_TYPE_NML namelist
!
! * the point file is a space separated values per line :
!   longitude latitude 'name' (C*40)
!
! * the full list of field names is :
!  All parameters listed below are available in output file of the types
!  ASCII and NetCDF. If selected output file types are grads or grib,
!  some parameters may not be available. The first two columns in the
!  table below identify such cases by flags, cols 1 (GRB) and 2 (GXO)
!  refer to grib (ww3_grib) and grads (gx_outf), respectively.
!
! Columns 3 and 4 provide group and parameter numbers per group.
! Columns 5, 6 and 7 provide:
!   5 - code name (internal)
!   6 - output tags (names used is ASCII file extensions, NetCDF
!       variable names and namelist-based selection
!   7 - Long parameter name/definition
!
!  G  G
!  R  X Grp  Param Code     Output  Parameter/Group
!  B  O Numb Numbr Name        Tag  Definition
!  --------------------------------------------------
!        1                          Forcing Fields
!   -------------------------------------------------
!  T  T  1     1   DW         DPT   Water depth.
!  T  T  1     2   C[X,Y]     CUR   Current velocity.
!  T  T  1     3   UA         WND   Wind speed.
!  T  T  1     4   AS         AST   Air-sea temperature difference.
!  T  T  1     5   WLV        WLV   Water levels.
!  T  T  1     6   ICE        ICE   Ice concentration.
!  T  T  1     7   IBG        IBG   Iceberg-induced damping.
!  T  T  1     8   TAUA       TAU   Atm. momentum.
!  T  T  1     9   RHOAIR     RHO   Air density.
!  T  T  1    10   D50        D50   Median sediment grain size.
!  T  T  1    11   IC1        IC1   Ice thickness.
!  T  T  1    12   IC5        IC5   Ice flow diameter.
!   -------------------------------------------------
!        2                          Standard mean wave Parameters
!   -------------------------------------------------
!  T  T  2     1   HS         HS    Wave height.
!  T  T  2     2   WLM        LM    Mean wave length.
!  T  T  2     3   T02        T02   Mean wave period (Tm0,2).
!  T  T  2     4   TM10       T0M1  Mean wave period (Tm-1,0).
!  T  T  2     5   T01        T01   Mean wave period (Tm0,1).
!  T  T  2     6   FP0        FP    Peak frequency.
!  T  T  2     7   THM        DIR   Mean wave direction.
!  T  T  2     8   THS        SPR   Mean directional spread.
!  T  T  2     9   THP0       DP    Peak direction.
!  T  T  2    10   HIG        HIG   Infragravity height
!  T  T  2    11   STMAXE     MXE   Max surface elev (STE)
!  T  T  2    12   STMAXD     MXES  St Dev of max surface elev (STE)
!  T  T  2    13   HMAXE      MXH   Max wave height (STE)
!  T  T  2    14   HCMAXE     MXHC  Max wave height from crest (STE)
!  T  T  2    15   HMAXD      SDMH  St Dev of MXC (STE)
!  T  T  2    16   HCMAXD     SDMHC St Dev of MXHC (STE)
!  F  T  2    17   WBT        WBT   Domiant wave breaking probability bT
!  F  F  2    18   FP0        TP    Peak period (from peak freq)
!  F  F  2    19   WNMEAN     WNM   Mean wavenumber
!   -------------------------------------------------
!        3                          Spectral Parameters (first 5)
!   -------------------------------------------------
!  F  F  3     1   EF         EF    Wave frequency spectrum
!  F  F  3     2   TH1M       TH1M  Mean wave direction from a1,b2
!  F  F  3     3   STH1M      STH1M Directional spreading from a1,b2
!  F  F  3     4   TH2M       TH2M  Mean wave direction from a2,b2
!  F  F  3     5   STH2M      STH2M Directional spreading from a2,b2
!  F  F  3     6   WN         WN    Wavenumber array
!   -------------------------------------------------
!        4                          Spectral Partition Parameters
!   -------------------------------------------------
!  T  T  4     1   PHS        PHS   Partitioned wave heights.
!  T  T  4     2   PTP        PTP   Partitioned peak period.
!  T  T  4     3   PLP        PLP   Partitioned peak wave length.
!  T  T  4     4   PDIR       PDIR  Partitioned mean direction.
!  T  T  4     5   PSI        PSPR  Partitioned mean directional spread.
!  T  T  4     6   PWS        PWS   Partitioned wind sea fraction.
!  T  T  4     7   PTHP0      PDP   Peak wave direction of partition.
!  T  T  4     8   PQP        PQP   Goda peakedness parameter of partition.
!  T  T  4     9   PPE        PPE   JONSWAP peak enhancement factor of partition.
!  T  T  4    10   PGW        PGW   Gaussian frequency width of partition.
!  T  T  4    11   PSW        PSW   Spectral width of partition.
!  T  T  4    12   PTM1       PTM10 Mean wave period (Tm-1,0) of partition.
!  T  T  4    13   PT1        PT01  Mean wave period (Tm0,1) of partition.
!  T  T  4    14   PT2        PT02  Mean wave period (Tm0,2) of partition.
!  T  T  4    15   PEP        PEP   Peak spectral density of partition.
!  T  T  4    16   PWST       TWS   Total wind sea fraction.
!  T  T  4    17   PNR        PNR   Number of partitions.
!   -------------------------------------------------
!        5                          Atmosphere-waves layer
!   -------------------------------------------------
!  T  T  5     1   UST        UST   Friction velocity.
!  F  T  5     2   CHARN      CHA   Charnock parameter
!  F  T  5     3   CGE        CGE   Energy flux
!  F  T  5     4   PHIAW      FAW   Air-sea energy flux
!  F  T  5     5   TAUWI[X,Y] TAW   Net wave-supported stress
!  F  T  5     6   TAUWN[X,Y] TWA   Negative part of the wave-supported stress
!  F  F  5     7   WHITECAP   WCC   Whitecap coverage
!  F  F  5     8   WHITECAP   WCF   Whitecap thickness
!  F  F  5     9   WHITECAP   WCH   Mean breaking height
!  F  F  5    10   WHITECAP   WCM   Whitecap moment
!  F  F  5    11   FWS        FWS   Wind sea mean period
!   -------------------------------------------------
!        6                          Wave-ocean layer
!   -------------------------------------------------
!  F  F  6     1   S[XX,YY,XY] SXY  Radiation stresses.
!  F  F  6     2   TAUO[X,Y]  TWO   Wave to ocean momentum flux
!  F  F  6     3   BHD        BHD   Bernoulli head (J term)
!  F  F  6     4   PHIOC      FOC   Wave to ocean energy flux
!  F  F  6     5   TUS[X,Y]   TUS   Stokes transport
!  F  F  6     6   USS[X,Y]   USS   Surface Stokes drift
!  F  F  6     7   [PR,TP]MS  P2S   Second-order sum pressure
!  F  F  6     8   US3D       USF   Spectrum of surface Stokes drift
!  F  F  6     9   P2SMS      P2L   Micro seism  source term
!  F  F  6    10   TAUICE     TWI   Wave to sea ice stress
!  F  F  6    11   PHICE      FIC   Wave to sea ice energy flux
!  F  F  6    12   USSP       USP   Partitioned surface Stokes drift
!  F  F  6    13   TAUOC[X,Y] TOC   Total momentum to the ocean
!   -------------------------------------------------
!        7                          Wave-bottom layer
!   -------------------------------------------------
!  F  F  7     1   ABA        ABR   Near bottom rms amplitides.
!  F  F  7     2   UBA        UBR   Near bottom rms velocities.
!  F  F  7     3   BEDFORMS   BED   Bedforms
!  F  F  7     4   PHIBBL     FBB   Energy flux due to bottom friction
!  F  F  7     5   TAUBBL     TBB   Momentum flux due to bottom friction
!   -------------------------------------------------
!        8                          Spectrum parameters
!   -------------------------------------------------
!  F  F  8     1   MSS[X,Y]   MSS   Mean square slopes
!  F  F  8     2   MSC[X,Y]   MSC   Spectral level at high frequency tail
!  F  F  8     3   MSSD       MSD   Slope direction
!  F  F  8     4   MSCD       MCD   Tail slope direction
!  F  F  8     5   QP         QP    Goda peakedness parameter
!  F  F  8     6   QKK        QKK   Wavenumber peakedness
!  F  F  8     7   SKEW       SKW   Skewness of elevation for zero slopes
!  F  F  8     8   EMBIA1     EMB   Mean sea level at zero slopes / Hs
!  F  F  8     9   EMBIA2     EMC   Tracker bias for LRM least square altimetry
!   -------------------------------------------------
!        9                          Numerical diagnostics
!   -------------------------------------------------
!  T  T  9     1   DTDYN      DTD   Average time step in integration.
!  T  T  9     2   FCUT       FC    Cut-off frequency.
!  T  T  9     3   CFLXYMAX   CFX   Max. CFL number for spatial advection.
!  T  T  9     4   CFLTHMAX   CFD   Max. CFL number for theta-advection.
!  F  F  9     5   CFLKMAX    CFK   Max. CFL number for k-advection.
!   -------------------------------------------------
!        10                         User defined
!   -------------------------------------------------
!  F  F  10    1              U1    User defined #1. (requires coding ...)
!  F  F  10    2              U2    User defined #1. (requires coding ...)
!   -------------------------------------------------
!
!     Section 4 consist of a set of fields, index 0 = wind sea, index
!     1:NOSWLL are first NOSWLL swell fields.
!
!
! * output track file formatted (T) or unformated (F)
!
! * coupling fields exchanged list is :
!   - Sent fields by ww3:
!       - Ocean model : T0M1 OCHA OHS DIR BHD TWO UBR FOC TAW TUS USS LM DRY TOC
!       - Atmospheric model : ACHA AHS TP (or FP) FWS
!       - Ice model : IC5 TWI
!   - Received fields by ww3:
!       - Ocean model : SSH CUR
!       - Atmospheric model : WND TAU RHO
!       - Ice model : ICE IC1 IC5
!
! * Coupling flag (T) or (F) at T+0 (extra fields in the restart needed)
!
! * extra fields to be written to the restart:
!   - The list includes all fields sent by coupling exchange only
!
! * namelist must be terminated with /
! * definitions & defaults:
!     TYPE%FIELD%LIST         =  'unset'
!     TYPE%POINT%FILE         =  'points.list'
!     TYPE%TRACK%FORMAT       =  T
!     TYPE%PARTITION%X0       =  0
!     TYPE%PARTITION%XN       =  0
!     TYPE%PARTITION%NX       =  0
!     TYPE%PARTITION%Y0       =  0
!     TYPE%PARTITION%YN       =  0
!     TYPE%PARTITION%NY       =  0
!     TYPE%PARTITION%FORMAT   =  T
!     TYPE%COUPLING%SENT      = 'unset'
!     TYPE%COUPLING%RECEIVED  = 'unset'
!     TYPE%COUPLING%COUPLET0  =  F
!     TYPE%RESTART%EXTRA      = 'unset'
!
! -------------------------------------------------------------------- !
&OUTPUT_TYPE_NML
EOF
else
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the output types point parameters via OUTPUT_TYPE_NML namelist
! -------------------------------------------------------------------- !
&OUTPUT_TYPE_NML
EOF
fi

if [ "${fielddate[3]}" != 0 ]; then  echo "  TYPE%FIELD%LIST          = '$fieldlist'" >> $nmlfile; fi

if [ "${pointdate[3]}" != 0 ]; then
  if [ "$point_filename" != 'points.list' ] ; then  echo "  TYPE%POINT%FILE          = '$point_filename'" >> $nmlfile; fi
fi

if [ "${trackdate[3]}" != 0 ] && [ "$trackflag" != T ];    then  
                                        echo "  TYPE%TRACK%FORMAT        = $trackflag" >> $nmlfile; fi
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
  echo "  TYPE%PARTITION           = ${partfield[1]} ${partfield[2]} ${partfield[3]} ${partfield[4]} ${partfield[5]} ${partfield[6]} ${partfield[7]}" >> $nmlfile; fi
fi

if [ "$coupling" = T ]; then
  if [ "${couplingdate[3]}" != 0 ]; then  
    echo "  TYPE%COUPLING%SENT       = '$sent'" >> $nmlfile
    echo "  TYPE%COUPLING%RECEIVED   = '$received'" >> $nmlfile
    if [ "$couplet0" = 'T' ] ; then echo "  TYPE%COUPLING%COUPLET0   =  T" >> $nmlfile; fi
  fi
fi

if [ "$extra" = 'T' ] ; then echo "  TYPE%RESTART%EXTRA       = '$extrafields'" >> $nmlfile; fi



# output date namelist
if [ "$comment" = "full" ]; then
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define output dates via OUTPUT_DATE_NML namelist
!
! * start and stop times are with format 'yyyymmdd hhmmss'
! * if time stride is equal '0', then output is disabled
! * time stride is given in seconds
!
! * namelist must be terminated with /
! * definitions & defaults:
!     DATE%FIELD%START         =  '19680606 000000'
!     DATE%FIELD%STRIDE        =  '0'
!     DATE%FIELD%STOP          =  '19680607 000000'
!     DATE%POINT%START         =  '19680606 000000'
!     DATE%POINT%STRIDE        =  '0'
!     DATE%POINT%STOP          =  '19680607 000000'
!     DATE%TRACK%START         =  '19680606 000000'
!     DATE%TRACK%STRIDE        =  '0'
!     DATE%TRACK%STOP          =  '19680607 000000'
!     DATE%RESTART%START       =  '19680606 000000'
!     DATE%RESTART%STRIDE      =  '0'
!     DATE%RESTART%STOP        =  '19680607 000000'
!     DATE%BOUNDARY%START      =  '19680606 000000'
!     DATE%BOUNDARY%STRIDE     =  '0'
!     DATE%BOUNDARY%STOP       =  '19680607 000000'
!     DATE%PARTITION%START     =  '19680606 000000'
!     DATE%PARTITION%STRIDE    =  '0'
!     DATE%PARTITION%STOP      =  '19680607 000000'
!     DATE%COUPLING%START      =  '19680606 000000'
!     DATE%COUPLING%STRIDE     =  '0'
!     DATE%COUPLING%STOP       =  '19680607 000000'
!
!     DATE%RESTART             =  '19680606 000000' '0' '19680607 000000'
! -------------------------------------------------------------------- !
&OUTPUT_DATE_NML
EOF
else
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define output dates via OUTPUT_DATE_NML namelist
! -------------------------------------------------------------------- !
&OUTPUT_DATE_NML
EOF
fi

if [ "${fielddate[3]}" != '0' ]; then
  echo "  DATE%FIELD          = '${fielddate[1]} ${fielddate[2]}' '${fielddate[3]}' '${fielddate[4]} ${fielddate[5]}'" >> $nmlfile; fi

if [ "${pointdate[3]}" != '0' ]; then
  echo "  DATE%POINT          = '${pointdate[1]} ${pointdate[2]}' '${pointdate[3]}' '${pointdate[4]} ${pointdate[5]}'" >> $nmlfile; fi

if [ "${trackdate[3]}" != '0' ]; then
  echo "  DATE%TRACK          = '${trackdate[1]} ${trackdate[2]}' '${trackdate[3]}' '${trackdate[4]} ${trackdate[5]}'" >> $nmlfile; fi

if [ "${restartdate[3]}" != '0' ]; then
  echo "  DATE%RESTART        = '${restartdate[1]} ${restartdate[2]}' '${restartdate[3]}' '${restartdate[4]} ${restartdate[5]}'" >> $nmlfile; fi

if [ "$restart2" = 'T' ]; then
  if [ "${restart2date[3]}" != '0' ]; then
    echo "  DATE%RESTART2       = '${restart2date[1]} ${restart2date[2]}' '${restart2date[3]}' '${restart2date[4]} ${restart2date[5]}'" >> $nmlfile; fi
fi

if [ "${boundarydate[3]}" != '0' ]; then
  echo "  DATE%BOUNDARY       = '${boundarydate[1]} ${boundarydate[2]}' '${boundarydate[3]}' '${boundarydate[4]} ${boundarydate[5]}'" >> $nmlfile; fi

if [ "${partitiondate[3]}" != '0' ]; then
  echo "  DATE%PARTITION      = '${partitiondate[1]} ${partitiondate[2]}' '${partitiondate[3]}' '${partitiondate[4]} ${partitiondate[5]}'" >> $nmlfile; fi

if [ "$coupling" = T ]; then
  if [ "${couplingdate[3]}" != '0' ]; then
    echo "  DATE%COUPLING       = '${couplingdate[1]} ${couplingdate[2]}' '${couplingdate[3]}' '${couplingdate[4]} ${couplingdate[5]}'" >> $nmlfile; fi
fi


# homogeneous input namelist
if [ "$comment" = "full" ]; then
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define homogeneous input via HOMOG_COUNT_NML and HOMOG_INPUT_NML namelist
!
! * the number of each homogeneous input is defined by HOMOG_COUNT
! * the total number of homogeneous input is automatically calculated
! * the homogeneous input must start from index 1 to N
! * if VALUE1 is equal 0, then the homogeneous input is desactivated
! * NAME can be IC1, IC2, IC3, IC4, IC5, MDN, MTH, MVS, LEV, CUR, WND, ICE, MOV
! * each homogeneous input is defined over a maximum of 3 values detailled below :
!     - IC1 is defined by thickness
!     - IC2 is defined by viscosity
!     - IC3 is defined by density
!     - IC4 is defined by modulus
!     - IC5 is defined by floe diameter
!     - MDN is defined by density
!     - MTH is defined by thickness
!     - MVS is defined by viscosity
!     - LEV is defined by height
!     - CUR is defined by speed and direction
!     - WND is defined by speed, direction and airseatemp
!     - ICE is defined by concentration
!     - MOV is defined by speed and direction
!
! * namelist must be terminated with /
! * definitions & defaults:
!     HOMOG_COUNT%N_IC1            =  0
!     HOMOG_COUNT%N_IC2            =  0
!     HOMOG_COUNT%N_IC3            =  0
!     HOMOG_COUNT%N_IC4            =  0
!     HOMOG_COUNT%N_IC5            =  0
!     HOMOG_COUNT%N_MDN            =  0
!     HOMOG_COUNT%N_MTH            =  0
!     HOMOG_COUNT%N_MVS            =  0
!     HOMOG_COUNT%N_LEV            =  0
!     HOMOG_COUNT%N_CUR            =  0
!     HOMOG_COUNT%N_WND            =  0
!     HOMOG_COUNT%N_ICE            =  0
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
else
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define homogeneous input via HOMOG_COUNT_NML and HOMOG_INPUT_NML namelist
! -------------------------------------------------------------------- !
&HOMOG_COUNT_NML
EOF
fi

if [ $ntot -gt 0 ] ; then

  if [ $nic1 -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_IC1                =  $nic1" >> $nmlfile; fi
  if [ $nic2 -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_IC2                =  $nic2" >> $nmlfile; fi
  if [ $nic3 -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_IC3                =  $nic3" >> $nmlfile; fi
  if [ $nic4 -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_IC4                =  $nic4" >> $nmlfile; fi
  if [ $nic5 -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_IC5                =  $nic5" >> $nmlfile; fi
  if [ $nmdn -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_MDN                =  $nmdn" >> $nmlfile; fi
  if [ $nmth -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_MTH                =  $nmth" >> $nmlfile; fi
  if [ $nmvs -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_MVS                =  $nmvs" >> $nmlfile; fi
  if [ $ncur -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_CUR                =  $ncur" >> $nmlfile; fi
  if [ $nlev -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_LEV                =  $nlev" >> $nmlfile; fi
  if [ $nwnd -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_WND                =  $nwnd" >> $nmlfile; fi
  if [ $nice -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_ICE                =  $nice" >> $nmlfile; fi
  if [ $nmov -gt 0 ] ; then
    echo "  HOMOG_COUNT%N_MOV                =  $nmov" >> $nmlfile; fi
fi

cat >> $nmlfile << EOF
/

&HOMOG_INPUT_NML
EOF

itot=0
if [ $ntot -gt 0 ] ; then
  if [ $nic1 -gt 0 ] ; then
    for iic1 in $(seq 1 $nic1)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'IC1'" >> $nmlfile
      if [ "${homogic1[$iic1,2]} ${homogic1[$iic1,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homogic1[$iic1,2]} ${homogic1[$iic1,3]}'" >> $nmlfile; fi
      if [ "${homogic1[$iic1,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homogic1[$iic1,4]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

  if [ $nic2 -gt 0 ] ; then
    for iic2 in $(seq 1 $nic2)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'IC2'" >> $nmlfile
      if [ "${homogic2[$iic2,2]} ${homogic2[$iic2,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homogic2[$iic2,2]} ${homogic2[$iic2,3]}'" >> $nmlfile; fi
      if [ "${homogic2[$iic2,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homogic2[$iic2,4]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

  if [ $nic3 -gt 0 ] ; then
    for iic3 in $(seq 1 $nic3)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'IC3'" >> $nmlfile
      if [ "${homogic3[$iic3,2]} ${homogic3[$iic3,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homogic3[$iic3,2]} ${homogic3[$iic3,3]}'" >> $nmlfile; fi
      if [ "${homogic3[$iic3,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homogic3[$iic3,4]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

  if [ $nic4 -gt 0 ] ; then
    for iic4 in $(seq 1 $nic4)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'IC4'" >> $nmlfile
      if [ "${homogic4[$iic4,2]} ${homogic4[$iic4,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homogic4[$iic4,2]} ${homogic4[$iic4,3]}'" >> $nmlfile; fi
      if [ "${homogic4[$iic4,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homogic4[$iic4,4]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

  if [ $nic5 -gt 0 ] ; then
    for iic5 in $(seq 1 $nic5)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'IC5'" >> $nmlfile
      if [ "${homogic5[$iic5,2]} ${homogic5[$iic5,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homogic5[$iic5,2]} ${homogic5[$iic5,3]}'" >> $nmlfile; fi
      if [ "${homogic5[$iic5,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homogic5[$iic5,4]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

  if [ $nmdn -gt 0 ] ; then
    for imdn in $(seq 1 $nmdn)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'MDN'" >> $nmlfile
      if [ "${homogmdn[$imdn,2]} ${homogmdn[$imdn,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homogmdn[$imdn,2]} ${homogmdn[$imdn,3]}'" >> $nmlfile; fi
      if [ "${homogmdn[$imdn,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homogmdn[$imdn,4]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

  if [ $nmth -gt 0 ] ; then
    for imth in $(seq 1 $nmth)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'MTH'" >> $nmlfile
      if [ "${homogmth[$imth,2]} ${homogmth[$imth,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homogmth[$imth,2]} ${homogmth[$imth,3]}'" >> $nmlfile; fi
      if [ "${homogmth[$imth,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homogmth[$imth,4]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

  if [ $nmvs -gt 0 ] ; then
    for imvs in $(seq 1 $nmvs)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'MVS'" >> $nmlfile
      if [ "${homogmvs[$imvs,2]} ${homogmvs[$imvs,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homogmvs[$imvs,2]} ${homogmvs[$imvs,3]}'" >> $nmlfile; fi
      if [ "${homogmvs[$imvs,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homogmvs[$imvs,4]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

  if [ $ncur -gt 0 ] ; then
    for icur in $(seq 1 $ncur)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'CUR'" >> $nmlfile
      if [ "${homogcur[$icur,2]} ${homogcur[$icur,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homogcur[$icur,2]} ${homogcur[$icur,3]}'" >> $nmlfile; fi
      if [ "${homogcur[$icur,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homogcur[$icur,4]}" >> $nmlfile; fi
      if [ "${homogcur[$icur,5]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE2      = ${homogcur[$icur,5]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

  if [ $nlev -gt 0 ] ; then
    for ilev in $(seq 1 $nlev)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'LEV'" >> $nmlfile
      if [ "${homoglev[$ilev,2]} ${homoglev[$ilev,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homoglev[$ilev,2]} ${homoglev[$ilev,3]}'" >> $nmlfile; fi
      if [ "${homoglev[$ilev,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homoglev[$ilev,4]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

  if [ $nwnd -gt 0 ] ; then
    for iwnd in $(seq 1 $nwnd)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'WND'" >> $nmlfile
      if [ "${homogwnd[$iwnd,2]} ${homogwnd[$iwnd,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homogwnd[$iwnd,2]} ${homogwnd[$iwnd,3]}'" >> $nmlfile; fi
      if [ "${homogwnd[$iwnd,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homogwnd[$iwnd,4]}" >> $nmlfile; fi
      if [ "${homogwnd[$iwnd,5]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE2      = ${homogwnd[$iwnd,5]}" >> $nmlfile; fi
      if [ "${homogwnd[$iwnd,6]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE3      = ${homogwnd[$iwnd,6]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

  if [ $nice -gt 0 ] ; then
    for iice in $(seq 1 $nice)
    do
      itot=$(($itot + 1))
        echo "  HOMOG_INPUT($itot)%NAME        = 'ICE'" >> $nmlfile
      if [ "${homogice[$iice,2]} ${homogice[$iice,3]}" != '19680606 000000' ]; then
        echo "  HOMOG_INPUT($itot)%DATE        = '${homogice[$iice,2]} ${homogice[$iice,3]}'" >> $nmlfile; fi
      if [ "${homogice[$iice,4]}" != '0' ]; then
        echo "  HOMOG_INPUT($itot)%VALUE1      = ${homogice[$iice,4]}" >> $nmlfile; fi
      if [ $itot -lt $ntot ]; then echo "" >> $nmlfile; fi
    done
  fi

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

#------------------------------


