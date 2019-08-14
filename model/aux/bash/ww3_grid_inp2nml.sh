#!/bin/bash -e

prog="ww3_grid"

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

declare -A depth2d
declare -A mask2d
declare -A obst2d
declare -A slope2d
declare -A sed2d

declare -A inptx
declare -A inpty
declare -A inptf
declare -A exptx
declare -A expty
declare -A exptf
declare -A exbdx
declare -A exbdy
declare -A oulnx0
declare -A oulny0
declare -A oulndx
declare -A oulndy
declare -A oulnnp

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

# grid name
gridname=$(echo $(echo "${lines[$il]}" | sed -e "s/\*//g" | cut -d \" -f2  | cut -d \' -f2))
echo $gridname

if [ "$(basename $inp)" == "ww3_grid.inp" ]
then
  grdname="$(echo $gridname | awk -F' ' '{print $1}')"
  echo 'grdname from grid name : ' "$grdname"
else
  baseinp=$(basename $inp)
  grdname="$(echo ${baseinp%%.*}  | awk -F'ww3_grid_' '{print $2}')"
  echo 'grdname from inp name : ' "$grdname"
fi

# spectrum
il=$(($il+1))
xfr="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
freq1="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
nk="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
nth="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
thoff="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
echo $xfr $freq1 $nk $nth $thoff

# run
il=$(($il+1))
fldry="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
flcx="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
flcy="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
flcth="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
flck="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
flsou="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
echo $fldry $flcx $flcy $flcth $flck $flsou

# timesteps
il=$(($il+1))
dtmax="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
dtxy="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
dtkth="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
dtmin="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
echo $dtmax $dtxy $dtkth $dtmin

# namelists
forinamelist="$cur_dir/namelists_${grdname}.nml"
fnamelist="$cur_dir/namelists_${grdname}.nml.new"
nml_filename=$forinamelist
rm -f $fnamelist
il=$(($il+1))
nml="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
echo $nml
while [ "$nml" != "END" ]
do
  echo "${lines[$il]}" >> $fnamelist
  il=$(($il+1))
  nml="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $nml
done
echo "${lines[$il]}" >> $fnamelist
if [ -f $forinamelist ]
then
  if [ -z "$(diff $forinamelist $fnamelist)" ]
  then
    echo $forinamelist ' and ' $fnamelist 'are same.'
    echo 'delete ' $fnamelist
    rm $fnamelist
  else
    echo 'diff between :' $forinamelist ' and new file : ' $fnamelist
    echo 'inp2nml conversion stopped'
    exit 1
  fi
else
  echo 'mv '$fnamelist ' to ' $forinamelist
  mv $fnamelist $forinamelist
fi


# unst open bound
if [ ! -z "$(grep 'UGOBCFILE' $forinamelist)" ]
then
  unst_ugobcfile=$(grep UGOBCFILE $forinamelist | awk -F'UGOBCFILE' '{print $2}' | awk -F'=' '{print $2}' | awk -F',' '{print $1}' | awk -F"'" '{print $2}')
else
  unst_ugobcfile='unset'
fi


# grid
il=$(($il+1))
type="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
flgcoord="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
if [ "$flgcoord" == 'T' ]; then 
  coord='SPHE'
else
  coord='CART'
fi
clos="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
echo $type $coord $clos

# nx/ny
if [ "$type" == 'RECT' ] || [ "$type" == 'CURV' ]
then
  il=$(($il+1))
  nx=$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2 | sed 's/0*//')
  ny=$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2 | sed 's/0*//')
  echo $nx $ny
  num_max=$(($nx * $ny))
fi

#rect
if [ "$type" == 'RECT' ]
then
  il=$(($il+1))
  rect_sx="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  rect_sy="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  rect_sf="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $rect_sx $rect_sy $rect_sf
  il=$(($il+1))
  rect_x0="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  rect_y0="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  rect_sf0="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  echo $rect_x0 $rect_y0 $rect_sf0

# curv
elif [ "$type" == 'CURV' ]
then
  # xcoord
  il=$(($il+1))
  xcoord_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  xcoord_sf="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  xcoord_off="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  xcoord_idla="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
  xcoord_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
  xcoord_format="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
  xcoord_from="$(echo ${lines[$il]} | awk -F' ' '{print $7}' | cut -d \" -f2  | cut -d \' -f2)"
  xcoord_filename="$(echo ${lines[$il]} | awk -F' ' '{print $8}' | cut -d \" -f2  | cut -d \' -f2)"
  echo 'xcoord : ' $xcoord_idf $xcoord_sf $xcoord_off $xcoord_idla $xcoord_idfm $xcoord_format $xcoord_from $xcoord_filename
  # ycoord
  il=$(($il+1))
  ycoord_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  ycoord_sf="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  ycoord_off="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  ycoord_idla="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
  ycoord_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
  ycoord_format="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
  ycoord_from="$(echo ${lines[$il]} | awk -F' ' '{print $7}' | cut -d \" -f2  | cut -d \' -f2)"
  ycoord_filename="$(echo ${lines[$il]} | awk -F' ' '{print $8}' | cut -d \" -f2  | cut -d \' -f2)"
  echo 'ycoord : ' $ycoord_idf $ycoord_sf $ycoord_off $ycoord_idla $ycoord_idfm $ycoord_format $ycoord_from $ycoord_filename
fi

# depth
depth_idf=50; depth_sf=1.; depth_idla=1; depth_idfm=1; depth_format='(....)'; depth_from='NAME'; depth_filename='unset';
il=$(($il+1))
depth_zlim="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
depth_dmin="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
depth_idf="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
depth_sf="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
depth_idla="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
depth_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
depth_format="$(echo ${lines[$il]} | awk -F' ' '{print $7}' | cut -d \" -f2  | cut -d \' -f2)"
depth_from="$(echo ${lines[$il]} | awk -F' ' '{print $8}' | cut -d \" -f2  | cut -d \' -f2)"
depth_filename="$(echo ${lines[$il]} | awk -F' ' '{print $9}' | cut -d \" -f2  | cut -d \' -f2)"
echo 'depth : ' $depth_zlim $depth_dmin $depth_idf $depth_sf $depth_idla $depth_idfm $depth_format $depth_from $depth_filename
if [ "$depth_from" == 'UNIT' ] || [ "$depth_idf" == '10' ]
then
  foridepth=$cur_dir/${grdname}.depth
  fdepth=$cur_dir/${grdname}.depth.new
  depth_filename=$foridepth
  depth_from='NAME'
  rm -f $fdepth
  num_total=0
  iread=0
  j=1
  # unfold the array
  while [ $num_total -lt $num_max ];  do
    il=$(($il+1))
    curline="$(echo ${lines[$il]} | sed -e 's/,/ /g')"
    line_elem=$(echo $curline} | awk -F' ' '{print NF}')  
    for n_elem in $(seq 1 $line_elem); do
      curelem=$(echo $curline | awk -F' ' "{print \$$n_elem}" | cut -d \" -f2  | cut -d \' -f2)
      if [ ! -z "$(echo $curelem | grep '*')" ]; then    
        num_times=$(echo $curelem | awk -F'*' '{print $1}')
        val_times=$(echo $curelem | awk -F'*' '{print $2}')
      else
        num_times=1
        val_times=$curelem
      fi
      num_total=$(($num_total + $num_times))
      for t in $(seq 1 $num_times); do
        iread=$(($iread+1))
        depth2d[$iread,$j]="$val_times"
#        echo -n "$(printf '%02s' $val_times) " >> $fdepth
        if [ $iread -ge $nx ]; then
          iread=0
          j=$(($j+1))
#          echo '' >> $fdepth
        fi
      done
    done
    echo "${lines[$il]}" >> $fdepth
  done
  # save file
  if [ -f $foridepth ]
  then
    if [ -z "$(diff $foridepth $fdepth)" ]
    then
      echo $foridepth ' and ' $fdepth 'are same.'
      echo 'delete ' $fdepth
      rm $fdepth
    else
      echo 'diff between :' $foridepth ' and new file : ' $fdepth
      echo 'inp2nml conversion stopped'
      exit 1
    fi
  else
    echo 'mv '$fdepth ' to ' $foridepth
    mv $fdepth $foridepth
  fi
fi

##########

# smc and/or obst
mcels_idf=31; mcels_idla=1; mcels_idfm=1; mcels_format='(....)'; mcels_filename='unset';
iside_idf=32; iside_idla=1; iside_idfm=1; iside_format='(....)'; iside_filename='unset';
jside_idf=33; jside_idla=1; jside_idfm=1; jside_format='(....)'; jside_filename='unset';
bundy_idf=35; bundy_idla=1; bundy_idfm=1; bundy_format='(....)'; bundy_filename='unset';
mbarc_idf=36; mbarc_idla=1; mbarc_idfm=1; mbarc_format='(....)'; mbarc_filename='unset';
aisid_idf=37; aisid_idla=1; aisid_idfm=1; aisid_format='(....)'; aisid_filename='unset';
ajsid_idf=38; ajsid_idla=1; ajsid_idfm=1; ajsid_format='(....)'; ajsid_filename='unset';
obst_idf=70; obst_sf=1.; obst_idla=1; obst_idfm=1; obst_format='(....)'; obst_from='NAME'; obst_filename='unset';
if [ "$type" == 'RECT' ] || [ "$type" == 'CURV' ]
then
  il=$(($il+1))
  # smc
  if [ $(echo ${lines[$il]} | awk -F' ' '{print NF}') -eq 5 ] && [ ! -z "$(echo ${lines[$il]} | awk -F' ' '{print $4}' | grep '(')" ]
  then
    SMC=1
    # smc mcels
    mcels_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    mcels_idla="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    mcels_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    mcels_format="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    mcels_filename="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
    echo 'mcels : ' $mcels_idf $mcels_idla $mcels_idfm $mcels_format $mcels_filename
    # smc iside
    il=$(($il+1))
    iside_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    iside_idla="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    iside_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    iside_format="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    iside_filename="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
    echo 'iside : '$iside_idf $iside_idla $iside_idfm $iside_format $iside_filename
    # smc jside
    il=$(($il+1))
    jside_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    jside_idla="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    jside_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    jside_format="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    jside_filename="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
    echo 'jside : ' $jside_idf $jside_idla $jside_idfm $jside_format $jside_filename

    # obst
    obst_idf=34; obst_sf=1.; obst_idla=1; obst_idfm=1; obst_format='(....)'; obst_from='NAME'; obst_filename='unset';
    if [ ! -z "$(grep 'FLAGTR' $forinamelist)" ]
    then
      flagtr=$(grep FLAGTR $forinamelist | awk -F'FLAGTR' '{print $2}' | awk -F'=' '{print $2}' | awk -F',' '{print $1}' | awk -F'/' '{print $1}' | awk -F' ' '{print $1}')
      echo 'flagtr : ' $flagtr
      if [ $flagtr -gt 0 ]
      then
        il=$(($il+1))
        obst_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
        obst_idla="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
        obst_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
        obst_format="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
        obst_filename="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
        echo 'smc obst : ' $obst_idf $obst_idla $obst_idfm $obst_format $obst_filename
        if [ "$obst_from" == 'UNIT' ] || [ "$obst_idf" == '10' ]
        then
          foriobst=$cur_dir/${grdname}.obst
          fobst=$cur_dir/${grdname}.obst.new
          obst_filename=$foriobst
          obst_from='NAME'
          rm -f $fobst
          num_total=0
          iread=0
          j=1
          # unfold the along-X array
          while [ $num_total -lt $num_max ];  do
            il=$(($il+1))
            curline="$(echo ${lines[$il]} | sed -e 's/,/ /g')"
            line_elem=$(echo $curline} | awk -F' ' '{print NF}')
            for n_elem in $(seq 1 $line_elem); do
              curelem=$(echo $curline | awk -F' ' "{print \$$n_elem}" | cut -d \" -f2  | cut -d \' -f2)
              if [ ! -z "$(echo $curelem | grep '*')" ]; then
                num_times=$(echo $curelem | awk -F'*' '{print $1}')
                val_times=$(echo $curelem | awk -F'*' '{print $2}')
              else
                num_times=1
                val_times=$curelem
              fi
              num_total=$(($num_total + $num_times))
              for t in $(seq 1 $num_times); do
                iread=$(($iread+1))
                obst2d[$iread,$j]="$val_times"
#                echo -n "$val_times " >> $fobst
                if [ $iread -ge $nx ]; then
                  iread=0
                  j=$(($j+1))
#                  echo '' >> $fobst
                fi
              done
            done
            echo "${lines[$il]}" >> $fobst
          done
          # unfold the along-Y array
          echo "carriage return for Y-along array"
          echo -n '' >> $fobst
          while [ $num_total -lt $(($num_max * 2)) ];  do
            il=$(($il+1))
            curline="$(echo ${lines[$il]} | sed -e 's/,/ /g')"
            line_elem=$(echo $curline} | awk -F' ' '{print NF}')  
            for n_elem in $(seq 1 $line_elem); do
              curelem=$(echo $curline | awk -F' ' "{print \$$n_elem}" | cut -d \" -f2  | cut -d \' -f2)
              if [ ! -z "$(echo $curelem | grep '*')" ]; then    
                num_times=$(echo $curelem | awk -F'*' '{print $1}')
                val_times=$(echo $curelem | awk -F'*' '{print $2}')
              else
                num_times=1
                val_times=$curelem
              fi
              num_total=$(($num_total + $num_times))
              for t in $(seq 1 $num_times); do
                iread=$(($iread+1))
                obst2d[$iread,$j]="$val_times"
#                echo -n "$val_times " >> $fobst
                if [ $iread -ge $nx ]; then
                  iread=0
                  j=$(($j+1))
#                  echo '' >> $fobst
                fi
              done
            done
            echo "${lines[$il]}" >> $fobst
          done
          # save file
          if [ -f $foriobst ]
          then
            if [ -z "$(diff $foriobst $fobst)" ]
            then
              echo $foriobst ' and ' $fobst 'are same.'
              echo 'delete ' $fobst
              rm $fobst
            else
              echo 'diff between :' $foriobst ' and new file : ' $fobst
              echo 'inp2nml conversion stopped'
              exit 1
            fi
          else
            echo 'mv '$fobst ' to ' $foriobst
            mv $fobst $foriobst
          fi
        fi
      fi
    fi

    # smc bundy
    bundy_idf=35; bundy_idla=1; bundy_idfm=1; bundy_format='(....)'; bundy_filename='unset';
    il=$(($il+1))
    if [ $(echo ${lines[$il]} | awk -F' ' '{print NF}') -eq 5 ] && [ ! -z "$(echo ${lines[$il]} | awk -F' ' '{print $4}' | grep '(')" ]
    then
      if [ ! -z $(grep 'NBISMC' $forinamelist) ]
      then
        if [ $(grep NBISMC $forinamelist | awk -F'NBISMC' '{print $2}' | awk -F'=' '{print $2}' | awk -F',' '{print $1}' | awk -F'/' '{print $1}' | awk -F' ' '{print $1}') -gt 0 ]
        then
          bundy_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
          bundy_idla="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
          bundy_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
          bundy_format="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
          bundy_filename="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
          echo 'bundy : ' $bundy_idf $bundy_idla $bundy_idfm $bundy_format $bundy_filename
        else
          il=$(($il-1))
        fi
      else
        il=$(($il-1))
      fi
    else
      il=$(($il-1))
    fi

    # smc mbarc
    mbarc_idf=36; mbarc_idla=1; mbarc_idfm=1; mbarc_format='(....)'; mbarc_filename='unset';
    aisid_idf=37; aisid_idla=1; aisid_idfm=1; aisid_format='(....)'; aisid_filename='unset';
    ajsid_idf=38; ajsid_idla=1; ajsid_idfm=1; ajsid_format='(....)'; ajsid_filename='unset';
    il=$(($il+1))
    if [ $(echo ${lines[$il]} | awk -F' ' '{print NF}') -eq 5 ] && [ ! -z "$(echo ${lines[$il]} | awk -F' ' '{print $4}' | grep '(')" ]
    then
      mbarc_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
      mbarc_idla="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
      mbarc_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
      mbarc_format="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
      mbarc_filename="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
      echo 'mbarc : ' $mbarc_idf $mbarc_idla $mbarc_idfm $mbarc_format $mbarc_filename
      # smc aisid
      il=$(($il+1))
      aisid_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
      aisid_idla="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
      aisid_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
      aisid_format="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
      aisid_filename="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
      echo 'aisid : ' $aisid_idf $aisid_idla $aisid_idfm $aisid_format $aisid_filename
      # smc ajsid
      il=$(($il+1))
      ajsid_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
      ajsid_idla="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
      ajsid_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
      ajsid_format="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
      ajsid_filename="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
      echo 'ajsid : ' $ajsid_idf $ajsid_idla $ajsid_idfm $ajsid_format $ajsid_filename
    else
      il=$(($il-1))
    fi

  # not smc
  else
    SMC=0
    # obst
    obst_idf=70; obst_sf=1.; obst_idla=1; obst_idfm=1; obst_format='(....)'; obst_from='NAME'; obst_filename='unset';
    if [ ! -z "$(grep 'FLAGTR' $forinamelist)" ]
    then
      flagtr=$(grep FLAGTR $forinamelist | awk -F'FLAGTR' '{print $2}' | awk -F'=' '{print $2}' | awk -F',' '{print $1}' | awk -F'/' '{print $1}' | awk -F' ' '{print $1}')
      echo 'flagtr : ' $flagtr
      if [ $flagtr -gt 0 ]
      then
        obst_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
        obst_sf="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
        obst_idla="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
        obst_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
        obst_format="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
        obst_from="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
        obst_filename="$(echo ${lines[$il]} | awk -F' ' '{print $7}' | cut -d \" -f2  | cut -d \' -f2)"
        echo 'obst : ' $obst_idf $obst_sf $obst_idla $obst_idfm $obst_format $obst_from $obst_filename
        if [ "$obst_from" == 'UNIT' ] || [ "$obst_idf" == '10' ]
        then
          foriobst=$cur_dir/${grdname}.obst
          fobst=$cur_dir/${grdname}.obst.new
          obst_filename=$foriobst
          obst_from='NAME'
          rm -f $fobst
          num_total=0
          iread=0
          j=1
          # unfold the along-X array
          while [ $num_total -lt $num_max ];  do
            il=$(($il+1))
            curline="$(echo ${lines[$il]} | sed -e 's/,/ /g')"
            line_elem=$(echo $curline} | awk -F' ' '{print NF}')  
            for n_elem in $(seq 1 $line_elem); do
              curelem=$(echo $curline | awk -F' ' "{print \$$n_elem}" | cut -d \" -f2  | cut -d \' -f2)
              if [ ! -z "$(echo $curelem | grep '*')" ]; then    
                num_times=$(echo $curelem | awk -F'*' '{print $1}')
                val_times=$(echo $curelem | awk -F'*' '{print $2}')
              else
                num_times=1
                val_times=$curelem
              fi
              num_total=$(($num_total + $num_times))
              for t in $(seq 1 $num_times); do
                iread=$(($iread+1))
                obst2d[$iread,$j]="$val_times"
#                echo -n "$val_times " >> $fobst
                if [ $iread -ge $nx ]; then
                  iread=0
                  j=$(($j+1))
#                  echo '' >> $fobst
                fi
              done
            done
            echo "${lines[$il]}" >> $fobst
          done
          # unfold the along-Y array
          echo "carriage return for Y-along array"
          echo -n '' >> $fobst
          while [ $num_total -lt $(($num_max * 2)) ];  do
            il=$(($il+1))
            curline="$(echo ${lines[$il]} | sed -e 's/,/ /g')"
            line_elem=$(echo $curline} | awk -F' ' '{print NF}')
            for n_elem in $(seq 1 $line_elem); do
              curelem=$(echo $curline | awk -F' ' "{print \$$n_elem}" | cut -d \" -f2  | cut -d \' -f2)
              if [ ! -z "$(echo $curelem | grep '*')" ]; then
                num_times=$(echo $curelem | awk -F'*' '{print $1}')
                val_times=$(echo $curelem | awk -F'*' '{print $2}')
              else
                num_times=1
                val_times=$curelem
              fi
              num_total=$(($num_total + $num_times))
              for t in $(seq 1 $num_times); do
                iread=$(($iread+1))
                obst2d[$iread,$j]="$val_times"
#                echo -n "$val_times " >> $fobst
                if [ $iread -ge $nx ]; then
                  iread=0
                  j=$(($j+1))
#                  echo '' >> $fobst
                fi
              done
            done
            echo "${lines[$il]}" >> $fobst
          done
          # save file
          if [ -f $foriobst ]
          then
            if [ -z "$(diff $foriobst $fobst)" ]
            then
              echo $foriobst ' and ' $fobst 'are same.'
              echo 'delete ' $fobst
              rm $fobst
            else
              echo 'diff between :' $foriobst ' and new file : ' $fobst
              echo 'inp2nml conversion stopped'
              exit 1
            fi
          else
            echo 'mv '$fobst ' to ' $foriobst
            mv $fobst $foriobst
          fi
        fi
      else
        il=$(($il-1))
      fi
    else
      il=$(($il-1))
    fi
  fi

fi # RECT or CURV


# mask
mask_idf=60; mask_idla=1; mask_idfm=1; mask_format='(....)'; mask_from='NAME'; mask_filename='unset';
il=$(($il+1))
mask_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
mask_idla="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
mask_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
mask_format="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
mask_from="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
mask_filename="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
echo 'mask : ' $mask_idf $mask_idla $mask_idfm $mask_format $mask_from $mask_filename
if [ "$mask_from" == 'UNIT' ] || [ "$mask_from" == 'NAME' ]
then
  if [ "$mask_idf" == '10' ]
  then
    forimask=$cur_dir/${grdname}.mask
    fmask=$cur_dir/${grdname}.mask.new
    mask_filename=$forimask
    mask_from='NAME'
    rm -f $fmask
    num_total=0
    iread=0
    j=1
    # unfold the array
    while [ $num_total -lt $num_max ];  do
      il=$(($il+1))
      curline="$(echo ${lines[$il]} | sed -e 's/,/ /g')"
      line_elem=$(echo $curline} | awk -F' ' '{print NF}')  
      for n_elem in $(seq 1 $line_elem); do
        curelem=$(echo $curline | awk -F' ' "{print \$$n_elem}" | cut -d \" -f2  | cut -d \' -f2)
        if [ ! -z "$(echo $curelem | grep '*')" ]; then    
          num_times=$(echo $curelem | awk -F'*' '{print $1}')
          val_times=$(echo $curelem | awk -F'*' '{print $2}')
        else
          num_times=1
          val_times=$curelem
        fi
        num_total=$(($num_total + $num_times))
        for t in $(seq 1 $num_times); do
          iread=$(($iread+1))
          mask2d[$iread,$j]="$val_times"
#          echo -n "$val_times " >> $fmask
          if [ $iread -ge $nx ]; then
            iread=0
            j=$(($j+1))
#            echo '' >> $fmask
          fi
        done
      done
      echo "${lines[$il]}" >> $fmask
    done
    # save file
    if [ -f $forimask ]
    then
      if [ -z "$(diff $forimask $fmask)" ]
      then
        echo $forimask ' and ' $fmask 'are same.'
        echo 'delete ' $fmask
        rm $fmask
      else
        echo 'diff between :' $forimask ' and new file : ' $fmask
        echo 'inp2nml conversion stopped'
        exit 1
      fi
    else
      echo 'mv '$fmask ' to ' $forimask
      mv $fmask $forimask
    fi
  fi
elif [ "$mask_from" == 'PART' ]
then
  mask_filename='unset'
  echo 'read map from inbound, excluded and outbound'
fi


# slope
slope_idf=80; slope_sf=1.; slope_idla=1; slope_idfm=1; slope_format='(....)'; slope_from='NAME'; slope_filename='unset';
if [ "$type" == 'RECT' ] || [ "$type" == 'CURV' ]
then
  if [ ! -z "$(grep 'REFMAP' $forinamelist)" ]
  then
    refmap=$(grep REFMAP $forinamelist | awk -F'REFMAP' '{print $2}' | awk -F'=' '{print $2}' | awk -F',' '{print $1}' | awk -F'/' '{print $1}' | awk -F' ' '{print $1}')
    echo 'refmap : ' $refmap
    if [ $(echo "$refmap == 2" | bc -l) -eq 1 ]
    then
      il=$(($il+1))
      slope_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
      slope_sf="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
      slope_idla="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
      slope_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
      slope_format="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
      slope_from="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
      slope_filename="$(echo ${lines[$il]} | awk -F' ' '{print $7}' | cut -d \" -f2  | cut -d \' -f2)"
      echo 'slope : ' $slope_idf $slope_sf $slope_idla $slope_idfm $slope_format $slope_from $slope_filename
      if [ "$slope_from" == 'UNIT' ] || [ "$slope_idf" == '10' ]
      then
        forislope=$cur_dir/${grdname}.slope
        fslope=$cur_dir/${grdname}.slope.new
        slope_filename=$forislope
        slope_from='NAME'
        rm -f $fslope
        num_total=0
        iread=0
        j=1
        # unfold the array
        while [ $num_total -lt $num_max ];  do
          il=$(($il+1))
          curline="$(echo ${lines[$il]} | sed -e 's/,/ /g')"
          line_elem=$(echo $curline} | awk -F' ' '{print NF}')  
          for n_elem in $(seq 1 $line_elem); do
            curelem=$(echo $curline | awk -F' ' "{print \$$n_elem}" | cut -d \" -f2  | cut -d \' -f2)
            if [ ! -z "$(echo $curelem | grep '*')" ]; then    
              num_times=$(echo $curelem | awk -F'*' '{print $1}')
              val_times=$(echo $curelem | awk -F'*' '{print $2}')
            else
              num_times=1
              val_times=$curelem
            fi
            num_total=$(($num_total + $num_times))
            for t in $(seq 1 $num_times); do
              iread=$(($iread+1))
              slope2d[$iread,$j]="$val_times"
#              echo -n "$val_times " >> $fslope
              if [ $iread -ge $nx ]; then
                iread=0
                j=$(($j+1))
#                echo '' >> $fslope
              fi
            done
          done
          echo "${lines[$il]}" >> $fslope
        done
        # save file
        if [ -f $forislope ]
        then
          if [ -z "$(diff $forislope $fslope)" ]
          then
            echo $forislope ' and ' $fslope 'are same.'
            echo 'delete ' $fslope
            rm $fslope
          else
            echo 'diff between :' $forislope ' and new file : ' $fslope
            echo 'inp2nml conversion stopped'
            exit 1
          fi
        else
          echo 'mv '$fslope ' to ' $forislope
          mv $fslope $forislope
        fi
      fi
    fi
  fi
fi


# included
n_inpt=0
if [ "$mask_from" == 'PART' ]
then
  n_inpt=1
  il=$(($il+1))
  inptx[$n_inpt]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  inpty[$n_inpt]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  inptf[$n_inpt]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  echo 'inpt : ' ${inptx[$n_inpt]} ${inpty[$n_inpt]} ${inptf[$n_inpt]}
  while [ "${inptx[$n_inpt]}" != "0" ] || [ "${inpty[$n_inpt]}" != "0" ] || [ "${inptf[$n_inpt]}" != "F" ]
  do
    il=$(($il+1))
    n_inpt=$(($n_inpt+1))
    inptx[$n_inpt]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    inpty[$n_inpt]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    inptf[$n_inpt]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    echo 'inpt : ' ${inptx[$n_inpt]} ${inpty[$n_inpt]} ${inptf[$n_inpt]}
  done
  n_inpt=$(($n_inpt-1))
fi

# excluded points
n_expt=0
if [ "$mask_from" == 'PART' ]
then
  n_expt=1
  il=$(($il+1))
  exptx[$n_expt]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  expty[$n_expt]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  exptf[$n_expt]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  echo 'expt : ' ${exptx[$n_expt]} ${expty[$n_expt]} ${exptf[$n_expt]}
  while [ "${exptx[$n_expt]}" != "0" ] || [ "${expty[$n_expt]}" != "0" ] || [ "${exptf[$n_expt]}" != "F" ]
  do
    il=$(($il+1))
    n_expt=$(($n_expt+1))
    exptx[$n_expt]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    expty[$n_expt]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    exptf[$n_expt]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    echo 'expt : ' ${exptx[$n_expt]} ${expty[$n_expt]} ${exptf[$n_expt]}
  done
  n_expt=$(($n_expt-1))
fi

# excluded bodies
n_exbd=0
if [ "$mask_from" == 'PART' ]
then
  n_exbd=1
  il=$(($il+1))
  exbdx[$n_exbd]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  exbdy[$n_exbd]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  echo 'exbd : ' ${exbdx[$n_exbd]} ${exbdy[$n_exbd]}
  while [ "${exbdx[$n_exbd]}" != "0" ] || [ "${exbdy[$n_exbd]}" != "0" ]
  do
    il=$(($il+1))
    n_exbd=$(($n_exbd+1))
    exbdx[$n_exbd]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    exbdy[$n_exbd]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    echo 'exbd : ' ${exbdx[$n_exbd]} ${exbdy[$n_exbd]}
  done
  n_exbd=$(($n_exbd-1))
fi

# sed
sed_idf=90; sed_sf=1.; sed_idla=1; sed_idfm=1; sed_format='(....)'; sed_from='NAME'; sed_filename='unset';
if [ ! -z "$(grep 'SEDMAPD50' $forinamelist)" ]
then
  sedmapd50=$(grep SEDMAPD50 $forinamelist | awk -F'SEDMAPD50' '{print $2}' | awk -F'=' '{print $2}' | awk -F',' '{print $1}' | awk -F'/' '{print $1}' | awk -F' ' '{print $1}')
  echo 'sedmapd50 : ' $sedmapd50
  if [ $sedmapd50 ]
  then
    il=$(($il+1))
    sed_idf="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
    sed_sf="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
    sed_idla="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
    sed_idfm="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
    sed_format="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
    sed_from="$(echo ${lines[$il]} | awk -F' ' '{print $6}' | cut -d \" -f2  | cut -d \' -f2)"
    sed_filename="$(echo ${lines[$il]} | awk -F' ' '{print $7}' | cut -d \" -f2  | cut -d \' -f2)"
    echo 'sed : ' $sed_idf $sed_sf $sed_idla $sed_idfm $sed_format $sed_from $sed_filename
    if [ "$sed_from" == 'UNIT' ] || [ "$sed_idf" == '10' ]
    then
      forised=$cur_dir/${grdname}.sed
      fsed=$cur_dir/${grdname}.sed.new
      sed_filename=$forised
      sed_from='NAME'
      rm -f $fsed
      num_total=0
      iread=0
      j=1
      # unfold the array
      while [ $num_total -lt $num_max ];  do
        il=$(($il+1))
        curline="$(echo ${lines[$il]} | sed -e 's/,/ /g')"
        line_elem=$(echo $curline} | awk -F' ' '{print NF}')  
        for n_elem in $(seq 1 $line_elem); do
          curelem=$(echo $curline | awk -F' ' "{print \$$n_elem}" | cut -d \" -f2  | cut -d \' -f2)
          if [ ! -z "$(echo $curelem | grep '*')" ]; then    
            num_times=$(echo $curelem | awk -F'*' '{print $1}')
            val_times=$(echo $curelem | awk -F'*' '{print $2}')
          else
            num_times=1
            val_times=$curelem
          fi
          num_total=$(($num_total + $num_times))
          for t in $(seq 1 $num_times); do
            iread=$(($iread+1))
            sed2d[$iread,$j]="$val_times"
#            echo -n "$val_times " >> $fsed
            if [ $iread -ge $nx ]; then
              iread=0
              j=$(($j+1))
#              echo '' >> $fsed
            fi
          done
        done
        echo "${lines[$il]}" >> $fsed
      done
      # save file
      if [ -f $forised ]
      then
        if [ -z "$(diff $forised $fsed)" ]
        then
          echo $forised ' and ' $fsed 'are same.'
          echo 'delete ' $fsed
          rm $fsed
        else
          echo 'diff between :' $forised ' and new file : ' $fsed
          echo 'inp2nml conversion stopped'
          exit 1
        fi
      else
        echo 'mv '$fsed ' to ' $forised
        mv $fsed $forised
      fi
    fi
  fi
fi

#outbound
n_ouln=1
il=$(($il+1))
oulnx0[$n_ouln]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
oulny0[$n_ouln]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
oulndx[$n_ouln]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
oulndy[$n_ouln]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
oulnnp[$n_ouln]="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
echo 'ouln : ' ${oulnx0[$n_ouln]} ${oulny0[$n_ouln]} ${oulndx[$n_ouln]} ${oulndy[$n_ouln]} ${oulnnp[$n_ouln]}
while [ "${oulnx0[$n_ouln]}" != "0." ] || [ "${oulny0[$n_ouln]}" != "0." ] || \
      [ "${oulndx[$n_ouln]}" != "0." ] || [ "${oulndy[$n_ouln]}" != "0." ] || [ "${oulnnp[$n_ouln]}" != "0" ]
do
  il=$(($il+1))
  n_ouln=$(($n_ouln+1))
  oulnx0[$n_ouln]="$(echo ${lines[$il]} | awk -F' ' '{print $1}' | cut -d \" -f2  | cut -d \' -f2)"
  oulny0[$n_ouln]="$(echo ${lines[$il]} | awk -F' ' '{print $2}' | cut -d \" -f2  | cut -d \' -f2)"
  oulndx[$n_ouln]="$(echo ${lines[$il]} | awk -F' ' '{print $3}' | cut -d \" -f2  | cut -d \' -f2)"
  oulndy[$n_ouln]="$(echo ${lines[$il]} | awk -F' ' '{print $4}' | cut -d \" -f2  | cut -d \' -f2)"
  oulnnp[$n_ouln]="$(echo ${lines[$il]} | awk -F' ' '{print $5}' | cut -d \" -f2  | cut -d \' -f2)"
  echo 'ouln : ' ${oulnx0[$n_ouln]} ${oulny0[$n_ouln]} ${oulndx[$n_ouln]} ${oulndy[$n_ouln]} ${oulnnp[$n_ouln]}
done
n_ouln=$(($n_ouln-1))



#------------------------------
# write value in a new nml file

nmlfile=$cur_dir/$(basename $inp .inp).nml

# header
cat > $nmlfile << EOF
! -------------------------------------------------------------------- !
! WAVEWATCH III - ww3_grid.nml - Grid pre-processing                   !
! -------------------------------------------------------------------- !

EOF

# spectrum namelist
cat >> $nmlfile << EOF
! -------------------------------------------------------------------- !
! Define the spectrum parameterization via SPECTRUM_NML namelist
!
! * namelist must be terminated with /
! * definitions & defaults:
!     SPECTRUM%XFR         = 0.            ! frequency increment
!     SPECTRUM%FREQ1       = 0.            ! first frequency (Hz)
!     SPECTRUM%NK          = 0             ! number of frequencies (wavenumbers)
!     SPECTRUM%NTH         = 0             ! number of direction bins
!     SPECTRUM%THOFF       = 0.            ! relative offset of first direction [-0.5,0.5]
! -------------------------------------------------------------------- !
&SPECTRUM_NML
EOF

if [ "$xfr" != 0. ];    then  echo "  SPECTRUM%XFR       =  $xfr" >> $nmlfile; fi
if [ "$freq1" != 0. ];  then  echo "  SPECTRUM%FREQ1     =  $freq1" >> $nmlfile; fi
if [ "$nk" != 0 ];      then  echo "  SPECTRUM%NK        =  $nk" >> $nmlfile; fi
if [ "$nth" != 0 ];     then  echo "  SPECTRUM%NTH       =  $nth" >> $nmlfile; fi
if [ "$thoff" != 0. ];  then  echo "  SPECTRUM%THOFF     =  $thoff" >> $nmlfile; fi



# run namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the run parameterization via RUN_NML namelist
!
! * namelist must be terminated with /
! * definitions & defaults:
!     RUN%FLDRY            = F             ! dry run (I/O only, no calculation)
!     RUN%FLCX             = F             ! x-component of propagation
!     RUN%FLCY             = F             ! y-component of propagation
!     RUN%FLCTH            = F             ! direction shift
!     RUN%FLCK             = F             ! wavenumber shift
!     RUN%FLSOU            = F             ! source terms
! -------------------------------------------------------------------- !
&RUN_NML
EOF

if [ "$fldry" != F ];    then  echo "  RUN%FLDRY        =  $fldry" >> $nmlfile; fi
if [ "$flcx" != F ];     then  echo "  RUN%FLCX         =  $flcx" >> $nmlfile; fi
if [ "$flcy" != F ];     then  echo "  RUN%FLCY         =  $flcy" >> $nmlfile; fi
if [ "$flcth" != F ];    then  echo "  RUN%FLCTH        =  $flcth" >> $nmlfile; fi
if [ "$flck" != F ];     then  echo "  RUN%FLCK         =  $flck" >> $nmlfile; fi
if [ "$flsou" != F ];    then  echo "  RUN%FLSOU        =  $flsou" >> $nmlfile; fi



# timesteps namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the timesteps parameterization via TIMESTEPS_NML namelist
!
! * It is highly recommended to set up time steps which are multiple 
!   between them. 
!
! * The first time step to calculate is the maximum CFL time step
!   which depend on the lowest frequency FREQ1 previously set up and the
!   lowest spatial grid resolution in meters DXY.
!   reminder : 1 degree=60minutes // 1minute=1mile // 1mile=1.852km
!   The formula for the CFL time is :
!   Tcfl = DXY / (G / (FREQ1*4*Pi) ) with the constants Pi=3,14 and G=9.8m/s²;
!   DTXY  ~= 90% Tcfl
!   DTMAX ~= 3 * DTXY   (maximum global time step limit)
!
! * The refraction time step depends on how strong can be the current velocities
!   on your grid :
!   DTKTH ~= DTMAX / 2   ! in case of no or light current velocities
!   DTKTH ~= DTMAX / 10  ! in case of strong current velocities
!
! * The source terms time step is usually defined between 5s and 60s.
!   A common value is 10s.
!   DTMIN ~= 10
!
! * namelist must be terminated with /
! * definitions & defaults:
!     TIMESTEPS%DTMAX      = 0.         ! maximum global time step (s)
!     TIMESTEPS%DTXY       = 0.         ! maximum CFL time step for x-y (s)
!     TIMESTEPS%DTKTH      = 0.         ! maximum CFL time step for k-th (s)
!     TIMESTEPS%DTMIN      = 0.         ! minimum source term time step (s)
! -------------------------------------------------------------------- !
&TIMESTEPS_NML
EOF

if [ "$dtmax" != 0. ];    then  echo "  TIMESTEPS%DTMAX        =  $dtmax" >> $nmlfile; fi
if [ "$dtxy" != 0. ];     then  echo "  TIMESTEPS%DTXY         =  $dtxy" >> $nmlfile; fi
if [ "$dtkth" != 0. ];    then  echo "  TIMESTEPS%DTKTH        =  $dtkth" >> $nmlfile; fi
if [ "$dtmin" != 0. ];    then  echo "  TIMESTEPS%DTMIN        =  $dtmin" >> $nmlfile; fi

# grid namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the grid to preprocess via GRID_NML namelist
!
! * the tunable parameters for source terms, propagation schemes, and 
!    numerics are read using namelists. 
! * Any namelist found in the folowing sections is temporarily written
!   to param.scratch, and read from there if necessary. 
! * The order of the namelists is immaterial.
! * Namelists not needed for the given switch settings will be skipped
!   automatically
!
! * grid type can be : 
!    'RECT' : rectilinear
!    'CURV' : curvilinear
!    'UNST' : unstructured (triangle-based)
!
! * coordinate system can be : 
!    'SPHE' : Spherical (degrees)
!    'CART' : Cartesian (meters)
!
! * grid closure can only be applied in spherical coordinates
!
! * grid closure can be : 
!    'NONE' : No closure is applied
!    'SMPL' : Simple grid closure. Grid is periodic in the
!           : i-index and wraps at i=NX+1. In other words,
!           : (NX+1,J) => (1,J). A grid with simple closure
!           : may be rectilinear or curvilinear.
!    'TRPL' : Tripole grid closure : Grid is periodic in the
!           : i-index and wraps at i=NX+1 and has closure at
!           : j=NY+1. In other words, (NX+1,J<=NY) => (1,J)
!           : and (I,NY+1) => (NX-I+1,NY). Tripole
!           : grid closure requires that NX be even. A grid
!           : with tripole closure must be curvilinear.
!
! * The coastline limit depth is the value which distinguish the sea 
!   points to the land points. All the points with depth values (ZBIN)
!   greater than this limit (ZLIM) will be considered as excluded points
!   and will never be wet points, even if the water level grows over.
!   It can only overwrite the status of a sea point to a land point.
!   The value must have a negative value under the mean sea level
!
! * The minimum water depth allowed to compute the model is the absolute
!   depth value (DMIN) used in the model if the input depth is lower to 
!   avoid the model to blow up.
!
! * namelist must be terminated with /
! * definitions & defaults:
!     GRID%NAME             = 'unset'            ! grid name (30 char)
!     GRID%NML              = 'namelists.nml'    ! namelists filename
!     GRID%TYPE             = 'unset'            ! grid type
!     GRID%COORD            = 'unset'            ! coordinate system
!     GRID%CLOS             = 'unset'            ! grid closure
!
!     GRID%ZLIM             = 0.        ! coastline limit depth (m)
!     GRID%DMIN             = 0.        ! abs. minimum water depth (m)
! -------------------------------------------------------------------- !
&GRID_NML
EOF

if [ "$gridname" != 'unset' ];               then  echo "  GRID%NAME         =  '$gridname'" >> $nmlfile; fi
if [ "$nml_filename" != 'namelists.nml' ];   then  echo "  GRID%NML          =  '$nml_filename'" >> $nmlfile; fi
if [ "$type" != 'unset' ];                   then  echo "  GRID%TYPE         =  '$type'" >> $nmlfile; fi
if [ "$coord" != 'unset' ];                  then  echo "  GRID%COORD        =  '$coord'" >> $nmlfile; fi
if [ "$clos" != 'unset' ];                   then  echo "  GRID%CLOS         =  '$clos'" >> $nmlfile; fi
if [ "$depth_zlim" != 0. ];                  then  echo "  GRID%ZLIM         =  $depth_zlim" >> $nmlfile; fi
if [ "$depth_dmin" != 0. ];                  then  echo "  GRID%DMIN         =  $depth_dmin" >> $nmlfile; fi


if [ "$type" == 'RECT' ]; then

# rect namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the rectilinear grid type via RECT_NML namelist
! - only for RECT grids -
!
! * The minimum grid size is 3x3.
!
! * If the grid increments SX and SY are given in minutes of arc, the scaling
!   factor SF must be set to 60. to provide an increment factor in degree.
!
! * If CSTRG='SMPL', then SX is forced to 360/NX.
!
! * value <= value_read / scale_fac
!
! * namelist must be terminated with /
! * definitions & defaults:
!     RECT%NX               = 0        ! number of points along x-axis
!     RECT%NY               = 0        ! number of points along y-axis
!
!     RECT%SX               = 0.       ! grid increment along x-axis
!     RECT%SY               = 0.       ! grid increment along y-axis
!     RECT%SF               = 1.       ! scaling division factor for x-y axis
!
!     RECT%X0               = 0.       ! x-coordinate of lower-left corner (deg)
!     RECT%Y0               = 0.       ! y-coordinate of lower-left corner (deg)
!     RECT%SF0              = 1.       ! scaling division factor for x0,y0 coord
! -------------------------------------------------------------------- !
&RECT_NML
EOF

  if [ "$nx" != 0 ];           then  echo "  RECT%NX           =  $nx" >> $nmlfile; fi
  if [ "$ny" != 0 ];           then  echo "  RECT%NY           =  $ny" >> $nmlfile; fi
  if [ "$rect_sx" != 0. ];     then  echo "  RECT%SX           =  $rect_sx" >> $nmlfile; fi
  if [ "$rect_sy" != 0. ];     then  echo "  RECT%SY           =  $rect_sy" >> $nmlfile; fi
  if [ "$rect_sf" != 1. ];     then  echo "  RECT%SF           =  $rect_sf" >> $nmlfile; fi
  if [ "$rect_x0" != 0. ];     then  echo "  RECT%X0           =  $rect_x0" >> $nmlfile; fi
  if [ "$rect_y0" != 0. ];     then  echo "  RECT%Y0           =  $rect_y0" >> $nmlfile; fi
  if [ "$rect_sf0" != 1. ];    then  echo "  RECT%SF0          =  $rect_sf0" >> $nmlfile; fi

fi # RECT

if [ "$type" == 'CURV' ]; then

# curv namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the curvilinear grid type via CURV_NML namelist
! - only for CURV grids -
!
! * The minimum grid size is 3x3.
!
! * If CSTRG='SMPL', then SX is forced to 360/NX.
!
! * value <= scale_fac * value_read + add_offset
!
! * IDLA : Layout indicator :
!                  1   : Read line-by-line bottom to top. (default)
!                  2   : Like 1, single read statement.
!                  3   : Read line-by-line top to bottom.
!                  4   : Like 3, single read statement.
! * IDFM : format indicator :
!                  1   : Free format. (default)
!                  2   : Fixed format.
!                  3   : Unformatted.
! * FORMAT : element format to read :
!               '(....)'  : auto detected (default)
!               '(f10.6)' : float type
!
! * Example :
!      IDF  SF    OFF  IDLA  IDFM  FORMAT    FILENAME
!      21   0.25 -0.5  3     1    '(....)'  'x.inp'
!      22   0.25  0.5  3     1    '(....)'  'y.inp'
!
! * namelist must be terminated with /
! * definitions & defaults:
!     CURV%NX               = 0        ! number of points along x-axis
!     CURV%NY               = 0        ! number of points along y-axis
!
!     CURV%XCOORD%SF         = 1.       ! x-coord scale factor 
!     CURV%XCOORD%OFF        = 0.       ! x-coord add offset
!     CURV%XCOORD%FILENAME   = 'unset'  ! x-coord filename
!     CURV%XCOORD%IDF        = 21       ! x-coord file unit number
!     CURV%XCOORD%IDLA       = 1        ! x-coord layout indicator
!     CURV%XCOORD%IDFM       = 1        ! x-coord format indicator
!     CURV%XCOORD%FORMAT     = '(....)' ! x-coord formatted read format
!
!     CURV%YCOORD%SF         = 1.       ! y-coord scale factor 
!     CURV%YCOORD%OFF        = 0.       ! y-coord add offset
!     CURV%YCOORD%FILENAME   = 'unset'  ! y-coord filename
!     CURV%YCOORD%IDF        = 22       ! y-coord file unit number
!     CURV%YCOORD%IDLA       = 1        ! y-coord layout indicator
!     CURV%YCOORD%IDFM       = 1        ! y-coord format indicator
!     CURV%YCOORD%FORMAT     = '(....)' ! y-coord formatted read format
! -------------------------------------------------------------------- !
&CURV_NML
EOF

  if [ "$nx" != 0. ];                   then  echo "  CURV%NX              =  $nx" >> $nmlfile; fi
  if [ "$ny" != 0. ];                   then  echo "  CURV%NY              =  $ny" >> $nmlfile; fi
  if [ "$xcoord_sf" != 1. ];            then  echo "  CURV%XCOORD%SF        =  $xcoord_sf" >> $nmlfile; fi
  if [ "$xcoord_off" != 0. ];           then  echo "  CURV%XCOORD%OFF       =  $xcoord_off" >> $nmlfile; fi
  if [ "$xcoord_filename" != 'unset' ]; then  echo "  CURV%XCOORD%FILENAME  =  '$xcoord_filename'" >> $nmlfile; fi
#  if [ "$xcoord_idf" != 21 ];           then  echo "  CURV%XCOORD%IDF       =  $xcoord_idf" >> $nmlfile; fi
  if [ "$xcoord_idla" != 1 ];           then  echo "  CURV%XCOORD%IDLA      =  $xcoord_idla" >> $nmlfile; fi
  if [ "$xcoord_idfm" != 1 ];           then  echo "  CURV%XCOORD%IDFM      =  $xcoord_idfm" >> $nmlfile; fi
  if [ "$xcoord_format" != '(....)' ];  then  echo "  CURV%XCOORD%FORMAT    =  '$xcoord_format'" >> $nmlfile; fi
#  if [ "$xcoord_from" != 'NAME' ];      then  echo "  CURV%XCOORD%FROM      =  '$xcoord_from'" >> $nmlfile; fi
#
  if [ "$ycoord_sf" != 1. ];            then  echo "  CURV%YCOORD%SF        =  $ycoord_sf" >> $nmlfile; fi
  if [ "$ycoord_off" != 0. ];           then  echo "  CURV%YCOORD%OFF       =  $ycoord_off" >> $nmlfile; fi
  if [ "$ycoord_filename" != 'unset' ]; then  echo "  CURV%YCOORD%FILENAME  =  '$ycoord_filename'" >> $nmlfile; fi
#  if [ "$ycoord_idf" != 22 ];           then  echo "  CURV%YCOORD%IDF       =  $ycoord_idf" >> $nmlfile; fi
  if [ "$ycoord_idla" != 1 ];           then  echo "  CURV%YCOORD%IDLA      =  $ycoord_idla" >> $nmlfile; fi
  if [ "$ycoord_idfm" != 1 ];           then  echo "  CURV%YCOORD%IDFM      =  $ycoord_idfm" >> $nmlfile; fi
  if [ "$ycoord_format" != '(....)' ];  then  echo "  CURV%YCOORD%FORMAT    =  '$ycoord_format'" >> $nmlfile; fi
#  if [ "$ycoord_from" != 'NAME' ];      then  echo "  CURV%YCOORD%FROM      =  '$ycoord_from'" >> $nmlfile; fi

fi # CURV


if [ "$type" == 'UNST' ]; then

# unst namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the unstructured grid type via UNST_NML namelist
! - only for UNST grids -
!
! * The minimum grid size is 3x3.
!
! * &MISC namelist must be removed
!
! * The depth value must have negative values under the mean sea level
!
! * The map value must be set as :
!    -2 : Excluded boundary point (covered by ice)
!    -1 : Excluded sea point (covered by ice)
!     0 : Excluded land point
!     1 : Sea point
!     2 : Active boundary point
!     3 : Excluded grid point
!     7 : Ice point
!
! * the file must be a GMESH grid file containing node and element lists.
!
! * Extra open boundary list file with UGOBCFILE in namelist &UNST
!   An example is given in regtest ww3_tp2.7
!
! * value <= scale_fac * value_read
!
! * IDLA : Layout indicator :
!                  1   : Read line-by-line bottom to top. (default)
!                  2   : Like 1, single read statement.
!                  3   : Read line-by-line top to bottom.
!                  4   : Like 3, single read statement.
! * IDFM : format indicator :
!                  1   : Free format. (default)
!                  2   : Fixed format.
!                  3   : Unformatted.
! * FORMAT : element format to read :
!               '(....)'  : auto detected (default)
!               '(f10.6)' : float type
!
! * Example :
!      IDF  SF   IDLA  IDFM   FORMAT       FILENAME
!      20  -1.   4     2     '(20f10.2)'  'ngug.msh'
!
! * namelist must be terminated with /
! * definitions & defaults:
!     UNST%SF             = 1.       ! unst scale factor
!     UNST%FILENAME       = 'unset'  ! unst filename
!     UNST%IDF            = 20       ! unst file unit number
!     UNST%IDLA           = 1        ! unst layout indicator
!     UNST%IDFM           = 1        ! unst format indicator
!     UNST%FORMAT         = '(....)' ! unst formatted read format
!
!     UNST%UGOBCFILE      = 'unset'  ! additional boundary list file
! -------------------------------------------------------------------- !
&UNST_NML
EOF

  if [ "$depth_sf" != 1. ];             then  echo "  UNST%SF          =  $depth_sf" >> $nmlfile; fi
  if [ "$depth_filename" != 'unset' ];  then  echo "  UNST%FILENAME    =  '$depth_filename'" >> $nmlfile; fi
#  if [ "$depth_idf" != 20 ];           then  echo "  UNST%IDF       =  $depth_idf" >> $nmlfile; fi
  if [ "$depth_idla" != 1 ];            then  echo "  UNST%IDLA      =  $depth_idla" >> $nmlfile; fi
  if [ "$depth_idfm" != 1 ];            then  echo "  UNST%IDFM      =  $depth_idfm" >> $nmlfile; fi
  if [ "$depth_format" != '(....)' ];   then  echo "  UNST%FORMAT    =  '$depth_format'" >> $nmlfile; fi
  if [ "$unst_ugobcfile" != 'unset' ];  then  echo "  UNST%UGOBCFILE   =  '$unst_ugobcfile'" >> $nmlfile; fi


fi # UNST


if [ "$mcels_filename" != 'unset' ]; then

# smc namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the spherical multiple-cell grid via SMC_NML namelist
! - only for SMC grids -
!
! * SMC cell 'MCELS' and face 'ISIDE & JSIDE' arrays
!   and obstruction ratio 'SUBTR'.
!
! * The input boundary cell file 'BUNDY' is only needed when NBISMC > 0.
!   Boundary cell id list file (unit 35) is only required if boundary 
!   cell number entered above is non-zero.  The cell id number should be 
!   the sequential number in the cell array (unit 31) S625MCels.dat.
!
! * Extra cell and face arrays for Arctic part if switch ARC is selected. 
!
! * Example :
!      IDF  IDLA  IDFM  FORMAT   FILENAME
!      31   1     1    '(....)'  'S6125MCels.dat'
!      32   1     1    '(....)'  'S6125ISide.dat'
!      33   1     1    '(....)'  'S6125JSide.dat'
!      34   1     1    '(....)'  'SMC25Subtr.dat'
!      35   1     1    '(....)'  'S6125Bundy.dat'
!      36   1     1    '(....)'  'S6125MBArc.dat'
!      37   1     1    '(....)'  'S6125AISid.dat'
!      38   1     1    '(....)'  'S6125AJSid.dat'
!
! * namelist must be terminated with /
! * definitions & defaults:
!     SMC%MCELS%FILENAME       = 'unset'  ! MCels filename
!     SMC%MCELS%IDF            = 31       ! MCels file unit number
!     SMC%MCELS%IDLA           = 1        ! MCels layout indicator
!     SMC%MCELS%IDFM           = 1        ! MCels format indicator
!     SMC%MCELS%FORMAT         = '(....)' ! MCels formatted read format
!
!     SMC%ISIDE%FILENAME       = 'unset'  ! ISide filename
!     SMC%ISIDE%IDF            = 32       ! ISide file unit number
!     SMC%ISIDE%IDLA           = 1        ! ISide layout indicator
!     SMC%ISIDE%IDFM           = 1        ! ISide format indicator
!     SMC%ISIDE%FORMAT         = '(....)' ! ISide formatted read format
!
!     SMC%JSIDE%FILENAME       = 'unset'  ! JSide filename
!     SMC%JSIDE%IDF            = 33       ! JSide file unit number
!     SMC%JSIDE%IDLA           = 1        ! JSide layout indicator
!     SMC%JSIDE%IDFM           = 1        ! JSide format indicator
!     SMC%JSIDE%FORMAT         = '(....)' ! JSide formatted read format
!
!     SMC%SUBTR%FILENAME       = 'unset'  ! Subtr filename
!     SMC%SUBTR%IDF            = 34       ! Subtr file unit number
!     SMC%SUBTR%IDLA           = 1        ! Subtr layout indicator
!     SMC%SUBTR%IDFM           = 1        ! Subtr format indicator
!     SMC%SUBTR%FORMAT         = '(....)' ! Subtr formatted read format
!
!     SMC%BUNDY%FILENAME       = 'unset'  ! Bundy filename
!     SMC%BUNDY%IDF            = 35       ! Bundy file unit number
!     SMC%BUNDY%IDLA           = 1        ! Bundy layout indicator
!     SMC%BUNDY%IDFM           = 1        ! Bundy format indicator
!     SMC%BUNDY%FORMAT         = '(....)' ! Bundy formatted read format
!
!     SMC%MBARC%FILENAME       = 'unset'  ! MBArc filename
!     SMC%MBARC%IDF            = 36       ! MBArc file unit number
!     SMC%MBARC%IDLA           = 1        ! MBArc layout indicator
!     SMC%MBARC%IDFM           = 1        ! MBArc format indicator
!     SMC%MBARC%FORMAT         = '(....)' ! MBArc formatted read format
!
!     SMC%AISID%FILENAME       = 'unset'  ! AISid filename
!     SMC%AISID%IDF            = 37       ! AISid file unit number
!     SMC%AISID%IDLA           = 1        ! AISid layout indicator
!     SMC%AISID%IDFM           = 1        ! AISid format indicator
!     SMC%AISID%FORMAT         = '(....)' ! AISid formatted read format
!
!     SMC%AJSID%FILENAME       = 'unset'  ! AJSid filename
!     SMC%AJSID%IDF            = 38       ! AJSid file unit number
!     SMC%AJSID%IDLA           = 1        ! AJSid layout indicator
!     SMC%AJSID%IDFM           = 1        ! AJSid format indicator
!     SMC%AJSID%FORMAT         = '(....)' ! AJSid formatted read format
! -------------------------------------------------------------------- !
&SMC_NML
EOF

  if [ "$mcels_filename" != 'unset' ];   then  echo "  SMC%MCELS%FILENAME        =  '$mcels_filename'" >> $nmlfile; fi
#  if [ "$mcels_idf" != 31 ];             then  echo "  SMC%MCELS%IDF             =  $mcels_idf" >> $nmlfile; fi
  if [ "$mcels_idla" != 1 ];             then  echo "  SMC%MCELS%IDLA            =  $mcels_idla" >> $nmlfile; fi
  if [ "$mcels_idfm" != 1 ];             then  echo "  SMC%MCELS%IDFM            =  $mcels_idfm" >> $nmlfile; fi
  if [ "$mcels_format" != '(....)' ];     then  echo "  SMC%MCELS%FORMAT          =  '$mcels_format'" >> $nmlfile; fi
#
  if [ "$iside_filename" != 'unset' ];   then  echo "  SMC%ISIDE%FILENAME        =  '$iside_filename'" >> $nmlfile; fi
#  if [ "$iside_idf" != 32 ];             then  echo "  SMC%ISIDE%IDF             =  $iside_idf" >> $nmlfile; fi
  if [ "$iside_idla" != 1 ];             then  echo "  SMC%ISIDE%IDLA            =  $iside_idla" >> $nmlfile; fi
  if [ "$iside_idfm" != 1 ];             then  echo "  SMC%ISIDE%IDFM            =  $iside_idfm" >> $nmlfile; fi
  if [ "$iside_format" != '(....)' ];     then  echo "  SMC%ISIDE%FORMAT          =  '$iside_format'" >> $nmlfile; fi
#
  if [ "$jside_filename" != 'unset' ];   then  echo "  SMC%JSIDE%FILENAME        =  '$jside_filename'" >> $nmlfile; fi
#  if [ "$jside_idf" != 33 ];             then  echo "  SMC%JSIDE%IDF             =  $jside_idf" >> $nmlfile; fi
  if [ "$jside_idla" != 1 ];             then  echo "  SMC%JSIDE%IDLA            =  $jside_idla" >> $nmlfile; fi
  if [ "$jside_idfm" != 1 ];             then  echo "  SMC%JSIDE%IDFM            =  $jside_idfm" >> $nmlfile; fi
  if [ "$jside_format" != '(....)' ];     then  echo "  SMC%JSIDE%FORMAT          =  '$jside_format'" >> $nmlfile; fi
#
  if [ "$obst_filename" != 'unset' ];   then  
                                                echo "  SMC%SUBTR%FILENAME        =  '$obst_filename'" >> $nmlfile;
#    if [ "$obst_idf" != 34 ];            then  echo "  SMC%SUBTR%IDF             =  $obst_idf" >> $nmlfile; fi
    if [ "$obst_idla" != 1 ];             then  echo "  SMC%SUBTR%IDLA            =  $obst_idla" >> $nmlfile; fi
    if [ "$obst_idfm" != 1 ];             then  echo "  SMC%SUBTR%IDFM            =  $obst_idfm" >> $nmlfile; fi
    if [ "$obst_format" != '(....)' ];     then  echo "  SMC%SUBTR%FORMAT          =  '$obst_format'" >> $nmlfile; fi
  fi # subtr
#
  if [ "$bundy_filename" != 'unset' ];   then  
                                                 echo "  SMC%BUNDY%FILENAME        =  '$bundy_filename'" >> $nmlfile; 
#    if [ "$bundy_idf" != 35 ];             then  echo "  SMC%BUNDY%IDF             =  $bundy_idf" >> $nmlfile; fi
    if [ "$bundy_idla" != 1 ];             then  echo "  SMC%BUNDY%IDLA            =  $bundy_idla" >> $nmlfile; fi
    if [ "$bundy_idfm" != 1 ];             then  echo "  SMC%BUNDY%IDFM            =  $bundy_idfm" >> $nmlfile; fi
    if [ "$bundy_format" != '(....)' ];     then  echo "  SMC%BUNDY%FORMAT          =  '$bundy_format'" >> $nmlfile; fi
  fi # bundy
#
  if [ "$mbarc_filename" != 'unset' ]; then 
                                                 echo "  SMC%MBARC%FILENAME        =  '$mbarc_filename'" >> $nmlfile;
#    if [ "$mbarc_idf" != 36 ];             then  echo "  SMC%MBARC%IDF             =  $mbarc_idf" >> $nmlfile; fi
    if [ "$mbarc_idla" != 1 ];             then  echo "  SMC%MBARC%IDLA            =  $mbarc_idla" >> $nmlfile; fi
    if [ "$mbarc_idfm" != 1 ];             then  echo "  SMC%MBARC%IDFM            =  $mbarc_idfm" >> $nmlfile; fi
    if [ "$mbarc_format" != '(....)' ];     then  echo "  SMC%MBARC%FORMAT          =  '$mbarc_format'" >> $nmlfile; fi
#
    if [ "$aisid_filename" != 'unset' ];   then  echo "  SMC%AISID%FILENAME        =  '$aisid_filename'" >> $nmlfile; fi
#    if [ "$aisid_idf" != 37 ];             then  echo "  SMC%AISID%IDF             =  $aisid_idf" >> $nmlfile; fi
    if [ "$aisid_idla" != 1 ];             then  echo "  SMC%AISID%IDLA            =  $aisid_idla" >> $nmlfile; fi
    if [ "$aisid_idfm" != 1 ];             then  echo "  SMC%AISID%IDFM            =  $aisid_idfm" >> $nmlfile; fi
    if [ "$aisid_format" != '(....)' ];     then  echo "  SMC%AISID%FORMAT          =  '$aisid_format'" >> $nmlfile; fi
#
    if [ "$ajsid_filename" != 'unset' ];   then  echo "  SMC%AJSID%FILENAME        =  '$ajsid_filename'" >> $nmlfile; fi
#    if [ "$ajsid_idf" != 38 ];             then  echo "  SMC%AJSID%IDF             =  $ajsid_idf" >> $nmlfile; fi
    if [ "$ajsid_idla" != 1 ];             then  echo "  SMC%AJSID%IDLA            =  $ajsid_idla" >> $nmlfile; fi
    if [ "$ajsid_idfm" != 1 ];             then  echo "  SMC%AJSID%IDFM            =  $ajsid_idfm" >> $nmlfile; fi
    if [ "$ajsid_format" != '(....)' ];     then  echo "  SMC%AJSID%FORMAT          =  '$ajsid_format'" >> $nmlfile; fi
  fi # mbarc

fi # SMC


if [ "$type" == 'RECT' ] || [ "$type" == 'CURV' ]; then 
  if [ $SMC == 0 ]; then

# depth namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the depth to preprocess via DEPTH_NML namelist
! - for RECT and CURV grids -
!
! * if no obstruction subgrid, need to set &MISC FLAGTR = 0
!
! * The depth value must have negative values under the mean sea level
!
! * value <= value_read * scale_fac
!
! * IDLA : Layout indicator :
!                  1   : Read line-by-line bottom to top.  (default)
!                  2   : Like 1, single read statement.
!                  3   : Read line-by-line top to bottom.
!                  4   : Like 3, single read statement.
! * IDFM : format indicator :
!                  1   : Free format.  (default)
!                  2   : Fixed format.
!                  3   : Unformatted.
! * FORMAT : element format to read :
!               '(....)'  : auto detected  (default)
!               '(f10.6)' : float type
!
! * Example :
!      IDF  SF     IDLA  IDFM   FORMAT    FILENAME
!      50   0.001  1     1     '(....)'  'GLOB-30M.bot'
!
! * namelist must be terminated with /
! * definitions & defaults:
!     DEPTH%SF             = 1.       ! scale factor
!     DEPTH%FILENAME       = 'unset'  ! filename
!     DEPTH%IDF            = 50       ! file unit number
!     DEPTH%IDLA           = 1        ! layout indicator
!     DEPTH%IDFM           = 1        ! format indicator
!     DEPTH%FORMAT         = '(....)' ! formatted read format
! -------------------------------------------------------------------- !
&DEPTH_NML
EOF

  if [ "$depth_sf" != 1. ];            then  echo "  DEPTH%SF        =  $depth_sf" >> $nmlfile; fi
  if [ "$depth_filename" != 'unset' ]; then  echo "  DEPTH%FILENAME  =  '$depth_filename'" >> $nmlfile; fi
#  if [ "$depth_idf" != 50 ];           then  echo "  DEPTH%IDF       =  $depth_idf" >> $nmlfile; fi
  if [ "$depth_idla" != 1 ];           then  echo "  DEPTH%IDLA      =  $depth_idla" >> $nmlfile; fi
  if [ "$depth_idfm" != 1 ];           then  echo "  DEPTH%IDFM      =  $depth_idfm" >> $nmlfile; fi
  if [ "$depth_format" != '(....)' ];  then  echo "  DEPTH%FORMAT    =  '$depth_format'" >> $nmlfile; fi
#  if [ "$depth_from" != 'NAME' ];      then  echo "  DEPTH%FROM      =  '$depth_from'" >> $nmlfile; fi

  fi # SMC
fi # RECT or CURV


if [ "$type" == 'RECT' ] || [ "$type" == 'CURV' ]; then 
  if [ $SMC == 0 ]; then
    if [ "$mask_filename" != 'unset' ]; then
# mask namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the point status map via MASK_NML namelist
! - only for RECT and CURV grids -
!
! * If no mask defined, INBOUND can be used to set active boundaries
!
! * IDLA : Layout indicator :
!                  1   : Read line-by-line bottom to top.  (default)
!                  2   : Like 1, single read statement.
!                  3   : Read line-by-line top to bottom.
!                  4   : Like 3, single read statement.
! * IDFM : format indicator :
!                  1   : Free format.  (default)
!                  2   : Fixed format.
!                  3   : Unformatted.
! * FORMAT : element format to read :
!               '(....)'  : auto detected  (default)
!               '(f10.6)' : float type
!
! * Example :
!      IDF  IDLA  IDFM   FORMAT    FILENAME
!      60   1     1     '(....)'  'GLOB-30M.mask'
!
! * The legend for the input map is :
!    -2 : Excluded boundary point (covered by ice)
!    -1 : Excluded sea point (covered by ice)
!     0 : Excluded land point
!     1 : Sea point
!     2 : Active boundary point
!     3 : Excluded grid point
!     7 : Ice point
!
! * namelist must be terminated with /
! * definitions & defaults:
!     MASK%FILENAME         = 'unset'  ! filename
!     MASK%IDF              = 60       ! file unit number
!     MASK%IDLA             = 1        ! layout indicator
!     MASK%IDFM             = 1        ! format indicator
!     MASK%FORMAT           = '(....)' ! formatted read format
! -------------------------------------------------------------------- !
&MASK_NML
EOF

  if [ "$mask_from" == 'NAME' ]; then
    if [ "$mask_filename" != 'unset' ]; then  echo "  MASK%FILENAME  =  '$mask_filename'" >> $nmlfile; fi
#    if [ "$mask_idf" != 60 ];           then  echo "  MASK%IDF       =  $mask_idf" >> $nmlfile; fi
    if [ "$mask_idla" != 1 ];           then  echo "  MASK%IDLA      =  $mask_idla" >> $nmlfile; fi
    if [ "$mask_idfm" != 1 ];           then  echo "  MASK%IDFM      =  $mask_idfm" >> $nmlfile; fi
    if [ "$mask_format" != '(....)' ];  then  echo "  MASK%FORMAT    =  '$mask_format'" >> $nmlfile; fi
  fi
#  if [ "$mask_from" == 'PART' ];      then  echo "  MASK%FROM      =  '$mask_from'" >> $nmlfile; fi
    fi # mask
  fi # SMC
fi # RECT or CURV


if [ "$type" == 'RECT' ] || [ "$type" == 'CURV' ]; then 
  if [ $SMC == 0 ]; then
    if [ ! -z "$(grep 'FLAGTR' $forinamelist)" ]; then
      if [ $flagtr -gt 0 ]; then

# obst namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the obstruction map via OBST_NML namelist
! - only for RECT and CURV grids -
!
! * only used if &MISC FLAGTR = 1 in param.nml
!                (transparencies at cell boundaries)
!          or if &MISC FLAGTR = 2 in param.nml
!                (transparencies at cell centers)
!          or if &MISC FLAGTR = 3 in param.nml
!                (transparencies at cell boundaries with cont. ice)
!          or if &MISC FLAGTR = 4 in param.nml
!                (transparencies at cell centers with cont. ice)
!
! * value <= value_read * scale_fac
!
! * IDLA : Layout indicator :
!                  1   : Read line-by-line bottom to top.  (default)
!                  2   : Like 1, single read statement.
!                  3   : Read line-by-line top to bottom.
!                  4   : Like 3, single read statement.
! * IDFM : format indicator :
!                  1   : Free format.  (default)
!                  2   : Fixed format.
!                  3   : Unformatted.
! * FORMAT : element format to read :
!               '(....)'  : auto detected  (default)
!               '(f10.6)' : float type
!
! * Example :
!      IDF  SF      IDLA  IDFM   FORMAT    FILENAME
!      70   0.0001  1     1     '(....)'  'GLOB-30M.obst'
!
! * If the file unit number equals 10, then the data is read from this
!   file. The data must follow the above record. No comment lines are
!   allowed within the data input.
!
! * In the case of unstructured grids, no obstruction file can be added
!
! * namelist must be terminated with /
! * definitions & defaults:
!     OBST%SF              = 1.       ! scale factor
!     OBST%FILENAME        = 'unset'  ! filename
!     OBST%IDF             = 70       ! file unit number
!     OBST%IDLA            = 1        ! layout indicator
!     OBST%IDFM            = 1        ! format indicator
!     OBST%FORMAT          = '(....)' ! formatted read format
! -------------------------------------------------------------------- !
&OBST_NML
EOF

      if [ "$obst_sf" != 1. ];            then  echo "  OBST%SF        =  $obst_sf" >> $nmlfile; fi
      if [ "$obst_filename" != 'unset' ]; then  echo "  OBST%FILENAME  =  '$obst_filename'" >> $nmlfile; fi
#      if [ "$obst_idf" != 70 ];           then  echo "  OBST%IDF       =  $obst_idf" >> $nmlfile; fi
      if [ "$obst_idla" != 1 ];           then  echo "  OBST%IDLA      =  $obst_idla" >> $nmlfile; fi
      if [ "$obst_idfm" != 1 ];           then  echo "  OBST%IDFM      =  $obst_idfm" >> $nmlfile; fi
      if [ "$obst_format" != '(....)' ];  then  echo "  OBST%FORMAT    =  '$obst_format'" >> $nmlfile; fi
#      if [ "$obst_from" != 'NAME' ];      then  echo "  OBST%FROM      =  '$obst_from'" >> $nmlfile; fi

      fi # flagtr
    fi # FLAGTR
  fi # SMC
fi # RECT or CURV

if [ "$type" == 'RECT' ] || [ "$type" == 'CURV' ]; then 
  if [ ! -z "$(grep 'REFMAP' $forinamelist)" ]; then
    if [ $(echo "$refmap == 2" | bc -l) -eq 1 ]; then

# slope namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the reflexion slope map via SLOPE_NML namelist
! - only for RECT and CURV grids -
!
! * only used if &REF1 REFMAP = 2 defined in param.nml
!
! * value <= value_read * scale_fac
!
! * IDLA : Layout indicator :
!                  1   : Read line-by-line bottom to top.  (default)
!                  2   : Like 1, single read statement.
!                  3   : Read line-by-line top to bottom.
!                  4   : Like 3, single read statement.
! * IDFM : format indicator :
!                  1   : Free format.  (default)
!                  2   : Fixed format.
!                  3   : Unformatted.
! * FORMAT : element format to read :
!               '(....)'  : auto detected  (default)
!               '(f10.6)' : float type
!
! * Example :
!      IDF  SF      IDLA  IDFM   FORMAT    FILENAME
!      80   0.0001  1     1     '(....)'  'GLOB-30M.slope'
!
! * In the case of unstructured grids, no sed file can be added
!
! * namelist must be terminated with /
! * definitions & defaults:
!     SLOPE%SF             = 1.       ! scale factor
!     SLOPE%FILENAME       = 'unset'  ! filename
!     SLOPE%IDF            = 80       ! file unit number
!     SLOPE%IDLA           = 1        ! layout indicator
!     SLOPE%IDFM           = 1        ! format indicator
!     SLOPE%FORMAT         = '(....)' ! formatted read format
! -------------------------------------------------------------------- !
&SLOPE_NML
EOF

      if [ "$slope_sf" != 1. ];            then  echo "  SLOPE%SF        =  $slope_sf" >> $nmlfile; fi
      if [ "$slope_filename" != 'unset' ]; then  echo "  SLOPE%FILENAME  =  '$slope_filename'" >> $nmlfile; fi
#      if [ "$slope_idf" != 80 ];           then  echo "  SLOPE%IDF       =  $slope_idf" >> $nmlfile; fi
      if [ "$slope_idla" != 1 ];           then  echo "  SLOPE%IDLA      =  $slope_idla" >> $nmlfile; fi
      if [ "$slope_idfm" != 1 ];           then  echo "  SLOPE%IDFM      =  $slope_idfm" >> $nmlfile; fi
      if [ "$slope_format" != '(....)' ];  then  echo "  SLOPE%FORMAT    =  '$slope_format'" >> $nmlfile; fi
#      if [ "$slope_from" != 'NAME' ];      then  echo "  SLOPE%FROM      =  '$slope_from'" >> $nmlfile; fi

    fi # refmap
  fi # REFMAP
fi # RECT or CURV

if [ ! -z "$(grep 'SEDMAPD50' $forinamelist)" ]; then
  if [ $sedmapd50 ]; then

# sed namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the sedimentary bottom map via SED_NML namelist
!
! * only used if &SBT4 SEDMAPD50 = T defined in param.nml
!
! * value <= value_read * scale_fac
!
! * IDLA : Layout indicator :
!                  1   : Read line-by-line bottom to top.  (default)
!                  2   : Like 1, single read statement.
!                  3   : Read line-by-line top to bottom.
!                  4   : Like 3, single read statement.
! * IDFM : format indicator :
!                  1   : Free format.  (default)
!                  2   : Fixed format.
!                  3   : Unformatted.
! * FORMAT : element format to read :
!               '(....)'  : auto detected  (default)
!               '(f10.6)' : float type
!
! * Example :
!      IDF SF  IDLA  IDFM   FORMAT    FILENAME
!      90  1.  1     2     '(f10.6)' 'SED.txt'
!
! * In the case of unstructured grids, no sed file can be added
!
! * namelist must be terminated with /
! * definitions & defaults:
!     SED%SF               = 1.       ! scale factor
!     SED%FILENAME         = 'unset'  ! filename
!     SED%IDF              = 90       ! file unit number
!     SED%IDLA             = 1        ! layout indicator
!     SED%IDFM             = 1        ! format indicator
!     SED%FORMAT           = '(....)' ! formatted read format
! -------------------------------------------------------------------- !
&SED_NML
EOF

      if [ "$sed_sf" != 1. ];            then  echo "  SED%SF        =  $sed_sf" >> $nmlfile; fi
      if [ "$sed_filename" != 'unset' ]; then  echo "  SED%FILENAME  =  '$sed_filename'" >> $nmlfile; fi
#      if [ "$sed_idf" != 90 ];           then  echo "  SED%IDF       =  $sed_idf" >> $nmlfile; fi
      if [ "$sed_idla" != 1 ];           then  echo "  SED%IDLA      =  $sed_idla" >> $nmlfile; fi
      if [ "$sed_idfm" != 1 ];           then  echo "  SED%IDFM      =  $sed_idfm" >> $nmlfile; fi
      if [ "$sed_format" != '(....)' ];  then  echo "  SED%FORMAT    =  '$sed_format'" >> $nmlfile; fi
#      if [ "$sed_from" != 'NAME' ];      then  echo "  SED%FROM      =  '$sed_from'" >> $nmlfile; fi

  fi # sedmap50
fi # SEDMAPD50


if [ "$mask_filename" == 'unset' ] && [ $n_inpt -gt 0 ]; then 

# inbound namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the input boundary points via INBND_COUNT_NML and
!                                      INBND_POINT_NML namelist
! - for RECT, CURV and UNST grids -
!
! * If no mask defined, INBOUND can be used
!
! * If the actual input data is not defined in the actual wave model run
!   the initial conditions will be applied as constant boundary conditions.
!
! * The number of points is defined by INBND_COUNT
!
! * The points must start from index 1 to N
!
! * Each line contains:
!     Discrete grid counters (IX,IY) of the active point and a
!     connect flag. If this flag is true, and the present and previous
!     point are on a grid line or diagonal, all intermediate points
!     are also defined as boundary points.
!
! * Included point :
!     grid points from segment data
!     Defines as lines identifying points at which
!     input boundary conditions are to be defined. 
!
! * namelist must be terminated with /
! * definitions & defaults:
!     INBND_COUNT%N_POINT     = 0        ! number of segments
!
!     INBND_POINT(I)%X_INDEX  = 0        ! x index included point
!     INBND_POINT(I)%Y_INDEX  = 0        ! y index included point
!     INBND_POINT(I)%CONNECT  = F        ! connect flag
!
! OR
!     INBND_POINT(I)          = 0 0 F    ! included point
! -------------------------------------------------------------------- !
&INBND_COUNT_NML
EOF

  if [ $n_inpt -gt 0 ]; then  echo "  INBND_COUNT%N_POINT    =  $n_inpt" >> $nmlfile; fi

cat >> $nmlfile << EOF
/

&INBND_POINT_NML
EOF

  if [ $n_inpt -gt 0 ]; then
    for i_inpt in $(seq 1 $n_inpt)
    do
      echo "  INBND_POINT($i_inpt)         =  ${inptx[$i_inpt]} ${inpty[$i_inpt]} ${inptf[$i_inpt]}" >> $nmlfile
    done
  fi

fi # PART

if [ "$mask_filename" != 'unset' ] && ( [ $n_expt -gt 0 ] || [ $n_exbd -gt 0 ] ); then 

# excluded namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the excluded points and bodies via EXCL_COUNT_NML, EXCL_POINT_NML 
!                                           and EXCL_BODY_NML namelist
! - only for RECT and CURV grids -
!
! * If no mask defined, EXCL can NOT be used
!
! * The number of points and bodies are defined by EXCL_COUNT
!
! * The points and bodies must start from index 1 to N
!
! * Each line contains:
!     Discrete grid counters (IX,IY) of the active point and a
!     connect flag. If this flag is true, and the present and previous
!     point are on a grid line or diagonal, all intermediate points
!     are also defined as boundary points.
!
! * Excluded point : 
!     grid points from segment data
!     Defined as lines identifying points at which
!     input boundary conditions are to be excluded.
!
! * Excluded body: 
!     Define a point in a closed body of sea points to remove the
!     entire body of sea points.
!
! * namelist must be terminated with /
! * definitions & defaults:
!     EXCL_COUNT%N_POINT      = 0        ! number of segments
!     EXCL_COUNT%N_BODY       = 0        ! number of bodies
!
!     EXCL_POINT(J)%X_INDEX   = 0        ! x index excluded point
!     EXCL_POINT(J)%Y_INDEX   = 0        ! y index excluded point
!     EXCL_POINT(J)%CONNECT   = F        ! connect flag
!
!     EXCL_BODY(K)%X_INDEX    = 0        ! x index excluded body
!     EXCL_BODY(K)%Y_INDEX    = 0        ! y index excluded body
! OR
!     EXCL_POINT(J)           = 0 0 F    ! excluded point
!     EXCL_BODY(K)            = 0 0      ! excluded body
! -------------------------------------------------------------------- !
&EXCL_COUNT_NML
EOF

  if [ $n_expt -gt 0 ]; then  echo "  EXCL_COUNT%N_POINT    =  $n_expt" >> $nmlfile; fi
  if [ $n_exbd -gt 0 ]; then  echo "  EXCL_COUNT%N_BODY     =  $n_exbd" >> $nmlfile; fi

cat >> $nmlfile << EOF
/

&EXCL_POINT_NML
EOF

  if [ $n_expt -gt 0 ]; then
    for i_expt in $(seq 1 $n_expt)
    do
      echo "  EXCL_POINT($i_expt)            =  ${exptx[$i_expt]} ${expty[$i_expt]} ${exptf[$i_expt]}" >> $nmlfile
    done
  fi

cat >> $nmlfile << EOF
/

&EXCL_BODY_NML
EOF

  if [ $n_exbd -gt 0 ]; then
    for i_exbd in $(seq 1 $n_exbd)
    do
      echo "  EXCL_BODY($i_exbd)             =  ${exbdx[$i_exbd]} ${exbdy[$i_exbd]}" >> $nmlfile
    done
  fi

fi # PART


if ( [ "$type" == 'RECT' ] || [ "$type" == 'CURV' ] ) && [ $n_ouln -gt 0 ]; then 

# outbound namelist
cat >> $nmlfile << EOF
/

! -------------------------------------------------------------------- !
! Define the output boundary points via OUTBND_COUNT_NML and
!                                       OUTBND_LINE_NML namelist
! - only for RECT and CURV grids -
!
! * It will creates a nest file with output boundaries for a inner grid.
!   The prefered way to do it is to use ww3_bounc program.
!
! * These do not need to be defined for data transfer between grids in
!    the multi grid driver.
!
! * The number of lines are defined by OUTBND_COUNT
!
! * The lines must start from index 1 to N
!
! * Output boundary points are defined as a number of straight lines,
!   defined by its starting point (X0,Y0), increments (DX,DY) and number
!   of points. A negative number of points starts a new output file.
!
! * Example for spherical grid in degrees :
!     '1.75  1.50  0.25 -0.10     3'
!     '2.25  1.50 -0.10  0.00    -6'
!     '0.10  0.10  0.10  0.00   -10'
!
! * namelist must be terminated with /
! * definitions & defaults:
!     OUTBND_COUNT%N_LINE   = 0               ! number of lines
!
!     OUTBND_LINE(I)%X0     = 0.              ! x index start point
!     OUTBND_LINE(I)%Y0     = 0.              ! y index start point
!     OUTBND_LINE(I)%DX     = 0.              ! x-along increment
!     OUTBND_LINE(I)%DY     = 0.              ! y-along increment
!     OUTBND_LINE(I)%NP     = 0               ! number of points
! OR
!     OUTBND_LINE(I)        = 0. 0. 0. 0. 0   ! included lines
! -------------------------------------------------------------------- !
&OUTBND_COUNT_NML
EOF

  if [ $n_ouln -gt 0 ]; then  echo "  OUTBND_COUNT%N_LINE   =  $n_ouln" >> $nmlfile; fi

cat >> $nmlfile << EOF
/

&OUTBND_LINE_NML
EOF

  if [ $n_ouln -gt 0 ]; then
    for i_ouln in $(seq 1 $n_ouln)
    do
      echo "  OUTBND_LINE($i_ouln)           =  ${oulnx0[$i_ouln]} ${oulny0[$i_ouln]} ${oulndx[$i_ouln]} ${oulndy[$i_ouln]} ${oulnnp[$i_ouln]}" >> $nmlfile
    done
  fi

fi # RECT or CURV


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


