
thisdir=`pwd`
ftndir='../ftn'
srcdir='../src'

mkdir $srcdir

cd $ftndir 
ftnfiles=`ls *.ftn`
ftnfiles=$(sed -e "s/.ftn//g" <<< $ftnfiles)
nonftnfiles=`ls -I "*.ftn"`

cd $thisdir

set -x
for file in $ftnfiles
do
  echo "convert $file.ftn to a .F90"
  gawk -f $thisdir/switch2cpp.awk < $ftndir/${file}.ftn > $srcdir/${file}.F90
done

for file in $nonftnfiles 
do 
  echo "copy $file to src" 
  cp $ftndir/${file} $srcdir/
done 

DIRLIST="SCRIP PDLIB" 

for DIR in $DIRLIST
do 

  cd $ftndir/$DIR
  ftnfiles=`ls *.ftn`
  ftnfiles=$(sed -e "s/.ftn//g" <<< $ftnfiles)
  nonftnfiles=`ls -I "*.ftn"`

  cd $thisdir
  if [ ! -d $srcdir/$DIR ] 
  then 
     mkdir -p $srcdir/$DIR
  fi  

  set -x
  for file in $ftnfiles
  do
    echo "convert $DIR/$file.ftn to a .F90"
    echo "$ftndir/$DIR/${file}.ftn"
    gawk -f $thisdir/switch2cpp.awk < $ftndir/$DIR/${file}.ftn > $srcdir/$DIR/${file}.F90
  done

  for file in $nonftnfiles
  do
    echo "copy $DIR/$file to src" 
    cp $ftndir/$DIR/${file} $srcdir/$DIR/
  done

done


