
thisdir=`pwd`
ftndir='../ftn'
srcdir='../src'

mkdir $srcdir

DIRLIST=". SCRIP PDLIB" 

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
    mv $ftndir/$DIR/${file}.ftn $ftndir/$DIR/${file}.tmp
    #process the file multiple times in case there is a double switch (and a thrid time for good measure) 
    gawk -f $thisdir/switch2cpp.awk < $ftndir/$DIR/${file}.tmp > $ftndir/$DIR/${file}.tmp1
    gawk -f $thisdir/switch2cpp.awk < $ftndir/$DIR/${file}.tmp1 > $ftndir/$DIR/${file}.tmp2
    gawk -f $thisdir/switch2cpp.awk < $ftndir/$DIR/${file}.tmp2 > $ftndir/$DIR/${file}.ftn
    rm $ftndir/$DIR/${file}.tmp $ftndir/$DIR/${file}.tmp1 $ftndir/$DIR/${file}.tmp2
    cp $ftndir/$DIR/${file}.ftn $srcdir/$DIR/${file}.F90
  done

  for file in $nonftnfiles
  do
    echo "copy $DIR/$file to src" 
    cp $ftndir/$DIR/${file} $srcdir/$DIR/
  done

done


