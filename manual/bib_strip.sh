#!/bin/sh

  cp $1.bbl tempfile
  sed '1a \
\\itemsep 0mm \
' tempfile > $1.bbl
  rm -f tempfile
