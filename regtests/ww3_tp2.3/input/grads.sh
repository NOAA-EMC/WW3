#!/bin/sh

  grep 'PRO' ww3_grid.out > run_ID

  if [ ! -s run_ID ]
  then
    echo ' '
    echo '        *** Set !/O0 (O-zero) switch to put run ID on plot *** '
    echo ' '
    rm -f run_ID
  fi

  res=`grep 'Increments' ww3_grid.out | awk '{ print $4 }'`
  unt=`grep 'Increments' ww3_grid.out | awk '{ print $2 }'`
  xfr=`grep 'Increment factor'  ww3_grid.out | awk '{ print $4 }'`
  dth=`grep 'Directional increment'  ww3_grid.out | awk '{ print $4 }'`

  echo "Grid resolution $res $unt, spectral resolution XFR = $xfr, $dth degr." > grd_ID

