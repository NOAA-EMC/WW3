*
* profile.gs : Visualize multi-scale wave model profiler info.
* ----------------------------------------------------------------
*              version 2: color of bw option added
*
* This GrADS scripts reads and displays information from the 
* profile output from WAVEWATCH III as generated with the !/MPRF
* switch. ALl data is read from the files prf.NNN.mww3
*
* General set up - - - - - - - - - - - - - - - - - - - - - - - - -
*
  'set display color white'
  'clear'
*
* Set up page  - - - - - - - - - - - - - - - - - - - - - - - - - -
*
  header = ' es'
  legend = 'yes'
*
  headf = 1 / 60
  subhf = 1 / 80
  axisf = 1 / 80
  legnf = 1 / 100
  bhmaxf = 3.0 * axisf
*
* Set up colors  - - - - - - - - - - - - - - - - - - - - - - - - -
*
*   20: WMINIT
*   21: WMFINL
*   22: WMWAVE ST00
*   23: WMWAVE ST01
*   24: WMWAVE ST02
*   25: WMWAVE ST04
*   26: WMWAVE ST05
*   27: WMWAVE ST06
*   28: WMWAVE ST07 UPTS
*   29: WMWAVE BCST
*   31+ WMWAVE ST03 --> 30 + IMOD
*
  'set rgb 20 105 105 105'
  'set rgb 21 155 155 155'
  'set rgb 22   0   0  55'
  'set rgb 23   0   0 105'
  'set rgb 24   0   0 155'
  'set rgb 25   0   0 255'
  'set rgb 26  55  55 255'
  'set rgb 27 105 105 255'
  'set rgb 28 155 155 255'
  'set rgb 29 205 205 205'
*
  'set rgb 31 255   0 255'
  'set rgb 32   0 105   0'
  'set rgb 33   0 190   0'
  'set rgb 34   0 215   0'
  'set rgb 35   0 255   0'
  'set rgb 36 205   0   0'
  'set rgb 37 105   0   0'
  'set rgb 38 255   0   0'
  'set rgb 39 255   0   0'
  'set rgb 40 255   0   0'
  'set rgb 41 255   0   0'
  'set rgb 42 255   0   0'
  'set rgb 43 255   0   0'
  'set rgb 44 255   0   0'
  'set rgb 45 255   0   0'
  'set rgb 46 255   0   0'
  'set rgb 47 255   0   0'
  'set rgb 48 255   0   0'
  'set rgb 49 255   0   0'
  'set rgb 50 255   0   0'
*
* Get size of page - - - - - - - - - - - - - - - - - - - - - - - -
*
  'q gxinfo'
  line = sublin(result,2)
  xmax = subwrd(line,4)
  ymax = subwrd(line,6)
*
* Current date and time  - - - - - - - - - - - - - - - - - - - - -
*
  gdate="yyyy/mm/dd"
  '!date -u "+%Y/%m/%d" > tmp_grads_gdate'
  result = read (tmp_grads_gdate)
  gdate = sublin(result,2)
  '!rm -f tmp_grads_gdate'
*
* Print initial info to screen - - - - - - - - - - - - - - - - - -
*
  say ' '
  say '--------------------------'
  say '*** Running profile.gs ***'
  say '--------------------------'
  say ' '
*
* Number of profile files  - - - - - - - - - - - - - - - - - - - -
*
  '!rm -f tmp_grads_data tmp_grads_err'
  '!ls prf.*.mww3 2> tmp_grads_err | wc -l > tmp_grads_data'

  result = read (tmp_grads_err)
  line = sublin(result,2)
  error = subwrd(line,1)

  if ( error = 'ls:' )
    say ' '
    say ' *** connot find profile data files ***'
    say ' '
    'quit'
  endif

  result = read (tmp_grads_data)
  line = sublin(result,2)
  nproc = subwrd(line,1)
  say 'Number of proceccors : ' nproc
*
  i_min = 1
  i_max = nproc
*
  '!rm -f tmp_grads_data tmp_grads_err'
*
* Min and max times  - - - - - - - - - - - - - - - - - - - - - - -
*

  '!ls -l prf.*.mww3 | wc -l > tmp_grads_wc'
  result = read (tmp_grads_wc)
  line = sublin(result,2)
  files = subwrd(line,1)

  if ( files = 1 )
    i1 = 1
    i2 = 2
  else
    i1 = 2
    i2 = 3
  endif

  '!grep WMINIT prf.*.mww3 | sort -un -k ' i1 ' | head -1> tmp_grads_tmin'
  result = read (tmp_grads_tmin)
  line = sublin(result,2)
  tmin = subwrd(line,i1)

  '!grep WMFINL prf.*.mww3 | sort -un -k ' i2 ' | tail -1> tmp_grads_tmax'
  result = read (tmp_grads_tmax)
  line = sublin(result,2)
  tmax = subwrd(line,i2)

  say 'Time range           : ' tmin ' - ' tmax

  '!rm -f tmp_grads_wc tmp_grads_tmin tmp_grads_tmax'
*
* Min and max times to plot  - - - - - - - - - - - - - - - - - - -
*
  tdmin = tmin
  tdmax = tmax
*
* tdmin = 18.000
* tdmax = 48.000
*
* Set up page  - - - - - - - - - - - - - - - - - - - - - - - - - -
*
  xpmin = 0.75
  xpmax = xmax - 0.5
*
  head  = headf * ( xpmax - xpmin )
  subh  = subhf * ( xpmax - xpmin )
  axis  = axisf * ( xpmax - xpmin )
  legn  = legnf * ( xpmax - xpmin )
  bhmax = bhmaxf * ( xpmax - xpmin )
*
  ypmin = 1.5
  ypmax = ymax - 1.0
  dy = ( ypmax - ypmin ) / ( i_max - i_min + 1 )
  if ( dy > bhmax )
    dy = bhmax
  endif
  ypmin = ypmax - ( i_max - i_min + 1 ) * dy
  xfac = ( xpmax - xpmin ) / ( tdmax - tdmin )
*
  'draw rec ' xpmin ' ' ypmin ' ' xpmax ' ' ypmax
*
  'set font 1'
*
* header
*
  if ( header = 'yes' )
    'set string 1 c 3'
    'set strsiz ' head
    xtc = 0.5 * ( xpmin + xpmax )
    ytc = ypmax + 2.5 * head
    'draw string ' xtc ' ' ytc ' MWW3 profiling info'
    'set strsiz ' subh
    ytc = ypmax + 1. * head
    'draw string ' xtc ' ' ytc ' times from ' tdmin ' to ' tdmax
  endif
*
* axes
*
  'set string 1 r 3'
  'set strsiz ' axis
  'set font 0'
  xa = xpmin - 0.5 * axis
  ya = ypmax - 0.5 * dy
  ylast = ya
  i = i_min
  'draw string ' xa ' ' ya ' ' i
  i = i_min + 1
  while ( i <= i_max )
    ya = ya - dy
    if ( ylast - ya > 2.5 * axis )
      'draw string ' xa ' ' ya ' ' i
      ylast = ya
    endif
  i = i + 1
  endwhile
*
  'set string 1 c 3 90'
  'set strsiz ' axis
  'set font 1'
  xtc = xpmin - 5.5 * axis
  ytc = 0.5 * ( ypmin + ypmax )
  'draw string ' xtc ' ' ytc ' processor'
  'set string 1 c 3 0'
*
  dtt = ( tdmax - tdmin ) / 8
  dttt = dtt
  if ( dttt <=  1. ) 
    i = 0
    while ( dttt <= 1. )
      dttt = dttt * 10.
      i = i + 1
    endwhile
    '!echo ' dttt ' | sed "s/\./ /g" > tmp_grads_dt'
    result = read (tmp_grads_dt)
    line = sublin(result,2)
    dt = subwrd(line,1)
    while ( i > 0 )
      dt = dt / 10.
      i = i - 1
    endwhile
  else
    i = 0
    while ( dttt >= 1. )
      dttt = dttt / 10.
      i = i + 1
    endwhile
    dttt = dttt * 10.
    i = i - 1
    '!echo ' dttt ' | sed "s/\./ /g" > tmp_grads_dt'
    result = read (tmp_grads_dt)
    line = sublin(result,2)
    dt = subwrd(line,1)
    while ( i > 0 )
      dt = dt * 10.
      i = i - 1
    endwhile
  endif
  '!rm -f tmp_grads_dt'
*
  'set string 1 c 3'
  'set strsiz ' axis
  'set font 1'
  xtc = 0.5 * ( xpmin + xpmax )
  ytc = ypmin - 3.5 * axis
  'draw string ' xtc ' ' ytc ' time (s)'
*
  nr_lev = ( tdmax - tdmin ) / dt
  if ( nr_lev > 12 )
    dt = dt * 2
  endif
  if ( nr_lev < 6 )
    dt = dt / 2
  endif
*
  'set string 1 c 3'
  'set line 1 1 5'
  xa = xpmin - 0.5 * axis
  ya = ypmin - 1.5 * axis
  yl = ypmin - 0.25 * axis
  'set font 0'
  t = 0
  while ( t < tdmax )
    if ( t >= tdmin )
      xa = xpmin + xfac * ( t - tdmin )
      'draw line ' xa ' ' ypmin ' ' xa ' ' yl
      'draw string ' xa ' ' ya ' ' t
    endif
    t = t + dt
  endwhile
*
  say ' '
*
* Loop over data files - - - - - - - - - - - - - - - - - - - - - -
*
  '!ls prf.*.mww3 > tmp_grads_files'
  i = i_min
  mmx = 0
  while ( i <= i_max )
    result = read (tmp_grads_files)
    file = subwrd(result,2)
*
    yh = ypmax - (i-1) * dy
    yl = ypmax - i * dy
*
* Loop over data in file - - - - - - - - - - - - - - - - - - - - -
*
    OK = 0
    j  = 0
    jj = 1
*
    while ( OK = 0 )
*
      result = read (file)
      line = sublin(result,1)
      OK = subwrd(result,1)
      if ( OK = 0 )
        line = sublin(result,2)
        t1 = subwrd(line,1)
        t2 = subwrd(line,2)
        ID = subwrd(line,4)
        if ( ID = 'WMWAVE' )
          ID2 = subwrd(line,5)
          if ( ID2 = 'TIME' )
            ttime.jj.i = t1
            jj = jj + 1
          endif
          if ( ID2 = 'ST03' )
            imod = subwrd(line,6)
            if ( imod > mmx )
              mmx = imod
            endif
          endif
        endif
*
* Check time range, convert to plot
*
        if ( t1 < tdmax & t2 > tdmin )
*
          if ( t1 < tdmin )
            tl = tdmin
          else
            tl = t1
          endif
          if ( t2 > tdmax )
            tr = tdmax
          else
            tr = t2
          endif
*
          xl = xpmin + xfac * ( tl - tdmin )
          xr = xpmin + xfac * ( tr - tdmin )
*
* Case WMINIT
*
          if ( ID = 'WMINIT' )
            'set line 20'
            'draw recf ' xl ' ' yl ' ' xr ' ' yh
          endif
*
* Case WMWAVE
*
          if ( ID = 'WMWAVE' )
            if ( t1 != t2 )
              lcol = -1
              if ( ID2 = 'ST00' )
                lcol = 22
              endif
              if ( ID2 = 'ST01' )
                lcol = 23
              endif
              if ( ID2 = 'ST02' )
                lcol = 24
              endif
              if ( ID2 = 'ST03' )
                lcol = 30 + imod
              endif
              if ( ID2 = 'ST04' )
                lcol = 25
              endif
              if ( ID2 = 'ST05' )
                lcol = 26
              endif
              if ( ID2 = 'ST06' )
                lcol = 27
              endif
              if ( ID2 = 'ST07' )
                lcol = 28
              endif
              if ( ID2 = 'UPTS' )
                lcol = 28
              endif
              if ( ID2 = 'BCST' )
                lcol = 29
              endif
              if ( lcol > 0 )
                'set line ' lcol
                'draw recf ' xl ' ' yl ' ' xr ' ' yh
              endif
            endif
          endif
*
* Case WMFINL
*
          if ( ID = 'WMFINL' )
            'set line 21'
            if ( t1 != t2 )
              'draw recf ' xl ' ' yl ' ' xr ' ' yh
            else
              'draw line ' xl ' ' yl ' ' xr ' ' yh
            endif
          endif
*
        endif
*
        j = j + 1
      else
        say 'file ' file '    : ' j ' data read'
      endif
*
    endwhile
*
    i = i + 1
  endwhile
*
  '!rm -f tmp_grads_tmin tmp_grads_files'
*
* time step lines
*
  'set line 1 1 5'
  j = 1
  xl = -1.
  while ( j < jj )
    i = i_min
    while ( i <= i_max )
      xlast = xl
      xl = xpmin + xfac * ( ttime.j.i - tdmin )
      yh = ypmax - (i-1) * dy
      yl = ypmax - i * dy
      if ( xl > xpmin & xl < xpmax )
        'draw line ' xl ' ' yl ' ' xl ' ' yh
      endif
      if ( i > 1 )
        if ( xlast < xl )
          xhl = xlast
          xhr = xl
        else
          xhl = xl
          xhr = xlast
        endif
        if ( xhl < xpmax & xhr > xpmin )
          if ( xhl < xpmin)
            xhl = xpmin
          endif
          if ( xhr > xpmax)
            xhr = xpmax
          endif
          'draw line ' xhl ' ' yh ' ' xhr ' ' yh
        endif
      endif
      i = i + 1
    endwhile
    j = j + 1
  endwhile
*
  'set line 1 1 10'
  'draw rec ' xpmin ' ' ypmin ' ' xpmax ' ' ypmax
*
* legend
*
  if ( legend = 'yes' )
*
    boxs = 10 + mmx
    bscale = boxs / 20
    if ( bscale > 0.9 )
      bscale = 0.9
    endif
    bsep = ( xpmax - xpmin ) / boxs * bscale
    bwdt = 0.5 * bsep
*   if ( bwdt > 0.75 * dy )
*     bwdt = 0.75 * dy
*     bsep = 2 * bwdt
*   endif
*
    xl = 0.5 * ( xpmax + xpmin ) - ( boxs / 2 * bsep ) + ( 0.5 * bsep )
    yt = ypmin - 5.5 * axis
    yb = yt - 1.0 * legn - 0.5 * bwdt
*
    'set string 1 c 3'
    'set strsiz ' legn
    'set font 1'
    i = 1
    while ( i <= boxs )
      if ( i = 1 )
        bID = 'ini'
      endif
      if ( i = 2 )
        bID = 'fnl'
      endif
      if ( i = 3 )
        bID = 'inp'
      endif
      if ( i = 4 )
        bID = 'low'
      endif
      if ( i = 5 )
        bID = 'tme'
      endif
      if ( i = 6 )
        bID = 'eql'
      endif
      if ( i = 7 )
        bID = 'hgh'
      endif
      if ( i = 8 )
        bID = 'das'
      endif
      if ( i = 9 )
        bID = 'out'
      endif
      if ( i = 10 )
        bID = 'bct'
      endif
      if ( i > 10 )
        bID = 'm' i-10
      endif
      'draw string ' xl ' ' yt ' ' bID
*
      if ( i <= 10 )
        icol = 19 + i
      else
        icol = 20 + i
      endif
      'set line ' icol
      ' draw recf ' xl-0.5*bwdt ' ' yb-0.5*bwdt ' ' xl+0.5*bwdt ' ' yb+0.5*bwdt
      'set line 1 1 3'
      ' draw rec ' xl-0.5*bwdt ' ' yb-0.5*bwdt ' ' xl+0.5*bwdt ' ' yb+0.5*bwdt
*
      i = i + 1
      xl = xl + bsep
    endwhile
*
  endif
*
* End of operations  - - - - - - - - - - - - - - - - - - - - - - -
*
  'printim ww3_profile.png'
*
  say ' '
  say '-------------------------'
  say '*** End of profile.gs ***'
  say '-------------------------'
  say ' '
*
  'quit'
*
* End of profile.gs  - - - - - - - - - - - - - - - - - - - - - - -
