*
* map.gs  : GrADS script for mww3_case_02
* ----------------------------------------------------------------
*           Scripts used :
*              colorset.gs : Sets up shading colors
*
* General set up

  t1 =  25
  tn =  25
  ts =   1

  plot_map = 'yes'
  plot_act = 'yes'
  plot_msk = 'yes'
  plot_hs  = 'yes'
  plot_con = 'yes'
  plot_dir = 'no'
  plot_wnd = 'no'
  plot_wdr = 'no'
  plot_bys = 'no'
  plot_lab = 'yes'
  plot_box = 'no'
  pan = '(d)'

  nrgr=3
  plot.1 = 'yes'
  plot.2 = 'yes'
  plot.3 = 'yes'

  sk.1 = 4
  sk.2 = 12
  sk.3 = 40

  xpl = 1.0
  xph = 7.5
  ypl = 2.5
  yph = 9.0

* xml =  172
* xmh =  190
* yml =   48
* ymh =   58
* dgr =    2

* xml =  160
* xmh =  190
* yml =   48
* ymh =   65
* dgr =    5

* xml =  150
* xmh =  240
* yml =   30
* ymh =   78
* dgr =   15

  xml =  120
  xmh =  295
  yml =  -78
  ymh =   78
  dgr =   15

* just the grids ....

* xml =  120
* xmh =  290
* yml =  -78
* ymh =   60
* dgr =   15
* xpl = 1.0
* xph = 7.5
* ypl = 3.5
* yph = 9.0

* xml =  160
* xmh =  238
* yml =   48
* ymh =   78
* dgr =   10
* xpl = 1.0
* xph = 7.5
* ypl = 3.5
* yph = 8.0

  xml =  162
  xmh =  235
  yml =   50
  ymh =   75
  dgr =   10
  xpl = 1.0
  xph = 7.5
  ypl = 3.5
  yph = 8.0

* Aleutian focus

* xml =  160
* xmh =  200
* yml =   48
* ymh =   60
* dgr =    5
* xml =  170
* xmh =  190
* yml =   49
* ymh =   55
* dgr =    2
* xpl = 1.0
* xph = 7.5
* ypl = 4.5
* yph = 8.0

  clevs = '1 2 3 4 5 6 7 8 9 10 11 12 13 14'
  wlevs = '2 4 6 8 12 14 16 18 20 22 24 26 28'
* wlevs = '3 6 9 12 15 18 21 24 27 30 33 36 39 42'
  ccols = '21  22  23  25  26  27  29  30  31  32  33  35  36  37  38'

  if ( plot_act = 'yes' )
    col_act = 69
  else
    col_act = 70
  endif

  if ( plot_msk = 'yes' )
    col_msk = 71
  else
    col_msk = 70
  endif

  pdx = ( xph - xpl ) * 0.01
  strsiz = ( xph - xpl ) * 0.03

  'set display color white'
  'run colorset.gs'

  gdate="yyyy/mm/dd"
  '!date -u "+%Y/%m/%d" > tmp_grads_gdate'
  result = read (tmp_grads_gdate)
  gdate = sublin(result,2)
  '!rm -f tmp_grads_gdate'

* Set buoy data from buoy.data

* '!sed -e "s/\./ /g"  -e "s/\_/ /g" buoy.data > tmp_grads_buoy'

  nr = 1
  x.1 = 9.5
  y.1 = 64.75
  s.1 = ' '

* Set box data

  xbl.1 =  119.5
  xbh.1 =  295.5
  ybl.1 =  -78.5
  ybh.1 =   60.5

  xbl.2 =  159.75
  xbh.2 =  238.25
  ybl.2 =   47.875
  ybh.2 =   78.125

  xbl.3 =  162.
  xbh.3 =  235.
  ybl.3 =   50.
  ybh.3 =   75.

* ID output to screen

  say ' '
  say '----------------------'
  say '*** Running map.gs ***'
  say '----------------------'
  say ' ' 
  say 'Time steps from ' t1 ' through ' tn ' with step ' ts

* Loop over time steps

  i = 1
  while ( i <= nrgr )
*   i = i + 1
    'open grd' i
    i = i + 1
  endwhile

  i = 1
  t = t1

  '!rm -f plot.grads.*'

  while ( t <= tn )
    'set t ' t

    'query time'
    gradsdate = subwrd(result,3)
    test = substr ( gradsdate, 3, 1 )
    if ( test='Z' )
      year = substr ( gradsdate, 9, 4 )
      mnth = substr ( gradsdate, 6, 3 )
      day  = substr ( gradsdate, 4, 2 )
      hour = substr ( gradsdate, 1, 2 )
      min  = '00'
    else
      year = substr ( gradsdate, 12, 4 )
        mnth = substr ( gradsdate, 9, 3 )
      day  = substr ( gradsdate, 7, 2 )
      hour = substr ( gradsdate, 1, 2 )
      min  = substr ( gradsdate, 4, 2 )
    endif

    month= '??'
    if (mnth='JAN'); month= '01'; endif;
    if (mnth='FEB'); month= '02'; endif;
    if (mnth='MAR'); month= '03'; endif;
    if (mnth='APR'); month= '04'; endif;
    if (mnth='MAY'); month= '05'; endif;
    if (mnth='JUN'); month= '06'; endif;
    if (mnth='JUL'); month= '07'; endif;
    if (mnth='AUG'); month= '08'; endif;
    if (mnth='SEP'); month= '09'; endif;
    if (mnth='OCT'); month= '10'; endif;
    if (mnth='NOV'); month= '11'; endif;
    if (mnth='DEC'); month= '12'; endif;

    vdate = year '/' month '/' day ' ' hour ':' min 'z'
*   vdate = pan ' ' hour ':' min 'z'
*   vdate = hour ':' min 'z'

    say '   processing time step ' t ', time is ' vdate

* Basic plot set up

    'enable print plot.grads.' t
    'clear'
    'set grads off'
    'set lon ' xml ' ' xmh
    'set lat ' yml ' ' ymh
    'set xlint ' dgr
    'set ylint ' dgr
    'set grid on 3 60'
*   'set mpdset hires'
    'set mpdraw off'

    'set parea ' xpl ' ' xph ' ' ypl ' ' yph
    'set mproj scaled'

* Plot map background

    if ( plot_map = 'yes' )
      'set line 70'
      'draw recf ' xpl ' ' ypl ' ' xph ' ' yph
    endif

    i = 1
    while ( i <= nrgr )
      if (  plot.i = 'yes' )
        say '      processing grid ' i

* Plot map background

        if ( plot_map = 'yes' )
          'set gxout grfill'
          'set clevs -3.5 -2.5 -1.5 -0.5  0.5 1.5'
          'set ccols ' col_msk '  67   68   63   62   0  ' col_act
          'd map.' i
        endif

* Plot Hs

        if ( plot_hs = 'yes' )
          'set gxout shaded'
          'set clevs ' clevs
          'set ccols ' ccols
          'd hs.' i
  
          if ( plot_con = 'yes' )
            'set gxout contour'
            'set cthick 1'
            'set ccolor 60'
            'set clevs ' clevs
            'set clab off'
            'd hs.' i
          endif
        endif

* Plot directions

        if ( plot_dir = 'yes' )
          'set gxout vector'
          'set cthick 3'
          'set arrscl 0.25'
          'set arrlab off'
          'set ccolor 1'
          'd skip(cos(dp.' i ' ),' sk.i ');skip(sin(dp.' i ' ),' sk.i ')'
        endif

* Plot wind

        if ( plot_wnd = 'yes' )
          'set gxout shaded'
          'set clevs ' wlevs
          'set ccols ' ccols
          'd mag(uwnd.' i ',vwnd.' i ')'

          if ( plot_con = 'yes' )
            'set gxout contour'
            'set cthick 1'
            'set ccolor 60'
            'set clevs ' wlevs
            'set clab off'
            'd mag(uwnd.' i ',vwnd.' i ')'
          endif
        endif

* Plot wind direction

        if ( plot_wdr = 'yes' )
          'set gxout vector'
          'set cthick 3'
          'set arrscl 0.25'
          'set arrlab off'
          'set ccolor 1'
          'd skip(uwnd.' i ',' sk.i ');skip(vwnd.' i ',' sk.i ')'
        endif

* Plot output locations

        'set strsiz ' 1.0 * pdx

        if ( plot_bys = 'yes' )
          j = 1
          while ( j <= nr )
            xb = xpl + ( xph - xpl ) * ( x.j - xml ) / ( xmh - xml )
            yb = ypl + ( yph - ypl ) * ( y.j - yml ) / ( ymh - yml )
            'set line 1'
            'draw recf ' xb-pdx ' ' yb-pdx ' ' xb+pdx ' ' yb+pdx
**          'set string 1 c'
**          'draw string ' xb + 3.0*pdx ' ' yb + 3.0*pdx ' ' s.j
            j = j + 1
          endwhile
        endif

      endif
      i = i + 1
    endwhile
    
*   'run cbarn'

* Plot boundary box

    i = 1
    while ( i <= nrgr )
      if (  plot.i = 'yes' )

        if ( plot_box = 'yes' )
          xbl = xpl + ( xph - xpl ) * ( xbl.i - xml ) / ( xmh - xml )
          ybl = ypl + ( yph - ypl ) * ( ybl.i - yml ) / ( ymh - yml )
          xbh = xpl + ( xph - xpl ) * ( xbh.i - xml ) / ( xmh - xml )
          ybh = ypl + ( yph - ypl ) * ( ybh.i - yml ) / ( ymh - yml )
          'set line 2 1 10'
          'draw rec ' xbl ' ' ybl ' ' xbh ' ' ybh
        endif

      endif
      i = i + 1
    endwhile

* Panel and date marker

    if ( plot_lab = 'yes' )
      'set string 1 l'
      'set strsiz ' strsiz
      'draw string 1.15 7.75 ' pan
*     'draw string 1.25 8.85 ' vdate
*     'draw string 1.25 9.15 ' vdate
    endif

* 'run cbarn'
 
* Finalize

    'print'
    'disable print'
    pull OK
    t = t + ts
    i = i + 1
  endwhile

* End of loop over time steps

  say ' '
  say '----------------------'
  say '*** End of map.gs  ***'
  say '----------------------'
  say ' ' 

  'quit'

* end of map.gs
