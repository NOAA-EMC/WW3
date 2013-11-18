*
* map_m02.gs  : GrADS script for test mww3_test_02
* ----------------------------------------------------------------
* Scripts used :
*    colorset.gs : Sets up shading colors
*
* Notes:
*    Set up for outer and fine grids, whan running with other
*    grid sets, script will need to be edited.
*
* General set up

  t1 =  1
  tn = 49
  ts =  1

  plot_map = 'yes'
  plot_act = 'yes'
  plot_msk = 'yes'
  plot_hs  = 'yes'
  plot_con = 'yes'
  plot_dir = 'no '
  plot_bys = 'yes'
  plot_lab = 'no '
  plot_ID  = 'yes'
  plot_box = 'yes'
  pan = '(-)'

  plot_1 = 'yes'
  plot_2 = 'yes'

  xpl = 1.0
  xph = 7.5
  ypl = 2.5
  yph = 9.0

  xml =  -25
  xmh = 1025
  yml =  -25
  ymh = 1025

* xml =  400
* xmh = 1000
* yml =  400
* ymh = 1000

  clevs = '0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1'
  ccols = '21 23  24  26  27  29  30  32  33  35  36  38'

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
  x.1 = 750.
  y.1 = 750.
  s.1 = ' '

* ID output to screen

  say ' '
  say '----------------------'
  say '*** Running map.gs ***'
  say '----------------------'
  say ' ' 
  say 'Number of ouptput points : ' nr
  say 'Time steps from ' t1 ' through ' tn ' with step ' ts

* Loop over time steps

  'open outer'
  'open fine'

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

*   vdate = year '/' month '/' day ' ' hour ':' min 'z'
*   vdate = pan ' ' hour ':' min 'z'
    vdate = hour ':' min 'z'

    say '   processing time step ' t ', time is ' vdate

* Basic plot set up

    'set grads off'
    'set lon ' xml / 10 ' ' xmh / 10
    'set lat ' yml / 10 ' ' ymh / 10
    'set xlint 10'
    'set ylint 10'
    'set xlab %.0f0'
    'set ylab %.0f0'
    'set grid on 3 60'
    'set mpdraw off'

    'set parea ' xpl ' ' xph ' ' ypl ' ' yph
    'set mproj scaled'

* Plot map

    if ( plot_map = 'yes' )
      'set line 70'
      'draw recf 1. 2.5 7.5 9.'
      'draw recf ' xpl ' ' ypl ' ' xph ' ' yph

      if ( plot_1 = 'yes' )
        'set gxout grfill'
        'set clevs -3.5 -2.5 -1.5 -0.5  0.5 1.5'
        'set ccols ' col_msk '  67   68   63   62   0  ' col_act
        'd map.1'
      endif

      if ( plot_2 = 'yes' )
        'set gxout grfill'
        'set clevs -3.5 -2.5 -1.5 -0.5  0.5 1.5'
        'set ccols ' col_msk '  67   68   63   62   0  ' col_act
        'd map.2'
      endif

    endif

* Plot Hs

    if ( plot_hs = 'yes' ) & ( plot_1 = 'yes' )
      'set gxout shaded'
      'set clevs ' clevs
      'set ccols ' ccols
      'd hs.1 / 10'
      if ( plot_ID = 'yes' )
        'run cbarn'
      endif
  
      if ( plot_con = 'yes' )
        'set gxout contour'
        'set cthick 1'
        'set ccolor 60'
        'set clevs ' clevs
        'set clab off'
        'd hs.1 / 10'
      endif
    endif

    if ( plot_hs = 'yes' ) & ( plot_2 = 'yes' ) & ( t > 1 )
*   if ( plot_hs = 'yes' ) & ( plot_2 = 'yes' )
      xbl = xpl + ( xph - xpl ) * ( 600. - xml ) / ( xmh - xml )
      ybl = ypl + ( yph - ypl ) * ( 600. - yml ) / ( ymh - yml )
      xbh = xpl + ( xph - xpl ) * ( 900. - xml ) / ( xmh - xml )
      ybh = ypl + ( yph - ypl ) * ( 900. - yml ) / ( ymh - yml )
      'set line 21'
      'draw recf ' xbl ' ' ybl ' ' xbh ' ' ybh

      'set gxout shaded'
      'set clevs ' clevs
      'set ccols ' ccols
      'd hs.2 / 10'
  
      if ( plot_con = 'yes' )
        'set gxout contour'
        'set cthick 1'
        'set ccolor 60'
        'set clevs ' clevs
        'set clab off'
        'd hs.2 / 10'
      endif
    endif

* Plot directions

    if ( plot_dir = 'yes' ) & ( plot_1 = 'yes' )
      'set gxout vector'
      'set cthick 3'
      'set arrscl 0.25'
      'set arrlab off'
      'set ccolor 1'
      'd skip(cos(dp.1),2);skip(sin(dp.1),2)'
    endif

    if ( plot_dir = 'yes' ) & ( plot_2 = 'yes' )
      'set gxout vector'
      'set cthick 3'
      'set arrscl 0.25'
      'set arrlab off'
      'set ccolor 5'
      'd skip(cos(dp.1),2);skip(sin(dp.2),2)'
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
*       'set string 1 c'
*       'draw string ' xb + 3.0*pdx ' ' yb + 3.0*pdx ' ' s.j
        j = j + 1
      endwhile
    endif

* Plot boundary box

    if ( plot_box = 'yes' )
      xbl = xpl + ( xph - xpl ) * ( 600. - xml ) / ( xmh - xml )
      ybl = ypl + ( yph - ypl ) * ( 600. - yml ) / ( ymh - yml )
      xbh = xpl + ( xph - xpl ) * ( 900. - xml ) / ( xmh - xml )
      ybh = ypl + ( yph - ypl ) * ( 900. - yml ) / ( ymh - yml )
      'set line 2 1 10'
      'draw rec ' xbl ' ' ybl ' ' xbh ' ' ybh
    endif
 
* Panel and date marker

    if ( plot_lab = 'yes' )
      if ( plot_hs = 'yes' ) & ( plot_1 = 'yes' )
        'set string 0 l'
      else
        'set string 1 l'
      endif
      'set strsiz ' strsiz
*     'draw string 1.25 8.55 ' pan
      'draw string 1.25 8.55 ' vdate
    endif

* Text around plot

    if ( plot_ID = 'yes' )
      'set strsiz 0.18'
      'set string 1 c 5'
      'draw string 4.5 9.5 WAVEWATCH III TEST'

      'set strsiz 0.12'
      'set string 1 l'
      'draw string  1.0 9.2 test mww3_test_02'
      'set string 1 r'
      'draw string 7.5 9.2 valid ' vdate
      'set string 1 c'
      'draw string 4.5 2.1 wave height (shaded, m), X,Y in km '
      'draw string 4.5 1.6 plot generated ' gdate
    endif
 
* Finalize

    'printim map_m02_'t'.png'
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
