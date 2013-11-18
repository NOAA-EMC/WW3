*
* map_m01.gs  : GrADS script for test case nww3_test_01
* ----------------------------------------------------------------
*
* General set up

  t1 =  1
  tn = 49
  ts =  4

  plot_map = 'yes'
  plot_act = 'yes'
  plot_hs  = 'yes'
  plot_dir = 'yes'

  xpl = 1.0
  xph = 7.5
  ypl = 2.5
  yph = 9.0

  xml = -10
  xmh = 210
  yml = -10
  ymh = 210

  mdx =   2.
  pdx = ( xph - xpl ) / ( xmh - xml ) * mdx

  'set display color white'
  'run colorset.gs'

  gdate="yyyy/mm/dd"
  '!date -u "+%Y/%m/%d" > tmp_grads_gdate'
  result = read (tmp_grads_gdate)
  gdate = sublin(result,2)
  '!rm -f tmp_grads_gdate'

* Get buoy data from buoy.data

  '!cat buoy.data | wc -l > tmp_grads_nbuoys'
  result = read (tmp_grads_nbuoys)
  line   = sublin(result,2)
  nr     = subwrd(line,1)
  nr = nr - 1
  '!rm -f tmp_grads_nbuoys'

  '!sed -e "s/\./ /g"  -e "s/\_/ /g" buoy.data > tmp_grads_buoy'

  i = 1
  while ( i <= nr )
    result = read (tmp_grads_buoy)
    line   = sublin(result,2)
    x.i = subwrd(line,1)
    y.i = subwrd(line,3)
    s.i = subwrd(line,6)
    i = i + 1
  endwhile
  '!rm -f tmp_grads_buoy'

* ID output to screen

  say ' '
  say '----------------------'
  say '*** Running map.gs ***'
  say '----------------------'
  say ' ' 
  say 'Number of ouptput points : ' nr
  say 'Time steps from ' t1 ' through ' tn ' with step ' ts

* Loop over time steps

  'open ww3'

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

    if ( i = 1 ) ; pan = '(a)' ; endif
    if ( i = 2 ) ; pan = '(b)' ; endif
    if ( i = 3 ) ; pan = '(c)' ; endif
    if ( i = 4 ) ; pan = '(d)' ; endif
    if ( i = 5 ) ; pan = '(e)' ; endif
    if ( i = 6 ) ; pan = '(f)' ; endif
    if ( i = 7 ) ; pan = '(g)' ; endif
    if ( i = 8 ) ; pan = '(h)' ; endif
    if ( i > 8 ) ; pan = '(-)' ; endif

*   vdate = year '/' month '/' day ' ' hour ':' min 'z'
*   vdate = pan ' ' hour ':' min 'z'
    vdate = hour ':' min 'z'

    say '   processing time step ' t ', time is ' vdate

* Basic plot set up

    'clear'
    'set grads off'
    'set lon ' xml / 10 ' ' xmh / 10
    'set lat ' yml / 10 ' ' ymh / 10
    'set xlint 5'
    'set ylint 5'
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
      'set gxout grfill'
      'set clevs -3.5 -2.5 -1.5 -0.5  0.5 1.5'
      if ( plot_act = 'yes' )
        'set ccols 60  67   68   63   62   0  69'
      else
        'set ccols 60  67   68   63   62   0  70'
      endif
      'd map'
    endif

* Plot Hs

    if ( plot_hs = 'yes' )
      'set gxout shaded'
      'set clevs  0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1'
      'set ccols 21 23  24  26  27  29  30  32  33  35  36  38'
      'd hs'
      'run cbarn'
  
      'set gxout contour'
      'set cthick 1'
      'set ccolor 60'
      'set clevs  0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1'
      'set clab off'
      'd hs'
    endif

* Plot directions

    if ( plot_dir = 'yes' )
      'set gxout vector'
      'set cthick 3'
      'set arrscl 0.25'
      'set arrlab off'
      'set ccolor 1'
      'd skip(cos(dp),1);skip(sin(dp),1)'
    endif

* Plot output locations

    'set strsiz ' 3.0 * pdx

    if ( plot_bys = 'yes' )
      j = 1
      while ( j <= nr )
        xb = xpl + ( xph - xpl ) * ( x.j - xml ) / ( xmh - xml )
        yb = ypl + ( yph - ypl ) * ( y.j - yml ) / ( ymh - yml )
        'set line 1'
        'draw recf ' xb-pdx ' ' yb-pdx ' ' xb+pdx ' ' yb+pdx
        'set string 1 c'
        'draw string ' xb + 3.0*pdx ' ' yb + 3.0*pdx ' ' s.j
        j = j + 1
      endwhile
    endif
 
* Panel and date marker

*   'set string 1 r'
*   'set strsiz ' 2.0 * pdx
*   'draw string 7.1 8.85 ' vdate

* Text around plot

    'set strsiz 0.18'
    'set string 1 c 5'
    'draw string 4.5 9.5 WAVEWATCH III TEST'

    'set strsiz 0.12'
    'set string 1 l'
    'draw string  1.0 9.2 test mww3_test_01'
    'set string 1 r'
    'draw string 7.5 9.2 valid ' vdate
    'set string 1 c'
    'draw string 4.5 2.1 wave height (shaded, m), X,Y in km '
    'draw string 4.5 1.6 plot generated ' gdate
 
* Finalize

    'printim map_m01_'t'.png'
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
