*
* map_m05.gs  : GrADS script for test mww3_test_05
* ----------------------------------------------------------------
* Scripts used :
*    colorset.gs : Sets up shading colors
*
* General set up

  t1 =  7
  tn = 25
  ts =  6

  plot_map = 'yes'
  plot_act = 'no '
  plot_msk = 'yes'
  plot_hs  = 'yes' ; hs_type = -1
  plot_con = 'yes'
  plot_dir = 'no '
  plot_wnd = 'no '
  plot_bys = 'no '
  plot_ID  = 'yes'
  plot_lab = 'no '
  pan = '(c)'

  if ( hs_type = -1 )
    hspar = 'hs'
    dirpar = 'dp'
  endif
  if ( hs_type = 0 )
    hspar = 'phs00'
    dirpar = 'pdir00'
  endif
  if ( hs_type = 1 )
    hspar = 'phs01'
    dirpar = 'pdir01'
  endif
  if ( hs_type = 2 )
    hspar = 'phs02' 
    dirpar = 'pdir02'
  endif
  if ( hs_type = 3 )
    hspar = 'phs03' 
    dirpar = 'pdir03'
  endif
  if ( hs_type = 4 )
    hspar = 'phs04' 
    dirpar = 'pdir04'
  endif
  if ( hs_type = 5 )
    hspar = 'phs05' 
    dirpar = 'pdir05'
  endif

  plot_1 = 'yes'
  plot_2 = 'yes'
  plot_3 = 'yes'

  xpl = 1.0
  xph = 7.5
  ypl = 2.5
  yph = 9.0

* xml = -250
* xmh =  250
* yml = -250
* ymh =  250

  xml = -500
  xmh =  500
  yml = -500
  ymh =  500

* xml = -750
* xmh =  750
* yml = -750
* ymh =  750

* xml = -1000
* xmh =  1000
* yml = -1000
* ymh =  1000

* xml = -1350
* xmh =  1350
* yml = -1350
* ymh =  1350

  vskip1 = 1
  vskip2 = 4
  vskip3 = 8
  
  wsc1 = 10.0
  wsc2 = 17.5
  wsc3 = 35.0

  clevs = '0.5 1. 2. 3. 4. 5. 6. 8. 10. 12. 14.'
* clevs = '1. 2. 3. 4. 5. 6. 7. 8. 9. 10. 11.'
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

  strsiz = ( xph - xpl ) * 0.03

  pdx = ( xph - xpl ) * 0.005
  strsiz2 = ( xph - xpl ) * 0.02

  'set display color white'
  'run colorset.gs'

  gdate="yyyy/mm/dd"
  '!date -u "+%Y/%m/%d" > tmp_grads_gdate'
  result = read (tmp_grads_gdate)
  gdate = sublin(result,2)
  '!rm -f tmp_grads_gdate'

* Set buoy data from buoy.data

  '!cp ../input/buoy.all buoy.all'
  '!cat buoy.all | wc -l > tmp_grads_nbuoys'
  result = read (tmp_grads_nbuoys)
  line   = sublin(result,2)
  nr     = subwrd(line,1)
  '!rm -f tmp_grads_nbuoys'

  i = 1
  while ( i <= nr )
    result = read (buoy.all)
    line   = sublin(result,2)
    x.i = subwrd(line,1)
    y.i = subwrd(line,2)
    s.i = subwrd(line,3)
    i = i + 1
  endwhile

* ID output to screen

  say ' '
  say '----------------------'
  say '*** Running map.gs ***'
  say '----------------------'
  say ' ' 
  say 'Number of ouptput points : ' nr
  say 'Time steps from ' t1 ' through ' tn ' with step ' ts

* Loop over time steps

  'open grd1'
  'open grd2'
  'open grd3'

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

    'clear'
    'set grads off'
    'set lon ' xml / 100 ' ' xmh / 100
    'set lat ' yml / 100 ' ' ymh / 100
    'set xlint 2.'
    'set ylint 2.'
*   'set xlint 3.'
*   'set ylint 3.'
    'set xlab %.0f00'
    'set ylab %.0f00'
    'set grid on 3 60'
    'set mpdraw off'

    'set parea ' xpl ' ' xph ' ' ypl ' ' yph
    'set mproj scaled'

    'set line 70'
    'draw recf 1. 2.5 7.5 9.'
    'draw recf ' xpl ' ' ypl ' ' xph ' ' yph

* Plot outer grid -------------------------------------------------------- *

    if ( plot_1 = 'yes' )

* Plot map

      if ( plot_map = 'yes' )
        'set gxout grfill'
        'set clevs -4.5 -3.5 -2.5 -1.5 -0.5  0.5 1.5'
        'set ccols 60  ' col_msk '  67   68   63   62   0  ' col_act
        'd map.1'
      endif

* Plot Hs

      if ( plot_hs = 'yes' )
        'set gxout shaded'
        'set clevs ' clevs
        'set ccols ' ccols
        'd ' hspar '.1'
        if ( plot_ID = 'yes' )
          'run cbarn'
        endif
  
        if ( plot_con = 'yes' )
          'set gxout contour'
          'set cthick 1'
          'set ccolor 60'
          'set clevs ' clevs
          'set clab off'
          'd ' hspar '.1'
        endif

      endif

* Plot directions

      if ( plot_dir = 'yes' )
        'set gxout vector'
        'set cthick 3'
        'set arrscl 0.25'
        'set arrlab off'
        'set ccolor 1'
        'd skip(cos(' dirpar '.1),' vskip1 ');skip(sin(' dirpar '.1),' vskip1 ')'
      endif

* Plot wind

      if ( plot_wnd = 'yes' )
        'set gxout vector'
        'set cthick 3'
        'set arrscl 0.25 ' wsc1
        'set arrlab off'
        'set ccolor 2'
        'd skip(wu.1,' vskip1 ');skip(wv.1,' vskip1 ')'
      endif

    endif

* Plot middle grid ------------------------------------------------------- *

    if ( plot_2 = 'yes' )

* Plot map

      if ( plot_map = 'yes' )
        'set gxout grfill'
        'set clevs -4.5 -3.5 -2.5 -1.5 -0.5  0.5 1.5'
        'set ccols 60  ' col_msk '  67   68   63   62   0  ' col_act
        'd map.2'
      endif

* Plot Hs

      if ( plot_hs = 'yes' )
        'set gxout shaded'
        'set clevs ' clevs
        'set ccols ' ccols
        'd ' hspar '.2'
        if ( plot_ID = 'yes' )
          'run cbarn'
        endif
  
        if ( plot_con = 'yes' )
          'set gxout contour'
          'set cthick 1'
          'set ccolor 60'
          'set clevs ' clevs
          'set clab off'
          'd ' hspar '.2'
        endif

      endif

* Plot directions

      if ( plot_dir = 'yes' )
        'set gxout vector'
        'set cthick 3'
        'set arrscl 0.25'
        'set arrlab off'
        'set ccolor 1'
        'd skip(cos(' dirpar '.2),' vskip2 ');skip(sin(' dirpar '.2),' vskip2 ')'
      endif

* Plot wind

      if ( plot_wnd = 'yes' )
        'set gxout vector'
        'set cthick 3'
        'set arrscl 0.25 ' wsc2
        'set arrlab off'
        'set ccolor 2'
        'd skip(wu.2,' vskip2 ');skip(wv.2,' vskip2 ')'
      endif

    endif

* Plot inner grid -------------------------------------------------------- *

    if ( plot_3 = 'yes' )

* Plot map

      if ( plot_map = 'yes' )
        'set gxout grfill'
        'set clevs -4.5 -3.5 -2.5 -1.5 -0.5  0.5 1.5'
        'set ccols 60  ' col_msk '  67   68   63   62   0  ' col_act
        'd map.3'
      endif

* Plot Hs

      if ( plot_hs = 'yes' )
        'set gxout shaded'
        'set clevs ' clevs
        'set ccols ' ccols
        'd ' hspar '.3'
        if ( plot_ID = 'yes' )
          'run cbarn'
        endif
  
        if ( plot_con = 'yes' )
          'set gxout contour'
          'set cthick 1'
          'set ccolor 60'
          'set clevs ' clevs
          'set clab off'
          'd ' hspar '.3'
        endif

      endif

* Plot directions

      if ( plot_dir = 'yes' )
        'set gxout vector'
        'set cthick 3'
        'set arrscl 0.125'
        'set arrlab off'
        'set ccolor 1'
        'd skip(cos(' dirpar '.3),' vskip3 ');skip(sin(' dirpar '.3),' vskip3 ')'
      endif
      
* Plot wind

      if ( plot_wnd = 'yes' )
        'set gxout vector'
        'set cthick 3'
        'set arrscl 0.25 ' wsc3
        'set arrlab off'
        'set ccolor 2'
        'd skip(uwnd.3,' vskip3 ');skip(vwnd.3,' vskip3 ')'
      endif

    endif

* Plot output locations -------------------------------------------------- *

    'set strsiz ' strsiz2

    if ( plot_bys = 'yes' )
      j = 1
      'set line 1'
      'set string 1 l'

      while ( j <= nr )
        if ( x.j < xmh ) & (x.j > xml ) & ( y.j < ymh ) & ( y.j > yml )
          xb = xpl + ( xph - xpl ) * ( x.j - xml ) / ( xmh - xml )
          yb = ypl + ( yph - ypl ) * ( y.j - yml ) / ( ymh - yml )
          'draw recf ' xb-pdx ' ' yb-pdx ' ' xb+pdx ' ' yb+pdx
*         'draw string ' xb + 0.05*strsiz2 ' ' yb + 0.8*strsiz2 ' ' s.j
        endif
        j = j + 1
      endwhile
    endif
 
* Panel and date marker

    if ( plot_lab = 'yes' )
      'set string 0 c'
      'set strsiz ' strsiz
*     'draw string 1.7 8.30 ' pan
      'draw string 2.3 8.55 ' vdate
    endif

* Text around plot

    if ( plot_ID = 'yes' )
      'set strsiz 0.18'
      'set string 1 c 5'
      'draw string 4.5 9.5 WAVEWATCH III TEST'

      'set strsiz 0.12'
      'set string 1 l'
      'draw string  1.0 9.2 test mww3_test_05'
      'set string 1 r'
      'draw string 7.5 9.2 valid ' vdate
      'set string 1 c'
      'draw string 4.5 2.1 wave height (shaded, m), X,Y in km '
      'draw string 4.5 1.6 plot generated ' gdate
    endif

* Finalize

    'printim map_m05_'t'.png'
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
