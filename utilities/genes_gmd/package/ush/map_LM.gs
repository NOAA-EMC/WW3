*
* map_hr.gs  : GrADS script for test case test_hr
* ----------------------------------------------------------------
*           Scripts used :
*              colorset.gs : Sets up shading colors
*
* General set up

  t1 =  1
  tn = 40
  ts =  1

  diff='yes'

  plot_map = 'yes'
  plot_act = 'yes'
  plot_msk = 'yes'
  plot_hs  = 'yes' ; hs_type = -1
  plot_con = 'yes'
  plot_dir = ' es'
  plot_wnd = ' es'
  plot_bys = 'yes'
  plot_lab = 'yes'
* pan = '(a) WRT'
* pan = '(b) WW3-WRT'
* pan = '(c) WAM-WRT'
* pan = '(d) GMD1-WRT'
* pan = '(e) GMD3-WRT'
  pan = '(f) G35d-WRT'

  if ( hs_type = -1 )
    hspar = 'hs'
    thpar = 'peakd'
  endif
  if ( hs_type = 0 )
    hspar = 'phs00'
    thpar = 'pth00'
  endif
  if ( hs_type = 1 )
    hspar = 'phs01'
    thpar = 'pth01'
  endif
  if ( hs_type = 2 )
    hspar = 'phs02' 
    thpar = 'pth02'
  endif
  if ( hs_type = 3 )
    hspar = 'phs03' 
    thpar = 'pth03'
  endif
  if ( hs_type = 4 )
    hspar = 'phs04' 
    thpar = 'pth04'
  endif
  if ( hs_type = 5 )
    hspar = 'phs05' 
    thpar = 'pth05'
  endif

  xpl = 1.5
  xph = 7.5
  ypl = 1.0
  yph = 10.0

  xml =  272.00
  xmh =  275.25
  yml =   41.58
  ymh =   46.19

  vskip1 = 4
  
  wsc1 = 10.0

  if ( diff = 'yes' )
*   clevs = '-0.35 -0.30 -0.25 -0.20 -0.15 -0.10 -0.05 0.05 0.10 0.15 0.20 0.25 0.30 0.35'
*   clevs = '-0.525 -0.45 -0.375 -0.3 -0.225 -0.15 -0.075 0.075 0.15 0.225 0.3 0.375 0.45 0.525'
*   clevs = '-35 -30 -25 -20 -15 -10 -5 5 10 15 20 25 30 35'
    clevs = '-28 -24 -20 -16 -12 -8 -4 4 8 12 16 20 24 28'
*   clevs = '-21 -18 -15 -12 -9 -6 -3 3 6 9 12 15 18 21'
*   clevs = '-17.5 -15 -12.5 -10 -7.5 -5 -2.5 2.5 5 7.5 10 12.5 15 17.5'
    ccols = '41 42 43 44 45 46 47 49 50 51 52 53 54 55 56'
  else
    clevs = '0.33 0.67 1 1.33 1.67 2 2.33 2.67 3 3.33 3.67 4 4.33'
    ccols = '21 23  24  26  27  29  30  32  33  35  36 37 38 40'
  endif

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

  pdx = ( xph - xpl ) * 0.01
  strsiz2 = ( xph - xpl ) * 0.025

  'set display color white'
  'run colorset.gs'

  gdate="yyyy/mm/dd"
  '!date -u "+%Y/%m/%d" > tmp_grads_gdate'
  result = read (tmp_grads_gdate)
  gdate = sublin(result,2)
  '!rm -f tmp_grads_gdate'

* Set buoy data hardcoded

  nr = 1
  x.1 = 273.030
  y.1 =  42.696
  s.1 = '45007'

* ID output to screen

  say ' '
  say '----------------------'
  say '*** Running map.gs ***'
  say '----------------------'
  say ' ' 
  say 'Number of ouptput points : ' nr
  say 'Time steps from ' t1 ' through ' tn ' with step ' ts

* Loop over time steps

  'open grl_mich'

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
    vdate = year '/' month '/' day ' ' hour 'z'
*   vdate = pan ' ' hour ':' min 'z'
*   vdate = hour ':' min 'z'

    say '   processing time step ' t ', time is ' vdate

* Basic plot set up

    'enable print plot.grads.' t
    'clear'
    'set grads off'
    'set lon ' xml ' ' xmh
    'set lat ' yml ' ' ymh
    'set xlint 0.5'
    'set ylint 0.5'
    'set grid on 3 60'
    'set mpdraw off'

    'set parea ' xpl ' ' xph ' ' ypl ' ' yph
    'set mproj scaled'

* Plot grl_mich grid ----------------------------------------------------- *

* Plot map

    if ( plot_map = 'yes' )
      'set gxout grfill'
      'set clevs -4.5 -3.5 -2.5 -1.5 -0.5  0.5 1.5'
      'set ccols 60  ' col_msk '  67   68   63   62   0  ' col_act
      'd map'
    endif

* Plot Hs

    if ( plot_hs = 'yes' )
      'set gxout shaded'
      'set clevs ' clevs
      'set ccols ' ccols
      'd ' hspar
  
      if ( plot_con = 'yes' )
        'set gxout contour'
        'set cthick 1'
        'set ccolor 60'
        'set clevs ' clevs
        'set clab off'
        'd ' hspar
      endif

    endif

* Plot directions

    if ( plot_dir = 'yes' )
      'set gxout vector'
      'set cthick 3'
      'set arrscl 0.25'
      'set arrlab off'
      'set ccolor 1'
      'd skip(cos(' thpar '),' vskip1 ');skip(sin(' thpar '),' vskip1 ')'
    endif

* Plot wind

    if ( plot_wnd = 'yes' )
      'set gxout vector'
      'set cthick 3'
      'set arrscl 0.25 ' wsc1
      'set arrlab off'
      'set ccolor 2'
      'd skip(wu,' vskip1 ');skip(wv,' vskip1 ')'
    endif

* Plot output locations -------------------------------------------------- *

    'set strsiz ' strsiz2

    if ( plot_bys = 'yes' )
      j = 1
      'set line 1'

      while ( j <= nr )
        if ( x.j < xmh ) & (x.j > xml ) & ( y.j < ymh ) & ( y.j > yml )
          xb = xpl + ( xph - xpl ) * ( x.j - xml ) / ( xmh - xml )
          yb = ypl + ( yph - ypl ) * ( y.j - yml ) / ( ymh - yml )
          'draw recf ' xb-pdx ' ' yb-pdx ' ' xb+pdx ' ' yb+pdx
          'set string 1 l'
*         'draw string ' xb + 0.14*strsiz2 ' ' yb + 1.2*strsiz2 ' ' s.j
        endif
        j = j + 1
      endwhile
    endif
 
* Panel and date marker

    if ( plot_lab = 'yes' )
      'set strsiz ' strsiz
      'set string 0 l'
      'draw string 1.7 9.70 ' pan
      'set strsiz ' strsiz2
      'set string 0 r'
      'draw string 7.25 1.25 ' vdate
    endif
 
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

* end of map_hr.gs
