*
* map_glo.gs  : GrADS script for mww3_case_03
* ----------------------------------------------------------------
*           Scripts used :
*              colorset.gs : Sets up shading colors
*
* General set up

  t1 =  82
  tn =  82
  ts =   6

  plot_map = 'yes'
  plot_act = 'yes'
  plot_hs  = 'yes'
  plot_con = 'yes'
  plot_dir = 'no'
  plot_wnd = 'no'
  plot_wdr = 'no'
  plot_bys = 'no'
  plot_lab = 'yes'
  pan = '(-)'

  nrgr=8
  grid.1 = 'glo_30m'
* grid.1 = 'glo_30S'
* grid.2 = 'glo_30C'
* grid.3 = 'glo_30N'
* grid.2 = 'us_10m'
  grid.2 = 'ak_10m'
  grid.3 = 'at_10m'
  grid.4 = 'wc_10m'
  grid.5 = 'ep_10m'
  grid.6 = 'ak_4m'
  grid.7 = 'at_4m'
  grid.8 = 'wc_4m'

  plot.1 = 'yes' ; mask.1 = 'yes'
  plot.2 = 'yes' ; mask.2 = 'yes'
  plot.3 = 'yes' ; mask.3 = 'yes'
  plot.4 = 'yes' ; mask.4 = 'yes'
  plot.5 = 'yes' ; mask.5 = 'yes'
  plot.6 = 'yes' ; mask.6 = 'yes'
  plot.7 = 'yes' ; mask.7 = 'yes'
  plot.8 = 'yes' ; mask.8 = 'yes'

  sk.1 = 12
  sk.2 = 12
  sk.3 = 40

  xpl =  1.0
  xph = 10.0
  ypl =  1.5
  yph =  7.0

  xml =   25
  xmh =  385
  yml =  -78
  ymh =   78
  dgr =   30

  xml =  130
  xmh =  310
  yml =  -20
  ymh =   75
  dgr =   15

* xml =  200
* xmh =  250
* yml =   30
* ymh =   60
* dgr =    5

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
  strsiz = ( xph - xpl ) * 0.015

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
    'open ' grid.i
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
*   'set mpdraw on'
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

* Plot map background

      if (  mask.i = 'yes' )
        say '      processing grid ' i '  mask'

        'set gxout grfill'
        'set clevs -3.5 -2.5 -1.5 -0.5  0.5 1.5'
        if ( plot.i = 'yes' )
          'set ccols ' col_msk '  67   68   63   62   0  ' col_act
        else
          'set ccols ' col_msk '   0    0    0   62   0  ' col_act
        endif
        'd map.' i

      endif

* Plot Hs

      if (  plot.i = 'yes' )
        say '      processing grid ' i '  data'

        if ( plot_hs = 'yes' )
          'set gxout shaded'
          'set clevs ' clevs
          'set ccols ' ccols
          'd hs.' i

*         'run cbarn'
  
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

*         'run cbarn'

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
    
* Panel and date marker

    if ( plot_lab = 'yes' )
      'set strsiz ' strsiz
      'set string 1 l'
      'draw string 1. 7.15 multi-grid WAVEWATCH III'
      'set string 1 r'
      'draw string 10. 7.15 ' vdate
    endif
 
* Finalize

    'print'
    'disable print'
*   'printim plot.' t '.gif gif'
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
