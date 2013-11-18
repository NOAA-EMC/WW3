*
* map_m03.gs  : GrADS script for test mww3_test_03
* ----------------------------------------------------------------
* Scripts used :
*    colorset.gs : Sets up shading colors
* Remarks:
*    Setup for grdset_d, with sox grids. 
*    Other grdset will require edit of file.
*
* General set up

  t1 =  1
  tn = 7
  ts =  1

  plot_mpl = 'no'
  plot_mph = 'yes'
  plot_act = 'yes'
  plot_msk = 'yes'
  plot_hs  = 'yes'
  plot_con = 'yes'
  plot_dir = 'no'
  plot_dpt = 'yes'
  plot_box = 'yes'
  plot_bys = 'yes'

  plot_ID  = 'yes'
  plot_lab = 'no'
  lab_col = 0
* lab_col = 1
* lab_col = 3
  pan = '(a)'

* Flags for low1-3 and hgh1-3

  grd.1 = 'yes'
  grd.2 = 'yes'
  grd.3 = 'yes'
  grd.4 = 'yes'
  grd.5 = 'yes'
  grd.6 = 'yes'

  xbl.1 =   0.0
  xbh.1 = 500.0
  ybl.1 =   0.0
  ybh.1 = 270.0

  xbl.2 =   0.0
  xbh.2 = 290.0
  ybl.2 = 210.0
  ybh.2 = 500.0

  xbl.3 = 240.0
  xbh.3 = 500.0
  ybl.3 = 190.0
  ybh.3 = 500.0

  xbl.4 =   0.0
  xbh.4 = 190.0
  ybl.4 = 215.0
  ybh.4 = 500.0

  xbl.5 =  35.0
  xbh.5 = 380.0
  ybl.5 = 290.0
  ybh.5 = 500.0

  xbl.6 = 285.0
  xbh.6 = 500.0
  ybl.6 = 355.0
  ybh.6 = 500.0

  xpl = 1.0
  xph = 7.5
  ypl = 2.5
  yph = 9.0

  xml =  -15
  xmh =  515
  yml =  -15
  ymh =  515

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

  nr = 1
  x.1 = 200.
  y.1 = 400.
  s.1 = ' '

* ID output to screen

  say ' '
  say '----------------------'
  say '*** Running map.gs ***'
  say '----------------------'
  say ' ' 
  say 'Time steps from ' t1 ' through ' tn ' with step ' ts

* Open files

  'open low1'
  'open low2'
  'open low3'
  'open hgh1'
  'open hgh2'
  'open hgh3'

* Loop over time steps

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
*   vdate = pan ' ' hour ':' min
    vdate = hour ':' min 'z'

    say '   processing time step ' t ', time is ' vdate

* Basic plot set up

    'clear'
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

    if ( plot_mpl = 'yes' | plot_mph = 'yes' ) 
      'set line 70'
      'draw recf ' xpl ' ' ypl ' ' xph ' ' yph
    endif

* loop over grids

    j = 1
    while ( j <= 6 )
      if ( grd.j = 'yes' )
        say '      processing grid ' j ', flag = ' grd.j

* Plot map

        if ( plot_mpl = 'yes' & j <= 3 )
          'set gxout grfill'
          'set clevs -3.5 -2.5 -1.5 -0.5  0.5 1.5'
          'set ccols ' col_msk '  67   68   63   62   0  ' col_act
          'd map.' j
        endif

        if ( plot_mph = 'yes' & j >= 4 )
          'set gxout grfill'
          'set clevs -3.5 -2.5 -1.5 -0.5  0.5 1.5'
          'set ccols ' col_msk '  67   68   63   62   0  ' col_act
          'd map.' j
        endif

* Plot Hs

        if ( plot_hs = 'yes' )
          'set gxout shaded'
          'set clevs ' clevs
          'set ccols ' ccols
          'd hs.' j ' / 10'
          if ( plot_ID = 'yes' )
            'run cbarn'
          endif
        endif

        if ( plot_con = 'yes' )
          'set gxout contour'
          'set cthick 1'
          'set ccolor 60'
          'set clevs ' clevs
          'set clab off'
          'd hs.' j ' / 10'
        endif

* Plot directions

        if ( plot_dir = 'yes' )
          'set gxout vector'
          'set cthick 3'
          'set arrscl 0.25'
          'set arrlab off'
          'set ccolor 1'
          if ( j <= 3 )
            k = 3
          else
            k = 6
          endif
          'd skip(cos(dp.' j '),' k ');skip(sin(dp.' j '),' k ')'
        endif

        if ( plot_dpt = 'yes' & j >=4 )
          'set gxout contour'
          'set cthick 1'
          'set ccolor 60'
          'set cint 50'
          'set clab off'
          'd dpt.' j
        endif

      endif
      j = j + 1
    endwhile

* Plot boundary boxes

    j = 1
    while ( j <= 6 )
      if ( grd.j = 'yes'  & plot_box = 'yes' )
        xblp = xpl + ( xph - xpl ) * ( xbl.j - xml ) / ( xmh - xml )
        yblp = ypl + ( yph - ypl ) * ( ybl.j - yml ) / ( ymh - yml )
        xbhp = xpl + ( xph - xpl ) * ( xbh.j - xml ) / ( xmh - xml )
        ybhp = ypl + ( yph - ypl ) * ( ybh.j - yml ) / ( ymh - yml )
        if ( j <= 3 )
          'set line 2 ' j ' 10'
        else
          'set line 3 ' j - 3 ' 10'
        endif
        'draw rec ' xblp ' ' yblp ' ' xbhp ' ' ybhp
      endif
      j = j + 1
    endwhile

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
 
* Panel and date marker

    if ( plot_lab = 'yes' )
      'set string ' lab_col ' l'
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
      'draw string  1.0 9.2 test mww3_test_03'
      'set string 1 r'
      'draw string 7.5 9.2 valid ' vdate
      'set string 1 c'
      'draw string 4.5 2.1 wave height (shaded, m), X,Y in km '
      'draw string 4.5 1.6 plot generated ' gdate
    endif
 
* Finalize

    'printim map_m03_'t'.png'
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
