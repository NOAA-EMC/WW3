*
* map_res.gs  : GrADS script for mww3_case_03
* ----------------------------------------------------------------
*           Scripts used :
*              colorset.gs : Sets up shading colors
*
* General set up

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
# grid.7 = 'ml_4m'
  grid.7 = 'at_4m'
  grid.8 = 'wc_4m'

  plot.1 = 'yes' ; res.1 = 1
  plot.2 = 'yes' ; res.2 = 2
  plot.3 = 'yes' ; res.3 = 3
  plot.4 = 'yes' ; res.4 = 3
  plot.5 = 'yes' ; res.5 = 3
  plot.6 = 'yes' ; res.6 = 4
  plot.7 = 'yes' ; res.7 = 5
  plot.8 = 'yes' ; res.8 = 5

  nrres = 5
  col.1 = 50 ; ID.1 = '30x30'
  col.2 = 22 ; ID.2 = '15x10'
  col.3 = 24 ; ID.3 = '10x10'
  col.4 = 35 ; ID.4 = '8x4'
  col.5 = 33 ; ID.5 = '4x4'

  xpl =  1.0
  xph = 10.0
  ypl =  1.5
  yph =  7.0

* xml =   25
* xmh =  385
* yml =  -78
* ymh =   78
* dgr =   30

  xml =  130
  xmh =  310
  yml =  -20
  ymh =   75
  dgr =   15

* clevs = '1 2 3 4 5 6 7 8 9 10 11 12 13 14'
* wlevs = '2 4 6 8 12 14 16 18 20 22 24 26 28'
* wlevs = '3 6 9 12 15 18 21 24 27 30 33 36 39 42'
* ccols = '21  22  23  25  26  27  29  30  31  32  33  35  36  37  38'

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
  say '--------------------------'
  say '*** Running map_res.gs ***'
  say '--------------------------'
  say ' ' 

* Loop over time steps

  i = 1

  while ( i <= nrgr )
    'open ' grid.i
    i = i + 1
  endwhile

  'set t 1'

  '!rm -f plot.grads.*'

* Basic plot set up

  'enable print plot.grads'
  'clear'
  'set grads off'
  'set lon ' xml ' ' xmh
  'set lat ' yml ' ' ymh
  'set xlint ' dgr
  'set ylint ' dgr
  'set grid on 3 60'
* 'set mpdset hires'
* 'set mpdraw on'
  'set mpdraw off'

  'set parea ' xpl ' ' xph ' ' ypl ' ' yph
  'set mproj scaled'

* Plot map background

  'set line 70'
  'draw recf ' xpl ' ' ypl ' ' xph ' ' yph

  i = 1
  while ( i <= nrgr )
    if (  plot.i = 'yes' )
      j = res.i
      rcolor = col.j
      rID = ID.j
      say '      processing grid ' i '  color ' rcolor '  resolution ' rID

* Plot map background

    'set gxout grfill'
    'set clevs -0.5 0.5'
    'set ccols ' rcolor ' 62 ' rcolor
    'd map.' i

    endif
    i = i + 1
  endwhile
    
* Legend

  boxs = nrres
  bsep = ( xph - xpl ) / boxs / 2
  bwdt = 0.25 * bsep
  legn = ( xph + xpl ) / 100
  axis = ( xph + xpl ) / 80

  xl = 0.5 * ( xph + xpl ) - ( boxs / 2 * bsep ) + ( 0.5 * bsep )
  xl = 0.5 * ( xph + xpl ) - ( boxs / 2 * bsep ) + ( 0.5 * bsep )
  yt = ypl - 3.5 * axis
  yb = yt - 1.0 * legn - 0.5 * bwdt

*

  'set string 1 c 3'
  'set strsiz ' legn
  'set font 1'
  i = 1

  while ( i <= boxs )

    'draw string ' xl ' ' yt ' ' ID.i
    'set line ' col.i
    'draw recf ' xl-0.5*bwdt ' ' yb-0.5*bwdt ' ' xl+0.5*bwdt ' ' yb+0.5*bwdt
    'set line 1 1 3'
    'draw rec ' xl-0.5*bwdt ' ' yb-0.5*bwdt ' ' xl+0.5*bwdt ' ' yb+0.5*bwdt


    i = i + 1
    xl = xl + bsep
  endwhile
 
* Finalize

    'print'
    'disable print'
*   'printim res_map.png -t 0'
*   'printim res_map.gif gif'
    pull OK

* End of loop over time steps

  say ' '
  say '--------------------------'
  say '*** End of map_res.gs  ***'
  say '--------------------------'
  say ' ' 

  'quit'

* end of map_res.gs
