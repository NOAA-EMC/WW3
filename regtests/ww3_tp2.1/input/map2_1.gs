*
* map2_1.gs    : GrADS wave heigt and peak direction for ww3_tp2.1
* ----------------------------------------------------------------
*              Data set with multiple times expected.
*              Input from ww3.ctl ww3.grads
*              Scripts used :
*                 colorset.gs : Sets up shading colors
*
* General set up

  'set display color white'
  'clear'
  'run colorset.gs'

  gdate="yyyy/mm/dd"
  '!date -u "+%Y/%m/%d" > tmp_grads_gdate'
  result = read (tmp_grads_gdate)
  gdate = sublin(result,2)
  '!rm -f tmp_grads_gdate'

* Get the data, loop through times ...

  'open ww3'

t = 1
while ( t <= 10 )

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

* Basic plot set up

  'set grads off'
  'set lon -5 35'
  'set lat -5 35'
  'set xlint 5'
  'set ylint 5'
  'set xlab %.0f0'
  'set ylab %.0f0'
  'set grid on 3 60'
  ' set mpdraw off'

  'set parea 1. 7.5 2.5 9.'
  'set mproj scaled'

* Land sea and ice map

  'set gxout grfill'
  'set clevs -0.5  0.5'
  'set ccols 63  62  0'
  'd map'
  
* Plot wave heights

  'set gxout shaded'
  'set clevs  0.1 0.2 0.3 0.5 0.7 0.9 1.1 1.3 1.5 1.7 1.9 2.1 2.2 2.3 2.4 2.5 2.6'
  'set ccols 21 22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38'
  'd hs'
  'run cbarn'

  'set gxout contour'
  'set cthick 1'
  'set clevs  0.1 0.2 0.3 0.5 0.7 0.9 1.1 1.3 1.5 1.7 1.9 2.1 2.2 2.3 2.4 2.5 2.6'
  'set ccolor 60'
  'set clab off'
  'd hs'
  'set grid off'

* Plot peak directions

  'set gxout vector'
  'set cthick 3'
  'set arrscl 0.25'
  'set arrlab off'
  'set ccolor 1'
  'd skip(cos(dp),2);skip(sin(dp),2)'

* Text around plot

  'set strsiz 0.18'
  'set string 1 c 5'
  'draw string 4.25 9.6 WAVEWATCH III TEST'

  'set strsiz 0.12'
  'set string 1 l'
  'draw string 1.0 9.2 Propagation test ww3_tp2.1'
  'set string 1 r'
  'draw string 7.5 9.2 valid ' vdate
  'set string 1 c'
  'draw string 4.25 2.1 wave height (shaded, m) and peak direction (vector, not scaled)'
  'draw string 4.25 1.8 X and Y in km, plot generated ' gdate

*  Print this page and clear

  'printim map2_1_'t'.png'
  'clear'
  'set grads off'

  t = t + 1
endwhile

'quit'

* end of map2_1.gs
