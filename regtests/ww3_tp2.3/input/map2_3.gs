*
* map2_3.gs    : GrADS wave heigt and mean direction for ww3_tp2.3
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


  '!../input/grads.sh'

  result = read (run_ID)
  OK = sublin(result,1)
  if ( OK = '0' )
    run_ID = sublin(result,2)
  else
    run_ID = 'run settings unknown'
  endif

  result = read (grd_ID)
  OK = sublin(result,1)
  if ( OK = '0' )
    grd_ID = sublin(result,2)
  else
    grd_ID = 'grid resolution unknown'
  endif

* Get the data, loop through times ...

  'open ww3'

t = 1
while ( t <= 6 )

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
  'set lat -5 30'
  'set lon -5 40'
  'set xlab %.0f00'
  'set ylab %.0f00'
  'set xlint 5'
  'set ylint 5'
  'set grid on 3 60'
  'set mpdraw off'

  'set parea 1.0 7.5 3.0 8.0'
  'set mproj scaled'
  
* Plot wave heights

  'set gxout shaded'
  'set clevs  0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.25 1.5 1.75 2. 2.25 2.5 2.75'
  'set ccols 21 22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38'
  'd hs'
  'run cbarn'

  'set gxout contour'
  'set clevs  0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.25 1.5 1.75 2. 2.25 2.5 2.75'
  'set clab off'
  'd hs'

* Text around plot

  'set strsiz 0.18'
  'set string 1 c 5'
  'draw string 4.5 8.5 WAVEWATCH III TEST'

  'set strsiz 0.12'
  'set string 1 l'
  'draw string  1.0 8.2 Propagation test ww3_tp2.3'
  'set string 1 r'
  'draw string 7.5 8.2 valid ' vdate
  'set string 1 c'
  'draw string 4.5 2.6 wave height (shaded, m), X,Y in km '
  'draw string 4.5 2.35' run_ID
  'draw string 4.5 2.1' grd_ID
  'draw string 4.5 0.9 plot generated ' gdate

*  Print this page and clear

  'printim map2_3_'t'.png'
  'clear'
  'set grads off'

  t = t + 1
endwhile

'quit'

* end of map2_2.gs
