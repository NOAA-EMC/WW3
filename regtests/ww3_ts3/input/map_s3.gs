*
* map_s2.gs  : GrADS winds, wave heigt and peak direction ww3_ts3
* ----------------------------------------------------------------
*              Data set with multiple times expected.
*              Input from ww3.ctl ww3.grads
*              Scripts used :
*                 colorset.gs : Sets up shading colors
*
* General set up  plot wind or waves

   wind = 'yes'

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
*
  'q file 1'
  line = sublin(result,5)
  ntime = subwrd(line,12)

t = 1
while ( t <= ntime )

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

  'set lat -50 50'
  'set lon -50 50'
  'set xlab %.0f0'
  'set ylab %.0f0'
  'set xlint 10'
  'set ylint 10'
  'set grid on 3 60'

* 'set lat -5 5'
* 'set lon -5 5'
* 'set xlint 1'
* 'set ylint 1'
* 'set grid on 3 60'

  'set mpdraw off'

  'set parea 1.0 7.5 2.5 9.0'
  'set mproj scaled'

if ( wind = 'yes' )

  'set gxout shaded'
  'set clevs 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34'
  'set ccols 21 22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38'
  'd sqrt ( uwnd*uwnd + vwnd*vwnd )'
  'run cbarn'

  'set gxout vector'
  'set cthick 3'
  'set arrscl 0.25'
  'set arrlab off'
  'set ccolor 1'
  'd skip(uwnd,2);skip(vwnd,2)'

  'set strsiz 0.18'
  'set string 1 c 5'
  'draw string 4.5 9.5 WAVEWATCH III TEST'

  'set strsiz 0.12'
  'set string 1 l'
  'draw string  1.0 9.2 Moving grid test ww3_ts3'
  'set string 1 r'
  'draw string 7.5 9.2 valid ' vdate
  'set string 1 c'
* 'draw string 4.5 2.1 wind speed (shaded, m/s), X,Y in km '
  'draw string 4.5 2.1 wind speed (shaded, m/s), X,Y in degree '
  'draw string 4.5 1.8 plot generated ' gdate

else

* Plot wave heights

  'set gxout shaded'
  'set clevs 0.5 1. 1.5 2 2.5 3 3.5 4 5 6 7 8 9 10 11 12 13'
  'set ccols 21 22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38'
  'd hs'
  'run cbarn'

  'set gxout contour'
  'set clevs 0.5 1. 1.5 2 2.5 3 3.5 4 5 6 7 8 9 10 11 12 13'
  'set clab off'
  'd hs'

* Text around plot

  'set strsiz 0.18'
  'set string 1 c 5'
  'draw string 4.5 9.5 WAVEWATCH III TEST'

  'set strsiz 0.12'
  'set string 1 l'
  'draw string  1.0 9.2 Moving grid test ww3_ts3'
  'set string 1 r'
  'draw string 7.5 9.2 valid ' vdate
  'set string 1 c'
* 'draw string 4.5 2.1 wave height (shaded, m), X,Y in km '
  'draw string 4.5 2.1 wave height (shaded, m/s), X,Y in degree '
  'draw string 4.5 1.8 plot generated ' gdate

endif

*  Print this page and clear

  'printim map_s3_'t'.png'
  'clear'
  'set grads off'

  t = t + 1
endwhile

'quit'

* end of map2_2.gs
