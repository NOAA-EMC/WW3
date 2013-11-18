*
* source.gs  : GrADS plots of wave spectrum and sources
* ----------------------------------------------------------------
*              version 2: grid options now in one script
*                         color of bw option added
*                         polar / Cartesian option added
*
* This is a generic grads scripts that runs without modifications
* for arbitary numbers of output locations and times. For each 
* and location a five-panel plot is produced.
*
* Scripts and files used :
*    colorset.gs    : Sets up shading colors
*    spec_ids       : Grid and center ID's, defaults if not given
*    ww3.spec.grads : GrADS data file.
*    ww3.spec.ctl   : GrADS control file.
*    ww3.mean.grads : Suplemental GrAds data file.
*
* The files ww3.* are generated automatically by gx_outp
* (WAVEWATCH postprocessor).
*
* *** NOTE it is expected that gx_outp produces spectra at
*     level = 1 and all available source terms.
*     This is not checked internally
* *** NOTE it is expected that the point name does not contains
*     spaces, othewise mean parameters will be clobbered 
*
* General set up - - - - - - - - - - - - - - - - - - - - - - - - -
*
  display=white
*
* llgrd : long-lat grid  [yes/---]
* color : color or b&w   [yes/---]
* polar : polar repres.  [yes/---]
*
  llgrd = ' es'
  color = 'yes'
  polar = 'yes'
*
  if ( polar = 'yes' )
*
* dth   : grid directional increment for polar plot
* dfr   : grid frequency increment for polar plot
* fmx   : maximum frequency in polar plot
*
    dth = 15
    dfr =  0.05
    fmx =  0.25
  else
*
* thoff : direction at bottom of plot
* dth   : grid directional increment for Cartesian plot
* dfr   : grid frequency increment for Cartesian plot
* fmn   : maximum frequency in Cartesian plot
* fmx   : minimum frequency in Cartesian plot
*
    thoff = -180.
    dth = 45.
    dfr =  0.05
    fmn =  0.0412
    fmx =  0.4056
  endif
*
  'set display color ' display
  'clear'
  'run colorset.gs'
*
* ID strings from file - - - - - - - - - - - - - - - - - - - - - -
*
  result = read (spec_ids)
  OK = sublin(result,1)
  if ( OK = '0' )
    grid_ID = sublin(result,2)
  else
    grid_ID = 'unidentified grid'
  endif
*
  result = read (spec_ids)
  OK = sublin(result,1)
  if ( OK = '0' )
    center_ID = sublin(result,2) ', '
  else
    center_ID = ''
  endif
*
* Current date and time  - - - - - - - - - - - - - - - - - - - - -
*
  gdate="yyyy/mm/dd"
  '!date -u "+%Y/%m/%d" > tmp_grads_gdate'
  result = read (tmp_grads_gdate)
  gdate = sublin(result,2)
  '!rm -f tmp_grads_gdate'
*
  center_ID = center_ID gdate
*
* Get size of page - - - - - - - - - - - - - - - - - - - - - - - -
*
  'q gxinfo'
  line = sublin(result,2)
  xmax = subwrd(line,4)
  ymax = subwrd(line,6)
*
* Print initial info to screen - - - - - - - - - - - - - - - - - -
*
  say ' '
  say '-------------------------'
  say '*** Running source.gs ***'
  say '-------------------------'
  say ' '
  say 'Grid ID      : ' grid_ID
  say 'Center ID    : ' center_ID
  say 'Page         : ' xmax ' by ' ymax
*
* Open data file - - - - - - - - - - - - - - - - - - - - - - - - -
*
  'open ww3.spec'
*
* Get some info for file - - - - - - - - - - - - - - - - - - - - -
*
  'q file 1'
  line = sublin(result,5)
  ntime = subwrd(line,12)
  line = sublin(result,6)
  nloc = subwrd(line,5)
*
  say 'Nr. of times : ' ntime
  say 'Nr. of loc.  : ' nloc
*
  iloc = 1
  while ( iloc <= nloc )
    iline = 6 + iloc
    line = sublin(result,iline)
    par_ID.iloc = subwrd(line,1)
    loc_ID.iloc = substr(line,21,10)
    say '   location ' iloc ' is called ' loc_ID.iloc ' and stored as ' par_ID.iloc
    iloc = iloc + 1
  endwhile
*
* loop over time and location to get mean pars and max - - - - - -
*
  say ' '
  say 'Reading mean data and getting maximum spectral density ...'
  'set gxout stat'
*
  fmin = 1.e20
  fmax = -1.e20
  smin = 1.e20
  smax = -1.e20
*
  itime = 1
  while ( itime <= ntime )
*
    'set t ' itime
    say '   processing time ' showtime(itime)
*
    iloc = 1
    while ( iloc <= nloc )
*
      result = read (ww3.mean.grads)
      line = sublin(result,2)
      lo.iloc.itime = subwrd(line,2)
      la.iloc.itime = subwrd(line,3)
      de.iloc.itime = subwrd(line,4)
      ua.iloc.itime = subwrd(line,5)
      ux.iloc.itime = subwrd(line,6)
      uy.iloc.itime = subwrd(line,7)
      ca.iloc.itime = subwrd(line,9)
      cx.iloc.itime = subwrd(line,10)
      cy.iloc.itime = subwrd(line,11)
      hs.iloc.itime = subwrd(line,12)
*
*     say result
*     say iloc ' ' itime ' ' hs.iloc.itime
*     say ua.iloc.itime ' ' ux.iloc.itime ' ' uy.iloc.itime
*     say ca.iloc.itime ' ' cx.iloc.itime ' ' cy.iloc.itime
*
      'set lev 1'
      'd ' par_ID.iloc
      line = sublin(result,8)
      lmin = subwrd(line,4)
      lmax = subwrd(line,5)
      if ( lmin < fmin) ; fmin = lmin ; endif
      if ( lmax > fmax) ; fmax = lmax ; endif
*
      ilev = 2
      while ( ilev <= 6 )
        'set lev ' ilev
        'd ' par_ID.iloc
        line = sublin(result,8)
        lmin = subwrd(line,4)
        lmax = subwrd(line,5)
        if ( lmin < smin) ; smin = lmin ; endif
        if ( lmax > smax) ; smax = lmax ; endif
        ilev = ilev + 1
      endwhile
*
*     say result
*     say fmin ' ' fmax ' ' lmin ' ' lmax
*
      iloc = iloc + 1
*
    endwhile
*
    itime = itime + 1
*
  endwhile
*
  if ( -smin > smax )
    sscl = -smin
  else
    sscl = smax
  endif

  say ' '
  say 'Done, min and max for spectrum are ' fmin ' and ' fmax
  say '      min and max for sources are ' smin ' and ' smax ' (' sscl ')'
*
* Generic page setup - - - - - - - - - - - - - - - - - - - - - - -
*
  say ' '
  say 'Setting up the page ...'
*
  st1 = ''
  st2 = ''
  st3 = ''
  st4 = ''
*
* settings for 6 panel ...
*
  xtc = 0.5 * xmax
  yt1 = 10.7
  yt2 = 0.2 
  yt3 = 0.4
  size1 = 0.20
  size2 = 0.12
  size3 = 0.12
  size4 = 0.12
  nc = 30
*
  pw  = 3.0
  sx  = 0.2
  sy  = 0.3
  xp0 = 0.5 * ( xmax - sx ) - pw
  yp0 = 7.25
*
  xt1 = xtc - pw
  xt2 = xtc - pw + 12*size4
  xt3 = 0.5 * ( xt2 + xtc )
  ytl0 = yp0 - pw - sy - 0.3*pw
*
* Actual plotting loop - - - - - - - - - - - - - - - - - - - - - -
*
  say ' '
  say 'Starting actual plotting ...'
*
  ipage = 0
*
  i = 1
  while ( i <= ntime )
    'set t ' i
*
    j = 1
    while ( j <= nloc )
*
* ... new page ...
*
      ipage = ipage + 1
      say '   Starting with page ' ipage
*
      if ( ipage > 1 )
        iplot = ipage - 1
        'printim sterms_plot_'iplot'.png'
        'clear'
      endif
*
      page_ID = loc_ID.j ' at ' showtime(i)
*
      'set string 1 c 5'
      'set strsiz ' size1
      'draw string ' xtc ' ' yt1 ' ' page_ID
      'set strsiz ' size2
      'draw string ' xtc ' ' yt2 ' ' grid_ID
      'set strsiz ' size3
      'draw string ' xtc ' ' yt3 ' ' center_ID
*
      'set string 1 l 5'
      'set strsiz ' size4
      ytl = ytl0
      if ( llgrd = 'yes' )
        'draw string ' xt1 ' ' ytl ' Location'
      else
        'draw string ' xt1 ' ' ytl ' Location (km)'
      endif
      'set string 1 c 5'
      'draw string ' xt2 ' ' ytl ' :'
      if ( llgrd = 'yes' )
        if ( la.j.i >= 0 )
          lat = la.j.i
          loc = lat 'N '
        else
          lat = - la.j.i
          loc = lat 'S '
        endif
        if ( lo.j.i > 180 )
          lon = 360 - lo.j.i
          loc = loc lon 'W'
        else
          lon = lo.j.i
          loc = loc lon 'E'
        endif
      else
        loc = lo.j.i '-' la.j.i
      endif
*
      'draw string ' xt3 ' ' ytl ' ' loc
*
      'set string 1 l 5'
      ytl = ytl - 2.0 * size4
      'draw string ' xt1 ' ' ytl ' Depth'
      'set string 1 c 5'
      'draw string ' xt2 ' ' ytl ' :'
      'draw string ' xt3 ' ' ytl ' ' de.j.i ' m'
*
      'set string 1 l 5'
      ytl = ytl - 2.0 * size4
      'draw string ' xt1 ' ' ytl ' Wind speed'
      'set string 1 c 5'
      'draw string ' xt2 ' ' ytl ' :'
      'draw string ' xt3 ' ' ytl ' ' ua.j.i ' m/s'
*
      ytl = ytl - 2.0 * size4
      'set string 1 l 5'
      'draw string ' xt1 ' ' ytl ' Current vel.'
      'set string 1 c 5'
      'draw string ' xt2 ' ' ytl ' :'
      'draw string ' xt3 ' ' ytl ' ' ca.j.i ' m/s'
*
      ytl = ytl - 2.0 * size4
      'set string 1 l 5'
      'draw string ' xt1 ' ' ytl ' Wave height'
      'set string 1 c 5'
      'draw string ' xt2 ' ' ytl ' :'
      'draw string ' xt3 ' ' ytl ' ' hs.j.i ' m'
*
* ... plot spectrum ...
*
      'set lev 1'
      st1 = 'spectrum'
      xp = xp0
      yp = yp0
*
      if ( polar = 'yes' )
        dummy = plotspec (xp,yp,pw,dth,dfr,fmx,par_ID.j,fmax,st1,st2,st3,st4,nc,ua.j.i,ux.j.i,uy.j.i,ca.j.i,cx.j.i,cy.j.i,color)
      else
        dummy = plotspec_c (xp,yp,pw,thoff,dth,fmn,fmx,dfr,par_ID.j,fmax,st1,st2,st3,st4,nc,ua.j.i,ux.j.i,uy.j.i,ca.j.i,cx.j.i,cy.j.i,color)
      endif
*
* ... plot input ...
*
      'set lev 2'
      st1 = 'wind source term'
      xp = xp0 + pw + sx
      yp = yp0
*
      if ( polar = 'yes' )
        dummy = plotsrce (xp,yp,pw,dth,dfr,fmx,par_ID.j,sscl,st1,st2,st3,st4,nc,color)
      else
        dummy = plotsrce_c (xp,yp,pw,thoff,dth,fmn,fmx,dfr,par_ID.j,sscl,st1,st2,st3,st4,nc,color)
      endif
*
* ... plot interactions ...
*
      'set lev 3'
      st1 = 'nonlinear interations'
      xp = xp0 + pw + sx
      yp = yp0 - pw - sy
*
      if ( polar = 'yes' )
        dummy = plotsrce (xp,yp,pw,dth,dfr,fmx,par_ID.j,sscl,st1,st2,st3,st4,nc,color)
      else
        dummy = plotsrce_c (xp,yp,pw,thoff,dth,fmn,fmx,dfr,par_ID.j,sscl,st1,st2,st3,st4,nc,color)
      endif
*
* ... plot dissipation ...
*
      'define diss = ' par_ID.j'(z=4) + ' par_ID.j'(z=5)'
      st1 = 'dissipation source term'
      xp = xp0 + pw + sx
      yp = yp0 - 2*pw - 2*sy
*
      if ( polar = 'yes' )
        dummy = plotsrce (xp,yp,pw,dth,dfr,fmx,diss,sscl,st1,st2,st3,st4,nc,color)
      else
        dummy = plotsrce_c (xp,yp,pw,thoff,dth,fmn,fmx,dfr,diss,sscl,st1,st2,st3,st4,nc,color)
      endif
*
* ... plot total source term ...
*
      'set lev 6'
      st1 = 'total source term'
      xp = xp0
      yp = yp0 - pw - sy
*
      if ( polar = 'yes' )
        dummy = plotsrce (xp,yp,pw,dth,dfr,fmx,par_ID.j,sscl,st1,st2,st3,st4,nc,color)
      else
        dummy = plotsrce_c (xp,yp,pw,thoff,dth,fmn,fmx,dfr,par_ID.j,sscl,st1,st2,st3,st4,nc,color)
      endif
*
      j = j + 1
    endwhile
*
    i = i + 1
  endwhile
*
* End of operations  - - - - - - - - - - - - - - - - - - - - - - -
*
  say ' '
  say '------------------------'
  say '*** End of source.gs ***'
  say '------------------------'
  say ' '
*
  'quit'
*
* End of main script - - - - - - - - - - - - - - - - - - - - - - -

* ------------------------------------------------------------------------------
  function plotspec (xp0,yp0,dxyp,dxgrid,dygrid,ymax,spec,scale,str1,str2,str3,str4,strlen,u,ux,uy,c,cx,cy,color)
*
* Function to plot a spectrum at a given location on the page with identifying
* output around it 
*
* Parameter list :
*
*   x/yp0  Lower left corner of the plot in paper coordinates.
*   dxy    Size of the plot in paper coordinates.
*   dxgrid Grid line increment for directions.
*   dygrid Grid line increment for frequencies.
*   ymax   Maximum frequency.
*   spec   Spectrum to be plotted.
*   scale  Scale (division) factor for spectrum.
*   strN   Strings around the plot, top left, clockwise, set to ''
*          to deactivate
*   strlen Number of charcters from left to right, used to scale text
*   ux/y   Wind speed and components.
*   cx/y   Current speed and components.
*   color  color of b&w [yes/---]
*
* 0. Initializations
*
  'set grads off'
  'set lon -180 180'
  'set lat ' 90-ymax ' 90'
*
* 1. Set up plot location
*
  'set parea ' xp0 ' ' xp0+dxyp ' ' yp0 ' ' yp0+dxyp
*
* 2. Set up contour intervals
*
  i = 15
  factor = 2
  level=1.001
  levels=''
*
  while ( i > 0 )
    level = level / factor
    levels = level ' ' levels
    i = i - 1
  endwhile
*
* 3. Plot spectrum
*
  'set mproj nps'
  'set grid off'
*
  if ( color = 'yes' )
    'set gxout shaded'
    'set clevs ' levels
    'set ccols 21 22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38'
    'd ' spec'/'scale
    cblack  = 1
    current = 65
    wind    = 64
  else
    cblack  = 3
    current = 66
    wind    = 63
  endif
*
  'set gxout contour'
  'set cthick ' cblack
  'set clevs ' levels
  'set ccolor 60'
  'set clab off'
  'set clevs ' levels
  'd ' spec'/'scale
*
* 4. Plot grid lines manually to avoid bug
*
  if ( color = 'yes' )
    'set line 66 5 2'
  else
    'set line 66 1 3'
  endif
*
  x = 0
  while ( x < 360+0.1*dxgrid )
    'q ll2xy 'x' '90.-dygrid
    x1 = subwrd(result,1)
    y1 = subwrd(result,2)
    'q ll2xy 'x' '90.-ymax
    x2 = subwrd(result,1)
    y2 = subwrd(result,2)
   'draw line ' x1 ' ' y1 ' ' x2 ' ' y2
    x = x + dxgrid
  endwhile
*
  dx = 5
  y = dygrid
  while ( y < ymax + 0.1*dygrid )
    x = 0
    'q ll2xy 'x' '90.-y
    x2 = subwrd(result,1)
    y2 = subwrd(result,2)
    while ( x < 360 )
      x = x+dx
      x1 = x2
      y1 = y2
      'q ll2xy 'x' '90.-y
      x2 = subwrd(result,1)
      y2 = subwrd(result,2)
      'draw line ' x1 ' ' y1 ' ' x2 ' ' y2
    endwhile
    y = y + dygrid
  endwhile
*
  'set gxout contour'
  'set cthick ' cblack
  'set clevs ' levels
  'set ccolor 60'
  'set clab off'
  'set clevs ' levels
  'd ' spec'/'scale
*
* 5. Plot current and wind vector
*
  xc = xp0 + 0.5*dxyp
  yc = yp0 + 0.5*dxyp
*
* 5.a current
*
  'set line ' current ' 1 6'
*
  cmax = 1.5
  cmin = 0.1
  if ( c > cmin )
    if ( c < cmax )
      dc  = 0.375 * (dygrid/ymax) * dxyp * c / cmax
    else
      dc  = 0.375 * (dygrid/ymax) * dxyp
    endif
    dcx = dc * cx / c
    dcy = dc * cy / c
    x2 = xc - dcx
    y2 = yc - dcy
    x1 = xc + dcx
    y1 = yc + dcy
*
*   'draw line ' x2 ' ' y2 ' ' x1 ' ' y1
*
    'q xy2w 'x1' 'y1
    lon = subwrd(result,3)
    lat = subwrd(result,6)
    lonh = lon + 10
    lath = 90. - 0.75*(90.-lat)
    'q ll2xy 'lonh' 'lath
    x2 = subwrd(result,1)
    y2 = subwrd(result,2)
    'draw line ' x1 ' 'y1 ' ' x2 ' ' y2
    lonh = lon - 10
    'q ll2xy 'lonh' 'lath
    x2 = subwrd(result,1)
    y2 = subwrd(result,2)
    'draw line ' x1 ' 'y1 ' ' x2 ' ' y2
*
  endif
*
* 5.b wind
*
  'set line ' wind '  1 6'
*
  umax = 20.
  umin = 2.5
  if ( u > umin )
    if ( u < umax )
      du  = 0.375 * (dygrid/ymax) * dxyp * u / umax
    else
      du  = 0.375 * (dygrid/ymax) * dxyp
    endif
    dux = du * ux / u
    duy = du * uy / u
    x2 = xc - dux
    y2 = yc - duy
    x1 = xc + dux
    y1 = yc + duy
*
    'draw line ' x2 ' ' y2 ' ' x1 ' ' y1
*
    'q xy2w 'x1' 'y1
    lon = subwrd(result,3)
    lat = subwrd(result,6)
    lonh = lon + 10
    lath = 90. - 0.75*(90.-lat)
    'q ll2xy 'lonh' 'lath
    x2 = subwrd(result,1)
    y2 = subwrd(result,2)
    'draw line ' x1 ' 'y1 ' ' x2 ' ' y2
    lonh = lon - 10
    'q ll2xy 'lonh' 'lath
    x2 = subwrd(result,1)
    y2 = subwrd(result,2)
    'draw line ' x1 ' 'y1 ' ' x2 ' ' y2
*
  endif
*
* 6. ID text around spectrum
*
  size = dxyp / strlen
  'set strsiz ' size
*
  if ( str1 != '' )
    'set string 1 bl 4'
    'draw string ' xp0      ' ' yp0+dxyp+0.75*size ' ' str1
  endif
*
  if ( str2 != '' )
    'set string 1 br 4'
    'draw string ' xp0+dxyp ' ' yp0+dxyp+0.75*size ' ' str2
  endif
*
  if ( str3 != '' )
    'set string 1 tr 4'
    'draw string ' xp0+dxyp ' ' yp0     -0.75*size ' ' str3
  endif
*
  if ( str4 != '' )
    'set string 1 tl 4'
    'draw string ' xp0      ' ' yp0     -0.75*size ' ' str4
  endif
*
  return

* ------------------------------------------------------------------------------
  function plotsrce (xp0,yp0,dxyp,dxgrid,dygrid,ymax,source,scale,str1,str2,str3,str4,strlen,color)
*
* Function to plot a source term at a given location on the page with identifying
* output around it 
*
* Parameter list :
*
*   x/yp0  Lower left corner of the plot in paper coordinates.
*   dxy    Size of the plot in paper coordinates.
*   dxgrid Grid line increment for directions.
*   dygrid Grid line increment for frequencies.
*   ymax   Maximum frequency.
*   source Source term to be plotted.
*   scale  Scale (division) factor for source term.
*   strN   Strings around the plot, top left, clockwise, set to ''
*          to deactivate
*   strlen Number of charcters from left to right, used to scale text
*   color  color of b&w [yes/---]
*
* 0. Initializations
*
  'set grads off'
  'set lon -180 180'
  'set lat ' 90-ymax ' 90'
*
* 1. Set up plot location
*
  'set parea ' xp0 ' ' xp0+dxyp ' ' yp0 ' ' yp0+dxyp
*
* 2. Set up contour intervals
*
  i = 7
  factor = 2
  levelp =  1.001
  leveln = -1.001
  levpos = ''
  levneg = ''
*
  while ( i > 0 )
    levelp = levelp / factor
    leveln = leveln / factor
    levpos = levelp ' ' levpos
    levneg = levneg ' ' leveln
    i = i - 1
  endwhile
  levels = levneg ' 0 ' levpos
*
* 3. Plot source term
*
  'set mproj nps'
  'set grid off'
*
  if ( color = 'yes' )
    'set gxout shaded'
    'set clevs ' levels
    'set ccols 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56'
*   'set ccols 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41'
    'd ' source'/'scale
    cblack  = 1
    stpos   = 1
    stneg   = 1
  else
    cblack  = 3
    stpos   = 1
    stneg   = 3
  endif
*
  'set gxout contour'
  'set cthick ' cblack
  'set ccolor 60'
  'set clab off'
  'set cstyle ' stpos
  'set clevs ' levpos
  'd ' source'/'scale
*
* 4. Plot grid lines manually to avoid bug
*
  if ( color = 'yes' )
    'set line 60 5 2'
  else
    'set line 66 1 3'
  endif
*
  x = 0
  while ( x < 360+0.1*dxgrid )
    'q ll2xy 'x' '90.-dygrid
    x1 = subwrd(result,1)
    y1 = subwrd(result,2)
    'q ll2xy 'x' '90.-ymax
    x2 = subwrd(result,1)
    y2 = subwrd(result,2)
   'draw line ' x1 ' ' y1 ' ' x2 ' ' y2
    x = x + dxgrid
  endwhile
*
  dx = 5
  y = dygrid
  while ( y < ymax + 0.1*dygrid )
    x = 0
    'q ll2xy 'x' '90.-y
    x2 = subwrd(result,1)
    y2 = subwrd(result,2)
    while ( x < 360 )
      x = x+dx
      x1 = x2
      y1 = y2
      'q ll2xy 'x' '90.-y
      x2 = subwrd(result,1)
      y2 = subwrd(result,2)
      'draw line ' x1 ' ' y1 ' ' x2 ' ' y2
    endwhile
    y = y + dygrid
  endwhile
*
  'set gxout contour'
  'set cthick ' cblack
  'set ccolor 60'
  'set clab off'
  'set cstyle ' stpos
  'set clevs ' levpos
  'd ' source'/'scale
  'set cthick ' cblack
  'set ccolor 60'
  'set clab off'
  'set cstyle ' stneg
  'set clevs ' levneg
  'd ' source'/'scale
*
* 5. ID text around source term
*
  size = dxyp / strlen
  'set strsiz ' size
*
  if ( str1 != '' )
    'set string 1 bl 4'
    'draw string ' xp0      ' ' yp0+dxyp+0.75*size ' ' str1
  endif
*
  if ( str2 != '' )
    'set string 1 br 4'
    'draw string ' xp0+dxyp ' ' yp0+dxyp+0.75*size ' ' str2
  endif
*
  if ( str3 != '' )
    'set string 1 tr 4'
    'draw string ' xp0+dxyp ' ' yp0     -0.75*size ' ' str3
  endif
*
  if ( str4 != '' )
    'set string 1 tl 4'
    'draw string ' xp0      ' ' yp0     -0.75*size ' ' str4
  endif
*
  return

* ------------------------------------------------------------------------------
  function plotspec_c (xp0,yp0,dxyp,xoff,dx,ymin,ymax,dy,spec,scale,str1,str2,str3,str4,strlen,u,ux,uy,c,cx,cy,color)
*
* Function to plot a spectrum at a given location on the page with identifying
* output around it 
*
* Parameter list :
*
*   x/yp0  Lower left corner of the plot in paper coordinates.
*   dxy    Size of the plot in paper coordinates.
*   xoff   Shift of first frequency (if 0, axis from 0-360 cartesian)
*   dx     Directional increment (axis).
*   ymin   Minimum frequency.
*   ymax   Maximum frequency.
*   dy     Frequency increment (axis).
*   spec   Spectrum to be plotted.
*   scale  Scale (division) factor for spectrum.
*   strN   Strings around the plot, top left, clockwise, set to ''
*          to deactivate
*   strlen Number of charcters from left to right, used to scale text
*   ux/y   Wind speed and components.
*   cx/y   Current speed and components.
*
* 0. Initializations
*
  'set grads off'
  'set lon ' xoff-270 ' ' xoff+90
  'set lat ' 90-ymax ' ' 90-ymin
*
* 1. Set up plot location
*
  'set parea ' xp0+0.07*dxyp ' ' xp0+dxyp ' ' yp0+0.05*dxyp ' ' yp0+dxyp
*
* 2. Set up contour intervals
*
  i = 15
  factor = 2
  level=1.001
  levels=''
*
  while ( i > 0 )
    level = level / factor
    levels = level ' ' levels
    i = i - 1
  endwhile
*
* 3. Plot spectrum
*
  'set mproj scaled'
  'set xyrev on'
  'set xflip on'
  'set grid off'
  'set xlab off'
  'set ylab off' 
  labsize = dxyp / strlen / 1.25
  'set ylopts 1 4 ' labsize
  'set ylint ' dx
  'set xlint ' dy
*
  if ( color = 'yes' )
    'set gxout shaded'
    'set clevs ' levels
    'set ccols 21 22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38'
    'd ' spec'/'scale
    cblack  = 1
  else
    cblack  = 3
  endif
*
  'set gxout contour'
  'set cthick ' cblack
  'set ccolor 60'
  'set clab off'
  'set clevs ' levels
  'd ' spec'/'scale
*
* 4. Plot axes manually
*
  freq = 0.
  'set line 1 1 4'
  'set string 1 c 4'
  'set strsiz ' labsize
*
  while ( freq <= ymax )
    if ( freq >= ymin )
      xt = xp0 + 0.07*dxyp + 0.93*dxyp * ( freq - ymin ) / ( ymax - ymin )
      'draw line ' xt ' ' yp0+1.5*labsize ' ' xt ' ' yp0 + 0.05*dxyp
      'draw string ' xt ' ' yp0+0.5*labsize ' ' freq
    endif
    freq = freq + dy
  endwhile
*
  thmin = xoff
  thmax = xoff + 360.
  th = 0.
*
  while ( th > thmin )
    th = th - dx
  endwhile
*
  'set string 1 r 4'
  while ( th <= thmax )
    if ( th >= thmin )
      yt = yp0 + 0.05*dxyp + 0.95*dxyp * ( th - thmin ) / ( thmax - thmin )
      'draw line ' xp0+0.07*dxyp ' ' yt ' ' xp0+0.07*dxyp-0.5*labsize ' ' yt
      'draw string ' xp0+0.07*dxyp-1.0*labsize ' ' yt ' ' th
    endif
    th = th + dx
  endwhile
*
* 5. Plot current and wind vector
*
*
* 6. ID text around spectrum
*
  size = dxyp / strlen
  'set strsiz ' size
*
  if ( str1 != '' )
    'set string 1 bl 4'
    'draw string ' xp0+0.07*dxyp ' ' yp0+dxyp+0.75*size ' ' str1
  endif
*
  if ( str2 != '' )
    'set string 1 br 4'
    'draw string ' xp0+dxyp ' ' yp0+dxyp+0.75*size ' ' str2
  endif
*
  if ( str3 != '' )
    'set string 1 tr 4'
    'draw string ' xp0+dxyp ' ' yp0     -0.75*size ' ' str3
  endif
*
  if ( str4 != '' )
    'set string 1 tl 4'
    'draw string ' xp0+0.07*dxyp ' ' yp0     -0.75*size ' ' str4
  endif
*
  return

* ------------------------------------------------------------------------------
  function plotsrce_c (xp0,yp0,dxyp,xoff,dx,ymin,ymax,dy,source,scale,str1,str2,str3,str4,strlen,color)
*
* Function to plot a source term at a given location on the page with identifying
* output around it 
*
* Parameter list :
*
*   x/yp0  Lower left corner of the plot in paper coordinates.
*   dxy    Size of the plot in paper coordinates.
*   xoff   Shift of first frequency (if 0, axis from 0-360 cartesian)
*   dx     Directional increment (axis).
*   ymin   Minimum frequency.
*   ymax   Maximum frequency.
*   dy     Frequency increment (axis).
*   source Source term to be plotted.
*   scale  Scale (division) factor for source term.
*   strN   Strings around the plot, top left, clockwise, set to ''
*          to deactivate
*   strlen Number of charcters from left to right, used to scale text
*
* 0. Initializations
*
  'set grads off'
  'set lon ' xoff-270 ' ' xoff+90
  'set lat ' 90-ymax ' ' 90-ymin
*
* 1. Set up plot location
*
  'set parea ' xp0+0.07*dxyp ' ' xp0+dxyp ' ' yp0+0.05*dxyp ' ' yp0+dxyp
*
* 2. Set up contour intervals
*
  i = 7
  factor = 2
  levelp =  1.001
  leveln = -1.001
  levpos = ''
  levneg = ''
*
  while ( i > 0 )
    levelp = levelp / factor
    leveln = leveln / factor
    levpos = levelp ' ' levpos
    levneg = levneg ' ' leveln
    i = i - 1
  endwhile
  levels = levneg ' 0 ' levpos
*
* 3. Plot source term
*
  'set mproj scaled'
  'set xyrev on'
  'set xflip on'
  'set grid off'
  'set xlab off'
  'set ylab off' 
  labsize = dxyp / strlen / 1.25
  'set ylopts 1 4 ' labsize
  'set ylint ' dx
  'set xlint ' dy
*
  if ( color = 'yes' )
    'set gxout shaded'
    'set clevs ' levels
    'set ccols 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56'
*   'set ccols 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41'
    'd ' source'/'scale
    cblack  = 1
    stpos   = 1
    stneg   = 1
  else 
    cblack  = 3 
    stpos   = 1
    stneg   = 3
  endif
*
  'set gxout contour'
  'set cthick ' cblack
  'set ccolor 60'
  'set clab off'
  'set cstyle ' stpos
  'set clevs ' levpos
  'd ' source'/'scale
  'set cthick ' cblack
  'set ccolor 60'
  'set clab off'
  'set cstyle ' stneg
  'set clevs ' levneg
  'd ' source'/'scale
*
* 4. Plot axes manually
*
  freq = 0.
  'set line 1 1 4'
  'set string 1 c 4'
  'set strsiz ' labsize
*
  while ( freq <= ymax )
    if ( freq >= ymin )
      xt = xp0 + 0.07*dxyp + 0.93*dxyp * ( freq - ymin ) / ( ymax - ymin )
      'draw line ' xt ' ' yp0+1.5*labsize ' ' xt ' ' yp0 + 0.05*dxyp
      'draw string ' xt ' ' yp0+0.5*labsize ' ' freq
    endif
    freq = freq + dy
  endwhile
*
  thmin = xoff
  thmax = xoff + 360.
  th = 0.
*
  while ( th > thmin )
    th = th - dx
  endwhile
*
  'set string 1 r 4'
  while ( th <= thmax )
    if ( th >= thmin )
      yt = yp0 + 0.05*dxyp + 0.95*dxyp * ( th - thmin ) / ( thmax - thmin )
      'draw line ' xp0+0.07*dxyp ' ' yt ' ' xp0+0.07*dxyp-0.5*labsize ' ' yt
      'draw string ' xp0+0.07*dxyp-1.0*labsize ' ' yt ' ' th
    endif
    th = th + dx
  endwhile
*
* 5. ID text around source term
*
  size = dxyp / strlen
  'set strsiz ' size
*
  if ( str1 != '' )
    'set string 1 bl 4'
    'draw string ' xp0+0.07*dxyp ' ' yp0+dxyp+0.75*size ' ' str1
  endif
*
  if ( str2 != '' )
    'set string 1 br 4'
    'draw string ' xp0+dxyp ' ' yp0+dxyp+0.75*size ' ' str2
  endif
*
  if ( str3 != '' )
    'set string 1 tr 4'
    'draw string ' xp0+dxyp ' ' yp0     -0.75*size ' ' str3
  endif
*
  if ( str4 != '' )
    'set string 1 tl 4'
    'draw string ' xp0+0.07*dxyp ' ' yp0     -0.75*size ' ' str4
  endif
*
  return

* ------------------------------------------------------------------------------
  function showtime (itime)
*
* Get date and time in preferred format
*
* Parameter list :
*
*   itime    Discrete time counter
*
  'set t ' itime
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

  if ( test='Z' )
    vdate = year '/' month '/' day ' ' hour 'z'
  else
    vdate = year '/' month '/' day ' ' hour ':' min 'z'
  endif
*
  return vdate
