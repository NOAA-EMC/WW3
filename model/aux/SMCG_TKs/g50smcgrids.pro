;;  Adapted from idl script for PVWave  12 Sept 2005
;;  It reads MFT model output files and plots contours of the
;;  tracer concentration in a ps file.
;;
;; First created: For 4 views     J G Li   26 Nov 2008
;; Last modified: Color grids     J G Li   26 Aug 2009
;; Adapted for 25km SMC grids     J G Li    3 Feb 2010
;; Updated G25SMC-12c grids.      J G Li   25 May 2011
;; Sterographic projection nR.    J G Li   30 Jun 2011
;; Adapted for G50SMC grid.       J G Li   19 Aug 2011
;; Extended to fill the Arctic.   J G Li    5 Oct 2011
;; Rectify polar cell position.   J G Li   25 Oct 2011
;; Simplify with readcell and steromap.  JGLi12Nov2014
;

;;  Read global and Arctic part cells. 
  Cel_file = 'DatGMC/G50SMCels.dat'
  Arc_file = 'DatGMC/G50SMCBAr.dat'

  READCELL, Cel_File, cel, nc, ArcFile = Arc_file, NArc = na, NBAr = nb

;; Maximum j row number in Global part
  jmxglb = max( cel(1,nc-na-1L) )
  Print, ' Maximum j row =', jmxglb

;;  Degree to radian conversion parameter.
  d2rad=!Pi/180.0

;;  Maximum mapping radius.
  radius=10.0

;; Whole global projection angle from N Pole to be 90.0
   pangle=90.0
;; Reduced projection angle from rotated N Pole
;  pangle=27.5

;; Size-1 cell increments
 dxlon=0.703125
 dylat=0.468750

;; Outline circle at equator
 ciran=findgen(1081)*d2rad/3.0
 xcirc=radius*cos(ciran)
 ycirc=radius*sin(ciran)

 colorlevls=[(indgen(11)+1)*23]
 heights=[-0.1, 0.1, 0.5, 0.9, 1.1, 2.0, 3.0, 4.0, 4.9, 5.1, 6.0]

;; Lon and lat grid numbers + 1  for cell faces
 nx=513
 ny=385
;;  Note full grid from south to north poles requires 384*dlat

;;  Half the latitude grid to set j=0 on the Equator.
 neqt=(ny-1)/2
 Print, 'Full and Half lat grid =', ny-1, neqt

 xlon=(findgen(nx))*dxlon
 ylat=(findgen(ny)-neqt)*dylat
;;  Equator is now at ylat(neqt)=0.0 

;; Color levels to be used
  mclr=128
  colrs=[indgen(mclr)]
  depth=[10000,1000,100,10,1]
  cdeps=TRANSPOSE( cel(4,*) )
  clrdep=FIX( (4.0-ALOG10(FLOAT(cdeps)))*31.5 )
  marks=FIX( (4.0-ALOG10(FLOAT(depth)))*31.5 )

;; Key sympol as filled vertical tangular
 xsymb=[0, 0, 0.8, 0.8, 0]
 ysymb=[0, 5.5, 5.5, 0, 0]
 usersym, xsymb, ysymb, /fill

 xkeys=[findgen(mclr)/float(mclr-1)-1.52]*radius
 ykeys= findgen(mclr)*0.0+radius*0.88


;; New Pole as projection direction (central point)
 file4=[ '0',  '1',  '2',  '3' ]
 plon4=[  0.0,  90.0,  180.0,  270.0 ]
 plat4=[ 23.5,  40.0,   40.0,   40.0 ]

;; Open two files to store converted quadrilaterals coordinates
;; They contain one loop of the 4 trapezoid corners (5 points)
;; x and y coornates (5 each) for each cell, respectively.
;; Equatorial lat lon are also stored to identify two hemispheres
 openw, 10, 'G50STLaL.dat'
 openw, 11, 'G50STSpX.dat'
 openw, 12, 'G50STSpY.dat'
 printf,10, nc, 2,  Format="(2I8)"
 printf,11, nc, 5,  Format="(2I8)"
 printf,12, nc, 5,  Format="(2I8)"

 sxcel=fltarr(5,nc+1)
 sycel=fltarr(5,nc+1)
 seqll=fltarr(2,nc)

;set_plot, 'PS'
;;  Repeat for 2x2 plots 
;; FOR kk=0, 3, 2 DO Begin
   kk=0
 
 plon= plon4(kk)
 plat= plat4(kk)

;PR,/ps,/landscape,/color,file="g50grdcxy"+file4(kk)+".ps", $
;  xsize=20.0, ysize=39.0, xoffset=2.5, yoffset=1.5

 SET_PLOT, 'PS'
;DEVICE, /Encaps, /Portrait, Bits_Per_Pixel=8, /Color, File="g50strgrd.eps",  $
 DEVICE,          /Portrait, Bits_Per_Pixel=8, /Color, File="g50smcgrd.ps",   $
         YSize=20.0, XSize=39.0, xoffset=1.5, yoffset=1.5

;;*************** Set colours                *******************
  RESTORE_COLOURS,"/home/h05/frjl/PVWave/palettes/linuxspectrum.clr"

;; Set back and fore ground color to be white and black, respectively
    tvlct, 255, 255, 255,  !P.Background
; PRINT, ' !P.Background=',!P.Background
    tvlct,  0,  0,  0, !P.Color
; PRINT, ' !P.Color=', !P.Color

;***************  *******************  *******************
  !p.charthick = 2  &  !p.charsize = 1.0

   !P.Multi = [0, 2, 0, 0, 0]

;  Northern hemisphere

   plot, xcirc, ycirc, xrange=[-10,10], yrange=[-10,10],  $
       xstyle=4, ystyle=4, thick=2, $
       xmargin=[2,1], ymargin=[1,1]
;      title=' GMC Grid North NC='+string(nc, Format='(I6)')+ $
;            ' N4='+string(n4, Format='(I4)')
;            ' N2='+string(n2, Format='(I5)')+ $
;            ' N1='+string(n1, Format='(I6)')

;; Exclude last cell, the North Polar Cell
 for i=0L,nc-2 do BEGIN

   xc=[cel(0,i),cel(0,i)+cel(2,i),cel(0,i)+cel(2,i),cel(0,i),cel(0,i)]
   yc=[cel(1,i),cel(1,i),cel(3,i)+cel(1,i),cel(3,i)+cel(1,i),cel(1,i)]
   slat=ylat(yc+neqt)
   slon=xlon(xc)

;; Convert slat slon to elat elon with given new pole

   STEROMAP, slat, slon, elat, elon, sxc, syc, $ 
             Polat=plat, Polon=plon, Pangl=pangle, /Onecl

   seq=[elat(0), elon(0)]

   sxcel(*,i)=sxc
   sycel(*,i)=syc
   seqll(*,i)=seq

;; Store converted x and y values
   printf,10, seq,  Format="(2F9.3)"
   printf,11, sxc,  Format="(5F9.4)"
   printf,12, syc,  Format="(5F9.4)"

   if(elat(0) ge 0) then Begin
;  oplot, sxc, syc, Color=0
   oplot, sxc, syc, Color=colrs(clrdep(i))
   xyouts, (sxc(0)+sxc(2))*0.5, (syc(0)+syc(2))*0.5, '.', $
           charsize=0.005*Abs(sin(elat(0)*d2rad)), alignment=0.5, Color=colrs(clrdep(i))
;  polyfill, sxc, syc, Color=colrs(clrdep(i))
;; Mark the arctical map-east reference region by first ring of its boundary cells at jmxglb-3
   if(cel(1,i) eq jmxglb-3 ) then polyfill, sxc, syc, Color=236
;; Mark the end row of local-east reference region by last row of the global cells at jmxglb
   if(cel(1,i) eq jmxglb ) then polyfill, sxc, syc, Color=136
   endif
 endfor

;;  Polar cell as a octagon, note polar cell size set as size-64 for flux calculation
;;  As it mergers 8 size-64 cells, each side of the octagon should be size-64 
;;  10 apexes are calculated to conform with other cells.  To plot it, drop the last apex.
   i=nc-1
   xc=cel(0,i)+indgen(9)*cel(2,i)
   yc=xc*0+cel(1,i)
   slat=ylat(yc+neqt)
   slon=xlon(xc)

;; Convert slat slon to elat elon with given new pole

   STEROMAP, slat, slon, elat, elon, sxc, syc, $ 
             Polat=plat, Polon=plon, Pangl=pangle, /Onecl

   seq=[elat(0), elon(0)]

   seqll(*,i)=seq
   sxcel(*,i)=sxc(0:4)
   sycel(*,i)=syc(0:4)
   sxcel(*,i+1)=sxc(4:8)
   sycel(*,i+1)=syc(4:8)

;; Store converted x and y values
   printf,10, seq,  Format="(2F9.3)"
   printf,11, sxc(0:4),  Format="(5F9.4)"
   printf,12, syc(0:4),  Format="(5F9.4)"
   printf,11, sxc(4:8),  Format="(5F9.4)"
   printf,12, syc(4:8),  Format="(5F9.4)"

   if(elat(0) ge 0) then Begin
   oplot, sxc, syc, Color=0 
   oplot, sxc, syc, Color=colrs(clrdep(i))
   xyouts, (sxc(0)+sxc(4))*0.5, (syc(0)+syc(4))*0.5, '.', $
           charsize=0.5*Abs(sin(elat(0)*d2rad)), alignment=0.5, Color=254
   endif

;  Southern hemisphere

   plot, xcirc, ycirc, xrange=[-10,10], yrange=[-10,10],  $
       xstyle=4, ystyle=4, thick=2, $
       xmargin=[1,2], ymargin=[1,1]

;; Exclude last cell, the North Polar Cell
 for i=0L,nc-2 do BEGIN

   seq=seqll(*,i)

   if(seq(0) lt 0) then Begin
   sxc=sxcel(*,i)
   syc=sycel(*,i)

;  oplot, sxc, syc, Color=0
   oplot, sxc, syc, Color=colrs(clrdep(i))
   xyouts, (sxc(0)+sxc(2))*0.5, (syc(0)+syc(2))*0.5, '.', $
           charsize=0.005*Abs(sin(elat(0)*d2rad)), alignment=0.5, Color=colrs(clrdep(i))
;  polyfill, sxc, syc, Color=colrs(clrdep(i))
   endif
 endfor
  
;;  Put cell information inside plot
   xyouts, -10.2, -10.0, '!5Spherical Multiple-Cell 50km Grid',  $
           Alignment=0.5, Color=0,   Charthick=2,CharSize=1.5
   xyouts, -10.2, - 9.0, 'NC='+string(nc, Format='(I7)'),$
           Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
   xyouts, -10.2, - 8.4, 'NA='+string(na, Format='(I7)'),$
           Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
   xyouts, -10.2, - 7.8, 'NB='+string(nb, Format='(I6)'),$
           Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
;  xyouts, -10.2, - 7.2, 'N4='+string(n4, Format='(I5)'),$
;          Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
;  xyouts, -10.2, - 6.6, 'N8='+string(n8, Format='(I5)'),$
;          Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
;  xyouts, -10.2, - 6.0, 'N16='+string(n9-41, Format='(I4)'),$
;          Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
;  xyouts, -10.2, - 5.4, 'N32='+string(32, Format='(I3)'),$
;          Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
;  xyouts, -10.2,   5.8, 'N64='+string(8, Format='(I2)'),$
;          Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
;  xyouts, -10.2,   6.4, 'NPo='+string(1, Format='(I2)'),$
;          Alignment=0.5, Color=254, Charthick=2,CharSize=1.3

;; Add a color key
 plots, xkeys, ykeys, psym=8, color=colrs, symsize=1.0
 plots, xkeys(marks), ykeys(marks), psym=8, color=colrs(marks), symsize=1.2
 for j=0,4 do xyouts, xkeys(marks(j)), ykeys(marks(j))+0.8, $
         string(fix(depth(j)), Format='(I5)'), charsize=1.0, $
         charthick=2.0, color=0, alignment=0.8
 xyouts, -10.2, 8.2, 'Depth (m)', Alignment=0.5, Color=0, Charthick=2,CharSize=1.2

;PREND,'B2_CL13',/noprint,/view,/keep

;; End of kk loop
;; Endfor

 DEVICE, /Close

 end

