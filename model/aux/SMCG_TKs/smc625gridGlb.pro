;;  Adapted from idl script for PVWave  12 Sept 2005
;;  It reads MFT model output files and plots contours of the
;;  tracer concentration in a ps file.
;;
;; First created: For 4 views     J G Li   26 Nov 2008
;; Last modified: Color grids     J G Li   26 Aug 2009
;; Adapted for 25km SMC grids     J G Li    3 Feb 2010
;; Adapted for 6-25km SMC grids   J G Li   18 Feb 2010
;; Add Arctic boundary rings.     J G Li   10 Jun 2011
;; S4R sterographic projection.   J G Li   30 Jun 2011
;; Modified for SMC625 grid.      J G Li   15 Dec 2011
;; Add 33 buoy locations.         J G Li   11 Apr 2012
;

 nc=0L
 ng=0L
 n1=0L
 n2=0L
 n4=0L
 n8=0L
 n9=0L

 openr, 9, 'DatGMC/SMC625Cels.dat'
 readf, 9, ng, n1, n2, n4 
 cel=intarr(5,ng)
 readf, 9, cel
 close, 9

 Print, ' GloMCels read done'
 Print, ng, n1, n2, n4 
 Print, cel(*,0)
 Print, cel(*,ng-1)

;; Maximum j row number in Global part
 jmxglb = max( cel(1,*) )
 Print, ' Maximum j row =', jmxglb

;;  Arctic part from SMC625Arcl.dat
 na=0L

 openr, 8, 'DatGMC/SMC625BArc.dat'
 readf, 8, na, n8, n9
 ael=intarr(5,na)
 readf, 8, ael
 close, 8

 Print, ' SMC625BArc read done'
 Print, na, n8, n9
 Print, ael(*,0)
 Print, ael(*,na-1)

;;  Merge Arctic part but excluding boundary cells together
 nb=n8+n9  
 nc=ng + na - nb 
 arl=ael(*,nb:na-1)
 cel=transpose([transpose(cel), transpose(arl)])
 Print, nc, n1, n2, n4, n8, n9 


 d2rad=!Pi/180.0
 radius=10.0

;; Number of radius of projection distance
 np=4.0

;; Projected Equator radius * np to be (np-1)*radius
 PRadus=(np-1.0)*radius

 dxlon=0.087890635D
 dylat=0.05859375D
;dxlon=0.3515625D
;dylat=0.2343750D

;; Outline circle at equator
 ciran=findgen(1081)*d2rad/3.0
 xcirc=radius*cos(ciran)
 ycirc=radius*sin(ciran)

 colorlevls=[(indgen(11)+1)*23]
 heights=[-0.1, 0.1, 0.5, 0.9, 1.1, 2.0, 3.0, 4.0, 4.9, 5.1, 6.0]

 nx=4097
 ny=3073
;;  Note full grid from south to north poles requires 3072*dlat

 xlon=(Dindgen(nx))*dxlon
 ylat=(Dindgen(ny)-1536.0D)*dylat
 jeqt=1536
;;  Equator is now at ylat(1536)=0.0 

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

;; Read in 33 buoys id lat lon data from buoy33.dat
   ndbuoy = 'DatGMC/buoy33.dat'
    nnumb = 33
   buoids = LONARR(nnumb)
   buolat = FLTARR(nnumb)
   buolon = FLTARR(nnumb)
   status = DC_READ_FREE( ndbuoy, buoids, buolat, buolon, /Column )

;; New Pole as projection direction (central point)
 file4=[ '0',  '1',  '2',  '3' ]
 plon4=[  0.0,  90.0,  180.0,  270.0 ]
 plat4=[ 23.5,  40.0,   40.0,   40.0 ]

;; Open two files to store converted quadrilaterals coordinates
;; They contain one loop of the 4 trapezoid corners (5 points)
;; x and y coornates (5 each) for each cell, respectively.
;; Equatorial lat lon are also stored to identify two hemispheres
 openw, 10, 'S6GSTLaL.dat'
 openw, 11, 'S6GSTSpX.dat'
 openw, 12, 'S6GSTSpY.dat'
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

;PR,/ps,/landscape,/color,file="g25grdcxy"+file4(kk)+".ps", $
;  xsize=20.0, ysize=39.0, xoffset=2.5, yoffset=1.5

 SET_PLOT, 'PS'
;DEVICE, /Encaps, /Portrait, Bits_Per_Pixel=8, /Color, File="g6kstrgrd.eps",  $
 DEVICE,          /Portrait, Bits_Per_Pixel=8, /Color, File="smc625grd.ps",   $
         YSize=21.0, XSize=40.0, xoffset=1.0, yoffset=3.0

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
 for i=0L,nc-2L do BEGIN

   xc=[cel(0,i),cel(0,i)+cel(2,i),cel(0,i)+cel(2,i),cel(0,i),cel(0,i)]
   yc=[cel(1,i),cel(1,i),cel(3,i)+cel(1,i),cel(3,i)+cel(1,i),cel(1,i)]
   slat=ylat(yc+jeqt)
   slon=xlon(xc)

;; Convert slat slon to elat elon with given new pole

   LL2EqDeg, slat, slon, elat, elon, PoLat=Plat, PoLon=Plon

   if(elat(0) ge 0.0) then BEGIN
     pradmp=PRadus*cos(elat*d2rad)/(np - 1.0 + sin(elat*d2rad))
     syc = pradmp*cos(elon*d2rad)
     sxc =-pradmp*sin(elon*d2rad)
   endif else BEGIN
     pradmp=PRadus*cos(elat*d2rad)/(np - 1.0 - sin(elat*d2rad))
     syc = pradmp*cos(elon*d2rad)
     sxc = pradmp*sin(elon*d2rad)
   endelse

;  syc= radius*cos(elat*d2rad)*cos(elon*d2rad)
;  sxc0=-radius*cos(elat*d2rad)*sin(elon*d2rad)
;  if(elat(0) ge 0.0) then sxc= sxc0
;  if(elat(0) lt 0.0) then sxc=-sxc0

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
           charsize=0.005*cel(3,i)*Abs(sin(elat(0)*d2rad)), alignment=0.5, Color=colrs(clrdep(i)) 
;; Mark the arctical map-east reference region by first ring of its boundary cells at jmxglb - 3*4
     if(cel(1,i) eq jmxglb - 12) then polyfill, sxc, syc, Color=236
;; Mark the end of local-east reference region by last row of global part at jmxglb
     if(cel(1,i) eq jmxglb     ) then polyfill, sxc, syc, Color=136
   endif
 endfor

;;  Polar cell as a octagon, note polar cell size set as size-128 for flux calculation
;;  As it mergers 8 size-128 cells, each side of the octagon should be size-128
;;  10 apexes are calculated to conform with other cells.  To plot it, drop the last apex.
   i=nc-1
   xc=cel(0,i)+indgen(9)*cel(2,i)
   yc=xc*0+cel(1,i)
   slat=ylat(yc+jeqt)
   slon=xlon(xc)

;; Convert slat slon to elat elon with given new pole

   LL2EqDeg, slat, slon, elat, elon, PoLat=Plat, PoLon=Plon

   if(elat(0) ge 0.0) then BEGIN
     pradmp=PRadus*cos(elat*d2rad)/(np - 1.0 + sin(elat*d2rad))
     syc = pradmp*cos(elon*d2rad)
     sxc =-pradmp*sin(elon*d2rad)
   endif else BEGIN
     pradmp=PRadus*cos(elat*d2rad)/(np - 1.0 - sin(elat*d2rad))
     syc = pradmp*cos(elon*d2rad)
     sxc = pradmp*sin(elon*d2rad)
   endelse

;  syc= radius*cos(elat*d2rad)*cos(elon*d2rad)
;  sxc0=-radius*cos(elat*d2rad)*sin(elon*d2rad)
;  if(elat(0) ge 0.0) then sxc= sxc0
;  if(elat(0) lt 0.0) then sxc=-sxc0

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
   oplot, sxc, syc, Color=colrs(clrdep(i))
   xyouts, (sxc(0)+sxc(4))*0.5, (syc(0)+syc(4))*0.5, '.', $
           charsize=1.0*Abs(sin(elat(0)*d2rad)), alignment=0.5, Color=254
;          charsize=0.6*Abs(sin(elat(0)*d2rad)), alignment=0.5, Color=colrs(clrdep(i))
   endif

;;  Convert buoy locations and overlay on grid map
   openw, 15, 'S6GSBuoy.dat'
   printf,15, nnumb, 7,  Format="(2I8)"
;   nnumb = 33
;  buoids = LONARR(nnumb)
   buosxc = FLTARR(nnumb)
   buosyc = FLTARR(nnumb)
   slat = buolat
   slon = buolon

   LL2EqDeg, slat, slon, elat, elon, PoLat=Plat, PoLon=Plon

   buelat = elat
   buelon = elon

  for i=0, nnumb-1 do BEGIN
   if(elat(i) ge 0.0) then BEGIN
     pradmp=PRadus*cos(elat(i)*d2rad)/(np - 1.0 + sin(elat(i)*d2rad))
     syc = pradmp*cos(elon(i)*d2rad)
     sxc =-pradmp*sin(elon(i)*d2rad)
;; Mark buoy position on map
     txtsiz=Abs(sin(elat(i)*d2rad))
     xyouts, sxc, syc, '.', charsize=4.0*txtsiz, alignment=0.5, Color=234
     xyouts, sxc, syc, 'Y', charsize=2.0*txtsiz, alignment=0.5, Color=234
   endif else BEGIN
     pradmp=PRadus*cos(elat(i)*d2rad)/(np - 1.0 - sin(elat(i)*d2rad))
     syc = pradmp*cos(elon(i)*d2rad)
     sxc = pradmp*sin(elon(i)*d2rad)
   endelse

     buosxc(i)=sxc
     buosyc(i)=syc

     printf,15, buoids(i), slat(i), slon(i), elat(i), elon(i), sxc, syc,  Format="(I8,4F9.3,2F9.4)"

  endfor
  
  close, 15


;  Southern hemisphere

   plot, xcirc, ycirc, xrange=[-10,10], yrange=[-10,10],  $
       xstyle=4, ystyle=4, thick=2, $
       xmargin=[1,2], ymargin=[1,1]

;; Exclude last cell, the North Polar Cell
 for i=0L,nc-2L do BEGIN

   seq=seqll(*,i)

   if(seq(0) lt 0) then Begin
   sxc=sxcel(*,i)
   syc=sycel(*,i)

;  oplot, sxc, syc, Color=0
   oplot, sxc, syc, Color=colrs(clrdep(i))
   xyouts, (sxc(0)+sxc(2))*0.5, (syc(0)+syc(2))*0.5, '.', $
           charsize=0.01*cel(3,i)*Abs(sin(elat(0)*d2rad)), alignment=0.5, Color=colrs(clrdep(i))
   endif
 endfor

;; Mark buoy position on map if visible
 for i=0, nnumb-1 do BEGIN
   if(buelat(i) lt 0.0) then BEGIN
     txtsiz=Abs(sin(buelat(i)*d2rad))
     xyouts, buosxc(i), buosyc(i), '.', charsize=4.0*txtsiz, alignment=0.5, Color=234
     xyouts, buosxc(i), buosyc(i), 'Y', charsize=2.0*txtsiz, alignment=0.5, Color=234
   endif
 endfor

;;  Put cell information inside plot
   xyouts, -10.2, -10.0, '!5Global SMC 6-25 km Grid',  $
           Alignment=0.5, Color=0,   Charthick=2,CharSize=1.5
   xyouts, -10.2, - 9.0, 'NC='+string(nc, Format='(I7)'),$
           Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
   xyouts, -10.2, - 8.4, 'N1='+string(n1, Format='(I7)'),$
           Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
   xyouts, -10.2, - 7.8, 'N2='+string(n2, Format='(I7)'),$
           Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
   xyouts, -10.2, - 7.2, 'N4='+string(n4, Format='(I7)'),$
           Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
;  xyouts, -10.2, - 6.6, 'N8='+string(n8, Format='(I5)'),$
;          Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
;  xyouts, -10.2, - 6.0, 'N16='+string(n9-169, Format='(I4)'),$
;          Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
;  xyouts, -10.2, - 5.4, 'N32='+string(128, Format='(I4)'),$
;          Alignment=0.5, Color=254, Charthick=2,CharSize=1.3

;  xyouts, -10.2,   5.8, 'N64='+string(32, Format='(I3)'),$
;          Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
;  xyouts, -10.2,   6.4, 'N128='+string(8, Format='(I2)'),$
;          Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
;  xyouts, -10.2,   7.0, 'NPol='+string(1, Format='(I2)'),$
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

