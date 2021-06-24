;;  Adapted from idl script for PVWave  12 Sept 2005
;;  It reads MFT model output files and plots contours of the
;;  tracer concentration in a ps file.
;;
;; First created: For 4 views     J G Li   26 Nov 2008
;; Last modified: Color grids     J G Li   26 Aug 2009
;; Adapted for 25km SMC grids     J G Li    3 Feb 2010
;; Adapted for 6-25km SMC grids   J G Li   18 Feb 2010
;; Produce eps format output.     J G Li   16 Sep 2010
;; New SMC625 full grid.          J G Li   15 Dec 2011
;; Wave model cell count line.    J G Li    5 Oct 2012
;; Full Arctic part cells.        J G Li   16 Jan 2014
;; Adapted for G50SMC grid.       J G Li   12 Feb 2014
;; Simplify with readcell and steromap.  JGLi12Nov2014
;; Adapted for Arctic A50SMC grid.   JGLi16Aug2017
;

;;  Read global and Arctic part cells. 
  Cel_file = './A50SMCels.dat'
  Arc_file = './G50SMCBAr.dat'

  READCELL, Cel_File, cel, nc, ArcFile = Arc_file, NArc = na, NBAr = nb

;; Maximum j row number in Global part
  jmxglb = max( cel(1,nc-na-1L) )
  Print, ' Maximum j row =', jmxglb

;;  Degree to radian conversion parameter.
  d2rad=!Pi/180.0

;;  Maximum mapping radius.
  radius=10.0

;; Whole global projection angle from N Pole to be 90.0
;  pangle=90.0
;; Reduced projection angle from rotated N Pole
   pangle=26.5

;; Size-1 cell increments
 dxlon=0.703125D
 dylat=0.468750D
;dxlon=0.087890635D
;dylat=0.05859375D
;dxlon=0.3515625D
;dylat=0.2343750D

;; Outline circle at equator
 ciran=findgen(1081)*d2rad/3.0
 xcirc=radius*cos(ciran)
 ycirc=radius*sin(ciran)

 colorlevls=[(indgen(11)+1)*23]
 heights=[-0.1, 0.1, 0.5, 0.9, 1.1, 2.0, 3.0, 4.0, 4.9, 5.1, 6.0]

;; Lon and lat grid numbers + 1  for cell faces
 nx=513
 ny=385
;nx=4097
;ny=3073
;nq=1536
;;  Note full grid from south to north poles requires 3072*dlat

;;  Half the latitude grid to set j=0 on the Equator.
 jeqt=(ny-1)/2
 Print, 'Full and Half lat grid =', ny-1, jeqt

 xlon=(Dindgen(nx))*dxlon
 ylat=(Dindgen(ny)-jeqt)*dylat
;;  Equator is now at ylat(jeqt)=0.0 

;; Color levels to be used
  mclr=128
  colrs=[indgen(mclr)]
  deptx=['10000','1000','100','10','1']
  depth=[10000,1000,100,10,1]
  cdeps=TRANSPOSE( cel(4,*) )
  clrdep=FIX( (4.0-ALOG10(FLOAT(cdeps)))*31.5 )
  marks=FIX( (4.0-ALOG10(FLOAT(depth)))*31.5 )

;; Key sympol as filled vertical tangular
 ysymb=[0, 0, 1, 1, 0]
 xsymb=[0, 8, 8, 0, 0]
 usersym, xsymb, ysymb, /fill

 ykeys=[findgen(mclr)/float(mclr-1)-0.50]*8.0-5.5
 xkeys= findgen(mclr)*0.0-9.0 

;; New Pole as projection direction (central point)
 file4=[ '0',  '1',  '2',  '3' ]
 plon4=[-80.0,  90.0,  180.0,  330.0 ]
 plat4=[ 86.5,  30.0,   30.0,   30.0 ]

   kk= 0
 plon= plon4(kk)
 plat= plat4(kk)

;; Open two files to store converted quadrilaterals coordinates
;; They contain one loop of the 4 trapezoid corners (5 points)
;; x and y coornates (5 each) for each cell, respectively.
;; Equatorial lat lon are also stored to identify two hemispheres
 openw, 10, 'A50rcLaL.dat'
 openw, 11, 'A50rcSpX.dat'
 openw, 12, 'A50rcSpY.dat'
 printf,10, nc, 2,  Format="(2I8)"
 printf,11, nc, 5,  Format="(2I8)"
 printf,12, nc, 5,  Format="(2I8)"


 SET_PLOT, 'PS'
;DEVICE, /Encaps, /Portrait, Bits_Per_Pixel=8, /Color, File="a50smcgrd.eps",  $
 DEVICE,          /Portrait, Bits_Per_Pixel=8, /Color, File="a50smcgrd.ps",   $
;        YSize=26.0, XSize=40.0, xoffset=1.0, yoffset=1.0
         YSize=28.0, XSize=39.0, xoffset=1.0, yoffset=1.0

;;*************** Set colours                *******************
  RESTORE_COLOURS,"/home/h05/frjl/PVWave/palettes/linuxspectrum.clr"

;; Set back and fore ground color to be white and black, respectively
    tvlct, 255, 255, 255,  !P.Background
; PRINT, ' !P.Background=',!P.Background
    tvlct,  0,  0,  0, !P.Color
; PRINT, ' !P.Color=', !P.Color

;***************  *******************  *******************
  !p.charthick = 2  &  !p.charsize = 1.0

;  !P.Multi = [0, 2, 0, 0, 0]

;  Northern hemisphere

   plot, xcirc, ycirc, xrange=[-14,14], yrange=[-10,10],  $
       xstyle=1, ystyle=1, thick=2, Color=!P.Background, $
       xmargin=[1,1], ymargin=[1,1], /Isotropic 

;  plot, xcirc, ycirc, xrange=[-10,10], yrange=[-10,10],  $
;      xstyle=4, ystyle=4, thick=2, $
;      xmargin=[2,1], ymargin=[1,1]

;; Exclude last cell, the North Polar Cell
 for i=0L,nc-2 do BEGIN

   xc=[cel(0,i),cel(0,i)+cel(2,i),cel(0,i)+cel(2,i),cel(0,i),cel(0,i)]
   yc=[cel(1,i),cel(1,i),cel(3,i)+cel(1,i),cel(3,i)+cel(1,i),cel(1,i)]
   slat=ylat(yc+jeqt)
   slon=xlon(xc)

;; Convert slat slon to elat elon with given new pole
   STEROMAP, slat, slon, elat, elon, sxc, syc, $ 
             Polat=plat, Polon=plon, Pangl=pangle, /Onecl

   seq=[elat(0), elon(0)]

;; Store converted x and y values
   printf,10, seq,  Format="(2F9.3)"
   printf,11, sxc,  Format="(5F8.3)"
   printf,12, syc,  Format="(5F8.3)"

   if( (elat(0) ge 40.0) AND (abs(sxc(0)) lt 15.0) AND    $
                             (abs(syc(0)) lt 11.0) ) then Begin
;  oplot, sxc, syc, Color=0
;; Mark the arctical map-east reference region by first ring of its boundary cells at jmxglb - 3*4
     if(cel(1,i) eq jmxglb - 3) then polyfill, sxc, syc, Color=236
;; Mark the end of local-east reference region by last row of global part at jmxglb
     if(cel(1,i) eq jmxglb    ) then polyfill, sxc, syc, Color=136
;; Plot outline of cells at depth color and a dot to mark the sea point.
   oplot, sxc, syc, Color=colrs(clrdep(i))
   xyouts, (sxc(0)+sxc(2))*0.5, (syc(0)+syc(2))*0.5, '.', $
           charsize=0.05*cel(3,i)*Abs(sin(elat(0)*d2rad)), alignment=0.5, Color=236
;          charsize=0.01*cel(3,i)*Abs(sin(elat(0)*d2rad)), alignment=0.5, Color=colrs(clrdep(i))
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

   STEROMAP, slat, slon, elat, elon, sxc, syc, $ 
             Polat=plat, Polon=plon, Pangl=pangle, /Onecl

   seq=[elat(0), elon(0)]

;; Store converted x and y values
   printf,10, seq,  Format="(2F9.3)"
   printf,11, sxc(0:4),  Format="(5F8.3)"
   printf,12, syc(0:4),  Format="(5F8.3)"
   printf,11, sxc(4:8),  Format="(5F8.3)"
   printf,12, syc(4:8),  Format="(5F8.3)"


   if(elat(0) ge 49.0) then Begin
   oplot, sxc, syc, Color=colrs(clrdep(i))
   xyouts, (sxc(0)+sxc(4))*0.5, (syc(0)+syc(4))*0.5, '.', $
           charsize=1.0*Abs(sin(elat(0)*d2rad)), alignment=0.5, Color=254
;          charsize=0.6*Abs(sin(elat(0)*d2rad)), alignment=0.5, Color=colrs(clrdep(i))
   endif


;;  Put cell information inside plot
   xyouts,   3.6, -4.1, '!5 A50SMC Grid',  $
           Alignment=0.5, Color=0,   Charthick=2,CharSize=1.5
   xyouts,   3.6, -3.5, 'NC='+string(nc, Format='(I7)'),$
           Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
   xyouts,   3.6, -3.0, 'NA='+string(na, Format='(I7)'),$
           Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
   xyouts,   3.6, -2.5, 'NB='+string(nb, Format='(I7)'),$
           Alignment=0.5, Color=254, Charthick=2,CharSize=1.3
;  xyouts,   3.6, -2.0, 'N1='+string(n1, Format='(I7)'),$
;          Alignment=0.5, Color=254, Charthick=2,CharSize=1.3

;; Add a color key
 plots, xkeys, ykeys, psym=8, color=colrs, symsize=1.0
 plots, xkeys(marks), ykeys(marks), psym=8, color=colrs(marks), symsize=1.2
 for j=0,4 do xyouts, xkeys(marks(j))+1.0, ykeys(marks(j))-0.05, $
         deptx(j), charsize=1.0, charthick=2.0, color=0, alignment=0.0 
 xyouts, xkeys(marks(1))+2.5, ykeys(marks(1))+0.0, 'Depth (m)', Alignment=0.5, Color=0, $
         Charthick=3,CharSize=1.5, Orientation=90

;PREND,'B2_CL13',/noprint,/view,/keep

;; End of kk loop
;; Endfor

 DEVICE, /Close

 end

