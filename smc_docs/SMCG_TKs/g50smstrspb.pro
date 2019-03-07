;;  Adapted from idl script for PVWave  12 Sept 2005
;;  It reads MFT model output files and plots contours of the
;;  tracer concentration in a ps file.
;;  Adapted for G25SMC wave propagation test.  JGLi 14 Oct 2010.
;;  Adapted for G50SMC wave propagation test.  JGLi 19 Aug 2010.
;;

;;  Read in GMC cell equatorial lat lon array
 nc=0L
 n2=0
 openr, 9, 'DatGMC/G50STLaL.dat'
 readf, 9, nc, n2
 elalon=fltarr(n2,nc)
 readf, 9, elalon
 close, 9

 Print, ' G50STLaL read done'
 Print, nc, n2
 Print, elalon(*,0)
 Print, elalon(*,nc-1)

;; Read in cell trapezoid corner x y coordiantes
;; Note polar cell is an octogon with two lines sxc, syc
 ncx=0L
 n5=0
 openr, 8, 'DatGMC/G50STSpX.dat'
 readf, 8, ncx, n5
 sxc=fltarr(n5,ncx+1)
 readf, 8, sxc
 close, 8

 ncy=0L
 n5=0
 openr, 7, 'DatGMC/G50STSpY.dat'
 readf, 7, ncy, n5
 syc=fltarr(n5,ncy+1)
 readf, 7, syc
 close, 7

;; Check whether nc ncx and ncy are identical
 if( nc ne ncx or nc ne ncy ) then begin
     print, ' nc ncx or ncy not matching', nc, ncx, ncy
     exit
 endif
 Print, ' sxc syc read with ncx ncy equal to nc', nc, ncx, ncy
 Print, sxc(*,nc-1)
 Print, syc(*,nc-1)

;; Sphere radius and conversion factor
 d2rad=!Pi/180.0
 radius=10.0

;; Color levels to be used
  mclr=255
  colorlevls=[251,252,253,254, indgen(mclr-4) ]
  waveheight=[findgen(mclr)*0.025-0.1]
  marks=indgen(7)*40+4

;; Key sympol as filled vertical tangular
 xsymb=[0,0,.6,.6,0]
 ysymb=[0,6, 6, 0,0]
 usersym, xsymb, ysymb, /fill

 xkeys=[findgen(mclr)/float(mclr-1)-0.51]*1.93*radius
 ykeys= findgen(mclr)*0.0-radius*1.16

;; Outline circle at equator
 ciran=findgen(1081)*d2rad/3.0
 xcirc=radius*cos(ciran)
 ycirc=radius*sin(ciran)

;;  Define spectral direction
    ndir=36
    theta=findgen(ndir)*!Pi*2.0/ndir

;; Add a spectral array plots for the Northern stripe 
  x0= 3.2
  y0= 5.3
  t0=!Pi*0.25
  cs=cos(theta + t0)
  xn=theta*0.0+x0
  yn=theta*0.0+y0
  for i=0,ndir-1 do BEGIN
      if (cs(i) GT 0.0) then BEGIN
         spc=1.2*cs(i)*cs(i)
         xn(i)=x0+spc*cos(theta(i))
         yn(i)=y0+spc*sin(theta(i))
      endif
  endfor

;; Add another spectral array plots for the Southern stripe 
  x1=-2.3
  y1=-8.0
  t1=!Pi*0.25
  cs=cos(theta - t1)
  xs=theta*0.0+x1
  ys=theta*0.0+y1
  for i=0,ndir-1 do BEGIN
      if (cs(i) GT 0.0) then BEGIN
         spc=1.2*cs(i)*cs(i)
         xs(i)=x1+spc*cos(theta(i))
         ys(i)=y1+spc*sin(theta(i))
      endif
  endfor

;;  Polar disk spectral array uses the Southern one but new location
  x2= 3.2
  y2= 7.8

;;  Atlantic disk spectral array uses the Southern one but new location
  xt=-1.0
  yt=-1.0

;; Read in cell concentration data file
 dfile='Cn10000.d'

;FileAgain: Print, ' Another file ? '

;Print, ' Please enter the data file for plot: 999 to exit'
;read, dfile
;if ( dfile eq '999' ) then exit

;; Default file or given file for plot
;dflen=strlen(dfile)
;if ( dflen lt 2 ) then  dfile='Cn10000.d'
;Print, ' Input date file will be ', dfile

;; select all outputs
;nss=[indgen(36)*40, indgen(13)*120+36*40]
;nss=[0,40,80,360,720,1440]
;nss=[120,indgen(37)*240]
 mhr=40

;nss=[mhr/4,indgen(20)*mhr/2]
;for nn=0, 20 do begin
;nss=[mhr/2,indgen(38)*mhr]
;for nn=0, 38 do begin

 ns1=[mhr/4,indgen(21)*mhr/2]
 ns2=[(11+indgen(38))*mhr]
 nss=[ns1, ns2]

 for nn=0, 41 do begin
;for nn=25, 36 do begin

 nt=nss(nn)

 fnt=string(nt+10000, Format="(I5)")
 dfile='Sp36RGD/Cn'+fnt+'.d'
;dfile='Cn'+fnt+'.d'

 openr, 9, dfile
 mt=0L
 mf=0L
 readf, 9, mt, mf
 PRINT, ' nt nf =', mt, mf
 cnf=fltarr(mf)
 readf, 9, cnf
 close, 9

;;  Convert time step for output file
 txt='NTS ='+string(mt, Format="(I6)")
 thr='T ='+string(Float(mt)*10/mhr, Format="(F6.2,' hr')")
;fnt=string(nt+10000, Format="(I5)")

   cmin=min(cnf)
   cmax=max(cnf)

;; filtering cnf for large values during test
   indx=where( cnf GT 6.0, mcount)
   if( mcount GT 0 ) then BEGIN
     Print, " Large value number =", mcount
     cnf(indx)=6.0
   endif
   indx=where( cnf LT -0.1, mcount)
   if( mcount GT 0 ) then BEGIN
     Print, " Negative value number =", mcount
     cnf(indx)=-0.1
   endif

 print, dfile+' range ', cmin, cmax
 cmxt='C!Dmax!N='+string(cmax, Format="(E11.3)")
 cnxt='C!Dmin!N='+string(cmin, Format="(E11.3)")
 cmxs='C!Dmx!N='+string(cmax, Format="(E10.3)")
 cnxs='C!Dmn!N='+string(cmin, Format="(E10.3)")
;cpos='C!Dpos!N='

;;  Convert (cnf+0.1)*40.0 as nearest integer for 0.025 interval
     icnf=nint(40.0*(cnf+0.1))

;PR,/ps,/land,/color,file="spb"+fnt+".ps", $
;  xsize=23.0, ysize=41.0, xoffset=2.5, yoffset=1.2

 SET_PLOT, 'PS'
 DEVICE, /Portrait, Bits_Per_Pixel=8, /Color, File="str"+fnt+".ps",  $
         YSize=23.0, XSize=40.0, xoffset=1.0, yoffset=3.0

;;*************** Set colours                *******************
  RESTORE_COLOURS,"/home/h05/frjl/PVWave/palettes/linuxspectrum.clr"

;; Set back and fore ground color to be white and black, respectively
    tvlct, 255, 255, 255,  !P.Background
; PRINT, ' !P.Background=',!P.Background
    tvlct,  0,  0,  0, !P.Color
; PRINT, ' !P.Color=', !P.Color

   !P.Multi = [0, 2, 0, 0, 0]

;  Western hemisphere

   plot, xcirc, ycirc, xrange=[-10,10], yrange=[-10,10],  $
       xstyle=4, ystyle=4, $
       xmargin=[2,1], ymargin=[6,2]
;      title=' GMC Grid North NC='+string(nc, Format='(I6)')

 for i=0L,nc-2 do BEGIN
;for i=0L,nc-1 do BEGIN
   if(elalon(0,i) ge 0.0) then Begin
      if( icnf(i) ne 4) then  polyfill, sxc(*,i), syc(*,i), color=colorlevls(icnf(i))
      if( icnf(i) eq 4) then     oplot, sxc(*,i), syc(*,i), color=colorlevls(icnf(i))
   endif
 endfor

;;  Mark postive overshoots
;    cpos=' '
;    mm=0
;for i=0L,nc-2 do BEGIN
;     if( cnf(i) gt 5.001 AND elalon(0,i) ge 0.0 ) then  BEGIN
;        mm=mm+1
;        xyouts, sxc(1,i), syc(1,i), strcompress(string(mm),/remove_all), color=colorlevls(250), $
;                CharSize=1.0, CharThick=2.0, Alignment=0.0
;        cpos=cpos+string(cnf(i), Format="(F6.3)")
;        Print, i, cnf(i)
;     endif
;endfor

;; Polar cell ploted as a octogon shape with two lines of sxc syc.
     i=nc-1
   if(elalon(0,i) ge 0.0) then Begin
      polyfill, [sxc(*,i),sxc(*,i+1)], [syc(*,i),syc(*,i+1)], color=colorlevls(icnf(i))
   endif

;; Add time step and cmax and cmin values
;xyouts,-0.8,0,  txt, CharSize=1.5, CharThick=5.0, Color=0
;xyouts,-1, -1, cmxt, CharSize=1.5, CharThick=3.0, Color=230
;xyouts,-1, -2, cnxt, CharSize=1.5, CharThick=3.0, Color=23
;xyouts,-10.0, 10.0, cpos, CharSize=1.0, CharThick=2.0, Alignment=0.0, Color=230

;; Add a color key
 plots, xkeys, ykeys, psym=8, color=colorlevls, symsize=1.3
 plots, xkeys(marks), ykeys(marks), psym=8, color=colorlevls(marks), symsize=1.5
 for j=4,mclr-10,40 do xyouts, xkeys(j), ykeys(j)+1.1, $
         string(fix(waveheight(j)), Format='(I1)'), charsize=1.0, $
         charthick=2.0, color=0, alignment=0.1

;; Add a spectral array plots for the northern region
  for i=0,ndir-1 do oplot, [x0, xn(i)], [y0, yn(i)], thick=2, color=236

;; Add a spectral array plots for the Polar disk but use the southern one
  for i=0,ndir-1 do oplot, [x2, x2-x1+xs(i)], [y2, y2-y1+ys(i)], thick=2, color=236

;; Add a spectral array plots for the Atlantic disk with modified southern one
  for i=0,ndir-1 do oplot, [xt, xt-x1+xs(i)], [yt, yt-y1+ys(i)], thick=2, color=236

;  Eastern hemisphere

   plot, xcirc, ycirc, xrange=[-10,10], yrange=[-10,10],  $
       xstyle=4, ystyle=4, $
       xmargin=[1,2], ymargin=[6,2]
;      title=' GMC Grid South N2='+string(n2, Format='(I5)')

 for i=0L,nc-2 do BEGIN
   if(elalon(0,i) lt 0.0) then Begin
      if( icnf(i) ne 4) then  polyfill, sxc(*,i), syc(*,i), color=colorlevls(icnf(i))
      if( icnf(i) eq 4) then     oplot, sxc(*,i), syc(*,i), color=colorlevls(icnf(i))
   endif
 endfor

;cpos=' '
;mm=0
;for i=0L,nc-2 do BEGIN
;     if( cnf(i) gt 5.001 AND elalon(0,i) lt 0.0 ) then  BEGIN
;        mm=mm+1
;        xyouts, sxc(1,i), syc(1,i), strcompress(string(mm),/remove_all), color=colorlevls(250), $
;                CharSize=1.0, CharThick=2.0, Alignment=0.0
;        cpos=cpos+string(cnf(i), Format="(F6.3)")
;        Print, i, cnf(i)
;     endif
;endfor

;; Add time and max/min values on eastern hemisphere
 xyouts,-10.0,  8.0,'G50RfrGctDif',CharSize=2.0, CharThick=5.0, Alignment=0.5, Color=0
 xyouts,-10.0, -7.0,'UNO2',CharSize=2.0, CharThick=5.0, Alignment=0.5, Color=0
 xyouts,-10.0, -8.0,  txt, CharSize=1.5, CharThick=3.0, Alignment=0.5, Color=0
 xyouts,-10.0, -8.6,  thr, CharSize=1.5, CharThick=3.0, Alignment=0.5, Color=0
 xyouts,-10.0, -9.2, cmxs, CharSize=1.3, CharThick=2.0, Alignment=0.5, Color=230
 xyouts,-10.0, -9.8, cnxs, CharSize=1.3, CharThick=2.0, Alignment=0.5, Color=23

;xyouts,-10.0, 10.0, cpos, CharSize=1.0, CharThick=2.0, Alignment=0.0, Color=230

;; Add a color key
 plots, xkeys, ykeys, psym=8, color=colorlevls, symsize=1.3
 plots, xkeys(marks), ykeys(marks), psym=8, color=colorlevls(marks), symsize=1.5
 for j=4,mclr-10,40 do xyouts, xkeys(j), ykeys(j)+1.1, $
         string(fix(waveheight(j)), Format='(I1)'), charsize=1.0, $
         charthick=2.0, color=0, alignment=0.1

;; Add a spectral array plots for the southern region
  for i=0,ndir-1 do oplot, [x1, xs(i)], [y1, ys(i)], thick=2, color=236

;PREND,'B2_CL13',/noprint,/view,/keep

 DEVICE, /Close

 endfor

;GOTO, FileAgain

 END

