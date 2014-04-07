;;  Adapted from idl script for PVWave  12 Sept 2005
;;  It reads MFT model output files and plots contours of the
;;  tracer concentration in a ps file.
;;  S4R sterographic projection.  JGLi30Jun2011
;;  SMC625 global part only.      JGLi12Dec2011
;;  SMC625 SWH plot from WW3 text files.  JGLi16Feb2012
;;  SMC625 30 Frequency bin SWH plot from SMC625Tx.  JGLi02Oct2012
;;  SMC6125 grid swh plot from Vn4 SMC6125Tx.  JGLi10Jan2013
;;  Input yymm from file.                      JGLi23Oct2013
;;  Adapted for G50SMC grid swh.               JGLi19Nov2013
;;  Extended to include Arctic.                JGLi11Feb2014
;;

;;  Path of the cell projection files
 DatGMC='/data/cr2/frjl/MFTModel/G50SMC/DatGMC/'
 Wrkdir='/data/cr2/frjl/MFTModel/G50SMC/' 

; Read in SMC cell arrays 
 ng=0L
 n1=0L
 n2=0L
 n4=0L
 openr, 9, DatGMC+'G50SMCels.dat'
 readf, 9, ng, n1, n2, n4 
 cel=intarr(5,ng)
 readf, 9, cel
 close, 9

 Print, ' G50SMCels read done'
 Print, ng, n1, n2, n4 
 Print, cel(*,0)
 Print, cel(*,ng-1)

;; Maximum j row number in Global part
 jmxglb = max( cel(1,*) )
 Print, ' Maximum j row =', jmxglb

;;  Arctic part from SMC625Arcl.dat
 na=0L
 nlb=0L
 nrc=0L
 openr, 8, DatGMC+'G50SMCBAr.dat'
 readf, 8, na, nlb, nrc
 ael=intarr(5,na)
 readf, 8, ael
 close, 8

 Print, ' SMC625BArc read done'
 Print, na, nlb, nrc
 Print, cel(*,0)
 Print, cel(*,na-1)

;;  Merge them together
 nga=ng + na
 Print, nga 

 cel=transpose([transpose(cel), transpose(ael)])


; Read in SMC cell equatorial lat lon array
 nc=0L
 n2=0
 openr, 9, DatGMC+'G50STLaL.dat'
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
 openr, 8, DatGMC+'G50STSpX.dat'
 readf, 8, ncx, n5
 sxc=fltarr(n5,ncx+1)
 readf, 8, sxc
 close, 8

 ncy=0L
 n5=0
 openr, 7, DatGMC+'G50STSpY.dat'
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
; waveheight=[findgen(mclr)*0.08 - 0.4]
; marks=indgen(10)*25+5
  nwht=6
  waveheight=[0,1,2,4,8,16,32]
  factor=254.0/alog(35.0)
  residu=exp(5.0/factor)
  resmn1=residu - 1.0
  marks=nint( factor*alog(waveheight+residu) )
  Print, 'factor and residu =', factor, residu

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

;; Read in cell concentration data file
 dfile='swh_10093018.dat'


;;*************** Set colours and PS format  *******************
  SET_PLOT, 'PS'
  RESTORE_COLOURS,"/home/h05/frjl/PVWave/palettes/linuxspectrum.clr"

;; Set back and fore ground color to be white and black, respectively
    tvlct, 255, 255, 255,  !P.Background
  PRINT, ' !P.Background=',!P.Background
    tvlct,  0,  0,  0, !P.Color
  PRINT, ' !P.Color=', !P.Color

;FileAgain: Print, ' Another file ? '
;Print, ' Please enter the data file for plot: 999 to exit'
;read, dfile
;if ( dfile eq '999' ) then exit

;; Default file or given file for plot
;dflen=strlen(dfile)
;if ( dflen lt 2 ) then  dfile='Cn10000.d'
;Print, ' Input date file will be ', dfile

;; select all outputs
 mhr=240
 nhr=[00,03,06,09,12,15,18,21]
 ndy=(indgen(31)+1)*100
 yymm='1209'

;; Read start and end date from fdate file
;; 12090109 
 fdate=strarr(1)
status=dc_read_free(Wrkdir+'fdate',fdate)
  yymm=STRMID(fdate,0,4)
 print, "yymm set as "+yymm
  str=STRMID(fdate,4,2) 
  snd=STRMID(fdate,6,2)
  mstr=FIX( str(0) )
  nend=FIX( snd(0) )
 print, "Start and end day ", mstr, nend

;for nn= 0, 30 do begin
 for nn=mstr-1, nend-1 do begin
 for mm=0, 7, 2   do begin

     nt=ndy(nn)+nhr(mm)
    fnt=yymm+string(nt, Format="(I4.4)")
  dfile='/data/cr2/frjl/G50SMCTx/ww3.'+fnt+'.hs'

 openr, 9, dfile
   fileid='WAVEWATCH III'
 yyyymmdd='20100930'
   hhmmss='230000'
     nsea= 520014L

 readf, 9, format='(30X,I9)', nsea
 PRINT, nsea
 cnf=fltarr(nsea)
 readf, 9, cnf
 close, 9

;;  Skip Arctic part if nsea < nc
   np=nc-1L
   if( nsea lt nc ) then Begin
       np = nsea
   endif
   PRINT, ' Plotting cell number np/nc =', np, nc

;;  Convert time step for output file
 txt='20'+fnt
 thr=string(nhr(mm), Format="(I2.2,'00 hr')")

;; Range excluding missing data values
;  indx=where( cnf LT 1.0E20, mcount)
;; Version 4 missing data indicator is -999.00
   indx=where( cnf GT -1.0E2, mcount)
   cmin=min(cnf(indx))
   cmax=max(cnf(indx))
   print, dfile+' range ', cmin, cmax
   cmxs='H!Dmx!N='+string(cmax, Format="(F6.2)")+' m'
   cnxs='H!Dmn!N='+string(cmin, Format="(E10.3)")

;; Reset missing data value
   indx=where( cnf LT -1.0E2, mcount)
   if( mcount GT 0 ) then BEGIN
     Print, " Miss data cells =", mcount
     cnf(indx)= -resmn1
   endif

;; filtering cnf for large values during test
   indx=where( cnf LT -0.01 AND cnf GT -resmn1, mcount)
   if( mcount GT 0 ) then BEGIN
     Print, " Negative value cells =", mcount
     Print, indx
     cnf(indx)= -0.5*resmn1
   endif
;  indx=where( cnf GT 1.0E20, mcount)
;  if( mcount GT 0 ) then BEGIN
;    Print, " Miss data cells =", mcount
;    cnf(indx)= -resmn1
;  endif
   indx=where( cnf GT 32.0, mcount)
   if( mcount GT 0 ) then BEGIN
     Print, " Large value cells =", mcount
     Print, indx
     cnf(indx)= 32.0   
   endif

;;  Convert cnf with logarithm scale.
     icnf=nint( factor*alog(cnf+residu) )


 DEVICE, /Portrait, Bits_Per_Pixel=8, /Color, File="swh_"+fnt+".ps",  $
         YSize=23.0, XSize=40.0, xoffset=1.0, yoffset=3.0

   !P.Multi = [0, 2, 0, 0, 0]

;  Western hemisphere

   plot, xcirc, ycirc, xrange=[-10,10], yrange=[-10,10],  $
       xstyle=4, ystyle=4, $
       xmargin=[2,1], ymargin=[6,2]

 for i=0L,np-1L do BEGIN
   if(elalon(0,i) ge 0.0) then Begin
      if( (i lt nc-na-nrc) or (i ge nc-na+nlb) )  then BEGIN
          if( icnf(i) gt 0) then  polyfill, sxc(*,i), syc(*,i), color=colorlevls(icnf(i))
          if( icnf(i) eq 0) then     oplot, sxc(*,i), syc(*,i), color=0 
      endif
   endif
 endfor

;; Fill the Arctic region with grid cells
 if( np lt nc-1L ) then Begin
 for i=np, nc-2L do BEGIN
   if(elalon(0,i) ge 0.0) then Begin
      oplot, sxc(*,i), syc(*,i), color=0 
   endif
 endfor
 endif

;; Polar cell ploted as a octogon shape with two lines of sxc syc.
   if( nsea eq nc  AND  elalon(0,i) ge 0.0 ) then Begin
      i=nc-1L
      if( icnf(i) gt 0) then  $ 
          polyfill, [sxc(*,i),sxc(*,i+1)], [syc(*,i),syc(*,i+1)], color=colorlevls(icnf(i))
      if( icnf(i) eq 0) then  $ 
             oplot, [sxc(*,i),sxc(*,i+1)], [syc(*,i),syc(*,i+1)], color=0
   endif

;; Add a color key
 plots, xkeys, ykeys, psym=8, color=colorlevls, symsize=1.3
 plots, xkeys(marks), ykeys(marks), psym=8, color=colorlevls(marks), symsize=1.5
;for j=5,mclr-10,25 do xyouts, xkeys(j), ykeys(j)+1.1, $
 for j=0,nwht do xyouts, xkeys(marks(j)), ykeys(marks(j))+1.1, $
         string(waveheight(j), Format='(I2)'), charsize=1.0, $
         charthick=2.0, color=0, alignment=0.5

;  Eastern hemisphere

   plot, xcirc, ycirc, xrange=[-10,10], yrange=[-10,10],  $
       xstyle=4, ystyle=4, $
       xmargin=[1,2], ymargin=[6,2]
;      title=' GMC Grid South N2='+string(n2, Format='(I5)')

;for i=0L,nc-2 do BEGIN
 for i=0L,np-1L do BEGIN
   if(elalon(0,i) lt 0.0) then Begin
      if( icnf(i) gt 0) then  polyfill, sxc(*,i), syc(*,i), color=colorlevls(icnf(i))
      if( icnf(i) eq 0) then     oplot, sxc(*,i), syc(*,i), color=0
   endif
 endfor

;; Add time and max/min values on eastern hemisphere
 xyouts,-10.0, +9.0,'G50SMC',CharSize=2.0, CharThick=5.0, Alignment=0.5, Color=0
 xyouts,-10.0, -7.0,'SWH m', CharSize=1.8, CharThick=4.0, Alignment=0.5, Color=0
 xyouts,-10.0, -8.0,  txt, CharSize=1.5, CharThick=3.0, Alignment=0.5, Color=240
 xyouts,-10.0, -9.0, cmxs, CharSize=1.3, CharThick=2.0, Alignment=0.5, Color=230
 xyouts,-10.0, -9.8, cnxs, CharSize=1.3, CharThick=2.0, Alignment=0.5, Color=23

;xyouts,-10.0, 10.0, cpos, CharSize=1.0, CharThick=2.0, Alignment=0.0, Color=230

;; Add a color key
 plots, xkeys, ykeys, psym=8, color=colorlevls, symsize=1.3
 plots, xkeys(marks), ykeys(marks), psym=8, color=colorlevls(marks), symsize=1.5
 for j=0,nwht do xyouts, xkeys(marks(j)), ykeys(marks(j))+1.1, $
         string(waveheight(j), Format='(I2)'), charsize=1.0, $
         charthick=2.0, color=0, alignment=0.5

 DEVICE, /Close

 endfor
 endfor

;GOTO, FileAgain

 END

