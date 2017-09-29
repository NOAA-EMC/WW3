;;################## For SWH plot in Arctic region            #####
;;################## Read in date, swh_yymmddhh.txt, cells    #####
;;################## Output swhAc_yymmddhh.eps               #####
;;##################                                          #####
;;################## First created:  1 Mar 2005, Jian-Guo Li  #####
;;################## Last modified: 16 Aug 2017, Jian-Guo Li  #####
;;##################                                          #####
;;
;; 
;;  Path of the cell projection files
 Celdir='Your/CellArray/diectory/'
 wrkdir='Your/working/directory/'
 Datdir='Your/model/text_out/directory/'

 ng=0L
 n1=0L
 n2=0L
 n4=0L
 n8=0L
 n9=0L

 openr, 9, Celdir+'A50SMCels.dat'
 readf, 9, ng, n1, n2, n4 
 cel=intarr(5,ng)
 readf, 9, cel
 close, 9

 Print, ' A50SMCels read done'
;Print, nc, n9, n8, n4, n2, n1
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
 openr, 8, Celdir+'G50SMCBAr.dat'
 readf, 8, na, nlb, nrc
 ael=intarr(5,na)
 readf, 8, ael
 close, 8

 Print, ' G50SMCBAr read done'
 Print, na, nlb, nrc
 Print, cel(*,0)
 Print, cel(*,na-1)

;;  Merge them together
 nc=ng + na
 n4=n4 + na
 Print, nc, n1, n2, n4 

 cel=transpose([transpose(cel), transpose(ael)])

;;  Read in SMC6-25 cell equatorial lat lon array
 ncc=0L
 n2=0
 openr, 9, Celdir+'A50rcLaL.dat'
 readf, 9, ncc, n9
 elalon=fltarr(n9,ncc)
 readf, 9, elalon
 close, 9

 Print, ' A50rcLaL read done'
 Print, ncc, n9
 Print, elalon(*,0)
 Print, elalon(*,ncc-1)
 elat=transpose(elalon(0,*))

;; Read in cell trapezoid corner x y coordiantes
;; Note polar cell is an octogon with two lines sxc, syc
 ncx=0L
 n5=0
 openr, 8, Celdir+'A50rcSpX.dat'
 readf, 8, ncx, n5
 sxc=fltarr(n5,ncx+1)
 readf, 8, sxc
 close, 8

 ncy=0L
 n5=0
 openr, 7, Celdir+'A50rcSpY.dat'
 readf, 7, ncy, n5
 syc=fltarr(n5,ncy+1)
 readf, 7, syc
 close, 7

;; Check whether nc nb ncx and ncy are identical
 if( ncc ne ncx or ncc ne ncy or ncc ne nc ) then begin
     print, ' nc ncc ncx or ncy not matching', nc, ncc, ncx, ncy
     exit
 endif
 Print, ' sxc syc read with ncx ncy equal to nc', nc, ncc, ncx, ncy
 Print, sxc(*,nc-1)
 Print, syc(*,nc-1)


;; Define projection parameters
 d2rad=!Pi/180.0
 reducd=10.0
 radius=reducd/cos(66.0*d2rad)

;; Outline circle at equator
 ciran=findgen(1081)*d2rad/3.0
 xcirc=radius*cos(ciran)
 ycirc=radius*sin(ciran)

;; Corresponding lat-lon grid numbers
;nx=4097
;ny=3073
;nq=1536
;;  Note full grid from south to north poles requires 3072*dlat
;dxlon=0.087890635D
;dylat=0.05859375D
;xlon=(Dindgen(nx))*dxlon
;ylat=(Dindgen(ny)-1536.0D)*dylat
;;  Equator is now at ylat(nq)=0.0 

;; Color levels to be used
  mclr=255
  colorlevls=[251,252,253,254, indgen(mclr-4) ]

;; Wave height on logarithm scale
  nwht=6
  waveheight=[0,1,2,4,8,16,32]
  factor=254.0/alog(35.0)
  residu=exp(5.0/factor)
  resmn1=residu - 1.0
  marks=nint( factor*alog(waveheight+residu) )
  Print, 'factor and residu =', factor, residu

;; Key sympol as filled vertical tangular
 ysymb=[0,0,1,1,0]
 xsymb=[0,8,8,0,0]
 usersym, xsymb, ysymb, /fill

 ykeys=[findgen(mclr)/float(mclr-1)-0.50]*8.0-5.5 
 xkeys= findgen(mclr)*0.0-11.0


;;*************** Set device PS and load colours *******************
  SET_PLOT, 'PS'
; RESTORE_COLOURS,"/home/h05/frjl/PVWave/palettes/linuxspectrum.clr"
  LoadCT, 6
;; Or check available colour table by issuing LoadCT alone and select a number.
;; Set back and fore ground color to be white and black, respectively
    tvlct, 255, 255, 255, !P.Background
    tvlct,   0,   0,   0, !P.Color
  PRINT,' !P.Background=',!P.Background
  PRINT,' !P.Color=     ',!P.Color

;; select all outputs
  mhr=240
  nhr=[00,03,06,09,12,15,18,21]
  ndy=(indgen(31)+1)*100
  yymm='1209' 

 fdate=strarr(1)
status=dc_read_free(Wrkdir+'fdate',fdate)
  yymm=STRMID(fdate,0,4)
 print, "yymm set as "+yymm
  str=STRMID(fdate,4,2) 
  snd=STRMID(fdate,6,2)
  mstr=FIX( str(0) )
  nend=FIX( snd(0) )
 print, "Start and end day ", mstr, nend

;for nn=0, 30 do begin
 for nn=mstr-1, nend-1 do begin
 for mm=0, 7, 2  do begin

     nt=ndy(nn)+nhr(mm)
    fnt=yymm+string(nt, Format="(I4.4)")
  dfile=Datdir+'ww3.'+fnt+'.hs'

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
 txt='20'+fnt+'00'
 thr=string(nhr(mm), Format="(I2.2,'00 hr')")

;; Range excluding missing data values
   indx=where( cnf LT 1.0E20, mcount)
   cmin=min(cnf(indx))
   cmax=max(cnf(indx))
   print, dfile+' range ', cmin, cmax
   cmxs='Hs!Dmax!N='+string(cmax, Format="(F5.2)")+' m'
   cnxs='Hs!Dmin!N='+string(cmin, Format="(E10.3)")

;; filtering cnf for large values during test
   indx=where( cnf LT -0.01, mcount)
   if( mcount GT 0 ) then BEGIN
     Print, " Negative value cells =", mcount
     Print, indx
     cnf(indx)= -0.5*resmn1
   endif
   indx=where( cnf GT 1.0E20, mcount)
   if( mcount GT 0 ) then BEGIN
     Print, " Miss data cells =", mcount
     cnf(indx)= -resmn1
   endif
   indx=where( cnf GT 13.0, mcount)
   if( mcount GT 0 ) then BEGIN
     Print, " Large value cells =", mcount
     Print, indx
   endif

;;  Convert cnf with logarithm scale.
     icnf=nint( factor*alog(cnf+residu) )

;pfile= wrkdir+'swhAc'+fnt+'.eps'
 pfile= wrkdir+'swhAc'+fnt+'.ps'

 DEVICE, /Portrait, Bits_Per_Pixel=8, /Color, File=pfile, $
;DEVICE, /Encaps, /Portrait, Bits_Per_Pixel=8, /Color, File=pfile, $
;        YSize=16.0, XSize=16.0, xoffset=1.0, yoffset=1.0
         YSize=28.0, XSize=40.0, xoffset=1.0, yoffset=0.5

   plot, xcirc, ycirc, xrange=[-14,14], yrange=[-10,10],  $
       xstyle=1, ystyle=1, thick=2, Color=!P.Background,  $
       xmargin=[1,1], ymargin=[1,1], /Isotropic

 for i=0L,nc-2L do BEGIN
   if( (elat(i) ge 40.0) AND (abs(sxc(0,i)) lt 14.5) AND    $
                             (abs(syc(0,i)) lt 10.5) ) then Begin
;     if( i lt np )  then BEGIN
      if( (i lt np) and (i lt nc-na-nlb or i ge nc-na+nrc) )  then BEGIN
         if( icnf(i) gt 0) then  polyfill, sxc(*,i), syc(*,i), color=colorlevls(icnf(i))
         if( icnf(i) eq 0) then  BEGIN
;; Mark the arctical map-east reference region by first ring of its boundary cells at jmxglb - 3*4
     if(cel(1,i) eq jmxglb - 3) then polyfill, sxc(*,i), syc(*,i), Color=236
;; Mark the end of local-east reference region by last row of global part at jmxglb
     if(cel(1,i) eq jmxglb    ) then polyfill, sxc(*,i), syc(*,i), Color=136
                                         oplot, sxc(*,i), syc(*,i), color=0 
         endif
      endif 
;     endif else BEGIN
;; Arctic part beyond np, just show the grid.
;                                   oplot, sxc(*,i), syc(*,i), color=0 
;           endelse

   endif
 endfor

;; Polar cell ploted as a octogon shape with two lines of sxc syc.
   i=nc-1L
   if( nsea eq nc  AND  elalon(0,i) ge 0.0 ) then Begin
      if( icnf(i) gt 0) then  $ 
          polyfill, [sxc(*,i),sxc(*,i+1)], [syc(*,i),syc(*,i+1)], color=colorlevls(icnf(i))
      if( icnf(i) eq 0) then  $ 
             oplot, [sxc(*,i),sxc(*,i+1)], [syc(*,i),syc(*,i+1)], color=0
   endif


;;  Put swh max and date inside plot
   tpx= 2.0
   tpy=-2.3
   xyouts, tpx,  tpy-0.0, '!5A50SMC SWH',  $
           Alignment=0.0, Color=0,   Charthick=3,CharSize=1.5
   xyouts, tpx,  tpy-0.5, txt,  $
           Alignment=0.0, Color=254, Charthick=2,CharSize=1.3
   xyouts, tpx,  tpy-1.0, cmxs, $
           Alignment=0.0, Color=254, Charthick=2,CharSize=1.0
   xyouts, tpx,  tpy-1.5, cnxs, $
           Alignment=0.0, Color=254, Charthick=2,CharSize=1.0

;; Add a color key
 plots, xkeys, ykeys, psym=8, color=colorlevls, symsize=1.0
 plots, xkeys(marks), ykeys(marks), psym=8, color=colorlevls(marks), symsize=1.2
 for j=0,nwht do xyouts, xkeys(marks(j))+1.0, ykeys(marks(j))-0.05, $
         string(waveheight(j), Format='(I2)'), charsize=1.2, $
         charthick=2.0, color=0, alignment=0.0
;xyouts, xkeys(marks(3))-0.3, ykeys(marks(3))+0.0, '!5H!Ds!N (m)', Alignment=0.5, Color=0, $
 xyouts, xkeys(marks(3))+2.0, ykeys(marks(3))+0.0, '!5 Hs (m)', Alignment=0.5, Color=0, $
         Charthick=3,CharSize=1.5, Orientation=90

 DEVICE, /Close

ENDFOR
ENDFOR

print,' ### Finished a50smcswhs.pro ### '

END

