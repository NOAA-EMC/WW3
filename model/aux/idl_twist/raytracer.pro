PRO Raytracemenu, anterior_root
COMMON Wray,Wrline1,Wrline2,Wrline3,Wrline4,Wrline5,WRline6
COMMON RAYS,    raysOK,raystype,raynsteps,rayx,rayy,raya,rayamin,rayamax, $
                rayres,rayfreq,rayGP,raytimestep,rayoffdep,rayflag,raydz,raymindepth
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath


   raypath=''
   raybase = WIDGET_BASE(TITLE='Ray tracer', /COLUMN)

   WRline1=LONARR(10)
   WRline1(0)=WIDGET_BASE(raybase, /FRAME, /ROW)
   WRline1(1)=WIDGET_DROPLIST(raybase, $
      VALUE=['ALL RAYS','OFF.RAYS','TRAP.RAYS','NO RAYS'])

   WRline2=LONARR(10)
   WRline2(0)= WIDGET_BASE(raybase, /FRAME, /ROW)
   WRline2(1)= WIDGET_TEXT(WRline2(0), $
   YSIZE=1,XSIZE=6,VALUE=strcompress('Freq.:'))
   WRline2(2)= WIDGET_TEXT(WRline2(0), $
   XSIZE=8,YSIZE=1,/EDITABLE,VALUE=strcompress(STRING(rayfreq)))

   WRline3=LONARR(10)
   WRline3(0)= WIDGET_BASE(raybase, /FRAME, /ROW)
   WRline3(1)= WIDGET_LABEL(WRline3(0),VALUE='dt:')
   WRline3(2)= WIDGET_TEXT(WRline3(0), $
   XSIZE=6,YSIZE=1,/EDITABLE,VALUE=strcompress(STRING(raytimestep)))
   WRline3(3)= WIDGET_LABEL(WRline3(0),VALUE='dth:')
   WRline3(4)= WIDGET_TEXT(WRline3(0), $
   XSIZE=6,YSIZE=1,/EDITABLE,VALUE=strcompress(STRING(rayres)))

   WRline4=LONARR(10)
   WRline4(0)= WIDGET_BASE(raybase, /FRAME, /ROW)

   WRline4(1)= WIDGET_SLIDER(WRline4(0), MAXIMUM=180, MINIMUM=-180, $
      value=rayamin,TITLE='amin')
   WRline4(2)= WIDGET_SLIDER(WRline4(0), MAXIMUM=180, MINIMUM=-180, $
      value=180,TITLE='amax')

   WRline5=LONARR(10)
   WRline5(0)= WIDGET_BASE(raybase, /FRAME, /ROW)
   WRline5(1)= WIDGET_LABEL(WRline5(0),VALUE='Depths, max:')
   WRline5(2)= WIDGET_TEXT(WRline5(0), $
   XSIZE=6,YSIZE=1,/EDITABLE,VALUE=strcompress(STRING(rayoffdep)))
   WRline5(3)= WIDGET_LABEL(WRline5(0),VALUE='Mean:')
   WRline5(4)= WIDGET_TEXT(WRline5(0), $
   XSIZE=6,YSIZE=1,/EDITABLE,VALUE=strcompress(STRING(raydz)))
   WRline5(5)= WIDGET_LABEL(WRline5(0),VALUE='Min:')
   WRline5(6)= WIDGET_TEXT(WRline5(0), $
   XSIZE=6,YSIZE=1,/EDITABLE,VALUE=strcompress(STRING(raymindepth)))


   WRline6=LONARR(10)
   WRline6(0)= WIDGET_BASE(raybase, /FRAME, /ROW)
   WRline6(2)=WIDGET_BUTTON(WRline6(0),VALUE="Get")
   WRline6(3)=WIDGET_BUTTON(WRline6(0),VALUE="Get global")
   WRline6(4)=WIDGET_BUTTON(WRline6(0),VALUE="Forward")
   WRline6(5)=WIDGET_BUTTON(WRline6(0),VALUE="Forward with current")
   Line7 = WIDGET_BUTTON(raybase, VALUE='OK')

   WIDGET_CONTROL, /REALIZE, raybase
   XMANAGER, 'Raytracemenu', raybase
RETURN
END


;----------------------------------------------------------------------------
PRO Raytracemenu_event, ev
;** 1 ** Display parameters
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,filltype
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON POSTSCRIPT, filep,prcoul,psor,pstype, $
                pwinx,pwiny,papierx,papiery,xoffset,yoffset, $
                facpolice,pssizex,pssizey
COMMON TROISD,  Ax3D,Az3D,smoothing
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth

;** 2 ** Display and data variables/parameters
COMMON FREQ,    freq,nfband,findex
COMMON DIR,     nabin,nabint,aindex,angle
COMMON TIME,    tindex,ntime,dtime,time0,day0,dtindex,nstep
COMMON TRANSECT,Ntrans,Xtrans,Ytrans,Ztrans,I1trans,I2trans,alphatrans,betatrans
COMMON SPACE,   c_gp,c_cut
COMMON PLOTOP,  addir,adgpsym,adbathy,adcoast

;** 3 ** I/O and data variables
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON ARR8m,   nfTom,naTom,TomF,Tomth,OBS8m,MOD8m
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON BUOYPOS, nptrans,KS,depthbuoy,ETF,tabETF
COMMON COAST,   coastx,coasty
COMMON DEPTHSTAT,distd,nbinsd
COMMON DIRSPEC, ds,ndir,kdkdth
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON GRIDPar, th1,th2,gfact,gsize1,gsize6
COMMON GRIDSPEC,nspecgp,ispecgp,jspecgp,whichgrid,namespecgp,specfontsize
COMMON RAYS,    raysOK,raystype,raynsteps,rayx,rayy,raya,rayamin,rayamax, $
                rayres,rayfreq,rayGP,raytimestep,rayoffdep,rayflag,raydz,raymindepth
COMMON SOURCE,  S0,snabin,sntstep
COMMON STEADY,  ETFM,etfdepth,etffact

COMMON Wray,Wrline1,Wrline2,Wrline3,Wrline4,Wrline5,WRline6
COMMON WW3,     ww3model,ww3date,ww3time,ww3fieldname,ww3scale,ww3miss,ww3unit,ww3lon, $
               ww3lat,ww3matrix,ww3dir,ww3path,spec2D,ww3matrix2,ww3dir2,ncid,ncid2
;*******END OF COMMON BLOCKS*******************************
   type = TAG_NAMES(ev, /STRUCTURE)
;   IF ev.id EQ WGsl(4) THEN BEGIN
;      WIDGET_CONTROL, ev.id, GET_VALUE=value
;       gfact=value
;   ENDIF ELSE BEGIN
   CASE type OF
   'WIDGET_BUTTON': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE value OF
             'Get': BEGIN
                  raystype=1
                  GET_LUN,unit
                  OPENW,unit,raypath+'Ray3a.inpray'
                  PRINTF,unit,paths(2)+filenames(2)
                  PRINTF,unit,paths(3)+filenames(3)
                  nray=ROUND(ABS(rayamin-rayamax)/rayres+1)
                  PRINTF,unit,nray
                  PRINTF,unit,rayfreq,' ',rayres,rayamin,' ',raytimestep,' ',rayoffdep,' ',raymindepth
                  PRINTF,unit,raydz
                  PRINTF,unit,gridmat(0,c_gp-1)*1000./sx,gridmat(1,c_gp-1)*1000./sy
                  print,'Start point:',gridmat(0,c_gp-1)*1000./sx,gridmat(1,c_gp-1)*1000./sy
                  PRINTF,unit,' '
                  CLOSE,unit
                  free_lun,unit
                  SPAWN,'rayp'
                  GET_LUN,unit
                  ;OPENR,unit,'rays.bin'
                  OPENR,unit,raypath+'rays.asc'
                  raynsteps=INTARR(nray)
                  rayflag=INTARR(nray)
                  raya=FLTARR(nray,500)
                  rayx=FLTARR(nray,500)
                  rayy=FLTARR(nray,500)
                  l=0
                  flagr=0
                  dummy=0
                  ; dummies fill in the overhead info of the 'unformatted'
                  ; fortran binary file
                  FOR I=0,nray-1 DO BEGIN
                     ;READU,unit,dummy,dummy,dummy,l,dummy,flagr
                READF,unit,l,flagr
                     ;PRINT,I,l,flagr
                     raynsteps(I)=l
                     rayflag(I)=flagr
                     x=FLTARR(l)
                     y=FLTARR(l)
                     a=FLTARR(l)
               ;      READU,unit,dummy,dummy,dummy,dummy,x,y,a
               ;      READU,unit,dummy,dummy
                          READF,unit,x,y,a

                     ;PRINT,'X',x
                     ;PRINT,'Y',y
                     ;PRINT,'a',a
                     rayx(I,0:(l-1))=x
                     rayy(I,0:(l-1))=y
                     raya(I,0:(l-1))=a
                  ENDFOR
                  CLOSE,unit
                  free_lun,unit
                  RaysOK=1
                  doplot
                  END
'Get global': BEGIN
                  raystype=1
                  print,'LAT,LON:',gridmat(2,c_gp-1),gridmat(4,c_gp-1)
                  GET_LUN,unit
                  OPENW,unit,raypath+'Ray3a.inpray'
                  PRINTF,unit,paths(2)+filenames(2)
                  PRINTF,unit,paths(3)+filenames(3)
                  nray=ROUND(ABS(rayamin-rayamax)/rayres+1)
                  PRINTF,unit,nray
                  PRINTF,unit,rayfreq,' ',rayres,rayamin,' ',raytimestep,' ',rayoffdep,' ',raymindepth
                  PRINTF,unit,raydz
                  lat=gridmat(2,c_gp-1)+gridmat(3,c_gp-1)/60.
                  lon=gridmat(4,c_gp-1)+gridmat(5,c_gp-1)/60.
                  IF lon LT rlonmin THEN lon=lon+360.
                  PRINTF,unit,lon,lat
                  print,'Start point (lat,lon):',lat,lon
                  PRINTF,unit,' '
                  CLOSE,unit
                  free_lun,unit
                  SPAWN,'raypg'
                  GET_LUN,unit
                  ;OPENR,unit,'rays.bin'
                  OPENR,unit,raypath +'rays.asc'
                  raynsteps=INTARR(nray)
                  rayflag=INTARR(nray)
                  raya=FLTARR(nray,500)
                  rayx=FLTARR(nray,500)
                  rayy=FLTARR(nray,500)
                  l=0
                  flagr=0
                  dummy=0
                  ; dummies fill in the overhead info of the 'unformatted'
                  ; fortran binary file
                  FOR I=0,nray-1 DO BEGIN
                     ;READU,unit,dummy,dummy,dummy,l,dummy,flagr
                READF,unit,l,flagr
                     ;PRINT,I,l,flagr
                     raynsteps(I)=l
                     rayflag(I)=flagr
                     lon=FLTARR(l)
                     lat=FLTARR(l)
                     a=FLTARR(l)
                     X=FLTARR(l)
                     y=FLTARR(l)

               ;      READU,unit,dummy,dummy,dummy,dummy,x,y,a
               ;      READU,unit,dummy,dummy
                          READF,unit,lon,lat,a
                          FOR J=0,l-1 DO BEGIN
                             lonj=lon(J)
                             latj=lat(J)
                             LatlontoXY,latJ,0.,lonJ,0.,xl,yl
                     x(J)=xl
                     y(J)=yl
                        ENDFOR
                    ;   PRINT,'X',x,lon
                     ;PRINT,'Y',y,lat
                     ;PRINT,'a',a
                     rayx(I,0:(l-1))=x
                     rayy(I,0:(l-1))=y
                      raya(I,0:(l-1))=a
                  ENDFOR
                  CLOSE,unit
                  free_lun,unit
                  RaysOK=1
                  doplot
                  END
    'Forward': BEGIN
                  raystype=2
                  GET_LUN,unit
                  OPENW,unit,raypath+'Ray3a.inpray'
				      PRINTF,unit,paths(2)+filenames(2)
                  PRINTF,unit,paths(3)+filenames(3)
                  ; The rays are separated by the same distance rayres
                  nray=FLOOR(((nx-41)*dx*ABS(sin(rayamin*!dtor)) $
                     +(ny-41)*dy*ABS(cos(rayamin*!dtor)))/rayres)
                  rayamax=(nray-1)*rayres+rayamin
                  PRINTF,unit,nray
                  PRINTF,unit,rayfreq,rayres,rayamin,raytimestep,rayoffdep,raymindepth,FORMAT='(6F13.7)'
                  PRINTF,unit,raydz
                  
                  PRINTF,unit,' '
                  CLOSE,unit
                  free_lun,unit
                  SPAWN,'raypf'
                  GET_LUN,unit
                  OPENR,unit,raypath+'rays.asc'
                  raynsteps=INTARR(nray)
                  rayflag=INTARR(nray)
                  raya=FLTARR(nray,3000)
                  rayx=FLTARR(nray,3000)
                  rayy=FLTARR(nray,3000)
                  l=0
                  flagr=0
                  dummy=0
; dummies fill in the overhead info of the 'unformatted'
; fortran binary file
                  FOR I=0,nray-1 DO BEGIN
                     READF,unit,l,flagr
                     raynsteps(I)=l
                     rayflag(I)=flagr
                     x=FLTARR(l)
                     y=FLTARR(l)
                     a=FLTARR(l)
                     READF,unit,x,y,a

                     rayx(I,0:(l-1))=x
                     rayy(I,0:(l-1))=y
                     raya(I,0:(l-1))=a
                  ENDFOR
                  CLOSE,unit
                  free_lun,unit
                  RaysOK=1
                  doplot
                  END

    'Forward with current': BEGIN
                  raystype=2
                  x=(findgen(nx))*dx
                  y=(findgen(ny))*dy
                  XGP=FLTARR(nngp)
                  YGP=FLTARR(nngp)
                  XGP(0:nngp-1)=gridmat(0,0:nngp-1)
                  YGP(0:nngp-1)=gridmat(1,0:nngp-1)

                  TRIANGULATE,xgp,ygp,tr,b
                  dircos=TRIGRID(xgp,ygp,ww3matrix*cos(ww3dir*!dtor),tr, $
                   [dx,dy],[MIN(X), MIN(Y), MAX(X), MAX(Y)],XOUT=X,YOUT=Y)
                  dirsin=TRIGRID(xgp,ygp,ww3matrix*sin(ww3dir*!dtor),tr, $
                   [dx,dy],[MIN(X), MIN(Y), MAX(X), MAX(Y)],XOUT=X,YOUT=Y)
                  Index=WHERE(FINITE(dircos) EQ 0, kount)
                  IF (kount GT 0) THEN BEGIN 
                    dircos(Index)=0.
                    dirsin(Index)=0.
                  ENDIF
                  GET_LUN,unit
                  OPENW,unit,raypath+'Ray3a.inpray'
                  PRINTF,unit,paths(2)+filenames(2)
                  PRINTF,unit,paths(3)+filenames(3)
                  ; The rays are separated by the same distance rayres
                  nray=FLOOR(((nx-41)*dx*ABS(sin(rayamin*!dtor)) $
                     +(ny-41)*dy*ABS(cos(rayamin*!dtor)))/rayres)
                  rayamax=(nray-1)*rayres+rayamin
                  PRINTF,unit,nray
                  PRINTF,unit,rayfreq,rayres,rayamin,raytimestep,rayoffdep,raymindepth,FORMAT='(6F13.7)'
                  PRINTF,unit,raydz
                  
                  PRINT,'DIRCOSBB :',size(dircos),nx,ny
                  PRINTF,unit,' '
                  FOR J=0,NY-1 DO BEGIN 
                     PRINTF,unit,-1.*dirsin(*,J),FORMAT='(20F8.3)'
                  ENDFOR
                  FOR J=0,NY-1 DO BEGIN 
                     PRINTF,unit,-1.*dircos(*,J),FORMAT='(20F8.3)'
                  ENDFOR
                  CLOSE,unit
                  free_lun,unit
                  SPAWN,'raypfc'
                  GET_LUN,unit
                  OPENR,unit,raypath+'rays.asc'
                  raynsteps=INTARR(nray)
                  rayflag=INTARR(nray)
                  raya=FLTARR(nray,3000)
                  rayx=FLTARR(nray,3000)
                  rayy=FLTARR(nray,3000)
                  l=0
                  flagr=0
                  dummy=0
; dummies fill in the overhead info of the 'unformatted'
; fortran binary file
                  FOR I=0,nray-1 DO BEGIN
                     READF,unit,l,flagr
                     raynsteps(I)=l
                     rayflag(I)=flagr
                     x=FLTARR(l)
                     y=FLTARR(l)
                     a=FLTARR(l)
                     READF,unit,x,y,a

                     rayx(I,0:(l-1))=x
                     rayy(I,0:(l-1))=y
                     raya(I,0:(l-1))=a
                  ENDFOR
                  CLOSE,unit
                  free_lun,unit
                  RaysOK=1
                  doplot
                  END


      'OK': BEGIN
         WIDGET_CONTROL, /DESTROY, ev.top
         RETURN
      END
      ENDCASE
   END
   'WIDGET_TEXT_CH': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE ev.id OF
            WRline2(2):rayfreq=FLOAT(value(0))
            WRline3(2):raytimestep=FLOAT(value(0))
            WRline3(4):rayres=FLOAT(value(0))
            WRline5(2):rayoffdep=FLOAT(value(0))
            WRline5(4):raydz=FLOAT(value(0))
            WRline5(6):raymindepth=FLOAT(value(0))
      ENDCASE
      END
   'WIDGET_DROPLIST':BEGIN
      CASE ev.id of
      WRline1(1): BEGIN
         raysOK=ev.index
         doplot
         END
      ENDCASE
      END
   'WIDGET_SLIDER': BEGIN
                WIDGET_CONTROL, ev.id, GET_VALUE=value
               CASE (ev.id) OF
               WRline4(1):BEGIN
                  rayamin=FLOAT(value)
                  END
                 WRline4(2):BEGIN
                  rayamax=FLOAT(value)
                  END
                 ENDCASE
      END
   ENDCASE
;   ENDELSE
RETURN
END

