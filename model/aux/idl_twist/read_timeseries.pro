;/
;/                  +-----------------------------------+
;/                  | TWIST                Ifremer-SHOM |
;/                  |           F. Ardhuin              |
;/                  |              IDL command language |
;/                  | Last update :         17-Oct-2013 |
;/                  +-----------------------------------+
;/
;/ Licence information: This code is distributed under the CeCILL license
;/                      generally compatible with the Gnu Public Licence (GPL) 
;/                      http://www.cecill.info/index.en.html
;/
;/    17-Oct-2013 : Creation                         ( version 2.00 )
;  1. Purpose :
;
;     Reads all sorts of time series formats ... 
;
;  2. Method :
;
;----------------------------------------------------------------------------
Pro read_timeseries,modspec,ncidtest,UNIT,I
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple


;      
; defines data structure
;
      modspec={specdata, typ:0, nfband:0, nabin:0, np:0, nvar:0, ntime:0L, $
            nrun:1, f1:ptr_new(), f2:ptr_new(), f:ptr_new(), df:ptr_new(), $
            theta:ptr_new(),dtheta:1.0, $
            plat:ptr_new(), plon:ptr_new(), pdepth:ptr_new(), $
            pU10:ptr_new(), pUdir:ptr_new(), $
            pCurr:ptr_new(), pCurrdir:ptr_new(), $
            pnames:ptr_new(), varnames:ptr_new(), dates: ptr_new(), $
            data:ptr_new(), flags:ptr_new(), path:ptr_new(), filename:ptr_new()}
            d={date, y:0, m:0, d:0, h:0, minu:0, s:0.0, zone:0.0, jday:0.0D}

      modspec.nabin=1
      
      IF (ncidtest LT 0) THEN BEGIN 
;
; in this case this is not a NetCDF file 
;          
         line='TOTO'
         READF,unit,line
         ts_filetype=1
         typ=0
         n1=0
         n2=0
         n3=0
;
; If not a number: branches out to bad_num label: should be 2D spectrum
;
         ON_IOERROR, bad_num
         READS,line,typ
         ts_filetype=0
         READF,unit,n1,n2,n3
         nrun=1
         d={date, y:0, m:0, d:0, h:0, minu:0, s:0.0, zone:0.0, jday:0.0D}
         modspec.typ=typ
         modspec.nfband=n1
         modspec.np=n2
         modspec.nvar=n3
         f1=FLTARR(n1)
         f2=f1
         READF,unit,f1,f2
         modspec.f1=ptr_new(f1)
         modspec.f2=ptr_new(f2)
         f=0.5*(f1+f2)
         f(0)=f1(0)
         f(n1-1)=f2(n1-1)
         df=ABS(f2-f1)
         modspec.f=ptr_new(f)
         modspec.df=ptr_new(df)
         n=REPLICATE('        ',modspec.np)
         FMT='(001(A8))'
         IF typ LE 1 THEN BEGIN
            modspec.nabin=1
            theta=FLTARR(1)
            modspec.theta=ptr_new(theta)
         ENDIF
         STRPUT,FMT,STRING(modspec.np,FORMAT='(I3)'),1
         READF,unit,n,FORMAT=FMT
         modspec.pnames=ptr_new(n)
         STRPUT,FMT,STRING(modspec.nvar,FORMAT='(I3)'),1
         n=REPLICATE('        ',modspec.nvar)
         READF,unit,n,FORMAT=FMT
         modspec.varnames=ptr_new(n)
         I=0L
         y=0
         d=0
         h=0
         m=0
         minu=0
         zone=0.
         nt=2600000L/(modspec.nabin*modspec.nfband)
         dar=REPLICATE({date},nt)
         data=FLTARR(1,modspec.np,modspec.nvar,modspec.nfband,modspec.nabin,nt)
;
; Reads frequency spectra
;
         IF (typ EQ 1) THEN BEGIN
            vecteur=FLTARR(modspec.nfband)
            WHILE NOT EOF(unit) DO BEGIN
               READF,unit,y,m,d,h,minu,zone
               sec=0
               jday=JULDAY(m,d,y,h,minu,zone)
               dar[i].y=y
               dar[i].d=d
               dar[i].m=m
               dar[i].h=h
               dar[i].minu=minu
               dar[i].s=zone ;sec
               dar[i].zone=0; zone
               dar[i].jday=jday
               FOR J=0,modspec.np-1 DO BEGIN
                  FOR K=0,modspec.nvar-1 DO BEGIN
                     READF,unit,vecteur
                     data(0,J,K,*,0,I)=vecteur
                  ENDFOR
               ENDFOR
               I=I+1
            ENDWHILE
;
; Reads bulk data
;
         ENDIF ELSE BEGIN
           vecteur=FLTARR(modspec.nvar)
           over=0
           jdayold=0
           WHILE NOT EOF(unit) AND over EQ 0 DO BEGIN
            READF,unit,y,m,d,h,minu,zone,vecteur
            sec=0
            IF (I MOD 2000 EQ 0) THEN print,I,y,m,d,h,minu,zone
            jday=JULDAY(m,d,y,h,minu,zone)
            dar[i].y=y
            dar[i].d=d
            dar[i].m=m
            dar[i].h=h
            dar[i].minu=minu
            dar[i].s=zone ;sec
            dar[i].zone=0. ;zone
            dar[i].jday=jday
            data(0,0,*,0,0,I)=vecteur
            IF jday LT jdayold THEN over=1
            jdayold=jday
            IF modspec.np GT 1 and over EQ 0 THEN BEGIN
               FOR J=1,modspec.np-1 DO BEGIN
                  READF,unit,vecteur
                  data(0,J,*,0,0,I)=vecteur
               ENDFOR
            ENDIF
            I=I+1
         ENDWHILE
       ENDELSE
;
; Reads spectra files (WWATCH ASCII format) 
;
bad_num: IF (ts_filetype EQ 1) THEN BEGIN  
        typ=2
        latdeg=0
        latmin=0.
        londeg=0
        lonmin=0.
        n1=0
        n2=0
        n3=0
        string1=' '
        string2=' '
        itoto=0
        READS,line,string1,n1,n2,n3,string2,FORMAT='(A23,X,3I6,X,A32)'
        nrun=1
        d={date, y:0, m:0, d:0, h:0, minu:0, s:0.0, zone:0.0, jday:0.0D}
        modspec.typ=typ
        modspec.nfband=n1
        modspec.nabin=n2
        modspec.np=n3
        print,string2
        IF STRMID(string2,0,1) EQ "'" OR STRMID(string2,0,1) EQ '"' THEN BEGIN
           modspec.nvar=1
           varn=STRARR(1)
           varn(0)='E(,)    '
        ENDIF ELSE BEGIN
           varnames=STRARR(6)
           varnames(0)='E(,)    '
           varnames(1)='Sin(,)  '
           varnames(2)='Snl(,)  '
           varnames(3)='Sds(,)  '
           varnames(4)='Sbt(,)  '
           varnames(5)='Stot(,) '
           string2=STRCOMPRESS(string2,/REMOVE_ALL)
           invar=INTARR(6)
           FOR I=0,5 DO BEGIN
              if STRMID(string2,I,1) EQ 'T' THEN invar(I)=1
           ENDFOR
           modspec.nvar=TOTAL(invar)
           varn=STRARR(modspec.nvar)
           J=0
           FOR I=0,modspec.nvar-1 DO BEGIN
              IF invar(I) THEN BEGIN
                 varn(J)=varnames(I)
                J=J+1
              ENDIF
           ENDFOR
        ENDELSE
        modspec.varnames=ptr_new(varn)
        PRINT,'Variables:',modspec.nvar,varn

        f1=FLTARR(modspec.nfband)
        df=FLTARR(modspec.nfband)
        READF,unit,f1
        df(0)=f1(1)-f1(0)
        FOR I=1,modspec.nfband-2 DO BEGIN
           df(I)=0.5*(f1(I+1)-f1(I-1))
        ENDFOR
        print,'DF:',F1,'##',DF
        df(modspec.nfband-1)=f1(modspec.nfband-1)-f1(modspec.nfband-2)
        f=f1
        f1=f+0.5*df
        f2=f-0.5*df
        modspec.f1=ptr_new(f1)
        modspec.f2=ptr_new(f2)
        f=f1
        df=ABS(f2-f1)
        modspec.f=ptr_new(f)
        modspec.df=ptr_new(df)
        theta=FLTARR(modspec.nabin)
        READF,unit,theta
        theta=!pi+theta               ;plot direction is direction from
        modspec.theta=ptr_new(theta)

        nt=24*10*100; *31*6;*8
        n=STRARR(modspec.np)
        lats=FLTARR(modspec.np)
        lons=FLTARR(modspec.np)
        depths=FLTARR(modspec.np)
        U10s=FLTARR(modspec.np,nt)
        Udirs=FLTARR(modspec.np,nt)
        Currs=FLTARR(modspec.np,nt)
        Currdirs=FLTARR(modspec.np,nt)
        I=0
        y=0
        d=0
        h=0
        m=0
        minu=0
        zone=0.
        lat=0.
        lon=0.
        depth=0.
        U10=0.
        Udir=0.
        Curr=0.
        Currdir=0.

        dar=REPLICATE({date},nt)
        data=FLTARR(1,modspec.np,modspec.nvar,modspec.nfband,modspec.nabin,nt)
        vecteur=FLTARR(modspec.nfband,modspec.nabin)
        WHILE NOT EOF(unit)  DO BEGIN
            READF,unit,y,m,d,h,minu,zone,FORMAT='(I4,2I2,X,3I2)'
            sec=0
            print,'Reading spectrum for:',I,y,m,d,h,minu,zone
            jday=JULDAY(m,d,y,h,minu,zone)
            dar[i].y=y
            dar[i].d=d
            dar[i].m=m
            dar[i].h=h
            dar[i].minu=minu
            dar[i].s=0.
            dar[i].zone=zone
            dar[i].jday=jday
            FOR J=0,modspec.np-1 DO BEGIN
               READF,unit,string1,lat,lon,depth,U10,Udir,Curr,Currdir, $
               FORMAT='(X,A10,X,2F7.2,F10.1,F7.2,F6.1,F7.2,F6.1)'
               LatLondegtomin,lat,lon,latdeg,latmin,londeg,lonmin,latlonstring
               n(J)=string1+'('+latlonstring+')'
               lats(J)=lat
               lons(J)=lon
               depths(J)=depth
               U10s(J,I)=U10
               Udirs(J,I)=Udir
               Currs(J,I)=Curr
               Currdirs(J,I)=Currdir
               FOR K=0,modspec.nvar-1 DO BEGIN
                  READF,unit,vecteur,FORMAT='(7G11.3)'
                  data(0,J,K,*,*,I)=vecteur
               ENDFOR
            ENDFOR
            I=I+1;
         ENDWHILE

         modspec.pnames=ptr_new(n)
         modspec.plat=ptr_new(lats)
         modspec.plon=ptr_new(lons)
         modspec.pdepth=ptr_new(depths)
         modspec.pU10=ptr_new(U10s)
         modspec.pUdir=ptr_new(Udirs)
         modspec.pCurr=ptr_new(Currs)
         modspec.pCurrdir=ptr_new(Currdirs)

      ENDIF  ; end of bad_num branch
      ENDIF ELSE BEGIN 
;
; in this case, it is a NetCDF file
;
      ncall=NCDF_INQUIRE(ncidtest)
      nvars=ncall.NVARS
      dimtid=NCDF_DIMID(ncidtest,'time')
;
; Globwave uses 'frequency1d'
;
      dimfid=NCDF_DIMID(ncidtest,'frequency')
      IF (dimfid LT 0) THEN dimfid=NCDF_DIMID(ncidtest,'frequency1d')
      IF (dimfid LT 0) THEN dimfid=NCDF_DIMID(ncidtest,'frequency2d')
      dimdid=NCDF_DIMID(ncidtest,'direction')
      dimsid=NCDF_DIMID(ncidtest,'station')
      dimxid=NCDF_DIMID(ncidtest,'string16')
;
      IF (dimtid GE 0 AND dimfid GE 0 and dimdid GE 0) THEN BEGIN 
;
; In this case we are reading type 2 time series: (f,theta) spectra
;
         NCDF_DIMINQ,ncidtest,dimfid,name1,n1
         NCDF_DIMINQ,ncidtest,dimdid,name1,n2
         NCDF_DIMINQ,ncidtest,dimsid,name1,n3
;
         freqid=NCDF_VARID(ncidtest,'frequency') 
         dirid =NCDF_VARID(ncidtest,'direction') 
         Efthid=NCDF_VARID(ncidtest,'Efth') 
         IF (Efthid EQ -1) THEN Efthid=NCDF_VARID(ncidtest,'efth')  ; new variable names ...  
         timeid=NCDF_VARID(ncidtest,'time') 

         modspec.typ=2
         modspec.nfband=n1
         modspec.nabin=n2
         modspec.np=n3
         modspec.nvar=1
         varn=STRARR(1)
         varn(0)='E(,)    '
         modspec.varnames=ptr_new(varn)
         PRINT,'Variables:',modspec.nvar,varn

         f1=FLTARR(modspec.nfband)
         df=FLTARR(modspec.nfband)
         varid=NCDF_VARID(ncidtest,'frequency') 
         NCDF_VARGET,ncidtest,varid,f
         varid=NCDF_VARID(ncidtest,'frequency1') 
         NCDF_VARGET,ncidtest,varid,f1
         varid=NCDF_VARID(ncidtest,'frequency2') 
         NCDF_VARGET,ncidtest,varid,f2
         df=f2-f1
         modspec.f=ptr_new(f)
         modspec.f1=ptr_new(f1)
         modspec.f2=ptr_new(f2)
         modspec.df=ptr_new(df)
         theta=FLTARR(modspec.nabin)
         varid=NCDF_VARID(ncidtest,'direction') 
         NCDF_VARGET,ncidtest,varid,theta
;
; Changes direction conventions to "from"
;
         NCDF_ATTGET,ncidtest,varid,'units',thetaunits
         NCDF_ATTGET,ncidtest,varid,'standard_name',thetaname
         IF (STRING(thetaunits) EQ "degree") THEN theta = theta *!dtor
         IF (STRING(thetaname) EQ "sea_surface_wave_to_direction") THEN theta=!pi+theta           
;
         modspec.theta=ptr_new(theta)
         NCDF_DIMINQ,ncidtest,dimtid,name1,nt
         print,'NC directions converted:',theta,STRING(thetaunits)
         n=STRARR(modspec.np)
         lats=FLTARR(modspec.np)
         lons=FLTARR(modspec.np)
         depths=FLTARR(modspec.np)
         U10s=FLTARR(modspec.np,nt)
         Udirs=FLTARR(modspec.np,nt)
         Currs=FLTARR(modspec.np,nt)
         Currdirs=FLTARR(modspec.np,nt)

         NCDF_VARGET,ncidtest,timeid,timeNC
         NCDF_ATTGET,ncidtest,timeid,'units',timeunits
         IF (STRING(timeunits) EQ "days since 1990-01-01T00:00:00Z") THEN BEGIN 
	      jdays = timeNC + julday(1,1,1990,0,0,0)
         ENDIF ELSE BEGIN 
            PRINT,'Unknown reference time:',STRING(timeunits)
            RETURN
         ENDELSE
         dar=REPLICATE({date},nt)
         FOR I=0,nt-1 DO BEGIN 
            dar[i].jday=jdays(I)
            CALDAT,jdays(I),m,d,y,h,minu,sec    
            sec=ROUND(sec)
            dar[i].y=y
            dar[i].d=d
            dar[i].m=m
            dar[i].h=h
            dar[i].minu=minu
            dar[i].s=sec
            dar[i].zone=0. ;zone
         ENDFOR
         varid=NCDF_VARID(ncidtest,'station_name')
         NCDF_VARGET,ncidtest,varid,n1
         n=STRING(n1)
         varid=NCDF_VARID(ncidtest,'latitude') 
         NCDF_VARGET,ncidtest,varid,lats
         varid=NCDF_VARID(ncidtest,'longitude') 
         NCDF_VARGET,ncidtest,varid,lons
         varid=NCDF_VARID(ncidtest,'depth') 
         IF (varid EQ -1) THEN varid=NCDF_VARID(ncidtest,'dpt')  ; new variable names ...  
         NCDF_VARGET,ncidtest,varid,depths
         varid=NCDF_VARID(ncidtest,'u10m') 
         IF (varid EQ -1) THEN varid=NCDF_VARID(ncidtest,'wnd')  ; new variable names ...  
         NCDF_VARGET,ncidtest,varid,U10s
         varid=NCDF_VARID(ncidtest,'udir') 
         IF (varid EQ -1) THEN varid=NCDF_VARID(ncidtest,'wnddir')  ; new variable names ...  
         NCDF_VARGET,ncidtest,varid,Udirs
         varid=NCDF_VARID(ncidtest,'curr') 
         IF (varid EQ -1) THEN varid=NCDF_VARID(ncidtest,'cur')  ; new variable names ...  
         NCDF_VARGET,ncidtest,varid,Currs
         varid=NCDF_VARID(ncidtest,'currdir') 
         IF (varid EQ -1) THEN varid=NCDF_VARID(ncidtest,'curdir')  ; new variable names ...  
         NCDF_VARGET,ncidtest,varid,Currdirs
         varid=NCDF_VARID(ncidtest,'currdir') 
         NCDF_VARGET,ncidtest,Efthid,dataCDF
         data=FLTARR(1,modspec.np,modspec.nvar,modspec.nfband,modspec.nabin,nt)
         FOR I=0,nt-1 DO BEGIN 
           FOR J=0,modspec.np-1 DO BEGIN
              FOR K=0,modspec.nfband-1 DO BEGIN 
                 data(0,J,0,K,*,I)=dataCDF(*,K,J,I)
              ENDFOR
            ENDFOR
         ENDFOR
         I=nt
         modspec.pnames=ptr_new(n)
         modspec.plat=ptr_new(lats)
         modspec.plon=ptr_new(lons)
         modspec.pdepth=ptr_new(depths)
         modspec.pU10=ptr_new(U10s)
         modspec.pUdir=ptr_new(Udirs)
         modspec.pCurr=ptr_new(Currs)
         modspec.pCurrdir=ptr_new(Currdirs)
         NCDF_CLOSE,ncidtest
;
       ENDIF ELSE BEGIN IF (dimfid GT 0) THEN BEGIN
;
; In this case we are reading type 1 time series: (f,theta) spectra
;
         modspec.typ=1
         modspec.nabin=1
         theta=FLTARR(1)
         modspec.theta=ptr_new(theta)
         NCDF_DIMINQ,ncidtest,dimfid,name1,n1
         IF (dimsid GT 0) THEN NCDF_DIMINQ,ncidtest,dimsid,name1,n3 ELSE n3=1
         freqid=NCDF_VARID(ncidtest,'frequency') 
         dirid =NCDF_VARID(ncidtest,'direction') 
         Efthid=NCDF_VARID(ncidtest,'Efth') 
         timeid=NCDF_VARID(ncidtest,'time') 
         modspec.nfband=n1
         modspec.np=n3
         modspec.nvar=1
         varn=STRARR(1)

         f1=FLTARR(modspec.nfband)
         df=FLTARR(modspec.nfband)
         varid=NCDF_VARID(ncidtest,'frequency') 
         IF (varid LT 0) THEN $ 
           varid=NCDF_VARID(ncidtest,'wave_directional_spectrum_central_frequency')
         IF (varid LT 0) THEN $ 
           varid=NCDF_VARID(ncidtest,'wave_frequency_spectrum_central_frequency')
         NCDF_VARGET,ncidtest,varid,f
         varid=NCDF_VARID(ncidtest,'frequency1') 
         IF (varid GE 0) THEN BEGIN 
           NCDF_VARGET,ncidtest,varid,f1
           varid=NCDF_VARID(ncidtest,'frequency2') 
           NCDF_VARGET,ncidtest,varid,f2
           df=f2-f1
         ENDIF ELSE BEGIN 
           varid=NCDF_VARID(ncidtest,'frequency') 
           IF (varid LT 0) THEN $ 
              varid=NCDF_VARID(ncidtest,'wave_directional_spectrum_frequency_range')
           IF (varid LT 0) THEN $ 
             varid=NCDF_VARID(ncidtest,'wave_frequency_spectrum_frequency_range')
           NCDF_VARGET,ncidtest,varid,df
           f1=f-df*0.5
           f2=f+df*0.5
         ENDELSE 
         modspec.f=ptr_new(f)
         modspec.f1=ptr_new(f1)
         modspec.f2=ptr_new(f2)
         modspec.df=ptr_new(df)
         varid=NCDF_VARID(ncidtest,'direction') 
         NCDF_DIMINQ,ncidtest,dimtid,name1,nt
         n=STRARR(modspec.np)
         lats=FLTARR(modspec.np)
         lons=FLTARR(modspec.np)
         depths=FLTARR(modspec.np)
         U10s=FLTARR(modspec.np,nt)
         Udirs=FLTARR(modspec.np,nt)
         Currs=FLTARR(modspec.np,nt)
         Currdirs=FLTARR(modspec.np,nt)

         NCDF_VARGET,ncidtest,timeid,timeNC
         NCDF_ATTGET,ncidtest,timeid,'units',timeunits
         CASE STRING(timeunits) OF  ; We will do a better jog with this later ;-) 
            'days since 1990-01-01T00:00:00Z': jdays = timeNC + julday(1,1,1990,0,0,0)
            'seconds since 1970-01-01 00:00:00': jdays = timeNC/86400. + julday(1,1,1970,0,0,0)
            ELSE: PRINT,'Unknown reference time:',STRING(timeunits),'.'
         ENDCASE
         dar=REPLICATE({date},nt)
         zone=0.
         FOR I=0,nt-1 DO BEGIN 
            dar[i].jday=jdays(I)
            CALDAT,jdays(I),m,d,y,h,minu,sec    
            sec=ROUND(sec)
            dar[i].y=y
            dar[i].d=d
            dar[i].m=m
            dar[i].h=h
            dar[i].minu=minu
            dar[i].s=0.
            dar[i].zone=zone
            IF (I EQ 0) THEN print,'Reading spectrum for:',I,y,m,d,h,minu,sec
         ENDFOR
         print,'Last spectrum for:',I-1,y,m,d,h,minu,sec
         
         varid=NCDF_VARID(ncidtest,'station_name')
         IF (varid EQ -1) THEN NCDF_ATTGET,ncidtest,'wmo_id',n1,/GLOBAL ELSE $
           NCDF_VARGET,ncidtest,varid,n1
         pn=STRARR(1)
         pn(0)=STRING(n1)
         modspec.pnames=ptr_new(pn)

         varid=NCDF_VARID(ncidtest,'latitude') 
         IF (varid EQ -1) THEN varid=NCDF_VARID(ncidtest,'lat') 
         NCDF_VARGET,ncidtest,varid,lats 
               
         varid=NCDF_VARID(ncidtest,'longitude') 
         IF (varid EQ -1) THEN varid=NCDF_VARID(ncidtest,'lon') 
         NCDF_VARGET,ncidtest,varid,lons
         varid=NCDF_VARID(ncidtest,'depth') 
         IF (varid EQ -1) THEN varid=NCDF_VARID(ncidtest,'dpt')  ; new variable names ...  
         NCDF_VARGET,ncidtest,varid,depths
         varid=NCDF_VARID(ncidtest,'u10m') 
         IF (varid EQ -1) THEN varid=NCDF_VARID(ncidtest,'wnd')  ; new variable names ...  
         IF (varid GE 0)  THEN NCDF_VARGET,ncidtest,varid,U10s
         varid=NCDF_VARID(ncidtest,'udir') 
         IF (varid EQ -1) THEN varid=NCDF_VARID(ncidtest,'wnddir')  ; new variable names ...  
         IF (varid GE 0)  THEN NCDF_VARGET,ncidtest,varid,Udirs
         IF (varid GE 0)  THEN varid=NCDF_VARID(ncidtest,'curr') 
         IF (varid EQ -1) THEN varid=NCDF_VARID(ncidtest,'cur')  ; new variable names ...  
         IF (varid GE 0)  THEN NCDF_VARGET,ncidtest,varid,Currs
         IF (varid GE 0)  THEN varid=NCDF_VARID(ncidtest,'currdir') 
         IF (varid EQ -1) THEN varid=NCDF_VARID(ncidtest,'curdir')  ; new variable names ...  
         IF (varid GE 0)  THEN  NCDF_VARGET,ncidtest,varid,Currdirs

         varid=NCDF_VARID(ncidtest,'ef')
         IF (varid EQ -1) THEN $
             varid=NCDF_VARID(ncidtest,'wave_directional_spectrum_spectral_density')
         IF (varid EQ -1) THEN $
             varid=NCDF_VARID(ncidtest,'wave_frequency_spectrum_spectral_density')
         NCDF_VARGET,ncidtest,varid,dataCDF
         IF (varid GE 0)  THEN  modspec.nvar = 1
         data=FLTARR(1,modspec.np,modspec.nvar,modspec.nfband,modspec.nabin,nt)
; Will add directional stuff later ... 
         varn=STRARR(1)
         varn(0)='Ef      '
         modspec.varnames=ptr_new(varn)
         PRINT,'Variables:',modspec.nvar,varn
         FOR I=0,nt-1 DO BEGIN 
           FOR J=0,modspec.np-1 DO BEGIN
              FOR K=0,modspec.nfband-1 DO BEGIN 
                 data(0,J,0,K,0,I)=dataCDF(K,I)
              ENDFOR
            ENDFOR
         ENDFOR
         I=nt
         modspec.plat=ptr_new(lats)
         modspec.plon=ptr_new(lons)
         modspec.pdepth=ptr_new(depths)
         modspec.pU10=ptr_new(U10s)
         modspec.pUdir=ptr_new(Udirs)
         modspec.pCurr=ptr_new(Currs)
         modspec.pCurrdir=ptr_new(Currdirs)
         NCDF_CLOSE,ncidtest
       ENDIF 
      ENDELSE
      ENDELSE

      modspec.ntime=I
      modspec.data=ptr_new(data(*,*,*,*,*,0:I-1))
      modspec.dates=ptr_new(dar(0:I-1))
      flags=INTARR(I)+1
      PRINT,'Read ',I,' time steps.'

      modspec.flags=ptr_new(flags)
      END 
