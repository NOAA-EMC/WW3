PRO EditSpecials, anterior_root
;*******COMMON BLOCKS**************************************
;** 1 ** Display parameters
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
;** 3 ** I/O and data variables
COMMON FILES,   filestatus,datastatus,paths,filters,filenames
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON GRIDPar, th1,th2,gfact,gsize1,gsize6
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
;** 4 ** subwindow widgets widgets
COMMON WSE,WSpeceditor,WSEline1
;*******END OF COMMON BLOCKS*******************************

	pbase=LONG(0)
	vecbase=LONG(0)
	Line1=LONG(0)



   pbase = WIDGET_BASE(TITLE='Special points editor', /COLUMN)

        XMENU, ['Insert point','Delete points','Import from file','Save with grid'],pbase, /ROW,$
		   UVALUE=[0,1,2,3]
         Wspeceditor= $
            WIDGET_TABLE(pbase, /EDITABLE, $

            COLUMN_LABELS=['GP','X','Y','LAT','min','LON','min','color', $
               'size','sym','thick','line','charsize'], $
            YSIZE=nspecgp,XSIZE=13, /RESIZEABLE_COLUMNS, $
            /SCROLL,Y_SCROLL_SIZE=10,X_SCROLL_SIZE=12, $
            COLUMN_WIDTHS=[40,80,80,40,80,40,80,40,40,40,40,40,40], $
            UNITS=0)


         Line1=WIDGET_BASE(pbase, /FRAME, /ROW)
	      WSEline1=LONARR(4)
         WSEline1(0)=WIDGET_LABEL(Line1,VALUE='Current point: ')
         WSEline1(1)=WIDGET_SLIDER(Line1, MAXIMUM=nspecgp, MINIMUM=1)
	      WSEline1(2)=WIDGET_TEXT(Line1,/EDITABLE, value=specname(0))


         Line6 = WIDGET_BUTTON(pbase, VALUE='OK')
	      WIDGET_CONTROL, /REALIZE, pbase
         XMANAGER, 'EditSpecials', pbase
         Displayspec
         subwin(5)=1
RETURN
END


;----------------------------------------------------------------------------
PRO EditSpecials_event, ev
;*******COMMON BLOCKS**************************************
;** 1 ** Display parameters
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
;** 3 ** I/O and data variables
COMMON FILES,   filestatus,datastatus,paths,filters,filenames
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON GRIDPar, th1,th2,gfact,gsize1,gsize6
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
;** 4 ** subwindow widgets widgets
COMMON WSE,WSpeceditor,WSEline1
;*******END OF COMMON BLOCKS*******************************

	type = TAG_NAMES(ev, /STRUCTURE)
   CASE type OF
   'WIDGET_BUTTON': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE value OF
      'OK': BEGIN
         WIDGET_CONTROL, /DESTROY, ev.top
		   subwin(0)=0
         RETURN
		END
		'Insert point':   BEGIN
 			WIDGET_CONTROL,WSpeceditor,INSERT_ROWS=1 ;,/USE_TABLE_SELECT
         nspecgp=nspecgp+1
         tempo=specmat
         specmat=FLTARR(13,nspecgp)
         specmat(*,0:nspecgp-2)=tempo(*,*)
         tempo=specname
         specname=STRARR(nspecgp)
         specname(0:nspecgp-2)=tempo
         WIDGET_CONTROL,WSEline1(1),SET_SLIDER_MAX=nspecgp
         Displayspec
         END
		'Delete points':   BEGIN
         selec=WIDGET_INFO(WSpeceditor,/TABLE_SELECT)
         idel=selec(3)-selec(1)+1
 			print,'selec',selec,'idel:',idel
         WIDGET_CONTROL,WSpeceditor,/DELETE_ROWS , $
            USE_TABLE_SELECT=[-1,selec(1),11,selec(3)]
         nspecgp=nspecgp-idel
         tempo=specmat
         specmat=FLTARR(13,nspecgp)
         specmat(*,0:selec(1)-1)=tempo(*,0:selec(1)-1)
         IF selec(1) LT nspecgp THEN specmat(*,selec(1):nspecgp-1)=tempo(*,selec(3)+1:nspecgp+idel-1)
         tempo=specname
         specname=STRARR(nspecgp)
         specname(0:selec(1)-1)=tempo(0:selec(1)-1)
         IF selec(1) LT nspecgp THEN specname(selec(1):nspecgp-1)=tempo(selec(3)+1:nspecgp+idel-1)
         c_spec=0
         WIDGET_CONTROL,WSEline1(1),SET_SLIDER_MAX=nspecgp
         WIDGET_CONTROL,WSEline1(2),SET_VALUE=specname(0)
         END
      'Import from file': 	BEGIN
        file=DIALOG_PICKFILE(/READ, FILTER = '../SHOWEX/*')
        IF file NE '' THEN BEGIN
        GET_LUN,unit
        OPENR,unit,file
        WHILE NOT EOF(unit) DO BEGIN
            latdeg=0
            latmin=0.
            londeg=0
            lonmin=0.
            line=0
            name=' '
            READF,unit,latdeg,latmin,londeg,lonmin,line ;,name
            LatLontoXY,latdeg,latmin,londeg,lonmin,x,y
            nspecgp=nspecgp+1
            symbol=8.
            IF line THEN symbol=-symbol
            vecteur=[x,y,latdeg,latmin,londeg,lonmin,0.,0.2,symbol,1.,0.,1.]
            tempo=specmat
            specmat=FLTARR(13,nspecgp)
            specmat(*,0:nspecgp-2)=tempo(*,*)
            specmat(*,nspecgp-1)=vecteur
            tempo=specname
            specname=STRARR(nspecgp)
            specname(0:nspecgp-2)=tempo(*)
            ;specname(nspecgp-1)=name
         ENDWHILE
         WIDGET_CONTROL,WSEline1(1),SET_SLIDER_MAX=nspecgp
         Displayspec
         CLOSE,unit
         FREE_LUN,unit
         ENDIF
	   END
      'Save with grid':BEGIN                                ;write grid file
            file=DIALOG_PICKFILE(/READ, FILTER = paths(5)+'*.grgp')
            IF file NE '' THEN BEGIN
               GET_LUN,unit
               OPENW,unit,file
               PRINTF,unit,nngp,'   Number of grid points'
               FOR I=1,(nngp) DO BEGIN
               PRINTF,unit,I,gridmat(0:5,I-1), $
                  FORMAT='(I6,2F9.4,I5,F9.4,I5,F9.4)'
               ENDFOR
               PRINTF,unit,nspecgp,'   Number of special points (display)'
               FOR I=0,(nspecgp-1) DO $
                   PRINTF,unit,specmat(*,I),' ',specname(I), $
                   FORMAT='(I6,2F9.4,I5,F9.4,I5,F9.4,I2,F4.1,3I3,F4.1,2a)'
               CLOSE,unit
               FREE_LUN,unit
               ENDIF
            END
      ENDCASE
      END
	'WIDGET_TABLE_CH': BEGIN
      CASE (ev.id) OF
		Wspeceditor:BEGIN
         IF (ev.type LE 2) THEN BEGIN
			   WIDGET_CONTROL, ev.id, GET_VALUE=value, $
            USE_TABLE_SELECT=[ev.x,ev.y,ev.x,ev.y]
            specmat(ev.X,ev.Y)=value
            IF ev.x LE 5 THEN UpdateSpectable,ev.X,ev.Y
         ENDIF
         END
      ENDCASE
      END
   'WIDGET_TEXT_CH': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE ev.id OF
      WSEline1(2):BEGIN
         specname(c_spec)=value(0)
         END
      ENDCASE
      END
	'WIDGET_DROPLIST':BEGIN
      CASE ev.id of
      WPline1(2) : c_repart=ev.index
      ENDCASE
      END
   'WIDGET_SLIDER': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE (ev.id) OF
      WSEline1(1): BEGIN
         c_spec=FIX(value)-1
         WIDGET_CONTROL,WSEline1(2), SET_VALUE=specname(c_spec)
         END
      ENDCASE
      END
   ENDCASE
RETURN
END

;----------------------------------------------------------------------------
PRO UpdateSpecTable,I,J
;*******COMMON BLOCKS**************************************
;** 3 ** I/O and data variables
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
;** 4 ** subwindow widgets widgets
COMMON WSE,WSpeceditor,WSEline1
;*******END OF COMMON BLOCKS*******************************
   londeg=0
   lonmin=0.
   latdeg=0
   latmin=0.
   x=0.
   y=0.
   CASE 1 OF
   (I LT 2):BEGIN    ;updates the grid points latitudes and longitudes
      XYtoLatLon,specmat(1,J),specmat(2,J),latdeg,latmin,londeg,lonmin
      specmat(3,J)=latdeg
      specmat(4,J)=latmin
      specmat(5,J)=londeg
      specmat(6,J)=lonmin
      WIDGET_CONTROL, WSpeceditor, SET_VALUE=specmat(3:6,J:J), $
      USE_TABLE_SELECT=[3,J,6,J]
      END
   (I GE 2) AND (I LE 5):BEGIN       ;updates the grid points X and Y coordinates
      LatLontoxy,specmat(3,J),specmat(4,J),specmat(5,J),specmat(6,J),x,y
      specmat(1,J)=x
      specmat(2,J)=y
      WIDGET_CONTROL, WSpeceditor, SET_VALUE=specmat(1:2,J:J), $
      USE_TABLE_SELECT=[1,J,2,J]
      END
   ENDCASE
END

;----------------------------------------------------------------------------
PRO Displayspec
;*******COMMON BLOCKS**************************************
;** 1 ** Display parameters
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
;** 3 ** I/O and data variables
COMMON FILES,   filestatus,datastatus,paths,filters,filenames
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON GRIDPar, th1,th2,gfact,gsize1,gsize6
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
;** 4 ** subwindow widgets widgets
COMMON WSE,WSpeceditor,WSEline1
;*******END OF COMMON BLOCKS*******************************
            WIDGET_CONTROL,WSpeceditor,TABLE_YSIZE=nspecgp
            WIDGET_CONTROL,USE_TABLE_SELECT=[-1,-1,12,nspecgp-1]
            ROWLAB=STRARR(nspecgp)
            ROWLAB=STRCOMPRESS(STRING(FINDGEN(nspecgp)+1,FORMAT='(I3)'))
            gridf=STRARR(13,nngp)
            gridf(0,*)='(I6)'
            gridf(1:2,*)='(F8.4)'
            gridf(3,*)='(I4)'
            gridf(4,*)='(F5.2)'
            gridf(5,*)='(I4)'
            gridf(6,*)='(F6.2)'
            gridf(7,*)='(I3)'
            gridf(8,*)='(F4.2)'
            gridf(9,*)='(I2)'
            gridf(10,*)='(F4.2)'
            gridf(11,*)='(I2)'
            gridf(12,*)='(F4.2)'
           ; COLUMN_LABELS=['GP','X','Y','LAT','min','LON','min','color', $
           ;    'size','sym','thick','line','charsize'], $

            WIDGET_CONTROL,WSpeceditor,FORMAT=gridf,ROW_LABELS=ROWLAB
            WIDGET_CONTROL,WSpeceditor, $
               USE_TABLE_SELECT=[0,0,12,nspecgp-1], $
               SET_VALUE=specmat
END
