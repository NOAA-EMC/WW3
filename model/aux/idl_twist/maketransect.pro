;----------------------------------------------------------------------------
PRO Maketransect, anterior_root
;----------------------------------------------------------------------------
; This procedure generates the widgets for the "transect designer" subwindow
; These widgets allow to interactively modify the position of the points
; used as a basis for the transect, as well as the method to get other points
; between these basis points.
; The interpolation coefficients for the triangle network are generated for
; each point of the transect by calling a fortran code, similar to Ray3a.
;----------------------------------------------------------------------------
COMMON TRANSECT,Ntrans,Strans,Xtrans,Ytrans,Ztrans,Itrans, $
                  COtrans,TransOK,transsym,transline,transthick,Ispectrans, $
                  spectransname,ntransgp,transsymsize
COMMON WTwidgets,WTline1,WTline2,WTline4,WTgrideditor
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON TRANSLOC,igpt,thetatrans,gridtmat,transtype
COMMON SPACE,   c_gp,c_cut,indexgp,c_x,c_y,c_lon,c_lat

   thetatrans=0
   ntrans=30
   transsym=1
   transsymsize=0.5
   transthick=1
   transline=0
   IF transOK EQ 0 THEN BEGIN
	   ntransgp=10
      GRIDTMAT=FLTARR(7,ntransgp)
      spectransname=STRARR(ntransgp)
   ENDIF
   igpt=0
   pbase=LONG(0)
	vecbase=LONG(0)
	Line1=LONG(0)
	WTline1=LONARR(5)


   pbase = WIDGET_BASE(TITLE='Transect designer', /COLUMN)
   WIDGET_CONTROL, pbase, GROUP_LEADER=root
        
        Line1=WIDGET_BASE(pbase, /FRAME, /ROW)
        WTline1(0)=WIDGET_LABEL(Line1, $
           VALUE='Basis points: ')
        WTline1(1)=WIDGET_TEXT(Line1,/EDITABLE, XSIZE=5, YSIZE=1, $
        VALUE=STRCOMPRESS(String(ntransgp)))
        WTline1(2)=WIDGET_LABEL(Line1,VALUE='Points in transect: ')
        WTline1(3)=WIDGET_TEXT(Line1,/EDITABLE, XSIZE=5, YSIZE=1, $
        VALUE=STRCOMPRESS(String(ntrans)))
        WTline1(4)=WIDGET_DROPLIST(Line1, $
            VALUE=['Lines','Splines'])

         Line2=WIDGET_BASE(pbase, /FRAME, /ROW)
         WTgrideditor=LONARR(3)
         WTgrideditor(1)= $
            WIDGET_TABLE(line2, /EDITABLE, $
            COLUMN_LABELS=['I','X','Y','Lat','min','Lon','min'], $
            YSIZE=ntransgp+1,XSIZE=9, /RESIZEABLE_COLUMNS, $
            COLUMN_WIDTHS=[40,80,80,60,100,60,100], $
            /SCROLL,Y_SCROLL_SIZE=10,X_SCROLL_SIZE=7, $
            UNITS=0)
         
         Line3=WIDGET_BASE(pbase, /FRAME, /ROW)
	      WTline2=LONARR(14)
         WTline2(0)=WIDGET_LABEL(Line3,VALUE='Current point and name: ')
         WTline2(1)=WIDGET_SLIDER(Line3, MAXIMUM=ntransgp, MINIMUM=1)
	      WTline2(2)=WIDGET_TEXT(Line3,/EDITABLE, value=spectransname(0))
	      Line4=WIDGET_BASE(pbase, /FRAME, /ROW)
	      WTline2(3)=WIDGET_BUTTON(Line4, VALUE='Take c_gp')
	      WTline2(4)=WIDGET_LABEL(Line4, value='Take special #:')
         WTline2(5)=WIDGET_TEXT(Line4,/EDITABLE,XSIZE=3,value='1')
	      Line5=WIDGET_BASE(pbase, /FRAME, /ROW)
	      WTline2(6)=WIDGET_LABEL(Line5, value='Sym:')
         WTline2(7)=WIDGET_TEXT(Line5, XSIZE=5, $
            value=STRCOMPRESS(STRING(transsym)))
         WTline2(8)=WIDGET_LABEL(Line5, value='Size:')
         WTline2(9)=WIDGET_TEXT(Line5, XSIZE=5, $
            value=STRCOMPRESS(STRING(transsymsize)))
         WTline2(10)=WIDGET_LABEL(Line5, value='Line:')
         WTline2(11)=WIDGET_TEXT(Line5, XSIZE=5, $
            value=STRCOMPRESS(STRING(transline)))
         WTline2(12)=WIDGET_LABEL(Line5, value='Thick:')
         WTline2(13)=WIDGET_TEXT(Line5, XSIZE=5, $
            value=STRCOMPRESS(STRING(transthick)))
            
	
	      XMENU, ['Move points','End move','Delete points'],pbase, /ROW,$
		   UVALUE=[0,1,2], BUTTONS = menu_ids, /FRAME
	      XMENU, ['Generate Transect','Compute inter','Save transect','Load transect'],pbase, /ROW,$
		   UVALUE=[0,1,2,3], BUTTONS = menu_ids, /FRAME
	      
   
         Line4=WIDGET_BASE(pbase, /FRAME, /ROW)
         WTline4=LONARR(3)
         WTline4(0)= WIDGET_TEXT(Line4,YSIZE=1,VALUE='Use ray with direction:')
	      WTline4(1) = WIDGET_TEXT(Line4,/EDITABLE, $
               value=strcompress(string(thetatrans)))
   
         
         Line5 = WIDGET_BUTTON(pbase, VALUE='OK')
	      WIDGET_CONTROL, /REALIZE, pbase
         DisplayTgrid
         subwin(0)=1
         XMANAGER, 'MakeTransect', pbase
RETURN
END


;----------------------------------------------------------------------------
PRO MakeTransect_event, ev
;----------------------------------------------------------------------------
; Deals with the actions on the widgets of the transect subwindow
;----------------------------------------------------------------------------
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON TRANSECT,Ntrans,Strans,Xtrans,Ytrans,Ztrans,Itrans, $
                  COtrans,TransOK,transsym,transline,transthick,Ispectrans, $
                  spectransname,ntransgp,transsymsize
COMMON WTwidgets,WTline1,WTline2,WTline4,WTgrideditor
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON TRANSLOC,igpt,thetatrans,gridtmat,transtype
COMMON SPACE,   c_gp,c_cut,indexgp
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused
COMMON FILES,   filestatus,datastatus,paths,filters,filenames
COMMON SPECIALS,nspecgp,specmat,specname

	typo = TAG_NAMES(ev, /STRUCTURE)
   CASE typo OF
   'WIDGET_BUTTON': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE value OF
      'OK': BEGIN
         WIDGET_CONTROL, /DESTROY, ev.top
		   subwin(4)=0
         RETURN
		   END
      'Move points':clickflag=4
      'End move':clickflag=0
      'Take c_gp':BEGIN
         print,'c_gp...:',c_gp,igpt
         gridtmat(0,igpt)=c_gp
         gridtmat(1:6,igpt)=gridmat(0:5,c_gp-1)
 			UpdateTtable,0,igpt
         END
      'Compute inter':BEGIN
         Ztrans=FLTARR(Ntrans)
         Itrans=INTARR(Ntrans,3)
         COtrans=FLTARR(Ntrans,3)
         IF (datastatus(7) NE 0) THEN BEGIN 
         GET_LUN,unit
         OPENW,unit,'transect.inp'
			PRINTF,unit,paths(2)+filenames(2)
         PRINTF,unit,paths(3)+filenames(3)
         PRINTF,unit,paths(5)+filenames(5)
         PRINTF,unit,paths(7)+filenames(7)
         PRINTF,unit,Ntrans
         FOR I=0,Ntrans-1 DO BEGIN
             PRINTF,unit,Xtrans(I),Ytrans(I)
         ENDFOR
         CLOSE,unit
         FREE_LUN,unit
         SPAWN,'transect'
         GET_LUN,unit
         OPENR,unit,'transect.tra'
         READF,unit,Ntrans
         x=0.
         y=0.
         z=0.
         i1=0
         i2=0
         i3=0
         a=0.
         b=0.
         c=0.
         FOR I=0,Ntrans-1 DO BEGIN
            READF,unit,x,y,z,i1,i2,i3,a,b,c
            print,I,x,y,z,i1,i2,i3,a,b,c
            Ztrans(I)=z
            Itrans(I,0)=i1
            Itrans(I,1)=i2
            Itrans(I,2)=i3
            COtrans(I,0)=a
            COtrans(I,1)=b
            COtrans(I,2)=c
         ENDFOR
         CLOSE,unit
         FREE_LUN,unit
         ENDIF ELSE BEGIN
            FOR I=0,Ntrans-1 DO BEGIN
            ;print,'dx:',dx,dy,sx,sy,Xtrans(I)/dx,Ytrans(I)/dy
               Ztrans(I)=gd(FLOOR(Xtrans(I)/dx),FLOOR(Ytrans(I)/dy))
            ENDFOR
         ENDELSE
         TransOK=1
         END
      'Delete points':BEGIN
            selec=WIDGET_INFO(WTGrideditor(1),/TABLE_SELECT)
            idel=selec(3)-selec(1)+1
 			   WIDGET_CONTROL,WTGrideditor(1),/DELETE_ROWS 
            ntransgp=ntransgp-idel
            print,'del :',ntransgp,idel
            tempo=gridtmat
            gridtmat=FLTARR(7,ntransgp)
            IF selec(1) GT 0 THEN gridtmat(*,0:selec(1)-1)=tempo(*,0:selec(1)-1)
            gridtmat(*,selec(1):ntransgp-1)=tempo(*,selec(3)+1:ntransgp+idel-1)
            tempo=spectransname
            spectransname=STRARR(nspecgp)
            spectransname(0:selec(1)-1)=tempo(0:selec(1)-1)
            spectransname(selec(1):ntransgp-1)=tempo(selec(3)+1:ntransgp+idel-1)
            igpt=0
            WIDGET_CONTROL,WTline2(1),SET_SLIDER_MAX=ntransgp,value=1
            WIDGET_CONTROL,WTline2(2),SET_VALUE=spectransname(0)
            Displaytgrid
         END
      'Save transect':BEGIN
         trfile=PICKFILE(/WRITE, FILTER = '*.tr')
         IF trfile NE '' THEN BEGIN
            GET_LUN,unit
            OPENW,unit,trfile
            PRINTF,unit,Ntrans,NtransGP,transsym,transsymsize, $
               transline,transthick
            FOR I=0,Ntrans-1 DO PRINTF,unit,Strans(I),Xtrans(I),Ytrans(I), $
                  Ztrans(I),TRANSPOSE(Itrans(I,*)),TRANSPOSE(COtrans(I,*))
            FOR I=0,Ntransgp-1 DO PRINTF,unit,Ispectrans(I), $
                  gridtmat(*,I),spectransname(I)
            CLOSE,unit
            FREE_LUN,unit
         ENDIF
         END
      'Load transect':BEGIN
         trfile=PICKFILE(/READ, FILTER = '*.tr')
         IF trfile NE '' THEN BEGIN
            GET_LUN,unit
            OPENR,unit,trfile
            READF,unit,Ntrans,NtransGP,transsym,transsymsize,transline,transthick
            Strans=FLTARR(Ntrans)
            Xtrans=FLTARR(Ntrans)
            Ytrans=FLTARR(Ntrans)
            Ztrans=FLTARR(Ntrans)
            Itrans=INTARR(Ntrans,3)
            COtrans=FLTARR(Ntrans,3)
            s=0.
            x=0.
            y=0.
            z=0.
            i1=0
            i2=0
            i3=0
            co1=0.
            co2=0.
            co3=0.
            FOR I=0,Ntrans-1 DO BEGIN
               READF,unit,s,x,y,z,i1,i2,i3,co1,co2,co3
               Strans(I)=s
               Xtrans(I)=x
               Ytrans(I)=y
               Ztrans(I)=z
               Itrans(I,0)=i1
               Itrans(I,1)=i2
               Itrans(I,2)=i3
               COtrans(I,0)=co1
               COtrans(I,1)=co2
               COtrans(I,2)=co3
            ENDFOR
            
            Ispectrans=INTARR(Ntransgp)
            spectransname=STRARR(Ntransgp)
            gridtmat=FLTARR(7,Ntransgp)
            vecteur=FLTARR(7) 
            i1=0
            name=' '          
            FOR I=0,Ntransgp-1 DO BEGIN
               READF,unit,i1,vecteur,name
               Ispectrans(I)=i1
               gridtmat(*,I)=vecteur
               spectransname(I)=name
            ENDFOR
            CLOSE,unit
            FREE_LUN,unit
            transOK=1
         ENDIF
         END
      'Generate Transect': 	BEGIN
         GenerateTransect
 		   END     
      ENDCASE
      END
	'WIDGET_TABLE_CH': BEGIN
      print,'Ev:',ev
      print,'type:',ev.type,ev.id,ev.x,ev.y
      CASE (ev.id) OF
		WTgrideditor(1):IF (ev.type LE 2) THEN BEGIN
            WIDGET_CONTROL,ev.id, GET_VALUE=valeur, $
            USE_TABLE_SELECT=[ev.x,ev.y,ev.x,ev.y]
            gridtmat(ev.X,ev.Y)=valeur(0,0)
            Updatettable,ev.X,ev.Y
         ENDIF
      ENDCASE
      END
   'WIDGET_TEXT_CH': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE ev.id OF
      WTline1(1):BEGIN
         ntransgp=FLOOR(FLOAT(value(0)))
         if Ntrans LT ntransgp THEN Ntrans=ntransgp
         WIDGET_CONTROL,WTline1(3),SET_VALUE=STRCOMPRESS(STRING(Ntrans))
         taille=size(gridtmat)
         IF (ntransgp GT taille(2)) THEN $
 			   WIDGET_CONTROL,WTGrideditor(1), $
            INSERT_ROWS=(ntransgp-taille(2)) ;,/USE_TABLE_SELECT
         IF (ntransgp LT taille(2)) THEN BEGIN
            selec=[0,ntransgp,7,taille(2)-1]
            WIDGET_CONTROL,WTGrideditor(1),/DELETE_ROWS,USE_TABLE_SELECT=selec
         ENDIF
         tempo=gridtmat
         taille=size(gridtmat)
         nmax=MIN([taille(2)-1,ntransgp-1])
         gridtmat=FLTARR(7,ntransgp)
         gridtmat(*,0:nmax)=tempo(*,0:nmax)
         tempo=spectransname
         spectransname=STRARR(ntransgp)
         spectransname(0:nmax)=tempo(0:nmax)
         igpt=0
         WIDGET_CONTROL,WTline2(1),SET_SLIDER_MAX=ntransgp
         WIDGET_CONTROL,WTline2(2),SET_VALUE=spectransname(0)
         Displaytgrid
         END
      WTline1(3):BEGIN
         ntrans=FLOOR(FLOAT(value(0)))
         END
      WTline2(2):spectransname(igpt)=value(0)
      WTline2(5):BEGIN
         ispec=FIX(value(0))-1 
         ;print,'specmat:',specmat(*,ispec)
         gridtmat(0:6,igpt)=specmat(0:6,ispec)
         spectransname(igpt)=specname(ispec)
         WIDGET_CONTROL,WTline2(2),SET_VALUE=spectransname(igpt)
         Displaytgrid
         END
      WTline2(7):transsym=ROUND(FLOAT(value(0)))
      WTline2(9):transsymsize=FLOAT(value(0))
      WTline2(11):transline=ROUND(FLOAT(value(0)))
      WTline2(13):transthick=ROUND(FLOAT(value(0)))
      WTsl(4): IF RaysOK THEN BEGIN
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
         WTline2(1): BEGIN
            igpt=FLOAT(value)-1
            WIDGET_CONTROL,WTline2(2),SET_VALUE=spectransname(igpt)
            END
         ENDCASE
      END
   ELSE:
   ENDCASE
;   ENDELSE
RETURN
END

;----------------------------------------------------------------------------
PRO UpdateTTable,I,J
COMMON TRANSLOC,igpt,thetatrans,gridtmat,transtype
COMMON SPACE,   c_gp,c_cut,indexgp,c_x,c_y
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused
COMMON WTwidgets,WTline1,WTline2,WTline4,WTgrideditor
      ;IF I EQ 0 THEN BEGIN
      ;   gridtmat(1:4,J)=gridmat(1:4,c_gp)
      ;ENDIF ELSE BEGIN
   londeg=0
   lonmin=0.
   latdeg=0
   latmin=0.
   x=0.
   y=0.
   IF I LT 3 THEN BEGIN
      XYtoLatLon,x,y,latdeg,latmin,londeg,lonmin,latlonstring
      gridtmat(3,J)=latdeg
      gridtmat(4,J)=latmin
      gridtmat(5,J)=londeg
      gridtmat(6,J)=lonmin
   ENDIF ELSE BEGIN
      LatLontoXY,latdeg,latmin,londeg,lonmin,x,y
      gridtmat(1,J)=x
      gridtmat(2,J)=y
   ENDELSE
      ;ENDELSE
   WIDGET_CONTROL,WTgrideditor(1),SET_VALUE=gridtmat(0:6,J:J), $
     USE_TABLE_SELECT=[0,J,6,J]
     print,'new values:',gridtmat(0:6,J:J)
   RETURN
END
            
;----------------------------------------------------------------------------
PRO GenerateTransect  ;generates a transect
COMMON TRANSECT,Ntrans,Strans,Xtrans,Ytrans,Ztrans,Itrans, $
                  COtrans,TransOK,transsym,transline,transthick,Ispectrans, $
                  spectransname,ntransgp,transsymsize
COMMON TRANSLOC,igpt,thetatrans,gridtmat,transtype
   N2=Ntrans-ntransGP
   Strans0=FLTARR(Ntrans)
   Ispectrans=FLTARR(Ntransgp)
   Strans0(0)=0.
   FOR I=1,ntransgp-1 DO BEGIN
      ds=SQRT((gridtmat(1,I-1)-gridtmat(1,I))^2 +(gridtmat(2,I-1)-gridtmat(2,I))^2) 
      Strans0(I)=Strans0(I-1)+ds
   ENDFOR
   STRANS0(ntransgp:ntrans-1)=(Strans0(ntransgp-1))/(N2+1)+FINDGEN(N2)*(Strans0(ntransgp-1))/(N2+1)
   sorted=SORT(STRANS0)
   Strans=Strans0(sorted)
   FOR I=0,ntransgp-1 DO BEGIN
      Index=WHERE(strans EQ Strans0(I),kount)
      IF kount NE 0 THEN Ispectrans(I)=Index(0)
   ENDFOR
   Xtrans=FLTARR(Ntrans)
   Ytrans=FLTARR(Ntrans)
   Xtrans(0)=gridtmat(1,0)
   Ytrans(0)=gridtmat(2,0)
   Xtrans(Ntrans-1)=gridtmat(1,ntransgp-1)
   Ytrans(Ntrans-1)=gridtmat(2,ntransgp-1)
   FOR I=1,ntransgp-1 DO BEGIN
      FOR J=Ispectrans(I-1)+1,Ispectrans(I) DO BEGIN
         Xtrans(J)=gridtmat(1,I-1)+ $
            (gridtmat(1,I)-gridtmat(1,I-1))* $
            (Strans(J)-Strans0(I-1))/(Strans0(I)-Strans0(I-1))
         Ytrans(J)=gridtmat(2,I-1)+ $
            (gridtmat(2,I)-gridtmat(2,I-1))* $
            (Strans(J)-Strans0(I-1))/(Strans0(I)-Strans0(I-1))
      ENDFOR
   ENDFOR
END

;----------------------------------------------------------------------------
PRO DisplayTgrid     
COMMON TRANSECT,Ntrans,Strans,Xtrans,Ytrans,Ztrans,Itrans, $
                  COtrans,TransOK,transsym,transline,transthick,Ispectrans, $
                  spectransname,ntransgp,transsymsize
COMMON TRANSLOC,igpt,thetatrans,gridtmat,transtype
COMMON WTwidgets,WTline1,WTline2,WTline4,WTgrideditor
   WIDGET_CONTROL,WTGrideditor(1),TABLE_YSIZE=ntransgp
   WIDGET_CONTROL,USE_TABLE_SELECT=[-1,-1,8,ntransgp-1], $
      COLUMN_WIDTHS=[40,40,40,80,80,40,80,40,80]
   ROWLAB=STRCOMPRESS(STRING(FINDGEN(ntransgp)+1,FORMAT='(I3)'))
   gridf=STRARR(9,ntransgp)
   gridf(0,*)='(I5)'
   gridf(1:2,*)='(F8.2)'
   gridf(3,*)='(I4)'
   gridf(4,*)='(F8.3)'
   gridf(5,*)='(I4)'
   gridf(6,*)='(F8.3)'
                     
   WIDGET_CONTROL,WTGrideditor(1),FORMAT=gridf,ROW_LABELS=ROWLAB
   WIDGET_CONTROL,WTGrideditor(1), $
      USE_TABLE_SELECT=[0,0,6,ntransgp-1], $
      SET_VALUE=gridtmat(0:6,0:ntransgp-1)
END
