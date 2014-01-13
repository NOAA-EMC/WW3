PRO Bathytool, anterior_root
; This procedure creates the widged for the "grid designer" subwindow
;*******COMMON BLOCKS**************************************
;** 1 ** Display parameters
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
;** 3 ** I/O and data variables
COMMON BATHYTOOL, bathy_ker_size,bathy_line,bathytool_click
COMMON BATHYDOT,ndotmax,ndot1,XDOT1,YDOT1,ZDOT1,ndot2,XDOT2,YDOT2,ZDOT2, $
   bathydot_path,showdot1,showdot2
;** 4 ** subwindow widgets widgets
COMMON WBat,Wbathy,rootbtool
;*******END OF COMMON BLOCKS*******************************
   bathy_line=-10.
   bathy_ker_size=2.
   showdot1=0
   showdot2=0
   bathydot_path='/data/span11/ardhuin/MULTIBEAM/LIST/'
   pbase=LONG(0)
	vecbase=LONG(0)
	WBLines=LONARR(10)
	WBathy=LONARR(10,10)

; Guardar valores en variables temporales
	rootbtool=anterior_root
;	WIDGET_CONTROL, root, SENSITIVE=0


   pbase = WIDGET_BASE(TITLE='Bathymetry tools', /COLUMN)

        i=0
        WBLines(i)=WIDGET_BASE(pbase, /FRAME, /ROW)
        WBathy(i,0)=WIDGET_LABEL(WBLines(i),VALUE='Operations on gridded data: ')
        i=i+1
        WBLines(i)=WIDGET_BASE(pbase, /FRAME, /ROW)
        WBathy(i,0)=WIDGET_BUTTON(WBLines(i),VALUE='Fill gaps')
        WBathy(i,1)=WIDGET_BUTTON(WBLines(i),VALUE='Draw lines at depth:')
        WBathy(i,2)=WIDGET_TEXT(WBLines(i), XSIZE=5, YSIZE=1,/EDITABLE, $
            VALUE=STRCOMPRESS(String(bathy_line)))
        WBathy(i,3)=WIDGET_BUTTON(WBLines(i),VALUE='End of drawing')
        i=i+1
        WBLines(i)=WIDGET_BASE(pbase, /FRAME, /ROW)
        WBathy(i,0)=WIDGET_BUTTON(WBLines(i),VALUE='Smooth')
        WBathy(i,1)=WIDGET_LABEL(WBLines(i),VALUE='Kernel size (km):')
        WBathy(i,2)=WIDGET_TEXT(WBLines(i), XSIZE=5, YSIZE=1,/EDITABLE, $
            VALUE=STRCOMPRESS(String(bathy_ker_size)))
        i=i+1
        WBLines(i)=WIDGET_BASE(pbase, /FRAME, /ROW)
        WBathy(i,0)=WIDGET_BUTTON(WBLines(i), VALUE='Save modified grid')

        i=i+1
        WBLines(i)=WIDGET_BASE(pbase, /FRAME, /ROW)
        WBathy(i,0)=WIDGET_LABEL(WBLines(i),VALUE='Operations on point lists: ')
        i=i+1
        WBLines(i)=WIDGET_BASE(pbase, /FRAME, /ROW)
        WBathy(i,0)=WIDGET_BUTTON(WBLines(i),VALUE='Read dot bathy1')
        WBathy(i,1)=WIDGET_BUTTON(WBLines(i),VALUE='Read dot bathy2')
        i=i+1
        WBLines(i)=WIDGET_BASE(pbase, /FRAME, /ROW)
        WBathy(i,0)=WIDGET_LABEL(WBLines(i),VALUE='Plot:')
        WBathy(i,1)=WIDGET_DROPLIST(WBLines(i), $
            VALUE=['Nothing','Plot bathy 1:dots','Plot bathy 1:contour'])
        WBathy(i,2)=WIDGET_DROPLIST(WBLines(i), $
            VALUE=['Nothing','Plot bathy 2:dots','Plot bathy 2:contour'])

        i=i+1
        WBLines(i)= WIDGET_BUTTON(pbase, VALUE='OK')
	WIDGET_CONTROL, /REALIZE, pbase
        XMANAGER, 'Bathytoolg', pbase
        subwin(5)=1
RETURN
END


;----------------------------------------------------------------------------
PRO Bathytoolg_event, ev
;*******COMMON BLOCKS**************************************
;** 1 ** Display parameters
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON SPACE,   c_gp,c_cut,indexgp,c_x,c_y,c_lon,c_lat
;** 3 ** I/O and data variables
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON FILES,   filestatus,datastatus,paths,filters,filenames
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON BATHYTOOL, bathy_ker_size,bathy_line,bathytool_click
COMMON BATHYDOT,ndotmax,ndot1,XDOT1,YDOT1,ZDOT1,ndot2,XDOT2,YDOT2,ZDOT2, $
   bathydot_path,showdot1,showdot2
COMMON OVERLAY2,adbathydot
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
;** 4 ** subwindow widgets widgets
COMMON WBat,Wbathy,rootbtool
;*******END OF COMMON BLOCKS*******************************

	type = TAG_NAMES(ev, /STRUCTURE)
   ;print,'type:',type
   CASE type OF
   'WIDGET_BUTTON': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE value OF
      'FillGaps':FillGaps
      'Smooth':Smbat
      'Draw lines at depth:':BEGIN
         WIDGET_CONTROL,Wdraw,EVENT_PRO='bathytool_draw_event'
         bathytool_click=0
         END
      'End of drawing':BEGIN
         WIDGET_CONTROL,Wdraw,EVENT_PRO='analyzer_event'
         END
      'Save modified grid':BEGIN
         file=DIALOG_PICKFILE(/WRITE, $
         PATH = PATHS(3),FILTER = '*.grd')
         IF file NE '' THEN BEGIN
            IF !VERSION.OS_FAMILY EQ 'Windows' THEN $
            spos=RSTRPOS(file,'\') ELSE spos=RSTRPOS(file,'/')
            PATHS(3)=STRMID(file,0,spos+1)
            GET_LUN,unit
            OPENW,unit,file
            IF !VERSION.OS_FAMILY EQ 'Windows' THEN  WRITEU,unit,transpose(SWAP_ENDIAN(gd)) $
               ELSE WRITEU,unit,transpose(gd)
            CLOSE,unit
            FREE_LUN,unit
         ENDIF
         END

      'Read dot bathy1':BEGIN
         file=DIALOG_PICKFILE(/WRITE, $
         PATH = bathydot_path,FILTER = '*.list')
         IF file NE '' THEN BEGIN
            GET_LUN,unit
            IF !VERSION.OS_FAMILY EQ 'Windows' THEN $
            spos=RSTRPOS(file,'\') ELSE spos=RSTRPOS(file,'/')
            bathydot_path=STRMID(file,0,spos+1)
            GET_LUN,unit
            OPENR,unit,file
            ndotmax=1000000
            XDOTT=MAKE_ARRAY(ndotmax,/DOUBLE)
            YDOTT=MAKE_ARRAY(ndotmax,/DOUBLE)
            ZDOTT=MAKE_ARRAY(ndotmax,/DOUBLE)
            x=0.D
            y=0.D
            z=0.D
            i=0L
            IF datastatus(2) THEN BEGIN
               WHILE NOT EOF(unit) DO BEGIN
                  READF,unit,y,x,z
                  IF X GE rlonmin AND X LE rlonmax AND y GE rlatmin AND y LE rlatmax $
                     THEN BEGIN
                     i=i+1L
                     XDOTT(i)=x
                     YDOTT(i)=y
                     ZDOTT(i)=z
                  ENDIF
               ENDWHILE
               ENDIF ELSE BEGIN
               WHILE NOT EOF(unit) DO BEGIN
                  READF,unit,y,x,z
                  i=i+1L
                  XDOTT(i)=x
                  YDOTT(i)=y
                  ZDOTT(i)=z
               ENDWHILE
            ENDELSE
            ndot1=i
            ;converts to X,Y in km
            XDOT1=DOUBLE(nx)*dx*(XDOTT(1:ndot1)-rlonmin)/((rlonmax-rlonmin))
            YDOT1=DOUBLE(ny)*dy*(YDOTT(1:ndot1)-rlatmin)/((rlatmax-rlatmin))

            ZDOT1=ZDOTT(1:ndot1)
            print,'Number of points in list:',ndot1
            CLOSE,unit
            FREE_LUN,unit
         ENDIF
         END
      'Read dot bathy2':BEGIN
         file=DIALOG_PICKFILE(/WRITE, $
         PATH = bathydot_path,FILTER = '*.list')
         IF file NE '' THEN BEGIN
            GET_LUN,unit
            IF !VERSION.OS_FAMILY EQ 'Windows' THEN $
            spos=RSTRPOS(file,'\') ELSE spos=RSTRPOS(file,'/')
            bathydot_path=STRMID(file,0,spos+1)
            GET_LUN,unit
            OPENR,unit,file
            ndotmax=1000000
            XDOTT=MAKE_ARRAY(ndotmax,/DOUBLE)
            YDOTT=MAKE_ARRAY(ndotmax,/DOUBLE)
            ZDOTT=MAKE_ARRAY(ndotmax,/DOUBLE)
            x=0.D
            y=0.D
            z=0.D
            i=0L
            IF datastatus(2) THEN BEGIN
               WHILE NOT EOF(unit) DO BEGIN
                  READF,unit,y,x,z
                  IF X GE rlonmin AND X LE rlonmax AND y GE rlatmin AND y LE rlatmax $
                     THEN BEGIN
                     i=i+1L
                     XDOTT(i)=x
                     YDOTT(i)=y
                     ZDOTT(i)=z
                  ENDIF
               ENDWHILE
               ENDIF ELSE BEGIN
               WHILE NOT EOF(unit) DO BEGIN
                  READF,unit,y,x,z
                  i=i+1L
                  XDOTT(i)=x
                  YDOTT(i)=y
                  ZDOTT(i)=z
               ENDWHILE
            ENDELSE
            ndot2=i
            ;converts to X,Y in km
            XDOT2=DOUBLE(nx)*dx*(XDOTT(1:ndot2)-rlonmin)/((rlonmax-rlonmin))
            YDOT2=DOUBLE(ny)*dy*(YDOTT(1:ndot2)-rlatmin)/((rlatmax-rlatmin))


            ZDOT2=ZDOTT(1:ndot2)
            print,'Number of points in list:',ndot2
            CLOSE,unit
            FREE_LUN,unit
         ENDIF
         END
      'OK': BEGIN
         WIDGET_CONTROL, /DESTROY, ev.top
		   WIDGET_CONTROL,rootbtool, SENSITIVE=1
         subwin(0)=0
         RETURN
		   END
		'Triangulate':BEGIN
         X=FLTARR(NNGP)
         Y=FLTARR(NNGP)
         X(*)=(gridmat(0,1:nngp))
         Y(*)=(gridmat(1,1:nngp))
         TRIANGULATE, X, Y, DELTRI, BOUNDARY, $
            CONNECTIVITY=LIST, REPEATS=REP
         TAILLE=SIZE(DELTRI)
         PRINT,'N points:',N_ELEMENTS(X)
         PRINT,'N triangles:',Taille(2)
         TAILLELIST=SIZE(LIST)
         ;******************************************************
         ; Builds the neighbor relationships between grid points
         ;******************************************************
         gpnei=INTARR(taillelist(1)+1)
         gpnei(1:taillelist(1))=LIST(*)+1
         ;******************************************************
         ; Creates the triangles and zones (SUBDOMAINS)
         ;******************************************************
         ntri=Taille(2)
         TRIGP=INTARR(ntri+1,4)
         xmax=(NX-1)*dx
         ymax=(Ny-1)*dy
         IZONE=5
         nzone=5
         FOR I=0,(ntri-1) DO BEGIN
            IZONE=5
            IF (X(DELTRI(0,I)) LT 0.) OR (X(DELTRI(1,I)) LT 0.) $
               OR (X(DELTRI(2,I)) LT 0) THEN IZONE=1
            IF (X(DELTRI(0,I)) GT xmax) OR (X(DELTRI(1,I)) GT xmax) $
               OR (X(DELTRI(2,I)) GT xmax) THEN IZONE=2
            IF (Y(DELTRI(0,I)) LT 0.) OR (Y(DELTRI(1,I)) LT 0.) $
               OR (Y(DELTRI(2,I)) LT 0) THEN IZONE=3
            IF (Y(DELTRI(0,I)) GT ymax) OR (Y(DELTRI(1,I)) GT ymax) $
               OR (Y(DELTRI(2,I)) GT ymax) THEN IZONE=4
            trigp(I+1,0)=IZONE
            trigp(I+1,1)=DELTRI(0,I)+1
            trigp(I+1,2)=DELTRI(1,I)+1
            trigp(I+1,3)=DELTRI(2,I)+1
            ; Sorts the vertices within each triangle
            IF trigp(I+1,1) GT TRIGP(I+1,2) THEN BEGIN
               tempo=TRIGP(I+1,2)
               TRIGP(I+1,2)=trigp(I+1,1)
               trigp(I+1,1)=tempo
            ENDIF
            IF trigp(I+1,2) GT TRIGP(I+1,3) THEN BEGIN
               tempo=TRIGP(I+1,3)
               TRIGP(I+1,3)=trigp(I+1,2)
               trigp(I+1,2)=tempo
            ENDIF
            IF trigp(I+1,1) GT TRIGP(I+1,2) THEN BEGIN
               tempo=TRIGP(I+1,2)
               TRIGP(I+1,2)=trigp(I+1,1)
               trigp(I+1,1)=tempo
            ENDIF
         ENDFOR
         ; Tests if all the boundary points are actually outside the domain
         nboundary=N_ELEMENTS(BOUNDARY)
         FOR I=0,nboundary-1 DO BEGIN
            IF X(BOUNDARY(I)) LE xmax AND X(BOUNDARY(I)) GE 0 $
               AND Y(BOUNDARY(I)) LE ymax AND Y(BOUNDARY(I)) GE 0 THEN $
               PRINT,'Warning, point #',I+1,' is on the boundary and in the domain'
         ENDFOR
         ;******************************************************
         ; Builds the point-> triangle lookup table
         ;******************************************************
         GPTRI=INTARR(ntri+1,5)
         GPTRI(*,0:3)=TRIGP
         GPTRI(*,4)=TRANSPOSE(FINDGEN(ntri+1))
         ; sort on index of first vertex
         I1=LONARR(ntri+1)
         I1(*)=(GPTRI(*,1)*NTRI+GPTRI(*,2))*NTRI+GPTRI(*,3)
         SO=SORT(I1)
         TEMPO=GPTRI
         GPTRI=GPTRI(SO,*)
         ;FOR I=1,NTRI DO PRINT,TRANSPOSE(GPTRI(I,*))
         ;ntrinei=taillelist(1)-nboundary-nngp-1
         ;print,'ntrinei 1',ntrinei,taillelist(1),nngp,'##',SIZE(boundary)
         ntrinei=ntri*15 ;(ntrinei/2-nboundary)*2*4
         ntrinei=ntri+1+ntrinei
         TRINEI=INTARR(ntrinei+1)
         index=ntri+2
         II=INTARR(4)
         TRINEI(1)=index
         FOR I=1,ntri DO BEGIN
            TRINEI(I+1)=TRINEI(I)
            tempo=INTARR(ntri)
            nindex=0
            FOR J=1,3 DO BEGIN
               GPC=TRIGP(I,J)
               IF GPC EQ GPNEI(GPNEI(GPC)) THEN K1=GPNEI(GPC)+1 ELSE K1=GPNEI(GPC)
               K2=GPNEI(GPC+1)-1
               ;IF I GE 755 THEN PRINT,I,J,'GPC:',GPC,'NEI:',GPNEI(K1:K2)
               FOR K=K1,K2 DO BEGIN
                  II(1)=GPC
                  II(2)=GPNEI(K)
                  IF K LT K2 THEN II(3)=GPNEI(K+1) ELSE $
                     IF GPC NE GPNEI(GPNEI(GPC)) THEN II(3)=GPNEI(K1) $
                     ELSE II(3)=GPNEI(K-1)
                  SO=SORT(II)
                  II=II(SO)
                  ;PRINT,'II',II(1:3)
                  IMIN=1
                  IMAX=NTRI
                  IND3=(II(1)*NTRI+II(2))*NTRI+II(3)
                  IGUESS=(NTRI*II(1))/NNGP
                  INDG=(GPTRI(IGUESS,1)*NTRI+GPTRI(IGUESS,2))*NTRI+GPTRI(IGUESS,3)
                  WHILE (INDG NE IND3) AND (IMIN NE IMAX) DO BEGIN
                     IF INDG LT IND3 THEN BEGIN
                        IMIN=IGUESS
                        IGUESS=(IMIN+IMAX)/2
                        IF IGUESS EQ IMIN THEN IGUESS=IMAX
                     ENDIF ELSE BEGIN
                         IMAX=IGUESS
                         IGUESS=(IMIN+IMAX)/2
                     ENDELSE
                     INDG=(GPTRI(IGUESS,1)*NTRI+GPTRI(IGUESS,2))*NTRI+GPTRI(IGUESS,3)
                  ENDWHILE
                  found=0
                  FOR L=TRINEI(I),TRINEI(I+1)-1 DO $
                     IF TRINEI(L) EQ GPTRI(IGUESS,4) THEN found=1
                  IF found EQ 0 AND I NE GPTRI(IGUESS,4) THEN BEGIN
                     TRINEI(TRINEI(I+1))=GPTRI(IGUESS,4)
                     TRINEI(I+1)=TRINEI(I+1)+1
                  ENDIF
               ENDFOR
            ENDFOR
            ;print,I,' neighbors:',TRINEI(TRINEI(I):TRINEI(I+1)-1)
         ENDFOR
         ntrinei=TRINEI(ntri+1)-1
         tempo=TRINEI
         TRINEI=INTARR(ntrinei+1)
         TRINEI(1:ntrinei)=TEMPO(1:ntrinei)
         PRINT,'ntrinei',ntrinei,'nei..',TEMPO(ntrinei),TEMPO(ntrinei+1)
         datastatus(18)=1
         END
      'Save tri':BEGIN                                ;write grid file
            file=DIALOG_PICKFILE(/READ, FILTER = '*.log')
            IF file NE '' THEN BEGIN
               GET_LUN,unit
               OPENW,unit,file
               PRINTF,unit,ntri,'  Number of triangles'
               IZONE=5
               FOR I=1,ntri DO BEGIN
                  PRINTF,unit,I,TRANSPOSE(TRIGP(I,0:3))
               ENDFOR

               ntrinei=TRINEI(ntri+1)-1
               PRINTF,unit,ntri,ntrinei,'   Size of neighbor array'
               PRINTF,unit,trinei(1:ntri+1)
               FOR I=1,ntri DO BEGIN
                  ;PRINT,I,trinei(trinei(I):trinei(I+1)-1)
                  PRINTF,unit,trinei(trinei(I):trinei(I+1)-1)
               ENDFOR

               zones=INTARR(ntri+1)
               zones(*)=TRIGP(0:ntri,0)
               SO=(SORT(zones))
               zones2=INDGEN(ntri+1)
               zones2=zones2(SO)
               zones=zones(SO)

               zonetri=INTARR(nzone+2+ntri)
               ZC=1
               ZONETRI(1)=nzone+2
               ZT=1
               ZZ=1
               FOR I=2,ntri DO BEGIN
                  IF zones(I) NE ZC THEN BEGIN
                     ZONETRI(ZC+1)=ZONETRI(ZC)+ZT
                     temp=zones2(ZZ:ZZ+ZT-1)
                     ZONETRI(ZONETRI(ZC):ZONETRI(ZC+1)-1)=temp(SORT(temp))
                     ZZ=ZZ+ZT
                     ZT=0
                     ZC=ZC+1
                  ENDIF
                  ZT=ZT+1
               ENDFOR
               ZONETRI(nzone+1)=NZONE+NTRI+2
               temp=zones2(ZZ:ZZ+ZT-1)
               ZONETRI(ZONETRI(nzone):ZONETRI(nzone+1)-1)=temp(SORT(temp))

               nzonetri=ZONETRI(nzone+1)-1
               PRINTF,unit,nzone,nzonetri,'   Size of Zone->triangle array'
               PRINTF,unit,ZONETRI(1:nzone+1)
               FOR I=1,nzone DO BEGIN
                  PRINTF,unit,ZONETRI(ZONETRI(I):ZONETRI(I+1)-1)
               ENDFOR


               ZONEGP=INTARR(nzone+2+NTRI*3)

               index=nzone+2
               ZONEGP(1)=index
               FOR I=1,nzone DO BEGIN
                  ZONEGP(I+1)=ZONEGP(I)
                  FOR J=ZONETRI(I),ZONETRI(I+1)-1 DO BEGIN
                  TRIC=ZONETRI(J)
                  FOR K=1,3 DO BEGIN
                     GPC=TRIGP(TRIC,K)
                     found=0
                     FOR L=ZONEGP(I),ZONEGP(I+1)-1 DO $
                        IF ZONEGP(L) EQ GPC THEN found=1
                     IF found EQ 0 THEN BEGIN
                        ZONEGP(ZONEGP(I+1))=GPC
                        ZONEGP(I+1)=ZONEGP(I+1)+1
                     ENDIF
                  ENDFOR
                  ENDFOR
               ENDFOR
               nzonegp=ZONEGP(nzone+1)
               tempo=ZONEGP
               ZONETRI=INTARR(nZONEGP)
               ZONETRI=tempo(0:nZONEGP-1)
               FOR I=1,nzone DO BEGIN
                  temp=ZONEGP(ZONEGP(I):ZONEGP(I+1)-1)
                  ZONEGP(ZONEGP(I):ZONEGP(I+1)-1)=temp(SORT(temp))
               ENDFOR

               nzonegp=ZONEGP(nzone+1)-1
               PRINTF,unit,nzone,nzonegp,'   Size of Zone->GP array'
               PRINTF,unit,ZONEGP(1:nzone+1)
               FOR I=1,nzone DO BEGIN
                  PRINTF,unit,ZONEGP(ZONEGP(I):ZONEGP(I+1)-1)
               ENDFOR

               GPZONE=INTARR(nngp*4)

               index=nngp+2
               GPZONE(1:nngp+1)=index
               FOR I=1,nzone DO BEGIN
                  FOR J=ZONEGP(I),ZONEGP(I+1)-1 DO BEGIN
                  GPC=ZONEGP(J)
                  IF GPZONE(NNGP+1) GT GPZONE(GPC) THEN $
                     GPZONE(GPZONE(GPC)+1:GPZONE(NNGP+1))=GPZONE(GPZONE(GPC):GPZONE(NNGP+1)-1)
                  GPZONE(GPC+1:NNGP+1)=GPZONE(GPC+1:NNGP+1)+1
                  GPZONE(GPZONE(GPC))=I
                  ENDFOR
               ENDFOR
               ngpzone=GPZONE(nngp+1)-1
               PRINTF,unit,nngp,ngpzone,'   Size of GP->zone array'
               PRINTF,unit,GPZONE(1:nngp+1)
               FOR I=1,nngp DO BEGIN
                  PRINTF,unit,I,GPZONE(GPZONE(I):GPZONE(I+1)-1)
               ENDFOR

               CLOSE,unit
               FREE_LUN,unit
            ENDIF
            END

      ENDCASE
      END
   'WIDGET_TEXT_CH': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE ev.id of
        WBathy(1,2):bathy_line=FLOAT(value(0))
        WBathy(2,2):bathy_ker_size=FLOAT(value(0))
      ENDCASE
      END
	'WIDGET_DROPLIST':BEGIN
      CASE ev.id of
      WBathy(6,1): BEGIN
         showdot1=ev.index
         adbathydot=showdot1+showdot2
         END
      WBathy(6,2): BEGIN
         showdot2=ev.index
         adbathydot=showdot1+showdot2
         END
      ENDCASE
      END
   ENDCASE
RETURN
END


;----------------------------------------------------------------------------
PRO bathytool_draw_event,ev
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,nposo
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON BATHYTOOL, bathy_ker_size,bathy_line,bathytool_click
COMMON WBat,Wbathy,rootbtool
COMMON LOCAL_bathytool,coordP,coordR
;*******END OF COMMON BLOCKS*******************************
   CASE ev.type OF	;case loop on action types
      0:BEGIN             ;ev.type=0 : click (button pushed down)
         coordP=CONVERT_COORD(ev.X,ev.Y, /DEVICE,/TO_DATA)
         coordP(0)=MIN([MAX([0.,coordP(0)]),(nx-1)*dx])
         coordP(1)=MIN([MAX([0.,coordP(1)]),(ny-1)*dy])
         END
       1:BEGIN        ;ev.type=1  button release
         coordR=CONVERT_COORD(ev.X,ev.Y, /DEVICE,/TO_DATA)
         coordR(0)=MIN([MAX([0.,coordR(0)]),(nx-1)*dx])
         coordR(1)=MIN([MAX([0.,coordR(1)]),(ny-1)*dy])
         CASE bathytool_click OF
             0:BEGIN ;draw lines
                IF datastatus(3) EQ 1 THEN BEGIN
                   i1=ROUND(coordP(0)/dx)
                   j1=FLOOR(coordP(1)/dy)
                   i2=ROUND(coordR(0)/dx)
                   j2=FLOOR(coordR(1)/dy)
                   nii=max([abs(j2-j1),abs(i2-i1)])+1
                   ddx=(coordR(0)/dx-coordP(0)/dx)/max([(nii-1),1])
                   ddy=(coordR(1)/dy-coordP(1)/dy)/max([(nii-1),1])
                   FOR ii=0,nii DO BEGIN
                      i3=ROUND(coordP(0)/dx+ddx*ii)
                      j3=ROUND(coordP(1)/dy+ddy*ii)
                      gd(i3,j3)=bathy_line
                   ENDFOR
                   ENDIF
                   END
             ELSE:
             ENDCASE
             END

          ; Updates the cursor position and value of the current field
          2:IF datastatus (2) THEN BEGIN  ;ev.type=2 when the cursor is in
                                          ;in the drawing window
             coord=CONVERT_COORD(ev.X,ev.Y, /DEVICE,/TO_DATA)
             i0=FLOOR(coord(0)/dx)
             j0=FLOOR(coord(1)/dy)
             IF ((i0 GE 0) AND (i0 LT nx) $
                AND (j0 GE 0) AND (j0 LT ny) $
                AND datastatus(3)) THEN BEGIN
                valeur=gd(i0,j0)
                WIDGET_CONTROL,Wright(13,5), $
                   SET_VALUE=STRING(valeur,FORMAT='(F8.2)')
             ENDIF
             WIDGET_CONTROL,Wright(13,1), $
                SET_VALUE=STRING(coord(0),FORMAT='(F7.3)')
             WIDGET_CONTROL,Wright(13,3), $
                SET_VALUE=STRING(coord(1),FORMAT='(F6.2)')
             latdeg=0.
             latmin=0.
             londeg=0.
             lonmin=0.
             latlonstring=''
             XYtoLatLon,coord(0),coord(1), $
                latdeg,latmin,londeg,lonmin,latlonstring
             WIDGET_CONTROL,Wright(14,1), $
                SET_VALUE=latlonstring
             ENDIF
             ELSE:
          ENDCASE

RETURN
END

;----------------------------------------------------------------------------
PRO plot_dot_bathy
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,filltype
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON BATHYDOT,ndotmax,ndot1,XDOT1,YDOT1,ZDOT1,ndot2,XDOT2,YDOT2,ZDOT2, $
   bathydot_path,showdot1,showdot2

   zmin=1000.
   zmax=0.
   kount1=0
   kount2=0
   IF showdot1 THEN BEGIN
         PLOTDOT1=INTARR(ndot1)
         FOR I=0L,ndot1-1 DO BEGIN
            IF (XDOT1(I) GE rangex(0) AND XDOT1(I) LE rangex(1) $
               AND YDOT1(I) GE rangey(0) AND YDOT1(I) LE rangey(1)) THEN BEGIN
                  PLOTDOT1(I)=1
                  IF ZDOT1(I) GT zmax THEN zmax=ZDOT1(I)
                  IF ZDOT1(I) LT zmin THEN zmin=ZDOT1(I)
            ENDIF
         ENDFOR
         Index1=WHERE(PLOTDOT1 EQ 1,Kount1)
   ENDIF
   IF showdot2 THEN BEGIN
         PLOTDOT2=INTARR(ndot2)
         FOR I=0L,ndot2-1 DO BEGIN
            IF (XDOT2(I) GE rangex(0) AND XDOT2(I) LE rangex(1) $
               AND YDOT2(I) GE rangey(0) AND YDOT2(I) LE rangey(1)) THEN BEGIN
                  PLOTDOT2(I)=1
                  IF ZDOT2(I) GT zmax THEN zmax=ZDOT2(I)
                  IF ZDOT2(I) LT zmin THEN zmin=ZDOT2(I)
            ENDIF
         ENDFOR
         Index2=WHERE(PLOTDOT2 EQ 1,Kount2)
   ENDIF

   c_numlev=Navailcolor-2
   MakeLevels,zmin,zmax
   colorind=1+FINDGEN(navailcolor-3)
   colorind=navailcolor-1-colorind
   if cbar THEN ColorBar,lev,colorind,cbtit,1,5

   PLOT,[rangex(0)],[rangey(0)],xstyle=5,ystyle=5,/NODATA, $
            XRANGE=rangex,YRANGE=rangey, /NOERASE,$
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
   IF (kount1 GT 0) THEN $
      A = FINDGEN(17) * (!PI*2/16.)    ;Make a vector of 16 points, A[i] = 2pi/16.
      USERSYM, COS(A), SIN(A), /FILL  ;Define the symbol to be a filled circle
            PLOTS,XDOT1(Index1),Ydot1(Index1),PSYM=8,SYMSIZE=0.1, $
            COLOR=1+(Navailcolor-3)*(zmax-ZDOT1(Index1))/(zmax-zmin)
   IF (kount2 GT 0) THEN BEGIN
      A = FINDGEN(17) * (!PI*2/16.)    ;Make a vector of 16 points, A[i] = 2pi/16.
      USERSYM, COS(A), SIN(A), /FILL  ;Define the symbol to be a filled circle
      PLOTS,XDOT2(Index2),Ydot2(Index2),PSYM=8,SYMSIZE=0.8, $
            COLOR=1+(Navailcolor-3)*(zmax-ZDOT2(Index2))/(zmax-zmin)
      A = FINDGEN(17) * (!PI*2/16.)    ;Make a vector of 16 points, A[i] = 2pi/16.
      USERSYM, COS(A), SIN(A)  ;Define the symbol to be a filled circle
      PLOTS,XDOT2(Index2),Ydot2(Index2),PSYM=8,SYMSIZE=0.8
   ENDIF
RETURN
END

;----------------------------------------------------------------------------
PRO Smbat
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON BATHYTOOL, bathy_ker_size,bathy_line,bathytool_click
   L=bathy_ker_size
   I0=FIX(0.5+L/dx)
   J0=FIX(0.5+L/dy)
   print,'dx,dy',dx,dy,I0,J0
   window=FLTARR(401)
   window(0:200)=HANNING(201)
   window(200:400)=HANNING(201)
   window(100:300)=1.
   kernel=FLTARR(401,401)
   FOR I=0,400 DO BEGIN
      FOR J=0,400 DO BEGIN
         Ind=FIX(SQRT(FLOAT(I-200)^2.+FLOAT(J-200)^2.)+0.5)
         IF Ind LE 200 THEN $
            kernel(I,J)=window(200+Ind)
      ENDFOR
   ENDFOR
   ker=CONGRID(kernel,I0,J0);,/MINUS_ONE)
   ker=ker/TOTAL(ker)
   surface,ker
   tempo=gd
   gd=CONVOL(tempo,ker,/CENTER,/EDGE_TRUNCATE)
   Index=WHERE(tempo LT 0,kount)
   IF kount GT 0 THEN gd(index)=tempo(index)
   RETURN
   END
;----------------------------------------------------------------------------
PRO Fillgaps
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
   I0=4
   J0=4
   I1=0
   I2=2*I0
   J1=0
   J2=2*J0
   HOLES=GD GT 5000.
   S = REPLICATE(1, 3, 3)
   FOR I=0,nx-1 DO BEGIN
      PRINT,'I:',I
      FOR J=0,ny-1 DO IF HOLES(I,J) THEN BEGIN
         ;PRINT,'I,J:',I,J
         A=INTARR(2*I0+1,2*J0+1)
         HOL=INTARR(2*I0+1,2*J0+1)
         GDA=FLTARR(2*I0+1,2*J0+1)
         I1=-MIN([I-I0,0])
         I2=2*I0+nx-1-MAX([I+I0,nx-1])
         J1=-MIN([J-J0,0])
         J2=2*I0+ny-1-MAX([J+J0,ny-1])

         HOL(I1:I2,J1:J2)= $
            HOLES(MAX([I-I0,0]):MIN([I+I0,nx-1]),MAX([J-J0,0]):MIN([J+J0,ny-1]))
         GDA(I1:I2,J1:J2)= $
            GD(MAX([I-I0,0]):MIN([I+I0,nx-1]),MAX([J-J0,0]):MIN([J+J0,ny-1]))
         A(*,*)=0.
         A(I0,J0)=1
         FOR K=1,I0 DO BEGIN
            A=DILATE(A,S)*HOL
         ENDFOR
         BORDER=(DILATE(A,S)-A)*(1-HOL)
         WEIGHT=0.
         GD(I,J)=0.
         FOR II=I1,I2 DO BEGIN
            FOR JJ=J1,J2 DO BEGIN
               IF BORDER(II,JJ) THEN BEGIN
                  GD(I,J)=GD(I,J)+GDA(II,JJ)*EXP(-((II-I0)^2+(JJ-J0)^2)/8)
                  WEIGHT=WEIGHT+EXP(-((II-I0)^2+(JJ-J0)^2)/8)
               ENDIF
            ENDFOR
         ENDFOR
         ;PRINT,'BORDER:'
         ;PRINT,BORDER(I1:I2,J1:J2)
         IF WEIGHT NE 0. THEN GD(I,J)=GD(I,J)/WEIGHT ELSE GD(I,J)=1.E4
         ;PRINT,'GD:',GD(I,J)
      ENDIF
   ENDFOR
   GET_LUN,unit
   OPENW,unit,'/d/span11/ardhuin/MULTIBEAM/square2_filled.grd'
      WRITEU,unit,transpose(gd)
   CLOSE,unit
   FREE_LUN,unit
   RETURN
   END
