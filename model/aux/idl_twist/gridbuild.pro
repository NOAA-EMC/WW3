;/
;/                  +-----------------------------------+
;/                  | TWIST                Ifremer-SHOM |
;/                  |           F. Ardhuin              |
;/                  |              IDL command language |
;/                  | Last update :         08-Oct-2013 |
;/                  +-----------------------------------+
;/
;/ Licence information: This code is distributed under the CeCILL license
;/                      generally compatible with the Gnu Public Licence (GPL) 
;/                      http://www.cecill.info/index.en.html
;/
;/    17-Oct-2013 : Clean up                            ( version 2.00 )
;/    08-Nov-2013 : Debug contourline                   ( version 2.00 )
;
;  1. Purpose :
;
;     GUI for mesh editing
;
;  2. Method :
;
;----------------------------------------------------------------------------
PRO Generator, anterior_root
; This procedure creates the widged for the "grid designer" subwindow
;*******COMMON BLOCKS**************************************
;** 1 ** Display parameters
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
;** 3 ** I/O and data variables
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON GRIDTIMESTEPS, zoffset, updateonmove, waveperiod
;** 4 ** subwindow widgets widgets
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
COMMON WGene,Wgrideditor,WG,root
COMMON Builder,ngp,H0,H1,Hi,KH0,KH1,DX0,DX1,T0,ROA,ROI,gridbuild_click

;*******END OF COMMON BLOCKS*******************************
   IF (datastatus(6) EQ 0) THEN nzone=5
   zcolor=INTARR(nzone+1)
   zcolor(1:4)=FINDGEN(4)*10+10
   zcolor(5)=-1.
   c_zone=5
   pbase=LONG(0)
      vecbase=LONG(0)
      Line1=LONG(0)
      WGlines=LONARR(10)
   WG=LONARR(12,14)

   IF (N_ELEMENTS(contourline) EQ 0) THEN contourline=LONARR(2,2)-1
   Speeds,H0,1/T0,cphi,cg,KH0
   Speeds,H1,1/T0,cphi,cg,KH1
   IF (N_ELEMENTS(gridmat) EQ 0) THEN BEGIN
      nngp=1
      gridmat=DBLARR(8,nngp)
      x=0.
      y=0.
      XYtoLATLON,x,y,latdeg,latmin,londeg,lonmin
      I=1
      gridmat(0,I-1)=x
      gridmat(1,I-1)=y
      gridmat(2,I-1)=latdeg
      gridmat(3,I-1)=latmin
      gridmat(4,I-1)=londeg
      gridmat(5,I-1)=lonmin
   ENDIF


; Guardar valores en variables temporales
      root=anterior_root
;      WIDGET_CONTROL, root, SENSITIVE=0

   pbase = WIDGET_BASE(TITLE='Mesh editor', /COLUMN)
   WIDGET_CONTROL, pbase, GROUP_LEADER=root
   WIDGET_CONTROL, /MANAGED, pbase
      Wgrideditor=LONARR(2)
      ROWLAB=STRARR(nngp)
      ROWLAB(*)=STRCOMPRESS(STRING(INDGEN(nngp)+1))
      IF (nngp EQ 1) THEN ROWLAB=['1'] ELSE ROWLAB = STRING(FINDGEN(nngp)+1, FORMAT='(I8)')
      Wgrideditor(1)= $
         WIDGET_TABLE(pbase, /EDITABLE, $
         COLUMN_LABELS=['X','Y','LAT','min','LON','min','contour','depth'], $
          ROW_LABELS=ROWLAB, YSIZE=max([10,nngp]),XSIZE=8, /RESIZEABLE_COLUMNS,  $
         /SCROLL,Y_SCROLL_SIZE=10,X_SCROLL_SIZE=8, $
         COLUMN_WIDTHS=[80,80,80,80,80,80,60,90],UNITS=0)
      WIDGET_CONTROL,Wgrideditor(1) ,SET_VALUE= gridmat


      I=0
      WGlines(I)=WIDGET_BASE(pbase, /FRAME, /ROW)
         WG(I,0)=WIDGET_LABEL(WGlines(I),VALUE='Number of mesh nodes: ')
         WG(I,3)=WIDGET_TEXT(WGlines(I), XSIZE=8, YSIZE=1, $
            VALUE=STRCOMPRESS(String(nngp)))
         WG(I,5)=WIDGET_LABEL(WGlines(I),VALUE='              ')
         WG(I,6)=WIDGET_BUTTON(WGlines(I),VALUE='UNDO')

      I=2
      WGlines(I)=WIDGET_BASE(pbase, /FRAME, /ROW)
         WG(2,0)=WIDGET_LABEL(WGlines(I),VALUE='Mesh and timesteps:')
         WG(2,1)=WIDGET_DROPLIST(WGlines(I), $
            VALUE=[  'Read model timesteps file', $
                     'Update model timesteps', $
                      'Optimize current node position', $
                     'Optimize nodes position (top 20)'])
         WG(2,2)=WIDGET_LABEL(WGlines(I),XSIZE=5,YSIZE=1,VALUE='z-offset:')
         WG(2,3)=WIDGET_TEXT(WGlines(I),XSIZE=7,YSIZE=1,/EDITABLE,VALUE=strcompress(STRING(zoffset)))
         WG(2,4)=WIDGET_BASE(WGlines(I), /ROW,/NonExclusive)
         WG(2,5)=Widget_Button(WG(2,4), Value='Update on move')
         Widget_Control, WG(2,5), Set_Button=1


      I=3
      WGlines(I)=WIDGET_BASE(pbase, /FRAME, /ROW)
         WG(I,0)=WIDGET_LABEL(WGlines(I),VALUE='Work on nodes / contour: ')
         WG(I,1)=WIDGET_DROPLIST(WGlines(I), $
            VALUE=[  'Initialize to specials from *.grgp', $
                     'Import polymesh rand and insel', $
                     'Initialize to zero', $
                     'Add open contour (DX1 in km)', $
                     'Force open contour (DX1 in km)', $
                     'Remove islands from contour', $
                     'Check for segment crossing', $
                     'Flip ordering of island', $
                     'Filter contour points ( < DX0/2)', $
                     'Remove islands out of contour', $
                     'Add depth line (Hi,DX0 in km):', $
                     'Draw line (DX0 in km)', $
                     'Fill with nodes', $
                     'Updates depths of nodes'])
         WG(I,2)=WIDGET_LABEL(WGlines(I),VALUE='In:')
         WG(I,3)=WIDGET_DROPLIST(WGlines(I), $
            VALUE=[  'Whole', $
                     'Zoom', $
                     'c_zone'])

         WG(I,4)=WIDGET_BUTTON(WGlines(I),VALUE='Zoom on point')
         WG(I,5)=WIDGET_BUTTON(WGlines(I),VALUE='Zoom -')

      I=4
         WGlines(I)=WIDGET_BASE(pbase, /FRAME, /ROW)
         WG(I,0)=WIDGET_LABEL(WGlines(I),VALUE='Work on mesh: ')
         WG(I,1)=WIDGET_DROPLIST(WGlines(I), $
            VALUE=[  'Remove selected node with 4 neighbors', $
                     'Remove selected non-bound. node with 3 neighbors', $
                     'Remove selected boundary node with 3 neighbors', $
                     'Remove selected boundary node with 2 neighbors', $
                     'Remove all nodes with 4 neighbors',  $
                     'Remove bound. nodes with 3 neigh. (top 200 only)',  $
                     'Remove all bound. nodes with 3 neighbors',  $
                     'Remove all bound. nodes with 2 neighbors', $
                     'Re-generates outside contour, set Z.', $
                     'Re-generates land+islands depths, set Z.' ]) 
         WG(I,3)=WIDGET_LABEL(WGlines(I),VALUE='Save file:')
         WG(I,4)=WIDGET_DROPLIST(WGlines(I), $
            VALUE=['grid (grgp)','mesh (msh)', $
                  'polymesh rand and insel','shoreline slope','slope grid'])


      I=5
      WGlines(I)=WIDGET_BASE(pbase, /FRAME, /ROW)
         WG(I,12)=WIDGET_LABEL(WGlines(I),VALUE='H land/sea:')
         WG(I,13)=WIDGET_TEXT(WGlines(I), $
            XSIZE=5,/EDITABLE,VALUE=strcompress(STRING(Hlandsea)))
         WG(I,0)=WIDGET_LABEL(WGlines(I),VALUE='H0:')
         WG(I,1)=WIDGET_TEXT(WGlines(I), $
            XSIZE=5,/EDITABLE,VALUE=strcompress(STRING(H0)))
         WG(I,10)=WIDGET_LABEL(WGlines(I),VALUE='Hi:')
         WG(I,11)=WIDGET_TEXT(WGlines(I), $
            XSIZE=5,/EDITABLE,VALUE=strcompress(STRING(Hi)))
         WG(I,8)=WIDGET_LABEL(WGlines(I),VALUE='H1:')
         WG(I,9)=WIDGET_TEXT(WGlines(I), $
            XSIZE=5,/EDITABLE,VALUE=strcompress(STRING(H1)))
         WG(I,2)=WIDGET_LABEL(WGlines(I),VALUE='DX0:')
         WG(I,3)=WIDGET_TEXT(WGlines(I), $
            XSIZE=5,/EDITABLE,VALUE=strcompress(STRING(DX0)))
         WG(I,4)=WIDGET_LABEL(WGlines(I),VALUE='DX1:')
         WG(I,5)=WIDGET_TEXT(WGlines(I), $
            XSIZE=5,/EDITABLE,VALUE=strcompress(STRING(DX1)))
         WG(I,6)=WIDGET_LABEL(WGlines(I),VALUE='T0:')
         WG(I,7)=WIDGET_TEXT(WGlines(I), $
            XSIZE=5,/EDITABLE,VALUE=strcompress(STRING(T0)))

      I=8

      WGlines(8)=WIDGET_BASE(pbase, /FRAME, /ROW)
      WG(I,0)=WIDGET_BUTTON(WGlines(I), VALUE='Move GPs')
      WG(I,1)=WIDGET_BUTTON(WGlines(I), VALUE='End of moves')
      WG(I,2)=WIDGET_BUTTON(WGlines(I), VALUE='Add GPs')
      WG(I,3)=WIDGET_BUTTON(WGlines(I), VALUE='End of adds')
      WG(I,3)=WIDGET_BUTTON(WGlines(I), VALUE='Clean GP notri')
      WG(I,5)=WIDGET_BUTTON(WGlines(I), VALUE='Add GPspec')
      WG(I,6)=WIDGET_BUTTON(WGlines(I), VALUE='Del GP (table)')
      WG(I,7)=WIDGET_BUTTON(WGlines(I), VALUE='Del GP (screen)')

      I=6
      WGlines(I)=WIDGET_BASE(pbase, /FRAME, /ROW)
         WG(I,0)=WIDGET_LABEL(WGlines(I),VALUE='Make contour:')
         WG(I,1)=WIDGET_DROPLIST(WGlines(I), $
            VALUE=[  'at Hlandsea (DX0)', $
                     'at coastline (DX0)'])
         WG(I,2)=WIDGET_LABEL(WGlines(I),VALUE='Make triangles:')
         WG(I,3)=WIDGET_DROPLIST(WGlines(I), $
            VALUE=[  'rough (with land)', $
                     'clean (no land)', 'full (with zones)'])

         Line7 = WIDGET_BUTTON(pbase, VALUE='Close mesh editor')
            WIDGET_CONTROL, /REALIZE, pbase
         Displaygrid
         subwin(3)=1
         XMANAGER, 'GENERATOR', /NO_BLOCK, pbase
RETURN
END


;----------------------------------------------------------------------------
PRO Generator_event, ev
;*******COMMON BLOCKS**************************************
;** 1 ** Display parameters
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON SPACE,   c_gp,c_cut,indexgp,c_x,c_y,c_lon,c_lat
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth
;** 3 ** I/O and data variables
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
;** 4 ** subwindow widgets widgets
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
COMMON WGene,Wgrideditor,WG,root
COMMON Builder,ngp,H0,H1,Hi,KH0,KH1,DX0,DX1,T0,ROA,ROI,gridbuild_click
COMMON LOCAL_tri,LIST
COMMON UNDO,gridmat0,nngp0,ntri0,trigp0,contourline0
COMMON COAST,   coastxy,coastl,coastnp,GSHHSPoly,GSHHSPoint,GSHHSPoly2,GSHHSPoint2
COMMON SHORELINE, slopes
COMMON SHORELINE2, open_boundary
COMMON TIMESTEPSUG,TIMESTEPS,NTIMESTEPS,FLAGTSTEP, TIMESTEPS_ORDER
COMMON TIMESTEPSUG3, SIRATIO, JCELLS, LEN, ANGLE0, CROSSDIFF, TIMESTEPS_ALL
COMMON GRIDTIMESTEPS, zoffset, updateonmove, waveperiod
;*******END OF COMMON BLOCKS*******************************

   file1=''
   type = TAG_NAMES(ev, /STRUCTURE)
   ON_IOERROR, not_OPEN
 
;
; Prepares for UNDO 
;
   IF (ev.id NE WG(0,6)) THEN BEGIN
      gridmat0=gridmat
      nngp0=nngp
      IF (N_ELEMENTS(ntri) GT 0) THEN ntri0=ntri
      IF (N_ELEMENTS(TRIGP) GT 0) THEN TRIGP0=TRIGP
      IF (N_ELEMENTS(contourline) GT 0) THEN contourline0=contourline
   ENDIF
;
; Performs actions
;
   CASE type OF
   'WIDGET_BUTTON': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE value OF
      'Update on move': updateonmove = 1-updateonmove; 
      'Close mesh editor': BEGIN
         WIDGET_CONTROL, /DESTROY, ev.top
               WIDGET_CONTROL,root, SENSITIVE=1
         subwin(0)=0
         RETURN
            END
            'Choose GP #1':BEGIN
         WIDGET_CONTROL,Wdraw,EVENT_PRO='gridbuild_draw_event'
         gridbuild_click=0
         END

      'UNDO':BEGIN
         gridmat=gridmat0
         nngp=nngp0
         IF (N_ELEMENTS(ntri0) GT 0) THEN BEGIN
           ntri=ntri0
           trigp=trigp0
         ENDIF
         IF (N_ELEMENTS(contourline0) GT 0) THEN BEGIN
           contourline=contourline0
         ENDIF
         IF (N_ELEMENTS(TRIGP0) GT 0) THEN BEGIN
           TRIGP=TRIGP0
         ENDIF
         Displaygrid
         END
            'Move GPs':BEGIN
         WIDGET_CONTROL,Wdraw,EVENT_PRO='gridbuild_draw_event'
         gridbuild_click=1
         END
      'End of moves':BEGIN
         WIDGET_CONTROL,Wdraw,EVENT_PRO='analyzer_event'
         END
      'Add GPs':BEGIN
         WIDGET_CONTROL,Wdraw,EVENT_PRO='gridbuild_draw_event'
         gridbuild_click=2
         FOUND=BYTARR(nngp)
         FOR I=0,ntri-1 DO BEGIN
            FOUND(TRIGP(I,1:3)-1)=1
         ENDFOR
         IND=WHERE(FOUND EQ 0,kount)
         IF (kount GT 0) THEN PRINT,'IND IN NO TRI:',IND+1
         END
      'End of adds':BEGIN
         WIDGET_CONTROL,Wdraw,EVENT_PRO='analyzer_event'
         END
      'Add GPspec':BEGIN
         IF N_ELEMENTS(specmat) NE 0 THEN BEGIN
            nngp=nngp+1
            temp=gridmat
            gridmat=DBLARR(8,nngp)
            gridmat(*,0:nngp-2)=temp
            gridmat(0:5,nngp-1)=specmat(1:6,c_spec)
            print,'New GP:',specmat(1:6,c_spec)
            ROWLAB=STRCOMPRESS(STRING(FINDGEN(nngp)+1,FORMAT='(I6)'))
            gridf=STRARR(6,nngp)
            gridf(0:1,*)='(F8.4)'
            gridf(2,*)='(I4)'
            gridf(3,*)='(F7.3)'
            gridf(4,*)='(I5)'
            gridf(5,*)='(F10.2)'

            WIDGET_CONTROL,WGrideditor(1),INSERT_ROW=1
            Displaygrid
            WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp-1))
         ENDIF
         END

      'Zoom on point':BEGIN  ; Zooms in on point
       selec=WIDGET_INFO(Wgrideditor(1),/TABLE_SELECT)
         XC=gridmat(0,selec(1))
       YC=gridmat(1,selec(1))
       ;DELX=(nx-1)*dx*(xll-rlonmin)/ABS(rlonmax-rlonmin)
       DDX=ROUND(FLOAT(nx)/100.)
       DDY=ROUND(FLOAT(ny)/100.)
       PRINT,'xc,yc:',xc,yc,ddx,ddy
         nxzmin=MIN([MAX([0,ROUND((xc)/dx)-ddx]),nx-1])
         nyzmin=MIN([MAX([0,ROUND((yc)/dy)-ddy]),ny-1])
         nxzmax=MIN([MAX([0,ROUND((xc)/dx)+ddx]),nx-1])
         nyzmax=MIN([MAX([0,ROUND((yc)/dy)+ddy]),ny-1])
          print,'ZOOM P:',nxzmin,nxzmax,nyzmin,nyzmax
         doplot
       END
      'Zoom -':BEGIN  ; Zooms out of point
       selec=WIDGET_INFO(Wgrideditor(1),/TABLE_SELECT)
         XC=gridmat(0,selec(1))
       YC=gridmat(1,selec(1))
       ;DELX=(nx-1)*dx*(xll-rlonmin)/ABS(rlonmax-rlonmin)
       DDX=ROUND(FLOAT(nx)/30.)
       DDY=ROUND(FLOAT(ny)/30.)
       PRINT,'xc,yc:',xc,yc,ddx,ddy
         nxzmin=MIN([MAX([0,ROUND((xc)/dx)-ddx]),nx-1])
         nyzmin=MIN([MAX([0,ROUND((yc)/dy)-ddy]),ny-1])
         nxzmax=MIN([MAX([0,ROUND((xc)/dx)+ddx]),nx-1])
         nyzmax=MIN([MAX([0,ROUND((yc)/dy)+ddy]),ny-1])
          print,'ZOOM P:',nxzmin,nxzmax,nyzmin,nyzmax
         doplot
       END
      'Del GP (table)':BEGIN  ; Deletes the selected range of grid points
         selec=WIDGET_INFO(Wgrideditor(1),/TABLE_SELECT)
         idel=selec(3)-selec(1)+1  ;number of deleted grid points
             ;      print,'selec',selec,'idel:',idel
         WIDGET_CONTROL,Wgrideditor(1),/DELETE_ROWS , $
            USE_TABLE_SELECT=[-1,selec(1),5,selec(3)]
         nngp=nngp-idel ;new number of grid points
         tempo=gridmat
         gridmat=DBLARR(8,nngp)
         IF selec(1) gt 0 THEN gridmat(*,0:selec(1)-1)=tempo(*,0:selec(1)-1)
         IF selec(1) LT nngp THEN gridmat(*,selec(1):nngp-1)=tempo(*,selec(3)+1:nngp+idel-1)
         MAKE_CONTOUR
         WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
         Displaygrid
         END
;
      'Del GP (screen)':BEGIN  ; Deletes the selected points
         PRINT,'Point ',C_GP
         IF (C_GP GT 0 ) THEN BEGIN 
            PRINT,'Point ',C_GP
            idel= 1;number of deleted grid points
            WIDGET_CONTROL,Wgrideditor(1),/DELETE_ROWS , $
            USE_TABLE_SELECT=[-1,C_GP-1,5,C_GP-1]
         nngp=nngp-idel ;new number of grid points
         tempo=gridmat
         gridmat=DBLARR(8,nngp)
         IF C_GP gt 1 THEN gridmat(*,0:C_GP-2)=tempo(*,0:C_GP-2)
         IF C_GP LT nngp THEN gridmat(*,C_GP-1:nngp-1)=tempo(*,C_GP:nngp)
         MAKE_CONTOUR
         WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
         Displaygrid
         PRINT,'Point ',C_GP,' has been deleted.'
         C_GP=0
         ENDIF
         END
       'Clean GP notri':BEGIN  ;
         FOUND=BYTARR(nngp)
         FOR I=0,ntri-1 DO BEGIN
            FOUND(TRIGP(I,1:3)-1)=1
         ENDFOR
         IND=WHERE(FOUND EQ 0,kount)
         IF (kount GT 0) THEN BEGIN
            IND2=REVERSE(SORT(IND))
            PRINT,'IND:',IND2+1
            FOR I=0,kount-1 DO BEGIN
               tempo=gridmat
               nngp=nngp-1
               gridmat=DBLARR(8,nngp)
               J=IND(IND2(I))
               PRINT,'REMOVING IND:',IND2(I),IND(IND2(I))

               IF J gt 0 THEN gridmat(*,0:J-1)=tempo(*,0:J-1)
               IF J LT nngp-1 THEN gridmat(*,J:nngp-1)=tempo(*,J+1:nngp)
            ENDFOR
         ENDIF
         MAKE_CONTOUR
         END

      ENDCASE
      END
      'WIDGET_TABLE_CH': BEGIN
      CASE (ev.id) OF
            Wgrideditor(1):BEGIN
         IF (ev.type LE 2) THEN BEGIN
            WIDGET_CONTROL, ev.id, GET_VALUE=valeur, $
            USE_TABLE_SELECT=[ev.x,ev.y,ev.x,ev.y]
            ;/USE_TEXT_SELECT
            gridmat(ev.X,ev.Y)=valeur(0,0)
            Updatetable,ev.X,ev.Y
            IF (ev.X EQ 6) THEN MAKE_CONTOUR
         ENDIF
         END
      ENDCASE
      END
   'WIDGET_TEXT_CH': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE ev.id OF
      WG(5,1):BEGIN
         H0=FLOAT(value(0))
         Speeds,H0,1/T0,cphi,cg,KH0
         END
      WG(2,3):zoffset=FLOAT(value(0))
      WG(5,9):BEGIN
         H1=FLOAT(value(0))
         Speeds,H1,1/T0,cphi,cg,KH1
         END



      WG(5,11):BEGIN
         Hi=FLOAT(value(0))
         Speeds,Hi,1/T0,cphi,cg,KHi
         END
      WG(5,13):Hlandsea=FLOAT(value(0))
      WG(5,3):DX0=FLOAT(value(0))
      WG(5,5):DX1=FLOAT(value(0))
      WG(5,7):BEGIN
         T0=FLOAT(value(0))
         Speeds,H0,1/T0,cphi,cg,KH0
         Speeds,H1,1/T0,cphi,cg,KH1
         END

      WG(2,1): gfact=FLOAT(value(0))
       ENDCASE
      END
;
; Management of droplists
;
      'WIDGET_DROPLIST':BEGIN
         CASE ev.id of
         WG(3,3) : ROA=ev.index
         WG(6,1) : BEGIN
            nngp_old2=nngp
            IF (ev.index EQ 0 AND N_ELEMENTS(gd) GT 0) THEN BEGIN
;
; Gets a depth contour 
;
               x=findgen(nx)*dx
               y=findgen(ny)*dy
               CONTOUR,gd,x,y,LEVELS=Hlandsea,PATH_INFO=info,PATH_XY=xyn,CLOSED=0
; Converts pixel units to kilometers
               xy=CONVERT_COORD(xyn,/NORMAL,/TO_DATA)

;
; Transforms the coastline data to the same format as the contours
;
            ENDIF ELSE BEGIN
               x=findgen(nx)*dx
               y=findgen(ny)*dy
               taille=SIZE(GSHHSPoly)
               taille2=SIZE(GSHHSPoint)
               Npolymax=taille(1)-1
               cont1={contourc,TYPE:1,HIGH_LOW:1,LEVEL:1, $
                   N:1L,OFFSET:1L,VALUE:0.d}
               info=REPLICATE(cont1,Npolymax)
               Npointmax=taille2(1)
               XY=FLTARR(2,Npointmax)
               Npoly=0L
               Npoint=0L
               FOR I=0L,Npolymax-1 DO BEGIN
                  info(Npoly).TYPE=1
                  Npoint_this=0L
                  FOR J=GSHHSPoly(I,0),GSHHSPoly(I+1,0)-1 DO BEGIN
;                    IF (GSHHSPoint(J,0) GE 0) AND  (GSHHSPoint(J,0) LE dx*(n-1)) $
;                        AND (GSHHSPoint(J,1) GE 0) AND (GSHHSPoint(J,1) LE dy*(ny-1)) THEN BEGIN
                        IF (GSHHSPoint(J,0) GE dx*nxzmin) AND  (GSHHSPoint(J,0) LE dx*nxzmax) $
                           AND (GSHHSPoint(J,1) GE dy*nyzmin) AND (GSHHSPoint(J,1) LE dy*nyzmax) THEN BEGIN
                           XY(0:1,Npoint+Npoint_this)=GSHHSPoint(J,0:1)
                           Npoint_this=Npoint_this+1
                        ENDIF ELSE BEGIN
;
; Detects the boundary crossing: TYPE=0
;
                        info(Npoly).TYPE=0
                     ENDELSE
                  ENDFOR
                  info(Npoly).OFFSET=Npoint
                  Npoint=Npoint+Npoint_this
                  info(Npoly).N=Npoint_this
                  IF (Npoint_this GT 0) THEN Npoly=Npoly+1
               ENDFOR
;
               IF (NPOLY EQ 0) THEN BEGIN 
                 Print,'Did not find any polygon in zoom window'
                 RETURN
               ENDIF

               info_old=info
               info=REPLICATE(cont1,Npoly)
               info(0:Npoly-1)=info_old(0:Npoly-1)
            ENDELSE
;
; Now works with the contour or coastline data
;
           Outdump=0
        
           IF (outdump EQ 1) THEN BEGIN 
              GET_LUN,unit1
              OPENW,unit1,'boundarystuff.txt'
           ENDIF
           contorder=LONARR(N_ELEMENTS(info.TYPE))
           naddmax=0
           FOR I = 0, (N_ELEMENTS(info.TYPE) - 1 ) DO BEGIN
              contorder(I)=info(I).N*((2*info(I).TYPE-1L))
              naddmax=naddmax+info(I).N
           ENDFOR
           I=WHERE(contorder GT 0,kount)
           IF (kount GT 0) THEN contorder(I)=max(contorder)-contorder(I)
           addpoints=DBLARR(2,naddmax)
           naddpoint=0
;
;2. Search for boundary intersections
;
;2.1 Sorts the polygons to put first those that cross the boundary (TYPE=0)
;
           contorder1=SORT(contorder)
;
;2.2 Searches the polygons
;
           FOR J2 = 0, (N_ELEMENTS(info) - 1 ) DO BEGIN
             I=contorder1(J2)
             S=0.
             naddJ=0L
  ; Sub-samples the points on the depth contour to make sure that no two points are too close
  ; further refined to avoid segments that are too long ? 
             X0=xy(0,INFO(I).OFFSET)
             Y0=xy(1,INFO(I).OFFSET)
;
; First computes the length of the contour
;
             FOR J=1L,info(I).N-1 DO BEGIN
                X1=xy(0,INFO(I).OFFSET+J)
                Y1=xy(1,INFO(I).OFFSET+J)
                LENGTH=SQRT((X1-X0)^2+(Y1-Y0)^2)
                S=S+LENGTH
                X0=X1
                Y0=Y1
             ENDFOR
;
; Only add points along islands that are long enough
;
             IF (S GT 2.5*DX0) THEN BEGIN 
                XB=-9000
                YB=-9000
                XPREV=-9000
                YPREV=-9000
                S=DX0
                naddJ=0L
                X0=xy(0,INFO(I).OFFSET+info(I).N-1)
                Y0=xy(1,INFO(I).OFFSET+info(I).N-1)
                FOR J=info(I).N-2,0,-1L DO BEGIN
                   X1=xy(0,INFO(I).OFFSET+J)
                   Y1=xy(1,INFO(I).OFFSET+J)
                   IF ((X1 NE x(0)) AND (X1 NE MAX(x)) AND $
                      (Y1 NE Y(0)) AND (Y1 NE MAX(Y))) THEN BEGIN 
              
                      LENGTH=SQRT((X1-X0)^2+(Y1-Y0)^2)
                      S=S+LENGTH
;
; We should probably add some other constraints (change in angle ...)
                      IF (S GT DX0) THEN BEGIN
                        addOK1=0
                        IF (naddJ GT 0L) THEN BEGIN
;
; Checks distance with first added point on the island and previous added point
;
                          L2=SQRT((X1-XB)^2+(Y1-YB)^2)
                          L1=SQRT((X1-XPREV)^2+(Y1-YPREV)^2)
                          IF (L2 GT 0.8*DX0 AND L1 GT DX0 ) THEN addOK1=1
                        ENDIF ELSE  BEGIN 
                          addOK1=1
                        ENDELSE
                        IF (addOK1 EQ 1) THEN BEGIN 
                            naddJ=naddJ+1L
                            WEIGHT=(DX0-(S-LENGTH))
                            addpoints(0,naddpoint-1+naddJ)=X1
                            addpoints(1,naddpoint-1+naddJ)=Y1
                            XPREV=X1
                            YPREV=Y1
                            IF (naddJ EQ 1) THEN BEGIN 
                              XB=X1
                              YB=Y1
                            ENDIF 
                            S=0.
                        ENDIF ; addOK1
                      ENDIF  ; S GT DX0 
                   ENDIF ELSE BEGIN
                      S=0
                   ENDELSE                     
                   X0=X1
                   Y0=Y1
                ENDFOR
                IMIN=0

                PRINT,'Adding nodes along contour? ',J2,naddj
;
; Removes islands with 2 points or less 
;
                IF (naddJ GT 2) THEN BEGIN
;
; Corrects for crossing points ... now only in additional list: should probably do
; over the entire contourline ... 
;
                   FOR ITER=0,4 DO BEGIN
                   NCROSS=0L
                   FOR J=0L,naddJ-2L DO BEGIN
                   X1=addpoints(0,naddpoint+J)
                   Y1=addpoints(1,naddpoint+J)
                   X2=addpoints(0,naddpoint+J+1)
                   Y2=addpoints(1,naddpoint+J+1)
                   K=0L
                   WHILE (K LT J-1L) DO BEGIN
                      X3=addpoints(0,naddpoint+K)
                      Y3=addpoints(1,naddpoint+K)
                      X4=addpoints(0,naddpoint+K+1)
                      Y4=addpoints(1,naddpoint+K+1)

                      DENOM=(Y4-Y3)*(X2-X1)-(Y2-Y1)*(X4-X3)
                      IF ABS(DENOM) GT 1E-8 THEN BEGIN 
;
; P and Q are the coordinates of the crossing point along the two segments
;
                         P=((X3-X1)*(Y4-Y3)-(Y3-Y1)*(X4-X3))/DENOM
                         Q=((X3-X1)*(Y2-Y1)-(Y3-Y1)*(X2-X1))/DENOM
                         IF (( P GT 0 AND P LT 1 ) AND (Q GT 0 AND Q LT 1)) THEN BEGIN 
;
; Flips the crossed part... 
;
                            NCROSS=NCROSS+1L
                            TEMPO=addpoints(*,naddpoint+K+1L:naddpoint+J)
                            FOR KK=0L,J-K-1L DO BEGIN 
                               addpoints(*,naddpoint+K+1L+KK)=TEMPO(*,J-K-1L-KK)
                            ENDFOR
                         ENDIF
                      ENDIF
                      K=K+1L  
                   ENDWHILE
                   ENDFOR
                   IF NCROSS EQ 0L THEN BREAK
                   ENDFOR ;ITER

                   IF info(I).TYPE EQ 0  THEN BEGIN
                      XALL=addpoints(0,naddpoint:naddpoint-1+naddJ)
                      YALL=addpoints(1,naddpoint:naddpoint-1+naddJ)
                      XG=gridmat(0,contourline(contourline(1,0),0))
                      YG=gridmat(1,contourline(contourline(1,0),0))
;
; FIX THIS : distances to window frame
;
                   IF ( (nx-1)*dx-XG LT DX0)  THEN $
                     DIST2=MAX(XALL , IMIN)  ELSE $
                        DIST2=SQRT( MIN( (XALL-XG)^2 + (YALL-YG)^2,IMIN) )
               ENDIF

               IF (J2 LT 10) THEN print,'Adding coastline on contour:',J2,info(I).TYPE,naddJ,contourline(1,0)+naddJ,IMIN
               INDEX=FINDGEN(naddJ)
               gridmatold=gridmat
               contold=contourline
               nngpold=nngp
               nngp=LONG(nngp)+LONG(naddJ)
               gridmat=DBLARR(8,nngp)
               ncontold=1+contold(1)
               ncontnew=1+contold(1)+naddJ
               contourline=LONARR(ncontold+naddJ,2)
; updates header of contourline
               contourline(0:1,0:1)=contold(0:1,0:1)
               contourline(1,0)=contourline(1,0)+naddJ
; copies points before boundary points
               IF contold(0,0) GT 0 THEN $ 
                  gridmat(*,0:contold(0,0)-1)=gridmatold(*,0:contold(0,0)-1)
; copies boundary points before last gap (included)
               I1=contold(contold(0,0),0)
               I2=contold(contold(1,0),0)
               gridmat(*,I1:I2)=gridmatold(*,I1:I2)
               contourline(2:ncontold-1,*)=contold(2:ncontold-1,*)
               ;contourline(2:ncontold-1,0)=FINDGEN(ncontold-2)+contold(0,0)
; adds new points at last gap
               IND=contold(contold(1,0),0)+1
               IF (J2 GT 0) THEN BEGIN 
                  gridmat(0:1,IND:IND+naddJ-1)=addpoints(*,naddpoint+REVERSE(SHIFT(INDEX,-IMIN))) 
               ENDIF ELSE BEGIN 
                  gridmat(0:1,IND:IND+naddJ-1)=addpoints(*,naddpoint+SHIFT(INDEX,-IMIN))
               ENDELSE
               contourline(ncontold:ncontold+naddJ-1,0)=FINDGEN(naddJ)+contold(1,0)-1
               contourline(ncontold-2:ncontold+naddJ-1,1)=1
               IF (info(I).TYPE NE 0) THEN BEGIN
                  contourline(ncontold,1)=-1
               ENDIF ELSE BEGIN 
                  contourline(ncontold:ncontold+naddJ-1,1)=1
               ENDELSE
               gridmat(6,contourline(ncontold:ncontold+naddJ-1,0))  $
                      =contourline(ncontold:ncontold+naddJ-1,1)
; 
; Checks distance with respect to contour
;
               DIST1=SQRT( (gridmat(0,contold(1,0)+1)-gridmat(0,contold(1,0)))^2 + $
                           (gridmat(1,contold(1,0)+1)-gridmat(1,contold(1,0)))^2 )
               IF info(I).TYPE EQ 0L AND DIST1 LT DX1 THEN BEGIN
                  contourline(ncontold+1:ncontold+2,1)=1
                  gridmat(6,contourline(ncontold+2,1))=1
               ENDIF
               IF (J2 LT 10) THEN PRINT,'HEY2
               IF (contold(1,0) LT nngpold-1) THEN $
                  gridmat(*,contold(1,0)+naddJ+1:nngp-1)=gridmatold(*,contold(1,0)+1:nngpold-1)
               IF (J2 LT 10) THEN  print,'New contourline:' ;, FIX(contourline) ; ,FORMAT='(10I8)'
               IF (J2 LT 10) THEN PRINT,'HEY3'
            ENDIF ; (naddJ GT 0)
            ENDIF
         ENDFOR
         IF (outdump EQ 1) THEN BEGIN 
           CLOSE,unit1
           FREE_LUN,unit1
         ENDIF 
 
         FOR I=contourline(contourline(0,0),0),contourline(contourline(1,0),0) DO BEGIN
             XYtoLatLon,gridmat(0,I),gridmat(1,I),latdeg,latmin,londeg,lonmin
             gridmat(2,I)=latdeg
             gridmat(3,I)=latmin
             gridmat(4,I)=londeg
             gridmat(5,I)=lonmin
         ENDFOR
         Displaygrid
      END
      WG(6,3) : BEGIN
         CASE ev.index OF
         0:MakeTriangles,0,0
         1:MakeTriangles,1,0
         2:MakeTriangles,1,1
         ENDCASE
         END
;
; Droplist actions 
;
      WG(4,1):BEGIN
         CASE ev.index OF
;
; Removing node that is in 4 triangles (4 neighbor nodes)
; 
         0: BEGIN
            IF (C_GP GT 0 ) THEN BEGIN 
               MESH_CLEAN_4,C_GP
               C_GP=0
            ENDIF
            END
;
; Removing node that is in 3 triangles (3 neighbor nodes)
; 
         1: BEGIN
            IF (C_GP GT 0 ) THEN BEGIN 
               IOBP=0
               MSH_BOUNDARY1,C_GP-1,IOBP
               IF (IOBP EQ 0) THEN BEGIN 
                 MESH_CLEAN_3,C_GP
                 C_GP=0
               ENDIF
            ENDIF
            END
;
; Removing boundary node that is in 2 triangles (3 neighbor nodes)
; 
         2: BEGIN
            IF (C_GP GT 0 ) THEN BEGIN 
               IOBP=0
               MSH_BOUNDARY1,C_GP-1,IOBP
               IF (IOBP EQ 1) THEN BEGIN 
                 DELOK = 1
                 MESH_CLEAN_2,C_GP,180.,DELOK
                 C_GP=0
               ENDIF
            ENDIF
            END

;
; Removing boundary node that is in 1 triangles (2 neighbors) 
; 
         3: BEGIN
            IF (C_GP GT 0 ) THEN BEGIN 
              IOBP=0
              MSH_BOUNDARY1,C_GP-1,IOBP
              IF (IOBP EQ 1) THEN BEGIN 
                MESH_CLEAN_1,C_GP
                C_GP=0
              ENDIF
            ENDIF
            END

         4: BEGIN 
            MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH
            NODES4=WHERE(NEIGH EQ 4,kount4)
            ORDER=REVERSE(SORT(NODES4))
; Loops in reverse order because the node number will change as nodes are deleted
            FOR IDEL=0,kount4-1 DO BEGIN 
            ;FOR IDEL=0,5 DO BEGIN 
              PRINT,'Treating node #',NODES4(ORDER(IDEL))+1,', ', IDEL+1,' out of ',kount4
              MESH_CLEAN_4,NODES4(ORDER(IDEL))+1
            ENDFOR
            END
;
; Removes boundary nodes with 3 neighbors that are in the top 200 
;
         5: BEGIN 
            UPDATE_TIMESTEPS
            MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH
            FOR IND=0,199 DO BEGIN 
              IDEL=TIMESTEPS_ORDER(IND)
              IF (NEIGH(IDEL) EQ 3 AND IOBP(IDEL) EQ 1 ) THEN BEGIN 
                DELOK=1
                PRINT,'Treating node #', IDEL+1,IND,' out of 200'
                MESH_CLEAN_2,IDEL+1,120., DELOK 
; Update TIMESTEPS_ORDER after deletion
                IF DELOK EQ 1 THEN BEGIN 
                  TIMESTEPS_ORDER_OLD=TIMESTEPS_ORDER
                  NODESUP=WHERE(TIMESTEPS_ORDER GT IDEL,kount)
                  IF (kount GT 0) THEN TIMESTEPS_ORDER(NODESUP) = TIMESTEPS_ORDER(NODESUP)-1
                ENDIF 
; Recomputes number of neighbors ... 
                MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH
              ENDIF

            ENDFOR
            END

;
; Removes all boundary nodes with 3 neighbors
;
         6: BEGIN 
            MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH
            NODES4=WHERE(NEIGH EQ 3 AND IOBP EQ 1,kount4)
            ORDER=REVERSE(SORT(NODES4))
; Loops in reverse order because the node number will change as nodes are deleted
            FOR IDEL=0,kount4-1 DO BEGIN 
              IF (NEIGH(NODES4(ORDER(IDEL))) EQ 3) THEN BEGIN 
                 DELOK=1
                PRINT,'Treating node #',NODES4(ORDER(IDEL))+1,', ', IDEL+1,' out of ',kount4
                MESH_CLEAN_2,NODES4(ORDER(IDEL))+1,120., DELOK
; Recomputes number of neighbors ... 
                MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH
              ENDIF

            ENDFOR
            END

;
; Removes all boundary nodes with 2 neighbors
;
         7: BEGIN 
            MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH
            NODES4=WHERE(NEIGH EQ 2 AND IOBP EQ 1,kount4)
            ORDER=REVERSE(SORT(NODES4))
; Loops in reverse order because the node number will change as nodes are deleted
            FOR IDEL=0,kount4-1 DO BEGIN 
            ;FOR IDEL=0,5 DO BEGIN 
              PRINT,'Treating node #',NODES4(ORDER(IDEL))+1,', ', IDEL+1,' out of ',kount4
              MESH_CLEAN_1,NODES4(ORDER(IDEL))+1
            ENDFOR
            END
;
; Re-generates contour ... 
; 
        8: BEGIN
            IOBP=INTARR(NNGP)
            MAXNEIGH=1
            print,'Updates depths on contour ... '
            IF (C_GP LT 0 ) THEN BEGIN 
; To be debugged ...               
              GET_BOUNDARY, IOBP, NEIGHBOR
              GetCycleBoundaries, IOBP, NEIGHBOR, NbCycle, LengthCycle, FirstInCycle, StatusCycle
              PRINT,'Boundary ?',NbCycle
                

              FOR iCycle=0,NbCycle-1 DO BEGIN 
               TheLength=LengthCycle(iCycle)
                PRINT,'CYCLE:',iCycle,TheLength
                IPwork=FirstInCycle(iCycle)
                FOR I=0,TheLength-1 DO BEGIN 
                   PRINT,'CYCLE:',iCycle,TheLength
                   IPwork=NEIGHBOR(IPwork)
                ENDFOR
              ENDFOR
            ENDIF

            MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH
            FOR I=0,NNGP-1 DO BEGIN 
              DEPTHMIN=12000
              DEPTHMAX=3
              IF IOBP(I) EQ 1 THEN BEGIN 
                ;GRIDMAT(7,I)=3
                
                FOR J=0,MAXNEIGH-1 DO BEGIN 
                    IF (VNEIGH(I,J) GT 0) THEN BEGIN 
                      I2=VNEIGH(I,J)
                      IF (IOBP(I2) EQ 0) THEN BEGIN ; only looks at point in domain
                        IF GRIDMAT(7,I2) LT DEPTHMIN THEN BEGIN 
                           DEPTHMIN=GRIDMAT(7,I2)
                           INDMIN=I2
                           ;PRINT,I+1,INDMIN+1,NNGP,IOBP(INDMIN+1),DEPTHMIN,c_lon,c_lat
                        ENDIF
                        IF GRIDMAT(7,I2) GT DEPTHMAX THEN BEGIN 
                           DEPTHMAX=GRIDMAT(7,I2)
                           INDMAX=I2
                           ;PRINT,I+1,INDMIN+1,NNGP,IOBP(INDMIN+1),DEPTHMIN,c_lon,c_lat
                        ENDIF
                      ENDIF 
                    ENDIF
                 ENDFOR
               ENDIF
               IF (GRIDMAT(7,I) EQ 3) THEN GRIDMAT(7,I)=DEPTHMIN
            ENDFOR
            END

;
; Re-generates contour for islands... 
; 
        9: BEGIN
            IOBP=INTARR(NNGP)
            MAXNEIGH=1
            print,'Updates depths on contour ... '
            IF (C_GP LT 0 ) THEN BEGIN 
; To be debugged ...               
              GET_BOUNDARY, IOBP, NEIGHBOR
              GetCycleBoundaries, IOBP, NEIGHBOR, NbCycle, LengthCycle, FirstInCycle, StatusCycle
              PRINT,'Boundary ?',NbCycle
                

              FOR iCycle=0,NbCycle-1 DO BEGIN 
               TheLength=LengthCycle(iCycle)
                PRINT,'CYCLE:',iCycle,TheLength
                IPwork=FirstInCycle(iCycle)
                FOR I=0,TheLength-1 DO BEGIN 
                   PRINT,'CYCLE:',iCycle,TheLength
                   IPwork=NEIGHBOR(IPwork)
                ENDFOR
              ENDFOR
            ENDIF

            MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH
            FOR I=0,NNGP-1 DO BEGIN 
            DEPTHMIN=3
            DEPTHMAX=3

            X=gridmat(0,I)
            Y=gridmat(1,I)
               
             IF (X GE nxzmin*dx AND X LE nxzmax*dx  $
                     AND Y GE nyzmin*dy AND y LE nyzmax*dy  AND IOBP(I) EQ 1) THEN BEGIN 
                GRIDMAT(7,I)=3
                
                FOR J=0,MAXNEIGH-1 DO BEGIN 
                    IF (VNEIGH(I,J) GT 0) THEN BEGIN 
                      I2=VNEIGH(I,J)
                      IF (IOBP(I2) EQ 0) THEN BEGIN ; only looks at point in domain
                        IF GRIDMAT(7,I2) LT DEPTHMIN THEN BEGIN 
                           DEPTHMIN=GRIDMAT(7,I2)
                           INDMIN=I2
                           ;PRINT,I+1,INDMIN+1,NNGP,IOBP(INDMIN+1),DEPTHMIN,c_lon,c_lat
                        ENDIF
                        IF GRIDMAT(7,I2) GT DEPTHMAX THEN BEGIN 
                           DEPTHMAX=GRIDMAT(7,I2)
                           INDMAX=I2
                           ;PRINT,I+1,INDMIN+1,NNGP,IOBP(INDMIN+1),DEPTHMIN,c_lon,c_lat
                        ENDIF
                      ENDIF 
                    ENDIF
                 ENDFOR
               ENDIF
               IF (DEPTHMIN LT 3) THEN BEGIN 
                 GRIDMAT(7,I)=DEPTHMIN-1.
                 ;PRINT,I+1, GRIDMAT(7,I)
               ENDIF

            ENDFOR
            END            
         ENDCASE
        END
; ./
     WG(3,1):BEGIN
         CASE ev.index OF
;
; initializes grid and contourline
;
         0: BEGIN
            WIDGET_CONTROL,Wgrideditor(1),/DELETE_ROWS , $
               USE_TABLE_SELECT=[-1,1,7,nngp-1]
            nngp=nspecgp
            gridmat=DBLARR(8,nngp)
            WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
            WIDGET_CONTROL,WGrideditor(1),INSERT_ROW=nngp-1
            gridmat(0:5,0:nngp-1)=specmat(1:6,0:nspecgp-1)
            contourline=LONARR(2,2)-1
            contourline(1,1)=1
            specmat(0,0:nspecgp-1)=FINDGEN(nspecgp)+1
            Displaygrid
            END
;
; Import polymesh rand and insel
;
         1:BEGIN
            nngp=0L
            contourline=LONARR(2,2)-1
            datastatus(5)=1
            IEL=0L
            file1='rand.dat'
            GET_LUN,unit1
;
; reads files a first time to get the array sizes
;
            OPENR,unit1,file1
            COMMENT='              '
            READF,unit1,COMMENT
            ;PRINT,'Comment:',COMMENT
            NRAND=0L
            WHILE (NRAND GE 0L) DO BEGIN 
               READF, unit1,iel
               IF iel GE 0L THEN NRAND=NRAND+1L ELSE BREAK
            ENDWHILE
            CLOSE,unit1
            FREE_LUN,unit1

            file1='insel.dat'
            GET_LUN,unit1
            OPENR,unit1,file1
            READF,unit1,COMMENT
            NINS=0L
            WHILE (NINS GE 0L) DO BEGIN 
               READF, unit1,iel
               IF iel GE 0 THEN BEGIN 
                  NINS=NINS+1L 
               ENDIF ELSE BEGIN 
                  IF iel EQ -1 THEN BEGIN 
                     READF,unit1,COMMENT
                     PRINT,'Comment:',COMMENT
                  ENDIF ELSE BEGIN  
                     BREAK
                  ENDELSE
               ENDELSE 
            ENDWHILE
            CLOSE,unit1
            FREE_LUN,unit1

            NNGP=NRAND+NINS
            contourline=LONARR(2+NNGP,2)
            contourline(0,0)=0
            contourline(1,0)=nngp+1
            contourline(0:1,1)=0
            gridmat=DBLARR(8,NNGP)
;
; reads files a second time to get the data
;
            file1='rand.dat'
            GET_LUN,unit1
            OPENR,unit1,file1
            READF,unit1,COMMENT
            lon=0.
            lat=0.
            depth=0.  
            I=0L
            WHILE (I LT NRAND) DO BEGIN 
               READF, unit1,iel,lon,lat,depth
               IF iel GE 0L THEN BEGIN 
;
; Converts decimal degrees to degree , minute and X, Y
;
                   latdeg=FLOOR(lat)
                   IF latdeg LT 0 THEN latdeg=latdeg+1
                   londeg=FLOOR(lon)
                   IF londeg LT 0 THEN londeg=londeg+1
                   latmin=(lat-latdeg)*60.
                   lonmin=(lon-londeg)*60.
                   LatLontoXY,latdeg,latmin,londeg,lonmin,x,y

                   gridmat(0,I)=x
                   gridmat(1,I)=y
                   gridmat(2,I)=latdeg
                   gridmat(3,I)=latmin
                   gridmat(4,I)=londeg
                   gridmat(5,I)=lonmin
                   gridmat(6,I)=1
                   gridmat(7,I)=depth
                   contourline(I+2,0)=I
                   contourline(I+2,1)=gridmat(6,I)
                   I=I+1L 
               ENDIF ELSE BREAK
            ENDWHILE
            CLOSE,unit1
            FREE_LUN,unit1
            
            file1='insel.dat'
            GET_LUN,unit1
            OPENR,unit1,file1
            READF,unit1,COMMENT
            CONTOURVAL=-1
            line='                             '
            WHILE (I GE 0L) DO BEGIN 
               READF, unit1,line
               READS,line,iel
               IF iel GE 0 THEN BEGIN 
;
; Converts decimal degrees to degree , minute and X, Y
;
                   READS,line,iel,lon,lat,depth
                   latdeg=FLOOR(lat)
                   IF latdeg LT 0 THEN latdeg=latdeg+1
                   londeg=FLOOR(lon)
                   IF londeg LT 0 THEN londeg=londeg+1
                   latmin=(lat-latdeg)*60.
                   lonmin=(lon-londeg)*60.
                   LatLontoXY,latdeg,latmin,londeg,lonmin,x,y
                   gridmat(0,I)=x
                   gridmat(1,I)=y
                   gridmat(2,I)=latdeg
                   gridmat(3,I)=latmin
                   gridmat(4,I)=londeg
                   gridmat(5,I)=lonmin
                   gridmat(6,I)=CONTOURVAL
                   gridmat(7,I)=depth
                   contourline(I+2,0)=I
                   contourline(I+2,1)=gridmat(6,I)
                   I=I+1L
                   CONTOURVAL=1 
               ENDIF ELSE BEGIN 
                  IF iel EQ -1 THEN BEGIN 
                     READF,unit1,COMMENT
                     CONTOURVAL=-1
                  ENDIF ELSE BEGIN  
                     BREAK
                  ENDELSE
               ENDELSE 
            ENDWHILE
            CLOSE,unit1
            FREE_LUN,unit1
            PRINT,'Rand and Insel:',NRAND,NINS
            Displaygrid
            END
;
; initializes grid and contourline
;
          2:BEGIN
            nngp=0L
            contourline=LONARR(2,2)-1 
            contourline(*,1)=1
            datastatus(5)=1
            datastatus(6)=0
            FLAGTSTEP=0
            END

;
; adds open boundary contour
;
         3: BEGIN 
            LX=(nx-1)*dx
            LY=(ny-1)*dy
            n1=LX/DX1
            n2=LY/DX1
            ndlon=1+ROUND((rlonmax-rlonmin)/ $
                        (ROUND(100.*(rlonmax-rlonmin)/n1)/100.))
            ddlon=(rlonmax-rlonmin)/(ndlon-1)
            ndlat=1+ROUND((rlatmax-rlatmin)/ $
                        (ROUND(100.*(rlatmax-rlatmin)/n2)/100.))
            ddlat=(rlatmax-rlatmin)/(ndlat-1)

            nngpold=nngp
            nngpnew=nngp+2*(ndlon+ndlat)-4
            contourline_all=LONARR(2*(ndlon+ndlat)-4,2)
            temp=gridmat
            gridmatnew=DBLARR(8,nngpnew)
            IF (nngpold GT 0) THEN gridmatnew(*,0:nngpold-1)=temp

;
; Loops over the 4 sides of the rectangular boundary 
;
            FOR KK=0,3 DO BEGIN
              nmax=ndlon*(1-(KK mod 2))+ndlat*(KK mod 2)

              gaps=0

              print,'Side ',KK+1,': will now test if each of the ',nmax-1,' points is in land.'
              print,'That can take a LOOONNG time if the shoreline data is big ...                '

              FOR I=0,nmax-2 DO BEGIN
              CASE KK OF
              0: BEGIN  ; bottom line
                 Inew=I*(FLOAT(nx-1)/FLOAT(ndlon-1))
                 Jnew=0
                 END
              1: BEGIN  ; right
                 Inew=(nx-1)
                 Jnew=I*(FLOAT(ny-1)/FLOAT(ndlat-1))
                 END
              2: BEGIN ; top
                 Inew=FLOAT(ndlon-1-I)*(FLOAT(nx-1)/FLOAT(ndlon-1))
                 Jnew=ny-1
                 END
              3: BEGIN
                 Inew=0  ;left
                 Jnew=FLOAT(ndlat-1-I)*(FLOAT(ny-1)/FLOAT(ndlat-1))
                 END
              ENDCASE
              print,'Testing for new point #:',I+1, ' out of ',nmax-1
;
;  Removes points that are in land
;
              inland=1
              IF N_ELEMENTS(gd) THEN BEGIN 
                 IF (gd(Inew,Jnew) GT Hlandsea) THEN inland=0
              ENDIF ELSE BEGIN 
                 IN_LAND2,Inew*dx,Jnew*dy,inland
              ENDELSE
              
              IF (inland EQ 0) THEN BEGIN
;
; These points are in sea
;
                gridmatnew(0,nngp)=Inew*dx
                gridmatnew(1,nngp)=Jnew*dy
                contourline_all(nngp-nngpold,0)=nngp
                contourline_all(nngp-nngpold,1)=1
                nngp=nngp+1L
              ENDIF ELSE BEGIN
;
; These points are in land
;
                contourline_all(nngp-nngpold-1,1)=-1
              ENDELSE
            ENDFOR
            ENDFOR
            gridmat=DBLARR(8,nngp)
            gridmat(*,0:nngp-1)=gridmatnew(*,0:nngp-1)
            londeg=0
            lonmin=0.
            latdeg=0
            latmin=0.
            contourline=LONARR(2+nngp-nngpold,2)
            contourline(0,0)=nngpold
            contourline(1,0)=nngp+1
            contourline(0:1,1)=0
            contourline(2:nngp-nngpold+1L,*)=contourline_all(0:nngp-nngpold-1,*)
; Search for gaps to shift the contour point indices so that the first gap is at the end
            indexz=WHERE(contourline(2:nngp-nngpold-1,1) EQ -1,kount)
            IF (kount GT 0) THEN BEGIN
               contourline(2:nngp-nngpold+1,1)=SHIFT(contourline(2:nngp-nngpold+1,1),-indexz(0)-1)
               shifted=SHIFT(contourline(2:nngp-nngpold+1,0),-indexz(0))
               gridmat(0,nngpold:nngp-1)=SHIFT(gridmat(0,nngpold:nngp-1),-indexz(0)-1)
               gridmat(1,nngpold:nngp-1)=SHIFT(gridmat(1,nngpold:nngp-1),-indexz(0)-1)
            ENDIF
            contourline(contourline(1,0),1)=1
            gridmat(6,contourline(0,0):contourline(1,0)-2)=contourline(2:nngp-nngpold+1,1)

            FOR I=nngpold,nngp-1 DO BEGIN
               XYtoLatLon,gridmat(0,I),gridmat(1,I),latdeg,latmin,londeg,lonmin
               gridmat(2,I)=latdeg
               gridmat(3,I)=latmin
               gridmat(4,I)=londeg
               gridmat(5,I)=lonmin
            ENDFOR

            WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
            Displaygrid
            END
;
         4: BEGIN 
;
; adds open boundary contour (no check on land / sea)
;
            LX=(nx-1)*dx
            LY=(ny-1)*dy
            n1=LX/DX1
            n2=LY/DX1
            ndlon=1+ROUND((rlonmax-rlonmin)/ $
                        (ROUND(100.*(rlonmax-rlonmin)/n1)/100.))
            ddlon=(rlonmax-rlonmin)/(ndlon-1)
            ndlat=1+ROUND((rlatmax-rlatmin)/ $
                        (ROUND(100.*(rlatmax-rlatmin)/n2)/100.))
            ddlat=(rlatmax-rlatmin)/(ndlat-1)
            
            nngpold=nngp
            nngpnew=nngp+2*(ndlon+ndlat)-4
            contourline_all=LONARR(2*(ndlon+ndlat)-4,2)
            temp=gridmat
            gridmatnew=DBLARR(8,nngpnew)
            IF (nngpold GT 0) THEN gridmatnew(*,0:nngpold-1)=temp

            FOR KK=0,3 DO BEGIN
            nmax=ndlon*(1-(KK mod 2))+ndlat*(KK mod 2)

            gaps=0

             print,'The program will now test if each of the ',nmax-1,' points is  in land or sea.'
             print,'No check on land / sea status  '

            FOR I=0,nmax-2 DO BEGIN
              CASE KK OF
              0: BEGIN  ; bottom line
                 Inew=I*(FLOAT(nx-1)/FLOAT(ndlon-1))
                 Jnew=0
                 END
              1: BEGIN  ; right
                 Inew=(nx-1)
                 Jnew=I*(FLOAT(ny-1)/FLOAT(ndlat-1))
                 END
              2: BEGIN ; top
                 Inew=FLOAT(ndlon-1-I)*(FLOAT(nx-1)/FLOAT(ndlon-1))
                 Jnew=ny-1
                 END
              3: BEGIN
                 Inew=0  ;left
                 Jnew=FLOAT(ndlat-1-I)*(FLOAT(ny-1)/FLOAT(ndlat-1))

                 END
              ENDCASE

                gridmatnew(0,nngp)=Inew*dx
                gridmatnew(1,nngp)=Jnew*dy
                contourline_all(nngp-nngpold,0)=nngp
                contourline_all(nngp-nngpold,1)=1
                nngp=nngp+1L
            ENDFOR
            ENDFOR

            gridmat=DBLARR(8,nngp)
            gridmat(*,0:nngp-1)=gridmatnew(*,0:nngp-1)
            londeg=0
            lonmin=0.
            latdeg=0
            latmin=0.
            contourline=LONARR(2+nngp-nngpold,2)
            contourline(0,0)=nngpold
            contourline(1,0)=nngp+1
            contourline(0:1,1)=0
            contourline(2:nngp-nngpold+1L,*)=contourline_all(0:nngp-nngpold-1,*)
            gridmat(6,contourline(0,0):contourline(1,0))=contourline(2:nngp-nngpold+1,1)

            FOR I=nngpold,nngp-1 DO BEGIN
               XYtoLatLon,gridmat(0,I),gridmat(1,I),latdeg,latmin,londeg,lonmin
               gridmat(2,I)=latdeg
               gridmat(3,I)=latmin
               gridmat(4,I)=londeg
               gridmat(5,I)=lonmin
            ENDFOR
            WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
            Displaygrid
            END
;
         5: BEGIN 
;
; Select island to be removed 
;
            print,'Click on islands to remove them ...'
            WIDGET_CONTROL,Wdraw,EVENT_PRO='gridbuild_draw_event'
            gridbuild_click=5
            END
         6: BEGIN 
;
; Search for crossing segments 
;
            NCROSS=0L
            FOR J=0,contourline(1,0)-contourline(0,0)-1L DO BEGIN
               X1=gridmat(0,contourline(J+2L,0))
               Y1=gridmat(1,contourline(J+2L,0))
               X2=gridmat(0,contourline(J+3L,0))
               Y2=gridmat(1,contourline(J+3L,0))
               IF contourline(J+3L,1) GT 0 THEN BEGIN  
               K=0L
               WHILE (K LT J-1L) DO BEGIN
                  IF contourline(K+3L,1) GT 0 THEN BEGIN  
               IF (K GT NNGP-5L  ) THEN PRINT,'TEST:',J,K,contourline(K+3L,0),contourline(K+3L,1)
                  X3=gridmat(0,contourline(K+2L,0))
                  Y3=gridmat(1,contourline(K+2L,0))
                  X4=gridmat(0,contourline(K+3L,0))
                  Y4=gridmat(1,contourline(K+3L,0))

                  DENOM=(Y4-Y3)*(X2-X1)-(Y2-Y1)*(X4-X3)
                  IF ABS(DENOM) GT 1E-8 THEN BEGIN 
;
; P and Q are the coordinates of the crossing point along the two segments
;
                     P=((X3-X1)*(Y4-Y3)-(Y3-Y1)*(X4-X3))/DENOM
                     Q=((X3-X1)*(Y2-Y1)-(Y3-Y1)*(X2-X1))/DENOM
                     IF (( P GT 0 AND P LT 1 ) AND (Q GT 0 AND Q LT 1)) THEN BEGIN 
;
; Flips the crossed part... 
;
                        NCROSS=NCROSS+1L
                        PRINT,'Crossing contour:',J+1,J+2,K+1,K+2
                     ENDIF
                  ENDIF
                  ENDIF
                  K=K+1L  
               ENDWHILE
               ENDIF
            ENDFOR
            END

         7: BEGIN 
;
; Flip the ordering of points along an island
;              
           print,'C_GP:',C_gp
           toto=0
           
            J=C_GP-1
            IMIN=0L
            IMAX=0L
                WHILE (IMIN EQ 0 AND J GT 0) DO BEGIN 
                   IF gridmat(6,J) EQ -1 THEN BEGIN 
                     IMIN=J
                   ENDIF
                   J=J-1L
                ENDWHILE
                IMAX=0L
                J=C_GP-1
                WHILE (IMAX EQ 0 AND J LT NNGP-1) DO BEGIN 
                   IF gridmat(6,J) EQ -1 THEN BEGIN 
                     IMAX=J-1
                   ENDIF
                   J=J+1L
                ENDWHILE
               
                NSHIFT=IMIN-(C_GP)
                print,'Flip and shift island:',IMIN,C_GP-1,IMAX,NSHIFT
                gridmatold=gridmat
                gridmatold(6,IMIN)=1
                IF IMAX GE IMIN THEN BEGIN 
                   gridmat(*,IMIN:IMAX)= $
                      gridmatold(*,REVERSE(IMIN+SHIFT(FINDGEN(IMAX-IMIN+1),NSHIFT)) )
                   contourlineold=contourline
                   gridmat(6,IMIN)=-1
                   gridmat(6,IMAX)=1
                ENDIF 
            END

;
; Filter out points closer than DX/2
;
         8: BEGIN 
            NSMALL=0L
            JFIRST=0L
            NISLAND=0L
            D2MIN=0.25*DX0^2

            BADPOINTS=BYTARR(nngp)
            FOR J=0L,contourline(1,0)-contourline(0,0)-1L DO BEGIN
               I1=contourline(J+2L,0)-1

               II=contourline(J+2L,1)-1   ; this is < 0 if this point is the first of the island
               IF (II LT 0) THEN BEGIN 
;
; Goes back to first point of the island
;
                 J2=JFIRST+2L
                 I2= contourline(J2,0)-1
                 JFIRST=J+1L
                 NISLAND=NISLAND+1L
                 D2=(X2-X1)^2 + (Y2-Y1)^2
                 ICONT=JFIRST+2L
               ENDIF ELSE BEGIN 
                 J2=J+3L
                 I2=contourline(J2,0)-1
                 ICONT=JFIRST+2L
               ENDELSE
;
               X1=gridmat(0,I1)
               Y1=gridmat(1,I1)
               X2=gridmat(0,I2)
               Y2=gridmat(1,I2)
               D2=((X2-X1)^2 + (Y2-Y1)^2)
      
               lon=gridmat(4,I1)+gridmat(5,I1)/60.0
               lat=gridmat(2,I1)+gridmat(3,I1)/60.0
               lon2=gridmat(4,I2)+gridmat(5,I2)/60.0
               lat2=gridmat(2,I2)+gridmat(3,I2)/60.0

               IF (D2 LT D2MIN AND BADPOINTS(I1) EQ 0) THEN BEGIN 

               IF ROA EQ 1 THEN BEGIN
                  IF (X1 GE nxzmin*dx AND X1 LE nxzmax*dx  $
                      AND X2 GE nxzmin*dx AND X2 LE nxzmax*dx $
                      AND Y1 GE nyzmin*dy AND y1 LE nyzmax*dy  $
                      AND y2 GE nyzmin*dy AND y2 LE nyzmax*dy ) THEN BEGIN 
                         PRINT,'To be deleted? ',lon,lat,I1,I2,gridmat(6,I2)
                         BADPOINTS(I2)=1
                       ENDIF
               ENDIF ELSE BEGIN
                 BADPOINTS(I2)=1
               ENDELSE

                 IF (BADPOINTS(I2) EQ 1 AND gridmat(6,I2) LT 0 ) THEN BEGIN
                   foundnext=0
                   I3=I2+1
                   WHILE (foundnext EQ 0 AND I3 LT NNGP) DO BEGIN 
                     IF  BADPOINTS(I3) EQ 0  THEN BEGIN 
                         gridmat(6,I2+1)=-1
                         foundnext=1
                      ENDIF
                      I3=I3+1
                   ENDWHILE
                  ENDIF

                 ; Marks the first point of the next island
                 ; PRINT,'TEST1:',SQRT(D2),D2MIN,contourline(J+2L,0)+1,J+1,JFIRST+1L,II
                 ;print,'This point will be deleted:',I2+1,lon2,lat2,' too close to ',I1+1,',',SQRT(D2),' < ',sqrt(D2MIN)
               ENDIF


            ENDFOR


            NBAD=WHERE(BADPOINTS EQ 1,kount)
                 print,'NUMBER OF POINTS TO BE DELETED:',kount,':',NBAD
            IF (kount GT 0) THEN BEGIN 
               FOR IDEL=0,KOUNT-1 DO BEGIN 
                  nngp=nngp-1 ;new number of grid points
                  tempo=gridmat
                  print,'HEY:',kount,NBAD(IDEL),NNGP
                  gridmat=DBLARR(8,nngp)
                 
                  gridmat(*,0:NBAD(IDEL)-1)=tempo(*,0:NBAD(IDEL)-1)
                  gridmat(*,NBAD(IDEL):NNGP-1)=tempo(*,NBAD(IDEL)+1:NNGP)
               ENDFOR
               MAKE_CONTOUR
            ENDIF 

;
; Second pass: remove 1 or 2 point islands 
;
            JFIRST=0L
            BADPOINTS=BYTARR(nngp)
            FOR J=0L,contourline(1,0)-contourline(0,0)-1L DO BEGIN
               II=contourline(J+3L,1)
               IF (II LT 0) THEN BEGIN 
                  ; print,'segment?:',J,JFIRST,contourline(J+1L:J+3L,1)
                 IF (JFIRST GE (J-1)) THEN BEGIN 
                    BADPOINTS(JFIRST:J)=1
                    lon=gridmat(4,J)+gridmat(5,J)/60.0
                    lat=gridmat(2,J)+gridmat(3,J)/60.0

                    print,'segment:',JFIRST+1,J+1,lon,lat,' CHECKING:',contourline(J+1L:J+3L,1)
                 ENDIF
                 JFIRST=J+1L
               ENDIF
            ENDFOR


            NBAD=WHERE(BADPOINTS EQ 1,kount)
                 print,'NUMBER OF POINTS TO BE DELETED:',kount,':',NBAD
            IF (kount GT 0) THEN BEGIN 
               FOR IDEL=0,KOUNT-1 DO BEGIN 
                  nngp=nngp-1 ;new number of grid points
                  tempo=gridmat
                  print,'HEY:',kount,NBAD(IDEL),NNGP
                  gridmat=DBLARR(8,nngp)
                 
                  gridmat(*,0:NBAD(IDEL)-1)=tempo(*,0:NBAD(IDEL)-1)
                  gridmat(*,NBAD(IDEL):NNGP-1)=tempo(*,NBAD(IDEL)+1:NNGP)
               ENDFOR
               MAKE_CONTOUR
            ENDIF 

               WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
               Displaygrid
            END

         9: BEGIN 
;
; removes islands that are not in the contour
;
            island=0L
            TOBEREMOVED=INTARR(NNGP,2)
            NREMOVE=0L
            INOUT=0
            J0=contourline(2L,0)
            subcont=contourline(2L+1:contourline(1,0)+2L,1)
            Index=WHERE(subcont LT 0, kount)
            J1=contourline(2L+Index(0),0)
            IF island EQ 0L THEN BEGIN 
;
;  Polygon defined by first polygon
;
                    PX=gridmat(0,J0:J1)
                    PY=gridmat(1,J0:J1)
                    NPP=J1-J0+1
                    PP=LINDGEN(NPP)+J0
            ENDIF

            FOR I=0L,contourline(1,0)-contourline(0,0) DO BEGIN
                IF  contourline(I+2,1) LT 0 THEN BEGIN    ; this is the first point of the polygon
                  J0=contourline(I+2L,0)
                  subcont=contourline(I+2L+1:contourline(1,0)+2L,1)
                  Index=WHERE(subcont LT 0, kount)
                  J1=contourline(I+2L+Index(0),0)

                  IF island GT 0L THEN BEGIN 
                    
                    XX=gridmat(0,J0)
                    YY=gridmat(1,J0)
                    
                    PNPOLY,XX,YY,PX,PY,NPP,INOUT

                    IF INOUT LT 0 THEN BEGIN 
                      TOBEREMOVED(NREMOVE,0)=J0
                      TOBEREMOVED(NREMOVE,1)=J1
                      NREMOVE=NREMOVE+1L
                    ENDIF
                  ENDIF
                  island=island+1L
                  
                  ENDIF ELSE BEGIN 
                    ;IF (INOUT GT 0) THEN BEGIN 
                    ;  TOBEREMOVED(NREMOVE,1)=contourline(I+2,0)
                    ;ENDIF
                  ENDELSE
  
             ENDFOR
             PRINT,'NREMOVE:',NREMOVE            
             FOR I=NREMOVE-1,0,-1 DO BEGIN 
                    J0=TOBEREMOVED(I,0)
                    J1=TOBEREMOVED(I,1)
                    print,'CUTTING ...:',I,J0,J1,nngp
                    IDEL=J1-J0+1
                    nngp=nngp-idel ;new number of grid points
                    tempo=gridmat
                    gridmat=DBLARR(8,nngp)
                    IF J0 gt 0 THEN gridmat(*,0:J0-1)=tempo(*,0:J0-1)
                    print,'TEST NN:',nngp,idel,J0,J1,'##',nngp+idel-(J1+1),nngp-J0
                    gridmat(*,J0:nngp-1)=tempo(*,J1+1:nngp+idel-1)
                    MAKE_CONTOUR
                 ENDFOR
                 c_gp=0
                 WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
                 Displaygrid
            RETURN
            END


         10: BEGIN
            ; Adds points along the depth contour given by Hi
            ; with a resoltuion DX0
               IF ROA EQ 1 THEN BEGIN
                  x=(findgen(nxzmax-nxzmin+1)+nxzmin)*dx
                  y=(findgen(nyzmax-nyzmin+1)+nyzmin)*dy
                  CONTOUR,gd(nxzmin:nxzmax,nyzmin:nyzmax),x,y,LEVELS=Hi,PATH_INFO=info,PATH_XY=xyn,CLOSED=0
               ENDIF ELSE BEGIN
                  x=findgen(nx)*dx
                  y=findgen(ny)*dy
                  CONTOUR,gd,x,y,LEVELS=Hi,PATH_INFO=info,PATH_XY=xyn,CLOSED=0
               ENDELSE
            xy=CONVERT_COORD(xyn,/NORMAL,/TO_DATA)
            addpoints=FLTARR(2,5000)+nx*dx+ny*dy
            naddpoint=0
            print,'Depth:',Hi,' spacing :',dx0
            FOR I = 0, (N_ELEMENTS(info) - 1 ) DO BEGIN
               S=0.
               X0=xy(0,INFO(I).OFFSET)
               Y0=xy(1,INFO(I).OFFSET)
               FOR J=1,info(I).N-1 DO BEGIN
                  X1=xy(0,INFO(I).OFFSET+J)
                  Y1=xy(1,INFO(I).OFFSET+J)
                  S=SQRT((x1-x0)^2+(y1-y0)^2)
                  IF (S GT DX0) AND ((X1 NE x(0)) AND (X1 NE MAX(x)) AND $
                     (Y1 NE Y(0)) AND (Y1 NE MAX(Y))) THEN BEGIN
                     d2gp=MIN((X1-gridmat(0,0:nngp-1))^2+(Y1-gridmat(1,0:nngp-1))^2)
                     d2ad=MIN((X1-addpoints(0,0:naddpoint))^2+(Y1-addpoints(1,0:naddpoint))^2)
                     IF (d2gp GT 0.25*DX0^2) AND (d2ad GT 0.25*DX0^2) THEN BEGIN
                     print,d2gp,d2ad,dx0,x1,y1,nngp+naddpoint
                        naddpoint=naddpoint+1
                        addpoints(0,naddpoint-1)=X1
                        addpoints(1,naddpoint-1)=Y1
                        S=0.
                        X0=X1
                        Y0=Y1
                     ENDIF
                  ENDIF
               ENDFOR
            ENDFOR
            nngp=nngp+naddpoint
            temp=gridmat
            gridmat=DBLARR(8,nngp)
            IF (nngp GT naddpoint) THEN gridmat(*,0:nngp-naddpoint-1)=temp
            gridmat(0:1,nngp-naddpoint:nngp-1)=addpoints(0:1,0:naddpoint-1)

            londeg=0
            lonmin=0.
            latdeg=0
            latmin=0.
            FOR I=1,naddpoint DO BEGIN
               XYtoLatLon,gridmat(0,nngp-I),gridmat(1,nngp-I),latdeg,latmin,londeg,lonmin
               gridmat(2,nngp-I)=latdeg
               gridmat(3,nngp-I)=latmin
               gridmat(4,nngp-I)=londeg
               gridmat(5,nngp-I)=lonmin
            ENDFOR
            PRINT,naddpoint,' new nodes.'
            Displaygrid
            WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
            MakeTriangles,0,0
            END
         11:BEGIN  ;changes the gridbuild_clik option ot allow the interactive
                     ;addition of points along a line that is defined with two points selected
                     ;with the mouse
            WIDGET_CONTROL,Wdraw,EVENT_PRO='gridbuild_draw_event'
            gridbuild_click=3
            END
;
         12:BEGIN
            tri_break=BYTARR(ntri+1)
            xg=FLTARR(ntri+1)
            yg=FLTARR(ntri+1)
            DXMAX=FLTARR(ntri+1)
            trigpnew=FLTARR(ntri*3,4)
            TRIGPNEW(0:ntri,0:3)=trigp(0:ntri,0:3)
            FOR I=1,NTRI DO BEGIN
              IF (I MOD 5000) EQ 0 THEN PRINT,'Looking at triangle ',I,NTRI
              IF TRIGP(I,1) GE 0 THEN BEGIN
                x1=MAX([0,MIN([(nx-1)*dx,gridmat(0,TRIGP(I,1)-1)])])
                x2=MAX([0,MIN([(nx-1)*dx,gridmat(0,TRIGP(I,2)-1)])])
                x3=MAX([0,MIN([(nx-1)*dx,gridmat(0,TRIGP(I,3)-1)])])
                y1=MAX([0,MIN([(ny-1)*dy,gridmat(1,TRIGP(I,1)-1)])])
                y2=MAX([0,MIN([(ny-1)*dy,gridmat(1,TRIGP(I,2)-1)])])
                y3=MAX([0,MIN([(ny-1)*dy,gridmat(1,TRIGP(I,3)-1)])])
                xg(I)=(x1+x2+x3)/3.
                yg(I)=(y1+y2+y3)/3.
                tri_in_ROA=0
                IF (ROA EQ 1) THEN BEGIN 
                IF ((xg(I)/dx GT FLOAT(nxzmin)) AND (xg(I)/dx LT FLOAT(nxzmax)) $
                     AND (yg(I)/dy GT FLOAT(nyzmin)) AND (yg(I)/dy LT FLOAT(nyzmax))) THEN tri_in_ROA=1
                ENDIF ELSE tri_in_ROA=1

                IF (tri_in_ROA EQ 1) THEN BEGIN 
                depthtri=(gd(x1/dx,y1/dy)+gd(x2/dx,y2/dy)+gd(x3/dx,y3/dy) $
                  +3.*gd(xg(I)/dx,yg(I)/dy))/6.
                Speeds,MAX([MIN([depthtri,H1]),H0]),1/T0,cphi,cg,KH
                d1=sqrt((x3-x2)^2+(y3-y2)^2)
                d2=sqrt((x3-x1)^2+(y3-y1)^2)
                d3=sqrt((x1-x2)^2+(y1-y2)^2)
                dmax=ABS(MAX([d1,d2,d3]))
                aamin=100.
                ddmin=100.
                DXmax(I)=DX0+(DX1-DX0)*(TANH(KH)-TANH(KH0))/(TANH(KH1)-TANH(KH0))
                IF (dmax GT  DXmax(I)) THEN BEGIN
                  a1=ASIN(((x2-x1)*(y3-y1)-(x3-x1)*(y2-y1))/(d3*d2))
                  a2=ASIN(((x3-x2)*(y1-y2)-(x1-x2)*(y3-y2))/(d1*d3))
                  a3=ASIN(((x1-x3)*(y2-y3)-(x2-x3)*(y1-y3))/(d2*d1))
; distances from barycenter
                  d5=sqrt((xg-x2)^2+(yg-y2)^2)
                  d4=sqrt((xg-x1)^2+(yg-y1)^2)
                  d6=sqrt((xg-x3)^2+(yg-y3)^2)
; angles relative to barycenter
                  a4=ASIN(((x2-xg)*(y3-yg)-(x3-xg)*(y2-yg))/(d5*d6))
                  a5=ASIN(((x3-xg)*(y1-yg)-(x1-xg)*(y3-yg))/(d4*d6))
                  a6=ASIN(((x1-xg)*(y2-yg)-(x2-xg)*(y1-yg))/(d4*d5))
                  amin=ABS(MIN([a1,a2,a3]))
                  amax=ABS(MAX([a4,a5,a6]))
                  dmin=ABS(MIN([d1,d2,d3]))
                  IF (dmin LT ddmin) THEN BEGIN
                    ddmin=dmin
                    Imin=I
                  ENDIF 
                  DXmax(I)=DXmax(I)*( 0.5 + 1.5*amin/!pi  )

                  IF amin LT !pi/10 AND tri_in_ROA THEN PRINT,'WARNING: SMALL ANGLE',I,TRIGP(I,1)-1, $
                     TRIGP(I,2)-1,TRIGP(I,3)-1,amin/!dtor
                  IF dmin LT DXmax(I)*0.1 AND tri_in_ROA THEN PRINT,'WARNING: SMALL TRIANGLE',I,TRIGP(I,1)-1, $
                     TRIGP(I,2)-1,TRIGP(I,3)-1,dmin,DXmax(I)
                  IF dmin LT DXmax(I)*0.1 AND tri_in_ROA THEN PRINT,'WARNING: NEW TRI TOO FLAT',I,TRIGP(I,1)-1, $
                     TRIGP(I,2)-1,TRIGP(I,3)-1,amax
                  IF (dmax GT  DXmax(I)) AND (amax LT !pi*0.8 ) AND tri_in_ROA THEN tri_break(I)=1
                ENDIF
                ENDIF
              ENDIF
            ENDFOR
               ; PRINT,'DDMIN:',ddmin,Imin,trigp(Imin,*)-1


            ; Makes sure that new node
            ; is not too close (dx0) to old ones
            index=WHERE(tri_break EQ 1,naddpoint)
            naddtrue=0
            IF naddpoint NE 0 THEN BEGIN
            FOR I=1,ntri DO BEGIN
               IF tri_break(I) THEN BEGIN
                              IN_SEA,xg(I),yg(I),in
                  index2=WHERE(tri_break EQ 2,naddpoint2)
                  d2gp=MAX((Xg(I)-gridmat(0,0:nngp-1))^2+(Yg(I)-gridmat(1,0:nngp-1))^2)
                  d2gpm=MIN((Xg(I)-gridmat(0,0:nngp-1))^2+(Yg(I)-gridmat(1,0:nngp-1))^2)
                  d2new=1.5*DXmax(I)^2
                  IF naddpoint2 NE 0 THEN d2new=MIN((Xg(I)-XG(index2))^2+(Yg(I)-YG(index2))^2)
                  IF (d2gp GT DXmax(I)^2) AND (d2new GT DXmax(I)^2)  AND (in EQ 1) THEN BEGIN 
                      tri_break(I)=2
                      naddtrue=naddtrue+1
                  ENDIF
                  print,'BREAK:',I,XG(I),YG(I),d2gp,d2gpm,DXmax(I),tri_break(I)

               ENDIF
            ENDFOR
            ENDIF
            index=WHERE(tri_break EQ 2,naddpoint)
            PRINT,'add points:',naddpoint
            IF naddpoint GT 1 THEN BEGIN
               ntriold=ntri
               trigpold=trigp
               nngp=nngp+naddpoint
               temp=gridmat
               gridmat=DBLARR(8,nngp)
               gridmat(*,0:nngp-naddpoint-1)=temp
               gridmat(0,nngp-naddpoint:nngp-1)=xg(index)
               gridmat(1,nngp-naddpoint:nngp-1)=yg(index)
               INDNEW=nngp+1+FINDGEN(naddpoint)-naddpoint
               ; Redifines the triangles
               PRINT,'add triangles:',naddpoint,ntri,ntri+2*naddpoint,'##',indnew
               ; defines zones of new triangles
               TRIGPNEW(ntri+1:ntri+naddpoint,0)=TRIGPNEW(index,0)
               TRIGPNEW(ntri+1+naddpoint:ntri+2*naddpoint,0)=TRIGPNEW(index,0)
               PRINT,'add triangles 1:'
               ; first point of new triangles: barycenter
               TRIGPNEW(ntri+1:ntri+naddpoint,1)=INDNEW
               TRIGPNEW(ntri+1+naddpoint:ntri+2*naddpoint,1)=INDNEW
               PRINT,'add triangles 2:'
               ; second point of new triangles: points 1 or 2
               TRIGPNEW(ntri+1:ntri+naddpoint,2)=TRIGPNEW(index,1)
               TRIGPNEW(ntri+1+naddpoint:ntri+2*naddpoint,2)=TRIGPNEW(index,2)
               ; 3rd point of new triangles: point 3
               TRIGPNEW(ntri+1:ntri+naddpoint,3)=TRIGPNEW(index,3)
               TRIGPNEW(ntri+1+naddpoint:ntri+2*naddpoint,3)=TRIGPNEW(index,3)
               ; 3rd point of old triangle : barycenter
               TRIGPNEW(index,3)=INDNEW
               ntri=ntri+2*naddpoint
               TRIGP=INTARR(ntri+1,4)
               TRIGP(*,*)=TRIGPNEW(0:ntri,*)
               PRINT,'add triangles 3:'

               londeg=0
               lonmin=0.
               latdeg=0
               latmin=0.
               PRINT,'Recomputes XY:'
               FOR I=1,naddpoint DO BEGIN
                  XYtoLatLon,gridmat(0,nngp-I),gridmat(1,nngp-I),latdeg,latmin,londeg,lonmin
                  gridmat(2,nngp-I)=latdeg
                  gridmat(3,nngp-I)=latmin
                  gridmat(4,nngp-I)=londeg
                  gridmat(5,nngp-I)=lonmin
               ENDFOR
               PRINT,'Displays grid:'
               Displaygrid
               WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
             ENDIF
             END
;
; Updates depths
;
         13:BEGIN
            IF datastatus(2) THEN BEGIN 
              FOR I=0,nngp-1 DO BEGIN
                X=[gridmat(0,I)]
                Y=[gridmat(1,I)]
                IF (X GT rangex(0) AND X LT rangex(1) and Y GT rangey(0) and Y LT rangey(1)) THEN BEGIN 
                  x1=(X)/dx
                  i1=FLOOR(x1) 
                  ip1=MIN([nx-1,i1+1])
                  dx1=x1-i1
                  y1=(y)/dy
                  j1=FLOOR(y1) 
                  jp1=MIN([ny-1,j1+1])
                  dy1=y1-j1
                  d1=gd(i1,j1 )
                  d2=gd(ip1,j1 )
                  d3=gd(i1,jp1)
                  d4=gd(ip1,jp1)
                  depthnew=(d1*(1-dx1)+d2*dx1)*(1-dy1) + (d3*(1-dx1)+d4*dx1)*dy1
                  ;PRINT,'TEST:',I+1,gridmat(7,I),depthnew,gd(i1,j1),gd(ip1,jp1),dx1,dy1,i1,ip1,j1,jp1
                  IF (d1 GT -9 AND d2 GT -9 AND d3 GT -9 AND d4 GT -9 ) THEN gridmat(7,I)=depthnew
                ENDIF
               ENDFOR
             ENDIF
             END
        ENDCASE
        END
;
; Timesteps droplist

;
; Read coordinates of nodes with smallest timesteps
;
      WG(2,1):BEGIN
         CASE ev.index OF
         0:BEGIN
            GET_LUN,unit1
            FILE1='timesteps.dat'
            OPENR,unit1,FILE1
            NTIMESTEPS=MAX([NNGP,200])
            TIMESTEPS=FLTARR(4,NTIMESTEPS)
            NRAND=0L
            lon=0.
            lat=0.
            depth=0.  
            IND1=0
            IND2=0
            tstep=0.
            dummy='  '
            READF, unit1,dummy
            FOR I=0,NTIMESTEPS-1 DO BEGIN 
               READF, unit1,IND1,tstep,lon,lat,depth
               TIMESTEPS(0,I)=IND1
               TIMESTEPS(1,I)=tstep
               latdeg=FLOOR(lat)
               IF latdeg LT 0 THEN latdeg=latdeg+1
               londeg=FLOOR(lon)
               IF londeg LT 0 THEN londeg=londeg+1
               latmin=(lat-latdeg)*60.
               lonmin=(lon-londeg)*60.
               LatLontoXY,latdeg,latmin,londeg,lonmin,x,y
               TIMESTEPS(2,I)=X
               TIMESTEPS(3,I)=Y
            ENDFOR
            FLAGTSTEP=1
            CLOSE,unit1
            FREE_LUN,unit1
            END
         1: IF (N_ELEMENTS(NTRI) GT 0) THEN UPDATE_TIMESTEPS
;
; Optimize position of current node
;
         2: BEGIN 
            MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH
            IND=WHERE(IOBP EQ 1) 
            SIRATIO(IND)=0
            IF (IOBP(C_GP-1) EQ 0) THEN MESH_OPTIMIZE,C_GP
            END
;
; Optimize position of top 20 nodes (not boundary) 
;
         3: BEGIN 
            MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH
            IND=WHERE(IOBP EQ 1) 
            SIRATIO(IND)=0
            FOR IND=0,NTIMESTEPS/10-1 DO BEGIN 
              IP=TIMESTEPS_ORDER(IND)
              IF (IOBP(IP) EQ 0  ) THEN BEGIN 
                PRINT,'Working on node ',IP+1,'. ',IND+1,'out of ',NTIMESTEPS/10
                MESH_OPTIMIZE,IP+1
                ENDIF
            ENDFOR
            END
         ENDCASE
         END
;
; File output droplist
;
      WG(4,4):BEGIN
         CASE ev.index OF
         0:BEGIN                                ;write grid file
            file=DIALOG_PICKFILE(/READ,PATH = PATHS(5), FILTER ='*.grgp')
            IF !VERSION.OS_FAMILY EQ 'Windows' THEN $
               spos=RSTRPOS(file,'\') ELSE spos=RSTRPOS(file,'/')
            PATHS(5)=STRMID(file,0,spos+1)
            filenopath=STRMID(file,spos+1,STRLEN(file)-spos-1)
            filenames(5)=filenopath
            IF file NE '' THEN BEGIN
               GET_LUN,unit
               OPENW,unit,file
               PRINTF,unit,nngp,'   Number of grid points'

               FOR I=1,(nngp) DO BEGIN
                  PRINTF,unit,I,gridmat(0:7,I-1), $
                     FORMAT='(I6,2F9.4,I5,F9.4,I5,F9.4)'
               ENDFOR
               CLOSE,unit
               FREE_LUN,unit
            ENDIF
            END
      1:BEGIN     ;write triangle mesh with gmesh format
            file=DIALOG_PICKFILE(/READ, PATH = PATHS(7),FILTER ='*.msh')
            IF !VERSION.OS_FAMILY EQ 'Windows' THEN $
               spos=RSTRPOS(file,'\') ELSE spos=RSTRPOS(file,'/')
            PATHS(7)=STRMID(file,0,spos+1)
            filenopath=STRMID(file,spos+1,STRLEN(file)-spos-1)
            filenames(7)=filenopath
            IF file NE '' THEN BEGIN
               GET_LUN,unit
               OPENW,unit,file
               PRINTF,unit,'$MeshFormat'
               PRINTF,unit,'2 0 8'
               PRINTF,unit,'$EndMeshFormat'
               ;IF (datastatus(2)) THEN PRINTF,unit,'$File  2 ',paths(2),filenames(2)
               ;IF (datastatus(3)) THEN PRINTF,unit,'$File  3 ',paths(3),filenames(3)
               ;IF (datastatus(18)) THEN PRINTF,unit,'$File 18 ',paths(18),filenames(18)
               PRINTF,unit,'$Nodes'
               PRINTF,unit,nngp
               FOR I=0L,nngp-1 DO BEGIN
                  x1=MAX([0,MIN([(nx-1),gridmat(0,I)/dx])])
                  y1=MAX([0,MIN([(ny-1),gridmat(1,I)/dy])])
                  lon=gridmat(4,I)+gridmat(5,I)/60.0
                  lat=gridmat(2,I)+gridmat(3,I)/60.0
                  IX=FLOOR(X1)
                  IP1=min([IX+1,nx-1])
                  JY=FLOOR(Y1)
                  JP1=min([JY+1,ny-1])
                  xx=X1-IX
                  yy=Y1-JY
                  ;IF (n_ELEMENTS(gd) GT 0) THEN $
                  ;   z=(gd(IX,JY) *(1-xx)+gd(IP1,JY )*xx)*(1-yy)  $
                  ;    +(gd(IX,JP1)*(1-xx)+gd(IP1,JP1)*xx)*yy+4.   $
                  ;ELSE  $
                  z=gridmat(7,I)
                  PRINTF,unit,I+1,lon,lat,z,FORMAT='(I8,2F16.10,F10.3)'
               ENDFOR
               PRINTF,unit,'$EndNodes'
               PRINTF,unit,'$Elements'

               iel=1L
               island=0L

; BIG BUG AFTER GRID EDIT ... COMES FROM BUG IN MAKE_CONTOUR ... 
; As a result we list here some nodes with depth > H0 
               PRINT,'Number of elements in contour:', N_ELEMENTS(contourline)
               IF N_ELEMENTS(contourline) GE -4 THEN BEGIN 
                 PRINT,'Not enough contour points ... lists nodes with depth > H0'
                 NOBC=0L
                 MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH
    Index=WHERE(IOBP EQ 1,kount)
                 FOR I=0,NNGP-1 DO BEGIN 
                   DEPTHMIN=3
                   IF IOBP(I) EQ 1 AND GRIDMAT(7,I) GT H0  THEN NOBC = NOBC +1
                 ENDFOR
                 PRINTF,unit,NOBC+NTRI  ; total number of elements 
                 FOR I=0,NNGP-1 DO BEGIN 
                   DEPTHMIN=3
                   IF IOBP(I) EQ 1 AND GRIDMAT(7,I) GT H0  THEN BEGIN 
                     PRINTF,unit,iel,' 15 2 0',island,I+1
                     iel=iel+1L
                   ENDIF
                 ENDFOR
               ENDIF ELSE BEGIN 
                 PRINTF,unit,1L+contourline(1,0)-contourline(0,0)+ntri

                 FOR I=0L,contourline(1,0)-contourline(0,0) DO BEGIN
                    IF  contourline(I+2,1) LT 0 THEN island=island+1L
                    PRINTF,unit,iel,' 15 2 0',island,contourline(I+2,0)+1
                    iel=iel+1
                 ENDFOR
               ENDELSE
;
               FOR I=0L,ntri-1 DO BEGIN
                   PRINTF,unit,iel,' 2 3 0 ',I+1, $
                      TRANSPOSE(TRIGP(I+1,0:3)), FORMAT='(I9,A,I9,4I9)'
                   iel=iel+1
               ENDFOR
               PRINTF,unit,'$EndElements'
               CLOSE,unit
               FREE_LUN,unit
            ENDIF
            END
      2:BEGIN     ;export information for Polymesh
         file1='rand.dat'
         GET_LUN,unit1
         OPENW,unit1,file1
         iel=0L
         island=0L
         PRINTF,unit1,'C Insel',island
         FOR I=2L,contourline(1,0) DO BEGIN
            IF contourline(I,1) LT 0L AND I GT 2 THEN BEGIN 
;
; Jumps to the next island
;
               IF island EQ 0L THEN BEGIN 
                  PRINTF,unit1, FORMAT='(I10)',-1
                  file2='insel.dat'
                  CLOSE,unit1
                  FREE_LUN,unit1
                  GET_LUN,unit1
                  OPENW,unit1,file2
               ENDIF ELSE BEGIN 
;
; Test if this is the last island
;
                  IF (I LT contourline(1,0)-contourline(0,0) ) THEN $
                     PRINTF,unit1, FORMAT='(I10)',-1 
               ENDELSE
               island=island+1L
               iel=0L
               PRINTF,unit1,'C Insel',island
            ENDIF
            J=contourline(I,0)
            lon=gridmat(4,J)+gridmat(5,J)/60.0
            lat=gridmat(2,J)+gridmat(3,J)/60.0
;
; J+1L instead of iel: easier to track the point numbers ... 
;
            PRINTF, unit1, FORMAT='(I10,3F16.6)',J+1L,lon,lat, gridmat(7,J)
            iel=iel+1L
         ENDFOR
         IF (island GT 0) THEN BEGIN 
            PRINTF,unit1, FORMAT='(I10)',-2
            CLOSE,unit1
            FREE_LUN,unit1
         ENDIF
         END   ; end of polymesh output
;
      3:BEGIN  ;export shoreline slope
         file1='shoreline_slope.dat'
         GET_LUN,unit1
         OPENW,unit1,file1
         island=0L
         print,'TEST SLOPES',N_ELEMENTS(slopes) 
         IF N_ELEMENTS(slopes) NE nngp THEN BEGIN 
           slopes=FLTARR(nngp)
         ENDIF ELSE BEGIN 
           print,'Updating shoreline slopes'
         ENDELSE
         IBOUND=0
         FOR IC=0L,contourline(1,0)-contourline(0,0) DO BEGIN
             IF  contourline(IC+2,1) LT 0 THEN island=island+1L
          depthc=0.2
;         FOR I=0,nngp-1 DO BEGIN
            I=contourline(IC+2,0)  

            ;IF (ABS(gridmat(6,I)) EQ 1) THEN BEGIN 
              IBOUND=IBOUND+1
              X=[gridmat(0,I)]
              Y=[gridmat(1,I)]
              IF (X GT rangex(0) AND X LT rangex(1) and Y GT rangey(0) and Y LT rangey(1)) THEN BEGIN 
                nxz1=MIN([MAX([1,ROUND((X)/dx-45)]),nx-2])
                nyz1=MIN([MAX([1,ROUND((Y)/dy-45)]),ny-2])
                nxz2=MIN([MAX([1,ROUND((X)/dx+45)]),nx-2])
                nyz2=MIN([MAX([1,ROUND((Y)/dy+45)]),ny-2])
                gd1=gd(nxz1:nxz2,nyz1:nyz2)-depthc
                gd2=gd(nxz1+1:nxz2+1,nyz1:nyz2)-depthc
                gd3=gd(nxz1:nxz2,nyz1+1:nyz2+1)-depthc
                prod1=gd1*gd2
                prod2=gd1*gd3
                x2=(repmat((findgen(nxz2-nxz1+1)+nxz1)*dx,1,nyz2-nyz1+1)-replicate(X,nxz2-nxz1+1,nyz2-nyz1+1))
                ;x2=repmat((findgen(nxz2-nxz1+1)+nxz1)*dx,1,nyz2-nyz1+1)
                y2=(repmat(transpose((findgen(nyz2-nyz1+1)+nyz1)*dy),nxz2-nxz1+1,1)-replicate(Y,nxz2-nxz1+1,nyz2-nyz1+1))
                J=where(prod1 LT 0 OR prod2 LT 0,kount)
                ;IF (I EQ 5807) THEN PRINT,'TOTO A',kount,prod1(*,16)
                IF (kount GT 0) THEN BEGIN 
                  dist=x2(J)^2+y2(J)^2;
                  DMIN=MIN(dist,location)
                  ;ind = ARRAY_INDICES(x2, J(location))
                  ARRAY_IND2,x2, J(location),ind
                  IF (ind[0] LT nxz2-nxz1 AND ind[1] LT nyz2-nyz1) THEN BEGIN 
                    if (gd1(ind[0],ind[1]) GT depthc AND gd1(ind[0]+1,ind[1]) LT depthc) THEN ind(0)=ind(0)-1 $
                       ELSE ind(0)=ind(0)+1
                    if (gd1(ind[0],ind[1]) GT depthc AND gd1(ind[0],ind[1]+1) LT depthc) THEN ind(1)=ind(1)-1 $
                       ELSE ind(1)=ind(1)-1
                    slope=0.001*sqrt(((gd1(ind[0],ind[1])-gd1(ind[0]+1,ind[1]))/dx)^2  $
                                       +((gd1(ind[0],ind[1])-gd1(ind[0],ind[1]+1))/dy)^2)
                    ;PRINT,'HEY:',I+1,sqrt(DMIN),slope,'##',gd1(ind[0],ind[1]),gd1(ind[0]+1,ind[1]),gd1(ind[0],ind[1]+1)
                    slopes(I)=MAX([slope,0.001])
                    IF (slopes(I) GT 4) THEN slopes(I) = 0.0  ; this is for bad bathymetry data ... 
                  ;IF (I EQ 5807) THEN PRINT,'HEY END:',I+1,slopes(I)
                  ENDIF 
                ENDIF
              ENDIF
              ;IF (I EQ 5807) THEN PRINT,'TOTO',slopes(I),I+1,contourline(IC+2,0)+1
              IF (slopes(I) GT 0) THEN BEGIN 
                 PRINTF,unit1,IBOUND,' 15 3 ',open_boundary(I),island,slopes(I),contourline(IC+2,0)+1
              ENDIF ELSE BEGIN 
                  PRINTF,unit1,IBOUND,' 15 2 ',open_boundary(I),island,contourline(IC+2,0)+1
              ENDELSE  

            ;ENDIF
          ENDFOR
          CLOSE,unit1
          FREE_LUN,unit1
          END

      4:BEGIN  ;  export slope grid (from gd)
          slopes=gd*0.
          dxlat=REPLICATE(dx,ny)
          lats=rlatmin+FINDGEN(ny)*(rlatmax-rlatmin)/(ny-1);
          dxlat(*)=(rlonmax-rlonmin)/(nx-1) $
                   *cos(lats*!dtor)*4E4/360.   ; spacing in km
          nse=3
          depthc=0.2
          counter=-10L
          dminOK=(3*dy)^2
          FOR J=nse,ny-1-nse DO BEGIN 
            nsy=ROUND(nse*dx/dxlat(J))
            Imax=2*nsy
            Jmax=2*nse
            PRINT,'j:',j,max(slopes(*,j-1)),nsy
            FOR I=nse,nx-1-nse DO BEGIN 
                nxz1=MIN([MAX([1,I-nsy]),nx-2])
                nyz1=MIN([MAX([1,J-nse]),ny-2])
                nxz2=MIN([MAX([1,I+nsy]),nx-2])
                nyz2=MIN([MAX([1,J+nse]),ny-2])
                gd1=gd(nxz1:nxz2,nyz1:nyz2)-depthc
                gd2=gd(nxz1+1:nxz2+1,nyz1:nyz2)-depthc
                gd3=gd(nxz1:nxz2,nyz1+1:nyz2+1)-depthc
                prod1=gd1*gd2
                prod2=gd1*gd3
                x2=(repmat((findgen(nxz2-nxz1+1)+nxz1-I)*dxlat(J),1,nyz2-nyz1+1))
                y2=(repmat(transpose((findgen(nyz2-nyz1+1)+nyz1-J)*dy),nxz2-nxz1+1,1))
                JJ=where(prod1 LT 0 OR prod2 LT 0,kount)
                ;IF (I EQ 5807) THEN PRINT,'TOTO A',kount,prod1(*,16)
                IF (kount GT 0) THEN BEGIN 
                  dist=x2(JJ)^2+y2(JJ)^2;
                  DMIN=MIN(dist,location)
                  ;ind = ARRAY_INDICES(x2, JJ(location))
                  ARRAY_IND2,x2, JJ(location),ind
                  IF (ind[0] LT nxz2-nxz1-1 AND ind[1] LT nyz2-nyz1-1) THEN BEGIN 
                    counter=counter+1
                   ; IF (counter EQ 1) THEN PRINT,'X2:',X2
                   ; IF (counter EQ 1) THEN PRINT,'Y2:',Y2
                   ; IF (counter EQ 1) THEN PRINT,'gd:',gd1
                   ; IF (counter EQ 1) THEN PRINT,'in:',ind,prod1(JJ(location)),prod2(JJ(location))
                   ; IF (counter EQ 1) THEN PRINT,'pr:',prod1
                   ; IF (counter EQ 1) THEN PRINT,'p2:',prod2
                   ;  IF (counter EQ 1) THEN PRINT,'HEY:',sqrt(DMIN),'##',gd1(ind[0],ind[1]),gd1(ind[0]+1,ind[1]), $
                   ;   gd1(ind2[0],ind2[1]),gd2(ind2[0],ind2[1]+1)
                    if (gd1(ind[0],ind[1]) GT depthc AND gd1(ind[0]+1,ind[1]) LT depthc) THEN ind(0)=ind(0)-1 
                    IF (ind(0) GT 0) THEN BEGIN 
                     if (gd1(ind[0],ind[1]) LT depthc AND gd1(ind[0]+1,ind[1]) GT depthc) THEN ind(0)=ind(0)+1 
                    IF (ind(0) LT Imax) THEN BEGIN 
                    ind2=ind
                    if (gd1(ind2[0],ind2[1]) GT depthc AND gd1(ind2[0],ind2[1]+1) LT depthc) THEN ind2(1)=ind2(1)-1 
                    IF (ind2(1) GT 0) THEN BEGIN 
                    
                    if (gd1(ind2[0],ind2[1]) LT depthc AND gd1(ind2[0],ind2[1]+1) GT depthc) THEN ind2(1)=ind2(1)+1 
                    IF (ind2(1) LT Jmax) THEN BEGIN 
                    
                    IF (counter EQ 1) THEN PRINT,'i2:',ind,ind2
                    slope=0.001*sqrt(((gd1(ind[0],ind[1])-gd1(ind[0]+1,ind[1]))/dxlat(J))^2  $
                                       +((gd1(ind2[0],ind2[1])-gd1(ind2[0],ind2[1]+1))/dy)^2)
                    IF (counter EQ 1) THEN PRINT,'HEY:',sqrt(DMIN),slope,'##',gd1(ind[0],ind[1]),gd1(ind[0]+1,ind[1]), $
                      gd1(ind2[0],ind2[1]),gd2(ind2[0],ind2[1]+1)
                    ;PRINT,'HEY:',I+1,sqrt(DMIN),slope,'##',gd1(ind[0],ind[1]),gd1(ind[0]+1,ind[1]),gd1(ind[0],ind[1]+1)
                    slopes(I,J)=MAX([slope,0.001])
                    
                     ENDIF 
                     ENDIF
                     ENDIF
                     ENDIF
                  ENDIF 
                ENDIF
            ENDFOR
          ENDFOR             
          GET_LUN,unit1
          OPENW,unit1,paths(3)+'etopo2_slopes.grd',/SWAP_ENDIAN
      WRITEU,unit1,transpose(slopes)
PRINT,'MIN / MAX:',MIN(slopes),MAX(slopes)
           CLOSE,unit1
           FREE_LUN,unit1

         END
      ENDCASE  ; end of file output droplist
      END

      ENDCASE  ; end of droplists
      END
   ENDCASE
RETURN
    CLOSE,unit1
NOT_OPEN:     print,'Error in opening or reading file: ',file1,' . Unexpected results may occur ...'

RETURN
END

;----------------------------------------------------------------------------
PRO MakeTriangles,excludeland,dozones
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON TRIANGLE2,nzonetri,zonetri,nzonegp,zonegp,ngpzone,gpzone
COMMON Builder,ngp,H0,H1,Hi,KH0,KH1,DX0,DX1,T0,ROA,ROI,gridbuild_click
COMMON LOCAL_tri,LIST
;*******END OF COMMON BLOCKS*******************************
         X=FLTARR(NNGP)
         Y=FLTARR(NNGP)
         X(*)=(gridmat(0,0:nngp-1))
         Y(*)=(gridmat(1,0:nngp-1))
         TRIANGULATE, X, Y, DELTRI, BOUNDARY, $
            CONNECTIVITY=LIST, REPEATS=REP
         TAILLE=SIZE(DELTRI)
;
; Clean up triangles in land
;
         IF excludeland THEN BEGIN
         n_cont=0
         IF (n_ELEMENTS(contourline) GT 4) THEN n_cont=(contourline(1,0)-contourline(0,0)+1)
         IF (n_cont GT 0) THEN BEGIN
            ntri=Taille(2)
            nsea=0
            SEATRI=LONARR(ntri)
            FOR I=0L,ntri-1 DO BEGIN
               I1=DELTRI(0,I)
               I2=DELTRI(1,I)
               I3=DELTRI(2,I)
               IF (I1 LT contourline(0,0) OR  I1 GT contourline(1,0)) $
               AND (I2 LT contourline(0,0) OR  I2 GT contourline(1,0)) $
               AND (I3 LT contourline(0,0) OR  I3 GT contourline(1,0)) THEN BEGIN
                 addtri=1
               ENDIF ELSE BEGIN
                  xt=gridmat(0,DELTRI(0:2,I))
                  yt=gridmat(1,DELTRI(0:2,I))
                  xg=TOTAL(xt)/3.
                  yg=TOTAL(yt)/3.
                  IN_SEA,xg,yg,addtri
               ENDELSE
               IF addtri THEN BEGIN
                 SEATRI(nsea)=I
                      nsea=nsea+1
               ENDIF
            ENDFOR
            DELTRI_OLD=DELTRI
            DELTRI=LONARR(3,nsea)
            DELTRI(*,*)=DELTRI_OLD(*,SEATRI(0:nsea-1))
         ENDIF
         TAILLE=SIZE(DELTRI)
         ENDIF
         ;PRINT,'N points:',N_ELEMENTS(X)
         ;PRINT,'N triangles:',Taille(2)
         ;******************************************************
         ; Creates the triangles and zones (SUBDOMAINS)
         ;******************************************************
         ntri=Taille(2)
         TRIGP=INTARR(ntri+1,4)
         xmax=(NX-1)*dx
         ymax=(Ny-1)*dy
         xg=FLTARR(ntri+1)
         yg=FLTARR(ntri+1)
         DXMAX=FLTARR(ntri)
         TRIGP(1:ntri,0)=5

print,'TRI OK, NOW DOING ZONES',ntri
         DIAGNOSTICS=FLTARR(ntri,3)
       ddmin=100.
       Imin=0
         FOR I=0,(ntri-1) DO BEGIN
            ;   (X(DELTRI(0,I)) LT 0.) OR (X(DELTRI(1,I)) LT 0.) $
            ;   OR (X(DELTRI(2,I)) LT 0) THEN IZONE=1
            ;IF (X(DELTRI(0,I)) GT xmax) OR (X(DELTRI(1,I)) GT xmax) $
            ;   OR (X(DELTRI(2,I)) GT xmax) THEN IZONE=2
            trigp(I+1,1)=DELTRI(0,I)+1
            trigp(I+1,2)=DELTRI(1,I)+1
            trigp(I+1,3)=DELTRI(2,I)+1
            IZONE=5
            IF (Y(DELTRI(0,I)) LT 0.) OR (Y(DELTRI(1,I)) LT 0.) $
               OR (Y(DELTRI(2,I)) LT 0) THEN IZONE=3
            IF (Y(DELTRI(0,I)) GT ymax) OR (Y(DELTRI(1,I)) GT ymax) $
               OR (Y(DELTRI(2,I)) GT ymax) THEN IZONE=4
            x1=MAX([0,MIN([(nx-1),gridmat(0,TRIGP(I+1,1)-1)])])
            x2=MAX([0,MIN([(nx-1),gridmat(0,TRIGP(I+1,2)-1)])])
            x3=MAX([0,MIN([(nx-1),gridmat(0,TRIGP(I+1,3)-1)])])
            y1=MAX([0,MIN([(ny-1),gridmat(1,TRIGP(I+1,1)-1)])])
            y2=MAX([0,MIN([(ny-1),gridmat(1,TRIGP(I+1,2)-1)])])
            y3=MAX([0,MIN([(ny-1),gridmat(1,TRIGP(I+1,3)-1)])])
            xg(I)=(x1+x2+x3)/3.
            yg(I)=(y1+y2+y3)/3.
            d1=sqrt((x3-x2)^2+(y3-y2)^2)
            d2=sqrt((x3-x1)^2+(y3-y1)^2)
            d3=sqrt((x1-x2)^2+(y1-y2)^2)
            area=ABS((x2-x1)*(y3-y1)-(x3-x1)*(y2-y1))
            a1=ASIN(((x2-x1)*(y3-y1)-(x3-x1)*(y2-y1))/(d3*d2))
            a2=ASIN(((x3-x2)*(y1-y2)-(x1-x2)*(y3-y2))/(d1*d3))
            a3=ASIN(((x1-x3)*(y2-y3)-(x2-x3)*(y1-y3))/(d2*d1))
            amin=ABS(MIN([a1,a2,a3]))
            dmin=ABS(MIN([d1,d2,d3]))
            dmax=ABS(MAX([d1,d2,d3]))
          ;IF (TRIGP(I+1,1) GT 1050 AND  TRIGP(I+1,1) LT 1053) THEN BEGIN 
          ;  PRINT,'#########DETAILS:',I+1,TRIGP(I+1,*)
          ;   PRINT,'area:',AREA,a1,a2,a3,d1,d2,d3,amin,dmin,dmax,AREA/dmax
          ;   ENDIF
            DIAGNOSTICS(I,0)=AMIN
            DIAGNOSTICS(I,1)=dmin
            DIAGNOSTICS(I,2)=2.*AREA/dmax
          ;depthtri=(gd(x1/dx,y1/dy)+gd(x2/dx,y2/dy)+gd(x3/dx,y3/dy) $
            ;  +3.*gd(xg(I)/dx,yg(I)/dy))/6.
            ;Speeds,MAX([MIN([depthtri,H1]),H0]),1/T0,cphi,cg,KH
                  d1=sqrt((x3-x2)^2+(y3-y2)^2)
                  d2=sqrt((x3-x1)^2+(y3-y1)^2)
                  d3=sqrt((x1-x2)^2+(y1-y2)^2)
                  a1=ASIN(((x2-x1)*(y3-y1)-(x3-x1)*(y2-y1))/(d3*d2))
                  a2=ASIN(((x3-x2)*(y1-y2)-(x1-x2)*(y3-y2))/(d1*d3))
                  a3=ASIN(((x1-x3)*(y2-y3)-(x2-x3)*(y1-y3))/(d2*d1))
                  amin=ABS(MIN([a1,a2,a3]))
                  dmin=ABS(MIN([d1,d2,d3]))
              IF (dmin LT ddmin) THEN BEGIN 
                ddmin=dmin
                Imin=I
              ENDIF 
                  IF amin LT !pi/10 THEN trigp(I+1,0)=0
                  DXmax(I)=DX0 ;+(DX1-DX0)*(TANH(KH)-TANH(KH0))/(TANH(KH1)-TANH(KH0))
                  DXmax(I)=DXmax(I)*( 0.5 + 1.5*amin/!pi  )
                  IF dmin LT DXmax(I)*0.1 THEN trigp(I+1,0)=1
            IF dozones THEN BEGIN
               IZONE=c_zone
               IF (Y(DELTRI(0,I)) LT 0.) OR (Y(DELTRI(1,I)) LT 0.) $
                  OR (Y(DELTRI(2,I)) LT 0) THEN IZONE=3
               IF (Y(DELTRI(0,I)) GT ymax) OR (Y(DELTRI(1,I)) GT ymax) $
                  OR (Y(DELTRI(2,I)) GT ymax) THEN IZONE=4
               IF depthtri LT H0 THEN IZONE=1
               IF depthtri GT H1 THEN IZONE=2
               trigp(I+1,0)=IZONE
            ENDIF
         ENDFOR
         IF excludeland EQ 1 THEN BEGIN
         FOR I=0,nngp-1 DO BEGIN
          IN_SEA,gridmat(0,I),gridmat(1,I),in
          IF (in EQ 0 AND gridmat(6,I) EQ 0) THEN PRINT,'WARNING 0: POINT',I+1,' IN LAND.'
         ENDFOR
         FOR I=0,ntri-1 DO BEGIN

            IF (DIAGNOSTICS(I,0) LT 0.2) THEN PRINT,'WARNING 1:', $
                  I+1,TRANSPOSE(DIAGNOSTICS(I,0:2)),TRIGP(I+1,1),TRIGP(I+1,2),TRIGP(I+1,3)
            IF (DIAGNOSTICS(I,1) LT DX0 OR DIAGNOSTICS(I,2) LT DX0 ) THEN PRINT,'WARNING 2:', $
                  I+1,TRANSPOSE(DIAGNOSTICS(I,0:2)),TRIGP(I+1,1),TRIGP(I+1,2),TRIGP(I+1,3)
         ENDFOR
         ENDIF

              PRINT,'DDMIN:',ddmin,Imin,trigp(Imin+1,*)

         IF (N_ELEMENTS(REP) GT 2) THEN PRINT,'WARNING: REPEATED POINTS',REP
         IF dozones THEN BEGIN
         FOR I=0,(ntri-1) DO BEGIN

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
         ; Sorts the triangles by first point...
         FIRSTS=LONARR(ntri+1)
         ;PRINT,'nnGP',nngp
         ;PRINT,TRANSPOSE(TRIGP)

         FIRSTS(*)=LONG(TRIGP(*,3))+(nngp+1)*LONG((TRIGP(*,2)) $
            +(nngp+1)*LONG(TRIGP(*,1)))
         ;PRINT,'FIRSTS:',FIRSTS
         ORDER=SORT(FIRSTS)
         ;PRINT,'ORDER:',ORDER
         TRIGP(*,0:3)=TRIGP(ORDER(*),0:3)
         ;PRINT,TRANSPOSE(TRIGP)
         ; Tests if all the boundary points are actually outside the domain
         nboundary=N_ELEMENTS(BOUNDARY)
         FOR I=0,nboundary-1 DO BEGIN
            IF X(BOUNDARY(I)) LE xmax AND X(BOUNDARY(I)) GE 0 $
               AND Y(BOUNDARY(I)) LE ymax AND Y(BOUNDARY(I)) GE 0 THEN $
               PRINT,'Warning, point #',I+1,' is on the boundary and in the domain'
         ENDFOR


               zones=INTARR(ntri+1)
               zones(*)=TRIGP(0:ntri,0)
               SO=(SORT(zones))
               zones2=INDGEN(ntri+1)
               zones2=zones2(SO)
               zones=zones(SO)

               zonetri=INTARR(nzone+2+ntri)
               ZC=1  ; current zone
               ZONETRI(1)=nzone+2
               ZT=1  ; numbers of triangles in zone
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
               ;print,I,ZONETRI(nzone),ZONETRI(nzone+1)-1,size(temp),temp
               ZONETRI(ZONETRI(nzone):ZONETRI(nzone+1)-1)=temp(SORT(temp))

               nzonetri=ZONETRI(nzone+1)-1

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

         ENDIF
print,'END OF TRI',ntri
         datastatus(6)=1
         RETURN
   END

;----------------------------------------------------------------------------
PRO gridbuild_draw_event,ev
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON GRIDTIMESTEPS, zoffset, updateonmove, waveperiod
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON OVERLAY, addir,adsyms,adbathy,adcoast,psyms,psymsizes,adtr,adtri

COMMON Builder,ngp,H0,H1,Hi,KH0,KH1,DX0,DX1,T0,ROA,ROI,gridbuild_click

COMMON WGene,Wgrideditor,WG,root
COMMON LOCAL_gridbuild,coordP,coordR,nxz1,nyz1,nxz2,nyz2,indexgp
COMMON TIMESTEPSUG,TIMESTEPS,NTIMESTEPS,FLAGTSTEP, TIMESTEPS_ORDER
COMMON TIMESTEPSUG2, CCON, IEN, SI, AREA, WCG, CELLVERTEX, IE_CELL, POS_CELL, C, K
COMMON TIMESTEPSUG3, SIRATIO, JCELLS, LEN, ANGLE0, CROSSDIFF, TIMESTEPS_ALL
;*******END OF COMMON BLOCKS*******************************
   CASE ev.type OF      ;case loop on action types
      0:BEGIN             ;ev.type=0 : click (button pushed down)
         coordP=CONVERT_COORD(ev.X,ev.Y, /DEVICE,/TO_DATA)
         ;coordP(0)=MIN([MAX([0.,coordP(0)]),(nx-1)*dx])
         ;coordP(1)=MIN([MAX([0.,coordP(1)]),(ny-1)*dy])
           CASE gridbuild_click OF
            1:BEGIN          ;move GP
                indexgp=0
                distmin=(coordP(0)-gridmat(0,0))^2 $
                   +(coordP(1)-gridmat(1,0))^2
                FOR I=1,nngp-1 DO BEGIN
                   dist=(coordP(0)-gridmat(0,I))^2 $
                      +(coordP(1)-gridmat(1,I))^2
                   IF (dist LT distmin) THEN BEGIN
                      indexgp=I
                      distmin=dist
                   ENDIF
                ENDFOR
                ;end of search
                ;highlights the grid point with a big triangle
                OPLOT,[gridmat(0,indexgp)],[gridmat(1,indexgp)], $
                   psym=5,SYMSIZE=1.5,COLOR=0
                indexgp=indexgp+1
                END
           2:BEGIN        ; Add point
               nngp=nngp+1
               temp=gridmat
               gridmat=DBLARR(8,nngp)
               gridmat(*,0:nngp-2)=temp
               gridmat(0,nngp-1)=coordP(0)
               gridmat(1,nngp-1)=coordP(1)
               XYtoLatLon,coordP(0),coordP(1),latdeg,latmin,londeg,lonmin
               print,'New GP:',coordP(0),coordP(1),latdeg,latmin,londeg,lonmin
               gridmat(2,nngp-1)=latdeg
               gridmat(3,nngp-1)=latmin
               gridmat(4,nngp-1)=londeg
               gridmat(5,nngp-1)=lonmin
               IF (adsyms/2) THEN BEGIN
                  strlab=strcompress(string(nngp))
                  OPLOT,[gridmat(0,nngp-1)],[gridmat(1,nngp-1)], $
                     psym=psyms(0),SYMSIZE=psymsizes(0)
                  XYOUTS,gridmat(0,nngp-1),gridmat(1,nngp-1),strlab,CHARSIZE=0.5
               ENDIF
               WIDGET_CONTROL,WGrideditor(1),TABLE_YSIZE=nngp
               WIDGET_CONTROL,WGrideditor(1), SET_VALUE=gridmat(0:7,0:nngp-1)
               END
            3:
            4:
            5:BEGIN    ; removel island island in contour
                distmin=(coordP(0)-gridmat(0,0))^2 $
                   +(coordP(1)-gridmat(1,0))^2
                found=0
                FOR I=1,nngp-1 DO BEGIN
                   dist=(coordR(0)-gridmat(0,I))^2 $
                      +(coordR(1)-gridmat(1,I))^2
                   IF (dist LT distmin) THEN BEGIN
                      index1=I
                      distmin=dist
                      found=1
                   ENDIF
                ENDFOR
;
; Finds range of node indices to be removed
;
                IF FOUND EQ 1 THEN BEGIN 
                IMIN=0L
                J=index1
                WHILE (IMIN EQ 0 AND J GT 0) DO BEGIN 
                   IF gridmat(6,J) EQ -1 THEN BEGIN 
                     IMIN=J
                   ENDIF
                   J=J-1L
                ENDWHILE
                IMAX=0L
                J=index1
                WHILE (IMAX EQ 0 AND J LT NNGP-1) DO BEGIN 
                   IF gridmat(6,J) EQ -1 THEN BEGIN 
                     IMAX=J-1
                   ENDIF
                   J=J+1L
                ENDWHILE
                selec=LONARR(4)
                selec(1)=IMIN
                selec(3)=IMAX
                idel=selec(3)-selec(1)+1  
                print,'Removing island of node ',index1, ', nodes in range:',IMIN,IMAX
                WIDGET_CONTROL,Wgrideditor(1),/DELETE_ROWS , $
                   USE_TABLE_SELECT=[-1,selec(1),5,selec(3)]
                nngp=nngp-idel ;new number of grid points
                tempo=gridmat
                gridmat=DBLARR(8,nngp)
                IF selec(1) gt 0 THEN gridmat(*,0:selec(1)-1)=tempo(*,0:selec(1)-1)
                IF selec(1) LT nngp THEN gridmat(*,selec(1):nngp-1)=tempo(*,selec(3)+1:nngp+idel-1)
                MAKE_CONTOUR

                WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
                Displaygrid
                ENDIF
                END
            ELSE:
             ENDCASE
             END
          1:BEGIN        ;ev.type=1  button release
             coordR=CONVERT_COORD(ev.X,ev.Y, /DEVICE,/TO_DATA)
             coordR(0)=MIN([MAX([0.,coordR(0)]),(nx-1)*dx])
             coordR(1)=MIN([MAX([0.,coordR(1)]),(ny-1)*dy])
             CASE gridbuild_click OF
             0:BEGIN ;gridbuild_click option to choose the position
                     ;of the first grid point. Used by the
                     ;grid builder
                IF datastatus(3) EQ 1 THEN BEGIN
                   i1=FLOOR(gridmat(0,0)/dx)
                   j1=FLOOR(gridmat(1,0)/dy)
                   IF ((i1 GE 0) AND (i1 LT nx) $
                      AND (j1 GE 0) AND (j1 LT ny)) THEN $
                      depth1=gd(i1,j1) ELSE depth1=0.
                   lev1=0
                   FOR K=1,numlevels-1 DO BEGIN
                      IF lev(K) LT depth1 THEN lev1=k
                   ENDFOR
                   OPLOT,[gridmat(0,0)],[gridmat(1,0)], $
                      psym=5,SYMSIZE=0.6,COLOR=colorind(lev1)
                   XYOUTS,gridmat(0,0),gridmat(1,0),'1', $
                      CHARSIZE=0.9,COLOR=colorind(lev1)
                ENDIF
                gridmat(0,0)=coordR(0)
                gridmat(1,0)=coordR(1)
                XYtoLatLon,coordP(0),coordP(1),latdeg,latmin,londeg,lonmin
               print,'New GP:',coordP(0),coordP(1),latdeg,latmin,londeg,lonmin
               UpdateTable,0,0        ;updates the positions
                                       ;of the first grid point
                OPLOT,[gridmat(0,0)],[gridmat(1,0)], $
                   psym=5,SYMSIZE=0.6
                XYOUTS,gridmat(0,0),gridmat(1,0),'1', $
                   CHARSIZE=0.9
                WIDGET_CONTROL,Wdraw,EVENT_PRO='analyzer_event'
                END
             1:BEGIN    ; moves grid point
                i1=FLOOR(gridmat(0,indexgp-1)/dx)
                j1=FLOOR(gridmat(1,indexgp-1)/dy)
                OPLOT,[gridmat(0,indexgp-1)],[gridmat(1,indexgp-1)], $
                   psym=5,SYMSIZE=1.5 ;,COLOR=colorind(lev1)
                strlab=strcompress(string(indexgp),/REMOVE_ALL)
                XYOUTS,gridmat(0,indexgp-1),gridmat(1,indexgp-1), $
                   strlab,CHARSIZE=0.9 ;,COLOR=colorind(lev1)
                gridmat(0,indexgp-1)=coordR(0)
                gridmat(1,indexgp-1)=coordR(1)
                XYtoLatLon,coordR(0),coordR(1),latdeg,latmin,londeg,lonmin
                gridmat(2,indexgp-1)=latdeg
                gridmat(3,indexgp-1)=latmin
                gridmat(4,indexgp-1)=londeg
                gridmat(5,indexgp-1)=lonmin
                print,'point #',indexgp
                ;IF subwin(0) THEN UpdateTable,0,indexgp-1
                OPLOT,[gridmat(0,indexgp-1)],[gridmat(1,indexgp-1)], $
                   psym=5,SYMSIZE=0.6
                XYOUTS,gridmat(0,indexgp-1),gridmat(1,indexgp-1), $
                   strlab,CHARSIZE=0.9
;
                   IF FLAGTSTEP AND UPDATEONMOVE THEN BEGIN
                     UPDATE_TIMESTEPS2,indexgp-1
                     doplot 
                   ENDIF

                END
             3:BEGIN     ;picks up the second end of the line to be drawn

               nadd=FLOOR(SQRT((coordR(0)-coordP(0))^2 $
                                   +(coordR(1)-coordP(1))^2)/DX0)+2

               xadd=FLTARR(nadd)
               yadd=FLTARR(nadd)
               naddpoint=0
               iadd=INTARR(nadd)
               FOR I = 0, nadd-1 DO BEGIN
                  Xadd(naddpoint)=coordP(0)+I*(coordR(0)-coordP(0))/(nadd-1)
                  Yadd(naddpoint)=coordP(1)+I*(coordR(1)-coordP(1))/(nadd-1)
                  d2gp=MIN((Xadd(I)-gridmat(0,0:nngp-1))^2+(Yadd(I)-gridmat(1,0:nngp-1))^2)
                  IF (d2gp GT 0.25*DX0^2) THEN BEGIN
                     print,d2gp,dx0,Xadd(naddpoint),Yadd(naddpoint)
                     naddpoint=naddpoint+1
                     iadd(I)=1
                  ENDIF
               ENDFOR
               nngp=nngp+naddpoint
               temp=gridmat
               gridmat=DBLARR(8,nngp)
               gridmat(*,0:nngp-naddpoint-1)=temp
               II=nngp-naddpoint
               FOR I = 0, nadd-1 DO BEGIN
                       IF Iadd(I) EQ 1 THEN BEGIN
                          gridmat(0,II)=Xadd(I)
                     gridmat(1,II)=Yadd(I)
                     londeg=0
                     lonmin=0.
                     latdeg=0
                     latmin=0.
                     XYtoLatLon,Xadd(I),Yadd(I),latdeg,latmin,londeg,lonmin
                     gridmat(2,II)=latdeg
                     gridmat(3,II)=latmin
                     gridmat(4,II)=londeg
                     gridmat(5,II)=lonmin
                     II=II+1
                  ENDIF
               ENDFOR
               Displaygrid
               WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
               WIDGET_CONTROL,Wdraw,EVENT_PRO='analyzer_event'
               END

            ELSE:
             ENDCASE
             END                ;when the button is released

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
                WIDGET_CONTROL,Wright(14,5), $
                   SET_VALUE=STRING(valeur,FORMAT='(F8.2)')
             ENDIF
             WIDGET_CONTROL,Wright(14,1), $
                SET_VALUE=STRING(coord(0),FORMAT='(F7.3)')
             WIDGET_CONTROL,Wright(14,3), $
                SET_VALUE=STRING(coord(1),FORMAT='(F6.2)')
             latdeg=0.
             latmin=0.
             londeg=0.
             lonmin=0.
             latlonstring=''
             XYtoLatLon,coord(0),coord(1), $
                latdeg,latmin,londeg,lonmin,latlonstring
             WIDGET_CONTROL,Wright(15,1), $
                SET_VALUE=latlonstring
             ENDIF
             ELSE:
          ENDCASE

RETURN
END

;----------------------------------------------------------------------------
PRO UpdateTable,I,J
;*******COMMON BLOCKS**************************************
;** 1 ** Display parameters
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
;** 3 ** I/O and data variables
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
;** 4 ** subwindow widgets widgets
COMMON WGene,Wgrideditor,WG,root
;*******END OF COMMON BLOCKS*******************************
   londeg=0
   lonmin=0.
   latdeg=0
   latmin=0.
   x=0.
   y=0.

   CASE 1 OF
   (I LT 2):BEGIN    ;updates the grid points latitudes and longitudes
      XYtoLatLon,gridmat(0,J),gridmat(1,J),latdeg,latmin,londeg,lonmin
      gridmat(2,J)=latdeg
      gridmat(3,J)=latmin
      gridmat(4,J)=londeg
      gridmat(5,J)=lonmin
      END
   ELSE:BEGIN       ;updates the grid points X and Y coordinates
      LatLontoxy,gridmat(2,J),gridmat(3,J),gridmat(4,J),gridmat(5,J),x,y
      gridmat(0,J)=x
      gridmat(1,J)=y
      END
   ENDCASE
   WIDGET_CONTROL, Wgrideditor(1), SET_VALUE=gridmat(0:7,J:J), $
      USE_TABLE_SELECT=[0,J,7,J]
   RETURN
END

;----------------------------------------------------------------------------
PRO Displaygrid
;*******COMMON BLOCKS**************************************
;** 1 ** Display parameters
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
;** 3 ** I/O and data variables
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
;** 4 ** subwindow widgets widgets
COMMON WGene,Wgrideditor,WG,root
;*******END OF COMMON BLOCKS*******************************
   WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
   IF (nngp EQ 1) THEN ROWLAB=['1'] ELSE ROWLAB = STRING(FINDGEN(nngp)+1, FORMAT='(I8)')
   WIDGET_CONTROL,WGrideditor(1),TABLE_YSIZE=nngp
   WIDGET_CONTROL,Wgrideditor(1),ROW_LABELS=ROWLAB
   WIDGET_CONTROL,WGrideditor(1), SET_VALUE=gridmat
END


;----------------------------------------------------------------------------
PRO IN_LAND2,x,y,in
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON COAST,   coastxy,coastl,coastnp,GSHHSPoly,GSHHSPoint,GSHHSPoly2,GSHHSPoint2
   TOTANGLE=0.
   in=0
   
   taille=SIZE(GSHHSPoly)
   Npoly=taille(1)-1
   in=0
   FOR I=0L,Npoly-1 DO BEGIN
      NP=GSHHSPoly(I+1,0)-GSHHSPoly(I,0)
      XX=GSHHSPoint(GSHHSPoly(I,0):GSHHSPoly(I+1,0)-1,0)
      YY=GSHHSPoint(GSHHSPoly(I,0):GSHHSPoly(I+1,0)-1,1)
      PNPOLY,X,Y,XX,YY,NP,INOUT
      IF INOUT EQ 1 THEN in =1
   ENDFOR
   RETURN
END

;----------------------------------------------------------------------------
PRO  PNPOLY,PX,PY,XX,YY,N,INOUT
;
; Test if point PX PY is inside or outside of polygon
;
      X=XX(*)-PX
      Y=YY(*)-PY
      INOUT=-1
      I=0L
      WHILE (I LT N-1) DO  BEGIN 
          J=1L+(I MOD N)
          MX=X(I) GE 0.0
          NX=X(J) GE 0.0
          MY=Y(I) GE 0.0
          NY=Y(J) GE 0.0
        ;  PRINT,'PNPOLY2:',N,I,J,X(I),X(J),Y(I),Y(J),MX,NX,MY,NY
;     IF(.NOT.((MY.OR.NY) AND (MX.OR.NX)).OR.(MX AND NX))
          IF( NOT ((MY OR NY) AND (MX OR NX)) OR (MX AND NX)) THEN BEGIN
             I=I+1L    
          ENDIF ELSE BEGIN 
         ;    PRINT,'PNPOLY3:',I,MX,NX,MY,NY

             IF( NOT (MY AND NY AND (MX OR NX) AND NOT (MX AND NX))) THEN BEGIN 
                INOUT=-INOUT
                RETURN
             ENDIF ELSE BEGIN 
                RAT=((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I)))
          ;      PRINT,'PNPOLY4:',I,RAT
                IF RAT EQ 0 THEN BEGIN 
                   INOUT=0  ; zero 
                   RETURN
                ENDIF ELSE BEGIN 
                   IF RAT LT 0 THEN BREAK ELSE $
                   INOUT=-INOUT  ;positive
                   I=I+1L
                ENDELSE
             ENDELSE

          ENDELSE  
      ENDWHILE
      RETURN  
      END

;----------------------------------------------------------------------------
PRO IN_SEA,x,y,in
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
   TOTANGLE=0.
   J0=contourline(0,0)
   in=0
   FOR J=contourline(0,0),contourline(1,0) DO BEGIN
      J1=J
      J2=J+1
      IF J2 GT contourline(1,0) THEN J2 =J0
      IF (contourline(2+J2-contourline(0,0),1) LT 0) THEN BEGIN
         J2=J0
         J0=J+1
      ENDIF
      x1=gridmat(0,J1)
      y1=gridmat(1,J1)
      x2=gridmat(0,J2)
      y2=gridmat(1,J2)
      ANGLE=ASIN(((x1-x)*(y2-y)-(x2-x)*(y1-y))/sqrt(((x1-x)^2+(y1-y)^2)*((x2-x)^2+(y2-y)^2)))
      TOTANGLE=TOTANGLE+ANGLE
      ; IF (I EQ 1572) THEN PRINT,'TRIN ?',I,nsea,J,J1,J2,xg,yg,TOTANGLE,ANGLE,2*!pi
      ; PRINT,'TRIN ?',J,TOTANGLE
   ENDFOR
   IF ABS(TOTANGLE-2*!pi) LT !pi THEN in=1
   RETURN
END

;----------------------------------------------------------------------------
PRO IN_SEA2,x,y,in
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
   TOTANGLE=0.
   ncont=contourline(1,0)-contourline(0,0)+2
   DJM=FLOOR(ncont/100);
   PRINT,DJM
   J0=contourline(0,0)
   in=0
   ANGLE=1
   J=contourline(0,0)
   WHILE (J LT contourline(1,0)) DO BEGIN
      J1=J
      DJMAX=MIN([DJM,MAX([1,FLOOR(1/ABS(ANGLE))])])
      DJ=1
      IND=2+J+DJ-contourline(0,0)
      ;PRINT,2+J+DJ-contourline(0,0),ncont,'##',size(contourline)
      IF (IND LT ncont) THEN BEGIN
         WHILE ((IND  LT ncont) AND (contourline(IND,1) EQ 1) AND (DJ LT DJMAX)) DO BEGIN
            DJ=DJ+1
            IND=2+J+DJ-contourline(0,0)
         ENDWHILE
      ENDIF
      J2=J1+DJ
      IF J2 GT contourline(1,0) THEN J2 =J0
      IF (contourline(2+J2-contourline(0,0),1) EQ 0) THEN BEGIN
         J2=J0
             J0=J+1
      ENDIF
      x1=gridmat(0,J1)
      y1=gridmat(1,J1)
      x2=gridmat(0,J2)
      y2=gridmat(1,J2)
      ANGLE=ASIN(((x1-x)*(y2-y)-(x2-x)*(y1-y))/sqrt(((x1-x)^2+(y1-y)^2)*((x2-x)^2+(y2-y)^2)))
      TOTANGLE=TOTANGLE+ANGLE
      J=J+DJ
   ENDWHILE
   IF ABS(TOTANGLE-2*!pi) LT !pi THEN in=1
   RETURN
END

;----------------------------------------------------------------------------
PRO GET_BOUNDARY, IOBP, NEIGHBOR
;  looks for points on the boundary ... 
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
;  Licensing:
;
;    This code is distributed under the GNU LGPL license.
;
;  Written:
;
;    Sometime in 2011
;    Adapted to IDL, by F. Ardhuin 22 June 2013
;    THERE SEEMS TO BE A BUG ... 
;
;  Author:
;
;    Mathieu Dutour Sikiric
;
;  Parameters:
;   Input:
;    NNGP: number of nodes
;    TRIGP: list of nodes
;    NTRI: number of triangles
;   Output:
;    IOBP: Boundary Points
;    NEIGHBOR
;
;  Description:
;    if a node belong to a boundary, the function
;    returns the neighbor of this point on one side.
;    if the point is interior then the value 0 is set.
;
        IE=0
        I=0
        OP=0

        STATUS=INTARR(NNGP)
        COLLECTED=INTARR(NNGP)
        PREVVERT=INTARR(NNGP)
        NEXTVERT=INTARR(NNGP)
        NEIGHBOR=INTARR(NNGP)
        NEIGHBOR(*)=0
        STATUS(*) = 0
        NEXTVERT(*) = 0
        PREVVERT(*) = 0
        FOR IE=1,NTRI DO BEGIN
          FOR I=1,3 DO BEGIN
            IF (I EQ 1) THEN BEGIN
              IPREV=3
            ENDIF ELSE BEGIN
              IPREV=I-1
            ENDELSE
            IF (I EQ 3) THEN BEGIN
              INEXT=1
            ENDIF ELSE BEGIN
              INEXT=I+1
            ENDELSE
            IP=TRIGP(IE,I)
            IPNEXT=TRIGP(IE,INEXT)
            IPPREV=TRIGP(IE,IPREV)
            IF (STATUS(IP-1) EQ 0) THEN BEGIN
              STATUS(IP-1)=1
              PREVVERT(IP-1)=IPPREV
              NEXTVERT(IP-1)=IPNEXT
            ENDIF
          ENDFOR
        ENDFOR
        STATUS(*)=0

        ISFINISHED=0        
        WHILE ISFINISHED EQ 0 DO BEGIN 
          COLLECTED(*)=0
          FOR IE=1,NTRI DO BEGIN
            FOR I=1,3 DO BEGIN
              IF (I EQ 1) THEN BEGIN
                IPREV=3
              ENDIF ELSE BEGIN
                IPREV=I-1
              ENDELSE
              IF (I EQ 3) THEN BEGIN
                INEXT=1
              ENDIF ELSE BEGIN
                INEXT=I+1
              ENDELSE
              IP=TRIGP(IE,I)
              IPNEXT=TRIGP(IE,INEXT)
              IPPREV=TRIGP(IE,IPREV)
              IF (STATUS(IP-1) EQ 0) THEN BEGIN
                ZNEXT=NEXTVERT(IP-1)
                IF (ZNEXT EQ IPPREV) THEN BEGIN
                  COLLECTED(IP-1)=1
                  NEXTVERT(IP-1)=IPNEXT
                  IF (NEXTVERT(IP-1) EQ PREVVERT(IP-1)) THEN BEGIN
                    STATUS(IP-1)=1
                  ENDIF
                ENDIF
              ENDIF
            ENDFOR
          ENDFOR

          ISFINISHED=1
          FOR IP=0,NNGP-1 DO BEGIN 
            IF ((COLLECTED(IP) EQ 0) AND (STATUS(IP) EQ 0)) THEN BEGIN
              STATUS(IP)=-1
              NEIGHBOR(IP)=NEXTVERT(IP)     ; new code
            ENDIF
            IF (STATUS(IP) EQ 0) THEN BEGIN
              ISFINISHED=0
            ENDIF
          ENDFOR
          IF (ISFINISHED EQ 1) THEN BEGIN
            ISFINISHED=0
          ENDIF
        ENDWHILE

        IOBP(*) = 0
        FOR IP=0,NNGP-1 DO BEGIN 
          IF (STATUS(IP) EQ -1  AND  IOBP(IP)  EQ  0) THEN BEGIN
            IOBP(IP)=1
          ENDIF
        ENDFOR
; Now some basic consistency checks
; The points that are neighbor should be organized in cycles.
; That is every node should be a neighbor and have a neighbor
; and only one
        NBneighbor=INTARR(NNGP)
        NBneighbor=0
        FOR IP=0,NNGP-1 DO BEGIN 
          eIdx=NEIGHBOR(IP)
          IF (eIdx GT 0) THEN BEGIN
            NBneighbor(eIdx)=NBneighbor(eIdx)+1
          ENDIF
        ENDFOR
        HaveError=0
        FOR IP=0,NNGP-1 DO BEGIN 
          IF (NBneighbor(IP) GT 1) THEN BEGIN
            Print , 'Inconsistency in the output'
            Print , '  Vertex ', IP, ' is ', NBneighbor(IP), ' times neighbor'
            HaveError=1
          ENDIF
          IF ((NBneighbor(IP) EQ 1) AND (NEIGHBOR(IP) EQ 0)) THEN BEGIN
            Print , 'Inconsistency in the output'
            Print , '  Vertex ', IP, ' is a neighbor'
            Print , '  but has no neighbor!'
            HaveError=1
          ENDIF
          IF ((NBneighbor(IP) EQ 0) AND (NEIGHBOR(IP) GT 0)) THEN BEGIN
            Print , 'Inconsistency in the output'
            Print , '  Vertex ', IP, ' has a neighbor'
            Print , '  but is not a neighbor!'
            HaveError=1
          ENDIF
        ENDFOR
        IF (HaveError EQ 1) THEN BEGIN
          Print , 'Find some errors in the output'
          Print , 'Please check for node contained in several boundaries'
        ENDIF
        RETURN
        END 

;**********************************************************************
;*                                                                    *
;**********************************************************************
      PRO GetCycleBoundaries, IOBP, NEIGHBOR, NbCycle, LengthCycle, FirstInCycle, StatusCycle
;  Licensing:
;
;    This code is distributed under the GNU LGPL license.
;
;  Written:
;
;    Sometime in 2011
;
;  Author:
;
;    Mathieu Dutour Sikiric
;
;  Parameters:
;   Input:
;    NNGP: number of nodes
;    TRIGP: list of nodes
;    NTRI: number of triangles
;    IOBP: List of Boundary Points
;    NEIGHBOR: neighbor of a point.
;
;   Output:
;    NbCycle
;    LengthCycle: list of length of cycles
;    FirstInCycle: First element in the cycle
;    StatusCycle: For every element specification to which
;                 cycle it belongs
;
        STATUS=INTARR(NNGP)
        LengthCycle=INTARR(NNGP)
        FirstInCycle=INTARR(NNGP)
        StatusCycle=INTARR(NNGP)
        STATUS(*) = 0
        FOR IP=0,NNGP-1 DO BEGIN 
          IF (IOBP(IP) EQ 1) THEN STATUS(IP)=1
        ENDFOR
        
        NbCycle = 0
        FOR IP = 0, NNGP-1 DO BEGIN 
          IF (STATUS(IP) EQ 1) THEN BEGIN
            NbCycle=NbCycle+1
            IPfirst=IP
            IPwork=IP
            FINISHED=0
            WHILE FINISHED EQ 0 DO BEGIN 
              IF (IPwork EQ 0) THEN FINISHED=1
              IF (FINISHED EQ 0) THEN BEGIN 
                STATUS(IPwork) = 0
                IPwork=NEIGHBOR(IPwork)
                IF (IPwork EQ IPfirst) THEN BEGIN
                  FINISHED = 1
                ENDIF
              ENDIF 
            ENDWHILE
          ENDIF
        ENDFOR

        FOR IP=0,NNGP-1 DO BEGIN 
          IF (IOBP(IP) EQ 1) THEN STATUS(IP) = 1
        ENDFOR
        
        iCycle=0
        FOR IP=0,NNGP-1 DO BEGIN 
          IF (STATUS(IP) EQ 1) THEN BEGIN
            iCycle=iCycle+1
            IPfirst=IP
            FirstInCycle(iCycle)=IPfirst
            IPwork=IP
            TheLength=0
            FINISHED=0
            WHILE (FINISHED EQ 0) DO BEGIN 
              IF (IPwork EQ 0) THEN FINISHED = 1
              IF (FINISHED EQ 0) THEN BEGIN 
                STATUS(IPwork)=0
                TheLength=TheLength+1
                StatusCycle(IPwork)=iCycle
                IPwork=NEIGHBOR(IPwork)
                IF (IPwork EQ IPfirst) THEN FINISHED = 1
              ENDIF
            ENDWHILE
            LengthCycle(iCycle) = TheLength
          ENDIF
        ENDFOR
      RETURN 
      END

;----------------------------------------------------------
PRO MESH_CLEAN_4,IPDEL
;  Removes node and triangles with 4 neighbors
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON WGene,Wgrideditor,WG,root
;*******END OF COMMON BLOCKS*******************************
;
      TRIGP_OLD=TRIGP(*,1:3)
      indtri=WHERE(TRIGP_OLD EQ IPDEL,kount)   

      IF (kount EQ 4) THEN BEGIN 
        PRINT,'Cleaning up 4-neighbor node:',IPDEL
        FOR ITRI=0,kount-1 DO BEGIN 
          ;ind1 = ARRAY_INDICES(TRIGP_OLD, indtri(ITRI)) 
          ARRAY_IND2,TRIGP_OLD, indtri(ITRI),ind1
          PRINT,'Node ', IPDEL,' is in element', ind1(0),ind1(1),TRANSPOSE(TRIGP_OLD(ind1(0),*))
        ENDFOR   

        TRI2=INDGEN(3)*0
        ;ind1 = ARRAY_INDICES(TRIGP_OLD, indtri(0)) 
        ;ind1 = ARRAY_IND2(TRIGP_OLD, indtri(0)) 
        ARRAY_IND2,TRIGP_OLD, indtri(0),ind1
        ind12=((ind1(1)+1) MOD 3)+1
        ind13=((ind1(1)+2) MOD 3)+1

        FOR ITRI=1,kount-1 DO BEGIN 
          ;indn = ARRAY_INDICES(TRIGP_OLD, indtri(ITRI)) 
          ;indn = ARRAY_IND2(TRIGP_OLD, indtri(ITRI)) 
          ARRAY_IND2,TRIGP_OLD, indtri(ITRI),indn
          indn2=((indn(1)+1) MOD 3)+1
          indn3=((indn(1)+2) MOD 3)+1
; looks for other common point of 2 triangles
          ;PRINT,'1', TRI2(ITRI-1),indn(0),indn(1),indn2,indn3,TRIGP_OLD(indn(0),*)       
          IF (    TRIGP(indn(0),indn2) EQ TRIGP(ind1(0),ind12)) THEN TRI2(ITRI-1)=1
          ; PRINT,'2', TRI2(ITRI-1),indn(0),indn(1),indn2,indn3,TRIGP_OLD(indn(0),*)       
          IF (    TRIGP(indn(0),indn3) EQ TRIGP(ind1(0),ind12)) THEN TRI2(ITRI-1)=1
          ;PRINT,'3', TRI2(ITRI-1),indn(0),indn(1),indn2,indn3,TRIGP_OLD(indn(0),*)       
          IF (    TRIGP(indn(0),indn2) EQ TRIGP(ind1(0),ind13)) THEN TRI2(ITRI-1)=1
          ;PRINT,'4', TRI2(ITRI-1),indn(0),indn(1),indn2,indn3,TRIGP_OLD(indn(0),*)       
          IF (    TRIGP(indn(0),indn3) EQ TRIGP(ind1(0),ind13)) THEN TRI2(ITRI-1)=1
          IF (TRI2(ITRI-1) EQ 0) THEN BEGIN 
            ind2 = indn  ; this is the opposite triangle    
            ind22=indn2
            ind23=indn3
          ENDIF 
          ;PRINT,'C_GP IN', ITRI-1,TRI2(ITRI-1),indn(0),indn(1),indn2,indn3,TRIGP_OLD(indn(0),*)       
        ENDFOR
; looks for opposite nodes
; assumes that ind22 and ind12 are opposite
        FLIP=0
        FOUND=0
        FOR ITRI=1,kount-1 DO BEGIN 
          ;indn = ARRAY_INDICES(TRIGP_OLD, indtri(ITRI)) 
          ;indn = ARRAY_IND2(TRIGP_OLD, indtri(ITRI)) 
          ARRAY_IND2,TRIGP_OLD, indtri(ITRI),indn
          IF (indn(0) NE ind2(0)) THEN BEGIN 
            indn2=((indn(1)+1) MOD 3)+1
            indn3=((indn(1)+2) MOD 3)+1
            IF (    TRIGP(indn(0),indn2) EQ TRIGP(ind1(0),ind12) $
                AND TRIGP(indn(0),indn3) EQ TRIGP(ind2(0),ind22)) THEN FLIP=1
            IF (FOUND EQ 0) THEN BEGIN 
              ind3=indn
              FOUND=1
            ENDIF ELSE BEGIN 
              ind4=indn
            ENDELSE
              
          ENDIF
        ENDFOR
        IF (FLIP EQ 1) THEN BEGIN 
          temp=ind22
          ind22=ind23
          ind23=temp
        ENDIF
        print,'Removing node in triangles:'
        print,'node ',IPDEL,TRIGP_OLD(ind1(0),ind1(1)),' in tri. ',ind1(0), $ 
              ' replaced by ', TRIGP_OLD(ind2(0),ind23-1)
        TRIGP_OLD(ind1(0),ind1(1))=TRIGP_OLD(ind2(0),ind23-1)
        print,'node ',IPDEL,TRIGP_OLD(ind2(0),ind2(1)),' in tri. ',ind2(0), $ 
              ' replaced by ', TRIGP_OLD(ind1(0),ind13-1)
        TRIGP_OLD(ind2(0),ind2(1))=TRIGP_OLD(ind1(0),ind13-1)
;
        print,'Removing triangles:',ind3(0),' and ',ind4(0)
        NTRI=NTRI-2
        temp3=ind3
        temp4=ind4
        IF (ind3(0) GT ind4(0)) THEN BEGIN 
          ind4=temp3
          ind3=temp4
        ENDIF 

        TRIGP=LONARR(ntri+1,4)
        TRIGP(0:ind3(0)-1,1:3)=TRIGP_OLD(0:ind3(0)-1,0:2)
        TRIGP(ind3(0):ind4(0)-2,1:3)=TRIGP_OLD(ind3(0)+1:ind4(0)-1,0:2)
        TRIGP(ind4(0)-1:ntri,1:3)=TRIGP_OLD(ind4(0)+1:ntri+2,0:2)

         
        FOR ITRI=1,NTRI DO BEGIN 
          FOR II=1,3 DO BEGIN 
            I1=TRIGP(ITRI,II)
            IF (I1 GE IPDEL) THEN TRIGP(ITRI,II)=TRIGP(ITRI,II)-1
          ENDFOR
        ENDFOR

        idel= 1;number of deleted grid points
        WIDGET_CONTROL,Wgrideditor(1),/DELETE_ROWS , $
        USE_TABLE_SELECT=[-1,IPDEL-1,5,IPDEL-1]
        nngp=nngp-idel ;new number of grid points
        tempo=gridmat
        gridmat=DBLARR(8,nngp)
        IF IPDEL gt 1 THEN gridmat(*,0:IPDEL-2)=tempo(*,0:IPDEL-2)
        IF IPDEL LT nngp THEN gridmat(*,IPDEL-1:nngp-1)=tempo(*,IPDEL:nngp)
        MAKE_CONTOUR
        WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
        Displaygrid
        PRINT,'Point ',IPDEL,' has been deleted.'

      ENDIF ; kount = 4

      RETURN 
      END

;----------------------------------------------------------
PRO MESH_CLEAN_3,IPDEL
;  Removes node and triangles with 3 neighbors (no boundary)
;  Only if angle of remaining triangle is less than ANGMAX
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON WGene,Wgrideditor,WG,root
;*******END OF COMMON BLOCKS*******************************
;
              TRIGP_OLD=TRIGP(*,1:3)
              indtri=WHERE(TRIGP_OLD EQ IPDEL,kount)   
              PRINT,'Removing node with 3 neighbors?',IPDEL,kount

              IF (kount EQ 3) THEN BEGIN 

                FOR ITRI=0,kount-1 DO BEGIN 
                  ARRAY_IND2,TRIGP_OLD, indtri(ITRI),ind1
                  PRINT,'C_GP IN', ind1(0),ind1(1),TRANSPOSE(TRIGP_OLD(ind1(0),*))
                  ;PRINT,'bound? ', IOBP(TRANSPOSE(TRIGP_OLD(ind1(0),*))-1)
                  ;SUMIOBP=SUMIOBP+TOTAL(IOBP(TRANSPOSE(TRIGP_OLD(ind1(0),*))-1))
                ENDFOR   

                ARRAY_IND2,TRIGP_OLD, indtri(0),ind1
                ARRAY_IND2,TRIGP_OLD, indtri(1),ind2
                ARRAY_IND2,TRIGP_OLD, indtri(2),ind3


                temp1=ind1
                temp2=ind2
                IF (ind1(0) GT ind2(0)) THEN BEGIN 
                  ind2=temp1
                  ind1=temp2
                ENDIF 
                temp1=ind1
                temp3=ind3
                IF (ind1(0) GT ind3(0)) THEN BEGIN 
                  ind3=temp1
                  ind1=temp3
                ENDIF 
                ind12=((ind1(1)+1) MOD 3)
                ind13=((ind1(1)+2) MOD 3)
                ind22=((ind2(1)+1) MOD 3)
                ind23=((ind2(1)+2) MOD 3)
                ind32=((ind3(1)+1) MOD 3)
                ind33=((ind3(1)+2) MOD 3)
                ind2n=ind22
; Finds node to be used for replacement 
                IF (TRIGP_OLD(ind2(0),ind22) EQ  TRIGP_OLD(ind1(0),ind12) OR  $
                     TRIGP_OLD(ind2(0),ind22) EQ  TRIGP_OLD(ind1(0),ind13) ) THEN ind2n=ind23
                
                ind1c=ind12
                ind1a=ind13
                  

                  print,'Removing node in triangles:'
                  print,'node ',IPDEL,' in triangle ',ind1(0), $ 
                      ' replaced by', TRIGP_OLD(ind2(0),ind2n)
                  TRIGP_OLD(ind1(0),ind1(1))=TRIGP_OLD(ind2(0),ind2n)
;
                  print,'Removing triangles:',ind2(0),ind3(0)

                  NTRI=NTRI-2
                  TRIGP=LONARR(ntri+1,4)
                  TRIGP(0:ind2(0)-1,1:3)=TRIGP_OLD(0:ind2(0)-1,0:2)
                  TRIGP(ind2(0):ind3(0)-2,1:3)=TRIGP_OLD(ind2(0)+1:ind3(0)-1,0:2)
                  TRIGP(ind3(0)-1:ntri,1:3)=TRIGP_OLD(ind3(0)+1:ntri+2,0:2)
;
; Shifts indices in remaining triangles
;
                  FOR ITRI=1,NTRI DO BEGIN 
                    FOR II=1,3 DO BEGIN 
                      I1=TRIGP(ITRI,II)
                      IF (I1 GE IPDEL) THEN TRIGP(ITRI,II)=TRIGP(ITRI,II)-1
                    ENDFOR
                  ENDFOR

                  idel= 1;number of deleted grid points
                  WIDGET_CONTROL,Wgrideditor(1),/DELETE_ROWS , $
                  USE_TABLE_SELECT=[-1,IPDEL-1,5,IPDEL-1]
                  nngp=nngp-idel ;new number of grid points
                  tempo=gridmat
                  gridmat=DBLARR(8,nngp)
                  IF IPDEL gt 1 THEN gridmat(*,0:IPDEL-2)=tempo(*,0:IPDEL-2)
                  IF IPDEL LT nngp THEN gridmat(*,IPDEL-1:nngp-1)=tempo(*,IPDEL:nngp)
                  MAKE_CONTOUR
                  WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
                  Displaygrid
                  PRINT,'Point ',IPDEL,' has been deleted.'

              ENDIF ; kount = 3
      RETURN 
      END


;----------------------------------------------------------
PRO MESH_CLEAN_2,IPDEL,ANGMAX, DELOK
;  Removes node and triangles with 2 neighbors
;  Only if angle of remaining triangle is less than ANGMAX
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON WGene,Wgrideditor,WG,root
COMMON UNDO,gridmat0,nngp0,ntri0,trigp0,contourline0

;*******END OF COMMON BLOCKS*******************************
;
              TRIGP_OLD=TRIGP(*,1:3)
              indtri=WHERE(TRIGP_OLD EQ IPDEL,kount)   
              PRINT,'Removing boundary node with 3 neighbors?',IPDEL,kount,ntri0,ntri

              IF (kount EQ 2) THEN BEGIN 

                FOR ITRI=0,kount-1 DO BEGIN 
                  ;ind1 = ARRAY_INDICES(TRIGP_OLD, indtri(ITRI)) 
                  ;ind1 = ARRAY_IND2(TRIGP_OLD, indtri(ITRI)) 
                  ARRAY_IND2,TRIGP_OLD, indtri(ITRI),ind1
                  PRINT,'C_GP IN', ind1(0),ind1(1),TRANSPOSE(TRIGP_OLD(ind1(0),*))
                  ;PRINT,'bound? ', IOBP(TRANSPOSE(TRIGP_OLD(ind1(0),*))-1)
                  ;SUMIOBP=SUMIOBP+TOTAL(IOBP(TRANSPOSE(TRIGP_OLD(ind1(0),*))-1))
                ENDFOR   

                ;ind1 = ARRAY_INDICES(TRIGP_OLD, indtri(0)) 
                ;ind2 = ARRAY_INDICES(TRIGP_OLD, indtri(1)) 
                ;ind1 = ARRAY_IND2(TRIGP_OLD, indtri(0)) 
                ;ind2 = ARRAY_IND2(TRIGP_OLD, indtri(1)) 
ARRAY_IND2,TRIGP_OLD, indtri(0),ind1
ARRAY_IND2,TRIGP_OLD, indtri(1),ind2
                temp1=ind1
                temp2=ind2
                IF (ind1(0) GT ind2(0)) THEN BEGIN 
                  ind2=temp1
                  ind1=temp2
                ENDIF 
                ind12=((ind1(1)+1) MOD 3)
                ind13=((ind1(1)+2) MOD 3)
                ind22=((ind2(1)+1) MOD 3)
                ind23=((ind2(1)+2) MOD 3)
                ind2n=ind22
                ind1c=ind12
                ind1a=ind13
; looks for other non-common point of triangle # 2, will be new point of triangle #1
                  IF (    TRIGP_OLD(ind2(0),ind22) EQ TRIGP_OLD(ind1(0),ind12)) THEN BEGIN 
                    ind2n=ind23
                    ind1c=ind12
                    ind1a=ind13
                  ENDIF
                  IF (    TRIGP_OLD(ind2(0),ind22) EQ TRIGP_OLD(ind1(0),ind13)) THEN BEGIN 
                    ind2n=ind23
                    ind1c=ind13
                    ind1a=ind12
                  ENDIF
                  IF (    TRIGP_OLD(ind2(0),ind23) EQ TRIGP_OLD(ind1(0),ind12)) THEN BEGIN 
                    ind2n=ind22
                    ind1c=ind12
                    ind1a=ind13
                  ENDIF
                  IF (    TRIGP_OLD(ind2(0),ind23) EQ TRIGP_OLD(ind1(0),ind13)) THEN BEGIN 
                    ind2n=ind22
                    ind1c=ind13
                    ind1a=ind12
                  ENDIF
                  
                  xc=gridmat(0,TRIGP_OLD(ind1(0),ind1c)-1)
                  yc=gridmat(1,TRIGP_OLD(ind1(0),ind1c)-1)
                  xa=gridmat(0,TRIGP_OLD(ind1(0),ind1a)-1)
                  ya=gridmat(1,TRIGP_OLD(ind1(0),ind1a)-1)
                  xb=gridmat(0,TRIGP_OLD(ind2(0),ind2n)-1)
                  yb=gridmat(1,TRIGP_OLD(ind2(0),ind2n)-1)
                  dxa=(xa-xc)
                  dxb=(xb-xc)
                  dya=(ya-yc) 
                  dyb=(yb-yc)
                  d1=sqrt(  dxa^2 + dya^2 )
                  d2=sqrt(  dxb^2 + dyb^2 )
                  SIA = (dxa*dyb-dxb*dya)
                  COA = (dxa*dxb+dya*dyb)
                  ANGLE=ABS(ATAN(SIA,COA)/!dtor)
               IF (ANGLE LT ANGMAX) THEN BEGIN 
                  print,'Removing node in triangles:'
                  print,'node ',IPDEL,ANGLE,d1,d2,'in tri. ',ind1(0), $ 
                      ' replaced by', TRIGP_OLD(ind2(0),ind2n)
                  TRIGP_OLD(ind1(0),ind1(1))=TRIGP_OLD(ind2(0),ind2n)
;
                  print,'Removing triangles:',ind2(0)
                  NTRI=NTRI-1
                  TRIGP=LONARR(ntri+1,4)
                  TRIGP(0:ind2(0)-1,1:3)=TRIGP_OLD(0:ind2(0)-1,0:2)
                  IF (ind2(0) LE NTRI) THEN TRIGP(ind2(0):ntri,1:3)=TRIGP_OLD(ind2(0)+1:ntri+1,0:2)
;
                  FOR ITRI=1,NTRI DO BEGIN 
                    FOR II=1,3 DO BEGIN 
                      I1=TRIGP(ITRI,II)
                      IF (I1 GE IPDEL) THEN TRIGP(ITRI,II)=TRIGP(ITRI,II)-1
                    ENDFOR
                  ENDFOR

                  idel= 1;number of deleted grid points
                  WIDGET_CONTROL,Wgrideditor(1),/DELETE_ROWS , $
                  USE_TABLE_SELECT=[-1,IPDEL-1,5,IPDEL-1]
                  nngp=nngp-idel ;new number of grid points
                  tempo=gridmat
                  gridmat=DBLARR(8,nngp)
                  IF IPDEL gt 1 THEN gridmat(*,0:IPDEL-2)=tempo(*,0:IPDEL-2)
                  IF IPDEL LT nngp THEN gridmat(*,IPDEL-1:nngp-1)=tempo(*,IPDEL:nngp)
                  MAKE_CONTOUR
                  WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
                  Displaygrid
                  PRINT,'Point ',IPDEL,' has been deleted.'
               ENDIF ELSE BEGIN
                 PRINT,'No removal, angle too large :',ANGLE, ' > ', ANGMAX 
                 DELOK= 0
               ENDELSE
              ENDIF ; kount = 2
      RETURN 
      END


;----------------------------------------------------------
PRO MESH_CLEAN_1,IPDEL
;  Removes node and triangles with 2 neighbors
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON WGene,Wgrideditor,WG,root
;*******END OF COMMON BLOCKS*******************************
;

              TRIGP_OLD=TRIGP(*,1:3)
              indtri=WHERE(TRIGP_OLD EQ IPDEL,kount)   
              IF (kount EQ 1) THEN BEGIN 
                PRINT,'Removing node with 2 neighbors:',IPDEL,kount
                ARRAY_IND2,TRIGP_OLD, indtri(0),ind1
                ind12=((ind1(1)+1) MOD 3)+1
                ind13=((ind1(1)+2) MOD 3)+1
;
                print,'Removing triangle:',ind1(0)
                NTRI=NTRI-1
                TRIGP=LONARR(ntri+1,4)
                TRIGP(0:ind1(0)-1,1:3)=TRIGP_OLD(0:ind1(0)-1,0:2)
                IF (ind1(0) LT NTRI) THEN TRIGP(ind1(0):ntri,1:3)=TRIGP_OLD(ind1(0)+1:ntri+1,0:2)
;
                FOR ITRI=1,NTRI DO BEGIN 
                  FOR II=1,3 DO BEGIN 
                    I1=TRIGP(ITRI,II)
                    IF (I1 GE IPDEL) THEN TRIGP(ITRI,II)=TRIGP(ITRI,II)-1
                  ENDFOR
                ENDFOR

                idel= 1;number of deleted grid points
                WIDGET_CONTROL,Wgrideditor(1),/DELETE_ROWS , $
                  USE_TABLE_SELECT=[-1,IPDEL-1,5,IPDEL-1]
                  nngp=nngp-idel ;new number of grid points
                  tempo=gridmat
                gridmat=DBLARR(8,nngp)
                IF IPDEL gt 1 THEN gridmat(*,0:IPDEL-2)=tempo(*,0:IPDEL-2)
                IF IPDEL LT nngp THEN gridmat(*,IPDEL-1:nngp-1)=tempo(*,IPDEL:nngp)
                MAKE_CONTOUR
                WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
                Displaygrid
                PRINT,'Point ',IPDEL,' has been deleted.'
              ENDIF
      RETURN 
      END


;----------------------------------------------------------------------------
PRO MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH
;  looks for points on the boundary ... 
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
;*******END OF COMMON BLOCKS*******************************
         CCON =INTARR(NNGP)
         NEIGH=INTARR(NNGP)
         IOBP=INTARR(NNGP)
         ICOMAX=0
         COUNTRI=0
;
; Builds neighbor node number array CCON
;
         FOR IE = 1 , NTRI DO BEGIN 
           FOR II=0,2 DO BEGIN
              IP = TRIGP(IE,II+1)
              CCON(IP-1) = CCON(IP-1) + 1
              IF CCON(IP-1) GT COUNTRI THEN BEGIN 
                COUNTRI= CCON(IP-1)
                ICOMAX=IP
              ENDIF
            ENDFOR
          ENDFOR
;        COUNTRI = MAX(CCON)
;
; Builds neighbor triangle number array NEIGH
;
        MAXNEIGH=2*COUNTRI
        VNEIGH=LONARR(NNGP,MAXNEIGH)

        FOR IE = 1 , NTRI DO BEGIN 
          FOR II=1,3 DO BEGIN 
            IP = TRIGP(IE,II)    ; node for which data is gathered
            FOR I2=0,1 DO BEGIN  
              IP2 = TRIGP(IE,1+((II+I2) MOD 3)); neighbor of this node
              FOUND=0
              FOR INEI =0, NEIGH(IP-1)-1 DO BEGIN 
                IF ((IP2-1) EQ VNEIGH(IP-1,INEI)) THEN FOUND = 1 
              ENDFOR
              IF (FOUND EQ 0) THEN BEGIN 
                VNEIGH(IP-1,NEIGH(IP-1)) = IP2-1
                NEIGH(IP-1)=NEIGH(IP-1)+1  
              ENDIF 
            ENDFOR
          ENDFOR 
        ENDFOR 
;
; Compares the 2 arrays
;
        FOR IX = 0, NNGP-1 DO BEGIN 
          IF (CCON(IX) LT NEIGH(IX)) THEN IOBP(IX)=1 ELSE IOBP(IX)=0
        ENDFOR
    RETURN
END 

;----------------------------------------------------------------------------
PRO MSH_BOUNDARY1,IP1,IOBP
;  looks for points on the boundary ... 
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
;*******END OF COMMON BLOCKS*******************************
         CCON =0
         IOBP =0
         NEIGH=0
;
; Builds neighbor triangle number array NEIGH and CCON
;
        MAXNEIGH=20
        VNEIGH=LONARR(MAXNEIGH)
        
        FOR IE = 1 , NTRI DO BEGIN 
          FOR II=1,3 DO BEGIN 
            IP = TRIGP(IE,II)    ; node for which data is gathered
            IF (IP EQ IP1+1) THEN BEGIN 
              CCON = CCON +1 
              FOR I2=0,1 DO BEGIN  
                IP2 = TRIGP(IE,1+((II+I2) MOD 3)); neighbor of this node
                FOUND=0
                FOR INEI =0, NEIGH-1 DO BEGIN 
                  IF (IP2 EQ VNEIGH(INEI)) THEN FOUND = 1 
                ENDFOR
                IF (FOUND EQ 0) THEN BEGIN 
                  VNEIGH(NEIGH) = IP2
                  NEIGH=NEIGH+1  
                ENDIF 
              ENDFOR
            ENDIF
          ENDFOR 
        ENDFOR 
;
; Compares the 2 values
;
        IF (CCON LT NEIGH ) THEN IOBP=1 ELSE IOBP=0
    RETURN
END 

;----------------------------------------------------------
PRO DISPERSION,SIG,DEP,WCG
;  Inverts dispersion
; inverts the linear dispersion relation (2*pi*f)^2=g*k*tanh(k*dep) to get 
; k from f and dep. 2 Arguments: f and dep. 
eps=0.000001;	
g=9.81;
   NDEP=N_ELEMENTS(DEP)
   WCG=FLTARR(NDEP)
   FOR I=0,NDEP-1 DO BEGIN 
   Y=dep(I)*sig^2./g ;   this is the squared adimensional frequency
	X=sqrt(Y);
	F=1.;
   while abs(F) GT eps DO BEGIN
		H=tanh(X);
		F=Y-X*H;
		FD=-H-X/cosh(X)^2;
		X=X-F/FD;
   endWHILE
   WNUM = X/DEP(I)
   WCG(I)=SIG/WNUM*(0.5+X/SINH(2*X))
   ;PRINT,'TEST:',I,DEP(I),WCG(I),Y,X*TANH(X)
   ENDFOR
RETURN 
END   

;----------------------------------------------------------
PRO UPDATE_TIMESTEPS
;  Recomputes timesteps
; 
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON GRIDTIMESTEPS, zoffset, updateonmove, waveperiod
COMMON TRIANGLES,ntri,TRIGP,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON TIMESTEPSUG,TIMESTEPS,NTIMESTEPS,FLAGTSTEP, TIMESTEPS_ORDER
COMMON TIMESTEPSUG2, CCON, IEN, SI, AREA, WCG, CELLVERTEX, IE_CELL, POS_CELL, C, K
COMMON TIMESTEPSUG3, SIRATIO, JCELLS, LEN, ANGLE0, CROSSDIFF, TIMESTEPS_ALL
COMMON GRIDTIMESTEPS, zoffset, updateonmove, waveperiod
;*******END OF COMMON BLOCKS*******************************
;
    NTIMESTEPS=MIN([NNGP,200])
    IEN = FLTARR(6,NTRI)
    AREA = FLTARR(NTRI)
    CCON = INTARR(NNGP)
    LEN = FLTARR(NTRI,3)
    ANGLE0 = FLTARR(NTRI,3)
    CROSSDIFF = FLTARR(6,NTRI)
    SI = FLTARR(NNGP)
    ONETHIRD=1/3.
    ONESIXTH=1/6.
    THR=1.E-20
    IF (NNGP GT 15000) THEN  message,'Computing time steps for mesh ... please wait',/INFO
    MSH_BOUNDARY,IOBP,VNEIGH,MAXNEIGH,NEIGH

    SIRATIO = FLTARR(NNGP)+1/THR
    TMPINV = FLTARR(3)
    TMP    = FLTARR(3)
    FOR IE = 0, NTRI-1 DO BEGIN 
      I1 = TRIGP(IE+1,1)-1
      I2 = TRIGP(IE+1,2)-1
      I3 = TRIGP(IE+1,3)-1
      NI = TRIGP(IE+1,*)
      DXP1 = gridmat(0,I2) - gridmat(0,I1)
      DYP1 = gridmat(1,I2) - gridmat(1,I1)
      DXP2 = gridmat(0,I3) - gridmat(0,I2)
      DYP2 = gridmat(1,I3) - gridmat(1,I2)
      DXP3 = gridmat(0,I1) - gridmat(0,I3)
      DYP3 = gridmat(1,I1) - gridmat(1,I3)
      IEN(0,IE) = - DYP2
      IEN(1,IE) =   DXP2
      IEN(2,IE) = - DYP3
      IEN(3,IE) =   DXP3
      IEN(4,IE) = - DYP1
      IEN(5,IE) =   DXP1

      LEN(IE,0) = SQRT(DXP2^2+DYP2^2)
      LEN(IE,1) = SQRT(DXP3^2+DYP3^2)
      LEN(IE,2) = SQRT(DXP1^2+DYP1^2)   


      TMPINV(0) = 1./ (LEN(IE,1) * LEN(IE,2))
      TMPINV(1) = 1./ (LEN(IE,0) * LEN(IE,2))
      TMPINV(2) = 1./ (LEN(IE,1) * LEN(IE,0))

      TMP(0)  = -1.*(DXP1*DXP3 +DYP1*DYP3) * TMPINV(0)
      TMP(1)  = -1.*(DXP2*DXP1 +DYP2*DYP1) * TMPINV(1)
      TMP(2)  = -1.*(DXP3*DXP2 +DYP3*DYP2) * TMPINV(2)
;
;  angles used in gradients computation 
;  normalizes angles (OK if not on boundary)
;     
      ANGLE0(IE,0) = ACOS(TMP(0))/(2*!pi)
      ANGLE0(IE,1) = ACOS(TMP(1))/(2*!pi)
      ANGLE0(IE,2) = ACOS(TMP(2))/(2*!pi)

;
; cross product of edge-vector  (orientated anticlockwise)
;                                   
      AREA(IE) = (DXP3*DYP1 - DYP3*DXP1)*0.5
      
;     The *1 factor is for km -> km 
;     For lat-lon calculations, there should also be the 1/cos(lat) in
;     FACTX 
 
      FACTX = 1./(2*AREA(IE)*1)
      FACTY = 1./(2*AREA(IE)*1)

      CROSSDIFF(0,IE) = -1*DYP2 * FACTX
      CROSSDIFF(1,IE) = -1*DYP3 * FACTX
      CROSSDIFF(2,IE) = -1*DYP1 * FACTX
      CROSSDIFF(3,IE) = DXP2 * FACTY
      CROSSDIFF(4,IE) = DXP3 * FACTY
      CROSSDIFF(5,IE) = DXP1 * FACTY

      CCON(I1) = CCON(I1)+1
      SI(I1) = SI(I1) + AREA(IE)*ONETHIRD
      CCON(I2) = CCON(I2)+1
      SI(I2) = SI(I2) + AREA(IE)*ONETHIRD
      CCON(I3) = CCON(I3)+1
      SI(I3) = SI(I3) + AREA(IE)*ONETHIRD
      IF (AREA(IE) LT SIRATIO(I1)) THEN SIRATIO(I1) = AREA(IE)
      IF (AREA(IE) LT SIRATIO(I2)) THEN SIRATIO(I2) = AREA(IE)
      IF (AREA(IE) LT SIRATIO(I3)) THEN SIRATIO(I3) = AREA(IE)
    ENDFOR 
    SIGMA = 8.d0*ATAN(1.d0)/WAVEPERIOD

    DEPTHS= gridmat(7,*) + zoffset
 
    DISPERSION,SIGMA,DEPTHS,WCG

    maxmnecon = MAX(CCON)
    CELLVERTEX=LONARR(NNGP,maxmnecon,2)

    CHILF = LONARR(NNGP)-1
    FOR IE = 0, NTRI-1 DO BEGIN 
      FOR J=1,3 DO BEGIN 
        I = TRIGP(IE+1,J)-1
        CHILF(I) = CHILF(I)+1
        CELLVERTEX(I,CHILF(I),0) = IE
        CELLVERTEX(I,CHILF(I),1) = J-1
      ENDFOR
    ENDFOR

    J = 0L
    JCELLS=LONARR(NNGP)
    FOR IP = 0, NNGP-1 DO BEGIN 
      JCELLS(IP)=J
      FOR I = 0, CCON(IP)-1 DO BEGIN 
        J = J + 1
      ENDFOR
    ENDFOR

    COUNT_MAX = J ; Max. Number of entries in the pointers used in the calculations

    IE_CELL=LONARR(COUNT_MAX)
    POS_CELL=INTARR(COUNT_MAX)
;
    J = -1L
    FOR IP = 0, NNGP-1 DO BEGIN 
      FOR I = 0, CCON(IP)-1 DO BEGIN 
        J = J + 1
        IE_CELL(J)  = CELLVERTEX(IP,I,0)
        POS_CELL(J) = CELLVERTEX(IP,I,1)
      ENDFOR
    ENDFOR

    DTMAX_EXP = FLTARR(NNGP) + 1./THR

    NTH=24
    THETA=FINDGEN(NTH)*2*!pi/NTH
    COSTH=COS(THETA)
    SINTH=SIN(THETA)
    C = FLTARR(2,NNGP)
    K = FLTARR(3,NTRI)

    LATTOM = 1000.  ; !dtor * 6378137.0d0
    LAMBDA = FLTARR(2)
;
; Loops on directions
;
;    IP0=70
;    GET_LUN,unit
;    OPENW,unit,'fort.996'
    FOR ID = 0, NTH-1 DO BEGIN 

      FOR IP = 0, NNGP-1 DO BEGIN 
        COSFAC = 1. ;COS((FLOAT(gridmat(2,IP))+gridmat(3,IP)/60.)*!dtor)
        C(0,IP) = WCG(IP)*COSTH(ID)/(LATTOM*COSFAC)
        C(1,IP) = WCG(IP)*SINTH(ID)/LATTOM
; IF (IP EQ 70) PRINTF,unit,FORMAT='(2I8,2F13.4,2F11.6,F8.2,3F13.4)', ID+1,IP+1,WCG(IP)*COSTH(ID), WCG(IP)*SINTH(ID), C(0,IP),C(1,IP),WCG(IP), $
;                           gridmat(4,IP)+gridmat(5,IP)/60.,gridmat(2,IP)+gridmat(3,IP)/60.,DEPTHS(IP)
      ENDFOR
      FOR IE = 0, NTRI-1 DO BEGIN 
        I1 = TRIGP(IE+1,1)-1
        I2 = TRIGP(IE+1,2)-1
        I3 = TRIGP(IE+1,3)-1
        LAMBDA(0) = ONESIXTH * (C(0,I1)+C(0,I2)+C(0,I3))
        LAMBDA(1) = ONESIXTH * (C(1,I1)+C(1,I2)+C(1,I3))
        K(0,IE)  = LAMBDA(0) * IEN(0,IE) + LAMBDA(1) * IEN(1,IE)
        K(1,IE)  = LAMBDA(0) * IEN(2,IE) + LAMBDA(1) * IEN(3,IE)
        K(2,IE)  = LAMBDA(0) * IEN(4,IE) + LAMBDA(1) * IEN(5,IE)
;IF (I1 EQ IP0 OR I2 EQ IP0 OR I3 EQ IP0)  THEN PRINTF,unit,FORMAT='(5I8,6F9.5,7F16.8)', $
;            ID+1,I1+1,I2+1,I3+1,IE+1,IEN(0:5,IE),LAMBDA(*),K(*,IE),COSTH(ID),SINTH(ID)
;IF (I1 EQ IP0 OR I2 EQ IP0 OR I3 EQ IP0)  THEN PRINTF,unit,FORMAT='(8I6,8F16.8)', $
;            ID+1,I1+1,I2+1,I3+1,IE+1,I1,I2,I3,LAMBDA(*),K(*,IE),C(0,I1),C(0,I2),C(0,I3)
      ENDFOR
    
      J = -1L
      FOR IP = 0, NNGP-1 DO BEGIN 
        KSUM = 0.0
        FOR I = 0, CCON(IP)-1 DO BEGIN 
          J = J + 1
          IE    = IE_CELL(J)
          POS   = POS_CELL(J)
          KSUM  = KSUM + MAX([K(POS,IE),0.0])
          I1 = TRIGP(IE+1,1)
          I2 = TRIGP(IE+1,2)
          I3 = TRIGP(IE+1,3)
;         IF (IP EQ IP0) THEN printf,unit,FORMAT='(9I6,6F14.9)',ID+1,IP+1,I+1,J+1,IE+1,POS+1,I1,I2,I3,K(POS,IE),KSUM
        ENDFOR
        DTMAX_EXP_LOCAL = SI(IP)/MAX([DOUBLE(10.E-10),KSUM]) 
;       IF (IP.EQ.392) WRITE(997,'(2I8,5F16.9)') ID,IP,SI(IP),WCG(IP),MYXYZ(3,IP),KSUM,DTMAX_EXP(IP)
 
        IF (IOBP(IP) EQ 0) THEN DTMAX_EXP(IP) = MIN([DTMAX_EXP_LOCAL,DTMAX_EXP(IP)])
;       IF (IP EQ IP0) THEN printf,unit,FORMAT='(2I8,6F16.9)', ID+1,IP+1,SI(IP),WCG(IP),GRIDMAT(7,IP),KSUM,DTMAX_EXP_LOCAL,DTMAX_EXP(IP)
      ENDFOR
    ENDFOR
;    close,unit
;    free_lun,unit    
    ORDER=SORT(DTMAX_EXP)
    PRINT,'Nodes giving smallest time steps:',ORDER(0:50)+1
    PRINT,'Values of these time steps (s)  :',DTMAX_EXP(ORDER(0:50))
    TIMESTEPS=FLTARR(4,NTIMESTEPS)
    FOR I=0,NTIMESTEPS-1 DO BEGIN 
       IND1=ORDER(I)
       TIMESTEPS(0,I)=IND1
       TIMESTEPS(1,I)=DTMAX_EXP(IND1)
       TIMESTEPS(2,I)=gridmat(0,IND1)
       TIMESTEPS(3,I)=gridmat(1,IND1)
    ;   PRINT,IND1+1, DTMAX_EXP(IND1), gridmat(4,IND1)+gridmat(5,IND1)/60.,gridmat(2,IND1)+gridmat(3,IND1)/60.,DEPTHS(IND1)
    ENDFOR
    FLAGTSTEP=1
    TIMESTEPS_ORDER=ORDER
    TIMESTEPS_ALL=DTMAX_EXP
    RETURN
END


;----------------------------------------------------------
PRO UG_GRADIENTS,Z,TRIANGLES,kount,WHICHNODE,DIFFX,DIFFY  
;  Computes gradients at position XP,YP
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON WGene,Wgrideditor,WG,root
COMMON TIMESTEPSUG3, SIRATIO, JCELLS, LEN, ANGLE0, CROSSDIFF, TIMESTEPS_ALL
;*******END OF COMMON BLOCKS*******************************

; WARNING: CROSSDIFF WAS COMPUTED FROM LAT, LON, NOT FROM X, Y
             
    DIFFX = 0
    DIFFY = 0
    TMP1=FLTARR(3)
    TMP2=FLTARR(3)
    FOR IE2 = 0, kount-1 DO BEGIN 
      IE = TRIANGLES(IE2) 
      I1 = TRIGP(IE+1,1)-1
      I2 = TRIGP(IE+1,2)-1
      I3 = TRIGP(IE+1,3)-1

      TMP1(*)  = CROSSDIFF(0:2, IE)
      TMP2(*)  = CROSSDIFF(3:5, IE)

      DIFFXTMP = Z(I1)*TMP1(0)+Z(I2)*TMP1(1)+Z(I3)*TMP1(2) 
      DIFFYTMP = Z(I1)*TMP2(0)+Z(I2)*TMP2(1)+Z(I3)*TMP2(2) 
; calculate global gradients via all the connection contributions.
      DIFFX = DIFFX + DIFFXTMP * ANGLE0(IE,WHICHNODE(IE2))
      DIFFY = DIFFY + DIFFYTMP * ANGLE0(IE,WHICHNODE(IE2))
    ENDFOR

    RETURN 
    END 
;----------------------------------------------------------
PRO MESH_OPTIMIZE,IP
;  Moves nodes to optimize time steps 
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON WGene,Wgrideditor,WG,root
COMMON TIMESTEPSUG2, CCON, IEN, SI, AREA, WCG, CELLVERTEX, IE_CELL, POS_CELL, C, K
COMMON TIMESTEPSUG3, SIRATIO, JCELLS, LEN, ANGLE0, CROSSDIFF, TIMESTEPS_ALL
;*******END OF COMMON BLOCKS*******************************
;
      indtri=WHERE(TRIGP EQ IP,kountP)

      ALLNODESP=LONARR(kountP*3)
      TRIANGLESP=LONARR(kountP)
      FOR ITRI=0,kountP-1 DO BEGIN 
         ARRAY_IND2,TRIGP, indtri(ITRI),ind1
         ; PRINT,'OPT:',IP,' IN', ind1(0),ind1(1),TRANSPOSE(TRIGP(ind1(0),*))
         ALLNODESP(ITRI*3:ITRI*3+2) = TRANSPOSE(TRIGP(ind1(0),1:3))-1
         TRIANGLESP(ITRI)=ind1(0)-1
      ENDFOR
      NODESP =ALLNODESP(UNIQ(ALLNODESP, SORT(ALLNODESP)))
      NN=N_ELEMENTS(NODESP)
      IND=0
;
; Finds candidate node IPS for shifting around
;      
      AREAP=SIRATIO(NODESP)
      NOZ=WHERE(AREAP GT 0,kkk)
      IF (KKK GT 0) THEN BEGIN 
      RATIO=SI(NODESP(NOZ))/AREAP(NOZ)
      MAXR=MAX(RATIO(NOZ),IND)
      IPS=NODESP(NOZ(IND))+1
;
; Tries to shift IPS around
;      
      IF (MAXR GT 2) THEN BEGIN 
        indtri=WHERE(TRIGP EQ IPS,kount)
        ALLNODES=LONARR(kount*3)
        TRIANGLES=LONARR(kount)
        WHICHNODE=LONARR(kount)
        FOR ITRI=0,kount-1 DO BEGIN 
          ARRAY_IND2,TRIGP, indtri(ITRI),ind1
          ; PRINT,'IPS:',IPS,' IN', ind1(0),ind1(1),TRANSPOSE(TRIGP(ind1(0),*))
          ALLNODES(ITRI*3:ITRI*3+2) = TRANSPOSE(TRIGP(ind1(0),1:3))-1
          TRIANGLES(ITRI)=ind1(0)-1
          WHICHNODE(ITRI)=ind1(1)-1
        ENDFOR
        NODES1 =ALLNODES(UNIQ(ALLNODES, SORT(ALLNODES)))
        INDSORT=SORT(TIMESTEPS_ALL(NODES1))
        NODES=NODES1(INDSORT)
;
; Computes bottom slopes
; 
        UG_GRADIENTS,GRIDMAT(7,*),TRIANGLES,kount,WHICHNODE,DIFFX,DIFFY  
        ;PRINT,'GRADIENT:',DIFFX,DIFFY 
        DIST1=SQRT(SI(IPS-1))
        NG=0.05/SQRT(DIFFX^2+DIFFY^2)
        NSHIFT=21
        XSHIFT=(FINDGEN(NSHIFT)-NSHIFT/2)*DIST1*NG
        YSHIFT=GRIDMAT(1,IPS-1)+XSHIFT*DIFFX
        XSHIFT=GRIDMAT(0,IPS-1)-XSHIFT*DIFFY
        X=gridmat(0,NODES)
        Y=gridmat(1,NODES)
        Triangulate, X, Y, tris, hull
        GOODSHIFT=INTARR(NSHIFT)
        INPOLY=0
; checks if shifted position is in shrinked convex hull. 
        XH=(X(HULL)-GRIDMAT(0,IPS-1))*0.2+GRIDMAT(0,IPS-1)
        YH=(Y(HULL)-GRIDMAT(1,IPS-1))*0.2+GRIDMAT(1,IPS-1)
        OPLOT,[XH ,XH(0)],[YH, YH(0)],COLOR=0
        FOR IS=0,NSHIFT-1 DO BEGIN
          PNPOLY,XSHIFT(IS),YSHIFT(IS),XH,YH,N_ELEMENTS(HULL),TEST
          GOODSHIFT(IS)=TEST
        ENDFOR
        ; Next line is in response to bug ... ?
        ; IF (GOODSHIFT(NSHIFT/2) LT 1) THEN GOODSHIFT = -1*GOODSHIFT
        INDOK=WHERE(GOODSHIFT EQ 1,kk) 
        IF (KK GT 1) THEN BEGIN 
          XSHIFT=XSHIFT(INDOK)
          YSHIFT=YSHIFT(INDOK)
                OPLOT,XSHIFT,YSHIFT, $
                   psym=4,SYMSIZE=1.5,COLOR=0
          PRINT,'Trying to move node ',IPS,' to increase timestep at ',IP
          UPDATE_TIMESTEPSIP,IP-1,TRIANGLESP,kountp,IPS-1,TRIANGLES,kount, NODES, XSHIFT,YSHIFT,TSHIFT
        ENDIF
      ENDIF; end of test on MAXR GT 2 
      ENDIF
      RETURN
      END

;----------------------------------------------------------
PRO UPDATE_TIMESTEPSIP,IP0,TRIANGLESP,kountp,IPS,TRIANGLES,kount, NODES, XSHIFT,YSHIFT,TSHIFT
;  Recomputes timesteps for node IP at shifted positions
; 
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,TRIGP,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON TIMESTEPSUG,TIMESTEPS,NTIMESTEPS,FLAGTSTEP, TIMESTEPS_ORDER
COMMON TIMESTEPSUG2, CCON, IEN, SI, AREA, WCG, CELLVERTEX, IE_CELL, POS_CELL, C, K
COMMON TIMESTEPSUG3, SIRATIO, JCELLS, LEN, ANGLE0, CROSSDIFF, TIMESTEPS_ALL
COMMON GRIDTIMESTEPS, zoffset, updateonmove, waveperiod
;*******END OF COMMON BLOCKS*******************************
;
    ONETHIRD=1/3.
    ONESIXTH=1/6.
    THR=1.E-20
    SIGMA = 8.d0*ATAN(1.d0)/WAVEPERIOD
    NTH=24
    THETA=FINDGEN(NTH)*2*!pi/NTH
    COSTH=COS(THETA)
    SINTH=SIN(THETA)
    LATTOM = 1000. ; !dtor * 6378137.0d0
    NN = N_ELEMENTS(NODES)

    gridmemo=gridmat(*,IPS)
    NSHIFT=N_ELEMENTS(XSHIFT) 
    TMIN=TIMESTEPS(0)
    NN=N_ELEMENTS(NODES)

;
; Finds all relevant triangles for SI update
;
    ALLTRI=[0]
    FOR IND=0,N_ELEMENTS(NODES)-1 DO BEGIN 
      SI(NODES(IND))=0
      indtri=WHERE(TRIGP EQ NODES(IND)+1,countloc)
      FOR ITRI=0,countloc-1 DO BEGIN 
        ARRAY_IND2,TRIGP, indtri(ITRI),ind1
        ALLTRI=[ALLTRI, ind1(0)-1]
      ENDFOR
    ENDFOR
    UNITRI=ALLTRI(UNIQ(ALLTRI, SORT(ALLTRI)))

    TSTEPS=FLTARR(NN,NSHIFT+1) 
    FOR ISHIFT = 0, NSHIFT-1 DO BEGIN 
      gridmat(0,IPS)=XSHIFT(ISHIFT)
      gridmat(1,IPS)=YSHIFT(ISHIFT)
      
;
; Updates SI array 
;
      SI(NODES)=0
      FOR IND=1,N_ELEMENTS(UNITRI)-1 DO BEGIN
        IE = UNITRI(IND)
        I1 = TRIGP(IE+1,1)-1
        I2 = TRIGP(IE+1,2)-1
        I3 = TRIGP(IE+1,3)-1
        DXP1 = gridmat(0,I2) - gridmat(0,I1)
        DYP1 = gridmat(1,I2) - gridmat(1,I1)
        DXP2 = gridmat(0,I3) - gridmat(0,I2)
        DYP2 = gridmat(1,I3) - gridmat(1,I2)
        DXP3 = gridmat(0,I1) - gridmat(0,I3)
        DYP3 = gridmat(1,I1) - gridmat(1,I3)
        IEN(0,IE) = - DYP2
        IEN(1,IE) =   DXP2
        IEN(2,IE) = - DYP3
        IEN(3,IE) =   DXP3
        IEN(4,IE) = - DYP1
        IEN(5,IE) =   DXP1
        AREA(IE) = (DXP3*DYP1 - DYP3*DXP1)*0.5
        INDNODE=WHERE(NODES EQ I1,knode)
        IF (knode GT 0) THEN SI(NODES(INDNODE)) = SI(NODES(INDNODE)) + AREA(IE)*ONETHIRD
        INDNODE=WHERE(NODES EQ I2,knode)
        IF (knode GT 0) THEN SI(NODES(INDNODE)) = SI(NODES(INDNODE)) + AREA(IE)*ONETHIRD
        INDNODE=WHERE(NODES EQ I3,knode)
        IF (knode GT 0) THEN SI(NODES(INDNODE)) = SI(NODES(INDNODE)) + AREA(IE)*ONETHIRD
        IF (AREA(IE) LT SIRATIO(I1)) THEN SIRATIO(I1) = AREA(IE)
        IF (AREA(IE) LT SIRATIO(I2)) THEN SIRATIO(I2) = AREA(IE)
        IF (AREA(IE) LT SIRATIO(I3)) THEN SIRATIO(I3) = AREA(IE)
      ENDFOR

      DEPTHS2= gridmat(7,IP0) + ZOFFSET  ; Should probably update depths when moving nodes 
 
      DISPERSION,SIGMA,DEPTHS2,WCG2
      WCG(IP0)=WCG2

; No update of connectivity   

      LAMBDA = FLTARR(2)
;
; Loops on directions
;
      DTMAX_EXP = FLTARR(NN)+1./THR
      FOR ID = 0, NTH-1 DO BEGIN 

;
; Updates speed at node IP0 
;
     FOR IP = 0, NNGP-1 DO BEGIN 
       COSFAC = 1. ;COS((FLOAT(gridmat(2,IP))+gridmat(3,IP)/60.)*!dtor)
       C(0,IP) = WCG(IP)*COSTH(ID)/(LATTOM*COSFAC)
       C(1,IP) = WCG(IP)*SINTH(ID)/LATTOM
     ENDFOR


        FOR IND=1,N_ELEMENTS(UNITRI)-1  DO BEGIN 
          IE = UNITRI(IND)
          I1 = TRIGP(IE+1,1)-1
          I2 = TRIGP(IE+1,2)-1
          I3 = TRIGP(IE+1,3)-1
          LAMBDA(0) = ONESIXTH * (C(0,I1)+C(0,I2)+C(0,I3))
          LAMBDA(1) = ONESIXTH * (C(1,I1)+C(1,I2)+C(1,I3))
          K(0,IE)  = LAMBDA(0) * IEN(0,IE) + LAMBDA(1) * IEN(1,IE)
          K(1,IE)  = LAMBDA(0) * IEN(2,IE) + LAMBDA(1) * IEN(3,IE)
          K(2,IE)  = LAMBDA(0) * IEN(4,IE) + LAMBDA(1) * IEN(5,IE)
;IF (ID EQ 0) THEN PRINT,'K CHANGE:',ID,ISHIFT,IE2,IE,K(*,IE)
        ENDFOR
    
        FOR II=0,NN-1 DO BEGIN
          IP = NODES(II) ; computes time step for fixed node ... 
          J=JCELLS(IP)-1
          KSUM = 0.0
          FOR I = 0, CCON(IP)-1 DO BEGIN 
            J = J + 1
            IE    = IE_CELL(J)
            POS   = POS_CELL(J)
            KSUM  = KSUM + MAX([K(POS,IE),0.0])
;IF (ID EQ 0) THEN PRINT,'K EFF CHANGE:',ID,ISHIFT,I,IE,K(POS,IE),KSUM 
          ENDFOR
          DTMAX_EXP_LOCAL = SI(IP)/MAX([DOUBLE(10.E-10),KSUM]) 
          DTMAX_EXP(II) = MIN([DTMAX_EXP_LOCAL,DTMAX_EXP(II)])
        ENDFOR
      ENDFOR
      TSTEPS(*,ISHIFT)=DTMAX_EXP(*)
    ENDFOR
    ISHIFT = NSHIFT/2
    TMAX=TIMESTEPS_ALL(IP0) 
    FOR I=0,NSHIFT-1 DO BEGIN 
       IF TSTEPS(0,I) GT TMAX AND MIN(TSTEPS(1:NN-1,I)) > TSTEPS(0,I) THEN BEGIN 
         ISHIFT = I 
         TMAX = TSTEPS(0,I)
       ENDIF
    ENDFOR
      ; PRINT,'OPTI DT:',IPS+1,IP0+1,NODES(0)+1,TIMESTEPS_ALL(IP0),TSTEPS(0,ISHIFT)
    TIMESTEPS_ALL(NODES)=TSTEPS(*,ISHIFT)
    gridmat(0,IPS)= XSHIFT(ISHIFT)
    gridmat(1,IPS)= YSHIFT(ISHIFT)
    XYtoLATLON,gridmat(0,IPS),gridmat(1,IPS),latdeg,latmin,londeg,lonmin
    gridmat(2,IPS)=latdeg
    gridmat(3,IPS)=latmin
    gridmat(4,IPS)=londeg
    gridmat(5,IPS)=lonmin

    OPLOT,[gridmat(0,IPS)],[gridmat(1,IPS)], $
                   psym=5,SYMSIZE=3,COLOR=255

    ORDER=SORT(TIMESTEPS_ALL )
;    PRINT,'Nodes giving smallest time steps:',ORDER(0:50)+1
;    PRINT,'Values of these time steps (s)  :',TIMESTEPS_ALL(ORDER(0:50))
    TIMESTEPS=FLTARR(4,NTIMESTEPS)
    FOR I=0,NTIMESTEPS-1 DO BEGIN 
       IND1=ORDER(I)
       TIMESTEPS(0,I)=IND1
       TIMESTEPS(1,I)=TIMESTEPS_ALL(IND1)
       TIMESTEPS(2,I)=gridmat(0,IND1)
       TIMESTEPS(3,I)=gridmat(1,IND1)
    ENDFOR
    FLAGTSTEP=1
    TIMESTEPS_ORDER=ORDER

    RETURN
END

;----------------------------------------------------------
PRO UPDATE_TIMESTEPS2,IP0
;  Recomputes timesteps: uses x-y coordinates
; 
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,TRIGP,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON TIMESTEPSUG,TIMESTEPS,NTIMESTEPS,FLAGTSTEP, TIMESTEPS_ORDER
COMMON TIMESTEPSUG2, CCON, IEN, SI, AREA, WCG, CELLVERTEX, IE_CELL, POS_CELL, C, K
COMMON TIMESTEPSUG3, SIRATIO, JCELLS, LEN, ANGLE0, CROSSDIFF, TIMESTEPS_ALL
COMMON GRIDTIMESTEPS, zoffset, updateonmove, waveperiod
;*******END OF COMMON BLOCKS*******************************
;
      indtri=WHERE(TRIGP EQ IP0+1,kount)
      ALLNODES=LONARR(kount*3)
      TRIANGLES=LONARR(kount)
      FOR ITRI=0,kount-1 DO BEGIN 
         ARRAY_IND2,TRIGP, indtri(ITRI),ind1
         ; PRINT,'C_GP:',IP0+1,' IN', ind1(0),ind1(1),TRANSPOSE(TRIGP(ind1(0),*))
         ALLNODES(ITRI*3:ITRI*3+2) = TRANSPOSE(TRIGP(ind1(0),1:3))-1
         TRIANGLES(ITRI)=ind1(0)-1
      ENDFOR
      NODES =ALLNODES(UNIQ(ALLNODES, SORT(ALLNODES)))

    ONETHIRD=1/3.
    ONESIXTH=1/6.
    THR=1.E-20
    NTRI2=kount
    NNGP2=N_ELEMENTS(NODES)
    SIGMA = 8.d0*ATAN(1.d0)/WAVEPERIOD

;
; Finds all relevant triangles 
;
    ALLTRI=[0]
    FOR IND=0,NNGP2-1 DO BEGIN 
      SI(NODES(IND))=0
      indtri=WHERE(TRIGP EQ NODES(IND)+1,kount)
      FOR ITRI=0,kount-1 DO BEGIN 
        ARRAY_IND2,TRIGP, indtri(ITRI),ind1
        ALLTRI=[ALLTRI, ind1(0)-1]
      ENDFOR
    ENDFOR
    UNITRI=ALLTRI(UNIQ(ALLTRI, SORT(ALLTRI)))
;
; Updates SI array 
;
    FOR IND=1,N_ELEMENTS(UNITRI)-1 DO BEGIN
        IE = UNITRI(IND)
        I1 = TRIGP(IE+1,1)-1
        I2 = TRIGP(IE+1,2)-1
        I3 = TRIGP(IE+1,3)-1
        DXP1 = gridmat(0,I2) - gridmat(0,I1)
        DYP1 = gridmat(1,I2) - gridmat(1,I1)
        DXP2 = gridmat(0,I3) - gridmat(0,I2)
        DYP2 = gridmat(1,I3) - gridmat(1,I2)
        DXP3 = gridmat(0,I1) - gridmat(0,I3)
        DYP3 = gridmat(1,I1) - gridmat(1,I3)
        IEN(0,IE) = - DYP2
        IEN(1,IE) =   DXP2
        IEN(2,IE) = - DYP3
        IEN(3,IE) =   DXP3
        IEN(4,IE) = - DYP1
        IEN(5,IE) =   DXP1
        AREA(IE) = (DXP3*DYP1 - DYP3*DXP1)*0.5
        INDNODE=WHERE(NODES EQ I1,knode)
        IF (knode GT 0) THEN SI(NODES(INDNODE)) = SI(NODES(INDNODE)) + AREA(IE)*ONETHIRD
        INDNODE=WHERE(NODES EQ I2,knode)
        IF (knode GT 0) THEN SI(NODES(INDNODE)) = SI(NODES(INDNODE)) + AREA(IE)*ONETHIRD
        INDNODE=WHERE(NODES EQ I3,knode)
        IF (knode GT 0) THEN SI(NODES(INDNODE)) = SI(NODES(INDNODE)) + AREA(IE)*ONETHIRD
        IF (AREA(IE) LT SIRATIO(I1)) THEN SIRATIO(I1) = AREA(IE)
        IF (AREA(IE) LT SIRATIO(I2)) THEN SIRATIO(I2) = AREA(IE)
        IF (AREA(IE) LT SIRATIO(I3)) THEN SIRATIO(I3) = AREA(IE)
    ENDFOR


    DEPTHS2= gridmat(7,NODES) + ZOFFSET  ; Should probably update depths when moving nodes 
 
    DISPERSION,SIGMA,DEPTHS2,WCG2
    WCG(NODES)=WCG2

; No update of connectivity   

    DTMAX_EXP =  TIMESTEPS_ALL
    DTMAX_EXP(NODES) =  1./THR

    NTH=24
    THETA=FINDGEN(NTH)*2*!pi/NTH
    COSTH=COS(THETA)
    SINTH=SIN(THETA)

    LATTOM = 1000. ; !dtor * 6378137.0d0
    LAMBDA = FLTARR(2)
;
; Loops on directions
;
;    GET_LUN,unit
;    OPENW,unit,'fort_upd.996'
    FOR ID = 0, NTH-1 DO BEGIN 

;
; Updates speed at nodes IP0 : maybe other change later when depth is updated ... 
;    FOR NOW WCG is not changed
;
     COSFAC = 1. ; COS((FLOAT(gridmat(2,IP0))+gridmat(3,IP0)/60.)*!dtor )
 
     FOR IP = 0, NNGP-1 DO BEGIN 
       COSFAC = 1. ;COS((FLOAT(gridmat(2,IP))+gridmat(3,IP)/60.)*!dtor)
       C(0,IP) = WCG(IP)*COSTH(ID)/(LATTOM*COSFAC)
       C(1,IP) = WCG(IP)*SINTH(ID)/LATTOM
     ENDFOR

     FOR IND = 1,N_ELEMENTS(UNITRI)-1 DO BEGIN 
        IE = UNITRI(IND)
        I1 = TRIGP(IE+1,1)-1
        I2 = TRIGP(IE+1,2)-1
        I3 = TRIGP(IE+1,3)-1
        LAMBDA(0) = ONESIXTH * (C(0,I1)+C(0,I2)+C(0,I3))
        LAMBDA(1) = ONESIXTH * (C(1,I1)+C(1,I2)+C(1,I3))
        K(0,IE)  = LAMBDA(0) * IEN(0,IE) + LAMBDA(1) * IEN(1,IE)
        K(1,IE)  = LAMBDA(0) * IEN(2,IE) + LAMBDA(1) * IEN(3,IE)
        K(2,IE)  = LAMBDA(0) * IEN(4,IE) + LAMBDA(1) * IEN(5,IE)
;IF (I1 EQ 70 OR I2 EQ 70 OR I3 EQ 70)  THEN PRINTF,unit,FORMAT='(5I8,6F9.5,7F16.8)', $
;            ID+1,I1+1,I2+1,I3+1,IE+1,IEN(0:5,IE),LAMBDA(*),K(*,IE),COSTH(ID),SINTH(ID)
;IF (I1 EQ 70 OR I2 EQ 70 OR I3 EQ 70)  THEN PRINTF,unit,FORMAT='(8I6,8F16.8)', $
;            ID+1,I1+1,I2+1,I3+1,IE+1,I1,I2,I3,LAMBDA(*),K(*,IE),C(0,I1),C(0,I2),C(0,I3)
     ENDFOR
    
      FOR IP2 = 0, NNGP2-1 DO BEGIN 
        IP = NODES(IP2)
        J=JCELLS(IP)-1
        KSUM = 0.0
        FOR I = 0, CCON(IP)-1 DO BEGIN 
          J = J + 1
          IE    = IE_CELL(J)
          POS   = POS_CELL(J)
          KSUM  = KSUM + MAX([K(POS,IE),0.0])
        I1 = TRIGP(IE+1,1)
        I2 = TRIGP(IE+1,2)
        I3 = TRIGP(IE+1,3)
; IF (IP EQ 70) THEN printf,unit,FORMAT='(9I6,6F14.9)',ID+1,IP+1,I+1,J+1,IE+1,POS+1,I1,I2,I3,K(POS,IE),KSUM
          ;IF ( ABS(K(POS,IE)) > KMAX ) KMAX = ABS(K(POS,IE))
        ENDFOR
        DTMAX_EXP_LOCAL = SI(IP)/MAX([DOUBLE(10.E-10),KSUM]) 
 ; IF (IP.EQ.392) WRITE(997,'(2I8,5F16.9)') ID,IP,SI(IP),WCG(IP),MYXYZ(3,IP),KSUM,DTMAX_EXP(IP)
 
        DTMAX_EXP(IP) = MIN([DTMAX_EXP_LOCAL,DTMAX_EXP(IP)])
; IF (IP EQ 70) THEN printf,unit,FORMAT='(2I8,6F16.9)', ID+1,IP+1,SI(IP),WCG(IP),GRIDMAT(7,IP),KSUM,DTMAX_EXP_LOCAL,DTMAX_EXP(IP)
      ENDFOR
    ENDFOR
;    close,unit
;    free_lun,unit    
    PRINT,'Updated time steps for nodes    :',NODES+1
    PRINT,'Old time steps (s)              :',TIMESTEPS_ALL(NODES)
    TIMESTEPS_ALL(NODES)=DTMAX_EXP(NODES)
    ORDER=SORT(DTMAX_EXP)
    PRINT,'New time steps (s)              :',DTMAX_EXP(NODES)
    TIMESTEPS=FLTARR(4,NTIMESTEPS)
    FOR I=0,NTIMESTEPS-1 DO BEGIN 
       IND1=ORDER(I)
       TIMESTEPS(0,I)=IND1
       TIMESTEPS(1,I)=DTMAX_EXP(IND1)
       TIMESTEPS(2,I)=gridmat(0,IND1)
       TIMESTEPS(3,I)=gridmat(1,IND1)
    ;;   PRINT,IND1+1, DTMAX_EXP(IND1), gridmat(4,IND1)+gridmat(5,IND1)/60.,gridmat(2,IND1)+gridmat(3,IND1)/60.,DEPTHS(IND1)
    ENDFOR
    FLAGTSTEP=1
    TIMESTEPS_ORDER=ORDER
    RETURN
END

;----------------------------------------------------------------------------
PRO ARRAY_IND2, ARRAY, indices, IND2
    ni = N_ELEMENTS(indices)

    IND2 = LONARR(2, ni, /NOZERO)
    TAILLE = SIZE(ARRAY, /DIMENSIONS)
    IND2(0, *) = indices(*) mod TAILLE[0]  
    IND2(1, *) = indices(*)/TAILLE[0]    

END 

;----------------------------------------------------------------------------
PRO MAKE_CONTOUR
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
   conti=FLTARR(nngp)
   conti(*)=gridmat(6,*)
   IND=WHERE(conti NE 0,kount)
   IF (kount EQ 0) THEN BEGIN
      contourline=LONARR(2,2)
   ENDIF ELSE BEGIN
      contourline=LONARR(kount+2,2)-1
      contourline(0,0)=IND(0)
      contourline(1,0)=IND(kount-1)+2    
      contourline(2:2+kount-1,0)=IND
      contourline(2:2+kount-1,1)=conti(IND)
   ENDELSE
   RETURN
END
