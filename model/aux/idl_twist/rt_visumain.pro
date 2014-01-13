;/
;/                  +-----------------------------------+
;/                  | TWIST                Ifremer-SHOM |
;/                  |           F. Ardhuin              |
;/                  |              IDL command language |
;/                  | Last update :         30-Nov-2013 |
;/                  +-----------------------------------+
;/
;/ Licence information: This code is distributed under the CeCILL license
;/                      generally compatible with the Gnu Public Licence (GPL) 
;/                      http://www.cecill.info/index.en.html
;/
;/    17-Oct-2013 : Clean up                            ( version 2.00 )
;/    17-Oct-2013 : Reading of GLOBWAVE NetCDF buoy data( version 2.00 )
;/    26-Oct-2013 : Manages different times for add. mod( version 2.00 )
;/    27-Oct-2013 : ASCII export from NetCDF files      ( version 2.00 )
;/    10-Nov-2013 : Debugged the update_coordinates     ( version 2.00 )
;/    30-Nov-2013 : Adding support for polar grids      ( version 2.01 )
;  1. Purpose :
;
;     Main GUI for wave analysis. Calls sub-windows.
;
;  2. Method :
;
;     - Reads some files
;     - Does some processing
;     - performs a plot
;
;  This files contains the main procedures
;  it also calls parametros.pro, raytracer.pro, gridbuild.pro
;
;  All the widgets of the main window are defined in 'analyzer'
;----------------------------------------------------------------------------
Pro rt_visumain
;+
; NAME:
;   ANALYZER
; PURPOSE:
;   Displays the widgets of the main window
; CALLING SEQUENCE:
;   rt_visumain
; INPUTS:none
; COMMON BLOCKS: See below
; MODIFICATION HISTORY: under construction (1/1/1999 - 17-Oct-2013)
; MODIFICATION HISTORY: increased slider width        (17-Oct-2013)
;-
;** 1 ** Display parameters
COMMON AXISFRAME,axis_orient,frametype,outx1lab,outx2lab,outy1lab,outy2lab
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON COLORBARPAR,cbnticks,cbexrange,cbtrx,cbtry,cbblx,cbbly, $
   filloutofrange,addmini,addmaxi
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON TITLES,  font,outtit,outxtit,outytit,outcbtit,textx,texty,textdx,textdy
COMMON POSTSCRIPT, filep,pspath,prcoul,psor,pstype, $
                pwinx,pwiny,papierx,papiery,xoffset,yoffset, $
                facpolice,fontrescale,basefontsize,pssizex,pssizey,psfont
COMMON THREED,  Ax3D,Az3D,smoothing
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
COMMON INTERFACE, TWISTVA_VERSION

;** 2 ** Display and data variables/parameters
COMMON FREQ,    freq,nfband,findex,findex2
COMMON DIR,     nabin,nabint,aindex,THETA
COMMON TIME,    timestep,tindex,tindex2,ntime,dtime,time0,day0,dtindex,nstep, $
                timezone_plot,timezone_string,months,time
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2

COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath

COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON GRIDPar, th1,th2,gfact,gsize1,gsize6
COMMON RAYS,    raysOK,raystype,raynsteps,rayx,rayy,raya,rayamin,rayamax, $
                rayres,rayfreq,rayGP,raytimestep,rayoffdep,rayflag,raydz,raymindepth
COMMON TIMESERIES, modspec,obs,obs2,om,ts_filetype, nbins, bulktype,scatvar
;*******END OF COMMON BLOCKS*******************************
   bulktype=0
   scatvar=0
   Initmain                     ;calls main initialization procedure
   YBUTSIZE = 30
   YSLISIZE = 50

;controls the device behaviour
   device, retain=2, decomposed=0             

   base = long(0)             ;main base widget
   base = WIDGET_BASE(TITLE='the Wave Ifremer-SHOM tool - v2.0 - rev301', /ROW)

   left = WIDGET_BASE(base,/COLUMN)    ; left widget

; Widget bar on top of drawing window:
   topleft = WIDGET_BASE(left, /FRAME, /ROW)
      Wtoprow=LONARR(10)
      Wtoprow(0)=WIDGET_LABEL(topleft,VALUE='Read file:')
      Wtoprow(1)=WIDGET_DROPLIST(topleft, VALUE=[ $
         'Click here to select file type', $
         'xyz.dat file', $
         'Bathy log file', $
         'Binary bathy', $
         'GSHHS coastline', $
         '*.line coastline', $
         'GMESH triangle-based grid', $
         'Special points (overlays)', $
         'Transect', $
         '   ', $
         'Observed time series', $
         'Modelled time series', $
         'Additional modelled TS', $
         '   ', $
         'All modelled TS for new point', $
         '   ', $
         'Additional bulk time series', $
         'WW3 NetCDF map', $
         'GSHHS coast', $
         'Data flags', $
         'WW3 map', $
         'WW3 map diff', $
         'Color palette'])
      IF (TWISTVA_VERSION EQ 'LICENCE') THEN $
         Wtoprow(2)=WIDGET_BUTTON(topleft, VALUE='Palette')  $
      ELSE Wtoprow(2)=WIDGET_LABEL(topleft,VALUE='   ')  
      Wtoprow(3)=WIDGET_BUTTON(topleft, VALUE='Refresh')
      Wtoprow(5)=WIDGET_LABEL(topleft,VALUE='     ')
      Wtoprow(4)=WIDGET_BUTTON(topleft, VALUE='Quit')


; drawing window:
   Wdraw = WIDGET_DRAW(left, XSIZE=winx,YSIZE=winy, $
      /BUTTON_EVENTS,/MOTION_EVENTS,SENSITIVE=0,    $
      /WHEEL_EVENTS,/COLOR_MODEL,COLORS=256)


; Widgets on the right:

   right = WIDGET_BASE(base, /COLUMN)    ; right widget

   Wrlines=LONARR(20)
   Wright=LONARR(20,15)
   Wrblock=LONARR(6)
   FOR I=0,5 DO BEGIN
      Wrblock(I)=WIDGET_BASE(right, /FRAME, /COLUMN)
   ENDFOR

;first line (part of first block)
   I=0
   Wrlines(I)=WIDGET_BASE(Wrblock(0), /ROW)
   Wright(I,3)=WIDGET_LABEL(Wrlines(I), VALUE='Time series:', YSIZE=YBUTSIZE)
   Wright(I,0)=WIDGET_DROPLIST(Wrlines(I),  $
         VALUE=['Modelled','Observed','Comparison'],     YSIZE=YBUTSIZE)
   Wright(I,1)=WIDGET_LABEL(Wrlines(I), VALUE='Flags:', YSIZE=YBUTSIZE)
   Wright(I,2)=WIDGET_DROPLIST(Wrlines(I), VALUE=['NO CHOICE'], YSIZE=YBUTSIZE)
;
;second line (part of first block)
;
   I=2
   Wrlines(I)=WIDGET_BASE(Wrblock(0), /ROW)
   ptype = ['Grid+Topo','Bathy from gmsh', 'WW3 map','WW3 map diff']
   Wright(I,0)=WIDGET_LABEL(Wrlines(I),VALUE='Other data:')
   Wright(I,1)=WIDGET_DROPLIST(Wrlines(I), VALUE=ptype)
   Wright(I,2)=WIDGET_BASE(Wrlines(I), /ROW,/NonExclusive)
   Wright(I,3)=Widget_Button(Wright(I,2), Value='Norm')
   Widget_Control, Wright(I,3), Set_Button=1
;3rd line (second block)
   I=1
   Wrlines(I)=WIDGET_BASE(Wrblock(0), /ROW)
   Wright(I,4)=WIDGET_LABEL(Wrlines(I), VALUE='Var.:', YSIZE=YBUTSIZE)
   Wright(I,1)=WIDGET_DROPLIST(Wrlines(I), VALUE=['NO CHOICE'], YSIZE=YBUTSIZE)
   Wright(I,2)=WIDGET_DROPLIST(Wrlines(I),VALUE=['NO CHOICE'], YSIZE=YBUTSIZE)
   Wright(I,3)=WIDGET_DROPLIST(Wrlines(I), VALUE=['NO CHOICE'], YSIZE=YBUTSIZE)

;4th line (second block)
   I=3
   Wrlines(I)=WIDGET_BASE(Wrblock(1), /ROW)
   Wright(I,4)=WIDGET_LABEL(Wrlines(I),VALUE='Overlays')
   Wright(I,0)=WIDGET_DROPLIST(Wrlines(I), $
      VALUE=['DIR','NO DIR'],  YSIZE=YBUTSIZE)
   Wright(I,1)=WIDGET_DROPLIST(Wrlines(I), $
      VALUE=['NO SYM','SPEC','GP','ALL','GP #'],  YSIZE=YBUTSIZE)
   Wright(I,2)=WIDGET_DROPLIST(Wrlines(I), $
      VALUE=['NO BATHY','DOT30-50','SOL300','DASH100','SOL20-40'], YSIZE=25, FRAME = 0)
;5th line (second block)
   I=4
   Wrlines(I)=WIDGET_BASE(Wrblock(1), /ROW)
   Wright(I,0)=WIDGET_DROPLIST(Wrlines(I), $
      VALUE=['Coast','No coast','outline','outline, no lakes'], YSIZE=YBUTSIZE)
   Wright(I,2)=WIDGET_DROPLIST(Wrlines(I), $
      VALUE=['NO TRI','TRI','CELLS','NODES'])
   Wright(I,3)=WIDGET_DROPLIST(Wrlines(I), $
      VALUE=['NO','Cont.'], YSIZE=25, FRAME = 0)

;6th line (3rd block)
   I=5
   Wrlines(I)=WIDGET_BASE(Wrblock(2), /ROW)
   Wright(I,0)=WIDGET_LABEL(Wrlines(I),VALUE='Plot type:')
   Wright(I,1)=WIDGET_DROPLIST(Wrlines(I), $
   VALUE=['CONTOURFILL','TV','POLYFILL','CONTOUR', $
      'NOTHING','POLAR FILL','POLAR_CONTOUR','POLAR_POLY','SURFACE','SHADE_SURF', $
      'SHADE_SURF2','LEGO','ONLY CBAR','2D-f spectrum'], YSIZE=YBUTSIZE)

;7th line (3rd block)
   I=6
   Wrlines(I)=WIDGET_BASE(Wrblock(2), /ROW)
   Wright(I,0)=WIDGET_LABEL(Wrlines(I),VALUE='Output:')
   Wright(I,1)=WIDGET_DROPLIST(Wrlines(I),VALUE=['Annotate','Save as PS', $
      'Send to las1r2','Send to col1r1','Save as EPS','Save as PNG', $
      'Save as JPEG','Save as TIFF','Save as GIF-M','time-GIF'], YSIZE=YBUTSIZE)

   Wright(I,2)=WIDGET_DROPLIST(Wrlines(I), $
       VALUE=['get time series','Save time series','from all files', $
              'save from all','Save snapshot'], YSIZE=YBUTSIZE)

;8th line (4th block)
   I=7
   Wrlines(I)=WIDGET_BASE(Wrblock(3), /ROW)
   Wright(I,4)=WIDGET_LABEL(Wrlines(I),VALUE='Toolboxes:')
   Wright(I,0)=WIDGET_BUTTON(Wrlines(I), VALUE='Plot parameters')
   ;Wright(I,1)=WIDGET_BUTTON(Wrlines(I), VALUE='Transect tool')
   Wright(I,2)=WIDGET_BUTTON(Wrlines(I), VALUE='Rays')
   Wright(I,3)=WIDGET_BUTTON(Wrlines(I), VALUE='Maps')
   I=8
   Wright(I,1)=WIDGET_BUTTON(Wrlines(I-1), VALUE='Mesh')
   Wright(I,2)=WIDGET_BUTTON(Wrlines(I-1), VALUE='Bathy')

;9th line (5th block)
   I=9
   Wrlines(I)=WIDGET_BASE(Wrblock(4), /ROW)
   Wright(I,0)=WIDGET_LABEL(Wrlines(I),VALUE='Min/max:')
   Wright(I,1)=WIDGET_TEXT(Wrlines(I),/EDITABLE, $
      XSIZE=8,YSIZE=1,VALUE=strcompress(STRING(mindepth)))
   ;Wright(I,2)= WIDGET_LABEL(Wrlines(I),VALUE='Max:')
   Wright(I,3)= WIDGET_TEXT(Wrlines(I),/EDITABLE, $
      XSIZE=8,YSIZE=1,VALUE=strcompress(STRING(maxdepth)))

;10th line (5th block)
   I=10
   ;Wrlines(I)=WIDGET_BASE(Wrblock(4), /ROW)
   Wright(I,0)=WIDGET_BUTTON(Wrlines(I-1), VALUE='Zoom')
   Wright(I,1)=WIDGET_BUTTON(Wrlines(I-1), VALUE='Zoom out')
   Wright(I,2)=WIDGET_BUTTON(Wrlines(I-1), VALUE='Move')

;11th line (5th block)
   I=11
   Wrlines(I)=WIDGET_BASE(Wrblock(4), /ROW)
   Wright(I,0)=WIDGET_LABEL(Wrlines(I),VALUE='Point:')
   Wright(I,1)=WIDGET_TEXT(Wrlines(I),XSIZE=6,YSIZE=1, $
      VALUE=strcompress('   1'),/EDITABLE)
   Wright(I,3)=WIDGET_DROPLIST(Wrlines(I),VALUE=['        '])
   Wright(I,2)= WIDGET_BUTTON(Wrlines(I),VALUE='PickNew')
   Wright(I,4)=WIDGET_BUTTON(Wrlines(I), VALUE='EndMove')

;12th line (5th block)
   I=12
   Wrlines(I)=WIDGET_BASE(Wrblock(4), /ROW)
   Wright(I,0) = WIDGET_SLIDER(Wrlines(I), MAXIMUM=nfband, MINIMUM=1, $
   value=findex,TITLE='Min freq index', YSIZE=YSLISIZE)
   Wright(I,5) = WIDGET_SLIDER(Wrlines(I), MAXIMUM=nfband, MINIMUM=1, $
   value=nfband,TITLE='Max freq index', YSIZE=YSLISIZE)
   Wright(I,1) = WIDGET_SLIDER(Wrlines(I), MAXIMUM=nabin-1, MINIMUM=0, $
   value=aindex,TITLE='angle', YSIZE=YSLISIZE)
   Wrlines(I)=WIDGET_BASE(Wrblock(4), /ROW)
   Wright(I,2) = WIDGET_SLIDER(Wrlines(I), MAXIMUM=ntime, MINIMUM=1, $
   value=tindex,TITLE='time index start',XSIZE='150', YSIZE=YSLISIZE)
   Wright(I,4) = WIDGET_SLIDER(Wrlines(I), MAXIMUM=ntime, MINIMUM=1, $
   value=dtindex,TITLE='time index end',XSIZE='150', YSIZE=YSLISIZE)

;15th line (6th block)
   I=15
   Wrlines(I)=WIDGET_BASE(Wrblock(5), /ROW)
   Wright(I,0)=WIDGET_LABEL(Wrlines(I),VALUE='Lat,lon:')
   Wright(I,1)=WIDGET_TEXT(Wrlines(I),XSIZE=44,YSIZE=1, $
      VALUE=strcompress('???????????? / ????????????'))

;14th line (6th block)
   I=14
   Wrlines(I)=WIDGET_BASE(Wrblock(5), /ROW)
   Wright(I,0)=WIDGET_LABEL(Wrlines(I),VALUE='X,Y:')
   Wright(I,1)=WIDGET_TEXT(Wrlines(I),XSIZE=17,YSIZE=1, $
      VALUE=strcompress('????????,????????'))
   Wright(I,2)= WIDGET_LABEL(Wrlines(I),VALUE='Z:')
   Wright(I,3)= WIDGET_TEXT(Wrlines(I),XSIZE=8,YSIZE=1, $
      VALUE=strcompress('???????'))
   Wright(I,4)= WIDGET_LABEL(Wrlines(I),VALUE='Value:')
   Wright(I,5)= WIDGET_TEXT(Wrlines(I),XSIZE=7,YSIZE=1, $
      VALUE=strcompress('???????'))


   WIDGET_CONTROL, /REALIZE, base                   ;make widgets visible
   WIDGET_CONTROL, GET_VALUE=drawval, Wdraw
   Wdraw_value_update=1
   WSET, drawval

   WIDGET_CONTROL, Wdraw, SENSITIVE=1
   XMANAGER, 'analyzer', /NO_BLOCK,base             ;specify event handler
END


;-----------------------------------------------------------------------------
PRO Addinpoly,xc,yc,marginx,marginy,x1,y1,x2,y2,xp,yp,add
   add=0
   IF (X1 GT xc) THEN BEGIN
      IF (X2 GT xc) THEN BEGIN
         IF (Y1 GT yc) THEN BEGIN
            IF (Y2 LE yc) THEN BEGIN
               add=2
               xp=[xc-marginx,xc-marginx]
               yp=[yc+marginy,yc-marginy]
            ENDIF
         ENDIF ELSE BEGIN
            IF (Y2 GT yc) THEN BEGIN
               add=2
               xp=[xc+marginx,xc+marginx]
               yp=[yc-marginy,yc+marginy]
            ENDIF
         ENDELSE
      ENDIF ELSE BEGIN
         IF (Y1 GT yc) THEN BEGIN
            IF (Y2 GT yc) THEN BEGIN
               add=2
               xp=[xc+marginx,xc-marginx]
               yp=[yc+marginy,yc+marginy]
            ENDIF ELSE BEGIN
               add=3
               xp=[xc+marginx,xc-marginx,xc-marginx]
               yp=[yc+marginy,yc+marginy,yc-marginy]
            ENDELSE
         ENDIF ELSE BEGIN
            IF (Y2 GT yc) THEN BEGIN
               add=3
               xp=[xc+marginx,xc+marginx,xc-marginx]
               yp=[yc-marginy,yc+marginy,yc+marginy]
            ENDIF ELSE BEGIN
               add=2
               xp=[xc+marginx,xc-marginx]
               yp=[yc-marginy,yc-marginy]
            ENDELSE
         ENDELSE
      ENDELSE
   ENDIF ELSE BEGIN
      IF (X2 GT xc) THEN BEGIN
         IF (Y1 GT yc) THEN BEGIN
            IF (Y2 LE yc) THEN BEGIN
               add=2
               xp=[xc-marginx,xc-marginx]
               yp=[yc+marginy,yc-marginy]
            ENDIF
         ENDIF ELSE BEGIN
            IF (Y2 GT yc) THEN BEGIN
               add=2
               xp=[xc+marginx,xc+marginx]
               yp=[yc-marginy,yc+marginy]
            ENDIF
         ENDELSE
      ENDIF ELSE BEGIN
         IF (Y1 GT yc) THEN BEGIN
            IF (Y2 GT yc) THEN BEGIN
               add=2
               xp=[xc+marginx,xc-marginx]
               yp=[yc+marginy,yc+marginy]
            ENDIF ELSE BEGIN
               add=3
               xp=[xc+marginx,xc-marginx,xc-marginx]
               yp=[yc+marginy,yc+marginy,yc-marginy]
            ENDELSE
         ENDIF ELSE BEGIN
            IF (Y2 GT yc) THEN BEGIN
               add=3
               xp=[xc+marginx,xc+marginx,xc-marginx]
               yp=[yc-marginy,yc+marginy,yc+marginy]
            ENDIF ELSE BEGIN
               add=2
               xp=[xc+marginx,xc-marginx]
               yp=[yc-marginy,yc-marginy]
            ENDELSE
         ENDELSE
      ENDELSE
   ENDELSE

END
;-----------------------------------------------------------------------------
PRO ReadDataFile,filename,filetype,widgetaction
;+
; NAME: ReadDataFile
; PURPOSE: Reads data from file using the format prescribed by datatype
; CALLING SEQUENCE: ReadDataFile,filename,filetype
; where filename is string (without path) and datatype is an integer
; INPUT: filename: string (without path)
;        filetype: integer
; COMMON BLOCKS: datainfo,xyz,choices,drawsize,plotvar,widgets,datatype,
;                current,smallval,zoomidx,vecprm,prprm,psprm,dlprm,pmprm,
;                flags,rotprm,rotarr
;-

;** 1 ** Display parameters
common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON TITLES,  font,outtit,outxtit,outytit,outcbtit,textx,texty,textdx,textdy
COMMON POSTSCRIPT, filep,pspath,prcoul,psor,pstype, $
                pwinx,pwiny,papierx,papiery,xoffset,yoffset, $
                facpolice,fontrescale,basefontsize,pssizex,pssizey,psfont
COMMON THREED,  Ax3D,Az3D,smoothing
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
COMMON WGene,Wgrideditor,WG,root
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth

;** 2 ** Display and data variables/parameters
COMMON FREQ,    freq,nfband,findex,findex2
COMMON DIR,     nabin,nabint,aindex,THETA
COMMON MAP,     MAPFLAG, MAPPROJ, MAPLONGCENTER, MAPLATCENTER, MAPCONTINENT,  $
                MAPCOUNTRIES, MAPLONLAT
COMMON MAPPOLE, XPOLE, YPOLE, SLAT, RE, E2, E
COMMON TIME,    timestep,tindex,tindex2,ntime,dtime,time0,day0,dtindex,nstep, $
                timezone_plot,timezone_string,months,time
COMMON TRANSECT,Ntrans,Strans,Xtrans,Ytrans,Ztrans,Itrans, $
                  COtrans,TransOK,transsym,transline,transthick,Ispectrans, $
                  spectransname,ntransgp,transsymsize
COMMON TRANSLOC,igpt,thetatrans,gridtmat,transtype

COMMON SPACE,   c_gp,c_cut,indexgp,c_x,c_y,c_lon,c_lat
COMMON OVERLAY, addir,adsyms,adbathy,adcoast,psyms,psymsizes,adtr,adtri

;** 3 ** I/O and data variables
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON DIR_STACK, DEPTH, STACK
COMMON ARR8m,   nfTom,naTom,TomF,Tomth,OBS8m,MOD8m
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON COAST,   coastxy,coastl,coastnp,GSHHSPoly,GSHHSPoint,GSHHSPoly2,GSHHSPoint2
COMMON DEPTHSTAT,nbind,bindres,HISTD,MEAND,SDEVD
COMMON DIRSPEC, ds,dsnfband,ndir,kdkdth,dsfreq,dstheta,dsdf,dstime,dsntime,ds_timezone
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON TRIANGLE2,nzonetri,zonetri,nzonegp,zonegp,ngpzone,gpzone
COMMON GRIDPar, th1,th2,gfact,gsize1,gsize6
COMMON TIMESERIES, modspec,obs,obs2,om,ts_filetype, nbins, bulktype,scatvar
COMMON RAYS,    raysOK,raystype,raynsteps,rayx,rayy,raya,rayamin,rayamax, $
                rayres,rayfreq,rayGP,raytimestep,rayoffdep,rayflag,raydz,raymindepth
COMMON SOURCE,  S0,snabin,sntstep
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
COMMON WW3,     ww3model,ww3date,ww3time,ww3fieldname,ww3scale,ww3miss,ww3unit,ww3lon, $
               ww3lat,ww3matrix,ww3dir,ww3path,spec2D,ww3matrix2,ww3dir2,ncid,ncid2,ncivar0,ncivar
COMMON WGene,Wgrideditor,WG,root
COMMON XYZDATA, NXYZ, XYZ
COMMON SHORELINE2, open_boundary
;*******END OF COMMON BLOCKS*******************************

;   FILE TYPES:
; 0              'Ray3.inp', $        general purpose input file
; 1              'Ray3b output', $    output of the RAY3 model
; 2              'Bathy log file', $  log file for the bathymetry
; 3              'Binary bathy', $    binary bathymetry file
; 4              'Coast file', $      coastline file used for overlay on plots: reads a gshhs 2.0 file
; 5              'Grid file', $       model grid positions (x,y, lat,lon)
; 6              'Depth stats', $     depth statistics around grid points
; 7              'GMESH triangle-based grid', $       mesh using GMESH format
; 8              'Transect', $        coordinates of points for transect display
; 9              'Offshore spec.', $  time series of boundary condition
;10              'Model specials', $
;11              'Observations', $      observed freq. spectra at sites X to I
;12              'Ray3b AtoI', $      model additional output for A to I sites
;13              '8m array OBS', $    observed 8m array spectrum
;14              '8m array MOD'])     model 8m array spectrum


   unit=filetype+1
   CD,'.',CURRENT=cpath
   raypath=cpath  ; BIG BUG TO BE FIXED
;
   IF (filetype NE 17) THEN BEGIN
;
; closes old files and open newly selected file
;
     ON_IOERROR, not_OPEN
     CLOSE,unit
NOT_OPEN:     print,'PATH0:',filetype,paths(filetype)
      ON_IOERROR, not_NETCDF
      openOK=0
      ncidtest=-1
      ncidtest=NCDF_OPEN(paths(filetype)+filename,/NOWRITE)
      openOK=1
NOT_NETCDF: IF (openOK EQ 0) THEN BEGIN   
         IF (((filetype EQ 1) OR (filetype EQ 3) $
         OR (filetype EQ 4))) $ ; AND !VERSION.OS_FAMILY EQ 'Windows' ) $
        THEN OPENR,unit,paths(filetype)+filename,/SWAP_ENDIAN $
        ELSE OPENR,unit,paths(filetype)+filename
      ENDIF
;
   ENDIF ELSE BEGIN
     IF N_ELEMENTS(ncid2) EQ 0 THEN BEGIN 
       IF N_ELEMENTS(ncid) GT 0 THEN NCDF_CLOSE,ncid
     ENDIF ELSE BEGIN 
       IF N_ELEMENTS(ncid) GT 0 AND (ncid NE ncid2)  THEN NCDF_CLOSE,ncid
     ENDELSE
     ncid=NCDF_OPEN(paths(filetype)+filename,/NOWRITE)
   ENDELSE
   ON_IOERROR, NULL
;
; sets the flag variable to indicate that file is read
;  
   filestatus(filetype)=filestatus(filetype)+1
   CASE filetype of

   1:BEGIN  ;reads xyz.dat
      nxyz=0L
      READF,unit,nxyz
      XYZ=FLTARR(NXYZ,3)
      XYZ1=FLTARR(3)
      FOR I=0L,nxyz-1 DO BEGIN 
         READF,unit,XYZ1
         XYZ(I,*)=XYZ1
      ENDFOR
      readOK=1
      lonmin=MIN(XYZ(*,0))
      lonmax=MAX(XYZ(*,0))
      latmin=MIN(XYZ(*,1))
      latmax=MAX(XYZ(*,1))
      UPDATE_COORDINATES,lonmin,lonmax,latmin,latmax,widgetaction
  
 
      FOR I=0L,nxyz-1 DO BEGIN                
               latdeg=FLOOR(XYZ(I,1))
               IF latdeg LT 0 THEN latdeg=latdeg+1
               londeg=FLOOR(XYZ(I,0))
               IF londeg LT 0 THEN londeg=londeg+1
               latmin=(XYZ(I,1)-latdeg)*60.
               lonmin=(XYZ(I,0)-londeg)*60.
               LatLontoXY,latdeg,latmin,londeg,lonmin,x,y
               XYZ(I,0)=X
               XYZ(I,1)=Y
      ENDFOR
      END
;
;reads bathy log file
;
   2:BEGIN 
      nx=0L
      ny=0L
      toto=0
      MAPLONLAT=1
      READF,unit,rlatmin,rlatmax,rlonmin,rlonmax
      readOK=0
      ON_IOERROR, bad_numlog
      READF,unit,toto
      READF,unit,sx,sy
      READF,unit,nx,ny
      readOK=1
bad_numlog: IF (readOK EQ 0) THEN BEGIN  
        nx=ROUND((rlonmax-rlonmin)*1200)+1
        ny=ROUND((rlatmax-rlatmin)*1200)+1
        sx=(rlonmax-rlonmin)*4E7/360./(nx-1)*cos(0.5*(rlatmax+rlatmin)*!pi/180.)
        sy=(rlatmax-rlatmin)*4E7/360./(ny-1)
        print,'Automatic setting of nx and ny:',nx,ny
      ENDIF
      dx=sx/1000.
      dy=sy/1000.
      nxzmin=0L
      nyzmin=0L
      nxzmax=nx-1
      nyzmax=ny-1
      rangex=[0,dx*FLOAT(nx-1)]
      rangey=[0,dy*FLOAT(ny-1)]
      IF widgetaction THEN WIDGET_CONTROL, Wdraw, SENSITIVE=1
      paths(filetype+1)=paths(filetype)
      datastatus(3)=0
      END
   3:BEGIN  ;reads binary bathymetry file (generated with matlab)
     print,'Test NetCDF:',ncidtest
     IF (ncidtest LT 0) THEN BEGIN 
;
; in this case this is not a NetCDF file 
;          
        gd=FLTARR(ny,nx)
        READU,unit,gd
        gd=transpose(gd)
      ENDIF ELSE BEGIN 
         varid=NCDF_VARID(ncidtest,'x_range') 
         NCDF_VARGET,ncidtest,varid,rangex
         rlonmin=rangex(0)
         rlonmax=rangex(1)
         varid=NCDF_VARID(ncidtest,'y_range') 
         NCDF_VARGET,ncidtest,varid,rangey
         rlatmin=rangey(0)
         rlatmax=rangey(1)
         varid=NCDF_VARID(ncidtest,'dimension') 
         NCDF_VARGET,ncidtest,varid,dimensions
         nx=dimensions(0)
         ny=dimensions(1)
         varid=NCDF_VARID(ncidtest,'z') 
         NCDF_VARGET,ncidtest,varid,gd
         gd = REFORM(gd, nx, ny, /OVERWRITE)
         gd=-1.*REVERSE(gd,2)
        sx=(rlonmax-rlonmin)*4E7/360./(nx-1)*cos(0.5*(rlatmax+rlatmin)*!pi/180.)
        sy=(rlatmax-rlatmin)*4E7/360./(ny-1)
        dx=sx/1000.
        dy=sy/1000.
        nxzmin=0L
        nyzmin=0L
        nxzmax=nx/2-1
        nyzmax=ny/2-1
        rangex=[0,dx*FLOAT(nxzmax-1)]
        rangey=[0,dy*FLOAT(nyzmax-1)]
        IF widgetaction THEN WIDGET_CONTROL, Wdraw, SENSITIVE=1
        datastatus(2)=1
      ENDELSE 
      END
   
   4:BEGIN    ; reads a gshhs v2.0 coastline file
               ; (cf. www.ngdc.noaa.gov/mgg/shorelines/gshhs.html)
      tempoPoly=LONARR(600000,10)
      tempoPoint=FLTARR(20000000,2)
      ID=0L    ;identity number of polygon
      NP=0L    ; number of points in polygon
      LEV=0L   ; level (1 land, 2 lake, 3 island in lake ...)
      LEVMORE=BYTARR(4)
      MW=0L
      ME=0L
      MS=0L
      MN=0L
      AREA=0L
      FLAG=0L
      AREA_FULL=0L
      CONTAINER=0L
      ANCESTOR=0L
      X=0L
      y=0L
      tempoPoly(0,0)=0 ;pointer to tempoPoint
                       ; the points of polygon I are stored in
                       ;tempoPoint(tempoPoly(I,0):tempoPoly(I+1,0)-1)
      IC=0L
      ; PRINT,'Rlonmax:',rlonmin,rlonmax,rlatmin,rlatmax
      marginx=0.
      marginy=(rlatmax-rlatmin)*0.2
      rlonminn=rlonmin
      IF ((rlonmax-rlonmin) LT 60) THEN BEGIN
        marginx=(rlonmax-rlonmin)*0.2
        rlonminn=rlonmin-6.*marginx
      ENDIF
      xc=(rlonmax+rlonmin)/2.
      yc=(rlatmax+rlatmin)/2.

      Outreduced=0
      Outdump=0
      
      IF (outdump EQ 1) THEN BEGIN 
         GET_LUN,unit2
         OPENW,unit2,'GSHHS_v2_fullres_xy_nolakes.txt'
         depthcote=-10.
      ENDIF

      WHILE NOT EOF(unit) DO BEGIN
         READU,unit,ID,NP,LEVMORE,MW,ME,MS,MN,AREA,AREA_FULL,CONTAINER,ANCESTOR ;reads the polygon header
         LEV=LONG(LEVMORE(0))
         ;IF (ID LE 100) THEN PRINT,'Np and bounds:',ID,NP,'##',LEV,'##',MW,ME,MS,MN
         VECTOR=LONARR(NP*2)
         READU,unit,VECTOR
         VECTOR=TRANSPOSE(REFORM(VECTOR,2,NP))
         tempoPoly(IC+1,0)=tempoPoly(IC,0)+NP
         tempoPoly(IC,1)=ID
         tempoPoly(IC,3)=LEV
         tempoPoly(IC,4)=MW
         tempoPoly(IC,5)=ME
         tempoPoly(IC,6)=MS
         tempoPoly(IC,7)=MN
         tempoPoly(IC,8)=AREA
         tempoPoly(IC,9)=FLAG

;
; Corrects for West indies shift ... 
;
  IF (MW GT  298400000 AND ME LT 299400000  AND MS GT    14200000  AND MN LT  15800000) THEN BEGIN 
          VECTOR(*,0) = VECTOR(*,0) + (0.0055*1E6)
          VECTOR(*,1) = VECTOR(*,1) + (0.0033*1E6)
      ENDIF
  IF (MW GT  298000000 AND ME LT 299400000  AND MS GT    15730000  AND MN LT  16600000) THEN BEGIN 
          VECTOR(*,0) = VECTOR(*,0) - (0.004*1E6)
          VECTOR(*,1) = VECTOR(*,1) - (0.0035*1E6)
      ENDIF
         ; Dump ASCII file with polygons
         IF (outdump EQ 1) THEN BEGIN 
            IF (LEV EQ 1L) THEN BEGIN 
               PRINTF,unit2,FORMAT='(2I10)',ID,NP
               FOR I=0L,NP-1 DO BEGIN
                 PRINTF,unit2,FORMAT='(2F11.6)',((FLOAT(VECTOR(I,0))*1.E-6+180.) MOD 360)-180,FLOAT(vectOR(I,1))*1.E-6 ;,depthcote
               ENDFOR
            ENDIF 
         ENDIF 

         IF (outreduced EQ 1) THEN BEGIN
            kountk=0L
            X=LONARR(NP)
            Y=X
            X(*)=VECTOR(*,0)
            Y(*)=VECTOR(*,1)
            IF (MAPLONLAT LT 2) THEN $
            Keep=WHERE(((((FLOAT(X)*1.E-6+180.) MOD 360.)  $
                           -rlonmax-marginx-180) LE 0.) AND $
                        ((((FLOAT(X)*1.E-6+180.) MOD 360.)  $
                           -rlonmin+marginx-180) GE 0.) AND $
                           ((FLOAT(Y)*1.E-6   $
                           -rlatmax-marginy) LE 0.) AND $
                        ((FLOAT(Y)*1.E-6   $
                           -rlatmin+marginy) GE 0.),kountk) $
            ELSE BEGIN 
              Keep=X*0+1
              kountk=NP
            ENDELSE

            NPO=NP-kountk
            NP=kountk
            IF kountk GT 0 THEN BEGIN
print,'IC:',kountk
               tempoPoint(tempoPoly(IC,0):tempoPoly(IC,0)+NP-1,0:1)=VECTOR(Keep,*)
               IP1=KEEP(0)
               IP2=KEEP(kountk-1)
               x2=((VECTOR(IP2,0)*1.E-6+180.) MOD 360.)-180.
               y2=VECTOR(IP2,1)*1.E-6
               x1=((VECTOR(IP1,0)*1.E-6+180.) MOD 360.)-180.
               y1=VECTOR(IP1,1)*1.E-6
               PRINT,'IC:   ',IC,ID,NP,kountk,NPO,NPO+NP,IP1,IP2,x1,x2,y1,y2,xc,yc

               IF (NPO GT 0) THEN BEGIN
                          IF (ABS(x2-x1) GT ABS(rlonmax-rlonmin)) $
                                OR ABS(y2-y1) GT ABS(rlatmax-rlatmin) THEN BEGIN
                               a=(y1-y2)/(y2*x1-x2*y1)
                             b=(x2-x1)/(y2*x1-x2*y1)
                             c=1

                          IF (X2-X1)*(y2-y1) LT 0 THEN BEGIN
                             IF (a*xc+b*yc+c)*(a*rlonmax+b*rlatmax+c) GT 0 THEN BEGIN
                                 ;tempoPoint(tempoPoly(IC,0)+NP,0)=FLOAT(rlonmax+marginx+360.)*1.E6
                              ;tempoPoint(tempoPoly(IC,0)+NP,1)=FLOAT(rlatmax+marginy)*1.E6
                                    PRINT,'Adding point in polygon (1,1)...'
                          ENDIF ELSE BEGIN
                            ;tempoPoint(tempoPoly(IC,0)+NP,0)=FLOAT(rlonmin-marginx+360.)*1.E6
                            ;tempoPoint(tempoPoly(IC,0)+NP,1)=FLOAT(rlatmin-marginy)*1.E6
                                  PRINT,'Adding point in polygon (-1,-1)...'
                         ENDELSE
                  ENDIF ELSE BEGIN
                     IF (a*xc+b*yc+c)*(a*rlonmax+b*rlatmin+c) GT 0 THEN BEGIN
                                 tempoPoint(tempoPoly(IC,0)+NP,0)=FLOAT(rlonmin-marginx+360.)*1.E6
                              tempoPoint(tempoPoly(IC,0)+NP,1)=FLOAT(rlatmax+marginy)*1.E6
                                    PRINT,'Adding point in polygon (-1,1)...'
                          ENDIF ELSE BEGIN
                            tempoPoint(tempoPoly(IC,0)+NP,0)=FLOAT(rlonmax+marginx+360.)*1.E6
                            tempoPoint(tempoPoly(IC,0)+NP,1)=FLOAT(rlatmin-marginy)*1.E6
                                  PRINT,'Adding point in polygon (1,-1)...'
                         ENDELSE
                  ENDELSE
                  NP=NP+1
                  ENDIF
               ENDIF
               tempoPoly(IC,2)=NP
               tempoPoly(IC+1,0)=tempoPoly(IC,0)+NP
               IC=IC+1
            ENDIF
         ENDIF ELSE BEGIN
            IF (FLOAT(MS)*1.E-6 LT -80. AND MAPLONLAT LE 1) THEN BEGIN
                  NP=NP+1
                print,'Antarctica:',vectOR(0:1,0)
                tempo=vectOR
                  VECTOR=LONARR(NP,2)
                  vectOR(0,0)=FIX(rlonmax)*1000000
                  vectOR(0,1)=FIX(-90)*1000000
                        vectOR(NP-1,0)=FIX(rlonminn)*1000000
                  vectOR(NP-1,1)=FIX(-90)*1000000

                  VECTOR(1:NP-2,0:1)=tempo(0:np-3,0:1)
            ENDIF
            X=FLTARR(NP)
            X(*)=FLOAT(VECTOR(*,0)*1.E-6 - rlonminn)
            index=WHERE(X EQ 360.,kount)
            X(*)=(X  MOD 360.)

            IF kount GT 0 THEN X(index)=360.
            Index1=WHERE(X LT 0,kount1)
            IF kount1 GT 0 THEN X(Index1)=X(Index1)+360.

                  ; Looks for wrap around
            XP1=SHIFT(X,-1)
            Indexch=WHERE(ABS(X-XP1) GT 350.,kountch)
            IF ((rlonmax-rlonmin) GT 60) AND (kountch GT 2) AND (MAPLONLAT NE 2) THEN BEGIN
                  ;PRINT,'Flip:',IC,kountch,Indexch,X(Indexch),XP1(Indexch)
                  NP2=Indexch(0)+1+NP-1-Indexch(kountch-1)
                FOR Iwrap=1,kountch-2,2 DO BEGIN
                      NP2=NP2+Indexch(Iwrap+1)-Indexch(Iwrap)
                ENDFOR
                NP3=NP-NP2
                VECTR=FLTARR(NP2,2)
                VECTR3=FLTARR(NP3,2)
                VECTR(0:Indexch(0),0)=X(0:Indexch(0))-marginx
                VECTR(0:Indexch(0),1)=FLOAT(VECTOR(0:Indexch(0),1))*1.E-6-rlatmin
                Ip2=Indexch(0)+1
                Iwrap=0
                Ip3=0
                FOR Iwrap=1,kountch-1,2 DO BEGIN
                   IF (Iwrap LT kountch-1) then BEGIN
                           VECTR(Ip2:Ip2+Indexch(Iwrap+1)-Indexch(Iwrap)-1,0) $
                                =X(Indexch(Iwrap)+1:Indexch(Iwrap+1))-marginx
                        VECTR(Ip2:Ip2+Indexch(Iwrap+1)-Indexch(Iwrap)-1,1) $
                                    =FLOAT(VECTOR(Indexch(Iwrap)+1:Indexch(Iwrap+1),1))*1.E-6-rlatmin
                      Ip2=Ip2+Indexch(Iwrap+1)-Indexch(Iwrap)
                   ENDIF
                   VECTR3(Ip3:Ip3+Indexch(Iwrap)-Indexch(Iwrap-1)-1,0) $
                               =X(Indexch(Iwrap-1)+1:Indexch(Iwrap))-marginx
                     VECTR3(Ip3:Ip3+Indexch(Iwrap)-Indexch(Iwrap-1)-1,1) $
                                 =FLOAT(VECTOR(Indexch(Iwrap-1)+1:Indexch(Iwrap),1))*1.E-6-rlatmin
                   Ip3=Ip3+Indexch(Iwrap)-Indexch(Iwrap-1)
                   ENDFOR
                   VECTR(Ip2:NP2-1,0)=X(Indexch(kountch-1)+1:NP-1)-marginx
                   VECTR(ip2:NP2-1,1)=FLOAT(VECTOR(Indexch(kountch-1)+1:NP-1,1))*1.E-6-rlatmin
                   NPold=NP
                   NP=NP2

                   IF (IP3 GT 3) THEN BEGIN
                     tempoPoint(tempoPoly(IC,0):tempoPoly(IC,0)+NP-1,0:1)=VECTR(*,*)
                     tempoPoly(IC,2)=NP
                         tempoPoly(IC+1,0)=tempoPoly(IC,0)+NP

                     IC=IC+1
                           NP=NP3
                           VECTR=VECTR3
                           tempoPoly(IC+1,0)=tempoPoly(IC,0)+NP
                     tempoPoly(IC,1)=ID
                     tempoPoly(IC,3)=LEV
                     tempoPoly(IC,4)=MW
                     tempoPoly(IC,5)=ME
                     tempoPoly(IC,6)=MS
                     tempoPoly(IC,7)=MN
                     tempoPoly(IC,8)=AREA
                     tempoPoly(IC,9)=FLAG
                  ENDIF

                ENDIF ELSE BEGIN
                  VECTR=FLTARR(NP,2)
                  VECTR(*,0)=X(*)-6.*marginx
                  VECTR(*,1)=FLOAT(VECTOR(*,1))*1.E-6
                  IF (MAPLONLAT LT 2) THEN VECTR(*,1)=VECTR(*,1)-rlatmin
              ENDELSE
            tempoPoint(tempoPoly(IC,0):tempoPoly(IC,0)+NP-1,0:1)=VECTR(*,*)
            tempoPoly(IC,2)=NP
            tempoPoly(IC+1,0)=tempoPoly(IC,0)+NP
            IC=IC+1
         ENDELSE

      ENDWHILE
      IF (outdump EQ 1) THEN BEGIN 
         CLOSE,unit2
         FREE_LUN,unit2
      ENDIF 
      Npoly=IC
      GSHHSPoly=tempoPoly(0:Npoly,*)
      PRINT,'Number of relevant polygons:',IC,GSHHSPoly(Npoly),SIZE(tempoPoint)
      PRINT,'Bounds of poly 1:',tempoPoly(0,4:7)*1E-6


      GSHHSPoint=tempoPoint(0:GSHHSPoly(Npoly)-1,*)

;************************************************
; Outputs a reduced version of the original file
      IF (outreduced EQ 1) THEN BEGIN
         GET_LUN,unit2
         OPENW,unit2,'gshhs_cal.b'
         FOR I=0L,Npoly-1 DO BEGIN
           WRITEU,unit2,GSHHSPoly(I,1:9)
           NP=GSHHSPoly(I,2)
           AR=LONARR(NP,2)
           AR(*,*)=GSHHSPoint(GSHHSPoly(I,0):GSHHSPoly(I+1,0)-1,*)
           WRITEU,unit2,REFORM(TRANSPOSE(AR),NP*2)
         ENDFOR
         CLOSE,unit2
         FREE_LUN,unit2
      ENDIF
;************************************************
      ; COnverts to kilometers
      tempo=GSHHSPOINT
      GSHHSPoint=FLTARR(GSHHSPoly(Npoly),2)
      IF (MAPLONLAT EQ 1) THEN BEGIN 
      GSHHSPoint(*,0)=(nx-1)*dx*tempo(*,0)/ABS(rlonmax-rlonmin)
      GSHHSPoint(*,1)=(ny-1)*dy*tempo(*,1)/ABS(rlatmax-rlatmin)
      ENDIF
      IF (MAPLONLAT EQ 2) THEN BEGIN 
        FOR I=0L,GSHHSPoly(Npoly)-1 DO BEGIN 
           mapll,xx,yy,tempo(I,1),tempo(I,0),1
           GSHHSPoint(I,0)=xx+XPOLE
           GSHHSPoint(I,1)=yy+YPOLE
        ENDFOR
      ENDIF
     END


 ; reads france.line format

   5:BEGIN   
      tempoPoly=LONARR(10000,10)
      tempoPoint=FLTARR(2000000,2)
      ID=0L    ;identity number of polygon
      NP=0L    ; number of points in polygon
      
      line='                                                                     '
      Outdump=0
      IF (outdump EQ 1) THEN BEGIN 
         GET_LUN,unit2
         OPENW,unit2,'france_more.line'
      ENDIF
      READF,unit,line
      IF (outdump EQ 1) THEN PRINTF,unit2,line
      READF,unit,line
      IF (outdump EQ 1) THEN PRINTF,unit2,line
      IC=0L
      ICORIGIN=0L
      ICALL=0L
      ALLMULTIPLES=0L
      ALLP=0L
      WHILE NOT EOF(unit) DO BEGIN
         ICALL=ICALL+1L
         READF,unit,NP,ICORIGIN
         VECTOR=FLTARR(NP,2)
         MULTIPLES=0L
         FOR I=0L,NP-1 DO BEGIN 
            READF,unit,LON,LAT
            VECTOR(I-MULTIPLES,0)=LON
            VECTOR(I-MULTIPLES,1)=LAT
            IF (I GT 0) THEN $
               IF (VECTOR(I,0) EQ VECTOR(I-1,0) AND VECTOR(I,1) EQ VECTOR(I-1,1)) $
                 THEN  MULTIPLES=MULTIPLES+1
         ENDFOR
         NP=NP-MULTIPLES
         IF (outdump EQ 1) THEN PRINTF,unit2,FORMAT='(2I10)',NP,ICORIGIN
         IF (outdump EQ 1) THEN BEGIN 
           IF (ICALL EQ 2) THEN BEGIN  
           FOR I=NP-1,0L,-1 DO BEGIN
               PRINTF,unit2,FORMAT='(2F13.8)',VECTOR(I,0:1) ;,depthcote
            ENDFOR
            ENDIF ELSE BEGIN 
          FOR I=0L,NP-1 DO BEGIN
               PRINTF,unit2,FORMAT='(2F13.8)',VECTOR(I,0:1) ;,depthcote
            ENDFOR
            ENDELSE        
         ENDIF 
         ALLP=ALLP+NP
         ALLMULTIPLES=ALLMULTIPLES+MULTIPLES
         tempoPoly(IC+1,0)=tempoPoly(IC,0)+NP
         tempoPoly(IC,1)=ID
         tempoPoly(IC,3)=1
         MW=MIN(VECTOR(*,0))
         ME=MAX(VECTOR(*,0))
         MS=MIN(VECTOR(*,1))
         MN=MAX(VECTOR(*,1))
         tempoPoly(IC,4)=ROUND(1E6*MW)
         tempoPoly(IC,5)=ROUND(1E6*ME)
         tempoPoly(IC,6)=ROUND(1E6*MS)
         tempoPoly(IC,7)=ROUND(1E6*MN)
         tempoPoly(IC,8)=0
         tempoPoly(IC,9)=0
         IF (ME GT rlonmin and MW LT rlonmax AND MN GT rlatmin and MS LT rlatmax) THEN BEGIN
;           
; Only keeps polygons that are in the area
;
            tempoPoint(tempoPoly(IC,0):tempoPoly(IC,0)+NP-1,0:1)=VECTOR(0:NP-1,*)
            tempoPoly(IC,2)=NP
            tempoPoly(IC+1,0)=tempoPoly(IC,0)+NP
            IC=IC+1
         ENDIF 
      ENDWHILE
      IF (outdump EQ 1) THEN BEGIN 
         CLOSE,unit2
         FREE_LUN,unit2
      ENDIF 

      Npoly=IC
      GSHHSPoly2=tempoPoly(0:Npoly,*)
      PRINT,'Number of relevant polygons:',IC,GSHHSPoly2(Npoly)
      PRINT,'Number of points and multiples:',ALLP, MULTIPLES

      GSHHSPoint2=tempoPoint(0:GSHHSPoly2(Npoly)-1,*)

      tempo=GSHHSPOINT2
      GSHHSPoint2=FLTARR(GSHHSPoly2(Npoly),2)
      GSHHSPoint2(*,0)=(nx-1)*dx*(tempo(*,0)-rlonmin)/ABS(rlonmax-rlonmin)
      GSHHSPoint2(*,1)=(ny-1)*dy*(tempo(*,1)-rlatmin)/ABS(rlatmax-rlatmin)
      ;print,min(GSHHSPoint2(*,0)),max(GSHHSPoint2(*,0)),min(GSHHSPoint2(*,1)),max(GSHHSPoint2(*,1))
      GSHHSPoint=GSHHSPoint2
      GSHHSPoly=GSHHSPoly2
      END

;
;  Reading mesh with GMSH format
;
   6 :BEGIN  
         line='                                                                                          '
         name=line
         WHILE NOT EOF(unit) DO BEGIN
            READF,unit,line
            linec=STRCOMPRESS(line)
            itri=0L
            CASE STRMID(linec,0,6) OF
               '$File ':BEGIN
                  I=0
                  READS,STRMID(line,6,2),I
                  READS,STRMID(line,8,80),name
                  filenames(I)=STRCOMPRESS(name,/REMOVE_ALL)
                  IF !VERSION.OS_FAMILY EQ 'Windows' THEN $
                    spos=STRPOS(filenames(I),'\') $
                  ELSE spos=STRPOS(filenames(I),'/')
                  PATHS(I)  =STRMID(filenames(I),0,spos+1)
                  filenopath=STRMID(filenames(I),spos+1, STRLEN(filenames(I))-spos-1)
                  filenames(I)=filenopath
                  ReadDataFile,filenames(I),I,1
                  END
               '$MeshF':BEGIN
                 READF,unit,line
                 READF,unit,line
                 END
               '$Nodes':BEGIN
                    nngp=0L
                    READF,unit,nngp
                    IF subwin(0) AND N_ELEMENTS(WG) > 0 THEN $
                       WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
                    gridmat=DBLARR(8,nngp)
                    Ind=0
                    lon=0.0d
                    lat=0.0d
                    z=0.0
                    FOR I=0L,nngp-1 DO BEGIN
                       READF,unit,Ind,lon,lat,z
;
; Converts decimal degrees to degree , minute and X, Y
;
                       latdeg=FLOOR(lat)
                       IF latdeg LT 0 THEN latdeg=latdeg+1
                       londeg=FLOOR(lon)
                       IF londeg LT 0 THEN londeg=londeg+1
                       latmin=(lat-latdeg)*60.
                       lonmin=(lon-londeg)*60.
                       gridmat(2,I)=latdeg
                       gridmat(3,I)=latmin
                       gridmat(4,I)=londeg
                       gridmat(5,I)=lonmin
                       gridmat(7,I)=z
                    ENDFOR
                    lonmin=MIN(gridmat(4,*)+gridmat(5,*)/60.)
                    lonmax=MAX(gridmat(4,*)+gridmat(5,*)/60.)
                    latmin=MIN(gridmat(2,*)+gridmat(3,*)/60.) 
                    latmax=MAX(gridmat(2,*)+gridmat(3,*)/60.)
                    
                    UPDATE_COORDINATES,lonmin,lonmax,latmin,latmax,widgetaction
                    FOR I=0L,nngp-1 DO BEGIN
                       LatLontoXY,gridmat(2,I),gridmat(3,I),gridmat(4,I),gridmat(5,I),x,y
                       gridmat(0,I)=x
                       gridmat(1,I)=y
                    ENDFOR
                    IF subwin(0) THEN Displaygrid
                    READF,unit,line
                    datastatus(5)=1
                    IF N_ELEMENTS(nspecgp) EQ 0 THEN nspecgp=0
                    END
               '$Eleme':BEGIN
                 nel=0L
                 READF,unit,nel
                 TRIGP0=LONARR(nel+1,4)
                 CONTOURTEMP=LONARR(NNGP,2)
                 open_boundary=INTARR(NNGP)
                 icont=0L
                 itri=0L
                 eltype=0
                 Ind=0L
                 Inode=0L
                 island=0
                 ntag=0
                 trinodes=LONARR(3)
                 FOR I=0L,nel-1 DO BEGIN
                       line='                                                                   '
                    READF,unit,line
                    READS,line,Ind,eltype,ntag
                    IF (I EQ 0) THEN BEGIN
                    ENDIF
                    ;IF (I EQ 0) THEN STOP
                    IF (ntag GT 0) THEN tags=LONARR(ntag)
                    CASE eltype OF
                    15:BEGIN
                          READS,line,Ind,eltype,ntag,tags,Inode
                          open_boundary(Inode-1)=ROUND(tags(0))
                          IF tags(1) EQ 0 THEN BEGIN
                           gridmat(6,Inode-1)=1
                          ENDIF ELSE BEGIN
                           gridmat(6,Inode-1)=-1
                           island=tags(1)
                          ENDELSE
                          CONTOURTEMP(icont,0)=Inode-1
                          CONTOURTEMP(icont,1)=gridmat(6,Inode-1)
                          icont=icont+1L
                          END
;
; Reads triangle element
;
                    2:BEGIN
                     IF (ntag GT 2) THEN BEGIN
                        READS,line,Ind,eltype,ntag,tags,trinodes
                        TRIGP0(ITRI+1,0)=tags(2)
                        IF tags(2) GT 5 THEN TRIGP0(ITRI+1,0)=5
                     ENDIF ELSE BEGIN
                        READS,line,Ind,eltype,ntag,tags,trinodes
                     ENDELSE
                     TRIGP0(ITRI+1,1)=trinodes(0)
                     TRIGP0(ITRI+1,2)=trinodes(1)
                     TRIGP0(ITRI+1,3)=trinodes(2)
                     ITRI=ITRI+1
                     END
;
                    1:BEGIN
                       END
                    ENDCASE
                 ENDFOR
                 READF,unit,line
;
; Cleans up contour
;                 
                 PRINT,'Number of contour nodes:',icont
                 IF (icont GT 0) THEN BEGIN 
                   CONTOURLINE=LONARR(icont+2,2)
                   CONTOURLINE(2:2+icont-1,*)=CONTOURTEMP(0:icont-1,*)
                   CONTOURLINE(0,0)=2
                   CONTOURLINE(1,0)=2+icont-1
                 ENDIF
; 
                 ntri=itri
                 AREATRI=FLTARR(ntri+1,2)
                 IF (ntri GT 0) THEN BEGIN
                    TRIGP=LONARR(ntri+1,4)
                    TRIGP(0:ntri,*)=TRIGP0(0:ntri,*)
                    FOR I=1L,ntri DO BEGIN 
                      FOR J=0,2 DO BEGIN
                        I1=TRIGP(I,J+1)-1
                        I2=TRIGP(I,((J+1) MOD 3)+1)-1
                        I3=TRIGP(I,((J+2) MOD 3)+1)-1
                        IF ((I1 NE -1) AND (I2 NE -1) AND (I3 NE -1)) THEN BEGIN
                          DDX=ABS(gridmat(0,I1)-gridmat(0,I2))
                          DDY=ABS(gridmat(1,I1)-gridmat(1,I2))
                          SIDEL=SQRT(DDX^2+DDY^2)
                          IF J EQ 0 THEN BEGIN 
                             AREATRI(I,0)=SIDEL 
                          ENDIF ELSE BEGIN 
                             IF AREATRI(I,0) GT SIDEL THEN AREATRI(I,0)=SIDEL
                          ENDELSE
                          ;IF (I MOD 2000) EQ 0 THEN PRINT,'SIDEL:',I,J,I1,I2,I3,AREATRI(I),DDX,DDY,SIDEL
                        ENDIF 
                     ENDFOR
                   ENDFOR
                   MEDTRI=MEDIAN(AREATRI(*,0))
                   PRINT,'Number of triangles and median size:',ntri,MEDTRI
                   PRINT,'Minimum size:', MIN(AREATRI(*,0))
                   ;MEDTRI=MEAN(AREATRI(*,0))
                   IKOUNT=WHERE(AREATRI(*,0) LT 0.01*MEDTRI,KOUNT)
                   IF (KOUNT GT 0L) THEN BEGIN 
                     FOR I=0L,KOUNT-1L DO BEGIN 
                        IF (IKOUNT(I) LT NTRI AND IKOUNT(I) GT 0) THEN $
                          PRINT,'WARNING SMALL TRIANGLE SIDE:',IKOUNT(I),AREATRI(IKOUNT(I),0),TRANSPOSE(TRIGP(IKOUNT(I),1:3)) $
                        ELSE $
                          PRINT, 'NO SMALL TRIANGLES:',IKOUNT(I),NTRI
                     ENDFOR
                   ENDIF
                 ENDIF
               END
             ENDCASE
         ENDWHILE
         datastatus(6)=1
         IF subwin(0) GT 0 THEN BEGIN 
           WIDGET_CONTROL,WG(0,3),SET_VALUE=STRCOMPRESS(STRING(nngp))
           WIDGET_CONTROL,WGrideditor(1),TABLE_YSIZE=nngp+1
         ENDIF
         ntri=itri
         END  ; end of GMSH file read
;
;reads grid file with special points (mostly for display purposes)
;
   7:BEGIN     
      nngptmp=0   ;number of grid points
      READF,unit,nngptmp
      x=0.
      y=0.
      ITOTO=1
      IF (nngptmp GT 0) THEN BEGIN 
      nngp=nngptmp
      gridmat=FLTARR(8,nngptmp)
      MAKE_CONTOUR
      FOR I=1,nngp DO BEGIN
         READF,unit,Itoto,x,y,latdeg,latmin,londeg,lonmin
         if (londeg LT lonmin) THEN londeg=londeg+360
         LatLontoXY,latdeg,latmin,londeg,lonmin,x,y
         gridmat(0,I-1)=x
         gridmat(1,I-1)=y
         gridmat(2,I-1)=latdeg
         gridmat(3,I-1)=latmin
         gridmat(4,I-1)=londeg
         gridmat(5,I-1)=lonmin
      ENDFOR
      subwin(0)=0
      IF subwin(0) THEN Displaygrid
      ENDIF

      ;reads info about special points (modified 10/7/99)
      nspecgp=0
      name='        '
      IF NOT EOF(unit) THEN BEGIN
         READF,unit,nspecgp
         IF (nspecgp GT 0) THEN BEGIN
            specmat=FLTARR(13,nspecgp)
            specname=STRARR(nspecgp)
            vecteur=FLTARR(13)
            FOR I=0,(nspecgp-1) DO BEGIN
               READF,unit,vecteur,name
               latdeg=vecteur(3)
               latmin=vecteur(4)
               londeg=vecteur(5)
               lonmin=vecteur(6)
               if (londeg LT lonmin) THEN londeg=londeg+360
               LatLontoXY,latdeg,latmin,londeg,lonmin,x,y
               vecteur(1)=x
               vecteur(2)=y

               specmat(0:12,I)=(vecteur)
               specname(I)=STRCOMPRESS(name)
print,'HEY:',I,x,y,specname(I)
            ENDFOR
         ENDIF
      ENDIF
      END
;
; reads transect files
;
   8:BEGIN         
      Ntrans=0     ;number of points in transect
      NtransGP=0
      transsym=0
      transsymsize=0.
      transline=0
      transthick=0
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
      TransOK=1
      END
;
; reads data time series output
;
   10:BEGIN     

     read_timeseries,obs,ncidtest,UNIT,I

      WIDGET_CONTROL,Wright(12,2),SET_SLIDER_MAX=I
      WIDGET_CONTROL,Wright(12,4),SET_SLIDER_MAX=I,SET_VALUE=I
      tindex2=I
      IF widgetaction THEN BEGIN
         findex=1
         findex2=obs.nfband
         WIDGET_CONTROL,Wright(12,0),SET_SLIDER_MAX=obs.nfband
         WIDGET_CONTROL,Wright(12,5),SET_SLIDER_MAX=obs.nfband, $
                                              SET_VALUE=obs.nfband
         WIDGET_CONTROL,Wright(12,2),SET_SLIDER_MAX=I
         WIDGET_CONTROL,Wright(12,4),SET_SLIDER_MAX=I,SET_VALUE=I
      ENDIF
      findex=1
      findex2=obs.nfband
      flags=INTARR(I)+1
      obs.flags=ptr_new(flags)
      IF widgetaction THEN BEGIN
         WIDGET_CONTROL,Wright(12,0),SET_SLIDER_MAX=obs.nfband
         WIDGET_CONTROL,Wright(12,5),SET_SLIDER_MAX=obs.nfband,SET_VALUE=obs.nfband
         findex=1
         findex2=obs.nfband
         WIDGET_CONTROL,Wright(12,2),SET_SLIDER_MAX=I
         WIDGET_CONTROL,Wright(12,4),SET_SLIDER_MAX=I,SET_VALUE=I
         tindex=1L
         tindex2=I
      ENDIF
      END
;
; reads model time series output
;
   11:BEGIN         

      read_timeseries,modspec,ncidtest,UNIT,I

      WIDGET_CONTROL,Wright(12,2),SET_SLIDER_MAX=I
      WIDGET_CONTROL,Wright(12,4),SET_SLIDER_MAX=I,SET_VALUE=I
      tindex2=I
      IF widgetaction THEN BEGIN
         WIDGET_CONTROL,Wright(12,0),SET_SLIDER_MAX=modspec.nfband
         WIDGET_CONTROL,Wright(12,5),SET_SLIDER_MAX=modspec.nfband, $
                                              SET_VALUE=modspec.nfband
         WIDGET_CONTROL,Wright(12,2),SET_SLIDER_MAX=I
         WIDGET_CONTROL,Wright(12,4),SET_SLIDER_MAX=I,SET_VALUE=I
      ENDIF
      findex=1
      findex2=modspec.nfband
      print,'findex ...:',findex,findex2
      print,'tindex2:',tindex2,modspec.ntime
      paths(filetype+1)=paths(filetype)
      paths(filetype+4)=paths(filetype)
      pathmod=STRARR(1)
      filemod=STRARR(1)
      pathmod(0)=paths(filetype)
      filemod(0)=filename
      modspec.path=ptr_new(pathmod)
      modspec.filename=ptr_new(filemod)
         print,'SIZE 2:',SIZE(*modspec.data)
      END
;
; reads model time series output (new run) 
; this assumes the same time step ... 
;
   12:IF (N_ELEMENTS(modspec) NE 0) THEN BEGIN
         nrun=modspec.nrun                    
         olddata=(*modspec.data)             
         oldpath=(*modspec.path)
         oldfile=(*modspec.filename)

         oldmod = modspec
         typ=0
         n1=0
         n2=0
         n3=0
         data=FLTARR(nrun+1,oldmod.np,oldmod.nvar,oldmod.nfband, $
            oldmod.nabin,oldmod.ntime)

         read_timeseries,modspec,ncidtest,UNIT,I

         time1=(*oldmod.dates)[0].jday
         time2=(*oldmod.dates)[oldmod.ntime-1].jday
         timeold=(*oldmod.dates).jday
         timenew=(*modspec.dates).jday
         IND=WHERE(timenew GE time1 AND timenew LE time2,kount)
         IND2=WHERE(timeold GE timenew(0) ,kount2)
        
         IF (kount GT 0) THEN BEGIN 
           data0=(*modspec.data)
           I1=IND(0)
           data(nrun,*,*,*,*,IND2(0):IND2(0)+kount-1)=data0(0,*,*,*,*,IND)
         ENDIF
         data(0:nrun-1,*,*,*,*,*)=olddata
         modspec=oldmod
         modspec.nrun=nrun+1

         PTR_FREE,modspec.data
         modspec.data=ptr_new(data)
         data=(*modspec.data)
         

         pathmod=STRARR(nrun+1)
         filemod=STRARR(nrun+1)
         pathmod(0:nrun-1)=oldpath
         filemod(0:nrun-1)=oldfile
         pathmod(nrun)=paths(filetype)
         filemod(nrun)=filename
         
         print,'Additional model run, assumed same time step, variables, points.'
         print,'Number of model runs:',nrun
         modspec.path=ptr_new(pathmod)
         modspec.filename=ptr_new(filemod)
      ENDIF


   13:BEGIN  
      END

   14:BEGIN          ; reads model time series output: new point
      oldpath=(*modspec.path)
      oldfile=(*modspec.filename)
      taille=size(oldpath)
      nrun=taille(1)
      print,'SIZE:',taille,'##',oldpath+filename
      paths(11)=oldpath(0)
      ReadDataFile,filename,11,1
      print,'HEY:',0,'##',oldpath(0),filename
      FOR irun=1,nrun-1 DO BEGIN
        paths(12)=oldpath(irun)
        ii=STRSPLIT(filename,'.')
        otherfile=oldfile(irun)
        jj=STRSPLIT(otherfile,'.')
        filename2=strmid(otherfile,jj(0),jj(1)-jj(0)) $
                 +strmid(filename,ii(1),strlen(filename)-ii(1))
                 print,'HEY:',irun,'##',oldpath(irun),filename2
        ReadDataFile,filename2,12,1
      END
      END


   19:BEGIN ;data flags for model output and comparison with field data
      IF N_ELEMENTS(modspec) NE 0 THEN BEGIN ; model time series output (file type 10)
      IF (modspec.ntime NE 0) THEN BEGIN
      Imax=modspec.ntime
      flags=INTARR(Imax)
      I=0
      y=0
      d=0
      h=0
      m=0
      min=0
      zone=0.
      sec=0
      flag=1
      I2o=0
      over=0
      totflag=0
      WHILE NOT EOF(unit) AND (over EQ 0) DO BEGIN
         READF,unit,y,m,d,h,minu,zone,flag
         jday=JULDAY(m,d,y,h,minu,zone)
         IF (jday GE (*modspec.dates)[0].jday) $
            AND (jday LE (*modspec.dates)[Imax-1].jday) THEN BEGIN
            found=0
            I2=I2o
            WHILE (NOT found) AND (I2 LT Imax) DO BEGIN
               IF (ABS(((*modspec.dates)[I2].jday+(*modspec.dates)[I2].zone/24.0d) $
                      -(jday+zone/24.0d)) LT 0.01) THEN BEGIN
                        flags(I2)=flag
                        I2o=I2
                        found=1
                        totflag=totflag+flag
                     ENDIF
               I2=I2+1
            ENDWHILE
            I=I+1
            ;print,y,m,d,h,min,zone,flag
         ENDIF
         IF (jday GT (*modspec.dates)[Imax-1].jday) THEN over = 0
      ENDWHILE
      modspec.flags=ptr_new(flags)
      print,'Number of good flags:',totflag
      ENDIF
      ENDIF
      END


   17:BEGIN    ;reads WW3 NC format
      datastatus(20)=1
;
; Stores old grid 
;
      IF (N_ELEMENTS(rlonmin) GT 0) THEN BEGIN 
         rlonminold=rlonmin
         rlatminold=rlatmin
         rlonmaxold=rlonmax
         rlatmaxold=rlatmax
         nxold=nx
         nyold=ny
       ENDIF ELSE BEGIN 
         rlonminold=0.
         rlatminold=-90
         rlonmaxold=360
         rlatmaxold=90
         nxold=0
         nyold=0
      ENDELSE
;
; Gets size of arrays
;         
      ww3model='           '
      ncall=NCDF_INQUIRE(ncid)
      nvars=ncall.NVARS

      dimtid=NCDF_DIMID(ncid,'time')
      IF (dimtid GE 0 ) THEN NCDF_DIMINQ,ncid,dimtid,timename,ntime
      timeid=NCDF_VARID(ncid,'time')
      triid=NCDF_VARID(ncid,'tri') 
      MAPSTAid=NCDF_VARID(ncid,'MAPSTA') 
      dimxid=NCDF_DIMID(ncid,'x')
      dimyid=NCDF_DIMID(ncid,'y')
      IF (dimxid EQ -1) THEN BEGIN 
        dimxid=NCDF_DIMID(ncid,'longitude')
        dimyid=NCDF_DIMID(ncid,'latitude')
      END
      IF (dimxid EQ -1) THEN BEGIN 
        dimxid=NCDF_DIMID(ncid,'lon')
        dimyid=NCDF_DIMID(ncid,'lat')
      END
      IF (dimxid NE -1) THEN BEGIN 
        dimn=dimxid
        NCDF_DIMINQ,ncid,dimxid,name1,size1
        NCDF_DIMINQ,ncid,dimxid,name0,size0
        lonid=NCDF_VARID(ncid,'longitude') 
        latid=NCDF_VARID(ncid,'latitude') 
        MAPLONLAT=1
;
; Sets MAPLONLAT = 2 for curvilinear grids
;
        ncvar=NCDF_VARINQ(ncid,lonid)
        ndimvar=ncvar.NDIMS
        IF (ndimvar EQ 2) THEN MAPLONLAT = 2

        IF (lonid EQ -1) THEN BEGIN 
          MAPLONLAT=0
          lonid=NCDF_VARID(ncid,'x') 
          latid=NCDF_VARID(ncid,'y') 
        ENDIF 

      ENDIF ELSE BEGIN 
;
; Unstructured case: reads grid positions 
;
        dimn=NCDF_DIMID(ncid,'node')
        dimel=NCDF_DIMID(ncid,'element')
        lonid=NCDF_VARID(ncid,'longitude') 
        latid=NCDF_VARID(ncid,'latitude') 
        IF (lonid EQ -1) THEN lonid=NCDF_VARID(ncid,'lon') 
        IF (latid EQ -1) THEN latid=NCDF_VARID(ncid,'lat') 
        MAPLONLAT=1
        IF (lonid EQ -1) THEN BEGIN 
          MAPLONLAT=0
          lonid=NCDF_VARID(ncid,'x') 
          latid=NCDF_VARID(ncid,'y') 
        ENDIF 
        NCDF_VARGET,ncid,lonid,lon
        NCDF_VARGET,ncid,latid,lat
        NCDF_DIMINQ,ncid,dimn,name1,nngp
        NCDF_DIMINQ,ncid,dimel,name1,ntri
        print,'Number of nodes and triangles:',nngp,ntri
        IF (N_ELEMENTS(nspecgp) EQ 0) THEN nspecgp=0
        rlonmin=MIN(lon)
        rlatmin=MIN(lat)
        rlonmax=MAX(lon)
        rlatmax=MAX(lat)
        newgrid=1
        dlon=rlonmax-rlonmin
        dlat=rlatmax-rlatmin
        ww3lon=lon
        ww3lat=lat
        IF ((ABS(rlonmaxold-rlonmax) GT dlon*0.001) OR (ABS(rlonminold-rlonmin) GT dlon*0.001) OR  $
            (ABS(rlatmaxold-rlatmax) GT dlat*0.001) OR (ABS(rlatminold-rlatmin) GT dlat*0.001) ) $
            THEN newgrid=1 ELSE newgrid=0
        IF (N_ELEMENTS(nx) EQ 0 OR newgrid EQ 1) THEN BEGIN 
           nx=1601
           ny=1601
           datastatus(2)=1
           CASE MAPLONLAT OF 
           0: BEGIN 
             dx=dlon*(1./((nx-1)*1000.))
             dy=dlat*(1./((ny-1)*1000.))
             END
           1: BEGIN 
             dx=dlon*(40000./((nx-1)*360.)) *cos(0.5*(rlatmin+rlatmax)*!dtor)
             dy=dlat*(40000./((ny-1)*360.))
             END
           2: BEGIN 
             SLAT = 70.
             RE = 6378.273
             E2 = .006693883
             PI = 3.141592654
             E =  sqrt(E2)
             la1=lat(0,0)
             lo1=lon(0,0)
             mapll,xmin,ymin,la1,lo1,1.
             mapll,xmax,ymax,lat(nx-1,ny-1),lon(nx-1,ny-1),1.
             dx=(xmax-xmin)*(1./((nx-1)*1000.))
             dy=(ymax-ymin)*(1./((ny-1)*1000.))
             XPOLE=-1.*xmin
             YPOLE=-1.*ymin
             END
           ENDCASE 
           rangex=[0,dx*FLOAT(nx-1)]
           rangey=[0,dy*FLOAT(ny-1)]
        ENDIF
        WIDGET_CONTROL, Wdraw,SENSITIVE=1
        Wdraw_value_update=1
        gridmat=DBLARR(8,nngp)
        FOR I=0L,nngp-1 DO BEGIN
          latdeg=FLOOR(lat(I))
          IF latdeg LT 0 THEN latdeg=latdeg+1
          londeg=FLOOR(lon(I))
          IF londeg LT 0 THEN londeg=londeg+1
          latmin=(lat(I)-latdeg)*60.
          lonmin=(lon(I)-londeg)*60.
          IF (MAPLONLAT EQ 1) THEN BEGIN 
            LatLontoXY,latdeg,latmin,londeg,lonmin,x,y
          ENDIF ELSE  BEGIN
            x=lon(I)-rlonmin
            y=lat(I)-rlatmin
          ENDELSE
          gridmat(0,I)=x
          gridmat(1,I)=y
          gridmat(2,I)=latdeg
          gridmat(3,I)=latmin
          gridmat(4,I)=londeg
          gridmat(5,I)=lonmin
        ENDFOR
        triid=NCDF_VARID(ncid,'tri') 
       
        NCDF_VARGET,ncid,triid,ww3tri 
        usetri=0
        IF (usetri EQ 1) THEN BEGIN 
           XGP=gridmat(0,*);
           YGP=gridmat(1,*);
           TRIANGULATE,XGP,YGP,tri1
           taille=size(tri1)
                 
           ntri1=taille[2]
           nngpold=nngp
           nngptemp=nngpold+ntri1
           XGPnew=DBLARR(nngptemp)
           YGPnew=DBLARR(nngptemp)
           XGPnew(0:nngpold-1)=XGP
           YGPnew(0:nngpold-1)=YGP
           NFOUND=0L
           FOR I=0L,ntri DO BEGIN 
              FOUND=0L
              tr0=ww3tri(*,I) ;
              tr0S=SORT(tr0)
              tr0=tr0(tr0S)-1
              FOR J=0L,ntri1-1 DO BEGIN 
                 tr1=tri1(*,J) ;
                 tr1S=SORT(tr1)
                 tr1=tr1(tr1S)
                 IF (tr0(0) EQ tr1(0) AND tr0(1) EQ tr1(1) AND tr0(2) EQ tr1(2)) THEN BEGIN 
                    FOUND=J
                    NFOUND=NFOUND+1L
                    ;print,'FOUND I,J:',I,J,'##',NFOUND,ntri1-ntri,'##',tr0,'##',tr1
                    BREAK 
                 ENDIF
              ENDFOR
              IF (FOUND EQ 0L) THEN BEGIN 
                 XGPnew(nngpold-1+NFOUND)=TOTAL(XGP(tr0))/3.
                 YGPnew(nngpold-1+NFOUND)=TOTAL(YGP(tr0))/3.
              ENDIF
           ENDFOR
           nngp=nngpold+NFOUND
           gridmatold=gridmat
           gridmat=DBLARR(8,nngp)
           gridmat(*,0:nngp-1)=gridmatold
           FOR I=nngp,nngp+NFOUND-1 DO BEGIN 
              x=XGPnew(I)
              y=YGPnew(I)
              XYtoLATLON,x,y,latdeg,latmin,londeg,lonmin
              gridmat(0,I)=x
              gridmat(1,I)=y
              gridmat(2,I)=latdeg
              gridmat(3,I)=latmin
              gridmat(4,I)=londeg
              gridmat(5,I)=lonmin
           ENDFOR
           TRIANGULATE,XGPNew,YGPNew,ww3tri
        ENDIF
           taille=size(ww3tri)
           ntri=taille[2]       
           TRIGP=LONARR(ntri+1,4)
           datastatus(0)=1
           datastatus(6)=1
           TRIGP(1:ntri,1:3)=TRANSPOSE(ww3tri)
           TRIGP(*,0)=5
           datastatus(5)=1
      ENDELSE
      
      ivar=3
      IF (triid NE -1) THEN ivar=4
      IF (MAPSTAid NE -1) THEN ivar=ivar+1
      IF (timeid EQ 3) THEN BEGIN 
        ivar=ivar+1
        freqid=NCDF_VARID(ncid,'f') 
        NCDF_VARGET,ncid,freqid,freq
        nfband=N_ELEMENTS(freq)
        WIDGET_CONTROL,Wright(12,0),SET_SLIDER_MAX=nfband
        WIDGET_CONTROL,Wright(12,5),SET_SLIDER_MAX=nfband
        FINDEX2=NFBAND-1
      ENDIF
;
; Gets date strings for first time step
;
      IF (timeid GE 0 ) THEN NCDF_VARGET,ncid,timeid,timenow,COUNT=1,OFFSET=0 $
                        ELSE timenow=0.
      dayi=JULDAY(1,1,1990,0,0,0.)
      TimetoDate,dayi,timenow*60*24,ww3date,ww3time
;
; Case of structured grids
;
      IF (triid EQ -1) THEN BEGIN 
         NCDF_VARGET,ncid,lonid,ww3lon
         NCDF_VARGET,ncid,latid,ww3lat
         rlonmin=min(ww3lon)
         rlonmax=max(ww3lon)
         rlatmin=min(ww3lat)
         rlatmax=max(ww3lat)
print,'HEYM:',MAPLONLAT
         IF (MAPLONLAT EQ 0) THEN BEGIN 
            rlonmax=(rlonmax-rlonmin)*360./4E7
            rlonmin=0
            rlatmax=(rlatmax-rlatmin)*360./4E7
            rlatmin=0
            dlon=rlonmax/1000
            dlat=rlatmax/1000
         ENDIF
         taille=size(ww3lon)
         nx=taille(1)
         taille=size(ww3lat)
         ny=taille(1)
         gridvarids=FLTARR(nvars)
         igrids=0

         varnamel=0
         igrids=0
         FOR I=0,nvars-1 DO BEGIN
            ncvar=NCDF_VARINQ(ncid,I)
            ndimvar=ncvar.NDIMS
            FOR J=0,ndimvar-1 DO BEGIN 
               IF (ncvar.DIM(J) EQ dimxid AND ncvar.NDIMS GT 1  $
                   AND ncvar.name NE 'MAPSTA' and ncvar.name NE 'longitude' $
                   AND ncvar.name NE 'latitude' ) THEN BEGIN 
                 gridvarids(igrids)=I
                 igrids=igrids+1
                 IF (STRLEN(ncvar.name) GT varnamel) THEN varnamel=STRLEN(ncvar.name)
               ENDIF
            ENDFOR
         ENDFOR  
         ncivar=gridvarids(0)
         ncivar0=ncivar
         ncvar=NCDF_VARINQ(ncid,ncivar)
         ndimvar=ncvar.NDIMS
;
         CASE ndimvar OF
          2:   BEGIN 
               NCDF_VARGET,ncid,ivar,ww3m,COUNT=[nx,ny],OFFSET=[0,0]
               END
          3:   BEGIN 
               NCDF_VARGET,ncid,ivar,ww3m,COUNT=[nx,ny,1],OFFSET=[0,0,0]
               END
          4:   BEGIN 
               NCDF_VARGET,ncid,ivar,ww3m,COUNT=[nx,ny,1,1],OFFSET=[0,0,findex-1,0]  
               END
             ELSE: print,'Problem with number of dimensions:',ndimvar
          ENDCASE


         
      ENDIF ELSE BEGIN


         gridvarids=FLTARR(nvars)
         igrids=0

         varnamel=0
         igrids=0
         FOR I=0,nvars-1 DO BEGIN
            ncvar=NCDF_VARINQ(ncid,I)
            ndimvar=ncvar.NDIMS
            FOR J=0,ndimvar-1 DO BEGIN 
               IF (ncvar.DIM(J) EQ dimn AND ncvar.NDIMS GT 1 AND ncvar.name NE 'MAPSTA' ) THEN BEGIN 
                 gridvarids(igrids)=I
                 igrids=igrids+1
                 IF (STRLEN(ncvar.name) GT varnamel) THEN varnamel=STRLEN(ncvar.name)
               ENDIF
            ENDFOR
         ENDFOR  
         ivar=gridvarids(0)
         ncivar=gridvarids(0)
         ncivar0=ncivar
         ncvar=NCDF_VARINQ(ncid,ncivar)
         ndimvar=ncvar.NDIMS
;
         CASE ndimvar OF
          1:   BEGIN 
               NCDF_VARGET,ncid,ivar,ww3m,COUNT=[nngp],OFFSET=[0]
               END
          2:   BEGIN 
               NCDF_VARGET,ncid,ivar,ww3m,COUNT=[nngp,1],OFFSET=[0,0]
               END
          3:   BEGIN 
               NCDF_VARGET,ncid,ivar,ww3m,COUNT=[nngp,1,1],OFFSET=[0,findex-1,0]
               END
             ELSE: print,'Problem with number of dimensions:',ndimvar
          ENDCASE


      ENDELSE

;
;  Defines all gridded variable names 
;
          names=STRARR(igrids)
          FOR I=0,igrids-1 DO BEGIN 
             ivar=gridvarids(I)
             ncvar=NCDF_VARINQ(ncid,ivar) 
             names(I)=ncvar.name
          ENDFOR
          print,'Variables in NetCDF file:'
          print,TRANSPOSE(names)

          WIDGET_CONTROL,Wright(1,1),SET_VALUE=STRCOMPRESS(names)
  
               
         newgrid=0
         dlon=rlonmax-rlonmin
         dlat=rlatmax-rlatmin
         IF (ABS(rlonmaxold-rlonmax) GT dlon*0.001 OR ABS(rlonminold-rlonmin) GT dlon*0.001 OR  $
             ABS(rlatmaxold-rlatmax) GT dlat*0.001 OR ABS(rlatminold-rlatmin) GT dlat*0.001 OR  $
             nxold NE nx OR nyold NE ny ) THEN newgrid=1 



      ivar=gridvarids(0)
      var3=NCDF_VARINQ(ncid,ivar)
      attOK=NCDF_ATTINQ(ncid,ivar,'scale_factor')
      IF (attOK.Length GT 0) THEN NCDF_ATTGET,ncid,ivar,'scale_factor',ww3scale $
                             ELSE ww3scale=1.
      NCDF_ATTGET,ncid,ivar,'_FillValue',ww3miss
      NCDF_ATTGET,ncid,ivar,'units',ww3unit


      Index=WHERE(ww3m EQ ww3miss,kount)
     ; ww3m=TRANSPOSE(ww3m)

      var3=NCDF_VARINQ(ncid,ncivar)

      IF  STRPOS(var3.name,'fp') GT -1  THEN BEGIN 
           ww3m=1./(ww3m*ww3scale ) 
           var3.name='Tp'
          ENDIF ELSE ww3m=ww3m*ww3scale
      IF (kount GT 0) THEN ww3m(index)=-2.*maxval

      
       IF (igrids GT 1) THEN  BEGIN
         IF (triid EQ -1) THEN $
         NCDF_VARGET,ncid,ivar+1,ww3m2,COUNT=[nx,ny,1],OFFSET=[0,0,0] $
            ELSE NCDF_VARGET,ncid,ivar+1,ww3m2,COUNT=[nngp,1],OFFSET=[0,0]
         ww3m2=ww3m2*ww3scale
         ww3dir=90.-ATAN(-ww3m2,-WW3M)/!dtor ; 90- transforms into trig direction
         CASE var3.name OF
           'Fp2(k)':
             'mssx': BEGIN 
                  WW3M=WW3M+WW3M2
                  var3.name='mss'
                  END
              'utaw': BEGIN 
                  WW3M=sqrt(ww3m^2+ww3M2^2)
                  var3.name='taw'
                  END
              'utwo': BEGIN 
                  WW3M=sqrt(ww3m^2+ww3M2^2)
                  var3.name='two'
                  END
              ELSE: WW3M=sqrt(ww3m^2+ww3M2^2)
           ENDCASE

         IF (kount GT 0) THEN ww3dir(index)=-2.*maxval
      ENDIF

      ww3fieldname = var3.name+' ('+STRING(ww3unit)+')'
      PRINT,'Array:',ww3fieldname,size(ww3m)
      IF  STRPOS(var3.name,'th') GT -1 OR var3.name  EQ 'dir'  $
                OR var3.name  EQ 'dp ' OR ww3fieldname EQ '.pth' THEN BEGIN
         ww3dir=ww3m;
         ncid2=ncid
      ENDIF
      IF (newgrid EQ 1) THEN BEGIN 
        nxzmin=0
        nyzmin=0
        nxzmax=nx-1
        nyzmax=ny-1
      ENDIF

      CASE MAPLONLAT OF 
        0: BEGIN 
           dx=(rlonmax-rlonmin)*(1./((nx-1)*1000.))
           dy=(rlatmax-rlatmin)*(1./((ny-1)*1000.))
           END
        1: BEGIN 
           dx=(rlonmax-rlonmin)*(40000./((nx-1)*360.)) *cos(0.5*(rlatmin+rlatmax)*!dtor)
           dy=(rlatmax-rlatmin)*(40000./((ny-1)*360.))
print,'HEYMAA:',MAPLONLAT,dx,dy,nx,ny,rlatmin,rlatmax
           END
        2: BEGIN 
             SLAT = 70.
             RE = 6378.273
             E2 = .006693883
             PI = 3.141592654
             E =  sqrt(E2)
             la1=ww3lat(0,0)
             lo1=ww3lon(0,0)
             mapll,xmin,ymin,la1,lo1,1.
             mapll,xmax,ymax,ww3lat(nx-1,ny-1),ww3lon(nx-1,ny-1),1.
             XPOLE=-1.*xmin
             YPOLE=-1.*ymin
             dx=(xmax-xmin)*(1./((nx-1)))
             dy=(ymax-ymin)*(1./((ny-1)))
             END
      ENDCASE 

      rangex=[0,dx*FLOAT(nx-1)]
      rangey=[0,dy*FLOAT(ny-1)]
      WIDGET_CONTROL, Wdraw,SENSITIVE=1
      Wdraw_value_update=1
      dtindex=1
      ww3matrix=ww3m
      tindex=1L
      tindex2=ntime
      WIDGET_CONTROL,Wright(12,2),SET_SLIDER_MAX=ntime
      ; WIDGET_CONTROL,Wright(12,3),SET_SLIDER_MAX=1
      WIDGET_CONTROL,Wright(12,4),SET_SLIDER_MAX=ntime,SET_VALUE=ntime
      
      gridvarids=FLTARR(nvars)
      igrids=0
      FOR I=0,nvars-1 DO BEGIN
         ncvar=NCDF_VARINQ(ncid,I)
         ndimvar=ncvar.NDIMS
         FOR J=0,ndimvar-1 DO BEGIN 
            IF (ncvar.DIM(J) EQ timeid) THEN BEGIN 
               gridvarids(igrids)=J
               igrids=igrids+1
             ENDIF
         ENDFOR
      ENDFOR  
      ncivar=ivar
      END
;
; WWATCH ASCII output
;            
   20:BEGIN ;WAVEWATCH III transfer file (see WW3 manual) for IDLA=3
      IF n_elements(ww3fieldname) EQ 0 THEN ww3fieldname="    "
      ww3fieldname="    "
      ww3fieldname0=ww3fieldname
      ww3model='           '
      ww3date='        '
      ww3time='      '
      ww3scale=1.
      ww3unit='m'
      ww3check=0
      toto=0
      toto2=0
      totof="(1X,20I6)"
      nx=0L
      ny=0L
      line='                                                                                          '
      READF,unit,line
      ww3model=STRMID(line,1,13)
      ww3date=STRMID(line,15,9)
      ww3time=STRMID(line,24,7)
      pos1=STRPOS(line,'(')
      pos2=STRPOS(line,')')
      pos3=STRPOS(line,'.',STRLEN(line)-40,/REVERSE_SEARCH)
      taille=size(line)
      READS,STRMID(line,31,STRLEN(line)-31),rlonmin,rlonmax,nx, $
                                              rlatmin,rlatmax,ny
      READS,STRMID(line,pos3,STRLEN(line)-pos3),ww3fieldname,ww3scale, $
            ww3unit,toto,toto2,FORMAT='(A4,F8.2,1X,A10,2I2)'
         ;ww3fieldname,ww3scale,ww3unit,toto,toto2
      READS,STRMID(line,pos1,pos2-pos1+1), totof
      READS,STRMID(line,pos2+1,STRLEN(line)-pos2-1), ww3miss
      IF (POS3 LT 83) THEN BEGIN
;
; file coordinates are lat and lon
;
        MAPLONLAT=1
        IF(abs(rlonmax-rlonmin)+abs(rlatmax-rlatmin) GT 1000.) THEN BEGIN
          rlonmax=(rlonmax-rlonmin)/1000.*(360/40000.)
          rlonmin=0.
          rlatmax=(rlatmax-rlatmin)/1000.*(360/40000.)
          rlatmin=0.
          rlatmax=rlatmax*0.5
          rlatmin=-rlatmax
          ENDIF
        ww3lat=FINDGEN(ny)*(rlatmax-rlatmin)+rlatmin
        IF (rlonmin GE 180 AND rlonmax GE 180) THEN BEGIN
          rlonmin=rlonmin-360
          rlonmax=rlonmax-360
          ENDIF 

      ENDIF ELSE BEGIN
;
; file coordinates are X and Y
;
        MAPLONLAT=0
        dlon=(rlonmax-rlonmin)*360./4E7
      dlat=(rlatmax-rlatmin)*360./4E7
      rlonmin=0
      rlonmax=dlon
      rlatmin=0
      rlatmax=dlat
        ENDELSE
      

      ;   This format is for Wavewatch version 1.18
      ;   FORMAT='(1X,A13,I9.8,I7.6,2(2F8.2,I4),1X,A4,F8.4,1X,A10,2I2,1X,A11,I4)'
      ;   This format is for Wavewatch version 2.22
      ;   FORMAT='(1X,A13,I9.8,I7.6,2(2F8.2,I4),1X,A4,F8.2,1X,A10,2I2,1X,A11,I4)'
      
      ww3m=FLTARR(nx,ny)
      vectora=STRARR(nx)
      vector=FLTARR(nx)
      totofa=totof
      STRPUT,totofa,'A',6
      IF (ww3check EQ 1) THEN BEGIN
        FOR J=0,ny-1 DO BEGIN
          IF (toto2 EQ 2) THEN $
            READF,unit,vectora,FORMAT=totofa $
          ELSE READF,unit,vectora
          FOR K=0,nx-1 DO BEGIN
            IF vectora(K) EQ '****' THEN vector(K)=ww3miss $
                ELSE vector(K)=FLOAT(vectora(K))
            ENDFOR
          ww3m(*,ny-J-1)=vector
          ENDFOR
      ENDIF ELSE BEGIN
        FOR J=0,ny-1 DO BEGIN
         IF (toto2 EQ 2) THEN $
            READF,unit,vector,FORMAT=totofa $
            ELSE READF,unit,vector
            ww3m(*,ny-J-1)=vector
      ENDFOR
      ENDELSE
      Index=WHERE(ww3m LE ww3miss,kount)
      ww3m=ww3m*ww3scale
;
;  Reads a two component vector
;
      IF (ww3fieldname EQ '.cur') OR (ww3fieldname EQ '.wnd')$
       OR (ww3fieldname EQ '.uss') OR (ww3fieldname EQ '.tus') THEN BEGIN
         ww3m2=FLTARR(nx,ny)
      IF (ww3check EQ 1) THEN BEGIN
      FOR J=0,ny-1 DO BEGIN
         IF (toto2 EQ 2) THEN $
            READF,unit,vectora,FORMAT=totofa $
         ELSE READF,unit,vectora
         FOR K=0,nx-1 DO BEGIN
            IF vectora(K) EQ '****' THEN vector(K)=ww3miss $
                ELSE vector(K)=FLOAT(vectora(K))
         ENDFOR
         ww3m2(*,ny-J-1)=vector
      ENDFOR
      ENDIF ELSE BEGIN

         FOR J=0,ny-1 DO BEGIN
            IF (toto2 EQ 2) THEN $
               READF,unit,vector,FORMAT=totof $
            ELSE READF,unit,vector
            ww3m2(*,ny-J-1)=vector
         ENDFOR
      ENDELSE
         ww3m2=ww3m2*ww3scale
         ww3dir=90.-ATAN(-ww3m2,-WW3M)/!dtor ;90- transforms into trig direction
         WW3M=SQRT(ww3m^2+ww3M2^2)
         IF (kount GT 0) THEN ww3dir(index)=2.*maxval
      ENDIF
 ;
 ;
 ;
      IF (kount GT 0) THEN ww3m(index)=-2.*maxval
      IF  ww3fieldname EQ '.1th' OR ww3fieldname EQ '.dir' OR ww3fieldname EQ '.dp ' $
      OR ww3fieldname EQ '.pth' THEN BEGIN
         ww3dir=ww3m
         IF (ww3fieldname0 EQ '    ') THEN ww3matrix=ww3m
      ENDIF ELSE ww3matrix=ww3m
      CASE ww3fieldname OF
      '.dpt':ww3fieldname = 'Depth ('+STRCOMPRESS(ww3unit,/REMOVE_ALL)+')'
      '.hs ':ww3fieldname = 'Hs ('+STRCOMPRESS(ww3unit,/REMOVE_ALL)+')'
      '.1hs ':ww3fieldname = 'Hs swell 1 ('+STRCOMPRESS(ww3unit,/REMOVE_ALL)+')'
      '.phs':ww3fieldname = 'Hs partition('+STRCOMPRESS(ww3unit,/REMOVE_ALL)+')'
      '.hsw':ww3fieldname = 'Hs wind sea ('+STRCOMPRESS(ww3unit,/REMOVE_ALL)+')'
      '.fp ':ww3fieldname = 'Tp (s)'
      '.dir':IF (ww3fieldname0 EQ '    ') THEN ww3fieldname = 'Direction ('+STRCOMPRESS(ww3unit,/REMOVE_ALL)+')'
      '.dp' :IF (ww3fieldname0 EQ '    ') THEN ww3fieldname = 'Direction ('+STRCOMPRESS(ww3unit,/REMOVE_ALL)+')'
      '.wnd':ww3fieldname = 'Wind ('+STRCOMPRESS(ww3unit,/REMOVE_ALL)+')'
      '.cur':ww3fieldname = 'Courant ('+STRCOMPRESS(ww3unit,/REMOVE_ALL)+')'
      '.uss':ww3fieldname = 'Surface Stokes drift ('+STRCOMPRESS(ww3unit,/REMOVE_ALL)+')'
      ELSE:
      ENDCASE
      ; This is for structured grids only (unstructured: ny=1)
      IF (ny GT 1) THEN BEGIN
         nxzmin=0
         nyzmin=0
         nxzmax=nx-1
         nyzmax=ny-1
         dx=(rlonmax-rlonmin)*(40000./((nx-1)*360.))*cos(0.5*(rlatmin+rlatmax)*!dtor)
         dy=(rlatmax-rlatmin)*(40000./((ny-1)*360.))
         rangex=[0,dx*FLOAT(nx-1)]
         rangey=[0,dy*FLOAT(ny-1)]
         WIDGET_CONTROL, Wdraw,SENSITIVE=1
         Wdraw_value_update=1
      ENDIF ELSE BEGIN
         nxzmin=0
         nyzmin=0
         nx=FLOOR(sqrt(nx))
         ny=nx
         nxzmax=nx-1
         nyzmax=ny-1
         dx=(rlonmax-rlonmin)*(40000./((nx-1)*360.))*cos(0.5*(rlatmin+rlatmax)*!dtor)
         dy=(rlatmax-rlatmin)*(40000./((ny-1)*360.))
         rangex=[0,dx*FLOAT(nx-1)]
         rangey=[0,dy*FLOAT(ny-1)]
         WIDGET_CONTROL, Wdraw, SENSITIVE=0
         Wdraw_value_update=0
      ENDELSE
      END

   21:BEGIN ;Wavewatch III transfer file (see WW3 manual) for IDLA=3
      ww3matrixold=ww3matrix
      IF N_ELEMENTS(ww3dir) THEN ww3dirold=ww3dir
      tempopath=paths(20)
      paths(20)=paths(21)
      ReadDataFile,filenames(21),20,1
      paths(20)=tempopath
      PRINT,'name:',paths(21),filenames(21)
      ww3matrix2=ww3matrix
      IF N_ELEMENTS(ww3dir) THEN ww3dir2=ww3dir
      ww3matrix=ww3matrixold
      IF N_ELEMENTS(ww3dir) THEN ww3dir=ww3dirold
      END

;
   16:BEGIN    ;reads extra time series for spectral analysis vs bulk parameters
      reference_data_present=0
      IF N_ELEMENTS(modspec) NE 0 THEN BEGIN ; model time series output (file type 10)
        IF (modspec.ntime NE 0) THEN BEGIN
          reference_data=modspec
          reference_data_present=1
          ENDIF
      ENDIF
      IF N_ELEMENTS(obs) NE 0 THEN BEGIN ; model time series output (file type 10)
        IF (obs.ntime NE 0) THEN BEGIN
          reference_data=obs
          reference_data_present=1
          ENDIF
      ENDIF
      ;print,(*modspec.dates)[0].jday,modspec.ntime,FORMAT='(f16.4)'
      typ=0
      n1=0
      n2=0
      n3=0
      READF,unit,typ
      IF (typ EQ 0 AND reference_data_present NE 0) THEN BEGIN
      READF,unit,n1,n2,n3

      obs2={specdata, typ:0, nfband:0, nabin:0, np:0, nvar:0, ntime:0L, $
         nrun:1, f1:ptr_new(), f2:ptr_new(), f:ptr_new(), df:ptr_new(), $
         theta:ptr_new(),dtheta:1.0, $
         plat:ptr_new(), plon:ptr_new(), pdepth:ptr_new(), $
         pU10:ptr_new(), pUdir:ptr_new(), $
         pCurr:ptr_new(), pCurrdir:ptr_new(), $
         pnames:ptr_new(), varnames:ptr_new(), dates: ptr_new(), $
         data:ptr_new(), flags:ptr_new(), path:ptr_new(), filename:ptr_new()}
      d={date, y:0, m:0, d:0, h:0, minu:0, s:0.0, zone:0.0, jday:0.0D}
      obs2.typ=typ
      obs2.nfband=n1
      obs2.np=n2
      obs2.nvar=n3
      f1=FLTARR(n1)
      f2=f1
      READF,unit,f1,f2
      df=ABS(f2-f1)
      obs2.f1=ptr_new(f1)
      obs2.f2=ptr_new(f2)
      f=f1*0.
      f(0)=f1(0)
      f(n1-1)=f2(n1-1)
      FOR J=1,n1-2 DO BEGIN
        f(J)=f(J-1)+0.5*(df(J-1)+df(J))
      ENDFOR
      df=ABS(f2-f1)
      obs2.f=ptr_new(f)
      obs2.df=ptr_new(df)
      IF typ LE 1 THEN BEGIN
         obs2.nabin=1
         theta=FLTARR(1)
         obs2.theta=ptr_new(theta)
      ENDIF
      n=REPLICATE('        ',obs2.np)
      FMT='(001(A8))'
      STRPUT,FMT,STRING(obs2.np,FORMAT='(I3)'),1
      READF,unit,n,FORMAT=FMT
      obs2.pnames=ptr_new(n)
      STRPUT,FMT,STRING(obs2.nvar,FORMAT='(I3)'),1
      n=REPLICATE('        ',obs2.nvar)
      READF,unit,n,FORMAT=FMT
      obs2.varnames=ptr_new(n)
      print,'Variables:',*obs2.varnames
      I=0L
      y=0
      d=0
      h=0
      m=0
      minu=0
      zone=0.
      ntmax=900000L/(obs2.nabin*obs2.nfband)
      dar=REPLICATE({date},ntmax)
      Imax=reference_data.ntime-1
      data=FLTARR(1,obs2.np,obs2.nvar,obs2.nfband,obs2.nabin,ntmax)
         vecteur=FLTARR(obs2.nvar)
         over=0
         WHILE NOT EOF(unit) AND (over EQ 0) DO BEGIN
            READF,unit,y,m,d,h,minu,zone,vecteur
            ;IF (I LT 12) THEN print,'temps:',I,y,m,d,h,minu,zone,vecteur(7),vecteur(10)
            sec=0
            jday=JULDAY(m,d,y,h,minu,zone)
            IF (jday GE (*reference_data.dates)[0].jday) $
               AND (jday LE (*reference_data.dates)[Imax].jday) THEN BEGIN
               dar[i].y=y
               dar[i].d=d
               dar[i].m=m
               dar[i].h=h
               dar[i].minu=minu
               dar[i].s=zone ;sec
               dar[i].zone=0. ;zone
               dar[i].jday=jday
               data(0,0,*,0,0,I)=vecteur
               IF obs2.np GT 1 THEN BEGIN

                  FOR J=1,obs2.np-1 DO BEGIN
                     READF,unit,vecteur
                     data(0,J,*,0,0,I)=vecteur
                  ENDFOR
                  ;print,I,y,m,d,h,min,zone
               ENDIF
               I=I+1
            ENDIF ELSE BEGIN
               FOR J=1,obs2.np-1 DO BEGIN
                  READF,unit,vecteur
               ENDFOR
            ENDELSE
            IF (jday GT (*reference_data.dates)[Imax].jday) THEN over=1
         ENDWHILE

      obs2.ntime=I
      IF (I GT 0) THEN BEGIN
         obs2.data=ptr_new(data(*,*,*,*,*,0:I-1))
         obs2.dates=ptr_new(dar(0:I-1))
         flags=INTARR(I)+1
         obs2.flags=ptr_new(flags)
      ENDIF ELSE BEGIN
         PRINT,'No data in common time range.'
      ENDELSE
      PRINT,'Read ',I,' time steps.'
      ENDIF
      END


   18:BEGIN    ; reads a gshhs file
               ; (cf. www.ngdc.noaa.gov/mgg/shorelines/gshhs.html)
      tempoPoly=LONARR(600000,10)
      tempoPoint=FLTARR(20000000,2)
      ID=0L    ;identity number of polygon
      NP=0L    ; number of points in polygon
      LEV=0L   ; level (1 land, 2 lake, 3 island in lake ...)
      MW=0L
      ME=0L
      MS=0L
      MN=0L
      AREA=0L
      FLAG=0L
      X=0L
      y=0L
      tempoPoly(0,0)=0 ;pointer to tempoPoint
                       ; the points of polygon I are stored in
                       ;tempoPoint(tempoPoly(I,0):tempoPoly(I+1,0)-1)
      IC=0L
      ;PRINT,rlonmin,rlonmax,rlatmin,rlatmax
      marginx=0.
      marginy=(rlatmax-rlatmin)*0.2
      rlonminn=rlonmin
      IF ((rlonmax-rlonmin) LT 60) THEN BEGIN
        marginx=(rlonmax-rlonmin)*0.2
        rlonminn=rlonmin-6.*marginx
      ENDIF
      xc=(rlonmax+rlonmin)/2.
      yc=(rlatmax+rlatmin)/2.

      Outreduced=0

      ;GET_LUN,unit2
      ;OPENW,unit2,'traitcote_gshhs.dat'
      ;depthcote=-10.

      WHILE NOT EOF(unit) DO BEGIN
         READU,unit,ID,NP,LEV,MW,ME,MS,MN,AREA,FLAG ;reads the polygon header
         ;PRINT,'Np and bounds:',ID,NP,LEV,MW,ME,MS,MN
         VECTOR=LONARR(NP*2)
         READU,unit,VECTOR
         VECTOR=TRANSPOSE(REFORM(VECTOR,2,NP))
         tempoPoly(IC+1,0)=tempoPoly(IC,0)+NP
         tempoPoly(IC,1)=ID
         tempoPoly(IC,3)=LEV
         tempoPoly(IC,4)=MW
         tempoPoly(IC,5)=ME
         tempoPoly(IC,6)=MS
         tempoPoly(IC,7)=MN
         tempoPoly(IC,8)=AREA
         tempoPoly(IC,9)=FLAG
         ;FOR I=0L,NP-1 DO BEGIN
         ;  PRINTF,unit2,FORMAT='(2F11.6,F8.2)',VECTOR(I,0)*1.E-6-360.,vectOR(I,1)*1.E-6,depthcote
         ;ENDFOR

         IF (outreduced EQ 1) THEN BEGIN
            kountk=0L
            X=LONARR(NP)
            Y=X
            X(*)=VECTOR(*,0)
            Y(*)=VECTOR(*,1)
            Keep=WHERE(((((FLOAT(X)*1.E-6+180.) MOD 360.)  $
                           -rlonmax-marginx-180) LE 0.) AND $
                        ((((FLOAT(X)*1.E-6+180.) MOD 360.)  $
                           -rlonmin+marginx-180) GE 0.) AND $
                           ((FLOAT(Y)*1.E-6   $
                           -rlatmax-marginy) LE 0.) AND $
                        ((FLOAT(Y)*1.E-6   $
                           -rlatmin+marginy) GE 0.),kountk)
            NPO=NP-kountk
            NP=kountk
            IF kountk GT 0 THEN BEGIN
               tempoPoint(tempoPoly(IC,0):tempoPoly(IC,0)+NP-1,0:1)=VECTOR(Keep,*)
               IP1=KEEP(0)
               IP2=KEEP(kountk-1)
               ;PRINT,'IP1, IP2:',IP1,IP2
               ;PRINT,'KEEP:',KEEP
               x2=((VECTOR(IP2,0)*1.E-6+180.) MOD 360.)-180.
               y2=VECTOR(IP2,1)*1.E-6
               x1=((VECTOR(IP1,0)*1.E-6+180.) MOD 360.)-180.
               y1=VECTOR(IP1,1)*1.E-6
               PRINT,'IC:   ',IC,ID,NP,kountk,NPO,NPO+NP,IP1,IP2,x1,x2,y1,y2,xc,yc

               IF (NPO GT 0) THEN BEGIN
                          IF (ABS(x2-x1) GT ABS(rlonmax-rlonmin)) $
                                OR ABS(y2-y1) GT ABS(rlatmax-rlatmin) THEN BEGIN
                               a=(y1-y2)/(y2*x1-x2*y1)
                             b=(x2-x1)/(y2*x1-x2*y1)
                             c=1

                          IF (X2-X1)*(y2-y1) LT 0 THEN BEGIN
                             IF (a*xc+b*yc+c)*(a*rlonmax+b*rlatmax+c) GT 0 THEN BEGIN
                                 ;tempoPoint(tempoPoly(IC,0)+NP,0)=FLOAT(rlonmax+marginx+360.)*1.E6
                              ;tempoPoint(tempoPoly(IC,0)+NP,1)=FLOAT(rlatmax+marginy)*1.E6
                                    PRINT,'Adding point in polygon (1,1)...'
                          ENDIF ELSE BEGIN
                            ;tempoPoint(tempoPoly(IC,0)+NP,0)=FLOAT(rlonmin-marginx+360.)*1.E6
                            ;tempoPoint(tempoPoly(IC,0)+NP,1)=FLOAT(rlatmin-marginy)*1.E6
                                  PRINT,'Adding point in polygon (-1,-1)...'
                         ENDELSE
                  ENDIF ELSE BEGIN
                     IF (a*xc+b*yc+c)*(a*rlonmax+b*rlatmin+c) GT 0 THEN BEGIN
                                 tempoPoint(tempoPoly(IC,0)+NP,0)=FLOAT(rlonmin-marginx+360.)*1.E6
                              tempoPoint(tempoPoly(IC,0)+NP,1)=FLOAT(rlatmax+marginy)*1.E6
                                    PRINT,'Adding point in polygon (-1,1)...'
                          ENDIF ELSE BEGIN
                            tempoPoint(tempoPoly(IC,0)+NP,0)=FLOAT(rlonmax+marginx+360.)*1.E6
                            tempoPoint(tempoPoly(IC,0)+NP,1)=FLOAT(rlatmin-marginy)*1.E6
                                  PRINT,'Adding point in polygon (1,-1)...'
                         ENDELSE
                  ENDELSE
                  NP=NP+1
                  ENDIF
               ENDIF
               tempoPoly(IC,2)=NP
               tempoPoly(IC+1,0)=tempoPoly(IC,0)+NP
               IC=IC+1
            ENDIF
         ENDIF ELSE BEGIN

            IF (FLOAT(MS)*1.E-6 LT -80.) THEN BEGIN
                  NP=NP+1
                print,'Antarctica:',vectOR(0:1,0)
                tempo=vectOR
                  VECTOR=LONARR(NP,2)
                  vectOR(0,0)=FIX(rlonmax)*1000000
                  vectOR(0,1)=FIX(-90)*1000000
                        vectOR(NP-1,0)=FIX(rlonminn)*1000000
                  vectOR(NP-1,1)=FIX(-90)*1000000

                  VECTOR(1:NP-2,0:1)=tempo(0:np-3,0:1)
            ENDIF
            X=FLTARR(NP)
            X(*)=FLOAT(VECTOR(*,0)*1.E-6 - rlonminn)
            index=WHERE(X EQ 360.,kount)
            X(*)=(X  MOD 360.)
            IF kount GT 0 THEN X(index)=360.
            Index1=WHERE(X LT 0,kount1)
            IF kount1 GT 0 THEN X(Index1)=X(Index1)+360.

                  ; Looks for wrap around
            XP1=SHIFT(X,-1)
            Indexch=WHERE(ABS(X-XP1) GT 350.,kountch)
            IF ((rlonmax-rlonmin) GT 60) AND (kountch GT 1) THEN BEGIN
                  ;PRINT,'Flip:',IC,kountch,Indexch,X(Indexch),XP1(Indexch)
                  NP2=Indexch(0)+1+NP-1-Indexch(kountch-1)
                FOR Iwrap=1,kountch-2,2 DO BEGIN
                      NP2=NP2+Indexch(Iwrap+1)-Indexch(Iwrap)
                ENDFOR
                NP3=NP-NP2
                  VECTR=FLTARR(NP2,2)
                  VECTR3=FLTARR(NP3,2)
                  VECTR(0:Indexch(0),0)=X(0:Indexch(0))-marginx
                  VECTR(0:Indexch(0),1)=FLOAT(VECTOR(0:Indexch(0),1))*1.E-6-rlatmin
                Ip2=Indexch(0)+1
                Iwrap=0
                Ip3=0
                  FOR Iwrap=1,kountch-1,2 DO BEGIN
                   IF (Iwrap LT kountch-1) then BEGIN
                           VECTR(Ip2:Ip2+Indexch(Iwrap+1)-Indexch(Iwrap)-1,0) $
                                =X(Indexch(Iwrap)+1:Indexch(Iwrap+1))-marginx
                        VECTR(Ip2:Ip2+Indexch(Iwrap+1)-Indexch(Iwrap)-1,1) $
                                    =FLOAT(VECTOR(Indexch(Iwrap)+1:Indexch(Iwrap+1),1))*1.E-6-rlatmin
                      Ip2=Ip2+Indexch(Iwrap+1)-Indexch(Iwrap)
                   ENDIF
                   VECTR3(Ip3:Ip3+Indexch(Iwrap)-Indexch(Iwrap-1)-1,0) $
                               =X(Indexch(Iwrap-1)+1:Indexch(Iwrap))-marginx
                     VECTR3(Ip3:Ip3+Indexch(Iwrap)-Indexch(Iwrap-1)-1,1) $
                                 =FLOAT(VECTOR(Indexch(Iwrap-1)+1:Indexch(Iwrap),1))*1.E-6-rlatmin
                   Ip3=Ip3+Indexch(Iwrap)-Indexch(Iwrap-1)
                  ENDFOR
                  VECTR(Ip2:NP2-1,0)=X(Indexch(kountch-1)+1:NP-1)-marginx
                  VECTR(ip2:NP2-1,1)=FLOAT(VECTOR(Indexch(kountch-1)+1:NP-1,1))*1.E-6-rlatmin
                NPold=NP
                  NP=NP2

                IF (IP3 GT 3) THEN BEGIN
                   tempoPoint(tempoPoly(IC,0):tempoPoly(IC,0)+NP-1,0:1)=VECTR(*,*)
                     tempoPoly(IC,2)=NP
                         tempoPoly(IC+1,0)=tempoPoly(IC,0)+NP

                   IC=IC+1
                           NP=NP3
                           VECTR=VECTR3
                           tempoPoly(IC+1,0)=tempoPoly(IC,0)+NP
                     tempoPoly(IC,1)=ID
                     tempoPoly(IC,3)=LEV
                     tempoPoly(IC,4)=MW
                     tempoPoly(IC,5)=ME
                     tempoPoly(IC,6)=MS
                     tempoPoly(IC,7)=MN
                     tempoPoly(IC,8)=AREA
                     tempoPoly(IC,9)=FLAG
                  ENDIF

                  ENDIF ELSE BEGIN
                  VECTR=FLTARR(NP,2)
                  VECTR(*,0)=X(*)-6.*marginx
                  VECTR(*,1)=FLOAT(VECTOR(*,1))*1.E-6-rlatmin
              ENDELSE
            tempoPoint(tempoPoly(IC,0):tempoPoly(IC,0)+NP-1,0:1)=VECTR(*,*)
            tempoPoly(IC,2)=NP
            tempoPoly(IC+1,0)=tempoPoly(IC,0)+NP
            IC=IC+1
         ENDELSE
      ENDWHILE
      ;CLOSE,unit2
      ;FREE_LUN,unit2

      Npoly=IC
      GSHHSPoly=tempoPoly(0:Npoly,*)
      PRINT,'Number of relevant polygons:',IC,GSHHSPoly(Npoly),SIZE(tempoPoint)
      PRINT,'Bounds of poly 1:',tempoPoly(0,4:7)*1E-6


      GSHHSPoint=tempoPoint(0:GSHHSPoly(Npoly)-1,*)

;************************************************
; Outputs a reduced version of the original file
      IF (outreduced EQ 1) THEN BEGIN
      GET_LUN,unit2
      OPENW,unit2,'gshhs_cal.b'
         FOR I=0L,Npoly-1 DO BEGIN
           WRITEU,unit2,GSHHSPoly(I,1:9)
           NP=GSHHSPoly(I,2)
           AR=LONARR(NP,2)
           AR(*,*)=GSHHSPoint(GSHHSPoly(I,0):GSHHSPoly(I+1,0)-1,*)
           WRITEU,unit2,REFORM(TRANSPOSE(AR),NP*2)
         ENDFOR
      CLOSE,unit2
      FREE_LUN,unit2
      ENDIF
;************************************************
      ; COnverts to kilometers
      tempo=GSHHSPOINT
      GSHHSPoint=FLTARR(GSHHSPoly(Npoly),2)
      GSHHSPoint(*,0)=(nx-1)*dx*tempo(*,0)/ABS(rlonmax-rlonmin)
      GSHHSPoint(*,1)=(ny-1)*dy*tempo(*,1)/ABS(rlatmax-rlatmin)
      END

     22: BEGIN 
               message,'Loading table ' + filenames(filetype),/INFO
              r=BYTARR(256)
              g=BYTARR(256)
              b=BYTARR(256)
              READF,unit,r,g,b
              cbot = 0
              nc = !d.table_size - cbot
              if nc ne 256 then begin	;Interpolate
	            p = (lindgen(nc) * 255) / (nc-1)
	            r = r[p]
	            g = g[p]
	            b = b[p]
	            endif
              r_orig[cbot] = r
              g_orig[cbot] = g
              b_orig[cbot] = b
              r_curr = r_orig
              g_curr = g_orig
              b_curr = b_orig
              tvlct,r, g, b, cbot
        END
     ELSE:PRINT,'File type not supported'

   ENDCASE
      filestatus(filetype)=filestatus(filetype)-1
   IF filestatus(filetype) EQ 0 THEN CLOSE,unit
   datastatus(filetype)=1
   PRINT,'File type ',STRCOMPRESS(filetype),' read:',paths(filetype),filenames(filetype)
RETURN
END
;-----------------------------------------------------------------------------
PRO READ_CDF,COUNTS,OFFSETS,times,varnorm,vardir,varname
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
COMMON FREQ,    freq,nfband,findex,findex2
COMMON TIME,    timestep,tindex,tindex2,ntime,dtime,time0,day0,dtindex,nstep, $
                timezone_plot,timezone_string,months,time
COMMON WW3,     ww3model,ww3date,ww3time,ww3fieldname,ww3scale,ww3miss,ww3unit,ww3lon, $
               ww3lat,ww3matrix,ww3dir,ww3path,spec2D,ww3matrix2,ww3dir2,ncid,ncid2,ncivar0,ncivar
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON SPACE,   c_gp,c_cut,indexgp,c_x,c_y,c_lon,c_lat
       ncall=NCDF_INQUIRE(ncid)
       nvars=ncall.NVARS
       triid=NCDF_VARID(ncid,'tri') 
       MAPSTAid=NCDF_VARID(ncid,'MAPSTA') 
       timeid=NCDF_VARID(ncid,'time') 
       taille=SIZE(counts)
       NC=taille(1)
;
       ncvar=NCDF_VARINQ(ncid,ncivar)
       ndimvar=ncvar.NDIMS
;
       var3=NCDF_VARINQ(ncid,ncivar)
       attOK=NCDF_ATTINQ(ncid,ncivar,'scale_factor')
       IF (attOK.Length GT 0) THEN NCDF_ATTGET,ncid,ncivar,'scale_factor',ww3scale $
                             ELSE ww3scale=1.
       NCDF_ATTGET,ncid,ncivar,'_FillValue',ww3miss
       NCDF_ATTGET,ncid,ncivar,'units',ww3unit
 
       IF (timeid GE 0) THEN  $
           NCDF_VARGET,ncid,timeid,timenow,COUNT=COUNTS(NC-1),OFFSET=OFFSETS(NC-1) $
           ELSE timenow=0.
       dayi=JULDAY(1,1,1990,0,0,0.)
       times=DOUBLE(timenow)+DOUBLE(dayi)

       freqid=NCDF_VARID(ncid,'f') 
       IF (freqid GE 0) THEN BEGIN 
         NCDF_VARGET,ncid,freqid,freq
         TEMP=COUNTS
         COUNTS=LONARR(NC+1)
         COUNTS(0:NC-2)=TEMP(0:NC-2)
         COUNTS(NC-1)=1
         COUNTS(NC)=TEMP(NC-1)
         TEMP=OFFSETS
         OFFSETS=LONARR(NC+1)
         OFFSETS(0:NC-2)=TEMP(0:NC-2)
         OFFSETS(NC-1)=1
         OFFSETS(NC)=TEMP(NC-1)

         IF (NC EQ 3) THEN varnorm=FLTARR(COUNTS(0),COUNTS(1)) ELSE varnorm=FLTARR(COUNTS(0))
         vardir=varnorm
         lg10=ALOG(10)
;
; Reads for all frequencies and adds up
;
            fac=0.5*(1.1-1./1.1)
            varnorm=varnorm*0.
            mat2=FLOAT(varnorm)
            FOR J=findex-1,findex2-1 DO BEGIN
               OFFSETS(NC-1)=J
               NCDF_VARGET,ncid,ncivar,mat1,COUNT=COUNTS,OFFSET=OFFSETS
               IF (J EQ findex-1) THEN Index=WHERE(mat1 EQ ww3miss,kount)
               IF (kount GT 0) THEN mat1(Index)=0
               IF (J EQ findex-1) THEN Index2=WHERE(mat1 NE 0.,kount2)
              CASE STRING(ww3unit) OF
              'log10(m2s+1E-12)':BEGIN 
                 IF (kount2 GT 0) THEN $
                      mat2(Index2)=exp(lg10*FLOAT(mat1(Index2))*ww3scale)-(1E-12-1E-16)
                  varnorm=varnorm+mat2*freq(J)*fac
                  END
              'log10(m4s+0.01)':BEGIN 
                 IF (kount2 GT 0) THEN $
                      mat2(Index2)=exp(lg10*FLOAT(mat1(Index2))*ww3scale)-0.00999
                  varnorm=varnorm+mat2*freq(J)*fac
                  END
              'log10(m4s+1E-12':BEGIN 
                 IF (kount2 GT 0) THEN $
                      mat2(Index2)=exp(lg10*FLOAT(mat1(Index2))*ww3scale)-(1E-12-1E-16)
                  varnorm=varnorm+mat2*freq(J)*fac
                  END
               ELSE: BEGIN 
                  varnorm=varnorm+FLOAT(mat1*ww3scale)*freq(J)*fac
                   END
               ENDCASE
            ENDFOR

              CASE STRING(ww3unit) OF
              'log10(m4s+0.01)': ww3unit='m4'
              'log10(m2s+1E-12)': BEGIN 
                ww3unit='m'
                varnorm=4.*sqrt(varnorm)
                var3.name='Hs'
                Index2=WHERE(mat1 NE ww3miss,kount2)
                maxOK=MAX(varnorm(Index2))
                IF (maxOK LT 2.) THEN BEGIN 
                   varnorm=1000*varnorm
                   ww3unit='mm'
                   var3.name='HsIG'
                ENDIF
                END 
              ELSE:
                ENDCASE
            varname = var3.name+' ('+STRING(ww3unit)+')'
            varname = varname + ' (integrated from '+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                  +' to '+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
            IF (kount GT 0) THEN varnorm(index)=-2.*maxval
       ENDIF ELSE BEGIN 
;
; non-spectral data
;
           NCDF_VARGET,ncid,ncivar,varnorm,COUNT=COUNTS(0:ndimvar-1),OFFSET=OFFSETS(0:ndimvar-1)
           var3=NCDF_VARINQ(ncid,ncivar)
           varname = var3.name+' ('+STRING(ww3unit)+')'
           Index=WHERE(varnorm EQ ww3miss,kount)
           IF  STRPOS(var3.name,'fp') GT -1  THEN BEGIN 
              ;varnorm=varnorm*ww3scale
              varnorm=1./(varnorm*ww3scale ) 
              var3.name='Tp'
              print,'conversion of fp to tp... ',var3.name,MAX(varnorm)
           ENDIF ELSE varnorm=varnorm*ww3scale
           vardir=varnorm*0
           IF (kount GT 0) THEN varnorm(index)=-2.*maxval
;
; Computes norm from 2 components 
;
            IF (normvec EQ 1) THEN BEGIN 
              IF (nvars GT ncivar+1 AND (triid GE 0 OR ndimvar GT 2) ) THEN  BEGIN
                var4=NCDF_VARINQ(ncid,ncivar+1)
                firstchar=STRUPCASE(STRMID(var4.name,0,1))
                IF firstchar EQ 'V' or var4.name EQ 'mssy' THEN BEGIN 
                  NCDF_VARGET,ncid,ncivar+1,var2,COUNT=COUNTS,OFFSET=OFFSETS
                  var2=var2*ww3scale
; transforms into trig direction
                  vardir=90.-ATAN(-var2,-varnorm)/!dtor
                  IF (var3.name EQ 'mssx') THEN BEGIN 
                    bvarnorm=varnorm+var2 
                    varname='mss'
                  ENDIF ELSE BEGIN 
                     varnorm=SQRT(varnorm^2+var2^2)
                     varname = 'sqrt('+var3.name+'^2'+var4.name+'^2'  $ 
                                     +') ('+STRING(ww3unit)+')'
                  ENDELSE
                  IF (kount GT 0) THEN vardir(index)=2.*maxval
               ENDIF
              ENDIF
            ENDIF
            IF  STRPOS(var3.name,'th') GT -1 OR var3.name  EQ 'dir'  $
                     OR var3.name  EQ 'dp ' OR ww3fieldname EQ '.pth' THEN BEGIN
              vardir=varnorm
              ncid2=ncid
            ENDIF
         ENDELSE
         RETURN
         END
;-----------------------------------------------------------------------------
PRO UPDATE_WW3_TIME
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON FREQ,    freq,nfband,findex,findex2
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TIME,    timestep,tindex,tindex2,ntime,dtime,time0,day0,dtindex,nstep, $
                timezone_plot,timezone_string,months,time
COMMON WW3,     ww3model,ww3date,ww3time,ww3fieldname,ww3scale,ww3miss,ww3unit,ww3lon, $
                ww3lat,ww3matrix,ww3dir,ww3path,spec2D,ww3matrix2,ww3dir2,ncid,ncid2,ncivar0,ncivar

       indext=tindex-1
;
;  Updates the NetCDF WW3 data and redo the plot
;
       triid=NCDF_VARID(ncid,'tri') 
       ncvar=NCDF_VARINQ(ncid,ncivar)
       ndimvar=ncvar.NDIMS

       IF (triid EQ -1) THEN BEGIN 
          dimxid=NCDF_DIMID(ncid,'longitude')
          dimyid=NCDF_DIMID(ncid,'latitude')
          IF (dimxid EQ -1) THEN BEGIN 
            dimxid=NCDF_DIMID(ncid,'x')
            dimyid=NCDF_DIMID(ncid,'y')
          END
          IF (dimxid EQ -1) THEN BEGIN 
            dimxid=NCDF_DIMID(ncid,'lon')
            dimyid=NCDF_DIMID(ncid,'lat')
          END
          
          NCDF_DIMINQ,ncid,dimxid,name1,nx
          NCDF_DIMINQ,ncid,dimyid,name1,ny

          READ_CDF,[nx,ny,1],[0,0,indext],times,varnorm,vardir,varname
          ww3m=FLTARR(nx,ny)
          IF ( ndimvar GT 2) THEN ww3m=varnorm(*,*,0) ELSE ww3m=varnorm
          
          IF (MIN(vardir) LT MAX(vardir)) THEN BEGIN 
             ww3dir=FLTARR(nx,ny)
             ww3dir=vardir(*,*,0)
          ENDIF
       ENDIF ELSE BEGIN 
          READ_CDF,[nngp,1],  [0,indext],times,varnorm,vardir,varname 
          ww3m=FLTARR(nngp)
          ww3m(*)=varnorm(*,0)
          IF (MIN(vardir) LT MAX(vardir)) THEN BEGIN 
             ww3dir=FLTARR(nngp)
             ww3dir=vardir(*,0)
          ENDIF
       ENDELSE 
; 
PRINT,'MAX & MIN:',MAX(ww3m),MIN(ww3m)
       ww3matrix=ww3m
       ww3fieldname=varname
       dayi=JULDAY(1,1,1990,0,0,0.)

       Time1toDate,times(0),ww3date,ww3time
       RETURN
       END



;-----------------------------------------------------------------------------
PRO analyzer_event, ev
;+
; NAME: analyzer_event
; PURPOSE: handles events on the main window
; (i.e. action on a widget) as they occur
; CALLING SEQUENCE: analyzer_event, ev
; where ev is a structure event variable
; INPUT: ev=widget id of the base widget
; COMMON BLOCKS: datainfo,xyz,choices,drawsize,plotvar,widgets,datatype,
;                current,smallval,zoomidx,vecprm,prprm,psprm,dlprm,pmprm,
;                flags,rotprm,rotarr
;-

;** 1 ** Display parameters
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON TITLES,  font,outtit,outxtit,outytit,outcbtit,textx,texty,textdx,textdy
COMMON POSTSCRIPT, filep,pspath,prcoul,psor,pstype, $
                pwinx,pwiny,papierx,papiery,xoffset,yoffset, $
                facpolice,fontrescale,basefontsize,pssizex,pssizey,psfont
COMMON THREED,  Ax3D,Az3D,smoothing
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
COMMON WGene,Wgrideditor,WG,root
COMMON INTERFACE, TWISTVA_VERSION
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth

;** 2 ** Display and data variables/parameters
COMMON FREQ,    freq,nfband,findex,findex2
COMMON MAP,     MAPFLAG, MAPPROJ, MAPLONGCENTER, MAPLATCENTER, MAPCONTINENT,  $
                MAPCOUNTRIES, MAPLONLAT
COMMON DIR,     nabin,nabint,aindex,THETA
COMMON TIME,    timestep,tindex,tindex2,ntime,dtime,time0,day0,dtindex,nstep, $
                timezone_plot,timezone_string,months,time
COMMON TRANSECT,Ntrans,Strans,Xtrans,Ytrans,Ztrans,Itrans, $
                  COtrans,TransOK,transsym,transline,transthick,Ispectrans, $
                  spectransname,ntransgp,transsymsize
COMMON SPACE,   c_gp,c_cut,indexgp,c_x,c_y,c_lon,c_lat
COMMON OVERLAY, addir,adsyms,adbathy,adcoast,psyms,psymsizes,adtr,adtri
common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

;** 3 ** I/O and data variables
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON ARR8m,   nfTom,naTom,TomF,Tomth,OBS8m,MOD8m
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON COAST,   coastxy,coastl,coastnp,GSHHSPoly,GSHHSPoint,GSHHSPoly2,GSHHSPoint2
COMMON DEPTHSTAT,nbind,bindres,HISTD,MEAND,SDEVD
COMMON DIRSPEC, ds,dsnfband,ndir,kdkdth,dsfreq,dstheta,dsdf,dstime,dsntime,ds_timezone
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON GRIDPar, th1,th2,gfact,gsize1,gsize6
COMMON RAY3,    crestsnap,freq1,freq2,datar3,datar3D,datar4D,titr1,titr2, $
                nfields2D,nfields3D,nfields,timer3,dayr3
COMMON TIMESERIES, modspec,obs,obs2,om,ts_filetype, nbins, bulktype,scatvar
COMMON RAY3ad,  NGPad,SIad,SCad,DEPTHad,HSad,PSIad,BBLDad
COMMON RAYS,    raysOK,raystype,raynsteps,rayx,rayy,raya,rayamin,rayamax, $
                rayres,rayfreq,rayGP,raytimestep,rayoffdep,rayflag,raydz,raymindepth
COMMON SOURCE,  S0,snabin,sntstep
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
COMMON LOCAL, nxz1,nyz1,nxz2,nyz2
COMMON WW3,     ww3model,ww3date,ww3time,ww3fieldname,ww3scale,ww3miss,ww3unit,ww3lon, $
               ww3lat,ww3matrix,ww3dir,ww3path,spec2D,ww3matrix2,ww3dir2,ncid,ncid2,ncivar0,ncivar
COMMON HScalc,Hsc
;*******END OF COMMON BLOCKS*******************************

   type = TAG_NAMES(ev, /STRUCTURE)   ;obtain name of event structure
   CASE type OF
   'WIDGET_BUTTON': BEGIN             ;callback for the widget buttons
       WIDGET_CONTROL, ev.id, GET_VALUE=value
       CASE value OF
          'Norm': normvec = 1-normvec; 
          'Quit': BEGIN            ;Quit button
             taille=size(filestatus)
             FOR I=0,taille(1)-1 DO CLOSE,i+1   ;closes all files
             WIDGET_CONTROL, /DESTROY, ev.top   ;destroys main widget
             RETURN
             END
          'Palette': PALETTE,GROUP=ev.top    ;calls the palette
                                        ;which is a modified version
                                        ;of the std idl tool
                                        ;subwindow
          'PickNew': BEGIN
                     clickflag=0      ;this flag tells the draw window
                     WIDGET_CONTROL,Wdraw,EVENT_PRO='analyzer_event'
                     END
                                        ;that the next click will be to
                                        ;choose the current grid point
            'Refresh' :    doplot       ;does a plot with current param

            ;'Mesh' : IF (subwin(3) EQ 0) THEN IF (N_ELEMENTS(dx) NE 0) THEN Generator, ev.top     ;calls the grid builder
            'Mesh' : IF (N_ELEMENTS(dx) NE 0) THEN Generator, ev.top     ;calls the grid builder
            'Maps' :      Mapping, ev.top       ;calls the map tool window
            ;'Plot parameters' : IF (subwin(0) EQ 0) THEN Parametros, ev.top   ;calls the parameters
            'Plot parameters' : Parametros, ev.top   ;calls the parameters
            'Rays': Raytracemenu,ev.top     ;calls the ray tracing
;            'Transect tool': Maketransect,ev.top;calls the transect maker
            'Special points': Editspecials,ev.top
            'Bathy': Bathytool,ev.top
            'Zoom': BEGIN
               WIDGET_CONTROL,Wdraw,EVENT_PRO='analyzer_event'
               clickflag=1   ;zoom (corners of the zoom window
               END                   ;are given by clicking on drawing)
            'Zoom out':BEGIN      ;resets the zoom to full area
               clickflag=0   
               print,'ZOOM COORDINATES WAS:',nxzmin,nxzmax,nyzmin,nyzmax
               nxzmin=0
               nyzmin=0
               nxzmax=nx-1
               nyzmax=ny-1
               doplot
               END
            'Move': BEGIN
               WIDGET_CONTROL,Wdraw,EVENT_PRO='analyzer_event'
               clickflag=2   ;zoom (corners of the zoom window
               END                   ;are given by clicking on drawing)
            'EndMove': BEGIN
               WIDGET_CONTROL,Wdraw,EVENT_PRO='analyzer_event'
               clickflag=0   ;zoom (corners of the zoom window
               END                   ;are given by clicking on drawing)
            'Export spec2D':BEGIN
               GET_LUN,unitspec
               OPENW,unitspec,'../REFDIF/Spec2D.dat'
               taille=size(Spec2d)
               FOR I=0,taille1(1)-1 DO BEGIN

               PRINTF,unitspec,Spec2d(I,*)
               ENDFOR
               END
       ELSE: PRINT, 'Last Request Ignored'
       ENDCASE
    END
   'WIDGET_DRAW':BEGIN
      CASE ev.id OF
      Wdraw: IF MAPFLAG EQ 0 THEN BEGIN
        CASE ev.type OF     ;case loop on action types
        0:BEGIN             ;ev.type=0 : click (button pushed down)
           CASE clickflag OF
             0:IF (datatype EQ 4) THEN BEGIN 
                coordP=CONVERT_COORD(ev.X,ev.Y, /DEVICE,/TO_DATA)
                c_x=coordP(0)
                c_y=coordP(1)
                NEAREST_POINT
                IF datastatus(5) THEN BEGIN          ;changes the current grid point
                   OPLOT,[gridmat(0,c_gp-1)],[gridmat(1,c_gp-1)], $
                      psym=5,SYMSIZE=0.6
                   strlab=strcompress(string(c_gp),/REMOVE_ALL)
                   XYOUTS,gridmat(0,c_gp-1),gridmat(1,c_gp-1), $
                      strlab,CHARSIZE=0.9
                   WIDGET_CONTROL,Wright(11,1), SET_VALUE=STRCOMPRESS(STRING(c_gp),/REMOVE_ALL)
                ENDIF ELSE BEGIN 
                   OPLOT,[c_x],[c_y], psym=5,SYMSIZE=0.6
                ENDELSE
              ENDIF
   
             1:BEGIN
                coordP=CONVERT_COORD(ev.X,ev.Y, /DEVICE,/TO_DATA)
                nxz1=MIN([MAX([nxzmin,ROUND((coordP(0))/dx)]),nxzmax])
                nyz1=MIN([MAX([nyzmin,ROUND((coordP(1))/dy)]),nyzmax])
                END
             2:BEGIN
                coordP=CONVERT_COORD(ev.X,ev.Y, /DEVICE,/TO_DATA)
                nxz2=MIN([MAX([nxzmin,ROUND((coordP(0))/dx)]),nxzmax])
                nyz2=MIN([MAX([nyzmin,ROUND((coordP(1))/dy)]),nyzmax])
                END
             ENDCASE
             END
          1:BEGIN        ;ev.type=1  button release
             CASE clickflag OF
             1:BEGIN     ;picks up the second corner of the
                         ;zoom window when the button is released
                coordR=CONVERT_COORD(ev.X,ev.Y, /DEVICE,/TO_DATA)
                nxz2=MAX([MIN([nxzmax,ROUND((coordR(0))/dx)]),nxzmin])
                nyz2=MAX([MIN([nyzmax,ROUND((coordR(1))/dy)]),nyzmin])
                nx1=MIN([nxz1,nxz2])
                nx2=MAX([nxz1,nxz2])
                IF (nx1 EQ nx2) THEN BEGIN 
                   IF nx2 LT nxzmax THEN nx2=nx2+1 ELSE nx1=nx1-1
                ENDIF
                ny1=MIN([nyz1,nyz2])
                ny2=MAX([nyz1,nyz2])
                IF (ny1 EQ ny2) THEN BEGIN 
                   IF ny2 LT nyzmax THEN ny2=ny2+1 ELSE ny1=ny1-1
                ENDIF
                nxzmax=nx2
                nxzmin=nx1
                nyzmax=ny2
                nyzmin=ny1
                clickflag=0        ;resets the clickflag to zero
                ;PRINT,'Zoom indices:',nxzmin,nxzmax,nyzmin,nyzmax
                doplot
                END
             2:BEGIN     ;Moves zoom around when the button is released
                coordR=CONVERT_COORD(ev.X,ev.Y, /DEVICE,/TO_DATA)
                nxz1=MAX([MIN([nxzmax,ROUND((coordR(0))/dx)]),nxzmin])
                nyz1=MAX([MIN([nyzmax,ROUND((coordR(1))/dy)]),nyzmin])
                IF nxz2 GT nxz1 THEN BEGIN 
                  movex=MIN([nxz2-nxz1,nx-1-nxzmax])
                  nxzmax=nxzmax+movex
                  nxzmin=nxzmin+movex
                ENDIF ELSE BEGIN 
                  movex=MAX([nxz2-nxz1,-nxzmin])
                  nxzmax=nxzmax+movex
                  nxzmin=nxzmin+movex
                ENDELSE
                IF nyz2 GT nyz1 THEN BEGIN 
                  movey=MIN([nyz2-nyz1,ny-1-nyzmax])
                  nyzmax=nyzmax+movey
                  nyzmin=nyzmin+movey
                ENDIF ELSE BEGIN 
                  movey=MAX([nyz2-nyz1,-nyzmin])
                  nyzmax=nyzmax+movey
                  nyzmin=nyzmin+movey
                ENDELSE
                ;PRINT,'Zoom indices:',nxzmin,nxzmax,nyzmin,nyzmax
                doplot
                END
             ELSE:
             ENDCASE
             END                ;when the button is released

          ; Updates the cursor position and value of the current field
          2:IF Wdraw_value_update GT 0 THEN BEGIN
               IF datastatus (2) OR datastatus(20) THEN BEGIN  ;ev.type=2 when the cursor is in
                                          ;in the drawing window
                coord=CONVERT_COORD(ev.X,ev.Y, /DEVICE,/TO_DATA)
                i0=max([0,ROUND(coord(0)/dx)])
                j0=max([0,ROUND(coord(1)/dy)])
                IF ((i0 LT nx) AND (j0 LT ny)) THEN BEGIN
                   valeur=0.
                   CASE 1 OF
                   datastatus(3) EQ 1:BEGIN
                      valeur=gd(i0,j0)
                      WIDGET_CONTROL,Wright(14,5), $
                      SET_VALUE=STRING(valeur,FORMAT='(F7.2)')
                      END
                   datastatus(20) EQ 1:BEGIN
                      taille=size(ww3matrix)
                       IF (taille(0) GT 1) THEN BEGIN 
                          valeur=ww3matrix(i0,j0)
                          WIDGET_CONTROL,Wright(14,3), $
                          SET_VALUE=STRING(valeur,FORMAT='(F7.2)')
                       ENDIF
                    END
                    ELSE:
                 ENDCASE


                 ENDIF
                 WIDGET_CONTROL,Wright(14,1), $
                    SET_VALUE=(STRING(coord(0),FORMAT='(F8.2)')+','+STRING(coord(1),FORMAT='(F8.2)'))
                 ;XYstring='X:'+STRING(coord(0),FORMAT='(F8.3)') $
                 ;       +', Y:'+STRING(coord(1),FORMAT='(F.3)')
                  XYtoLatLon,coord(0),coord(1), $
                      latdeg,latmin,londeg,lonmin,latlonstring,latlonstring2
                  IF (MAPLONLAT GE 1) THEN WIDGET_CONTROL,Wright(15,1), $
                     SET_VALUE=(latlonstring+'/'+latlonstring2) ELSE $
                     WIDGET_CONTROL,Wright(15,1),SET_VALUE=''
                ENDIF
             ENDIF
          ELSE:BEGIN  ; Wheel action  
             IF (N_ELEMENTS(dx) NE 0 ) THEN BEGIN
                coordR=CONVERT_COORD(ev.X,ev.Y, /DEVICE,/TO_DATA)
                nxz1=MAX([MIN([nxzmax,ROUND((coordR(0))/dx)]),nxzmin])
                nyz1=MAX([MIN([nyzmax,ROUND((coordR(1))/dy)]),nyzmin])
                width=FLOAT(nxzmax-nxzmin)/2.
                height=FLOAT(nyzmax-nyzmin)/2.
                IF (ev.CLICKS EQ 1) THEN BEGIN 
                   width=width/1.5
                   height=height/1.5
                ENDIF ELSE BEGIN 
                   width=width*1.5
                   height=height*1.5
                ENDELSE
                nxzmax=MIN([nx-1,nxz1+ROUND(width)])
                nxzmin=MAX([0   ,nxz1-ROUND(width)])
                nyzmax=MIN([ny-1,nyz1+ROUND(height)])
                nyzmin=MAX([0   ,nyz1-ROUND(height)])
                doplot
             ENDIF
                END
          ENDCASE ; ev.type
          END
      ENDCASE  ; end of case test on ev.id
      END
   'WIDGET_DROPLIST':BEGIN
      WIDGET_CONTROL, Wdraw, SENSITIVE=0
      CASE ev.id of
      Wright(0,0):BEGIN
         CASE ev.index of
            ;0:BEGIN
            ;      datatype=0
            ;      WIDGET_CONTROL,Wright(1,2),SET_VALUE=['2D','Trans','Local']
            ;   END
            0:IF N_ELEMENTS(modspec) NE 0 THEN BEGIN
                  WIDGET_CONTROL,Wright(1,1),SET_VALUE=STRCOMPRESS(*modspec.varnames)
                  WIDGET_CONTROL,Wright(11,3),SET_VALUE=STRCOMPRESS(*modspec.pnames)

                  IF (modspec.nabin GT 1) THEN WIDGET_CONTROL,Wright(1,2),SET_VALUE= $
                       ['TS Bulk','Time series','f-spectrum', $
                     'Averaged f-spectrum','f-time plot','f-theta spec'] $ 
                  ELSE IF (modspec.nfband GT 1) THEN  WIDGET_CONTROL,Wright(1,2),SET_VALUE= $
                       ['TS Bulk','Time series','f-spectrum','time-average', $
                        'freq-time','Diff vs fp','????', $
                        'Diff vs val','Binned plot','var U10,Hs'] $
                  ELSE WIDGET_CONTROL,Wright(1,2),SET_VALUE= $
                     ['Time series'] 

                  IF modspec.nfband GT 1 THEN WIDGET_CONTROL,Wright(1,3), $
                     SET_VALUE=['bulk=Hs','bulk=mss','bulk=Uss','bulk=Ussd','bulk=varu','bulk=alpha','bulk=T01','bulk=T02']
                  WIDGET_CONTROL,Wright(12,0),SET_SLIDER_MAX=modspec.nfband
                  WIDGET_CONTROL,Wright(12,5),SET_SLIDER_MAX=modspec.nfband
                  findex2=min([modspec.nfband,findex2])
                  findex=min([modspec.nfband,findex])
                  IF (findex2 LE findex) THEN findex=findex2-1
                  IF (findex LT 1) THEN findex=1

                  WIDGET_CONTROL,Wright(12,2),SET_SLIDER_MAX=modspec.ntime
                  WIDGET_CONTROL,Wright(12,4),SET_SLIDER_MAX=modspec.ntime
                  IF tindex2 GT modspec.ntime THEN tindex2=modspec.ntime
                  IF tindex GT modspec.ntime THEN tindex=modspec.ntime
                  IF findex2 GT modspec.nfband THEN findex2=modspec.nfband
                  IF findex GT modspec.nfband THEN findex=modspec.nfband
                  IF (findex2 LE findex) THEN findex=findex2-1
                  IF (findex LT 1) THEN findex=1

                  freq=(*modspec.f)
                  nfband=N_ELEMENTS(freq)
                  time=(*modspec.dates)
                  datatype=2
               ENDIF
            1:IF N_ELEMENTS(obs) NE 0 THEN BEGIN
                  WIDGET_CONTROL,Wright(1,1),SET_VALUE=STRCOMPRESS(*obs.varnames)
                  WIDGET_CONTROL,Wright(0,2),SET_VALUE=STRCOMPRESS(*obs.pnames)
                  WIDGET_CONTROL,Wright(11,3),SET_VALUE=STRCOMPRESS(*obs.pnames)
                  IF (obs.nabin GT 1) THEN WIDGET_CONTROL,Wright(1,2),SET_VALUE= $
                       ['TS Bulk','Time series','f-spectrum', $
                     'Averaged f-spectrum','f-time plot','f-theta spec'] $ 
                  ELSE IF (obs.nfband GT 1) THEN  WIDGET_CONTROL,Wright(1,2),SET_VALUE= $
                       ['TS Bulk','Time series','f-spectrum','Averaged f-spectrum', $
                        'scatter plot','Diff vs fp','Q-Q plot', $
                        'Diff vs val','Binned plot','var U10,Hs'] $
                  ELSE WIDGET_CONTROL,Wright(1,2),SET_VALUE= $
                     ['Time series'] 

                  WIDGET_CONTROL,Wright(12,0),SET_SLIDER_MAX=obs.nfband
                  WIDGET_CONTROL,Wright(12,5),SET_SLIDER_MAX=obs.nfband
                  findex2=min([obs.nfband,findex2])
                  findex=min([obs.nfband,findex])
                  nfband=obs.nfband
                  IF obs.nfband GT 1 THEN WIDGET_CONTROL,Wright(1,3), $
                     SET_VALUE=['bulk=Hs','bulk=mss','bulk=Ussnd','bulk=Uss','bulk=alpha','bulk=varu','bulk=T01','bulk=T02']
                
                  freq=(*obs.f)
                  nfband=N_ELEMENTS(freq)
                  time=(*obs.dates)
                  WIDGET_CONTROL,Wright(12,2),SET_SLIDER_MAX=obs.ntime
                  WIDGET_CONTROL,Wright(12,4),SET_SLIDER_MAX=obs.ntime
                  IF tindex2 GT obs.ntime THEN tindex2=obs.ntime
                  IF tindex GT obs.ntime THEN tindex=obs.ntime
                  IF findex2 GT obs.nfband THEN findex2=obs.nfband
                  IF findex GT obs.nfband THEN findex=obs.nfband
                  datatype=1

               ENDIF
            2: IF ((N_ELEMENTS(modspec) NE 0) AND (N_ELEMENTS(obs) NE 0)) THEN BEGIN
                  ;for comparisons: determines common variables, frequencies ...
                  ;The following structures contains arrays of indices for the arrays of
                  ;observations and model output BUT NOT THE DATA ITSELF !
                  om={commons, typ:0, nfband:0, nabin:0, np:0, nvar:0, ntime:0L, $
                     fo:ptr_new(), po:ptr_new(), vo:ptr_new(), dateo: ptr_new(), $
                     fm:ptr_new(), pm:ptr_new(), vm:ptr_new(), datem: ptr_new()}
                  nfmax=MAX([modspec.nfband,obs.nfband])
                  npmax=MAX([modspec.np,obs.np])
                  nvmax=MAX([modspec.nvar,obs.nvar])
                  ntimemax=MAX([modspec.ntime,obs.ntime])
                  fo=INTARR(nfmax)
                  fm=INTARR(nfmax)
                  po=INTARR(npmax)
                  pm=INTARR(npmax)
                  vo=INTARR(nvmax)
                  vm=INTARR(nvmax)
                  dateo=LONARR(ntimemax)
                  datem=LONARR(ntimemax)
;
; Finds common variables
;
                  nvarc=0
                  FOR I=0,obs.nvar-1 DO BEGIN
                     FOR J=0,modspec.nvar-1  DO BEGIN
                        IF (strcompress((*obs.varnames)[I],/REMOVE_ALL) EQ $
                           strcompress((*modspec.varnames)[J],/REMOVE_ALL)) THEN BEGIN
                           vo(nvarc)=I
                           vm(nvarc)=J
                           nvarc=nvarc+1
                        ENDIF
                     ENDFOR
                  ENDFOR
;
; Finds common points
;
                  IF (obs.np EQ 1 AND modspec.np EQ 1 ) THEN BEGIN 
                    npc=1
                    po(0)=0
                    pm(0)=0
                  ENDIF ELSE BEGIN 
                    npc=0
                    FOR I=0,obs.np-1 DO BEGIN
                       FOR J=0,modspec.np-1  DO BEGIN
                          IF (strcompress((*obs.pnames)[I],/REMOVE_ALL) $
                              EQ strcompress((*modspec.pnames)[J],/REMOVE_ALL)) THEN BEGIN
                             po(npc)=I
                             pm(npc)=J
                             npc=npc+1
                          ENDIF
                       ENDFOR
                    ENDFOR
                  ENDELSE
;
; Finds common frequencies
;
                  nfc=0
                  IF (obs.typ GT 0) THEN BEGIN
                     FOR I=0,obs.nfband-1 DO BEGIN
                     FOR J=0,modspec.nfband-1  DO BEGIN
                        IF (ABS((*obs.f)[I]-(*modspec.f)[J]) $
                           LT 0.2*((*obs.df)[I]+(*modspec.df)[J]) AND I LT obs.nfband) THEN BEGIN
                           ;print,'commons :',I,J,(*obs.f)[I],(*modspec.f)[J],(*obs.df)[I],(*modspec.df)[J]
                           fo(nfc)=I
                           fm(nfc)=J
                           IF (nfc LT nfmax-1) THEN nfc=nfc+1
                        ENDIF
                     ENDFOR
                     ENDFOR
                  ENDIF ELSE BEGIN
                     nfc=1
                     fo(0)=0
                     fm(0)=0
                  ENDELSE
                  ntimec=0L
                  I=0L
                  J=0L
                  dtm=abs((*modspec.dates)[2].jday-(*modspec.dates)[1].jday)
                  WHILE (J LT modspec.ntime) DO BEGIN
 ;
; Finds common times
;
                    found=0
                     WHILE (found EQ 0) AND (I LT obs.ntime) AND (J LT modspec.ntime) DO BEGIN
                        t1=((*obs.dates)[I].jday-(*obs.dates)[I].zone/24.0d)
                        t2=((*modspec.dates)[J].jday-(*modspec.dates)[J].zone/24.0d)
                        IF (ABS(t1 -t2) LT 0.3*dtm) THEN BEGIN
                           dateo(ntimec)=I
                           datem(ntimec)=J
                           ntimec=ntimec+1
                           I=I+1
                           J=J+1
                           found=1
                        ENDIF ELSE BEGIN
                        IF ((t1 GT t2) OR (I EQ obs.ntime)) THEN BEGIN
                         J=J+1
                        ENDIF ELSE BEGIN
                         I=I+1
                        ENDELSE
                        IF (J EQ modspec.ntime) THEN I=I+1
                        ENDELSE
                     ENDWHILE
                     IF (I EQ obs.ntime) THEN J=J+1
                  ENDWHILE
                  om.nvar=nvarc
                  om.nfband=nfc
                  om.np=npc
                  om.ntime=ntimec
                  print,'commons :',npc,nvarc,nfc,ntimec
                  WIDGET_CONTROL,Wright(12,5),SET_SLIDER_MAX=nfc
                  findex2=max(nfc,findex2)
                  IF (nvarc*nfc*npc*ntimec NE 0) THEN BEGIN
                     om.vm=PTR_NEW(vm(0:nvarc-1))
                     om.vo=PTR_NEW(vo(0:nvarc-1))
                     om.fm=PTR_NEW(fm(0:nfc-1))
                     om.fo=PTR_NEW(fo(0:nfc-1))
                     om.pm=PTR_NEW(pm(0:npc-1))
                     om.po=PTR_NEW(po(0:npc-1))
                     om.datem=PTR_NEW(datem(0:ntimec-1))
                     om.dateo=PTR_NEW(dateo(0:ntimec-1))
                     WIDGET_CONTROL,Wright(1,1),SET_VALUE=STRCOMPRESS((*obs.varnames)[*om.vo])
                     WIDGET_CONTROL,Wright(11,3),SET_VALUE=STRCOMPRESS((*obs.pnames)[*om.po])
                     
                  IF (obs.nabin GT 1) THEN WIDGET_CONTROL,Wright(1,2),SET_VALUE= $
                       ['TS Bulk','Time series','f-spectrum', $
                     'Averaged f-spectrum','f-time plot','f-theta spec'] $ 
                  ELSE IF (nfc GT 1) THEN  WIDGET_CONTROL,Wright(1,2),SET_VALUE= $
                       ['TS Bulk','Time series','f-spectrum','Averaged f-spectrum', $
                        'scatter plot','Diff vs fp','Q-Q plot', $
                        'Diff vs val','Binned plot','var U10,Hs'] $
                  ELSE WIDGET_CONTROL,Wright(1,2),SET_VALUE= $
                     ['Time series',' ',' ',' ','scatter plot','Q-Q plot','binned plot','var U10,Hs'] 
                     ; Adjusts the time sliders range to avoid errors
                     WIDGET_CONTROL,Wright(12,2),SET_SLIDER_MAX=om.ntime
                     WIDGET_CONTROL,Wright(12,4), $
                        SET_SLIDER_MAX=om.ntime,SET_VALUE=om.ntime
                     freq=(*modspec.f)[(*om.fm)]
                     time=(*modspec.dates)[(*om.datem)]
                     nfband=om.nfband
                     WIDGET_CONTROL,Wright(12,0),SET_SLIDER_MAX=om.nfband
                     WIDGET_CONTROL,Wright(12,5),SET_SLIDER_MAX=om.nfband
                     IF om.nfband GT 1 THEN WIDGET_CONTROL,Wright(1,3), $
                      SET_VALUE=['bulk=Hs','bulk=mss','bulk=Uss','bulk=Ussd','bulk=varu','bulk=alpha','bulk=T01','bulk=T02']
                     WIDGET_CONTROL,Wright(12,5), $
                        SET_SLIDER_MAX=om.nfband,SET_VALUE=om.nfband
                     IF findex > nfband THEN findex=1
                     IF findex2 > nfband THEN findex2=nfband

                     IF tindex2 GT ntimec THEN tindex2=ntimec
                     print,'tindex2:',tindex2,ntimeC
                  ENDIF ELSE BEGIN
                     print,'No data in common'
                     om.dateo=PTR_NEW(!NULL)
                  ENDELSE
                  datatype=3
               ENDIF
            ENDCASE
         END


      Wright(5,1): BEGIN
         filltype=ev.index
         doplot
         END
      Wtoprow(1) : BEGIN      ;file droplist
         IF (ev.index GT 0) THEN BEGIN 
         file=DIALOG_PICKFILE(/READ, $
            PATH = PATHS(ev.index),FILTER = FILTERS(ev.index))
         IF filestatus(ev.index) THEN CLOSE,ev.index+1
         IF file NE '' THEN BEGIN
            IF !VERSION.OS_FAMILY EQ 'Windows' THEN $
               spos=STRPOS(file,'\',/REVERSE_SEARCH) $
            ELSE spos=STRPOS(file,'/',/REVERSE_SEARCH)
            PATHS(ev.index)=STRMID(file,0,spos+1)
            filenopath=STRMID(file,spos+1,STRLEN(file)-spos-1)
            filenames(ev.index)=filenopath
            ReadDataFile,filenopath,ev.index,1
         ENDIF
         ENDIF
         END
      Wright(2,1):BEGIN
         datatype=4
         plotncvar=ev.index
         doplot
         END
      Wright(1,1):BEGIN
         ;plottypeold=plottype
         plottype=ev.index
         IF (datatype EQ 4 )  THEN BEGIN 
            ncivar=ncivar0+ev.index
            UPDATE_WW3_TIME
         ENDIF
         ;plottype=plottypeold
         doplot
         END
      Wright(1,3):BEGIN
         bulktype=ev.index
         doplot
         END
      Wright(1,2):c_cut=ev.index
      Wright(3,0):addir=ev.index
      Wright(3,1):adsyms=ev.index
      Wright(3,2):adbathy=ev.index
      Wright(4,0):adcoast=1-ev.index
      Wright(4,1):adtr=1-ev.index
      Wright(4,2):adtri=ev.index
      Wright(4,3):adcontour=ev.index
      Wright(6,1):IF ev.index GT 0 THEN BEGIN
            filtersps=['*.ps','*.ps','*.ps','*.eps','*.png','*.jpg', $
        '*.tif','*.gif','*.mpg']
        print,size(filtersps)
            msgs=['Sending to file ','Sending job to las1r2 ', $
               'Sending job to col1r1 ','Saving as EPS file to ', $
               'Saving as png file to ','Saving as JPEG file to ', $
               'Saving as TIFF file to ','Saving as GIF file to ', $
           'Saving as MPEG file to ']
            CASE ev.index OF
               2:filep='bw.ps'
               3:filep='color.ps'
               ELSE:filep=DIALOG_PICKFILE(/WRITE,PATH=pspath,FILTER=filtersps(ev.index-1))
            ENDCASE
            IF filep NE '' THEN BEGIN
               IF !VERSION.OS_FAMILY EQ 'Windows' THEN $
               spos=RSTRPOS(filep,'\') $
               ;spos=STRPOS(filep,'\',/REVERSE_SEARCH) $
               ELSE spos=RSTRPOS(filep,'/')
               ;ELSE spos=STRPOS(filep,'/',/REVERSE_SEARCH)
               pspath=STRMID(filep,0,spos+1)
               print,msgs(ev.index-1),filep

               IF (ev.index LE 4) THEN BEGIN
                  IF ev.index EQ 4 THEN pstype=1 ELSE pstype=0
                  mydevice= !D.NAME
                  set_plot,'ps'
                  DEVICE,LANGUAGE_LEVEL=2
                  output=1
                  !p.thick=1.0
                  print,'mydevice:',mydevice
                  doplot
                  DEVICE,/CLOSE_FILE
                  set_plot, mydevice
                  IF ev.index EQ 2 THEN SPAWN,'lp -dlas1r2 bw.ps'
                  IF ev.index EQ 3 THEN SPAWN,'lp -dcol1r1 color.ps'
                  print, 'Done'
                  output=0
               ENDIF ELSE BEGIN
               CASE ev.index OF
                 5: BEGIN
                  image=TVRD(TRUE=1)
                 taille=size(image)
                 ymax=MIN([taille(3)-1,taille(3)*winy/mwiny+20])
                  WRITE_PNG,FILEP,image(*,0:taille(2)*winx/mwinx-1,0:ymax),r_curr,g_curr,b_curr
                  END
                 6: BEGIN
                  image=TVRD(TRUE=1)
                  taille=size(image)
                   WRITE_JPEG,FILEP,image,TRUE=1,QUALITY=100
                  END
                 7: BEGIN
                  image=TVRD(TRUE=1)
                  taille=size(image)
                   WRITE_TIFF,FILEP,image,COMPRESSION=0,/SHORT
                  END
                 8: BEGIN
                  image=TVRD()
                  taille=size(image)
                   print,'Writing GIF file of size:',taille
                   WRITE_GIF,FILEP,image,r_curr,g_curr,b_curr ;,/MULTIPLE
                   ;WRITE_GIF,FILEP,/CLOSE
                  END
                 9: BEGIN 
                    image=TVRD(TRUE=1)
                    taille=size(image)
                    mpegID = MPEG_OPEN([taille(1),taille(2)]) 
                    FOR it=tindex,ntime DO BEGIN
                       indext=it-1
                       UPDATE_WW3_TIME
                       doplot
                       image=TVRD(TRUE=1)
                       taille=size(image)
                       iframe=tindex-it 
                       MPEG_PUT, mpegID, IMAGE=image, FRAME=iframe
                    ENDFOR
                    MPEG_SAVE, mpegID, FILENAME=FILEP 
                    MPEG_CLOSE, mpegID 
                    indext=tindex-1
                    END
                  ENDCASE
               ENDELSE
            ENDIF
         ENDIF ELSE BEGIN
            ANNOTATE
         ENDELSE
;
;  Extract time series from NetCDF file
;
      Wright(6,2):IF  datastatus(17) THEN BEGIN
         NEAREST_POINT
         IF (ev.index) LT 4 THEN BEGIN 
         ts_filetype=1
         typ=0
         modspec={specdata, typ:0, nfband:0, nabin:0, np:0, nvar:0, ntime:0L, $
         nrun:1, f1:ptr_new(), f2:ptr_new(), f:ptr_new(), df:ptr_new(), $
         theta:ptr_new(),dtheta:1.0, $
         plat:ptr_new(), plon:ptr_new(), pdepth:ptr_new(), $
         pU10:ptr_new(), pUdir:ptr_new(), $
         pCurr:ptr_new(), pCurrdir:ptr_new(), $
         pnames:ptr_new(), varnames:ptr_new(), dates: ptr_new(), $
         data:ptr_new(), flags:ptr_new(), path:ptr_new(), filename:ptr_new()}
         d={date, y:0, m:0, d:0, h:0, minu:0, s:0.0, zone:0.0, jday:0.0D}
         

         modspec.typ=typ
         modspec.nfband=1
         modspec.nabin=1
         modspec.np=1
         modspec.nvar=1
         modspec.nrun=1
         dimtid=NCDF_DIMID(ncid,'time')
         NFILES=1
         triid=NCDF_VARID(ncid,'tri') 
         IF (triid EQ -1) THEN BEGIN 
            c_i=ROUND(c_x/dx)
            c_j=ROUND(c_y/dy)
            XYtoLatLon,c_x,c_y,latdeg,latmin,londeg,lonmin,latlonstring,latlonstring2       
         ENDIF ELSE XYtoLatLon,gridmat(0,c_gp),gridmat(1,c_gp),latdeg,latmin, $
              londeg,lonmin, latlonstring,latlonstring2       

         varn=STRARR(modspec.nvar)
         varn='data from NC'
         modspec.varnames=ptr_new(varn)

         varp=STRARR(modspec.np)
         varp='Point'
         modspec.pnames=ptr_new(varp)
         temp=fltarr(1)
         modspec.f=ptr_new(temp)
         modspec.theta=ptr_new(temp)
         modspec.df=ptr_new(temp)

         IF (ev.index) EQ 2 OR (ev.index) EQ 3 THEN BEGIN
           myfile=filenames(17)
           lasti = STRPOS(myfile, '_', /REVERSE_SEARCH)
           PATTERN='*'+STRMID(myfile,lasti+1) 
           FILESOK=FILE_SEARCH(paths(17), PATTERN) 
           NN=SIZE(FILESOK)
           NFILES=NN(1)
           NCDF_CLOSE,ncid
           ncid=NCDF_OPEN(FILESOK(0),/NOWRITE)
         ENDIF 

         ntt=0
         FOR IFILE=0,NFILES-1 DO BEGIN 
           NCDF_DIMINQ,ncid,dimtid,timename,nt
           IF (triid EQ -1) THEN BEGIN 
             READ_CDF,[1,1,nt],[c_i,c_j,0],times1,varnorm,vardir,varname
           ENDIF ELSE BEGIN 
              READ_CDF,[1,nt],[C_gp-1,0],times1,varnorm,vardir,varname
           ENDELSE 
           ntt=ntt+nt
           data=FLTARR(1,modspec.np,modspec.nvar,modspec.nfband,modspec.nabin,ntt)
           times=DBLARR(ntt)
           IF (ntt GT nt) THEN BEGIN 
             data(0,0,0,0,0,0:ntt-nt-1)=dataold
             times(0:ntt-nt-1)=timesold
           ENDIF
           data(0,0,0,0,0,ntt-nt:ntt-1)=varnorm
           times(ntt-nt:ntt-1)=times1
           dataold=data
           timesold=times
           IF (NFILES GT 1) THEN BEGIN 
             NCDF_CLOSE,ncid
             IF (IFILE LT NFILES-1) THEN NEWFILE = FILESOK(IFILE+1) ELSE $ 
                  NEWFILE = paths(17)+filenames(17) 
             ncid=NCDF_OPEN(NEWFILE,/NOWRITE) 
             message,'Reading ' + NEWFILE,/INFO
           ENDIF
         ENDFOR
         

         modspec.ntime=ntt

         temp=INTARR(ntt)+1
         modspec.flags=ptr_new(temp)

         modspec.data=ptr_new(data)
         d={date, y:0, m:0, d:0, h:0, minu:0, s:0.0, zone:0.0, jday:0.0D}
         dar=REPLICATE({date},ntt)

         FOR I=0,ntt-1 DO BEGIN 
            CALDAT,times(i),m,d,y,h,minu,sec
            dar[i].y=y
            dar[i].d=d
            dar[i].m=m
            dar[i].h=h
            dar[i].minu=minu
            dar[i].s=sec
            dar[i].zone=0. ;zone
            dar[i].jday=times(i)
         ENDFOR
         modspec.dates=ptr_new(dar(0:I-1))
         datastatus(10)=1
         c_cut=0
         ENDIF
;
; Writes time series to file
;
         IF (ev.index) EQ 2 OR (ev.index) EQ 3 THEN BEGIN
            GET_LUN,unit
            OPENW,unit,'series_from_NC.txt'
            PRINTF,unit,'Time series for point:',latlonstring,' // ',latlonstring2
            FOR I=0,ntt-1 DO BEGIN 
		         PRINTF,unit,dar[i].y,dar[i].m,dar[i].d,dar[i].h,dar[i].minu, $
                    dar[i].s,data(0,0,0,0,0,i), $
                    FORMAT='(I4,X,I2,X,I2,X,I2,X,I2,X,F3.1,X,F13.4)'
            ENDFOR
            CLOSE,unit
            free_lun,unit
         ENDIF 
;
         IF (ev.index) EQ 4 THEN  BEGIN
            triid=NCDF_VARID(ncid,'tri') 
;
; Gets date strings for first time step
;
            timeid=NCDF_VARID(ncid,'time') 
            NCDF_VARGET,ncid,timeid,timenow,COUNT=1,OFFSET=0
            GET_LUN,unit
            OPENW,unit,'map_from_NC.txt'
            PRINTF,unit,'longitude, latitude, ',ww3fieldname,'  at time (MM/DD/YYYY) ',ww3date,' ',ww3time
            PRINTF,unit,'missing values: ',-2.*maxval
;
; Case of structured grids
;
            IF (triid EQ -1) THEN BEGIN 
               PRINTF,unit,'Number of points: ',NX*NY
               FOR I=0,NX-1 DO BEGIN 
                  FOR J=0,NY-1 DO BEGIN 
                     PRINTF,unit,ww3lon(I),ww3lat(J),ww3matrix(I,J),FORMAT='(F11.6,X,f11.6,X,G12.6)'
                  ENDFOR
               ENDFOR 
            ENDIF ELSE BEGIN 
               NCDF_VARGET,ncid,triid,ww3tri 
               PRINTF,unit,'Number of points: ',NNGP
               FOR I=0,NNGP-1 DO BEGIN 
                  PRINTF,unit,ww3lon(I),ww3lat(I),ww3matrix(I,0),FORMAT='(F10.6,X,f10.6,X,G12.6)'
               ENDFOR
            ENDELSE
            CLOSE,unit
            free_lun,unit
         ENDIF

      ENDIF




      Wright(11,3): c_spec=ev.index
      ENDCASE
      WIDGET_CONTROL, Wdraw, SENSITIVE=1
      END
   'WIDGET_TEXT_CH': BEGIN
      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE (ev.id) OF
         Wright(9,1): mindepth=FLOAT(value(0))
         Wright(9,3): maxdepth=FLOAT(value(0))
         Wright(11,1): IF (FLOAT(value(0)) EQ FIX(value(0))) THEN BEGIN 
             c_gp=FIX(value(0)) 
             ENDIF ELSE BEGIN 
               real1=0.
               real2=0.
               READS,value(0),c_lon,c_lat
               IF N_ELEMENTS(C_X) GT 0 THEN print,'TEST1:',c_x,c_y
               C_x = !values.F_NaN
               NEAREST_POINT
               print,'TEST2:',c_x,c_y
           ENDELSE
      ENDCASE
      END
   'WIDGET_SLIDER': BEGIN

      WIDGET_CONTROL, ev.id, GET_VALUE=value
      CASE (ev.id) OF
         Wright(12,0): BEGIN
            findex=FIX(value)
            IF (findex2 LE findex) THEN findex=findex2-1
            IF (findex LT 1) THEN findex=1
            WIDGET_CONTROL,Wright(12,0), SET_VALUE=findex
            
            IF (datatype EQ 4 AND plottype EQ 2) THEN BEGIN
              UPDATE_WW3_TIME
            ENDIF 
            END
         Wright(12,5): BEGIN
            findex2=FIX(value)
print,'TEST1:',findex,findex2,nfband
            IF (findex2 LT findex) THEN findex2=findex+1
            IF (findex GE nfband) THEN findex=nfband-1
            WIDGET_CONTROL,Wright(12,5), SET_VALUE=findex2
            IF (datatype EQ 4 AND plottype EQ 2) THEN BEGIN
              UPDATE_WW3_TIME
            ENDIF 
print,'TEST2:',findex,findex2
            END
         Wright(12,1): BEGIN
            aindex=FIX(value)
             END

         Wright(12,2): BEGIN
            tindex=LONG(value)
            IF (tindex LT 1) THEN tindex=1
            IF (tindex2 LT tindex) THEN tindex=tindex2
            WIDGET_CONTROL,Wright(12,2), SET_VALUE=tindex

            indext=tindex-1
            IF (datatype EQ 4 ) THEN BEGIN
              UPDATE_WW3_TIME
            ENDIF ELSE BEGIN



            IF N_elements(Hsc) NE 0 THEN BEGIN
               gd(*,*)=Hsc(indext,*,*)
            ENDIF
            IF datatype LT 4 THEN BEGIN
            zs=' UTC'
            assos=STRING(time[indext].m,FORMAT='(I2)')+'/'+ $
                  STRING(time[indext].d,FORMAT='(I2)')+'/'+ $
                  STRING(time[indext].y,FORMAT='(I4)')+' '+ $
                  STRING(time[indext].h,FORMAT='(I2)')+':'+ $
                  STRING(time[indext].minu,FORMAT='(I2)')+' UTC'
            z=time[indext].zone
            IF (z EQ -5) THEN zs=' EST' ELSE $
               IF (z NE 0) THEN BEGIN
                  IF (z < 0) THEN zs=zs+'-'+STRING(ABS(z),FORMAT='(f3.1)') $
                  ELSE zs=zs+'+'+STRING(ABS(z),FORMAT='(f3.1)')
               ENDIF
            ; WIDGET_CONTROL,Wright(13,2),SET_VALUE=assos+zs
            ENDIF
      ENDELSE
            END
         Wright(12,4): BEGIN
            tindex2=LONG(value)
            IF (tindex2 LT tindex) THEN tindex2=tindex
            WIDGET_CONTROL,Wright(12,4), SET_VALUE=tindex2
            END
      ENDCASE
      doplot
      ;IF (ev.id NE Wright(12,2)) THEN doplot
      END
   ENDCASE
   RETURN
END

;----------------------------------------------------------------------------
PRO Time1toDate,dayi,date,hour
COMMON TIME,    timestep,tindex,tindex2,ntime,dtime,time0,day0,dtindex,nstep, $
                timezone_plot,timezone_string,months,time
COMMON LANGUAGE, ilang,dictionnaire
   ;converts a time in juldays
   ;to date and hour strings using julian day functions of IDL
   CALDAT,dayi,month,day,year,h,min,sec
  date='01/01/1994'
   IF ilang GT 0 THEN BEGIN
         date='01/01/1994'
         STRPUT,date,STRING([48b+BYTE(month/10),48b+BYTE(month MOD 10)]),3
         STRPUT,date,STRING([48b+BYTE(day/10),48b+BYTE(day MOD 10)]),0
   ENDIF ELSE BEGIN
         date='01/01/1994'
         STRPUT,date,STRING([48b+BYTE(month/10),48b+BYTE(month MOD 10)]),0
         STRPUT,date,STRING([48b+BYTE(day/10),48b+BYTE(day MOD 10)]),3
   ENDELSE
   STRPUT,date,STRING([48b+BYTE((year MOD 10000)/1000),48b+BYTE((year MOD 1000)/100), $
      48b+BYTE((year MOD 100)/10),48b+BYTE(year MOD 10)]),6
   hour='00:00 '+timezone_string
   STRPUT,hour,STRING([48b+BYTE(h/10),48b+BYTE(h MOD 10)]),0
   STRPUT,hour,STRING([48b+BYTE(min/10),48b+BYTE(min MOD 10)]),3
END
;----------------------------------------------------------------------------
PRO TimetoDate,dayi,timei,date,hour
COMMON TIME,    timestep,tindex,tindex2,ntime,dtime,time0,day0,dtindex,nstep, $
                timezone_plot,timezone_string,months,time
COMMON LANGUAGE, ilang,dictionnaire
   ;converts a time in minutes (counted from julian day dayi at 0:00)
      time2=timei+30.d*365.d*60.d*24.d
   ;to date and hour strings using julian day functions of IDL
   CALDAT,dayi+FIX(timei/(60*24)),month,day,year
      h=((time2 MOD (60*24))/ 60.d)
   h=(h MOD 24)
   min=(time2 MOD 60)
   date='01/01/1994'
   IF ilang GT 0 THEN BEGIN
         date='01/01/1994'
         STRPUT,date,STRING([48b+BYTE(month/10),48b+BYTE(month MOD 10)]),3
         STRPUT,date,STRING([48b+BYTE(day/10),48b+BYTE(day MOD 10)]),0
   ENDIF ELSE BEGIN
         date='01/01/1994'
         STRPUT,date,STRING([48b+BYTE(month/10),48b+BYTE(month MOD 10)]),0
         STRPUT,date,STRING([48b+BYTE(day/10),48b+BYTE(day MOD 10)]),3
   ENDELSE
   STRPUT,date,STRING([48b+BYTE((year MOD 10000)/1000),48b+BYTE((year MOD 1000)/100), $
      48b+BYTE((year MOD 100)/10),48b+BYTE(year MOD 10)]),6
   hour='00:00 '+timezone_string
   STRPUT,hour,STRING([48b+BYTE(h/10),48b+BYTE(h MOD 10)]),0
   STRPUT,hour,STRING([48b+BYTE(min/10),48b+BYTE(min MOD 10)]),3
END

;----------------------------------------------------------------------------
PRO LatLondegtomin,latdegi,londegi,latdeg,latmin,londeg,lonmin,latlonstring
;+
; NAME: LatLondegtomin
; PURPOSE: converts X,Y coordinates into latitude and longitude
;          (negative for west and south)
;          with decimal minutes and a latlon string,
;          e.g.: 36d40.500'N 74d22.100'W
;
; CALLING SEQUENCE: XYtoLatLon,x,y
; INPUT: x and y in kilometers
; OUTPUT: latdeg and londeg in degrees, latmin and lonmin in minutes
;          if west or south, both minutes and degrees are negative
; COMMON BLOCKS: bathy
;-
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON LANGUAGE, ilang,dictionnaire

   IF londegi GT 180. THEN londegi=londegi-360.
   londeg=FLOOR(londegi)
   IF londeg LT 0 THEN londeg=londeg+1
   lonmin=(londegi-londeg)*60.
   latdeg=FLOOR(latdegi)
   IF latdeg LT 0 THEN latdeg=latdeg+1
   latmin=(latdegi-latdeg)*60.   ;***check for negative values
   ;latlonstring='36d40.500`N  74d22.100`'+dictionnaire.Wdir(ilang)
   latlonstring='36d40.500`N  74d22.100`W'
   STRPUT,latlonstring,string(ABS(latdeg),format='(I2)'),0
   STRPUT,latlonstring,string(ABS(latmin),format='(f6.3)'),3
   STRPUT,latlonstring,string(ABS(londeg),format='(I3)'),12
   STRPUT,latlonstring,string(ABS(lonmin),format='(f6.3)'),16
   IF (FLOAT(latdeg)+latmin) LT 0. THEN STRPUT,latlonstring,'S',10
   IF (FLOAT(londeg)+lonmin) GE 0. THEN STRPUT,latlonstring,'E',23
END

;----------------------------------------------------------------------------
PRO XYtoLatLon,x,y,latdeg,latmin,londeg,lonmin,latlonstring,latlonstring2
;+
; NAME: XYtoLatLon
; PURPOSE: converts X,Y coordinates into latitude and longitude
;          (negative for west and south)
;          with decimal minutes and a latlon string,
;          e.g.: 36d40.500'N 74d22.100'W
;
; CALLING SEQUENCE: XYtoLatLon,x,y
; INPUT: x and y in kilometers
; OUTPUT: latdeg and londeg in degrees, latmin and lonmin in minutes
;          if west or south, both minutes and degrees are negative
; COMMON BLOCKS: bathy
;-
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON MAP,     MAPFLAG, MAPPROJ, MAPLONGCENTER, MAPLATCENTER, MAPCONTINENT,  $
                MAPCOUNTRIES, MAPLONLAT
COMMON MAPPOLE, XPOLE, YPOLE, SLAT, RE, E2, E

   IF (MAPLONLAT LE 1) THEN BEGIN 
     dlon=rlonmax-rlonmin
     dlat=rlatmax-rlatmin
     lonmin=rlonmin+x*dlon/(dx*(nx-1))
     latmin=rlatmin+y*dlat/(dy*(ny-1))
   ENDIF ELSE BEGIN
     MAPXY,X,Y,latmin,lonmin,1
   ENDELSE
   lonmin=(lonmin-360*ROUND(FLOAT(lonmin)/360))  ;gives a longitude between -180 and 180
   londec=lonmin
   londeg=FLOOR(lonmin)
   IF londeg LT 0 THEN londeg=londeg+1
   lonmin=(lonmin-londeg)*60.
   latdec=latmin
   latdeg=FLOOR(latmin)
   IF latdeg LT 0 THEN latdeg=latdeg+1
   latmin=(latmin-latdeg)*60.   ;***check for negative values
   latlonstring='36d40.500`N  74d22.100`W'
   STRPUT,latlonstring,string(ABS(latdeg),format='(I2)'),0
   STRPUT,latlonstring,string(ABS(latmin),format='(f6.3)'),3
   STRPUT,latlonstring,string(ABS(londeg),format='(I3)'),12
   STRPUT,latlonstring,string(ABS(lonmin),format='(f6.3)'),16
   IF (FLOAT(latdeg)+latmin) LT 0. THEN STRPUT,latlonstring,'S',10
   IF (FLOAT(londeg)+lonmin) GE 0. THEN STRPUT,latlonstring,'E',23
   latlonstring2='36.000000N  74.000000W'
   IF (latdec) LT 0. THEN STRPUT,latlonstring2,'S',9
   IF (londec) GE 0. THEN STRPUT,latlonstring2,'E',21
   STRPUT,latlonstring2,string(ABS(latdec),format='(f9.6)'),0
   STRPUT,latlonstring2,string(ABS(londec),format='(f10.6)'),11
END


;----------------------------------------------------------------------------
PRO LatLontoXY,latdeg,latmin,londeg,lonmin,x,y
;+
COMMON MAP,     MAPFLAG, MAPPROJ, MAPLONGCENTER, MAPLATCENTER, MAPCONTINENT,  $
                MAPCOUNTRIES, MAPLONLAT
COMMON MAPPOLE, XPOLE, YPOLE, SLAT, RE, E2, E
;
; NAME: LatlontoXY
; PURPOSE: converts latitude and longitude into X,Y coordinates
;          (negative for west and south)
;          with decimal minutes
;
; CALLING SEQUENCE: LatLontoXY,latdeg,latmin,londeg,lonmin
; INPUT: latdeg and londeg in degrees, latmin and lonmin in minutes
; OUTPUT: x and y in kilometers
; COMMON BLOCKS: bathy
;-
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
;
   IF (MAPLONLAT EQ 2) THEN BEGIN 
     mapll,xx,yy,FLOAT(latdeg)+latmin/60.,FLOAT(londeg)+lonmin/60.,1
     X=xx+XPOLE
     Y=yy+YPOLE
   ENDIF ELSE BEGIN 
     lon=FLOAT(londeg)+(lonmin/60.)
     IF lon-rlonmin GT 360 THEN lon=lon-360*(FLOOR((FLOAT(londeg)-rlonmin)/360))
     IF rlonmax-lon GT 360 THEN lon=lon-360*(FLOOR((rlonmax-FLOAT(londeg))/360))
     X=(nx-1)*dx*(lon-rlonmin)/ABS(rlonmax-rlonmin)
     Y=(ny-1)*dy*(FLOAT(latdeg)+latmin/60.-rlatmin)/ABS(rlatmax-rlatmin)
   ENDELSE
END




;-----------------------------------------------------------------------------
PRO NEAREST_POINT
;+
; NAME: NEAREST_POINT
; PURPOSE: Finds nearest point either in a triangle-based grid or a regular grid
; (i.e. action on a widget) as they occur
; CALLING SEQUENCE: NEAREST_POINT

COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth
COMMON SPACE,   c_gp,c_cut,indexgp,c_x,c_y,c_lon,c_lat
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple

;*******END OF COMMON BLOCKS*******************************
;
    IF (FINITE(c_x) EQ 0) THEN BEGIN 
      lat=c_lat
      lon=c_lon
      print,'Looking for nearest point:',c_lon,c_lat
      latdeg=FLOOR(lat)
      IF latdeg LT 0 THEN latdeg=latdeg+1
      londeg=FLOOR(lon)
      IF londeg LT 0 THEN londeg=londeg+1
      latmin=(lat-latdeg)*60.
      lonmin=(lon-londeg)*60.
      LatLontoXY,latdeg,latmin,londeg,lonmin,c_x,c_y
   ENDIF 
;
   IF datastatus(5) THEN BEGIN          ;changes the current grid point
      c_gp=0
      distmin=(c_x-gridmat(0,0))^2+(c_y-gridmat(1,0))^2
      FOR I=1L,nngp-1 DO BEGIN
         dist=(c_x-gridmat(0,I))^2+(c_y-gridmat(1,I))^2
         IF (dist LT distmin) THEN BEGIN
            c_gp=I
            distmin=dist
         ENDIF
      ENDFOR
      c_x=gridmat(0,c_gp)
      c_y=gridmat(1,c_gp) 
      c_gp=c_gp+1
      PRINT,'Nearest point from triangles:',c_gp
   ENDIF ELSE BEGIN 
      WIDGET_CONTROL,Wright(11,1), SET_VALUE=STRCOMPRESS(STRING(c_x),/REMOVE_ALL)
      c_i=MIN([MAX([nxzmin,ROUND(c_x/dx)]),nxzmax])
      c_j=MIN([MAX([nyzmin,ROUND(c_y/dy)]),nyzmax])
      c_x=dx*(c_i)
      c_y=dy*(c_j)
      PRINT,'Nearest point: (',c_i,',',c_j,')'
   ENDELSE
   XYtoLatLon,c_x,c_y,latdeg,latmin,londeg,lonmin,latlonstring,latlonstring2
   c_lat=latdeg+latmin/60.
   c_lon=londeg+lonmin/60.
   print,'Actual position of point:',c_lon,c_lat
   print,'Actual position of point, X, Y in km:',c_x,c_y

   END



;----------------------------------------------------------------------------
PRO Speeds,H,f,cphi,cg,KH

   tpi=6.2831853
   sig=tpi*f
   g=9.81
   a=H*sig*sig/g
   ga=-1.84
   IF(a ge 1.) THEN BEGIN
      yhat=a*(1.+1.26*exp(ga*a))
      t=exp((-2.)*yhat)
      KH=a*(1.+2.*t*(1.+t))
   ENDIF  ELSE BEGIN
      KH=sqrt(a)*(1.+a/6.*(1+a/5.))
   ENDELSE
   dk=KH/H
   cphi=tpi*f/dk
   cg=g*((a/KH)+KH*(1-(a/KH)^2))/(2.*sig)

  RETURN
END

;----------------------------------------------------------------------------
PRO Colorbar,levels,cbtit,truemin,truemax
COMMON COLORBARPAR,cbnticks,cbexrange,cbtrx,cbtry,cbblx,cbbly, $
   filloutofrange,addmini,addmaxi
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON OVERLAY, addir,adsyms,adbathy,adcoast,psyms,psymsizes,adtr,adtri

;
; Position of the colorbar in the page (normalized coordinantes)
;
   POSCB=[blx*cbblx*winx/mwinx,bly*cbbly*winy/mwiny,trx*cbtrx*winx/mwinx,bly*cbtry*winy/mwiny]
   nulevels=N_ELEMENTS(levels)-addmini-addmaxi
   ncolind2=SIZE(colorind)
   ncolind=ncolind2(1)
   xcb=levels(addmini:addmini+nulevels-1)
   ycb=[1]
   IF xcb(nulevels-1) GT xcb(0) THEN BEGIN
   PLOT,xcb,ycb,XSTYLE=cbexrange+4,YSTYLE=4,XTICKS=cbnticks, $
      POSITION=POSCB,/NOERASE,XRANGE=[xcb(0),xcb(nulevels-1)],XTICK_GET=XV
   AXIS,XAXIS=0,TICKLEN=-0.15,XTICKS=cbnticks,XSTYLE=cbexrange,XTITLE=cbtit, $$
      XRANGE=[xcb(0),xcb(nulevels-1)]
   IF truemin GE xcb(0) THEN BEGIN
      IF truemax GT xv(cbnticks) THEN BEGIN
         FOR I=0,nulevels-2 DO $
            POLYFILL,[xcb(i),xcb(i),xcb(i+1),xcb(i+1)],[0,1,1,0],COLOR=colorind(I)
         POLYFILL,[xv(cbnticks),xv(cbnticks),xcb(nulevels-1),xcb(nulevels-1)], $
            [0,1,1,0],COLOR=colorind(N_ELEMENTS(colorind)-2)
         trix=[xv(cbnticks),1.05*xv(cbnticks)-0.05*xv(0),xv(cbnticks)]
         triy=[1,0.5,0]
         IF filloutofrange THEN POLYFILL,trix,triy,COLOR=colorind(N_ELEMENTS(colorind)-1)
         PLOTS,trix,triy
         FOR I=0,nulevels-2 DO $
            POLYFILL,[xcb(i),xcb(i),xcb(i+1),xcb(i+1)],[0,1,1,0],COLOR=colorind(I)
      ENDIF ELSE BEGIN
         FOR I=0,nulevels-2 DO $
            POLYFILL,[xcb(i),xcb(i),xcb(i+1),xcb(i+1)],[0,1,1,0],COLOR=colorind(I)
      ENDELSE
   ENDIF ELSE BEGIN
      IF truemax LE xv(cbnticks) THEN BEGIN
;         FOR I=0,nulevels-2 DO $
         FOR I=0,nulevels-3 DO $
            POLYFILL,[xcb(i),xcb(i),xcb(i+1),xcb(i+1)],[0,1,1,0],COLOR=colorind(I+1)
         IF cbexrange EQ 0 THEN POLYFILL,[xv(0),xv(0),xcb(0),xcb(0)],[0,1,1,0],COLOR=colorind(0)
         trix=[xv(0),1.05*xv(0)-0.05*xv(cbnticks),xv(0)]
         triy=[1,0.5,0]
         IF filloutofrange THEN POLYFILL,trix,triy,COLOR=colorind(0)
         IF (datatype EQ 4) AND (plottype EQ 0) AND (adcoast) AND (xv(0) EQ 0) $
            THEN POLYFILL,trix,triy,color=0
         PLOTS,trix,triy
      ENDIF ELSE BEGIN
         TAILLE=SIZE(colorind);
         INDMAX=TAILLE(1)
         FOR I=0,nulevels-2 DO BEGIN 
         ;   POLYFILL,[xcb(i),xcb(i),xcb(i+1),xcb(i+1)],[0,1,1,0],COLOR=MIN([colorind(I+1),ncolind-1])
            POLYFILL,[xcb(i),xcb(i),xcb(i+1),xcb(i+1)],[0,1,1,0],COLOR=colorind(MIN([INDMAX-1,I+1]))
         ENDFOR
         IF cbexrange EQ 0 THEN BEGIN
            POLYFILL,[xv(cbnticks),xv(cbnticks),xcb(nulevels-1),xcb(nulevels-1)], $
               [0,1,1,0],COLOR=colorind(N_ELEMENTS(colorind)-1)
            POLYFILL,[xv(0),xv(0),xcb(0),xcb(0)],[0,1,1,0],COLOR=colorind(0)
         ENDIF
         trix=[xv(cbnticks),1.05*xv(cbnticks)-0.05*xv(0),xv(cbnticks)]
         triy=[1,0.5,0]
;TV         IF filloutofrange THEN POLYFILL,trix,triy,COLOR=colorind(N_ELEMENTS(colorind)-2)
         IF filloutofrange THEN POLYFILL,trix,triy,COLOR=colorind(N_ELEMENTS(colorind)-1)
         PLOTS,trix,triy
         trix=[xv(0),1.05*xv(0)-0.05*xv(cbnticks),xv(0)]
         triy=[1,0.5,0]
         IF filloutofrange THEN POLYFILL,trix,triy,COLOR=colorind(0)
         IF (datatype EQ 4) AND (plottype EQ 0) AND (adcoast) AND (xv(0) EQ 0) $
            THEN POLYFILL,trix,triy,color=Navailcolor-4
         PLOTS,trix,triy
      ENDELSE
   ENDELSE

   AXIS,XAXIS=0,XTICKS=cbnticks,TICKLEN=1.,XTICKNAME=REPLICATE(' ',cbnticks+1,1),XSTYLE=cbexrange
   AXIS,XAXIS=1,XTICKS=1,TICKLEN=0.,XTICKNAME=[' ',' '],XSTYLE=cbexrange
   AXIS,YAXIS=0,YTICKS=1,TICKLEN=0.,YTICKNAME=[' ',' '],YSTYLE=1
   AXIS,YAXIS=1,YTICKS=1,TICKLEN=0.,YTICKNAME=[' ',' '],YSTYLE=1
   ENDIF
END

;-----------------------------------------------------------------------------
PRO Drawframe,xtit0,xtit1,ytit0,ytit1,frame=imposed_frame
;+
; NAME:
;   Drawframe
; PURPOSE:
;   Draws axes around plot
; CALLING SEQUENCE:
;   Drawframe,frametype
; INPUTS:
;     frametype: integer, specifies the type of frame
;               0:no frame
;               1:"normal"
;               2:mixed lat-lon and cartesian
;               3:
; COMMON BLOCKS:
;   DRAWING,TITLES,BATHY
;     FREQ,DIR,TIME
; COMMENTS
;     The ranges must be already set by the variables rangex
;     and rangey before calling this procedure
; MODIFICATION HISTORY: Created 10/3/99
;-----------------------------------------------------------------------------
COMMON AXISFRAME,axis_orient,frametype,outx1lab,outx2lab,outy1lab,outy2lab
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON TITLES,  font,outtit,outxtit,outytit,outcbtit,textx,texty,textdx,textdy
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON MAP,     MAPFLAG, MAPPROJ, MAPLONGCENTER, MAPLATCENTER, MAPCONTINENT,  $
                MAPCOUNTRIES, MAPLONLAT

   nonames=REPLICATE(' ',30)
   nonames=REPLICATE(' ',30)

   IF N_ELEMENTS(imposed_frame) NE 0 THEN frame=imposed_frame ELSE $
      frame=1+MAPLONLAT
   CASE frame of
   0: ; No frame
   1:BEGIN
      tickn=REPLICATE(' ',30)
      IF outxtit THEN xtit='x (km)' ELSE xtit=' '
      AXIS,XRANGE=rangex,XAXIS=0,XSTYLE=1,XTITLE=xtit, XMINOR=5
      AXIS,XRANGE=rangex,XAXIS=1,XSTYLE=1,XTICKNAME=tickn
      IF outytit THEN ytit='y (km)' ELSE ytit=' '
      AXIS,YRANGE=rangey,YAXIS=0,YSTYLE=1,YTITLE=ytit,YMINOR=5
      AXIS,YRANGE=rangey,YAXIS=1,YSTYLE=1,YTICKNAME=tickn
      END
   2:BEGIN
      ; This frame shows Longitude on the X axis below the plot
      ; And latitude on the y axix on the left of the plot
      latdeg=0.
      latmin=0.
      londeg=0.
      lonmin=0.
      latlonstring=''
      XYtoLatLon,rangex(0),rangey(0), $
                latdeg,latmin,londeg,lonmin,latlonstring
      y2min=latdeg+latmin/60.
      x2min=londeg+lonmin/60.
      XYtoLatLon,rangex(1),rangey(1), $
                latdeg,latmin,londeg,lonmin,latlonstring
      x2max=londeg+lonmin/60.
      IF x2max LE x2min THEN x2max=x2max+360.
      rangex2=[x2min,x2max]
      rangey2=[y2min,latdeg+latmin/60.]

      ;**********x axes******************

      IF (rangex2(1)*rangex2(0) LT 0) THEN BEGIN
      ; If the range covers positive and neg. values
         AXIS,XRANGE=rangex2,XAXIS=0,XSTYLE=5,XMINOR=4,XTICK_GET=XV, $
            XTICKNAME=nonames
         siz=SIZE(XV)
         xnames=nonames(0:siz(1)-1)
         IF outx2lab THEN BEGIN
         FOR I=0,siz(1)-1 DO BEGIN
            name='  75d  `'
            lonmin=XV(I)
            londeg=FLOOR(lonmin)
            IF londeg LT 0 AND (FLOAT(londeg)-lonmin LT -0.001) $
               THEN londeg=londeg+1
            lonmin=(lonmin-londeg)*60.
            STRPUT,name,string(londeg,format='(I4)'),0
            STRPUT,name,string(ROUND(ABS(lonmin)),format='(I2)'),5
            xnames(I)=name
         ENDFOR
         ENDIF
         IF outxtit THEN xtit='Longitude' ELSE xtit=' '
      ; Plots the axis with the longitude
         AXIS,XRANGE=rangex2,XAXIS=0,XSTYLE=1,XMINOR=4,XTITLE=xtit, $
            XTICKNAME=xnames
         AXIS,XRANGE=rangex2,XAXIS=1,XSTYLE=1,XMINOR=4, $
            XTICKNAME=nonames
      ENDIF ELSE BEGIN
      IF (ABS(FLOOR(rangex2(1))-FLOOR(rangex2(0))) GT 10) THEN BEGIN
      ; If the range covers more than 10 degrees
         AXIS,XRANGE=rangex2,XAXIS=0,XSTYLE=5,XMINOR=4,XTICK_GET=XV, $
            XTICKNAME=nonames
         siz=SIZE(XV)
         xnames=nonames(0:siz(1)-1)
         IF outx2lab THEN BEGIN
         FOR I=0,siz(1)-1 DO BEGIN
            name=' 75d'
            lonmin=XV(I)
            londeg=FLOOR(lonmin)
            IF londeg LT 0 AND (FLOAT(londeg)-lonmin LT -0.001) $
               THEN londeg=londeg+1
            STRPUT,name,string(ABS(londeg),format='(I3)'),0
            xnames(I)=name
         ENDFOR
         ENDIF
         IF XV(0) LT 0 THEN hemi='W' ELSE hemi='E'
       IF (ABS(FLOOR(rangex2(1))-FLOOR(rangex2(0))) GT 180) THEN BEGIN
            IF outxtit THEN xtit='Longitude' ELSE xtit=' '
      ENDIF ELSE BEGIN
            IF outxtit THEN xtit='Longitude ('+hemi+')' ELSE xtit=' '
        ENDELSE
      ; Plots the axis with the longitude
         AXIS,XRANGE=rangex2,XAXIS=0,XSTYLE=1,XMINOR=4,XTITLE=xtit, $
            XTICKNAME=xnames
         AXIS,XRANGE=rangex2,XAXIS=1,XSTYLE=1,XMINOR=4, $
            XTICKNAME=nonames
      ENDIF ELSE BEGIN

      IF (FLOOR(rangex2(1)) NE  FLOOR(rangex2(0))) THEN BEGIN
      ; If the range covers more than one degree
         AXIS,XRANGE=rangex2,XAXIS=0,XSTYLE=5,XMINOR=4,XTICK_GET=XV, $
            XTICKNAME=nonames
         siz=SIZE(XV)
         xnames=nonames(0:siz(1)-1)
         IF outx2lab THEN BEGIN
         FOR I=0,siz(1)-1 DO BEGIN
            name=' 75d40`'
            lonmin=XV(I)
            londeg=FLOOR(lonmin)
            IF londeg LT 0 AND (FLOAT(londeg)-lonmin LT -0.001) $
               THEN londeg=londeg+1
            lonmin=(lonmin-londeg)*60.
            STRPUT,name,string(ABS(londeg),format='(I3)'),0
            STRPUT,name,string(ROUND(ABS(lonmin)),format='(I2)'),4
            xnames(I)=name
         ENDFOR
         ENDIF
         IF XV(0) LT 0 THEN hemi='W' ELSE hemi='E'
         IF outxtit THEN xtit='Longitude ('+hemi+')' ELSE xtit=' '
      ; Plots the axis with the longitude
         AXIS,XRANGE=rangex2,XAXIS=0,XSTYLE=1,XMINOR=4,XTITLE=xtit, $
            XTICKNAME=xnames
      ENDIF ELSE BEGIN
      ; If the range covers less than one degree
         AXIS,XRANGE=rangex2*60.,XAXIS=0,XSTYLE=5,XMINOR=4,XTICK_GET=XV, $
            XTICKNAME=nonames
         siz=SIZE(XV)
         xnames=nonames(0:siz(1)-1)
         IF outx2lab THEN BEGIN
         FOR I=0,siz(1)-1 DO BEGIN
            name='40.00`'
            lonmin=XV(I)
            londeg=FLOOR(lonmin/60.)
            IF londeg LT 0 AND (FLOAT(londeg*60)-lonmin LT 0.001) $
               THEN londeg=londeg+1
            lonmin=(lonmin-londeg*60)
            STRPUT,name,string(ABS(lonmin),format='(F5.2)'),0
            xnames(I)=name
         ENDFOR
         ENDIF
         IF XV(0) LT 0 THEN hemi='W' ELSE hemi='E'
         IF outxtit*outx2lab THEN xtit='Longitude ('+ $
            string(ABS(londeg),format='(I3)')+'d'+hemi+')' ELSE xtit=' '
         AXIS,XRANGE=rangex2*60,XAXIS=0,XSTYLE=1,XMINOR=4,XTITLE=xtit, $
            XTICKNAME=xnames
      ENDELSE
      IF outxtit*outx1lab THEN xtit='x (km)' ELSE xtit=' '
      ; Plots the axis with the kilometers
      IF outX1lab THEN $
         AXIS,XRANGE=rangex,XAXIS=1,XSTYLE=1,XMINOR=5,XTITLE=xtit $
      ELSE AXIS,XRANGE=rangex,XAXIS=1,XSTYLE=1,XMINOR=5,XTITLE=xtit, $
             XTICKNAME=nonames
      ENDELSE
      ENDELSE

      ;**********y axes******************
      IF (rangey2(1)*rangey2(0) LT 0 AND ABS(FLOOR(rangey2(1)) -FLOOR(rangey2(0))) LE 10) THEN BEGIN
      ;case where the range covers latitudes north and south of the equator
         AXIS,YRANGE=rangey2,YAXIS=0,YSTYLE=5,YMINOR=4,YTICK_GET=YV, $
            YTICKNAME=nonames
         siz=SIZE(YV)
         ynames=nonames(0:siz(1)-1)
         IF outy2lab THEN BEGIN
         FOR I=0,siz(1)-1 DO BEGIN
            name=' 75d40`  '
            latmin=YV(I)
            latdeg=FLOOR(latmin)
            IF latdeg LT 0 AND (FLOAT(latdeg)-latmin LT 0.001) THEN latdeg=latdeg+1
            latmin=(latmin-latdeg)*60.
            STRPUT,name,string(ABS(latdeg),format='(I3)'),0
            STRPUT,name,string(ROUND(ABS(latmin)),format='(I2)'),4
            ynames(I)=name
         ENDFOR
         ENDIF
         IF outytit THEN ytit='Latitude' ELSE ytit=' '
         AXIS,YRANGE=rangey2,YAXIS=0,YSTYLE=1,YMINOR=4,YTITLE=ytit, $
            YTICKNAME=ynames
         AXIS,YRANGE=rangey2,YAXIS=1,YSTYLE=1,YMINOR=4, $
            YTICKNAME=nonames
      ENDIF ELSE BEGIN
      IF (ABS(FLOOR(rangey2(1)) -FLOOR(rangey2(0))) GT 10) THEN BEGIN
         AXIS,YRANGE=rangey2,YAXIS=0,YSTYLE=5,YMINOR=4,YTICK_GET=YV, $
            YTICKNAME=nonames
         siz=SIZE(YV)
         ynames=nonames(0:siz(1)-1)
         IF outy2lab THEN BEGIN
         FOR I=0,siz(1)-1 DO BEGIN
            name='75d'
            latmin=YV(I)
            latdeg=FIX(latmin)
            STRPUT,name,string(ABS(latdeg),format='(I2)'),0
            ynames(I)=name
         ENDFOR
         ENDIF
         IF YV(0) LT 0 THEN hemi='S' ELSE hemi='N'
         IF outytit THEN ytit='Latitude ('+hemi+')' ELSE ytit=' '
         AXIS,YRANGE=rangey2,YAXIS=0,YSTYLE=1,YMINOR=4,YTITLE=ytit, $
            YTICKNAME=ynames
         AXIS,YRANGE=rangey2,YAXIS=1,YSTYLE=1,YMINOR=4, $
            YTICKNAME=nonames
      ENDIF ELSE BEGIN
      IF (FLOOR(rangey2(1)) NE FLOOR(rangey2(0))) THEN BEGIN
         AXIS,YRANGE=rangey2,YAXIS=0,YSTYLE=5,YMINOR=4,YTICK_GET=YV, $
            YTICKNAME=nonames
         siz=SIZE(YV)
         ynames=nonames(0:siz(1)-1)
         IF outy2lab THEN BEGIN
         FOR I=0,siz(1)-1 DO BEGIN
            name='75d40`  '
            latmin=YV(I)
            latdeg=FLOOR(latmin)
            IF latdeg LT 0 AND (FLOAT(latdeg)-latmin LT 0.001) THEN latdeg=latdeg+1
            latmin=(latmin-latdeg)*60.
            STRPUT,name,string(ABS(latdeg),format='(I2)'),0
            STRPUT,name,string(ROUND(ABS(latmin)),format='(I2)'),3
            ynames(I)=name
         ENDFOR
         ENDIF
         IF YV(0) LT 0 THEN hemi='S' ELSE hemi='N'
         IF outytit THEN ytit='Latitude ('+hemi+')' ELSE ytit=' '
         AXIS,YRANGE=rangey2,YAXIS=0,YSTYLE=1,YMINOR=4,YTITLE=ytit, $
            YTICKNAME=ynames
      ENDIF ELSE BEGIN
         ;case where the range covers less than 1 degree
         ;The degree number is written in the axis title
         AXIS,YRANGE=rangey2*60.,YAXIS=0,YSTYLE=5,YMINOR=4,YTICK_GET=YV, $
         YTICKNAME=nonames
         siz=SIZE(YV)
         ynames=nonames(0:siz(1)-1)
         IF outy2lab THEN BEGIN
         FOR I=0,siz(1)-1 DO BEGIN
            name='40.00`'
            latmin=YV(I)
            latdeg=FLOOR(latmin/60.)
            IF latdeg LT 0 AND (FLOAT(latdeg*60)-latmin LT 0.001) THEN latdeg=latdeg+1
            latmin=(latmin-latdeg*60)
            STRPUT,name,string(ABS(latmin),format='(F5.2)'),0
            ynames(I)=name
         ENDFOR
         ENDIF
         IF YV(0) LT 0 THEN hemi='S' ELSE hemi='N'
         IF outytit*outy2lab THEN ytit='Latitude ('+ $
            string(ABS(latdeg),format='(I2)')+'d'+hemi+')' ELSE ytit=' '
      AXIS,YRANGE=rangey2*60,YAXIS=0,YSTYLE=1,YMINOR=4,YTITLE=ytit, $
         YTICKNAME=ynames
      ENDELSE
      IF outytit*outy1lab THEN ytit='y (km)' ELSE ytit=' '
      IF outy1lab THEN $
          AXIS,YRANGE=rangey,YAXIS=1,YSTYLE=1,YMINOR=5,YTITLE=ytit $
      ELSE AXIS,YRANGE=rangey,YAXIS=1,YSTYLE=1,YMINOR=5,YTITLE=ytit,  $
            YTICKNAME=nonames
      ENDELSE
      ENDELSE

      END
   3:BEGIN
      PLOT,x,y,/NODATA,xstyle=1,ystyle=1, $
         XTITLE=xtitle,YTITLE=ytitle, TITLE=title,/NOERASE, $
         XRANGE=rangex,YRANGE=rangey, $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
      END
   4:BEGIN
      tickvals=[0.,30.,60.,90.,120.,150.,180.]
      ticknames=['North','30','60','East','120','150','South']
      PLOT,x,y,/NODATA,xstyle=1,ystyle=1, $
         XTITLE=xtitle,YTITLE=ytitle, TITLE=title,$
         XRANGE=rangex,YRANGE=rangey, /NOERASE, $
         XTICKS=6,XTICKV=tickvals,XTICKNAME=ticknames,XMINOR=3, $
         YTICKS=6,YTICKV=tickvals,YTICKNAME=ticknames,YMINOR=3, $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
      END
   5:BEGIN
      tickvals=[0.,30.,60.,90.,120.,150.,180.,210.,240.,270.,300.,330,360.]
      ticknames=['North','30','60','East','120','150','South', $
                 '210','240','West','300','330','North']
      PLOT,[0],[0],/NODATA,xstyle=1,ystyle=1, $
         XTITLE=xtitle,YTITLE=ytitle, TITLE=title,$
         XRANGE=rangex,YRANGE=rangey, /NOERASE, $
         YTICKS=12,YTICKV=tickvals,YTICKNAME=ticknames,YMINOR=3, $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
      END
   6:BEGIN
      tickvals=[0.,30.,60.,90.,120.,150.,180.]
      ticknames=['North','30','60','East','120','150','South']
      PLOT,[0],[0],/NODATA,xstyle=1,ystyle=1, $
         XTITLE=xtitle,YTITLE=ytitle, TITLE=title,$
         XRANGE=rangex,YRANGE=rangey, /NOERASE, $
         YTICKS=6,YTICKV=tickvals,YTICKNAME=ticknames,YMINOR=3, $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
      END
   7:BEGIN
      ;tickn=REPLICATE(' ',30)
      latdeg=0.
      latmin=0.
      londeg=0.
      lonmin=0.
      latlonstring=''
      XYtoLatLon,rangex(0),rangey(0), $
                latdeg,latmin,londeg,lonmin,latlonstring
      y2min=latdeg+latmin/60.
      x2min=londeg+lonmin/60.
      XYtoLatLon,rangex(1),rangey(1), $
                latdeg,latmin,londeg,lonmin,latlonstring
      rangex2=[x2min,londeg+lonmin/60.]
      rangey2=[y2min,latdeg+latmin/60.]
      ;**********x axes******************
      xnames=REPLICATE(' ',30)
      IF (rangey2(1)-rangey2(0)) GT 1. THEN BEGIN
      AXIS,XRANGE=rangex2,XAXIS=1,XSTYLE=5,XMINOR=4,XTICK_GET=XV, $
         XTICKNAME=xnames
      siz=SIZE(XV)
      xnames=STRARR(siz(1))
      FOR I=0,siz(1)-1 DO BEGIN
         name='75d40`'
         lonmin=XV(I)
         londeg=FLOOR(lonmin)
         IF londeg LT 0 AND (FLOAT(londeg)-lonmin LT -0.001) THEN londeg=londeg+1
         lonmin=(lonmin-londeg)*60.
         STRPUT,name,string(ABS(londeg),format='(I2)'),0
         STRPUT,name,string(ROUND(ABS(lonmin)),format='(I2)'),3
         xnames(I)=name
      ENDFOR
      IF XV(0) LT 0 THEN hemi='W' ELSE hemi='E'
      IF outxtit THEN xtit='Longitude ('+hemi+')' ELSE xtit=' '
      AXIS,XRANGE=rangex2,XAXIS=1,XSTYLE=1,XMINOR=4,XTITLE=xtit, $
         XTICKNAME=xnames
      IF outxtit THEN xtit='x (km)' ELSE xtit=' '
      ENDIF ELSE BEGIN
      AXIS,XRANGE=rangex2*60.,XAXIS=1,XSTYLE=5,XMINOR=4,XTICK_GET=XV, $
         XTICKNAME=xnames
      siz=SIZE(XV)
      xnames=STRARR(siz(1))
      FOR I=0,siz(1)-1 DO BEGIN
         name='40.00`'
         lonmin=XV(I)
         londeg=FLOOR(lonmin/60.)
         IF londeg LT 0 AND (FLOAT(londeg*60)-lonmin LT 0.001) THEN londeg=londeg+1
         lonmin=(lonmin-londeg*60)
         STRPUT,name,string(ABS(lonmin),format='(F5.2)'),0
         xnames(I)=name
      ENDFOR
      IF XV(0) LT 0 THEN hemi='W' ELSE hemi='E'
      IF outxtit THEN xtit='Longitude ('+ $
      string(ABS(londeg),format='(I2)')+'d'+hemi+')' ELSE xtit=' '
      AXIS,XRANGE=rangex2*60,XAXIS=1,XSTYLE=1,XMINOR=4,XTITLE=xtit, $
         XTICKNAME=xnames
      ENDELSE
      END
   8:BEGIN
      latdeg=0.
      latmin=0.
      londeg=0.
      lonmin=0.
      latlonstring=''
      XYtoLatLon,rangey(0),rangex(0), $
                latdeg,latmin,londeg,lonmin,latlonstring
      X2min=latdeg+latmin/60.
      XYtoLatLon,rangey(1),rangex(1), $
                latdeg,latmin,londeg,lonmin,latlonstring
      rangeX2=[X2min,latdeg+latmin/60.]
      ;**********X axes******************
      Xnames=REPLICATE(' ',30)
      IF (rangeX2(1)-rangeX2(0)) GT 1. THEN BEGIN
      AXIS,XRANGE=rangeX2,XAXIS=1,XSTYLE=5,XMINOR=4,XTICK_GET=XV, $
         XTICKNAME=Xnames
      siz=SIZE(XV)
      Xnames=STRARR(siz(1))
      FOR I=0,siz(1)-1 DO BEGIN
         name='75d40`  '
         latmin=XV(I)
         latdeg=FLOOR(latmin)
         IF latdeg LT 0 AND (FLOAT(latdeg)-latmin LT 0.001) THEN latdeg=latdeg+1
         latmin=(latmin-latdeg)*60.
         STRPUT,name,string(ABS(latdeg),format='(I2)'),0
         STRPUT,name,string(ROUND(ABS(latmin)),format='(I2)'),3
         Xnames(I)=name
      ENDFOR
      IF XV(0) LT 0 THEN hemi='S' ELSE hemi='N'
      IF outXtit THEN Xtit='Latitude ('+hemi+')' ELSE Xtit=' '
      AXIS,XRANGE=rangeX2,XAXIS=1,XSTYLE=1,XMINOR=4,XTITLE=Xtit, $
         XTICKNAME=Xnames
      ENDIF ELSE BEGIN
      AXIS,XRANGE=rangeX2*60.,XAXIS=1,XSTYLE=5,XMINOR=4,XTICK_GET=XV, $
         XTICKNAME=Xnames
      siz=SIZE(XV)
      Xnames=STRARR(siz(1))
      FOR I=0,siz(1)-1 DO BEGIN
         name='40.00`'
         latmin=XV(I)
         latdeg=FLOOR(latmin/60.)
         IF latdeg LT 0 AND (FLOAT(latdeg*60)-latmin LT 0.001) THEN latdeg=latdeg+1
         latmin=(latmin-latdeg*60)
         STRPUT,name,string(ABS(latmin),format='(F5.2)'),0
         Xnames(I)=name
      ENDFOR
      IF XV(0) LT 0 THEN hemi='S' ELSE hemi='N'
      IF outXtit THEN Xtit='Latitude ('+ $
         string(ABS(latdeg),format='(I2)')+'d'+hemi+')' ELSE Xtit=' '
      AXIS,XRANGE=rangeX2*60,XAXIS=1,XSTYLE=1,XMINOR=4,XTITLE=Xtit, $
         XTICKNAME=Xnames
      ENDELSE
      END
   ENDCASE
RETURN
END


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pro makebin,xval,yval,rangex,rangey,nx,ny,bindata,bindataw,ibin,jbin,binweight=binweight
   nel=N_elements(xval)
   nely=N_elements(yval)
   IF (nel EQ nely) THEN BEGIN
   dx=(rangex(1)-rangex(0))/(nx-1)
   dy=(rangey(1)-rangey(0))/(ny-1)
   bindata=FLTARR(nx,ny)
   bindataw=FLTARR(nx,ny)
   ibin=INTARR(nel)
   jbin=INTARR(nel)
   FOR K=0L,nel-1 DO BEGIN
      i=ROUND((xval(K)-rangex(0))/dx)
      j=ROUND((yval(K)-rangey(0))/dy)
      IF (I GT 0 AND I LT nx-1 AND J GT 0 AND J LT ny-1) THEN BEGIN
         bindata(i,j)=bindata(i,j)+1
         IF keyword_set(binweight) THEN bindataw(i,j)=bindataw(i,j)+binweight(K)
         ibin(K)=ROUND(i)
         jbin(K)=ROUND(j)
     ENDIF ELSE BEGIN
         ibin(K)=-1
         jbin(K)=-1
        ENDELSE
   ENDFOR
   IF keyword_set(binweight) THEN BEGIN
      KK=WHERE(bindata GE 1,kount)
      IF (kount GT 0) THEN  bindataw(KK)=bindataw(KK)/bindata(KK)
   ENDIF
   ENDIF ELSE BEGIN
      print,'Cannot bin data, different sizes:',nel,nely
   ENDELSE
   RETURN
END

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;----------------------------------------------------------------------------
PRO Contorno,table,x,y,tit,xtit,ytit,cbtit,contoption,swap_palette,cbaxis, clev=clev, coli=coli, tri=tri
;+
; NAME:
;   Contorno
; PURPOSE:
;   Plots a 2D array of data using various techniques
; CALLING SEQUENCE:
;   Contorno,table,x,y,tit,cbtit,contoption,swap_palette
; INPUTS:
;          table: the 2D data to be plotted
;              x: x coordinate of the data
;              y: y coordinate of the data
;            tit: title of the plot
;          cbtit: title for the color bar
;     contoption: changes the fill technique for CONTOUR
;                 should be set to 1 for irregular grids
;   swap_palette:
; COMMON BLOCKS:
;   CONTOURPARAM,CURRENT...
; MODIFICATION HISTORY: Created 1/1/99-10/10/99
;-
;** 1 ** Display parameters
common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
COMMON COLORBARPAR,cbnticks,cbexrange,cbtrx,cbtry,cbblx,cbbly, $
   filloutofrange,addmini,addmaxi
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON OVERLAY2,adbathydot,adcontour
COMMON TITLES,  font,outtit,outxtit,outytit,outcbtit,textx,texty,textdx,textdy
COMMON POSTSCRIPT, filep,pspath,prcoul,psor,pstype, $
                pwinx,pwiny,papierx,papiery,xoffset,yoffset, $
                facpolice,fontrescale,basefontsize,pssizex,pssizey,psfont
COMMON THREED,  Ax3D,Az3D,smoothing
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth
COMMON MAP,     MAPFLAG, MAPPROJ, MAPLONGCENTER, MAPLATCENTER, MAPCONTINENT,  $
                MAPCOUNTRIES, MAPLONLAT
;*******END OF COMMON BLOCKS*******************************

   tablep=table
   IF (filltype GE 5) AND (filltype LE 7) AND (maxdepth EQ 1) THEN BEGIN
      taille=size(tablep)
      FOR I=0,taille(1)-1 DO BEGIN
         tablep(I,*)=tablep(I,*)/x(I)  ; division by the radial coordinate
                                       ; to obtain densities for the polar plots
      ENDFOR
      taille=SIZE(tablep)
      table2=FLTARR(taille(1)+1,taille(2)) ;+2.*maxval
      table2(1:taille(1),*)=tablep      ;adds one value for R so that
                                        ;there will be no contours around R=0
      x2=FLTARR(taille(1)+1)
      x2(1:taille(1))=x
      x2(0)=MAX([0,2*x(0)-x(1)])
      tablep=tablep/max(tablep)
   ENDIF

   index=WHERE(tablep GT maxval,kount)
   table2=tablep
   ;IF (kount GE 1) THEN table2(index)=-2.*maxval
   truemin=min(tablep)
   truemax=max(table2)
   
   IF logplot THEN BEGIN
      indexminus=WHERE(tablep LT 0. AND tablep GT -1*maxval,kountminus)
      indexplus=WHERE(tablep GT 0. AND tablep LT maxval,kountplus)
      indexzero=WHERE(tablep EQ 0.,kountzero)
      truemax=-25.
      truemin=-30.
      IF (kountplus GT 1) THEN BEGIN
         truemin=MIN(ALOG10(tablep(indexplus)))
         truemax=MAX(ALOG10(tablep(indexplus)))
      ENDIF
      IF (kountminus GT 1) THEN BEGIN
         truemin2=MIN(ALOG10(-1.*tablep(indexminus)))
         truemax2=MAX(ALOG10(-1.*tablep(indexminus)))
         IF truemin2 LT truemin THEN truemin=truemin2
         IF truemax2 GT truemax THEN truemax=truemax2
      ENDIF
      IF (kountzero GT 1) THEN truemin=-30.
      cbtit='LOG10['+cbtit+']'
   ENDIF
   IF fixrange THEN BEGIN
      mindata=mindepth
      maxdata=maxdepth
   ENDIF ELSE BEGIN
      maxdata=MIN([truemax,maxdepth])
      mindata=MAX([truemin,mindepth])
   ENDELSE
   IF (mindata LT maxdata) THEN BEGIN 
   IF outtit EQ 1 THEN title=tit ELSE title=''

   IF (MAPFLAG EQ 1) THEN CASE MAPPROJ OF 
         0: MAP_SET,MAPLATCENTER,MAPLONGCENTER, /NOBORDER, /orthographic,/isotropic , /HORIZON, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
       1 :MAP_SET,MAPLATCENTER,MAPLONGCENTER, /NOBORDER, /mollweide,/isotropic , /HORIZON, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
       2 :MAP_SET,MAPLATCENTER,MAPLONGCENTER, /NOBORDER, /mercator,/isotropic, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
       3 :MAP_SET,MAPLATCENTER,MAPLONGCENTER, /NOBORDER, /azimuthal,/isotropic , /HORIZON, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
        ENDCASE
   
   CASE filltype OF
   0:BEGIN
      IF keyword_set(clev) THEN BEGIN
         taille=size(clev)
         c_numlev=taille(1)-1
         table(0,0)=clev(c_numlev)
         truemax=table(0,0)
         addmaxi=1
         addmini=0
         lev=clev
         colorind=coli
      ENDIF ELSE BEGIN
         c_numlev=numlevels
         MakeLevels,mindata,maxdata,truemin,truemax
         colorind=1+FINDGEN(c_numlev-1+addmini+addmaxi) $
            *(Navailcolor-5)/(c_numlev+addmini+addmaxi-2)
      ENDELSE
      IF swap_palette THEN colorind=navailcolor-2-colorind
      if cbar AND MAPFLAG EQ 0 THEN ColorBar,lev,cbtit,truemin,truemax
      IF logplot THEN BEGIN
         IF (kountplus GT 1) THEN tablep(indexplus)=ALOG10(tablep(indexplus))
         IF (kountminus GT 1) THEN tablep(indexminus)=ALOG10(-1.*tablep(indexminus))
         IF (kountzero GT 1) THEN tablep(indexzero)=-30.
         indexsmall=WHERE(tablep LT -30., kountsmall)
         IF (kountsmall GE 1) THEN tablep(indexsmall)=-30.
      ENDIF
      taille=size(lev)
      doit=1
      trie=sort(lev)
      FOR I=0,taille(1)-1 DO BEGIN
         if trie(I) NE I THEN doit=0
      ENDFOR
      IF doit AND lev(taille(1)-1) GT lev(0) THEN BEGIN
      IF contoption EQ 0 THEN BEGIN
          IF (MAPFLAG EQ 1) THEN BEGIN
   
             taille=SIZE(tablep)
             nx=taille(1);
             IF (rlonmax-rlonmin GT 360*(nx-1.2)/nx) THEN  BEGIN 
                PRINT,'GLOBAL'
                table2=FLTARR(nx+1,taille(2)) ;
                table2(0:nx-1,*)=tablep      
                table2(nx,*)=tablep(0,*)
                x2=FLTARR(taille(1)+1)
                x2(0:nx-1,*)=x
                x2(nx)=x(0)
                x=x2
                tablep=table2    
             ENDIF 
             CONTOUR,tablep,rlonmin+x*(rlonmax-rlonmin)/(max(x)-min(x)), $
                 rlatmin+y*(rlatmax-rlatmin)/max(y),xstyle=5,ystyle=5,/FOLLOW,CELL_FILL=kount,  $
                 /FILL,C_COLOR=colorind(0:c_numlev-2+addmini+addmaxi), $
                 LEVELS=lev, /NOERASE,TITLE=title,   /overplot,MAX_VALUE=maxval
        
             c_numlev=numlevels
             MakeLevels,mindata,maxdata,truemin,truemax
             colorind=1+FINDGEN(c_numlev-1+addmini+addmaxi) $
                       *(Navailcolor-5)/(c_numlev+addmini+addmaxi-2)
       
          ENDIF ELSE BEGIN 
             CONTOUR,tablep,x,y,xstyle=5,ystyle=5,  $
                 /FILL,C_COLOR=colorind(0:c_numlev-2+addmini+addmaxi), $
                 LEVELS=lev,    /NOERASE,TITLE=title, $
                 XRANGE=rangex,YRANGE=rangey, $
                 POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny] 
           ENDELSE 
       ENDIF ELSE BEGIN
         CONTOUR,tablep,x,y,$
            xstyle=5,ystyle=5, TRIANGULATION=tri, $  ; TRIANGULATION=tri2(0:2,0:16578), $
            /CELL_FILL,C_COLOR=colorind(0:c_numlev-2+addmini+addmaxi), $
            LEVELS=lev,/NOERASE,TITLE=title,CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)], $
            XRANGE=rangex,YRANGE=rangey,MAX_VALUE=maxval, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
        ENDELSE
      ENDIF
            
 
            
      END
   1:BEGIN ;Uses the TV procedure to diplay an image
      IF contoption EQ 0 THEN BEGIN
      c_numlev=Navailcolor-4
      MakeLevels,mindata,maxdata,truemin,truemax
      colorind=1+FINDGEN(navailcolor-5+addmini+addmaxi)
            toto=size(tablep)
            image=1+ $
               BYTSCL(tablep,min=mindata,max=maxdata,Top=(navailcolor-5))
            IF swap_palette THEN  BEGIN
               image=navailcolor-2-image
               colorind=navailcolor-2-colorind
            ENDIF
            if cbar AND MAPFLAG EQ 0 THEN ColorBar,lev,cbtit,truemin,truemax
            IF (MAPFLAG EQ 0) THEN PLOT,[x(0)],[y(0)],xstyle=5,ystyle=5,/NODATA, $
            XRANGE=rangex,YRANGE=rangey, /NOERASE,TITLE=title, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
            IF (output EQ 0) THEN BEGIN

               IF (MAPFLAG EQ 1) THEN BEGIN
                  result=MAP_IMAGE(image , $
                  x0,y0,xx,yy, $
                  LATMIN=rlatmin,LONMIN=rlonmin,LATMAX=rlatmax,LONMAX=rlonmax,COMPRESS=1 )
                  TV, result,x0,y0  ;, xsize=(trx-blx)*winx,ysize=(try-bly)*winy,/device
               ENDIF ELSE $
                  TV,POLY_2D(image,$
                     [[0,0],[(toto(1)+0.5)^2/((toto(1)+1.5)*winx*(trx-blx)),0]], $
                     [[0,(toto(2)+0.5)^2/((toto(2)+1.5)*winy*(try-bly))],[0,0]],$
                     keyword_set(interp),(trx-blx)*winx,(try-bly)*winy), $
                     xsize=(trx-blx)*winx,ysize=(try-bly)*winy, $
                     min(x),min(y),/data
            ENDIF ELSE BEGIN
               print,'min et max col:',min(image),max(image)
               TV,image,blx*pssizex,bly*pssizey, $
               XSIZE=(trx-blx)*pssizex,YSIZE=(try-bly)*pssizey,/CENTIMETERS
            ENDELSE
         ENDIF
         END
      2:BEGIN ; Uses Polyfill to draw squares filled with a color
         taille=size(tablep)
         IF (numlevels LT 0) THEN BEGIN
            c_numlev=-numlevels
            MakeLevels,mindata,maxdata,truemin,truemax
            colorind=1+FINDGEN(c_numlev-1+addmini+addmaxi) $
              *(Navailcolor-5)/(c_numlev+addmini+addmaxi-2)
      IF swap_palette THEN colorind=navailcolor-2-colorind
            COLORf=tablep*0.
              FOR I=1,c_numlev-1 DO BEGIN
                 IND=WHERE (tablep GT lev(I-1) AND tablep LE lev(I),kount)
                 IF (kount GT 0) THEN COLORf(IND)=colorind(I-1)
               ENDFOR
         ENDIF ELSE BEGIN
            c_numlev=Navailcolor-4
            MakeLevels,mindata,maxdata,truemin,truemax
            colorind=1+FINDGEN(navailcolor-5+addmini+addmaxi)
            COLORf=1+ROUND((Navailcolor-3)*(tablep-mindata)/(maxdata-mindata))
           
            IF swap_palette THEN COLORf=navailcolor-2-COLORf
            IND=WHERE (COLORF LT 1,kount)
            IF (kount GT 0)  THEN COLORF(IND)=1
            IND=WHERE (COLORF GT Navailcolor-3,kount)
            IF (kount GT 0)  THEN COLORF(IND)=Navailcolor-3
            IND=WHERE (tablep GT MAXVAL,kount)
            IF (kount GT 0)  THEN COLORF(IND)=Navailcolor-2
         ENDELSE
         if cbar THEN ColorBar,lev,cbtit,truemin,truemax
         PLOT,[x(0)],[y(0)],xstyle=5,ystyle=5,/NODATA, $
            XRANGE=rangex,YRANGE=rangey, /NOERASE,TITLE=title, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         i=0
         dx2=0.5*(x(1)-x(0)) ;for regular grids only
         dy2=0.5*(y(1)-y(0))
         FOR J=0,taille(2)-1 DO BEGIN
            firstx=0
            FOR i=0,taille(1)-1 DO BEGIN
            IF (tablep(i,j) LT maxval) THEN BEGIN 

            IF (firstx EQ 0) AND (COLORF(I,J) NE Navailcolor-2) THEN firstx=1
            IF (COLORf(I,J) LT Navailcolor) THEN BEGIN
            im1=max([i-1,0])
            xim1=x(i)-dx2
            IF (im1 EQ i) THEN xim1=x(i)
            ip1=min([i+1,taille(1)-1])
            xip1=x(i)+dx2
            IF (ip1 EQ i) THEN xip1=x(i)

            jm1=max([j-1,0])
            yjm1=y(j)-dy2
            IF (jm1 EQ j) THEN yjm1=y(j)
            jp1=min([j+1,taille(2)-1])
            yjp1=y(j)+dy2
            IF (jp1 EQ j) THEN yjp1=y(j)

            xx=[xim1,xip1,xip1,xim1]
            yy=[yjm1,yjm1,yjp1,yjp1]
            IF firstx EQ 1 THEN POLYFILL,xx,yy, COLOR=COLORf(I,J),NOCLIP=0,  $
               CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
            ENDIF

            ENDIF
            ENDFOR
         ENDFOR
         END
      3:BEGIN
         c_numlev=numlevels
         MakeLevels,mindata,maxdata,truemin,truemax
         IF contoption EQ 0 AND kount  EQ 0 THEN BEGIN 
            CONTOUR,tablep,x,y,xstyle=5,ystyle=5,/FOLLOW, $
               TITLE=tit,LEVELS=lev,/NOERASE,XRANGE=rangex,YRANGE=rangey, $ ; C_LABELS=[0] ,$
               POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]  
         ENDIF ELSE BEGIN 
            CONTOUR,tablep,x,y,TRIANGULATION=tri, $
               xstyle=5,ystyle=5, LEVELS=lev,/NOERASE,TITLE=title, $
               XRANGE=rangex,YRANGE=rangey,MAX_VALUE=maxval, $
               NOCLIP=0,CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)], $
               POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         ENDELSE
         END
      4:PLOT,[x(0)],[y(0)],xstyle=5,ystyle=5,/NODATA, $
         XRANGE=rangex,YRANGE=rangey, /NOERASE,TITLE=title, $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]

      5:BEGIN     ; makes a filled polar contour plot
         IF logplot THEN BEGIN
            IF (kountplus GE 1) THEN tablep(indexplus)=ALOG10(tablep(indexplus))
            IF (kountminus GE 1) THEN tablep(indexminus)=ALOG10(-1.*tablep(indexminus))
            IF (kountzero GE 1) THEN tablep(indexzero)=-30.
            indexsmall=WHERE(tablep LT -30., kountsmall)
            IF (kountsmall GE 1) THEN tablep(indexsmall)=-30.
         ENDIF
         c_numlev=numlevels   ;number of contour levels
         MakeLevels,mindata,maxdata,truemin,truemax
         colorind=1+FINDGEN(c_numlev-1+addmini+addmaxi) $
            *(Navailcolor-5)/(c_numlev+addmini+addmaxi-2)
         IF swap_palette THEN colorind=navailcolor-1-colorind
         ;print,'lev',lev
         ;print,'colorind',colorind
         ;print,'tablep',tablep(*,25)
         yrad=(90.-y)*!dtor
         PLOT,[x(0)],[y(0)],xstyle=4,ystyle=5,/NODATA, $
         XRANGE=rangex,YRANGE=rangey, /NOERASE,$
         XTICK_GET=Radii, $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         rangex=[-rangex(1),rangex(1)]
         rangey=[-rangex(1),rangex(1)]

         Rescale,1
         POLAR_CONTOUR,TRANSPOSE(tablep),yrad,x,/FILL,XRANGE=rangex,$
            TITLE=title,YRANGE=rangey,POSITION=[blx*winx/mwinx,bly*winy/mwiny, $
            trx*winx/mwinx,try*winy/mwiny],xstyle=5,ystyle=5, $
            C_COLOR=colorind(1-filloutofrange*addmini: $
            numlevels-2+filloutofrange*(addmini+addmaxi)), $
            LEVELS=lev(1-filloutofrange*addmini: $
            numlevels-1-(1-filloutofrange)*addmaxi), $
            MAX_VALUE=maxval,/NOERASE
         IF (logplot EQ 1) THEN $
            IF (kountplus GE 1) AND (kountminus GE 1) THEN BEGIN

               POLAR_CONTOUR,TRANSPOSE(table2),yrad,x2,XRANGE=rangex,$
                  YRANGE=rangey, $
                  POSITION=[blx*winx/mwinx,bly*winy/mwiny, $
                  trx*winx/mwinx,try*winy/mwiny],xstyle=5,ystyle=5, $
                  LEVELS=[0],/NOERASE,C_LABEL=[0],THICK=2

               XX=FLTARR(taille(2),taille(1)+1)
               YY=FLTARR(taille(2),taille(1)+1)
               FOR I=0,taille(1) DO XX(*,I)=COS(yrad(*)*!dtor)*x2(I)
               FOR I=0,taille(1) DO YY(*,I)=SIN(yrad(*)*!dtor)*x2(I)
            ENDIF
         FOR I=0,N_ELEMENTS(Radii)-1 DO IF Radii(I) LE rangex(1) THEN BEGIN
            acirc=FLTARR(N_ELEMENTS(yrad)+1)
            acirc(0:N_ELEMENTS(yrad)-1)=yrad
            acirc(N_ELEMENTS(yrad))=yrad(0)
            OPLOT,Radii(I)*COS(acirc),Radii(I)*SIN(acirc),LINESTYLE=1, $
            NOCLIP=0,CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
            XYOUTS,-Radii(I),0,STRING(Radii(I),FORMAT='(f4.2)'),ALIGNMENT=0.5
         ENDIF
         FOR I=0,5 DO OPLOT,rangex*COS(!pi*I/6.),rangex*SIN(!pi*I/6.), $
            LINESTYLE=((1+(I MOD 3))/2)
         if cbar THEN ColorBar,lev,cbtit,truemin,truemax
         PLOT,[x(0)],[y(0)],xstyle=5,ystyle=5,/NODATA, $
         XRANGE=rangex,YRANGE=rangey,/NOERASE, $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         END

      6:BEGIN
         c_numlev=numlevels
         MakeLevels,mindata,maxdata,truemax,truemin
         yrad=(90.-y)*!dtor
         PLOT,[x(0)],[y(0)],xstyle=4,ystyle=4,/NODATA, $
         XRANGE=rangex,YRANGE=rangey, /NOERASE,$
         XTICK_GET=Radii, $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         rangex=[-rangex(1),rangex(1)]
         rangey=[-rangex(1),rangex(1)]
         Rescale,1
         IF logplot THEN BEGIN
            IF (kountplus GE 1 and kountminus GE 1) THEN $
            POLAR_CONTOUR,TRANSPOSE(tablep),yrad,x,XRANGE=rangex,$
               TITLE=title,YRANGE=rangey, $
               POSITION=[blx*winx/mwinx,bly*winy/mwiny, $
               trx*winx/mwinx,try*winy/mwiny],xstyle=5,ystyle=5, $
               LEVELS=[0],/NOERASE,TITLE=title,C_LABEL=[1],THICK=3

            tableplot=tablep
            tableplot(*,*)=2.*maxval
            IF (kountplus GT 1) THEN BEGIN
               tableplot(indexplus)=ALOG10(tablep(indexplus))
               POLAR_CONTOUR,TRANSPOSE(tableplot),yrad,x,XRANGE=rangex,$
                  YRANGE=rangey,MAX_VALUE=maxval, $
                  POSITION=[blx*winx/mwinx,bly*winy/mwiny, $
                  trx*winx/mwinx,try*winy/mwiny],xstyle=5,ystyle=5, $
                  LEVELS=lev,/NOERASE,TITLE=title,C_LABEL=[0, 1]
            ENDIF
            tableplot(*,*)=2.*maxval
            IF (kountminus GT 1) THEN BEGIN
               tableplot(indexminus)=ALOG10(tablep(-indexminus))
               POLAR_CONTOUR,TRANSPOSE(tableplot),yrad,x,XRANGE=rangex,$
                  YRANGE=rangey,MAX_VALUE=maxval, $
                  POSITION=[blx*winx/mwinx,bly*winy/mwiny, $
                  trx*winx/mwinx,try*winy/mwiny],xstyle=5,ystyle=5, $
                  LEVELS=lev,/NOERASE,TITLE=title,C_LABEL=[0, 1],LINESTYLE=1
            ENDIF
         ENDIF ELSE BEGIN
            taille=SIZE(tablep)
            table2=FLTARR(taille(1)+1,taille(2))+2.*maxval
            table2(1:taille(1),*)=tablep      ;adds one value for R so that
                                    ;there will be no contours around R=0
            linestyles=FIX(lev)*0.
            linesthick=FIX(lev)*0.+1
            labels=FIX(lev)*0.

            Index=WHERE(lev LT 0,kount)
            IF kount GE 1 THEN linestyles(index)=2.


            POLAR_CONTOUR,TRANSPOSE(table2),yrad,x2,XRANGE=rangex,$
               YRANGE=rangey, $
               POSITION=[blx*winx/mwinx,bly*winy/mwiny, $
               trx*winx/mwinx,try*winy/mwiny],xstyle=5,ystyle=5, $
               LEVELS=lev,C_LINESTYLE=linestyles,C_THICK=[2], $
               /NOERASE,TITLE=title,C_LABEL=[1],MAX_VALUE=maxval,C_CHARSIZE=1.2
            POLAR_CONTOUR,TRANSPOSE(table2),yrad,x2,XRANGE=rangex,$
               YRANGE=rangey, $
               POSITION=[blx*winx/mwinx,bly*winy/mwiny, $
               trx*winx/mwinx,try*winy/mwiny],xstyle=5,ystyle=5, $
               LEVELS=[0],C_LINESTYLE=[0],C_THICK=[4],C_CHARTHICK=[2], $
               /NOERASE,TITLE=title,C_LABEL=[1],MAX_VALUE=maxval,C_CHARSIZE=1.2

         ENDELSE
         adpolarstuff=0
         IF adpolarstuff THEN BEGIN
            XYOUTS,Rangex(0),Rangey(0),'Minimum: '+STRING(truemin,FORMAT='(E10.2)'), $
               ALIGNMENT=0.
            XYOUTS,Rangex(1),Rangey(0),'Maximum: '+STRING(truemax,FORMAT='(E10.2)'), $
                 ALIGNMENT=1.
            FOR I=0,N_ELEMENTS(Radii)-1 DO BEGIN
               IF Radii(I) LE rangex(1) THEN BEGIN
                  acirc=FLTARR(N_ELEMENTS(yrad)+1)
                  acirc(0:N_ELEMENTS(yrad)-1)=yrad
                  acirc(N_ELEMENTS(yrad))=yrad(0)
                  OPLOT,Radii(I)*COS(acirc),Radii(I)*SIN(acirc),LINESTYLE=1, $
                  NOCLIP=0,CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
                  XYOUTS,-Radii(I),0,STRING(Radii(I),FORMAT='(f4.2)'),ALIGNMENT=0.5
               ENDIF
           ENDFOR
            FOR I=0,5 DO OPLOT,rangex*COS(!pi*I/6.),rangex*SIN(!pi*I/6.), $
               LINESTYLE=((1+(I MOD 3))/2)
         ENDIF
         END
   7:BEGIN     ; makes a filled polar contour plot using POLYFILL
      taille=size(tablep)
      c_numlev=Navailcolor-4
      MakeLevels,mindata,maxdata,truemin,truemax
      colorind=1+FINDGEN(navailcolor-5+addmini+addmaxi)
      PLOT,[x(0)],[y(0)],xstyle=4,ystyle=5,/NODATA, $
         XRANGE=rangex,YRANGE=rangey, /NOERASE,$
         XTICK_GET=Radii, $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         rangex=[-rangex(1),rangex(1)]
         rangey=[-rangex(1),rangex(1)]
         Rescale,1

         PLOT,[x(0)],[y(0)],xstyle=5,ystyle=5,/NODATA, $
         XRANGE=rangex,YRANGE=rangey,/NOERASE, $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         yrad=(90.-y)*!dtor
         da=yrad(1)-yrad(0)
         FOR i=0,taille(1)-1 DO BEGIN
            IF I EQ 0 THEN dr1=(x(1)-x(0))/2. ELSE dr1=(x(i)-x(i-1))/2.
            IF I EQ taille(1)-1 THEN dr2=(x(taille(1)-1)-x(taille(1)-2))/2. $
                                ELSE dr2=(x(i+1)-x(i))/2.

            IF (x(I) LE rangex(1)) AND (dr1> 0) and (dr2> 0) THEN BEGIN
            FOR J=0,taille(2)-1 DO BEGIN
               x1=(x(i)-dr1)*cos(yrad(J))
               y1=(x(i)-dr1)*sin(yrad(J))
               x1=(x(i)-dr1)*cos(yrad(J))
               y1=(x(i)-dr1)*sin(yrad(J))
               dy=y(1)-y(0)
               xx=[(x(i)-dr1)*cos(yrad(J)-da/2),(x(i)+dr2)*cos(yrad(J)-da/2), $
                     (x(i)+dr2)*cos(yrad(J)+da/2),(x(i)-dr1)*cos(yrad(J)+da/2)]
               yy=[(x(i)-dr1)*sin(yrad(J)-da/2),(x(i)+dr2)*sin(yrad(J)-da/2), $
                     (x(i)+dr2)*sin(yrad(J)+da/2),(x(i)-dr1)*sin(yrad(J)+da/2)]
                  COLORf=1+(Navailcolor-5)*(tablep(i,J)-mindata)/(maxdata-mindata)
               IF COLORF LT 1 THEN COLORF=1
               IF COLORF GT Navailcolor-4 THEN COLORF=Navailcolor-4
               POLYFILL,xx,yy, $
                  COLOR=COLORf
            ENDFOR
            ENDIF
         ENDFOR
         ;XYOUTS,Rangex(0),Rangey(0),'Minimum: '+STRING(truemin,FORMAT='(E10.2)'), $
         ;   ALIGNMENT=0.
         ;XYOUTS,Rangex(1),Rangey(0),'Maximum: '+STRING(truemax,FORMAT='(E10.2)'), $
         ;   ALIGNMENT=1.
         FOR I=0,N_ELEMENTS(Radii)-1 DO IF Radii(I) LE rangex(1) THEN BEGIN
            acirc=FLTARR(N_ELEMENTS(yrad)+1)
            acirc(0:N_ELEMENTS(yrad)-1)=yrad
            acirc(N_ELEMENTS(yrad))=yrad(0)
            OPLOT,Radii(I)*COS(acirc),Radii(I)*SIN(acirc),LINESTYLE=1, $
            NOCLIP=0,CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
            XYOUTS,-Radii(I),0,STRING(Radii(I),FORMAT='(f4.2)'),ALIGNMENT=0.5
         ENDIF
         FOR I=0,5 DO OPLOT,rangex*COS(!pi*I/6.),rangex*SIN(!pi*I/6.), $
            LINESTYLE=((1+(I MOD 3))/2)
         if cbar THEN ColorBar,lev,cbtit,truemin,truemax
         PLOT,[x(0)],[y(0)],xstyle=5,ystyle=5,/NODATA, $
         XRANGE=rangex,YRANGE=rangey,/NOERASE, $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         END

      8:BEGIN
         pfontmemo=!P.font
         !P.font=-1
         IF outxtit THEN xti=xtit ELSE xti=' '
         IF outytit THEN yti=ytit ELSE yti=' '
         IF outcbtit THEN cbti=cbtit ELSE cbti=' '
         SURFACE,(1-2*swap_palette)*tablep,x,y,AX=Ax3D,AZ=Az3D,XSTYLE=1,YSTYLE=1, $
            XTITLE=xti,YTITLE=yti,TITLE=ti,MAX_VALUE=maxval
         !P.font=pfontmemo
         END

      9:BEGIN
         c_numlev=Navailcolor-4
         MakeLevels,mindata,maxdata,truemin,truemax
         toto=size(tablep)
         image=1+ $
            BYTSCL(tablep,min=mindata,max=maxdata,Top=(navailcolor-3))
            colorind=1+FINDGEN(navailcolor-5+addmini+addmaxi)
         IF swap_palette THEN  BEGIN
            image=navailcolor-2-image
            colorind=navailcolor-2-colorind
         ENDIF
         if cbar THEN ColorBar,lev,cbtit,truemin,truemax
         index1=WHERE(tablep GT maxdata,count1)
         index2=WHERE(tablep LT mindata,count2)
         IF count1 GT 0 THEN tablep(index1)=maxdepth
         IF count2 GT 0 THEN tablep(index2)=mindepth
         IF (smoothing GT 1) THEN tablep=SMOOTH(tablep,smoothing)
         pfontmemo=!P.font
         !P.font=-1
         IF outxtit THEN xti=xtit ELSE xti=' '
         IF outytit THEN yti=ytit ELSE yti=' '
         IF outcbtit THEN cbti=cbtit ELSE cbti=' '
         SHADE_SURF,(1-2*swap_palette)*tablep,x,y,shades=image,xstyle=1,ystyle=1, $
            XTITLE=xti,/NOERASE,MAX_VALUE=maxval, $
            YTITLE=yti, TITLE=ti,$
            XRANGE=rangex,YRANGE=rangey,/SAVE,Ax=ax3D,Az=az3D, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         !P.font=pfontmemo
            END

      10:BEGIN
         index1=WHERE(tablep GT maxdata,count1)
         index2=WHERE(tablep LT mindata,count2)
         IF count1 GT 0 THEN tablep(index1)=maxdepth
         IF count2 GT 0 THEN tablep(index2)=mindepth
         IF (smoothing GT 1) THEN tablep=SMOOTH(tablep,smoothing)
         pfontmemo=!P.font
         !P.font=-1
         IF outxtit THEN xti=xtit ELSE xti=' '
         IF outytit THEN yti=ytit ELSE yti=' '
         IF outcbtit THEN cbti=cbtit ELSE cbti=' '

         SHADE_SURF,(1-2*swap_palette)*tablep,x,y,xstyle=1,ystyle=1,/SAVE,Ax=ax3D,Az=az3D, $
            XTITLE=xti,/NOERASE,MAX_VALUE=maxval, $
            YTITLE=yti, TITLE=ti,$
            XRANGE=rangex,YRANGE=rangey, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         !P.font=pfontmemo
         END
      11:BEGIN
         pfontmemo=!P.font
         !P.font=-1
         IF outxtit THEN xti=xtit ELSE xti=' '
         IF outytit THEN yti=ytit ELSE yti=' '
         IF outcbtit THEN cbti=cbtit ELSE cbti=' '

         SURFACE,(1-2*swap_palette)*tablep,x,y,MAX_VALUE=maxval, $
            AX=Ax3D,AZ=Az3D,/LEGO,XSTYLE=1,YSTYLE=1, $
            XTITLE=xti,YTITLE=yti,ZTITLE=cbti
         !P.font=pfontmemo
         IF outtit THEN ti=tit ELSE ti=' '
         XYOUTS,rangex(0)+0.5*(rangex(1)-rangex(0)), $
            rangey(1)-0.05*(rangey(1)-rangey(0)),ti,ALIGNMENT=0.5
         END
      12:BEGIN ; color bar only
         c_numlev=Navailcolor-4
         MakeLevels,mindata,maxdata,truemin,truemax
         colorind=1+FINDGEN(navailcolor-5+addmini+addmaxi)
            toto=size(tablep)
            IF swap_palette THEN  BEGIN
               colorind=navailcolor-2-colorind
            ENDIF
            if cbar THEN ColorBar,lev,cbtit,truemin,truemax
         END
      13:BEGIN ; collapses the 2D plot into a 1D plot
         taille=size(x)
         y1=TOTAL(tablep,2)*(y(1)-y(0))*!dtor
         print,'Hs:',4.*SQRT(TOTAL(y1*(x(1)-x(0))))
         print,'dtheta:',y(1)-y(0),!dtor
         H=8.
         cg=x
         cg1=0.
         kh=0.
         cphi=0.
         FOR I=0,taille(1)-1 DO BEGIN
            Speeds,H,x(I),cphi,cg1,KH
            cg(I)=cg1
         ENDFOR
         print,'Cg:',cg
         Eflux=0.
         Etot=0.
         FOR I=0,taille(1)-1 DO BEGIN
            Eflux=Eflux+cg(I)*(y(1)-y(0))*!dtor*(x(1)-x(0))*TOTAL(tablep(I,*)*ABS(cos((70-y(*))*!dtor)))
         ENDFOR
         Eflux2=TOTAL(cg*TOTAL(tablep,1)*(y(1)-y(0))*!dtor*ABS(sin((90-y)*!dtor))*(x(1)-x(0)))
         print,'Eflux:',Eflux,Eflux*1026.*9.81,TOTAL(cg*y1*(x(1)-x(0))),Eflux2
         plot,x,y1, $
            xstyle=1,ystyle=0,$
            /NOERASE,TITLE=' ',XTITLE=xtit,YTITLE='E(f) (m^2/Hz)',THICK=3, $
            XRANGE=rangex,YRANGE=[0,max(y1)],MAX_VALUE=maxval, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         END
      ENDCASE
      IF (filltype LE 3) AND (adcontour GT 0) THEN BEGIN ; Adds the contour lines corresponding to
      lev2=mindata+(maxdata-mindata)*FINDGEN(cbnticks+1)/(cbnticks)
      print,'LEV2:',lev2,'##',adcontour
      Cstyle=FLTARR(cbnticks+1)+adcontour-1
      IF contoption EQ 0 AND MAPFLAG EQ 0 THEN $
         CONTOUR,tablep,x,y,xstyle=5,ystyle=5,/FOLLOW, $
            TITLE=tit,$
            LEVELS=lev2,C_LINESTYLE=Cstyle,/NOERASE,XRANGE=rangex,YRANGE=rangey, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny] $
      ELSE $
         CONTOUR,tablep,x,y,/IRREGULAR, $
            xstyle=5,ystyle=5,/FOLLOW, $
            LEVELS=lev2,C_LINESTYLE=Cstyle,/NOERASE,TITLE=title, $
            XRANGE=rangex,YRANGE=rangey,MAX_VALUE=maxval, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
      ENDIF
      IF MAPFLAG EQ 1 THEN BEGIN
        IF (adcontour) THEN BEGIN 
         c_numlev=numlevels
           MakeLevels,mindata,maxdata,truemin,truemax
           colorind=1+FINDGEN(c_numlev-1+addmini+addmaxi) $
            *(Navailcolor-5)/(c_numlev+addmini+addmaxi-2)

           CONTOUR,tablep,rlonmin+x*(rlonmax-rlonmin)/max(x),rlatmin+y*(rlatmax-rlatmin)/max(y),xstyle=5,ystyle=5,/overplot,/FOLLOW, $
            LEVELS=lev
        ENDIF
      IF MAPCONTINENT GT 0 THEN MAP_CONTINENTS, FILL=MAPCONTINENT-1
      MAP_GRID,LATDEL=10, LONDEL=10,LONS=-170,LABEL=1
      if cbar THEN ColorBar,lev,cbtit,truemin,truemax
         IF (title NE '') THEN XYOUTS,blx,try,title,/NORMAL
            ENDIF  
     ENDIF ELSE BEGIN 
       PRINT,'Array is constant:',mindata
     ENDELSE

RETURN
END

;----------------------------------------------------------------------------
PRO Makelevels,mini,maxi,truemin,truemax
; Generates an increasing series of values from mini to maxi, used for
; contouring and the color bar. The true min and max are also used for cases
; in which the range of values of the displayed data goes beyond mini and maxi
; in that case the color bar may be extended by triangles to indicate that the
; range shown does not cover all the data (the parameters addmini and addmaxi
; are passed in the COLORBARPAR common block to the Colorbar procedure)
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON COLORBARPAR,cbnticks,cbexrange,cbtrx,cbtry,cbblx,cbbly, $
   filloutofrange,addmini,addmaxi
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
   addmini=0
   addmaxi=0
   IF (truemin LT mini) AND (fixrange GE 0) THEN addmini=1
   IF (truemax GT maxi) AND (fixrange GE 0) THEN addmaxi=1
   lev=FLTARR(c_numlev+addmini+addmaxi)
   IF addmini THEN lev(0)=truemin-0.00001*(maxi-truemin)
   IF addmaxi THEN lev(c_numlev+addmini+addmaxi-1)=truemax+0.00001*(truemax-mini)
   ;print,'C_numlev:',c_numlev,addmini
   CASE C_repart OF
   0: BEGIN
      lev(addmini:c_numlev+addmini-1)=mini+(maxi-mini)*FINDGEN(c_numlev)/(c_numlev-1)
      END
   1: BEGIN
      toto=EXP(2*(FINDGEN(c_numlev)+1-c_numlev)/c_numlev)
      mini2=toto(0)
      lev(addmini:c_numlev+addmini-1)=mini+(maxi-mini)/(1-mini2) $
      *(EXP(2*(FINDGEN(c_numlev)+1-numlevels)/c_numlev)-mini2)
      END
   2: BEGIN
      toto=EXP(6.*(FINDGEN(c_numlev)+1-c_numlev)/c_numlev)
      mini2=toto(0)
      lev(addmini:c_numlev+addmini-1)=mini+(maxi-mini)/(1-mini2) $
      *(EXP(6.*(FINDGEN(c_numlev)+1-c_numlev)/c_numlev)-mini2)
      END
   3: BEGIN
      toto=EXP(5*(FINDGEN(c_numlev)+1-numlevels)/c_numlev)
      mini2=toto(0)
      lev(addmini:c_numlev+addmini-1)=mini+(maxi-mini)/(1-mini2) $
      *(EXP(5*(FINDGEN(c_numlev)+1-c_numlev)/c_numlev)-mini2)
      END
   4: BEGIN
      IF maxi LT 1.2 THEN BEGIN
         lev(addmini:c_numlev+addmini-1)=mini+(maxi-mini)*FINDGEN(c_numlev)/(c_numlev-1)
      ENDIF ELSE BEGIN
         n1=c_numlev/2
         n2=2*n1
         lev(addmini:c_numlev+addmini-1)=mini+(1.2-mini)*FINDGEN(c_numlev)/(n1-1)
         lev(n1:c_numlev-1+addmini)=1.2+ $
            (maxi-1.2)*(1.+FINDGEN(c_numlev-n1))/(c_numlev-n1)
      ENDELSE
      END
   5: BEGIN
      logmax=FIX(MAX([ALOG10(abs(maxi)),ALOG10(abs(mini))]))+1
      lev(addmini:c_numlev+addmini-1)=mini+(maxi-mini)*FINDGEN(c_numlev)/(c_numlev-1)
      END
   ENDCASE
RETURN
END


;----------------------------------------------------------------------------
PRO VECT,u,v,x,y,miss,rangex,rangey
   s = size(u)

   r = .3            ;len of arrow head
   angle = 22.5 * !dtor      ;Angle of arrowhead
   st = r * sin(angle)      ;sin 22.5 degs * length of head
   ct = r * cos(angle)
; arrow
   xmotif=[0.,1.,1.-ct,1.,1.-ct]*(rangex(1)-rangex(0))/40.
   ymotif=[0.,0.,st,0.,-st]*(rangex(1)-rangex(0))/40.

   mag=SQRT(u^2+v^2)
   u0=u/mag
   v0=v/mag
   Index=WHERE(miss EQ 0 and x GT rangex(0)  AND x LT rangex(1) $
      AND  y GT rangey(0)  AND y LT rangey(1),kount)
   IF (kount GT 0) AND (kount LT 10000) THEN BEGIN
   FOR i=0,kount-1 DO BEGIN
         PLOTS,x(Index(I))+xmotif*u0(Index(I))-ymotif*v0(Index(I)), $
            y(Index(I))+xmotif*v0(Index(I))+ymotif*u0(Index(I)),THICK=2
   ENDFOR
   ENDIF
END

;----------------------------------------------------------------------------
PRO ad2Dmisc
;**********DESCRIPTION****
; This procedure overlays sympols and lines over 2d plots.
; Symbols can be grid points, special points, and arrows for wave direction
; lines can be bathymetry contours, rays, survey lines, coastline
; The flags governing the display (or not) of these are contain in the
; PLOTOP group of common variables
; This procedure is called by doplotothers after a backgroung plot
; has been produced. The coordinates for all symbols are in kilometers from the
; bottom left corner of the bathymetry grid.
;** 1 ** Display parameters
COMMON COLORBARPAR,cbnticks,cbexrange,cbtrx,cbtry,cbblx,cbbly, $
   filloutofrange,addmini,addmaxi
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON OVERLAY, addir,adsyms,adbathy,adcoast,psyms,psymsizes,adtr,adtri
COMMON OVERLAY2,adbathydot,adcontour
COMMON POSTSCRIPT, filep,pspath,prcoul,psor,pstype, $
                pwinx,pwiny,papierx,papiery,xoffset,yoffset, $
                facpolice,fontrescale,basefontsize,pssizex,pssizey,psfont
COMMON TITLES,  font,outtit,outxtit,outytit,outcbtit,textx,texty,textdx,textdy
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth

;** 3 ** I/O and data variables
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON COAST,   coastxy,coastl,coastnp,GSHHSPoly,GSHHSPoint,GSHHSPoly2,GSHHSPoint2
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON TRIANGLE2,nzonetri,zonetri,nzonegp,zonegp,ngpzone,gpzone
COMMON RAYS,    raysOK,raystype,raynsteps,rayx,rayy,raya,rayamin,rayamax, $
                rayres,rayfreq,rayGP,raytimestep,rayoffdep,rayflag,raydz,raymindepth
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
COMMON TIMESTEPSUG,TIMESTEPS,NTIMESTEPS,FLAGTSTEP, TIMESTEPS_ORDER
COMMON TRANSECT,Ntrans,Strans,Xtrans,Ytrans,Ztrans,Itrans, $
                  COtrans,TransOK,transsym,transline,transthick,Ispectrans, $
                  spectransname,ntransgp,transsymsize
COMMON MAPPOLE, XPOLE, YPOLE, SLAT, RE, E2, E

COMMON XYZDATA, NXYZ, XYZ
;*******END OF COMMON BLOCKS*******************************

;
; Plots xyz.dat
;
      IF datastatus(1) THEN BEGIN 

        index=WHERE(XYZ(*,2) GT maxval,kount)
        table2=XYZ(*,2)
        truemin=min(XYZ(*,2))
        truemax=max(table2)
   IF fixrange THEN BEGIN
      mindata=mindepth
      maxdata=maxdepth
   ENDIF ELSE BEGIN
      maxdata=MIN([truemax,maxdepth])
      mindata=MAX([truemin,mindepth])
   ENDELSE

      c_numlev=Navailcolor-4
      MakeLevels,mindata,maxdata,truemin,truemax
      colorind=1+FINDGEN(navailcolor-5+addmini+addmaxi)
      c_numlev=Navailcolor-4

      FOR I=0,c_numlev+addmini+addmaxi-2 DO BEGIN 
        IND=WHERE(XYZ(*,2) GE lev(I) AND XYZ(*,2) LE lev(I+1),kount)
        IF (kount GT 0) THEN  OPLOT,XYZ(IND,0),XYZ(IND,1),psym=3, $  ; 
               linestyle=0,SYMSIZE=1,COLOR=I
       ENDFOR

      ;ENDFOR

      ENDIF
;
; Adds shoreline
;
      IF datastatus(4) OR datastatus(18) THEN BEGIN
         taille=SIZE(GSHHSPoly)
         Npoly=taille(1)-1
         CASE adcoast OF
         1: BEGIN
            FOR I=0L,Npoly-1 DO BEGIN
               IF GSHHSPoly(I,0) LT GSHHSPoly(I+1,0)-1 THEN BEGIN
                  COLORI=253.+(GSHHSPoly(I,3) MOD 2)
                  POLYFILL,GSHHSPoint(GSHHSPoly(I,0):GSHHSPoly(I+1,0)-1,0), $
                  GSHHSPoint(GSHHSPoly(I,0):GSHHSPoly(I+1,0)-1,1), $
                  COLOR=253.+(GSHHSPoly(I,3) MOD 2), $
                              NOCLIP=0, $
                  CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
                  drx=0; (rangex(1)-rangex(0))*0.25
                  dry=0;(rangey(1)-rangey(0))*0.25
                  ;,COLOR=Navailcolor-1 $
               ENDIF
            ENDFOR
              outdump=0
              IF (outdump EQ 1) THEN BEGIN 
                 GET_LUN,unit2
                 OPENW,unit2,'GSHHS_v2_islands_in_zoom.txt'
                 FOR I=0L,Npoly-1 DO BEGIN 
                    XBOUNDS=(((FLOAT(GSHHSPoly(I,4:5))*1.E-6+180.) MOD 360)-180-rlonmin)/(ABS(rlonmax-rlonmin)/((nx-1)*dx))
                    YBOUNDS=  (FLOAT(GSHHSPoly(I,6:7))*1.E-6 -rlatmin)/(ABS(rlatmax-rlatmin)/((ny-1)*dy))
                    IF (  XBOUNDS(0) GT rangex(0) AND XBOUNDS(1) LT rangex(1) AND $
                          YBOUNDS(0) GT rangey(0) AND YBOUNDS(1) LT rangey(1) ) THEN BEGIN 
                       PRINTF,unit2,FORMAT='(2I10)',GSHHSPoly(I+1,0)-GSHHSPoly(I,0), GSHHSPoly(I,1)
                       FOR J=GSHHSPoly(I,0),GSHHSPoly(I+1,0)-1 DO BEGIN
                          PRINTF,unit2,FORMAT='(2F11.6)', rlonmin+GSHHSPoint(J,0)*ABS(rlonmax-rlonmin)/((nx-1)*dx), $
                                                          rlatmin+GSHHSPoint(J,1)*ABS(rlatmax-rlatmin)/((ny-1)*dy)
                       ENDFOR
                    ENDIF
                 ENDFOR
                 CLOSE,unit2
                 FREE_LUN,unit2
              ENDIF 
            END
         -1:BEGIN
            FOR I=0L,Npoly-1 DO BEGIN
            PLOTS,GSHHSPoint(GSHHSPoly(I,0):GSHHSPoly(I+1,0)-1,0)+0*dx, $
               GSHHSPoint(GSHHSPoly(I,0):GSHHSPoly(I+1,0)-1,1)-0*dy, $
               COLOR=253.+(GSHHSPoly(I,3) MOD 2),NOCLIP=0, THICK=2, $
               CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
               IF (rangex(1)-rangex(0) LT 2 ) AND (adsyms GT 3) THEN BEGIN 
                  STRA=STRARR(2)
                  STRA(0)=strcompress(string(I+1L))
                  FOR J=GSHHSPoly(I,0),GSHHSPoly(I+1,0)-1 DO BEGIN
                     IF (adsyms GT 3 AND GSHHSPoint(J,0) GT rangex(0) and GSHHSPoint(J,0) LT rangex(1) $
                                     AND GSHHSPoint(J,1) GT rangey(0) AND GSHHSPoint(J,1) LT rangey(1) )  THEN BEGIN
                        STRA(1)=strcompress(string(1L+J-GSHHSPoly(I,0)))
                        strlab=STRJOIN(STRA,',')
                        XYOUTS,GSHHSPoint(J,0),GSHHSPoint(J,1),strlab, $
                           CHARSIZE=0.5,NOCLIP=0,COLOR=253,  $
                           CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
                     ENDIF
                  ENDFOR
               ENDIF         
            ENDFOR
            END
         -2:BEGIN
            FOR I=0L,Npoly-1 DO BEGIN
               IF (GSHHSPoly(I,3) MOD 2 EQ 1) THEN $
               PLOTS,GSHHSPoint(GSHHSPoly(I,0):GSHHSPoly(I+1,0)-1,0)+0*dx, $
                  GSHHSPoint(GSHHSPoly(I,0):GSHHSPoly(I+1,0)-1,1)-0*dy, $
                  COLOR=253.+(GSHHSPoly(I,3) MOD 2),NOCLIP=0, THICK=2, $
                  CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
             ENDFOR
            END
         0:BEGIN
           END
         ENDCASE
      ENDIF

      IF datastatus(4)  THEN BEGIN
         taille=SIZE(GSHHSPoly2)
         Npoly=taille(1)-1
        CASE adcoast OF
         1: BEGIN
            FOR I=0L,Npoly-1 DO BEGIN
            IF GSHHSPoly2(I,0) LT GSHHSPoly2(I+1,0)-1  THEN BEGIN
               POLYFILL,GSHHSPoint2(GSHHSPoly2(I,0):GSHHSPoly2(I+1,0)-1,0)+dx*0, $
               GSHHSPoint2(GSHHSPoly2(I,0):GSHHSPoly2(I+1,0)-1,1)+dy*0, $
               COLOR=Navailcolor-2, NOCLIP=0, $
               CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
               drx=0; (rangex(1)-rangex(0))*0.25
               dry=0;(rangey(1)-rangey(0))*0.25
               ;COLOR=252.+(GSHHSPoly2(I,3) MOD 2), $
               ;                              
            ENDIF
            ENDFOR
            END
         -1:BEGIN
            FOR I=0L,Npoly-1 DO BEGIN
               PLOTS,GSHHSPoint2(GSHHSPoly2(I,0):GSHHSPoly2(I+1,0)-1,0)+0*dx, $
                  GSHHSPoint2(GSHHSPoly2(I,0):GSHHSPoly2(I+1,0)-1,1)-0*dy, $
                  COLOR=251.+(GSHHSPoly2(I,3) MOD 2),NOCLIP=0, THICK=2, $
                  CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
               IF (rangex(1)-rangex(0) LT 2 ) AND (adsyms GT 3) THEN BEGIN 
                  STRA=STRARR(2)
                  STRA(0)=strcompress(string(I+1L))
                  FOR J=GSHHSPoly2(I,0),GSHHSPoly2(I+1,0)-1 DO BEGIN
                     STRA(1)=strcompress(string(1L+J-GSHHSPoly2(I,0)))
                     strlab=STRJOIN(STRA,',')
                     IF (adsyms GT 3 AND GSHHSPoint2(J,0) GT rangex(0) and GSHHSPoint2(J,0) LT rangex(1) $
                         and GSHHSPoint2(J,1) GT rangey(0) and GSHHSPoint2(J,1) LT rangey(1) ) $
                         THEN XYOUTS,GSHHSPoint2(J,0),GSHHSPoint2(J,1)+0.002*(I MOD 2L),strlab, $
                     CHARSIZE=0.5,NOCLIP=0,COLOR=252,  $
                     CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
                  ENDFOR
               ENDIF         
            ENDFOR
            END
         ELSE:BEGIN
           END
         ENDCASE
      ENDIF


;
;  Displays the triangle mesh
;
   IF adtri GE 1 AND adtri LE 2 AND datastatus(6) THEN BEGIN
      IIGP=INTARR(3)
      FOR I=1L,ntri DO BEGIN
;
; Gets indices of the three points in the triangle
;
            I1=TRIGP(I,1)-1
            I2=TRIGP(I,2)-1
            I3=TRIGP(I,3)-1
            IF ((I1 NE -1) AND (I2 NE -1) AND (I3 NE -1)) THEN BEGIN
               X=[gridmat(0,I1),gridmat(0,I2),gridmat(0,I3),gridmat(0,I1)]
               Y=[gridmat(1,I1),gridmat(1,I2),gridmat(1,I3),gridmat(1,I1)]
                 PLOTS,X,Y,NOCLIP=0, CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)],THICK=1 ;,COLOR=250
;
; Plots the median dual cells 
;
               IF adtri EQ 2 THEN BEGIN 
                 XG=MEAN(X(0:2))
                 YG=MEAN(Y(0:2))
                 PLOTS,XG,YG,PSYM=3,SYMSIZE=1,NOCLIP=0,CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
                 OPLOT,[0.5*(X(0)+X(1)),XG,0.5*(X(1)+X(2)),XG,0.5*(X(2)+X(0))],  $
                       [0.5*(Y(0)+Y(1)) ,YG,0.5*(Y(2)+Y(1)) ,YG ,0.5*(Y(0)+Y(2))],LINESTYLE=1, $
                      NOCLIP=0, CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)],THICK=1
               ENDIF    
            ENDIF
     ENDFOR
     ENDIF

   IF adtri EQ 3 AND datastatus(6) THEN BEGIN
      ALLIND = UNIQ(SORT( [TRIGP(0:NTRI,1), TRIGP(0:NTRI,2), TRIGP(0:NTRI,3)] -1));
      X=[gridmat(0,ALLIND)]
      Y=[gridmat(1,ALLIND)]
      PLOTS,X,Y,LINESTYLE=-1,PSYM=8,SYMSIZE=0.3, $
                  NOCLIP=0,CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)] 
  ENDIF


;
;  Draws the mesh contour line
;
     IF  adcoast EQ -1 THEN BEGIN
     OK=0
     IF (n_elements(contourline) GT 5) THEN BEGIN
        NP=n_elements(contourline)
;
; J0 is the index of first point in polygon
;
        J0=contourline(2,0)
        I=0L
        FOR J=2,contourline(1,0) DO BEGIN
           J1=contourline(J,0)
           IF (J LT contourline(1,0)) THEN BEGIN 
             J2=contourline(J+1,0) 
             IF (contourline(J+1,1) LT 0) THEN BEGIN
;
; If this is negative: end of poygon: closing it back to J0
;
               J2=J0
               J0=J-1
             ENDIF
           ENDIF ELSE J2 = J0
           x1=gridmat(0,J1)
           y1=gridmat(1,J1)
           x2=gridmat(0,J2)
           y2=gridmat(1,J2)
           PLOTS,[x1,x2],[y1,y2], $ ;HICK=4, $
               NOCLIP=0,CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)],COLOR=253+(I MOD 2L), THICK=2
           IF (J LT contourline(1,0)) THEN BEGIN 
             IF (contourline(J+1,1) LT 0) THEN I=I+1L
           ENDIF
        ENDFOR
     ENDIF
   ENDIF

   IF datastatus(7) THEN BEGIN
      IF (adsyms GE 2) THEN BEGIN
         NP=8
         A = FINDGEN(NP+1) * (!PI*2/FLOAT(NP))    ;Make a vector of 16 points, A[i] = 2pi/16.
         USERSYM, COS(A), SIN(A), /FILL  ;Define the symbol to be a filled circle
         FOR I=1L,nngp DO  BEGIN
            OPLOT,[gridmat(0,i-1)],[gridmat(1,i-1)], $
               psym=psyms(0),SYMSIZE=psymsizes(0),NOCLIP=0, $
               CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
            strlab=strcompress(string(I))
            IF (adsyms GT 3) THEN XYOUTS,gridmat(0,i-1),gridmat(1,i-1),strlab, $
            CHARSIZE=0.5,NOCLIP=0, $
               CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]
         ENDFOR
      ENDIF

      IF (N_ELEMENTS(nspecgp) GT 0) THEN BEGIN 
      IF (adsyms MOD 2) AND (nspecgp GT 0) THEN BEGIN
         FOR I=0,nspecgp-1 DO BEGIN
            IF (specmat(7,I) GE 0) THEN BEGIN
               IF specmat(9,I) GT 0 THEN BEGIN
                  OPLOT,[specmat(1,I)],[specmat(2,I)], $
                     PSYM=specmat(9,I),SYMSIZE=specmat(8,I), THICK=specmat(10,I)
               ENDIF ELSE BEGIN
                  OPLOT,specmat(1,I-1:I),specmat(2,I-1:I), $
                     PSYM=specmat(9,I),SYMSIZE=specmat(8,I), $
                     LINESTYLE=specmat(11,I), THICK=specmat(10,I),NOCLIP=0, $
                     CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)];
               ENDELSE
               deltax0=0.
               deltay0=-0.1
               deltax=deltax0*(rangex(1)-rangex(0))/30.
               deltay=deltay0*(rangex(1)-rangex(0))/40.
               ; 1 point is 1/72 US inch or 0.0353 cm
               IF output EQ 1 THEN deltay=deltay0*0.4*specmat(12,I)*basefontsize*0.0353 $
                                            *(rangex(1)-rangex(0))/(FLOAT(pssizex)*(trx-blx))
               IF I MOD 2 THEN BEGIN
                  deltay=-deltay0*(rangex(1)-rangex(0))/20.
                  IF output EQ 1 THEN deltay=-1.4*deltay0*specmat(12,I)*basefontsize*0.0353 $
                     *(rangex(1)-rangex(0))/(FLOAT(pssizex)*(trx-blx))
               ENDIF
               IF specmat(12,I) GT 0 THEN BEGIN
                  xout=specmat(1,I)+deltax
                  yout=specmat(2,I)+deltay
                  IF (xout GT rangex(0)) AND (xout LT rangex(1)) AND $
                     (yout GT rangey(0)) AND (yout LT rangey(1)) THEN $
                  XYOUTS,xout,yout,STRMID(specname(I),0,15),CHARSIZE=specmat(12,I),ALIGNMENT=0. ;, $
               ENDIF
            ENDIF
         ENDFOR
      ENDIF
      ENDIF ; end of test : (N_ELEMENTS(nspecgp) GT 0)
   ENDIF

   IF datastatus(3) AND adbathy GT 0 THEN BEGIN
      x=(findgen(nxzmax-nxzmin+1)+nxzmin)*dx
      y=(findgen(nyzmax-nyzmin+1)+nyzmin)*dy
      CASE adbathy OF
      1:CONTOUR,gd(nxzmin:nxzmax,nyzmin:nyzmax),x,y, $
         XRANGE=rangex,YRANGE=rangey, /NOERASE,$
         XSTYLE=5,YSTYLE=5, C_LINESTYLE=[1,1],C_LABELS=[1,1], $
         LEVELS=[30,50], $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
      2:CONTOUR,gd(nxzmin:nxzmax,nyzmin:nyzmax),x,y, $
         XRANGE=rangex,YRANGE=rangey, /NOERASE,$
         XSTYLE=5,YSTYLE=5, C_LINESTYLE=[0],C_LABELS=[1], $
         LEVELS=[300], $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
      3:CONTOUR,SMOOTH(gd(nxzmin:nxzmax,nyzmin:nyzmax),[10,10]),x,y, $
         XRANGE=rangex,YRANGE=rangey, /NOERASE,$
         XSTYLE=5,YSTYLE=5, C_LINESTYLE=[0],C_LABELS=[1,1,1], $
         LEVELS=[50,80,100], C_THICK=[2], $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
      4:CONTOUR,gd(nxzmin:nxzmax,nyzmin:nyzmax),x,y, $
         XRANGE=rangex,YRANGE=rangey, /NOERASE,$
         XSTYLE=5,YSTYLE=5, C_LINESTYLE=[0],C_LABELS=[0], $
         LEVELS=[0,1000,2000,3000], C_THICK=[1], $
         POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
      ENDCASE
   ENDIF


   IF N_ELEMENTS(raya) NE 0 THEN BEGIN
         nrays=(rayamax-rayamin)/rayres+1
         nray0=(180+rayamin)/rayres
         IF (raystype EQ 1) THEN BEGIN
            N1=nray0
            N2=nray0+nrays-1
         ENDIF ELSE BEGIN
            N1=0
            N2=nrays-1
         ENDELSE
         nstepmax=MAX(raynsteps,Imax)
         FOR I=N1,N2 DO BEGIN
            doit=0
            CASE RaysOK OF
            0:doit=1
            1:IF rayflag(I) THEN doit=1
            2:IF rayflag(I) THEN doit=0
            3:doit=0
            ENDCASE
            IF doit THEN BEGIN
            x0=[rayx(I,0)]
            y0=[rayy(I,0)]
            ;OPLOT,x0,y0,psym=1
            FOR J=1,raynsteps(I)-1 DO BEGIN
               IF (rayx(I,J) GT rangex(0) and rayx(I,J) LT rangex(1) and  $
                rayy(I,J) GT rangey(0) and rayy(I,J) LT rangey(1)) THEN BEGIN 
                
               OPLOT,[x0,rayx(I,J)],[y0,rayy(I,J)],THICK=4, $ ;psym=-1, $
               linestyle=0,SYMSIZE=0.15 ;,COLOR=2+((J/10)MOD 8)*(Navailcolor-5)/6
               ENDIF
               x0=rayx(I,J)
               y0=rayy(I,J)
            ENDFOR
            IF raynsteps(I) GE 1 THEN OPLOT,[x0,x0],[y0,y0],psym=-6, $
               linestyle=0,THICK=4,SYMSIZE=0.4,NOCLIP=0
            x0=rayx(I,J)
            y0=rayy(I,J)
            ENDIF
         ENDFOR
      ENDIF

         
   IF FLAGTSTEP THEN BEGIN
      MAXS=MAX(TIMESTEPS(1,*))
      MINS=MAX(TIMESTEPS(1,*))

      FOR I=NTIMESTEPS-1,0,-1 DO BEGIN
         OPLOT,[TIMESTEPS(2,I)],[TIMESTEPS(3,I)],THICK=4, psym=4, $  ; *TIMESTEPS(1,I)/MAXS
               linestyle=0,SYMSIZE=4,COLOR=Navailcolor-3-I
      ENDFOR
   ENDIF


   IF transOK and adtr THEN BEGIN
      PSIZES=FLTARR(Ntrans)+0.2
      PSIZES(0)=1.
      Psizes(Ntrans-1)=1.
      OPLOT,Xtrans,Ytrans,PSYM=transsym, $
      LINESTYLE=transline,THICK=transthick,SYMSIZE=transsymsize
   ENDIF
   IF adbathydot GT 0 THEN plot_dot_bathy
   RETURN
END

;----------------------------------------------------------------------------
PRO doplot
;** 1 ** Display parameters
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
;*******END OF COMMON BLOCKS*******************************

   ERASE
   WIDGET_CONTROL, Wdraw, SENSITIVE=1

   CASE datatype OF
      0:print,'Plot not updated'
      4:doplotothers
      ELSE:IF TOTAL(datastatus(10:13)) GE 1 THEN doplottimeseries
   ENDCASE
END

;----------------------------------------------------------------------------
PRO wavepar_from_spectrum,Spec,freq,theta,df,dtheta,Hs,fp,thet,sigth

         nf=N_ELEMENTS(freq)
         nth=N_ELEMENTS(theta)
         a1=FLTARR(nf)
         b1=FLTARR(nf)
         IN2=FLTARR(nf)
         thet=FLTARR(nf)
         Etot=FLTARR(nf)
         Etot1=FLTARR(nf)
         Etot2=FLTARR(nf)
         Etot3=FLTARR(nf)
         Etot4=FLTARR(nf)
         I1=WHERE(sin(theta*!dtor) < 0.)
         I2=WHERE(sin(theta*!dtor) > 0.)
         I3=WHERE(cos(theta*!dtor) < 0.)
         I4=WHERE(cos(theta*!dtor) > 0.)
;        I5=WHERE((-1.*cos(theta*!dtor)-0.8) > 0.)
;        I5=WHERE((sin(theta*!dtor)) > 0.)
         FOR I=0,nf-1 DO BEGIN
;Spec(I,I5)=0.
            Etot(I)=TOTAL(Spec(I,*))*Df(I)*dtheta*!dtor
            Etot1(I)=TOTAL(Spec(I,I1))*Df(I)*dtheta*!dtor
            Etot2(I)=TOTAL(Spec(I,I2))*Df(I)*dtheta*!dtor
            Etot3(I)=TOTAL(Spec(I,I3))*Df(I)*dtheta*!dtor
            Etot4(I)=TOTAL(Spec(I,I4))*Df(I)*dtheta*!dtor
         ENDFOR
         ETOTW=TOTAL(ETOT1)
         ETOTE=TOTAL(ETOT2)
         ETOTN=TOTAL(ETOT3)
         ETOTS=TOTAL(ETOT4)
         ETOT2=TOTAL(ETOT)
                  PRINT,NF,ETOT2,',  4 QUAD W E S N:',4*ETOTW,4*ETOTE,4*ETOTN,4*ETOTS
                  PRINT,'R EW NS:',ETOTE/ETOTW,ETOTS/ETOTN
        Hs=4*SQRT(Etot2)
         ; COMPUTES BULK PARAMETERS, WITH THE REFLECTED WAVES REMOVED
         ;Index=where((y GT 180) AND (y LT 360),kount)
         ;IF kount GT 0 THEN table(*,index)=0.     ;removes the reflected wave
         Ef=TOTAL(Spec,2)*dtheta*!dtor ;frequency spectrum
         Efmax=MAX(Ef,ifp)
         fp=(freq(ifp)*Ef(ifp)+freq(max([ifp-1,0]))*Ef(max([ifp-1,0]))+ $
            freq(min([ifp+1,nf-1]))*Ef(min([ifp+1,nf-1])))
         if (FP NE 0) THEN $
            fp=fp/(Ef(ifp)+Ef(max([ifp-1,0]))+Ef(min([ifp+1,nf-1])))
         FOR I=0,nf-1 DO BEGIN
            a1(I)=TOTAL(cos(theta(*)*!dtor)*spec(I,*))*dtheta*!dtor
            b1(I)=TOTAL(sin(theta(*)*!dtor)*spec(I,*))*dtheta*!dtor
            IN2(I)=TOTAL(spec(I,0:nth/2-1)*spec(I,nth/2:nth-1))*dtheta*!dtor/(Ef(I)^2)
            thet(I)=ATAN(b1(I),a1(I))
            IF thet(I) < 0. THEN thet(I)=thet(I)+2.*!pi
         ENDFOR

         ; Spread at peak
         sigth=SQRT(ABS(2*(1-(a1(Ifp)*cos(thet(Ifp)) $
            +b1(Ifp)*sin(thet(ifp)))/Efmax)))/!dtor
         thet=thet(ifp)/!dtor
         PRINT,'I at fp:',IN2(ifp)

END


;----------------------------------------------------------------------------
PRO doplottimeseries
;
;     Performs all plots based on time series data
;
;     Method: first processes the data from modelled or observed time series
;             then plots the data
;
;            - three possible values for 'datatype': (1) observed, (2) modeled, or (3) comparison
;                                                      NB: other values (4) correspond to maps ... 
;            - three possible 'typ' of data: (2) f-theta spectra, (1) f-spectra, (0) parameters (Hs ...)

;** 1 ** Display parameters
COMMON AXISFRAME,axis_orient,frametype,outx1lab,outx2lab,outy1lab,outy2lab
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON COLORBARPAR,cbnticks,cbexrange,cbtrx,cbtry,cbblx,cbbly, $
   filloutofrange,addmini,addmaxi
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON LANGUAGE, ilang,dictionnaire
COMMON TITLES,  font,outtit,outxtit,outytit,outcbtit,textx,texty,textdx,textdy
COMMON OVERLAY, addir,adsyms,adbathy,adcoast,psyms,psymsizes,adtr,adtri
COMMON POSTSCRIPT, filep,pspath,prcoul,psor,pstype, $
                pwinx,pwiny,papierx,papiery,xoffset,yoffset, $
                facpolice,fontrescale,basefontsize,pssizex,pssizey,psfont
COMMON THREED,  Ax3D,Az3D,smoothing
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth

;** 2 ** Display and data variables/parameters
COMMON FREQ,    freq,nfband,findex,findex2
COMMON TIME,    timestep,tindex,tindex2,ntime,dtime,time0,day0,dtindex,nstep, $
                timezone_plot,timezone_string,months,time
COMMON TRANSECT,Ntrans,Strans,Xtrans,Ytrans,Ztrans,Itrans, $
                  COtrans,TransOK,transsym,transline,transthick,Ispectrans, $
                  spectransname,ntransgp,transsymsize

COMMON SPACE,   c_gp,c_cut,indexgp,c_x,c_y,c_lon,c_lat

;** 3 ** I/O and data variables
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON TIMESERIES, modspec,obs,obs2,om,ts_filetype, nbins, bulktype,scatvar
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
COMMON LOCALY, Ymem,ymem2,ymem3,ymem4,ymem5
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
;*******END OF COMMON BLOCKS*******************************
   indext=0L+(tindex-1)*dtindex
   indext2=0L+(tindex2-1)*dtindex
   nt=0L+indext2-indext+1
   POSBOX=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]

ytit=''
   nf=findex2-findex+1

;
; 1. manages type of data (1: observed, 2: modeled, 3: comparison) 
;
   CASE datatype OF
   1:BEGIN        
      dates0=*obs.dates
      dates=dates0(indext:indext2)
      nabin=obs.nabin
      np=obs.np
      nv=obs.nvar
      nr=obs.nrun
      typ=obs.typ
      f=*obs.f
      theta=*obs.theta
      df=*obs.df
      varname=strcompress((*obs.varnames)[plottype],/REMOVE_ALL)
      pname=(*obs.pnames)[c_spec]
      pnames=(*obs.pnames)
      data0=(*obs.data)
      data=data0(*,*,*,findex-1:findex2-1,0:nabin-1,indext:indext2)
      flag0=*obs.flags
      flags=flag0(indext:indext2)
      END
;
   2:BEGIN        
      dates0=*modspec.dates
      IF (indext GT indext2) THEN  BEGIN
        RETURN
      ENDIF
      dates=dates0(indext:indext2)
      nv=modspec.nvar
      nabin=modspec.nabin
      np=modspec.np
      nr=modspec.nrun
      typ=modspec.typ
      IF typ EQ 2 THEN BEGIN
         U10=*modspec.pU10
         Udir=*modspec.pUdir
         Curr=*modspec.pCurr
         Currdir=*modspec.pCurrdir
         depth=*modspec.pdepth
      ENDIF
      f=*modspec.f
      theta=*modspec.theta
      df=*modspec.df
      flag0=*modspec.flags
      varname=strcompress((*modspec.varnames)[plottype],/REMOVE_ALL)
      pname=(*modspec.pnames)[c_spec]
      pnames=(*modspec.pnames)
      data0=(*modspec.data)
      size0=size(data0)
      f1=MIN([findex,size0(4)])-1
      f2=MIN([findex2,size0(4)])-1
print,'TEST:',f1,f2,size(data0)
      data=data0(*,*,*,f1:f2,0:nabin-1,indext:indext2)
      flags=flag0(indext:indext2)
      END
;
   3:BEGIN        
      IF (*om.dateo EQ !NULL) THEN BEGIN 
         PRINT,'No common dates: no plot.'
         RETURN
      ENDIF
      dates0=(*obs.dates)[*om.dateo]
      IF (indext LT 0) THEN indext=0 
         
      IF (indext GT indext2) THEN BEGIN 
         PRINT,'time index 1 > time index2:',indext,indext2
         RETURN
      ENDIF
      dates=dates0(indext:indext2)

      nv=om.nvar
      nabin=obs.nabin
      np=om.np
      nr=obs.nrun
      typ=modspec.typ
      f=(*obs.f)[*om.fo]
      df=(*obs.df)[*om.fo]
      theta=*modspec.theta
      data=FLTARR(1,np,nv,findex2-findex+1,nabin,nt)
      data2=FLTARR(modspec.nrun,np,nv,findex2-findex+1,nabin,nt)
      names=(*obs.varnames)
      varname=strcompress(names((*om.vo)[plottype]),/REMOVE_ALL)
      IF varname EQ 'sth1' THEN varname='sth1m'
      pname=(*obs.pnames)[(*om.po)[c_spec]]
      pnames=(*obs.pnames)[(*om.po)]
      data0=(*modspec.data)*1.;

      FOR I=0L,nt-1 DO $
         IF ((*modspec.flags)[indext+I] NE 0) THEN BEGIN
            J=0L+(*om.datem)[indext+I]
            data2(*,*,*,*,*,I)= $
             data0(*,*om.pm,*om.vm,(*om.fm)[findex-1:findex2-1],0:nabin-1,J);
            ENDIF
      data0=(*obs.data)
      ;I=WHERE(data0 NE 0) 
      ;data0(I)=1./data0(I)
      FOR I=0L,nt-1 DO $
         IF ((*modspec.flags)[indext+I] NE 0) THEN $
            data(0,*,*,*,*,I)= $
            data0(0,*om.po,*om.vo,(*om.fo)[findex-1:findex2-1],0:nabin-1,(*om.dateo)[indext+I])
      flag0=*modspec.flags
      flags=flag0(indext:indext2)
      END
   ENDCASE

;
; 2. manages typ of data (2: 2D spectra, 1: 1D spectra ... 
;
;
; Frequency-direction spectrum
;
   IF typ EQ 2 THEN BEGIN
      IF (c_cut EQ 5) THEN BEGIN
         rangex=[min(F),max(F)]
         x=f(findex-1:findex2-1)
         y=theta/!dtor
         rangey=[min(y),max(y)]
         ; Restricts the frequency range to that of the model if defined
         rangex=[min(x),max(x)]
         Rescale,0
         yp=FLTARR(findex2-findex+1,modspec.nabin)
         yp(*,*)=data(0,c_spec,plottype,*,*,0)
         ats='at'
         ; ats=dictionnaire.at(ilang)
         ;tit=varname+' '+ats+' '+pname
         tit=varname+' '+'at'+' '+pname
         xtit='f (Hz)'
         ytit='Compass direction (from) in degrees '
         cbtit='Energy density (m^2/Hz/rad)'
         taille2=size(yp)
         Contorno,yp,x,y,tit,xtit,' ',' ',0,0,1
         IF (filltype LE 4) THEN Drawframe,xtit,' ',ytit,' ',FRAME=5
;
         spec2D=yp
         timec=((dates[0].h+timezone_plot-dates[0].zone)*60+dates[0].minu)
         TimetoDate,dates[0].jday,timec,date,hour
         strlab3=date+' '+hour
         XYOUTS,rangex(0)+0.95*(rangex(1)-rangex(0)), $
               rangey(1)-0.05*(rangey(1)-rangey(0)),strlab3,ALIGNMENT=1.0
;
        IF varname EQ 'E(,)' THEN BEGIN
            r = .3            ;len of arrow head
            angle = 22.5 * !dtor      ;Angle of arrowhead
            st = r * sin(angle)      ;sin 22.5 degs * length of head
            ct = r * cos(angle)
            ; arrow
            xmotif=[0.,1.,1.-ct,1.,1.-ct]
            ymotif=[0.,0.,st,0.,-st]


            u0=2*cos((90-Udir(c_spec,indext))*!dtor)*x(1)
            v0=2*sin((90-Udir(c_spec,indext))*!dtor)*x(1)
            IF (U10(c_spec,indext) GT 0) THEN PLOTS,u0*(1-xmotif)+v0*ymotif, $
               v0*(1-xmotif)-U0*ymotif,THICK=4
            u0=-2*cos((90-Currdir(c_spec,indext))*!dtor)*x(1)
            v0=-2*sin((90-Currdir(c_spec,indext))*!dtor)*x(1)
            IF (Curr(c_spec,indext) GT 0) THEN PLOTS,u0*(xmotif)+v0*ymotif, $
               v0*(xmotif)-U0*ymotif,THICK=2,LINESTYLE=0


            df2=df(findex-1:findex2-1)
            dtheta=360/N_ELEMENTS(Y)
            Hs=0.
            fp=0.
            thet=0.
            sigth=0.
;
; Computes bulk parameters from spectrum
;
            wavepar_from_spectrum,yp,x,y,df2,dtheta,Hs,fp,thet,sigth
;
; Overlays parameter values on plot
;
            strlab='Hs:'+STRCOMPRESS(STRING(Hs,FORMAT='(F5.2)'))+'m'
            XYOUTS,rangex(0)+0.05*(rangex(1)-rangex(0)), $
                 rangey(1)-0.05*(rangey(1)-rangey(0)),strlab
            strlab='fp:'+STRCOMPRESS(STRING(Fp,FORMAT='(F6.4)'))+' Hz'
            XYOUTS,rangex(0)+0.05*(rangex(1)-rangex(0)), $
               rangey(1)-0.1*(rangey(1)-rangey(0)),strlab
            strlab='Mean dir. at fp: ' $
              +STRCOMPRESS(STRING(thet,FORMAT='(F6.2)')) +' deg'
            XYOUTS,rangex(0)+0.05*(rangex(1)-rangex(0)), $
               rangey(1)-0.15*(rangey(1)-rangey(0)),strlab
            strlab='Spread at fp: ' $
            +STRCOMPRESS(STRING(sigth,FORMAT='(F5.2)')) +' deg'
            XYOUTS,rangex(0)+0.05*(rangex(1)-rangex(0)), $
               rangey(1)-0.2*(rangey(1)-rangey(0)),strlab

            strlab='U10: ' $
            +STRCOMPRESS(STRING(U10(c_spec,indext),FORMAT='(F5.2)'))+' m/s'
            XYOUTS,rangex(0)+0.05*(rangex(1)-rangex(0)), $
               rangey(1)-0.25*(rangey(1)-rangey(0)),strlab

            strlab='Curr: ' $
            +STRCOMPRESS(STRING(Curr(c_spec,indext),FORMAT='(F5.2)'))+' m/s'
            XYOUTS,rangex(0)+0.05*(rangex(1)-rangex(0)), $
               rangey(1)-0.3*(rangey(1)-rangey(0)),strlab
         ENDIF
         RETURN
      ENDIF ELSE BEGIN
;
; Integrated the f,theta spectrum over directions 
;
         tempo=TOTAL(data,5)*abs(theta(1)-theta(0))
         nabin=1
         data=FLTARR(nr,np,nv,findex2-findex+1,nabin,indext2-indext+1)
         data(*,*,*,*,0,*)=tempo
         typ=1
         varname='Ef'
      ENDELSE
   ENDIF




   ;*****************************************************************
   ; The following section determines the appearance of the x-axis
   ; for time series: ticks, labels, dates, ...
   ;*****************************************************************
   xposi=0.
   xnminor=1
   IF (c_cut LE 1) OR (c_cut EQ 4) OR (c_cut EQ 5 and typ EQ 1) THEN BEGIN    
;
; Prepares data for time series plot: the x-axis is time
;
      IF (FLOOR(dates[0].jday-0.5) EQ FLOOR(dates[nt-1].jday-0.5)) THEN BEGIN
;
; all the data is from the same day, therefore the
; x-axis shows hours, and the x-label indicates the date.
;
         xtit='Hours ('+months(ilang,dates[0].m-1)+' '+ $
            STRING(dates[0].d,FORMAT='(I2)')+', '+ STRING(dates[0].y,FORMAT='(I4)')+')'
         rangex=[dates[0].h,dates[nt-1].h + 1]
         xnticks=dates[nt-1].h+1-dates[0].h
         ;print,dates[0].h,dates[nt-1].h+1,xnticks
         xnames=['']
         xposi=[0]
         x=dates.h+dates.minu/60.+dates.s/3600.
      ENDIF ELSE BEGIN
         x=dates.jday-0.5-LONG(dates[0].jday-0.5); +((dates[0].h+dates[0].minu/60.+dates[0].s/3600.)/24.)
        ; IF x(0) GE 1 THEN x=x-1.
         IF (dates[0].m EQ dates[nt-1].m AND dates[0].y EQ dates[nt-1].y) THEN BEGIN
;
; Here all the data is from the same month: the ticks are the
; 00:00 hour at the end of each day, and the tick labels (day numbers) are shifted to appear between two ticks
;
            rangex=[0,dates[nt-1].d-dates[0].d+ 1]
;            xtit=dictionnaire.days(ilang)+' ('+months(ilang,dates[0].m-1)+' '+ STRING(dates[0].y,FORMAT='(I4)')+')'
            xtit='days'+' ('+months(ilang,dates[0].m-1)+' '+ STRING(dates[0].y,FORMAT='(I4)')+')'
            xnticks=dates[nt-1].d+1-dates[0].d
            IF (xnticks LE 10) THEN xnminor=4
            IF (xnticks LE 3) THEN xnminor=24
            xnames=STRING(FINDGEN(xnticks)+dates[0].d,FORMAT='(I2)')
;
; Position of ticks on the date (x) axis
;
            xposi=FINDGEN(xnticks)+0.5
            xntickval=FINDGEN(xnticks+1)
         ENDIF ELSE BEGIN
            rangex=[0,CEIL(max(x))]
;            xtit=dictionnaire.days(ilang)+' ('+STRING(dates[0].y,FORMAT='(I4)')+')'
            xtit='days'+' ('+STRING(dates[0].y,FORMAT='(I4)')+')'
;
; defines maximum number of days for writing dates
;
            jmax=62
            IF (axis_orient EQ 1) THEN jmax=124
            IF ((dates[nt-1].Jday-dates[0].Jday) LT jmax) THEN BEGIN
               xnticks=rangex(1)
               ;dj=2; 
           dj=1+xnticks/15
           IF (axis_orient EQ 1) THEN dj=2; %1+xnticks/30
               xnminor=dj
               xnticks=xnticks/dj
               CALDAT,FINDGEN(xnticks)*dj+dates[0].jday,m,d,y
               xnames=STRARR(xnticks)
               FOR I=0,xnticks-1 DO BEGIN
               IF (m(I) LT 10) THEN ms=STRING(m(I),FORMAT='(I1)') ELSE ms=STRING(m(I),FORMAT='(I2)')
               IF (ilang EQ 0) THEN  $
                  xnames(I)=ms+'/'+STRING(d(I),FORMAT='(I2)') $
                  ELSE xnames(I)=STRING(d(I),FORMAT='(I2)')+'/'+ms
               ENDFOR
               xntickval=FINDGEN(xnticks+1)*dj
               xposi=FINDGEN(xnticks)*dj+0.5
               IF (axis_orient EQ 1) THEN xposi=xposi+0.4

            ENDIF ELSE BEGIN
               j1=JULDAY(dates[nt-1].m,1,dates[nt-1].y)
               j0=JULDAY(dates[0].m,1,dates[0].y)
               xnticks=LONG(dates[nt-1].jday)+1-LONG(dates[nt-1].jday)
               IF (xnticks GT 59) THEN xnticks=xnticks/2
               IF (xnticks > 60) THEN xnticks=xnticks/2
               xnposi=FIX((j1-j0)/29.5)
               rangex=[j0-dates[0].jday,j1-dates[0].jday]
               xposi=FINDGEN(xnposi)*29.5+14.5+rangex(0)
               CALDAT,xposi+dates[0].jday,m,d,y
               ;print,'months',m
               xnames=months(ilang,m-1)
               xntickval=FINDGEN(xnticks+1)
            ENDELSE
            IF (xnticks LE 10) THEN xnminor=4
         ENDELSE
         xtnames=REPLICATE(' ',xnticks+1)
      ENDELSE
   ENDIF
;
   IF typ EQ 1 THEN BEGIN   ; for frequency-dependent variables
     y0=FLTARR(nr,np,findex2-findex+1,nt)
     y0w=FLTARR(nr,np,findex2-findex+1,nt)
     y0(*,*,*,*)=data(*,*,plottype,*,0,*)
     y0w(*,*,*,*)=data(*,*,0,*,0,*)
     y0d=y0w;
     TAILLE=SIZE(data)
     IF (TAILLE(3) GE 2) THEN y0d(*,*,*,*)=data(*,*,1,*,0,*)
     IF (datatype EQ 3) THEN BEGIN
        y02=FLTARR(modspec.nrun,np,findex2-findex+1,nt)
        y02w=FLTARR(modspec.nrun,np,findex2-findex+1,nt)
        y02(*,*,*,*)=data2(*,*,plottype,*,0,*)
        y02w(*,*,*,*)=data2(*,*,0,*,0,*)
        y02d=y02w;
        IF (TAILLE(3) GE 2) THEN y02d(*,*,*,*)=data2(*,*,1,*,0,*)
        y2=FLTARR(modspec.nrun,np,nt)
     ENDIF
     y=FLTARR(nr,np,nt)
;
     CASE varname OF
      'Ef': BEGIN
         IF (bulktype EQ 3)  THEN BEGIN
            Isth=-1
            IF (datatype NE 1) THEN BEGIN  ; case of model data
               FOR J=modspec.nvar-1,0,-1  DO BEGIN
                  var3=STRMID((*modspec.varnames)[J],0,4)
                  IF   STRPOS(var3,'sth1') GT -1  THEN Isth=J
               ENDFOR
            ENDIF ELSE BEGIN
               FOR J=obs.nvar-1,0,-1  DO BEGIN ; case of obs.
                  var3=STRMID((*obs.varnames)[J],0,4)
                  IF   STRPOS(var3,'sth1') GT -1  THEN Isth=J
               ENDFOR
            ENDELSE
            y01=FLTARR(nr,np,findex2-findex+1,nt)
            IF (Isth GE 0) THEN y01(*,*,*,*)=data(*,*,Isth,*,0,*)
         ENDIF ; (bulktype EQ 3)
;         GET_LUN,unit1
;         OPENW,unit1,'buoy.txt'
         FOR I=0,nt-1 DO BEGIN
            FOR J=0,np-1 DO BEGIN
               FOR K=0,nr-1 DO BEGIN
;
; Computes the bulk parameters according to the chosen moment: E(f), E(f)*f^4 ... 
;
                  IF (bulktype EQ 0) THEN $
                               y(k,J,I)=4.*SQRT(TOTAL(y0(K,J,*,I)*df(findex-1:findex2-1)))
                  IF (bulktype EQ 1) THEN $
                               y(k,J,I)=TOTAL((2*!pi*freq(findex-1:findex2-1))^4*y0(K,J,*,I)*df(findex-1:findex2-1))/9.81^2
                  IF (bulktype EQ 2) THEN $
                               y(k,J,I)=2.*TOTAL((2*!pi*freq(findex-1:findex2-1))^3*y0(K,J,*,I)*df(findex-1:findex2-1))/9.81
                  IF (bulktype EQ 3) THEN $
                               y(k,J,I)=2.*TOTAL((2*!pi*freq(findex-1:findex2-1))^3*y0(K,J,*,I) $
                                                           *df(findex-1:findex2-1)*(1-0.5*(y01(K,J,*,I)*!dtor)^2))/9.81
                  IF (bulktype EQ 4) THEN  $
                               y(k,J,I)=MEAN((2*!pi)^4*freq(findex-1:findex2-1)^5*y0(K,J,*,I))/9.81^2
                  IF (bulktype EQ 5) THEN y(k,J,I)=TOTAL((2*!pi*freq(findex-1:findex2-1))^2*y0(K,J,*,I)*df(findex-1:findex2-1))
                  IF (bulktype EQ 6) THEN BEGIN 
                        m1=TOTAL((freq(findex-1:findex2-1))^1*y0(K,J,*,I)*df(findex-1:findex2-1))
                        m0=TOTAL(y0(K,J,*,I)*df(findex-1:findex2-1))
                        y(k,J,I)=m0/m1
;         PRINTF,unit1,dates[I].y,dates[I].m,dates[I].d,dates[I].h,m1,y(k,J,I)
                        ENDIF
                  IF (bulktype EQ 7) THEN BEGIN 
                        m2=TOTAL((freq(findex-1:findex2-1))^(2)*y0(K,J,*,I)*df(findex-1:findex2-1))
                        m0=TOTAL(y0(K,J,*,I)*df(findex-1:findex2-1))
                        y(k,J,I)=(m0/m2)^(0.5)
                        ENDIF
               ENDFOR
            ENDFOR
         ENDFOR
;         close,unit1
;         FREE_LUN,unit1

         yp2=FLTARR(nt)
         FOR I=0,nt-1 DO BEGIN
            yp2(I)=4.*SQRT(TOTAL(y0(0,0,*,I)*df(findex-1:findex2-1)))
         ENDFOR
         IF (datatype EQ 3) THEN BEGIN
            IF (bulktype EQ 3)  THEN BEGIN
               y01=FLTARR(modspec.nrun,np,findex2-findex+1,nt)
               y01(*,*,*,*)=data2(*,*,Isth,*,0,*)
            ENDIF
;         GET_LUN,unit1
;         OPENW,unit1,'model.txt'
            FOR I=0,nt-1 DO BEGIN
               FOR J=0,np-1 DO BEGIN
                  FOR K=0,modspec.nrun-1 DO BEGIN
                     IF (bulktype EQ 0) THEN  $
                                  y2(K,J,I)=4.*SQRT(TOTAL(y02(K,J,*,I)*df(findex-1:findex2-1)))
                     IF (bulktype EQ 1) THEN  $
                                  y2(K,J,I)=TOTAL((2*!pi*freq(findex-1:findex2-1))^4*y02(K,J,*,I)*df(findex-1:findex2-1))/9.81^2
                     IF (bulktype EQ 2) THEN  $
                                  y2(K,J,I)=2.*TOTAL((2*!pi*freq(findex-1:findex2-1))^3*y02(K,J,*,I)*df(findex-1:findex2-1))/9.81
                     IF (bulktype EQ 3) THEN  $
                                  y2(K,J,I)=2.*TOTAL((2*!pi*freq(findex-1:findex2-1))^3*y02(K,J,*,I)  $
                                                               *df(findex-1:findex2-1)*(1-0.5*(y01(K,J,*,I)*!dtor)^2))/9.81
                    IF (bulktype EQ 4) THEN   $
                                 y2(k,J,I)=MEAN((2*!pi)^4*freq(findex-1:findex2-1)^5*y02(K,J,*,I))/9.81^2
                    IF (bulktype EQ 5) THEN   $
                                 y2(k,J,I)=TOTAL((2*!pi*freq(findex-1:findex2-1))^2*y02(K,J,*,I)*df(findex-1:findex2-1))
                    IF (bulktype EQ 6) THEN BEGIN
                       m1=TOTAL((freq(findex-1:findex2-1))^1*y02(K,J,*,I)*df(findex-1:findex2-1))
                        m0=TOTAL(y02(K,J,*,I)*df(findex-1:findex2-1))
                        y2(k,J,I)=m0/m1
;         PRINTF,unit1,dates[I].y,dates[I].m,dates[I].d,dates[I].h,m1,y2(k,J,I)

                       ENDIF
                    IF (bulktype EQ 7) THEN BEGIN
                       m2=TOTAL((freq(findex-1:findex2-1))^(2)*y02(K,J,*,I)*df(findex-1:findex2-1))
                        m0=TOTAL(y02(K,J,*,I)*df(findex-1:findex2-1))
                        y2(k,J,I)=(m0/m2)^(0.5)
;         PRINTF,unit1,dates[I].y,dates[I].m,dates[I].d,dates[I].h,m1,y2(k,J,I)

                       ENDIF
                   ENDFOR
               ENDFOR
            ENDFOR
;         close,unit1
;         FREE_LUN,unit1

         ENDIF ;(datatype EQ 3)
      END
     'sth1m':BEGIN
        FOR I=0,nt-1 DO BEGIN
            FOR J=0,np-1 DO BEGIN
               FOR K=0,nr-1 DO BEGIN
               ;   y(k,J,I)=SQRT(TOTAL(y0(K,J,*,I)^2*y0w(K,J,*,I)*df(findex-1:findex2-1))/ $
               ;                  TOTAL(y0w(K,J,*,I)*df(findex-1:findex2-1))          )
                   m1temp=abs(1-0.5*(y0(K,J,*,I)*!dtor)^2)
                   a1temp=cos(y0d(K,J,*,I)*!dtor)*m1temp
                   b1temp=sin(y0d(K,J,*,I)*!dtor)*m1temp
                   a1=TOTAL(a1temp*y0w(K,J,*,I)*df(findex-1:findex2-1))
                   b1=TOTAL(b1temp*y0w(K,J,*,I)*df(findex-1:findex2-1))
                   E=TOTAL(y0w(K,J,*,I)*df(findex-1:findex2-1))
                   m1=sqrt(a1^2+b1^2)/E
                   y(k,J,I)=sqrt(abs(2*(1-m1)))/!dtor
               ENDFOR
            ENDFOR
         ENDFOR
         IF (datatype EQ 3) THEN BEGIN
            FOR I=0,nt-1 DO BEGIN
               FOR J=0,np-1 DO BEGIN
                  FOR K=0,modspec.nrun-1 DO BEGIN
                  ;   y2(k,J,I)=SQRT(TOTAL(y02(K,J,*,I)^2*y02w(K,J,*,I)*df(findex-1:findex2-1))/ $
                  ;               TOTAL(y02w(K,J,*,I)*df(findex-1:findex2-1))          )
                   m1temp=abs(1-0.5*(y02(K,J,*,I)*!dtor)^2)
                   a1temp=cos(y02d(K,J,*,I)*!dtor)*m1temp
                   b1temp=sin(y02d(K,J,*,I)*!dtor)*m1temp
                   a1=TOTAL(a1temp*y02w(K,J,*,I)*df(findex-1:findex2-1))
                   b1=TOTAL(b1temp*y02w(K,J,*,I)*df(findex-1:findex2-1))
                   E=TOTAL(y02w(K,J,*,I)*df(findex-1:findex2-1))
                   m1=sqrt(a1^2+b1^2)/E
                   y2(k,J,I)=sqrt(abs(2*(1-m1)))/!dtor


                  ENDFOR
               ENDFOR
            ENDFOR
         ENDIF
         END 
;
      ELSE: BEGIN 
         yp=FLTARR(nr,np,findex2-findex+1,nt)
         yp(*,*,*,*)=data(*,*,plottype,*,0,*)
         y=FLTARR(nr,np,nt)
         FOR I=0,nt-1 DO BEGIN
            FOR J=0,np-1 DO BEGIN
               FOR K=0,nr-1 DO BEGIN
                  fp=MAX(data(K,J,0,*,0,I),Ifmax)
                  y(K,J,I)=data(K,J,plottype,Ifmax,0,I)
               ENDFOR
            ENDFOR
         ENDFOR
         IF (datatype EQ 3) THEN BEGIN
            y2=FLTARR(modspec.nrun,np,nt)
            FOR I=0,nt-1 DO BEGIN
            FOR J=0,np-1 DO BEGIN
               FOR K=0,modspec.nrun-1 DO BEGIN
                  fp=MAX(data2(K,J,0,*,0,I),Ifmax)
                  y2(K,J,I)=data2(K,J,plottype,Ifmax,0,I)
               ENDFOR
            ENDFOR
            ENDFOR
         ENDIF
         END
      ENDCASE
   ENDIF


   tit=' '
   CASE varname of
   'Ef':BEGIN
         ats='at'
         ; ats=dictionnaire.at(ilang)
         hss='significant wave height'
         ; hss=dictionnaire.hs(ilang)
         IF (c_cut EQ 0) OR (c_cut EQ 3) OR (c_cut EQ 4) OR (c_cut GE 6) THEN BEGIN
               IF (bulktype EQ 0) THEN tit=hss+' (Hs) '+ats+' '+pname
               IF (bulktype EQ 1) THEN tit='pseudo-m.s.s. '+ats+' '+pname
               IF (bulktype EQ 2) THEN tit='Non-directional surface Stokes drift '+ats+' '+pname
               IF (bulktype EQ 3) THEN tit='Surface Stokes drift '+ats+' '+pname
               IF (bulktype EQ 4) THEN tit="Phillips' constant "+ats+' '+pname
               IF (bulktype EQ 5) THEN tit="Surface velocity variance "+ats+' '+pname
               IF (bulktype EQ 6) THEN tit="Mean period Tm01 "+ats+' '+pname
               IF (bulktype EQ 0) THEN BEGIN 
                 mmtom='m'
                 IF (MAX(y) LT 0.5) THEN BEGIN 
                    mmtom='cm'
                    y=y*100.
                    IF (datatype EQ 3) THEN y2=y2*100.
                    IF (MAX(y) LT 3) THEN BEGIN 
                      mmtom='mm'
                      y=y*10.
                      IF (datatype EQ 3) THEN y2=y2*10.
                    ENDIF 
                 ENDIF 
                 ytit='Hs ('+ mmtom+' '+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                  +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
               ENDIF
               IF (bulktype EQ 1) THEN ytit='pseudo-m.s.s. (m, '+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                 +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
               IF (bulktype EQ 1) THEN ytit='pseudo-m.s.s. (m, '+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                 +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
               IF (bulktype EQ 2) THEN ytit='Ussnd (m/s, '+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                 +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
               IF (bulktype EQ 3) THEN ytit='Uss (m/s, '+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                 +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
               IF (bulktype EQ 4) THEN ytit='mean alpha ('+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                 +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
               IF (bulktype EQ 5) THEN ytit='<u_orb^2> (m2/s2, '+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                 +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
               IF (bulktype EQ 6) THEN ytit='Mean period Tm01  (s, '+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                 +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
         ENDIF ELSE BEGIN
               tit='E(f) at '+pname
               ytit='E (m^2/Hz)'
         ENDELSE
      END
   'Fp':BEGIN
         ats='at'
         ; ats=dictionnaire.at(ilang)
         IF (c_cut EQ 0) OR (c_cut EQ 3) OR (c_cut EQ 4) OR (c_cut EQ 7) THEN BEGIN
               IF (bulktype EQ 0) THEN tit='Microseismic bottom pressure variance'+' (m2/s2) '+ats+' '+pname
               IF (bulktype EQ 1) THEN tit='pseudo-m.s.s. '+ats+' '+pname
               IF (bulktype EQ 0) THEN ytit='Hs (m, '+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                  +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
               IF (bulktype EQ 1) THEN ytit='pseudo-m.s.s. (m, '+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                 +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
         ENDIF ELSE BEGIN
               tit='Microseismic bottom pressure spectrum at '+pname
               ytit='Fp (m^2/s)'
         ENDELSE
      END
   'Hs':BEGIN
      ytit = 'Hs (m)'
      tit='Significant wave height at '+pname
      END
   'Curr':BEGIN
      ytit = 'Current speed (m/s)'
      tit='Current at '+pname
      END
   'fp':BEGIN
      ytit = 'fp (Hz)'
      tit='Peak period at '+pname
      END
   'f0-1':BEGIN
      ytit = 'f0-1 (Hz)'
      tit='Mean frequency (f0-1) at '+pname
      END
   'f02':BEGIN
      ytit = 'f02 (Hz)'
      tit='Mean frequency (f02) at '+pname
      END
   'U10':BEGIN
      ytit = 'U10 (m/s)'
      tit='Wind speed at '+pname
      END
   'Udir':BEGIN
      ytit = 'wind direction (deg)'
      tit='Wind direction at '+pname
      END
   'Hsband':BEGIN
      ytit = 'Hs (m, 0.055 - 0.155 Hz only)'
      tit='Significant wave height at '+pname
      END
   'psi':ytit = 'Normalised Shields number'
   'ab':BEGIN
      ytit = '0.7 * d1/3  (m)'
      ypnoz=0.7*ypnoz
      END
   'ub':ytit = 'r.m.s. bottom velocity amplitude (m/s)'
   'th1':BEGIN
      IF (c_cut EQ 0) OR (c_cut EQ 3) OR (c_cut EQ 4) OR (c_cut GE 7) THEN BEGIN
        ytit ='mean direction ('+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                 +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' from, degrees)' 
      ENDIF ELSE BEGIN
        ytit = 'theta_mean(fp) (from, degrees)'
      ENDELSE
      tit='Mean direction at '+pname
      IF typ EQ 1 THEN BEGIN
      index180=WHERE(yp LT 0, kount)
      IF kount GT 0 THEN yp(index180)=yp(index180)+360.
      ENDIF
      END
   'th1m':BEGIN
      IF (c_cut EQ 0) OR (c_cut EQ 3) OR (c_cut EQ 4) OR (c_cut GE 7) THEN BEGIN
        ytit ='mean direction ('+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                 +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' from, degrees)' 
      ENDIF ELSE BEGIN
        ytit = 'theta_mean(fp) (from, degrees)'
      ENDELSE
      tit='Mean direction at '+pname
      IF typ EQ 1 THEN BEGIN
      index180=WHERE(yp LT 0, kount)
      IF kount GT 0 THEN yp(index180)=yp(index180)+360.
      ENDIF
      END
   'th2':BEGIN
      ytit = 'Mean direction 2 (from, degrees)'
      IF typ EQ 1 THEN BEGIN
      index180=WHERE(yp GE 180, kount)
      IF kount GT 0 THEN yp(index180)=yp(index180)-180.
      index180=WHERE(yp LT 0, kount)
      IF kount GT 0 THEN yp(index180)=yp(index180)+180.
      ENDIF
      END
   'th2m':BEGIN
      ytit = 'Mean direction 2 (from, degrees)'
      IF typ EQ 1 THEN BEGIN
      index180=WHERE(yp GE 180, kount)
      IF kount GT 0 THEN yp(index180)=yp(index180)-180.
      index180=WHERE(yp LT 0, kount)
      IF kount GT 0 THEN yp(index180)=yp(index180)+180.
      ENDIF
      END
   'thu':ytit = 'theta_u  (degrees)'
   'sth1':BEGIN
         ats='at'
         ; ats=dictionnaire.at(ilang)
         IF (c_cut EQ 0) OR (c_cut EQ 3) OR (c_cut EQ 4) OR (c_cut GE 7) THEN BEGIN
               tit='Directional spread  (sth1m) '+ats+' '+pname
               ytit='sth1m (deg., '+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                  +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
         END
      END
   'sth1m':BEGIN
      IF typ EQ 0 THEN BEGIN 
         ytit = 'sigma_theta(fp) (degrees)'
         tit = 'Directional spread at '+pname
      ENDIF ELSE BEGIN 
          ats='at'
         ; ats=dictionnaire.at(ilang)
              tit='Directional spread  (sth1m) '+ats+' '+pname
               ytit='sth1m (deg., '+STRING(FREQ(findex-1),FORMAT='(F5.3)') $
                  +'-'+STRING(FREQ(findex2-1),FORMAT='(F5.3)')+' Hz)'
      ENDELSE
      END
   'sth2':ytit = 'Directional spread 2 (degrees)'
   'sth2m':ytit = 'Directional spread 2 (degrees)'
   ELSE:
   ENDCASE

   IF typ EQ 1 THEN BEGIN
      CASE c_cut OF
      0:BEGIN     ;time series of bulk parameter
         yp=FLTARR(nr,nt)
         yp(*,*)=y(*,c_spec,*)
         IF (datatype EQ 3) THEN BEGIN
            yp2=FLTARR(modspec.nrun,nt)
            yp2(*,*)=y2(*,c_spec,*)
         ENDIF
         END
      1:BEGIN     ;time series
         yp=FLTARR(nt)
         yp(*)=data(0,c_spec,plottype,findex-1,0,*)
         IF (datatype EQ 3) THEN BEGIN
            yp2=FLTARR(modspec.nrun,nt)
            yp2(*,*)=data2(*,c_spec,plottype,findex-1,0,*)
         ENDIF
         END
      2:BEGIN     ;f-spectrum
         x=f(findex-1:findex2-1)
         rangex=[min(x),max(x)]
         xtit='f (Hz)'
         yp=FLTARR(nr,findex2-findex+1)
         yp(*,*)=data(*,c_spec,plottype,*,0) 
         ;time average
         IF (smoothing GT 1) THEN BEGIN
            nstrue=MIN([indext2-indext+1,smoothing])
            FOR I=0,nstrue-1 DO yp(*)=yp(*)+data(0,c_spec,plottype,*,0,I)
            yp(*)=yp(*)/nstrue
         ENDIF
         IF (datatype EQ 3) THEN BEGIN
          yp2=FLTARR(modspec.nrun,findex2-findex+1)
            yp2(*,*)=data2(*,c_spec,plottype,*,0)  
            IF (smoothing GT 1) THEN BEGIN
               nstrue=MIN([indext2-indext+1,smoothing])
               FOR I=0,nstrue-1 DO yp2(*,*)=yp2(*,*)+data2(*,c_spec,plottype,*,0,I)
               yp2(*,*)=yp2(*,*)/nstrue
            ENDIF
         ENDIF
         rangey=[min(yp),max(yp)]
         IF fixrange THEN rangey=[mindepth,maxdepth]
         END
;
      3:BEGIN     ;mean f-spectrum
         x=f(findex-1:findex2-1)
         rangex=[min(x),max(x)]
         xtit='f (Hz)'
         yp=FLTARR(nr,findex2-findex+1)
         yp(*,*)=TOTAL(data(*,c_spec,plottype,*,0,*),6)/nt  
         IF (datatype EQ 3) THEN BEGIN
            yp2=FLTARR(modspec.nrun,findex2-findex+1)
            yp2(*,*)=TOTAL(data2(*,c_spec,plottype,*,0,*),6)/nt 
         ENDIF
         rangey=[min(yp),max(yp)]
         IF fixrange THEN rangey=[mindepth,maxdepth]
         END;
      4:BEGIN     
         kount=nt
         I=FINDGEN(NT)
         yp=FLTARR(nr,kount)
         yp(*)=data(*,c_spec,plottype,0,0,I)
         yp=FLTARR(nr,kount)
         yp(*,*)=y(*,c_spec,I)
         IF (datatype EQ 3) THEN BEGIN ;scatterplot of bulk
            yp2=FLTARR(modspec.nrun,kount)
            yp2(*,*)=y2(*,c_spec,I)
            x=yp
            rangex=[min(x),max(x)]
            IF fixrange THEN rangex=[mindepth,maxdepth]
            xtit='observations'
         ENDIF ELSE BEGIN 		;f-time plot
            yp2=FLTARR(nt,findex2-findex+1)
            yp2(*,*)=TRANSPOSE(data(0,c_spec,plottype,*,0,*))
            ytit='frequency (Hz)'
         ENDELSE
         END

      5:BEGIN     ;frequency-time plot
         yp=FLTARR(nt,findex2-findex+1)
         yp(*,*)=TRANSPOSE(data(0,c_spec,plottype,*,0,*))
         END
;
;Q-Q plot
;
      6:BEGIN
         nav=nbins;
         yp=FLTARR(1,nt)
         yp(0,*)=y(0,c_spec,*)
         indexnoz=WHERE(yp NE 0,kountz)
         print,'YP:',size(yp),max(indexnoz),min(indexnoz)
         ys=SORT(yp(indexnoz));
         taille=size(ys);
         nQ=taille(1)/nav;
         Indexz=WHERE(yp EQ 0,kountz)
         yQ=REBIN(yp(ys(0:nQ*nav-1)),nav)
         yp2=FLTARR(nt)
         dirs=FLTARR(nt)
         yp2(*)=y(0,c_spec,*)
         IF (datatype EQ 3) THEN BEGIN
            yp1=FLTARR(modspec.nrun,nt)
            yp3=FLTARR(modspec.nrun,nt)
            FOR K=0,modspec.nrun-1 DO BEGIN
               yp1(K,*)=y2(K,c_spec,*)
               yp3(K,indexnoz)=(y2(K,c_spec,indexnoz)-yp(indexnoz))^2
            ENDFOR
            yp2=REBIN(yp1(*,ys(0:nQ*nav-1)),modspec.nrun,nav)
            yp4=sqrt(REBIN(yp3(*,ys(0:nQ*nav-1)),modspec.nrun,nav)) ; STD dev of error
         ENDIF
         x=yQ
         yp=FLTARR(1,nav);
         yp(0,*)=yQ
         rangex=[min(x),max(x)]
         IF fixrange THEN rangex=[mindepth,maxdepth]
         xtit='observations'
         END
;
; Binned data plot: should make a separate subroutine !!
;
       8:BEGIN     
 
          yp=FLTARR(nr,nt)
         yp(*)=y(*,c_spec,*)
         yp2=FLTARR(modspec.nrun,nt)
         indexnoz=WHERE(yp NE 0,kountz)
         ys=yp(indexnoz);
         K=0 ; first model run
         x=yp;

         yp2(K,*)=y2(K,c_spec,*)
         y=yp2;

         rangex=[min(x),max(x)]
         rangey=[min(y),max(y)]
         IF fixrange THEN rangex=[mindepth,maxdepth]
         IF fixrange THEN rangey=[mindepth,maxdepth]
         nbin=nbins;
         makebin,x,y,rangex,rangey,nbin,nbin,bindata,bindataw, ibin,jbin
         xx=rangex(0)+FINDGEN(nbin)*(rangex(1)-rangex(0))/(nbin-1)
         yy=rangey(0)+FINDGEN(nbin)*(rangey(1)-rangey(0))/(nbin-1)
         fixrangememo=fixrange
         mindepthmemo=mindepth
         maxdepthmemo=maxdepth
         mindepth=0
         maxdepth=1
         fixrange=1
         
         NMAX=MAX(bindata)
         bindata=bindata/NMAX
         IND=WHERE(bindata EQ 0,kount)
         IF (kount GT 0) THEN bindata(IND)=2*maxval
         ;IND=WHERE(bindata LT maxval,kount)
         ;IF (kount GT 0) THEN bindata(IND)=bindata(IND)/nmax
         IF outxtit THEN xtit='observations' ELSE xtit=''
         IF outxtit THEN ytit='model' ELSE xtit=''
         Rescale,1
         POSBOX=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         PLOT,rangex,rangey,XSTYLE=1,YSTYLE=1, $ ;xticks=cbnticks, yticks=cbnticks, $
            XTITLE=xtit,YTITLE=ytit,TITLE=tit,MAX_VALUE=maxval, $
            xrange=rangex,yrange=rangey,THICK=3,/NODATA,LINESTYLE=2, $
            YGRIDSTYLE=2,Yticklen=1, XGRIDSTYLE=2,Xticklen=1 , POSITION=POSBOX
         IF outcbtit THEN cbtit='Normalized occurence (nmax='+STRING(NMAX,FORMAT='(I6)')+')' $
             ELSE cbtit=''
         binplot=bindata
         Contorno,binplot,xx,yy,'',xtit,ytit,cbtit,0,0,1
         xm1=FLTARR(nbin)
         ym1=xm1
         xm2=xm1
         ym2=ym1
         FOR I=0,nbin-1 DO BEGIN
           IND=WHERE(ibin EQ I,kount)
            IF (kount GT 0) THEN BEGIN
               xm1(I)=TOTAL(x(IND))/kount
               ym1(I)=TOTAL(y(IND))/kount
               xm2(I)=MEDIAN(x(IND))
               ym2(I)=MEDIAN(y(IND))
            ENDIF ELSE BEGIN
                 xm1(I)=maxval*1.2
                 ym1(I)=maxval*1.2
                 xm2(I)=maxval*1.2
                 ym2(I)=maxval*1.2
               ENDELSE
         ENDFOR
         OPLOT,xm1,ym1,THICK=4,LINESTYLE=0,MAX_VALUE=maxval
         ;OPLOT,xm2,ym2,THICK=2,LINESTYLE=0,MAX_VALUE=maxval,COLOR=120
         OPLOT,rangex,rangey,THICK=3,LINESTYLE=2
         mindepth=mindepthmemo
         maxdepth=maxdepthmemo
         fixrange=fixrangememo
         RETURN
         END

       9:BEGIN     ;mss avg in bin ... JEJE PLOT
         varn=STRMID(varname,0,3)
         IU10=-1
         IHs=-1
         Imsc=-1
         FOR J=obs2.nvar-1,0,-1  DO BEGIN
;
;  Looks for wind and other variables
;
            var3=STRMID((*obs2.varnames)[J],0,3)
             IF (var3 EQ 'U10') THEN IU10=J
             IF (var3 EQ 'msc') THEN Imsc=J
             ENDFOR
;
         FOR J=obs2.nvar-1,0,-1  DO BEGIN
            var2=STRMID((*obs2.varnames)[J],0,2)
            IF (var2 EQ 'Hs') THEN IHs=J
             ENDFOR
         IF (iHS GE 0 AND iU10 GE 0) THEN BEGIN

         yp=FLTARR(nt)
         IF (datatype EQ 1) THEN yp(*)=y(0,c_spec,*) ELSE  yp(*)=y2(0,c_spec,*)


         x=FLTARR(nt)
         y=FLTARR(nt)
         data02=(*obs2.data)

         FOR I=0L,nt-1 DO $
            IF ((*obs2.flags)[indext+I] NE 0) THEN BEGIN
               x(I)=data02(0,c_spec,IU10,0,0,I)
               y(I)=data02(0,c_spec,IHs,0,0,I)
               ENDIF

         indexnoz=WHERE(yp NE 0 AND x LT 45,kountz)

         ys=yp(indexnoz);

         IF fixrange THEN rangex=[mindepth,maxdepth]
         IF fixrange THEN rangey=[mindepth,maxdepth]
         hsmax=maxdepth;
         nbiny=nbins;
         nbinx=26
         rangex=[0,25]; [min(x),max(x)]
         hsmin=0.0;
         rangey=[hsmin,hsmax]

         ys2=FLTARR(kountz);
         FOR I=0L,kountz-1 DO BEGIN
            ys2(I)=0.85*(1.25-0.25*(0.5/freq(findex2-1))^1.3)*2*(0.000295*x(indexnoz(I)) $
                                   *MIN([x(indexnoz(I)),14.5])+(y(indexnoz(I))-0.0)*0.0135);
            ys2(I)=1*(1.25-0.25*(0.5/freq(findex2-1))^1.3)*(0.00059*x(indexnoz(I)) $
                                       *MIN([x(indexnoz(I)),14.5])+(y(indexnoz(I))-0.4)*0.027);
            ENDFOR

         makebin,x(indexnoz),y(indexnoz),rangex,rangey,nbinx,nbiny,bindata,bindataw, $
            ibin,jbin ,binweight=ys
         xx=rangex(0)+FINDGEN(nbinx)*(rangex(1)-rangex(0))/(nbinx-1)
         yy=hsmin+FINDGEN(nbiny)*(hsmax-hsmin)/(nbiny-1)
         yyy=bindataw(ibin,jbin)


         var1=sqrt(mean((ys-yyy)^2))
         var2=sqrt(mean((ys2-ys)^2))
         rms1=sqrt(mean(ys^2))
         PRINT,'fcut:',freq(findex2-1),'rel to 0.5:',(1.125-0.0625/freq(findex2-1))
         PRINT,'Variance left:',var1,var2,mean(ys-ys2),100*var1/rms1,100*var2/rms1
         IF outxtit THEN xtit='U10 (m/s)' ELSE xtit=''
         IF outytit THEN ytit=ytit ELSE xtit=''
         rangey=[min(ys),max(ys)]
         Rescale,0
         PLOT,rangex,rangey,XSTYLE=1,YSTYLE=1, $ ;xticks=cbnticks, yticks=cbnticks, $
            XTITLE=xtit,YTITLE=ytit,TITLE=tit,MAX_VALUE=maxval, $
            xrange=rangex,yrange=rangey,THICK=3,/NODATA,LINESTYLE=2, $
            YGRIDSTYLE=2,Yticklen=1, XGRIDSTYLE=2,Xticklen=1 , POSITION=POSBOX
         IF outcbtit THEN cbtit='Hs (m)' ELSE cbtit=''
         c_numlev=Navailcolor-4

         zmin=min(y)
         zmax=max(y)
         IF fixrange THEN BEGIN
            mindata=mindepth
            maxdata=maxdepth
         ENDIF ELSE BEGIN
            maxdata=MIN([zmax,maxdepth])
            mindata=MAX([zmin,mindepth])
         ENDELSE
         IF (mindepth GT zmin) THEN zmin=mindepth
         IF (maxdepth LT zmax) THEN zmax=maxdepth

         MakeLevels,mindata,maxdata,zmin,zmax
         colorind=1+FINDGEN(navailcolor-5+addmini+addmaxi)
         image=1+ BYTSCL(yy,min=mindata,max=maxdata,Top=(navailcolor-4))

         FOR I=0,nbinx-1 DO BEGIN
         FOR J=0,nbiny-1 DO BEGIN
            ;cindex=MAX(MIN(ROUND((y(J)-zmin)/(zmax-zmin))*255,255),0)
            IF (bindata(I,J) GT 0) THEN BEGIN
            II=WHERE(IBIN EQ I AND JBIN EQ J,kount)
               IF (kount GE 2) THEN BEGIN
                   err=0.5*STDDEV(ys(II)-bindataw(I,J))
                   PLOTS,xx(I),[bindataw(I,J)-err,bindataw(I,J)+err],THICK=2
                   err=STDDEV(ys(II)-bindataw(I,J))
               ENDIF
               PLOTS,xx(I),bindataw(I,J),THICK=2,PSYM=-4-(J MOD 4),COLOR=image(J),SYMSIZE=2
               ;PLOTS,xx(I),0.02+3.2*0.000295*xx(I)*MIN([xx(I),14.5])+(YY(J)-0.0)*0.0135,THICK=2,PSYM=-4-(J MOD 4),COLOR=image(J),SYMSIZE=2
               ;PLOTS,xx(I),0.00028*xx(I)+(YY(J)-0.0)*0.022,THICK=2,PSYM=-4-(J MOD 4),COLOR=image(J),SYMSIZE=2
            ENDIF
         ENDFOR
         ENDFOR
         if cbar THEN ColorBar,lev,cbtit,zmin,zmax
         ENDIF
         RETURN
         END

      ELSE: BEGIN 
       Print,'This plot option is not available'
      RETURN 
         END 
      ENDCASE
   ENDIF




   IF typ EQ 0 THEN BEGIN ; For data files with bulk parameters. 
                          ; some of the code here can probably be merged with typ EQ 1 code ... 

         yp=FLTARR(nr,nt)
         yp(*)=data(*,c_spec,plottype,0,0,*)
         FOR K=0,nr-1 DO BEGIN
            yp(K,*)=data(K,c_spec,plottype,0,0,*)
         ENDFOR
         yp2=FLTARR(nt)
         dirs=FLTARR(nt)
         yp2(*)=data(0,c_spec,plottype,0,0,*)
         IF (datatype EQ 3) THEN BEGIN
            yp2=FLTARR(modspec.nrun,nt)
            FOR K=0,modspec.nrun-1 DO BEGIN
               yp2(K,*)=data2(K,c_spec,plottype,0,0,*)
            ENDFOR
         ENDIF


      CASE c_cut OF
;
;scatterplot  for typ EQ 0
;
      4:IF (datatype EQ 3) THEN BEGIN
          yp=FLTARR(nr,nt)
          yp(*)=data(*,c_spec,plottype,0,0,*)
          FOR K=0,nr-1 DO BEGIN
            yp(K,*)=data(K,c_spec,plottype,0,0,*)
          ENDFOR
          yp2=FLTARR(nt)
          dirs=FLTARR(nt)
          yp2(*)=data(0,c_spec,plottype,0,0,*)
          yp2=FLTARR(modspec.nrun,nt)
          FOR K=0,modspec.nrun-1 DO BEGIN
            yp2(K,*)=data2(K,c_spec,plottype,0,0,*)
          ENDFOR
          x=yp
          rangex=[min(x),max(x)]
          IF fixrange THEN rangex=[mindepth,maxdepth]
          xtit='observations'
        ENDIF
;
;Q-Q plot for typ EQ 0
;
      5:BEGIN     
         nav=nbins;
         yp=FLTARR(nr,nt)
         yp(*)=data(*,c_spec,plottype,0,0,*)
         indexnoz=WHERE(yp NE 0,kountz)
         ys=SORT(yp(indexnoz));
         taille=size(ys);

         nQ=taille(1)/nav;
         Indexz=WHERE(yp EQ 0,kountz)
         yQ=REBIN(yp(0,ys(0:nQ*nav-1)),nr,nav)
         FOR K=0,nr-1 DO BEGIN
            yp(K,*)=data(K,c_spec,plottype,0,0,*)
         ENDFOR
         yp2=FLTARR(nt)
         dirs=FLTARR(nt)
         yp2(*)=data(0,c_spec,plottype,0,0,*)
         ;yp2=0.6*data(0,0,0,0,0,*)/data(0,0,2,0,0,*)
         IF (datatype EQ 3) THEN BEGIN
            yp1=FLTARR(modspec.nrun,nt)
            yp3=FLTARR(modspec.nrun,nt)
            FOR K=0,modspec.nrun-1 DO BEGIN
               yp1(K,*)=data2(K,c_spec,plottype,0,0,*)
               yp3(K,indexnoz)=(data2(K,c_spec,plottype,0,0,indexnoz)-yp(indexnoz))^2
            ENDFOR
            yp2=REBIN(yp1(*,ys(0:nQ*nav-1)),modspec.nrun,nav)
            yp4=sqrt(REBIN(yp3(*,ys(0:nQ*nav-1)),modspec.nrun,nav)) ; STD dev of error
         ENDIF
         x=yQ
         yp=FLTARR(1,nav);
         yp(0,*)=yQ
         rangex=[min(x),max(x)]
         IF fixrange THEN rangex=[mindepth,maxdepth]
         xtit='observations'
         END

;
; Binned data plot for typ EQ 0
;
       6:BEGIN     
         yp=FLTARR(nr,nt)
         yp(*)=data(*,c_spec,plottype,0,0,*)
         yp2=FLTARR(modspec.nrun,nt)
         indexnoz=WHERE(yp NE 0,kountz)
         ys=yp(indexnoz);
         K=0 ; first model run
         x=yp;
         yp2(K,*)=data2(K,c_spec,plottype,0,0,*)
         y=yp2;
         rangex=[min(x),max(x)]
         rangey=[min(y),max(y)]
         IF fixrange THEN rangex=[mindepth,maxdepth]
         IF fixrange THEN rangey=[mindepth,maxdepth]
         nbin=nbins;
         makebin,x,y,rangex,rangey,nbin,nbin,bindata,bindataw, ibin,jbin
         xx=rangex(0)+FINDGEN(nbin)*(rangex(1)-rangex(0))/(nbin-1)
         yy=rangey(0)+FINDGEN(nbin)*(rangey(1)-rangey(0))/(nbin-1)
         fixrangememo=fixrange
         mindepthmemo=mindepth
         maxdepthmemo=maxdepth
         mindepth=0
         maxdepth=1
         fixrange=1
         
         NMAX=MAX(bindata)
         bindata=bindata/NMAX
         IND=WHERE(bindata EQ 0,kount)
         IF (kount GT 0) THEN bindata(IND)=2*maxval
         ;IND=WHERE(bindata LT maxval,kount)
         ;IF (kount GT 0) THEN bindata(IND)=bindata(IND)/nmax
         IF outxtit THEN xtit='observations' ELSE xtit=''
         IF outxtit THEN ytit='model' ELSE xtit=''
         Rescale,1
         POSBOX=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         PLOT,rangex,rangey,XSTYLE=1,YSTYLE=1, $ ;xticks=cbnticks, yticks=cbnticks, $
            XTITLE=xtit,YTITLE=ytit,TITLE=tit,MAX_VALUE=maxval, $
            xrange=rangex,yrange=rangey,THICK=3,/NODATA,LINESTYLE=2, $
            YGRIDSTYLE=2,Yticklen=1, XGRIDSTYLE=2,Xticklen=1 , POSITION=POSBOX
         IF outcbtit THEN cbtit='Normalized occurence (nmax='+STRING(NMAX,FORMAT='(I6)')+')' $
             ELSE cbtit=''
         binplot=bindata
         Contorno,binplot,xx,yy,'',xtit,ytit,cbtit,0,0,1
 ;        IF filltype LE 4 THEN Drawframe,xtit,' ',ytit,' ',FRAME=1
         xm1=FLTARR(nbin)
         ym1=xm1
         xm2=xm1
         ym2=ym1
         FOR I=0,nbin-1 DO BEGIN
           IND=WHERE(ibin EQ I,kount)
            IF (kount GT 0) THEN BEGIN
               xm1(I)=TOTAL(x(IND))/kount
               ym1(I)=TOTAL(y(IND))/kount
               xm2(I)=MEDIAN(x(IND))
               ym2(I)=MEDIAN(y(IND))
            ENDIF ELSE BEGIN
                 xm1(I)=maxval*1.2
                 ym1(I)=maxval*1.2
                 xm2(I)=maxval*1.2
                 ym2(I)=maxval*1.2
               ENDELSE
         ENDFOR
         OPLOT,xm1,ym1,THICK=4,LINESTYLE=0,MAX_VALUE=maxval
         ;OPLOT,xm2,ym2,THICK=2,LINESTYLE=0,MAX_VALUE=maxval,COLOR=120
         OPLOT,rangex,rangey,THICK=3,LINESTYLE=2
         mindepth=mindepthmemo
         maxdepth=maxdepthmemo
         fixrange=fixrangememo
         RETURN
         END
     7:BEGIN     ;mss avg in bin ... JEJE PLOT
         varn=STRMID(varname,0,3)
         IU10=-1
         IHs=-1
         Imsc=-1
         IF datatype NE 1 THEN BEGIN        ; model
            FOR J=modspec.nvar-1,0,-1  DO BEGIN
              var3=STRMID((*modspec.varnames)[J],0,3)
               IF (var3 EQ 'U10') THEN IU10=J
               IF (var3 EQ 'msc') THEN Imsc=J
             ENDFOR
;            FOR J=obs.nvar-1,0,-1  DO BEGIN
            FOR J=modspec.nvar-1,0,-1  DO BEGIN
               var2=STRMID((*modspec.varnames)[J],0,2)
               IF (var2 EQ 'Hs') THEN IHs=J
             ENDFOR
            IF (iHS GE 0 AND iU10 GE 0) THEN BEGIN
             yp=FLTARR(nr,nt)
             IF Imsc GE 0 THEN BEGIN
               ymsc=FLTARR(nt)
               data0=(*modspec.data)
               FOR I=0L,nt-1 DO $
               IF ((*modspec.flags)[indext+I] NE 0) THEN BEGIN
                 J=0L+(*om.datem)[indext+I]
                 ymsc(I)=data0(0,c_spec,Imsc,(*om.fm)[findex-1:findex2-1],0,J)
               ENDIF
               yp(*)=data0(0,c_spec,plottype,0,0,*); ymsc(*)*1.5
             ENDIF ELSE BEGIN
               yp(*)=data0(0,c_spec,plottype,0,0,*)
             ENDELSE

             data0=(*modspec.data)
             data1=(*obs.data)
             x=FLTARR(nt)
             y=FLTARR(nt)
             FOR I=0L,nt-1 DO $
             IF ((*modspec.flags)[indext+I] NE 0) THEN BEGIN
               J=0L+(*om.datem)[indext+I]
               ;JO=0L+(*om.dateo)[indext+I]  ; index for obs ... 
               x(I)=data0(0,c_spec,IU10,(*om.fm)[findex-1:findex2-1],0,J)
               y(I)=data0(0,c_spec,IHs,(*om.fm)[findex-1:findex2-1],0,J)
;               y(I)=data1(0,c_spec,IHs,(*obs.f)[findex-1:findex2-1],0,JO)
             ENDIF
            ENDIF
         ENDIF ELSE BEGIN   ; observations
            FOR J=obs.nvar-1,0,-1  DO BEGIN
          ;  FOR J=modspec.nvar-1,0,-1  DO BEGIN
              var3=STRMID((*modspec.varnames)[J],0,3)
               IF (var3 EQ 'U10') THEN IU10=J
               IF (var3 EQ 'msc') THEN Imsc=J
             ENDFOR
            FOR J=obs.nvar-1,0,-1  DO BEGIN
               var2=STRMID((*obs.varnames)[J],0,2)
               IF (var2 EQ 'Hs') THEN IHs=J
             ENDFOR

            IF (iHS GE 0 AND iU10 GE 0) THEN BEGIN
             yp=FLTARR(nt)
             yp(*)=data(0,c_spec,plottype,0,0,*)
            ENDIF ELSE BEGIN
             yp(*)=data(0,c_spec,plottype,0,0,*)
            ENDELSE

           data0=(*modspec.data)
              x=FLTARR(nt)
            y=FLTARR(nt)
            FOR I=0L,nt-1 DO $
           IF ((*obs.flags)[indext+I] NE 0) THEN BEGIN
              x(I)=data0(0,c_spec,IU10,(*om.fm)[findex-1:findex2-1],0,I)
              ; x(I)=data(0,c_spec,IU10,(*obs.f)[findex-1:findex2-1],0,I)
            y(I)=data(0,c_spec,IHs,(*obs.f)[findex-1:findex2-1],0,I)
           ENDIF
         ENDELSE ; end of if test on model / obs



         IF (iHS GE 0 AND iU10 GE 0) THEN BEGIN
         indexnoz=WHERE(yp NE 0 AND x LT 45,kountz)
         ys=yp(indexnoz);
         IF fixrange THEN rangex=[mindepth,maxdepth]
         IF fixrange THEN rangey=[mindepth,maxdepth]
         nbiny=nbins;
         nbinx=26
         rangex=[0,25]; [min(x),max(x)]
         hsmin=0.0;
         hsmax=13 ;
         rangey=[hsmin,hsmax]

         ys2=ys;

         makebin,x(indexnoz),y(indexnoz),rangex,rangey,nbinx,nbiny,bindata,bindataw, $
            ibin,jbin ,binweight=ys
         xx=rangex(0)+FINDGEN(nbinx)*(rangex(1)-rangex(0))/(nbinx-1)
         yy=hsmin+FINDGEN(nbiny)*(hsmax-hsmin)/(nbiny-1)
         yyy=bindataw(ibin,jbin)
         varl=sqrt(mean((ys-yyy)^2))
         var2=sqrt(mean((ys2-ys)^2))
         rms1=sqrt(mean(ys^2))
         PRINT,'Variance left:',varl,var2,mean(ys-ys2),100*var2/rms1
         IF outxtit THEN xtit='U10 (m/s)' ELSE xtit=''
         IF outxtit THEN ytit=varname ELSE xtit=''
         rangey=[min(ys),max(ys)]
         Rescale,0
         PLOT,rangex,rangey,XSTYLE=1,YSTYLE=1, $ ;xticks=cbnticks, yticks=cbnticks, $
            XTITLE=xtit,YTITLE=ytit,TITLE=tit,MAX_VALUE=maxval, $
            xrange=rangex,yrange=rangey,THICK=3,/NODATA,LINESTYLE=2, $
            YGRIDSTYLE=2,Yticklen=1, $
            XGRIDSTYLE=2,Xticklen=1 , $
            POSITION=POSBOX
         IF outcbtit THEN cbtit='Hs (m)' ELSE cbtit=''
         c_numlev=Navailcolor-4

         zmin=min(y)
         zmax=max(y)
         IF fixrange THEN BEGIN
            mindata=mindepth
            maxdata=maxdepth
         ENDIF ELSE BEGIN
            maxdata=MIN([zmax,maxdepth])
            mindata=MAX([zmin,mindepth])
         ENDELSE
         IF (mindepth GT zmin) THEN zmin=mindepth
         IF (maxdepth LT zmax) THEN zmax=maxdepth

         MakeLevels,mindata,maxdata,zmin,zmax
         colorind=1+FINDGEN(navailcolor-5+addmini+addmaxi)
         image=1+ BYTSCL(yy,min=mindata,max=maxdata,Top=(navailcolor-4))

         FOR I=0,nbinx-1 DO BEGIN
         FOR J=0,nbiny-1 DO BEGIN
            ;cindex=MAX(MIN(ROUND((y(J)-zmin)/(zmax-zmin))*255,255),0)
            IF (bindata(I,J) GT 0) THEN BEGIN
            II=WHERE(IBIN EQ I AND JBIN EQ J,kount)
               IF (kount GE 2) THEN BEGIN
                   err=0.5*STDDEV(ys(II)-bindataw(I,J))
                   PLOTS,xx(I),[bindataw(I,J)-err,bindataw(I,J)+err],THICK=2
                   err=STDDEV(ys(II)-bindataw(I,J))
               ENDIF
               PLOTS,xx(I),bindataw(I,J),THICK=2,PSYM=-4-(J MOD 4),COLOR=image(J),SYMSIZE=2
            ENDIF
         ENDFOR
         ENDFOR
         if cbar THEN ColorBar,lev,cbtit,zmin,zmax
         ENDIF
       ;  ENDIF
         RETURN
         END
     8:BEGIN     ;error map
         varn=STRMID(varname,0,3)
         Ilat=-1
         Ilon=-1
         names=(*obs.varnames)
         FOR J=obs.nvar-1,0,-1  DO BEGIN
             var3=STRMID(names(J),0,3)
             IF (var3 EQ 'lon') THEN Ilon=J
             IF (var3 EQ 'lat') THEN Ilat=J
         ENDFOR
         IF (ilon GE 0 AND ilat GE 0) THEN BEGIN
         yp=FLTARR(nr,nt)
         yp(*)=data2(0,c_spec,plottype,0,0,*)-data(0,c_spec,plottype,0,0,*)

         data0=(*modspec.data)
         x=FLTARR(nt)
         y=FLTARR(nt)
         FOR I=0L,nt-1 DO $
         IF ((*modspec.flags)[indext+I] NE 0) THEN BEGIN
            J=0L+(*om.datem)[indext+I]
            x(I)=data0(0,c_spec,Ilon,(*om.fm)[findex-1:findex2-1],0,J)
            y(I)=data0(0,c_spec,Ilat,(*om.fm)[findex-1:findex2-1],0,J)
         ENDIF
          indexnoz=WHERE(yp NE 0,kountz)
          ys=yp(indexnoz);
         IF fixrange THEN rangex=[mindepth,maxdepth]
         IF fixrange THEN rangey=[mindepth,maxdepth]
         nbiny=nbins;
         nx=180
         ny=72

         rangex=[0,360]; [min(x),max(x)]
         rangey=[-72,72]
         makebin,x(indexnoz),y(indexnoz),rangex,rangey,nx,ny,bindata,bindataw, $
            ibin,jbin ,binweight=ys
         IZ=WHERE(bindata EQ 0,kount)
         bindataw(IZ)=2*maxval
         xx=rangex(0)+FINDGEN(nx)*(rangex(1)-rangex(0))/(nx-1)
         yy=min(y)+FINDGEN(ny)*(max(y)-min(y))/(ny-1)

         IF outxtit THEN cbtit='mss bias' ELSE cbtit=''
         rlonmax=max(xx)
         rlonmin=min(xx)
         rlatmax=max(yy)
         rlatmin=min(yy)

         nxzmin=0
         nyzmin=0
         nxzmax=nx-1
         nyzmax=ny-1
         dx=(rlonmax-rlonmin)*(40000./((nx-1)*360.))*cos(0.5*(rlatmin+rlatmax)*!dtor)
         dy=(rlatmax-rlatmin)*(40000./((ny-1)*360.))

         dlat=rlatmax-rlatmin

         Rescale,1

         PLOT,rangex,rangey,XSTYLE=1,YSTYLE=1, $ ;xticks=cbnticks, yticks=cbnticks, $
            XTITLE=xtit,YTITLE=ytit,TITLE=tit,MAX_VALUE=maxval, $
            xrange=rangex,yrange=rangey,THICK=3,/NODATA,LINESTYLE=2, $
            YGRIDSTYLE=2,Yticklen=1, $
            XGRIDSTYLE=2,Xticklen=1 , $
            POSITION=POSBOX
         Contorno,bindataw,xx,yy,tit,'x (km)','y (km)',cbtit,0,0,0
         IF (filltype LE 4)  THEN BEGIN
         !P.CLIP=[min(x),min(y),0,max(x),max(y),1]
            ad2dmisc
         ENDIF

         ENDIF
         RETURN
         END
     9:BEGIN     ;rms error map for typ EQ 0
         varn=STRMID(varname,0,3)
         Ilat=-1
         Ilon=-1
         names=(*obs.varnames)
         FOR J=obs.nvar-1,0,-1  DO BEGIN
             var3=STRMID(names(J),0,3)
             IF (var3 EQ 'lon') THEN Ilon=J
             IF (var3 EQ 'lat') THEN Ilat=J
         ENDFOR
         IF (ilon GE 0 AND ilat GE 0) THEN BEGIN
         yp=FLTARR(nt)
         yp(*)=data2(0,c_spec,plottype,0,0,*)-data(0,c_spec,plottype,0,0,*)

         data0=(*modspec.data)
         x=FLTARR(nt)
         y=FLTARR(nt)
         FOR I=0L,nt-1 DO $
         IF ((*modspec.flags)[indext+I] NE 0) THEN BEGIN
            J=0L+(*om.datem)[indext+I]
            x(I)=data0(0,c_spec,Ilon,(*om.fm)[findex-1:findex2-1],0,J)
            y(I)=data0(0,c_spec,Ilat,(*om.fm)[findex-1:findex2-1],0,J)
         ENDIF
          indexnoz=WHERE(yp NE 0,kountz)
          ys=yp(indexnoz);
         IF fixrange THEN rangex=[mindepth,maxdepth]
         IF fixrange THEN rangey=[mindepth,maxdepth]
         nbiny=nbins;
         nx=180
         ny=72

         rangex=[0,360]; [min(x),max(x)]
         rangey=[-72,72]
         makebin,x(indexnoz),y(indexnoz),rangex,rangey,nx,ny,bindata,bindataw0, $
            ibin,jbin ,binweight=ys

         ys=FLTARR(kountz)
         ys(*)=(data2(0,c_spec,plottype,0,0,indexnoz)-  $
                 data(0,c_spec,plottype,0,0,indexnoz))^2
         ;-bindataw0(ibin(indexnoz),jbin(indexnoz))
         makebin,x(indexnoz),y(indexnoz),rangex,rangey,nx,ny,bindata,bindataw, $
            ibin,jbin ,binweight=ys
         IZ=WHERE(bindata EQ 0,kount)
         bindataw=sqrt(bindataw)
         bindataw(IZ)=2*maxval
         xx=rangex(0)+FINDGEN(nx)*(rangex(1)-rangex(0))/(nx-1)
         yy=min(y)+FINDGEN(ny)*(max(y)-min(y))/(ny-1)

         IF outxtit THEN cbtit='RMSE' ELSE cbtit=''
         rlonmax=max(xx)
         rlonmin=min(xx)
         rlatmax=max(yy)
         rlatmin=min(yy)

         nxzmin=0
         nyzmin=0
         nxzmax=nx-1
         nyzmax=ny-1
         dx=(rlonmax-rlonmin)*(40000./((nx-1)*360.))*cos(0.5*(rlatmin+rlatmax)*!dtor)
         dy=(rlatmax-rlatmin)*(40000./((ny-1)*360.))

         dlat=rlatmax-rlatmin

         Rescale,1

         PLOT,rangex,rangey,XSTYLE=1,YSTYLE=1, $ ;xticks=cbnticks, yticks=cbnticks, $
            XTITLE=xtit,YTITLE=ytit,TITLE=tit,MAX_VALUE=maxval, $
            xrange=rangex,yrange=rangey,THICK=3,/NODATA,LINESTYLE=2, $
            YGRIDSTYLE=2,Yticklen=1, XGRIDSTYLE=2,Xticklen=1 , POSITION=POSBOX
         Contorno,bindataw,xx,yy,tit,'x (km)','y (km)',cbtit,0,0,0
         IF (filltype LE 4)  THEN BEGIN
         !P.CLIP=[min(x),min(y),0,max(x),max(y),1]
            ad2dmisc
         ENDIF

         ENDIF
         RETURN
         END      
     10:BEGIN     ;diff vs fp
         IF (datatype EQ 3) THEN BEGIN

            FOR I=0,nt-1 DO BEGIN
               IF (yp(0,I) EQ 0.) OR (yp2(0,I) EQ 0.) THEN yp2(*,I)=0. $
                  ELSE yp2(*,I)=(yp2(*,I)-yp(0,I))/yp(0,I)
            ENDFOR
            yp(*,*)=0.
         ENDIF
         x=FLTARR(nt)
         FOR I=0,nt-1 DO BEGIN
            ;print,I,yp2(0,I),yp(0,I),data0(0,((*om.po)[c_spec]),7,0,(*om.dateo)[indext+I])
            ;x(I)=data0(0,(*om.po)[c_spec],7,0,(*om.dateo)[indext+I])
            x(I)=data0(0,(*om.po)[c_spec],2,0,(*om.dateo)[indext+I])
         ENDFOR
         rangex=[0.06,0.12]
         xtit='fp (Hz)'
         ytit='Relative error on '+varname+' at '+pname
         END

      11:BEGIN     ;diff vs value
         IF (datatype EQ 3) THEN BEGIN

            FOR I=0,nt-1 DO BEGIN
               IF (yp(0,I) EQ 0.) OR (yp2(0,I) EQ 0.) THEN yp2(*,I)=0. $
;                  ELSE yp2(*,I)=(yp2(*,I)-yp(0,I))/yp(0,I)
                  ELSE yp2(*,I)=yp2(*,I)
            ENDFOR
         ENDIF
         x=FLTARR(nt)
         FOR I=0,nt-1 DO BEGIN
            x(I)=data0(0,5,(*om.vo)[plottype],0,0,(*om.dateo)[indext+I])
         ENDFOR
         yp(*,*)=0.
         rangex=[Min(x),MAX(x)]
         xtit='Measured '+varname+' at X6'
         ytit='Measured '+varname+' at '+pname
         END

      ELSE:
      ENDCASE
      IF (varname EQ 'th1') THEN BEGIN
      index180=WHERE(yp LT 0, kount)
      IF kount GT 0 THEN yp(index180)=yp(index180)+360.
      ;index180=WHERE(yp GT 180, kount)
      ;IF kount GT 0 THEN yp(index180)=4.*maxval
      index180=WHERE(yp2 LT 0, kount)
      IF kount GT 0 THEN yp2(index180)=yp2(index180)+360.
      ENDIF
      IF (varname EQ 'th2') THEN BEGIN
      index180=WHERE(yp GE 180, kount)
      IF kount GT 0 THEN yp(index180)=yp(index180)-180.
      index180=WHERE(yp2 LT 0, kount)
      IF kount GT 0 THEN yp2(index180)=yp2(index180)+180.
      ENDIF
   ENDIF



   IF varname EQ 'th2' THEN BEGIN
      indexpi=WHERE(yp LT 0,kountpi)
      IF kountpi NE 0 THEN yp(indexpi)=yp(indexpi)+180.
      indexpi=WHERE(yp GE 180,kountpi)
      IF kountpi NE 0 THEN yp(indexpi)=yp(indexpi)-180.
   ENDIF
   IF varname EQ 'th2m' THEN BEGIN
      indexpi=WHERE(yp GE 180.,kountpi)
      IF kountpi NE 0 THEN yp(indexpi)=yp(indexpi)-180.
      indexpi=WHERE(yp LT 0,kountpi)
      IF kountpi NE 0 THEN yp(indexpi)=yp(indexpi)+180.
   ENDIF
   taille=size(yp)
   ypnoz=yp

   Indexz=WHERE(yp EQ 0,kountz)
   Indexnoz=WHERE(yp NE 0,kountnoz)
   wherez=yp*0
   IF (kountnoz GT 0) THEN wherez(Indexnoz)=1
   wherez2=wherez
   kountz2=0
   IF (typ EQ 0 AND taille(2) GT 3 AND taille(0) EQ 2) THEN BEGIN
      wherez2(*,1:taille(2)-1)=wherez2(*,1:taille(2)-1)+ $
         wherez(*,0:taille(2)-2)
      wherez2(*,0:taille(2)-2)=wherez2(*,0:taille(2)-2)+ $
         wherez(*,1:taille(2)-1)
      Indexz2=WHERE((wherez2 EQ 1) OR (wherez2 EQ 2),kountz2)
   ENDIF
   IF kountz GT 0 THEN ypnoz(Indexz)=4.*maxval
   rangey=[min(ypnoz),max(yp)]
   IF (datatype EQ 3) THEN BEGIN
      yp2noz=yp2
      Indexnoz2=WHERE(yp2 EQ 0,kountnoz2)
      IF kountnoz2 GT 0 THEN yp2noz(Indexnoz2)=4.*maxval
      rangey=[min([min(ypnoz),min(yp2noz)]),max([max(yp),max(yp2)])]
   ENDIF

   IF C_cut EQ 2 THEN BEGIN
      y=f(findex-1:findex2-1)
      cbtit=ytit
      ytit='E(f) (m2/Hz)'
   ENDIF
   IF fixrange THEN rangey=[mindepth,maxdepth]


   IF (kountnoz EQ 0) THEN PRINT,'All data are zero' ELSE BEGIN
   Rescale,0
   nonames=REPLICATE(' ',30)

   IF outtit EQ 0 THEN tit=''
   IF outxtit EQ 0 THEN xtit=''
   IF outytit EQ 0 THEN ytit=''

   ts_processed = c_cut
   IF (typ EQ 0 AND c_cut LT 4) THEN ts_processed = 0

   IF (NT GT 1) THEN BEGIN 
; Plots with orientation "portrait" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  IF axis_orient EQ 0 THEN BEGIN
      IF (datatype EQ 3 OR ts_processed NE 4) THEN BEGIN
          IF (ts_processed NE 4 AND ts_processed NE 7) THEN BEGIN
            IF (N_ELEMENTS(xposi) GT 1) THEN BEGIN
               PLOT,x,ypnoz(0,*),/NODATA,XSTYLE=1,YSTYLE=0, $; yticks=cbnticks, $ ;Ytickname=nonames, $
               xminor=xnminor,xticks=xnticks,xtickname=xtnames, $
               XTITLE=xtit,YTITLE=ytit,TITLE=tit,MAX_VALUE=maxval, $
               xrange=rangex,yrange=rangey,THICK=3, $
               YGRIDSTYLE=2,Yticklen=1, $
               POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
            ENDIF ELSE BEGIN
print,'TEST:',c_cut,ts_processed 
               PLOT,x,ypnoz(0,*),XSTYLE=1, YSTYLE=0, $
                      XTITLE=xtit,YTITLE=ytit,TITLE=tit,MAX_VALUE=maxval, $
                      xrange=rangex,yrange=rangey,THICK=3, POSITION=POSBOX
            ENDELSE
;
; Adds model runs for comparison
;
            IF (datatype EQ 3) THEN BEGIN
               FOR I=0,modspec.nrun-1 DO BEGIN
                  OPLOT,x,yp2noz(I,*),LINESTYLE=I+1,MAX_VALUE=maxval,THICK=2, $
                     PSYM=1+((I+3) MOD 8), $
                     COLOR=1+(I+1)*(Navailcolor-4)/modspec.nrun-1
               ENDFOR
            ENDIF

            OPLOT,x,ypnoz(0,*),THICK=5,MAX_VALUE=maxval
            IF (kountz2 NE 0) THEN  OPLOT,x(indexz2),ypnoz(0,indexz2),PSYM=1,MAX_VALUE=maxval,THICK=2
            IF (outx1lab EQ 1) AND (N_ELEMENTS(xposi) GT 1) THEN BEGIN
               yposi=xposi*0+bly 
               XY=CONVERT_COORD(xposi,yposi,/DATA,/TO_NORMAL)
               XYOUTS,XY(0,*),yposi-0.020,xnames,ALIGNMENT=0.5,/NORMAL
            ENDIF
         ENDIF ELSE BEGIN
; Scatter plot %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            rangep=rangex
            if rangey(0) LT rangex(0) THEN rangep(0)=rangey(0)
            if rangey(1) GT rangex(1) THEN rangep(1)=rangey(1)
            rangex=rangep
            rangey=rangep
            Rescale,1
            PLOT,rangex,rangex,XSTYLE=1,YSTYLE=1, xticks=cbnticks, yticks=cbnticks, $ 
               XTITLE=xtit,YTITLE=ytit,TITLE=tit,MAX_VALUE=maxval, $
               xrange=rangex,yrange=rangex,THICK=3, $
               YGRIDSTYLE=2,Yticklen=1, XGRIDSTYLE=2,Xticklen=1, POSITION=POSBOX
            FOR I=modspec.nrun-1,0,-1 DO BEGIN
            OPLOT,x(Indexnoz),yp2noz(I,Indexnoz),LINESTYLE=I+1,MAX_VALUE=maxval,THICK=2, $
               PSYM=1+((I+3) MOD 8), $
               COLOR=1+(I+1)*(Navailcolor-4)/modspec.nrun-1
            ENDFOR
         ENDELSE
;
; this draw the E(f,time) plot
;
      ENDIF ELSE BEGIN 
         yp=f(findex-1:findex2-1)
         rangey=[min(yp),max(yp)]
         tit='time-frequency diagram at '+pname
         PLOT,rangex,rangey,YSTYLE=1,XSTYLE=1,/NODATA,/NOERASE, $
         xminor=xnminor,xticks=xnticks,xtickname=xtnames,xtickv=xntickval, $
         Yticklen=1,YTITLE=ytit,XTITLE=xtit,TITLE=tit, yminor=1, $
         Xticklen=1, $
         yrange=rangey,xrange=rangex, $
         XGRIDSTYLE=2,YGRIDSTYLE=2, $
         POSITION=POSBOX
         Contorno,yp2,x,yp,tit,xtit,ytit,cbtit,0,0,1
         fp=FLTARR(nt)
   
         FOR I=0,nt-1 DO BEGIN
            efmax=MAX(yp2(I,*),J)
            fp(I) = yp(J)
         ENDFOR

         IF (NF GT 2) THEN BEGIN 
         XFRALL=yp(1:NF-1)/yp(0:NF-2)
         IF (ABS(MIN(XFRALL)-MAX(XFRALL)) LT 0.2 ) THEN BEGIN 
         XFR=MEAN(XFRALL)
         
         XL     = 1./XFR - 1.
         XH     =  XFR - 1.
         XL2    = XL^2
         XH2    = XH^2
         NMAX=N_ELEMENTS(yp)
         FOR I=0,nt-1 DO BEGIN
            efmax=MAX(yp2(I,*),J)
    
            ILOW   = MAX ([ 0 , J-1] )
            ICEN   = MAX ([  0 , J  ] )
            IHGH   = MIN ([ NMAX-1, J+1 ])
            EL     = yp2(I,ILOW) - yp2(I,ICEN)  
            EH     = yp2(I,IHGH) - yp2(I,ICEN)  
            DENOM  = XL*EH - XH*EL
            TEMP=DENOM
            IF ABS(DENOM) LT 1E-15 THEN DENOM = 1E-15 
            IF (TEMP LT 0) THEN DENOM = -1E-15 
            IF efmax GT 0 THEN fp(I) = yp(J) * ( 1. + 0.5 * ( XL2*EH - XH2*EL )     $
                       / DENOM) ELSE yp(I)=4.*maxval
         ENDFOR
         ENDIF
         ENDIF

         OPLOT,x,fp,THICK=3,MAX_VALUE=maxval
         IF (outx1lab EQ 1) AND (N_ELEMENTS(xposi) GT 1) THEN BEGIN
           yposi=xposi*0+bly-textdy
           XY=CONVERT_COORD(xposi,yposi,/DATA,/TO_NORMAL)
           XYOUTS,XY(0,*),yposi-0.0670*!p.charsize,xnames,ALIGNMENT=0.5,/NORMAL
         ENDIF

      ENDELSE
   
; Plots with orientation "landscape" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   ENDIF ELSE BEGIN
         IF ts_processed LT 4 THEN BEGIN
            xpnoz=-x
            indexz=WHERE(ypnoz(0,*) GT MAXVAL,kount)
            IF kount GT 0 THEN xpnoz(indexz)=4.*maxval
            IF (N_ELEMENTS(xposi) GT 1) THEN BEGIN
            PLOT,ypnoz(0,*),xpnoz,/nodata,XSTYLE=1,YSTYLE=0, $
               yminor=xnminor,yticks=xnticks,ytickname=xtnames,ytickv=-xntickval, $
               YTITLE=xtit,XTITLE=ytit,TITLE=tit, xminor=1, $
               xticks=cbnticks, $
               yrange=[-rangex(1),-rangex(0)],xrange=rangey, $
               Xticklen=1,Yticklen=1,XGRIDSTYLE=2,YGRIDSTYLE=2, $
               POSITION=POSBOX
            ENDIF ELSE BEGIN
               PLOT,ypnoz(0,*),xpnoz,YSTYLE=1,XSTYLE=0, $
               YTITLE=xtit,XTITLE=ytit,TITLE=tit,MAX_VALUE=maxval, $
               yrange=[-rangex(1),-rangex(0)],xrange=rangey,THICK=4,COLOR=252,  $
               POSITION=POSBOX
            ENDELSE
            xpnoz2=-x
            indexz=WHERE(yp2 EQ 0,kount)
            IF kount GT 0 THEN xpnoz2(indexz)=4.*maxval
            IF (datatype EQ 3) THEN BEGIN
               FOR I=0,modspec.nrun-1 DO BEGIN
                  OPLOT,yp2noz(I,*),xpnoz,LINESTYLE=I+1,MAX_VALUE=maxval,THICK=2, $
                     PSYM=1+((I+3) MOD 8), COLOR=1+(I+1)*(Navailcolor-4)/modspec.nrun-1
               ENDFOR
            ENDIF
            OPLOT,ypnoz(0,*),xpnoz,THICK=5,MAX_VALUE=maxval

         ENDIF ELSE BEGIN  ; this draw the E(f,time) plot
            memo=rangex
            xp=f(findex-1:findex2-1)
         rangex=[min(xp),max(xp)]
         rangey=[-memo(1),-memo(0)]
         taille=size(yp)
         yy=FLTARR(taille(1))
         yy(*)=x(0,0,*)
;         yp2=SMOOTH(ALOG(yp+0.00001)/ALOG(10),3)
         PLOT,rangex,rangey,YSTYLE=1,XSTYLE=1,/NODATA,/NOERASE, $
         yminor=xnminor,yticks=xnticks,ytickname=xtnames,ytickv=-xntickval, $
         Yticklen=1,YTITLE=xtit,XTITLE=ytit,TITLE=tit, xminor=1, $
         Xticklen=1, $
         yrange=rangey,xrange=rangex, $
         XGRIDSTYLE=2,YGRIDSTYLE=2, $
         POSITION=POSBOX
         IF (filltype EQ 1) THEN $
              Contorno,transpose(REVERSE(yp2,1)),xp,-x,tit,xtit,ytit,cbtit,0,0,1 $
         ELSE Contorno,transpose(yp2),xp,-x,tit,xtit,ytit,cbtit,0,0,1 

         fp=FLTARR(nt)
         FOR I=0,nt-1 DO BEGIN
            efmax=MAX(yp2(I,*),J)
            IF efmax GT 0 THEN fp(I)=xp(J) ELSE xp(I)=4.*maxval
         ENDFOR
         OPLOT,fp,-x,THICK=3,MAX_VALUE=maxval

      ENDELSE
      IF (outx1lab EQ 1) AND (N_ELEMENTS(xposi) GT 1) THEN BEGIN
         yposi=xposi*0+blx-textdy*0.1
         XY=CONVERT_COORD(yposi,-xposi,/DATA,/TO_NORMAL)
         XYOUTS,yposi-0.0670*!p.charsize,XY(1,*),xnames,ALIGNMENT=0.5,/NORMAL
      ENDIF
   ENDELSE
;
;  Overlays some text on the plot
;
   IF (datatype EQ 3) AND (ts_processed LE 4 OR ts_processed EQ 7 OR ts_processed EQ 9)  THEN BEGIN
      ;comparison of model and data
      SI=FLTARR(modspec.nrun)
      SI2=FLTARR(modspec.nrun)
      BIAS=FLTARR(modspec.nrun)
      CORR=FLTARR(modspec.nrun)
      MOYC=COMPLEXARR(modspec.nrun+1)
      this_var_is_a_direction=(varname EQ 'th1tot') OR (varname EQ 'th1p') OR (varname EQ 'th1m') $
                  OR (varname EQ 'Currdir') OR (varname EQ 'Udir')
      IF (kountnoz NE 0) THEN BEGIN
         IF this_var_is_a_direction THEN BEGIN
              ypnoz=complex(cos(ypnoz*!dtor),sin(ypnoz*!dtor))
              yp2=complex(cos(yp2*!dtor),sin(yp2*!dtor))
         ENDIF
         MOYC(0)=TOTAL(ypnoz(0,indexnoz),/PRESERVE_TYPE)/kountnoz


         FOR I=0,modspec.nrun-1 DO BEGIN
            IF this_var_is_a_direction THEN $
               SI(I)=TOTAL(ATAN(IMAGINARY(yp2(I,indexnoz)/ypnoz(0,indexnoz)), $
                                REAL_PART(yp2(I,indexnoz)/ypnoz(0,indexnoz)))^2)  ELSE $
               SI(I)=TOTAL((ypnoz(0,indexnoz)-yp2(I,indexnoz))^2)

            IF rangex(1) EQ 360 THEN BEGIN
               SI(I)=0.
               FOR II=0,kountnoz-1 DO BEGIN
                mod1=ypnoz(0,indexnoz(II))
                obs1=yp2(I,indexnoz(II))
               ENDFOR
            ENDIF
            MOYC(I+1)=TOTAL(yp2(I,indexnoz),/PRESERVE_TYPE)/kountnoz
            MOY=REAL_PART(MOYC)
            IF this_var_is_a_direction THEN $
            BIAS(I)=ATAN(IMAGINARY(MOYC(I+1)/MOYC(0)),REAL_PART(MOYC(I+1)/MOYC(0))) $
            ELSE BIAS(I)=(MOY(I+1)-MOY(0))

            CORR(I)=(TOTAL((yp2(I,indexnoz)-MOYC(I+1)) $
                            *(ypnoz(0,indexnoz)-MOYC(0)))) $
                    /SQRT(TOTAL(ABS(yp2(I,indexnoz)-MOY(I+1))^2) $
                         *TOTAL(ABS(ypnoz(0,indexnoz)-MOY(0))^2))
             IF this_var_is_a_direction THEN $
               SI2(I)=TOTAL(ATAN(IMAGINARY(yp2(I,indexnoz)/ypnoz(0,indexnoz)), $
                                REAL_PART(yp2(I,indexnoz)/ypnoz(0,indexnoz))-BIAS(I))^2)  ELSE $
               SI2(I)=TOTAL((ypnoz(0,indexnoz)+BIAS(I)-yp2(I,indexnoz))^2)
         ENDFOR
         true_norm=TOTAL(ypnoz(0,indexnoz)^2)
         true_norm2=sqrt(true_norm/kountnoz)
      ENDIF ELSE BEGIN
         true_norm=1.
         FOR I=0,modspec.nrun-1 DO BEGIN
            Indexnoz2=WHERE(yp2(I,*) NE 0,kountnoz2)
            IF (kountnoz2 GT 0) THEN  $
               MOY(I+1)=TOTAL(yp2(I,Indexnoz2))/kountnoz2 ELSE MOY(I+1)=0.

         ENDFOR
      ENDELSE
      FOR I=0,modspec.nrun-1 DO BEGIN
         ;print, true_norm, I, SI(I)/true_norm
         PLOTS,textx-0.015,texty+0.007-textdy*(1+I),THICK=2,PSYM=1+((I+3) MOD 8), $
            COLOR=1+(I+1)*(Navailcolor-4)/modspec.nrun-1,/NORMAL
         IF this_var_is_a_direction THEN BEGIN
            XYOUTS,textx+textdx,texty-textdy*(1+I), $
               STRING(BIAS(I)/!dtor,FORMAT='(F6.2)'), $
               ALIGNMENT=0.0,/NORMAL
            XYOUTS,textx,texty-textdy*(1+I), $
               STRING(SQRT(SI(I)/kountnoz)/!dtor,FORMAT='(F6.3)'), $
               ALIGNMENT=0.0,/NORMAL
            XYOUTS,textx+2*textdx,texty-textdy*(1+I), $
               STRING(SQRT(SI2(I)/kountnoz)/!dtor,FORMAT='(F6.3)'), $
               ALIGNMENT=0.0,/NORMAL

         ENDIF ELSE BEGIN
            XYOUTS,textx,texty-textdy*(1+I), $
               STRING(100.*SQRT(SI(I)/true_norm),FORMAT='(F5.1)'), $
               ALIGNMENT=0.0,/NORMAL
            XYOUTS,textx+textdx,texty-textdy*(1+I), $
               STRING(SQRT(SI(I)/kountnoz),FORMAT='(F6.3)'), $
               ALIGNMENT=0.0,/NORMAL
            XYOUTS,textx+2.*textdx,texty-textdy*(1+I), $
               STRING(100.*BIAS(I)/true_norm2,FORMAT='(F6.2)'), $
               ALIGNMENT=0.0,/NORMAL
            XYOUTS,textx+3.*textdx,texty-textdy*(1+I), $
               STRING(CORR(I),FORMAT='(F7.4)'), $
               ALIGNMENT=0.0,/NORMAL
            XYOUTS,textx+4.*textdx,texty-textdy*(1+I), $
               STRING(100.*SQRT(SI2(I)/true_norm),FORMAT='(F5.1)'), $
               ALIGNMENT=0.0,/NORMAL
         ENDELSE
      ENDFOR
         IF this_var_is_a_direction THEN BEGIN
      XYOUTS,textx+textdx,texty,'Bias (deg):',ALIGNMENT=0.0,/NORMAL
      XYOUTS,textx,texty,'RMSE (deg):',ALIGNMENT=0.0,/NORMAL
      XYOUTS,textx+2*textdx,texty,'std. dev. (deg):',ALIGNMENT=0.0,/NORMAL
  ENDIF ELSE BEGIN
      XYOUTS,textx,texty,'NRMSE (%):',ALIGNMENT=0.0,/NORMAL
      XYOUTS,textx+textdx,texty,'RMSE:',ALIGNMENT=0.0,/NORMAL
      XYOUTS,textx+2*textdx,texty,'Bias (%):',ALIGNMENT=0.0,/NORMAL
      XYOUTS,textx+3*textdx,texty,'Corr.(r):',ALIGNMENT=0.0,/NORMAL
      XYOUTS,textx+4*textdx,texty,'S. I.(%):',ALIGNMENT=0.0,/NORMAL
  ENDELSE
      PRINT,'NRMSE,NB, r, SI:'
      FOR I=0,modspec.nrun-1 DO BEGIN
         PRINT,'&',100.*SQRT(SI(I)/true_norm),  '      ',FORMAT='($,A,F6.1,A)'
         ENDFOR
      PRINT,''
      FOR I=0,modspec.nrun-1 DO BEGIN
         PRINT,'&',100.*BIAS(I)/true_norm2,  '      ',FORMAT='($,A,F6.1,A)'
         ENDFOR
      PRINT,''
      FOR I=0,modspec.nrun-1 DO BEGIN
         PRINT,'&',CORR(I),'   ',FORMAT='($,A,F9.4,A)'
         ENDFOR
      PRINT,''
      FOR I=0,modspec.nrun-1 DO BEGIN
         PRINT,'&',100.*SQRT(SI2(I)/true_norm),  '      ',FORMAT='($,A,F6.1,A)'
         ENDFOR
      PRINT,''
      PRINT,'MEAN [',STRING(Moy(*),FORMAT='(7F9.4)'),'] ...'
      PRINT,'RMSE [',STRING(SQRT(SI(*)/kountnoz),FORMAT='(7F7.3)'),'] ...'
      print, 'NP:',kountnoz
      print, 'Norm:',true_norm
   ENDIF ELSE BEGIN
       ;FOR I=0,taille(1)-1 DO PRINT,x(I),yp(*,I)
   ENDELSE
   
   IF (ts_processed GE 2) AND (ts_processed LT 4) THEN BEGIN
      zs=' UTC'
      assos=STRING(time[tindex-1].m,FORMAT='(I2)')+'/'+ $
                  STRING(time[tindex-1].d,FORMAT='(I2)')+'/'+ $
                  STRING(time[tindex-1].y,FORMAT='(I4)')+' '+ $
                  STRING(time[tindex-1].h,FORMAT='(I2)')+':'+ $
                  STRING(time[tindex-1].minu,FORMAT='(I2)')
      z=time[tindex-1].zone
      IF (z EQ -5) THEN zs=' EST' ELSE $
      IF (z NE 0) THEN BEGIN
         IF (z < 0) THEN zs=zs+'-'+STRING(ABS(z),FORMAT='(f3.1)') $
            ELSE zs=zs+'+'+STRING(ABS(z),FORMAT='(f3.1)')
      ENDIF
      XYOUTS,rangex(0)+0.05*(rangex(1)-rangex(0)), $
               rangey(1)-0.1*(rangey(1)-rangey(0)),assos+zs
   ENDIF
   ENDIF
   ENDELSE
   RETURN
END

;----------------------------------------------------------------------------
PRO doplotothers
;
;     Performs all plots except time series
;
;** 1 ** Display parameters
COMMON AXISFRAME,axis_orient,frametype,outx1lab,outx2lab,outy1lab,outy2lab
COMMON CONTOURPARAM,numlevels,c_repart,lev,maxval,fixrange
COMMON COLORBARPAR,cbnticks,cbexrange,cbtrx,cbtry,cbblx,cbbly, $
   filloutofrange,addmini,addmaxi
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON TITLES,  font,outtit,outxtit,outytit,outcbtit,textx,texty,textdx,textdy
COMMON POSTSCRIPT, filep,pspath,prcoul,psor,pstype, $
                pwinx,pwiny,papierx,papiery,xoffset,yoffset, $
                facpolice,fontrescale,basefontsize,pssizex,pssizey,psfont
COMMON THREED,  Ax3D,Az3D,smoothing
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth

;** 2 ** Display and data variables/parameters
COMMON FREQ,    freq,nfband,findex,findex2
COMMON DIR,     nabin,nabint,aindex,THETA
COMMON LANGUAGE, ilang,dictionnaire
COMMON MAP,     MAPFLAG, MAPPROJ, MAPLONGCENTER, MAPLATCENTER, MAPCONTINENT,  $
                MAPCOUNTRIES, MAPLONLAT
COMMON TIME,    timestep,tindex,tindex2,ntime,dtime,time0,day0,dtindex,nstep, $
                timezone_plot,timezone_string,months,time
COMMON TRANSECT,Ntrans,Strans,Xtrans,Ytrans,Ztrans,Itrans, $
                  COtrans,TransOK,transsym,transline,transthick,Ispectrans, $
                  spectransname,ntransgp,transsymsize

COMMON SPACE,   c_gp,c_cut,indexgp,c_x,c_y,c_lon,c_lat
COMMON OVERLAY, addir,adsyms,adbathy,adcoast,psyms,psymsizes,adtr,adtri

;** 3 ** I/O and data variables
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON COAST,   coastxy,coastl,coastnp,GSHHSPoly,GSHHSPoint,GSHHSPoly2,GSHHSPoint2
COMMON DEPTHSTAT,nbind,bindres,HISTD,MEAND,SDEVD
COMMON DIRSPEC, ds,dsnfband,ndir,kdkdth,dsfreq,dstheta,dsdf,dstime,dsntime,ds_timezone
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON GRIDPar, th1,th2,gfact,gsize1,gsize6
COMMON RAY3ad,  NGPad,SIad,SCad,DEPTHad,HSad,PSIad,BBLDad
COMMON RAYS,    raysOK,raystype,raynsteps,rayx,rayy,raya,rayamin,rayamax, $
                rayres,rayfreq,rayGP,raytimestep,rayoffdep,rayflag,raydz,raymindepth
COMMON SOURCE,  S0,snabin,sntstep
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
COMMON TIMESERIES, modspec,obs,obs2,om,ts_filetype, nbins, bulktype,scatvar
COMMON TRANSECT,Ntrans,Strans,Xtrans,Ytrans,Ztrans,Itrans, $
                  COtrans,TransOK,transsym,transline,transthick,Ispectrans, $
                  spectransname,ntransgp,transsymsize
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON LOCAL2,ymemo,ymemo2
COMMON WW3,     ww3model,ww3date,ww3time,ww3fieldname,ww3scale,ww3miss,ww3unit,ww3lon, $
               ww3lat,ww3matrix,ww3dir,ww3path,spec2D,ww3matrix2,ww3dir2,ncid,ncid2,ncivar0,ncivar
;*******END OF COMMON BLOCKS*******************************

   CASE plotncvar OF
   0 :IF c_cut EQ 1 AND transOK THEN BEGIN
   ; Plots the depth along a transect defined in a *.tr file
         X=Strans
         Y=-Ztrans
         rangex=[min(x,imin) -10.,max(x)]
         rangey=[max([min(y),-1*maxdepth]),10.] ;max(y)]
         Rescale,0
         IF (MAX(Xtrans)-min(Xtrans)) GE ((MAX(Ytrans)-min(Ytrans))) THEN BEGIN
            IF Xtrans(Ntrans-1) LT Xtrans(0) THEN BEGIN
               PRINT,'REVERSE X AXIS !!'
               X=X(Ntrans-1)-X
               imin=Ntrans-1-imin
               ;Y=Reverse(Y)
            ENDIF
         ENDIF ELSE BEGIN
            IF Ytrans(Ntrans-1) LT Ytrans(0) THEN BEGIN
               PRINT,'REVERSE Y AXIS !!'
               X=X(Ntrans-1)-X
               ;Y=Reverse(Y)
            ENDIF
         ENDELSE
         PLOT,X,Y,XSTYLE=1,YSTYLE=1,MAX_VALUE=maxval,/YNOZERO, $
            YTITLE='Depth (m)',XRANGE=rangex, $
            XTITLE='Distance along transect (km)',YRANGE=rangey, $
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         XX=FLTARR(Ntrans+3)
         YY=FLTARR(Ntrans+3)
         XX(0:Ntrans-1)=X
         YY(0:Ntrans-1)=Y
         XX(Ntrans)=rangex(1)
         XX(Ntrans+1:Ntrans+2)=rangex(0)
         YY(Ntrans:Ntrans+1)=rangey(0)
         YY(Ntrans+2)=Y(0)
         POLYFILL,XX,YY,/LINE_FILL,ORIENTATION=120,NOCLIP=0, $
               CLIP=[rangex(0),rangey(0),rangex(1),rangey(1)]

         ;XYOUTS,X(Ispectrans(0)),alignment=0.5, $
         ;   rangey(1)-0.2*(rangey(1)-rangey(0)),spectransname(0)
         ;XYOUTS,X(Ispectrans(ntransgp-1)),alignment=0.5, $
         ;   rangey(1)-0.2*(rangey(1)-rangey(0)),spectransname(ntransgp-1)
         ;FOR I=1,ntransgp-1 DO $
         ;XYOUTS,X(Ispectrans(I)),rangey(1)-0.1*(rangey(1)-rangey(0)),spectransname(I)
         rangex0=rangex
         IF (MAX(Xtrans)-min(Xtrans)) GE ((MAX(Ytrans)-min(Ytrans))) THEN BEGIN
            rangex=rangex+Xtrans(imin)
         ;   Drawframe,'Distance along transect (km)', $
         ;      'Longitude (W)','Bottom elevation (m)',' ',FRAME=7,
         ENDIF ELSE BEGIN
            rangex=rangex+Ytrans(imin)
            Drawframe,'Distance along transect (km)', $
               'Latitude (N)','Bottom elevation (m)',' ',FRAME=8
         ENDELSE
         posi=CONVERT_COORD(X(Ispectrans),REPLICATE(rangey(1),ntransgp),/DATA,/TO_NORMAL)
         Y(Ispectrans(1))=0.
         FOR I=0,ntransgp-1 DO BEGIN
            OPLOT,[X(Ispectrans(I))],[Y(Ispectrans(I))],PSYM=6
            XYOUTS,X(Ispectrans(I))+0.04*(1 $;+2*(I MOD 2)
            )*(rangex(1)-rangex(0)),ALIGNMENT=0.5, $
            Y(Ispectrans(I))-0.02*(1-2*(I MOD 2))*(rangey(1)-rangey(0)),spectransname(I)
         ;PLOT,[posi(0,I),posi(0,I)],[0,1],LINESTYLE=3,/NORMAL, $
         ;      XSTYLE=4,YSTYLE=4,/NOERASE,XRANGE=[0,1], $
         ;      POSITION=[0,bly*winy/mwiny,1,try*winy/mwiny]
         ENDFOR
         xp=rangex0(0)+FINDGEN(850)*(rangex0(1)-rangex0(0))/849.
         wavel=33
         waveh=4.
         taille=SIZE(xp)
         ywave=FLTARR(taille(1))
         meanw=1*!pi/SQRT(3)-0.5
         FOR I=0,taille(1)-1 DO BEGIN
            I2=I-(wavel-1)*(I/(wavel-1))
            ywave(I)=meanw-2.*SQRT(1.-(FLOAT(I2-wavel/2)/FLOAT(wavel/2))^2)
         ENDFOR
         OPLOT,xp,ywave

      ENDIF ELSE BEGIN
         x=(findgen(nxzmax-nxzmin+1)+nxzmin)*dx
         y=(findgen(nyzmax-nyzmin+1)+nyzmin)*dy
         rangex=[min(x),max(x)]
         rangey=[min(y),max(y)]
         Rescale,1
         IF ((filltype LE 2) OR (filltype EQ 7)) $
            AND ((RaysOK MOD 2) EQ 1) THEN BEGIN
            stfr=string(rayfreq,format='(f6.4)')
            tit='Rays of '+stfr+' Hz waves'
            cbtit='Depth (m)'
         ENDIF ELSE BEGIN
            tit='Bottom topography'
            cbtit=dictionnaire.dpt(ilang)+' (m)' ; au dessus du zero des cartes)'
         ENDELSE
       IF datastatus(3) THEN BEGIN
;AAA=stddev(gd(nxzmin:nxzmax,nyzmin:nyzmax))
;  print,'STDDEV:',AAA, AAA/mean(gd(nxzmin:nxzmax,nyzmin:nyzmax))
            table=gd(nxzmin:nxzmax,nyzmin:nyzmax)
            Contorno,table,x,y,tit,'x (km)','y (km)',cbtit,0,1,0

;slope=0.
;shore=0.1
;ns=0
;FOR J=1,nyzmax-nyzmin-2 DO BEGIN 
;   ;LAND=WHERE(table(*,J) < shore,kount)
;   LAND=WHERE(table(*,J) > shore,kount)
;   IF (kount GT 0) THEN BEGIN 
;     shift=0
;     I0=LAND(0)
;     ;I0=LAND(kount-1)
;     shift=2
;     slope=slope+0.001*sqrt(((table(I0-2+shift,J)-table(I0-1+shift,J))/dx)^2+((table(I0,J+1)-table(I0,J))/dy)^2)
;     ns=ns+1
;   ENDIF
;ENDFOR
;meanslope=slope/ns
;print,'Mean slope:',meanslope

         ENDIF ELSE PLOT,[x(0)],[y(0)],xstyle=5,ystyle=5,/NODATA, $
            XRANGE=rangex,YRANGE=rangey, /NOERASE,$
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]

         IF (filltype LE 4)  THEN BEGIN
         ;!P.CLIP=[min(x),min(y),0,max(x),max(y),1]
            ad2dmisc
            Drawframe,Drawframe,FRAME=1+MAPLONLAT
         ENDIF

      ENDELSE

;
; Plots bathymetry from gmsh file
;
   1 :BEGIN
         x=(findgen(nxzmax-nxzmin+1)+nxzmin)*dx
         y=(findgen(nyzmax-nyzmin+1)+nyzmin)*dy
         rangex=[min(x),max(x)]
         rangey=[min(y),max(y)]
         Rescale,1
         IF ((filltype LE 2) OR (filltype EQ 7)) $
            AND ((RaysOK MOD 2) EQ 1) THEN BEGIN
            stfr=string(rayfreq,format='(f6.4)')
            tit='Rays of '+stfr+' Hz waves'
            cbtit='Depth (m)'
         ENDIF ELSE BEGIN
            tit='Bottom topography'
            cbtit='Depth (m)'
             ;cbtit=dictionnaire.dpt(ilang)+' (m)' ; au dessus du zero des cartes)'
             cbtit='Depth (m)' ; au dessus du zero des cartes)'
         ENDELSE
         IF (N_ELEMENTS(gridmat) GT 0) THEN BEGIN          
            Z=gridmat(7,*)
            X=gridmat(0,*)
            Y=gridmat(1,*)
            IF (MAX(Z) GT MIN(Z)) THEN BEGIN 
               tri=TRANSPOSE(TRIGP(1:ntri,1:3)-1)  
               IF (filltype EQ 0 OR FILLTYPE EQ 3) THEN BEGIN 
                   TRIANGULATE,X,Y,tri
                   ;Contorno,Z,X,Y,tit,'x (km)','y (km)',cbtit,1,0,1,tri=tri 
                   Contorno,Z,X,Y,tit,'x (km)','y (km)',cbtit,1,1,1,tri=tri 
                ENDIF ELSE BEGIN 
                  nnx=nxzmax-nxzmin+1
                  nny=nyzmax-nyzmin+1
                  IF (LONG(NNX)*LONG(NNY) LT 80000000L) THEN BEGIN 
                     arr=trigrid(X,Y,z,tri,[dx,dy],[rangex(0),rangey(0),rangex(1),rangey(1)])
                     x=(findgen(nnx)+nxzmin)*dx
                     y=(findgen(nny)+nyzmin)*dy
                     Contorno,arr,x,y,tit,'x (km)','y (km)',cbtit,0,1,0
                  ENDIF ELSE BEGIN 
                     PRINT,'Plot not performed because interpolation on too large array:',nnx,nny
                     PRINT,'Please change resolution in *.log file'
                  ENDELSE
               ENDELSE
               IF (filltype LE 4)  THEN BEGIN
                  ad2dmisc
                  IF (MAPFLAG EQ 0) THEN   Drawframe,FRAME=1+MAPLONLAT
               ENDIF
            ENDIF ELSE PRINT,'This array is constant'
         ENDIF  ; N_ELEMENTS(gridmat) 
      END
;
;  Plots WWATCH gridded output 
;
   2 :BEGIN
;
; Shifts indices for global grids to move longitudes
;     
     nnx=nxzmax-nxzmin+1
     nny=nyzmax-nyzmin+1
     taille=size(ww3matrix)
     nlat=taille(0)
     IF (nlat GT 1) THEN BEGIN 
        nshift=0;
        ww3matrixshift=SHIFT(ww3matrix,nshift,0);
        nxzmaxshift=nxzmax+nshift*dx
        nxzminshift=nxzmin+nshift*dx
        nnx=nxzmaxshift-nxzminshift+1
     ENDIF 
     x=(findgen(nnx)+nxzmin)*dx
     y=(findgen(nny)+nyzmin)*dy
     rangex=[min(x),max(x)]
     rangey=[min(y),max(y)]
     Rescale,1

     tit=ww3model+' '+ww3date+' '+ww3time
     cbtit=ww3fieldname;"Vitesse d'agitation sur le fond"
;
;
;
    IF (nlat GT 1 ) THEN BEGIN ; This is for structured grids
      IF (STRPOS(cbtit,'W/m2') NE -1) THEN BEGIN 
        flux=0.
        
        I=WHERE(ww3matrix EQ -2.*maxval,KOUNT) 
        IF (KOUNT GT 0) THEN ww3matrix(I)=0.
        FOR J=nyzmin,nyzmax DO BEGIN 
          flux=flux+TOTAL(ww3matrix(nxzmin:nxzmax,J))*cos(ww3lat(J)*!dtor)
        ENDFOR
        flux=flux*(ww3lat(2)-ww3lat(1))*(ww3lon(2)-ww3lon(1))*(!dtor*4E7/(2*!pi))^2
        print,'FLUX =',flux,' W'
      
      ENDIF
        IF datastatus(20) THEN BEGIN
           table=ww3matrixshift(nxzmin:nxzmax,nyzmin:nyzmax)
          IF ((STRPOS(cbtit,'Hs') NE -1 OR STRPOS(cbtit,'hs') NE -1  )AND fixrange EQ 0) THEN BEGIN
               clev=[0, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, $
                 2.25 ,2.5, 2.75, 3, 3.25, 3.5, 3.75, 4. ,4.5 ,5., 5.5, 6 ,7, $
                     8 ,  9,   10,  11,  12,  13 , 14, 15,  40]
               IF (fixrange) THEN clev=clev*maxdepth/15.
               coli=[1,  3,    5,    8,   9,   15, 20, 21,   26,  30, $
                     35,  39,  40,   45,  52, 57,   61,  66, 67,   79, 89, 98,  99 , 134,  $
                     149, 150, 182, 183, 215, 216, 251, 252]
               Contorno,table,x,y,tit,'x (km)','y (km)',cbtit,0,0,0,clev=clev,coli=coli
            ENDIF ELSE BEGIN
               table(0,0)=-1.
               Contorno,table,x,y,tit,'x (km)','y (km)',cbtit,0,0,0
            ENDELSE
         ENDIF ELSE PLOT,[x(0)],[y(0)],xstyle=5,ystyle=5,/NODATA, $
            XRANGE=rangex,YRANGE=rangey, /NOERASE,$
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]
         IF (addir EQ 0) and (n_elements(ww3dir) NE 0) THEN BEGIN
            dirplot=ww3dir
; In case of polar projection: rotation ... 
            IF (MAPLONLAT EQ 2) THEN dirplot=dirplot-(ww3lon+45);

            xvect=-sin(dirplot(nxzmin:nxzmax,nyzmin:nyzmax)*!dtor)
            yvect=-cos(dirplot(nxzmin:nxzmax,nyzmin:nyzmax)*!dtor)
            xx=transpose(1.+FLTARR(nny))##x
            yy=transpose(y)##(1.+FLTARR(nnx))
            missing=FLTARR(nnx,nny)
            Index=WHERE(abs(ww3dir(nxzmin:nxzmax,nyzmin:nyzmax)) GT maxval,kount)
            IF kount GT 0 THEN missing(Index)=1
            IF (smoothing GT 1) THEN BEGIN
               FOR I=0,nnx-1 DO BEGIN
                  FOR J=0,nny-1 DO BEGIN
                     IF (I MOD smoothing) +(J MOD smoothing) NE 0 $
                        THEN missing(i,j)=1.
                  ENDFOR
               ENDFOR
            ENDIF
            VECT,reform(xvect,LONG(nnx)*LONG(nny)),reform(yvect,LONG(nnx)*LONG(nny)), $
               reform(xx,LONG(nnx)*LONG(nny)),reform(yy,LONG(nnx)*LONG(nny)), $
               reform(missing,LONG(nnx)*LONG(nny)),rangex,rangey
         ENDIF
      ENDIF ELSE BEGIN  ; this is for unstructured grids
           XGP=FLTARR(nngp)
           YGP=FLTARR(nngp)
           array2=ww3matrix
           array2=array2;   ^2*0.001*0.0015
           XGP(0:nngp-1)=gridmat(0,0:nngp-1)
           YGP(0:nngp-1)=gridmat(1,0:nngp-1)
           IF (STRPOS(cbtit,'Hs') NE -1 AND STRPOS(cbtit,'HsIG') EQ -1 AND fixrange EQ 0) THEN BEGIN
               clev=[0, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75,  $
                 2, 2.25 ,2.5, 2.75, 3, 3.25, 3.5, 3.75, 4. ,4.5 ,5., 5.5, 6 ,7, $
                     8 ,  9,   10,  11,  12,  13 , 14, 15,  40]
               IF (fixrange) THEN clev=clev*maxdepth/15.
               coli=[1,  3,    5,    8,   9,   15, 20, 21,   26,  $ 
                     30,  35,  39,  40,   45,  52, 57,   61,  66, 67,   79, 89, 98,  99 , 134,  $
                     149, 150, 182, 183, 215, 216, 251, 252]
               tri=TRANSPOSE(TRIGP(1:ntri,1:3)-1)
              IF (filltype EQ 0 OR FILLTYPE EQ 3) THEN $
                  Contorno,array2,XGP,YGP,tit,'x (km)','y (km)',cbtit,1,0,1,clev=clev,coli=coli,tri=tri $
            ELSE BEGIN 
            ENDELSE
       ENDIF ELSE BEGIN
          tri=TRANSPOSE(TRIGP(1:ntri,1:3))-1L
                ;X=XGP
                ;Y=YGP
                ;Z=array2
                ;SAVE,X,Y,Z,TRI,rangex,rangeY,dx,dy,filename='TRIANGULATION_MARSEILLE.sav', $
                ;     DESCRIPTION="Example of grid that fails in CONTOUR with TRIANGULATION keyword"
                IF (filltype EQ 0 OR FILLTYPE EQ 3) THEN BEGIN 
                   TRIANGULATE,XGP,YGP,tri
                   Contorno,array2,XGP,YGP,tit,'x (km)','y (km)',cbtit,1,0,1,tri=tri 
                ENDIF ELSE BEGIN 
                  arr=trigrid(XGP,YGP,array2,tri,[dx,dy],[rangex(0),rangey(0),rangex(1),rangey(1)], $
                              MAX_VALUE=maxval)
                  x=(findgen(nnx)+nxzmin)*dx
                  y=(findgen(nny)+nyzmin)*dy
                  Contorno,arr,x,y,tit,'x (km)','y (km)',cbtit,0,0,0
               ENDELSE
            ENDELSE
 

        IF (addir EQ 0 AND MAPFLAG EQ 0) and (n_elements(ww3dir) NE 0) THEN BEGIN
            TRIANGULATE,xgp,ygp,tr,b
        Index=WHERE(ww3matrix GT maxval,kount)
        IF (kount GT 1) THEN ww3dir(Index)=1./0
            dircos=TRIGRID(xgp,ygp,cos(ww3dir*!dtor),tr, $
               [dx,dy],[MIN(X), MIN(Y), MAX(X), MAX(Y)],XOUT=X,YOUT=Y)
            dirsin=TRIGRID(xgp,ygp,sin(ww3dir*!dtor),tr, $
                [dx,dy],[MIN(X), MIN(Y), MAX(X), MAX(Y)],XOUT=X,YOUT=Y)
            xvect=-dirsin
            yvect=-dircos
       
            xx=transpose(1.+FLTARR(nny))##x
            yy=transpose(y)##(1.+FLTARR(nnx))
            missing=FLTARR(nnx,nny)
            IF (smoothing GT 1) THEN BEGIN
               FOR I=0,nnx-1 DO BEGIN
                  FOR J=0,nny-1 DO BEGIN
                     IF (I MOD smoothing) +(J MOD smoothing) NE 0 $
                        THEN missing(i,j)=1.
                  ENDFOR
               ENDFOR
            ENDIF
            VECT,reform(xvect,LONG(nnx)*LONG(nny)),reform(yvect,LONG(nnx)*LONG(nny)), $
               reform(xx,LONG(nnx)*LONG(nny)),reform(yy,LONG(nnx)*LONG(nny)), $
               reform(missing,LONG(nnx)*LONG(nny)),rangex,rangey
           ENDIF



         ENDELSE
 
         IF (filltype LE 4)  THEN BEGIN
         ;!P.CLIP=[min(x),min(y),0,max(x),max(y),1]
            ad2dmisc
         IF (MAPFLAG EQ 0) THEN  Drawframe,FRAME=2-ABS(MAPLONLAT-1)
         ENDIF

      END
   3:BEGIN
         nnx=nxzmax-nxzmin+1
         nny=nyzmax-nyzmin+1
         x=(findgen(nnx)+nxzmin)*dx
         y=(findgen(nny)+nyzmin)*dy
         rangex=[min(x),max(x)]
         rangey=[min(y),max(y)]
         Rescale,1
         tit=ww3model+' '+ww3date+' '+ww3time
         cbtit='diff. '+ww3fieldname
         taille=size(ww3matrix)
         nlat=taille(2)
         IF (nlat GT 1) THEN BEGIN
         IF datastatus(21) THEN BEGIN
            table=ww3matrix(nxzmin:nxzmax,nyzmin:nyzmax)-ww3matrix2(nxzmin:nxzmax,nyzmin:nyzmax)
            Contorno,table,x,y,tit,'x (km)','y (km)',cbtit,0,0,0
         ENDIF ELSE PLOT,[x(0)],[y(0)],xstyle=5,ystyle=5,/NODATA, $
            XRANGE=rangex,YRANGE=rangey, /NOERASE,$
            POSITION=[blx*winx/mwinx,bly*winy/mwiny,trx*winx/mwinx,try*winy/mwiny]

         IF (addir EQ 0) and (n_elements(ww3dir) NE 0) THEN BEGIN
            xvect=-sin(ww3dir(nxzmin:nxzmax,nyzmin:nyzmax)*!dtor)
            yvect=-cos(ww3dir(nxzmin:nxzmax,nyzmin:nyzmax)*!dtor)
            xx=transpose(1.+FLTARR(nny))##x
            yy=transpose(y)##(1.+FLTARR(nnx))
            missing=FLTARR(nnx,nny)
            Index=WHERE(ww3dir(nxzmin:nxzmax,nyzmin:nyzmax) GT maxval,kount)
            IF kount GT 0 THEN missing(Index)=1
            IF (smoothing GT 1) THEN BEGIN
               FOR I=0,nnx-1 DO BEGIN
                  FOR J=0,nny-1 DO BEGIN
                     IF (I MOD smoothing) +(J MOD smoothing) NE 0 $
                        THEN missing(i,j)=1.
                  ENDFOR
               ENDFOR
            ENDIF
            VECT,reform(xvect,LONG(nnx)*LONG(nny)),reform(yvect,LONG(nnx)*LONG(nny)), $
               reform(xx,LONG(nnx)*LONG(nny)),reform(yy,LONG(nnx)*LONG(nny)), $
               reform(missing,LONG(nnx)*LONG(nny)),rangex,rangey
         ENDIF
         ENDIF ELSE BEGIN
           XGP=FLTARR(nngp)
           YGP=FLTARR(nngp)
           array2=ww3matrix-ww3matrix2
           XGP(0:nngp-1)=gridmat(0,0:nngp-1)
           YGP(0:nngp-1)=gridmat(1,0:nngp-1)
           Contorno,array2,XGP,YGP,tit,'x (km)','y (km)',cbtit,1,0,1
         ENDELSE

         IF (filltype LE 4)  THEN BEGIN
         ;!P.CLIP=[min(x),min(y),0,max(x),max(y),1]
            ad2dmisc
          IF (MAPFLAG EQ 0) THEN   Drawframe,FRAME=1+MAPLONLAT
         ENDIF

      END
   ENDCASE
   RETURN
END


;----------------------------------------------------------------------------
pro gfunct,X,A,f,pder
   bx=EXP(A[3]*X)
   p1=A[0]*X^2+A[1]*X+A[2]
   F=p1*bx+A[4]
   IF N_PARAMS() GE 4 THEN $
   pder=[[X^2*bx],[X*bx],[bx],[x*p1*bx],[replicate(1.0, N_ELEMENTS(X))]]
   RETURN
END
;----------------------------------------------------------------------------
PRO LINEPLOT,X,Y,xtit,ytit,tit,SYMOP,LOGFLAG

    rangex=[min(x),max(x)]
    rangey=[min(y),max(y)]
    Rescale,0
    SYMARR=[-1,4,5,6,8,8,8,-7,2]
    XUSYM=FLTARR(9,5)
    YUSYM=FLTARR(9,5)
    XUSYM(4,*)=[-1,1,0.,-1,1]
    YUSYM(4,*)=[1,1,-1,1,1]
    XUSYM(5,*)=[-1,1,-1,1,-1]
    YUSYM(5,*)=[1,1,-1,-1,1]
    XUSYM(6,*)=[-1,1,1,-1,-1]
    YUSYM(6,*)=[1,-1,1,-1,1]
    XUSYM(8,*)=[-1,1,1,-1,-1]
    YUSYM(8,*)=[1,-1,1,-1,1]
    PLOT,X,Y(*,0),YRANGE=rangey,XRANGE=rangex, $
         title=title,YLOG=LOGFLAG,XTITLE=xtit,YTITLE=ytit,/NODATA
    taille=size(y)
    print,taille
    IF SYMOP THEN BEGIN
      FOR I=0,taille(2)-1 DO BEGIN
         USERSYM,XUSYM(I,*),YUSYM(I,*)
         OPLOT,X,Y(*,I),PSYM=SYMARR(I),SYMSIZE=0.6
      ENDFOR
    ENDIF ELSE BEGIN
      SARR=['A','B','C','D','E','F','G','H','I']
      SYMARR=[-3,3,3,3,3,3,3,-3,3]
      FOR I=0,taille(2)-1 DO BEGIN
         OPLOT,X,Y(*,I),PSYM=SYMARR(I)
         XYOUTS,X,Y(*,I),REPLICATE(SARR(I),taille(1)),ALIGNMENT=0.5
      ENDFOR
   ENDELSE
END
;----------------------------------------------------------------------------
PRO UPDATE_COORDINATES,lonmin,lonmax,latmin,latmax,widgetaction
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth

      inbox=1
      IF (datastatus(2) EQ 1) THEN BEGIN
        IF (lonmin LT rlonmin) THEN inbox = 0 
        IF (latmin LT rlatmin) THEN inbox = 0 
        IF (lonmax GT rlonmax) THEN inbox = 0 
        IF (latmax GT rlatmax) THEN inbox = 0 
      ENDIF
      IF (rlonmin EQ 0 and rlatmax EQ 0) THEN inbox = 0
      IF (datastatus(2) EQ 0 OR inbox EQ 0) THEN BEGIN 
         nx=4001
         ny=4001
         rlonmin=lonmin
         rlonmax=lonmax
         rlatmin=latmin
         rlatmax=latmax
         dlon=rlonmax-rlonmin
         dlat=rlatmax-rlatmin
         LX=dlon*cos(0.5*(rlatmax+rlatmin)*!pi/180.)*4E7/360.
         LY=dlat*4E7/360.

         sx=LX/(nx-1)
         sy=LY/(ny-1)
         dx=sx/1000.
         dy=sy/1000.
         PRINT,'Defining coordinate system :',nx,ny,dx,dy
         PRINT,'Defining coordinate system :',rlonmin,rlonmax,rlatmin,rlatmax
         nxzmin=0L
         nyzmin=0L
         nxzmax=nx-1
         nyzmax=ny-1
         rangex=[0,dx*FLOAT(nx-1)]
         rangey=[0,dy*FLOAT(ny-1)]
         datastatus(2)=1
         MAPLONLAT=1
         IF widgetaction THEN WIDGET_CONTROL, Wdraw, SENSITIVE=1
      ENDIF
END

;-------------- determine les dimensions du graphique -------------------
PRO Rescale,flag            ;appele par Doplot
;** 1 ** Display parameters
COMMON CURRENT, datatype,plottype,line,column,c_numlev,output,plotncvar,normvec
COMMON DRAWING, Navailcolor,colorind,rangex,rangey,xtoy,filltype,logplot
COMMON DRAWSIZE,winx,winy,mwinx,mwiny,blx,bly,trx,try
COMMON FLAGS,   eqscale,cbar,clickflag,subwin
COMMON POSTSCRIPT, filep,pspath,prcoul,psor,pstype, $
                pwinx,pwiny,papierx,papiery,xoffset,yoffset, $
                facpolice,fontrescale,basefontsize,pssizex,pssizey,psfont
COMMON MAP,     MAPFLAG, MAPPROJ, MAPLONGCENTER, MAPLATCENTER, MAPCONTINENT,  $
                MAPCOUNTRIES, MAPLONLAT
;*******END OF COMMON BLOCKS*******************************


   IF output EQ 1 THEN BEGIN
!p.thick = 2
!x.thick = 2
!y.thick = 2
!z.thick = 2
      winx=mwinx
      winy=mwiny
; Conditions sur la taille de la fenetre:
; pssizex < or = pwinx  with pssizex=fx*mwinx
; pssizey < or = pwiny  with pssizey=fx*ff*mwiny
; The ratio ff between the two factors is determined by:
; (in the case EQSCALE=1):
;  pssizex*(trx-blx)/(pssizey*(try-bly))
;      = (rangex(1)-rangex(0))/(rangey(1)-rangey(0))
;

      IF psor EQ 0 THEN BEGIN    ;portrait
         pssizex=pwinx
         pssizey=pwiny
         IF flag EQ 1 AND MAPFLAG EQ 0 THEN BEGIN ;portrait and equal scaling
         f=MIN([FLOAT(pssizex)*(trx-blx)/(rangex(1)-rangex(0)), $
                FLOAT(pssizey)*(try-bly)/(rangey(1)-rangey(0))])
         ;print,'factors',f,FLOAT(pssizex)*(trx-blx)/(rangex(1)-rangex(0)), $
         ;  FLOAT(pssizey)*(try-bly)/(rangey(1)-rangey(0))
         ;f is the min of the maximum ratio of
         ;poststcript distance and data distance for both axes
         ;then we apply the same f ratio to both axes:
         winx=f*FLOAT(mwinx)*(rangex(1)-rangex(0))/ $
            ((trx-blx)*pssizex) ; horizontal size of plot output
         winy=f*FLOAT(mwiny)*(rangey(1)-rangey(0))/ $
            ((try-bly)*pssizey) ; vertical size of plot output
         ;print,'winx',winx,mwinx,winy,mwiny
         ;print,'plots',pssizex*FLOAT(winx)*(trx-blx)/(rangex(1)-rangex(0))/FLOAT(mwinx), $
         ;pssizey*FLOAT(winy)*(try-bly)/(rangey(1)-rangey(0))/FLOAT(mwiny)
                  POSI=[blx*FLOAT(winx)/FLOAT(mwinx),bly*FLOAT(winy)/FLOAT(mwiny), $
                        trx*FLOAT(winx)/FLOAT(mwinx),try*FLOAT(winy)/FLOAT(mwiny)]
         ;print,POSI
         ;print,POSI*pssizex
         ;print,POSI*pssizey
         ;print,'ranges',rangex,rangey
         ENDIF ELSE BEGIN        ;portrait without equal scaling
            winx=mwinx
            winy=mwiny
         ENDELSE
      ENDIF ELSE BEGIN
         pssizex=pwiny
         pssizey=pwinx
         IF flag EQ 1 THEN BEGIN ; landscape and equal scaling
         f=MIN([FLOAT(pssizex)*(trx-blx)/(rangex(1)-rangex(0)), $
                FLOAT(pssizey)*(try-bly)/(rangey(1)-rangey(0))])
         IF pstype THEN BEGIN    ; case of encapsulated postscript
            pssizex=f*(rangex(1)-rangex(0))/(trx-blx)
            pssizey=f*(rangey(1)-rangey(0))/(try-bly)
         ENDIF
         ;f is the min of the maximum ratio of
         ;poststcript distance and data distance for both axes
         ;then we apply the same f ratio to both axes:
         winx=f*mwinx*(rangex(1)-rangex(0))/ $
            ((trx-blx)*pssizex) ; horizontal size of plot output
         winy=f*mwiny*(rangey(1)-rangey(0))/ $
            ((try-bly)*pssizey) ; vertical size of plot output
         ENDIF ELSE BEGIN        ;landscape without equal scaling
            winx=mwinx
            winy=mwiny
         ENDELSE
      ENDELSE

     IF output EQ 1 THEN BEGIN
         IF psfont EQ 0 THEN DEVICE,/HELVETICA ELSE DEVICE,/TIMES
         PRINT,'PSfont:',psfont
      ENDIF
      IF !P.FONT EQ 1 THEN !P.FONT=0
      !p.charsize=1.5*MIN([pssizex,pssizey])*facpolice/20.0

      ; adapte la taille de la police a la
      ; dimension du graphique
      PRINT,'Dimensions de la zone imprimee:',pssizex,pssizey
      IF fontrescale THEN BEGIN
         !p.charsize=1.5*MIN([pssizex,pssizey])*facpolice/20.0
         PRINT,'Taille reelle de la police (Max(' $
            +strcompress(string(pssizex))+',' $
            +strcompress(string(pssizey))+')*' $
            +strcompress(string(facpolice))+'/20.0):',!p.charsize*basefontsize
      ENDIF ELSE BEGIN
         !p.charsize=facpolice
         PRINT,'Taille reelle de la police :',!p.charsize*basefontsize
      ENDELSE
      DEVICE,/ISOLATIN1
      IF psor EQ 1 THEN BEGIN   ;ouverture fichier postcript couleur
            print,'pssizex',pssizex,pssizey
            DEVICE,/COLOR, XSIZE=pssizex, YSIZE=pssizey, BITS_PER_PIXEL=8, $
            FONT_SIZE=basefontsize, XOFFSET=xoffset, YOFFSET=(papiery-yoffset), $
            /LANDSCAPE,ENCAPSULATED=pstype, filename=filep
      ENDIF ELSE BEGIN
            DEVICE,/COLOR, XSIZE=pssizex, YSIZE=pssizey,BITS_PER_PIXEL=8, $
            FONT_SIZE=basefontsize, XOFFSET=xoffset, YOFFSET=yoffset-1., /PORTRAIT, $
            ENCAPSULATED=pstype,filename=filep
      ENDELSE
   ENDIF ELSE BEGIN
; calculate x and y size for screen display
; Conditions sur la taille de la fenetre:
; winx < ou = mwinx   avec winx=f*
; winy < ou = mwiny   avec winy=f*
; le rapport ff entre les deux facteurs est determine par la condition:
; (dans le cas ou EQSCALE=1):
;  winx*(trx-blx)/(winy*(try-bly))
;      = (rangex(1)-rangex(0))/(rangey(1)-rangey(0))

      IF flag EQ 1 AND MAPFLAG EQ 0 THEN BEGIN
         f=MIN([FLOAT(mwinx)*(trx-blx)/(rangex(1)-rangex(0)), $
                FLOAT(mwiny)*(try-bly)/(rangey(1)-rangey(0))])
         ;f is the maximum number of pixels per data unit

         winx=f*(rangex(1)-rangex(0))/(trx-blx) ; horizontal size of plot output
         winy=f*(rangey(1)-rangey(0))/(try-bly) ; vertical size of plot output
      ENDIF ELSE BEGIN ;no equal scaling required
         winx=(mwinx)   ; horizontal size of plot output
         winy=(mwiny)   ; vertical size of plot output
      ENDELSE
   ENDELSE
   xtoy=1./(FLOAT(winx)/FLOAT(winy)*(rangey(1)-rangey(0))/(rangex(1)-rangex(0)))
END

;**********************************************************************
PRO LOCATE,itype,ihem,itrans,lat,lon,i,j
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  
; attention  
;parametres du locate
;ihem=1		;Pole NORD
;itype=1 	;resolution 12.5km
;itrans=1	;i,j==>lat,lon 	;LOCATE,itype,ihem,itrans,lat,lon,ix,iy
		;sous IDL rajouter 1 a ix et iy avant appel au locate
;itrans=2	;lat,lon ==>i,j	;LOCATE,itype,ihem,itrans,lat,lon,ix,iy
		;sous IDL enlever 1 a ix et iy apres appel au locate
;;;;;  
  
  
  
;ATTENTION cette routine est traduite du fortran. 
;les indice i et j commencent a 1 et non pas a zero
; (Attention a PV-Wave il faut enlever 1 a I et J dans le sens lat lon vers I,J
; (Attention a PV-Wave il faut ajouter 1 a I et J dans le sens I,J vers lat lon 
;
; LOCATE - This routine transforms I,J coordinates of an SSM/I grid cell
;          to latitude and longitude coordinates. This program provides
;          the inverse functions as well. LOCATE interfaces to the revised
;          forms of the subroutines, MAPXY and MAPLL.
;
;  User-defined Parameters:
;
;     itype   : Integer supplied by the user to describe one of the three
;               grid cell dimensions (12.5 km, 25.0 km, or 50.0 km).
;
;     ihem    : Integer supplied by the user to describe one of the two
;               polar regions (1=North , 2=South)
;
;     itrans  : Integer supplied by the user to describe the type of
;               transformation LOCATE will perform (1=I,J-to-Lat,Lon;
;               2=Lat,Lon-to-I,J)
;
;     i,j     : Integers supplied by the user when itrans = 1. These 
;               integers describe the position of a cell in an SSM/I grid.
;
;     lat,lon : Reals supplied by the user when itrans = 2. These
;               integers describe the latitude and longitude in an SSM/I
;               grid which LOCATE will transform to an I,J grid cell position.
;               Note: All latitudes and longitudes must be entered as 
;                     positive numbers!
;
;  Internal:
;
;     x,y     : Distance in kilometers from the origin of the grid
;               (ie., pole).
;
;     alat,
;     alon    : Computed latitude and longitude returned from MAPXY.
;
;     SGN     : Sign of the latitude (positive = north latitude,
;               negative = south latitude)
;
;     delta   : Meridian offset for the SSM/I grids (0 degrees for
;               the South Polar grids; 45 degrees for the North Polar
;               grids.
;
;     kk      : Integer variable used for reorientation of the grid. The
;               grid is 'flipped' in the Y direction for transformations.
;
;     SLAT    : Standard latitude for the SSM/I grids is 70 degrees.
;
;     numy    : Number of lines in an SSM/I grid. This attribute varies
;               for each of the six grids.
;
;    cell    : Size of the SSM/I grid ( 12.5 km, 25.0 km, 50.0 km)
;
;     xydist  : Distance from the origin of the grid in the cartesian plane.
;               The x-y coordinates for the edge of the lower left pixel
;               is (3850.0, 5350.0) for the northern grids and
;               (3950.0, 3950.0) for the southern grids.
;
;     RE      : Radius of the earth in kilometers.
;
;     E       : Eccentricity of the Hughes ellipsoid
;
;     E2      : Eccentricity squared
;
;     PI      : Pi
;                              Written by  V.J.Troisi - January, 1990
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
numy=INTARR(2,3)
cell=FLTARR(3)
xydist=FLTARR(2,2)
numy(0,0)=896
numy(1,0)=664
numy(0,1)=448
numy(1,1)=332
numy(0,2)=224
numy(1,2)=166
cell(0)=12.5
cell(1)=25.
cell(2)=50.
xydist(0,0)=3850.
xydist(1,0)=5350.
xydist(0,1)=3950.
xydist(1,1)=3950.
;
SLAT = 70.
RE = 6378.273
E2 = .006693883
PI = 3.141592654
E =  SQRT(E2)
;
;     grid cell size.
;
;       the grid cell dimension: itype
;         1 - 12.5 Km'
;         2 - 25.0 Km'
;         3 - 50.0 Km'
;
;
;       the hemisphere of interest: ihem
;         1  - North'
;         2  - South'
;
; Define the sign and meridian offset (delta) for the SSM/I grids.
;
IF (ihem EQ 1) THEN BEGIN
SGN   = 1.0
delta = 45.
ENDIF ELSE BEGIN
SGN   = -1.0
delta = 0.0
ENDELSE
;
; translation type : itrans
;        0 - Exit'
;        1 - Convert I,J to Latitude, Longitude'
;        2 - Convert Latitude, Longitude to I,J'
;
; Start translation
;
; Because of PV-WAVE indexing starts with 0:
ihem=ihem-1
itype=itype-1
IF itrans EQ 0 THEN GOTO, sortie
IF itrans EQ 1 THEN GOTO, ijintoll
IF itrans EQ 2 THEN GOTO, llintoij
;
; Convert I,J pairs to x and y distances from origin. The grid will be
;
; 'flipped' in the 'Y' direction.
;
ijintoll: x=((i-1)*cell(itype))-(xydist(0,ihem)-cell(itype)/2.)
kk=numy(ihem,itype)-(j-1)
y=((kk-1)*cell(itype))-(xydist(1,ihem)-cell(itype)/2.)
;
; Transform x and y distances to latitude and longitude
;
MAPXY,x,y,alat,alon,SLAT,SGN,E,RE
;
; Transform radians to degrees.
;
alon=alon*180./PI
lat=alat*180./PI
alon=alon-delta
;
; Convert longitude to positive degrees
;
lon=alon
IF alon LE 0.0 THEN lon=alon+360.
IF alon GE 360.0 THEN lon=alon-360.
;
GOTO, sortie
;
;     llintoij transforms lat,lon en i,j
;
llintoij: alat=abs(lat)*PI/180.
alon=(lon+delta)*PI/180.

;
; Transform latitude and longitude to x and y distances from origin
;
MAPLL,x,y,alat,alon,SLAT,SGN,E,RE
;
; Convert x and y distances from origin to I,J pair (ii,jj)
;


i=FIX(((x+xydist(0,ihem)-cell(itype)/2.)/cell(itype))+.5)+1
j=FIX(((y+xydist(1,ihem)-cell(itype)/2.)/cell(itype))+.5)+1


;
; Flip grid orientation in the 'Y' direction
; 
j=numy(ihem,itype)-(j-1)

;
sortie: ihem=ihem+1
itype=itype+1
RETURN
END
;*****************************************************************************
PRO MAPLL,X,Y,AALAT,AALONG,SGN
;*****************************************************************************
;                                                                    
;    DESCRIPTION:
;
;    This subroutine converts from geodetic latitude and longitude to Polar  *
;    Stereographic (X,Y) coordinates for the polar regions.  The equations   *
;    are from Snyder, J. P., 1982,  Map Projections Used by the U.S.         *
;    Geological Survey, Geological Survey Bulletin 1532, U.S. Government     *
;    Printing Office.  See JPL Technical Memorandum 3349-85-101 for further  *
;    details.
;
;                                                                            ;
;    ARGUMENTS:
;
;    Variable    Type        I/O    Description
;                                                                            *
;    ALAT       REAL*4        I     Geodetic Latitude (degrees, +90 to -90)  *
;    ALONG      REAL*4        I     Geodetic Longitude (degrees, 0 to 360)   *
;    X          REAL*4        O     Polar Stereographic X Coordinate (km)    *
;    Y          REAL*4        O     Polar Stereographic Y Coordinate (km)    *
;                                                                            *
;                                                                            *
;                 Written by C. S. Morris - April 29, 1985                  *
;                  Revised by C. S. Morris - December 11, 1985               *
; 
;                  Revised by V. J. Troisi - January 1990 
;                  SGN - provides hemisphere dependency (+/- 1)
;*****************************************************************************
;    DEFINITION OF CONSTANTS:                                                *
;                                                                            *
;    Conversion constant from degrees to radians = 57.29577951.              *
COMMON MAPPOLE, XPOLE, YPOLE, SLAT, RE, E2, E

CDR=57.29577951
E2=E*E
PI=3.141592654
;*****************************************************************************
;     Compute X and Y in grid coordinates.

      ALAT = AALAT / CDR
      ALONG = (AALONG+45) / CDR

IF ABS(ALAT) LT PI/2. THEN GOTO, T
      X=0.0
      Y=0.0
      GOTO, ret
T: T=TAN(PI/4.-ALAT/2.)/((1.-E*SIN(ALAT))/(1.+E*SIN(ALAT)))^(E/2.)
IF ABS(90.-SLAT) LT 1.E-5 THEN BEGIN
RHO=2.*RE*T/((1.+E)^(1.+E)*(1.-E)^(1.-E))^(1/2.)

ENDIF ELSE BEGIN
SL=SLAT*PI/180.
TC=TAN(PI/4.-SL/2.)/((1.-E*SIN(SL))/(1.+E*SIN(SL)))^(E/2.)
MC=COS(SL)/SQRT(1.0-E2*(SIN(SL)^2))
RHO=RE*MC*T/TC
ENDELSE
Y=-RHO*SGN*COS(SGN*ALONG)
X= RHO*SGN*SIN(SGN*ALONG)

ret: RETURN
END
;*****************************************************************************
PRO MAPXY,XI,YI,ALAT,ALONG,SGN
;*****************************************************************************
;                                                                            *
;                                                                            *
;    DESCRIPTION:                                                            *
;                                                                            *
;    This subroutine converts from Polar Stereographic (X,Y) coordinates     *
;    to geodetic latitude and longitude for the polar regions. The equations *
;    are from Snyder, J. P., 1982,  Map Projections Used by the U.S.         *
;    Geological Survey, Geological Survey Bulletin 1532, U.S. Government     *
;    Printing Office.  See JPL Technical Memorandum 3349-85-101 for further  *
;    details.                                                                *
;                                                                            *
;                                                                            *
;    ARGUMENTS:                                                              *
;                                                                            *
;    Variable    Type        I/O    Description                              *
;                                                                            *
;    X          REAL*4        I     Polar Stereographic X Coordinate (km)    *
;    Y          REAL*4        I     Polar Stereographic Y Coordinate (km)    *
;    ALAT       REAL*4        O     Geodetic Latitude (degrees, +90 to -90)  *
;    ALONG      REAL*4        O     Geodetic Longitude (degrees, 0 to 360)   *
;                                                                            *
;                                                                            *
;                  Written by C. S. Morris - April 29, 1985                  *
;                  Revised by C. S. Morris - December 11, 1985               *
;                                                                            *
;                  Revised by V. J. Troisi - January 1990
;                  SGN - provide hemisphere dependency (+/- 1)
;
;*****************************************************************************
;                                                                            *
;    DEFINITION OF CONSTANTS:                                                *
;                                                                            *
;    Conversion constant from degrees to radians = 57.29577951.              *
COMMON MAPPOLE, XPOLE, YPOLE, SLAT, RE, E2, E

X=XI-XPOLE
Y=YI-YPOLE
CDR=57.29577951
E2=E*E
PI=3.141592654
;                                                                            *
;*****************************************************************************
SL = SLAT*PI/180.
E200: RHO=SQRT(X^2+Y^2)
IF RHO GT 0.1 THEN GOTO, E250
ALAT=90.*SGN
ALONG=0.0
GOTO, E999
E250: CM=COS(SL)/SQRT(1.0-E2*(SIN(SL)^2))
T=TAN((PI/4.0)-(SL/(2.0)))/((1.0-E*SIN(SL))/(1.0+E*SIN(SL)))^(E/2.0)
IF ABS(SLAT-90.) LT 1.E-5 THEN BEGIN
T=RHO*SQRT((1.+E)^(1.+E)*(1.-E)^(1.-E))/2./RE
ENDIF ELSE BEGIN
T=RHO*T/(RE*CM)
ENDELSE
CHI=(PI/2.0)-2.0*ATAN(T)
ALAT=CHI+((E2/2.0)+(5.0*E2^2.0/24.0)+(E2^3.0/12.0))*SIN(2*CHI)
ALAT=ALAT+((7.0*E2^2.0/48.0)+(29.0*E2^3/240.0))*SIN(4.0*CHI)+(7.0*E2^3.0/120.0)*SIN(6.0*CHI)
AALAT=SGN*ALAT
ALONG=ATAN(SGN*X,-SGN*Y)
AALONG=SGN*ALONG

      ALAT  = AALAT * CDR
      ALONG = AALONG/CDR -45

E999: RETURN
END




