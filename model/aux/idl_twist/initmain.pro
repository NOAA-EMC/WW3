;/
;/                  +-----------------------------------+
;/                  | TWIST                Ifremer-SHOM |
;/                  |           F. Ardhuin              |
;/                  |              IDL command language |
;/                  | Last update :         27-Oct-2013 |
;/                  +-----------------------------------+
;/
;/ Licence information: This code is distributed under the CeCILL license
;/                      generally compatible with the Gnu Public Licence (GPL) 
;/                      http://www.cecill.info/index.en.html
;/
;/    17-Oct-2013 : Clean up                            ( version 2.00 )
;/    27-Oct-2013 : Debugged interface version          ( version 2.00 )
;----------------------------------------------------------------------------
; This subroutine defines user-dependent settings (default paths ...)
;----------------------------------------------------------------------------
Pro Initmain
;+
; NAME:
;      Initmain
; PURPOSE:
;      Initializes variables and flags (display, data ...)
; CALLING SEQUENCE:
;      initmain
; INPUTS:none
; COMMON BLOCKS:
;      CONTOURPARAM,CURRENT,DRAWING,DRAWSIZE,FLAGS,FONTS,TITLES,POSTSCRIPT
;     THREED,ZOOM
;     FREQ,DIR,TIME,SPACE,
;     FILES,GRID,GRIDpar,RAY3,RAY3flags,RAYS
; MODIFICATION HISTORY:
;     under construction (1/1/1999 - 6/1/2001)
;-

;*******COMMON BLOCKS**************************************
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
COMMON POSTSCRIPT, filep,pspath,prcoul,psor,pstype, $
                pwinx,pwiny,papierx,papiery,xoffset,yoffset, $
                facpolice,fontrescale,basefontsize,pssizex,pssizey,psfont
COMMON THREED,  Ax3D,Az3D,smoothing
COMMON ZOOM,    nxzmax,nyzmax,nxzmin,nyzmin,maxdepth,mindepth
COMMON WIDGETS, Wtoprow,Wright,Wdraw,Wdraw_value_update,Wsimple
COMMON INTERFACE, TWISTVA_VERSION


;** 2 ** Display and data variables/parameters
common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

COMMON FREQ,    freq,nfband,findex,findex2
COMMON DIR,     nabin,nabint,aindex,THETA
COMMON TIME,    timestep,tindex,tindex2,ntime,dtime,time0,day0,dtindex,nstep, $
                timezone_plot,timezone_string,months,time
COMMON SPACE,   c_gp,c_cut,indexgp,c_x,c_y,c_lon,c_lat
COMMON TRANSECT,Ntrans,Strans,Xtrans,Ytrans,Ztrans,Itrans, $
                  COtrans,TransOK,transsym,transline,transthick,Ispectrans, $
                  spectransname,ntransgp,transsymsize

COMMON MAP,     MAPFLAG, MAPPROJ, MAPLONGCENTER, MAPLATCENTER, MAPCONTINENT,  $
                MAPCOUNTRIES, MAPLONLAT
COMMON OVERLAY, addir,adsyms,adbathy,adcoast,psyms,psymsizes,adtr,adtri
COMMON OVERLAY2,adbathydot,adcontour
COMMON SPECIALS,nspecgp,specmat,specname,c_spec,ispec1,ispec2
COMMON TIMESTEPSUG,TIMESTEPS,NTIMESTEPS,FLAGTSTEP, TIMESTEPS_ORDER

;** 3 ** I/O and data variables
COMMON FILES,   filestatus,datastatus,paths,filters,filenames,raypath
COMMON DIR_STACK, DEPTH, STACK
COMMON BATHY,   gd,nx,ny,dx,dy,sx,sy,rlonmax,rlonmin,rlatmin,rlatmax
COMMON GRID,    gridmat,nngp,ngpused,zonestart,zoneend,truegp,gpused,gpnotused
COMMON GRIDTIMESTEPS, zoffset, updateonmove, waveperiod
COMMON Builder,ngp,H0,H1,Hi,KH0,KH1,DX0,DX1,T0,ROA,ROI,gridbuild_click
COMMON TRIANGLES,ntri,trigp,nzone,c_zone,zcolor,Hlandsea,contourline
COMMON GRIDTIMESTEPS, zoffset, updateonmove, waveperiod
COMMON TIMESERIES, modspec,obs,obs2,om,ts_filetype, nbins, bulktype,scatvar
COMMON RAYS,    raysOK,raystype,raynsteps,rayx,rayy,raya,rayamin,rayamax, $
                rayres,rayfreq,rayGP,raytimestep,rayoffdep,rayflag,raydz,raymindepth

;*******END OF COMMON BLOCKS*******************************

;***** Screen drawing dimensions *****************
 IF !D.NAME EQ 'WIN' THEN BEGIN  ;adjusts the size of the draw window to PC/UNIX
; For MS Windows
    xsize=800                    ;x pixel size of drawing window
    ysize=800                    ;y pixel size of drawing window
 ENDIF ELSE BEGIN
; For UNIX
    xsize=600                    ;x pixel size of drawing window
    ysize=700                    ;y pixel size of drawing window
 ENDELSE
 Wsimple=0        ;complexity of graphics interface
 mwinx=xsize      ;maximum length of draw window
 mwiny=ysize      ;maximum height of draw window
 winx=xsize       ;width of plot window
 winy=ysize       ;height of plot window
 cbar=1           ;color bar flag: 1 adds color bar/ 0 no color bar (for 2D-3D color plots)
 axis_orient=0
 bly = 0.1+float(cbar)/10.    ; normalized bottom left y coordinate for draw region
 blx = 0.15  ; normalized bottom left y coordinate for draw region
 trx = 0.95 ; normalized top right x coordinate for draw region
            ; not equal to one in order to leave space for the numbers on the
            ;axes
 try = 0.95 ; normalized top right y coordinate for draw region
 textx=0.2
 texty=0.1
 textdx=0.16
 textdy=0.03

 c_x=!values.F_NAN
 cbblx=1.   ;position of the color bar in the bottom margin (see procedure colorbar)
 cbtrx=1.
 cbbly=0.45
 cbtry=0.6

 cbnticks=5
 cbexrange=1
 
 ; Mapping parameters
 
 MAPFLAG=0
 MAPPROJ=0
 MAPLONGCENTER=-150
 MAPLATCENTER=0 
 MAPLONLAT=1
 MAPCONTINENT=2
 MAPCOUNTRIES=0
 c_lon=1/0.
 c_lat=1/0.

;   FILE TYPES:
; 2:              'Bathy log file'                 log file for the bathymetry
; 3:              'Binary bathy'                   binary bathymetry file
; 4:              'GSHHS coastline'                binary shoreline file
; 5:              '   ',  
; 6:              '*.line coastline'               ASCII polyline 
; 7:              'GMESH triangle-based grid', $   GMESH grid (as used in WAVEWATCH III)
; 8:              'Transect', $
; 9:              '   ', $
;10:              'Observed time series', $
;11:               'Modelled time series', $
;12:               'Additional modelled TS', $
;13:               '   ', $
;14:               'All modelled TS for new point', $
;15:               '   ', $
;16:               'Additional bulk time series', $
;17:               'WW3 NetCDF map', $
;18:               'GSHHS coast', $
;19:               'Data flags', $
;20:               'WW3 map', $
;21:               'WW3 map diff', $
;22:               'Color palette'
; DEFAULT ACCESS PATHS FOR THESE FILES:

; Paths under windows

 IF !VERSION.OS_FAMILY EQ 'Windows' THEN BEGIN
    set_plot,'WIN'
    device, decomposed=0
    raypath='D:\fabrice\IDL\'
    paths=['D:\Fabrice\Runs\','C:\Fabrice\bathy\', $
       'D:\DATA\bathy\','D:\DATA\bathy\', $
       'D:\Fabrice\bathy\GLOBAL_COAST\','C:\fabrice\runs\', $
       'C:\Fabrice\runs\', $
       'C:\Fabrice\runs\','C:\Fabrice\runs\', $
       'C:\Fabrice\DATA\', $
       'C:\Fabrice\runs\', $
       'C:\Fabrice\runs\', $
       'C:\Fabrice\DATA\', $
       'C:\Fabrice\DATA\SHOWEX\', $
       'C:\Fabrice\DATA\DUCK94\8MARR94\', $
       'C:\Fabrice\runs\','C:\Fabrice\SHOWEX\ANALYSIS\', $
       'I:\runs\','D:\DATA\Bathy\GLOBAL_COAST\', $
       'C:\Fabrice\DATA\','C:\Fabrice\DATA\','C:\Fabrice\DATA\','']
       pspath='C:\Fabrice\PAPERS\'
 ENDIF ELSE BEGIN
;
; For UNIX
;
   set_plot,'X' ,/COPY
   device, pseudo_color=8
   device, decomposed=0, RETAIN=2
   raypath='.'
   paths=['/export/home/ardhuin/RUNS/','/export/home/ardhuin/TOOLS/POLYMESH/TESTCASES/',  $
          '/export/home/ardhuin/DATA/BATHY/','/export/home/ardhuin/DATA/BATHY/', $
	  '/export/home/ardhuin/DATA/BATHY/GLOBAL_COAST/', '/export/home/ardhuin/', $
	   '/export/home/ardhuin/DATA/BATHY/GLOBAL_COAST/','/export/home/ardhuin/', $
	   '/export/home/ardhuin/','/export/home/ardhuin/', $
          '/export/home/ardhuin/', '/export/home/ardhuin/', $
          '/export/home/ardhuin/DATA/', '/export/home/ardhuin/DATA/', $
       '/export/home/ardhuin/', $
       '/data/span11/ardhuin/PAPERSC/','/export/home/ardhuin/TEMP/', $
  '/export/home/ardhuin/TEMP/','/export/home/ardhuin/DATA/BATHY/GLOBAL_COAST/', $
    '/export/home/ardhuin/DATA/','/export/home/ardhuin/RUNSAB/TESTWW3/', $
    '/export/home/ardhuin/RUNSAB/TESTWW3/','/export/home/ardhuin/TOOLS/IDL/palettes']
       pspath='~'
 ENDELSE
 !P.FONT=0
 LOADCT, 13


; DEFAULT FILTERS FOR THESE FILES:

 filters=[  '*.inp','xyz.dat','*.log','*.grd','gshhs*.b' , $
            '*.line','*.*msh','*.grgp','*.tr', '*.*'     , $
            '*.*','*.*','*.*','*.*','*.*'    , $
            '8MARR*','*bulk*','*.nc','gshhs*.b','*.dat', $
            '*.*','*.*','*.ct']

; DEFAULT NAMES FOR THESE FILES:
 filenames=['crest.inp','','duck1.log','w_duck1.grd','duck.coast', $
            'duck30.grgp','depthstat.dat','trinet8zone.tri','','', $
            '','','','','','','','','','flags.dat', '', '','']

;This defines the user symbol, used when PSYM=8
A = FINDGEN(17) * (!PI*2/16.)    ;Make a vector of 16 points, A[i] = 2pi/16.
USERSYM, COS(A), SIN(A), /FILL   ;Define the symbol to be a filled circle

;General display variables

 outtit=1         ;1: diplays title / 0: hides title
 outxtit=1        ;1: diplays xtitle / 0: hides xtitle
 outytit=1        ;1: diplays ytitle / 0: hides ytitle
 outcbtit=1       ;1: diplays color bar title / 0: hides title

 frametype=2
 outx1lab=1
 outx2lab=1
 outy1lab=1
 outy2lab=1

 plottype=0
 plotncvar=0
 normvec=1
 filltype=0               ;current scheme for 2d plots (contour,bitmap..)
 maxval=1.E6              ;maximum value for contouring
 IF !D.NAME EQ 'WIN' THEN $
 Navailcolor=256 ELSE $
 Navailcolor=!D.N_colors;-1  ;number of colors in color table
 IF Navailcolor GT 256 THEN BEGIN
   Navailcolor=256
 ENDIF

 numlevels=11     ;default number of levels for contours
 c_repart=0       ;current repartition of the contour levels (linear,log ...)
 fixrange=0       ;0:the range of contours is fitted to data, 1:fixed to current values
 filloutofrange=1 ;

 subwin=[0,0,0,0,0,0,0] ;status of subwindows (0:closed, 1: open):
         ;     [mesh editor,raytracer,parameters,palette,transects,specials]

 Ax3D=35.         ;rotation angle around x-axis for 3D views
 Az3D=45.         ;rotation angle around z-axis for 3D views
 smoothing=1      ;size of the square boxcar filter applied to 3D-displayed matrix
 nbins=51         ;number of bins for QQ and binned plots


;Display options for overlays
 addir=0         ;adds wave direction to plot if = 1
 adsyms=0        ;adds symbols for the grid points if = 1
 adbathy=0       ;overlays bathymetry contours if = 1
 adcoast=1       ;adds coast to 2D plots if =1
 adtri=0
 adbathydot=0
 adcontour=0

 psyms=[8]        ;symbol for grid points (5: triangles, 8: user defined)
 psymsizes=[0.25] ;size of these symbols

 nstep=1          ;number of raytimesteps in the ABCDEFGHI an 8MARR files
 dtime=180        ;raytimestep between two files in minutes

 c_gp=1           ;current grid point (used for "local" plots: f spectrum...)
 c_spec=0
 c_cut=0          ;current "cut": 2D plane, local ...
 datatype=0
FLAGTSTEP=0


 ;***** postscript settings, units are cm *****************
 papierx=21.5;21.0          ; x-size of the paper
 papiery=27.8 ;29.7         ; y-size of the paper
 pwinx=19.5 ;19.0           ; width of paper in cm (printable area)
 pwiny=25.8 ;27.7           ; height of draw paper in cm (printable area)
 facpolice= 1.              ; scaling factor for text on postscript
 fontrescale=1
 basefontsize=10
 psfont=0
 xoffset=1.0                ; offset from the paper edge
 yoffset=2.0                ; offset from the paper edge
 psor=0                     ; print orientation eq 0 for portrait, 1 for landscape
 eqscale=1                  ; 1 if x and y scales are equal
 output=0                   ; 0: screen / 1: postscript file
 pstype=0                   ; type of postcript file, 0: normal / 1: encapsulated
 prcoul=1                   ; black and white (0) / color (1)


; variables for Bathy grid and coastline
 rlonmin=0.D       ;minimum longitude (negative if W)
 rlonmax=0.D       ;maximum longitude (neg. if W)
 rlatmin=0.D       ;min. latitude (neg. if S)
 rlatmax=0.D       ;max. latitude
 sx=0.D            ;grid resolution in meters in the x direction (W-E)
 sy=0.D            ;grid resolution in meters in the y direction (S-N)
 nx=0             ;number of grid points (bathymetry) in x
 ny=0             ;number of grid points (bathymetry) in y




; defines default parameters for grid
 zoffset=4        ; 
 WAVEPERIOD =  26.809651 
 updateonmove=1   ;
 nzone=5
 c_zone=5
 zcolor=INTARR(nzone+1)-1



 nfband=2         ;number of frequencies
 nabin=2          ;number of directions
 timezone_plot=-0 ;-5;time zone of the plot output
 timezone_string='UTC' ;'EST' ;corresponding abbreviation
 ntime=2          ;number of raytimesteps for which data is available,
                  ;maximum range of the time "widget slider"
 ndtime=2         ;ndtime: maximum range of the "dt" slider, i.e.
                  ;  maximum value of the dtindex variables
                  ;  which prescribes the number of raytimesteps to be skipped
                  ;  when moving the time slider
 findex=1         ;current frequency (prescribed by action on a slider)
 findex2=1         ;current frequency (prescribed by action on a slider)
 aindex=0         ;current direction (prescribed by action on a slider)
 tindex=1         ;current timestep (slider)
 dtindex=1        ;cf. ndtime
 tindex2=1


;Variables and flags for the computation and display of rays
 RaysOk=0         ;1 if rays have be computed (call to fortran) and are
                  ;ready for display
 raytimestep=5.   ;timestep between two points on a ray
 rayfreq=0.05     ;frequency of the rays
 rayres=1.        ;difference in initial directions between 2 rays (deg)
 rayGP=1.         ;combined index of the grid points from which rays are
                  ;computed
 rayamin=-90.     ;angle (relative to x axis) of the first ray
 rayamax=90.      ;angle (idem) of the last ray
 rayoffdep=300.   ;depth at which the ray are stopped
 raydz=0.         ;mean water level adjustment for the ray computation
 raymindepth=8.

;Variables and flags for the computation and display of transects
 adtr=1
 TransOK=0
 transthick=1
 transline=0
 transsym=1

 nxzmax=0         ;x index of the top right point for the zoom
 nyzmax=0         ;y index of the top right point for the zoom
 nxzmin=0         ;x index of the bottom left point for the zoom
 nyzmin=0         ;y index of the bottom left point for the zoom
 maxdepth=100.    ;top of range for 2D plot (bathy, RAY3 ...)
 mindepth=0.      ;min of range for 2D plot (bathy, RAY3 ...)
 logplot=0

 ts_filetype=0

 clickflag=0      ;flags that specifies the action triggered by the
                  ;next click.
                  ;0: uptates c_gp with the index of the grid point
                  ;   closest to the cursor

 nfiletypes=23

 filestatus=BYTARR(nfiletypes) ;resets all sorts of data files to "closed"
 datastatus=BYTARR(nfiletypes) ;resets all sorts of data  to "not available"

 ilang=0
 dictionnaire={days:['Days','Jours'],at:['at','a'], $
          hs:['Significant wave height','Hauteur significative des vagues'], $
          Wdir:['W','O'], dpt:['depth','profondeur']}
 months=STRARR(2,12)
 months(0,*)=['January', 'February', 'March', 'April', 'May', 'June', 'July', $
         'August', 'September', 'October', 'November', 'December']
 months(1,*)=['Janvier', 'Fevrier', 'Mars', 'Avril', 'Mai', 'Juin', 'Juillet', $
         'Aout', 'Septembre', 'Octobre', 'Novembre', 'Decembre']

;
; Variables for mesh editor
;
   ROA=0
   H0=8.
   H1=1500.
   Hi=60.
   DX0=1.
   DX1=15.
   T0=10.
   Hlandsea=-9.;


TWISTVA_VERSION='RUNTIME';
 
  GET_LUN,unit
  OPENR,unit,'TWIST_init.txt', ERROR=openerr
  IF (OPENERR EQ 0) THEN BEGIN 
     PRINT,'Reading user defined parameters from TWIST_init.txt'
     RSTRING='                                                                                       '
     RINT=0
     RINT2=0
     RFLOAT=0.

     RFLOAT2=0.
     NCOM=0
     WHILE (NOT EOF(unit)) DO BEGIN
        CASE NCOM OF
           0: BEGIN 
             PRINT,'Reading plot window size'
             READF,unit,RINT1,RINT2
               mwinx=RINT1      ;maximum length of draw window
               mwiny=RINT2      ;maximum height of draw window
               winx=RINT1       ;width of plot window
               winy=RINT2       ;height of plot window
              END
           1:BEGIN 
             PRINT,'Reading paths for the ',nfiletypes,' file types.'
             FOR I=0,nfiletypes-1 DO BEGIN 
               READF,unit,RSTRING,FORMAT='(A)'
               IPOS=STRPOS(RSTRING,' ') 
               IF (IPOS GT 0) THEN RSTRING=STRMID(RSTRING,0,IPOS-1)
               paths(I)=RSTRING
             ENDFOR
             END
           2:BEGIN 
               PRINT,'Reading longitude and latitude for time series export ... '
               READF,unit,RFLOAT,RFLOAT2
               c_lon=RFLOAT
               c_lat=RFLOAT2
               print,'Target longitude and latitude:',c_lon,c_lat
             END
           3: BEGIN 
             PRINT,'Reading interface complexity flag: (0 = full interface, 1 = simplified interface)'
             READF,unit,RINT1 
             Wsimple=RINT1
             END
           4: BEGIN 
               file2='';
               READF,unit,RSTRING,FORMAT='(A)'
               IPOS=STRPOS(RSTRING,' ') 
               IF (IPOS GT 0) THEN RSTRING=STRMID(RSTRING,0,IPOS)
               file2=RSTRING
               message,'Loading table ' + file2,/INFO
               GET_LUN,lun2
               OPENR,lun2,file2
              r=BYTARR(256)
              g=BYTARR(256)
              b=BYTARR(256)
              READF,lun2,r,g,b
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
              CLOSE,lun2
            END 
           5: BEGIN 
               TWISTVA_VERSION='LICENCE';
               READF,unit,RSTRING,FORMAT='(A)'
               IPOS=STRPOS(RSTRING,'R') 
               IF (IPOS GE 0) THEN TWISTVA_VERSION='RUNTIME'
               PRINT,RSTRING,'Choice of inteface version (LICENCE/RUNTIME):',TWISTVA_VERSION
               END
            ELSE: 
           ENDCASE
         NCOM=NCOM+1
     ENDWHILE
     CLOSE,UNIT
     FREE_LUN,UNIT
  ENDIF

END
