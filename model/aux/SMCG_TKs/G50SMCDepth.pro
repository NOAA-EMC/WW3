; WAVE Version 8.00 (Linux i386)
; Journal File for frjl@eld330
; Working directory: /net/data/cr2/frjl/Bathy
; Date: Fri Sep 16 09:55:18 2005
; To plot NAEbathy.pp as a contour gif file
; First created:   16 Sep 2005   Jian-Guo Li
; To smooth out land/sea for the final bathymetry
; Prepare mask, depth and trans files for GSMC model. 
;                   25 May 2011   Jian-Guo Li
; Adapted for SMC50 model input file creation.
;                    8 Aug 2011   Jian-Guo Li
; Prepared for WW3 users outside UKMO.  JGLi15Nov2013
;               
 
;; Open the 50km global bathymetry data file
   OPENR, 9, "./G50kmBathy.dat"

;; Skip the first comment line
   Messge='Text'
   READF, 9, Messge

;; Declare header variables to store data
   NCol=0L
   NRow=0L
   FLon=0.0
   FLat=0.0
   DLon=0.0
   DLat=0.0

   READF, 9, NCol, NRow, FLon, FLat, DLon, DLat

;; Declare bathymetry array
   Bathy=FLTARR(NCol, NRow)

   READF, 9, Bathy

   Print, "Bathymetry data read done."
   CLOSE, 9

   Print, "NCol, NRow, FLon, FLat, DLon, DLat"
   Print,  NCol, NRow, FLon, FLat, DLon, DLat

;;  Read in GSMC cell array to redefine depth.
;;  Global part from G50SMCels.dat
 ng=0L
 n1=0L
 n2=0L
 n4=0L

 openr, 9, './G50SMCels.dat'
 readf, 9, ng, n1, n2, n4
 cel=intarr(5,ng)
 readf, 9, cel
 close, 9

 Print, ' GloMCels read done'
 Print, ng, n1, n2, n4
 Print, cel(*,0)
 Print, cel(*,ng-1)

;; Work out Equator index for G50SMC model row
   EqtDlt= 0.0 - FLat + 0.5*DLAT
   NEqutr= FIX( EqtDlt/DLAT + 0.001 )
  Print, ' Equator index NEqutr =', NEqutr
   ICel = cel(0,*)
   JCel = cel(1,*) + NEqutr
   KCel = cel(2,*)
   HCel = FLOAT( cel(3,*) )

;; Select N boundary latitude 
; NR = 344  ;; about 83N
; NR=NRow 
  NR=MAX(JCel) + 1
  NC=NCol 

  PRINT, "NC, NR=", NC, NR
  PRINT, "DX, DY=", DLon, DLat 
  PRINT, "X1, Y1=", FLon, FLat
  PRINT, "XN, YN=", FLon+DLon*(NC-1), FLat+DLat*(NR-1)

;; Create new depth full-grid array
;; Set land points to be -1.0 m 
   Depth = fltarr(NC, NR)
   Depth(*,*) = -1.0 
   FOR n=0L, ng-1 do begin
       i=ICel(n)
       j=JCel(n)
       k=KCel(n)
;      Print, n, i, j, k, i+k-1, HCel(n)
       for m=i, i+k-1 do Depth(m, j) = HCel(n)
;      Depth(i:i+k-1, j) = HCel(n)
   ENDFOR
   
;; Count land points.
 lands=where(Depth LE 0.0,   landscnt)
 PRINT, 'Land points=', landscnt

;; Limit maximum depth to 999 m
 deeps=where(Depth GE 999.0, deepscnt)
 Depth(deeps)=999.0

;;  Make minimum depth 10 m 
 lwlnd=where((Depth GE 0.0) AND (Depth LE 10.0), lwlndcnt)
 Depth(lwlnd)= 10.0
 
;;  Sea points and minimum and maximum sea depth
 seaps=where(Depth GT 0.0, seapscnt)
 PRINT, ' Sea points=', seapscnt

  PRINT, "Hm, HM=", min(Depth(seaps)), max(Depth(seaps))

 OPENW, 9, "G50SMCDepth.dat"
 FOR j=0, NR-1 DO BEGIN
    PRINTF, 9, Depth(*,j), format='((10F8.1))'
 ENDFOR
 CLOSE, 9

;; Read in land percentage data from GSMC50kObstr.dat. JGLi8Aug2011
  OPENR, 10, "G50kmObstr.dat"
  NCobs=1
  NRobs=1
  READF, 10, NCobs, NRobs
  PRINT,     NCobs, NRobs
  IF( NCobs NE NC  OR  NRobs NE NR ) THEN BEGIN
      PRINT, " Inconsistent NC or NR =", NC, NR
  ENDIF 
      Fobsin=fltarr(NCobs, NRobs)
      READF, 10, Fobsin
  CLOSE, 10

   Fobstr=fltarr(NC, NR)
   Fobstr(*,*)=0.0      

   NRmin = MIN([NR, NRobs]) 
   NCmin = MIN([NC, NCobs]) 
   Print, "NRmin, NCmin=", NRmin, NCmin
 FOR j=0, NRmin -1 DO BEGIN
 FOR i=0, NCmin -1 DO BEGIN
     Fobstr(i,j)=Fobsin(i,j)
 ENDFOR
 ENDFOR

;; Due to filling of isolated sea points, obstruction for these filled points
;; need to be redefined as land (100).
   Fobstr(lands) = 1.0

;; WW3 read in obstruction rather transparency so 1.0 mean complete blocking!
;; The value will be from 0.0 for transparent sea point to 1.0 for full land 
;; blocking with a scaling factor 1.0 at the input line.   JGLi  26 Nov 2009
;; The obstruction will be equal in both x and y direction.
 OPENW, 8, "G50SMCSubtr.dat"
 FOR j=0, NR-1 DO BEGIN
    PRINTF, 8, Fobstr(*,j), format='((20F6.2))'
 ENDFOR
 FOR j=0, NR-1 DO BEGIN
    PRINTF, 8, Fobstr(*,j), format='((20F6.2))'
 ENDFOR
 CLOSE, 8

;; Grid mask is set to 0 for all land points, 
;;                     1 for sea points
;;                     2 for active boundary points
;;                     3 for excluded boundary points
 mask=fix(Depth)
 mask(where(mask GE 0))=1
 mask(where(mask LT 0))=0
;;  Set west and east boundaries land points for NAEW   26 Nov 2009
;mask(   0,*)=2
;mask(NC-1,*)=2
;;  Set south and north boundaries land points   26 Nov 2009
;mask(*,   0)=0
;mask(*,NR-1)=0
;;  No longer needs boundary points for GSMC.  15 Apr 2011

 OPENW, 8, "G50SMCMasks.dat"
 FOR j=0, NR-1 DO BEGIN
    PRINTF, 8, mask(*,j), format='((25I3))'
 ENDFOR
 CLOSE, 8

 PRINT,  " *** G50SMCDepth completed. *** "
 END


