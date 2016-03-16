; WAVE Version 8.00 (Linux i386)
; Journal File for frjl@eld330
; Working directory: /net/data/cr2/frjl/Bathy
; Date: Fri Sep 16 09:55:18 2005
; To plot NAEbathy.pp as a contour gif file
; First created:   16 Sep 2005   Jian-Guo Li
; To smooth out land/sea for the final bathymetry
; Prepare mask, depth and trans files for GSMC model. 
; Use multi-resolution cell to modify base grid depth.
; Last modified:     8 Jan 2013   Jian-Guo Li
; SMC6125 grid with refined UK waters.  JGLi28Feb2013
; SMC36125 grid with refined 3km UK waters.  JGLi28Feb2014
; SMC24816 grid with refined 2km UK waters.  JGLi05Sep2014
; SMC36125 grid with 3km European waters.    JGLi19Sep2014
; Generate sub-grid obstruction for SMC36125 grid.  JGLi14Oct2014
; Adapted for G50SMC grid obstruction ratio.   JGLi20Oct2014
;
 
;; Open a message file to save parameters for ww3_grid.inp 
 OPENW,  11, "SMC50Regul.txt"
 Printf, 11, " Regular 50km domain parameters for SMC50km grid "

;; Set regular grid domain
  NRF=768/2
  NC=1024/2

  DLATM=180.0/FLOAT(NRF) 
  DLNGM=360.0/FLOAT(NC) 

;; Find new NR to skip Antarctic (>80S) but cover Arctic 
;  NAS= FIX( (90.0 - 80.0)/(DLATM*16.0) )*16
   NAS=16 
   NR= NRF - NAS
   Y0Lat=-90.0 - 0.5*DLATM + FLOAT(NAS)*DLATM
   X0Lon=  0.0 - 0.5*DLNGM 

   Print, "Skip rows ", NAS  
   PRINT, "NC, NR=", NC, NR, NRF
   PRINT, "DX, DY=", DLNGM, DLATM
   PRINT, "X0, Y0=", X0Lon, Y0Lat
   PRINT, "X1, Y1=", X0Lon+DLNGM, Y0Lat+DLATM
   PRINT, "XN, YN=", X0Lon+DLNGM*NC, Y0Lat+DLATM*NR

;; Saved parameters
   PRINTF, 11, " NC, NR=", NC, NR
   PRINTF, 11, " DX, DY=", DLNGM, DLATM
   PRINTF, 11, " X1, Y1=", X0Lon+DLNGM, Y0Lat+DLATM
   PRINTF, 11, " XN, YN=", X0Lon+DLNGM*NC, Y0Lat+DLATM*NR


;; Read cell arrays by a procedure

 Cel_file = './G50SMCels.dat'
;Arc_file = './G50SMCBAr.dat'
;READCELL, Cel_File, cel, nc, ArcFile = Arc_file, NArc = na, NBAr = nb
 READCELL, Cel_File, cel, nc

;; Maximum j row number in Global part
;ng = nc - na
 ng = nc 
 jmxglb = cel(1,ng-1L)
 Print, ' Maximum j row =', jmxglb

;; Multi-resolution levels and factor
   MRL=1
   MFct=2^(MRL-1)
   Print, "Multi-Resol MRL, MFct=", MRL, MFct 

;; Work out Equator index for G50 model row
   EqtDlt= 0.0 - Y0Lat - 0.5*DLATM
   NEqutr= FIX( EqtDlt/DLATM + 0.001 )*MFct
   Print, ' Equator index NEqutr =', NEqutr

;; Saved parameters
   Printf, 11, ' Levels and j-shift', MRL, NEqutr

;; Test mod function
   Print, ' 6 mod 10 =',  6 mod 10
   Print, '16 mod 10 =', 16 mod 10

;; Test max and min functions
   Print, 'min([0.5,1.0])=', min([0.5,1.0])
   Print, 'max([0.5,1.0])=', max([0.5,1.0])

;; Read in land percentage data from G50kmObstr.dat. JGLi20Oct2014
  Print, " Read G50kmObstr.dat ..." 
  OPENR, 9, "G50kmObstr.dat"
  Messg='Header Text'
;; Skip header line
  READF, 9, Messg 
  PRINT,    Messg 

;; Declare header variables to store data
   NCobs=0L
   NRobs=0L
   FLonb=0.0
   FLatb=0.0
   DLonb=0.0
   DLatb=0.0

   READF, 9, NCobs, NRobs, FLonb, FLatb, DLonb, DLatb
   PRINT,    NCobs, NRobs, FLonb, FLatb, DLonb, DLatb

   Fobsin=fltarr(NCobs, NRobs)
   READF, 9, Fobsin
   CLOSE, 9

;; Declare global part cell obstruction array, excluding Arctic part.
;; Arctic part is set no sub-grid obstruction.
   Kobstr=INTARR(ng)

;; Create sub-grid obstruction ratio for all cells except for size-1 cells
;; Set size-1 cell obstruction to be 0

   FOR n=0L, ng-1L do begin
;; Excluding Arctic part 
;; All refined cells are rounded to base-level size MFct.
       i=FIX(Cel(0,n)/MFct)
       j=FIX(Cel(1,n)/MFct) + NEqutr
       IF( j GE NRobs OR j LT 0 ) THEN Print, 'n, j=', n, j
      mi=FIX(Cel(2,n)/MFct)
      nj=FIX(Cel(3,n)/MFct)

;; No obstruction for all size-1 cells 
      IF(nj EQ 0 OR mi EQ 0) THEN  Begin
        Kobstr(n) = 0
      ENDIF ELSE BEGIN
;; Loop over merged cells if any
        avrobs = 0.0
        for ii=i, i+mi - 1 do BEGIN
        for jj=j, j+nj - 1 do BEGIN
            avrobs = avrobs + Fobsin(ii, jj)
        endfor
        endfor
        Kobstr(n) = MIN( [99, FIX( 100.0*avrobs/FLOAT(nj*mi) )] )
      ENDELSE
   ENDFOR

;; WW3 read in obstruction rather transparency so 1.0 mean complete blocking!
;; The value will be from 0.0 for transparent sea point to 1.0 for full land 
;; blocking with a scaling factor 1.0 at the input line.   JGLi  26 Nov 2009
;; The obstruction will be equal in both x and y direction.
   Obstfile='G50GObstr.dat'
   OPENW, 12, Obstfile 
   Print, " Write "+Obstfile+" ..." 
      PRINTF, 12, ng,  1, format='((2I8))'
   FOR n=0L, ng-1L DO BEGIN
      PRINTF, 12, Kobstr(n), format='((20I4))'
   ENDFOR
   CLOSE, 12

   Printf, 11, " Subgrid obstruction in "+Obstfile 
   CLOSE, 11

;; Grid mask is set to 0 for all land points, 
;;                     1 for sea points
;;                     2 for active boundary points
;;                     3 for excluded boundary points

   Print, " All done! " 

 END


