; IDL program to generate SMC 50km global grid. 
; Read bathymetry ASCII file and output cell array. 
; For WW3 users outside Met Office.  JGLi15Nov2013
; Add single sea point filling.   JGLi20Oct2014
;
 
;; Open a message file to store SMC grid cell array 
   OPENW, 8, "./G50SMCelsA.dat"

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

;; Workout zero lat/lon
   NR=NRow 
   NC=NCol 
   zlat=FLat - DLat
   zlon=FLon - DLon 
   PRINT,    "Basic 50km NR, NC=", NR, NC
   PRINT,    "Zlon, Dlon, Zlat, Dlat=", zlon, dlon, zlat, dlat


;; Add island Andros as two 50km cells at 78W 24.3N
   iandr = FIX( (282.0 - FLon)/DLon ) 
   jandr = FIX( ( 24.0 - FLat)/DLat ) 
   Print, 'Island Andros i, j=', iandr, jandr, Bathy(iandr, jandr)
   Bathy(iandr, jandr) = 10.0 
   Print, 'Island Andros i j+=', iandr, jandr+1, Bathy(iandr, jandr+1)
   Bathy(iandr, jandr+1) = 10.0 
;; Add Grand Bahama island as single cell at 78.0W 26.0N
   iandr = FIX( (282.0 - FLon)/DLon ) 
   jandr = FIX( ( 26.0 - FLat)/DLat ) 
   Print, 'Grand Bahama  i, j=', iandr, jandr, Bathy(iandr, jandr)
   Bathy(iandr, jandr) = 10.0 
;; These 3 islands are added to control high wave energy. JGLi20Nov2013


;; Filling isolated sea points before generating SMC cells
   Print, "Filling isolated sea points before generating SMC cells ..."
   BATHYFILL, Bathy, NC, NR, MinPnt=6, LndSgn=-1, Global=1

 
;; Check whether N Pole is included
   Arctic = 0
   fnlat=FLat+FLOAT(NR)*DLat
   IF( fnlat GT 90.0-DLat ) THEN  Arctic = 1


;; Cell array output format
;  oldformat='(2i6,2i4,i6)' 
   celformat='(2i6,i5,i4,i6)' 

;;  Initialise cell count variables
   N9=0L
   N8=0L
   N4=0L
   N2=0L
   N1=0L
   NL=0L

;; Set cell SW corner j count = 0 on Equator, i = 0 at zero meridian
   j0 = NINT( -0.5 - zlat/dlat ) 
   Print, ' j0 =', j0

;; Loop over each row in 50km data set 
     NRND = NR - 1
;; Except for last row as polar cell 
     IF( Arctic GT 0 ) THEN NRND = NR - 2
  
     FOR j=0, NRND DO BEGIN
         jj=j - j0
         yj=zlat+ Float(j+1)*dlat

         IF( Abs(yj) GE 89.5 )  THEN Begin
;; Replace 50km data with 3200km data for every 128x2 grids
         FOR i=0, NC-127, 128 DO BEGIN
                 dsum = 0.0
                 ksum = 0
             FOR k = 0, 127 DO BEGIN
                 IF( Bathy(i+k,j) LT 0.0 ) THEN BEGIN
                 dsum = dsum + Bathy(i+k,j)
                 ksum = ksum + 1
                 ENDIF
             ENDFOR
             IF( ksum GE 64) THEN Begin
                kdepth = NINT( -dsum/FLOAT(ksum) )
                Printf,8, i, jj, 128, 1, max([5,kdepth]), Format=celformat 
                NL = NL + 1
                N9 = N9 + 1
                Print,  ' Size 128 cells for j, jj, yj=', j, jj, yj
             ENDIF
         ENDFOR
             LastSize=128
         ENDIF 

;; Replace 50km data with 1600km data for every 64x2 grids
         IF( Abs(yj) GE 89.1  AND  Abs(yj) LT 89.5 )  THEN Begin
         FOR i=0, NC-63, 64 DO BEGIN
                 dsum = 0.0
                 ksum = 0
             FOR k = 0, 31 DO BEGIN
                 IF( Bathy(i+k,j) LT 0.0 ) THEN BEGIN
                 dsum = dsum + Bathy(i+k,j)
                 ksum = ksum + 1
                 ENDIF
             ENDFOR
             IF( ksum GE 32 ) THEN Begin
                kdepth = NINT( -dsum/FLOAT(ksum) )
                Printf,8, i, jj, 64, 1, max([5,kdepth]), Format=celformat 
                NL = NL + 1
                N9 = N9 + 1
                Print, ' Size 64 cells for j, jj, yj=', j, jj, yj
             ENDIF
         ENDFOR
             LastSize=64
         ENDIF

         IF( Abs(yj) GE 88.2  AND  Abs(yj) LT 89.1 )  THEN Begin
;; Replace 50km data with 800km data for every 32x2 grids
         FOR i=0, NC-31, 32 DO BEGIN
                 dsum = 0.0
                 ksum = 0
             FOR k = 0, 31 DO BEGIN
                 IF( Bathy(i+k,j) LT 0.0 ) THEN BEGIN
                 dsum = dsum + Bathy(i+k,j)
                 ksum = ksum + 1
                 ENDIF
             ENDFOR
             IF( ksum GE 16 ) THEN Begin
                kdepth = NINT( -dsum/FLOAT(ksum) )
                Printf,8, i, jj, 32, 1, max([5,kdepth]), Format=celformat 
                NL = NL + 1
                N9 = N9 + 1
         Print,    ' Size 32 cells for j, jj, yj=', j, jj, yj
             ENDIF
         ENDFOR
         ENDIF

         IF( Abs(yj) GE 86.4  AND  Abs(yj) LT 88.2 )  THEN Begin
;; Replace 50km data with 400km data for every 16x2 grids
         FOR i=0, NC-15, 16 DO BEGIN
                 dsum = 0.0
                 ksum = 0
             FOR k = 0, 15 DO BEGIN
                 IF( Bathy(i+k,j) LT 0.0 ) THEN BEGIN
                 dsum = dsum + Bathy(i+k,j)
                 ksum = ksum + 1
                 ENDIF
             ENDFOR
             IF( ksum GE 8 ) THEN Begin
                kdepth = NINT( -dsum/FLOAT(ksum) )
                Printf,8, i, jj, 16, 1, max([5,kdepth]), Format=celformat 
                NL = NL + 1
                N9 = N9 + 1
                IF(N9 LT 3) THEN Print, ' Size 16 cells for j, jj, yj=', j, jj, yj 
             ENDIF
         ENDFOR
         ENDIF


         IF( Abs(yj) GE 82.8 AND Abs(yj) LT 86.4 )  THEN Begin
;; Replace 50km data with 200km data for every 8x2 grids
         FOR i=0, NC-7, 8 DO BEGIN
                 dsum = 0.0
                 ksum = 0
             FOR k = 0, 7 DO BEGIN
                 IF( Bathy(i+k,j) LT 0.0 ) THEN BEGIN
                 dsum = dsum + Bathy(i+k,j)
                 ksum = ksum + 1
                 ENDIF
             ENDFOR
             IF( ksum GE 4 ) THEN Begin
                kdepth = NINT( -dsum/FLOAT(ksum) )
                Printf,8, i, jj, 8, 1, max([5,kdepth]), Format=celformat 
                NL = NL + 1
                N8 = N8 + 1
                IF(N8 LT 3) THEN Print, ' Size 8 cells for j, jj, yj=', j, jj, yj
             ENDIF
         ENDFOR
         ENDIF

         IF( Abs(yj) GE 75.5 AND Abs(yj) LT 82.8 )  THEN Begin
;; Replace 50km data with 200km data for every 4x2 cells
         FOR i=0, NC-3, 4 DO BEGIN
                 dsum = 0.0
                 ksum = 0
             FOR k = 0, 3 DO BEGIN
                 IF( Bathy(i+k,j) LT 0.0 ) THEN BEGIN
                 dsum = dsum + Bathy(i+k,j)
                 ksum = ksum + 1
                 ENDIF
             ENDFOR
             IF( ksum GE 2 ) THEN Begin
                kdepth = NINT( -dsum/FLOAT(ksum) )
                Printf,8, i, jj, 4, 1, max([5,kdepth]), Format=celformat  
                NL = NL + 1
                N4 = N4 + 1
                IF(N4 LT 3) THEN Print, ' Size 4 cells for j, jj, yj=', j, jj, yj 
             ENDIF
         ENDFOR
         ENDIF 

         IF( Abs(yj) GE 60.0 AND Abs(yj) LT 75.5 )  THEN Begin
;; Replace 50km data with 100 km data for every 2 grids
             FOR i=0, NC-1, 2 DO BEGIN
                 dsum = 0.0
                 ksum = 0
               FOR k = 0, 1 DO BEGIN
                 IF( Bathy(i+k,j) LT 0.0 ) THEN BEGIN
                 dsum = dsum + Bathy(i+k,j)
                 ksum = ksum + 1
                 ENDIF
               ENDFOR
               IF( ksum GE 1 ) THEN Begin
                 kdepth = NINT( -dsum/FLOAT(ksum) ) 
                 Printf,8, i, jj, 2, 1, max([5,kdepth]), Format=celformat 
                 NL = NL + 1
                 N2 = N2 + 1
                 IF(N2 LT 3) THEN Print, ' Size 2 cells for j, jj, yj=', j, jj, yj 
               ENDIF
             ENDFOR
         ENDIF


         IF( Abs(yj) LT 60.0 )  THEN Begin
;; Use 50km data for size 1 cells 
           FOR i=0, NC-1  DO BEGIN
                depth=fix(Bathy(i,j))
             IF( depth LT 0 ) THEN Begin
                Printf,8, i, jj, 1, 1, max([5,-depth]), Format=celformat 
                NL = NL + 1
                N1 = N1 + 1
                IF(N1 LT 3) THEN Print, ' Size 1 cells for j, jj, yj=', j, jj, yj 
             ENDIF
           ENDFOR
         ENDIF

;; End of j=0, NR-2 loop
     ENDFOR

;; Final polar cell
     IF ( Arctic GT 0 ) THEN Begin
         jj=jj+1
         j = NR-1
         Print,    ' Polar cell for j, jj, size, yj=', j, jj, LastSize, yj
         dsum = 0.0
         ksum = 0
         FOR i = 0, NC-1 DO BEGIN
             IF( Bathy(i,j) LT 0.0 ) THEN BEGIN
                 dsum = dsum + Bathy(i,j)
                 ksum = ksum + 1
             ENDIF
         ENDFOR
             kdepth = NINT( -dsum/FLOAT(ksum) ) 
             Printf,8, 0, jj, LastSize, 1, max([5,kdepth]), Format=celformat 
             NL = NL + 1
             N9 = N9 + 1
     ENDIF 

     PRINT,    " ********* Done all cells NL N9 N8 N4 =", NL, N9, N8, N4 
;              Printf,8,  NL, N9, N8, N4, N2, N1,  Format='(6i8)'
               Print,     NL, N9, N8, N4, N2, N1,  Format='(6i8)'
  
;; Close the SMC cell array for the global 50km model part 
   CLOSE, 8

 END

