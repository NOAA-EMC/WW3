;;;  steromap procedure converts standard latitude longitude to rotated 
;;;  or equatorial latitude longitude with a given new pole position and
;;;  map the rotated lat/lon with a sterographic projection.
;;;  Created on  14 May 2014  by Jian-Guo Li
;+
; name:     steromap
;
; purpose:  Converts standard lat/lon to rotated lat/lon and sterographic map
;
; usage:    STEROMAP, SLat, SLon, ELat, ELon, SXxc, SYyc, Pangl=Pangl, Polat=Polat, Polon=Polon, Onecl=Onecl
;
; input:    SLat, SLon --- Standard lat lon (scalar or vector) in deg
;           Polat, Polon --- New North Pole position in standard lat/lon in deg
;           Pangl --- angle deg from rotated Pole so its projected radius is 10 unit.
;           Onecl --- if defined 1, keep all points in one hemisphere as one cell. 
; output:   ELat, ELon --- Corresponding lat lon in rotated grid in deg
;           SXxc, SYyc --- Corresponding projected x/y coordinates. 
;
; history:  14 May 2014  First created by Jian-Guo Li
;           11 Nov 2014  Last modified by Jian-Guo Li
;-

PRO  STEROMAP, SLat, SLon, ELat, ELon, SXxc, SYyc, Polat=Polat, Polon=Polon, Pangl=Pangl, Onecl=Onecl
 
;; Check Input SLat, Slon elements, should be equal
   nlat=N_ELEMENTS(SLat)
   nlon=N_ELEMENTS(SLon)

   IF( (nlat NE nlon) ) THEN BEGIN
      PRINT, ' SLat and SLon elements should have equal elements!'
      PRINT, ' SLat and SLon elements are', nlat, nlat
      RETURN
   ENDIF

;; Default settings if not provided by Keywords
  IF( NOT  KEYWORD_SET(Polat)   ) THEN Polat=90.0
  IF( NOT  KEYWORD_SET(Polon)   ) THEN Polon= 0.0
  IF( NOT  KEYWORD_SET(Pangl)   ) THEN Pangl=90.0
  IF( NOT  KEYWORD_SET(Onecl)   ) THEN Onecl= 0

;; No need to calculate if North Pole unchanged
   IF( Polat EQ 90.0 AND Polon EQ 0.0 ) THEN BEGIN
       Elat = SLat
       ELon = SLon
   ENDIF ELSE BEGIN
;; Convert slat slon to elat elon with given new pole
       LL2EqDeg, slat, slon, Elat, Elon, PoLat=Polat, PoLon=Polon
   ENDELSE

;; Projection parameters
;; Constants 
   d2rad=!Pi/180.0
   r2deg=180.0/!Pi
  radius=10.0

;; Number of radius of projection distance
    np=4.0

;; Adjusted projected radius so that projected edge radius 
;; to be equal to the original radius.
  PRadus=radius*(np - 1.0 + cos(Pangl*d2rad))/sin(Pangl*d2rad)
;; Note the np factor has been included in PRadus.

;; Generate projecting coordiantes
  pradmp=PRadus*cos(Elat*d2rad)/(np - 1.0 + sin(Elat*d2rad))
     SYyc = pradmp*cos(Elon*d2rad)
     SXxc =-pradmp*sin(Elon*d2rad)

;; If it is for one single cell projection, keep all in one hemisphere
   IF( Onecl EQ 1  AND  Elat(0) LT 0.0 ) THEN Begin
  pradmp=PRadus*cos(Elat*d2rad)/(np - 1.0 - sin(Elat*d2rad))
     SYyc = pradmp*cos(Elon*d2rad)
     SXxc = pradmp*sin(Elon*d2rad)
   ENDIF

;; Reverse southern hemisphere x-coordinate for individual points.
   indx=where( Elat LT 0.0, mcount)
   IF( Onecl EQ 0  AND mcount GT 0 ) then BEGIN
     pradmp(indx)=PRadus*cos(Elat(indx)*d2rad)/(np - 1.0 - sin(Elat(indx)*d2rad))
     SYyc(indx) = pradmp(indx)*cos(Elon(indx)*d2rad)
     SXxc(indx) = pradmp(indx)*sin(Elon(indx)*d2rad)
   ENDIF

;; print,'... Finishing steromap.pro'

RETURN

END

