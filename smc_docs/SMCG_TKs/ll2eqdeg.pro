;;;  ll2eqdeg procedure converts standard latitude longitude to rotated 
;;;  or equatorial latitude longitude with a given new pole position.
;;;  Created on  6 Dec 2007  by Jian-Guo Li
;;;  $Id: ll2eqdeg.pro, v 1.0 2007/12/06 $
;+
; name:     LL2EQDEG
;
; purpose:  Converts standard lat/lon to rotated lat/lon
;
; usage:    LL2EQDEG, SLat, SLon, ELat, ELon, Polat=Polat, Polon=Polon
;
; input:    SLat, SLon --- Standard lat lon (scalar or vector) in deg
;           Polat, Polon --- New North Pole position in standard lat/lon in deg
; output:   ELat, ELon --- Corresponding lat lon in rotated grid in deg
;
; history:  06 Dec 2007  First created by Jian-Guo Li
;-

PRO  LL2EQDEG, SLat, SLon, ELat, ELon, Polat=Polat, Polon=Polon
 
;; Check Input SLat, Slon elements, should be equal
   nlat=N_ELEMENTS(SLat)
   nlon=N_ELEMENTS(SLon)

   IF( (nlat NE nlon) ) THEN BEGIN
      PRINT, ' SLat and SLon elements should have equal elements!'
      PRINT, ' SLat and SLon elements are', nlat, nlat
      RETURN
   ENDIF

;; Default settings if not provided by Keywords
  IF( NOT  PARAM_PRESENT(Polat) ) THEN Polat=90.0
  IF( NOT  PARAM_PRESENT(Polon) ) THEN Polon= 0.0
; IF( NOT  KEYWORD_SET(ssize)   ) THEN ssize=0.8
; IF( NOT  KEYWORD_SET(tsize)   ) THEN tsize=1.6

;; No need to calculate if North Pole unchanged
   IF( Polat EQ 90.0 AND Polon EQ 0.0 ) THEN BEGIN
       Elat = SLat
       ELon = SLon
;; Return from here
       RETURN
   ENDIF

;; Constants 
   Pie=!Pi
   D2Rad=Pie/180.0
   R2Deg=180.0/Pie

;; Make Pole longitude within range -180 to 180
   IF( Polon GT 180.0 ) THEN Polon=Polon - 360.0

;; Sine and cosine of PoLat
   IF( Polat GE 0.0 ) THEN Begin
     sinpolat= sin(Polat*D2Rad)
     cospolat= cos(Polat*D2Rad)
   ENDIF
   IF( Polat LT 0.0 ) THEN Begin
     sinpolat=-sin(Polat*D2Rad)
     cospolat=-cos(Polat*D2Rad)
   ENDIF

;; Conversion of SLat/SLon to ELat/ELon
   ZeroLon = Polon + 180.0
   ALon = SLon - ZeroLon

   lonind=where(ALon GT  180.0, loncnt)
   IF(loncnt GT 0) THEN ALon(lonind)=ALon(lonind)-360.0
   lonind=where(ALon LE -180.0, loncnt)
   IF(loncnt GT 0) THEN ALon(lonind)=ALon(lonind)+360.0

   Apt_Ang = - cospolat*cos(SLat*D2Rad)*cos(ALon*D2Rad) $
             + sinpolat*sin(SLat*D2Rad)
   lonind=where(Apt_Ang GT  1.0, loncnt)
   IF(loncnt GT 0) THEN Apt_Ang(lonind)= 1.0
   lonind=where(Apt_Ang LE -1.0, loncnt)
   IF(loncnt GT 0) THEN Apt_Ang(lonind)=-1.0

   ELatRad = ASIN(Apt_Ang)
   cosElat = cos(ElatRad)

;  sinElon = sin(ALon*D2Rad)*cos(SLat*D2Rad)
   cosElon = sinpolat*cos(SLat*D2Rad)*cos(ALon*D2Rad) $
            +cospolat*sin(SLat*D2Rad)

   ELat = ELatRad*R2Deg
   ELon = ELat*0.0

;; Only set Elon where cosELat is non zero
   latind = where(cosElat GT 0.0, latcnt)
   IF(latcnt GT 0) THEN Begin
      Tmprat=cosElon(latind)/cosElat(latind)

      lonind=where(Tmprat GT  1.0, loncnt)
      IF(loncnt GT 0) THEN Tmprat(lonind)= 1.0
      lonind=where(Tmprat LE -1.0, loncnt)
      IF(loncnt GT 0) THEN Tmprat(lonind)=-1.0

      ELon(latind)= R2Deg*ACOS(Tmprat)
   ENDIF

;; A possible bug to use SGN(x) when x=0 as SGN(0.0)=0
;; Here only when ALon is negative, pass the sign to ELon
;; Fortran SIGN(x) function return 1 for x=0.0
   lonind=where(ALon LT  0.0, loncnt)
   IF(loncnt GT 0) THEN ELon(lonind)= - ELon(lonind)

;; print,'... Finishing LL2Eqdeg.pro'

RETURN

END

