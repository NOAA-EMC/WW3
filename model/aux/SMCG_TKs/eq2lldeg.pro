;Li
;Li  First created:   Jian-Guo Li     26 May 2005
;Li  Last modified:   Jian-GUo Li     10 Nov 2009
;Li
;LL  Procedure EQ2LLDEG ---------------------------------------------
;LL
;LL  Purpose:  Calculates latitude and longitude on standard grid
;LL            from input arrays of latitude and longitude on
;LL            equatorial latitude-longitude (eq) grid used
;LL            in regional models. Both input and output latitudes
;LL            and longitudes are in degrees.
;Li	       Also calculate rotation angle in degree to tranform
;Li            standard wind velocity into equatorial wind.
;Li	       Valid for 0<PHI_POLE<90 or new pole in N. hemisphere.
;LL
;LL  Arguments:--------------------------------------------------------

PRO  EQ2LLDEG, SLat, SLon, ELat, ELon, Polat=Polat, Polon=Polon

;; Check Input ELat, Elon elements, should be equal
   nlat=N_ELEMENTS(ELat)
   nlon=N_ELEMENTS(ELon)

   IF( (nlat NE nlon) ) THEN BEGIN
      PRINT, ' ELat and ELon elements should have equal elements!'
      PRINT, ' ELat and ELon elements are', nlat, nlat
      RETURN
   ENDIF

;; Default settings if not provided by Keywords
  IF( NOT  PARAM_PRESENT(Polat) ) THEN Polat=90.0
  IF( NOT  PARAM_PRESENT(Polon) ) THEN Polon= 0.0

;; No need to calculate if North Pole unchanged
   IF( Polat EQ 90.0 AND Polon EQ 0.0 ) THEN BEGIN
       Slat = ELat
       SLon = ELon
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
   ALon = ELon 

   lonind=where(ALon GT  180.0, loncnt)
   IF(loncnt GT 0) THEN ALon(lonind)=ALon(lonind)-360.0
   lonind=where(ALon LE -180.0, loncnt)
   IF(loncnt GT 0) THEN ALon(lonind)=ALon(lonind)+360.0

   Apt_Ang = cospolat*cos(ELat*D2Rad)*cos(ALon*D2Rad) $
           + sinpolat*sin(ELat*D2Rad)
   lonind=where(Apt_Ang GT  1.0, loncnt)
   IF(loncnt GT 0) THEN Apt_Ang(lonind)= 1.0
   lonind=where(Apt_Ang LE -1.0, loncnt)
   IF(loncnt GT 0) THEN Apt_Ang(lonind)=-1.0

   ELatRad = ASIN(Apt_Ang)
   cosElat = cos(ElatRad)

   cosElon = sinpolat*cos(ELat*D2Rad)*cos(ALon*D2Rad) $
            -cospolat*sin(ELat*D2Rad)

   SLat = ELatRad*R2Deg
   SLon = SLat*0.0

;; Only set Elon where cosELat is non zero
   latind = where(cosElat GT 0.0, latcnt)
   IF(latcnt GT 0) THEN Begin
      Tmprat=cosElon(latind)/cosElat(latind)

      lonind=where(Tmprat GT  1.0, loncnt)
      IF(loncnt GT 0) THEN Tmprat(lonind)= 1.0
      lonind=where(Tmprat LE -1.0, loncnt)
      IF(loncnt GT 0) THEN Tmprat(lonind)=-1.0

      SLon(latind)= R2Deg*ACOS(Tmprat)
   ENDIF

;; A possible bug to use SGN(x) when x=0 as SGN(0.0)=0
;; Here only when ALon is negative, pass the sign to ELon
;; Fortran SIGN(x) function return 1 for x=0.0
   lonind=where(ALon LT  0.0, loncnt)
   IF(loncnt GT 0) THEN SLon(lonind)= - SLon(lonind)

;; Add zero-lon and stream back range

   SLon = SLon + ZeroLon
   lonind=where(SLon GT  360.0, loncnt)
   IF(loncnt GT 0) THEN SLon(lonind)=SLon(lonind)-360.0
   lonind=where(ALon LT   -0.0, loncnt)
   IF(loncnt GT 0) THEN SLon(lonind)=SLon(lonind)+360.0

;; print,'... Finishing Eq2LL2Deg.pro'

RETURN

END

