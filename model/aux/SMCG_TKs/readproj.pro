;Li
;Li  First created:   Jian-Guo Li     11 Nov 2014
;Li  Last modified:   Jian-GUo Li     16 Jun 2015
;Li
;Li  Procedure READPROJ ---------------------------------------------
;Li
;Li  Purpose:  Read SMC grid projection arrays from given array files.  
;Li            Arctic part is already included in the projection arrays.
;Li            Returned variables are the full list of projection arrays, 
;Li            plus the NC cell number.
;Li
;Li  Arguments:--------------------------------------------------------
;Li  ELL_File	Equatorial lat/lon for all cells (SW corner).
;Li  SpX_File	Sterographic projection X (5 corners as a ring).
;Li  SpY_File	Sterographic projection Y (5 corners as a ring).
;Li  elalon(2,NC) returned cell SW corner rotated lat/lon values.  
;Li  sxc(5,NC+1)  returned cell x coordinates (5 corners). 
;Li  syc(5,NC+1)  returned cell y coordinates (5 corners). 
;Li  NC  	returned number of cells (global and Arctic if any)  
;Li

PRO  READPROJ, ELL_File, SpX_File, SpY_File, Ncc, Elalon, Sxc, Syc, Arctic=Arctic

;; Cell file path has to be none zero. Otherwise abort.
    IF( STRLEN(ELL_File) LT 3 OR STRLEN(SpX_File) LT 3 OR  $
        STRLEN(SpY_File) LT 3 ) THEN RETURN
   
;; Arctic part is not included if not provided by Keyword.
;   IF( NOT  ARG_PRESENT(Arctic) ) THEN  Arctic = 0
    IF( NOT  KEYWORD_SET(Arctic) ) THEN  Arctic = 0
    Print, 'Arctic set to be ', Arctic

;; Open elaton array file
    OPENR, unell, ELL_File, /Get_Lun

;; Initial cell number varaibles
    ncl=0L
    n2=0

;; Read cell number from first line
    READF, unell, ncl, n2

;;  Pass total cell number back
    Ncc = ncl
    Print, "ncc =", ncc

;; Declare elalon array and read from file
    elalon=fltarr(n2,ncl)
    READF, unell, elalon
    FREE_LUN, unell

    Print, ELL_File+' read done'
    Print, elalon(*,0)
    Print, elalon(*,ncl-1)


;; Open SpX array file
    OPENR, unspx, SpX_File, /Get_Lun

;; Initial cell number varaibles
    ncx=0L
    n5=0

;; Read cell number from first line
    READF, unspx, ncx, n5
    Print, "ncx =", ncx

;; Declare elalon array and read from file
    sxc=fltarr(n5,ncx+Arctic)
    READF, unspx, sxc
    FREE_LUN, unspx

    Print, SpX_File+' read done'
    Print, sxc(*,0)
    Print, sxc(*,ncx-1)

;; Open SpY array file
    OPENR, unspy, SpY_File, /Get_Lun

;; Initial cell number varaibles
    ncy=0L
    n5=0

;; Read cell number from first line
    READF, unspy, ncy, n5
    Print, "ncy =", ncy

;; Declare elalon array and read from file
    syc=fltarr(n5,ncy+Arctic)
    READF, unspx, syc
    FREE_LUN, unspy

    Print, SpY_File+' read done'
    Print, syc(*,0)
    Print, syc(*,ncy-1)

;; Check whether ncl ncx and ncy are identical
 if( ncl ne ncx or ncl ne ncy ) then begin
     print, ' *** Warning ncl ncx ncy not matching', ncl, ncx, ncy
 endif


;; print,'... Finishing READPROJ'

RETURN

END

