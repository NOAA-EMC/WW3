;Li
;Li  First created:   Jian-Guo Li     23 Apr 2014
;Li  Last modified:   Jian-GUo Li     26 May 2015
;Li
;Li  Procedure READCELL ---------------------------------------------
;Li
;Li  Purpose:  Read SMC grid cells from given cell array files.  
;Li            Arctic part is appended if Arctic file is provided.
;Li            Returned variables are the full list of cel array, 
;Li            plus the NC and NArc cell numbers.
;Li
;Li  Arguments:--------------------------------------------------------
;Li  Cel_File	path of cell array file (string variable)
;Li  Arc_File	path of Arctic cell file (string variable)
;Li  Cel(5,NC)  returned cell array (integer array)
;Li  NC  	returned number of cells (global and Arctic if any)  
;Li  NArc	returned number of Arctic cells (default 0 if no Arctic part)  
;Li  NBAr	returned number of boundary cells (default 0 if no Arctic part)  
;Li

PRO  READCELL, Cel_File, Cel, NC, ArcFile = Arc_File, NArc = NArc, NBAr = NBAr

;; Cell file path has to be none zero. Otherwise abort.
    IF( STRLEN(Cel_File) LT 3 ) THEN RETURN
   
;; Default Arctic part setting if not provided by Keywords
; IF( NOT  PARAM_PRESENT(Arc_File) ) THEN Begin
  IF( NOT  KEYWORD_SET(Arc_File) ) THEN Begin
           Arc_File=' '
           NArc = 0L
           NBAr = 0L
  ENDIF

;; Open cell array file
    OPENR, ungl, Cel_File, /Get_Lun

;; Initial cell number varaibles
    ng=0L

;; Read cell number from first line
    READF, ungl, ng
    Print, "ng =", ng

;; Declare cel array and read from file
    cel=intarr(5,ng)
    READF, ungl, cel
    FREE_LUN, ungl

    Print, Cel_File+' read done'
    Print, cel(*,0)
    Print, cel(*,ng-1)

;;  Pass total cell number back
    NC = ng

;;  Read Arctic file if CArc_File string is > 3
    IF( STRLEN(Arc_File) GT 3 ) THEN Begin

;; Open cell array file
        OPENR, unar, Arc_File, /Get_Lun

;; Initial cell number varaibles
        na=0L
        nb=0L
        n9=0L

;; Read cell number from first line
        READF, unar, na, nb, n9
        Print, "na nb n9 =", na, nb, n9

;; Declare cel array and read from file
        ael=intarr(5,na)
        READF, unar, ael
        FREE_LUN, unar

        Print, Arc_File+' read done'
        Print, ael(*,0)
        Print, ael(*,na-1)

;; Merge with global part and update cell numbers
        cel=transpose([transpose(cel), transpose(ael)])
        NC = ng + na
        NArc = na
        NBAr = nb + n9

;; End read Arctic cells.
    ENDIF

;; print,'... Finishing READCELL.pro'

RETURN

END

