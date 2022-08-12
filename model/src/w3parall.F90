MODULE W3PARALL
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |                                   |
  !/                  | Aron Roland (BGS IT&E GmbH)       |
  !/                  | Mathieu Dutour-Sikiric (IRB)      |
  !/                  |                                   |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :        01-June-2018 |
  !/                  +-----------------------------------+
  !/
  !/   01-June-2018 : Origination.                        ( version 6.04 )
  !/
  !  1. Purpose : Parallel routines for implicit solver
  !  2. Method :
  !  3. Parameters :
  !
  !     Parameter list
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      STRACE    Subr. W3SERVMD Subroutine tracing.
  !     ----------------------------------------------------------------
  !
  !  5. Called by :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  6. Error messages :
  !  7. Remarks
  !  8. Structure :
  !  9. Switches :
  !
  !     !/S  Enable subroutine tracing.
  !
  ! 10. Source code :
  !
  !/ ------------------------------------------------------------------- /
  USE W3SERVMD  , ONLY : STRACE ! W3_S
  USE W3ODATMD  , ONLY : OUTPTS, IAPROC, NTPROC, NAPROC
  USE CONSTANTS , ONLY : LPDLIB
  use wav_shr_flags

  ! module default
  IMPLICIT NONE
  !/
  !/ ------------------------------------------------------------------- /
  !/ Parameter list
  !/
  !/ ------------------------------------------------------------------- /
  !/ Local parameters
  !/
  !
#ifdef W3_PDLIB
  INTEGER              :: PDLIB_NSEAL, PDLIB_NSEALM
  INTEGER, ALLOCATABLE :: JX_TO_JSEA(:), ISEA_TO_JSEA(:)
#endif
  INTEGER, ALLOCATABLE :: ListISPnextDir(:), ListISPprevDir(:)
  INTEGER, ALLOCATABLE :: ListISPnextFreq(:), ListISPprevFreq(:)

  LOGICAL, PARAMETER   :: LSLOC = .true.
  INTEGER, PARAMETER   :: IMEM = 1

  REAL,    PARAMETER   :: ONESIXTH  = 1.0d0/6.0d0
  REAL,    PARAMETER   :: ONETHIRD  = 1.0d0/3.0d0
  REAL,    PARAMETER   :: ZERO      = 0.0d0

  REAL*8,  PARAMETER   :: THR8      = TINY(1.d0)
  REAL,    PARAMETER   :: THR       = TINY(1.0)

CONTAINS
  !/ ------------------------------------------------------------------- /
  SUBROUTINE WAV_MY_WTIME(eTime)
    !/ ------------------------------------------------------------------- /
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose :
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL(8), intent(out) :: eTime
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER, SAVE           :: IENT = 0 ! W3_S
    INTEGER mpimode
#ifdef W3_MPI
    REAL(8) mpi_wtime
#endif
    mpimode=0
#ifdef W3_MPI
    mpimode=1
    eTime=mpi_wtime()
#endif
    if (w3_s_flag) then
       CALL STRACE (IENT, 'WAV_MY_WTIME')
    end if
    IF (mpimode .eq. 0) THEN
       CALL CPU_TIME(eTime)
    END IF
    !/
  END SUBROUTINE WAV_MY_WTIME
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PRINT_MY_TIME(string)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Print timings
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    character(*), intent(in) :: string
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER, SAVE   :: IENT = 0 ! W3_S
    REAL(8)         :: eTime
    !/
    !/ ------------------------------------------------------------------- /
    !
    if (w3_s_flag) then
       CALL STRACE (IENT, 'PRINT_MY_TIME')
    end if
    CALL WAV_MY_WTIME(eTime)
    WRITE(740+IAPROC,*) 'TIMING time=', eTime, ' at step ', string
    !/
  END SUBROUTINE PRINT_MY_TIME
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PROP_REFRACTION_PR1(ISEA,DTG, CAD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute refraction part in matrix
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD , ONLY : NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN
    USE W3GDATMD , ONLY : EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK
    USE W3GDATMD , ONLY : CTMAX, DMIN, DTH, CTHG0S, MAPSF
    USE W3ADATMD , ONLY : CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, DDDX, DDDY, DW
#ifdef W3_REFRX
    USE W3ADATMD , ONLY : DCDX, DCDY
#endif
    USE W3IDATMD , ONLY : FLCUR
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER , intent(in)  :: ISEA
    REAL    , intent(in)  :: DTG
    REAL    , intent(out) :: CAD(NSPEC)
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER       :: ISP, IK, ITH, IX, IY
    REAL          :: FRK(NK), FRG(NK), DSDD(0:NK+1)
    REAL          :: FACTH, DCXY, DCYX, DCXXYY, DTTST
    REAL          :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eDDDX, eDDDY, eCTHG0
    REAL          :: VCFLT(NSPEC), DEPTH, FDG
    REAL          :: FDDMAX

    INTEGER, SAVE :: IENT = 0 ! W3_S
    !/
    !/ ------------------------------------------------------------------- /

    if (w3_s_flag) then
       CALL STRACE (IENT, 'PROP_REFRACTION_PR1')
    end if
    IX=MAPSF(ISEA,1)
    IY=MAPSF(ISEA,2)
    eDDDX=DDDX(IY,IX)
    eDDDY=DDDY(IY,IX)
    eCTHG0 = CTHG0S(ISEA)
    FACTH  = DTG / DTH
    !
    FDG    = FACTH * eCTHG0
    DEPTH  = MAX ( DMIN , DW(ISEA) )
    DO IK=0, NK+1
       IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
          DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / DEPTH
       ELSE
          DSDD(IK) = 0.
       END IF
    END DO
    FDDMAX=0
    DO ITH=1, NTH
       FDDMAX = MAX ( FDDMAX , ABS(ESIN(ITH)*eDDDX - ECOS(ITH)*eDDDY ) )
    END DO
    if (w3_debug_flag) then
       WRITE(740+IAPROC,*) 'eDDDX=', eDDDX, ' Y=', eDDDY
       WRITE(740+IAPROC,*) 'FDDMAX=', FDDMAX
    end if
    DO IK=1, NK
       FRK(IK) = FACTH * DSDD(IK) / WN(IK,ISEA)
       !FRK(IK) = FRK(IK) / MAX ( 1. , FRK(IK)*FDDMAX/CTMAX )
       FRG(IK) = FDG * CG(IK,ISEA)
    END DO
    DO ISP=1, NSPEC
       VCFLT(ISP) = FRG(MAPWN(ISP)) * ECOS(ISP) +                     &
            FRK(MAPWN(ISP)) * ( ESIN(ISP)*eDDDX - ECOS(ISP)*eDDDY )
    END DO
    if (w3_debug_flag) then
       WRITE(740+IAPROC,*) 'pdlib: FACTH=', FACTH
       WRITE(740+IAPROC,*) 'pdlib: CTHG0=', eCTHG0
       WRITE(740+IAPROC,*) 'pdlib: FDG=', FDG
       WRITE(740+IAPROC,*) 'pdlib: FDDMAX=', FDDMAX
       WRITE(740+IAPROC,*) 'pdlib: sum(FRK)=', sum(FRK)
       WRITE(740+IAPROC,*) 'pdlib: sum(FRG)=', sum(FRG)
       WRITE(740+IAPROC,*) 'pdlib: sum(DSDD)=', sum(DSDD)
       WRITE(740+IAPROC,*) 'ISEA=', ISEA, ' sum(VCTH)=', sum(VCFLT)
    end if
    !
#ifdef W3_REFRX
    ! 3.c @C/@x refraction and great-circle propagation
    VCFLT  = 0.
    FRK    = 0.
    DO IK=1, NK
       FRK(IK) = FACTH * CG(IK,ISEA) * WN(IK,ISEA) / SIG(IK)
    END DO
    DO ISP=1, NSPEC
       VCFLT(ISP) = FRG(MAPWN(ISP)) * ECOS(ISP)           &
            + FRK(MAPWN(ISP)) * ( ESIN(ISP)*DCDX(ISP,1,MAPWN(ISP)) &
            - ECOS(ISP)*DCDY(ISP,1,MAPWN(ISP)) )
    END DO
#endif
    !
    IF ( FLCUR ) THEN
       eDCXDX=DCXDX(IY,IX)
       eDCXDY=DCXDY(IY,IX)
       eDCYDX=DCYDX(IY,IX)
       eDCYDY=DCYDY(IY,IX)
       DCYX   = FACTH *   eDCYDX
       DCXXYY = FACTH * ( eDCXDX - eDCYDY )
       DCXY   = FACTH *   eDCXDY
       DO ISP=1, NSPEC
          VCFLT(ISP) = VCFLT(ISP) + ES2(ISP)*DCYX  + ESC(ISP)*DCXXYY - EC2(ISP)*DCXY
       END DO
    END IF
    DO ISP=1,NSPEC
       CAD(ISP)=DBLE(VCFLT(ISP))
    END DO
    !/
  END SUBROUTINE PROP_REFRACTION_PR1
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PROP_REFRACTION_PR3(IP, ISEA, DTG, CAD, DoLimiter)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute refraction part in matrix alternative approach
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD , ONLY : NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN
    USE W3GDATMD , ONLY : EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK
    USE W3GDATMD , ONLY : CTMAX, DMIN, DTH, CTHG0S, MAPSF, SIG
    USE W3ADATMD , ONLY : CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, DDDX, DDDY, DW
    USE W3IDATMD , ONLY : FLCUR
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    INTEGER , intent(in)  :: ISEA, IP
    REAL    , intent(in)  :: DTG
    logical , intent(in)  :: DoLimiter
    REAL    , intent(out) :: CAD(NSPEC)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER       :: ISP, IK, ITH, IX, IY
    REAL          :: FRK(NK), FRG(NK), DSDD(0:NK+1)
    REAL          :: FACTH, DCXY, DCYX, DCXXYY, DTTST
    REAL          :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eDDDX, eDDDY, eCTHG0
    REAL          :: VCFLT(NSPEC), DEPTH, FDG, CG1(0:NK+1), WN1(0:NK+1)
    REAL          :: FDDMAX, CFLTHMAX, VELNOFILT, CTMAX_eff
    INTEGER, SAVE :: IENT = 0 ! W3_S
    !/
    !/ ------------------------------------------------------------------- /

    if (w3_s_flag) then
       CALL STRACE (IENT, 'PROP_REFRACTION_PR3')
    end if

    IX = MAPSF(ISEA,1)
    IY = MAPSF(ISEA,2)
    eDDDX=DDDX(1,IP)
    eDDDY=DDDY(1,IP)
    eCTHG0 = CTHG0S(ISEA)
    FACTH  = 1.0 / DTH
    !
    FDG    = FACTH * eCTHG0
    DEPTH  = MAX ( DMIN , DW(ISEA) )
    DO IK=0, NK+1
       IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
          DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / DEPTH
       ELSE
          DSDD(IK) = 0.
       END IF
    END DO
    DO IK=1, NK
       FRK(IK) = FACTH * DSDD(IK) / WN(IK,ISEA)
       FRG(IK) = FDG * CG(IK,ISEA)
    END DO
    IF (FLCUR) THEN
       eDCXDX = DCXDX(1,IP)
       eDCXDY = DCXDY(1,IP)
       eDCYDX = DCYDX(1,IP)
       eDCYDY = DCYDY(1,IP)
       DCYX   = FACTH *   eDCYDX
       DCXXYY = FACTH * ( eDCXDX - eDCYDY )
       DCXY   = FACTH *   eDCXDY
       DO ISP=1, NSPEC
          VCFLT(ISP) = ES2(ISP)*DCYX  + ESC(ISP)*DCXXYY - EC2(ISP)*DCXY
       END DO
    ELSE
       VCFLT=0
    END IF
    !
#ifdef W3_REFRX
    ! 3.c @C/@x refraction and great-circle propagation
    DO IK=1, NK
       FRK(IK) = FACTH * CG(IK,ISEA) * WN(IK,ISEA) / SIG(IK)
    END DO
#endif
    !
    CTMAX_eff=CTMAX/DTG
    DO ISP=1, NSPEC
       VELNOFILT = VCFLT(ISP)                                       &
            + FRG(MAPWN(ISP)) * ECOS(ISP)                             &
            + FRK(MAPWN(ISP)) * (ESIN(ISP)*eDDDX - ECOS(ISP)*eDDDY)
       !
       ! Puts filtering on total velocity (including currents and great circle effects)
       ! the filtering limits VCFLT to be less than CTMAX
       ! this modification was proposed by F. Ardhuin 2011/03/06
       !
       IF (DoLimiter) THEN
          VCFLT(ISP)=SIGN(MIN(ABS(VELNOFILT),CTMAX_eff),VELNOFILT)
       ELSE
          VCFLT(ISP)=VELNOFILT
       END IF
    END DO
    DO ISP=1,NSPEC
       CAD(ISP)=DBLE(VCFLT(ISP))
    END DO
    !/
  END SUBROUTINE PROP_REFRACTION_PR3
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PROP_FREQ_SHIFT(IP, ISEA, CAS, DMM, DTG)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute freq. shift in matrix
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD , ONLY : NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN
    USE W3GDATMD , ONLY : EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK
    USE W3GDATMD , ONLY : CTMAX, DMIN, DTH, MAPSF
    USE W3ADATMD , ONLY : CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, CX, CY
    USE W3ADATMD , ONLY : DDDX, DDDY, DW

    !/ Parameter list
    INTEGER , intent(in)  :: ISEA, IP
    REAL    , intent(in)  :: DTG
    REAL    , intent(out) :: DMM(0:NK2)
    REAL    , intent(out) :: CAS(NSPEC)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    REAL          :: DB(NK2), DSDD(0:NK+1)
    REAL          :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eCX, eCY, eDDDX, EDDDY
    REAL          :: DCXX, DCXYYX, DCYY, FKD, FACK
    REAL          :: VELNOFILT, VELFAC, DEPTH
    REAL          :: CFLK(NK2,NTH), FKC(NTH), FKD0
    INTEGER       :: IK, ITH, ISP, IY, IX
    INTEGER, SAVE :: IENT = 0 ! W3_S
    !/
    !/ ------------------------------------------------------------------- /

    if (w3_s_flag) then
       CALL STRACE (IENT, 'PROP_FREQ_SHIFT')
    end if
    !
    IF (LPDLIB) THEN
       eDCXDX = DCXDX(1,IP)
       eDCXDY = DCXDY(1,IP)
       eDCYDX = DCYDX(1,IP)
       eDCYDY = DCYDY(1,IP)
       eDDDX  = DDDX(1,IP)
       eDDDY  = DDDY(1,IP)
    ELSE
       IX=MAPSF(ISEA,1)
       IY=MAPSF(ISEA,2)
       eDCXDX=DCXDX(IY,IX)
       eDCXDY=DCXDY(IY,IX)
       eDCYDX=DCYDX(IY,IX)
       eDCYDY=DCYDY(IY,IX)
       eDDDX=DDDX(IY,IX)
       eDDDY=DDDY(IY,IX)
    ENDIF
    eCX=CX(ISEA)
    eCY=CY(ISEA)
    DCXX   =  -   eDCXDX
    DCXYYX =  - ( eDCXDY + eDCYDX )
    DCYY   =  -   eDCYDY
    FKD    =    ( eCX*eDDDX + eCY*eDDDY )
    FACK = DTG
    if (w3_debug_flag) then
       WRITE(740+IAPROC,*) 'DCXX=', DCXX, ' DCXYYX=', DCXYYX
       WRITE(740+IAPROC,*) 'DCYY=', DCYY, ' FKD=', FKD
       WRITE(740+IAPROC,*) 'DTG=', DTG
    end if
    DO ITH=1, NTH
       FKC(ITH) = EC2(ITH)*DCXX + ESC(ITH)*DCXYYX + ES2(ITH)*DCYY
    END DO
    DO IK=0, NK
       DB(IK+1) = DSIP(IK) / CG(IK,ISEA)
       DMM(IK+1) = DBLE(WN(IK+1,ISEA) - WN(IK,ISEA))
    END DO
    DB(NK+2) = DSIP(NK+1) / CG(NK+1,ISEA)
    DMM(NK+2) = ZERO
    DMM(0)=DMM(1)
    !
    DEPTH  = MAX ( DMIN , DW(ISEA) )
    DO IK=0, NK+1
       IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
          DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / DEPTH
       ELSE
          DSDD(IK) = 0.
       END IF
    END DO
    if (w3_debug_flag) then
       WRITE(740+IAPROC,*) 'DSDD(min/max)=', minval(DSDD), maxval(DSDD)
    end if
    DO IK=0, NK+1
       FKD0   = FKD / CG(IK,ISEA) * DSDD(IK)
       VELFAC =  FACK/DB(IK+1)
       DO ITH=1, NTH
          VELNOFILT = ( FKD0 + WN(IK,ISEA)*FKC(ITH) ) * VELFAC
          CFLK(IK+1,ITH) = VELNOFILT/VELFAC
       END DO
    END DO
    if (w3_debug_flag) then
       WRITE(740+IAPROC,*) 'sum(CFLK)=', sum(CFLK)
    end if
    DO IK=1,NK
       DO ITH=1,NTH
          ISP=ITH + (IK-1)*NTH
          CAS(ISP)=DBLE(CFLK(IK,ITH))
       END DO
    END DO
    if (w3_debug_flag) then
       WRITE(740+IAPROC,*) 'sum(abs(CAS))=', sum(abs(CAS))
    end if
    !/
  END SUBROUTINE PROP_FREQ_SHIFT
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PROP_FREQ_SHIFT_M2(IP, ISEA, CWNB_M2, DWNI_M2, DTG)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute freq. shift alternative approach
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    USE W3GDATMD , ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN
    USE W3GDATMD , ONLY : EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK
    USE W3GDATMD , ONLY : CTMAX, DMIN, DTH, MAPSF
    USE W3ADATMD , ONLY : CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, CX, CY
    USE W3ADATMD , ONLY : DDDX, DDDY, DW
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER , intent(in)  :: ISEA, IP
    REAL    , intent(in)  :: DTG
    REAL    , intent(out) :: CWNB_M2(1-NTH:NSPEC)
    REAL    , intent(out) :: DWNI_M2(NK)
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    REAL          :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eCX, eCY, eDDDX, EDDDY
    REAL          :: DCXX, DCXYYX, DCYY, FKD, FACK
    REAL          :: DEPTH
    REAL          :: FKC(NTH), FKD0
    REAL          :: VCWN(1-NTH:NSPEC+NTH)
    REAL          :: DSDD(0:NK+1)
    REAL          :: sumDiff, sumDiff1, sumDiff2, sumDiff3
    REAL          :: sumDiff0, sumDiff4, sumDiff5
    INTEGER       :: IK, ITH, ISP, IY, IX
    INTEGER, SAVE :: IENT = 0 ! W3_S
    !/ ------------------------------------------------------------------- /
    if (w3_s_flag) then
       CALL STRACE (IENT, 'PROP_FREQ_SHIFT_M2')
    end if

    if (w3_debugdcxdx_flag) then
       WRITE(740+IAPROC,*) 'Now we use DCXDX array in PROP_FREQ_SHIFT_M2'
    end if

    IF (LPDLIB) THEN
       eDCXDX = DCXDX(1,IP)
       eDCXDY = DCXDY(1,IP)
       eDCYDX = DCYDX(1,IP)
       eDCYDY = DCYDY(1,IP)
       eDDDX  = DDDX(1,IP)
       eDDDY  = DDDY(1,IP)
    ELSE
       IX=MAPSF(ISEA,1)
       IY=MAPSF(ISEA,2)
       eDCXDX=DCXDX(IY,IX)
       eDCXDY=DCXDY(IY,IX)
       eDCYDX=DCYDX(IY,IX)
       eDCYDY=DCYDY(IY,IX)
       eDDDX=DDDX(IY,IX)
       eDDDY=DDDY(IY,IX)
    ENDIF

    eCX = CX(ISEA)
    eCY = CY(ISEA)
    FACK = DTG
    DCXX   =  - FACK *   eDCXDX
    DCXYYX =  - FACK * ( eDCXDY + eDCYDX )
    DCYY   =  - FACK *   eDCYDY
    FKD    =    FACK * ( eCX*eDDDX + eCY*eDDDY )

    if (w3_debugdcxdx_flag) then
       sumDiff0=0
       sumDiff1=0
       sumDiff2=0
       sumDiff3=0
       sumDiff4=0
       sumDiff5=0
    end if
    DO ITH=1, NTH
       FKC(ITH) = EC2(ITH)*DCXX + ESC(ITH)*DCXYYX + ES2(ITH)*DCYY
       if (w3_debugdcxdx_flag) then
          sumDiff0 = sumDiff0 + MIN(EC2(ITH), ZERO)
          sumDiff1 = sumDiff1 + MIN(DCXX, ZERO)
          sumDiff2 = sumDiff2 + MIN(ESC(ITH), ZERO)
          sumDiff3 = sumDiff3 + MIN(DCXYYX, ZERO)
          sumDiff4 = sumDiff4 + MIN(ES2(ITH), ZERO)
          sumDiff5 = sumDiff5 + MIN(DCYY, ZERO)
       end if
    END DO
    if (w3_debugdcxdx_flag) then
       WRITE(740+IAPROC,*) 'sumDiff0=', sumDiff0
       WRITE(740+IAPROC,*) 'sumDiff1=', sumDiff1
       WRITE(740+IAPROC,*) 'sumDiff2=', sumDiff2
       WRITE(740+IAPROC,*) 'sumDiff3=', sumDiff3
       WRITE(740+IAPROC,*) 'sumDiff4=', sumDiff4
       WRITE(740+IAPROC,*) 'sumDiff5=', sumDiff5
    end if
    !
    DEPTH  = MAX ( DMIN , DW(ISEA) )
    DO IK=0, NK+1
       IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
          DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / DEPTH
       ELSE
          DSDD(IK) = 0.
       END IF
    END DO
    ISP = -NTH
    if (w3_debugdcxdx_flag) then
       sumDiff=0
       sumDiff1=0
       sumDiff2=0
       sumDiff3=0
    end if
    DO IK=0, NK+1
       FKD0   = FKD / CG(IK,ISEA) * DSDD(IK)
       DO ITH=1, NTH
          ISP = ISP + 1
          VCWN(ISP) = FKD0 + WN(IK,ISEA)*FKC(ITH)
          if (w3_debugdcxdx_flag) then
             sumDiff = sumDiff + MAX(VCWN(ISP),ZERO)
             sumDiff1 = sumDiff1 + MAX(FKD0,ZERO)
             sumDiff2 = sumDiff2 + MAX(WN(IK,ISEA),ZERO)
             sumDiff3 = sumDiff3 + MAX(FKC(ITH),ZERO)
          end if
       END DO
    END DO
    if (w3_debugdcxdx_flag) then
       WRITE(740+IAPROC,*) 'sumDiff=', sumDiff
       WRITE(740+IAPROC,*) 'sumDiff1=', sumDiff1
       WRITE(740+IAPROC,*) 'sumDiff2=', sumDiff2
       WRITE(740+IAPROC,*) 'sumDiff3=', sumDiff3
    end if

    sumDiff=0
    DO ISP=1-NTH,NSPEC
       CWNB_M2(ISP) = DBLE(0.5 * ( VCWN(ISP) + VCWN(ISP+NTH) ))
       sumDiff = sumDiff + MAX(CWNB_M2(ISP), ZERO)
    END DO
    if (w3_debugdcxdx_flag) then
       WRITE(740+IAPROC,*) 'sumDiff=', sumDiff
    end if
    DO IK=1,NK
       DWNI_M2(IK) = DBLE( CG(IK,ISEA) / DSIP(IK) )
    END DO
    !/
  END SUBROUTINE PROP_FREQ_SHIFT_M2
  !/ ------------------------------------------------------------------- /
  SUBROUTINE SYNCHRONIZE_IPGL_ETC_ARRAY(IMOD, IsMulti)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Sync global local arrays
    !  2. Method :
    ! 		All the process need to have IPGL_tot and IPGL_TO_PROC
    ! 		This is especially the case for the output process.
    ! 		So we need some painful exportation business
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
#ifdef W3_PDLIB
    USE yowDatapool   , only : istatus
    USE yowNodepool   , only : np_global
    USE W3GDATMD      , ONLY : MAPSF, NSEA
    USE W3ADATMD      , ONLY : MPI_COMM_WAVE, MPI_COMM_WCMP
    USE yowRankModule , only : IPGL_TO_PROC, IPGL_tot
    USE WMMDATMD      , ONLY : MDATAS
#endif
#ifdef W3_PDLIB
    INCLUDE "mpif.h"
#endif
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, intent(in) :: IMOD
    logical, intent(in) :: IsMulti
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER, SAVE           :: IENT = 0 ! W3_S
#ifdef W3_PDLIB
    INTEGER :: Iarr(1)
    INTEGER :: ISEA, IP_glob
    INTEGER :: IPROC, IERR_MPI, istat
#endif
    !/
    !/ ------------------------------------------------------------------- /

    if (w3_s_flag) then
       CALL STRACE (IENT, 'SYNCHRONIZE_IPGL_ETC_ARRAY')
    end if

#ifdef W3_PDLIB
    IF (IAPROC .le. NAPROC) THEN
       IF (IAPROC .eq. 1) THEN
          Iarr(1)=np_global
          DO IPROC=NAPROC+1,NTPROC
             CALL MPI_SEND(Iarr,1,MPI_INT, IPROC-1, 37, MPI_COMM_WAVE, IERR_MPI)
          END DO
          DO IPROC=NAPROC+1,NTPROC
             CALL MPI_SEND(ipgl_tot,np_global,MPI_INT, IPROC-1, 43, MPI_COMM_WAVE, IERR_MPI)
             CALL MPI_SEND(ipgl_to_proc,np_global,MPI_INT, IPROC-1, 91, MPI_COMM_WAVE, IERR_MPI)
          END DO
       END IF
    ELSE
       CALL MPI_RECV(Iarr,1,MPI_INT, 0, 37, MPI_COMM_WAVE, istatus, IERR_MPI)
       np_global=Iarr(1)
       allocate(IPGL_tot(np_global), IPGL_TO_PROC(np_global), stat=istat)
       CALL MPI_RECV(ipgl_tot,np_global,MPI_INT, 0, 43, MPI_COMM_WAVE, istatus, IERR_MPI)
       CALL MPI_RECV(ipgl_to_proc,np_global,MPI_INT, 0, 91, MPI_COMM_WAVE, istatus, IERR_MPI)
    END IF
    IF (IsMulti) THEN
       WRITE(*,*) ' Before allocation of MDATAS % SEA_IPGL, SEA_IPGL_TO_PROC : IMOD=', IMOD, ' NSEA=', NSEA
       ALLOCATE(MDATAS(IMOD)%SEA_IPGL(NSEA), MDATAS(IMOD)%SEA_IPGL_TO_PROC(NSEA), STAT=ISTAT)
       !CHECK_ALLOC_STATUS ( ISTAT )
       DO ISEA=1,NSEA
          IP_glob = MAPSF(ISEA, 1)
          MDATAS(IMOD)%SEA_IPGL(ISEA) = IPGL_tot(IP_glob)
          MDATAS(IMOD)%SEA_IPGL_TO_PROC(ISEA) = IPGL_TO_PROC(IP_glob)
       END DO
    END IF
#endif
    !/
  END SUBROUTINE SYNCHRONIZE_IPGL_ETC_ARRAY
  !/ ....................----------------------------------------------- /
  SUBROUTINE SET_UP_NSEAL_NSEALM(NSEALout, NSEALMout)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Setup nseal, nsealm in contect of pdlib
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
#ifdef W3_PDLIB
    use yowDatapool   , only : istatus
    use yowNodepool   , only : npa
    use yowRankModule , only : rank
    USE W3GDATMD      , ONLY : GTYPE, UNGTYPE
#endif
#ifdef W3_MPI
    USE W3ADATMD      , ONLY : MPI_COMM_WAVE, MPI_COMM_WCMP
#endif
    USE W3GDATMD      , ONLY : NSEA

    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    INTEGER, intent(out) :: NSEALout, NSEALMout
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER, SAVE           :: IENT = 0 ! W3_S
    !/ ------------------------------------------------------------------- /
    !/
    if (w3_s_flag) then
       CALL STRACE (IENT, 'SET_UP_NSEAL_NSEALM')
    end if
    !!/PDLIB      WRITE(*,*) 'LPDLIB=', LPDLIB
    !!/PDLIB      WRITE(*,*) 'GTYPE=', GTYPE, ' UNGTYPE=', UNGTYPE
    if (w3_debug_flag) then
       WRITE(740+IAPROC,*) 'SET_UP, PDLIB=', LPDLIB
       FLUSH(740+IAPROC)
    end if

#ifdef W3_SHRD
    NSEALout  = NSEA
    NSEALMout = NSEA
#endif
    !
#ifdef W3_DIST
    IF (.NOT. LPDLIB ) THEN
       IF ( IAPROC .LE. NAPROC ) THEN
          NSEALout  = 1 + (NSEA-IAPROC)/NAPROC
       ELSE
          NSEALout  = 0
       END IF
       NSEALMout = 1 + (NSEA-1)/NAPROC
    ELSE
#endif
#ifdef W3_PDLIB
       IF (GTYPE .eq. UNGTYPE) THEN
          NSEALout  = PDLIB_NSEAL
          NSEALMout = PDLIB_NSEALM
       ELSE
          IF ( IAPROC .LE. NAPROC ) THEN
             NSEALout  = 1 + (NSEA-IAPROC)/NAPROC
          ELSE
             NSEALout  = 0
          END IF
          NSEALMout = 1 + (NSEA-1)/NAPROC
       ENDIF
#endif
#ifdef W3_DIST
    ENDIF
#endif
    !/
  END SUBROUTINE SET_UP_NSEAL_NSEALM
  !/ ------------------------------------------------------------------- /
  SUBROUTINE INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
    !/ ------------------------------------------------------------------- /
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Set Jsea for all schemes
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    USE W3GDATMD      , ONLY : GTYPE, UNGTYPE, MAPSF
#ifdef W3_PDLIB
    USE yowRankModule , only : IPGL_TO_PROC, IPGL_tot
    use yowNodepool   , only : ipgl, iplg
#endif
    !/ ------------------------------------------------------------------- /
    !/ Parameter list

    INTEGER, intent(in)  :: ISEA
    INTEGER, intent(out) :: JSEA, ISPROC
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER       :: IP_glob
    INTEGER, SAVE :: IENT = 0 ! W3_S
    !/
    !/ ------------------------------------------------------------------- /
    !/
    if (w3_s_flag) then
       CALL STRACE (IENT, 'INIT_GET_JSEA_ISPROC')
    end if
    !!/DEBUG      WRITE(740+IAPROC,*) 'PDLIB=', PDLIB
    !!/DEBUG      WRITE(740+IAPROC,*) 'GTYPE=', GTYPE, ' UNGTYPE=', UNGTYPE
    !!/DEBUG      FLUSH(740+IAPROC)
    IF (.NOT. LPDLIB) THEN
       JSEA   = 1 + (ISEA-1)/NAPROC
       ISPROC = ISEA - (JSEA-1)*NAPROC
    ELSE
#ifdef W3_PDLIB
       IF (GTYPE .ne. UNGTYPE) THEN
          JSEA   = 1 + (ISEA-1)/NAPROC
          ISPROC = ISEA - (JSEA-1)*NAPROC
       ELSE
          IP_glob = MAPSF(ISEA,1)
          IF (IAPROC .le. NAPROC) THEN
             JSEA   = ISEA_TO_JSEA(ISEA)
          ELSE
             JSEA   = -1
          END IF
          ISPROC = IPGL_TO_PROC(IP_glob)
       ENDIF
#endif
    ENDIF
    !/
  END SUBROUTINE INIT_GET_JSEA_ISPROC
  !/ ------------------------------------------------------------------- /
  SUBROUTINE GET_JSEA_IBELONG(ISEA, JSEA, IBELONG)
    !/ ------------------------------------------------------------------- /
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Set belongings of jsea in context of pdlib
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    USE W3GDATMD      , ONLY : GTYPE, UNGTYPE, MAPSF
#ifdef W3_PDLIB
    USE yowRankModule , only : IPGL_TO_PROC, IPGL_tot, IPGL_npa
    use yowNodepool   , only : ipgl, iplg
#endif
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER , intent(in)  :: ISEA
    INTEGER , intent(out) :: JSEA, IBELONG
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER       :: ISPROC, IX, JX
    INTEGER, SAVE :: IENT = 0 ! W3_S
    !/ ------------------------------------------------------------------- /
    !/
    if (w3_s_flag) then
       CALL STRACE (IENT, 'GET_JSEA_IBELONG')
    end if
    IF (.NOT. LPDLIB) THEN
       JSEA   = 1 + (ISEA-1)/NAPROC
       ISPROC = ISEA - (JSEA-1)*NAPROC
       IF (ISPROC .eq. IAPROC) THEN
          IBELONG=1
       ELSE
          IBELONG=0
       END IF
    ELSE
#ifdef W3_PDLIB
       IF (GTYPE .ne. UNGTYPE) THEN
          JSEA   = 1 + (ISEA-1)/NAPROC
          ISPROC = ISEA - (JSEA-1)*NAPROC
          IF (ISPROC .eq. IAPROC) THEN
             IBELONG=1
          ELSE
             IBELONG=0
          END IF
       ELSE
          IF (IAPROC .le. NAPROC) THEN
             IX     = MAPSF(ISEA,1)
             JX     = IPGL_npa(IX)
             IF (JX .eq. 0) THEN
                IBELONG=0
                JSEA=-1
             ELSE
                IBELONG=1
                JSEA = JX_TO_JSEA(JX)
             END IF
          ELSE
             IBELONG=0
             JSEA=-1
          END IF
       ENDIF
#endif
    ENDIF
    !/
    !/ End of INIT_GET_ISEA ---------------------------------------------- /
    !/
  END SUBROUTINE GET_JSEA_IBELONG
  !/ ------------------------------------------------------------------- /
  SUBROUTINE INIT_GET_ISEA(ISEA, JSEA)
    !/ ------------------------------------------------------------------- /
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Set Isea for all schemes
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    USE W3GDATMD   , ONLY : GTYPE, UNGTYPE
#ifdef W3_PDLIB
    USE YOWNODEPOOL, ONLY : iplg
#endif
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    INTEGER , intent(in)  :: JSEA
    INTEGER , intent(out) :: ISEA
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER, SAVE           :: IENT = 0 ! W3_S
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !
    if (w3_s_flag) then
       CALL STRACE (IENT, 'INIT_GET_ISEA')
    end if
#ifdef W3_SHRD
    ISEA         = JSEA
#endif
#ifdef W3_DIST
    IF (.NOT. LPDLIB) THEN
       ISEA         = IAPROC + (JSEA-1)*NAPROC
    ELSE
#endif
#ifdef W3_PDLIB
       IF (GTYPE .eq. UNGTYPE) THEN
          ISEA         = iplg(JSEA)
       ELSE
          ISEA         = IAPROC + (JSEA-1)*NAPROC
       ENDIF
#endif
#ifdef W3_DIST
    ENDIF
#endif
    !/
    !/ End of INIT_GET_ISEA ------------------------------------------------ /
    !/
  END SUBROUTINE INIT_GET_ISEA
  !**********************************************************************
  !*  An array of size (NSEA) is send but only the (1:NSEAL) values     *
  !*  are correct. The program synchonizes everything on all nodes.     *
  !**********************************************************************
  SUBROUTINE SYNCHRONIZE_GLOBAL_ARRAY(TheVar)
    !/ ------------------------------------------------------------------- /
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Sync global array in context of pdlib
    !  2. Method :
    !			An array of size (NSEA) is send but only the (1:NSEAL) values
    ! 			are correct. The program synchonizes everything on all nodes.
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    USE W3GDATMD    , ONLY : NSEAL, NSEA, NX
#ifdef W3_PDLIB
    USE W3ADATMD    , ONLY : MPI_COMM_WCMP
    use yowDatapool , only : rtype, istatus
    USE yowNodepool , only : npa
    use yowNodepool , only : iplg
#endif
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    REAL*8, intent(inout) :: TheVar(NX)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER, SAVE           :: IENT = 0 ! W3_S
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_MPI
    INCLUDE "mpif.h"
#endif
    INTEGER :: ISEA, JSEA, Status(NX), rStatus(NX)
    INTEGER :: IPROC, I, ierr, IP, IX, IP_glob
    REAL*8  :: rVect(NX)

    Status=0
    if (w3_s_flag) then
       CALL STRACE (IENT, 'SYNCHRONIZE_GLOBAL_ARRAY')
    end if
#ifdef W3_PDLIB
    DO IP=1,npa
       IP_glob=iplg(IP)
       Status(IP_glob)=1
    END DO
    IF (IAPROC .eq. 1) THEN
       DO iProc=2,NAPROC
          CALL MPI_RECV(rVect,NX,rtype, iProc-1, 19, MPI_COMM_WCMP, istatus, ierr)
          CALL MPI_RECV(rStatus,NX,MPI_INTEGER, iProc-1, 23, MPI_COMM_WCMP, istatus, ierr)
          DO I=1,NX
             IF (rStatus(I) .eq. 1) THEN
                TheVar(I)=rVect(I)
                Status(I)=1
             END IF
          END DO
       END DO
       DO IPROC=2,NAPROC
          CALL MPI_SEND(TheVar,NX,rtype, iProc-1, 29, MPI_COMM_WCMP, ierr)
       END DO
    ELSE
       CALL MPI_SEND(TheVar,NX,rtype, 0, 19, MPI_COMM_WCMP, ierr)
       CALL MPI_SEND(Status,NX,MPI_INTEGER, 0, 23, MPI_COMM_WCMP, ierr)
       CALL MPI_RECV(TheVar,NX,rtype, 0, 29, MPI_COMM_WCMP, istatus, ierr)
    END IF
#endif
    !/
  END SUBROUTINE SYNCHRONIZE_GLOBAL_ARRAY
  !/ ------------------------------------------------------------------- /
END MODULE W3PARALL
!/ ------------------------------------------------------------------- /
