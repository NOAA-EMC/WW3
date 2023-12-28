!> @file
!> @brief Parallel routines for implicit solver.
!>
!> @author Aron Roland
!> @author Mathieu Dutour-Sikiric
!> @date   01-Jun-2018
!>

!/ ------------------------------------------------------------------- /
!>
!> @brief Parallel routines for implicit solver.
!>
!> @author Aron Roland
!> @author Mathieu Dutour-Sikiric
!> @date   01-Jun-2018
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>
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
#ifdef W3_S
  USE W3SERVMD, ONLY: STRACE
#endif
  !/
  !/ ------------------------------------------------------------------- /
  !/ Parameter list
  !/
  !/ ------------------------------------------------------------------- /
  !/ Local parameters
  !/
#ifdef W3_S
  INTEGER, SAVE           :: IENT = 0
#endif
  !
#ifdef W3_PDLIB
  INTEGER :: PDLIB_NSEAL, PDLIB_NSEALM
  INTEGER, ALLOCATABLE :: JX_TO_JSEA(:), ISEA_TO_JSEA(:)
#endif

  INTEGER, ALLOCATABLE :: ListISPnextDir(:), ListISPprevDir(:)
  INTEGER, ALLOCATABLE :: ListISPnextFreq(:), ListISPprevFreq(:)

  LOGICAL, PARAMETER   :: LSLOC = .true.
  INTEGER, PARAMETER   :: IMEM = 1

  REAL,  PARAMETER     :: ONESIXTH  = 1.0d0/6.0d0
  REAL,  PARAMETER     :: ONETHIRD  = 1.0d0/3.0d0
  REAL,  PARAMETER     :: ZERO      = 0.0d0

  REAL*8,  PARAMETER    :: THR8 = TINY(1.d0)
  REAL,  PARAMETER      :: THR  = TINY(1.0)

  REAL*8, PARAMETER     :: PI = 3.14159265358979323846D0
  REAL*8, PARAMETER     :: DEGRAD    = PI/180.d0
  REAL*8, PARAMETER     :: RADDEG    = 180.d0/PI
  REAL*8, PARAMETER     :: KDMAX     = 200.d0


  REAL*8, ALLOCATABLE   :: DIFRX(:), DIFRY(:), DIFRM(:)
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief NA
  !>
  !> @param[out] eTime
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
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
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    IMPLICIT NONE
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    INTEGER mpimode
    REAL(8), intent(out) :: eTime
#ifdef W3_MPI
    REAL(8) mpi_wtime
#endif
    mpimode=0
#ifdef W3_MPI
    mpimode=1
    eTime=mpi_wtime()
#endif
#ifdef W3_S
    CALL STRACE (IENT, 'WAV_MY_WTIME')
#endif
    IF (mpimode .eq. 0) THEN
      CALL CPU_TIME(eTime)
    END IF
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE WAV_MY_WTIME
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Print timings.
  !>
  !> @param[in] string
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
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
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE W3ODATMD, ONLY : IAPROC
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !
    character(*), intent(in) :: string
    REAL(8) :: eTime
#ifdef W3_S
    CALL STRACE (IENT, 'PRINT_MY_TIME')
#endif
    CALL WAV_MY_WTIME(eTime)
    WRITE(740+IAPROC,*) 'TIMING time=', eTime, ' at step ', string
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE PRINT_MY_TIME
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Compute refraction part in matrix.
  !>
  !> @param[in]  ISEA
  !> @param[in]  DTG
  !> @param[out] CAD
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
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
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
         EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
         CTMAX, DMIN, DTH, CTHG0S, MAPSF
    USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, DDDX,   &
         DDDY, DW
#ifdef W3_REFRX
    USE W3ADATMD, ONLY: DCDX, DCDY
#endif
    USE W3IDATMD, ONLY: FLCUR
    USE W3ODATMD, only : IAPROC
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !/
    REAL, intent(out) :: CAD(NSPEC)
    INTEGER, intent(in) :: ISEA
    REAL, intent(in) :: DTG
    INTEGER :: ISP, IK, ITH, IX, IY
    REAL :: FRK(NK), FRG(NK), DSDD(0:NK+1)
    REAL :: FACTH, DCXY, DCYX, DCXXYY, DTTST
    REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eDDDX, eDDDY, eCTHG0
    REAL :: VCFLT(NSPEC), DEPTH, FDG
    REAL :: FDDMAX
#ifdef W3_S
    CALL STRACE (IENT, 'PROP_REFRACTION_PR1')
#endif
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
    DO IK=1, NK
      FRK(IK) = FACTH * DSDD(IK) / WN(IK,ISEA)
      !FRK(IK) = FRK(IK) / MAX ( 1. , FRK(IK)*FDDMAX/CTMAX )
      FRG(IK) = FDG * CG(IK,ISEA)
    END DO
    DO ISP=1, NSPEC
      VCFLT(ISP) = FRG(MAPWN(ISP)) * ECOS(ISP) +                     &
           FRK(MAPWN(ISP)) * ( ESIN(ISP)*eDDDX - ECOS(ISP)*eDDDY )
    END DO
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
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE PROP_REFRACTION_PR1
  !/ ------------------------------------------------------------------- /
  !
  !>
  !> @brief Compute refraction part in matrix alternative approach.
  !>
  !> @param[in]  IP
  !> @param[in]  ISEA
  !> @param[in]  DTG
  !> @param[out] CAD
  !> @param[in]  DoLimiter
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
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
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE CONSTANTS, ONLY : LPDLIB
    USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
         EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
         CTMAX, DMIN, DTH, CTHG0S, MAPSF, SIG, B_JGS_LDIFR
    USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, DDDX,   &
         DDDY, DW
    USE W3IDATMD, ONLY: FLCUR
    USE W3ODATMD, only : IAPROC
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL, intent(out) :: CAD(NSPEC)
    INTEGER, intent(in) :: ISEA, IP
    REAL, intent(in) :: DTG
    logical, intent(in) :: DoLimiter
    INTEGER :: ISP, IK, ITH, IX, IY
    REAL :: FRK(NK), FRG(NK), DSDD(0:NK+1)
    REAL :: FACTH, DCXY, DCYX, DCXXYY, DTTST
    REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eDDDX, eDDDY, eCTHG0
    REAL :: VCFLT(NSPEC), DEPTH, FDG, CG1(0:NK+1), WN1(0:NK+1)
    REAL :: FDDMAX, CFLTHMAX, VELNOFILT, CTMAX_eff
#ifdef W3_S
    CALL STRACE (IENT, 'PROP_REFRACTION_PR3')
#endif

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

    IF (B_JGS_LDIFR) THEN
      DO IK = 1, NK
        DO ITH = 1, NTH
          ISP = ITH + (IK-1)*NTH
          CAD(ISP) = DIFRM(IP)*CAD(ISP)-CG(IK,IP)*(DIFRX(IP)*ESIN(ITH)-DIFRY(IP)*ECOS(ITH))
        END DO
      END DO
    END IF
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE PROP_REFRACTION_PR3
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Compute frequency shift in matrix.
  !>
  !> @param[in]  IP
  !> @param[in]  ISEA
  !> @param[out] CAS
  !> @param[out] DMM
  !> @param[in]  DTG
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
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
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE CONSTANTS, ONLY : LPDLIB
    USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
         EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
         CTMAX, DMIN, DTH, MAPSF, B_JGS_LDIFR
    USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, CX, CY, DDDX, DDDY, DW
    USE W3ODATMD, only : IAPROC
    IMPLICIT NONE
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    INTEGER, intent(in) :: ISEA, IP
    REAL, intent(out) :: DMM(0:NK2)
    REAL, intent(in) :: DTG
    REAL, intent(out) :: CAS(NSPEC)
    REAL :: DB(NK2), DSDD(0:NK+1)
    REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eCX, eCY, eDDDX, EDDDY
    REAL :: DCXX, DCXYYX, DCYY, FKD, FACK
    REAL :: VELNOFILT, VELFAC, DEPTH
    REAL :: CFLK(NK2,NTH), FKC(NTH), FKD0
    INTEGER :: IK, ITH, ISP, IY, IX
#ifdef W3_S
    CALL STRACE (IENT, 'PROP_FREQ_SHIFT')
#endif
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
        IF (B_JGS_LDIFR) THEN
          DSDD(IK) = MAX ( 0. , DIFRM(IP)*CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / DEPTH
        ELSE
          DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / DEPTH
        ENDIF
      ELSE
        DSDD(IK) = 0.
      END IF
    END DO
    DO IK=0, NK+1
      FKD0   = FKD / CG(IK,ISEA) * DSDD(IK)
      VELFAC =  FACK/DB(IK+1)
      DO ITH=1, NTH
        VELNOFILT = ( FKD0 + WN(IK,ISEA)*FKC(ITH) ) * VELFAC
        CFLK(IK+1,ITH) = VELNOFILT/VELFAC
      END DO
    END DO
    DO IK=1,NK
      DO ITH=1,NTH
        ISP=ITH + (IK-1)*NTH
        CAS(ISP)=DBLE(CFLK(IK,ITH))
      END DO
    END DO
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE PROP_FREQ_SHIFT
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Compute frequency shift alternative approach.
  !>
  !> @param[in]  IP
  !> @param[in]  ISEA
  !> @param[out] CWNB_M2
  !> @param[out] DWNI_M2
  !> @param[in]  DTG
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
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
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE CONSTANTS, ONLY : LPDLIB
    USE W3GDATMD, ONLY: NK, NK2, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
         EC2, ESC, ES2, FACHFA, MAPWN, FLCTH, FLCK,  &
         CTMAX, DMIN, DTH, MAPSF, B_JGS_LDIFR
    USE W3ADATMD, ONLY: CG, WN, DCXDX, DCXDY, DCYDX, DCYDY, CX, CY, DDDX, DDDY, DW
    USE W3ODATMD, only : IAPROC

    IMPLICIT NONE

    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif

    INTEGER, intent(in) :: ISEA, IP
    REAL, intent(out) :: CWNB_M2(1-NTH:NSPEC)
    REAL, intent(out) :: DWNI_M2(NK)
    REAL, intent(in) :: DTG
    !
    REAL :: eDCXDX, eDCXDY, eDCYDX, eDCYDY, eCX, eCY, eDDDX, EDDDY
    REAL :: DCXX, DCXYYX, DCYY, FKD, FACK
    REAL :: DEPTH
    REAL :: FKC(NTH), FKD0
    REAL :: VCWN(1-NTH:NSPEC+NTH)
    REAL :: DSDD(0:NK+1)
    REAL :: sumDiff, sumDiff1, sumDiff2, sumDiff3
    REAL :: sumDiff0, sumDiff4, sumDiff5
    INTEGER :: IK, ITH, ISP, IY, IX

    !/ ------------------------------------------------------------------- /
#ifdef W3_S
    CALL STRACE (IENT, 'PROP_FREQ_SHIFT_M2')
#endif

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

    DO ITH=1, NTH
      FKC(ITH) = EC2(ITH)*DCXX + ESC(ITH)*DCXYYX + ES2(ITH)*DCYY
    END DO
    !
    DEPTH  = MAX ( DMIN , DW(ISEA) )
    DO IK=0, NK+1
      IF ( DEPTH*WN(IK,ISEA) .LT. 5. ) THEN
        IF (B_JGS_LDIFR) THEN
          DSDD(IK) = MAX ( 0. , DIFRM(IP)*CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / DEPTH
        ELSE
          DSDD(IK) = MAX ( 0. , CG(IK,ISEA)*WN(IK,ISEA)-0.5*SIG(IK) ) / DEPTH
        ENDIF 
      ELSE
        DSDD(IK) = 0.
      END IF
    END DO
    ISP = -NTH
    DO IK=0, NK+1
      FKD0   = FKD / CG(IK,ISEA) * DSDD(IK)
      DO ITH=1, NTH
        ISP = ISP + 1
        VCWN(ISP) = FKD0 + WN(IK,ISEA)*FKC(ITH)
      END DO
    END DO

    sumDiff=0
    DO ISP=1-NTH,NSPEC
      CWNB_M2(ISP) = DBLE(0.5 * ( VCWN(ISP) + VCWN(ISP+NTH) ))
      sumDiff = sumDiff + MAX(CWNB_M2(ISP), ZERO)
    END DO
    DO IK=1,NK
      DWNI_M2(IK) = DBLE( CG(IK,ISEA) / DSIP(IK) )
    END DO
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE PROP_FREQ_SHIFT_M2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Sync global local arrays.
  !>
  !> @param[in] IMOD
  !> @param[in] IsMulti
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
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
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
#ifdef W3_PDLIB
    USE yowDatapool, only: istatus
    USE yowNodepool, only: np_global
    USE W3ODATMD, ONLY: NTPROC, NAPROC, IAPROC
    USE W3GDATMD, ONLY: MAPSF, NSEA
    USE W3ADATMD, ONLY: MPI_COMM_WAVE, MPI_COMM_WCMP
    USE yowRankModule, only : IPGL_TO_PROC, IPGL_tot
    USE WMMDATMD, ONLY: MDATAS
#endif
    IMPLICIT NONE
#ifdef W3_PDLIB
    INCLUDE "mpif.h"
#endif
    INTEGER, intent(in) :: IMOD
    logical, intent(in) :: IsMulti
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_PDLIB
    INTEGER :: Iarr(1)
    INTEGER :: ISEA, IP_glob
    INTEGER :: IPROC, IERR_MPI, istat
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'SYNCHRONIZE_IPGL_ETC_ARRAY')
#endif

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
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE SYNCHRONIZE_IPGL_ETC_ARRAY
  !/ ....................----------------------------------------------- /
  !>
  !> @brief Setup NSEAL, NSEALM in context of PDLIB.
  !>
  !> @param[out] NSEALout
  !> @param[out] NSEALMout
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
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
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    !/ ------------------------------------------------------------------- /
#ifdef W3_PDLIB
    use yowDatapool, only: istatus
    use yowNodepool, only: npa
    use yowRankModule, only : rank
    USE W3GDATMD, ONLY: GTYPE, UNGTYPE
#endif
#ifdef W3_MPI
    USE W3ADATMD, ONLY: MPI_COMM_WAVE, MPI_COMM_WCMP
#endif
    USE CONSTANTS, ONLY : LPDLIB
    USE W3GDATMD, ONLY: NSEA
    USE W3ODATMD, ONLY: NTPROC, NAPROC, IAPROC
    IMPLICIT NONE
    INTEGER, intent(out) :: NSEALout, NSEALMout
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'SET_UP_NSEAL_NSEALM')
#endif

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
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE SET_UP_NSEAL_NSEALM
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Set JSEA for all schemes.
  !>
  !> @param[in]  ISEA
  !> @param[out] JSEA
  !> @param[out] ISPROC
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
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
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
    USE W3GDATMD, ONLY: GTYPE, UNGTYPE, MAPSF
    USE CONSTANTS, ONLY : LPDLIB
#ifdef W3_PDLIB
    USE yowRankModule, only : IPGL_TO_PROC, IPGL_tot
    use yowNodepool, only: ipgl, iplg
#endif
    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    INTEGER, intent(in) :: ISEA
    INTEGER, intent(out) :: JSEA, ISPROC
    INTEGER IP_glob
#ifdef W3_S
    CALL STRACE (IENT, 'INIT_GET_JSEA_ISPROC')
#endif

#ifdef W3_PDLIB
    IF (.NOT. LPDLIB) THEN
#endif
      JSEA   = 1 + (ISEA-1)/NAPROC
      ISPROC = ISEA - (JSEA-1)*NAPROC
#ifdef W3_PDLIB
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
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE INIT_GET_JSEA_ISPROC
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Set belongings of JSEA in context of PDLIB.
  !>
  !> @param[in]  ISEA
  !> @param[out] JSEA
  !> @param[out] IBELONG
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
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
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
    USE W3GDATMD, ONLY: GTYPE, UNGTYPE, MAPSF
    USE CONSTANTS, ONLY : LPDLIB
#ifdef W3_PDLIB
    USE yowRankModule, only : IPGL_TO_PROC, IPGL_tot, IPGL_npa
    use yowNodepool, only: ipgl, iplg
#endif
    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
    INTEGER, intent(in) :: ISEA
    INTEGER, intent(out) :: JSEA, IBELONG
    INTEGER ISPROC, IX, JX
#ifdef W3_S
    CALL STRACE (IENT, 'GET_JSEA_IBELONG')
#endif
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
  !>
  !> @brief Set ISEA for all schemes.
  !>
  !> @param[out] ISEA
  !> @param[in]  JSEA
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
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
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
    USE W3GDATMD, ONLY: GTYPE, UNGTYPE
    USE CONSTANTS, ONLY : LPDLIB
#ifdef W3_PDLIB
    USE YOWNODEPOOL, ONLY: iplg
#endif
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !
    USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
    USE W3GDATMD, ONLY: GTYPE, UNGTYPE
    USE CONSTANTS, ONLY : LPDLIB
#ifdef W3_PDLIB
    USE YOWNODEPOOL, ONLY: iplg
#endif
    IMPLICIT NONE
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    INTEGER, intent(in) :: JSEA
    INTEGER, intent(out) :: ISEA
#ifdef W3_S
    CALL STRACE (IENT, 'INIT_GET_ISEA')
#endif
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

  !>
  !> @brief Sync global array in context of PDLIB.
  !>
  !> @details An array of size (NSEA) is send but only the (1:NSEAL) values
  !>          are correct. The program synchonizes everything on all nodes.
  !>
  !> @param[inout] TheVar
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE SYNCHRONIZE_GLOBAL_ARRAY(TheVar)
    !/ ------------------------------------------------------------------- /
    !**********************************************************************
    !*  An array of size (NSEA) is send but only the (1:NSEAL) values     *
    !*  are correct. The program synchonizes everything on all nodes.     *
    !**********************************************************************
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
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    USE W3GDATMD, ONLY: NSEAL, NSEA, NX
#ifdef W3_PDLIB
    USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
    USE W3ADATMD, ONLY: MPI_COMM_WCMP
    use yowDatapool, only: rtype, istatus
    USE yowNodepool, only: npa
    use yowNodepool, only: iplg
    use yowDatapool, only: rkind
#endif
    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_MPI
    INCLUDE "mpif.h"
#endif
    INTEGER ISEA, JSEA, Status(NX), rStatus(NX)
    INTEGER IPROC, I, ierr, IP, IX, IP_glob
#ifdef W3_PDLIB
    REAL(rkind), intent(inout) :: TheVar(NX)
    REAL(rkind) ::  rVect(NX)
#else
    DOUBLE PRECISION, intent(inout) :: TheVar(NX)
    DOUBLE PRECISION                ::  rVect(NX)
#endif
    Status=0
#ifdef W3_S
    CALL STRACE (IENT, 'SYNCHRONIZE_GLOBAL_ARRAY')
#endif
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
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE SYNCHRONIZE_GLOBAL_ARRAY
  !/ ------------------------------------------------------------------- /
#ifdef W3_PDLIB
  SUBROUTINE DIFFRA_SIMPLE(VA)
    USE W3GDATMD, only: ECOS, ESIN, DMIN, FLAGLL, NTH, SIG, NK, CLATS, DTH, NSEAL, DDEN
    USE W3ADATMD, only: CG, CX, CY, DW, WN, CG
    !USE W3WDATMD, only: VA
    USE CONSTANTS, ONLY: RADIUS
    USE yowNodepool, ONLY: np, npa
    IMPLICIT NONE
    REAL*4, INTENT(IN) :: VA(:,:)
    INTEGER :: IP, IK, ITH, IS, ID, ISP, ISEA, JSEA
    REAL(8) :: ETOT, EWKTOT, ECGTOT, EAD
    REAL(8) :: TMP, AUX, TRANS_X(NPA), TRANS_Y(NPA)
    REAL(8) :: EWK(NPA), ECG(NPA), ENG(NPA)
    REAL(8) :: CGK(NPA)
    REAL(8) :: DXENG(NPA), DYENG(NPA), DXXEN(NPA), DYYEN(NPA), DXYEN(NPA)
    REAL(8) :: DXCGK(NPA), DYCGK(NPA)

    if (.not. allocated(difrm) ) then
      allocate(difrm(np),difrx(np),difry(np))
      difrm = 0.
      difrx = 0.
      difry = 0. 
    endif 

    EWK = 0.d0
    ECG = 0.d0
    ENG = 0.d0
    CGK = 0.d0

    DO IP = 1, NP
      CALL INIT_GET_ISEA(ISEA, IP)
      IF (DW(IP) .GT. DMIN) THEN
        ETOT   = 0.d0 
        EWKTOT = 0.d0 
        ECGTOT = 0.d0
        EAD    = 0.d0 
        DO IS = 1, NK
          DO ID = 1, NTH
            ISP = ID + (IS-1) * NTH
            EAD = EAD + VA(ISP,IP) * DDEN(IS) / CG(IS,ISEA)!VA(ISP,IP)/CG(IS,ISEA)*CLATS(ISEA)*DTH*SIG(IS)**2
          ENDDO
          ETOT   = ETOT + EAD
          EWKTOT = EWKTOT + WN(IS,ISEA) * EAD
          ECGTOT = ECGTOT + CG(IS,ISEA) * EAD
        END DO
        IF (ETOT .GT. TINY(1.)) THEN
          EWK(IP) = EWKTOT / ETOT
          ECG(IP) = ECGTOT / ETOT
          ENG(IP) = SQRT(ETOT)
        ELSE
          EWK(IP) = 0.d0
          ECG(IP) = 0.d0
          ENG(IP) = 0.d0
        END IF
        IF (EWK(IP) .GT. TINY(1.)) THEN
          CGK(IP) = ECG(IP) / EWK(IP)
        ELSE
          CGK(IP) = 0.d0
        END IF
      ELSE
        EWK(IP) = 0.d0
        ECG(IP) = 0.d0
        ENG(IP) = 0.d0
        CGK(IP) = 0.d0
      END IF
    END DO

    CALL DIFFERENTIATE_XYDIR(ENG  , DXENG, DYENG)
    CALL DIFFERENTIATE_XYDIR(DXENG, DXXEN, DXYEN)
    CALL DIFFERENTIATE_XYDIR(DYENG, DXYEN, DYYEN)
    CALL DIFFERENTIATE_XYDIR(CGK  , DXCGK, DYCGK)

    IF (FLAGLL) THEN
      TRANS_X = 1.d0/(DEGRAD*RADIUS*CLATS(ISEA))
      TRANS_Y = 1.d0/(DEGRAD*RADIUS)
      DXENG = DXENG * TRANS_X
      DYENG = DYENG * TRANS_Y
      DXXEN = DXXEN * TRANS_X**2
      DYYEN = DYYEN * TRANS_Y**2
      DXCGK = DXCGK * TRANS_X
      DYCGK = DYCGK * TRANS_Y
    END IF

    DO IP = 1, NP
       IF (DW(IP) .GT. DMIN .AND. ENG(IP) .GT. TINY(1.)) THEN
         TMP = ECG(IP)*EWK(IP)*ENG(IP)
         IF (TMP > TINY(1.)) THEN
            AUX = DXCGK(IP)*DXENG(IP)+DYCGK(IP)*DYENG(IP)+CGK(IP)*(DXXEN(IP)+DYYEN(IP))
            TMP = AUX/TMP
         ELSE
            TMP = 0.d0
         END IF
         IF (TMP < -1.d0) THEN
            DIFRM(IP) = 1.d0
         ELSE
            DIFRM(IP) = SQRT(1.d0+TMP)
         END IF
       ELSE
         DIFRM(IP) = 1.d0
       END IF
       !WRITE(*,*) 'DIFRM', IP, DIFRM(IP)
    END DO

    CALL DIFFERENTIATE_XYDIR(DIFRM, DIFRX, DIFRY)

    IF (FLAGLL) THEN
      DIFRX = DIFRX * TRANS_X
      DIFRY = DIFRY * TRANS_Y
    END IF

    !IF (.FALSE.) THEN
    !  OPEN(555, FILE  = 'ergdiffr.bin'  , FORM = 'UNFORMATTED')
    !  WRITE(555) SNGL(RTIME)
    !  WRITE(555) (SNGL(DIFRX(IP)), SNGL(DIFRY(IP)),SNGL(DIFRM(IP))-1., IP = 1, NP)
    !END IF

    END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
    SUBROUTINE BOTEFCT( EWK, DFBOT )
      USE W3ADATMD, only: DW
      USE CONSTANTS, only: GRAV 
      USE YOWNODEPOOL, only: NP
      IMPLICIT NONE

      REAL(8), INTENT(IN)    :: EWK(NP)
      REAL(8), INTENT(OUT)   :: DFBOT(NP)

      REAL(8) :: SLPH(NP), CURH(NP)
      REAL(8) :: DXDEP(NP) , DYDEP(NP)
      REAL(8) :: DXXDEP(NP), DXYDEP(NP), DYYDEP(NP)

      REAL(8) :: KH, BOTFC, BOTFS

      INTEGER :: IP

      LOGICAL :: MY_ISNAN, MY_ISINF

!      CALL SMOOTH( -1.1, NP, XP, YP, DEP ) ! Add smoothing

      CALL DIFFERENTIATE_XYDIR(DBLE(DW), DXDEP ,  DYDEP)
      CALL DIFFERENTIATE_XYDIR(DXDEP , DXXDEP, DXYDEP)
      CALL DIFFERENTIATE_XYDIR(DYDEP , DXYDEP, DYYDEP)

      SLPH = DXDEP**2 + DYDEP**2
      CURH = DXXDEP   + DYYDEP

      DFBOT = 0.
      DO IP = 1, NP
        IF (EWK(IP) < TINY(1.)) CYCLE
        KH = EWK(IP)*DW(IP)
        IF (KH > PI) CYCLE
        CALL CALC_BOTFC(KH,BOTFC)
        CALL CALC_BOTFS(KH,BOTFS)
        DFBOT(IP) = (BOTFC*CURH(IP)+BOTFS*EWK(IP)*SLPH(IP))*GRAV
        IF (DFBOT(IP) .NE. DFBOT(IP)) THEN
          WRITE(*,*)'DFBOT is NaN', IP,KH, CURH(IP), BOTFS, BOTFC, EWK(IP), SLPH(IP)
          STOP
        END IF
      END DO
    END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
    SUBROUTINE CALC_BOTFC(KH,BOTFC)
      IMPLICIT NONE

      REAL*8, INTENT(OUT) :: BOTFC
      REAL*8, INTENT(IN)  :: KH
      REAL*8              :: DKH, AUX, AUX1
      REAL*8              :: COSHKH, COSH2KH, SINHKH, SINH2KH, SINH3KH

      SINHKH  = DSINH(MIN(KDMAX,KH))
      SINH2KH = DSINH(MIN(KDMAX,2*KH))
      SINH3KH = DSINH(MIN(KDMAX,3*KH))
      COSHKH  = DCOSH(MIN(KDMAX,KH))
      COSH2KH = DCOSH(MIN(KDMAX,2*KH))
      AUX = -4*KH*COSHKH+SINH3KH+SINHKH+8*(KH**2)*SINHKH
      AUX1 = 8*COSHKH**3*(2*KH+SINH2KH)
      BOTFC = AUX/MAX(TINY(1.),AUX1) - KH*DTANH(KH)/MAX(TINY(1.),(2*(COSHKH)**2))

      IF (BOTFC .NE. BOTFC) THEN
        WRITE(*,*)'BOTFC is NaN Aron', KH, AUX, AUX1, DTANH(KH)
        STOP
      END IF
    END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE CALC_BOTFS(KH,BOTFS)
        IMPLICIT NONE
        REAL(8), INTENT(OUT) :: BOTFS
        REAL(8), INTENT(IN)  :: KH
        REAL(8)              :: SECH, SINH2KH, SINHKH, COSH2KH
        REAL(8)              :: AUX, AUX1

        IF (DABS(DCOSH(KH)) > TINY(1.)) THEN
          SECH = 1.d0 / DCOSH(KH)
        ELSE
          SECH = 0.d0
        END IF

        SINHKH  = DSINH(MIN(KDMAX,KH))
        SINH2KH = DSINH(MIN(KDMAX,2.0_8*KH))
        COSH2KH = DCOSH(MIN(KDMAX,2.0_8*KH))
        AUX     = SECH**2/MAX(TINY(1.),(6.d0*(2*KH+SINH2KH)**3))
        AUX1    = 8.d0*(KH**4)+16.d0*(KH**3)*SINH2KH-9.d0*(SINH2KH**2*COSH2KH+12.d0*KH*(1.d0+2.d0*(SINHKH)**4)*(KH+SINH2KH))
        BOTFS   = AUX * AUX1

        IF (BOTFS .NE.  BOTFS) THEN
          WRITE(*,*)'BOTFS is NaN', BOTFS, AUX, AUX1
          STOP 
        ENDIF
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE CUREFCT( NP, DXENG, DYENG, CURT, DFCUR )
         IMPLICIT NONE

         INTEGER, INTENT(IN)    :: NP
         REAL(8), INTENT(IN)    :: DXENG(NP), DYENG(NP), CURT(NP,2)
         REAL(8), INTENT(INOUT) :: DFCUR(NP)

         REAL(8)                :: AUX(NP), AUXX(NP), AUXY(NP)
         REAL(8)                :: DXAUXX(NP), DYAUXY(NP)

         AUX(:)  = CURT(:,1) * DXENG(:) + CURT(:,2) * DYENG(:)
         AUXX(:) = AUX(:) * CURT(:,1)
         AUXY(:) = AUX(:) * CURT(:,2)

         CALL DIFFERENTIATE_XYDIR(AUXX, DXAUXX, AUX)
         CALL DIFFERENTIATE_XYDIR(AUXY, DYAUXY, AUX)

         DFCUR(:) = DXAUXX(:) + DYAUXY(:)
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE CALC_BOTFC2(KH,BOTFC2)
        IMPLICIT NONE
        REAL(8), INTENT(IN)  :: KH
        REAL(8), INTENT(OUT) :: BOTFC2
        REAL(8)              :: AUX, AUX1
        REAL(8)              :: SINHKH, COSHKH, SINH2KH, SINH3KH

        IF (KH .GT. 1.d0/TINY(1.)) THEN
          BOTFC2 = 0.d0
          RETURN
        END IF

        COSHKH  = DCOSH(MIN(KDMAX,KH))
        SINHKH  = DSINH(MIN(KDMAX,KH))
        SINH2KH = DSINH(MIN(KDMAX,2*KH))
        SINH3KH = DSINH(MIN(KDMAX,3*KH))

        AUX = -4.d0*KH*COSHKH + SINH3KH + SINHKH + 8.d0*(KH**2)*SINHKH
        AUX1 = 8.d0*COSHKH**3.d0*(2*KH + SINH2KH)
        BOTFC2 = AUX / MAX(TINY(1.),AUX1) - KH*DTANH(KH) / (2*(COSHKH)**2)

        IF (BOTFC2 .NE. BOTFC2) THEN
           WRITE(*,*) 'BOTFC2'
           WRITE(*,*) SINHKH, COSHKH, SINH2KH, SINH3KH, KH
           WRITE(*,*) AUX, AUX1, KH*DTANH(KH), (2*(COSHKH)**2)
           STOP 
        ENDIF
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE CALC_BOTFS2(KH,BOTFS2)
        IMPLICIT NONE
        REAL(8), INTENT(IN)  :: KH
        REAL(8), INTENT(OUT) :: BOTFS2
        REAL(8)              :: SECH
        REAL(8)              :: AUX, AUX1, SINHKH, COSHKH
        REAL(8)              :: SINH2KH, SINH3KH, COSH2KH

        IF (KH .GT. 1.d0/TINY(1.d0)) THEN
          BOTFS2 = 0.
          RETURN
        END IF

        COSHKH  = DCOSH(MIN(KDMAX,KH))
        COSH2KH = DCOSH(MIN(KDMAX,2*KH))
        SINHKH  = DSINH(MIN(KDMAX,KH))
        SINH2KH = DSINH(MIN(KDMAX,2*KH))
        SINH3KH = DSINH(MIN(KDMAX,3*KH))

        SECH = 1.d0 / MAX(TINY(1.),COSHKH)
        !WRITE(*,*) '1st part', KH, COSHKH, COSH2KH
        !WRITE(*,*) '2nd part', SINHKH, SINH2KH, SINH3KH
        !WRITE(*,*) '3rd part', SECH**2, MAX(TINY(1.), (6.d0*(2*KH + SINH2KH)**3))
        AUX = SECH**2 / MAX(TINY(1.), (6.d0*(2*KH + SINH2KH)**3))
        IF (AUX .GT. TINY(1.)) THEN
          AUX1 = 8.d0*(KH**4.d0) + 16.d0*(KH**3)*SINH2KH - 9.d0*(SINH2KH)**2*COSH2KH + 12.d0*KH*(1.d0 + 2*SINHKH**4)*(KH + SINH2KH)
        ELSE
          AUX1 = 0.d0
        END IF

        BOTFS2 = AUX * AUX1

        IF (BOTFS2 .NE. BOTFS2) THEN
          WRITE(*,*) 'BOTFS2'
          WRITE(*,*) COSHKH, COSH2KH, SINHKH, SINH2KH, SINH3KH
          WRITE(*,*) AUX, AUX1, KH, SECH, SECH**2, COSHKH
          STOP 
        END IF
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE CUREFCT2( NP, DXENG, DYENG, CX, CY, DFCUR )
         IMPLICIT NONE
         INTEGER, INTENT(IN)    :: NP
         REAL(8), INTENT(IN)    :: DXENG(NP), DYENG(NP), CX(NP), CY(NP)
         REAL(8), INTENT(OUT)   :: DFCUR(NP)
         REAL(8)                :: AUX(NP), AUXX(NP), AUXY(NP)
         REAL(8)                :: DXAUXX(NP), DYAUXY(NP)

         AUX  = CY * DXENG + CY * DYENG
         AUXX = AUX * CX
         AUXY = AUX * CY

         CALL DIFFERENTIATE_XYDIR(AUXX, DXAUXX, AUX)
         CALL DIFFERENTIATE_XYDIR(AUXY, DYAUXY, AUX)

         DFCUR(:) = DXAUXX(:) + DYAUXY(:)
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE BOTEFCT2(NP, EWK, DEP, DFBOT)
        USE W3ADATMD, only: DW        
        IMPLICIT NONE
        REAL(8), PARAMETER     :: GRAV = 9.81
        INTEGER, INTENT(IN)    :: NP
        REAL(8), INTENT(IN)    :: EWK(NP)
        REAL(8), INTENT(IN)    :: DEP(NP)
        REAL(8), INTENT(INOUT) :: DFBOT(NP)
        REAL(8)                :: SLPH(NP), CURH(NP)
        REAL(8)                :: DXDEP(NP), DYDEP(NP)
        REAL(8)                :: DXXDEP(NP), DXYDEP(NP), DYYDEP(NP)
        REAL(8)                :: KH
        INTEGER                :: IP
        REAL(8)                :: BOTFC2, BOTFS2

        CALL DIFFERENTIATE_XYDIR(DBLE(DW), DXDEP,  DYDEP)
        CALL DIFFERENTIATE_XYDIR(DXDEP, DXXDEP, DXYDEP)
        CALL DIFFERENTIATE_XYDIR(DYDEP, DXYDEP, DYYDEP)

        SLPH(:) = DXDEP(:)**2 + DYDEP(:)**2
        CURH(:) = DXXDEP(:) + DYYDEP(:)

        DO IP = 1, NP
          KH = EWK(IP)*DW(IP)
          !WRITE(*,*) 'DEPTH', IP, DW(IP), EWK(IP)
          CALL CALC_BOTFS2(KH,BOTFS2)
          CALL CALC_BOTFC2(KH,BOTFC2)
          DFBOT(IP) = (BOTFC2*CURH(IP)+BOTFS2*EWK(IP)*SLPH(IP))*GRAV
          IF (DFBOT(IP) .NE. DFBOT(IP)) THEN
            WRITE(*,*) 'DFBOT'
            WRITE(*,*) DFBOT(IP), BOTFC2, BOTFS2, CURH(IP), EWK(IP), SLPH(IP)
            STOP 
          ENDIF
        END DO
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE DIFFRA_EXTENDED(VA)
         USE W3GDATMD, only: ECOS, ESIN, DMIN, NTH, SIG, NK, CLATS, DTH, DSII, DDEN
         USE W3ADATMD, only: CG, CX, CY, DW
         !USE W3WDATMD, only: VA
         USE W3DISPMD, ONLY : WAVNU3
         USE W3IDATMD, ONLY: FLCUR
         USE YOWNODEPOOL, ONLY: NP
         IMPLICIT NONE
         REAL*4, INTENT(IN) :: VA(:,:)
         INTEGER :: IP, IK, ITH, ISEA, JSEA, IS, ID, ISP
         REAL :: WVC, WVK, WVCG, WVKDEP, WVN
         REAL(8) :: ETOT, EWKTOT, EWCTOT, ECGTOT, EAD
         REAL(8) :: DFWAV
         REAL(8) :: AUX
         REAL(8) :: DELTA
         REAL(8) :: EWK(NP), EWC(NP), ECG(NP), ENG(NP)
         REAL(8) :: CCG(NP)
         REAL(8) :: DXENG(NP), DYENG(NP), DXXEN(NP), DYYEN(NP), DXYEN(NP)
         REAL(8) :: DXCCG(NP), DYCCG(NP)
         REAL(8) :: DFCUR(NP)
         REAL(8) :: DFBOT(NP)
         REAL(8) :: DFCUT
         REAL(8) :: ETOTC, ETOTS, DM
         REAL(8) :: US
         REAL(8) :: CAUX, CAUX2
         REAL(8) :: NAUX
         REAL:: DEPTH 

         if (.not. allocated(difrm) ) then
           allocate(difrm(np),difrx(np),difry(np))
           difrm = 0.
           difrx = 0.
           difry = 0.
         endif

         DFBOT(:) = 0.d0 
         DFCUR(:) = 0.d0 

         DO JSEA = 1, NP
           CALL INIT_GET_ISEA(ISEA, JSEA)
           ETOT = 0.d0 
           EWKTOT = 0.d0
           EWCTOT = 0.d0
           ECGTOT = 0.d0
           DEPTH = DW(ISEA)
           IF (DEPTH .GT. DMIN) THEN
             DO IS = 1, NK
               CALL WAVNU3(SIG(IS),DEPTH,WVK,WVCG,WVC)
               EAD = 0.d0 
               DO ID = 1, NTH 
                 ISP = ID + (IS-1) * NTH
                 EAD = EAD + VA(ISP,JSEA) * DDEN(IS) / CG(IS,ISEA)
               ENDDO 
               ETOT   = ETOT   + EAD
               EWKTOT = EWKTOT + WVK  * EAD
               EWCTOT = EWCTOT + WVC  * EAD
               ECGTOT = ECGTOT + WVCG * EAD
!AR: todo: check integration!
             END DO
             IF (ETOT .LT. TINY(1.)) THEN
               EWK(JSEA) = 0.d0
               EWC(JSEA) = 0.d0
               ECG(JSEA) = 0.d0
               ENG(JSEA) = 0.d0
               CCG(JSEA) = 0.d0
             ELSE
               EWK(JSEA) = EWKTOT / ETOT
               EWC(JSEA) = EWCTOT / ETOT
               ECG(JSEA) = ECGTOT / ETOT
               ENG(JSEA) = SQRT(ETOT)
               CCG(JSEA) = EWC(JSEA) * ECG(JSEA)
             END IF
           ELSE
             EWK(JSEA) = 0.d0 
             EWC(JSEA) = 0.d0 
             ECG(JSEA) = 0.d0 
             ENG(JSEA) = 0.d0 
             CCG(JSEA) = 0.d0
           END IF
         END DO

         CALL DIFFERENTIATE_XYDIR(ENG, DXENG, DYENG)
         CALL DIFFERENTIATE_XYDIR(DXENG, DXXEN, DXYEN)
         CALL DIFFERENTIATE_XYDIR(DYENG, DXYEN, DYYEN)
         CALL DIFFERENTIATE_XYDIR(CCG, DXCCG, DYCCG)
         CALL BOTEFCT2( NP, EWK, DBLE(DW), DFBOT )

         IF (FLCUR) CALL CUREFCT2( NP, DXENG, DYENG, DBLE(CX), DBLE(CY), DFCUR )

         DO JSEA = 1, NP  
            CALL INIT_GET_ISEA(ISEA, JSEA)
            AUX = CCG(JSEA)*EWK(JSEA)*EWK(JSEA)
            IF ( AUX*ENG(JSEA) .GT. TINY(1.)) THEN
               DFWAV = ( DXCCG(JSEA) * DXENG(JSEA) + DYCCG(JSEA) * DYENG(JSEA) + CCG(JSEA) * (DXXEN(JSEA) + DYYEN(JSEA)) ) / MAX(TINY(1.),ENG(JSEA))
               NAUX = ECG(JSEA) / MAX(TINY(1.),EWC(JSEA))
               IF (FLCUR) THEN
                 ETOTC = 0.d0 
                 ETOTS = 0.d0 
                 DO ID = 1, NTH
                   EAD = 0.d0 
                   DO IK = 1, NK 
                     ISP = ID + (ISP-1) * NTH
                     EAD = EAD + VA(ISP,IP) * DDEN(IS) / CG(IS,ISEA)
                   ENDDO 
                   ETOTC = ETOTC + EAD * ECOS(ITH)
                   ETOTS = ETOTS + EAD * ESIN(ITH) 
                 END DO
                 DM    = ATAN2(ETOTS,ETOTC)
                 US    = CX(JSEA)*COS(DM)+CY(JSEA)*SIN(DM)
                 CAUX  = US / EWC(JSEA)
                 DFCUT = (2.d0/NAUX+NAUX*CAUX)*CAUX
               ELSE
                 DFCUT = 0.d0 
                 CAUX  = 0.d0 
               END IF
               CAUX2 = CAUX * CAUX
               DELTA = CAUX2*(1.d0+CAUX)**2-NAUX*(CAUX2-NAUX)*(1.d0+(DFWAV+DFBOT(JSEA)+DFCUR(JSEA))/AUX+DFCUT)
               IF (DELTA <= 0.d0) THEN
                 DIFRM(JSEA) = 1.d0 
               ELSE
                 DIFRM(JSEA) = 1.d0/(CAUX2-NAUX)*(CAUX*(1.d0+CAUX)-SQRT(DELTA))
               END IF
            ELSE
               DIFRM(JSEA) = 1.d0 
            END IF
         END DO
         CALL DIFFERENTIATE_XYDIR(DIFRM, DIFRX, DIFRY)
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE DIFFERENTIATE_XYDIR(VAR, DVDX, DVDY)
         USE W3GDATMD, only: ECOS, ESIN, DMIN, NTH, SIG, NK
         USE W3ADATMD, only: CG, CX, CY, DW
         USE yowExchangeModule, only : PDLIB_exchange1DREAL
         USE yowNodepool, only : PDLIB_IEN, PDLIB_TRIA, NP, NPA
         USE yowElementpool, only : NE, INE
         IMPLICIT NONE
         REAL*8, INTENT(IN)    :: VAR(NPA)
         REAL*8, INTENT(INOUT) :: DVDX(NPA), DVDY(NPA)
         REAL*4                :: DVDX4(NPA), DVDY4(NPA)
         REAL*8                :: DEDY(3),DEDX(3)
         REAL*8                :: DVDXIE, DVDYIE
         REAL*8                :: WEI(NPA)
         INTEGER               :: NI(3)
         INTEGER               :: IE, JSEA

         WEI(:)  = 0.d0
         DVDX(:) = 0.d0
         DVDY(:) = 0.d0

#ifdef DEBUG
         WRITE(STAT%FHNDL,*) 'DIFFERENTIATE_XYDIR'
         WRITE(STAT%FHNDL,*) 'sum(VAR ) = ', sum(VAR)
         WRITE(STAT%FHNDL,*) 'sum(IEN ) = ', sum(IEN)
         WRITE(STAT%FHNDL,*) 'sum(TRIA) = ', sum(TRIA)
#endif

         DO IE = 1, NE
            NI = INE(:,IE)
            WEI(NI) = WEI(NI) + 2.*PDLIB_TRIA(IE)
            DEDX(1) = PDLIB_IEN(1,IE)
            DEDX(2) = PDLIB_IEN(3,IE)
            DEDX(3) = PDLIB_IEN(5,IE)
            DEDY(1) = PDLIB_IEN(2,IE)
            DEDY(2) = PDLIB_IEN(4,IE)
            DEDY(3) = PDLIB_IEN(6,IE)
            DVDXIE  = DOT_PRODUCT( VAR(NI),DEDX)
            DVDYIE  = DOT_PRODUCT( VAR(NI),DEDY)
            DVDX(NI) = DVDX(NI) + DVDXIE
            DVDY(NI) = DVDY(NI) + DVDYIE
         END DO

         DVDX = DVDX/WEI
         DVDY = DVDY/WEI

         DO JSEA = 1, NP
           IF (DW(JSEA) .LT. DMIN) THEN
             DVDX(JSEA) = 0.
             DVDY(JSEA) = 0.
           END IF
         END DO

         DVDX4 = DVDX
         DVDY4 = DVDY
         CALL PDLIB_exchange1DREAL(DVDX4) ! AR: todo, checck pipes.
         CALL PDLIB_exchange1DREAL(DVDY4)
         DVDX = DVDX4
         DVDY = DVDY4

#ifdef DEBUG
         WRITE(STAT%FHNDL,*) 'sum(DVDX) = ', sum(DVDX)
         WRITE(STAT%FHNDL,*) 'sum(DVDY) = ', sum(DVDY)
#endif

         IF (.FALSE.) THEN
           OPEN(2305, FILE  = 'erggrad.bin'  , FORM = 'UNFORMATTED')
           WRITE(2305) 1.
           WRITE(2305) (DVDX(JSEA), DVDY(JSEA), SQRT(DVDY(JSEA)**2+DVDY(JSEA)**2), JSEA = 1, NP)
         ENDIF
      END SUBROUTINE
#endif
END MODULE W3PARALL
!/ ------------------------------------------------------------------- /
