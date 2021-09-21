#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3UPDTMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    21-Jan-2000 : Origination.                        ( version 2.00 )
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    02-Apr-2001 : Adding sub-grid obstacles.          ( version 2.10 )
!/    18-May-2001 : Clean up and bug fixes.             ( version 2.11 )
!/    11-Jan-2002 : Sub-grid ice.                       ( version 2.15 )
!/    30-Apr-2002 : Water level fixes.                  ( version 2.20 )
!/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
!/    26-Dec-2002 : Moving grid wind correction.        ( version 3.02 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    15-Jul-2005 : Adding MAPST2.                      ( version 3.07 )
!/    07-Sep-2005 : Upgrading W3UBPT.                   ( version 3.08 )
!/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    11-Jan-2007 : Clean-up W3UTRN boundary points.    ( version 3.10 )
!/    11-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    17-Aug-2010 : ABPI0-N(:,0) init. bug fix.         ( version 3.14 )
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    05-Apr-2011 : Place holder for XGR in UNGTYPE     ( version 4.04 ) 
!/                  (A. Roland/F. Ardhuin)
!/    13-Mar-2012 : Add initialization of UST on re-    ( version 4.07 )
!/                  activation of grid point.
!/    06-Jun-2012 : Porting bugfixes from 3.14 to 4.07  ( version 4.07 )
!/    12-Jun-2012 : Add /RTD option or rotated grid option. 
!/                  (Jian-Guo Li)                       ( version 4.07 )
!/    26-Sep-2012 : Adding update from tidal analysis   ( version 4.08 )
!/                  (F. Ardhuin)
!/    16-Sep-2013 : Add Arctic part for SMC grid.       ( version 4.11 )
!/    11-Nov-2013 : SMC and rotated grid incorporated in the main 
!/                  trunk                               ( version 4.13 )
!/    13-Nov-2013 : Moved reflection from ww3_grid.ftn  ( version 4.13 )
!/    27-May-2014 : Ading OMPG parallelizations dir,    ( version 5.02 )
!/    08-May-2014 : Implement tripolar grid for first order propagation
!/                  scheme                              ( version 5.03 )
!/                  (W. E. Rogers, NRL)
!/    27-Aug-2015 : New function to update variables    ( version 5.08 )
!/                  ICEF and ICEDMAX at the first time step
!/                  and add ICEH initialization in W3UICE.
!/    13-Jan-2016 : Changed initial value of ICEDMAX    ( version 5.08 )
!/    26-Mar-2018 : Sea-point only Wnd/Cur input.  JGLi ( version 6.04 )
!/    07-Oct-2019 : RTD option with standard lat-lon
!/                  grid when nesting to rotated grid   ( version 7.11 )
!/    22-Mar-2021 : Add W3UTAU, W3URHO routines         ( version 7.13 )
!/    06-May-2021 : Use ARCTC option for SMC grid. JGLi ( version 7.13 )
!/
!/    Copyright 2009-2014 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Bundles all input updating routines for WAVEWATCH III.
!
!  2. Variables and types :
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3UCUR    Subr. Public   Update current fields.
!      W3UWND    Subr. Public   Update wind fields.
!      W3UTAU    Subr. Public   Update atmospheric momentum fields.
!      W3UINI    Subr. Public   Update initial conditions.
!      W3UBPT    Subr. Public   Update boundary conditions.
!      W3UICE    Subr. Public   Update ice concentrations.
!      W3ULEV    Subr. Public   Update water levels.
!      W3URHO    Subr. Public   Update air density.
!      W3UTRN    Subr. Public   Update cell boundary transparancies.
!      W3DZXY    Subr. Public   Calculate derivatives of a field.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      DSEC21    Func. W3TIMEMD Difference in time.
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      EXTCDE    Subr. W3SERVMD Exit program with error code.
!      PRTBLK    Subr. W3ARRYMD Print plot output.
!      PRT2DS    Subr. W3ARRYMD Print plot output.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!  6. Switches :
!
!       !/SHRD   Switch for shared / distributed memory architecture.
!       !/DIST   Id.
!
!       !/OMPG   OpenMP compiler directives.
!
!       !/CRT0   No current interpolation.
!       !/CRT1   Linear current interpolation.
!       !/CRT2   Quasi-quadratic current interpolation.
!
!       !/WNT0   No wind/momentum interpolation.
!       !/WNT1   Linear wind/momentum interpolation.
!       !/WNT2   Energy conservation in wind/momentum interpolation.
!
!       !/RWND   Use wind speeds relative to currents.
!
!       !/STAB2  Calculate effective wind speed factor for stability
!                to be used with !/ST2.
!
!       !/S      Enable subroutine tracing.
!       !/Tn     Test output
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
      USE W3ODATMD, ONLY: NDSE, NDST, NAPROC, IAPROC, NAPERR
#ifdef W3_S
      USE W3SERVMD, ONLY : STRACE
#endif
      USE W3TIMEMD, ONLY: DSEC21
!/
!/ ------------------------------------------------------------------- /
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UCUR ( FLFRST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         15-Dec-2004 |
!/                  +-----------------------------------+
!/
!/    09-Dec-1996 : Final FORTRAN 77                    ( version 1.18 )
!/    20-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    27-Aug-2015 : Rename DT0,DTT by DT0T,DT0N         ( version 5.10 )
!/    23-Mar-2016 : SMC grid Arctic part adjustment.    ( version 5.18 )
!/    26-Mar-2018 : Sea-point only current on SMC grid. ( version 6.02 )
!/
!  1. Purpose :
!
!     Interpolate the current field to the present time.
!
!  2. Method :
!
!     Linear interpolation of speed and direction, with optionally
!     a correction to get approximate quadratic interpolation of speed
!     only.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       FLFRST  Log.  I   Flag for first pass through routine.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     None.
!
!  7. Remarks :
!
!     - Only currents at sea points are considered.
!     - Time ranges checked in W3WAVE.
!     - Currents are stored by components to save on the use of
!       SIN and COS functions. The actual interpolations, however
!       are by absolute value and direction.
!
!  8. Structure :
!
!     --------------------------------------
!      1.  Prepare auxiliary arrays.
!      2.  Calculate interpolation factors.
!      3.  Get actual winds.
!     --------------------------------------
!
!  9. Switches :
!
!     !/CRT0  No current interpolation.
!     !/CRT1  Linear current interpolation.
!     !/CRT2  Quasi-quadratic current interpolation.
!
!     !/S     Enable subroutine tracing.
!     !/T     Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSF
#ifdef W3_SMC
      USE W3GDATMD, ONLY: NARC, NGLO, ANGARC 
      USE W3GDATMD, ONLY: FSWND, ARCTC 
#endif
      USE W3WDATMD, ONLY: TIME
      USE W3ADATMD, ONLY: CX, CY, CA0, CAI, CD0, CDI
      USE W3IDATMD, ONLY: TC0, CX0, CY0, TCN, CXN, CYN
#ifdef W3_TIDE
      USE W3GDATMD, ONLY: YGRD
      USE W3TIMEMD
      USE W3IDATMD, ONLY: FLCURTIDE, CXTIDE, CYTIDE, NTIDE
      USE W3TIDEMD
#endif
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      LOGICAL, INTENT(IN)     :: FLFRST
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: ISEA, IX, IY
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      REAL                    :: D0, DN, DD, DT0N, DT0T, RD, CABS, CDIR
#ifdef W3_CRT2
      REAL                    :: RD2, CI2
#endif
#ifdef W3_TIDE
      INTEGER          :: J,K
      INTEGER(KIND=4)  :: TIDE_KD0, INT24, INTDYS       ! "Gregorian day constant"
      REAL             :: WCURTIDEX, WCURTIDEY, TIDE_ARGX, TIDE_ARGY
      REAL(KIND=8)     :: d1,h,TIDE_HOUR,HH,pp,s,p,enp,dh,dpp,ds,dp,dnp,tau
      REAL             :: FX(44),UX(44),VX(44)
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3UCUR')
#endif
!
! 1.  Prepare auxiliary arrays
!
      IF ( FLFRST ) THEN
          DO ISEA=1, NSEA
#ifdef W3_SMC
 !!Li  For sea-point SMC grid current, the 1-D current is stored on
 !!Li  2-D CX0(NSEA, 1) variable. 
        IF( FSWND ) THEN
            IX = ISEA 
            IY = 1
        ELSE
#endif
            IX        = MAPSF(ISEA,1)
            IY        = MAPSF(ISEA,2)
#ifdef W3_SMC
        ENDIF
#endif

            CA0(ISEA) = SQRT ( CX0(IX,IY)**2 + CY0(IX,IY)**2 )
            CAI(ISEA) = SQRT ( CXN(IX,IY)**2 + CYN(IX,IY)**2 )
            IF ( CA0(ISEA) .GT. 1.E-7) THEN
                D0     = MOD ( TPI+ATAN2(CY0(IX,IY),CX0(IX,IY)) , TPI )
              ELSE
                D0     = 0
              END IF
            IF ( CAI(ISEA) .GT. 1.E-7) THEN
                DN     = MOD ( TPI+ATAN2(CYN(IX,IY),CXN(IX,IY)) , TPI )
              ELSE
                DN     = D0
              END IF
            IF ( CA0(ISEA) .GT. 1.E-7) THEN
                CD0(ISEA) = D0
              ELSE
                CD0(ISEA) = DN
              END IF
            DD     = DN - CD0(ISEA)
            IF (ABS(DD).GT.PI) DD = DD - TPI*SIGN(1.,DD)
            CDI(ISEA) = DD
            CAI(ISEA) = CAI(ISEA) - CA0(ISEA)
            END DO
        END IF
!
! 2.  Calculate interpolation factor
!
      DT0N    = DSEC21 ( TC0, TCN )
      DT0T    = DSEC21 ( TC0, TIME )
!
#ifdef W3_CRT0
      RD     = 0.
#endif
#ifdef W3_CRT1
      RD     = DT0T / MAX ( 1.E-7 , DT0N )
#endif
#ifdef W3_CRT2
      RD     = DT0T / MAX ( 1.E-7 , DT0N )
      RD2    = 1. - RD
#endif
#ifdef W3_OASOCM
     RD     = 1.
#endif
!
#ifdef W3_T
      WRITE (NDST,9000) DT0N, DT0T, RD
#endif

#ifdef W3_TIDE
        IF (FLCURTIDE) THEN 
!          WRITE(6,*) 'TIME CUR:',TIME, '##',TC0, '##',TCN
          TIDE_HOUR = TIME2HOURS(TIME)
!
!*  THE ASTRONOMICAL ARGUMENTS ARE CALCULATED BY LINEAR APPROXIMATION
!*  AT THE MID POINT OF THE ANALYSIS PERIOD.
          d1=TIDE_HOUR/24.d0
          TIDE_KD0= 2415020
          d1=d1-dfloat(TIDE_kd0)-0.5d0
          call astr(d1,h,pp,s,p,enp,dh,dpp,ds,dp,dnp)
          INT24=24
          INTDYS=int((TIDE_HOUR+0.00001)/INT24)
          HH=TIDE_HOUR-dfloat(INTDYS*INT24)
          TAU=HH/24.D0+H-S
        END IF
!
!  ONLY THE FRACTIONAL PART OF A SOLAR DAY NEED BE RETAINED FOR COMPU-
!  TING THE LUNAR TIME TAU.
!
#endif

!
! 3.  Actual currents for all grid points
!
      DO ISEA=1, NSEA
#ifdef W3_TIDE
        IF (FLCURTIDE) THEN  ! could move IF test outside of ISEA loop ... 
! VUF should only be updated in latitude changes significantly ...
          IX        = MAPSF(ISEA,1)
          IY        = MAPSF(ISEA,2)
          CALL SETVUF_FAST(h,pp,s,p,enp,dh,dpp,ds,dp,dnp,tau,YGRD(IY,IX),FX,UX,VX)
          WCURTIDEX = CXTIDE(IX,IY,1,1)
          WCURTIDEY = CYTIDE(IX,IY,1,1)
 
            DO J=2,TIDE_MF
              TIDE_ARGX=(VX(J)+UX(J))*twpi-CXTIDE(IX,IY,J,2)*DERA
              TIDE_ARGY=(VX(J)+UX(J))*twpi-CYTIDE(IX,IY,J,2)*DERA
              WCURTIDEX = WCURTIDEX+FX(J)*CXTIDE(IX,IY,J,1)*COS(TIDE_ARGX)
              WCURTIDEY = WCURTIDEY+FX(J)*CYTIDE(IX,IY,J,1)*COS(TIDE_ARGY)                    
              END DO
     
#endif

#ifdef W3_TIDET
 !Verification 
         IF (ISEA.EQ.1) THEN 

            TIDE_AMPC(1:NTIDE,1)=CXTIDE(IX,IY,1:NTIDE,1)
            TIDE_PHG(1:NTIDE,1 )=CXTIDE(IX,IY,1:NTIDE,2)
            TIDE_AMPC(1:NTIDE,2)=CYTIDE(IX,IY,1:NTIDE,1)
            TIDE_PHG(1:NTIDE,2) =CYTIDE(IX,IY,1:NTIDE,2)

              WRITE(993,'(A,F20.2,13F8.3)') 'TEST ISEA 0:',    &
                          d1,H,S,TAU,pp,s,p,enp,dh,dpp,ds,dp,dnp,YGRD(IY,IX)

              DO J=1,TIDE_MF
                WRITE(993,'(A,4I9,F12.0,3F8.3,I4,X,A)') 'TEST ISEA 1:',IX,J,TIME,TIDE_HOUR,    &
                            FX(J),UX(J),VX(J),TIDE_INDEX2(J),TIDECON_ALLNAMES(TIDE_INDEX2(J))
                END DO
          DO K=1,2
            DO J=1,TIDE_MF
              WRITE(993,'(A,5I9,F12.0,5F8.3)') 'TEST ISEA 2:',IX,K,J,TIME,TIDE_HOUR,    &
                         FX(J),UX(J),VX(J),TIDE_AMPC(J,K),TIDE_PHG(J,K)
             END DO
             END DO

            WRITE(993,'(A,2F8.4,A,2F8.4)') '#:',CX0(IX,IY),CY0(IX,IY),'##',WCURTIDEX,WCURTIDEY
            CLOSE(993)
           END IF
 ! End of verification
#endif
#ifdef W3_TIDE
            CX(ISEA) = WCURTIDEX
            CY(ISEA) = WCURTIDEY
          ELSE 
#endif

        CABS    = CA0(ISEA) + RD * CAI(ISEA)
#ifdef W3_CRT2
        CI2      = SQRT ( RD2 *      CA0(ISEA)**2 +             &
                          RD  *(CA0(ISEA)+CAI(ISEA))**2 )
        CABS    = CABS * MIN( 1.25 , CI2/MAX(1.E-7,CABS) )
#endif
        CDIR    = CD0(ISEA) + RD * CDI(ISEA)

#ifdef W3_SMC
  !Li   Rotate curreent direction by ANGARC for Arctic part cells.  JGLi23Mar2016
        IF( ARCTC .AND. (ISEA .GT. NGLO) ) THEN
            DN = CDIR + ANGARC( ISEA - NGLO )*DERA
            CDIR = MOD ( TPI + DN, TPI )
        ENDIF
#endif

        CX(ISEA) = CABS * COS(CDIR)
        CY(ISEA) = CABS * SIN(CDIR)
#ifdef W3_TIDE
  !        IF (ISEA.EQ.1)  WRITE(6,'(A,4F8.4,A,4F8.4)') 'CUR#:',RD,CA0(ISEA),CAI(ISEA),CABS,'##', &
  !                                      CX(ISEA), CY(ISEA),WCURTIDEX, WCURTIDEY        
          END IF
#endif
!
        END DO
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT (' TEST W3UCUR : DT0N, DT0T, RD :',2F8.1,F6.3)
#endif
!/
!/ End of W3UCUR ----------------------------------------------------- /
!/
      END SUBROUTINE W3UCUR
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UWND ( FLFRST, VGX, VGY )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         27-May-2014 |
!/                  +-----------------------------------+
!/
!/    03-Dec-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    20-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
!/    26-Dec-2002 : Moving grid wind correction.        ( version 3.02 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    16-Sep-2013 : Rotating wind for Arctic part.      ( version 4.11 )
!/    27-May-2014 : Adding OMPG parallelizations dir.   ( version 5.02 )
!/    27-Aug-2015 : Rename DT0,DTT by DT0T,DT0N         ( version 5.10 )
!/    26-Mar-2018 : Sea-point only wind for SMC grid.   ( version 6.07 )
!/
!  1. Purpose :
!
!     Interpolate wind fields to the given time.
!
!  2. Method :
!
!     Linear interpolation of wind speed and direction, with a simple
!     correction to obtain quasi-conservation of energy.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       FLFRST  Log.  I   Flag for first pass through routine.
!       VGX/Y   Real  I   Grid velocity                      (!/MGW)
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!     - Only winds over sea points are considered.
!     - Time ranges checked in W3WAVE.
!
!  8. Structure :
!
!     --------------------------------------
!      1.  Prepare auxiliary arrays.
!      2.  Calculate interpolation factors
!      3.  Get actual winds
!      4.  Correct for currents
!      5.  Convert to stresses
!      6.  Stability correction
!     --------------------------------------
!
!  9. Switches :
!
!     !/OMPG   OpenMP compiler directives.
!
!     !/WNT0   No wind interpolation.
!     !/WNT1   Linear wind interpolation.
!     !/WNT2   Energy conservation in wind interpolation.
!
!     !/RWND   Use wind speeds relative to currents.
!     !/MGW    Moving grid wind correction.
!
!     !/STAB2  Calculate effective wind speed factor for stability
!              to be used with !/ST2.
!
!     !/S      Enable subroutine tracing.
!     !/T      Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSF
#ifdef W3_WCOR
      USE W3GDATMD, ONLY:   WWCOR
#endif
#ifdef W3_RWND
      USE W3GDATMD, ONLY:   RWINDC
#endif
#ifdef W3_ST2
      USE W3GDATMD, ONLY: ZWIND, OFSTAB, FFNG, FFPS, CCNG, CCPS, SHSTAB
#endif
#ifdef W3_SMC
      USE W3GDATMD, ONLY: NARC, NGLO, ANGARC, ARCTC, FSWND 
#endif
      USE W3WDATMD, ONLY: TIME, ASF
      USE W3ADATMD, ONLY: DW, CX, CY, UA, UD, U10, U10D, AS,          &
                          UA0, UAI, UD0, UDI, AS0, ASI
      USE W3IDATMD, ONLY: TW0, WX0, WY0, DT0, TWN, WXN, WYN, DTN, FLCUR
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(IN)        :: VGX, VGY
      LOGICAL, INTENT(IN)     :: FLFRST
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: ISEA, IX, IY
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      REAL                    :: D0, DN, DD, DT0N, DT0T, RD, UI2,      &
                                 UXR, UYR
#ifdef W3_WNT2
      REAL                    :: RD2
#endif
#ifdef W3_STAB2
      REAL                    :: STAB0, STAB, THARG1, THARG2, COR1, COR2
#endif
      REAL                    :: UDARC
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3UWND')
#endif
!
! 1.  Prepare auxiliary arrays
!
      IF ( FLFRST ) THEN
          DO ISEA=1, NSEA
#ifdef W3_SMC
 !!Li  For sea-point only SMC grid wind 1-D wind is stored on
 !!Li  2-D WX0(NSEA, 1) variable.
        IF( FSWND ) THEN
            IX = ISEA 
            IY = 1
        ELSE
#endif
            IX        = MAPSF(ISEA,1)
            IY        = MAPSF(ISEA,2)
#ifdef W3_SMC
        ENDIF
#endif

            UA0(ISEA) = SQRT ( WX0(IX,IY)**2 + WY0(IX,IY)**2 )
            UAI(ISEA) = SQRT ( WXN(IX,IY)**2 + WYN(IX,IY)**2 )
            IF ( UA0(ISEA) .GT. 1.E-7) THEN
                D0     = MOD ( TPI+ATAN2(WY0(IX,IY),WX0(IX,IY)) , TPI )
              ELSE
                D0     = 0
              END IF
            IF ( UAI(ISEA) .GT. 1.E-7) THEN
                DN     = MOD ( TPI+ATAN2(WYN(IX,IY),WXN(IX,IY)) , TPI )
              ELSE
                DN     = D0
              END IF
            IF ( UA0(ISEA) .GT. 1.E-7) THEN
                UD0(ISEA) = D0
              ELSE
                UD0(ISEA) = DN
              END IF
            DD     = DN - UD0(ISEA)
            IF (ABS(DD).GT.PI) DD = DD - TPI*SIGN(1.,DD)
            UDI(ISEA) = DD
            UAI(ISEA) = UAI(ISEA) - UA0(ISEA)
            AS0(ISEA) = DT0(IX,IY)
            ASI(ISEA) = DTN(IX,IY) - DT0(IX,IY)
            END DO
        END IF
!
! 2.  Calculate interpolation factor
!
      DT0N    = DSEC21 ( TW0, TWN )
      DT0T   = DSEC21 ( TW0, TIME )
!
#ifdef W3_WNT0
      RD     = 0.
#endif
#ifdef W3_WNT1
      RD     = DT0T / MAX ( 1.E-7 , DT0N )
#endif
#ifdef W3_WNT2
      RD     = DT0T / MAX ( 1.E-7 , DT0N )
      RD2    =  1. - RD
#endif
#ifdef W3_OASACM
     RD     = 1.
#endif
!
#ifdef W3_T
      WRITE (NDST,9000) DT0N, DT0T, RD
#endif
!
! 3.  Actual wind for all grid points
!
#ifdef W3_OMPG
!$OMP PARALLEL DO PRIVATE (ISEA,UI2,UXR,UYR,UDARC)
#endif
!
      DO ISEA=1, NSEA
!
        UA(ISEA) = UA0(ISEA) + RD * UAI(ISEA)
#ifdef W3_WNT2
        UI2      = SQRT ( RD2 *      UA0(ISEA)**2 +             &
                          RD  *(UA0(ISEA)+UAI(ISEA))**2 )
        UA(ISEA) = UA(ISEA) * MIN(1.25,UI2/MAX(1.E-7,UA(ISEA)))
#endif
        UD(ISEA) = UD0(ISEA) + RD * UDI(ISEA)
#ifdef W3_MGW
        UXR        = UA(ISEA)*COS(UD(ISEA)) + VGX
        UYR        = UA(ISEA)*SIN(UD(ISEA)) + VGY
        UA(ISEA) = MAX ( 0.001 , SQRT(UXR**2+UYR**2) )
        UD(ISEA) = MOD ( TPI+ATAN2(UYR,UXR) , TPI )
#endif
#ifdef W3_SMC
  !Li   Rotate wind direction by ANGARC for Arctic part cells.
        IF( ARCTC .AND. (ISEA .GT. NGLO) ) THEN
            UDARC = UD(ISEA) + ANGARC( ISEA - NGLO )*DERA
            UD(ISEA) = MOD ( TPI + UDARC, TPI )
        ENDIF
#endif
!
        AS(ISEA) = AS0(ISEA) + RD * ASI(ISEA)
!        IF (UA(ISEA).NE.UA(ISEA)) WRITE(6,*) 'BUG WIND:',ISEA,UA(ISEA),MAPSF(ISEA,1), MAPSF(ISEA,2),UA0(ISEA),RD,UAI(ISEA)
!        IF (UD(ISEA).NE.UD(ISEA)) WRITE(6,*) 'BUG WIN2:',ISEA,UD(ISEA),MAPSF(ISEA,1), MAPSF(ISEA,2)
!
        END DO
!
#ifdef W3_OMPG
!$OMP END PARALLEL DO
#endif
!
! 3.b  Bias correction ( !/WCOR )
#ifdef W3_WCOR
      WHERE ( UA .GE. WWCOR(1) ) UA = UA+(UA-WWCOR(1))*WWCOR(2)
#endif

!
! 4.  Correct for currents and grid motion
!
#ifdef W3_RWND
      IF ( FLCUR ) THEN
#endif
!
#ifdef W3_RWND
          DO ISEA=1, NSEA
            UXR        = UA(ISEA)*COS(UD(ISEA)) - RWINDC*CX(ISEA)
            UYR        = UA(ISEA)*SIN(UD(ISEA)) - RWINDC*CY(ISEA)
            U10 (ISEA) = MAX ( 0.001 , SQRT(UXR**2+UYR**2) )
            U10D(ISEA) = MOD ( TPI+ATAN2(UYR,UXR) , TPI )
            END DO
#endif
!
#ifdef W3_RWND
        ELSE
#endif
!
#ifdef W3_OMPG
!$OMP PARALLEL DO PRIVATE (ISEA)
#endif
!
          DO ISEA=1, NSEA
            U10 (ISEA) = MAX ( UA(ISEA) , 0.001 )
            U10D(ISEA) = UD(ISEA)
            END DO
!
#ifdef W3_OMPG
!$OMP END PARALLEL DO
#endif
!
#ifdef W3_RWND
        END IF
#endif
!
! 5.  Stability correction ( !/STAB2 )
!     Original settings :
!
!     SHSTAB =    1.4
!     OFSTAB =   -0.01
!     CCNG   =   -0.1
!     CCPS   =    0.1
!     FFNG   = -150.
!     FFPS   =  150.
!
#ifdef W3_STAB2
      STAB0  = ZWIND * GRAV / 273.
#endif
!
#ifdef W3_STAB2
      DO ISEA=1, NSEA
        STAB   = STAB0 * AS(ISEA) / MAX(5.,U10(ISEA))**2
        STAB   = MAX ( -1. , MIN ( 1. , STAB ) )
#endif
!
#ifdef W3_STAB2
        THARG1 = MAX ( 0. , FFNG*(STAB-OFSTAB))
        THARG2 = MAX ( 0. , FFPS*(STAB-OFSTAB))
        COR1   = CCNG * TANH(THARG1)
        COR2   = CCPS * TANH(THARG2)
#endif
!
#ifdef W3_STAB2
        ASF(ISEA) = SQRT ( (1.+COR1+COR2)/SHSTAB )
        U10(ISEA) = U10(ISEA) / ASF(ISEA)
        END DO
#endif
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT (' TEST W3UWND : DT0N, DT0T, RD :',2F8.1,F6.3)
#endif
!/
!/ End of W3UWND ----------------------------------------------------- /
!/
      END SUBROUTINE W3UWND
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UTAU ( FLFRST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           J. M. Castillo          |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    22-Mar-2021 : First implementation                ( version 7.13 )
!/
!  1. Purpose :
!
!     Interpolate atmosphere momentum fields to the given time.
!
!  2. Method :
!
!     Linear interpolation of momentum module and direction, with a simple
!     correction to obtain quasi-conservation of energy.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       FLFRST  Log.  I   Flag for first pass through routine.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!     - Only momentum over sea points is considered.
!     - Time ranges checked in W3WAVE.
!
!  8. Structure :
!
!     --------------------------------------
!      1.  Prepare auxiliary arrays.
!      2.  Calculate interpolation factors
!      3.  Get actual momentum
!     --------------------------------------
!
!  9. Switches :
!
!     !/OMPG   OpenMP compiler directives.
!
!     !/WNT0   No momentum interpolation.
!     !/WNT1   Linear momentum interpolation.
!     !/WNT2   Energy conservation in momentum interpolation.
!
!     !/S      Enable subroutine tracing.
!     !/T      Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NSEA, MAPSF
#ifdef W3_SMC
      USE W3GDATMD, ONLY: NARC, NGLO, ANGARC 
      USE W3GDATMD, ONLY: FSWND, ARCTC  
#endif
      USE W3WDATMD, ONLY: TIME
      USE W3ADATMD, ONLY: TAUA, TAUADIR, MA0, MAI, MD0, MDI
      USE W3IDATMD, ONLY: TU0, UX0, UY0, TUN, UXN, UYN
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      LOGICAL, INTENT(IN)     :: FLFRST
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: ISEA, IX, IY
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      REAL                    :: D0, DN, DD, DT0N, DT0T, RD, MI2,      &
                                 MXR, MYR
#ifdef W3_WNT2
      REAL                    :: RD2
#endif
      REAL                    :: MDARC
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3UTAU')
#endif
!
! 1.  Prepare auxiliary arrays
!
      IF ( FLFRST ) THEN
          DO ISEA=1, NSEA
#ifdef W3_SMC
 !!Li  For sea-point only SMC grid momentum 1-D momentum is stored on
 !!Li  2-D UX0(NSEA, 1) variable.
        IF( FSWND ) THEN
            IX = ISEA 
            IY = 1
        ELSE
#endif
            IX        = MAPSF(ISEA,1)
            IY        = MAPSF(ISEA,2)
#ifdef W3_SMC
        ENDIF
#endif

            MA0(ISEA) = SQRT ( UX0(IX,IY)**2 + UY0(IX,IY)**2 )
            MAI(ISEA) = SQRT ( UXN(IX,IY)**2 + UYN(IX,IY)**2 )
            IF ( MA0(ISEA) .GT. 1.E-7) THEN
                D0     = MOD ( TPI+ATAN2(UY0(IX,IY),UX0(IX,IY)) , TPI )
              ELSE
                D0     = 0
              END IF
            IF ( MAI(ISEA) .GT. 1.E-7) THEN
                DN     = MOD ( TPI+ATAN2(UYN(IX,IY),UXN(IX,IY)) , TPI )
              ELSE
                DN     = D0
              END IF
            IF ( MA0(ISEA) .GT. 1.E-7) THEN
                MD0(ISEA) = D0
              ELSE
                MD0(ISEA) = DN
              END IF
            DD     = DN - MD0(ISEA)
            IF (ABS(DD).GT.PI) DD = DD - TPI*SIGN(1.,DD)
            MDI(ISEA) = DD
            MAI(ISEA) = MAI(ISEA) - MA0(ISEA)
            END DO
        END IF
!
! 2.  Calculate interpolation factor
!
      DT0N    = DSEC21 ( TU0, TUN )
      DT0T   = DSEC21 ( TU0, TIME )
!
#ifdef W3_WNT0
      RD     = 0.
#endif
#ifdef W3_WNT1
      RD     = DT0T / MAX ( 1.E-7 , DT0N )
#endif
#ifdef W3_WNT2
      RD     = DT0T / MAX ( 1.E-7 , DT0N )
      RD2    =  1. - RD
#endif
#ifdef W3_OASACM
     RD     = 1.
#endif
!
#ifdef W3_T
      WRITE (NDST,9000) DT0N, DT0T, RD
#endif
!
! 3.  Actual momentum for all grid points
!
#ifdef W3_OMPG
!$OMP PARALLEL DO PRIVATE (ISEA,MI2,MXR,MYR,MDARC)
#endif
!
      DO ISEA=1, NSEA
!
        TAUA(ISEA) = MA0(ISEA) + RD * MAI(ISEA)
#ifdef W3_WNT2
        MI2      = SQRT ( RD2 *      MA0(ISEA)**2 +             &
                          RD  *(MA0(ISEA)+MAI(ISEA))**2 )
        TAUA(ISEA) = TAUA(ISEA) * MIN(1.25,MI2/MAX(1.E-7,TAUA(ISEA)))
#endif
        TAUADIR(ISEA) = MD0(ISEA) + RD * MDI(ISEA)
#ifdef W3_SMC
  !Li   Rotate momentum direction by ANGARC for Arctic part cells.
        IF( ARCTC .AND. (ISEA .GT. NGLO) ) THEN
            MDARC = TAUADIR(ISEA) + ANGARC( ISEA - NGLO )*DERA
            TAUADIR(ISEA) = MOD ( TPI + MDARC, TPI )
        ENDIF
#endif
!
        END DO
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT (' TEST W3UTAU : DT0N, DT0T, RD :',2F8.1,F6.3)
#endif
!/
!/ End of W3UTAU ----------------------------------------------------- /
!/
      END SUBROUTINE W3UTAU
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UINI ( A )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Jun-2018 |
!/                  +-----------------------------------+
!/
!/    19-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    20-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    18-May-2001 : Fix CG declaration.                 ( version 2.11 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/    06-Jun-2018 : use W3PARALL and INIT_GET_ISEA      ( version 6.04 )
!/
!  1. Purpose :
!
!     Initialize the wave field with fetch-limited spectra before the
!     actual calculation start. (Named as an update routine due to
!     placement in code.)
!
!  2. Method :
!
!     Fetch-limited JONSWAP spectra with a cosine^2 directional
!     distribution and a mean direction taken from the wind.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       A       R.A.   O   Action density spectra.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!     - Wind speeds filtered by U10MIN and U10MAX (DATA statements)
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!
!       !/S     Enable subroutine tracing.
!       !/T     General test output.
!       !/T1    Parameters at grid points.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSF,                 &
                          NK, NTH, TH, SIG, DTH, DSIP, UNGTYPE,       &
                          RLGTYPE, CLGTYPE, GTYPE, FLAGLL,            &
                          HPFAC, HQFAC
      USE W3ADATMD, ONLY: U10, U10D, CG
      USE W3PARALL, only : INIT_GET_JSEA_ISPROC, INIT_GET_ISEA
      USE W3PARALL, only : GET_JSEA_IBELONG
#ifdef W3_T
      USE W3ARRYMD, ONLY : PRTBLK
#endif
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(OUT)       :: A(NTH,NK,0:NSEAL)
!/
!/ ------------------------------------------------------------------- /
!/ Local variables
!/
      INTEGER                 :: IX, IY, ISEA, JSEA, IK, ITH, ISPROC
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T
      INTEGER                 :: IX0, IXN, MAPOUT(NX,NY)
      INTEGER                 :: NXP = 60
#endif
      REAL                    :: ALFA(NSEAL), FP(NSEAL), YLN(NSEAL),  &
                                 AA, BB, CC
      REAL                    :: XGR, U10C, U10DIR, XSTAR, FSTAR,     &
                                 GAMMA, FR, D1(NTH), D1INT, F1, F2
      REAL                    :: ETOT, E1I
      REAL                    :: U10MIN =  1.
      REAL                    :: U10MAX = 20.
#ifdef W3_T
      REAL                    :: HSIG(NX,NY)
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3UINI')
#endif
!
!
! Pre-process JONSWAP data for all grid points ----------------------- *
!
#ifdef W3_T1
      WRITE (NDST,9010)
#endif
!
!  this is not clear what is going on betwen w3init and this ... 
      A(:,:,:)=0
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        IF (GTYPE.EQ.UNGTYPE) THEN 
          XGR=1.  ! to be fixed later 
        ELSE
          IX     = MAPSF(ISEA,1)
          IY     = MAPSF(ISEA,2)
          XGR    = 0.5 * SQRT(HPFAC(IY,IX)**2+HQFAC(IY,IX)**2)
          END IF
        IF ( FLAGLL ) THEN
            XGR    = XGR * RADIUS * DERA
          END IF
!
        U10C   = MAX ( MIN(U10(ISEA),U10MAX) , U10MIN )
!
        XSTAR  = GRAV * XGR / U10C**2
        FSTAR  = 3.5 / XSTAR**(0.33)
        GAMMA  = MAX ( 1. , 7.0 / XSTAR**(0.143) )
!
        ALFA(JSEA) = 0.076 / XSTAR**(0.22)
        FP  (JSEA) = FSTAR * GRAV / U10C
        YLN (JSEA) = LOG ( GAMMA )
!
#ifdef W3_T1
        WRITE (NDST,9011) ISEA, U10C, XSTAR,                      &
                          ALFA(JSEA), FP(JSEA), GAMMA
#endif
!
        END DO
!
! 1-D spectrum at location ITH = NTH --------------------------------- *
!
      DO IK=1, NK
        FR     = SIG(IK) * TPIINV
        DO JSEA=1, NSEAL
!
!/ ----- INLINED EJ5P (REDUCED) -------------------------------------- /
!
          AA     = ALFA(JSEA) * 0.06175/FR**5
          BB     = MAX( -50. , -1.25*(FP(JSEA)/FR)**4 )
          CC     = MAX( -50. , -0.5*((FR-FP(JSEA))/(0.07*FP(JSEA)))**2 )
          A(NTH,IK,JSEA)                                              &
                 = AA * EXP(BB + EXP(CC) * YLN(JSEA))
!
!/ ----- INLINED EJ5P (END) ------------------------------------------ /
!
          END DO
        END DO
!
! Apply directional distribution ------------------------------------- *
!
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        U10DIR = U10D(ISEA)
        D1INT  = 0.
!
        DO ITH=1, NTH
          D1(ITH) = ( MAX ( 0. , COS(TH(ITH)-U10DIR) ) )**2
          D1INT   = D1INT + D1(ITH)
          END DO
!
        D1INT  = D1INT * DTH
        F1     = TPIINV / D1INT
!
        DO IK=1, NK
          F2     = F1 * A(NTH,IK,JSEA) * CG(IK,ISEA) / SIG(IK)
          DO ITH=1, NTH
            A(ITH,IK,JSEA) = F2 * D1(ITH)
            END DO
          END DO
!
        END DO
!
! Test output -------------------------------------------------------- *
!
#ifdef W3_T
      HSIG   = 0.
      MAPOUT = 0
#endif
!
#ifdef W3_T
      DO ISEA=IAPROC, NSEA, NAPROC
        JSEA   = 1 + (ISEA-1)/NAPROC
        ETOT   = 0.
        DO IK=1, NK
          E1I    = 0.
          DO ITH=1, NTH
            E1I    = E1I + A(ITH,IK,JSEA)
            END DO
          ETOT   = ETOT + E1I * DSIP(IK) * SIG(IK) / CG(IK,ISEA)
          END DO
        IX            = MAPSF(ISEA,1)
        IY            = MAPSF(ISEA,2)
        HSIG  (IX,IY) = 4. * SQRT ( ETOT * DTH )
        MAPOUT(IX,IY) = 1
        END DO
#endif
!
#ifdef W3_T
      IX0    = 1
      DO
        IXN    = MIN ( NX , IX0+NXP-1 )
        CALL PRTBLK (NDST, NX, NY, NX, HSIG, MAPOUT, 0, 0.,        &
                     IX0, IXN, 1, 1, NY, 1, 'Hs', 'm')
        IF ( IXN .EQ. NX ) EXIT
        IX0    = IX0 + NXP
        END DO
#endif
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT (' TEST W3UINI : XGR = ',E10.3)
#endif
!
#ifdef W3_T1
 9010 FORMAT (' TEST W3UINI :  ISEA, U10C, XSTAR, ALPHA, FP, GAMMA')
 9011 FORMAT ('   ',I6,F8.2,F10.1,2F6.3,F6.2)
#endif
!/
!/ End of W3UINI ----------------------------------------------------- /
!/
      END SUBROUTINE W3UINI
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UBPT
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Jun-2018 |
!/                  +-----------------------------------+
!/
!/    19-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    20-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    07-Sep-2005 : Moving update to end of time step.  ( version 3.08 )
!/    17-Aug-2010 : Add initialization ABPI0-N(:,0).  ( version 3.14.5 )
!/    12-Jun-2012 : Add /RTD option or rotated grid option. 
!/                  (Jian-Guo Li)                       ( version 4.06 )
!/    06-Jun-2018 : Add DEBUGIOBC/SETUP/DEBUGW3ULEV     ( version 6.04 )
!/    13-Jun-2019 : Rotation only if POLAT<90 (C.Hansen)( version 7.11 ) 
!/
!  1. Purpose :
!
!     Update spectra at the active boundary points.
!
!  2. Method :
!
!     Spectra are read and interpolated in space and time from the
!     data read by W3IOBC.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!       STRACE, DSEC21
!                Service routines.
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!     - The data arrays contain sigma spectra to assure conservation
!       when changing grids.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S     Enable subroutine tracing.
!     !/T0    Test output of wave heights.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NSPEC, MAPWN, SIG2, DDEN
#ifdef W3_RTD
 !!   Use rotation angle and action conversion sub.  JGLi12Jun2012
      USE W3GDATMD, ONLY: NK, NTH, NSPEC, AnglD, PoLat
      USE W3SERVMD, ONLY: W3ACTURN
#endif
      USE W3ADATMD, ONLY: CG
      USE W3ODATMD, ONLY: NBI, ABPI0, ABPIN, ISBPI, IPBPI, RDBPI,     &
                          BBPI0, BBPIN
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: IBI, ISP, ISEA
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T0
      REAL                    :: HS1, HS2
#endif
#ifdef W3_RTD
 !!    Declare a temporary spectr variable.  JGLi12Jun2012 
       REAL :: Spectr(NSPEC), AnglBP
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3UBPT')
#endif
!
! 1.  Process BBPI0 -------------------------------------------------- *
! 1.a First intialization

#ifdef W3_DEBUGIOBC
     WRITE(740+IAPROC,*) 'Beginning of W3UBPT'
     FLUSH(740+IAPROC)
#endif

!
      IF ( BBPI0(1,0) .EQ. -1. ) THEN
!
          BBPI0(:,0) = 0.
          BBPIN(:,0) = 0.
          ABPI0(:,0) = 0.
          ABPIN(:,0) = 0.
!
          DO IBI=1, NBI
            ISEA   = ISBPI(IBI)
            DO ISP=1, NSPEC
              BBPI0(ISP,IBI) = CG(MAPWN(ISP),ISEA) / SIG2(ISP) *      &
                           ( RDBPI(IBI,1) * ABPI0(ISP,IPBPI(IBI,1))   &
                           + RDBPI(IBI,2) * ABPI0(ISP,IPBPI(IBI,2))   &
                           + RDBPI(IBI,3) * ABPI0(ISP,IPBPI(IBI,3))   &
                           + RDBPI(IBI,4) * ABPI0(ISP,IPBPI(IBI,4)) )
              END DO
            END DO
!
! 1.b Shift BBPIN
!
        ELSE
          BBPI0 = BBPIN
        END IF
!
! 2.  Process BBPIN -------------------------------------------------- *
!
      DO IBI=1, NBI
        ISEA   = ISBPI(IBI)
        DO ISP=1, NSPEC
          BBPIN(ISP,IBI) = CG(MAPWN(ISP),ISEA) / SIG2(ISP) *          &
                       ( RDBPI(IBI,1) * ABPIN(ISP,IPBPI(IBI,1))       &
                       + RDBPI(IBI,2) * ABPIN(ISP,IPBPI(IBI,2))       &
                       + RDBPI(IBI,3) * ABPIN(ISP,IPBPI(IBI,3))       &
                       + RDBPI(IBI,4) * ABPIN(ISP,IPBPI(IBI,4)) )
          END DO
!
#ifdef W3_RTD
 !!  Rotate the spectra if model is on rotated grid.  JGLi12Jun2012
 !!  PoLat == 90. if the grid is standard lat/lon (C. Hansen 20190613)
        IF ( PoLat < 90. ) THEN
          Spectr = BBPIN(:,IBI)
          AnglBP = AnglD(ISEA)
          CALL  W3ACTURN( NTH, NK, AnglBP, Spectr )
          BBPIN(:,IBI) = Spectr
        END IF

#endif
!
        END DO

! 3.  Wave height test output ---------------------------------------- *
!
#ifdef W3_T0
      WRITE (NDST,9000)
      DO IBI=1, NBI
        HS1    = 0.
        HS2    = 0.
        DO ISP=1, NSPEC
          HS1    = HS1 + BBPI0(ISP,IBI) * DDEN(MAPWN(ISP)) /       &
                                          CG(MAPWN(ISP),ISBPI(IBI))
          HS2    = HS2 + BBPIN(ISP,IBI) * DDEN(MAPWN(ISP)) /       &
                                          CG(MAPWN(ISP),ISBPI(IBI))
          END DO
        HS1    = 4. * SQRT ( HS1 )
        HS2    = 4. * SQRT ( HS2 )
        WRITE (NDST,9001) IBI, ISBPI(IBI), HS1, HS2
        END DO
#endif
!
#ifdef W3_DEBUGIOBC
     WRITE(740+IAPROC,*) 'sum(abs(ABPI0))=', sum(abs(ABPI0))
     WRITE(740+IAPROC,*) 'sum(abs(ABPIN))=', sum(abs(ABPIN))
     WRITE(740+IAPROC,*) 'sum(abs(BBPI0))=', sum(abs(BBPI0))
     WRITE(740+IAPROC,*) 'sum(abs(BBPIN))=', sum(abs(BBPIN))
     WRITE(740+IAPROC,*) 'End of W3UBPT'
     FLUSH(740+IAPROC)
#endif
      RETURN
!
! Formats
!
#ifdef W3_T0
 9000 FORMAT ( ' TEST W3UBPT : WAVE HEIGHTS BBPI0/N (NO TAIL)')
 9001 FORMAT ( '         ',2I8,2X,2F8.2)
#endif

!/
!/ End of W3UBPT ----------------------------------------------------- /
!/
      END SUBROUTINE W3UBPT
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UIC1( FLFRST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           C. Sevigny              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         27-Aug-2015 |
!/                  +-----------------------------------+
!/
!/    27-Aug-2015 : Creation                            ( version 5.10 )   
!/
!  1. Purpose :
!
!     Update ice thickness in the wave model.
!
!  2. Method :
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!      FLFRST   L.    I     Spectra in 1-D or 2-D representation
!                           (points to same address).
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     None.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/SHRD  Switch for shared / distributed memory architecture.
!     !/DIST  Id.
!
!     !/S  Enable subroutine tracing.
!     !/T  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /   
      USE W3GDATMD, ONLY: NSEA, NSEA, MAPSF, IICEHMIN, IICEHFAC
      USE W3WDATMD, ONLY: TIME, TIC1, ICEH
      USE W3IDATMD, ONLY: TI1, ICEP1, FLIC1
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
      LOGICAL, INTENT(IN)     :: FLFRST
!/    
!/ ------------------------------------------------------------------- /
!/ Local variables
!/      
      INTEGER                 :: IX, IY, ISEA
!/
!/
! 1.  Preparations --------------------------------------------------- *
! 1.a Update times
!
#ifdef W3_T
      WRITE (NDST,9010) TIME, TIC1, TI1
#endif
      TIC1(1) = TI1(1)
      TIC1(2) = TI1(2)

! 2.  Main loop over sea points -------------------------------------- *

      DO ISEA=1, NSEA
!
        IX        = MAPSF(ISEA,1)
        IY        = MAPSF(ISEA,2)
        ICEH(ISEA) = MAX(IICEHMIN,IICEHFAC*ICEP1(IX,IY))
      END DO
!
      RETURN
#ifdef W3_T
 9010 FORMAT ( ' TEST W3UIC1 : TIME     :',I9.8,I7.6/              &
               '               OLD TICE :',I9.8,I7.6/              &
               '               NEW TICE :',I9.8,I7.6)
#endif
!/
!/ End of W3UIC1 ----------------------------------------------------- /
!/
      END SUBROUTINE W3UIC1
!/ ------------------------------------------------------------------- /  
      SUBROUTINE W3UIC5( FLFRST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |  C. Sevigny  & F. Ardhuin         |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         13-Jan-2016 |
!/                  +-----------------------------------+
!/
!/    27-Aug-2015 : Creation                            ( version 5.08 )   
!/    13-Jan-2016 : Changed initial value of ICEDMAX    ( version 5.08 )   
!/
!  1. Purpose :
!
!     Update ice floe mean and max diameters in the wave model.
!
!  2. Method :
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!      FLFRST   L.    I     Spectra in 1-D or 2-D representation
!                           (points to same address).
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     None.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/SHRD  Switch for shared / distributed memory architecture.
!     !/DIST  Id.
!
!     !/S  Enable subroutine tracing.
!     !/T  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /   
      USE W3IDATMD, ONLY: TI5, ICEP5
      USE W3GDATMD, ONLY: NSEA, MAPSF
      USE W3WDATMD, ONLY: TIME, TIC5, ICE, ICEH, ICEF, ICEDMAX
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
      LOGICAL, INTENT(IN)     :: FLFRST
!/    
!/
!/ ------------------------------------------------------------------- /
!/ Local variables
!/      
      INTEGER                 :: IX, IY, ISEA
      LOGICAL                 :: FLFLOE
!/
!/
! 1.  Preparations --------------------------------------------------- *
! 1.a Update times
!
#ifdef W3_T
      WRITE (NDST,9010) TIME, TIC5, TI5
#endif
      TIC5(1) = TI5(1)
      TIC5(2) = TI5(2)

! 2.  Main loop over sea points -------------------------------------- *

      DO ISEA=1, NSEA 
!
        IX        = MAPSF(ISEA,1)
        IY        = MAPSF(ISEA,2)
        FLFLOE = ICE(ISEA) .EQ. 0 .OR. ICEH(ISEA) .EQ. 0
        IF ( FLFLOE) THEN          
          ICEF(ISEA) = 0.0
          ICEDMAX(ISEA) = 1000.0
        ELSE
          ICEF(ISEA) = ICEP5(IX,IY)
          ICEDMAX(ISEA) = ICEP5(IX,IY)          
        END IF
      END DO
!
      RETURN
#ifdef W3_T
 9010 FORMAT ( ' TEST W3UIC5 : TIME     :',I9.8,I7.6/              &
               '               OLD TICE :',I9.8,I7.6/              &
               '               NEW TICE :',I9.8,I7.6)
#endif

!/
!/
!/ End of W3UIC5 ----------------------------------------------------- /
!/
      END SUBROUTINE W3UIC5
!/ ------------------------------------------------------------------- /      
      
      SUBROUTINE W3UICE ( A, VA )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         28-Mar-2014 |
!/                  +-----------------------------------+
!/
!/    19-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    20-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    11-Jan-2002 : Sub-grid ice.                       ( version 2.15 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    28-Jun-2005 : Adding MAPST2.                      ( version 3.07 )
!/                  Taking out initilization.
!/    11-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    15-May-2010 : Adding second field for icebergs    ( version 3.14 )
!/    13-Mar-2012 : Add initialization of UST on re-    ( version 4.07 )
!/                  activation of grid point.
!/    06-Jun-2012 : Porting bugfixes from 3.14 to 4.07  ( version 4.07 )
!/    28-Mar-2014 : Adapting to ICx source terms        ( version 4.18 )
!/
!  1. Purpose :
!
!     Update ice map in the wave model.
!
!  2. Method :
!
!     Points with an ice concentration larger than FICEN are removed
!     from the sea map in the wave model. Such points are identified
!     by negative numbers is the grid status map MAPSTA. For ice
!     points spectra are set to zero. Points from wich ice disappears
!     are initialized with a "small" JONSWAP spectrum, based on the
!     frequency SIG(NK-1) and the local wind direction.
!
!     In the case of icebergs, the iceberg attenuation coefficient is
!     added to the subgrid obstruction map.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!      (V)A     R.A.  I/O   Spectra in 1-D or 2-D representation
!                           (points to same address).
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     None.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/SHRD  Switch for shared / distributed memory architecture.
!     !/DIST  Id.
!
!     !/S  Enable subroutine tracing.
!     !/T  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSF, MAPSTA, MAPST2, &
                          NTH, NK, NSPEC, SIG, TH, DTH, FICEN
      USE W3WDATMD, ONLY: TIME, TICE, ICE, BERG, UST
!!    USE W3ADATMD, ONLY: U10, U10D, CG
      USE W3ADATMD, ONLY: CG
      USE W3IDATMD, ONLY: TIN, ICEI, BERGI
      USE W3PARALL, only : INIT_GET_JSEA_ISPROC, INIT_GET_ISEA
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(INOUT)     :: A(NTH,NK,0:NSEAL), VA(NSPEC,0:NSEAL)
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: IK, ITH, ISEA, JSEA, IX, IY, ISP
#ifdef W3_S
      INTEGER, SAVE            :: IENT = 0
#endif
      INTEGER                 :: MAPICE(NY,NX), ISPROC
      LOGICAL                 :: LOCAL
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3UICE')
#endif
!
      LOCAL   = IAPROC .LE. NAPROC
!
#ifdef W3_T
      WRITE (NDST,9000) FICEN
      IF ( .NOT. LOCAL ) WRITE (NDST,9001)
#endif
!
! 1.  Preparations --------------------------------------------------- *
! 1.a Update times
!
#ifdef W3_T
      WRITE (NDST,9010) TIME, TICE, TIN
#endif
      TICE(1) = TIN(1)
      TICE(2) = TIN(2)
!
! 1.b Process maps
!
#ifdef W3_IC0
      MAPICE = MOD(MAPST2,2)
      MAPST2 = MAPST2 - MAPICE
#endif
!
! 2.  Main loop over sea points -------------------------------------- *
!
      DO ISEA=1, NSEA
!
! 2.a Get grid counters
!
        IX        = MAPSF(ISEA,1)
        IY        = MAPSF(ISEA,2)
        ICE(ISEA) = ICEI(IX,IY)
        BERG(ISEA)= BERGI(IX,IY)
!
! 2.b Sea point to be de-activated..
!
#ifdef W3_IC0
        IF ( ICEI(IX,IY).GE.FICEN .AND. MAPICE(IY,IX).EQ.0 ) THEN
            MAPSTA(IY,IX) = - ABS(MAPSTA(IY,IX))
            MAPICE(IY,IX) = 1
#endif
!AR: Take care here situation is not totally clear!
#ifdef W3_IC0
            CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
            IF (LOCAL .AND. (IAPROC .eq. ISPROC)) THEN
#endif
#ifdef W3_T
                WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX),     &
                                  ICEI(IX,IY), 'ICE (NEW)'
#endif
#ifdef W3_IC0
                VA(:,JSEA) = 0.
              ELSE
#endif
#ifdef W3_T
                WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX),     &
                                  ICEI(IX,IY), 'ICE (NEW X)'
#endif
#ifdef W3_IC0
              END IF
#endif
!
#ifdef W3_IC0
          ELSE IF ( ICEI(IX,IY).GE.FICEN ) THEN
#endif
#ifdef W3_T
            WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX),         &
                              ICEI(IX,IY), 'ICE'
#endif
#ifdef W3_IC0
          END IF
#endif
!
! 2.b Ice point to be re-activated.
!
#ifdef W3_IC0
        IF ( ICEI(IX,IY).LT.FICEN .AND. MAPICE(IY,IX).EQ.1 ) THEN
#endif
!
#ifdef W3_IC0
            MAPICE(IY,IX) = 0
            UST(ISEA)     = 0.05
#endif
!
#ifdef W3_IC0
            IF ( MAPST2(IY,IX) .EQ. 0 ) THEN
                MAPSTA(IY,IX) = ABS(MAPSTA(IY,IX))
#endif
!
#ifdef W3_IC0
                CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
                IF ( LOCAL .AND. (IAPROC .eq. ISPROC) ) THEN
#endif
#ifdef W3_T
                    WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX), &
                                      ICEI(IX,IY), 'SEA (NEW)'
#endif
#ifdef W3_IC0
                    VA(:,JSEA) = 0.
#endif
!
#ifdef W3_IC0
                  ELSE
#endif
#ifdef W3_T
                    WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX), &
                                      ICEI(IX,IY), 'SEA (NEW X)'
#endif
#ifdef W3_IC0
                  END IF
#endif
!
#ifdef W3_IC0
              ELSE
#endif
#ifdef W3_T
                WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX),     &
                                  ICEI(IX,IY), 'DIS'
#endif
#ifdef W3_IC0
              END IF
#endif
!
#ifdef W3_IC0
             ELSE IF ( ICEI(IX,IY).LT.FICEN ) THEN
#endif
#ifdef W3_T
                WRITE (NDST,9021) ISEA, IX, IY, MAPSTA(IY,IX),     &
                                  ICEI(IX,IY), 'SEA'
#endif
!
#ifdef W3_IC0
          END IF
#endif
!
        END DO
!
! 3.  Update MAPST2 -------------------------------------------------- *
!
#ifdef W3_IC0
      MAPST2 = MAPST2 + MAPICE
#endif
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT ( ' TEST W3UICE : FICEN    :',F9.3)
 9001 FORMAT ( ' TEST W3UICE : NO LOCAL SPECTRA')
#endif
!
#ifdef W3_T
 9010 FORMAT ( ' TEST W3UICE : TIME     :',I9.8,I7.6/              &
               '               OLD TICE :',I9.8,I7.6/              &
               '               NEW TICE :',I9.8,I7.6)
#endif
!
#ifdef W3_T
 9020 FORMAT ( ' TEST W3UICE : ISEA, IX, IY, MAP, ICE, STATUS :')
 9021 FORMAT ( '           ',I8,3I4,F6.2,2X,A)
#endif
!/
!/ End of W3UICE ----------------------------------------------------- /
!/
      END SUBROUTINE W3UICE
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3ULEV ( A, VA )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         26-Sep-2012 |
!/                  +-----------------------------------+
!/
!/    15-Jan-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    21-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    30-Apr-2002 : Water level fixes.                  ( version 2.20 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    15-Jul-2005 : Adding drying out of points.        ( version 3.07 )
!/    11-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    23-Aug-2011 : Bug fix for UG grids : new boundary ( version 4.04 )
!/    13-Mar-2012 : Add initialization of UST on re-    ( version 4.07 )
!/                  activation of grid point.
!/    06-Jun-2012 : Porting bugfixes from 3.14 to 4.07  ( version 4.07 )
!/    26-Sep-2012 : Adding update from tidal analysis   ( version 4.08 )
!/
!  1. Purpose :
!
!     Update the water level.
!
!  2. Method :
!
!     The wavenumber grid is modified without modyfying the spectrum
!     (conservative linear interpolation to new grid).
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!      (V)A     R.A.  I/O  2-D and 1-D represetation of the spectra.
!     ----------------------------------------------------------------
!
!     Local variables
!     ----------------------------------------------------------------
!       KDMAX   Real   Deep water cut-off for kd.
!       WNO     R.A.   Old wavenumbers.
!       CGO     R.A.   Old group velocities.
!       OWN     R.A.   Old wavenumber band width.
!       DWN     R.A.   New wavenumber band width.
!       TA      R.A.   Auxiliary spectrum.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     None.
!
!  7. Remarks :
!
!     - The grid is updated only if KDmin > KDMAX.
!     - The grid is updated for inactive points too.
!     - The local wavenumber bandwidth is DSIGMA/CG.
!     - The local spectrum is updated only if the grid is updated,
!       the grid point is not disabled (MAPST2) and if the change of
!       the lowest wavenumber exceeds RDKMIN times the band width.
!     - No spectral initialization for newly wet points.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S     Enable subroutine tracing.
!     !/T     Basic test output.
!     !/T2    Output of minimum relative depth per grid point.
!     !/T3    Spectra before and after
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSF, MAPSTA, MAPST2, &
                          ZB, DMIN, NK, NTH, NSPEC, SIG, DSIP,        &
                          MAPWN, MAPTH, FACHFA, GTYPE, UNGTYPE, W3SETREF
      USE W3WDATMD, ONLY: TIME, TLEV, WLV, UST
      USE W3ADATMD, ONLY: CG, WN, DW
      USE W3IDATMD, ONLY: TLN, WLEV
      USE W3SERVMD, ONLY: EXTCDE
      USE W3DISPMD, ONLY: WAVNU1
      USE W3TRIAMD, ONLY: SETUGIOBP
      USE W3TIMEMD
      USE W3PARALL, only : INIT_GET_JSEA_ISPROC, INIT_GET_ISEA
      USE W3PARALL, only : GET_JSEA_IBELONG
      USE W3DISPMD, ONLY: WAVNU1
#ifdef W3_TIDE
      USE W3GDATMD, ONLY: YGRD
      USE W3IDATMD, ONLY: FLLEVTIDE, WLTIDE, NTIDE
      USE W3TIDEMD
#endif
#ifdef W3_SETUP
      USE W3WDATMD, ONLY: ZETA_SETUP
      USE W3GDATMD, ONLY : DO_CHANGE_WLV
#endif


#ifdef W3_T3
      USE W3ARRYMD, ONLY: PRT2DS
#endif
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(INOUT)     :: A(NTH,NK,0:NSEAL), VA(NSPEC,0:NSEAL)
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: ISEA, JSEA, IX, IY, IK, I1, I2,      &
                                 ISPEC, IK0, ITH
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      INTEGER                 :: MAPDRY(NY,NX), ISPROC
      REAL                    :: DWO(NSEA), KDCHCK, WNO(0:NK+1),      &
                                 CGO(0:NK+1), DEPTH,                  &
                                 RDK, RD1, RD2, TA(NTH,NK),           &
                                 OWN(NK), DWN(NK)
      REAL                    :: KDMAX = 4., RDKMIN = 0.05
      REAL                    :: WLVeff
#ifdef W3_T3
      REAL                    :: OUT(NK,NTH)
#endif
      LOGICAL                 :: LOCAL
      INTEGER                 :: IBELONG
!
#ifdef W3_TIDE
      INTEGER          :: J
      INTEGER(KIND=4)  :: TIDE_KD0, INT24, INTDYS       ! "Gregorian day constant"
      REAL             :: WLEVTIDE, TIDE_ARG, WLEVTIDE2(1)
      REAL(KIND=8)     :: d1,h,TIDE_HOUR,HH,pp,s,p,enp,dh,dpp,ds,dp,dnp,tau
      REAL             :: FX(44),UX(44),VX(44)
#endif
#ifdef W3_DEBUGW3ULEV
           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 1'
           FLUSH(740+IAPROC)
#endif
      
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3ULEV')
#endif
!
      LOCAL   = IAPROC .LE. NAPROC
#ifdef W3_DEBUGW3ULEV
           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 2'
           FLUSH(740+IAPROC)
#endif
!
#ifdef W3_T
      WRITE (NDST,9000) KDMAX, RDKMIN
#endif
!
! 1.  Preparations --------------------------------------------------- *
! 1.a Check NK
!
#ifdef W3_DEBUGW3ULEV
           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 3'
           FLUSH(740+IAPROC)
#endif
      IF ( NK .LT. 2 ) THEN
          IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000)
          CALL EXTCDE ( 1 )
        END IF
!
! 1.b Update times
!
#ifdef W3_T
      WRITE (NDST,9010) TIME, TLEV
#endif
      TLEV = TLN
#ifdef W3_T
      WRITE (NDST,9011) TLEV
#endif
!
! 1.c Extract dry point map, and residual MAPST2
!
      MAPDRY = MOD(MAPST2/2,2)
      MAPST2 = MAPST2 - 2*MAPDRY
#ifdef W3_DEBUGW3ULEV
           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 4'
           FLUSH(740+IAPROC)
#endif
!
! 1.d Update water levels and save old
!
#ifdef W3_TIDE
        IF (FLLEVTIDE) THEN 
!          WRITE(6,*) 'TIME:',TIME
          TIDE_HOUR = TIME2HOURS(TIME)
!
!*  THE ASTRONOMICAL ARGUMENTS ARE CALCULATED BY LINEAR APPROXIMATION
!*  AT THE MID POINT OF THE ANALYSIS PERIOD.
          d1=TIDE_HOUR/24.d0
          TIDE_KD0= 2415020
          d1=d1-dfloat(TIDE_kd0)-0.5d0
          call astr(d1,h,pp,s,p,enp,dh,dpp,ds,dp,dnp)
          INT24=24
          INTDYS=int((TIDE_HOUR+0.00001)/INT24)
          HH=TIDE_HOUR-dfloat(INTDYS*INT24)
          TAU=HH/24.D0+H-S
        END IF
!
!  ONLY THE FRACTIONAL PART OF A SOLAR DAY NEED BE RETAINED FOR COMPU-
!  TING THE LUNAR TIME TAU.
!
#endif
#ifdef W3_DEBUGW3ULEV
           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 5'
           FLUSH(740+IAPROC)
#endif
      DO ISEA=1, NSEA
        IX     = MAPSF(ISEA,1)
        IY     = MAPSF(ISEA,2)
        DWO(ISEA) = DW(ISEA)
!
#ifdef W3_TIDE
        IF (FLLEVTIDE) THEN 
! VUF should be updated only if latitude changes significantly ...
          CALL SETVUF_FAST(h,pp,s,p,enp,dh,dpp,ds,dp,dnp,tau,YGRD(IY,IX),FX,UX,VX)
          WLEVTIDE = WLTIDE(IX,IY,1,1)
 !Verification 
 !          IF (ISEA.EQ.1) THEN 

            TIDE_AMPC(1:NTIDE,1)=WLTIDE(IX,IY,1:NTIDE,1)
            TIDE_PHG(1:NTIDE,1)=WLTIDE(IX,IY,1:NTIDE,2)
!
   !           WRITE(991,'(A,F20.2,13F8.3)') 'TEST ISEA 0:',    &
   !                       d1,H,S,TAU,pp,s,p,enp,dh,dpp,ds,dp,dnp,YGRD(IY,IX)
              J=1
   !           WRITE(991,'(A,4I9,F12.0,3F8.3,I4,X,A)') 'TEST ISEA 1:',IX,J,TIME,TIDE_HOUR,    &
   !                       FX(J),UX(J),VX(J),TIDE_INDEX2(J),TIDECON_ALLNAMES(TIDE_INDEX2(J))
            DO J=2,TIDE_MF
              TIDE_ARG=(VX(J)+UX(J))*twpi-WLTIDE(IX,IY,J,2)*DERA
              WLEVTIDE =WLEVTIDE+FX(J)*WLTIDE(IX,IY,J,1)*COS(TIDE_ARG)
   !           WRITE(991,'(A,4I9,F12.0,3F8.3,I4,X,A)') 'TEST ISEA 1:',IX,J,TIME,TIDE_HOUR,    &
   !                       FX(J),UX(J),VX(J),TIDE_INDEX2(J),TIDECON_ALLNAMES(TIDE_INDEX2(J))
              END DO
            DO J=1,TIDE_MF
   !           WRITE(991,'(A,4I9,F12.0,5F8.3)') 'TEST ISEA 2:',IX,J,TIME,TIDE_HOUR,    &
   !                       FX(J),UX(J),VX(J),TIDE_AMPC(J,1),TIDE_PHG(J,1)
              END DO
   !         WRITE(991,'(A,3F7.3)') '#:',WLEV(IX,IY),WLEVTIDE,WLEV(IX,IY)-WLEVTIDE
#endif

#ifdef W3_TIDE
   !         CLOSE(991)
   !         END IF
 ! End of verification
            WLV(ISEA) = WLEVTIDE
          ELSE 
#endif
!
        WLV(ISEA) = WLEV(IX,IY)
        WLVeff=WLV(ISEA)
#ifdef W3_SETUP
     IF (DO_CHANGE_WLV) THEN
       WLVeff=WLVeff + ZETA_SETUP(ISEA)
     END IF
#endif
#ifdef W3_TIDE
          ENDIF 
#endif
        DW (ISEA) = MAX ( 0. , WLVeff-ZB(ISEA) )
        END DO
#ifdef W3_DEBUGW3ULEV
           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 6'
           FLUSH(740+IAPROC)
#endif
!
! 2.  Loop over all sea points --------------------------------------- *
!
#ifdef W3_T2
      WRITE (NDST,9020)
#endif
!
      DO ISEA=1, NSEA
!
        IX     = MAPSF(ISEA,1)
        IY     = MAPSF(ISEA,2)
!
! 2.a Check if deep water
!
        KDCHCK = WN(1,ISEA) * MIN( DWO(ISEA) , DW(ISEA) )
        IF ( KDCHCK .LT. KDMAX ) THEN
!
! 2.b Update grid and save old grid
!
            DEPTH  = MAX ( DMIN, DW(ISEA) )
!
            DO IK=0, NK+1
              WNO(IK) = WN(IK,ISEA)
              CGO(IK) = CG(IK,ISEA)
!
!             Calculate wavenumbers and group velocities.
              CALL WAVNU1(SIG(IK),DEPTH,WN(IK,ISEA),CG(IK,ISEA))
!
              END DO
!
            DO IK=1, NK
              OWN(IK) = DSIP(IK) / CGO(IK)
              DWN(IK) = DSIP(IK) / CG(IK,ISEA)
              END DO
!
! 2.c Process dry points
!
            IF ( WLV(ISEA)-ZB(ISEA) .LE.0. ) THEN
                IF ( MAPDRY(IY,IX) .EQ. 0 ) THEN
                    CALL GET_JSEA_IBELONG(ISEA, JSEA, IBELONG)
                    IF ( LOCAL .AND. (IBELONG .eq. 1) ) THEN
                        VA(:,JSEA) = 0.
                      END IF
                    MAPDRY(IY,IX) = 1
                    MAPSTA(IY,IX) = -ABS(MAPSTA(IY,IX))
#ifdef W3_T2
                    WRITE (NDST,9021) ISEA, WLV(ISEA)-ZB(ISEA),   &
                                      0., 0., '  (NEW DRY)'
                  ELSE
                    WRITE (NDST,9021) ISEA, WLV(ISEA)-ZB(ISEA),   &
                                      0., 0., '  (DRY)'
#endif
                  ENDIF
                CYCLE
             END IF
!
! 2.d Process new wet point
!
            IF (WLV(ISEA)-ZB(ISEA).GT.0. .AND. MAPDRY(IY,IX).EQ.1) THEN
                MAPDRY(IY,IX) = 0
!
! Resets the spectrum to zero
!
                    CALL GET_JSEA_IBELONG(ISEA, JSEA, IBELONG)
                    IF ( LOCAL .AND. (IBELONG .eq. 1) ) THEN
                        VA(:,JSEA) = 0.
                      END IF
!
                UST(ISEA)     = 0.05
                IF ( MAPST2(IY,IX) .EQ. 0 ) THEN
                    MAPSTA(IY,IX) = ABS(MAPSTA(IY,IX))
#ifdef W3_T2
                    WRITE (NDST,9021) ISEA, WLV(ISEA)-ZB(ISEA),   &
                                      0., 0., '  (NEW WET)'
                  ELSE
                    WRITE (NDST,9021) ISEA, WLV(ISEA)-ZB(ISEA),   &
                                      0., 0., '  (NEW WET INACTIVE)'
#endif
                  END IF
                CYCLE
             END IF
!
! 2.e Check if ice on grid point, or if grid changes negligible
!
            RDK    = ABS(WNO(1)-WN(1,ISEA)) / DWN(1)
!
#ifdef W3_T2
            IF ( MAPSTA(IY,IX) .LT. 0 ) THEN
                WRITE (NDST,9021)                                 &
                       ISEA, DW(ISEA), KDCHCK, RDK, '  (INACTIVE)'
              ELSE IF ( RDK .LT. RDKMIN ) THEN
                WRITE (NDST,9021)                                 &
                       ISEA, DW(ISEA), KDCHCK, RDK, '  (NEGL)'
              ELSE
                WRITE (NDST,9021)                                 &
                       ISEA, DW(ISEA), KDCHCK, RDK, ' '
              END IF
#endif
!
            IF ( RDK.LT.RDKMIN .OR. MAPSTA(IY,IX).LT.0 ) CYCLE
            CALL GET_JSEA_IBELONG(ISEA, JSEA, IBELONG)
            IF ( IBELONG .eq. 0) CYCLE
!
            IF ( .NOT. LOCAL ) CYCLE
!
! 2.d Save discrete actions and clean spectrum
!
            DO IK=1, NK
              DO ITH=1, NTH
#ifdef W3_T3
                OUT(IK,ITH) = A(ITH,IK,JSEA) * SIG(IK) / CGO(IK)
#endif
                TA(ITH,IK) = A(ITH,IK,JSEA) * OWN(IK)
                END DO
              END DO
!
            VA(:,JSEA) = 0.
!
#ifdef W3_T3
            CALL PRT2DS ( NDST, NK, NK, NTH, OUT, SIG, ' ',    &
                 TPI, 0., 1.E-5, 'F(f,th)', 'm2s', 'Before' )
#endif
!
! 2.e Redistribute discrete action density
!
            IF ( WNO(1) .LT. WN(1,ISEA) ) THEN
                IK0    = 1
                I1     = 0
                I2     = 1
  220           CONTINUE
                IK0    = IK0 + 1
                IF ( IK0 .GT. NK+1 ) GOTO 251
                IF ( WNO(IK0) .GE. WN(1,ISEA) ) THEN
                     IK0    = IK0 - 1
                   ELSE
                     GOTO 220
                   END IF
              ELSE
                IK0    = 1
                I1     = 1
                I2     = 2
              END IF
!
            DO 250, IK=IK0, NK
!
  230         CONTINUE
              IF ( WNO(IK) .GT. WN(I2,ISEA) ) THEN
                  I1     = I1 + 1
                  IF ( I1 .GT. NK ) GOTO 250
                  I2     = I1 + 1
                  GOTO 230
                END IF
!
              IF ( I1 .EQ. 0 ) THEN
                  RD1    = ( WN(1,ISEA) - WNO(IK) ) / DWN(1)
                  RD2    = 1. - RD1
                ELSE
                  RD1    = ( WN(I2,ISEA) - WNO(IK) ) /                &
                           ( WN(I2,ISEA) - WN(I1,ISEA) )
                  RD2    = 1. - RD1
                END IF
!
                IF ( I1 .GE. 1 ) THEN
                    DO ITH=1, NTH
                      A(ITH,I1,JSEA) = A(ITH,I1,JSEA) + RD1*TA(ITH,IK)
                      END DO
                  END IF
!
                IF ( I2 .LE. NK ) THEN
                    DO ITH=1, NTH
                      A(ITH,I2,JSEA) = A(ITH,I2,JSEA) + RD2*TA(ITH,IK)
                      END DO
                  END IF
!
  250         CONTINUE
  251       CONTINUE
!
! 2.f Convert discrete action densities to spectrum
!
            DO ISPEC=1, NSPEC
              VA(ISPEC,JSEA) = VA(ISPEC,JSEA) / DWN(MAPWN(ISPEC))
              END DO
!
! 2.f Add tail if necessary
!
            IF ( I2.LE.NK .AND. RD2.LE.0.95 ) THEN
                DO IK=MAX(I2,2), NK
                  DO ITH=1, NTH
                    A(ITH,IK,JSEA) = FACHFA * A(ITH,IK-1,JSEA)
                    END DO
                  END DO
              END IF
!
#ifdef W3_T3
            DO ISPEC=1, NSPEC
              IK          = MAPWN(ISPEC)
              ITH         = MAPTH(ISPEC)
              OUT(IK,ITH) = A(ITH,IK,JSEA) * SIG(IK) / CG(IK,ISEA)
              END DO
#endif
!
#ifdef W3_T3
            CALL PRT2DS ( NDST, NK, NK, NTH, OUT, SIG, ' ',    &
                 TPI, 0., 1.E-5, 'F(f,th)', 'm2s', 'After' )
#endif
!
#ifdef W3_T2
          ELSE
            WRITE (NDST,9021) ISEA, KDCHCK, '  (DEEP)'
#endif
          END IF
!
        END DO
#ifdef W3_DEBUGW3ULEV
           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 7'
           FLUSH(740+IAPROC)
#endif
!
! 3.  Reconstruct new MAPST2 ----------------------------------------- *
!
      MAPST2 = MAPST2 + 2*MAPDRY
!
! 4. Re-generates the boundary data ---------------------------------- *
!
#ifdef W3_DEBUGW3ULEV
           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 8'
           FLUSH(740+IAPROC)
#endif
      IF (GTYPE.EQ.UNGTYPE) THEN 
#ifdef W3_DEBUGW3ULEV
           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 9'
           FLUSH(740+IAPROC)
#endif
        CALL SETUGIOBP
#ifdef W3_DEBUGW3ULEV
           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 10'
           FLUSH(740+IAPROC)
#endif
#ifdef W3_REF1
      ELSE 
        CALL W3SETREF
#endif
      ENDIF 
#ifdef W3_DEBUGW3ULEV
           WRITE(740+IAPROC,*) 'Beginning of W3ULEV, step 11'
           FLUSH(740+IAPROC)
#endif
!
      RETURN
!
! Formats
!
 1000 FORMAT (/' *** ERROR W3ULEV *** '/                              &
               '     THIS ROUTINE REQUIRES NK > 1 '/)
!
#ifdef W3_T
 9000 FORMAT ( ' TEST W3ULEV : KDMAX    :',F6.1/                   &
               '               RDKMIN   :',F8.3)
#endif
!
#ifdef W3_T
 9010 FORMAT ( ' TEST W3ULEV : TIME     :',I9.8,I7.6/              &
               '               OLD TLEV :',I9.8,I7.6)
 9011 FORMAT ( '               NEW TLEV :',I9.8,I7.6)
#endif
!
#ifdef W3_T2
 9020 FORMAT ( ' TEST W3ULEV : LOOP OVER ALL POINTS:',            &
               ' ISEA, DW, KDMIN, RDK : ')
 9021 FORMAT ( '             ',I6,F8.2,F6.2,F7.3,A)
#endif
!/
!/ End of W3ULEV ----------------------------------------------------- /
!/
      END SUBROUTINE W3ULEV
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3URHO ( FLFRST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           J. M. Castillo          |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         13-Aug-2021 |
!/                  +-----------------------------------+
!/
!/    22-Mar-2021 : First implementation                ( version 7.13 )
!/    13-Aug-2021 : Enable time interpolation           ( version 7.xx )
!/
!  1. Purpose :
!
!     Interpolate air density field to the given time.
!
!  2. Method :
!
!     Linear interpolation.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       FLFRST  Log.  I   Flag for first pass through routine.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     None.
!
!  7. Remarks :
!
!     - Only air density over sea points is considered.
!     - Time ranges checked in W3WAVE.
!
!  8. Structure :
!
!     --------------------------------------
!      1.  Prepare auxiliary arrays
!      2.  Calculate interpolation factors
!      3.  Get actual air density
!     --------------------------------------
!
!  9. Switches :
!
!     !/OMPG   OpenMP compiler directives
!
!     !/WNT0   No air density interpolation.
!     !/WNT1   Linear air density interpolation.
!     !/WNT2   Linear air density interpolation (and energy conservation for momentum).
!
!     !/S  Enable subroutine tracing.
!     !/T  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NSEA, MAPSF
#ifdef W3_SMC
      USE W3GDATMD, ONLY: FSWND
#endif
      USE W3WDATMD, ONLY: TIME, TRHO, RHOAIR
      USE W3IDATMD, ONLY: TR0, TRN, RH0, RHN
      USE W3ADATMD, ONLY: RA0, RAI
      USE W3ODATMD, ONLY: IAPROC, NAPROC
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      LOGICAL, INTENT(IN)     :: FLFRST
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: ISEA, IX, IY
#ifdef W3_S
      INTEGER, SAVE            :: IENT = 0
#endif
      REAL                    :: DT0N, DT0T, RD
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3URHO')
#endif
!
! 1.  Prepare auxiliary arrays
!
      IF ( FLFRST ) THEN
          DO ISEA=1, NSEA
#ifdef W3_SMC
 !!Li  For sea-point only SMC grid air density is stored on
 !!Li  2-D RH0(NSEA, 1) variable.
        IF( FSWND ) THEN
            IX = ISEA 
            IY = 1
        ELSE
#endif
            IX        = MAPSF(ISEA,1)
            IY        = MAPSF(ISEA,2)
#ifdef W3_SMC
        ENDIF
#endif

            RA0(ISEA) = RH0(IX,IY)
            RAI(ISEA) = RHN(IX,IY) - RH0(IX,IY)
            END DO
        END IF
!
! 2.  Calculate interpolation factor
!
      DT0N   = DSEC21 ( TR0, TRN )
      DT0T   = DSEC21 ( TR0, TIME )
!
#ifdef W3_WNT0
      RD     = 0.
#endif
#ifdef W3_WNT1
      RD     = DT0T / MAX ( 1.E-7 , DT0N )
#endif
#ifdef W3_WNT2
      RD     = DT0T / MAX ( 1.E-7 , DT0N )
#endif
#ifdef W3_OASACM
     RD     = 1.
#endif
!
#ifdef W3_T
      WRITE (NDST,9000) DT0N, DT0T, RD
#endif
!
! 3.  Actual momentum for all grid points
!
#ifdef W3_OMPG
!$OMP PARALLEL DO PRIVATE (ISEA,RA0,RAI)
#endif
!
      DO ISEA=1, NSEA
!
        RHOAIR(ISEA) = RA0(ISEA) + RD * RAI(ISEA)
!
        END DO
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT (' TEST W3URHO : DT0N, DT0T, RD :',2F8.1,F6.3)
#endif
!/
!/ End of W3URHO ----------------------------------------------------- /
!/
      END SUBROUTINE W3URHO
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3UTRN ( TRNX, TRNY )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         30-Oct-2009 |
!/                  +-----------------------------------+
!/
!/    02-Apr-2001 : Origination.                        ( version 2.10 )
!/    11-Jan-2002 : Sub-grid ice.                       ( version 2.15 )
!/    30-Apr-2002 : Change to ICE on storage grid.      ( version 2.20 )
!/    15-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    11-Jan-2007 : Clean-up for boundary points.       ( version 3.10 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/
!  1. Purpose :
!
!     Update cell boundary transparencies for general use in propagation
!     routines.
!
!  2. Method :
!
!     Two arrays are generated with the size (NY*NX,-1:1). The value 
!     at (IXY,-1) indicates the transparency to be used if the lower
!     or left boundary is an inflow boundary. (IXY,1) is used if the
!     upper or right boundary is an inflow boundary. (IXY,0) is used
!     for all other cases (by definition full transparency).
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       TRNX/Y  R.A.   I   Transparencies from model defintion file.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     None.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/S      Enable subroutine tracing.
!       !/T      Basic test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSTA, MAPSF,                &
                          TRFLAG, FICE0, FICEN, FICEL,                &
                          RLGTYPE, CLGTYPE, GTYPE, FLAGLL,            &
                          HPFAC, HQFAC, FFACBERG
      USE W3WDATMD, ONLY: ICE, BERG
      USE W3ADATMD, ONLY: ATRNX, ATRNY
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/    
      REAL, INTENT(IN)        :: TRNX(NY*NX), TRNY(NY*NX)
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: ISEA, IX, IY, IXY, IXN, IXP, IYN, IYP
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T
      INTEGER                 :: ILEV, NLEV
#endif

      REAL                    :: TRIX(NY*NX), TRIY(NY*NX), DX, DY,    &
                                 LICE0, LICEN
#ifdef W3_T
      REAL                    :: LEVS(0:10)
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3UTRN')
#endif
#ifdef W3_T
      WRITE (NDST,9000) TRFLAG
#endif
!
! 1.  Preparations --------------------------------------------------- *
!
      ATRNX = 1.
      ATRNY = 1.
#ifdef W3_T
      WRITE (NDST,9001) 'INITIALIZING ATRNX/Y'
#endif
!
! 2.  Filling arrays from TRNX/Y for obstructions -------------------- *
! 2.a TRFLAG = 0, no action needed
      IF ( TRFLAG .EQ. 0 ) THEN
#ifdef W3_T
          WRITE (NDST,9001) 'NO FURTHER ACTION REQUIRED'
#endif
          RETURN
!
! 2.b TRFLAG = 1,3: TRNX/Y defined at boundaries
!
        ELSE IF ( TRFLAG.EQ.1 .OR. TRFLAG.EQ.3 .OR. TRFLAG.EQ.5 ) THEN
#ifdef W3_T
          WRITE (NDST,9001) 'DATA APPLIED AT CELL BOUNDARIES'
          LEVS   = 0.
#endif
!
          DO ISEA=1, NSEA
!
            IX            = MAPSF(ISEA,1)
            IY            = MAPSF(ISEA,2)
            IXY           = MAPSF(ISEA,3)
            IF ( IX .EQ. 1 ) THEN
                ATRNX(IXY,-1) = TRNX(IY+(NX-1)*NY)
                ATRNX(IXY, 1) = TRNX(IXY)
              ELSE IF ( IX .EQ. NX ) THEN
                ATRNX(IXY,-1) = TRNX(IXY-NY)
                ATRNX(IXY, 1) = TRNX(IY)
              ELSE
                ATRNX(IXY,-1) = TRNX(IXY-NY)
                ATRNX(IXY, 1) = TRNX(IXY)
              END IF
            ATRNY(IXY,-1) = TRNY(IXY-1)
            ATRNY(IXY, 1) = TRNY(IXY)
!
#ifdef W3_T
            ILEV          = NINT(10.*MIN(TRNX(IXY),TRNY(IXY)))
            LEVS(ILEV)    = LEVS(ILEV) + 1.
#endif
!
            END DO
!
! 2.c TRFLAG = 2,4: TRNX/Y defined at cell centers
!
        ELSE
#ifdef W3_T
          WRITE (NDST,9001) 'DATA APPLIED AT CELL CENTERS'
          LEVS   = 0.
#endif
!
          DO ISEA=1, NSEA
!
            IX            = MAPSF(ISEA,1)
            IY            = MAPSF(ISEA,2)
            IXY           = MAPSF(ISEA,3)
!
            IF ( IX .EQ. 1 ) THEN
                IXN    = IY + (NX-1)*NY
                IXP    = IY +  IX   *NY
              ELSE IF ( IX .EQ. NX ) THEN
                IXN    = IY + (IX-2)*NY
                IXP    = IY
              ELSE
                IXN    = IY + (IX-2)*NY
                IXP    = IY +  IX   *NY
              END IF
!
            IF ( IY .EQ. 1 ) THEN
                IYN    = IXY
                IYP    = IXY + 1
              ELSE IF ( IY .EQ. NY ) THEN
                IYN    = IXY - 1
                IYP    = IXY
              ELSE
                IYN    = IXY - 1
                IYP    = IXY + 1
              END IF
!
! factors 0.5 in first term and 2. in second term cancel
!
            ATRNX(IXY,-1) = (1.+TRNX(IXY)) * TRNX(IXN)/(1.+TRNX(IXN))
            ATRNX(IXY, 1) = (1.+TRNX(IXY)) * TRNX(IXP)/(1.+TRNX(IXP))
            ATRNY(IXY,-1) = (1.+TRNY(IXY)) * TRNY(IYN)/(1.+TRNY(IYN))
            ATRNY(IXY, 1) = (1.+TRNY(IXY)) * TRNY(IYP)/(1.+TRNY(IYP))
!
            IF ( MAPSTA(IY,IX) .EQ. 2 ) THEN
                IF ( IX .EQ. 1  ) THEN
                    ATRNX(IXY,-1) = 1.
                  ELSE IF ( MAPSTA( IY ,IX-1) .LE. 0 ) THEN
                    ATRNX(IXY,-1) = 1.
                  END IF
                IF ( IX .EQ. NX ) THEN
                     ATRNX(IXY, 1) = 1.
                  ELSE IF ( MAPSTA( IY ,IX+1) .LE. 0 ) THEN
                     ATRNX(IXY, 1) = 1.
                  END IF
                IF ( IY .EQ. 1  ) THEN
                     ATRNY(IXY,-1) = 1.
                  ELSE IF ( MAPSTA(IY-1, IX ) .LE. 0 ) THEN
                     ATRNY(IXY,-1) = 1.
                  END IF
                IF ( IY .EQ. NY ) THEN
                     ATRNY(IXY, 1) = 1.
                  ELSE IF ( MAPSTA(IY+1, IX ) .LE. 0 ) THEN
                     ATRNY(IXY, 1) = 1.
                  END IF
              END IF
!
#ifdef W3_T
            ILEV          = NINT(10.*MIN(TRNX(IXY),TRNY(IXY)))
            LEVS(ILEV)    = LEVS(ILEV) + 1.
#endif
!
            END DO
        END IF
!
#ifdef W3_T
      WRITE(NDST,9010) 'ISLANDS'
      NLEV   = 0
      DO ILEV=0, 10
        WRITE (NDST,9011) ILEV, LEVS(ILEV)/REAL(NSEA)
        NLEV = NLEV + NINT(LEVS(ILEV))
        END DO
#endif
!
! 3.  Adding ice to obstructions ------------------------------------- *
! 3.a TRFLAG < 3, no action needed
!
      IF ( TRFLAG.LT.3 .OR. FICEN-FICE0.LT.1.E-6 ) THEN
#ifdef W3_T
          WRITE (NDST,9001) 'NO ICE ACTION REQUIRED'
#endif
          RETURN
!
! 3.b TRFLAG = 3,4: Calculate ice transparencies
!
        ELSE
#ifdef W3_T
          WRITE (NDST,9001) 'CALCULATE ICE TRANSPARENCIES'
          LEVS   = 0.
#endif
          TRIX   = 1.
          TRIY   = 1.
!
          DO ISEA=1, NSEA
!
            IX     = MAPSF(ISEA,1)
            IY     = MAPSF(ISEA,2)
            IXY    = MAPSF(ISEA,3)
!
            DX     = HPFAC(IY,IX)
            DY     = HQFAC(IY,IX)
            IF ( FLAGLL ) THEN
                DX     = DX * RADIUS * DERA
                DY     = DY * RADIUS * DERA
              END IF

!
#ifdef W3_IC0
            IF (ICE(ISEA).GT.0) THEN
              IF (FICEL.GT.0.) THEN
                TRIX(IXY) = EXP(-ICE(ISEA)*DX/FICEL)
                TRIY(IXY) = EXP(-ICE(ISEA)*DY/FICEL)
              ELSE 
#endif
! Otherwise: original Tolman expression (Tolman 2003)
#ifdef W3_IC0
                LICE0  = FICE0*DX
                LICEN  = FICEN*DX
                TRIX(IXY) = ( LICEN - ICE(ISEA)*DX ) / ( LICEN - LICE0 )
#endif

! begin temporary notes
!                TRIX = (   LICEN    - ICE(ISEA)*DX ) / (   LICEN -      LICE0 )
!    thus, it is TRIX=  ( (FICEN*DX) - ICE(ISEA)*DX ) / ( (FICEN*DX) - (FICE0*DX) )
!    thus, it is TRIX=  (   FICEN -    ICE(ISEA) )   /  (  FICEN     -   FICE0 )
!    in other words, the variables DX DY are not used
!                    and the variables LICE0 LICEN are not necessary.
! end temporary notes

#ifdef W3_IC0
                LICE0  = FICE0*DY
                LICEN  = FICEN*DY
                TRIY(IXY) = ( LICEN - ICE(ISEA)*DY ) / ( LICEN - LICE0 )
                END IF
#endif
!
#ifdef W3_IC0
              TRIX(IXY) = MAX ( 0. , MIN ( 1. , TRIX(IXY) ) )
              TRIY(IXY) = MAX ( 0. , MIN ( 1. , TRIY(IXY) ) ) 
              END IF          
#endif
!
!  Adding iceberg attenuation
!
            IF (BERG(ISEA).GT.0) THEN
              TRIX(IXY) = TRIX(IXY)*EXP(-BERG(ISEA)*FFACBERG  *DX*0.0001)
              TRIY(IXY) = TRIY(IXY)*EXP(-BERG(ISEA)*FFACBERG  *DY*0.0001)
              END IF
!
#ifdef W3_T
            ILEV          = NINT(10.*MIN(TRIX(IXY),TRIY(IXY)))
            LEVS(ILEV)    = LEVS(ILEV) + 1.
#endif
!
          END DO
!
#ifdef W3_T
      WRITE(NDST,9010) 'ICE'
      NLEV   = 0
      DO ILEV=0, 10
        WRITE (NDST,9011) ILEV, LEVS(ILEV)/REAL(NSEA)
        NLEV = NLEV + NINT(LEVS(ILEV))
        END DO
#endif
!
! 3.c Combine transparencies, ice always defined at cell center !
!
          DO ISEA=1, NSEA
!
            IX            = MAPSF(ISEA,1)
            IY            = MAPSF(ISEA,2)
            IXY           = MAPSF(ISEA,3)
!
            IF ( IX .EQ. 1 ) THEN
                IXN    = IY + (NX-1)*NY
                IXP    = IY +  IX   *NY
              ELSE IF ( IX .EQ. NX ) THEN
                IXN    = IY + (IX-2)*NY
                IXP    = IY
              ELSE
                IXN    = IY + (IX-2)*NY
                IXP    = IY +  IX   *NY
              END IF
!
            IF ( IY .EQ. 1 ) THEN
                IYN    = IXY
                IYP    = IXY + 1
              ELSE IF ( IY .EQ. NY ) THEN
                IYN    = IXY - 1
                IYP    = IXY
              ELSE
                IYN    = IXY - 1
                IYP    = IXY + 1
              END IF
!
            ATRNX(IXY,-1) = ATRNX(IXY,-1)                             &
                          * (1.+TRIX(IXY)) * TRIX(IXN)/(1.+TRIX(IXN))
            ATRNX(IXY, 1) = ATRNX(IXY, 1)                             &
                          * (1.+TRIX(IXY)) * TRIX(IXP)/(1.+TRIX(IXP))
            ATRNY(IXY,-1) = ATRNY(IXY,-1)                             &
                          * (1.+TRIY(IXY)) * TRIY(IYN)/(1.+TRIY(IYN))
            ATRNY(IXY, 1) = ATRNY(IXY, 1)                             &
                          * (1.+TRIY(IXY)) * TRIY(IYP)/(1.+TRIY(IYP))
!
            END DO
!
        END IF
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT ( ' TEST W3UTRN : TRFLAG = ',I3)
 9001 FORMAT ( ' TEST W3UTRN : ',A)
 9010 FORMAT ( ' TEST W3UTRN : OBSTRICTION LEVELS FOR ',A,' :')
 9011 FORMAT ( '             ',I4,F8.5)
#endif
!/
!/ End of W3UTRN ----------------------------------------------------- /
!/
      END SUBROUTINE W3UTRN
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3DZXY( ZZ, ZUNIT, DZZDX, DZZDY )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |         W. E. Rogers, NRL         |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Dec-2010 |
!/                  +-----------------------------------+
!/
!/    30-Oct-2009 : Origination.                        ( version 3.14 )
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/
!  1. Purpose :
!
!     Calculate derivatives of a field.
!
!  2. Method :
!
!     Derivatives are calculated in m/m from the longitude/latitude
!     grid, central in space for iternal points, one-sided for
!     coastal points.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       ZZ      R.A.  I   Field to calculate derivatives of.
!       ZUNIT   R.A.  I   Units of ZZ (used for test output).
!       DZZDX   R.A.  O   Derivative in X-direction (W-E).
!       DZZDY   R.A.  O   Derivative in Y-direction (S-N).
!       IXP: IX plus 1 (with branch cut incorporated)
!       IYP, IXM, IYM: ditto
!       IXPS: value to use for IXP if IXPS is not masked. 
!             (use IX if masked)
!       IYPS, IXMS, IYMS : ditto
!       IXTRPL : in case of needing IY+1 for IY=NY, IX needs to be
!                modified (tripole grid only)
!       IXTRPLS : value to use for IXTRPL if IXTRPLS is not masked
!             (use IX if masked)
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!     This routine replaces the functionality of W3DDXY and W3DCXY.
!     NB: subroutine "W3CGDM" has a similar purpose.
!     Output arrays are always initialized to zero.
!
!  8. Structure :
!
!     ----------------------------------------
!      1.  Preparations
!        a Initialize arrays
!        b Set constants
!      2.  Derivatives in X-direction (W-E).
!      3.  Derivatives in Y-direction (S-N).
!     ----------------------------------------
!
!  9. Switches :
!
!       !/S   Enable subroutine tracing.
!       !/T   Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSTA, MAPFS, MAPFS, &
             DPDX, DPDY, DQDX, DQDY, FLAGLL, ICLOSE,          &
             ICLOSE_NONE, ICLOSE_SMPL, ICLOSE_TRPL
      USE W3ODATMD, ONLY: NDSE, IAPROC, NAPERR, NAPROC
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_T
      USE W3ARRYMD, ONLY : PRTBLK
#endif
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      REAL, INTENT(IN)        :: ZZ(NSEA)
      CHARACTER, INTENT(IN)   :: ZUNIT*(*)
      REAL, INTENT(OUT)       :: DZZDX(NY,NX), DZZDY(NY,NX)
      INTEGER                 :: ISEA, IX, IY, IXP, IXM, IYP, IYM
#ifdef W3_T
      INTEGER                 :: ISX, ISY, MAPOUT(NX,NY)
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T
      INTEGER, SAVE           :: NXS = 49
#endif
      REAL                    :: DFAC , STX, STY
      INTEGER                 :: IXPS,IYPS,IXMS,IYMS,IXTRPL,IXTRPLS
      INTEGER                 :: IXSTART,IXEND
#ifdef W3_T
      REAL                    :: XOUT(NX,NY)
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3DZXY')
#endif
!
! 1.  Preparations --------------------------------------------------- *

! 1.a Initialize arrays
!
      DZZDX = 0.
      DZZDY = 0.
!
! 1.b Set constants
!

      IF ( FLAGLL ) THEN
         DFAC   = 1. / ( DERA * RADIUS )
      ELSE
         DFAC   = 1.
      END IF

!
! 2.  Derivatives in X-direction (W-E) and Y-direction (S-N) ----- *
!

! 2a. All points previously done in 2a,2b,2c of v4.18 done in 2a now:
      IF ( ICLOSE.EQ.ICLOSE_NONE ) THEN
         IXSTART=2
         IXEND=NX-1
      ELSE
         IXSTART=1
         IXEND=NX
      ENDIF

      DO IY=2, NY-1
         DO IX=IXSTART,IXEND
            IF ( MAPSTA(IY,IX) .NE. 0 ) THEN
               STX    = 0.5
               IF (IX.EQ.NX)THEN
                  IXPS=1
               ELSE
                  IXPS=IX+1
               ENDIF

               IF (MAPSTA(IY,IXPS).EQ.0) THEN
                  IXP    = IX
                  STX    = 1.0
               ELSE
                  IXP    = IXPS
               END IF

               IF(IX.EQ.1)THEN
                  IXMS=NX
               ELSE
                  IXMS=IX-1
               ENDIF

               IF (MAPSTA(IY,IXMS).EQ.0) THEN
                  IXM    = IX
                  STX    = 1.0
               ELSE
                  IXM    = IXMS
               END IF
               STY    = 0.5
               IYPS=IY+1
               IF (MAPSTA(IYPS,IX).EQ.0) THEN
                  IYP    = IY
                  STY    = 1.0
               ELSE
                  IYP    = IYPS
               END IF
               IYMS=IY-1
               IF (MAPSTA(IYMS,IX).EQ.0) THEN
                  IYM    = IY
                  STY    = 1.0
               ELSE
                  IYM    = IYMS
               END IF
               DZZDX(IY,IX) = (ZZ(MAPFS(IY ,IXP))-ZZ(MAPFS(IY ,IXM))) * STX * DPDX(IY,IX) &
                    + (ZZ(MAPFS(IYP,IX ))-ZZ(MAPFS(IYM,IX ))) * STY * DQDX(IY,IX)
               DZZDY(IY,IX) = (ZZ(MAPFS(IY ,IXP))-ZZ(MAPFS(IY ,IXM))) * STX * DPDY(IY,IX) &
                    + (ZZ(MAPFS(IYP,IX ))-ZZ(MAPFS(IYM,IX ))) * STY * DQDY(IY,IX)
               DZZDX(IY,IX) = DZZDX(IY,IX) * DFAC
               DZZDY(IY,IX) = DZZDY(IY,IX) * DFAC
            END IF
         END DO
      END DO

! 2b. column IY=NY for tripole case
! This is more complex, since for these two points: (IYP,IX) (IYM,IX),
!     not only is the first index different (IYP.NE.IYM), but also the
!     second index is different (IX.NE.IX)!
      IF ( ICLOSE.EQ.ICLOSE_TRPL ) THEN

         IY=NY
         DO IX=1, NX
            IF ( MAPSTA(IY,IX) .NE. 0 ) THEN

               STX    = 0.5
               
               IF (IX.EQ.NX)THEN
                  IXPS=1
               ELSE
                  IXPS=IX+1
               ENDIF
               IF (MAPSTA(IY,IXPS).EQ.0) THEN
                  IXP    = IX
                  STX    = 1.0
               ELSE
                  IXP    = IXPS
               END IF
               
               IF(IX.EQ.1)THEN
                  IXMS=NX
               ELSE
                  IXMS=IX-1
               ENDIF
               IF (MAPSTA(IY,IXMS).EQ.0) THEN
                  IXM    = IX
                  STX    = 1.0
               ELSE
                  IXM    = IXMS
               END IF

               STY    = 0.5

!..............next point: j+1: tripole: j==>j+1==>j and i==>ni-i+1
!..............i.e. target point is MAPFS(IY,(NX-IX+1))
               IXTRPLS=NX-IX+1
               IF (MAPSTA(IY,IXTRPLS).EQ.0) THEN
                  IXTRPL = IX
                  STY    = 1.0
               ELSE
                  IXTRPL=IXTRPLS
               END IF

               IYMS=IY-1
               IF (MAPSTA(IYMS,IX).EQ.0) THEN
                  IYM    = IY
                  STY    = 1.0
               ELSE
                  IYM    = IYMS
               END IF

! tripole grid: (IYP,IX) is replaced with (IY,IXTRPL)
               DZZDX(IY,IX) = (ZZ(MAPFS(IY ,IXP))-ZZ(MAPFS(IY ,IXM))) * STX * DPDX(IY,IX) &
                          + (ZZ(MAPFS(IY,IXTRPL))-ZZ(MAPFS(IYM,IX ))) * STY * DQDX(IY,IX)
               DZZDY(IY,IX) = (ZZ(MAPFS(IY ,IXP))-ZZ(MAPFS(IY ,IXM))) * STX * DPDY(IY,IX) &
                          + (ZZ(MAPFS(IY,IXTRPL))-ZZ(MAPFS(IYM,IX ))) * STY * DQDY(IY,IX)
               DZZDX(IY,IX) = DZZDX(IY,IX) * DFAC
               DZZDY(IY,IX) = DZZDY(IY,IX) * DFAC
            END IF
         END DO

      END IF ! IF ( ICLOSE.EQ.ICLOSE_TRPL ) THEN

!
! 3.  Test output of fields ------------------------------------------ *
!
#ifdef W3_T
      WRITE (NDST,9010)
      ISX   = 1 + NX/NXS
      ISY   = 1 + NY/NXS
      DO IY=1, NY
        DO IX=1, NX
          MAPOUT(IX,IY) = MAPSTA(IY,IX)
          IF ( MAPFS(IY,IX) .NE. 0 )                               &
               XOUT(IX,IY) = ZZ(MAPFS(IY,IX))
          END DO
        END DO
      CALL PRTBLK (NDST, NX, NY, NX, XOUT, MAPOUT, 0, 0.,          &
                      1, NX, ISX, 1, NY, ISY, 'ZZ', ZUNIT)
      DO IY=1, NY
        DO IX=1, NX
          XOUT(IX,IY) = DZZDX(IY,IX)
          END DO
        END DO
      CALL PRTBLK (NDST, NX, NY, NX, XOUT, MAPOUT, 0, 0.,          &
                      1, NX, ISX, 1, NY, ISY, 'DZZDX',TRIM(ZUNIT)//'/m')
      DO IY=1, NY
        DO IX=1, NX
          XOUT(IX,IY) = DZZDY(IY,IX)
          END DO
        END DO
      CALL PRTBLK (NDST, NX, NY, NX, XOUT, MAPOUT, 0, 0.,          &
                      1, NX, ISX, 1, NY, ISY, 'DZZDY',TRIM(ZUNIT)//'/m')
#endif
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT (' TEST W3DZXY : DX0I, DY0I : ',2E12.5)
 9010 FORMAT (' TEST W3DZXY : FIELDS ')
#endif
!/
!/ End of W3DZXY ----------------------------------------------------- /
!/
      END SUBROUTINE W3DZXY
!/ ------------------------------------------------------------------- /
!/ End of module W3UPDTMD -------------------------------------------- /
!/
      END MODULE W3UPDTMD
