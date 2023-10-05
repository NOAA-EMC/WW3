!> @file
!> @brief Interface module to exact nonlinear interactions.
!>
!> @author H. L. Tolman
!> @author G. Ph. van Vledder
!> @date   29-May-2009
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Interface module to exact nonlinear interactions.
!>
!> @author H. L. Tolman
!> @author G. Ph. van Vledder
!> @date   29-May-2009
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>
MODULE W3SNL2MD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |        G. Ph. van Vledder         |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         29-May-2009 |
  !/                  +-----------------------------------+
  !/
  !/    14-Feb-2000 : Origination.                        ( version 2.01 )
  !/    02-Feb-2001 : Exact-NL version 3.0                ( version 2.07 )
  !/    26-Aug-2002 : Exact-NL version 4.0                ( version 2.22 )
  !/    11-Nov-2002 : Interface fix.                      ( version 3.00 )
  !/    25-Sep-2003 : Exact-NL version 5.0                ( version 3.05 )
  !/    24-Dec-2004 : Multiple model version.             ( version 3.06 )
  !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Interface module to exact nonlinear interactions.
  !
  !  2. Variables and types :
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      W3SNL2    Subr. Public   Interface to Xnl calculation routines.
  !      INSNL2    Subr. Public   Initialization of Xnl routines.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !     See subroutine.
  !
  !  5. Remarks :
  !
  !  6. Switches :
  !
  !       !/S   Enable subroutine tracing.
  !       !/T   Enable general test output.
  !       !/T0  2-D print plot of source term.
  !       !/T1  Print arrays.
  !
  !  7. Source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  PUBLIC
  !/
CONTAINS
!/ ------------------------------------------------------------------- /
!>
!> @brief Interface to exact interactions.
!>
!> @param[in] A      Action spectrum A(ITH,IK) as a function of
!>                   direction (rad)  and wavenumber.
!> @param[in] CG     Group velocities (dimension NK).
!> @param[in] DEPTH  Water depth in meters.
!> @param[out] S     Source term.
!> @param[out] D     Diagonal term of derivative.
!>
!> @author H. L. Tolman
!> @author G. Ph. van Vledder
!> @date   24-Dec-2004
!>
  SUBROUTINE W3SNL2 (  A, CG, DEPTH, S, D )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |        G. Ph. van Vledder         |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         24-Dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    14-Feb-2000 : Origination                         ( version 2.01 )
    !/    02-Feb-2001 : Exact-NL version 3.0                ( version 2.07 )
    !/    26-Aug-2002 : Exact-NL version 4.0                ( version 2.22 )
    !/    11-Nov-2002 : Interface fix                       ( version 3.00 )
    !/    25-Sep-2003 : Exact-NL version 5.0                ( version 3.05 )
    !/    24-Dec-2004 : Multiple model version.             ( version 3.06 )
    !/
    !  1. Purpose :
    !
    !     Interface to exact interactions
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.  I   Action spectrum A(ITH,IK) as a function of
    !                         direction (rad)  and wavenumber.
    !       CG      R.A.  I   Group velocities (dimension NK).
    !       DEPTH   Real  I   Water depth in meters.
    !       S       R.A.  O   Source term.
    !       D       R.A.  O   Diagonal term of derivative.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module     Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD   Subroutine tracing.
    !      xnl_main  Subr. m_xnldata  Main Xnl routine.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3SRCE    Subr. W3SRCEMD Source term integration.
    !      W3EXPO    Subr.   N/A    Point output post-processor.
    !      GXEXPO    Subr.   N/A    GrADS point output post-processor.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !       None.
    !
    !  7. Remarks :
    !
    !     - The following settings are hardwired into the xnl_init routine
    !       of Gerbrant van Vledder.
    !
    !         iufind = 0
    !         iq_prt = 0
    !         iq_test = 0
    !         iq_trace = 0
    !         iq_log = 0
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !       !/S   Enable subroutine tracing.
    !       !/T   Enable general test output.
    !       !/T0  2-D print plot of source term.
    !       !/T1  Print arrays.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    USE CONSTANTS
    USE W3GDATMD, ONLY: NK, NTH, SIG, TH, IQTPE
    USE W3ODATMD, ONLY: NDSE, NDST, IAPROC, NAPERR
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
#ifdef W3_T0
    USE W3ARRYMD, ONLY: PRT2DS
#endif
#ifdef W3_T1
    USE W3ARRYMD, ONLY: OUTMAT
#endif
    USE m_xnldata, ONLY: xnl_main
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)        :: A(NTH,NK), CG(NK), DEPTH
    REAL, INTENT(OUT)       :: S(NTH,NK), D(NTH,NK)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IK, ITH, IERR = 0
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: A2(NK,NTH), S2(NK,NTH), D2(NK,NTH)
#ifdef W3_T0
    REAL                    :: SOUT(NK,NK), DOUT(NK,NK)
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3SNL2')
#endif
#ifdef W3_T
    WRITE (NDST,9000) IQTPE
#endif
    !
    ! 1.  Convert input spectrum ----------------------------------------- *
    !     (Action sigma spectrum, reversed indices)
    !
    DO IK=1, NK
      DO ITH=1, NTH
        A2(IK,ITH) = A(ITH,IK) / CG(IK)
      END DO
    END DO
    !
    ! 2.  Call exact interaction routines -------------------------------- *
    !
    CALL xnl_main ( A2, SIG(1:NK), TH, NK, NTH, DEPTH, IQTPE,       &
         S2, D2, IAPROC, IERR )
    !
    IF ( IERR .NE. 0 ) GOTO 800
    !
    ! 3.  Pack results in proper format ---------------------------------- *
    !
    DO IK=1, NK
      DO ITH=1, NTH
        S(ITH,IK) = S2(IK,ITH) * CG(IK)
        D(ITH,IK) = D2(IK,ITH)
      END DO
    END DO
    !
    ! ... Test output :
    !
#ifdef W3_T0
    DO IK=1, NK
      DO ITH=1, NTH
        SOUT(IK,ITH) = S(IK,ITH) * TPI * SIG(IK) / CG(IK)
        DOUT(IK,ITH) = D(IK,ITH)
      END DO
    END DO
    CALL PRT2DS (NDST, NK, NK, NTH, SOUT, SIG(1:NK), '  ', 1.,  &
         0.0, 0.001, 'Snl(f,t)', ' ', 'NONAME')
    CALL PRT2DS (NDST, NK, NK, NTH, DOUT, SIG(1:NK), '  ', 1.,  &
         0.0, 0.001, 'Diag Snl', ' ', 'NONAME')
#endif
    !
#ifdef W3_T1
    CALL OUTMAT (NDST, S, NTH, NTH, NK, 'Snl')
    CALL OUTMAT (NDST, D, NTH, NTH, NK, 'Diag Snl')
#endif
    !
    RETURN
    !
    !     Error escape locations
    !
800 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) IERR
    CALL EXTCDE ( 1 )
    !
    ! Format statements
    !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3SNL2 :'/                &
         '     xnl_main RETURN CODE NON ZERO : ',I4,' ***'/)
    !
#ifdef W3_T
9000 FORMAT (' TEST W3SNL2 : IQTPE :',I4)
#endif
    !/
    !/ End of W3SNL2 ----------------------------------------------------- /
    !/
  END SUBROUTINE W3SNL2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Preprocessing for nonlinear interactions (Xnl).
  !>
  !> @author H. L. Tolman
  !> @author G. Ph. van Vledder
  !> @date   24-Dec-2004
  !>
  SUBROUTINE INSNL2
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |        G. Ph. van Vledder         |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         24-Dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    02-Feb-2001 : Origination.                        ( version 2.07 )
    !/    25-Sep-2003 : Exact-NL version 5.0                ( version 3.05 )
    !/    24-Dec-2004 : Multiple model version.             ( version 3.06 )
    !/
    !  1. Purpose :
    !
    !     Preprocessing for nonlinear interactions (Xnl).
    !
    !  2. Method :
    !
    !     See Xnl documentation.
    !
    !  3. Parameters :
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module      Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD    Subroutine tracing.
    !      init_constants
    !                Subr. m_xnldata   Xnl initialization routine.
    !      xnl_init  Subr. m_constants Xnl initialization routine.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3IOGR    Subr. W3IOGRMD Model definition file management.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     - See source code.
    !
    !  9. Switches :
    !
    !       !/S      Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD, ONLY: NK, NTH, SIG, TH,                           &
         NLTAIL, DPTHNL, NDPTHS, IQTPE
    USE W3ODATMD, ONLY: NDSE, NDST, IAPROC, NAPERR
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE m_xnldata
    USE m_constants, ONLY: init_constants
    !/
    IMPLICIT NONE
    !/
    !/ Local parameters
    !/
    INTEGER                 :: IGRD, IERR
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: XGRAV
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'INSNL2')
#endif
    !
    ! 1.  Set necessary values : ----------------------------------------- *
    !
    XGRAV  = GRAV
    IGRD   = 3
    !
#ifdef W3_T
    WRITE (NDST,9000) NLTAIL, XGRAV, IQTPE, IGRD, NDPTHS
    WRITE (NDST,9001) DPTHNL
    WRITE (NDST,9002) SIG(1)*TPIINV, SIG(NK)*TPIINV,             &
         TH(1)*RADE, TH(NTH)*RADE
#endif
    !
    ! 2.  Call initialization routines : --------------------------------- *
    !
    CALL init_constants
    !
    CALL xnl_init ( SIG(1:NK), TH, NK, NTH, NLTAIL, XGRAV,          &
         DPTHNL, NDPTHS, IQTPE, IGRD, IAPROC, IERR )
    !
    IF ( IERR .NE. 0 ) GOTO 800
    !
    RETURN
    !
    !     Error escape locations
    !
800 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) IERR
    CALL EXTCDE ( 1 )
    !
    !     Format statements
    !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN INSNL2 :'/                &
         '     xnl_init RETURN CODE NON ZERO : ',I8/)
    !
#ifdef W3_T
9000 FORMAT (' TEST INSNL2 : NLTAIL :',F6.1/                      &
         '               XGRAV  :',F8.3/                      &
         '               IQTPE  :',I4/                        &
         '               IGRD   :',I4/                        &
         '               NDPTHS :',I4,'   (depths follow)')
9001 FORMAT ('             ',5E10.3)
9002 FORMAT ('               FREQS  :',2F8.3/                     &
         '               DIRS   :',2F6.1)
#endif
    !/
    !/ End of INSNL2 ----------------------------------------------------- /
    !/
  END SUBROUTINE INSNL2
  !/
  !/ End of module W3SNL2MD -------------------------------------------- /
  !/
END MODULE W3SNL2MD
