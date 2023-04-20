!> @file
!> @brief Contains module WMUPDTMD.
!>
!> @author H. L. Tolman @date 22-Mar-2021

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Update model input at the driver level of the multi-grid
!>  version of WAVEWATCH III.
!>
!> @author H. L. Tolman @date 22-Mar-2021
!>
MODULE WMUPDTMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         22-Mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    22-Feb-2005 : Origination.                        ( version 3.07 )
  !/    14-Oct-2006 : Adding separate input grids.        ( version 3.10 )
  !/    10-Dec-2006 : Bug fix WMUPD2 initial fields.      ( version 3.10 )
  !/    12-Jan-2007 : General clean-up and bug fixes.     ( version 3.10 )
  !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
  !/                  (W. E. Rogers & T. J. Campbell, NRL)
  !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
  !/                  specify index closure for a grid.   ( version 3.14 )
  !/                  (T. J. Campbell, NRL)
  !/    20-Jan-2017 : Enable using input from coupler     ( version 6.02 )
  !/                  (T. J. Campbell, NRL)
  !/    01-Jul-2019 : Generalize output to curv grids     ( version 7.13 )
  !/                  (R. Padilla-Hernandez, J.H. Alves, EMC/NOAA)
  !/    08-Feb-2021 : Add FSWND option for SMC grid. JGLi ( version 7.13 )
  !/    22-Mar-2021 : Add momentum and air density input  ( version 7.13 )
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Update model input at the driver level of the multi-grid
  !     version of WAVEWATCH III.
  !
  !  2. Variables and types :
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      WMUPDT    Subr. Public   Updating of all model inputs.
  !      WMUPD1    Subr. Public   Native inputs.
  !      WMUPD2    Subr. Public   From input grids.
  !      WMUPDV    Subr. Public   For WMUPD2 vector fields.
  !      WMUPDS    Subr. Public   For WMUPD2 scalar fields.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !     See subroutine documentation.
  !
  !  5. Remarks :
  !
  !  6. Switches :
  !
  !       !/CRX0   Current vector component conservation.
  !       !/CRX1   Current speed conservation.
  !       !/CRX2   Current exenrgy conservation.
  !
  !       !/WNX0   Wind vector component conservation.
  !       !/WNX1   Wind speed conservation.
  !       !/WNX2   Wind exenrgy conservation.
  !
  !       !/S     Enable subroutine tracing.
  !       !/T     Enable test output
  !       !/T1    Test output interpolation data.
  !
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
  PUBLIC
  !/
  INTEGER, PARAMETER      :: SWPMAX = 5   !< SWPMAX
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Update inputs for selected wave model grid.
  !>
  !> @details Reading from native grid files if update is needed based
  !>  on time of data.
  !>
  !> @param[in] IMOD Model number
  !> @param[inout] TDATA Time for which all is data available.
  !> @author H. L. Tolman  @date 22-Mar-2021
  !>
  SUBROUTINE WMUPDT ( IMOD ,TDATA )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    22-Feb-2005 : Origination.                        ( version 3.07 )
    !/    14-Oct-2006 : Adding separate input grids.        ( version 3.10 )
    !/    12-Jan-2007 : General clean-up and bug fixes.     ( version 3.10 )
    !/    20-Jan-2017 : Enable using input from coupler     ( version 6.02 )
    !/                  (T. J. Campbell, NRL)
    !/    22-Mar-2021 : Add momentum and air density input  ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Update inputs for seleceted wave model grid.
    !
    !  2. Method :
    !
    !     Reading from native grid files if update is needed based on
    !     time of data.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IMOD    Int.   I   Model number,
    !       TDATA   I.A.   I   Time for which all is data available.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3SETG    Subr. W3GDATMD Point to grid/model.
    !      W3SETW    Subr. W3WDATMD Point to grid/model.
    !      W3SETI    Subr. W3IDATMD Point to grid/model.
    !      WMSETM    Subr. WMMDATMD Point to grid/model.
    !      STRACE    Subr. W3ERVMD  Subroutine tracing.
    !      EXTCDE    Subr.   Id.    Program abort.
    !      DSEC21    Func. W3TIMEMD Time difference.
    !      STME21    Subr.   Id.    Write time string.
    !      TICK21    Subr.   Id.    Advancing time.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WMWAVE    Subr. WMWAVEMD Multi-grid model main routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !     - IDFLDS dimensioning is hardwired as IDFLDS(-7:9) where
    !       lowest possible value of JFIRST is JFIRST=-7
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !       !/S     Enable subroutine tracing.
    !       !/T     Enable test output
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    USE W3GDATMD, ONLY: W3SETG
    USE W3WDATMD, ONLY: W3SETW
    USE W3IDATMD, ONLY: W3SETI
    USE WMMDATMD, ONLY: WMSETM
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE W3TIMEMD, ONLY: DSEC21, STME21, TICK21
    !/
    USE W3GDATMD, ONLY: NX, NY, FILEXT
    USE W3WDATMD, ONLY: TIME

    USE W3IDATMD, ONLY: INFLAGS1, TLN, TC0, TCN, TW0, TWN, TU0,     &
         TUN, TR0, TRN, TIN, T0N, T1N, T2N, TG0,     &
         TGN, TFN, TDN, TTN, TVN, TZN, TI1, TI2,     &
         TI3, TI4, TI5, JFIRST

    USE WMMDATMD, ONLY: IMPROC, MDSO, MDSS, MDST, MDSE, NMPSCR,     &
         NMPERR, ETIME, FLLSTL, FLLSTR, FLLSTI,      &
         INPMAP, IDINP, IFLSTI, IFLSTL, IFLSTR
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: IMOD
    INTEGER, INTENT(INOUT)  :: TDATA(2)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: MDSEN, J, DTIME(2), IERR, NDTNEW, JJ
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: DTTST
    LOGICAL                 :: FIRST
    CHARACTER(LEN=13)       :: IDFLDS(-7:10)
    CHARACTER(LEN=23)       :: DTME21
    !
    DATA IDFLDS / 'ice param. 1 ' , 'ice param. 2 ' ,               &
         'ice param. 3 ' , 'ice param. 4 ' ,               &
         'ice param. 5 ' ,                                 &
         'mud density  ' , 'mud thkness  ' ,               &
         'mud viscos.  ' ,                                 &
         'water levels ' , 'currents     ' ,               &
         'winds        ' , 'ice fields   ' ,               &
         'momentum     ' , 'air density  ' ,               &
         'mean param.  ' , '1D spectra   ' ,               &
         '2D spectra   ' , 'grid speed   ' /
    !/
    !/ ------------------------------------------------------------------- /
    ! 0.  Initialization
    ! 0.a Subroutine tracing and echo of input
    !
#ifdef W3_S
    CALL STRACE (IENT, 'WMUPDT')
#endif
#ifdef W3_T
    WRITE (MDST,9000) IMOD, TDATA
#endif
    !
    IF ( IMPROC .EQ. NMPERR ) THEN
      MDSEN  = MDSE
    ELSE
      MDSEN  = -1
    END IF
    !
    ! 0.b Point to proper grids and initialize
    !
    CALL W3SETG ( IMOD, MDSE, MDST )
    CALL W3SETW ( IMOD, MDSE, MDST )
    CALL W3SETI ( IMOD, MDSE, MDST )
    CALL WMSETM ( IMOD, MDSE, MDST )
    !
    FLLSTL = .FALSE.
    FLLSTI = .FALSE.
    FLLSTR = .FALSE.
    IERR   = 0
    !
    ! 0.c Output
    !
    IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
      CALL STME21 ( TIME , DTME21 )
      WRITE (MDSS,900) IMOD, DTME21
    END IF
    !
#ifdef W3_T
    WRITE (MDST,9001) ' J', '0-N', TIME, ETIME
    IF (LBOUND(IDINP,2).LE.-7) WRITE (MDST,9002) -7, IDINP(IMOD,-7), INFLAGS1(-7), TI1
    IF (LBOUND(IDINP,2).LE.-6) WRITE (MDST,9002) -6, IDINP(IMOD,-6), INFLAGS1(-6), TI2
    IF (LBOUND(IDINP,2).LE.-5) WRITE (MDST,9002) -5, IDINP(IMOD,-5), INFLAGS1(-5), TI3
    IF (LBOUND(IDINP,2).LE.-4) WRITE (MDST,9002) -4, IDINP(IMOD,-4), INFLAGS1(-4), TI4
    IF (LBOUND(IDINP,2).LE.-3) WRITE (MDST,9002) -3, IDINP(IMOD,-3), INFLAGS1(-3), TI5
    IF (LBOUND(IDINP,2).LE.-2) WRITE (MDST,9002) -2, IDINP(IMOD,-2), INFLAGS1(-2), TZN
    IF (LBOUND(IDINP,2).LE.-1) WRITE (MDST,9002) -1, IDINP(IMOD,-1), INFLAGS1(-1), TTN
    IF (LBOUND(IDINP,2).LE. 0) WRITE (MDST,9002)  0, IDINP(IMOD, 0), INFLAGS1( 0), TVN
    WRITE (MDST,9002) 1, IDINP(IMOD,1), INFLAGS1(1), TLN
    WRITE (MDST,9003) 2, IDINP(IMOD,2), INFLAGS1(2), TC0, TCN
    WRITE (MDST,9003) 3, IDINP(IMOD,3), INFLAGS1(3), TW0, TWN
    WRITE (MDST,9002) 4, IDINP(IMOD,4), INFLAGS1(4), TIN
    WRITE (MDST,9003) 5, IDINP(IMOD,5), INFLAGS1(5), TU0, TUN
    WRITE (MDST,9003) 6, IDINP(IMOD,6), INFLAGS1(6), TR0, TRN
    WRITE (MDST,9002) 7, IDINP(IMOD,7), INFLAGS1(7), T0N
    WRITE (MDST,9002) 8, IDINP(IMOD,8), INFLAGS1(8), T1N
    WRITE (MDST,9002) 9, IDINP(IMOD,9), INFLAGS1(9), T2N
    WRITE (MDST,9003) 10, 'MOV'        , INFLAGS1(10), TG0, TGN
    WRITE (MDST,9004)    'GRD', NX, NY
#endif
    !
    ! 1.  Loop over input types ------------------------------------------ /
    !
    DO J=JFIRST, 10
      !
      ! 1.a Check if update needed
      !
      IF ( .NOT. INFLAGS1(J) ) CYCLE
      !
#ifdef W3_T
      WRITE (MDST,9010) J, INFLAGS1(J), INPMAP(IMOD,J)
#endif
      !
      ! 1.b Test time
      !
      IF ( TFN(1,J) .EQ. -1 ) THEN
        FIRST  = .TRUE.
        DTTST  = 0.
      ELSE
        FIRST  = .FALSE.
        DTTST  = DSEC21 ( TIME , TFN(:,J) )
      END IF
      !
#ifdef W3_T
      WRITE (MDST,9011) IDINP(IMOD,J), DTTST, TFN(:,J)
#endif
      !
      IF ( DTTST .GT. 0. ) CYCLE
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                    &
           WRITE (MDSS,901) IDFLDS(J)
      !
      ! 2.  Forcing input from file & defined on the native grid ----------- /
      !
      IF ( INPMAP(IMOD,J) .EQ. 0 ) THEN
        !
#ifdef W3_T
        WRITE (MDST,9020)
#endif
        !
        CALL WMUPD1 ( IMOD, IDINP(IMOD,J), J, IERR )
        !
        ! 3.  Forcing input from file & defined on an input grid ------------- /
        !
      ELSE IF ( INPMAP(IMOD,J) .GT. 0 ) THEN
        !
#ifdef W3_T
        WRITE (MDST,9030) INPMAP(IMOD,J)
#endif
        !
        ! 3.a Check if input grid is available
        !
        JJ     = -INPMAP(IMOD,J)
        CALL W3SETG ( JJ, MDSE, MDST )
        CALL W3SETI ( JJ, MDSE, MDST )
        !
        IF ( TFN(1,J) .EQ. -1 ) THEN
          DTTST  = 0.
        ELSE
          IF ( FIRST .OR. ( J.EQ.1 .AND. IFLSTL(-JJ) )          &
               .OR. ( J.EQ.4 .AND. IFLSTI(-JJ) )          &
               .OR. ( J.EQ.6 .AND. IFLSTR(-JJ) ) ) THEN
            DTTST  = 1.
          ELSE
            DTTST  = DSEC21 ( TIME , TFN(:,J) )
          END IF
        END IF
        !
        IF ( J .EQ. 1 ) FLLSTL = IFLSTL(-JJ)
        IF ( J .EQ. 4 ) FLLSTI = IFLSTI(-JJ)
        IF ( J .EQ. 6 ) FLLSTR = IFLSTR(-JJ)
        !
#ifdef W3_T
        WRITE (MDST,9031) J, IDINP(JJ,J), DTTST, TFN(:,J)
#endif
        !
        ! 3.b If needed, update input grid
        !     Note: flags in WMMDATMD set for grid IMOD !
        !
        IF ( DTTST .LE. 0. ) THEN
          !
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )            &
               WRITE (MDSS,930) FILEXT
          !
          CALL WMUPD1 ( JJ, IDINP(JJ,J), J, IERR )
          !
          IF ( J .EQ. 1 ) IFLSTL(-JJ) = FLLSTL
          IF ( J .EQ. 4 ) IFLSTI(-JJ) = FLLSTI
          IF ( J .EQ. 6 ) IFLSTR(-JJ) = FLLSTR
          !
        END IF
        !
        ! 3.c Set up for update, and call updating routine
        !
        CALL W3SETG ( IMOD, MDSE, MDST )
        CALL W3SETI ( IMOD, MDSE, MDST )
        !
        CALL WMUPD2 ( IMOD, J, JJ, IERR )
        !
        ! 4.  Forcing input from CPL ----------------------------------------- /
        !
      ELSE ! INPMAP(IMOD,J) .LT. 0
        ! Data input and time stamp settings for forcing input from
        ! CPL are handled in wmesmfmd.ftn:GetImport
        !
#ifdef W3_T
        IF ( INPMAP(IMOD,J) .EQ. -999 ) THEN
          ! *** Forcing input from CPL & defined on native grid ***
          WRITE (MDST,9040)
        ELSE
          ! *** Forcing input from CPL & defined on an input grid ***
          WRITE (MDST,9050) -INPMAP(IMOD,J)
        END IF
#endif
        !
      END IF
      !
      ! 5.  Finalize for each type ----------------------------------------- /
      ! 5.a Process IERR output
      !
      IF ( IERR.GT.0 ) GOTO 2000
      IF ( IERR.LT.0 .AND. MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )    &
           WRITE (MDSS,950) IDFLDS(J)
      !
      ! 5.b End of master loop
      !
    END DO
    !
    ! 6.  Compute TDATA -------------------------------------------------- /
    !
    TDATA  = ETIME
    !
    DO J=JFIRST, 10
      IF ( .NOT. INFLAGS1(J) ) CYCLE
      DTTST  = DSEC21 ( TFN(:,J) , TDATA )
      IF ( DTTST.GT.0. .AND. .NOT.  ( (FLLSTL .AND. J.EQ.1) .OR.    &
           (FLLSTI .AND. J.EQ.4) .OR.    &
           (FLLSTR .AND. J.EQ.6) ) ) THEN
        TDATA  = TFN(:,J)
      END IF
    END DO
    !
    ! 6.  Compute TDN ---------------------------------------------------- /
    !
    TDN    = TDATA
    CALL TICK21 ( TDN, 1. )
    DO J=7, 9
      IF ( INFLAGS1(J) ) THEN
        DTTST  = DSEC21 ( TFN(:,J) , TDN )
        IF ( DTTST.GT.0. ) TDN = TFN(:,J)
      END IF
    END DO
    !
    ! 7.  Final test output ---------------------------------------------- /
    !
#ifdef W3_T
    WRITE (MDST,9070) ' J', '0-N', TIME, ETIME, TDATA
    IF (LBOUND(IDINP,2).LE.-7) WRITE (MDST,9071) -7, IDINP(IMOD,-7), INFLAGS1(-7), TI1
    IF (LBOUND(IDINP,2).LE.-6) WRITE (MDST,9071) -6, IDINP(IMOD,-6), INFLAGS1(-6), TI2
    IF (LBOUND(IDINP,2).LE.-5) WRITE (MDST,9071) -5, IDINP(IMOD,-5), INFLAGS1(-5), TI3
    IF (LBOUND(IDINP,2).LE.-4) WRITE (MDST,9071) -4, IDINP(IMOD,-4), INFLAGS1(-4), TI4
    IF (LBOUND(IDINP,2).LE.-3) WRITE (MDST,9071) -3, IDINP(IMOD,-3), INFLAGS1(-3), TI5
    IF (LBOUND(IDINP,2).LE.-2) WRITE (MDST,9071) -2, IDINP(IMOD,-2), INFLAGS1(-2), TZN
    IF (LBOUND(IDINP,2).LE.-1) WRITE (MDST,9071) -1, IDINP(IMOD,-1), INFLAGS1(-1), TTN
    IF (LBOUND(IDINP,2).LE. 0) WRITE (MDST,9071)  0, IDINP(IMOD, 0), INFLAGS1( 0), TVN
    WRITE (MDST,9071) 1, IDINP(IMOD,1), INFLAGS1(1), TLN
    WRITE (MDST,9072) 2, IDINP(IMOD,2), INFLAGS1(2), TC0, TCN
    WRITE (MDST,9072) 3, IDINP(IMOD,3), INFLAGS1(3), TW0, TWN
    WRITE (MDST,9071) 4, IDINP(IMOD,4), INFLAGS1(4), TIN
    WRITE (MDST,9072) 5, IDINP(IMOD,5), INFLAGS1(5), TU0, TUN
    WRITE (MDST,9072) 6, IDINP(IMOD,6), INFLAGS1(6), TR0, TRN
    WRITE (MDST,9071) 7, IDINP(IMOD,7), INFLAGS1(7), T0N
    WRITE (MDST,9071) 8, IDINP(IMOD,8), INFLAGS1(8), T1N
    WRITE (MDST,9073) 9, IDINP(IMOD,9), INFLAGS1(9), T2N, TDN
    WRITE (MDST,9072) 10, 'MOV'        , INFLAGS1(10), TG0, TGN
#endif
    !
    RETURN
    !
    ! Error escape locations
    !
2000 CONTINUE
    CALL EXTCDE ( 2000 )
    RETURN
    !
    ! Formats
    !
900 FORMAT ( '  Updating input for grid',I3,' at ',A)
901 FORMAT ( '     Updating ',A)
930 FORMAT ( '        First updating ',A)
950 FORMAT ( '        Past last ',A)
    !
#ifdef W3_T
9000 FORMAT ( ' TEST WMUPDT : INPUT : ',I4,I10.8,I7.6,            &
         '  <============================')
9001 FORMAT ( ' TEST WMUPDT : ',A2,1X,A3,3X,    2(I10.8,I7.6))
9002 FORMAT ( '               ',I2,1X,A3,L3,17X,1(I10.8,I7.6))
9003 FORMAT ( '               ',I2,1X,A3,L3,    2(I10.8,I7.6))
9004 FORMAT ( '               ',2X,1X,A3,3X,2I10             )
9010 FORMAT ( ' TEST WMUPDT : J, FLAG, INPMAP : ',I2,L2,I4)
9011 FORMAT ( ' TEST WMUPDT : ',A,', DTTST = ',E10.3,2X,I9.8,I7.6)
9020 FORMAT ( ' TEST WMUPDT : FORCING INPUT FROM FILE & DEFINED ON THE NATIVE GRID')
9030 FORMAT ( ' TEST WMUPDT : FORCING INPUT FROM FILE & DEFINED ON INPUT GRID',I4)
9031 FORMAT ( ' TEST WMUPDT : J =',I4,3XA,', DTTST = ',           &
         E10.3,2X,I9.8,I7.6)
9040 FORMAT ( ' TEST WMUPDT : FORCING INPUT FROM CPL & DEFINED ON THE NATIVE GRID')
9050 FORMAT ( ' TEST WMUPDT : FORCING INPUT FROM CPL & DEFINED ON INPUT GRID',I4)
9070 FORMAT ( ' TEST WMUPDT : ',A2,1X,A3,3X,    3(I10.8,I7.6))
9071 FORMAT ( '               ',I2,1X,A3,L3,17X,1(I10.8,I7.6))
9072 FORMAT ( '               ',I2,1X,A3,L3,    2(I10.8,I7.6))
9073 FORMAT ( '               ',I2,1X,A3,L3,17X,2(I10.8,I7.6))
#endif
    !/
    !/ End of WMUPDT ----------------------------------------------------- /
    !/
  END SUBROUTINE WMUPDT
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Update selected input using native input files.
  !>
  !> @details Reading from native grid files.
  !>
  !> @param[in] IMOD Model number.
  !> @param[in] IDSTR ID string corresponding to J.
  !> @param[in] J Input type.
  !> @param[out] IERR Error indicator.
  !> @author H. L. Tolman  @date 22-Mar-2021
  !>
  SUBROUTINE WMUPD1 ( IMOD, IDSTR, J, IERR )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    07-Oct-2006 : Origination.                        ( version 3.10 )
    !/    22-Mar-2021 : Add momentum and air density input  ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Update selected input using native input files.
    !
    !  2. Method :
    !
    !     Reading from native grid files.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IMOD    Int.   I   Model number,
    !       IDSTR   C*3    I   ID string corresponding to J.
    !       J       Int.   I   Input type.
    !       IERR    Int.   O   Error indicator.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WMDIMD    Subr. WMMDATMD Set dimension of data grids.
    !      W3FLDG    Subr. W3FLDSMD Get input field.
    !      W3FLDD    Subr.   Id.    Get input data.
    !      W3FLDM    Subr.   Id.    Get grid speed data.
    !      STRACE    Subr. W3ERVMD  Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WMUPDT    Subr. WMUPDTMD Master inpu update routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !     - Pointers set in calling routine.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !       !/S     Enable subroutine tracing.
    !       !/T     Enable test output
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    USE WMMDATMD, ONLY: WMDIMD
    USE W3FLDSMD, ONLY: W3FLDG, W3FLDD, W3FLDM
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    USE W3GDATMD, ONLY: NX, NY
#ifdef W3_SMC
    USE W3GDATMD, ONLY: FSWND, NSEA
#endif
    USE W3WDATMD, ONLY: TIME
    USE W3IDATMD, ONLY: TLN, WLEV, TC0, TCN, CX0, CXN, CY0, CYN,    &
         TW0, TWN, TU0, TUN, TR0, TRN, WX0, WXN,     &
         WY0, WYN, DT0, DTN, TIN, TRN, ICEI, UX0,    &
         UXN, UY0, UYN, RH0, RHN, T0N, T1N, T2N,     &
         TDN, INFLAGS1, TG0, TGN, GA0, GD0, GAN,     &
         GDN, BERGI, TTN, MUDT, TVN, MUDV, TZN,      &
         MUDD, TI1, TI2, TI3, TI4, TI5, ICEP1,       &
         ICEP2, ICEP3, ICEP4, ICEP5
    USE WMMDATMD, ONLY: IMPROC, NMPERR, MDST, MDSE, MDSF, ETIME,    &
         FLLSTL, FLLSTI, FLLSTR, RCLD, NDT, DATA0,   &
         DATA1, DATA2, NMV, NMVMAX, TMV, AMV, DMV
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)          :: IMOD, J
    INTEGER, INTENT(OUT)         :: IERR
    CHARACTER(LEN=3), INTENT(IN) :: IDSTR
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: MDSEN, DTIME(2), NDTNEW
    REAL                    :: XXX(NY,NX)
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    ! 0.  Initialization
    ! 0.a Subroutine tracing and echo of input
    !
#ifdef W3_S
    CALL STRACE (IENT, 'WMUPD1')
#endif
#ifdef W3_T
    WRITE (MDST,9000) IMOD, J
#endif
    !
    IF ( IMPROC .EQ. NMPERR ) THEN
      MDSEN  = MDSE
    ELSE
      MDSEN  = -1
    END IF
    !
    ! 0.b Start case selection
    !
    SELECT CASE (J)
      !
      ! -7.  Ice parameter 1 ---------------------------------------------- /
      !
    CASE (-7)
      CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           NX, NY, NX, NY, TIME, ETIME, DTIME,            &
           XXX, XXX, XXX, TI1, XXX, XXX, ICEP1, IERR)
      !
      ! -6.  Ice parameter 2 ---------------------------------------------- /
      !
    CASE (-6)
      CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           NX, NY, NX, NY, TIME, ETIME, DTIME,            &
           XXX, XXX, XXX, TI2, XXX, XXX, ICEP2, IERR)
      !
      ! -5.  Ice parameter 3 ---------------------------------------------- /
      !
    CASE (-5)
      CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           NX, NY, NX, NY, TIME, ETIME, DTIME,            &
           XXX, XXX, XXX, TI3, XXX, XXX, ICEP3, IERR)
      !
      ! -4.  Ice parameter 4 ---------------------------------------------- /
      !
    CASE (-4)
      CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           NX, NY, NX, NY, TIME, ETIME, DTIME,            &
           XXX, XXX, XXX, TI4, XXX, XXX, ICEP4, IERR)
      !
      ! -3.  Ice parameter 5 ---------------------------------------------- /
      !
    CASE (-3)
      CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           NX, NY, NX, NY, TIME, ETIME, DTIME,            &
           XXX, XXX, XXX, TI5, XXX, XXX, ICEP5, IERR)
      !
      ! -2.  Mud Density -------------------------------------------------- /
      !
    CASE (-2)
      CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           NX, NY, NX, NY, TIME, ETIME, DTIME,            &
           XXX, XXX, XXX, TZN, XXX, XXX, MUDD, IERR)
      !
      ! -1.  Mud Thickness -------------------------------------------------- /
      !
    CASE (-1)
      CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           NX, NY, NX, NY, TIME, ETIME, DTIME,            &
           XXX, XXX, XXX, TTN, XXX, XXX, MUDT, IERR)
      !
      ! 0.  Mud Viscosity -------------------------------------------------- /
      !
    CASE (0)
      CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           NX, NY, NX, NY, TIME, ETIME, DTIME,            &
           XXX, XXX, XXX, TVN, XXX, XXX, MUDV, IERR)
      !
      ! 1.  Water levels --------------------------------------------------- /
      !
    CASE (1)
      CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           NX, NY, NX, NY, TIME, ETIME, DTIME,            &
           XXX, XXX, XXX, TLN, XXX, XXX, WLEV, IERR)
      IF ( IERR .LT. 0 ) FLLSTL = .TRUE.
      !
      ! 2.  Currents ------------------------------------------------------- /
      !
    CASE (2)
#ifdef W3_SMC
      !!Li  For sea point current option FSWND.   JGLi08Feb2021
      IF( FSWND ) THEN
        CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
             NSEA, 1, NSEA, 1, TIME, ETIME, TC0,            &
             CX0, CY0, XXX, TCN, CXN, CYN, XXX, IERR)
      ELSE
#endif
        CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
             NX, NY, NX, NY, TIME, ETIME, TC0,              &
             CX0, CY0, XXX, TCN, CXN, CYN, XXX, IERR)
#ifdef W3_SMC
      END IF
#endif
      !
      ! 3.  Winds ---------------------------------------------------------- /
      !
    CASE (3)
#ifdef W3_SMC
      !!Li  For sea point wind option FSWND.   JGLi08Feb2021
      IF( FSWND ) THEN
        CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
             NSEA, 1, NSEA, 1, TIME, ETIME, TW0,            &
             WX0, WY0, DT0, TWN, WXN, WYN, DTN, IERR)
      ELSE
#endif
        CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
             NX, NY, NX, NY, TIME, ETIME, TW0,              &
             WX0, WY0, DT0, TWN, WXN, WYN, DTN, IERR)
#ifdef W3_SMC
      END IF
#endif
      !
      ! 4.  Ice ------------------------------------------------------------ /
      !
    CASE (4)
      CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           NX, NY, NX, NY, TIME, ETIME, DTIME,            &
           XXX, XXX, XXX, TIN, XXX , BERGI, ICEI, IERR)
      IF ( IERR .LT. 0 ) FLLSTI = .TRUE.
      !
      ! 5.  Momentum ------------------------------------------------------- /
      !
    CASE (5)
      CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           NX, NY, NX, NY, TIME, ETIME, TU0,              &
           UX0, UY0, XXX, TUN, UXN, UYN, XXX, IERR)
      !
      ! 6.  Air density ---------------------------------------------------- /
      !
    CASE (6)
      CALL W3FLDG ('READ', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           NX, NY, NX, NY, TIME, ETIME, TR0,              &
           XXX, XXX, RH0, TRN, XXX, XXX, RHN, IERR)
      IF ( IERR .LT. 0 ) FLLSTR = .TRUE.
      !
      ! 7.  Data type 0 ---------------------------------------------------- /
      !
    CASE (7)
      CALL W3FLDD ('SIZE', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           TIME, T0N, RCLD(1), NDT(1), NDTNEW,           &
           DATA0, IERR )
      IF ( IERR .LT. 0 ) THEN
        INFLAGS1(J) = .FALSE.
        RCLD(1)  = 1
        NDT(1)   = 1
        CALL WMDIMD ( IMOD, MDSE, MDST, 1 )
      ELSE
        NDT(J) = NDTNEW
        CALL WMDIMD ( IMOD, MDSE, MDST, 1 )
        CALL W3FLDD ('SIZE', IDSTR, MDSF(IMOD,J), MDST,         &
             MDSEN, TIME, T0N, RCLD(1), NDT(1),        &
             NDTNEW, DATA0, IERR )
      END IF
      !
      ! 8.  Data type 1 ---------------------------------------------------- /
      !
    CASE ( 8 )
      CALL W3FLDD ('SIZE', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           TIME, T1N, RCLD(2), NDT(2), NDTNEW,           &
           DATA1, IERR )
      IF ( IERR .LT. 0 ) THEN
        INFLAGS1(J) = .FALSE.
        RCLD(2)  = 1
        NDT(2)   = 1
        CALL WMDIMD ( IMOD, MDSE, MDST, 2 )
      ELSE
        NDT(J) = NDTNEW
        CALL WMDIMD ( IMOD, MDSE, MDST, 2 )
        CALL W3FLDD ('SIZE', IDSTR, MDSF(IMOD,J), MDST,         &
             MDSEN, TIME, T1N, RCLD(2), NDT(2),         &
             NDTNEW, DATA1, IERR )
      END IF
      !
      ! 9.  Data type 2 ---------------------------------------------------- /
      !
    CASE ( 9 )
      CALL W3FLDD ('SIZE', IDSTR, MDSF(IMOD,J), MDST, MDSEN,      &
           TIME, T2N, RCLD(3), NDT(3), NDTNEW,           &
           DATA2, IERR )
      IF ( IERR .LT. 0 ) THEN
        INFLAGS1(J) = .FALSE.
        RCLD(3)  = 1
        NDT(3)   = 1
        CALL WMDIMD ( IMOD, MDSE, MDST, 3 )
      ELSE
        NDT(J) = NDTNEW
        CALL WMDIMD ( IMOD, MDSE, MDST, 3 )
        CALL W3FLDD ('SIZE', IDSTR, MDSF(IMOD,J), MDST,         &
             MDSEN, TIME, T2N, RCLD(3), NDT(3),         &
             NDTNEW, DATA2, IERR )
      END IF
      !
      ! 10. Moving grid data ---------------------------------------------- /
      !
    CASE ( 10 )
      ! notes:
      ! SUBROUTINE W3FLDM in w3fldsmd.ftn :
      !       INTEGER, INTENT(INOUT)  :: NH, THO(2,6,NHM), TF0(2), TFN(2)
      !       INTEGER, INTENT(INOUT)  :: NH, THO(2,-7:6,NHM), TF0(2), TFN(2)
      !       REAL, INTENT(INOUT)     :: HA(NHM,6), HD(NHM,6), A0, AN, D0, DN
      !       REAL, INTENT(INOUT)     :: HA(NHM,-7:6), HD(NHM,-7:6), A0, AN, D0, DN
      ! Arguments #
      !   THO     8
      !   HA      9
      !   HD     10
      ! Here, that is TMV AMV DMV
      CALL W3FLDM ( 4, MDST, MDSEN, TIME, ETIME, NMV, NMVMAX, TMV,&
           AMV, DMV, TG0, GA0, GD0, TGN, GAN, GDN, IERR )
      !
    END SELECT
    !
    ! 9. End of routine -------------------------------------------------- /
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT ( ' TEST WMUPD1 : INPUT : ',2I4)
#endif
    !/
    !/ End of WMUPD1 ----------------------------------------------------- /
    !/
  END SUBROUTINE WMUPD1
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Update selected input using input grids.
  !>
  !> @details Managing data, interpolation done in other routines.
  !>
  !> @param[in] IMOD Model number.
  !> @param[in] J Input type.
  !> @param[in] JMOD Model number source grid.
  !> @param[out] IERR Error indicator.
  !>
  !> @author H. L. Tolman @date 22-Mar-2021
  !>
  SUBROUTINE WMUPD2 ( IMOD, J, JMOD, IERR )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    14-Oct-2006 : Origination.                        ( version 3.10 )
    !/    10-Dec-2006 : Bug fix WMUPD2 initial fields.      ( version 3.10 )
    !/    22-Mar-2021 : Add momentum and air density input  ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Update selected input using input grids.
    !
    !  2. Method :
    !
    !     Managing data, interpolation done inother routines.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IMOD    Int.   I   Model number,
    !       J       Int.   I   Input type.
    !       JMOD    Int.   I   Model number source grid.
    !       IERR    Int.   O   Error indicator.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3ERVMD  Subroutine tracing.
    !      EXTCDE    Subr.   Id.    Program abort.
    !      WMUPDV    Subr.  local   Interpolation of vector fields.
    !      WMUPDS    Subr.  local   Interpolation of scalar fields.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WMUPDT    Subr. WMUPDTMD Master input update routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !       !/CRX0   Current vector component conservation.
    !       !/CRX1   Current speed conservation.
    !       !/CRX2   Current exenrgy conservation.
    !
    !       !/WNX0   Wind vector component conservation.
    !       !/WNX1   Wind speed conservation.
    !       !/WNX2   Wind exenrgy conservation.
    !
    !       !/S     Enable subroutine tracing.
    !       !/T     Enable test output
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    USE W3SERVMD,  ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD,  ONLY: STRACE
#endif
    !/
    USE W3WDATMD,  ONLY: TIME
    USE W3IDATMD,  ONLY: INPUTS
    USE WMMDATMD,  ONLY: IMPROC, NMPERR, NMPSCR, MDST, MDSE, MDSS,  &
         MDSO, ETIME, IDINP
    USE CONSTANTS, ONLY: DAIR
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: IMOD, J, JMOD
    INTEGER, INTENT(OUT)    :: IERR
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: ICONSC, ICONSW, ICONSU
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    ! 0.  Initialization
    ! 0.a Subroutine tracing and echo of input
    !
#ifdef W3_S
    CALL STRACE (IENT, 'WMUPD2')
#endif
    !
#ifdef W3_T
    WRITE (MDST,9000) IMOD, J, JMOD
    WRITE (MDST,9001) INPUTS(IMOD)%TFN(:,J),                     &
         INPUTS(JMOD)%TFN(:,J), ETIME
#endif
    !
    IERR = 0
#ifdef W3_CRX0
    ICONSC = 0
#endif
#ifdef W3_CRX1
    ICONSC = 1
#endif
#ifdef W3_CRX2
    ICONSC = 2
#endif
#ifdef W3_WNX0
    ICONSW = 0
#endif
#ifdef W3_WNX1
    ICONSW = 1
#endif
#ifdef W3_WNX2
    ICONSW = 2
#endif
#ifdef W3_WNX0
    ICONSU = 0
#endif
#ifdef W3_WNX1
    ICONSU = 1
#endif
#ifdef W3_WNX2
    ICONSU = 2
#endif
    !
    ! 1. Shift fields ( currents and winds only ) ------------------------ /
    !
    SELECT CASE (J)
      !
      ! 1.a Currents
      !
    CASE (2)
      IF ( INPUTS(IMOD)%TFN(1,J) .GT. 0 ) THEN
        INPUTS(IMOD)%TC0(:) = INPUTS(IMOD)%TFN(:,J)
        INPUTS(IMOD)%CX0    = INPUTS(IMOD)%CXN
        INPUTS(IMOD)%CY0    = INPUTS(IMOD)%CYN
#ifdef W3_T
        WRITE (MDST,9010) J, INPUTS(IMOD)%TFN(:,J)
      ELSE
        WRITE (MDST,9011) J
#endif
      END IF
      !
      ! 1.b Winds
      !
    CASE (3)
      IF ( INPUTS(IMOD)%TFN(1,J) .GT. 0 ) THEN
        INPUTS(IMOD)%TW0(:) = INPUTS(IMOD)%TFN(:,J)
        INPUTS(IMOD)%WX0    = INPUTS(IMOD)%WXN
        INPUTS(IMOD)%WY0    = INPUTS(IMOD)%WYN
        INPUTS(IMOD)%DT0    = INPUTS(IMOD)%DTN
#ifdef W3_T
        WRITE (MDST,9010) J, INPUTS(IMOD)%TFN(:,J)
      ELSE
        WRITE (MDST,9011) J
#endif
      END IF
      !
      ! 1.c Momentum
      !
    CASE (5)
      IF ( INPUTS(IMOD)%TFN(1,J) .GT. 0 ) THEN
        INPUTS(IMOD)%TU0(:) = INPUTS(IMOD)%TFN(:,J)
        INPUTS(IMOD)%UX0    = INPUTS(IMOD)%UXN
        INPUTS(IMOD)%UY0    = INPUTS(IMOD)%UYN
#ifdef W3_T
        WRITE (MDST,9010) J, INPUTS(IMOD)%TFN(:,J)
      ELSE
        WRITE (MDST,9011) J
#endif
      END IF
      !
    END SELECT
    !
    ! 2. Process fields at ending time ----------------------------------- /
    !
#ifdef W3_T
    WRITE (MDST,9020) J, INPUTS(JMOD)%TFN(:,J)
#endif
    INPUTS(IMOD)%TFN(:,J) = INPUTS(JMOD)%TFN(:,J)
    !
    SELECT CASE (J)
      !
      ! 2.a-3 Ice parameter 1
      !
    CASE (-7)
      CALL WMUPDS ( IMOD, INPUTS(IMOD)%ICEP1,                      &
           JMOD, INPUTS(JMOD)%ICEP1, 0. )
      !
      ! 2.a-3 Ice parameter 2
      !
    CASE (-6)
      CALL WMUPDS ( IMOD, INPUTS(IMOD)%ICEP2,                      &
           JMOD, INPUTS(JMOD)%ICEP2, 0. )
      !
      ! 2.a-3 Ice parameter 3
      !
    CASE (-5)
      CALL WMUPDS ( IMOD, INPUTS(IMOD)%ICEP3,                      &
           JMOD, INPUTS(JMOD)%ICEP3, 0. )
      !
      ! 2.a-3 Ice parameter 4
      !
    CASE (-4)
      CALL WMUPDS ( IMOD, INPUTS(IMOD)%ICEP4,                      &
           JMOD, INPUTS(JMOD)%ICEP4, 0. )
      !
      ! 2.a-3 Ice parameter 5
      !
    CASE (-3)

      CALL WMUPDS ( IMOD, INPUTS(IMOD)%ICEP5,                      &
           JMOD, INPUTS(JMOD)%ICEP5, 0. )
      !
      ! 2.a-2 Mud densities
      !
    CASE (-2)
      CALL WMUPDS ( IMOD, INPUTS(IMOD)%MUDD,                      &
           JMOD, INPUTS(JMOD)%MUDD, 0. )
      !
      ! 2.a-1 Mud viscosities
      !
    CASE (-1)
      CALL WMUPDS ( IMOD, INPUTS(IMOD)%MUDT,                      &
           JMOD, INPUTS(JMOD)%MUDT, 0. )
      !
      ! 2.a-0 Mud thicknesses
      !
    CASE (0)
      CALL WMUPDS ( IMOD, INPUTS(IMOD)%MUDV,                      &
           JMOD, INPUTS(JMOD)%MUDV, 0. )
      !
      ! 2.a Water levels
      !
    CASE (1)
      CALL WMUPDS ( IMOD, INPUTS(IMOD)%WLEV,                      &
           JMOD, INPUTS(JMOD)%WLEV, 0. )
      !
      ! 2.b Curents
      !
    CASE (2)
      CALL WMUPDV ( IMOD, INPUTS(IMOD)%CXN, INPUTS(IMOD)%CYN,     &
           JMOD, INPUTS(JMOD)%CXN, INPUTS(JMOD)%CYN,     &
           0., ICONSC )
      !
      ! 2.c Wind speeds
      !
    CASE (3)
      CALL WMUPDV ( IMOD, INPUTS(IMOD)%WXN, INPUTS(IMOD)%WYN,     &
           JMOD, INPUTS(JMOD)%WXN, INPUTS(JMOD)%WYN,     &
           0., ICONSW )
      !
      IF ( IDINP(IMOD,J) .EQ. 'WNS' ) CALL WMUPDS                 &
           ( IMOD, INPUTS(IMOD)%DTN,                       &
           JMOD, INPUTS(JMOD)%DTN, 0. )
      !
      ! 2.d Ice concentrations
      !
    CASE (4)
      CALL WMUPDS ( IMOD, INPUTS(IMOD)%ICEI,                      &
           JMOD, INPUTS(JMOD)%ICEI, 0. )
      IF ( IDINP(IMOD,J) .EQ. 'ISI' ) CALL WMUPDS                 &
           ( IMOD, INPUTS(IMOD)%BERGI,                     &
           JMOD, INPUTS(JMOD)%BERGI, 0. )
      !
      ! 2.e Momentum
      !
    CASE (5)
      CALL WMUPDV ( IMOD, INPUTS(IMOD)%UXN, INPUTS(IMOD)%UYN,     &
           JMOD, INPUTS(JMOD)%UXN, INPUTS(JMOD)%UYN,     &
           0., ICONSU )
      !
      ! 2.f Air density
      !
    CASE (6)
      CALL WMUPDS ( IMOD, INPUTS(IMOD)%RHN,                       &
           JMOD, INPUTS(JMOD)%RHN, DAIR )
      !
      ! 2.g Assimilation data 0
      !
    CASE (7)
      GOTO 2999
      !
      ! 2.h Assimilation data 1
      !
    CASE (8)
      GOTO 2999
      !
      ! 2.i Assimilation data 2
      !
    CASE (9)
      GOTO 2999
      !
    END SELECT
    !
    ! 3. Check and update first fields ( currents and winds only ) ------- /
    !
    SELECT CASE (J)
      !
      ! 3.a Currents
      !
    CASE (2)
      IF ( INPUTS(IMOD)%TC0(1) .LT. 0 ) THEN
        INPUTS(IMOD)%TC0(:) = INPUTS(JMOD)%TC0(:)
#ifdef W3_T
        WRITE (MDST,9030) J, INPUTS(IMOD)%TC0(:)
#endif
#ifdef W3_CRX0
        ICONSC = 0
#endif
#ifdef W3_CRX1
        ICONSC = 1
#endif
#ifdef W3_CRX2
        ICONSC = 2
#endif
        CALL WMUPDV ( IMOD, INPUTS(IMOD)%CX0, INPUTS(IMOD)%CY0, &
             JMOD, INPUTS(JMOD)%CX0, INPUTS(JMOD)%CY0, &
             0., ICONSC )
      END IF
      !
      ! 3.b Winds
      !
    CASE (3)
      IF ( INPUTS(IMOD)%TW0(1) .LT. 0 ) THEN
        INPUTS(IMOD)%TW0(:) = INPUTS(JMOD)%TW0(:)
#ifdef W3_T
        WRITE (MDST,9030) J, INPUTS(IMOD)%TW0(:)
#endif
#ifdef W3_WNX0
        ICONSW = 0
#endif
#ifdef W3_WNX1
        ICONSW = 1
#endif
#ifdef W3_WNX2
        ICONSW = 2
#endif
        CALL WMUPDV ( IMOD, INPUTS(IMOD)%WX0, INPUTS(IMOD)%WY0, &
             JMOD, INPUTS(JMOD)%WX0, INPUTS(JMOD)%WY0, &
             0., ICONSW )
        IF ( IDINP(IMOD,J) .EQ. 'WNS' ) CALL WMUPDS             &
             ( IMOD, INPUTS(IMOD)%DT0,                   &
             JMOD, INPUTS(JMOD)%DT0, 0. )
      END IF
      !
      ! 3.c Momentum
      !
    CASE (5)
      IF ( INPUTS(IMOD)%TU0(1) .LT. 0 ) THEN
        INPUTS(IMOD)%TU0(:) = INPUTS(JMOD)%TU0(:)
#ifdef W3_T
        WRITE (MDST,9030) J, INPUTS(IMOD)%TU0(:)
#endif
#ifdef W3_WNX0
        ICONSU = 0
#endif
#ifdef W3_WNX1
        ICONSU = 1
#endif
#ifdef W3_WNX2
        ICONSU = 2
#endif
        CALL WMUPDV ( IMOD, INPUTS(IMOD)%UX0, INPUTS(IMOD)%UY0, &
             JMOD, INPUTS(JMOD)%UX0, INPUTS(JMOD)%UY0, &
             0., ICONSU )
      END IF
      !
    END SELECT
    !
    ! 4. End of routine -------------------------------------------------- /
    !
    RETURN
    !
    ! Error escape locations
    !
2999 CONTINUE
    IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSE,1999)
    CALL EXTCDE ( 2999 )
    RETURN
    !
    ! Formats
    !
1999 FORMAT (/' *** ERROR WMUPD2: OPTION NOT YET IMPLEMENTED ***'/)
    !
#ifdef W3_T
9000 FORMAT ( ' TEST WMUPD2 : INPUT : ',3I4)
9001 FORMAT ( ' TEST WMUPD2 : TIME OF IMOD : ',I9.8,1X,I6.6/      &
         '               TIME OF JMOD : ',I9.8,1X,I6.6/      &
         '               ENDING TIME  : ',I9.8,1X,I6.6)
9010 FORMAT ( ' TEST WMUPD2 : SHIFTING ',I1,' TIME = ',I8.8,I7.6)
9011 FORMAT ( ' TEST WMUPD2 : NO DATA FOR ',I1,' TO SHIFT')
9020 FORMAT ( ' TEST WMUPD2 : PROCESSING ',I1,' TIME = ',I8.8,I7.6)
9030 FORMAT ( ' TEST WMUPD2 : INITIAL FIELD FOR ',I1,             &
         ' TIME = ',I8.8,I7.6)
#endif
    !/
    !/ End of WMUPD2 ----------------------------------------------------- /
    !/
  END SUBROUTINE WMUPD2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Interpolate vector field from input grid to model grid.
  !>
  !> @details Interpolating or averaging from input grid.
  !>
  !> @param[in] IMOD Output model number
  !> @param[out] VX Output vector field
  !> @param[out] VY Output vector field
  !> @param[in] JMOD Input model number
  !> @param[in] VXI Input vector field
  !> @param[in] VYI Input vector field
  !> @param[in] UNDEF Value for mapped out point and points not covered.
  !> @param[in] CONSTP Convervation type
  !>
  !> @author H. L. Tolman  @date 06-Dec-2010
  !>
  SUBROUTINE WMUPDV ( IMOD, VX, VY, JMOD, VXI, VYI, UNDEF, CONSTP )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         06-Dec-2010 |
    !/                  +-----------------------------------+
    !/
    !/    14-Oct-2006 : Origination.                        ( version 3.10 )
    !/    12-Jan-2007 : General clean-up and bug fixes.     ( version 3.10 )
    !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
    !/                  (W. E. Rogers & T. J. Campbell, NRL)
    !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (T. J. Campbell, NRL)
    !/    01-Jul-2019 : Generalize output to curv grids     ( version 7.13 )
    !/                  (R. Padilla-Hernandez, J.H. Alves, EMC/NOAA)
    !/
    !  1. Purpose :
    !
    !     Interpolate vector field from input grid to model grid.
    !
    !  2. Method :
    !
    !     Interpolating or averaging from input grid.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IMOD    Int.   I   Output model number,
    !       VX/Y    Int.   O   Output vector field.
    !       JMOD    Int.   I   Input model number,
    !       VX/YI   Int.   I   Input vector field.
    !       UNDEF   Int.   I   Value for mapped out point and points not
    !                          covered.
    !       CONSTP  Int.   I   Conservation type :
    !                           1: Vector speed.
    !                           2: Vector speed squared.
    !                           *: Vector components.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3ERVMD  Subroutine tracing.
    !      EXTCDE    Subr.   Id.    Program abort.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WMUPD2    Subr. WMUPDTMD Input update routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !     - Grid pointers for output grid need to be set externally.
    !     - If input grid does not cover point of target grid, target grid
    !       values are set to UNDEF.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !       !/S     Enable subroutine tracing.
    !       !/T     Enable test output
    !       !/T1    Test output interpolation data.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    USE W3GDATMD, ONLY: NX, NY, X0, Y0, SX, SY, GRIDS, FLAGLL,      &
         GTYPE, RLGTYPE, CLGTYPE, UNGTYPE,           &
         ICLOSE_NONE, ICLOSE_SMPL, ICLOSE_TRPL,      &
         HPFAC, HQFAC, XGRD, YGRD
    USE WMMDATMD, ONLY: IMPROC, NMPERR, NMPSCR, MDST, MDSE, MDSO,   &
         MDSS
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: IMOD, JMOD, CONSTP
    REAL, INTENT(OUT)       :: VX(NX,NY), VY(NX,NY)
    REAL, INTENT(IN)        :: VXI(GRIDS(JMOD)%NX,GRIDS(JMOD)%NY),  &
         VYI(GRIDS(JMOD)%NX,GRIDS(JMOD)%NY),  &
         UNDEF
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IXO, IYO, IX, IY, IXF0, IXFN, IYF0,  &
         IYFN, IXS0, IXSN, IYS0, IYSN, IXS,   &
         MXA, MYA, J, J1, J2, IXC, IYC, JJ,   &
         JX, JY
    INTEGER                 :: NPOIX, NPOIY, I,  IFIELDS,CURVI   !RP

#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    INTEGER, ALLOCATABLE    :: NXA(:,:), NYA(:,:)
    REAL                    :: XR, YR, R1, R2, RT, XFL, XFR, XSL,   &
         XSR, YFL, YFR, YSL, YSR
    REAL                    :: VXL, VYL, VA0, VA, VA2, FACTOR,      &
         WTOT, WL

    REAL                    :: LONC, LATC, SXYC,                    &
         XDI, DTOLER, VALUEX, VALUEY

    REAL, ALLOCATABLE       :: RXA(:,:), RYA(:,:)

    !      REAL, ALLOCATABLE       :: VARIN(:,:)                        !RP

    LOGICAL                 :: MAP1(NX,NY), MAP2(NX,NY),            &
         MAP3(NX,NY), FLAGUP
    !
    INTEGER, POINTER        :: NXI, NYI, MAP(:,:), MAPI(:,:)
    REAL, POINTER           :: X0I, Y0I, SXI, SYI !RP , HPFACI, HQFACI

    REAL, POINTER           :: HPFACI(:,:), HQFACI(:,:)
    DOUBLE PRECISION, POINTER :: XGRDI(:,:), YGRDI(:,:), XGRDC(:,:), YGRDC(:,:)

    INTEGER, POINTER        :: ICLOSE
    REAL, ALLOCATABLE       :: XGRTMP(:),YGRTMP(:)
#ifdef W3_T1
    CHARACTER(LEN=17)       :: FORMAT1
#endif
    !/
    !/ ------------------------------------------------------------------- /
    ! 0.  Initialization
    ! 0.a Subroutine tracing and test output
    !
#ifdef W3_S
    CALL STRACE (IENT, 'WMUPDV')
#endif
    !
    IF ( GRIDS(IMOD)%GTYPE .EQ. UNGTYPE .OR. &
         GRIDS(JMOD)%GTYPE .EQ. UNGTYPE ) THEN
      WRITE (MDSE,'(/2A)') ' *** ERROR WMUPDV: ', &
           'UNSTRUCTURED GRID SUPPORT NOT YET IMPLEMENTED ***'
      CALL EXTCDE ( 999 )
    END IF
    !
    NXI    => GRIDS(JMOD)%NX
    NYI    => GRIDS(JMOD)%NY
    X0I    => GRIDS(JMOD)%X0
    Y0I    => GRIDS(JMOD)%Y0
    SXI    => GRIDS(JMOD)%SX
    SYI    => GRIDS(JMOD)%SY
    HPFACI => GRIDS(JMOD)%HPFAC
    HQFACI => GRIDS(JMOD)%HQFAC
    MAP    => GRIDS(IMOD)%MAPSTA
    MAPI   => GRIDS(JMOD)%MAPSTA
    ICLOSE => GRIDS(JMOD)%ICLOSE
    !
    IF ( ICLOSE .EQ. ICLOSE_TRPL ) THEN
      IF ( IMPROC.EQ.NMPERR ) WRITE(MDSE,*)'SUBROUTINE WMUPDV IS'// &
           ' NOT YET ADAPTED FOR TRIPOLE GRIDS. STOPPING NOW.'
      CALL EXTCDE ( 1 )
    END IF

    !
#ifdef W3_T
    WRITE (MDST,9000) IMOD, NX, NY, X0, Y0, SX, SY,              &
         JMOD, NXI, NYI, X0I, Y0I, SXI, SYI, UNDEF
#endif
    !
    ! 0.b Initialize fields
    !
    VX     = UNDEF
    VY     = UNDEF
    !
    CURVI=0
    IF ( GRIDS(IMOD)%GTYPE .EQ. CLGTYPE .OR. &
         GRIDS(JMOD)%GTYPE .EQ. CLGTYPE ) THEN
      CURVI=1
    END IF

    ! 1. Case of identical resolution and coinciding grids --------------- /
    IF(CURVI .EQ. 0) THEN
      !
      IF ( ABS(SX/SXI-1.) .LT. 1.E-3                         .AND.    &
           ABS(SY/SYI-1.) .LT. 1.E-3                         .AND.    &
           ABS(MOD((ABS(X0-X0I))/SX+0.5,1.)-0.5) .LT. 1.E-2  .AND.    &
           ABS(MOD((ABS(Y0-Y0I))/SY+0.5,1.)-0.5) .LT. 1.E-2 ) THEN
        !
        ! 1.a Offsets
        !

        IXO    = NINT((X0-X0I)/SX)
        !
        IF ( FLAGLL ) THEN
          IXF0   = 1
          IXFN   = NX
          IXS0   = -999
          IXSN   = -999
        ELSE
          IXF0   = MAX (  1  , 1-IXO )
          IXFN   = MIN ( NX  , NXI-IXO )
          IXS0   = MAX (  1  , 1+IXO )
          IXSN   = IXS0 + IXFN - IXF0
        END IF
        !
        IYO    = NINT((Y0-Y0I)/SY)
        !
        IYF0   = MAX (  1  , 1-IYO )
        IYFN   = MIN ( NY  , NYI-IYO )
        IYS0   = MAX (  1  , 1+IYO )
        IYSN   = IYS0 + IYFN - IYF0
        !
#ifdef W3_T
        WRITE (MDST,9010) IXO, IYO, IXF0, IXFN, IYF0, IYFN,      &
             IXS0, IXSN, IYS0, IYSN
#endif
        !
        ! 1.b Fill arrays for sea points only
        !

        DO IX=IXF0, IXFN
          IF ( FLAGLL ) THEN
            IXS    = 1 + NINT ( MOD (                             &
                 1080.+X0+(REAL(IX)-0.5)*SX-X0I , 360. ) / SX - 0.5 )
            IF ( IXS .GT. NXI ) CYCLE
          ELSE
            IXS    = IX + IXO
          END IF
          VX(IX,IYF0:IYFN) = VXI(IXS,IYS0:IYSN)
          VY(IX,IYF0:IYFN) = VYI(IXS,IYS0:IYSN)
        END DO
        !
        ! 1.c Return to calling routine
        !
        RETURN
        !
      END IF
    END IF !CURVI
    !
    ! 2.  General case --------------------------------------------------- /
    !
    ! 2.a Curvilinear grids
    !
    IF ( GRIDS(IMOD)%GTYPE .EQ. CLGTYPE .OR. &
         GRIDS(JMOD)%GTYPE .EQ. CLGTYPE ) THEN

      XGRDI => GRIDS(JMOD)%XGRD   !LONS FOR INPUT FIELD
      YGRDI => GRIDS(JMOD)%YGRD    !LATS FOR INPUT FIELD

      !       GETTING THE INFO FOR THE CURVILINEAR GRID
      XGRDC => GRIDS(IMOD)%XGRD     !LONS FOR CURVI GRID
      YGRDC => GRIDS(IMOD)%YGRD     !LATS FOR CURVI GRID
      !HPFAC => GRIDS(IMOD)%HPFAC   !DELTAS IN LON FOR CURVI GRID
      !HQFAC => GRIDS(IMOD)%HQFAC   !DELTAS IN LAT FOR CURVI  GRID
      !
      !
      !  FOR NOW ONLY INTERPOLATION NOT AVERAGING THEN MXA=2
      MXA=2
      MYA=2
      ALLOCATE ( NXA(NX,0:MXA) , RXA(NX,MXA) )
      NXA    = 0
      RXA    = 0.
      ALLOCATE ( NYA(NY,0:MYA) , RYA(NY,MYA) )
      NYA    = 0
      RYA    = 0.

      !IS THE TOLERANCE USED TO DETERMINE IF TWO VALUES ARE EQUAL IN LOCATION
      DTOLER = 1E-5
      ! 2.a.1  running over the curvilinear grid
      ALLOCATE (XGRTMP(NXI),YGRTMP(NYI))
      XGRTMP=REAL(XGRDI(1,:))
      YGRTMP=REAL(YGRDI(:,1))
#ifdef W3_OMPH
      !$OMP PARALLEL DO PRIVATE(J,I,LONC,LATC,VALUEX,VALUEY)
#endif
      DO J=1,NY
        DO I=1,NX
          LONC=REAL(XGRDC(J,I))   !LON FOR EVERY CURVL GRID POINT
          LATC=REAL(YGRDC(J,I))   !LAT FOR EVERY CURVL GRID POINT

          CALL INTERPOLATE2D(NXI,REAL(XGRTMP),NYI,REAL(YGRTMP), &
               VXI,VYI,LONC,LATC,DTOLER,VALUEX,VALUEY)
          VX(I,J)=VALUEX
          VY(I,J)=VALUEY

        END DO  !END I
      END DO  !END J
#ifdef W3_OMPH
      !$OMP END PARALLEL DO
#endif
      DEALLOCATE (XGRTMP, YGRTMP)

    ELSE
      !
      ! 2.b Rectilinear grids
      !
      ! 2.b.1 Interpolation / averaging data for X axis
      !
      IF ( SX/SXI .LT. 1.0001 ) THEN
        MXA    = 2
      ELSE
        MXA    = 2 + INT(SX/SXI)
      END IF
      !
#ifdef W3_T
      WRITE (MDST,9020) 'X'
#endif
#ifdef W3_T1
      WRITE (FORMAT1,'(A,I2,A,I2,A)') "'(10X,",MXA+1,'I5,',MXA+1,"F6.2)'"
      WRITE (MDST,9021) NX, MXA
#endif
      !
      ALLOCATE ( NXA(NX,0:MXA) , RXA(NX,MXA) )
      NXA    = 0
      RXA    = 0.
      !
      IF ( MXA .EQ. 2 ) THEN
        !
        DO IX=1, NX
          IF ( FLAGLL ) THEN
            XR     = 1. + MOD       &
                 ( 1080.+X0+REAL(IX-1)*SX-X0I , 360. ) / SXI
          ELSE
            XR     = 1. + ( X0+REAL(IX-1)*SX - X0I ) / SXI
          END IF
          IF ( XR.GT.0. ) THEN
            J1     = INT(XR)
            J2     = J1 + 1
            R2     = MAX ( 0. , XR-REAL(J1) )
            R1     = 1. - R2
            IF ( FLAGLL .AND. ICLOSE.NE.ICLOSE_NONE ) THEN
              J1 = 1 + MOD(J1-1,NXI)
              J2 = 1 + MOD(J2-1,NXI)
            END IF
            IF ( J1.GE.1 .AND. J1.LE.NXI .AND. R1.GT.0.05 ) THEN
              NXA(IX,0) = NXA(IX,0) + 1
              NXA(IX,NXA(IX,0)) = J1
              RXA(IX,NXA(IX,0)) = R1
            END IF
            IF ( J2.GE.1 .AND. J2.LE.NXI .AND. R2.GT.0.05 ) THEN
              NXA(IX,0) = NXA(IX,0) + 1
              NXA(IX,NXA(IX,0)) = J2
              RXA(IX,NXA(IX,0)) = R2
            END IF
            IF ( NXA(IX,0)  .GT. 0 ) THEN
              RT     = SUM ( RXA(IX,:) )
              IF ( RT .LT. 0.7 ) THEN
                NXA(IX,:) = 0
                RXA(IX,:) = 0.
              END IF
            END IF
          END IF
        END DO
        !
      ELSE
        !
        DO IX=1, NX
          !
          XFL    = X0 + REAL(IX-1)*SX - 0.5*SX
          XFR    = X0 + REAL(IX-1)*SX + 0.5*SX
          IF ( FLAGLL ) THEN
            IXC    = 1 + NINT ( MOD (                             &
                 1080.+X0+REAL(IX-1)*SX-X0I , 360. ) / SXI )
            IXS0   = IXC - 1 - MXA/2
            IXSN   = IXC + 1 + MXA/2
          ELSE
            IXC    = NINT ( 1. + ( X0+REAL(IX-1)*SX - X0I ) / SXI )
            IXS0   = MAX (  1  , IXC - 1 - MXA/2 )
            IXSN   = MIN ( NXI , IXC + 1 + MXA/2 )
          END IF
          DO J=IXS0, IXSN
            JJ=J
            IF ( FLAGLL ) THEN
              IF ( ICLOSE.NE.ICLOSE_NONE ) JJ = 1 + MOD(J-1+NXI,NXI)
              IF ( JJ.LT.1 .OR. JJ.GT. NXI ) CYCLE
              IXC   = NINT((0.5*(XFL+XFR)-X0I-REAL(JJ-1)*SXI)/360.)
              IF ( IXC .NE. 0 ) THEN
                XFL    = XFL - REAL(IXC) * 360.
                XFR    = XFR - REAL(IXC) * 360.
              END IF
            ELSE
              JJ     = J
            END IF
            XSL    = MAX ( XFL , X0I + REAL(JJ-1)*SXI - 0.5*SXI )
            XSR    = MIN ( XFR , X0I + REAL(JJ-1)*SXI + 0.5*SXI )
            R1     = MAX ( 0. , XSR - XSL ) / SX
            IF ( R1 .GT. 0 ) THEN
              NXA(IX,0) = NXA(IX,0) + 1
              NXA(IX,NXA(IX,0)) = JJ
              RXA(IX,NXA(IX,0)) = R1
            END IF
          END DO
          IF ( NXA(IX,0)  .GT. 0 ) THEN
            RT     = SUM ( RXA(IX,:) )
            IF ( RT .LT. 0.7 ) THEN
              NXA(IX,:) = 0
              RXA(IX,:) = 0.
            END IF
          END IF
        END DO
        !
      END IF
      !
#ifdef W3_T1
      DO, IX=1, NX
        IF ( NXA(IX,0) .GT. 0 ) WRITE (MDST,FORMAT1)                &
             IX, NXA(IX,1:MXA), RXA(IX,1:MXA), SUM(RXA(IX,1:MXA))
      END DO
#endif
      !
      ! 2.b.2 Interpolation / averaging data for Y axis
      !
      IF ( SY/SYI .LT. 1.0001 ) THEN
        MYA    = 2
      ELSE
        MYA    = 2 + INT(SY/SYI)
      END IF
      !
#ifdef W3_T
      WRITE (MDST,9020) 'Y'
#endif
#ifdef W3_T1
      FORMAT1 = '(10X,  I5,  F6.2)'
      WRITE (FORMAT1,'(A,I2,A,I2,A)') "'(10X,",MYA+1,'I5,',MYA+1,"F6.2)'"
      WRITE (MDST,9021) NY, MYA
#endif
      !
      ALLOCATE ( NYA(NY,0:MYA) , RYA(NY,MYA) )
      NYA    = 0
      RYA    = 0.
      !
      IF ( MYA .EQ. 2 ) THEN
        !
        DO IY=1, NY
          YR     = 1. + ( Y0+REAL(IY-1)*SY - Y0I ) / SYI
          IF ( YR.GT.0. ) THEN
            J1     = INT(YR)
            J2     = J1 + 1
            R2     = MAX ( 0. , YR-REAL(J1) )
            R1     = 1. - R2
            IF ( J1.GE.1 .AND. J1.LE.NYI .AND. R1.GT.0.05 ) THEN
              NYA(IY,0) = NYA(IY,0) + 1
              NYA(IY,NYA(IY,0)) = J1
              RYA(IY,NYA(IY,0)) = R1
            END IF
            IF ( J2.GE.1 .AND. J2.LE.NYI .AND. R2.GT.0.05 ) THEN
              NYA(IY,0) = NYA(IY,0) + 1
              NYA(IY,NYA(IY,0)) = J2
              RYA(IY,NYA(IY,0)) = R2
            END IF
            IF ( NYA(IY,0)  .GT. 0 ) THEN
              RT     = SUM ( RYA(IY,:) )
              IF ( RT .LT. 0.7 ) THEN
                NYA(IY,:) = 0
                RYA(IY,:) = 0.
              END IF
            END IF
          END IF
        END DO
        !
      ELSE
        !
        DO IY=1, NY
          YFL    = Y0 + REAL(IY-1)*SY - 0.5*SY
          YFR    = Y0 + REAL(IY-1)*SY + 0.5*SY
          IYC    = NINT ( 1. + ( Y0+REAL(IY-1)*SY - Y0I ) / SYI )
          IYS0   = MAX (  1  , IYC - 1 - MYA/2 )
          IYSN   = MIN ( NYI , IYC + 1 + MYA/2 )
          DO J=IYS0, IYSN
            YSL    = MAX ( YFL , Y0I + REAL(J-1)*SYI - 0.5*SYI )
            YSR    = MIN ( YFR , Y0I + REAL(J-1)*SYI + 0.5*SYI )
            R1     = MAX ( 0. , YSR - YSL ) / SY
            IF ( R1 .GT. 0 ) THEN
              NYA(IY,0) = NYA(IY,0) + 1
              NYA(IY,NYA(IY,0)) = J
              RYA(IY,NYA(IY,0)) = R1
            END IF
          END DO
          IF ( NYA(IY,0)  .GT. 0 ) THEN
            RT     = SUM ( RYA(IY,:) )
            IF ( RT .LT. 0.7 ) THEN
              NYA(IY,:) = 0
              RYA(IY,:) = 0.
            END IF
          END IF
        END DO
        !
      END IF
      !
    END IF

    !
#ifdef W3_T1
    DO, IY=1, NY
      IF ( NYA(IY,0) .GT. 0 ) WRITE (MDST,FORMAT1)                &
           IY, NYA(IY,1:MYA), RYA(IY,1:MYA), SUM(RYA(IY,1:MYA))
    END DO
#endif
    !
    ! 2.c Process grid
    !
    MAP1   = .FALSE.
    MAP2   = .FALSE.
    FACTOR = 1.
    !

    DO IX=1, NX
      IF ( NXA(IX,0) .EQ. 0 ) CYCLE
      DO IY=1, NY
        IF ( NYA(IY,0) .EQ. 0 ) CYCLE
        IF ( MAP(IY,IX).NE.0 ) THEN
          VXL    = 0.
          VYL    = 0.
          VA     = 0.
          VA2    = 0.
          WTOT   = 0.
          DO J1=1, NXA(IX,0)
            JX     = NXA(IX,J1)
            DO J2=1, NYA(IY,0)
              JY     = NYA(IY,J2)
              IF ( MAPI(JY,JX) .NE. 0 ) THEN
                WL     = RXA(IX,J1) * RYA(IY,J2)
                WTOT   = WTOT + WL
                VXL    = VXL + WL * VXI(JX,JY)
                VYL    = VYL + WL * VYI(JX,JY)
                VA     = VA  + WL * SQRT                        &
                     ( VXI(JX,JY)**2 + VYI(JX,JY)**2 )
                VA2    = VA2 + WL *                             &
                     ( VXI(JX,JY)**2 + VYI(JX,JY)**2 )
              END IF
            END DO
          END DO
          IF ( WTOT .LT. 0.05 ) THEN
            MAP1(IX,IY) = .TRUE.
          ELSE
            MAP2(IX,IY) = .TRUE.
            VXL    = VXL / WTOT
            VYL    = VYL / WTOT
            VA     = VA  / WTOT
            VA2    = SQRT ( VA2 / WTOT )
            VA0    = SQRT ( VXL**2 + VYL**2 )
            IF ( CONSTP .EQ. 1 ) THEN
              FACTOR    = MIN ( 1.25 , VA/MAX(1.E-7,VA0) )
            ELSE IF ( CONSTP .EQ. 2 ) THEN
              FACTOR    = MIN ( 1.25 , VA2/MAX(1.E-7,VA0) )
            END IF
            VX(IX,IY) = FACTOR * VXL
            VY(IX,IY) = FACTOR * VYL
          END IF
        END IF
      END DO
    END DO

    !
    ! 2.d Reconcile mask differences
    !
#ifdef W3_T
    WRITE (MDST,9022)
#endif
    !
    JJ     = 0
    ICLOSE => GRIDS(IMOD)%ICLOSE
    !
    DO
      IF ( JJ .GT. SWPMAX ) EXIT
      FLAGUP = .FALSE.
      MAP3   = .FALSE.
      JJ     = JJ + 1
#ifdef W3_T
      WRITE (MDST,9023) JJ
#endif
      DO IX=1, NX
        DO IY=1, NY
          IF ( MAP1(IX,IY) ) THEN
            VXL    = 0.
            VYL    = 0.
            J1     = 0
            IF ( FLAGLL ) THEN
              DO J2=IX-1, IX+1
                IF ( (J2.GT.1 .AND. J2.LE.NX) .OR. ICLOSE.NE.ICLOSE_NONE ) THEN
                  JX     = 1 + MOD(NX+J2-1,NX)
                  DO JY=IY-1, IY+1
                    IF ( JY.GT.1 .AND. JY.LE.NY ) THEN
                      IF ( MAP2(JX,JY) ) THEN
                        VXL     = VXL + VX(JX,JY)
                        VYL     = VYL + VY(JX,JY)
                        J1      = J1 + 1
                      END IF
                    END IF
                  END DO
                END IF
              END DO
            ELSE
              DO JX=IX-1, IX+1
                IF ( JX.GT.1 .AND. JX.LE.NX ) THEN
                  DO JY=IY-1, IY+1
                    IF ( JY.GT.1 .AND. JY.LE.NY ) THEN
                      IF ( MAP2(JX,JY) ) THEN
                        VXL     = VXL + VX(JX,JY)
                        VYL     = VYL + VY(JX,JY)
                        J1      = J1 + 1
                      END IF
                    END IF
                  END DO
                END IF
              END DO
            END IF !FLAGLL
            IF ( J1 .GT. 0 ) THEN
              VX(IX,IY) = VXL / REAL(J1)
              VY(IX,IY) = VYL / REAL(J1)
              MAP1(IX,IY) = .FALSE.
              MAP3(IX,IY) = .TRUE.
              FLAGUP = .TRUE.
            END IF
          END IF
        END DO
      END DO
      IF ( FLAGUP ) THEN
        MAP2   = MAP2 .OR. MAP3
      ELSE
        EXIT
      END IF
    END DO

    !
    ! 3. End of routine -------------------------------------------------- /
    !
    DEALLOCATE ( NXA, NYA, RXA, RYA )
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT ( ' TEST WMUPDV : GRID INFORMATION : '/               &
         '             ',3I5,4E11.3/                         &
         '             ',3I5,4E11.3/                         &
         '               UNDEFINED = ',E10.3)
9010 FORMAT ( ' TEST WMUPDV : COINCIDING GRIDS, OFFSETS :',2I6/   &
         '               TARGET GRID RANGES        :',4I6/   &
         '               SOURCE GRID RANGES        :',4I6)
9020 FORMAT ( ' TEST WMUPDV : WEIGHTS FOR ',A,' INTERPOATION')
#endif
#ifdef W3_T1
9021 FORMAT ( ' TEST WMUPDV : ARAY DIMENSIONED AS : ',2I6)
#endif
#ifdef W3_T
9022 FORMAT ( ' TEST WMUPDV : RECONCILING MASKS')
9023 FORMAT ( '               SWEEP NR ',I4)
#endif
    !/
    !/ End of WMUPDV ----------------------------------------------------- /
    !/
  END SUBROUTINE WMUPDV
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Interpolate scalar field from input grid to model grid.
  !>
  !> @details Interpolating or averaging from input grid.
  !>
  !> @param[in] IMOD Output model number
  !> @param[out] FD Output scalar field
  !> @param[in] JMOD Input model number
  !> @param[in] FDI Input scalar field
  !> @param[in] UNDEF Value for mapped out point and points not covered.
  !>
  !> @author H. L. Tolman  @date 06-Dec-2010
  !>
  SUBROUTINE WMUPDS ( IMOD, FD, JMOD, FDI, UNDEF )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         06-Dec-2010 |
    !/                  +-----------------------------------+
    !/
    !/    14-Oct-2006 : Origination.                        ( version 3.10 )
    !/    12-Jan-2007 : General clean-up and bug fixes.     ( version 3.10 )
    !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
    !/                  (W. E. Rogers & T. J. Campbell, NRL)
    !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (T. J. Campbell, NRL)
    !/    11-May-2015 : Updates to 2-ways nestings for UG   ( version 5.08 )
    !/    01-Jul-2019 : Generalize output to curv grids     ( version 7.13 )
    !/                  (R. Padilla-Hernandez, J.H. Alves, EMC/NOAA)
    !/
    !  1. Purpose :
    !
    !     Interpolate scalar field from input grid to model grid.
    !
    !  2. Method :
    !
    !     Interpolating or averaging from input grid.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IMOD    Int.   I   Output model number,
    !       FD      Int.   O   Output scaler field.
    !       JMOD    Int.   I   Input model number,
    !       FDI     Int.   I   Input scaler field.
    !       UNDEF   Int.   I   Value for mapped out point and points not
    !                          covered.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3ERVMD  Subroutine tracing.
    !      EXTCDE    Subr.   Id.    Program abort.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WMUPD2    Subr. WMUPDTMD Input update routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !     - Grid pointers for output grid need to be set externally.
    !     - If input grid does not cover point of target grid, target grid
    !       values are set to UNDEF.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !       !/S     Enable subroutine tracing.
    !       !/T     Enable test output
    !       !/T1    Test output interpolation data.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    USE W3GDATMD, ONLY: NX, NY, X0, Y0, SX, SY, GRIDS, FLAGLL,      &
         GTYPE, RLGTYPE, CLGTYPE, UNGTYPE,           &
         ICLOSE_NONE, ICLOSE_SMPL, ICLOSE_TRPL,      &
         HPFAC, HQFAC, XGRD, YGRD
    USE WMMDATMD, ONLY: IMPROC, NMPERR, NMPSCR, MDST, MDSE, MDSO,   &
         MDSS
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: IMOD, JMOD
    REAL, INTENT(OUT)       :: FD(NX,NY)
    REAL, INTENT(IN)        :: FDI(GRIDS(JMOD)%NX,GRIDS(JMOD)%NY),  &
         UNDEF
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IXO, IYO, IX, IY, IXF0, IXFN, IYF0,  &
         IYFN, IXS0, IXSN, IYS0, IYSN, IXS,   &
         MXA, MYA, J, J1, J2, IXC, IYC, JJ,   &
         JX, JY

    INTEGER                 :: NPOIX, NPOIY, I, CURVI               !RP

#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    INTEGER, ALLOCATABLE    :: NXA(:,:), NYA(:,:)
    REAL                    :: XR, YR, R1, R2, RT, XFL, XFR, XSL,   &
         XSR, YFL, YFR, YSL, YSR
    REAL                    :: FDL, WTOT, WL

    REAL                    :: LONC, LATC,  SXYC,                   &
         XDI, DTOLER, VALUEINTER

    REAL, ALLOCATABLE       :: RXA(:,:), RYA(:,:)


    LOGICAL                 :: MAP1(NX,NY), MAP2(NX,NY),            &
         MAP3(NX,NY), FLAGUP
    !
    INTEGER, POINTER        :: NXI, NYI, MAP(:,:), MAPI(:,:)

    DOUBLE PRECISION, POINTER :: XGRDI(:,:), YGRDI(:,:), XGRDC(:,:), YGRDC(:,:)
    REAL, POINTER             :: HPFACI(:,:), HQFACI(:,:)  !RP

    REAL, POINTER           :: X0I, Y0I, SXI, SYI !RPXXX , HPFACI, HQFACI
    INTEGER, POINTER        :: ICLOSE
#ifdef W3_T1
    CHARACTER(LEN=17)       :: FORMAT1
#endif
    !/
    !/ ------------------------------------------------------------------- /
    ! 0.  Initialization
    ! 0.a Subroutine tracing and test output
    !
#ifdef W3_S
    CALL STRACE (IENT, 'WMUPDS')
#endif
    !
    NXI    => GRIDS(JMOD)%NX
    NYI    => GRIDS(JMOD)%NY
    X0I    => GRIDS(JMOD)%X0
    Y0I    => GRIDS(JMOD)%Y0
    SXI    => GRIDS(JMOD)%SX
    SYI    => GRIDS(JMOD)%SY
    HPFACI => GRIDS(JMOD)%HPFAC
    HQFACI => GRIDS(JMOD)%HQFAC
    MAP    => GRIDS(IMOD)%MAPSTA
    MAPI   => GRIDS(JMOD)%MAPSTA
    ICLOSE => GRIDS(JMOD)%ICLOSE
    !
    IF ( ICLOSE .EQ. ICLOSE_TRPL ) THEN
      IF ( IMPROC.EQ.NMPERR ) WRITE(MDSE,*)'SUBROUTINE WMUPDS IS'// &
           ' NOT YET ADAPTED FOR TRIPOLE GRIDS. STOPPING NOW.'
      CALL EXTCDE ( 1 )
    END IF
    !
#ifdef W3_T
    WRITE (MDST,9000) IMOD, NX, NY, X0, Y0, SX, SY,              &
         JMOD, NXI, NYI, X0I, Y0I, SXI, SYI, UNDEF
#endif
    !
    ! 0.b Initialize fields
    !
    FD     = UNDEF
    !
    CURVI=0
    IF ( GRIDS(IMOD)%GTYPE .EQ. CLGTYPE .OR. &
         GRIDS(JMOD)%GTYPE .EQ. CLGTYPE ) THEN
      CURVI=1
    END IF

    ! 1. Case of identical resolution and coinciding grids --------------- /
    !
    IF(CURVI .EQ. 0) THEN
      IF ( ABS(SX/SXI-1.) .LT. 1.E-3                         .AND.    &
           ABS(SY/SYI-1.) .LT. 1.E-3                         .AND.    &
           ABS(MOD((ABS(X0-X0I))/SX+0.5,1.)-0.5) .LT. 1.E-2  .AND.    &
           ABS(MOD((ABS(Y0-Y0I))/SY+0.5,1.)-0.5) .LT. 1.E-2 ) THEN
        !
        ! 1.a Offsets
        !
        IXO    = NINT((X0-X0I)/SX)
        !
        IF ( FLAGLL ) THEN
          IXF0   = 1
          IXFN   = NX
          IXS0   = -999
          IXSN   = -999
        ELSE
          IXF0   = MAX (  1  , 1-IXO )
          IXFN   = MIN ( NX  , NXI-IXO )
          IXS0   = MAX (  1  , 1+IXO )
          IXSN   = IXS0 + IXFN - IXF0
        END IF
        !
        IYO    = NINT((Y0-Y0I)/SY)
        !
        IYF0   = MAX (  1  , 1-IYO )
        IYFN   = MIN ( NY  , NYI-IYO )
        IYS0   = MAX (  1  , 1+IYO )
        IYSN   = IYS0 + IYFN - IYF0
        !
#ifdef W3_T
        WRITE (MDST,9010) IXO, IYO, IXF0, IXFN, IYF0, IYFN,      &
             IXS0, IXSN, IYS0, IYSN
#endif
        !
        ! 1.b Fill arrays for sea points only
        !
        IF ( FLAGLL ) THEN
          DO IX=IXF0, IXFN
            IXS    = 1 + NINT ( MOD (                             &
                 1080.+X0+(REAL(IX)-0.5)*SX-X0I , 360. ) / SX - 0.5 )
            IF ( IXS .GT. NXI ) CYCLE
            FD(IX,IYF0:IYFN) = FDI(IXS,IYS0:IYSN)
          END DO
        ELSE
          DO IX=IXF0, IXFN
            IXS    = IX + IXO
            FD(IX,IYF0:IYFN) = FDI(IXS,IYS0:IYSN)
          END DO
        END IF
        !
        ! 1.c Return to calling routine
        !
        RETURN
        !
      END IF
    END IF !CURVI
    !
    ! 2.  General case --------------------------------------------------- /
    !
    !
    ! 2.a Curvilinear grids
    !
    IF ( GRIDS(IMOD)%GTYPE .EQ. CLGTYPE .OR. &
         GRIDS(JMOD)%GTYPE .EQ. CLGTYPE ) THEN

      ! 2.a.1    Getting the info for reg and curvi grids
      XGRDI => GRIDS(JMOD)%XGRD !LONS FOR INPUT FIELD
      YGRDI => GRIDS(JMOD)%YGRD !LATS FOR INPUT FIELD

      !       GETTING THE INFO FOR THE CURVILINEAR GRID
      XGRDC => GRIDS(IMOD)%XGRD     !LONS FOR CURVI GRID
      YGRDC => GRIDS(IMOD)%YGRD     !LATS FOR CURVI GRID
      !HPFAC => GRIDS(IMOD)%HPFAC   !DELTAYGRDC(:,:)YGRDC(:,:)YGRDC(:,:)YGRDC(:,:)S IN LON FOR CURVI GRID
      !HQFAC => GRIDS(IMOD)%HQFAC   !DELTAS IN LAT FOR CURVI  GRID

      !  FOR NOW ONLY INTERPOLATION NOT AVERAGING THEN MXA=2
      MXA=2
      MYA=2
      ALLOCATE ( NXA(NX,0:MXA) , RXA(NX,MXA) )
      NXA    = 0
      RXA    = 0.
      ALLOCATE ( NYA(NY,0:MYA) , RYA(NY,MYA) )
      NYA    = 0
      RYA    = 0.
      !
      !IS THE TOLERANCE USED TO DETERMINE IF TWO VALUES ARE EQUAL IN LOCATION
      DTOLER = 1E-5
      ! 2.a.2  running over the curvilinear grid
      DO J=1,NY
        DO I=1,NX
          LONC=REAL(XGRDC(J,I))   !LON FOR EVERY CURVL GRID POINT
          LATC=REAL(YGRDC(J,I))   !LAT FOR EVERY CURVL GRID POINT
          !SXC  =HPFAC(J,I)  !DELTA IN LON FOR CURVI GRID
          !SYC  =HQFAC(J,I)  !DELTA IN LAT FOR CURVI GRID

          VALUEINTER=INTERPOLATE(NXI,REAL(XGRDI(1,:)),NYI,REAL(YGRDI(:,1)),  &
               FDI,LONC,LATC,DTOLER)
          FD(I,J)=VALUEINTER
        END DO  !END I
      END DO  !END J

    ELSE
      !
      ! 2.b Rectilinear grids
      !
      ! 2.b.1 Interpolation / averaging data for X axis
      !
      IF ( SX/SXI .LT. 1.0001 ) THEN
        MXA    = 2
      ELSE
        MXA    = 2 + INT(SX/SXI)
      END IF
      !
#ifdef W3_T
      WRITE (MDST,9020) 'X'
#endif
#ifdef W3_T1
      FORMAT1 = '(10X,  I5,  F6.2)'
      WRITE (FORMAT1,'(A,I2,A,I2,A)') "'(10X,",MXA+1,'I5,',MXA+1,"F6.2)'"
      WRITE (MDST,9021) NX, MXA
#endif
      !
      ALLOCATE ( NXA(NX,0:MXA) , RXA(NX,MXA) )
      NXA    = 0
      RXA    = 0.
      !
      !
      IF ( MXA .EQ. 2 ) THEN
        !
        DO IX=1, NX
          IF ( FLAGLL ) THEN
            XR     = 1. + MOD       &
                 ( 1080.+X0+REAL(IX-1)*SX-X0I , 360. ) / SXI
          ELSE
            XR     = 1. + ( X0+REAL(IX-1)*SX - X0I ) / SXI
          END IF
          IF ( XR.GT.0. ) THEN
            J1     = INT(XR)
            J2     = J1 + 1
            R2     = MAX ( 0. , XR-REAL(J1) )
            R1     = 1. - R2
            IF ( FLAGLL .AND. ICLOSE.NE.ICLOSE_NONE ) THEN
              J1 = 1 + MOD(J1-1,NXI)
              J2 = 1 + MOD(J2-1,NXI)
            END IF
            IF ( J1.GE.1 .AND. J1.LE.NXI .AND. R1.GT.0.05 ) THEN
              NXA(IX,0) = NXA(IX,0) + 1
              NXA(IX,NXA(IX,0)) = J1
              RXA(IX,NXA(IX,0)) = R1
            END IF
            IF ( J2.GE.1 .AND. J2.LE.NXI .AND. R2.GT.0.05 ) THEN
              NXA(IX,0) = NXA(IX,0) + 1
              NXA(IX,NXA(IX,0)) = J2
              RXA(IX,NXA(IX,0)) = R2
            END IF
            IF ( NXA(IX,0)  .GT. 0 ) THEN
              RT     = SUM ( RXA(IX,:) )
              IF ( RT .LT. 0.7 ) THEN
                NXA(IX,:) = 0
                RXA(IX,:) = 0.
              END IF
            END IF
          END IF
        END DO
        !
      ELSE
        !
        DO IX=1, NX
          !
          XFL    = X0 + REAL(IX-1)*SX - 0.5*SX
          XFR    = X0 + REAL(IX-1)*SX + 0.5*SX
          IF ( FLAGLL ) THEN
            IXC    = 1 + NINT ( MOD (                             &
                 1080.+X0+REAL(IX-1)*SX-X0I , 360. ) / SXI )
            IXS0   = IXC - 1 - MXA/2
            IXSN   = IXC + 1 + MXA/2
          ELSE
            IXC    = NINT ( 1. + ( X0+REAL(IX-1)*SX - X0I ) / SXI )
            IXS0   = MAX (  1  , IXC - 1 - MXA/2 )
            IXSN   = MIN ( NXI , IXC + 1 + MXA/2 )
          END IF
          DO J=IXS0, IXSN
            IF ( FLAGLL ) THEN
              IF ( ICLOSE.NE.ICLOSE_NONE ) JJ = 1 + MOD(J-1+NXI,NXI)
              IF ( JJ.LT.1 .OR. JJ.GT. NXI ) CYCLE
              IXC   = NINT((0.5*(XFL+XFR)-X0I-REAL(JJ-1)*SXI)/360.)
              IF ( IXC .NE. 0 ) THEN
                XFL    = XFL - REAL(IXC) * 360.
                XFR    = XFR - REAL(IXC) * 360.
              END IF
            ELSE
              JJ     = J
            END IF
            XSL    = MAX ( XFL , X0I + REAL(JJ-1)*SXI - 0.5*SXI )
            XSR    = MIN ( XFR , X0I + REAL(JJ-1)*SXI + 0.5*SXI )
            R1     = MAX ( 0. , XSR - XSL ) / SX
            IF ( R1 .GT. 0 ) THEN
              NXA(IX,0) = NXA(IX,0) + 1
              NXA(IX,NXA(IX,0)) = JJ
              RXA(IX,NXA(IX,0)) = R1
            END IF
          END DO
          IF ( NXA(IX,0)  .GT. 0 ) THEN
            RT     = SUM ( RXA(IX,:) )
            IF ( RT .LT. 0.7 ) THEN
              NXA(IX,:) = 0
              RXA(IX,:) = 0.
            END IF
          END IF
        END DO
        !
      END IF
      !
#ifdef W3_T1
      DO, IX=1, NX
        IF ( NXA(IX,0) .GT. 0 ) WRITE (MDST,FORMAT1)                &
             IX, NXA(IX,1:MXA), RXA(IX,1:MXA), SUM(RXA(IX,1:MXA))
      END DO
#endif
      !
      ! 2.b.2 Interpolation / averaging data for Y axis
      !
      IF ( SY/SYI .LT. 1.0001 ) THEN
        MYA    = 2
      ELSE
        MYA    = 2 + INT(SY/SYI)
      END IF
      !
#ifdef W3_T
      WRITE (MDST,9020) 'Y'
#endif
#ifdef W3_T1
      WRITE (FORMAT1,'(A,I2,A,I2,A)') "'(10X,",MYA+1,'I5,',MYA+1,"F6.2)'"
      WRITE (MDST,9021) NY, MYA
#endif
      !
      ALLOCATE ( NYA(NY,0:MYA) , RYA(NY,MYA) )
      NYA    = 0
      RYA    = 0.
      !
      !
      IF ( MYA .EQ. 2 ) THEN
        !
        DO IY=1, NY
          YR     = 1. + ( Y0+REAL(IY-1)*SY - Y0I ) / SYI
          IF ( YR.GT.0. ) THEN
            J1     = INT(YR)
            J2     = J1 + 1
            R2     = MAX ( 0. , YR-REAL(J1) )
            R1     = 1. - R2
            IF ( J1.GE.1 .AND. J1.LE.NYI .AND. R1.GT.0.05 ) THEN
              NYA(IY,0) = NYA(IY,0) + 1
              NYA(IY,NYA(IY,0)) = J1
              RYA(IY,NYA(IY,0)) = R1
            END IF
            IF ( J2.GE.1 .AND. J2.LE.NYI .AND. R2.GT.0.05 ) THEN
              NYA(IY,0) = NYA(IY,0) + 1
              NYA(IY,NYA(IY,0)) = J2
              RYA(IY,NYA(IY,0)) = R2
            END IF
            IF ( NYA(IY,0)  .GT. 0 ) THEN
              RT     = SUM ( RYA(IY,:) )
              IF ( RT .LT. 0.7 ) THEN
                NYA(IY,:) = 0
                RYA(IY,:) = 0.
              END IF
            END IF
          END IF
        END DO
        !
      ELSE
        !
        DO IY=1, NY
          YFL    = Y0 + REAL(IY-1)*SY - 0.5*SY
          YFR    = Y0 + REAL(IY-1)*SY + 0.5*SY
          IYC    = NINT ( 1. + ( Y0+REAL(IY-1)*SY - Y0I ) / SYI )
          IYS0   = MAX (  1  , IYC - 1 - MYA/2 )
          IYSN   = MIN ( NYI , IYC + 1 + MYA/2 )
          DO J=IYS0, IYSN
            YSL    = MAX ( YFL , Y0I + REAL(J-1)*SYI - 0.5*SYI )
            YSR    = MIN ( YFR , Y0I + REAL(J-1)*SYI + 0.5*SYI )
            R1     = MAX ( 0. , YSR - YSL ) / SY
            IF ( R1 .GT. 0 ) THEN
              NYA(IY,0) = NYA(IY,0) + 1
              NYA(IY,NYA(IY,0)) = J
              RYA(IY,NYA(IY,0)) = R1
            END IF
          END DO
          IF ( NYA(IY,0)  .GT. 0 ) THEN
            RT     = SUM ( RYA(IY,:) )
            IF ( RT .LT. 0.7 ) THEN
              NYA(IY,:) = 0
              RYA(IY,:) = 0.
            END IF
          END IF
        END DO
        !
      END IF
      !
    END IF
    !
#ifdef W3_T1
    DO, IY=1, NY
      IF ( NYA(IY,0) .GT. 0 ) WRITE (MDST,FORMAT1)                &
           IY, NYA(IY,1:MYA), RYA(IY,1:MYA), SUM(RYA(IY,1:MYA))
    END DO
#endif
    !
    ! 2.c Process grid
    !
    MAP1   = .FALSE.
    MAP2   = .FALSE.
    !
    DO IX=1, NX
      IF ( NXA(IX,0) .EQ. 0 ) CYCLE
      DO IY=1, NY
        IF ( NYA(IY,0) .EQ. 0 ) CYCLE
        IF ( MAP(IY,IX).NE.0 ) THEN
          FDL    = 0.
          WTOT   = 0.
          DO J1=1, NXA(IX,0)
            JX     = NXA(IX,J1)
            DO J2=1, NYA(IY,0)
              JY     = NYA(IY,J2)
              IF ( MAPI(JY,JX) .NE. 0 ) THEN
                WL     = RXA(IX,J1) * RYA(IY,J2)
                WTOT   = WTOT + WL
                FDL    = FDL + WL * FDI(JX,JY)
              END IF
            END DO
          END DO
          IF ( WTOT .LT. 0.05 ) THEN
            MAP1(IX,IY) = .TRUE.
          ELSE
            MAP2(IX,IY) = .TRUE.
            FDL    = FDL / WTOT
            FD(IX,IY) = FDL
          END IF
        END IF
      END DO
    END DO
    !
    ! 2.d Reconcile mask differences
    !
#ifdef W3_T
    WRITE (MDST,9022)
#endif
    !
    JJ     = 0
    ICLOSE => GRIDS(IMOD)%ICLOSE
    !
    DO
      IF ( JJ .GT. SWPMAX ) EXIT
      FLAGUP = .FALSE.
      MAP3   = .FALSE.
      JJ     = JJ + 1
#ifdef W3_T
      WRITE (MDST,9023) JJ
#endif
      DO IX=1, NX
        DO IY=1, NY
          IF ( MAP1(IX,IY) ) THEN
            FDL    = 0.
            J1     = 0
            IF ( FLAGLL ) THEN
              DO J2=IX-1, IX+1
                IF ( (J2.GT.1 .AND. J2.LE.NX) .OR. ICLOSE.NE.ICLOSE_NONE ) THEN
                  JX     = 1 + MOD(NX+J2-1,NX)
                  DO JY=IY-1, IY+1
                    IF ( JY.GT.1 .AND. JY.LE.NY ) THEN
                      IF ( MAP2(JX,JY) ) THEN
                        FDL     = FDL + FD(JX,JY)
                        J1      = J1 + 1
                      END IF
                    END IF
                  END DO
                END IF
              END DO
            ELSE
              DO JX=IX-1, IX+1
                IF ( JX.GT.1 .AND. JX.LE.NX ) THEN
                  DO JY=IY-1, IY+1
                    IF ( JY.GT.1 .AND. JY.LE.NY ) THEN
                      IF ( MAP2(JX,JY) ) THEN
                        FDL     = FDL + FD(JX,JY)
                        J1      = J1 + 1
                      END IF
                    END IF
                  END DO
                END IF
              END DO
            END IF !FLAGLL
            IF ( J1 .GT. 0 ) THEN
              FD(IX,IY) = FDL / REAL(J1)
              MAP1(IX,IY) = .FALSE.
              MAP3(IX,IY) = .TRUE.
              FLAGUP = .TRUE.
            END IF
          END IF
        END DO
      END DO
      IF ( FLAGUP ) THEN
        MAP2   = MAP2 .OR. MAP3
      ELSE
        EXIT
      END IF
    END DO
    !
    ! 3. End of routine -------------------------------------------------- /
    !
    DEALLOCATE ( NXA, NYA, RXA, RYA )
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT ( ' TEST WMUPDS : GRID INFORMATION : '/               &
         '             ',3I5,4E11.3/                         &
         '             ',3I5,4E11.3/                         &
         '               UNDEFINED = ',E10.3)
9010 FORMAT ( ' TEST WMUPDS : COINCIDING GRIDS, OFFSETS :',2I6/   &
         '               TARGET GRID RANGES        :',4I6/   &
         '               SOURCE GRID RANGES        :',4I6)
9020 FORMAT ( ' TEST WMUPDS : WEIGHTS FOR ',A,' INTERPOATION')
#endif
#ifdef W3_T1
9021 FORMAT ( ' TEST WMUPDS : ARAY DIMENSIONED AS : ',2I6)
#endif
#ifdef W3_T
9022 FORMAT ( ' TEST WMUPDS : RECONCILING MASKS')
9023 FORMAT ( '               SWEEP NR ',I4)
#endif
    !/
    !/ End of WMUPDS ----------------------------------------------------- /
    !/
  END SUBROUTINE WMUPDS
  !=======================================================================
  !>
  !> @brief Search the location of a point(XC,YC) in a regular grid.
  !>
  !> @details Given an array and a value to search, it returns the index of
  !>  the element on the rectilinear grid that is closest to, but less
  !>  than, the given value. "delta" is the threshold used to determine
  !>  if two values are equal.
  !> @verbatim
  !>       if ( abs(x1 - x2) <= delta) then
  !>          assume x1 = x2
  !>       endif
  !> @endverbatim
  !>
  !> @param LENGTH Dimension of input array
  !> @param ARRAY 1D array for lats or longs
  !> @param VALUE Value to located in ARRAY
  !> @param DELTA Threshold to determine if two values are equal
  !> @returns XYCURVISEARCH
  !>
  !> @author H. L. Tolman @date 20-Jan-2017
  !>
  FUNCTION XYCURVISEARCH(LENGTH, ARRAY, VALUE, DELTA)
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         20-Jan-2017 |
    !/                  +-----------------------------------+
    !/                  (R. Padilla-Hernandez, EMC/NOAA)
    !/
    !/    01-Jul-2019 : Origination.                        ( version 7.13 )
    !/    01-Jul-2019 : Generalize output to curv grids     ( version 7.13 )
    !/
    !/    Copyright 2009 National Weather Service (NWS),
    !/       National Oceanic and Atmospheric Administration.  All rights
    !/       reserved.  WAVEWATCH III is a trademark of the NWS.
    !/       No unauthorized use without permission.
    !/
    !  1. Purpose :
    !
    !    Search the location of a point(XC,YC) in a regular grid
    !
    !
    !  2. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       LENGTH  Int.  Input  Dimension of input array
    !       ARRAY   Int.  Input  1D array for lats or longs
    !       VALUE   Real  Input  Value to be located in ARRAY
    !       DELTA   Real  Input  Threshold to determine if two values
    !                            are equal
    !     ----------------------------------------------------------------
    !
    !     Internal parameters
    !     ----------------------------------------------------------------
    !
    !     ----------------------------------------------------------------
    !
    !  3. Subroutines and functions :
    !
    !      Name           Type       Scope            Description
    !     ----------------------------------------------------------------
    !     XYCURVISEARCH   Function   Find indexes     See bellow
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines and functions used :
    !
    !     -
    !
    !  5. Remarks :
    !       GIVEN AN ARRAY  AND A VALUE TO SEARCH, IT RETURNS THE INDEX OF
    !       THE ELEMENT ON THE RECTILINEAR GRID THAT IS CLOSEST TO, BUT
    !       LESS THAN, THE GIVEN VALUE.
    !       "DELTA" IS THE THERSHOLD  USED TO DETERMINE IF TWO VALUES ARE EQUAL
    !       IF ( ABS(X1 - X2) <= DELTA) THEN
    !          ASSUME X1 = X2
    !       ENDIF
    !
    !  6. Switches :
    !     -
    !
    !  7. Source code :

    INTEGER, INTENT(IN) :: LENGTH
    REAL, DIMENSION(LENGTH), INTENT(IN) :: ARRAY
    REAL, INTENT(IN) :: VALUE
    REAL, INTENT(IN), OPTIONAL :: DELTA

    INTEGER :: XYCURVISEARCH

    INTEGER :: LEFT, MIDDLE, RIGHT


    LEFT = 1
    RIGHT = LENGTH
    DO
      IF (LEFT > RIGHT) THEN
        EXIT
      ENDIF
      MIDDLE = NINT((LEFT+RIGHT) / 2.0)
      IF ( ABS(ARRAY(MIDDLE) - VALUE) <= DELTA) THEN
        XYCURVISEARCH = MIDDLE
        RETURN
      ELSE IF (ARRAY(MIDDLE) > VALUE) THEN
        RIGHT = MIDDLE - 1
      ELSE
        LEFT = MIDDLE + 1
      END IF
    END DO
    XYCURVISEARCH = RIGHT

  END FUNCTION XYCURVISEARCH
  !  End of function -------------------------------------------------- /
  !

  !>
  !> @brief Perform interpolation from regular to curvilinear grid
  !>  for a scalar field.
  !>
  !> @details This function uses bilinear interpolation to
  !>  estimate the value of a function f at point (x,y). f is assumed
  !>  to be on a regular grid, with the grid x values specified by
  !>  xarray with dimension x_len and the grid y values specified by
  !>  yarray with dimension y_len.
  !>
  !> @param X_LEN Dimension in X
  !> @param XARRAY 1D array for Longitudes
  !> @param Y_LEN Dimension in Y
  !> @param YARRAY 1D array for Latitudes
  !> @param FUNC 1D Field
  !> @param X Long for point in the curv grid
  !> @param Y Lat for point in the curv grid
  !> @param DELTA Threshold to determine if two values are equal
  !>
  !> @returns INTERPOLATE
  !>
  !> @author H. L. Tolman @date 25-Jul-2019
  !>
  REAL FUNCTION INTERPOLATE(X_LEN,XARRAY,Y_LEN,YARRAY,FUNC, &
       X,Y,DELTA)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        25-July-2019 |
    !/                  +-----------------------------------+
    !/
    !/                     (R. Padilla-Hernandez, EMC/NOAA)
    !/
    !/    29-July-2019 :                        ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Perform interpolation from regular to curvilinear grid for a
    !     scalar field. THIS FUNCTION USES BILINEAR INTERPOLATION TO
    !     ESTIMATE THE VALUE OF A FUNCTION F AT POINT (X,Y) F IS ASSUMED
    !     TO BE ON A REGULAR GRID, WITH THE GRID X VALUES SPECIFIED BY
    !     XARRAY WITH DIMENSION X_LEN AND THE GRID Y VALUES SPECIFIED BY
    !     YARRAY WITH DIMENSION Y_LEN
    !
    !  2. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       X_LEN    Int.  Dimension in X
    !       XARRAY   Int.  1D array for Longitudes
    !       Y_LEN    Int.  Dimension in Y
    !       YARRAY   Int.  1D array for Latitudes
    !       FUNC     Int.  1D Field
    !       X,Y      Real  Long-Lat for point in the curv grid
    !       DELTA    Real  Threshold to determine if two values are equal
    !     ----------------------------------------------------------------
    !
    !     Internal parameters
    !     ----------------------------------------------------------------
    !       INX     Int.  Index in X on the rectiliniear grid that is
    !                     closest to, but less than, the given value for a
    !                     point in the curvilinear grid.
    !       JNX     Int.  Idem INX for for Y.
    !       X1,Y1   Real  (Long, Lat) left-bottom corner for the square in
    !                     regular grid, where the given value for the point
    !                     in the curvilinear grid lies
    !       X2,Y2   Real  (Long, Lat) right-upper corner for the square in
    !                     regular grid, where the given value for the point
    !                     in the curvilinear grid lies
    !     ----------------------------------------------------------------
    !
    !  3. Subroutines used :
    !
    !      Name          Type   Module   Description
    !     ----------------------------------------------------------------
    !     XYCURVISEARCH   Func. wmupdtmd Look for indexes in 1D array.
    !     ----------------------------------------------------------------
    !
    !  4. Called by :
    !
    !     Main program in which it is contained.
    !
    !  5. Error messages :
    !
    !       None.
    !
    !  6. Remarks :
    !
    !     -
    !
    !  7. Structure :
    !
    !     See source code.
    !
    !  8. Switches :
    !
    !       -
    !
    !  9. Source code :
    ! THIS FUNCTION USES BILINEAR INTERPOLATION TO ESTIMATE THE VALUE
    ! OF A FUNCTION F AT POINT (X,Y)
    ! F IS ASSUMED TO BE ON A REGULAR GRID, WITH THE GRID X VALUES SPECIFIED
    ! BY XARRAY WITH DIMENSION X_LEN
    ! AND THE GRID Y VALUES SPECIFIED BY YARRAY WITH DIMENSION Y_LEN
    !
    INTEGER, INTENT(IN) :: X_LEN, Y_LEN
    REAL, DIMENSION(X_LEN), INTENT(IN) :: XARRAY
    REAL, DIMENSION(Y_LEN), INTENT(IN) :: YARRAY
    REAL, DIMENSION(X_LEN, Y_LEN), INTENT(IN) :: FUNC
    REAL, INTENT(IN) :: X,Y
    REAL, INTENT(IN), OPTIONAL :: DELTA
    REAL :: DENOM, X1, X2, Y1, Y2
    INTEGER :: INX,JNX

    INX = XYCURVISEARCH(X_LEN, XARRAY, X, DELTA)
    JNX = XYCURVISEARCH(Y_LEN, YARRAY, Y, DELTA)
    !
    IF (INX .GE. X_LEN) THEN
      INX=INX-1
    END IF
    IF (JNX .GE. Y_LEN) THEN
      JNX=JNX-1
    END IF
    !
    X1 = XARRAY(INX)
    X2 = XARRAY(INX+1)
    Y1 = YARRAY(JNX)
    Y2 = YARRAY(JNX+1)
    !
    DENOM = (X2 - X1)*(Y2 - Y1)
    !
    INTERPOLATE = (FUNC(INX,JNX)*(X2-X)*(Y2-Y) +           &
         FUNC(INX+1,JNX)*(X-X1)*(Y2-Y) +         &
         FUNC(INX,JNX+1)*(X2-X)*(Y-Y1)+          &
         FUNC(INX+1, JNX+1)*(X-X1)*(Y-Y1))/DENOM
    !
  END FUNCTION INTERPOLATE

  !========================================================================
  !>
  !> @brief Perform interpolation from regular to curvilinear grid for a
  !>  vector field.
  !>
  !> @details This function uses bilinear interpolation to
  !>  estimate the value of a function f at point (x,y). f is assumed
  !>  to be on a regular grid, with the grid x values specified by
  !>  xarray with dimension x_len and the grid y values specified by
  !>  yarray with dimension y_len.
  !>
  !> @param[in] X_LEN Dimension in X
  !> @param[in] XARRAY 1D array for Longitudes
  !> @param[in] Y_LEN Dimension in Y
  !> @param[in] YARRAY 1D array for Latitudes
  !> @param[in] FUNC1 First component of the 2D array
  !> @param[in] FUNC2 Second component of the 2D array
  !> @param[in] X Long for point the curv grid
  !> @param[in] Y Lat for point the curv grid
  !> @param[in] DELTA Threshold to determine if two values are equal
  !> @param[out] VAL1 Interpolated value at a point in curv grid
  !> @param[out] VAL2 Interpolated value at a point in curv grid
  !>
  !> @author H. L. Tolman  @date 25-Jul-2019
  !>
  SUBROUTINE INTERPOLATE2D(X_LEN,XARRAY,Y_LEN,YARRAY,FUNC1, &
       FUNC2,X,Y,DELTA,VAL1,VAL2)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        25-July-2019 |
    !/                  +-----------------------------------+
    !/
    !/                     (R. Padilla-Hernandez, EMC/NOAA)
    !/
    !/    29-July-2019 :                        ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Perform interpolation from regular to curvilinear grid for a
    !     Vector field. THIS FUNCTION USES BILINEAR INTERPOLATION TO
    !     ESTIMATE THE VALUE OF A FUNCTION F AT POINT (X,Y) F IS ASSUMED
    !     TO BE ON A REGULAR GRID, WITH THE GRID X VALUES SPECIFIED BY
    !     XARRAY WITH DIMENSION X_LEN AND THE GRID Y VALUES SPECIFIED BY
    !     YARRAY WITH DIMENSION Y_LEN
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       X_LEN     Int.  Dimension in X
    !       XARRAY    Int.  1D array for Longitudes
    !       Y_LEN     Int.  Dimension in Y
    !       YARRAY    Int.  1D array for Latitudes
    !       FUNC1     Int.  First componen of the 2D array
    !       FUNC2     Int.  Second component of the 2D array
    !       X,Y       Real  Long-Lat for point in the curv grid
    !       DELTA     Real  Threshold to determine if two values are equal
    !       VAL1,VAL2 Real  Interpolated values at a point in curvi grid
    !     ----------------------------------------------------------------
    !
    !     Internal parameters
    !     ----------------------------------------------------------------
    !       INX     Int.  Index in X on the rectiliniear grid that is
    !                     closest to, but less than, the given value for a
    !                     point in the curvilinear grid.
    !       JNX     Int.  Idem INX for for Y.
    !       X1,Y1   Real  (Long, Lat) left-bottom corner for the square in
    !                     regular grid, where the given value for the point
    !                     in the curvilinear grid lies
    !       X2,Y2   Real  (Long, Lat) right-upper corner for the square in
    !                     regular grid, where the given value for the point
    !                     in the curvilinear grid lies
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name          Type   Module   Description
    !     ----------------------------------------------------------------
    !     XYCURVISEARCH   Func. wmupdtmd Look for indexes in 1D array.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !     Main program in which it is contained.
    !
    !  6. Error messages :
    !
    !       None.
    !
    !  7. Remarks :
    !
    !     -
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !       -
    !
    ! 10. Source code :
    !
    INTEGER, INTENT(IN) :: X_LEN, Y_LEN
    REAL, DIMENSION(X_LEN), INTENT(IN) :: XARRAY
    REAL, DIMENSION(Y_LEN), INTENT(IN) :: YARRAY
    REAL, DIMENSION(X_LEN, Y_LEN), INTENT(IN) :: FUNC1, FUNC2
    REAL, INTENT(IN) :: X,Y
    REAL, INTENT(IN), OPTIONAL :: DELTA
    REAL, INTENT(OUT) :: VAL1,VAL2

    REAL :: DENOM, X1, X2, Y1, Y2,C1,C2,C3,C4
    INTEGER :: INX,JNX

    INX = XYCURVISEARCH(X_LEN, XARRAY, X, DELTA)
    JNX = XYCURVISEARCH(Y_LEN, YARRAY, Y, DELTA)
    !
    IF (INX .GE. X_LEN) THEN
      INX=INX-1
    END IF
    IF (JNX .GE. Y_LEN) THEN
      JNX=JNX-1
    END IF
    !
    X1 = XARRAY(INX)
    X2 = XARRAY(INX+1)
    Y1 = YARRAY(JNX)
    Y2 = YARRAY(JNX+1)
    !
    DENOM = (X2 - X1)*(Y2 - Y1)
    C1=(X2-X)*(Y2-Y)
    C2=(X-X1)*(Y2-Y)
    C3=(X2-X)*(Y-Y1)
    C4=(X-X1)*(Y-Y1)
    VAL1 = (FUNC1(INX,JNX)  *C1 + FUNC1(INX+1,JNX)  *C2 +  &
         FUNC1(INX,JNX+1)*C3 + FUNC1(INX+1,JNX+1)*C4)/DENOM

    VAL2 = (FUNC2(INX,JNX)  *C1 + FUNC2(INX+1,JNX)  *C2 +  &
         FUNC2(INX,JNX+1)*C3 + FUNC2(INX+1,JNX+1)*C4)/DENOM
    !
  END SUBROUTINE INTERPOLATE2D
  !====================================================================
  !>
  !> @brief This function uses averaging to estimate the value
  !>  of a function f at point (x,y).
  !>
  !> @details f is assumed to be on a regular grid, with the grid x values specified
  !>  by xarray with dimension x_len
  !>  and the grid y values specified by yarray with dimension y_len,
  !>  the number of point to be taken into account in x and y.
  !>
  !> @param X_LEN
  !> @param XARRAY
  !> @param Y_LEN
  !> @param YARRAY
  !> @param FUNC
  !> @param X
  !> @param Y
  !> @param NPX
  !> @param NPY
  !> @returns AVERAGING
  !>
  !> @author H. L. Tolman @date 25-Jul-2019
  !>
  REAL FUNCTION AVERAGING(X_LEN,XARRAY,Y_LEN,YARRAY,FUNC, &
       X,Y,NPX,NPY)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        25-July-2019 |
    !/                  +-----------------------------------+
    !/
    !/                     (R. Padilla-Hernandez, EMC/NOAA)
    !/
    !/    29-July-2019 :                        ( version 7.13 )
    !/

    ! THIS FUNCTION USES AVERAGING TO ESTIMATE THE VALUE
    ! OF A FUNCTION F AT POINT (X,Y)
    ! F IS ASSUMED TO BE ON A REGULAR GRID, WITH THE GRID X VALUES SPECIFIED
    ! BY XARRAY WITH DIMENSION X_LEN
    ! AND THE GRID Y VALUES SPECIFIED BY YARRAY WITH DIMENSION Y_LEN
    ! ININI AND INEND, THE NUMBER OF POINT TO BE TAKEN INTO ACCOUNT
    ! IN X AND Y

    !IMPLICIT NONE
    INTEGER  X_LEN, Y_LEN, INXEND, INYEND, NPX,NPY
    REAL, DIMENSION(X_LEN) :: XARRAY
    REAL, DIMENSION(Y_LEN)  :: YARRAY
    REAL, DIMENSION(X_LEN, Y_LEN) :: FUNC
    REAL :: X,Y

    REAL :: X1, X2, Y1, Y2, SUM
    INTEGER :: INX,INY, INITIALX, INITIALY
    INTEGER :: INFINX, INFINY,ICOUNT,I,J

    INX = XYCURVISEARCH(X_LEN, XARRAY, X)
    INY = XYCURVISEARCH(Y_LEN, YARRAY, Y)

    X1 = XARRAY(INX)
    !X2 = XARRAY(INX+1)

    Y1 = YARRAY(INY)
    !Y2 = YARRAY(INY+1)

    INXEND=NPX+1
    INYEND=NPY+1
    ! LETS FIX THE INITIAL INDEX =1 NEGATIVE INDEXES IN LONG
    IF (INX-NPX .LT. 1) THEN
      INITIALX=1
    ELSE
      INITIALX=INX-NPX
    END IF
    ! LETS FIX THE FINAL INDEX =NX IF LOOKING FOR INDEXES > NX
    IF (INX+INXEND .GT. X_LEN) THEN
      INFINX=X_LEN
    ELSE
      INFINX=INX+INXEND
    END IF
    ! LETS FIX THE INITIAL INDEX =1 FOR NEGATIVE INDEXES FOR LAT
    IF (INY-NPY .LT. 1) THEN
      INITIALY=1
    ELSE
      INITIALY=INY-NPY
    END IF
    ! LETS FIX THE FINAL INDEX =NX IF LOOKING FOR INDEXES > NX
    IF (INY+INYEND .GT. Y_LEN) THEN
      INFINY=Y_LEN
    ELSE
      INFINY=INY+INYEND
    END IF


    SUM=0.0
    ICOUNT=0
    DO J=INITIALY,INFINY
      DO I=INITIALX,INFINX
        ICOUNT=ICOUNT+1
        SUM=SUM+FUNC(I,J)
      END DO
    END DO
    AVERAGING=SUM/REAL(ICOUNT)

  END FUNCTION AVERAGING

  !=======================================================================

  !/
  !/ End of module WMUPDTMD -------------------------------------------- /
  !/
END MODULE WMUPDTMD
