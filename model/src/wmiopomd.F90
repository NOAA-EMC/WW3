!> @file
!> @brief Contains module WMIOPOMD.
!>
!> @author H. L. Tolman @date 06-Jun-2012
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Module for generating a single point output file for a multi-
!>  grid model implementation.
!>
!> @author H. L. Tolman @date 06-Jun-2012
!>
MODULE WMIOPOMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         06-Jun-2012 |
  !/                  +-----------------------------------+
  !/
  !/    09-Aug-2006 : Origination.                        ( version 3.10 )
  !/    01-May-2007 : Addd diagnostic output O7a/b.       ( version 3.11 )
  !/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
  !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
  !/                  (W. E. Rogers & T. J. Campbell, NRL)
  !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
  !/                  specify index closure for a grid.   ( version 3.14 )
  !/                  (T. J. Campbell, NRL)
  !/    06-Mar-2012 : Using MPI_COMM_NULL in checks.      ( version 4.07 )
  !/    06-Jun-2012 : Porting bugfixes from 3.14 to 4.07  ( version 4.07 )
  !/
  !/    Copyright 2009-2012 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Module for generating a single point output file for a multi-
  !     grid model implementation.
  !
  !  2. Variables and types :
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      WMIOPP    Subr  Public   Initialization routine.
  !      WMIOPO    Subr  Public   Gather and write routine.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      W3SETG    Subr  W3GDATMD Point to model grid.
  !      W3SETW    Subr  W3WDATMD Point to model grid.
  !      W3SETA    Subr  W3ADATMD Point to model grid.
  !      W3SETO    Subr  W3ODATMD Point to model grid.
  !      W3DMO2    Subr     Id.   Dimention model grids output 2.
  !      WMSETM    Subr  WMMDATMD Point to model grid.
  !      W3MPIP    Subr  W3INITMD Model intiailization.
  !      W3IOPP    Sunr  W3IOPOMD Prepare point output for single model.
  !      W3IOPO    Sunr     Id.   Point output for single model.
  !      W3CSPC    Subr. W3CSPCMD Spectral grid conversion.
  !      STRACE    Subr  W3SERVMD Subroutine tracing.
  !      EXTCDE    Subr     Id.   Program abort.
  !      MPI_SEND, MPI_RECV
  !                Subr.  mpif.h  Standard MPI library routines.
  !     ----------------------------------------------------------------
  !
  !  5. Remarks :
  !
  !  6. Switches :
  !
  !       !/SHRD Distributed memory model.
  !       !/MPI
  !
  !       !O7a   Disgnostic output to NMPSCR.
  !       !O7b
  !
  !       !/S    Enable subroutine tracing.
  !       !/T    Enable test output
  !       !/MPIT
  !
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
  PUBLIC
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Initialization for unified point output.
  !>
  !> @details Find highest resolution grid for each point.
  !>
  !> @param[in] NPT Number of output points in input.
  !> @param[in] XPT (longitude) coordinates of output points.
  !> @param[in] YPT (latitude) coordinates of output points.
  !> @param[in] PNAMES Names of output points.
  !>
  !> @author H. L. Tolman  @date 01-Sep-2012
  !>
  SUBROUTINE WMIOPP ( NPT, XPT, YPT, PNAMES )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         01-Sep-2012 !
    !/                  +-----------------------------------+
    !/
    !/    09-Aug-2006 : Origination.                        ( version 3.10 )
    !/    01-May-2007 : Addd diagnostic output O7a,b        ( version 3.11 )
    !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
    !/                  (W. E. Rogers & T. J. Campbell, NRL)
    !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (T. J. Campbell, NRL)
    !/    16-Mar-2012 : Using MPI_COMM_NULL in checks.      ( version 4.07 )
    !/    06-Jun-2012 : Porting bugfixes from 3.14 to 4.07  ( version 4.07 )
    !/    01-Sep-2012 : Added tests for unstructured grid   ( version 4.07 )
    !/                  (M. Dutour Sikiric, IRB & Aron Roland, Z&P)
    !/
    !  1. Purpose :
    !
    !     Initialization for unified point output.
    !
    !  2. Method :
    !
    !     Find highest resolution grid for each point.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NPT     Int.   I   Number of output points in input.
    !       XPT     R.A.   I   X (longitude) coordinates of output points.
    !       YPT     R.A.   I   Id. Y.
    !       PNAMES  C*40   I   Names of output points.
    !     ----------------------------------------------------------------
    !       Note: all are optional, and should be given on the first call
    !             only, will be taken from storage after that.
    !             NPT needs to be ginve always, but can be dummy after
    !             first call.
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3SETG    Subr  W3GDATMD Point to model grid.
    !      W3SETW    Subr  W3WDATMD Point to model grid.
    !      W3SETA    Subr  W3ADATMD Point to model grid.
    !      W3SETO    Subr  W3ODATMD Point to model grid.
    !      W3DMO2    Subr     Id.   Dimension model grids output 2.
    !      WMSETM    Subr  WMMDATMD Point to model grid.
    !      W3MPIP    Subr  W3INITMD Model intiailization.
    !      W3IOPP    Sunr  W3IOPOMD Point output for single model.
    !      STRACE    Subr  W3SERVMD Subroutine tracing.
    !      EXTCDE    Subr     Id.   Program abort.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WMINIT    Subr. WMINITMD Wave model initialization routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !     - The algorithm used to decide if the pont is in the grid needs
    !       to be strictly consistent with W3IOPP.
    !     - MPI communication is set up separately from W3MPIO to assure
    !       that data are gathered in a single processor even if this
    !       procesor is not part of the communicator of the individual
    !       model.
    !     - In section 2.b the soring of the grids by rand is utilized.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !       !/SHRD Distributed memory model.
    !       !/MPI
    !
    !       !O7a   Disgnostic output to NMPSCR.
    !       !O7b
    !
    !       !/S    Enable subroutine tracing.
    !       !/T    Enable test output
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    USE W3GSRUMD
    USE W3GDATMD, ONLY: W3SETG
    USE W3ADATMD, ONLY: W3SETA
    USE W3WDATMD, ONLY: W3SETW
    USE W3ODATMD, ONLY: W3SETO, W3DMO2
    USE WMMDATMD, ONLY: WMSETM
#ifdef W3_MPI
    USE W3INITMD, ONLY: W3MPIP
#endif
    USE W3IOPOMD, ONLY: W3IOPP
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    USE W3GDATMD, ONLY: NX, NY, X0, Y0, SX, MAPSTA, GRIDS,            &
         FLAGLL, ICLOSE, ICLOSE_NONE, GTYPE, UNGTYPE,  &
         CLGTYPE, GSU
    USE W3GDATMD, ONLY: TRIGP, MAXX, MAXY, DXYMAX  ! unstructured grids
    USE W3ODATMD, ONLY: O2INIT, NOPTS, PTLOC, PTNME, GRDID, OUTPTS
#ifdef W3_MPI
    USE W3ODATMD, ONLY: O2IRQI
#endif
    USE WMMDATMD, ONLY: MDSE, MDST, NRGRD, MDATAS, IMPROC, NMPSCR,  &
         NMPERR, MDSS
    USE W3TRIAMD
#ifdef W3_MPI
    USE WMMDATMD, ONLY: MPI_COMM_GRD, MPI_COMM_MWAVE
#endif
    !
    IMPLICIT NONE
    !
#ifdef W3_MPI
    INCLUDE "mpif.h"
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)                    :: NPT
    REAL, INTENT(IN), OPTIONAL             :: XPT(NPT), YPT(NPT)
    CHARACTER(LEN=40),INTENT(IN), OPTIONAL :: PNAMES(NPT)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IPT, J, II
    INTEGER                 :: IX(4), IY(4)         ! created by w3grmp
    REAL                    :: RD(4)                ! created by w3grmp
    INTEGER                 :: itout, I1, I2, I3    ! unstructured grids
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    INTEGER                 :: IERR_MPI
    REAL                    :: RX, RY, RDX, RDY
    REAL, PARAMETER         :: ACC = 0.05
    REAL, ALLOCATABLE       :: XP(:), YP(:)
    REAL                    :: FACTOR
    LOGICAL, ALLOCATABLE    :: INGRID(:,:)
    LOGICAL, SAVE           :: SETUP = .FALSE., FLGO7a = .FALSE.
    CHARACTER(LEN=40), ALLOCATABLE :: PN(:)
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'WMIOPP')
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 0.  Initializations
    !
    CALL W3SETO ( 0, MDSE, MDST )
    !
#ifdef W3_T
    WRITE (MDST,9000) O2INIT, NPT, PRESENT(XPT),                 &
         PRESENT(YPT), PRESENT(PNAMES)
#endif
#ifdef W3_O7a
    FLGO7a = .TRUE.
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Initialize if necessary and possible
    !
    IF ( .NOT. O2INIT ) THEN
      !
#ifdef W3_T
      WRITE (MDST,9010)
#endif
      !
      IF ( .NOT.PRESENT(XPT) .OR. .NOT.PRESENT(YPT) .OR.          &
           .NOT.PRESENT(PNAMES) ) THEN
        WRITE (MDSE,1000)
        CALL EXTCDE (1)
      END IF
      !
      CALL W3DMO2 ( 0, MDSE, MDST, NPT )
      !
      NOPTS      = NPT
      PTLOC(1,:) = XPT
      PTLOC(2,:) = YPT
      PTNME      = PNAMES
      GRDID      = 'none'
      !
    END IF
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Locate points in grids
    ! 2.a Check all points for all grids
    !
#ifdef W3_T
    WRITE (MDST,9020)
#endif
    !
    IF ( FLAGLL ) THEN
      FACTOR = 1.
    ELSE
      FACTOR = 1.E-3
    END IF
    !
    ALLOCATE ( INGRID(NRGRD,NOPTS), XP(NOPTS), YP(NOPTS) )
    !
    INGRID = .FALSE.
    XP     = PTLOC(1,:)
    YP     = PTLOC(2,:)
    !
    DO J=1, NRGRD
      !
      CALL W3SETG ( J, MDSE, MDST )
      !
      ! Loop over output points
      !
      ! notes.....Here, we have pulled coding for UNGTYPE and CLGTYPE from w3iopomd.ftn
      ! ..........in w3iopomd.ftn, it is "DO IPT=1, NPT" but otherwise very similar
      DO IPT=1, NOPTS
        !
        !     Check if point within grid
        !
        IF (GTYPE .NE. UNGTYPE) THEN
          INGRID(J,IPT) = W3GRMP( GSU, XPT(IPT), YPT(IPT), IX, IY, RD )
          IF ( .NOT.INGRID(J,IPT) ) THEN
            CYCLE
          END IF
        ELSE
          CALL IS_IN_UNGRID(J, DBLE(XPT(IPT)), DBLE(YPT(IPT)), itout, IX, IY, RD )
          IF (itout.eq.0) THEN
            INGRID(J,IPT)=.FALSE.
          END IF
        END IF
        !
        !     Check if point not on land
        !
        IF ( MAPSTA(IY(1),IX(1)) .EQ. 0 .AND. &
             MAPSTA(IY(2),IX(2)) .EQ. 0 .AND. &
             MAPSTA(IY(3),IX(3)) .EQ. 0 .AND. &
             MAPSTA(IY(4),IX(4)) .EQ. 0 ) THEN
          INGRID(J,IPT) = .FALSE.
          CYCLE
        END IF

        !.........If we've gotten to this point, then we are satisfied that
        !................the point is in this grid.

      END DO !        DO IPT=1, NOPTS
      !
    END DO !      DO J=1, NRGRD
    !
    DEALLOCATE ( XP, YP )
    !
    ! 2.b Select a grid for each point
    !     start from last, which is supposedly higher resolution
    !
    MDATAS(:)%NRUPTS = 0
    !
    DO IPT=1, NOPTS
      GRDID(IPT) = '...none...'
      DO J= NRGRD, 1, -1
        IF ( INGRID(J,IPT) ) THEN
          GRDID(IPT) = GRIDS(J)%FILEXT
          MDATAS(J)%NRUPTS = MDATAS(J)%NRUPTS + 1
          EXIT
        END IF
      END DO
    END DO
    !
    ! 2.c Diagnostic output
    !
#ifdef W3_O7b
    IF ( IMPROC .EQ. NMPSCR ) THEN
      WRITE (MDSS,920)
      DO IPT=1, NOPTS
        DO J=1, NRGRD
          IF ( GRIDS(J)%FILEXT .EQ. GRDID(IPT) ) EXIT
        END DO
        IF ( J .GT. NRGRD ) THEN
          WRITE (MDSS,921) PTNME(IPT), PTLOC(:,IPT)*FACTOR
        ELSE
          WRITE (MDSS,922) PTNME(IPT), PTLOC(:,IPT)*FACTOR, &
               GRIDS(J)%FILEXT
        END IF
      END DO
      WRITE (MDSS,929)
    END IF
#endif
    !
    ! 2.d Test output
    !
#ifdef W3_T
    DO IPT=1, NOPTS
      WRITE (MDST,9021) IPT, PTNME(IPT), GRDID(IPT)
    END DO
#endif
    !
#ifdef W3_T
    IPT      = NOPTS
    WRITE (MDST,9022)
    DO J=1, NRGRD
      WRITE (MDST,9023) J, MDATAS(J)%NRUPTS, GRIDS(J)%FILEXT
      IPT      = IPT - MDATAS(J)%NRUPTS
    END DO
    WRITE (MDST,9024) IPT
#endif
    !
    DEALLOCATE ( INGRID )
    !
    ! -------------------------------------------------------------------- /
    ! 3.  Initialize individual grids
    ! 3.a Loop over grids
    !
    DO J=1, NRGRD
      !
#ifdef W3_T
      WRITE (MDST,9030) J
#endif
      !
      ! 3.b (De)allocate map arrays
      !
      IPT      = MAX ( 1 , MDATAS(J)%NRUPTS )
      IF ( SETUP ) DEALLOCATE ( MDATAS(J)%UPTMAP )
      ALLOCATE ( MDATAS(J)%UPTMAP(IPT) )
      !
      IF ( MDATAS(J)%NRUPTS .EQ. 0 ) CYCLE
      !
      ALLOCATE ( XP(IPT), YP(IPT), PN(IPT) )
      !
      ! 3.c Set up mapping and point arrays
      !
      IPT      = 0
      DO II=1, NOPTS
        IF ( GRDID(II) .NE. GRIDS(J)%FILEXT ) CYCLE
        IPT      = IPT + 1
        MDATAS(J)%UPTMAP(IPT) = II
        XP(IPT)  = PTLOC(1,II)
        YP(IPT)  = PTLOC(2,II)
        PN(IPT)  = PTNME(II)
      END DO
      !
#ifdef W3_T
      DO IPT=1, MDATAS(J)%NRUPTS
        WRITE (MDST,9031) IPT, MDATAS(J)%UPTMAP(IPT),XP(IPT),YP(IPT),PN(IPT)
      END DO
#endif
      !
#ifdef W3_MPI
      IF ( FLGO7a ) CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
#endif
#ifdef W3_O7a
      IF ( IMPROC.EQ.NMPSCR ) WRITE (MDSS,930)               &
           J, GRIDS(J)%FILEXT, IPT
#endif
      !
      ! 3.d Preprocessing for output
      !
#ifdef W3_T
      WRITE (MDST,9032)
#endif
      !
      ! 3.d.1 Shared memory version
      !
#ifdef W3_SHRD
      CALL W3SETO ( J, MDSE, MDST )
      CALL W3SETG ( J, MDSE, MDST )
#endif
      !
#ifdef W3_SHRD
      IF ( O2INIT ) THEN
        DEALLOCATE ( OUTPTS(J)%OUT2%IPTINT,               &
             OUTPTS(J)%OUT2%IL   , OUTPTS(J)%OUT2%IW    ,  &
             OUTPTS(J)%OUT2%II   , OUTPTS(J)%OUT2%PTIFAC,  &
             OUTPTS(J)%OUT2%PTNME, OUTPTS(J)%OUT2%GRDID ,  &
             OUTPTS(J)%OUT2%DPO  , OUTPTS(J)%OUT2%WAO   ,  &
             OUTPTS(J)%OUT2%WDO  , OUTPTS(J)%OUT2%ASO   ,  &
             OUTPTS(J)%OUT2%CAO  , OUTPTS(J)%OUT2%CDO   ,  &
             OUTPTS(J)%OUT2%SPCO , OUTPTS(J)%OUT2%PTLOC )
        O2INIT = .FALSE.
      END IF
#endif
      !
#ifdef W3_SHRD
      CALL W3IOPP ( MDATAS(J)%NRUPTS, XP, YP, PN, J )
#endif
      !
      ! 3.d.2 Distributed memory version
      !
#ifdef W3_MPI
      CALL WMSETM ( J, MDSE, MDST )
#endif
      !
#ifdef W3_MPI
      IF ( MPI_COMM_GRD .NE. MPI_COMM_NULL ) THEN
#endif
        !
#ifdef W3_MPI
        CALL W3SETO ( J, MDSE, MDST )
        CALL W3SETG ( J, MDSE, MDST )
        CALL W3SETA ( J, MDSE, MDST )
        CALL W3SETW ( J, MDSE, MDST )
#endif
        !
#ifdef W3_MPI
        IF ( O2INIT ) THEN
          DEALLOCATE ( OUTPTS(J)%OUT2%IPTINT,               &
               OUTPTS(J)%OUT2%IL   , OUTPTS(J)%OUT2%IW    ,  &
               OUTPTS(J)%OUT2%II   , OUTPTS(J)%OUT2%PTIFAC,  &
               OUTPTS(J)%OUT2%PTNME, OUTPTS(J)%OUT2%GRDID ,  &
               OUTPTS(J)%OUT2%DPO  , OUTPTS(J)%OUT2%WAO   ,  &
               OUTPTS(J)%OUT2%WDO  , OUTPTS(J)%OUT2%ASO   ,  &
               OUTPTS(J)%OUT2%CAO  , OUTPTS(J)%OUT2%CDO   ,  &
               OUTPTS(J)%OUT2%SPCO , OUTPTS(J)%OUT2%PTLOC )
          O2INIT = .FALSE.
        END IF
#endif
        !
#ifdef W3_MPI
        CALL W3IOPP ( MDATAS(J)%NRUPTS, XP, YP, PN, J )
#endif
        !
#ifdef W3_MPI
        IF ( O2IRQI ) THEN
          DEALLOCATE (OUTPTS(J)%OUT2%IRQPO1,                &
               OUTPTS(J)%OUT2%IRQPO2 )
          O2IRQI = .FALSE.
        END IF
#endif
        !
#ifdef W3_MPI
        CALL W3MPIP ( J )
#endif
        !
#ifdef W3_MPI
      END IF
#endif
      !
      ! This barrier is needed to straighten out output.
      !
#ifdef W3_O7a
      IF ( IMPROC.EQ.NMPSCR ) WRITE (MDSS,939)
#endif
      !
      ! 3.e Reset pointers and clean up
      !
      CALL W3SETO ( 0, MDSE, MDST )
      DEALLOCATE ( XP, YP, PN )
      !
    END DO
    !
#ifdef W3_MPI
    IF ( FLGO7a ) CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 4.  Finalize
    !
    SETUP  = .TRUE.
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_O7b
920 FORMAT (/'       Diagnostic test output for output points :'/ &
         '       -------------------------------------------------')
921 FORMAT ( '          ',A,' (',2F8.2,') NO GRID FOUND')
922 FORMAT ( '          ',A,' (',2F8.2,') grid ',A)
929 FORMAT ( ' ')
#endif
    !
#ifdef W3_O7a
930 FORMAT (/'       Grid ',I3,' [',A,']',I4,' points :'/    &
         '       -------------------------------------------------')
939 FORMAT ( ' ')
#endif
    !
1000 FORMAT (/' *** ERROR WMIOPP : INITALIZATION DATA NOT',          &
         ' AVAILABLE *** '/)
    !
#ifdef W3_T
9000 FORMAT ( ' TEST WMIOPP : O2INIT   :',L2/                     &
         '               PAR LIST :',I4,3L2)
#endif
    !
#ifdef W3_T
9010 FORMAT ( ' TEST WMIOPP : INITIALIZING DATA GRID 0')
#endif
    !
#ifdef W3_T
9020 FORMAT ( ' TEST WMIOPP : FINDING POINTS IN GRID')
9021 FORMAT ( '               ',I4,2X,A,2X,A)
9022 FORMAT ( ' TEST WMIOPP : OUTPUT POINTS PER GRID')
9023 FORMAT ( '                  GRID',I3,' HAS',I4,' OUTPUT ',   &
         'POINTS, NAME = ',A)
9024 FORMAT ( '                  UNALLOCATED POINTS :',I4)
#endif
    !
#ifdef W3_T
9030 FORMAT ( ' TEST WMIOPP : PREPPING GRID',I3)
9031 FORMAT ( '             ',2I5,2E12.3,2X,A)
9032 FORMAT ( ' TEST WMIOPP : RUNNING W3IOPP / W3MPIP')
#endif
    !/
    !/ End of WMIOPP ----------------------------------------------------- /
    !/
  END SUBROUTINE WMIOPP
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Gather and write unified point output.
  !>
  !> @details Per-grid point output is already gathered. All data are
  !>  gathered in the proper storage, and written using the standard
  !>  W3IOPO routint from grid number 0.
  !>
  !> @param[in] TOUT Time for output file.
  !>
  !> @author H. L. Tolman  @date 16-Mar-2012
  !>
  SUBROUTINE WMIOPO ( TOUT )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         16-Mar-2012 !
    !/                  +-----------------------------------+
    !/
    !/    09-Aug-2006 : Origination.                        ( version 3.10 )
    !/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
    !/    16-Mar-2012 : Using MPI_COMM_NULL in checks.      ( version 3.14 )
    !/
    !  1. Purpose :
    !
    !     Gather and write unified point output.
    !
    !  2. Method :
    !
    !     Per-grid point output is already gathered. All data are gathered
    !     in the porper storage, and writen using the standard W3IOPO
    !     routint from grid number 0.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       TOUT    I.A.   I   Time for output file.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3SETG    Subr. W3GDATMD Point to model grid.
    !      W3SETW    Subr. W3WDATMD Point to model grid.
    !      W3SETO    Subr. W3ODATMD Point to model grid.
    !      WMSETM    Subr. WMMDATMD Point to model grid.
    !      W3CSPC    Subr. W3CSPCMD Spectral grid conversion.
    !      W3IOPO    Subr. W3IOPOMD Point output for single model.
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !      MPI_SEND, MPI_RECV
    !                Subr.  mpif.h  Standard MPI library routines.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WMWAVE    Prog. WMWAVEMD Multi-grid wave model routine.
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
    !       !/MPI  Distributed memory model.
    !
    !       !/S    Enable subroutine tracing.
    !       !/T    Enable test output
    !       !/MPIT
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !     USE CONSTANTS
    !
    USE W3GDATMD, ONLY: W3SETG
    USE W3WDATMD, ONLY: W3SETW
    USE W3ODATMD, ONLY: W3SETO
    USE WMMDATMD, ONLY: WMSETM
    USE W3CSPCMD, ONLY: W3CSPC
    USE W3IOPOMD
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, XFR, FR1, TH, SGRDS
    USE W3WDATMD, ONLY: TIME
    USE W3ODATMD, ONLY: IAPROC, NAPROC, NAPPNT, NOPTS, SPCO, DPO,   &
         WAO, WDO, ASO, CAO, CDO, OUTPTS,            &
         ICEO,ICEHO,ICEFO
    USE WMMDATMD, ONLY: MDST, MDSE, IMPROC, NMPROC, NMPUPT, NRGRD,  &
         RESPEC, UPTMAP, MDSUP
#ifdef W3_ASCII
    USE WMMDATMD, ONLY: MDSUPA
#endif
#ifdef W3_MPI
    USE WMMDATMD, ONLY: MPI_COMM_MWAVE, MPI_COMM_GRD, ALLPRC,  &
         MTAG0
#endif
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !
#ifdef W3_MPI
    INCLUDE "mpif.h"
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: TOUT(2)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: J, I, II, IT0, IT, ITARG, IFROM
#ifdef W3_SHRD
    INTEGER                 :: MPI_COMM_GRD = 1, CROOT = 1
    INTEGER, PARAMETER      :: MPI_COMM_NULL = -1
#endif
#ifdef W3_MPI
    INTEGER                 :: IERR_MPI, NMPPNT
    INTEGER, ALLOCATABLE    :: STATUS(:,:)
#endif
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL, POINTER           :: SPEC(:,:)
#ifdef W3_MPI
    REAL, POINTER           :: SPCR(:,:), DPR(:), WAR(:),      &
         WDR(:), ASR(:), CAR(:), CDR(:)
    REAL, POINTER           :: ICRO(:), ICRFO(:), ICRHO(:)
#endif
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'WMIOPO')
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 0.  Initializations
    !
#ifdef W3_T
    WRITE (MDST,9000) NMPUPT, IMPROC
#endif
    !
    IF ( IMPROC .EQ. NMPUPT ) THEN
      OUTPTS(0)%OUT2%SPCO  = 0.
      OUTPTS(0)%OUT2%DPO   = 1.
      OUTPTS(0)%OUT2%WAO   = 0.
      OUTPTS(0)%OUT2%WDO   = 0.
      OUTPTS(0)%OUT2%ASO   = 0.
      OUTPTS(0)%OUT2%CAO   = 0.
      OUTPTS(0)%OUT2%CDO   = 0.
      OUTPTS(0)%OUT2%ICEO  = 0.
      OUTPTS(0)%OUT2%ICEFO = 0.
      OUTPTS(0)%OUT2%ICEHO = 0.
    END IF
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Loop over grids for processing local data
    !
    DO J=1, NRGRD
      !
      ! 1.a Set up loop
      !
      CALL W3SETO ( J, MDSE, MDST )
      CALL W3SETG ( J, MDSE, MDST )
      CALL WMSETM ( J, MDSE, MDST )
      !
#ifdef W3_T
      WRITE (MDST,9010) J, NOPTS, IAPROC, NAPPNT
#endif
      !
      ! 1.b Determine if action
      !
      IF ( MPI_COMM_GRD .EQ. MPI_COMM_NULL ) THEN
#ifdef W3_T
        WRITE (MDST,9011)
#endif
        CYCLE
      END IF
      !
      IF ( NOPTS .EQ. 0 ) THEN
#ifdef W3_T
        WRITE (MDST,9012)
#endif
        CYCLE
      END IF
      !
      IF ( IAPROC .NE. NAPPNT ) THEN
#ifdef W3_T
        WRITE (MDST,9014)
#endif
        CYCLE
      END IF
      !
      ! 1.c Data here, and to remain on present processor.
      !
      IF ( IMPROC .EQ. NMPUPT ) THEN
#ifdef W3_T
        WRITE (MDST,9015)
#endif
        !
        ! 1.c.1 Spectral conversion if needed
        !
        IF ( RESPEC(0,J) ) THEN
#ifdef W3_T
          WRITE (MDST,9016) 'YES'
#endif
          ALLOCATE ( SPEC(SGRDS(0)%NSPEC,NOPTS) )
          CALL W3CSPC ( SPCO, NK, NTH, XFR, FR1, TH(1), SPEC,   &
               SGRDS(0)%NK, SGRDS(0)%NTH, SGRDS(0)%XFR,         &
               SGRDS(0)%FR1, SGRDS(0)%TH(1), NOPTS, MDST, MDSE, &
               SGRDS(0)%FACHFE )
          !
          ! 1.c.2 Spectral conversion not needed
          !
        ELSE
#ifdef W3_T
          WRITE (MDST,9016) 'NO'
#endif
          SPEC   => SPCO
        END IF
        !
        ! 1.d Store data at grid 0
        !
#ifdef W3_T
        WRITE (MDST,9017) J
#endif
        !
        DO I=1, NOPTS
          II     = UPTMAP(I)
          OUTPTS(0)%OUT2%SPCO(:,II)  = SPEC(:,I)
          OUTPTS(0)%OUT2%DPO(II)     = DPO(I)
          OUTPTS(0)%OUT2%WAO(II)     = WAO(I)
          OUTPTS(0)%OUT2%WDO(II)     = WDO(I)
          OUTPTS(0)%OUT2%ASO(II)     = ASO(I)
          OUTPTS(0)%OUT2%CAO(II)     = CAO(I)
          OUTPTS(0)%OUT2%CDO(II)     = CDO(I)
          OUTPTS(0)%OUT2%ICEO(II)    = ICEO(I)
          OUTPTS(0)%OUT2%ICEFO(II)   = ICEFO(I)
          OUTPTS(0)%OUT2%ICEHO(II)   = ICEHO(I)
        END DO
        !
        IF ( RESPEC(0,J) ) DEALLOCATE ( SPEC )
        !
        ! 1.e Data here, and to be sent to other processor.
        !
#ifdef W3_MPI
      ELSE
#endif
        !
#ifdef W3_MPIT
        WRITE (MDST,9018) J, IMPROC, NMPUPT
#endif
        !
#ifdef W3_MPI
        IT0    = MTAG0 - 7*NRGRD - 1
        IT     = IT0 + (J-1)*7
        ITARG  = NMPUPT - 1
#endif
        !
#ifdef W3_MPI
        IT     = IT + 1
        CALL MPI_SEND ( SPCO(1,1), NSPEC*NOPTS, MPI_REAL,    &
             ITARG, IT, MPI_COMM_MWAVE, IERR_MPI )
#endif
#ifdef W3_MPIT
        WRITE (MDST,9019) IT-IT0, 'SPECTRA'
#endif
#ifdef W3_MPI
        IT     = IT + 1
        CALL MPI_SEND ( DPO(1), NOPTS, MPI_REAL, ITARG, IT,  &
             MPI_COMM_MWAVE, IERR_MPI )
#endif
#ifdef W3_MPIT
        WRITE (MDST,9019) IT-IT0, 'WATER DEPTHS'
#endif
#ifdef W3_MPI
        IT     = IT + 1
        CALL MPI_SEND ( WAO(1), NOPTS, MPI_REAL, ITARG, IT,  &
             MPI_COMM_MWAVE, IERR_MPI )
#endif
#ifdef W3_MPIT
        WRITE (MDST,9019) IT-IT0, 'WIND SPEED'
#endif
#ifdef W3_MPI
        IT     = IT + 1
        CALL MPI_SEND ( WDO(1), NOPTS, MPI_REAL, ITARG, IT,  &
             MPI_COMM_MWAVE, IERR_MPI )
#endif
#ifdef W3_MPIT
        WRITE (MDST,9019) IT-IT0, 'WIND DIRECTION'
#endif
#ifdef W3_MPI
        IT     = IT + 1
        CALL MPI_SEND ( ASO(1), NOPTS, MPI_REAL, ITARG, IT,  &
             MPI_COMM_MWAVE, IERR_MPI )
#endif
#ifdef W3_MPIT
        WRITE (MDST,9019) IT-IT0, 'AIR_SEA TEMP DIFF'
#endif
#ifdef W3_MPI
        IT     = IT + 1
        CALL MPI_SEND ( CAO(1), NOPTS, MPI_REAL, ITARG, IT,  &
             MPI_COMM_MWAVE, IERR_MPI )
#endif
#ifdef W3_MPIT
        WRITE (MDST,9019) IT-IT0, 'CURRENT VELOCITY'
#endif
#ifdef W3_MPI
        IT     = IT + 1
        CALL MPI_SEND ( CDO(1), NOPTS, MPI_REAL, ITARG, IT,  &
             MPI_COMM_MWAVE, IERR_MPI )
#endif
#ifdef W3_MPIT
        WRITE (MDST,9019) IT-IT0, 'CURRENT DIRECTION'
#endif
        !JDM: The below should be added for points using partitioned processors
        ! for multigrid, however I am unsure if the IT0 (7 to 10?) should be changed so
        ! this is being left here commented out for now.
        ! There is a corresponding section to this below
        !!/MPI            IT     = IT + 1
        !!/MPI            CALL MPI_SEND ( ICEO(1), NOPTS, MPI_REAL, ITARG, IT,  &
        !!/MPI                            MPI_COMM_MWAVE, IERR_MPI )
        !!/MPIT            WRITE (MDST,9019) IT-IT0, 'ICEO'
        !!/MPI            IT     = IT + 1
        !!/MPI            CALL MPI_SEND ( ICEFO(1), NOPTS, MPI_REAL, ITARG, IT,  &
        !!/MPI                            MPI_COMM_MWAVE, IERR_MPI )
        !!/MPIT            WRITE (MDST,9019) IT-IT0, 'ICEFO'
        !!/MPI            IT     = IT + 1
        !!/MPI            CALL MPI_SEND ( ICEHO(1), NOPTS, MPI_REAL, ITARG, IT,  &
        !!/MPI                            MPI_COMM_MWAVE, IERR_MPI )
        !!/MPIT            WRITE (MDST,9019) IT-IT0, 'ICEHO'
        !
      END IF
      !
    END DO
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Check if this is output processor, otherwise exit
    !
    IF ( IMPROC .NE. NMPUPT ) THEN
#ifdef W3_T
      WRITE (MDST,9020)
#endif
      RETURN
    END IF
    !
    ! -------------------------------------------------------------------- /
    ! 3.  Loop over grids for processing remote data
    !
#ifdef W3_MPIT
    WRITE (MDST,9030)
#endif
    !
    ! 3.a Loop setup
    !
#ifdef W3_MPI
    DO J=1, NRGRD
#endif
      !
#ifdef W3_MPI
      CALL W3SETO ( J, MDSE, MDST )
      CALL W3SETG ( J, MDSE, MDST )
      CALL WMSETM ( J, MDSE, MDST )
#endif
      !
#ifdef W3_MPI
      DO NMPPNT= NMPROC, 1, -1
        IF ( ALLPRC(NMPPNT,J) .EQ. NAPPNT ) EXIT
      END DO
#endif
      !
#ifdef W3_MPIT
      WRITE (MDST,9031) J, NOPTS, NMPPNT
#endif
#ifdef W3_MPI
      IF ( NMPPNT.EQ.NMPUPT .OR. NOPTS.EQ.0 ) THEN
#endif
#ifdef W3_MPIT
        WRITE (MDST,9032)
#endif
#ifdef W3_MPI
        CYCLE
      END IF
#endif
      !
      ! 3.b Receive data
      !
#ifdef W3_MPI
      IT0    = MTAG0 - 7*NRGRD - 1
      IT     = IT0 + (J-1)*7
      IFROM  = NMPPNT - 1
      ALLOCATE ( SPCR(NSPEC,NOPTS), STATUS(MPI_STATUS_SIZE,1),  &
           DPR(NOPTS), WAR(NOPTS), WDR(NOPTS), ASR(NOPTS),&
           CAR(NOPTS), CDR(NOPTS), ICRO(NOPTS),           &
           ICRFO(NOPTS), ICRHO(NOPTS) )
#endif
      !
#ifdef W3_MPI
      IT     = IT + 1
      CALL MPI_RECV ( SPCR(1,1), NSPEC*NOPTS, MPI_REAL, IFROM,  &
           IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
#endif
#ifdef W3_MPIT
      WRITE (MDST,9019) IT-IT0, 'SPECTRA'
#endif
#ifdef W3_MPI
      IT     = IT + 1
      CALL MPI_RECV ( DPR(1), NSPEC*NOPTS, MPI_REAL, IFROM,     &
           IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
#endif
#ifdef W3_MPIT
      WRITE (MDST,9019) IT-IT0, 'WATER DEPTHS'
#endif
#ifdef W3_MPI
      IT     = IT + 1
      CALL MPI_RECV ( WAR(1), NSPEC*NOPTS, MPI_REAL, IFROM,     &
           IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
#endif
#ifdef W3_MPIT
      WRITE (MDST,9019) IT-IT0, 'WIND SPEED'
#endif
#ifdef W3_MPI
      IT     = IT + 1
      CALL MPI_RECV ( WDR(1), NSPEC*NOPTS, MPI_REAL, IFROM,     &
           IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
#endif
#ifdef W3_MPIT
      WRITE (MDST,9019) IT-IT0, 'WIND DIRECTION'
#endif
#ifdef W3_MPI
      IT     = IT + 1
      CALL MPI_RECV ( ASR(1), NSPEC*NOPTS, MPI_REAL, IFROM,     &
           IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
#endif
#ifdef W3_MPIT
      WRITE (MDST,9019) IT-IT0, 'AIR_SEA TEMP DIFF'
#endif
#ifdef W3_MPI
      IT     = IT + 1
      CALL MPI_RECV ( CAR(1), NSPEC*NOPTS, MPI_REAL, IFROM,     &
           IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
#endif
#ifdef W3_MPIT
      WRITE (MDST,9019) IT-IT0, 'CURRENT VELOCITY'
#endif
#ifdef W3_MPI
      IT     = IT + 1
      CALL MPI_RECV ( CDR(1), NSPEC*NOPTS, MPI_REAL, IFROM,     &
           IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
#endif
#ifdef W3_MPIT
      WRITE (MDST,9019) IT-IT0, 'CURRENT DIRECTION'
#endif
      !JDM: The below should be added for points using partitioned processors
      ! for multigrid, however I am unsure if the IT0 (7 to 10?) should be changed so
      ! this is being left here commented out for now.
      ! There is a corresponding section to this above
      !!/MPI         IT     = IT + 1
      !!/MPI         CALL MPI_RECV ( ICRO(1), NSPEC*NOPTS, MPI_REAL, IFROM,   &
      !!/MPI                        IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
      !!/MPIT         WRITE (MDST,9019) IT-IT0, 'ICEO'
      !!/MPI         IT     = IT + 1
      !!/MPI         CALL MPI_RECV (ICRFO(1), NSPEC*NOPTS, MPI_REAL, IFROM,   &
      !!/MPI                        IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
      !!/MPIT         WRITE (MDST,9019) IT-IT0, 'ICEFO'
      !!/MPI         IT     = IT + 1
      !!/MPI         CALL MPI_SEND (ICRHO(1), NSPEC*NOPTS, MPI_REAL, IFROM,   &
      !!/MPI                        IT, MPI_COMM_MWAVE, STATUS, IERR_MPI )
      !!/MPIT            WRITE (MDST,9019) IT-IT0, 'ICEHO'
      !
      ! 3.c Convert if necessary
      !
#ifdef W3_MPI
      IF ( RESPEC(0,J) ) THEN
#endif
#ifdef W3_MPIT
        WRITE (MDST,9016) 'YES'
#endif
#ifdef W3_MPI
        ALLOCATE ( SPEC(SGRDS(0)%NSPEC,NOPTS) )
        CALL W3CSPC ( SPCR, NK, NTH, XFR, FR1, TH(1), SPEC,   &
             SGRDS(0)%NK, SGRDS(0)%NTH, SGRDS(0)%XFR,         &
             SGRDS(0)%FR1, SGRDS(0)%TH(1), NOPTS, MDST, MDSE, &
             SGRDS(0)%FACHFE )
      ELSE
#endif
#ifdef W3_MPIT
        WRITE (MDST,9016) 'NO'
#endif
#ifdef W3_MPI
        SPEC   => SPCR
      END IF
#endif
      !
      ! 3.d Store data at grid 0
      !
#ifdef W3_MPIT
      WRITE (MDST,9117) J
#endif
      !
#ifdef W3_MPI
      DO I=1, NOPTS
        II     = UPTMAP(I)
        OUTPTS(0)%OUT2%SPCO(:,II)  = SPEC(:,I)
        OUTPTS(0)%OUT2%DPO(II)     = DPR(I)
        OUTPTS(0)%OUT2%WAO(II)     = WAR(I)
        OUTPTS(0)%OUT2%WDO(II)     = WDR(I)
        OUTPTS(0)%OUT2%ASO(II)     = ASR(I)
        OUTPTS(0)%OUT2%CAO(II)     = CAR(I)
        OUTPTS(0)%OUT2%CDO(II)     = CDR(I)
        OUTPTS(0)%OUT2%ICEO(II)    = ICEO(I)
        OUTPTS(0)%OUT2%ICEFO(II)    = ICEFO(I)
        OUTPTS(0)%OUT2%ICEHO(II)    = ICEHO(I)
      END DO
#endif
      !
#ifdef W3_MPI
      IF ( RESPEC(0,J) ) DEALLOCATE ( SPEC )
      DEALLOCATE ( SPCR, DPR, WAR, WDR, ASR, CAR, CDR, STATUS )
#endif
      !        !JDM add deallocates here and check the itag stuff.. really not
      !        sure aabout that
#ifdef W3_MPI
      DEALLOCATE (ICRO, ICRFO, ICRHO)
    END DO
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 4.  Output data
    !
#ifdef W3_T
    WRITE (MDST,9040)
#endif
    !
    CALL W3SETO ( 0, MDSE, MDST )
    CALL W3SETG ( 0, MDSE, MDST )
    CALL W3SETW ( 0, MDSE, MDST )
    !
    TIME   = TOUT
    !
#ifdef W3_BIN2NC
    CALL W3IOPON ( 'WRITE', MDSUP, II, 0)
#else
    CALL W3IOPO ( 'WRITE', MDSUP, II, 0 &
#ifdef W3_ASCII
            ,MDSUPA                     &
#endif
            )
#endif 
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT ( ' TEST WMIOPO : OUTPUT/ACTUAL PROCESS    : ',2I6)
9010 FORMAT ( ' TEST WMIOPO : PROCESSING GRID          : ',I6/    &
         '               OUTPUT POINTS            : ',I6/    &
         '               ACTUAL/OUTPUT PROCESS    : ',2I6)
9011 FORMAT ( '       CYCLE : GRID NOT ON PROCESS')
9012 FORMAT ( '       CYCLE : GRID WITHOUT OUTPUT POINTS')
9014 FORMAT ( '       CYCLE : DATA NOT ON PRESENT PROCESS')
9015 FORMAT ( ' TEST WMIOPO : PROCESSING DATA LOCALLY')
9016 FORMAT ( ' TEST WMIOPO : NEED FOR SPECTRAL CONVERSION : ',A)
9017 FORMAT ( ' TEST WMIOPO : STORING DATA FROM GRID',I4,         &
         ' IN GRID 0')
#endif
#ifdef W3_MPIT
9117 FORMAT ( ' TEST WMIOPO : STORING DATA FROM GRID',I4,      &
         ' IN GRID 0')
9018 FORMAT ( ' TEST WMIOPO : GRID',I4,' SEND FROM',I4,' TO',I4)
9019 FORMAT ( '                 IT = ',I4,'  PAR = ',A)
#endif
    !
#ifdef W3_T
9020 FORMAT ( ' TEST WMIOPO : DONE AT THIS PROCESSOR')
#endif
    !
#ifdef W3_MPIT
9030 FORMAT ( ' TEST WMIOPO : LOOP OVER GRIDS FOR REMOTE DATA')
9031 FORMAT ( ' TEST WMIOPO : GRID',I4,',',I4,' POINTS FROM',I4)
9032 FORMAT ( '               NOTHING TO RECEIVE')
#endif
    !
9040 FORMAT ( ' TEST WMIOPO : PERFORM OUTPUT')
    !/
    !/ End of WMIOPO ----------------------------------------------------- /
    !/
  END SUBROUTINE WMIOPO
  !/
  !/ End of module WMIOPOMD -------------------------------------------- /
  !/
END MODULE WMIOPOMD
