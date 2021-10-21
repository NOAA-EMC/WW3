#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3IOPOMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         05-Jun-2018 |
!/                  +-----------------------------------+
!/
!/    25-Jan-2001 : Origination.                        ( version 2.00 )
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    11-Jun-2001 : Clean-up.                           ( version 2.11 )
!/    10-Nov-2004 : Multiple grid version.              ( version 3.06 )
!/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    25-Jul-2006 : Adding grid ID per point.           ( version 3.10 )
!/    01-May-2007 : Move O7a output from W3INIT.        ( version 3.11 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    29-Oct-2010 : Implement unstructured grid         ( version 3.14.4 )
!/                  (A. Roland and F. Ardhuin)
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    12-Jun-2012 : Add /RTD option or rotated grid option.
!/                  (Jian-Guo Li)                       ( version 4.06 )
!/    02-Sep-2012 : Clean up of open BC for UG grids    ( version 4.07 )
!/    25-Feb-2013 : ITOUT=0 bug correction for UG grids ( version 4.08 )
!/    11-Nov-2013 : SMC and rotated grid incorporated in the main
!/                  trunk                               ( version 4.13 )
!/    05-Jun-2018 : Add SETUP                           ( version 6.04 )
!/    04-Oct-2019 : Optional one file per output stride ( version 7.00 )
!/                  (R. Padilla-Hernandez & J.H. Alves)
!/
!/    Copyright 2009 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Process point output.
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      VEROPT    C*10  Private  Point output file version number.
!      IDSTR     C*32  Private  Point output file ID string.
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3IOPP    Subr. Public   Preprocessing of point output req.
!      W3IOPE    Subr. Public   Extract point data from grid.
!      W3IOPO    Subr. Public   Point data IO.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETO    Subr. W3ODATMD Data structure management.
!      W3SETG    Subr. W3GDATMD Data structure management.
!      W3SETW    Subr. W3WDATMD Data structure management.
!      W3DMO2    Subr. W3ODATMD Data structure management.
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      EXTCDE    Subr. W3SERVMD Program abort with exit code.
!      MPI_STARTALL, MPIWAITALL
!                Subr.          MPI persistent communication routines.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!     - Allocation of allocatable arrays takes place at different
!       places throughout the code, in W3IOPP on write, and in
!       W3IOPO on read.
!
!  6. Switches :
!
!       !/S     Enable subroutine tracing.
!       !/T     Enable test output.
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   MPI message passing.
!
!       !/O7a   Diagnostic output for output points.
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      PUBLIC
!/
!/ Private parameter statements (ID strings)
!/
      CHARACTER(LEN=10), PARAMETER, PRIVATE :: VEROPT = '2021-04-06'
      CHARACTER(LEN=31), PARAMETER, PRIVATE ::                        &
                           IDSTR = 'WAVEWATCH III POINT OUTPUT FILE'
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3IOPP ( NPT, XPT, YPT, PNAMES, IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         02-Sep-2012 |
!/                  +-----------------------------------+
!/
!/    14-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    30-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    09-Nov-2004 : Multiple grid version.              ( version 3.06 )
!/    25-Jul-2006 : Adding grid ID per point.           ( version 3.10 )
!/    01-May-2007 : Move O7a output from W3INIT.        ( version 3.11 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    12-Jun-2012 : Add /RTD option or rotated grid option.
!/                  (Jian-Guo Li)                       ( version 4.06 )
!/    02-Sep-2012 : Clean up of open BC for UG grids    ( version 4.07 )
!/    01-Mar-2018 : Add option to unrotate spectra      ( version 6.02 )
!/                  from RTD grid models
!/
!  1. Purpose :
!
!     Preprocessing of point output.
!
!  2. Method :
!
!     Check location of points in grid and calculate interpolation
!     factors.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NPT     Int.   I   Number of output points in input.
!       XPT     R.A.  I/O  X (longitude) coordinates of output points.
!       YPT     R.A.  I/O  Id. Y.
!       PNAMES  C*40   I   Names of output points.
!       IMOD    Int.   I   Grid ID number.
!     ----------------------------------------------------------------
!
!     Local data
!     ----------------------------------------------------------------
!       ACC     Real  "Accuracy" factor to determine if output point
!                     is grid point.
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
!      W3INIT    Subr. W3INITMD Wave model initialization routine.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     - Warnings for points out of the grid or on land.
!
!  7. Remarks :
!
!     - The output points are obtained by bi-linear interpolation from
!       the spectra at the grid points. Given the possibility of ice
!       coverage, the actual interpolation factors can only be
!       determined at the actual output time. Hence only the basic
!       bilinear interpolation factors are stored.
!     - Implementation of the /O7a diagnostic output section is
!       currently incomplete and non-functional for curvilinear grids
!       and/or tripole grids
!
!  8. Structure :
!
!     -------------------------------------------
!      Determine grid range
!      do for all defined points
!      -----------------------------------------
!        Check if point within grid
!        Calculate interpolation data
!        Check if point not on land
!        Store interpolation data
!     -------------------------------------------
!
!  9. Switches :
!
!       !/S     Enable subroutine tracing.
!       !/T     Test output.
!
!       !/O7a   Diagnostic output for output points.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GSRUMD
      USE W3GDATMD, ONLY: NTH, NK, NSPEC, NX, NY, X0, Y0, SX, GSU,&
                          RLGTYPE, CLGTYPE, UNGTYPE, GTYPE, FLAGLL,   &
                          ICLOSE,ICLOSE_NONE,ICLOSE_SMPL,ICLOSE_TRPL, &
                          MAPSTA, MAPFS, FILEXT, ZB, TRNX, TRNY
      USE W3GDATMD, ONLY: XYB, TRIGP,MAXX, MAXY, DXYMAX
#ifdef W3_RTD
  !!  Use rotated N-Pole lat/lon and conversion sub.  JGLi12Jun2012
      USE W3GDATMD, ONLY: PoLat, PoLon, FLAGUNR
      USE W3SERVMD, ONLY: W3LLTOEQ
#endif
      USE W3ODATMD, ONLY: W3DMO2
      USE W3ODATMD, ONLY: NDSE, NDST, IAPROC, NAPERR, NAPOUT, SCREEN, &
                          NOPTS, PTLOC, PTNME, GRDID, IPTINT, PTIFAC
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
      USE W3TRIAMD
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)          :: NPT, IMOD
      REAL, INTENT(INOUT)          :: XPT(NPT), YPT(NPT)
      CHARACTER(LEN=40),INTENT(IN) :: PNAMES(NPT)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      LOGICAL                 :: INGRID
      INTEGER                 :: IPT, J, K
      INTEGER                 :: IX1, IY1, IXS, IYS
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_O7a
      INTEGER                 :: IX0, IXN, IY0, IYN, NNX,         &
                                 KX, KY, JX, IIX, IX2, IY2, IS1
#endif
      INTEGER                 :: IX(4), IY(4)   ! Indices of points used in interp.
      REAL                    :: RD(4)          ! Interpolation coefficient
#ifdef W3_O7a
      REAL                    :: RD1, RD2, RDTOT, ZBOX(4), DEPTH
#endif
      REAL, PARAMETER         :: ACC = 0.05
      REAL                    :: FACTOR
      INTEGER                 :: ITOUT          ! Triangle index in unstructured grids
#ifdef W3_O7a
       CHARACTER(LEN=1)         :: SEA(5), LND(5), OUT(5)
       CHARACTER(LEN=9)         :: PARTS
       CHARACTER(LEN=1), ALLOCATABLE :: STRING(:), LINE1(:), LINE2(:)
#endif
!
#ifdef W3_O7a
       DATA SEA / ' ', 's', 'e', 'a', ' ' /
       DATA LND / ' ', 'l', 'n', 'd', ' ' /
       DATA OUT / ' ', 'x', 'x', 'x', ' ' /
#endif
!/
#ifdef W3_RTD
  !!  Declare a few temporary variables for rotated grid.  JGLi12Jun2012
       REAL, ALLOCATABLE :: EquLon(:),EquLat(:),StdLon(:),StdLat(:),AnglPT(:)
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3IOPP')
#endif
!
      IF ( FLAGLL ) THEN
          FACTOR = 1.
        ELSE
          FACTOR = 1.E-3
        END IF
!
      CALL W3DMO2 ( IMOD, NDSE, NDST, NPT )
      GRDID  = FILEXT
!
      NOPTS  = 0
!
#ifdef W3_RTD
 !!   Convert standard lon/lat to rotated lon/lat  JGLi12Jun2012
       ALLOCATE( EquLon(NPT), EquLat(NPT),                 &
      &          StdLon(NPT), StdLat(NPT), AnglPT(NPT) )

       StdLon = XPT
       StdLat = YPT

       CALL W3LLTOEQ ( StdLat, StdLon, EquLat, EquLon,     &
      &                AnglPT, PoLat, PoLon, NPT )

       XPT = EquLon
       YPT = EquLat

#endif
!
!   Removed by F.A. 2011/04/04  /T      CALL W3GSUP( GSU, NDST )
!
! Loop over output points
!
      DO IPT=1, NPT
!
#ifdef W3_T
        WRITE (NDST,9010) IPT, XPT(IPT), YPT(IPT), PNAMES(IPT)
#endif
!
#ifdef W3_RTD
 !!   Need to wrap rotated Elon values greater than X0.  JGLi12Jun2012
          XPT(IPT) = MOD( EquLon(IPT)+360.0, 360.0 )
          IF( XPT(IPT) .LT. X0 )  XPT(IPT) = XPT(IPT) + 360.0
#endif
!
!     Check if point within grid and compute interpolation weights
!
        IF (GTYPE .NE. UNGTYPE) THEN
          INGRID = W3GRMP( GSU, XPT(IPT), YPT(IPT), IX, IY, RD )
        ELSE
          CALL IS_IN_UNGRID(IMOD, XPT(IPT), YPT(IPT), itout, IX, IY, RD)
          INGRID = (ITOUT.GT.0)
          END IF
!
        IF ( .NOT.INGRID ) THEN
          IF ( IAPROC .EQ. NAPERR ) THEN
            IF ( FLAGLL ) THEN
              WRITE (NDSE,1000) XPT(IPT), YPT(IPT), PNAMES(IPT)
            ELSE
              WRITE (NDSE,1001) XPT(IPT), YPT(IPT), PNAMES(IPT)
              END IF
            END IF
          CYCLE
          END IF
!
#ifdef W3_T
     DO K = 1,4
       WRITE (NDST,9012) IX(K), IY(K), RD(K)
       END DO
#endif
!
!     Check if point not on land
!
        IF ( MAPSTA(IY(1),IX(1)) .EQ. 0 .AND. &
             MAPSTA(IY(2),IX(2)) .EQ. 0 .AND. &
             MAPSTA(IY(3),IX(3)) .EQ. 0 .AND. &
             MAPSTA(IY(4),IX(4)) .EQ. 0 ) THEN
          IF ( IAPROC .EQ. NAPERR ) THEN
            IF ( FLAGLL ) THEN
              WRITE (NDSE,1002) XPT(IPT), YPT(IPT), PNAMES(IPT)
            ELSE
              WRITE (NDSE,1003) XPT(IPT), YPT(IPT), PNAMES(IPT)
              END IF
            END IF
          CYCLE
          END IF
!
!     Store interpolation data
!
        NOPTS  = NOPTS + 1
!
        PTLOC (1,NOPTS) = XPT(IPT)
        PTLOC (2,NOPTS) = YPT(IPT)
#ifdef W3_RTD
 !!   Store the standard lon/lat in PTLOC for output purpose, assuming
 !!   they are not used for any inside calculation.  JGLi12Jun2012
        PTLOC (1,NOPTS) = StdLon(IPT)
        PTLOC (2,NOPTS) = StdLat(IPT)
#endif
!
        DO K = 1,4
          IPTINT(1,K,NOPTS) = IX(K)
          IPTINT(2,K,NOPTS) = IY(K)
          PTIFAC(K,NOPTS) = RD(K)
          END DO

        PTNME(NOPTS) = PNAMES(IPT)
!
        END DO ! End loop over output points (IPT).
!
#ifdef W3_RTD
       DEALLOCATE( EquLon, EquLat, StdLon, StdLat, AnglPT )
#endif
!
! Diagnostic output
!
#ifdef W3_O7a
      IF ( IAPROC .EQ. NAPOUT ) THEN
          WRITE (SCREEN,940) NOPTS
          DO J=1, NOPTS
#endif
!
#ifdef W3_O7a
            WRITE (SCREEN,941) PTNME(J), PTLOC(:,J)*FACTOR
            IX(:) = IPTINT(1,:,J)
            IY(:) = IPTINT(2,:,J)
            RD(:) = PTIFAC(:,J)
            WRITE (SCREEN,942) (IX(K),IY(K),RD(K),K=1,4)
#endif
!
#ifdef W3_O7a
            ZBOX   = 0.
            RDTOT  = 0.
            DO K = 1,4
                IF ( MAPFS(IY(K),IX(K)) .GT. 0 ) THEN
                    ZBOX(K) = ZB(IX(K))
                    RDTOT   = RDTOT + RD(K)
                  END IF
              END DO
            RDTOT  = MAX ( 1.E-7 , RDTOT )
#endif
!
#ifdef W3_O7a
            DEPTH  = - ( RD(1)*ZBOX(1) + &
                         RD(2)*ZBOX(2) + &
                         RD(3)*ZBOX(3) + &
                         RD(4)*ZBOX(4) ) / RDTOT
            WRITE (SCREEN,943) DEPTH
#endif
!
#ifdef W3_O7a
      ! *** implementation of O7a option with curvilinear grids is incomplete ***
#endif
!
#ifdef W3_O7a
            IF ( RD1 .LT. 0.05 ) IX2 = IX1
            IF ( RD1 .GT. 0.95 ) IX1 = IX2
            IF ( RD2 .LT. 0.05 ) IY2 = IY1
            IF ( RD2 .GT. 0.95 ) IY1 = IY2
            IX0    = IX1 - 1
            IXN    = IX2 + 1
            IY0    = MAX ( 1 , IY1 - 1 )
            IYN    = MIN ( IY2 + 1 , NY )
            NNX    = 13 * ( IXN - IX0 + 1 )
#endif
!
#ifdef W3_O7a
            ALLOCATE ( STRING(NNX), LINE1(NNX), LINE2(NNX) )
            DO KX=1, NNX
              LINE1(KX) = ' '
              LINE2(KX) = '-'
              END DO
            DO KX=7, NNX, 13
              LINE1(KX) = '|'
              LINE2(KX) = '+'
              END DO
#endif
!
#ifdef W3_O7a
            IF ( ICLOSE.NE.ICLOSE_NONE ) THEN
                WRITE (SCREEN,945) (1+MOD(KX+NX-1,NX),KX=IX0,IXN)
              ELSE
                WRITE (SCREEN,945) (KX,KX=IX0,IXN)
              END IF
            WRITE (SCREEN,946) LINE1
#endif
!
#ifdef W3_O7a
            DO KY=IYN, IY0, -1
#endif
!
#ifdef W3_O7a
              STRING  = LINE1
              DO KX=IX0, IXN
                IF ( ICLOSE.NE.ICLOSE_NONE .OR. (KX.GE.1 .AND. KX.LE.NX) ) THEN
                    IIX    = 1 + MOD(KX-1+NX,NX)
                    IS1    = MAPFS(KY,IIX)
                    IF ( MAPSTA(KY,IIX) .NE. 0 ) THEN
                        WRITE (PARTS,'(F8.1,1X)') -ZB(IS1)
                        NNX    = 2 + (KX-IX0)*13
                        DO JX=1, 9
                          STRING(NNX+JX:NNX+JX) = PARTS(JX:JX)
                          END DO
                      ENDIF
                  END IF
                END DO
              WRITE (SCREEN,946) STRING
#endif
!
#ifdef W3_O7a
              STRING = LINE2
              DO KX=IX0, IXN
                NNX    = 5 + (KX-IX0)*13
                IF ( ICLOSE.EQ.ICLOSE_NONE .AND. (KX.LT.1.OR.KX.GT.NX) ) THEN
                    STRING(NNX:NNX+4) = OUT
                  ELSE
                    IIX    = 1 + MOD(KX-1+NX,NX)
                    IF ( MAPSTA(KY,IIX) .EQ. 0 ) THEN
                        STRING(NNX:NNX+4) = LND
                      ELSE
                        STRING(NNX:NNX+4) = SEA
                      END IF
                  END IF
                END DO
              WRITE (SCREEN,947) KY, STRING
#endif
!
#ifdef W3_O7a
              STRING  = LINE1
              DO KX=IX0, IXN
                IF ( ICLOSE.NE.ICLOSE_NONE .OR. (KX.GE.1 .AND. KX.LE.NX) ) THEN
                    IS1    = MAPFS(KY,KX)
                    IIX    = 1 + MOD(KX-1+NX,NX)
                    IF ( MAPSTA(KY,IIX) .NE. 0 ) THEN
                        WRITE (PARTS,'(I4,1A,I4)')               &
                               NINT(1000.*TRNX(KY,IIX)),         &
                               '|', NINT(1000.*TRNY(KY,IIX))
                        NNX    = 2 + (KX-IX0)*13
                        DO JX=1, 9
                          STRING(NNX+JX:NNX+JX) = PARTS(JX:JX)
                          END DO
                      ENDIF
                  END IF
                END DO
              WRITE (SCREEN,946) STRING
              WRITE (SCREEN,946) LINE1
#endif
!
#ifdef W3_O7a
              END DO
#endif
!
#ifdef W3_O7a
            IF ( ICLOSE.NE.ICLOSE_NONE ) THEN
                WRITE (SCREEN,945) (1+MOD(KX+NX-1,NX),KX=IX0,IXN)
              ELSE
                WRITE (SCREEN,945) (KX,KX=IX0,IXN)
              END IF
            DEALLOCATE ( STRING, LINE1, LINE2 )
#endif

#ifdef W3_O7a
            END DO
          WRITE (SCREEN,*)
          WRITE (SCREEN,*)
        END IF
#endif
!
      RETURN
!
! Formats
!
#ifdef W3_O7a
  940 FORMAT (/' Diagnostic output for output points [',I3,'] :'/&
                '--------------------------------------------'/  &
                '    Bottom level in m above grid point'/        &
                '    X/Y transparency in thousands below')
  941 FORMAT (/'    Point ',A,' at ',2F8.2,' (degr or km)'/    &
          '    -------------------------------------------------')
  942 FORMAT ( '       Interp. cell :',4(' (',2I5,F4.2,')'))
  943 FORMAT ( '       Depth (water level = 0)  :',F10.1,' m'/)
  945 FORMAT ( '          IX =  ',4I13)
  946 FORMAT ( '                     ',52A1)
  947 FORMAT ( '          IY =',I5,2X,52A1)
#endif
!
 1000 FORMAT (/' *** WAVEWATCH-III WARNING :'/                   &
               '     OUTPUT POINT OUT OF GRID : ',2F10.3,2X,A/   &
               '     POINT SKIPPPED '/)
 1001 FORMAT (/' *** WAVEWATCH-III WARNING :'/                   &
               '     OUTPUT POINT OUT OF GRID : ',2E10.3,2X,A/   &
               '     POINT SKIPPPED '/)
!
 1002 FORMAT (/' *** WAVEWATCH-III WARNING :'/                   &
               '     OUTPUT POINT ON LAND : ',2F10.3,2X,A/       &
               '     POINT SKIPPPED '/)
 1003 FORMAT (/' *** WAVEWATCH-III WARNING :'/                   &
               '     OUTPUT POINT ON LAND : ',2E10.3,2X,A/       &
               '     POINT SKIPPPED '/)
!
#ifdef W3_T
 9010 FORMAT (' TEST W3IOPP : INPUT  : ',I4,2F12.2,2X,A)
 9011 FORMAT ('               CORR.  :     ',2F12.2)
 9012 FORMAT (' TEST W3IOPP : INT. DATA: ',2I6,1F8.2)
 9013 FORMAT (' TEST W3IOPP : INT. DATA B): ',4I4,2F8.2)
 9020 FORMAT (' TEST W3IOPP : PREPROCESSED DATA',I4,2X,A,2X,2F12.2, &
           4(/'             ',2I5,2F6.3))
 9021 FORMAT (' TEST W3IOPP : PREPROCESSED DATA',I4,2X,A,2X,2F12.2, &
           4(/'             ',2I5,F6.3))
#endif
!/
!/ End of W3IOPP ----------------------------------------------------- /
!/
      END SUBROUTINE W3IOPP
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3IOPE ( A )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         12-Jun-2012 |
!/                  +-----------------------------------+
!/
!/    12-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    25-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    11-Jun-2001 : Clean-up.                           ( version 2.11 )
!/    09-Nov-2004 : Multiple grid version.              ( version 3.06 )
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.4 )
!/                  (A. Roland and F. Ardhuin)
!/    12-Jun-2012 : Add /RTD option or rotated grid option.
!/                  (Jian-Guo Li)                       ( version 4.06 )
!/    01-Mar-2018 : Add option to unrotate spectra      ( version 6.02 )
!/                  from RTD grid models
!/    19-Jul-2021 : Momentum and air density support    ( version 7.xx )
!/
!  1. Purpose :
!
!     Extract point output data and store in output COMMONs. This
!     action is taken from an earlier version of W3IOPO so that the
!     point output postprocessor does not need the full sea-point
!     grid to be able to run.
!       Note that the output spectrum is F(f,theta). Interpolation
!     is performed for this spectrum.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       A       R.A.   I   Action spectra on storage grid.
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
!     - To allow for dynamic ice edges, interpolation factors are
!       calculated for every time step separately.
!     - Wind current and depth data are interpolated ignoring ice,
!       spectrum is interpolated removing ice points.
!     - Spectra are left in par list to allow for change of shape of
!       arrays.
!     - IMOD is not passed to this routine. Since it is used only
!       in W3WAVE, it is assumed that the pointer are set
!       appropriately outside this routine.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/SHRD  Switch for shared / distributed memory architecture.
!     !/DIST  Id.
!     !/MPI   Switch for message passing method.
!
!     !/S     Enable subroutine tracing.
!     !/T     Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
      USE W3GDATMD, ONLY: NK, NTH, SIG, NX, NY, NSEA, NSEAL,          &
                          MAPSTA, MAPFS
#ifdef W3_RTD
 !!   Use spectral rotation sub and angle.  JGLi12Jun2012
      USE W3GDATMD, ONLY: NSPEC, AnglD, FLAGUNR
      USE W3SERVMD, ONLY: W3ACTURN
#endif
      USE W3WDATMD, ONLY: ICE, ICEH, ICEF
#ifdef W3_FLX5
      USE W3WDATMD, ONLY: RHOAIR
#endif
      USE W3ADATMD, ONLY: CG, DW, UA, UD, AS, CX, CY,                 &
                          SP => SPPNT
#ifdef W3_FLX5
      USE W3ADATMD, ONLY: TAUA, TAUADIR 
#endif
      USE W3ODATMD, ONLY: NDST, NOPTS, IPTINT, PTIFAC, IL, IW, II,    &
                          DPO, WAO, WDO, ASO, CAO, CDO, ICEO, ICEHO,  &
                          ICEFO, SPCO, NAPROC
#ifdef W3_FLX5
      USE W3ODATMD, ONLY: TAUAO, TAUDO, DAIRO
#endif
#ifdef W3_SETUP
      USE W3WDATMD, ONLY: ZETA_SETUP
      USE W3ODATMD, ONLY: ZET_SETO
#endif
#ifdef W3_MPI
      USE W3ODATMD, ONLY: IRQPO2
#endif
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
#ifdef W3_T
      USE W3ARRYMD, ONLY: PRT2DS
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
      REAL, INTENT(IN)        :: A(NTH,NK,0:NSEAL)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: I, IX1, IY1, IX(4), IY(4), J, IS(4), &
                                 IM(4), IK, ITH, ISP
#ifdef W3_MPI
      INTEGER                 :: IOFF, IERR_MPI
      INTEGER                 :: STAT(MPI_STATUS_SIZE,4*NOPTS)
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      REAL                    :: RD(4), RDS, RDI, FACRD,              &
                                 WNDX, WNDY, CURX, CURY, FAC1(NK),    &
                                 FAC2(NK), FAC3(NK), FAC4(NK)
#ifdef W3_FLX5
      REAL                    :: TAUX, TAUY
#endif
      INTEGER                 :: JSEA, ISEA
#ifdef W3_T
      REAL                    :: SPTEST(NK,NTH)
#endif
#ifdef W3_RTD
      REAL :: Spectr(NSPEC), AnglDIS
      INTEGER :: IROT
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3IOPE')
#endif
!
      CX(0)  = 0.
      CY(0)  = 0.
!
! Loop over spectra -------------------------------------------------- *
!
      DO I=1, NOPTS
!
#ifdef W3_T
        WRITE (NDST,9000) I
#endif
!
! Unpack interpolation data
!
        IX(:)  = IPTINT(1,:,I)
        IY(:)  = IPTINT(2,:,I)
        RD(:)  = PTIFAC(:,I)
!
#ifdef W3_T
!        WRITE (NDST,9001) IX1, IY1, IX(2)
#endif
!
!
! Correct for land and ice and get sea point counters
!
        IL(I)  = 0
        IW(I)  = 0
        II(I)  = 0
        RDS    = 0.
        RDI    = 0.
!
        DO J=1, 4
          IS(J)  = MAPFS (IY(J),IX(J))
          IM(J)  = MAPSTA(IY(J),IX(J))
          IF ( IM(J).GT.0 ) THEN
              IW(I)  = IW(I) + 1
              RDS    = RDS + RD(J)
#ifdef W3_RTD
              IROT   = IS(J) ! For rotation angle
#endif
            ELSE
              IF ( IM(J).LT.0 ) THEN
                  II(I)  = II(I) + 1
                  RDI    = RDI + RD(J)
                ELSE
                  IL(I)  = IL(I) + 1
                  RD(J)  = 0.
                END IF
            END IF
          END DO
!
! Depth, wind and current, ignore ice
!
        IF ( RDS+RDI .GT. 1.E-7 ) THEN
            FACRD  = 1. / (RDS+RDI)
            RD     = RD * FACRD
          END IF
!
#ifdef W3_T
        WRITE (NDST,9002) (IS(J),J=1,4), (IM(J),J=1,4), (RD(J),J=1,4)
#endif
!
! Interpolate ice depth, wind, stresses, rho air and current
!
        IF (.NOT. LPDLIB) THEN
          ICEFO(I) = 0
          DO J=1, 4
            ISEA = MAPFS(IY(J),IX(J))
#ifdef W3_DIST
          JSEA = 1 + (ISEA-1)/NAPROC
#endif
#ifdef W3_SHRD
          JSEA = ISEA
#endif
            ICEFO(I) = ICEFO(I) + RD(J)*ICEF(JSEA)
          END DO
        ELSE
          ICEFO(I) = RD(1)*ICEF(IS(1)) + RD(2)*ICEF(IS(2)) +          &
                   RD(3)*ICEF(IS(3)) + RD(4)*ICEF(IS(4))
        END IF

        ICEO(I) = RD(1)*ICE(IS(1)) + RD(2)*ICE(IS(2)) +               &
                 RD(3)*ICE(IS(3)) + RD(4)*ICE(IS(4))

        ICEHO(I) = RD(1)*ICEH(IS(1)) + RD(2)*ICEH(IS(2)) +            &
                 RD(3)*ICEH(IS(3)) + RD(4)*ICEH(IS(4))
!
        DPO(I) = RD(1)*DW(IS(1)) + RD(2)*DW(IS(2)) +                  &
                 RD(3)*DW(IS(3)) + RD(4)*DW(IS(4))
#ifdef W3_SETUP
        DPO(I) = RD(1)*ZETA_SETUP(IS(1)) +                     &
                 RD(2)*ZETA_SETUP(IS(2)) +                     &
                 RD(3)*ZETA_SETUP(IS(3)) +                     &
                 RD(4)*ZETA_SETUP(IS(4))
#endif
!
#ifdef W3_FLX5
        DAIRO(I) = RD(1)*RHOAIR(IS(1)) + RD(2)*RHOAIR(IS(2)) +        &
                 RD(3)*RHOAIR(IS(3)) + RD(4)*RHOAIR(IS(4))
#endif
!
        WNDX   = RD(1) * UA(IS(1)) * COS(UD(IS(1))) +                 &
                 RD(2) * UA(IS(2)) * COS(UD(IS(2))) +                 &
                 RD(3) * UA(IS(3)) * COS(UD(IS(3))) +                 &
                 RD(4) * UA(IS(4)) * COS(UD(IS(4)))
        WNDY   = RD(1) * UA(IS(1)) * SIN(UD(IS(1))) +                 &
                 RD(2) * UA(IS(2)) * SIN(UD(IS(2))) +                 &
                 RD(3) * UA(IS(3)) * SIN(UD(IS(3))) +                 &
                 RD(4) * UA(IS(4)) * SIN(UD(IS(4)))
!
        WAO(I) = SQRT ( WNDX**2 + WNDY**2 )
        IF ( WAO(I).GT.1.E-7 ) THEN
            WDO(I) = ATAN2(WNDY,WNDX)
#ifdef W3_RTD
            IF ( FLAGUNR ) WDO(I) = WDO(I) - AnglD(IS(1))*DERA
#endif
          ELSE
            WDO(I) = 0.
          END IF
!
#ifdef W3_FLX5
        TAUX   = RD(1) * TAUA(IS(1)) * COS(TAUADIR(IS(1))) +          &
                 RD(2) * TAUA(IS(2)) * COS(TAUADIR(IS(2))) +          &
                 RD(3) * TAUA(IS(3)) * COS(TAUADIR(IS(3))) +          &
                 RD(4) * TAUA(IS(4)) * COS(TAUADIR(IS(4)))
        TAUY   = RD(1) * TAUA(IS(1)) * SIN(TAUADIR(IS(1))) +          &
                 RD(2) * TAUA(IS(2)) * SIN(TAUADIR(IS(2))) +          &
                 RD(3) * TAUA(IS(3)) * SIN(TAUADIR(IS(3))) +          &
                 RD(4) * TAUA(IS(4)) * SIN(TAUADIR(IS(4)))
!
        TAUAO(I) = SQRT ( TAUX**2 + TAUY**2 )
        IF ( TAUAO(I).GT.1.E-7 ) THEN
            TAUDO(I) = ATAN2(TAUY,TAUX)
#ifdef W3_RTD
            IF ( FLAGUNR ) TAUDO(I) = TAUDO(I) - AnglD(IS(1))*DERA
#endif
          ELSE
            TAUDO(I) = 0.
          END IF
!
#endif
        ASO(I) = RD(1)*AS(IS(1)) + RD(2)*AS(IS(2)) +                  &
                 RD(3)*AS(IS(3)) + RD(4)*AS(IS(4))
!
        CURX   = RD(1)*CX(IS(1)) + RD(2)*CX(IS(2)) +                  &
                 RD(3)*CX(IS(3)) + RD(4)*CX(IS(4))
        CURY   = RD(1)*CY(IS(1)) + RD(2)*CY(IS(2)) +                  &
                 RD(3)*CY(IS(3)) + RD(4)*CY(IS(4))
!
        CAO(I) = SQRT ( CURX**2 + CURY**2 )
        IF ( CAO(I).GT.1.E-7 ) THEN
            CDO(I) = ATAN2(CURY,CURX)
#ifdef W3_RTD
            IF ( FLAGUNR ) CDO(I) = CDO(I) - AnglD(IS(1))*DERA
#endif
          ELSE
            CDO(I) = 0.
          END IF
!
! Interp. weights for spectra, no ice points (spectra by def. zero)
!
        IF ( RDS .GT. 1.E-7 ) THEN
            FACRD  = (RDS+RDI) / RDS
            RD     = RD * FACRD
          END IF
!
#ifdef W3_T
        WRITE (NDST,9003) (RD(J),J=1,4)
#endif
!
! Extract spectra, shared memory version
!        (done in separate step for MPP compatibility)
!
#ifdef W3_SHRD
        DO J=1, 4
          DO IK=1, NK
            DO ITH=1, NTH
              SP(ITH,IK,J) = A(ITH,IK,IS(J))
              END DO
            END DO
          END DO
#endif
!
! Extract spectra, distributed memory version(s)
!
#ifdef W3_MPI
        IOFF   = 1 + 4*(I-1)
        CALL MPI_STARTALL ( 4, IRQPO2(IOFF), IERR_MPI )
        CALL MPI_WAITALL  ( 4, IRQPO2(IOFF), STAT, IERR_MPI )
#endif
!
! Interpolate spectrum
!
        DO IK=1, NK
          FAC1(IK) = TPI * SIG(IK) / CG(IK,IS(1))
          FAC2(IK) = TPI * SIG(IK) / CG(IK,IS(2))
          FAC3(IK) = TPI * SIG(IK) / CG(IK,IS(3))
          FAC4(IK) = TPI * SIG(IK) / CG(IK,IS(4))
          END DO
!
        DO IK=1,NK
          DO ITH=1,NTH
            ISP    = ITH + (IK-1)*NTH
            SPCO(ISP,I) = RD(1) * SP(ITH,IK,1) * FAC1(IK)             &
                        + RD(2) * SP(ITH,IK,2) * FAC2(IK)             &
                        + RD(3) * SP(ITH,IK,3) * FAC3(IK)             &
                        + RD(4) * SP(ITH,IK,4) * FAC4(IK)
#ifdef W3_T
            SPTEST(IK,ITH) = SPCO(ISP,I)
#endif
            END DO
          END DO
!
#ifdef W3_RTD
        !!  Rotate the interpolated spectrum by -AnglD(IS(1)).  JGLi12Jun2012
        !!  SPCO still holds action not energy spectrum yet.  JGLi18Jun2013
        !!  Use new index IROT rather than IS(1) as in some cases
        !!  IS(1) will be a coast point and have an index of 0. C.Bunney 15/02/2011
        IF ( FLAGUNR ) THEN
          Spectr = SPCO(:,I)
          AnglDIS = -AnglD(IROT)
          CALL  W3ACTURN( NTH, NK, AnglDIS, Spectr )
          SPCO(:,I) = Spectr
        END IF

#endif
!
#ifdef W3_T
        WRITE (NDST,9004) DPO(I), WAO(I), WDO(I)*RADE,             &
                                  CAO(I), CDO(I)*RADE
#endif

! FA COMMENTED OUT: BUG
!At line 1974 of file w3arrymd.f90
!Fortran runtime error: Index '52' of dimension 1 of array 'pnum2' above upper bound of 51
#ifdef W3_T
       ! CALL PRT2DS (NDST, NK, NK, NTH, SPTEST, SIG(1:), ' ', 1.,0.,&
       !              0.0001, 'E(f,theta)', 'm**2s', 'TEST OUTPUT' )
#endif
!
        END DO
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT (' TEST W3IOPE : POINT NR.:',I3)
 9001 FORMAT (' TEST W3IOPE :',2I8,'   (',I3,')')
 9002 FORMAT (' TEST W3IOPE :',4I7,2X,4I2,2X,4F5.2)
 9003 FORMAT (' TEST W3IOPE :',40X,4F5.2)
 9004 FORMAT (' TEST W3IOPE :',F8.1,2(F7.2,F7.1))
#endif
!/
!/ End of W3IOPE ----------------------------------------------------- /
!/
      END SUBROUTINE W3IOPE
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3IOPO ( INXOUT, NDSOP, IOTST, IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         25-Jul-2006 |
!/                  +-----------------------------------+
!/
!/    07-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    30-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    10-Nov-2004 : Multiple grid version.              ( version 3.06 )
!/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    25-Jul-2006 : Adding grid ID per point.           ( version 3.10 )
!/    27-Aug-2015 : Adding interpolation for the ice.   ( version 5.10 )
!/    19-Jul-2021 : Momentum and air density support    ( version 7.xx )
!/
!  1. Purpose :
!
!     Read/write point output.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       INXOUT  C*(*)  I   Test string for read/write, valid are:
!                          'READ' and 'WRITE'.
!       NDSOP   Int.   I   File unit number.
!       IOTST   Int.   O   Test indictor for reading.
!                           0 : Data read.
!                          -1 : Past end of file.
!       IMOD    I(O)   I   Model number for W3GDAT etc.
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
!      WW3_OUTP  Prog.   N/A    Postprocessing for point output.
!      GX_OUTP   Prog.   N/A    Grads postprocessing for point output.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       Tests on INXOUT, file status and on array dimensions.
!
!  7. Remarks :
!
!     - The output file has the pre-defined name 'out_pnt.FILEXT'.
!     - In MPP version of model data is supposed to be gatherd at the
!       correct processor before the routine is called.
!     - No error output filtering needed.
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
!     !/S     Enable subroutine tracing.
!     !/T     Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: W3SETG
      USE W3WDATMD, ONLY: W3SETW
      USE W3ODATMD, ONLY: W3SETO, W3DMO2
!/
      USE W3GDATMD, ONLY: NTH, NK, NSPEC, FILEXT
      USE W3WDATMD, ONLY: TIME
      USE W3ODATMD, ONLY: NDST, NDSE, IPASS => IPASS2, NOPTS, IPTINT, &
                          IL, IW, II, PTLOC, PTIFAC, DPO, WAO, WDO,   &
                          ASO, CAO, CDO, SPCO, PTNME, O2INIT, FNMPRE, &
                          GRDID, ICEO, ICEHO, ICEFO
#ifdef W3_FLX5
      USE W3ODATMD, ONLY: TAUAO, TAUDO, DAIRO
#endif
      USE W3ODATMD, ONLY :  OFILES
!/
#ifdef W3_SETUP
      USE W3ODATMD, ONLY: ZET_SETO
#endif
!/
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)           :: NDSOP
      INTEGER, INTENT(OUT)          :: IOTST
      INTEGER, INTENT(IN), OPTIONAL :: IMOD
      CHARACTER, INTENT(IN)         :: INXOUT*(*)
!/
!/ ------------------------------------------------------------------- /
!/ local parameters
!/
      INTEGER                 :: IGRD, IERR, MK, MTH, I, J
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      LOGICAL,SAVE            :: WRITE
      CHARACTER(LEN=31)       :: IDTST
      CHARACTER(LEN=10)       :: VERTST
!/
      CHARACTER(LEN=15) :: TIMETAG
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3IOPO')
#endif
      IPASS  = IPASS + 1
      IOTST  = 0
!
! test input parameters ---------------------------------------------- *
!
      IF ( PRESENT(IMOD) ) THEN
          IGRD   = IMOD
        ELSE
          IGRD   = 1
        END IF
!
      CALL W3SETO ( IGRD, NDSE, NDST )
      CALL W3SETG ( IGRD, NDSE, NDST )
      CALL W3SETW ( IGRD, NDSE, NDST )
!
      IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE' ) THEN
          WRITE (NDSE,900) INXOUT
          CALL EXTCDE ( 1 )
        END IF
!
!      IF ( IPASS.EQ.1 ) THEN
      IF ( IPASS.EQ.1  .AND. OFILES(2) .EQ. 0) THEN
          WRITE  = INXOUT.EQ.'WRITE'
        ELSE
          IF ( WRITE .AND. INXOUT.EQ.'READ' ) THEN
              WRITE (NDSE,901) INXOUT
              CALL EXTCDE ( 2 )
            END IF
        END IF
!
! open file ---------------------------------------------------------- *
!
      IF ( IPASS.EQ.1 .AND. OFILES(2) .EQ. 0 ) THEN
!
          I      = LEN_TRIM(FILEXT)
          J      = LEN_TRIM(FNMPRE)
!
#ifdef W3_T
          WRITE (NDST,9001) FNMPRE(:J)//'out_pnt.'//FILEXT(:I)
#endif
          IF ( WRITE ) THEN
              OPEN (NDSOP,FILE=FNMPRE(:J)//'out_pnt.'//FILEXT(:I),    &
                    FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
            ELSE
              OPEN (NDSOP,FILE=FNMPRE(:J)//'out_pnt.'//FILEXT(:I),    &
                    FORM='UNFORMATTED',ERR=800,IOSTAT=IERR,STATUS='OLD')
            END IF
!
          REWIND ( NDSOP )
!
! test info ---------------------------------------------------------- *
! ( IPASS = 1 )
!
          IF ( WRITE ) THEN
              WRITE (NDSOP)                                           &
                IDSTR, VEROPT, NK, NTH, NOPTS
            ELSE
              READ (NDSOP,END=801,ERR=802,IOSTAT=IERR)                &
                IDTST, VERTST, MK, MTH, NOPTS
!
              IF ( IDTST .NE. IDSTR ) THEN
                  WRITE (NDSE,902) IDTST, IDSTR
                  CALL EXTCDE ( 10 )
                END IF
              IF ( VERTST .NE. VEROPT ) THEN
                  WRITE (NDSE,903) VERTST, VEROPT
                  CALL EXTCDE ( 11 )
                END IF
              IF (NK.NE.MK .OR. NTH.NE.MTH) THEN
                  WRITE (NDSE,904) MK, MTH, NK, NTH
                  CALL EXTCDE ( 12 )
                END IF
              IF ( .NOT. O2INIT )                                     &
                  CALL W3DMO2 ( IGRD, NDSE, NDST, NOPTS )
            END IF
!
#ifdef W3_T
          WRITE (NDST,9002) IDSTR, VEROPT, NK, NTH, NOPTS
#endif
!
! Point specific info ------------------------------------------------ *
! ( IPASS = 1 )
!
          IF ( WRITE ) THEN
              WRITE (NDSOP)                                           &
                    ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)
           ELSE
              READ  (NDSOP,END=801,ERR=802,IOSTAT=IERR)               &
                    ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)
            END IF
!
#ifdef W3_T
          WRITE (NDST,9003)
          DO I=1, NOPTS
            WRITE (NDST,9004) I, PTLOC(1,I), PTLOC(2,I), PTNME(I)
            END DO
#endif
!
        END IF
!
!
      IF ( IPASS.GE. 1  .AND. OFILES(2) .EQ. 1) THEN
          WRITE  = INXOUT.EQ.'WRITE'
        ELSE
          IF ( WRITE .AND. INXOUT.EQ.'READ' ) THEN
              WRITE (NDSE,901) INXOUT
              CALL EXTCDE ( 2 )
            END IF
        END IF

! open file ---------------------------------------------------------- *
!
      IF ( IPASS.GE.1 .AND. OFILES(2) .EQ. 1) THEN
!
          I      = LEN_TRIM(FILEXT)
          J      = LEN_TRIM(FNMPRE)

! Create TIMETAG for file name using YYYYMMDD.HHMMS prefix
          WRITE(TIMETAG,"(i8.8,'.'i6.6)")TIME(1),TIME(2)
!
#ifdef W3_T
          WRITE (NDST,9001) FNMPRE(:J)//TIMETAG//'.out_pnt.'// &
                               FILEXT(:I)
#endif
          IF ( WRITE ) THEN
              OPEN (NDSOP,FILE=FNMPRE(:J)//TIMETAG//'.out_pnt.'   &
                    //FILEXT(:I),FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
            END IF
!
          REWIND ( NDSOP )
!
!
! test info ---------------------------------------------------------- *
! ( IPASS GE.1 .AND. OFILES(2) .EQ. 1)
!
          IF ( WRITE ) THEN
              WRITE (NDSOP)                                           &
                IDSTR, VEROPT, NK, NTH, NOPTS
            ELSE
              READ (NDSOP,END=801,ERR=802,IOSTAT=IERR)                &
                IDTST, VERTST, MK, MTH, NOPTS
!
              IF ( IDTST .NE. IDSTR ) THEN
                  WRITE (NDSE,902) IDTST, IDSTR
                  CALL EXTCDE ( 10 )
                END IF
              IF ( VERTST .NE. VEROPT ) THEN
                  WRITE (NDSE,903) VERTST, VEROPT
                  CALL EXTCDE ( 11 )
                END IF
              IF (NK.NE.MK .OR. NTH.NE.MTH) THEN
                  WRITE (NDSE,904) MK, MTH, NK, NTH
                  CALL EXTCDE ( 12 )
                END IF
              IF ( .NOT. O2INIT )                                     &
                  CALL W3DMO2 ( IGRD, NDSE, NDST, NOPTS )
            END IF
!
#ifdef W3_T
          WRITE (NDST,9002) IDSTR, VEROPT, NK, NTH, NOPTS
#endif
!
! Point specific info ------------------------------------------------ *
! ( IPASS GE.1 .AND. OFILES(2) .EQ. 1)
!
          IF ( WRITE ) THEN
              WRITE (NDSOP)                                           &
                    ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)
           ELSE
              READ  (NDSOP,END=801,ERR=802,IOSTAT=IERR)               &
                    ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)
            END IF
!
#ifdef W3_T
          WRITE (NDST,9003)
          DO I=1, NOPTS
            WRITE (NDST,9004) I, PTLOC(1,I), PTLOC(2,I), PTNME(I)
            END DO
#endif
!
        END IF
!
!
! TIME --------------------------------------------------------------- *
!
      IF ( WRITE ) THEN
          WRITE (NDSOP)                            TIME
       ELSE
          READ (NDSOP,END=803,ERR=802,IOSTAT=IERR) TIME
        END IF
!
#ifdef W3_T
      WRITE (NDST,9010) TIME
#endif
!
!
! Loop over spectra -------------------------------------------------- *
!
      DO I=1, NOPTS
!
        IF ( WRITE ) THEN
             ! set IW, II and IL to 0 because it is not used and gives &
             ! outlier values in out_pnt.points
             IW(I) = 0
             II(I) = 0
             IL(I) = 0
             WRITE (NDSOP)                                            &
                    IW(I), II(I), IL(I), DPO(I), WAO(I), WDO(I),      &
#ifdef W3_FLX5
                    TAUAO(I), TAUDO(I), DAIRO(I),                     &
#endif
#ifdef W3_SETUP
             ZET_SETO(I),                                      &
#endif
                    ASO(I), CAO(I), CDO(I), ICEO(I), ICEHO(I),        &
                    ICEFO(I), GRDID(I), (SPCO(J,I),J=1,NSPEC)
          ELSE
             READ (NDSOP,END=801,ERR=802,IOSTAT=IERR)                 &
                    IW(I), II(I), IL(I), DPO(I), WAO(I), WDO(I),      &
#ifdef W3_FLX5
                    TAUAO(I), TAUDO(I), DAIRO(I),                     &
#endif
#ifdef W3_SETUP
             ZET_SETO(I),                                      &
#endif
                    ASO(I), CAO(I), CDO(I), ICEO(I), ICEHO(I),        &
                    ICEFO(I), GRDID(I), (SPCO(J,I),J=1,NSPEC)
          END IF
!
        END DO
      IF (OFILES(2) .EQ. 1)  CLOSE (NDSOP)
!
      RETURN
!
! Escape locations read errors
!
  800 CONTINUE
      WRITE (NDSE,1000) IERR
      CALL EXTCDE ( 20 )
!
  801 CONTINUE
      WRITE (NDSE,1001)
      CALL EXTCDE ( 21 )
!
  802 CONTINUE
      WRITE (NDSE,1002) IERR
      CALL EXTCDE ( 22 )
!
  803 CONTINUE
      IOTST  = -1
#ifdef W3_T
      WRITE (NDST,9011)
#endif
      RETURN
!
! Formats
!
  900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
               '     ILEGAL INXOUT VALUE: ',A/)
  901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
               '     MIXED READ/WRITE, LAST REQUEST: ',A/)
  902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
               '     ILEGAL IDSTR, READ : ',A/                        &
               '                  CHECK : ',A/)
  903 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
               '     ILEGAL VEROPT, READ : ',A/                       &
               '                   CHECK : ',A/)
  904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO :'/                &
               '     ERROR IN SPECTRA, MK, MTH : ',2I8/               &
               '              ARRAY DIMENSIONS : ',2I8/)
!
 1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO : '/               &
               '     ERROR IN OPENING FILE'/                          &
               '     IOSTAT =',I5/)
 1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO : '/               &
               '     PREMATURE END OF FILE'/)
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOPO : '/               &
               '     ERROR IN READING FROM FILE'/                     &
               '     IOSTAT =',I5/)
!
#ifdef W3_T
 9000 FORMAT (' TEST W3IOPO : IPASS =',I4,'    INXOUT = ',A,       &
              ' WRITE = ',L1,' UNIT =',I3/                         &
              '               IGRD =',I3,' FEXT = ',A)
#endif

#ifdef W3_T
 9001 FORMAT (' TEST W3IOPO : OPENING NEW FILE [',A,']')
 9002 FORMAT (' TEST W3IOPO : TEST PARAMETERS:'/                   &
              '       IDSTR : ',A/                                 &
              '      VEROPT : ',A/                                 &
              '      NK,NTH :',I5,I8/                              &
              '        NOPT :',I5)
 9003 FORMAT (' TEST W3IOPO : POINT LOCATION AND ID')
 9004 FORMAT (3X,I4,2F10.2,2X,A)
#endif
!
#ifdef W3_T
 9010 FORMAT (' TEST W3IOPO : TIME  :',I9.8,I7.6)
 9011 FORMAT (' TEST W3IOPO : END OF FILE REACHED')
#endif
!
#ifdef W3_T
 9020 FORMAT (' TEST W3IOPO : POINT NR.:',I5)
 9021 FORMAT (' TEST W3IOPO :',2I4,2F6.3)
 9022 FORMAT (' TEST W3IOPO :',4I7,2X,4I2,2X,4F5.2)
 9030 FORMAT (' TEST W3IOPO :',F8.1,2(F7.2,F7.1))
#endif
!/
!/ End of W3IOPO ----------------------------------------------------- /
!/
      END SUBROUTINE W3IOPO
!/
!/ End of module W3IOPOMD -------------------------------------------- /
!/
      END MODULE W3IOPOMD
