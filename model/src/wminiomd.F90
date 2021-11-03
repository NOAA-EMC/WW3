#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE WMINIOMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         28-Sep-2016 |
!/                  +-----------------------------------+
!/
!/    29-May-2006 : Origination.                        ( version 3.09 )
!/    21-Dec-2006 : VTIME change in WMIOHx and WMIOEx.  ( version 3.10 )
!/    22-Jan-2007 : Adding NAVMAX in WMIOEG.            ( version 3.10 )
!/    30-Jan-2007 : Fix memory leak WMIOBS.             ( version 3.10 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    28-Sep-2016 : Add error traps for MPI tags.       ( version 5.15 )
!/    16-Dec-2020 : Modify WMIOES/G for SMC grid.  JGLi ( version 7.13 )
!/
!/    Copyright 2009 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Internal IO routines for the multi-grid model.
!
!  2. Variables and types :
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      WMIOBS    Subr. Public   Stage internal boundary data.
!      WMIOBG    Subr. Public   Gather internal boundary data.
!      WMIOBF    Subr. Public   Finalize WMIOBS.            ( !/MPI )
!      WMIOHS    Subr. Public   Stage internal high to low data.
!      WMIOHG    Subr. Public   Gather internal high to low data.
!      WMIOHF    Subr. Public   Finalize WMIOHS.            ( !/MPI )
!      WMIOES    Subr. Public   Stage internal same rank data.
!      WMIOEG    Subr. Public   Gather internal same rank data.
!      WMIOEF    Subr. Public   Finalize WMIOES.            ( !/MPI )
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETG, W3SETW, W3SETA, W3SETO, WMSETM
!                Subr. WxxDATMD Manage data structures.
!      W3UBPT    Subr. W3UBPTMD Update internal bounday spectra.
!      W3IOBC    Subr  W3IOBCMD I/O of boundary data.
!      W3CSPC    Subr. W3CSPCMD Spectral grid conversion.
!      STRACE    Sur.  W3SERVMD Subroutine tracing.
!
!      MPI_ISEND, MPI_IRECV, MPI_TESTALL, MPI_WAITALL
!                Subr.  mpif.h  MPI routines.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!       !/SHRD   Shared/distributed memory models.
!       !/DIST
!       !/MPI
!
!       !/S      Enable subroutine tracing.
!       !/T      Enable test output
!       !/MPIT
!
!  6. Switches :
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      PUBLIC
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE WMIOBS ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Jun-2018 !
!/                  +-----------------------------------+
!/
!/    06-Oct-2005 : Origination.                        ( version 3.08 )
!/    29-May-2006 : Adding buffering for MPI.           ( version 3.09 )
!/    30-Jan-2007 : Fix memory leak.                    ( version 3.10 )
!/    28-Sep-2016 : Add error traps for MPI tags.       ( version 5.15 )
!/    06-Jun-2018 : Use W3PARALL/add DEBUGIOBC/PDLIB    ( version 6.04 )
!/
!  1. Purpose :
!
!     Stage internal boundary data in the data structure BPSTGE.
!
!  2. Method :
!
!     For the shared memory version, arrays are initialized and the
!     data are copied. For the distributed memory version, the data
!     are moved using a non-blocking send. in this case, the arrays
!     are dimensioned on the recieving side.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number of grid from which data is to
!                          be staged.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETG, W3SETW, W3SETA, W3SETO, WMSETM
!                Subr. WxxDATMD Manage data structures.
!      W3CSPC    Subr. W3CSPCMD Spectral grid conversion.
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      EXTCDE    Sur.    Id.    Program abort.
!
!      MPI_ISEND
!                Subr. mpif.h   MPI routines.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      WMINIT    Subr  WMINITMD Multi-grid model initialization.
!      WMWAVE    Subr  WMWAVEMD Multi-grid wave model.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     See FORMAT label 1001.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/SHRD   Shared/distributed memory models.
!       !/DIST
!       !/MPI
!
!       !/S      Enable subroutine tracing.
!       !/T      Enable test output
!       !/MPIT
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE W3GDATMD
      USE W3WDATMD
      USE W3ADATMD
      USE W3ODATMD
      USE WMMDATMD
!
      USE W3CSPCMD, ONLY: W3CSPC
      USE W3SERVMD, ONLY: EXTCDE
      USE W3PARALL, ONLY: INIT_GET_JSEA_ISPROC
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
      INTEGER, INTENT(IN)     :: IMOD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: J, I, IOFF, ISEA, JSEA, IS
#ifdef W3_DIST
      INTEGER                 :: ISPROC
#endif
#ifdef W3_MPI
      INTEGER                 :: IP, IT0, ITAG, IERR_MPI
      INTEGER, POINTER        :: NRQ, IRQ(:)
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      REAL, POINTER           :: SBPI(:,:), TSTORE(:,:)
!/
#ifdef W3_S
      CALL STRACE (IENT, 'WMIOBS')
#endif
!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
#ifdef W3_T
      WRITE (MDST,9000) IMOD
      WRITE (MDST,9001) NBI2G(:,IMOD)
#endif
!
      IF ( SUM(NBI2G(:,IMOD)) .EQ. 0 ) RETURN
!
      CALL W3SETO ( IMOD, MDSE, MDST )
      CALL W3SETG ( IMOD, MDSE, MDST )
      CALL W3SETW ( IMOD, MDSE, MDST )
      CALL W3SETA ( IMOD, MDSE, MDST )
!
! -------------------------------------------------------------------- /
! 1.  Loop over grids
!
      DO J=1, NRGRD
!
        IF ( NBI2G(J,IMOD) .EQ. 0 ) CYCLE
!
        CALL WMSETM (   J , MDSE, MDST )
!
        IF ( IMOD .EQ. 1 ) THEN
            IOFF   = 0
          ELSE
            IOFF   = SUM(NBI2G(J,1:IMOD-1))
          END IF
!
#ifdef W3_T
        WRITE (MDST,9010) NBI2G(J,IMOD),IMOD,J,IOFF+1,RESPEC(J,IMOD)
#endif
!
! -------------------------------------------------------------------- /
! 2.  Allocate arrays
!
#ifdef W3_SHRD
        IF ( BPSTGE(J,IMOD)%INIT ) THEN
            IF ( SIZE(BPSTGE(J,IMOD)%SBPI(:,1)) .NE. NSPEC .OR. &
                 SIZE(BPSTGE(J,IMOD)%SBPI(1,:))                 &
                                   .NE. NBI2G(J,IMOD) ) THEN
                DEALLOCATE ( BPSTGE(J,IMOD)%SBPI )
                BPSTGE(J,IMOD)%INIT = .FALSE.
              END IF
          END IF
#endif
!
#ifdef W3_SHRD
        IF ( .NOT. BPSTGE(J,IMOD)%INIT ) THEN
            NSPEC  => SGRDS(J)%NSPEC
            ALLOCATE ( BPSTGE(J,IMOD)%SBPI(NSPEC,NBI2G(J,IMOD)) )
            NSPEC  => SGRDS(IMOD)%NSPEC
            BPSTGE(J,IMOD)%INIT  = .TRUE.
          END IF
#endif
!
#ifdef W3_SHRD
        IF ( RESPEC(J,IMOD) ) THEN
            ALLOCATE ( TSTORE(NSPEC,NBI2G(J,IMOD)) )
            SBPI   => TSTORE
          ELSE
            SBPI   => BPSTGE(J,IMOD)%SBPI
          END IF
#endif
!
#ifdef W3_MPI
        NAPROC => OUTPTS(J)%NAPROC
        ALLOCATE ( IRQ(NBI2G(J,IMOD)*NAPROC+NAPROC) )
        ALLOCATE ( BPSTGE(J,IMOD)%TSTORE(NSPEC,NBI2G(J,IMOD)) )
        NAPROC => OUTPTS(IMOD)%NAPROC
#endif
!
#ifdef W3_MPI
        NRQ    => BPSTGE(J,IMOD)%NRQBPS
        SBPI   => BPSTGE(J,IMOD)%TSTORE
#endif
!
#ifdef W3_MPI
        NRQ    = 0
        IRQ    = 0
#endif
!
! -------------------------------------------------------------------- /
! 3.  Set the time
!     Note that with MPI the send needs to be posted to the local
!     processor too to make time management possible.
!
#ifdef W3_T
        WRITE (MDST,9030) TIME
#endif
#ifdef W3_MPIT
        WRITE (MDST,9080)
#endif
!
#ifdef W3_SHRD
        BPSTGE(J,IMOD)%VTIME = TIME
#endif
!
#ifdef W3_MPI
        IF ( IAPROC .EQ. 1 ) THEN
            BPSTGE(J,IMOD)%STIME = TIME
            ITAG   = MTAG0 + IMOD + (J-1)*NRGRD
            IF ( ITAG .GT. MTAG1 ) THEN
                WRITE (MDSE,1001)
                CALL EXTCDE (1001) 
              END IF
            DO IP=1, NMPROC
              IF ( ALLPRC(IP,J) .NE. 0 .AND.                 &
                   ALLPRC(IP,J) .LE. OUTPTS(J)%NAPROC ) THEN
                  NRQ    = NRQ + 1
                  CALL MPI_ISEND ( BPSTGE(J,IMOD)%STIME, 2,  &
                                   MPI_INTEGER, IP-1, ITAG,  &
                                   MPI_COMM_MWAVE, IRQ(NRQ), &
                                   IERR_MPI )
#endif
#ifdef W3_MPIT
                  WRITE (MDST,9081) NRQ, IP, ITAG-MTAG0,     &
                                    IRQ(NRQ), IERR_MPI
#endif
#ifdef W3_MPI
                END IF
              END DO
          END IF
#endif
!
! -------------------------------------------------------------------- /
! 4.  Stage the spectral data
!
        DO I=1, NBI2G(J,IMOD)
!
          ISEA   = NBI2S(IOFF+I,2)
#ifdef W3_SHRD
          JSEA   = ISEA
#endif
#ifdef W3_DIST
          CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
          IF ( ISPROC .NE. IAPROC ) CYCLE
#endif
#ifdef W3_MPI
          IT0    = MTAG0 + NRGRD**2 + SUM(NBI2G(1:J-1,:)) +      &
                                      SUM(NBI2G(J,1:IMOD-1))
#endif
!
          DO IS=1, NSPEC
            SBPI(IS,I) = VA(IS,JSEA) * SIG2(IS) / CG(1+(IS-1)/NTH,ISEA)
            END DO
!
#ifdef W3_MPI
          DO IP=1, NMPROC
            IF ( ALLPRC(IP,J) .NE. 0 .AND.                   &    
                 ALLPRC(IP,J) .LE. OUTPTS(J)%NAPROC ) THEN
                NRQ    = NRQ + 1
                ITAG   = IT0 + I
                IF ( ITAG .GT. MTAG1 ) THEN
                    WRITE (MDSE,1001)
                    CALL EXTCDE (1001) 
                  END IF
                CALL MPI_ISEND ( SBPI(1,I), NSPEC, MPI_REAL, &
                                 IP-1, ITAG, MPI_COMM_MWAVE, &
                                 IRQ(NRQ), IERR_MPI )
#endif
#ifdef W3_MPIT
                WRITE (MDST,9082) NRQ, JSEA, IP, ITAG-MTAG0, &
                                  IRQ(NRQ), IERR_MPI
#endif
#ifdef W3_MPI
              END IF
            END DO
#endif
!
          END DO
!
#ifdef W3_MPIT
        WRITE (MDST,9083)
        WRITE (MDST,9084) NRQ
#endif
!
#ifdef W3_MPI
        IF ( NRQ .GT. 0 ) THEN
            ALLOCATE ( BPSTGE(J,IMOD)%IRQBPS(NRQ) )
            BPSTGE(J,IMOD)%IRQBPS = IRQ(:NRQ)
          ELSE
            DEALLOCATE ( BPSTGE(J,IMOD)%TSTORE )
          END IF
#endif
!
#ifdef W3_MPI
        DEALLOCATE ( IRQ )
#endif
!
! -------------------------------------------------------------------- /
! 5.  Convert spectra ( !/SHRD only )
!
#ifdef W3_SHRD
        IF ( RESPEC(J,IMOD) ) THEN
            SBPI   => BPSTGE(J,IMOD)%SBPI
            CALL W3CSPC ( TSTORE, NK, NTH, XFR, FR1, TH(1),     &
                 SBPI, SGRDS(J)%NK, SGRDS(J)%NTH, SGRDS(J)%XFR, &
                 SGRDS(J)%FR1, SGRDS(J)%TH(1), NBI2G(J,IMOD),   &
                 MDST, MDSE, SGRDS(J)%FACHFE )
            DEALLOCATE ( TSTORE )
          END IF
#endif
!
! ... End of loop over grids
!
        END DO
!
      RETURN
!
! Formats
!
#ifdef W3_MPI
 1001 FORMAT (/' *** ERROR WMIOBS : REQUESTED MPI TAG EXCEEDS', &
                                    ' UPPER BOUND (MTAG1) ***')
#endif
#ifdef W3_T
 9000 FORMAT ( ' TEST WMIOBS : STAGING DATA FROM GRID ',I3)
 9001 FORMAT ( ' TEST WMIOBS : NR. OF SPECTRA PER GRID : '/        &
               '             ',25I4)
#endif
!
#ifdef W3_T
 9010 FORMAT ( ' TEST WMIOBS : STAGING',I4,' SPECTRA FROM GRID ',  &
                 I3,' TO GRID ',I3/                                &
               '               STARTING WITH SPECTRUM ',I4,        &
               ', RESPEC =',L2)
#endif
!
#ifdef W3_T
 9030 FORMAT ( ' TEST WMIOBS : TIME :',I10.8,I7.6)
#endif
!
#ifdef W3_MPIT
 9080 FORMAT (/' MPIT WMIOBS: COMMUNICATION CALLS            '/ &
               ' +------+------+------+------+--------------+'/ &
               ' |  IH  |  ID  | TARG |  TAG |   handle err |'/ &
               ' +------+------+------+------+--------------+')
 9081 FORMAT ( ' |',I5,' | TIME |',2(I5,' |'),I9,I4,' |')
 9082 FORMAT ( ' |',I5,' |',I5,' |',2(I5,' |'),I9,I4,' |')
 9083 FORMAT ( ' +------+------+------+------+--------------+')
 9084 FORMAT ( ' MPIT WMIOBS: NRQBPT:',I10/)
#endif
!/
!/ End of WMIOBS ----------------------------------------------------- /
!/
      END SUBROUTINE WMIOBS
!/ ------------------------------------------------------------------- /
      SUBROUTINE WMIOBG ( IMOD, DONE ) 
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         29-May-2006 !
!/                  +-----------------------------------+
!/
!/    18-Oct-2005 : Origination.                        ( version 3.08 )
!/    29-May-2006 : Adding buffering for MPI.           ( version 3.09 )
!/
!  1. Purpose :
!
!     Gather internal boundary data for a given model.
!
!  2. Method :
!
!     For the shared memory version, datat are gathered from the data
!     structure BPSTGE. For the distributed memeory version, the
!     gathering of thee data are finished first.
!
!     Gathering of data is triggered by the time stamp of the data
!     that is presently in the storage arrays.
!
!     This routine preempts the data flow normally executed by
!     W3IOBC and W3UBPT, and hence bypasses both routines in W3WAVE.
!
!  2. Method :
!
!     Using storage array BPSTAGE and time stamps.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number of grid from which data is to
!                          be gathered.
!       DONE    Log.   O   Flag for completion of operation (opt).
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETG, W3SETW, W3SETA, W3SETO, WMSETM
!                Subr. WxxDATMD Manage data structures.
!      W3CSPC    Subr. W3CSPCMD Spectral grid conversion.
!      W3UBPT    Subr. W3UBPTMD Update internal bounday spectra.
!      W3IOBC    Subr  W3IOBCMD I/O of boundary data.
!      STRACE    Sur.  W3SERVMD Subroutine tracing.
!      EXTCDE    Sur.    Id.    Program abort.
!      DSEC21    Func. W3TIMEMD Difference between times.
!
!      MPI_IRECV, MPI_TESTALL, MPI_WAITALL
!                Subr.  mpif.h  MPI routines.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      WMINIT    Subr  WMINITMD Multi-grid model initialization.
!      WMWAVE    Subr  WMWAVEMD Multi-grid wave model.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     See FORMAT labels 1001-1002.
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
!       !/SHRD   Shared/distributed memory models.
!       !/DIST
!       !/MPI
!
!       !/S      Enable subroutine tracing.
!       !/T      Enable test output
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE W3GDATMD
      USE W3WDATMD
      USE W3ADATMD
      USE W3ODATMD
      USE WMMDATMD
!
      USE W3CSPCMD, ONLY: W3CSPC
      USE W3TIMEMD, ONLY: DSEC21
      USE W3UPDTMD, ONLY: W3UBPT
      USE W3IOBCMD, ONLY: W3IOBC
      USE W3SERVMD, ONLY: EXTCDE
!      USE W3PARALL, ONLY: INIT_GET_JSEA_ISPROC_GLOB
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
      INTEGER, INTENT(IN)            :: IMOD
      LOGICAL, INTENT(OUT), OPTIONAL :: DONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: J, I, IOFF, TTEST(2), ITEST
#ifdef W3_MPI
      INTEGER                 :: IERR_MPI, IT0, ITAG, IFROM,     &
                                 ISEA, JSEA, ISPROC
#endif
#ifdef W3_MPIT
      INTEGER                 :: ICOUNT
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      INTEGER, POINTER        :: VTIME(:)
#ifdef W3_MPI
      INTEGER, POINTER        :: NRQ, IRQ(:)
      INTEGER, ALLOCATABLE    :: STATUS(:,:)
#endif
      REAL                    :: DTTST, DT1, DT2, W1, W2
      REAL, POINTER           :: SBPI(:,:)
#ifdef W3_MPI
      REAL, ALLOCATABLE       :: TSTORE(:,:)
      LOGICAL                 :: FLAGOK
#endif
#ifdef W3_MPIT
      LOGICAL                 :: FLAG
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'WMIOBG')
#endif
#ifdef W3_DEBUGIOBC
      WRITE(740+IAPROC,*)  'Begin of W3IOBG'
      FLUSH(740+IAPROC)
#endif


!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
#ifdef W3_T
      WRITE (MDST,9000) IMOD
      WRITE (MDST,9001) NBI2G(IMOD,:)
#endif
!
      IF ( PRESENT(DONE) ) DONE = .FALSE.
!
      CALL W3SETO ( IMOD, MDSE, MDST )
!
      IF ( IAPROC .GT. NAPROC ) THEN
          IF ( PRESENT(DONE) ) DONE = .TRUE.
#ifdef W3_T
          WRITE (MDST,9002)
#endif
          RETURN
        END IF
!
      IF ( SUM(NBI2G(IMOD,:)) .EQ. 0 ) THEN
          IF ( PRESENT(DONE) ) DONE = .TRUE.
#ifdef W3_T
          WRITE (MDST,9003)
#endif
          RETURN
        END IF
!
      CALL W3SETG ( IMOD, MDSE, MDST )
      CALL W3SETW ( IMOD, MDSE, MDST )
      CALL W3SETA ( IMOD, MDSE, MDST )
!
      IF ( TBPIN(1) .NE. -1 ) THEN
          IF ( DSEC21(TIME,TBPIN) .GT. 0. ) THEN
              IF ( PRESENT(DONE) ) DONE = .TRUE.
#ifdef W3_T
              WRITE (MDST,9004)
#endif
              RETURN
            END IF
        END IF
!
! -------------------------------------------------------------------- /
! 1.  Testing / gathering data in staging arrays 
!
#ifdef W3_T
      WRITE (MDST,9010)
#endif
!
! 1.a Shared memory version, test valid times. - - - - - - - - - - - - /
!
#ifdef W3_SHRD
      DO J=1, NRGRD
#endif
!
#ifdef W3_SHRD
        IF ( NBI2G(IMOD,J) .EQ. 0 ) CYCLE
        VTIME  => BPSTGE(IMOD,J)%VTIME
#endif
!
#ifdef W3_SHRD
        IF ( VTIME(1) .EQ. -1 ) THEN
            IF ( NMPROC .EQ. NMPERR ) WRITE (MDSE,1001)
            CALL EXTCDE ( 1001 )
          END IF
#endif
!
#ifdef W3_SHRD
        DTTST  = DSEC21 ( TIME, VTIME )
        IF ( DTTST.LE.0. .AND. TBPIN(1).NE.-1 ) RETURN
#endif
!
#ifdef W3_SHRD
        END DO
#endif
!
! 1.b Distributed memory version - - - - - - - - - - - - - - - - - - - /
!
#ifdef W3_MPIT
        WRITE (MDST,9011) NBISTA(IMOD)
#endif
! 
! 1.b.1 NBISTA = 0
!       Check if staging arrays are initialized.
!       Post the proper receives.
!
#ifdef W3_MPI
      IF ( NBISTA(IMOD) .EQ. 0 ) THEN
#endif
!
#ifdef W3_MPI
          NRQ    => MDATAS(IMOD)%NRQBPG
          NRQ    = NRGRD + SUM(NBI2G(IMOD,:))
          ALLOCATE ( MDATAS(IMOD)%IRQBPG(NRQ) )
          IRQ    => MDATAS(IMOD)%IRQBPG
          IRQ    = 0
          NRQ    = 0
#endif
!
#ifdef W3_MPI
          DO J=1, NRGRD
            IF ( NBI2G(IMOD,J) .EQ. 0 ) CYCLE
#endif
!
! ..... Staging arrays
!
#ifdef W3_MPI
            IF ( BPSTGE(IMOD,J)%INIT ) THEN
                IF ( RESPEC(IMOD,J) ) THEN
                    DEALLOCATE ( BPSTGE(IMOD,J)%SBPI )
                    BPSTGE(IMOD,J)%INIT  = .FALSE.
#endif
#ifdef W3_MPIT
                    WRITE (MDST,9012) J, 'RESET'
#endif
#ifdef W3_MPI
                  ELSE
                    IF ( SIZE(BPSTGE(IMOD,J)%SBPI(:,1)) .NE.     &
                                             SGRDS(J)%NSPEC .OR. &
                         SIZE(BPSTGE(IMOD,J)%SBPI(1,:)) .NE.     &
                                             NBI2G(IMOD,J) ) THEN
                        IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1003)
                        CALL EXTCDE (1003) 
                      END IF
#endif
#ifdef W3_MPIT
                    WRITE (MDST,9012) J, 'TESTED'
#endif
#ifdef W3_MPI
                  END IF
              END IF
#endif
!
#ifdef W3_MPI
            IF ( .NOT. BPSTGE(IMOD,J)%INIT ) THEN
                NSPEC  => SGRDS(J)%NSPEC
                ALLOCATE (BPSTGE(IMOD,J)%SBPI(NSPEC,NBI2G(IMOD,J)))
                NSPEC  => SGRDS(IMOD)%NSPEC
                BPSTGE(IMOD,J)%INIT  = .TRUE.
#endif
#ifdef W3_MPIT
                WRITE (MDST,9012) J, 'INITIALIZED'
#endif
#ifdef W3_MPI
              END IF
#endif
!
! ..... Check valid time to determine staging.
!
#ifdef W3_MPI
            VTIME  => BPSTGE(IMOD,J)%VTIME
            IF ( VTIME(1) .EQ. -1 ) THEN
                DTTST  = 0.
              ELSE
                DTTST  = DSEC21 ( TIME, VTIME )
              END IF
#endif
#ifdef W3_MPIT
            WRITE (MDST,9013) VTIME, DTTST
#endif
!
! ..... Post receives for data gather
!
#ifdef W3_MPI
            IF ( DTTST .LE. 0. ) THEN
#endif
#ifdef W3_MPIT
                WRITE (MDST,9014) J
#endif
!
! ..... Time
!
#ifdef W3_MPI
                ITAG   = MTAG0 + J + (IMOD-1)*NRGRD
                IFROM  = MDATAS(J)%CROOT - 1
                NRQ    = NRQ + 1
                CALL MPI_IRECV ( BPSTGE(IMOD,J)%VTIME, 2,        &
                                 MPI_INTEGER, IFROM, ITAG,       &
                                 MPI_COMM_MWAVE, IRQ(NRQ),       &
                                 IERR_MPI )
#endif
#ifdef W3_MPIT
                WRITE (MDST,9015) NRQ, IFROM+1, ITAG-MTAG0,      &
                                      IRQ(NRQ), IERR_MPI
#endif
!
! ..... Spectra
!
#ifdef W3_MPI
                IF ( J .EQ. 1 ) THEN
                    IOFF   = 0
                  ELSE
                    IOFF   = SUM(NBI2G(IMOD,1:J-1))
                  END IF
#endif
!
#ifdef W3_MPI
                IT0 = MTAG0 + NRGRD**2 + SUM(NBI2G(1:IMOD-1,:))  &
                                       + SUM(NBI2G(IMOD,1:J-1))
#endif
!
#ifdef W3_MPI
                SBPI  => BPSTGE(IMOD,J)%SBPI
#endif
!
#ifdef W3_MPI
                NAPROC => OUTPTS(J)%NAPROC
                NSPEC  => SGRDS(J)%NSPEC
                DO I=1, NBI2G(IMOD,J)
                  ISEA   = NBI2S(IOFF+I,2)
                  CALL INIT_GET_JSEA_ISPROC_GLOB(ISEA, J, JSEA, ISPROC)
                  NRQ    = NRQ + 1
                  ITAG   = IT0 + I
                  CALL MPI_IRECV ( SBPI(1,I), NSPEC,             &
                                   MPI_REAL, ISPROC-1,           &
                                   ITAG, MPI_COMM_MWAVE,         &
                                   IRQ(NRQ), IERR_MPI )
#endif
#ifdef W3_MPIT
                WRITE (MDST,9016) NRQ, JSEA, ISPROC,             &
                       ITAG-MTAG0, IRQ(NRQ), IERR_MPI
#endif
#ifdef W3_MPI
                  END DO
                NSPEC  => SGRDS(IMOD)%NSPEC
                NAPROC => OUTPTS(IMOD)%NAPROC
#endif
!
! ..... End IF for posting receives 1.b.1
!
#ifdef W3_MPIT
                WRITE (MDST,9017)
#endif
#ifdef W3_MPI
              END IF
#endif
!
! ..... End grid loop J in 1.b.1
!
#ifdef W3_MPI
            END DO
#endif
#ifdef W3_MPIT
          WRITE (MDST,9018) NRQ
#endif
!
! ..... Reset status
!       NOTE: if NBI.EQ.0 all times are already OK, skip to section 2
!
#ifdef W3_MPI
          IF ( NBI .GT. 0 ) THEN
              NBISTA(IMOD) = 1
#endif
#ifdef W3_MPIT
              WRITE (MDST,9011) NBISTA(IMOD)
#endif
#ifdef W3_MPI
            END IF
#endif
!
! ..... End IF in 1.b.1
!
#ifdef W3_MPI
        END IF
#endif
! 
! 1.b.2 NBISTA = 1
!       Wait for communication to finish.
!       If DONE defined, check if done, otherwise wait.
!
#ifdef W3_MPI
      IF ( NBISTA(IMOD) .EQ. 1 ) THEN
#endif
!
#ifdef W3_MPI
          NRQ    => MDATAS(IMOD)%NRQBPG
          IRQ    => MDATAS(IMOD)%IRQBPG
          ALLOCATE ( STATUS(MPI_STATUS_SIZE,NRQ) )
#endif
!
! ..... Test communication if DONE is present, wait otherwise
!
#ifdef W3_MPI
          IF ( PRESENT(DONE) ) THEN
#endif
!
#ifdef W3_MPI
              CALL MPI_TESTALL ( NRQ, IRQ, FLAGOK, STATUS,       &
                                 IERR_MPI )
#endif
!
#ifdef W3_MPIT
              ICOUNT = 0
              DO I=1, NRQ
                CALL MPI_TEST ( IRQ(I), FLAG, STATUS(1,1),      &
                                IERR_MPI )
                FLAGOK = FLAGOK .AND. FLAG
                IF ( FLAG ) ICOUNT = ICOUNT + 1
                END DO
              WRITE (MDST,9019) 100. * REAL(ICOUNT) / REAL(NRQ)
#endif
!
#ifdef W3_MPI
            ELSE
#endif
!
#ifdef W3_MPI
              CALL MPI_WAITALL ( NRQ, IRQ, STATUS, IERR_MPI )
              FLAGOK = .TRUE.
#endif
!
#ifdef W3_MPI
            END IF
#endif
!
#ifdef W3_MPI
              DEALLOCATE ( STATUS )
#endif
!
! ..... Go on based on FLAGOK
!
#ifdef W3_MPI
          IF ( FLAGOK ) THEN
              DEALLOCATE ( MDATAS(IMOD)%IRQBPG )
              NRQ    = 0
            ELSE
              RETURN
            END IF
#endif
!
#ifdef W3_MPI
          NBISTA(IMOD) = 2
#endif
#ifdef W3_MPIT
          WRITE (MDST,9011) NBISTA(IMOD)
#endif
! 
! 1.b.3 Convert spectra if needed
!
#ifdef W3_MPI
          DO J=1, NRGRD
#endif
!
#ifdef W3_MPI
            IF ( RESPEC(IMOD,J) .AND. NBI2G(IMOD,J).NE.0 ) THEN
#endif
!
#ifdef W3_MPIT
                WRITE (MDST,9100) J
#endif
#ifdef W3_MPI
                NSPEC  => SGRDS(J)%NSPEC
                ALLOCATE ( TSTORE(NSPEC,NBI2G(IMOD,J)))
                NSPEC  => SGRDS(IMOD)%NSPEC
                TSTORE = BPSTGE(IMOD,J)%SBPI
                DEALLOCATE ( BPSTGE(IMOD,J)%SBPI )
                ALLOCATE (BPSTGE(IMOD,J)%SBPI(NSPEC,NBI2G(IMOD,J)))
#endif
!
#ifdef W3_MPI
                SBPI   => BPSTGE(IMOD,J)%SBPI
                CALL W3CSPC ( TSTORE, SGRDS(J)%NK, SGRDS(J)%NTH, &
                     SGRDS(J)%XFR, SGRDS(J)%FR1, SGRDS(J)%TH(1), &
                     SBPI, NK, NTH, XFR, FR1, TH(1),             &
                     NBI2G(IMOD,J), MDST, MDSE, SGRDS(IMOD)%FACHFE)
#endif
!
#ifdef W3_MPI
                DEALLOCATE ( TSTORE )
#endif
!
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
            END DO
#endif
!
#ifdef W3_MPI
          NBISTA(IMOD) = 0
#endif
#ifdef W3_MPIT
          WRITE (MDST,9011) NBISTA(IMOD)
#endif
!
#ifdef W3_MPI
        END IF
#endif
!
! -------------------------------------------------------------------- /
! 2.  Update arrays ABPI0/N and data times
!
#ifdef W3_T
      WRITE (MDST,9020)
#endif
!
! 2.a Determine next valid time
!
      TTEST  = -1
      DO J=1, NRGRD
        IF ( NBI2G(IMOD,J) .EQ. 0 ) CYCLE
        VTIME  => BPSTGE(IMOD,J)%VTIME
        IF ( TTEST(1) .EQ. -1 ) THEN
            TTEST  = VTIME
          ELSE
            DTTST  = DSEC21(VTIME,TTEST)
            IF ( DTTST .GT. 0. ) TTEST  = VTIME
          END IF
        END DO
!
#ifdef W3_T
      WRITE (MDST,9021) TTEST
#endif
!
! 2.b Shift data
!
      IF ( TBPIN(1) .EQ. -1 ) THEN
          DTTST  = DSEC21(TTEST,TIME)
          IF ( DTTST .NE. 0. ) THEN
              IF ( NMPROC .EQ. NMPERR ) WRITE (MDSE,1002)
              CALL EXTCDE(1002)
            END IF
          ABPI0  = 0.
        ELSE
          TBPI0  = TBPIN
          ABPI0  = ABPIN
        END IF
!
! 2.c Loop over grids for new spectra
!
      DO J=1, NRGRD
!
        IF ( NBI2G(IMOD,J) .EQ. 0 ) CYCLE
        VTIME  => BPSTGE(IMOD,J)%VTIME
        SBPI   => BPSTGE(IMOD,J)%SBPI
!
        IF ( J .EQ. 1 ) THEN
            IOFF   = 0
          ELSE
            IOFF   = SUM(NBI2G(IMOD,1:J-1))
          END IF
!
        IF ( TBPIN(1) .EQ. -1 ) THEN
            W1     = 0.
            W2     = 1.
          ELSE
            DT1    = DSEC21(TBPI0,VTIME)
            DT2    = DSEC21(TBPI0,TTEST)
            W2     = DT2 / DT1
            W1     = 1. - W2
          END IF
#ifdef W3_T
        WRITE (MDST,9022) NBI2G(IMOD,J), J, IOFF+1, W1, W2
#endif
!
        ABPIN(:,IOFF+1:IOFF+NBI2G(IMOD,J)) =                          &
                    W1 * ABPI0(:,IOFF+1:IOFF+NBI2G(IMOD,J)) +         &
                    W2 * SBPI(:,1:NBI2G(IMOD,J))
!
        END DO
!
! 2.d New time
!
      TBPIN  = TTEST
!
! -------------------------------------------------------------------- /
! 3.  Dump data to file if requested
!
      IF ( IAPROC.EQ.NAPBPT .AND. BCDUMP(IMOD) ) THEN
#ifdef W3_T
          WRITE (MDST,9030)
#endif
          CALL W3IOBC ( 'DUMP', NDS(9), TBPIN, TBPIN, ITEST, IMOD )
        END IF
!
! -------------------------------------------------------------------- /
! 4.  Update arrays BBPI0/N
!
#ifdef W3_T
      WRITE (MDST,9040)
#endif
!
      CALL W3UBPT
!
! -------------------------------------------------------------------- /
! 5.  Successful update
!
      IF ( PRESENT(DONE) ) DONE = .TRUE.
#ifdef W3_DEBUGIOBC
      WRITE(740+IAPROC,*)  'End of W3IOBG'
      FLUSH(740+IAPROC)
#endif
!
      RETURN
!
! Formats
!
#ifdef W3_SHRD
 1001 FORMAT (/' *** ERROR WMIOBG : NO DATA IN STAGING ARRAY ***'/    &
               '                    CALL WMIOBS FIRST '/)
#endif
 1002 FORMAT (/' *** ERROR WMIOBG : INITIAL DATA NOT AT INITAL ',     &
                                   'MODEL TIME ***'/)
#ifdef W3_MPI
 1003 FORMAT (/' *** ERROR WMIOBG : UNEXPECTED SIZE OF STAGING', &
                                   ' ARRAY ***')
#endif
!
#ifdef W3_T
 9000 FORMAT ( ' TEST WMIOBG : GATHERING DATA FOR GRID ',I3)
 9001 FORMAT ( ' TEST WMIOBG : NR. OF SPECTRA PER SOURCE GRID : '/ &
               '             ',25I4)
 9002 FORMAT ( ' TEST WMIOBG : NO DATA NEEDED ON PROCESSOR')
 9003 FORMAT ( ' TEST WMIOBG : NO DATA TO BE GATHERED')
 9004 FORMAT ( ' TEST WMIOBG : DATA UP TO DATE')
#endif
!
#ifdef W3_T
 9010 FORMAT ( ' TEST WMIOBG : TEST DATA AVAILABILITY')
#endif
#ifdef W3_MPIT
 9011 FORMAT ( ' MPIT WMIOBG : NBISTA =',I2)
 9012 FORMAT ( '               STAGING ARRAY FROM',I4,1X,A)
 9013 FORMAT ( '               VTIME, DTTST :',I9.8,I7.6,1X,F8.1)
 9014 FORMAT (/' MPIT WMIOBG : RECEIVE FROM GRID',I4/           &
               ' +------+------+------+------+--------------+'/ &
               ' |  IH  |  ID  | FROM |  TAG |   handle err |'/ &
               ' +------+------+------+------+--------------+')
 9015 FORMAT ( ' |',I5,' | TIME |',2(I5,' |'),I9,I4,' |')
 9016 FORMAT ( ' |',I5,' |',I5,' |',2(I5,' |'),I9,I4,' |')
 9017 FORMAT ( ' +------+------+------+------+--------------+'/)
 9018 FORMAT ( ' MPIT WMIOBG : NRQHGH:',I10/)
 9019 FORMAT ( ' MPIT WMIOBG : RECEIVES FINISHED :',F6.1,'%')
 9100 FORMAT ( ' MPIT WMIOBG : CONVERTING SPECTRA FROM GRID',I3)
#endif
!
#ifdef W3_T
 9020 FORMAT ( ' TEST WMIOBG : FILLING ABPI0/N AND TIMES')
 9021 FORMAT ( ' TEST WMIOBG : NEXT VALID TIME FOR ABPIN:',I9.8,I7.6)
 9022 FORMAT ( ' TEST WMIOBG : GETTING',I4,' SPECTRA FROM GRID ',  &
                               I3,' STORING AT ',I3/               &
               '               WEIGHTS : ',2F6.3)
#endif
!
#ifdef W3_T
 9030 FORMAT ( ' TEST WMIOBG : DUMP DATA TO FILE')
#endif
!
#ifdef W3_T
 9040 FORMAT ( ' TEST WMIOBG : FILLING BBPI0/N')
#endif
!/
!/ End of WMIOBG ----------------------------------------------------- /
!/
      END SUBROUTINE WMIOBG
!/ ------------------------------------------------------------------- /
      SUBROUTINE WMIOBF ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         29-May-2006 !
!/                  +-----------------------------------+
!/
!/    18-Oct-2005 : Origination.                        ( version 3.08 )
!/    29-May-2006 : Adding buffering for MPI.           ( version 3.09 )
!/
!  1. Purpose :
!
!     Finalize staging of  internal boundary data in the data
!     structure BPSTGE (MPI only).
!
!  2. Method :
!
!     Post appropriate 'wait' functions to assure that the
!     communication has finished.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number of grid from which data has
!                          been staged.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!
!      MPI_WAITALL
!                Subr. mpif.h   MPI routines.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      WMINIT    Subr  WMINITMD Multi-grid model initialization.
!      WMWAVE    Subr  WMWAVEMD Multi-grid wave model.
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
!       !/SHRD   Shared/distributed memory models.
!       !/DIST
!       !/MPI
!
!       !/S      Enable subroutine tracing.
!       !/T      Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE WMMDATMD
!
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
      INTEGER, INTENT(IN)     :: IMOD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: J
#ifdef W3_MPI
      INTEGER                 :: IERR_MPI
      INTEGER, POINTER        :: NRQ, IRQ(:)
      INTEGER, ALLOCATABLE    :: STATUS(:,:)
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'WMIOBF')
#endif
!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
#ifdef W3_T
      WRITE (MDST,9000) IMOD
#endif
!
! -------------------------------------------------------------------- /
! 1.  Loop over grids
!
      DO J=1, NRGRD
!
#ifdef W3_MPI
        NRQ    => BPSTGE(J,IMOD)%NRQBPS
#endif
!
! 1.a Nothing to finalize
!
#ifdef W3_MPI
        IF ( NRQ .EQ. 0 ) CYCLE
        IRQ    => BPSTGE(J,IMOD)%IRQBPS
#endif
!
! 1.b Wait for communication to end
!
#ifdef W3_MPI
        ALLOCATE ( STATUS(MPI_STATUS_SIZE,NRQ) )
        CALL MPI_WAITALL ( NRQ, IRQ, STATUS, IERR_MPI )
        DEALLOCATE ( STATUS )
#endif
!
! 1.c Reset arrays and counter
!
#ifdef W3_MPI
        NRQ    = 0
        DEALLOCATE ( BPSTGE(J,IMOD)%IRQBPS ,                     &
                     BPSTGE(J,IMOD)%TSTORE )
#endif
!
#ifdef W3_T
        WRITE (MDST,9010) J
#endif
!
        END DO
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT ( ' TEST WMIOBF : FINALIZE STAGING DATA FROM GRID ',I3)
 9010 FORMAT ( ' TEST WMIOBF : FINISHED WITH TARGET ',I3)
#endif
!/
!/ End of WMIOBF ----------------------------------------------------- /
!/
      END SUBROUTINE WMIOBF
!/ ------------------------------------------------------------------- /
      SUBROUTINE WMIOHS ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         28-Sep-2016 !
!/                  +-----------------------------------+
!/
!/    27-Jan-2006 : Origination.                        ( version 3.08 )
!/    20-Dec-2006 : Remove VTIME from MPI comm.         ( version 3.10 )
!/    28-Sep-2016 : Add error traps for MPI tags.       ( version 5.15 )
!/
!  1. Purpose :
!
!     Stage internal high-to-low data in the data structure HGSTGE.
!
!  2. Method :
!
!     Directly fill staging arrays in shared memory version, or post
!     the corresponding sends in distributed memory version.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number of grid from which data is to
!                          be staged.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETG, W3SETW, W3SETA, W3SETO, WMSETM
!                Subr. WxxDATMD Manage data structures.
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      EXTCDE    Sur.    Id.    Program abort.
!      DSEC21    Func. W3TIMEMD Difference between times.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      WMWAVE    Subr  WMWAVEMD Multi-grid wave model.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     See FORMAT label 1001.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/SHRD   Shared/distributed memory models.
!       !/DIST
!       !/MPI
!
!       !/S      Enable subroutine tracing.
!       !/T      Enable test output
!       !/MPIT
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE W3GDATMD
      USE W3WDATMD
      USE W3ADATMD
      USE W3ODATMD
      USE WMMDATMD
!
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
      USE W3TIMEMD, ONLY: DSEC21
      USE W3PARALL, ONLY: INIT_GET_ISEA
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
      INTEGER, INTENT(IN)     :: IMOD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: J, NR, I, JSEA, ISEA, IS
#ifdef W3_MPI
      INTEGER                 :: ITAG, IP, IT0, IERR_MPI
#endif
      INTEGER                 :: I1, I2
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_MPI
      INTEGER, POINTER        :: NRQ, IRQ(:), NRQOUT, OUTDAT(:,:)
#endif
      REAL                    :: DTOUTP
#ifdef W3_SHRD
      REAL, POINTER           :: SHGH(:,:,:)
#endif
#ifdef W3_MPI
      REAL, POINTER           :: SHGH(:,:)
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'WMIOHS')
#endif
!
! -------------------------------------------------------------------- /
! 0.  Initializations
! 
#ifdef W3_T
      WRITE (MDST,9000) IMOD, FLGHG1
#endif
!
      IF ( .NOT. FLGHG1 ) THEN
#ifdef W3_T
          WRITE (MDST,9001) HGSTGE(:,IMOD)%NSND
#endif
          IF ( SUM(HGSTGE(:,IMOD)%NSND) .EQ. 0 ) RETURN
        ELSE
#ifdef W3_T
          WRITE (MDST,9001) HGSTGE(:,IMOD)%NSN1
#endif
          IF ( SUM(HGSTGE(:,IMOD)%NSN1) .EQ. 0 ) RETURN
        END IF
!
      CALL W3SETO ( IMOD, MDSE, MDST )
      CALL W3SETG ( IMOD, MDSE, MDST )
      CALL W3SETW ( IMOD, MDSE, MDST )
      CALL W3SETA ( IMOD, MDSE, MDST )
! 
! -------------------------------------------------------------------- /
! 1.  Loop over grids
!
      DO J=1, NRGRD
!
        IF ( J .EQ. IMOD ) CYCLE
!
        IF ( .NOT. FLGHG1 ) THEN
            NR     = HGSTGE(J,IMOD)%NSND
          ELSE IF ( FLGHG2 ) THEN
            NR     = HGSTGE(J,IMOD)%NSN1
          ELSE
            IF ( TOUTP(1,J) .EQ. -1 ) THEN
                DTOUTP = 1.
              ELSE
                DTOUTP = DSEC21(TIME,TOUTP(:,J))
              END IF
            IF ( DTOUTP .EQ. 0. ) THEN
                NR     = HGSTGE(J,IMOD)%NSND
              ELSE
                NR     = HGSTGE(J,IMOD)%NSN1
              END IF
          END IF
!
#ifdef W3_T
        IF ( NR .EQ. 0 ) THEN
            WRITE (MDST,9010) J, NR
          ELSE
            WRITE (MDST,9011) J, NR, DSEC21(TIME,TSYNC(:,J)), DTOUTP
          END IF
#endif
!
        IF ( NR .EQ. 0 ) CYCLE
        IF ( DSEC21(TIME,TSYNC(:,J)) .NE. 0. ) CYCLE
!
! -------------------------------------------------------------------- /
! 2.  Allocate arrays and/or point pointers
!
#ifdef W3_SHRD
        SHGH   => HGSTGE(J,IMOD)%SHGH
#endif
#ifdef W3_MPI
        ALLOCATE ( HGSTGE(J,IMOD)%TSTORE(NSPEC,NR) )
        SHGH   => HGSTGE(J,IMOD)%TSTORE
#endif
!
#ifdef W3_MPI
        ALLOCATE ( HGSTGE(J,IMOD)%IRQHGS(NR) )
        ALLOCATE ( HGSTGE(J,IMOD)%OUTDAT(NR,3) )
#endif
!
#ifdef W3_MPI
        NRQ    => HGSTGE(J,IMOD)%NRQHGS
        NRQOUT => HGSTGE(J,IMOD)%NRQOUT
        IRQ    => HGSTGE(J,IMOD)%IRQHGS
        OUTDAT => HGSTGE(J,IMOD)%OUTDAT
        NRQ    = 0
        NRQOUT = 0
        IRQ    = 0
#endif
!
! -------------------------------------------------------------------- /
! 3.  Set the time
!     !/SHRD only.
!
#ifdef W3_T
        WRITE (MDST,9030) TIME
#endif
!
#ifdef W3_SHRD
        HGSTGE(J,IMOD)%VTIME = TIME
#endif
!
! -------------------------------------------------------------------- /
! 4.  Stage the spectral data
!
#ifdef W3_MPIT
        WRITE (MDST,9080)
#endif
#ifdef W3_MPI
        IT0    = MTAG1 + 1
#endif
!
        DO I=1, NR
!
          JSEA   = HGSTGE(J,IMOD)%ISEND(I,1)
          CALL INIT_GET_ISEA(ISEA, JSEA)
#ifdef W3_DIST
          IP     = HGSTGE(J,IMOD)%ISEND(I,2)
#endif
          I1     = HGSTGE(J,IMOD)%ISEND(I,3)
          I2     = HGSTGE(J,IMOD)%ISEND(I,4)
#ifdef W3_MPI
          ITAG   = HGSTGE(J,IMOD)%ISEND(I,5) + IT0
          IF ( ITAG .GT. MTAG2 ) THEN
              WRITE (MDSE,1001)
              CALL EXTCDE (1001) 
            END IF
#endif
!
          DO IS=1, NSPEC
#ifdef W3_SHRD
            SHGH(IS,I2,I1) = VA(IS,JSEA) * SIG2(IS)             &
                                 / CG(1+(IS-1)/NTH,ISEA)
#endif
#ifdef W3_MPI
            SHGH(  IS,I  ) = VA(IS,JSEA) * SIG2(IS)             &
                                 / CG(1+(IS-1)/NTH,ISEA)
#endif
            END DO
!
#ifdef W3_MPI
          IF ( IP .NE. IMPROC ) THEN
              NRQ    = NRQ + 1
              CALL MPI_ISEND ( SHGH(1,I), NSPEC, MPI_REAL, IP-1, &
                       ITAG, MPI_COMM_MWAVE, IRQ(NRQ), IERR_MPI )
#endif
#ifdef W3_MPIT
              WRITE (MDST,9082) NRQ, JSEA, IP, ITAG-MTAG1,       &
                                IRQ(NRQ), IERR_MPI
#endif
#ifdef W3_MPI
            ELSE
              NRQOUT = NRQOUT + 1
              OUTDAT(NRQOUT,1) = I
              OUTDAT(NRQOUT,2) = I2
              OUTDAT(NRQOUT,3) = I1
            END IF
#endif
!
          END DO
!
#ifdef W3_MPIT
        WRITE (MDST,9083)
        WRITE (MDST,9084) NRQ
#endif
!
        END DO
!
      RETURN
!
! Formats
!
#ifdef W3_MPI
 1001 FORMAT (/' *** ERROR WMIOHS : REQUESTED MPI TAG EXCEEDS', &
                                    ' UPPER BOUND (MTAG2) ***')
#endif
#ifdef W3_T
 9000 FORMAT ( ' TEST WMIOHS : STAGING DATA FROM GRID ',I3,        &
               '   FLGHG1 = ',L1)
 9001 FORMAT ( ' TEST WMIOHS : NR. OF SPECTRA PER GRID : '/        &
               '             ',15I6)
#endif
!
#ifdef W3_T
 9010 FORMAT ( ' TEST WMIOHS : POSTING DATA TO GRID ',I3,          &
               '   NR = ',I6)
 9011 FORMAT ( ' TEST WMIOHS : POSTING DATA TO GRID ',I3,          &
               '   NR = ',I6,'   TIME GAP = ',2F8.1)
#endif
!
#ifdef W3_T
 9030 FORMAT ( ' TEST WMIOHS : TIME :',I10.8,I7.6)
#endif
!
#ifdef W3_MPIT
 9080 FORMAT (/' MPIT WMIOHS: COMMUNICATION CALLS            '/ &
               ' +------+------+------+------+--------------+'/ &
               ' |  IH  |  ID  | TARG |  TAG |   handle err |'/ &
               ' +------+------+------+------+--------------+')
 9082 FORMAT ( ' |',I5,' |',I5,' |',2(I5,' |'),I9,I4,' |')
 9083 FORMAT ( ' +------+------+------+------+--------------+')
 9084 FORMAT ( ' MPIT WMIOHS: NRQHGS:',I10/)
#endif
!/
!/ End of WMIOHS ----------------------------------------------------- /
!/
      END SUBROUTINE WMIOHS
!/ ------------------------------------------------------------------- /
      SUBROUTINE WMIOHG ( IMOD, DONE ) 
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         20-Dec-2006 !
!/                  +-----------------------------------+
!/
!/    27-Jan-2006 : Origination.                        ( version 3.08 )
!/    20-Dec-2006 : Remove VTIME from MPI comm.         ( version 3.10 )
!/
!  1. Purpose :
!
!     Gather internal high-to-low data for a given model.
!
!  2. Method :
!
!     For distributed memory version first receive all staged data.
!     After staged data is present, average, convert as necessary,
!     and store in basic spatral arrays.
!
!  2. Method :
!
!     Using storage array HGSTAGE and time stamps.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number of grid from which data is to
!                          be gathered.
!       DONE    Log.   O   Flag for completion of operation (opt).
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETG, W3SETW, W3SETA, W3SETO
!                Subr. WxxDATMD Manage data structures.
!      W3CSPC    Subr. W3CSPCMD Spectral grid conversion.
!      STRACE    Sur.  W3SERVMD Subroutine tracing.
!      DSEC21    Func. W3TIMEMD Difference between times.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      WMWAVE    Subr  WMWAVEMD Multi-grid wave model.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     See FORMAT labels 1001-1002.
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
!       !/SHRD   Shared/distributed memory models.
!       !/DIST
!       !/MPI
!
!       !/S      Enable subroutine tracing.
!       !/T      Enable test output
!       !/MPIT
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE W3GDATMD
      USE W3WDATMD
      USE W3ADATMD
      USE W3ODATMD
      USE WMMDATMD
!
      USE W3CSPCMD, ONLY: W3CSPC
      USE W3TIMEMD, ONLY: DSEC21
!     USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_PDLIB
      use yowNodepool, only: npa
      USE yowExchangeModule, only : PDLIB_exchange2Dreal
#endif
      USE W3PARALL, ONLY : INIT_GET_ISEA
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
      INTEGER, INTENT(IN)            :: IMOD
      LOGICAL, INTENT(OUT), OPTIONAL :: DONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NTOT, J, IS, NA, IA, JSEA, ISEA, I
#ifdef W3_MPI
      INTEGER                 :: ITAG, IT0, IFROM, ILOC, NLOC,   &
                                 ISPROC, IERR_MPI, ICOUNT,       &
                                 I0, I1, I2
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      INTEGER, POINTER        :: VTIME(:)
#ifdef W3_MPI
      INTEGER, POINTER        :: NRQ, IRQ(:), STATUS(:,:)
#endif
      REAL                    :: DTTST, WGTH
      REAL, POINTER           :: SPEC1(:,:), SPEC2(:,:), SPEC(:,:)
#ifdef W3_MPI
      REAL, POINTER           :: SHGH(:,:,:)
#endif
      LOGICAL                 :: FLGALL
#ifdef W3_MPI
      LOGICAL                 :: FLAGOK
#endif
#ifdef W3_MPIT
      LOGICAL                 :: FLAG
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'WMIOHG')
#endif
!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
      IF ( TOUTP(1,IMOD) .EQ. -1 ) THEN
          DTTST  = 1.
        ELSE
          DTTST  = DSEC21 ( WDATAS(IMOD)%TIME , TOUTP(:,IMOD) )
        END IF
!
      IF ( .NOT. FLGHG1 ) THEN
          FLGALL = .TRUE.
        ELSE IF ( FLGHG2 ) THEN
          FLGALL = .FALSE.
        ELSE IF ( DTTST .EQ. 0. ) THEN
          FLGALL = .TRUE.
        ELSE
          FLGALL = .FALSE.
       END IF
!
#ifdef W3_T
      WRITE (MDST,9000) IMOD, DTTST, FLGALL
#endif
!
      IF ( FLGALL ) THEN
#ifdef W3_T
          WRITE (MDST,9001) HGSTGE(IMOD,:)%NREC
#endif
          NTOT   = SUM(HGSTGE(IMOD,:)%NREC)
        ELSE
#ifdef W3_T
          WRITE (MDST,9001) HGSTGE(IMOD,:)%NRC1
#endif
          NTOT   = SUM(HGSTGE(IMOD,:)%NRC1)
        END IF
!
      IF ( PRESENT(DONE) ) DONE = .FALSE.
!
      IF ( NTOT .EQ. 0 ) THEN
          IF ( PRESENT(DONE) ) DONE = .TRUE.
#ifdef W3_T
          WRITE (MDST,9003)
#endif
          RETURN
        END IF
!
      CALL W3SETO ( IMOD, MDSE, MDST )
      CALL W3SETG ( IMOD, MDSE, MDST )
      CALL W3SETW ( IMOD, MDSE, MDST )
      CALL W3SETA ( IMOD, MDSE, MDST )
!
! -------------------------------------------------------------------- /
! 1.  Testing / gathering data in staging arrays 
! 
#ifdef W3_T
      WRITE (MDST,9010) TIME
#endif
!
! 1.a Shared memory version, test valid times. - - - - - - - - - - - - /
! 
#ifdef W3_SHRD
      DO J=1, NRGRD
#endif
!
#ifdef W3_SHRD
        IF ( FLGALL ) THEN
            NTOT   = HGSTGE(IMOD,J)%NREC
          ELSE
            NTOT   = HGSTGE(IMOD,J)%NRC1
          END IF
        IF ( NTOT .EQ. 0 ) CYCLE
#endif
!
#ifdef W3_SHRD
        VTIME  => HGSTGE(IMOD,J)%VTIME
        IF ( VTIME(1) .EQ. -1 ) RETURN
        DTTST  = DSEC21 ( TIME, VTIME )
        IF ( DTTST .NE. 0. ) RETURN
#endif
!
#ifdef W3_SHRD
        END DO
#endif
!
! 1.b Distributed memory version - - - - - - - - - - - - - - - - - - - /
!
#ifdef W3_MPIT
        WRITE (MDST,9011) HGHSTA(IMOD)
#endif
!
! 1.b.1 HGHSTA = 0
!       Check if staging arrays are initialized.
!       Post the proper receives.
!
#ifdef W3_MPI
      IF ( HGHSTA(IMOD) .EQ. 0 ) THEN
#endif
!
#ifdef W3_MPI
          NRQ    => MDATAS(IMOD)%NRQHGG
          NRQ    = 0
          DO J=1, NRGRD
            IF ( FLGALL ) THEN
                NRQ    = NRQ + HGSTGE(IMOD,J)%NREC *             &
                               HGSTGE(IMOD,J)%NSMX
              ELSE
                NRQ    = NRQ + HGSTGE(IMOD,J)%NRC1 *             &
                               HGSTGE(IMOD,J)%NSMX
              END IF
            END DO
          NRQ    = MAX(1,NRQ)
          ALLOCATE ( IRQ(NRQ) )
          IRQ    = 0
          NRQ    = 0
#endif
!
#ifdef W3_MPI
          DO J=1, NRGRD
            IF ( HGSTGE(IMOD,J)%NTOT .EQ. 0 ) CYCLE
#endif
!
! ..... Check valid time to determine staging.
!
#ifdef W3_MPI
            VTIME  => HGSTGE(IMOD,J)%VTIME
            IF ( VTIME(1) .EQ. -1 ) THEN
                DTTST  = 1.
              ELSE
                DTTST  = DSEC21 ( TIME, VTIME )
              END IF
#endif
#ifdef W3_MPIT
            WRITE (MDST,9013) VTIME, DTTST
#endif
!
! ..... Post receives for data gather
!
#ifdef W3_MPI
            IF ( DTTST .NE. 0. ) THEN
#endif
#ifdef W3_MPIT
                WRITE (MDST,9014) J
#endif
!
! ..... Spectra
!
#ifdef W3_MPI
                IT0 = MTAG1 + 1
                SHGH  => HGSTGE(IMOD,J)%SHGH 
#endif
!
#ifdef W3_MPI
                IF ( FLGALL ) THEN
                    NTOT   = HGSTGE(IMOD,J)%NREC
                  ELSE
                    NTOT   = HGSTGE(IMOD,J)%NRC1
                  END IF
#endif
!
#ifdef W3_MPI
                DO I=1, NTOT
#endif
#ifdef W3_MPIT
                  JSEA   = HGSTGE(IMOD,J)%LJSEA(I)
#endif
#ifdef W3_MPI
                  NLOC   = HGSTGE(IMOD,J)%NRAVG(I)
                  DO ILOC=1, NLOC
                    ISPROC = HGSTGE(IMOD,J)%IMPSRC(I,ILOC)
                    ITAG   = HGSTGE(IMOD,J)%ITAG(I,ILOC) + IT0
                    IF ( ISPROC .NE. IMPROC ) THEN
                        NRQ    = NRQ + 1
                        CALL MPI_IRECV ( SHGH(1,ILOC,I),         &
                             SGRDS(J)%NSPEC, MPI_REAL,           &
                             ISPROC-1, ITAG, MPI_COMM_MWAVE,     &
                             IRQ(NRQ), IERR_MPI )
#endif
#ifdef W3_MPIT
                        WRITE (MDST,9016) NRQ, JSEA, ISPROC,     &
                               ITAG-MTAG1, IRQ(NRQ), IERR_MPI
#endif
#ifdef W3_MPI
                      END IF
                    END DO
                  END DO
#endif
!
! ..... End IF for posting receives 1.b.1
!
#ifdef W3_MPIT
                WRITE (MDST,9017) 
#endif
#ifdef W3_MPI
              END IF
#endif
!
! ..... End grid loop J in 1.b.1
!
#ifdef W3_MPI
            END DO
#endif
#ifdef W3_MPIT
          WRITE (MDST,9018) NRQ
#endif
!
#ifdef W3_MPI
          ALLOCATE ( MDATAS(IMOD)%IRQHGG(NRQ) )
          MDATAS(IMOD)%IRQHGG = IRQ(1:NRQ)
          DEALLOCATE ( IRQ )
#endif
!
! ..... Reset status
! 
#ifdef W3_MPI
          IF ( NRQ .GT. 0 ) THEN
              HGHSTA(IMOD) = 1
#endif
#ifdef W3_MPIT
              WRITE (MDST,9011) HGHSTA(IMOD)
#endif
#ifdef W3_MPI
            END IF
#endif
!
! ..... End IF in 1.b.1
!
#ifdef W3_MPI
        END IF
#endif
!
! 1.b.2 HGHSTA = 1
!       Wait for communication to finish.
!       If DONE defined, check if done, otherwise wait.
!
#ifdef W3_MPI
      IF ( HGHSTA(IMOD) .EQ. 1 ) THEN
#endif
!
#ifdef W3_MPI
          NRQ    => MDATAS(IMOD)%NRQHGG
          IRQ    => MDATAS(IMOD)%IRQHGG
          ALLOCATE ( STATUS(MPI_STATUS_SIZE,NRQ) )
#endif
!
! ..... Test communication if DONE is present, wait otherwise
!
#ifdef W3_MPI
          IF ( PRESENT(DONE) ) THEN
#endif
!
#ifdef W3_MPI
              CALL MPI_TESTALL ( NRQ, IRQ, FLAGOK, STATUS,       &
                                 IERR_MPI )
#endif
!
#ifdef W3_MPIT
              ICOUNT = 0
              DO I=1, NRQ
                CALL MPI_TEST ( IRQ(I), FLAG, STATUS(1,1),      &
                                IERR_MPI )
                FLAGOK = FLAGOK .AND. FLAG
                IF ( FLAG ) ICOUNT = ICOUNT + 1
                END DO
              WRITE (MDST,9019) 100. * REAL(ICOUNT) / REAL(NRQ)
#endif
!
#ifdef W3_MPI
            ELSE
#endif
!
#ifdef W3_MPI
              CALL MPI_WAITALL ( NRQ, IRQ, STATUS, IERR_MPI )
              FLAGOK = .TRUE.
#endif
#ifdef W3_MPIT
              WRITE (MDST,9019) 100.
#endif
!
#ifdef W3_MPI
            END IF
#endif
!
#ifdef W3_MPI
          DEALLOCATE ( STATUS )
#endif
!
! ..... Go on based on FLAGOK
!
#ifdef W3_MPI
          IF ( FLAGOK ) THEN
              NRQ    = 0
              DEALLOCATE ( MDATAS(IMOD)%IRQHGG )
            ELSE
              RETURN
            END IF
#endif
!
#ifdef W3_MPI
          HGHSTA(IMOD) = 0
#endif
#ifdef W3_MPIT
          WRITE (MDST,9011) HGHSTA(IMOD)
#endif
!
#ifdef W3_MPI
        END IF
#endif
!
! ..... process locally stored data
!
#ifdef W3_MPI
      DO J=1, NRGRD
        HGSTGE(IMOD,J)%VTIME = TIME
        IF ( J .EQ. IMOD ) CYCLE
        DO IS=1, HGSTGE(IMOD,J)%NRQOUT
          I0     = HGSTGE(IMOD,J)%OUTDAT(IS,1)
          I2     = HGSTGE(IMOD,J)%OUTDAT(IS,2)
          I1     = HGSTGE(IMOD,J)%OUTDAT(IS,3)
          HGSTGE(IMOD,J)%SHGH(:,I2,I1) = HGSTGE(IMOD,J)%TSTORE(:,I0)
          END DO
      END DO
#endif
!
! -------------------------------------------------------------------- /
! 2.  Data available, process grid by grid
! 
#ifdef W3_T
      WRITE (MDST,9020)
#endif
!
! 2.a Loop over grids
!
      DO J=1, NRGRD
!
        IF ( FLGALL ) THEN
            NTOT   = HGSTGE(IMOD,J)%NREC
          ELSE
            NTOT   = HGSTGE(IMOD,J)%NRC1
          END IF
        IF ( NTOT .EQ. 0 ) CYCLE
!
#ifdef W3_T
        WRITE (MDST,9021) J, NTOT
#endif
!
! 2.b Set up temp data structures
!
        IF ( RESPEC(IMOD,J) ) THEN
            ALLOCATE ( SPEC1(SGRDS(J)%NSPEC,NTOT), SPEC2(NSPEC,NTOT) )
            SPEC   => SPEC1
          ELSE
            ALLOCATE ( SPEC2(NSPEC,NTOT) )
            SPEC   => SPEC2
          END IF
!
! 2.c Average spectra to temp storage
!
#ifdef W3_T
        WRITE (MDST,9022)
#endif
!
        DO IS=1, NTOT
          NA     = HGSTGE(IMOD,J)%NRAVG(IS)
          WGTH   = HGSTGE(IMOD,J)%WGTH(IS,1)
          SPEC(:,IS) = WGTH * HGSTGE(IMOD,J)%SHGH(:,1,IS)
          DO IA=2, NA
            WGTH   = HGSTGE(IMOD,J)%WGTH(IS,IA)
            SPEC(:,IS) = SPEC(:,IS) + WGTH*HGSTGE(IMOD,J)%SHGH(:,IA,IS)
            END DO
          END DO
!
! 2.d Convert spectral grid as needed
!
        IF ( RESPEC(IMOD,J) ) THEN
!
#ifdef W3_T
            WRITE (MDST,9023)
#endif
!
            CALL W3CSPC ( SPEC1, SGRDS(J)%NK, SGRDS(J)%NTH,           &
                          SGRDS(J)%XFR, SGRDS(J)%FR1, SGRDS(J)%TH(1), &
                          SPEC2 , NK, NTH, XFR, FR1, TH(1),           &
                          NTOT, MDST, MDSE, FACHFE)
            DEALLOCATE ( SPEC1 )
!
          END IF
!
! 2.e Move spectra to model
!
#ifdef W3_T
        WRITE (MDST,9024)
#endif
!
        DO IS=1, NTOT
          JSEA   = HGSTGE(IMOD,J)%LJSEA(IS)
          CALL INIT_GET_ISEA(ISEA, JSEA)
          DO I=1, NSPEC
            VA(I,JSEA) = SPEC2(I,IS) / SIG2(I) * CG(1+(I-1)/NTH,ISEA)
            END DO
          END DO
!
        DEALLOCATE ( SPEC2 )
!
        END DO
!
! -------------------------------------------------------------------- /
! 3.  Set flag if reqeusted
! 
      IF ( PRESENT(DONE) ) DONE = .TRUE.
!
#ifdef W3_PDLIB
      CALL PDLIB_exchange2Dreal(VA(:,1:NPA))
#endif
!
! Formats
!
#ifdef W3_T
 9000 FORMAT ( ' TEST WMIOHG : GATHERING DATA FOR GRID ',I3/       &
               '               DTOUTP, FLGALL :',F8.1,L4)
 9001 FORMAT ( ' TEST WMIOHG : NR. OF SPECTRA PER SOURCE GRID : '/ &
               '             ',25I4)
 9003 FORMAT ( ' TEST WMIOHG : NO DATA TO BE GATHERED')
#endif
!
#ifdef W3_T
 9010 FORMAT ( ' TEST WMIOHG : TEST DATA AVAILABILITY FOR',I9.8,I7.6)
#endif
#ifdef W3_MPIT
 9011 FORMAT ( ' MPIT WMIOHG : HGHSTA =',I2)
 9013 FORMAT ( '               VTIME, DTTST :',I9.8,I7.6,1X,F8.1)
 9014 FORMAT (/' MPIT WMIOHG : RECEIVE FROM GRID',I4/           &
               ' +------+------+------+------+--------------+'/ &
               ' |  IH  |  ID  | FROM |  TAG |   handle err |'/ &
               ' +------+------+------+------+--------------+')
 9016 FORMAT ( ' |',I5,' |',I5,' |',2(I5,' |'),I9,I4,' |')
 9017 FORMAT ( ' +------+------+------+------+--------------+'/)
 9018 FORMAT ( ' MPIT WMIOHG : NRQBPT:',I10/)
 9019 FORMAT ( ' MPIT WMIOHG : RECEIVES FINISHED :',F6.1,'%')
#endif
!
#ifdef W3_T
 9020 FORMAT ( ' TEST WMIOHG : PROCESSING DATA GRID BY GRID')
 9021 FORMAT ( '               FROM GRID ',I3,'   NR OF SPECTRA :',I6)
 9022 FORMAT ( '               AVERAGE SPECTRA TO TEMP STORAGE')
 9023 FORMAT ( '               CONVERT SPECTRAL GRID')
 9024 FORMAT ( '               MOVE SPECTRA TO PERMANENT STORAGE')
#endif
!/
!/ End of WMIOHG ----------------------------------------------------- /
!/
      END SUBROUTINE WMIOHG
!/ ------------------------------------------------------------------- /
      SUBROUTINE WMIOHF ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         16-Jan-2006 !
!/                  +-----------------------------------+
!/
!/    16-Jan-2006 : Origination.                        ( version 3.08 )
!/
!  1. Purpose :
!
!     Finalize staging of internal high-to-low data in the data
!     structure HGSTGE (MPI only).
!
!  2. Method :
!
!     Post appropriate 'wait' functions to assure that the
!     communication has finished.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number of grid from which data has
!                          been staged.
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
!      WMWAVE    Subr  WMWAVEMD Multi-grid wave model.
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
!       !/SHRD   Shared/distributed memory models.
!       !/DIST
!       !/MPI
!
!       !/S      Enable subroutine tracing.
!       !/T      Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE WMMDATMD
!
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
      INTEGER, INTENT(IN)     :: IMOD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: J
#ifdef W3_MPI
      INTEGER                 :: IERR_MPI
      INTEGER, POINTER        :: NRQ, IRQ(:)
      INTEGER, ALLOCATABLE    :: STATUS(:,:)
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'WMIOHF')
#endif
!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
#ifdef W3_T
      WRITE (MDST,9000) IMOD
#endif
!
! -------------------------------------------------------------------- /
! 1.  Loop over grids
!
      DO J=1, NRGRD
!
#ifdef W3_MPI
        NRQ    => HGSTGE(J,IMOD)%NRQHGS
#endif
!
! 1.a Nothing to finalize
!
#ifdef W3_MPI
        IF ( NRQ .EQ. 0 ) CYCLE
        IRQ    => HGSTGE(J,IMOD)%IRQHGS
#endif
!
! 1.b Wait for communication to end
!
#ifdef W3_MPI
        ALLOCATE ( STATUS(MPI_STATUS_SIZE,NRQ) )
        CALL MPI_WAITALL ( NRQ, IRQ, STATUS, IERR_MPI )
        DEALLOCATE ( STATUS )
#endif
!
! 1.c Reset arrays and counter
!
#ifdef W3_MPI
        NRQ    = 0
        DEALLOCATE ( HGSTGE(J,IMOD)%IRQHGS,                      &
                     HGSTGE(J,IMOD)%TSTORE,                      &
                     HGSTGE(J,IMOD)%OUTDAT )
#endif
!
#ifdef W3_T
        WRITE (MDST,9010) J
#endif
!
        END DO
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT ( ' TEST WMIOHF : FINALIZE STAGING DATA FROM GRID ',I3)
 9010 FORMAT ( ' TEST WMIOHF : FINISHED WITH TARGET ',I3)
#endif
!/
!/ End of WMIOHF ----------------------------------------------------- /
!/
      END SUBROUTINE WMIOHF
!/ ------------------------------------------------------------------- /
      SUBROUTINE WMIOES ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         28-Sep-2016 !
!/                  +-----------------------------------+
!/
!/    25-May-2006 : Origination.                        ( version 3.09 )
!/    21-Dec-2006 : Remove VTIME from MPI comm.         ( version 3.10 )
!/    28-Sep-2016 : Add error traps for MPI tags.       ( version 5.15 )
!/    16-Dec-2020 : SMC grid use 1-1 spectral exchanges.( version 7.13 )
!/
!  1. Purpose :
!
!     Stage internal same-rank data in the data structure EQSTGE.
!
!  2. Method :
!
!     Directly fill staging arrays in shared memory version, or post
!     the corresponding sends in distributed memory version.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number of grid from which data is to
!                          be staged.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETG, W3SETW, W3SETA, W3SETO, WMSETM
!                Subr. WxxDATMD Manage data structures.
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      EXTCDE    Sur.    Id.    Program abort.
!      DSEC21    Func. W3TIMEMD Difference between times.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      WMWAVE    Subr  WMWAVEMD Multi-grid wave model.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     See FORMAT label 1001.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/SHRD   Shared/distributed memory models.
!       !/DIST
!       !/MPI
!
!       !/S      Enable subroutine tracing.
!       !/T      Enable test output
!       !/MPIT
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE W3GDATMD
      USE W3WDATMD
      USE W3ADATMD
      USE W3ODATMD
      USE WMMDATMD
!
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
      USE W3TIMEMD, ONLY: DSEC21
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
      INTEGER, INTENT(IN)     :: IMOD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: J, NR, I, ISEA, JSEA, IS, I1, I2
#ifdef W3_MPI
      INTEGER                 :: IT0, ITAG, IP, IERR_MPI
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_MPI
      INTEGER, POINTER        :: NRQ, IRQ(:), NRQOUT, OUTDAT(:,:)
#endif
#ifdef W3_SHRD
      REAL, POINTER           :: SEQL(:,:,:)
#endif
#ifdef W3_MPI
      REAL, POINTER           :: SEQL(:,:)
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'WMIOES')
#endif
!
! -------------------------------------------------------------------- /
! 0.  Initializations
! 
#ifdef W3_T
      WRITE (MDST,9000) IMOD
      WRITE (MDST,9001) EQSTGE(:,IMOD)%NSND
#endif
!
      CALL W3SETO ( IMOD, MDSE, MDST )
      CALL W3SETG ( IMOD, MDSE, MDST )
      CALL W3SETW ( IMOD, MDSE, MDST )
      CALL W3SETA ( IMOD, MDSE, MDST )
! 
! -------------------------------------------------------------------- /
! 1.  Loop over grids
!
      DO J=1, NRGRD
!
        IF ( J .EQ. IMOD ) CYCLE
        NR     = EQSTGE(J,IMOD)%NSND
!
#ifdef W3_T
        IF ( NR .EQ. 0 ) THEN
            WRITE (MDST,9010) J, NR
          ELSE
            WRITE (MDST,9011) J, NR, DSEC21(TIME,TSYNC(:,J))
          END IF
#endif
!
        IF ( NR .EQ. 0 ) CYCLE
        IF ( DSEC21(TIME,TSYNC(:,J)) .NE. 0. ) STOP
!
!!Li  Report sending for test.  JGLi22Dec2020
!       WRITE (MDSE,*) ' ***WMIOES: Send to GRID', J,           &
!                      ' from', IMOD, ' NS=', NR, ' on IP', IMPROC
! -------------------------------------------------------------------- /
! 2.  Allocate arrays and/or point pointers
!
#ifdef W3_SHRD
        SEQL   => EQSTGE(J,IMOD)%SEQL
#endif
#ifdef W3_MPI
        ALLOCATE ( EQSTGE(J,IMOD)%TSTORE(NSPEC,NR) )
        SEQL   => EQSTGE(J,IMOD)%TSTORE
#endif
!
#ifdef W3_MPI
        ALLOCATE ( EQSTGE(J,IMOD)%IRQEQS(NR)   ,                 &
                   EQSTGE(J,IMOD)%OUTDAT(NR,3) )
#endif
!
#ifdef W3_MPI
        NRQ    => EQSTGE(J,IMOD)%NRQEQS
        NRQOUT => EQSTGE(J,IMOD)%NRQOUT
        IRQ    => EQSTGE(J,IMOD)%IRQEQS
        OUTDAT => EQSTGE(J,IMOD)%OUTDAT
        NRQ    = 0
        NRQOUT = 0
        IRQ    = 0
#endif
!
! -------------------------------------------------------------------- /
! 3.  Set the time
!     Note that with MPI the send needs to be posted to the local
!     processor too to make time management possible.
!
#ifdef W3_T
        WRITE (MDST,9030) TIME
#endif
!
#ifdef W3_SHRD
        EQSTGE(J,IMOD)%VTIME = TIME
#endif
!
! -------------------------------------------------------------------- /
! 4.  Stage the spectral data
!
#ifdef W3_MPIT
        WRITE (MDST,9080)
#endif
#ifdef W3_MPI
        IT0 = MTAG2 + 1
#endif
!
        DO I=1, NR
!
          ISEA   = EQSTGE(J,IMOD)%SIS(I)
          JSEA   = EQSTGE(J,IMOD)%SJS(I)
          I1     = EQSTGE(J,IMOD)%SI1(I)
          I2     = EQSTGE(J,IMOD)%SI2(I)
#ifdef W3_MPI
          IP     = EQSTGE(J,IMOD)%SIP(I)
          ITAG   = EQSTGE(J,IMOD)%STG(I) + IT0
          IF ( ITAG .GT. MTAG_UB ) THEN
              WRITE (MDSE,1001)
              CALL EXTCDE (1001) 
            END IF
#endif
!
#ifdef W3_SMC
 !!  Equal ranked SMC grids simply pass the wave action.  JGLi16Dec2020
#endif
#ifdef W3_MPI
#ifdef W3_SMC
          IF( GTYPE .EQ. SMCTYPE ) THEN
             SEQL(:, I) = VA(:, JSEA) 
          ELSE 
#endif
#endif
          DO IS=1, NSPEC
#ifdef W3_SHRD
            SEQL(IS,I1,I2) = VA(IS,JSEA) * SIG2(IS)             &
                                 / CG(1+(IS-1)/NTH,ISEA)
#endif
#ifdef W3_MPI
            SEQL(  IS,I  ) = VA(IS,JSEA) * SIG2(IS)             &
                                 / CG(1+(IS-1)/NTH,ISEA)
#endif
            END DO
#ifdef W3_MPI
#ifdef W3_SMC
          ENDIF 
#endif
#endif
!
#ifdef W3_MPI
          IF ( IP .NE. IMPROC ) THEN
              NRQ    = NRQ + 1
              CALL MPI_ISEND ( SEQL(1,I), NSPEC, MPI_REAL, IP-1, &
                       ITAG, MPI_COMM_MWAVE, IRQ(NRQ), IERR_MPI )
#endif
#ifdef W3_MPIT
              WRITE (MDST,9082) NRQ, JSEA, IP, ITAG-MTAG2,      &
                                IRQ(NRQ), IERR_MPI
#endif
#ifdef W3_MPI
            ELSE 
              NRQOUT = NRQOUT + 1
              OUTDAT(NRQOUT,1) = I
              OUTDAT(NRQOUT,2) = I1
              OUTDAT(NRQOUT,3) = I2
            END IF
#endif
!
          END DO
!
#ifdef W3_MPIT
        WRITE (MDST,9083)
        WRITE (MDST,9084) NRQ
#endif
!
        END DO
!
      RETURN
!
! Formats
!
#ifdef W3_MPI
 1001 FORMAT (/' *** ERROR WMIOES : REQUESTED MPI TAG EXCEEDS', &
                                  ' UPPER BOUND (MTAG_UB) ***')
#endif
#ifdef W3_T
 9000 FORMAT ( ' TEST WMIOES : STAGING DATA FROM GRID ',I3)
 9001 FORMAT ( ' TEST WMIOES : NR. OF SPECTRA PER GRID : '/        &
               '             ',15I6)
#endif
!
#ifdef W3_T
 9010 FORMAT ( ' TEST WMIOES : POSTING DATA TO GRID ',I3,          &
               '   NR = ',I6)
 9011 FORMAT ( ' TEST WMIOES : POSTING DATA TO GRID ',I3,          &
               '   NR = ',I6,'   TIME GAP = ',F8.1)
#endif
!
#ifdef W3_T
 9030 FORMAT ( ' TEST WMIOES : TIME :',I10.8,I7.6)
#endif
!/
#ifdef W3_MPIT
 9080 FORMAT (/' MPIT WMIOES: COMMUNICATION CALLS            '/ &
               ' +------+------+------+------+--------------+'/ &
               ' |  IH  |  ID  | TARG |  TAG |   handle err |'/ &
               ' +------+------+------+------+--------------+')
 9082 FORMAT ( ' |',I5,' |',I5,' |',2(I5,' |'),I9,I4,' |')
 9083 FORMAT ( ' +------+------+------+------+--------------+')
 9084 FORMAT ( ' MPIT WMIOES: NRQEQS:',I10/)
#endif
!/
!/ End of WMIOES ----------------------------------------------------- /
!/
      END SUBROUTINE WMIOES
!/ ------------------------------------------------------------------- /
      SUBROUTINE WMIOEG ( IMOD, DONE ) 
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Jan-2007 !
!/                  +-----------------------------------+
!/
!/    25-May-2006 : Origination.                        ( version 3.09 )
!/    21-Dec-2006 : Remove VTIME from MPI comm.         ( version 3.10 )
!/    22-Jan-2007 : Adding NAVMAX.                      ( version 3.10 )
!/
!  1. Purpose :
!
!     Gather internal same-rank data for a given model.
!
!  2. Method :
!
!     For distributed memory version first receive all staged data.
!     After staged data is present, average, convert as necessary,
!     and store in basic spatral arrays.
!
!  2. Method :
!
!     Using storage array EQSTGE and time stamps.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number of grid from which data is to
!                          be gathered.
!       DONE    Log.   O   Flag for completion of operation (opt).
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETG, W3SETW, W3SETA, W3SETO
!                Subr. WxxDATMD Manage data structures.
!      W3CSPC    Subr. W3CSPCMD Spectral grid conversion.
!      STRACE    Sur.  W3SERVMD Subroutine tracing.
!      DSEC21    Func. W3TIMEMD Difference between times.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      WMWAVE    Subr  WMWAVEMD Multi-grid wave model.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     See FORMAT labels 1001-1002.
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
!       !/SHRD   Shared/distributed memory models.
!       !/DIST
!       !/MPI
!
!       !/S      Enable subroutine tracing.
!       !/T      Enable test output
!       !/MPIT
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE W3GDATMD
      USE W3WDATMD
      USE W3ADATMD
      USE W3ODATMD
      USE WMMDATMD
!
      USE W3CSPCMD, ONLY: W3CSPC
      USE W3TIMEMD, ONLY: DSEC21
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_PDLIB
      use yowNodepool, only: npa
      USE yowExchangeModule, only : PDLIB_exchange2Dreal
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
      INTEGER, INTENT(IN)            :: IMOD
      LOGICAL, INTENT(OUT), OPTIONAL :: DONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: J, I, ISEA, JSEA, IA, IS
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_MPI
      INTEGER                 :: IT0, ITAG, IFROM, IERR_MPI,     &
                                 NA, IP, I1, I2
#endif
#ifdef W3_MPIT
      INTEGER                 :: ICOUNT
#endif
      INTEGER, POINTER        :: VTIME(:)
#ifdef W3_MPI
      INTEGER, POINTER        :: NRQ, IRQ(:), STATUS(:,:)
#endif
      REAL                    :: DTTST, WGHT
      REAL, POINTER           :: SPEC1(:,:), SPEC2(:,:), SPEC(:,:)
#ifdef W3_MPI
      REAL, POINTER           :: SEQL(:,:,:)
      LOGICAL                 :: FLAGOK
      LOGICAL                 :: FLAG
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'WMIOEG')
#endif
!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
#ifdef W3_T
      WRITE (MDST,9000) IMOD
      WRITE (MDST,9001) 'NREC', EQSTGE(IMOD,:)%NREC
#endif
!
      IF ( PRESENT(DONE) ) DONE = .FALSE.
!
      IF ( EQSTGE(IMOD,IMOD)%NREC .EQ. 0 ) THEN
          IF ( PRESENT(DONE) ) DONE = .TRUE.
#ifdef W3_T
          WRITE (MDST,9002)
#endif
          RETURN
        END IF
!
      CALL W3SETO ( IMOD, MDSE, MDST )
      CALL W3SETG ( IMOD, MDSE, MDST )
      CALL W3SETW ( IMOD, MDSE, MDST )
      CALL W3SETA ( IMOD, MDSE, MDST )
!
! -------------------------------------------------------------------- /
! 1.  Testing / gathering data in staging arrays
!
#ifdef W3_T
      WRITE (MDST,9010) TIME
#endif
!
! 1.a Shared memory version, test valid times. - - - - - - - - - - - - /
!
#ifdef W3_SHRD
      DO J=1, NRGRD
#endif
!
#ifdef W3_SHRD
        IF ( IMOD .EQ. J ) CYCLE
        IF ( EQSTGE(IMOD,J)%NREC .EQ. 0 ) CYCLE
#endif
!
#ifdef W3_SHRD
        VTIME  => EQSTGE(IMOD,J)%VTIME
        IF ( VTIME(1) .EQ. -1 ) RETURN
        DTTST  = DSEC21 ( TIME, VTIME )
        IF ( DTTST .NE. 0. ) RETURN
#endif
!
#ifdef W3_SHRD
        END DO
#endif
!
! 1.b Distributed memory version - - - - - - - - - - - - - - - - - - - /
!
#ifdef W3_MPIT
        WRITE (MDST,9011) EQLSTA(IMOD)
#endif
!
! 1.b.1 EQLSTA = 0
!       Check if staging arrays are initialized.
!       Post the proper receives.
!
#ifdef W3_MPI
      IF ( EQLSTA(IMOD) .EQ. 0 ) THEN
#endif
!
#ifdef W3_MPI
          NRQ    => MDATAS(IMOD)%NRQEQG
          NRQ    = 0
          DO J=1, NRGRD
            IF ( J .EQ. IMOD ) CYCLE
            NRQ    = NRQ + EQSTGE(IMOD,J)%NREC *                 &
                           EQSTGE(IMOD,J)%NAVMAX
            END DO
          ALLOCATE ( IRQ(NRQ) )
          IRQ    = 0
          NRQ    = 0
#endif
!
#ifdef W3_MPI
          DO J=1, NRGRD
            IF ( IMOD .EQ. J ) CYCLE
            IF ( EQSTGE(IMOD,J)%NREC .EQ. 0 ) CYCLE
#endif
!
! ..... Check valid time to determine staging.
!
#ifdef W3_MPI
            VTIME  => EQSTGE(IMOD,J)%VTIME
            IF ( VTIME(1) .EQ. -1 ) THEN
                DTTST  = 1.
              ELSE
                DTTST  = DSEC21 ( TIME, VTIME )
              END IF
#endif
#ifdef W3_MPIT
            WRITE (MDST,9013) VTIME, DTTST
#endif
!
! ..... Post receives for data gather
! 
#ifdef W3_MPI
            IF ( DTTST .NE. 0. ) THEN
#endif
#ifdef W3_MPIT
                WRITE (MDST,9014) J
#endif
!
! ..... Spectra
! 
#ifdef W3_MPI
                IT0 = MTAG2 + 1
                SEQL  => EQSTGE(IMOD,J)%SEQL
#endif
!
#ifdef W3_MPI
                DO I=1, EQSTGE(IMOD,J)%NREC
                  JSEA   = EQSTGE(IMOD,J)%JSEA(I)
                  NA     = EQSTGE(IMOD,J)%NAVG(I)
                  DO IA=1, NA
                    IP     = EQSTGE(IMOD,J)%RIP(I,IA)
                    ITAG   = EQSTGE(IMOD,J)%RTG(I,IA) + IT0
                    IF ( IP .NE. IMPROC ) THEN
                        NRQ    = NRQ + 1
                        CALL MPI_IRECV ( SEQL(1,I,IA),           &
                             SGRDS(J)%NSPEC, MPI_REAL,           &
                             IP-1, ITAG, MPI_COMM_MWAVE,         &
                             IRQ(NRQ), IERR_MPI )
#endif
#ifdef W3_MPIT
                        WRITE (MDST,9016) NRQ, JSEA, IP,         &
                               ITAG-MTAG2, IRQ(NRQ), IERR_MPI
#endif
#ifdef W3_MPI
                      END IF
                    END DO
                  END DO
#endif
!
! ..... End IF for posting receives 1.b.1
!
#ifdef W3_MPIT
                WRITE (MDST,9017)
#endif
#ifdef W3_MPI
              END IF
#endif
!
! ..... End grid loop J in 1.b.1
!
#ifdef W3_MPI
            END DO
#endif
#ifdef W3_MPIT
          WRITE (MDST,9018) NRQ
#endif
!
#ifdef W3_MPI
          IF ( NRQ .NE. 0 ) THEN
              ALLOCATE ( MDATAS(IMOD)%IRQEQG(NRQ) )
              MDATAS(IMOD)%IRQEQG = IRQ(1:NRQ)
            END IF
#endif
!
#ifdef W3_MPI
          DEALLOCATE ( IRQ )
#endif
!
! ..... Reset status
! 
#ifdef W3_MPI
          IF ( NRQ .GT. 0 ) THEN
              EQLSTA(IMOD) = 1
#endif
#ifdef W3_MPIT
              WRITE (MDST,9011) EQLSTA(IMOD)
#endif
#ifdef W3_MPI
            END IF
#endif
!
! ..... End IF in 1.b.1
!
#ifdef W3_MPI
        END IF
#endif
!
! 1.b.2 EQLSTA = 1
!       Wait for communication to finish.
!       If DONE defined, check if done, otherwise wait.
!
#ifdef W3_MPI
      IF ( EQLSTA(IMOD) .EQ. 1 ) THEN
#endif
!
#ifdef W3_MPI
          NRQ    => MDATAS(IMOD)%NRQEQG
          IRQ    => MDATAS(IMOD)%IRQEQG
          ALLOCATE ( STATUS(MPI_STATUS_SIZE,NRQ) )
#endif
!
! ..... Test communication if DONE is present, wait otherwise
!
#ifdef W3_MPI
          IF ( PRESENT(DONE) ) THEN
#endif
!
#ifdef W3_MPI
              CALL MPI_TESTALL ( NRQ, IRQ, FLAGOK, STATUS,       &
                                 IERR_MPI )
#endif
!
#ifdef W3_MPIT
              ICOUNT = 0
              DO I=1, NRQ
                CALL MPI_TEST ( IRQ(I), FLAG, STATUS(1,1),      &
                                IERR_MPI )
                FLAGOK = FLAGOK .AND. FLAG
                IF ( FLAG ) ICOUNT = ICOUNT + 1
                END DO
              WRITE (MDST,9019) 100. * REAL(ICOUNT) / REAL(NRQ)
#endif
!
#ifdef W3_MPI
            ELSE
#endif
!
#ifdef W3_MPI
              CALL MPI_WAITALL ( NRQ, IRQ, STATUS, IERR_MPI )
              FLAGOK = .TRUE.
#endif
#ifdef W3_MPIT
              WRITE (MDST,9019) 100.
#endif
!
#ifdef W3_MPI
            END IF
#endif
!
#ifdef W3_MPI
          DEALLOCATE ( STATUS )
#endif
!
! ..... Go on based on FLAGOK
!
#ifdef W3_MPI
          IF ( FLAGOK ) THEN
              IF ( NRQ.NE.0 ) DEALLOCATE ( MDATAS(IMOD)%IRQEQG )
              NRQ    = 0
            ELSE
              RETURN
            END IF
#endif
!
#ifdef W3_MPI
          EQLSTA(IMOD) = 0
#endif
#ifdef W3_MPIT
          WRITE (MDST,9011) EQLSTA(IMOD)
#endif
!
#ifdef W3_MPI
        END IF
#endif
!
! ..... process locally stored data
!
#ifdef W3_MPI
      DO J=1, NRGRD
        EQSTGE(IMOD,J)%VTIME = TIME
        IF ( J .EQ. IMOD ) CYCLE
        DO IS=1, EQSTGE(IMOD,J)%NRQOUT
          I      = EQSTGE(IMOD,J)%OUTDAT(IS,1)
          I1     = EQSTGE(IMOD,J)%OUTDAT(IS,2)
          I2     = EQSTGE(IMOD,J)%OUTDAT(IS,3)
          EQSTGE(IMOD,J)%SEQL(:,I1,I2) = EQSTGE(IMOD,J)%TSTORE(:,I)
          END DO
      END DO
#endif
!
! -------------------------------------------------------------------- /
! 2.  Data available, process grid by grid
!
#ifdef W3_T
      WRITE (MDST,9020)
#endif
!
! 2.a Do 'native' grid IMOD
!
#ifdef W3_T
      WRITE (MDST,9021) IMOD, EQSTGE(IMOD,IMOD)%NREC
#endif
!
      DO I=1, EQSTGE(IMOD,IMOD)%NREC
        JSEA   = EQSTGE(IMOD,IMOD)%JSEA(I)
        WGHT   = EQSTGE(IMOD,IMOD)%WGHT(I)
        VA(:,JSEA) = WGHT * VA(:,JSEA)
        END DO
!
! 2.b Loop over other grids
!
      DO J=1, NRGRD
        IF ( IMOD.EQ.J .OR. EQSTGE(IMOD,J)%NREC.EQ.0 ) CYCLE
!
#ifdef W3_T
        WRITE (MDST,9022) J, EQSTGE(IMOD,J)%NREC
#endif
!
#ifdef W3_SMC
 !! Use 1-1 full boundary spectra without modification. JGLi16Dec2020
        IF( GTYPE .EQ. SMCTYPE ) THEN
          DO I=1, EQSTGE(IMOD,J)%NREC
             JSEA   = EQSTGE(IMOD,J)%JSEA(I)
             VA(:,JSEA) = EQSTGE(IMOD,J)%SEQL(:,I,1) 
          END DO
        ELSE
 !! Other grid boundary spectra may need conversion.   JGLi12Apr2021
#endif
!
! 2.c Average spectra
!
#ifdef W3_T
        WRITE (MDST,9023)
#endif
        ALLOCATE ( SPEC1(SGRDS(J)%NSPEC,EQSTGE(IMOD,J)%NREC) )
        SPEC1  = 0.
!
        DO I=1, EQSTGE(IMOD,J)%NREC
          DO IA=1, EQSTGE(IMOD,J)%NAVG(I)
            SPEC1(:,I) = SPEC1(:,I) + EQSTGE(IMOD,J)%SEQL(:,I,IA) *   &
                                       EQSTGE(IMOD,J)%WAVG(I,IA)
            END DO
          END DO
!
! 2.d Convert spectra
!
        IF ( RESPEC(IMOD,J) ) THEN
#ifdef W3_T
            WRITE (MDST,9024)
#endif
            ALLOCATE ( SPEC2(NSPEC,EQSTGE(IMOD,J)%NREC) )
!
            CALL W3CSPC ( SPEC1, SGRDS(J)%NK, SGRDS(J)%NTH,           &
                          SGRDS(J)%XFR, SGRDS(J)%FR1, SGRDS(J)%TH(1), &
                          SPEC2 , NK, NTH, XFR, FR1, TH(1),           &
                          EQSTGE(IMOD,J)%NREC, MDST, MDSE, FACHFE)
!
            SPEC   => SPEC2
          ELSE
            SPEC   => SPEC1
          END IF
!
! 2.e Apply to native grid
!
        DO I=1, EQSTGE(IMOD,J)%NREC
          ISEA   = EQSTGE(IMOD,J)%ISEA(I)
          JSEA   = EQSTGE(IMOD,J)%JSEA(I)
          WGHT   = EQSTGE(IMOD,J)%WGHT(I)
#ifdef W3_SMC
 !!  Regular grid in same ranked SMC group uses 1-1 mapping. JGLi12Apr2021
          IF( NGRPSMC .GT. 0 ) THEN
             VA(:,JSEA) = SPEC(:,I)
          ELSE 
#endif
          DO IS=1, NSPEC
            VA(IS,JSEA) = VA(IS,JSEA) + WGHT *                        &
               SPEC(IS,I) / SIG2(IS) * CG(1+(IS-1)/NTH,ISEA)
            END DO
#ifdef W3_SMC
          ENDIF !! NGRPSMC .GT. 0
#endif
          END DO
!
! 2.f Final clean up
!
        DEALLOCATE ( SPEC1 )
        IF ( RESPEC(IMOD,J) ) DEALLOCATE ( SPEC2 )

#ifdef W3_SMC
 !!  End GTYPE .EQ. SMCTYPE 
        ENDIF    
#endif

!!  End 2.b J grid loop.
        END DO
!
! -------------------------------------------------------------------- /
! 3.  Set flag if requested
!
      IF ( PRESENT(DONE) ) DONE = .TRUE.
!       
#ifdef W3_PDLIB
  CALL PDLIB_exchange2Dreal(VA(:,1:NPA))
#endif
!
! Formats
!
#ifdef W3_T
 9000 FORMAT ( ' TEST WMIOEG : GATHERING DATA FOR GRID ',I4)
 9001 FORMAT ( ' TEST WMIOEG : ',A,' PER SOURCE GRID : '/13X,20I5)
 9002 FORMAT ( ' TEST WMIOEG : NO DATA TO BE GATHERED')
#endif
!
#ifdef W3_T
 9010 FORMAT ( ' TEST WMIOEG : TEST DATA AVAILABILITY FOR',I9.8,I7.6)
#endif
#ifdef W3_MPIT
 9011 FORMAT ( ' MPIT WMIOEG : EQLSTA =',I2)
 9012 FORMAT ( '               STAGING ARRAY FROM',I4,1X,A)
 9013 FORMAT ( '               VTIME, DTTST :',I9.8,I7.6,1X,F8.1)
 9014 FORMAT (/' MPIT WMIOEG : RECEIVE FROM GRID',I4/           &
               ' +------+------+------+------+--------------+'/ &
               ' |  IH  |  ID  | FROM |  TAG |   handle err |'/ &
               ' +------+------+------+------+--------------+')
 9016 FORMAT ( ' |',I5,' |',I5,' |',2(I5,' |'),I9,I4,' |')
 9017 FORMAT ( ' +------+------+------+------+--------------+'/)
 9018 FORMAT ( ' MPIT WMIOEG : NRQBPT:',I10/)
 9019 FORMAT ( ' MPIT WMIOEG : RECEIVES FINISHED :',F6.1,'%')
#endif
!
#ifdef W3_T
 9020 FORMAT ( ' TEST WMIOEG : PROCESSING DATA GRID BY GRID')
 9021 FORMAT ( '               NATIVE    GRID ',I3,'   DATA :',I6)
 9022 FORMAT ( '               RECEIVING GRID ',I3,'   DATA :',I6)
 9023 FORMAT ( '                  AVERAGE SPECTRA')
 9024 FORMAT ( '                  CONVERTING SPECTRA')
#endif
!/
!/ End of WMIOEG ----------------------------------------------------- /
!/
      END SUBROUTINE WMIOEG
!/ ------------------------------------------------------------------- /
      SUBROUTINE WMIOEF ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         25-May-2006 !
!/                  +-----------------------------------+
!/
!/    25-May-2006 : Origination.                        ( version 3.09 )
!/
!  1. Purpose :
!
!     Finalize staging of internal same-rank data in the data
!     structure EQSTGE (MPI only).
!
!  2. Method :
!
!     Post appropriate 'wait' functions to assure that the
!     communication has finished.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number of grid from which data has
!                          been staged.
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
!      WMWAVE    Subr  WMWAVEMD Multi-grid wave model.
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
!       !/SHRD   Shared/distributed memory models.
!       !/DIST
!       !/MPI
!
!       !/S      Enable subroutine tracing.
!       !/T      Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE WMMDATMD
!
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
      INTEGER, INTENT(IN)     :: IMOD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: J
#ifdef W3_MPI
      INTEGER                 :: IERR_MPI
      INTEGER, POINTER        :: NRQ, IRQ(:)
      INTEGER, ALLOCATABLE    :: STATUS(:,:)
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'WMIOEF')
#endif
!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
#ifdef W3_T
      WRITE (MDST,9000) IMOD
#endif
!
! -------------------------------------------------------------------- /
! 1.  Loop over grids
!
      DO J=1, NRGRD
!
#ifdef W3_MPI
        NRQ    => EQSTGE(J,IMOD)%NRQEQS
#endif
!
! 1.a Nothing to finalize
!
#ifdef W3_MPI
        IF ( NRQ .EQ. 0 ) CYCLE
        IRQ    => EQSTGE(J,IMOD)%IRQEQS
#endif
!
! 1.b Wait for communication to end
!
#ifdef W3_MPI
        ALLOCATE ( STATUS(MPI_STATUS_SIZE,NRQ) )
        CALL MPI_WAITALL ( NRQ, IRQ, STATUS, IERR_MPI )
        DEALLOCATE ( STATUS )
#endif
!
! 1.c Reset arrays and counter
!
#ifdef W3_MPI
        DEALLOCATE ( EQSTGE(J,IMOD)%IRQEQS,                      &
                     EQSTGE(J,IMOD)%TSTORE,                      &
                     EQSTGE(J,IMOD)%OUTDAT )
        NRQ    = 0
#endif
!
#ifdef W3_T
        WRITE (MDST,9010) J
#endif
!
        END DO
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT ( ' TEST WMIOEF : FINALIZE STAGING DATA FROM GRID ',I3)
 9010 FORMAT ( ' TEST WMIOEF : FINISHED WITH TARGET ',I3)
#endif
!/
!/ End of WMIOEF ----------------------------------------------------- /
!/
      END SUBROUTINE WMIOEF
!/
!/ End of module WMINIOMD -------------------------------------------- /
!/
      END MODULE WMINIOMD
