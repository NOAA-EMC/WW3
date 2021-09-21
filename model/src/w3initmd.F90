#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3INITMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    28-Dec-2004 : Origination (out of W3WAVEMD).      ( version 3.06 )
!/                  Multiple grid version.
!/    03-Jan-2005 : Add US2x to MPI communication.      ( version 3.06 )
!/    04-Jan-2005 : Add grid output flags to W3INIT.    ( version 3.06 )
!/    07-Feb-2005 : Combined vs. separate test output.  ( version 3.07 )
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    21-Jul-2005 : Add output fields.                  ( version 3.07 )
!/    09-Nov-2005 : Drying out of points added.         ( version 3.08 )
!/    13-Jun-2006 : Splitting STORE in G/SSTORE.        ( version 3.09 )
!/    26-Jun-2006 : adding wiring for output type 6.    ( version 3.09 )
!/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    02-Aug-2006 : Adding W3MPIP.                      ( version 3.10 )
!/    02-Nov-2006 : Adding partitioning options.        ( version 3.10 )
!/    11-Jan-2007 : Updating IAPPRO computation.        ( version 3.10 )
!/    02-Apr-2007 : Add partitioned field data.         ( version 3.11 )
!/                  Add user-defined field data.
!/    01-May-2007 : Move O7a output to W3IOPP.          ( version 3.11 )
!/    08-May-2007 : Starting from calm as an option.    ( version 3.11 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
!/    29-Feb-2008 : Add NEC compiler directives.        ( version 3.13 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    23-Jul-2009 : Implement unstructured grids        ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    02-Sep.2012 : Set up for > 999 test files.        ( version 4.10 )
!/                  Reset UST initialization.
!/    03-Sep-2012 : Switch test file on/off (TSTOUT)    ( version 4.10 )
!/    03-Sep-2012 : Clean up of UG grids                ( version 4.08 )
!/    30-Sep-2012 : Implemetation of tidal constituents ( version 4.09 )
!/    07-Dec-2012 : Initialize UST non-zero.            ( version 4.11 )
!/    12-Dec-2012 : Changes for SMC grid.  JG_Li        ( version 4.11 )
!/    26-Dec-2012 : Modify field output MPI for new     ( version 4.11 )
!/                  structure and smaller memory footprint.
!/    02-Jul-2013 : Bug fix MPI_FLOAT -> MPI_REAL.      ( version 4.11 )
!/    10-Oct-2013 : CG and WN values at DMIN for ISEA=0 ( version 4.12 )
!/    14-Nov-2013 : Remove UST(DIR) initialization.     ( version 4.13 )
!/    15-Dec-2013 : Adds fluxes to ice                  ( version 5.01 )
!/    01-May-2017 : Adds directional MSS parameters     ( version 6.04 )
!/    05-Jun-2018 : Adds PDLIB/MEMCHECK/DEBUG           ( version 6.04 )
!/    21-Aug-2018 : Add WBT parameter                   ( version 6.06 )
!/    26-Aug-2018 : UOST (Mentaschi et al. 2015, 2018)  ( version 6.06 )
!/    25-Sep-2020 : Extra fields for coupling restart   ( version 7.10 )
!/    22-Mar-2021 : Extra coupling fields               ( version 7.13 )
!/    22-Jun-2021 : GKE NL5 (Q. Liu)                    ( version 7.13 )
!/
!/    Copyright 2009-2013 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!/    Note: Changes in version numbers not logged above.
!/
!  1. Purpose :
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      CRITOS    R.P.  Public   Critical percentage of resources used
!                               for output to trigger warning.
!      WWVER     C*10  Public   Model version number.
!      SWITCHES  C*256 Public   switches taken from bin/switch
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3INIT    Subr. Public   Wave model initialization.
!      W3MPII    Subr. Public   Initialize MPI data transpose.
!      W3MPIO    Subr. Public   Initialize MPI output gathering.
!      W3MPIP    Subr. Public   Initialize MPI point output gathering.
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
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   Id.
!
!       !/S     Enable subroutine tracing.
!       !/Tn    Enable test output.
!       !/MPIT  Enable test output (MPI).
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      PUBLIC
!/
      REAL, PARAMETER                :: CRITOS = 15.
      CHARACTER(LEN=10), PARAMETER   :: WWVER  = '7.13  '
      CHARACTER(LEN=512), PARAMETER  :: SWITCHES  = &
                    __WW3_SWITCHES__ 
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3INIT ( IMOD, IsMulti, FEXT, MDS, MTRACE, ODAT      &
                          , FLGRD,                               &
                           FLGR2, FLGD, FLG2, NPT, XPT, YPT, PNAMES,   &
                          IPRT, PRTFRM, MPI_COMM, FLAGSTIDEIN)

!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         03-Sep-2012 |
!/                  +-----------------------------------+
!/
!/    17-Mar-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    13-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    14-Feb-2000 : Exact-NL added.                     ( version 2.01 )
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    24-Jan-2002 : Zero time step for data ass.        ( version 2.17 )
!/    18-Feb-2002 : Point output diagnostics added.     ( version 2.18 )
!/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
!/    20-Aug-2003 : Output server options added.        ( version 3.04 )
!/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/                  Taken out of W3WAVE.
!/    04-Jan-2005 : Add grid output flags to par list.  ( version 3.06 )
!/    07-Feb-2005 : Combined vs. separate test output.  ( version 3.07 )
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    09-Nov-2005 : Drying out of points added.         ( version 3.08 )
!/    26-Jun-2006 : adding wiring for output type 6.    ( version 3.09 )
!/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    02-Aug-2006 : Adding W3MPIP.                      ( version 3.10 )
!/    02-Nov-2006 : Adding partitioning options.        ( version 3.10 )
!/    11-Jan-2007 : Updating IAPPRO computation.        ( version 3.10 )
!/    01-May-2007 : Move O7a output to W3IOPP.          ( version 3.11 )
!/    08-May-2007 : Starting from calm as an option.    ( version 3.11 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 
!/    13-Sep-2009 : Add coupling option                 ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.1 )
!/                  (A. Roland and F. Ardhuin) 
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    02-Sep.2012 : Set up for > 999 test files.        ( version 4.10 )
!/    03-Sep-2012 : Switch test file on/off (TSTOUT)    ( version 4.10 )
!/    03-Sep-2012 : Clean up of UG grids                ( version 4.08 )
!/
!  1. Purpose :
!
!     Initialize WAVEWATCH III.
!
!  2. Method :
!
!     Initialize data structure and wave fields from data files.
!     Initialize grid from local and instantaneous data.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number.
!       FEXT    Char   I   Extension of data files.
!       MDS     I.A.   I   Array with dataset numbers (see below), 
!                          saved as NDS in W3ODATMD.
!                           1: General output unit number ("log file").
!                           2: Error output unit number.
!                           3: Test output unit number.
!                           4: "screen", i.e., direct output location,
!                              can be the screen or the output file of
!                              the shell. 
!                           5: Model definition file unit number.
!                           6: Restart file unit number.
!                           7: Grid output file unit number.
!                           8: Point output file unit number.
!                           9: Input boundary data file unit number.
!                          10: Output boundary data file unit number
!                              (first).
!                          11: Track information file unit number.
!                          12: Track output file unit number.
!       MTRACE  I.A.   I   Array with subroutine tracing information.
!                           1: Output unit number for trace.
!                           2: Maximum number of trace prints.
!       ODAT    I.A.   I   Output data, five parameters per output type
!                           1-5  Data for OTYPE = 1; gridded fields.
!                                1 YYYMMDD for first output.
!                                2 HHMMSS for first output.
!                                3 Output interval in seconds.
!                                4 YYYMMDD for last output.
!                                5 HHMMSS for last output.
!                           6-10 Id. for OTYPE = 2; point output.
!                          11-15 Id. for OTYPE = 3; track point output.
!                          16-20 Id. for OTYPE = 4; restart files.
!                          21-25 Id. for OTYPE = 5; boundary data.
!                          31-35 Id. for OTYPE = 7; coupling data.
!                          36-40 Id. for OTYPE = 8; second restart file
!       FLGRD   L.A.   I   Flags for gridded output.
!       FLGR2   L.A.   I   Flags for coupling output.
!       NPT     Int.   I   Number of output points
!       X/YPT   R.A.   I   Coordinates of output points.
!       PNAMES  C.A.   I   Output point names.
!       IPRT    I.A.   I   Partitioning grid info.
!       PRTFRM  I.A.   I   Partitioning format flag.
!       MPI_COMM Int.  I   MPI communicator to be used for model.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETG    Subr. W3GDATMD Point to data structure.
!      W3SETW    Subr. W3WDATMD Point to data structure.
!      W3DIMW    Subr.   Id.    Set array sizes in data structure.
!      W3SETA    Subr. W3ADATMD Point to data structure.
!      W3DIMA    Subr.   Id.    Set array sizes in data structure.
!      W3SETI    Subr. W3IDATMD Point to data structure.
!      W3DIMI    Subr.   Id.    Set array sizes in data structure.
!      W3SETO    Subr. W3ODATMD Point to data structure.
!      W3DMO5    Subr.   Id.    Set array sizes in data structure.
!      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
!      STRACE    Subr.   Id.    Subroutine tracing.
!      EXTCDE    Subr.   Id.    Program abort.
!      WWDATE    Subr.   Id.    System date.
!      WWTIME    Subr.   Id.    System time.
!      DSEC21    Func. W3TIMEMD Compute time difference.
!      TICK21    Func.   Id.    Advance the clock.
!      STME21    Func.   Id.    Print the time readable.
!      PRTBLK    Func. W3ARRYMD Print plot of array.
!      W3IOGR    Subr. W3IOGRMD Read/write model definition file.
!      W3IORS    Subr. W3IORSMD Read/write restart file.
!      W3IOPP    Subr. W3IOPOMD Preprocess point output.
!      CALL MPI_COMM_SIZE, CALL MPI_COMM_RANK 
!                Subr. mpif.h   Standard MPI routines.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any program shell or integrated model which uses WAVEWATCH III.
!
!  6. Error messages :
!
!     On opening of log file only. Other error messages are generated
!     by W3IOGR and W3IORS.
!
!  7. Remarks :
!
!     - The log file is called 'log.FEXT', where FEXT is passed to
!       the routine.
!     - The test output file is called 'test.FEXT' in shared memory
!       version or testNNN.FEXT in distributed memory version.
!     - A water level and ice coverage are transferred with the
!       restart file. To assure consistency within the model, the
!       water level and ice coverage are re-evaluated at the 0th
!       time step in the actual wave model routine.
!     - When running regtests in cases where disk is non-local 
!       (i.e. NFS used), there can be a huge improvment in compute
!       time by using /var/tmp/ for log files. 
!       See commented line at "OPEN (MDS(1),FILE=..."
!
!  8. Structure :
!
!     ----------------------------------------------------
!      1.  Set-up of idata structures and I/O.
!        a Point to proper data structures.
!        b Number of processors and processor number.
!        c Open files.
!        d Dataset unit numbers
!        e Subroutine tracing
!        f Initial and test outputs
!      2.  Model definition.
!        a Read model definition file         ( W3IOGR )
!        b Save MAPSTA.
!        c MPP preparation
!      3.  Model initialization.
!        a Read restart file.                 ( W3IORS )
!        b Compare grid and restart MAPSTA.
!        c Initialize with winds if requested (set flag).
!        d Initialize calm conditions if requested.
!        e Preparations for prop. scheme.
!      4.  Set-up output times.
!        a Unpack ODAT.
!        b Check if output available.
!        c Get first time per output and overall.
!        d Prepare point output               ( W3IOPP )
!      5.  Define wavenumber grid.
!        a Calculate depth.
!        b Fill wavenumber and group velocity arrays.
!      6.  Initialize arrays.
!      7.  Write info to log file.
!      8.  Final MPI set up  ( W3MPII , W3MPIO , W3MPIP )
!     ----------------------------------------------------
!
!  9. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   Id.
!
!       !/S     Enable subroutine tracing.
!       !/Tn    Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
#ifdef W3_MEMCHECK
      USE MallocInfo_m
#endif
!/
      USE W3GDATMD, ONLY: W3SETG, RSTYPE
      USE W3WDATMD, ONLY: W3SETW, W3DIMW
      USE W3ADATMD, ONLY: W3SETA, W3DIMA
#ifdef W3_MEMCHECK
 USE W3ADATMD, ONLY: MALLINFOS
#endif
      USE W3IDATMD, ONLY: W3SETI, W3DIMI
      USE W3ODATMD, ONLY: W3SETO, W3DMO5
      USE W3IOGOMD, ONLY: W3FLGRDUPDT
      USE W3IOGRMD, ONLY: W3IOGR
      USE W3IORSMD, ONLY: W3IORS
      USE W3IOPOMD, ONLY: W3IOPP
      USE W3SERVMD, ONLY: ITRACE, EXTCDE, WWDATE, WWTIME
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
      USE W3TIMEMD, ONLY: DSEC21, TICK21, STME21
      USE W3ARRYMD, ONLY: PRTBLK
!/
      USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, MAPSTA, MAPST2, MAPFS, &
                          MAPSF, FLAGLL,   &
                          ICLOSE, ZB, TRNX, TRNY, DMIN, DTCFL, DTMAX, &
                          FLCK, NK, NTH, NSPEC, SIG, GNAME
#ifdef W3_PDLIB
     USE W3GDATMD, ONLY : FLCTH
#endif
      USE W3WDATMD, ONLY: TIME, TLEV, TICE, TRHO, WLV, UST, USTDIR, VA
      USE W3ODATMD, ONLY: NDSO, NDSE, NDST, SCREEN, NDS, NTPROC,      &
                          NAPROC, IAPROC, NAPLOG, NAPOUT, NAPERR,     &
                          NAPFLD, NAPPNT, NAPTRK, NAPRST, NAPBPT,     &
                          NAPPRT, TOFRST, DTOUT, TONEXT, TOLAST,      &
                          FLOUT, FLOGRD, FLBPO, NOPTS, PTNME,         &
                          PTLOC, IPTINT, PTIFAC, UNDEF, IDOUT, FLBPI, &
                          OUTPTS, FNMPRE, IX0, IXN, IXS, IY0, IYN,    &
                          IYS, FLFORM, IOSTYP, UNIPTS, UPPROC, NOTYPE,&
                          FLOGR2, NOGRP, NGRPP, FLOGD, FLOG2
#ifdef W3_NL5
      USE W3ODATMD, ONLY: TOSNL5
#endif
      USE W3ADATMD, ONLY: NSEALM, IAPPRO, FLCOLD, FLIWND, DW, CG, WN, &
                          UA, UD, U10, U10D, AS
#ifdef W3_MPI
      USE W3ADATMD, ONLY: MPI_COMM_WAVE, MPI_COMM_WCMP
#endif
      USE W3IDATMD, ONLY: FLLEV, FLCUR, FLWIND, FLICE, FLTAUA, FLRHOA,&
                          FLMDN, FLMTH, FLMVS, FLIC1, FLIC2, FLIC3,   &
                          FLIC4, FLIC5 
      USE W3DISPMD, ONLY: WAVNU1, WAVNU3
      USE W3PARALL, ONLY : AC_tot
      USE W3PARALL, ONLY: SET_UP_NSEAL_NSEALM
#ifdef W3_PDLIB
      USE W3PARALL, ONLY: SYNCHRONIZE_IPGL_ETC_ARRAY, ISEA_TO_JSEA
      use yowNodepool, only: npa
      use yowRankModule, only : rank
#endif
     USE W3GDATMD, ONLY: GTYPE, UNGTYPE
#ifdef W3_PDLIB
      USE PDLIB_W3PROFSMD, ONLY : PDLIB_MAPSTA_INIT, VA_SETUP_IOBPD
      USE PDLIB_W3PROFSMD, ONLY : BLOCK_SOLVER_INIT, PDLIB_STYLE_INIT
      use yowDatapool, only: istatus
#endif
#ifdef W3_SETUP
      USE W3WAVSET, ONLY : PREPARATION_FD_SCHEME
      USE W3WDATMD, ONLY: ZETA_SETUP
      USE W3GDATMD, ONLY : DO_CHANGE_WLV
#endif
      USE W3GDATMD, ONLY: FSN,FSPSI,FSFCT,FSNIMP, FSTOTALIMP, FSTOTALEXP
      USE W3GDATMD, ONLY: FSREFRACTION, FSFREQSHIFT
      USE W3PARALL, ONLY: INIT_GET_JSEA_ISPROC, INIT_GET_ISEA
#ifdef W3_TIMINGS
      USE W3PARALL, ONLY: PRINT_MY_TIME
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
     USE PDLIB_W3PROFSMD, ONLY: ALL_VA_INTEGRAL_PRINT, TEST_MPI_STATUS
#endif
#ifdef W3_DEBUGINIT
    USE PDLIB_W3PROFSMD, ONLY: PRINT_WN_STATISTIC
#endif
#endif
#ifdef W3_UOST
     USE W3UOSTMD, ONLY: UOST_SETGRID
#endif
!/
      IMPLICIT NONE
!
#ifdef W3_MPI
      INCLUDE "mpif.h"
#endif
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)           :: IMOD, MDS(13), MTRACE(2),      &
                                       ODAT(40),NPT, IPRT(6),&
                                       MPI_COMM
      LOGICAL, INTENT(IN)           :: IsMulti
      REAL, INTENT(INOUT)           :: XPT(NPT), YPT(NPT)
      LOGICAL, INTENT(INOUT)        :: FLGRD(NOGRP,NGRPP), FLGD(NOGRP),&
                                       FLGR2(NOGRP,NGRPP), FLG2(NOGRP),&
                                       PRTFRM
      CHARACTER, INTENT(IN)         :: FEXT*(*)
      CHARACTER(LEN=40), INTENT(IN) :: PNAMES(NPT)
      LOGICAL, INTENT(IN), OPTIONAL :: FLAGSTIDEIN(4)
      INTEGER                       :: NSEALout, NSEALMout
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      integer :: IRANK, I, ISTAT
      INTEGER                 :: IE, IFL, IFT, IERR, NTTOT, NTLOC,    &
                                 NTTARG, IK, IP, ITH, IX, IY, &
                                 J, J0, TOUT(2), TLST(2), ISEA, IS,   &
                                 K, I1, I2, JSEA, NTTMAX
#ifdef W3_DIST
      INTEGER                 :: ISTEP, ISP, IW
#endif
#ifdef W3_MPI
      INTEGER                 :: IERR_MPI, BGROUP, LGROUP
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T
      INTEGER                 :: NX0, NXN
      INTEGER, ALLOCATABLE    :: MAPOUT(:,:)
#endif
#ifdef W3_MPI
      INTEGER, ALLOCATABLE    :: TMPRNK(:)
#endif
      INTEGER, ALLOCATABLE    :: NT(:), MAPTST(:,:)
#ifdef W3_T
      INTEGER, SAVE           :: NXS = 49
#endif
      REAL                    :: DTTST, DEPTH, FRACOS
      REAL                    :: FACTOR
      REAL                    :: WLVeff
#ifdef W3_T
       REAL, ALLOCATABLE      :: XOUT(:,:)
#endif
      LOGICAL                 :: OPENED
      CHARACTER(LEN=8)        :: STTIME
      CHARACTER(LEN=10)       :: STDATE
      INTEGER                 :: ISPROC
#ifdef W3_DIST
      CHARACTER(LEN=12)       :: FORMAT
#endif
      CHARACTER(LEN=23)       :: DTME21
      CHARACTER(LEN=30)       :: LFILE, TFILE
#ifdef W3_PDLIB
      INTEGER                 :: IScal(1), IPROC
#endif
!/
!/ ------------------------------------------------------------------- /
!
! 1.  Set-up of data structures and I/O  ----------------------------- /
! 1.a Point to proper data structures.
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 1")

      CALL W3SETO ( IMOD, MDS(2), MDS(3) )
      CALL W3SETG ( IMOD, MDS(2), MDS(3) )
      CALL W3SETW ( IMOD, MDS(2), MDS(3) )
      CALL W3SETA ( IMOD, MDS(2), MDS(3) )
      CALL W3SETI ( IMOD, MDS(2), MDS(3) )
#ifdef W3_UOST
      CALL UOST_SETGRID(IMOD)
#endif
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 2")
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'Beginning of W3INIT'
      WRITE(740+IAPROC,*) '  FLGR2(10,1)=', FLGR2(10,1)
      WRITE(740+IAPROC,*) '  FLGR2(10,2)=', FLGR2(10,2)
      FLUSH(740+IAPROC)
#endif
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("Case 2")
#endif


#ifdef W3_MEMCHECK
       WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 1'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
!
! 1.b Number of processors and processor number.
!     Overwrite some initializations from W3ODATMD.
!
!     *******************************************************
!     *** NOTE : OUTPUT PROCESSOR ASSIGNMENT NEEDS TO BE  ***
!     ***        CONSISTENT WITH ASSIGNMENT IN WMINIT.    ***
!     *******************************************************
!
#ifdef W3_SHRD
      NTPROC = 1
      NAPROC = 1
      IAPROC = 1
      IOSTYP = 1
#endif
!
#ifdef W3_MPI
      MPI_COMM_WAVE = MPI_COMM
      CALL MPI_COMM_SIZE ( MPI_COMM_WAVE, NTPROC, IERR_MPI )
      NAPROC = NTPROC
      CALL MPI_COMM_RANK ( MPI_COMM_WAVE, IAPROC, IERR_MPI )
      IAPROC = IAPROC + 1
#endif
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 3")
      IF ( IOSTYP .LE. 1 ) THEN
!
          NAPFLD = MAX(1,NAPROC-1)
          NAPPNT = MAX(1,NAPROC-2)
          NAPTRK = MAX(1,NAPROC-5)
          NAPRST = NAPROC
          NAPBPT = MAX(1,NAPROC-3)
          NAPPRT = MAX(1,NAPROC-4)
!
        ELSE
!
          NAPPNT = NAPROC
          IF ( UNIPTS .AND. UPPROC ) NAPROC = MAX(1,NTPROC - 1)
          NAPFLD = NAPROC
          NAPRST = NAPROC
          NAPBPT = NAPROC
          NAPTRK = NAPROC
          NAPPRT = NAPROC
!
          IF ( IOSTYP .EQ. 2 ) THEN
              NAPROC = MAX(1,NAPROC-1)
            ELSE IF ( IOSTYP .EQ. 3 ) THEN
!
! For field or coupling output
!
              IF ( ODAT( 3).GT.0 .OR.  ODAT(33).GT.0 ) THEN
                  NAPFLD =       NAPROC
                  NAPROC = MAX(1,NAPROC-1)
                END IF
              IF ( ODAT(13).GT.0 ) THEN
                  NAPTRK =       NAPROC
                  NAPROC = MAX(1,NAPROC-1)
                END IF
              IF ( ODAT(28).GT.0 ) THEN
                  NAPPRT =       NAPROC
                  NAPROC = MAX(1,NAPROC-1)
                END IF
              IF ( ODAT( 8).GT.0 ) NAPPNT = NAPROC
              IF ( ODAT(18).GT.0 ) NAPRST = NAPROC
              IF ( ODAT(23).GT.0 ) NAPBPT = NAPROC
              IF ( ( ODAT( 8).GT.0 .OR. ODAT(18).GT.0 .OR.            &
                     ODAT(23).GT.0 ) ) NAPROC = MAX(1,NAPROC-1)
            END IF
        END IF
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 4")
      FRACOS = 100. * REAL(NTPROC-NAPROC) / REAL(NTPROC)
      IF ( FRACOS.GT.CRITOS .AND. IAPROC.EQ.NAPERR )                  &
                                           WRITE (NDSE,8002) FRACOS
!
#ifdef W3_MPI
      IF ( NAPROC .EQ. NTPROC ) THEN
          MPI_COMM_WCMP = MPI_COMM_WAVE
        ELSE
          CALL MPI_COMM_GROUP ( MPI_COMM_WAVE, BGROUP, IERR_MPI )
          ALLOCATE ( TMPRNK(NAPROC) )
          DO J=1, NAPROC
            TMPRNK(J) = J - 1
            END DO
          CALL MPI_GROUP_INCL ( BGROUP, NAPROC, TMPRNK, LGROUP,  &
                                IERR_MPI )
          CALL MPI_COMM_CREATE ( MPI_COMM_WAVE, LGROUP,          &
                                 MPI_COMM_WCMP, IERR_MPI )
          CALL MPI_GROUP_FREE ( LGROUP, IERR_MPI )
          CALL MPI_GROUP_FREE ( BGROUP, IERR_MPI )
          DEALLOCATE ( TMPRNK )
      END IF
#endif
!!!/PDLIB    CALL W3SETG(IMOD, NDSE, NDST)
!
           LPDLIB = .FALSE. 
#ifdef W3_PDLIB
    LPDLIB = .TRUE.
#endif
           IF (FSTOTALIMP .and. .NOT. LPDLIB) THEN
             WRITE(NDSE,*) 'IMPTOTAL is selected'
             WRITE(NDSE,*) 'But PDLIB is not'
             STOP 'Stop, case 1'
           ELSE IF (FSTOTALEXP .and. .NOT. LPDLIB) THEN
             WRITE(NDSE,*) 'EXPTOTAL is selected'
             WRITE(NDSE,*) 'But PDLIB is not'
             STOP 'Stop, case 1'
           END IF
!
! 1.c Open files without unpacking MDS ,,,
!
      IE     = LEN_TRIM(FEXT)
      LFILE  = 'log.' // FEXT(:IE)
      IFL    = LEN_TRIM(LFILE)
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 5")
#ifdef W3_SHRD
       TFILE  = 'test.' // FEXT(:IE)
#endif
#ifdef W3_DIST
      IW     = 1 + INT ( LOG10 ( REAL(NAPROC) + 0.5 ) )
      IW     = MAX ( 3 , MIN ( 9 , IW ) ) 
      WRITE (FORMAT,'(A5,I1.1,A1,I1.1,A4)')                     &
                   '(A4,I', IW, '.', IW, ',2A)'
      WRITE (TFILE,FORMAT) 'test',                             &
                    OUTPTS(IMOD)%IAPROC, '.', FEXT(:IE)
#endif
      IFT    = LEN_TRIM(TFILE)
      J      = LEN_TRIM(FNMPRE)
!
      IF ( OUTPTS(IMOD)%IAPROC .EQ. OUTPTS(IMOD)%NAPLOG )             &
#ifdef W3_DEBUGINIT
       WRITE(*,*) '1: w3initmd f=', TRIM(FNMPRE(:J)//LFILE(:IFL))
#endif
          OPEN (MDS(1),FILE=FNMPRE(:J)//LFILE(:IFL),ERR=888,IOSTAT=IERR)
!
      IF ( MDS(3).NE.MDS(1) .AND. MDS(3).NE.MDS(4) .AND. TSTOUT ) THEN
          INQUIRE (MDS(3),OPENED=OPENED)
#ifdef W3_DEBUGINIT
       WRITE(*,*) '2: w3initmd f=', TRIM(FNMPRE(:J)//TFILE(:IFT))
#endif
          IF ( .NOT. OPENED ) OPEN                                    &
               (MDS(3),FILE=FNMPRE(:J)//TFILE(:IFT),ERR=889,IOSTAT=IERR)
        END IF
!
! 1.d Dataset unit numbers
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 6")
      NDS    = MDS
      NDSO   = NDS(1)
      NDSE   = NDS(2)
      NDST   = NDS(3)
      SCREEN = NDS(4)
!
! 1.e Subroutine tracing
!
      CALL ITRACE ( MTRACE(1), MTRACE(2) )
!
! 1.f Initial and test outputs
!
#ifdef W3_MEMCHECK
       WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 2'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 7")

      IF ( IAPROC .EQ. NAPLOG ) THEN
          CALL WWDATE ( STDATE )
          CALL WWTIME ( STTIME )
          WRITE (NDSO,900) WWVER, STDATE, STTIME
        END IF

#ifdef W3_MEMCHECK
       WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 2a'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
#ifdef W3_S
      CALL STRACE (IENT, 'W3INIT')
#endif
#ifdef W3_T
      WRITE(NDST,9000) IMOD, FEXT(:IE)
      WRITE (NDST,9001) NTPROC, NAPROC, IAPROC, NAPLOG, NAPOUT,    &
        NAPERR, NAPFLD, NAPPNT, NAPTRK, NAPRST, NAPBPT, NAPPRT
      WRITE (NDST,9002) NDSO, NDSE, NDST, SCREEN
      WRITE (NDST,9003) LFILE(:IFL), TFILE(:IFT)
#endif
!
! 2.  Model defintition ---------------------------------------------- /
! 2.a Read model defintition file
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 8")
      CALL W3IOGR ( 'READ', NDS(5), IMOD, FEXT )
#ifdef W3_PDLIB
    IF (GTYPE .ne. UNGTYPE) THEN
#endif
#ifdef W3_SETUP
      CALL PREPARATION_FD_SCHEME(IMOD)
#endif
#ifdef W3_PDLIB
    ELSE
#ifdef W3_DEBUGINIT
      WRITE(*,*) 'Before PDLIB_STYLE_INIT, IMOD=', IMOD
#endif
      CALL PDLIB_STYLE_INIT(IMOD)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'After set up of NSEAL, NSEALM=', NSEALM
     WRITE(740+IAPROC,*) 'After PDLIB_STYLE_INIT'
     WRITE(740+IAPROC,*) 'allocated(ISEA_TO_JSEA)=', allocated(ISEA_TO_JSEA)
     FLUSH(740+IAPROC)
#endif
#endif
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("After PDLIB_STYLE_INIT")
#endif

#ifdef W3_PDLIB
#ifdef W3_DEBUGINIT
      WRITE(*,*) 'After PDLIB_STYLE_INIT, IMOD=', IMOD
#endif
      CALL SYNCHRONIZE_IPGL_ETC_ARRAY(IMOD, IsMulti)
    END IF
#endif
! Update of output parameter flags based on mod_def parameters (for 3D arrays)
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'Before W3FLGRDUPDT'
      FLUSH(740+IAPROC)
#endif
      CALL W3FLGRDUPDT ( NDSO, NDSE, FLGRD, FLGR2, FLGD, FLG2 )
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 9")
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("After W3FLGRDUPDT")
#endif

      IF ( FLAGLL ) THEN
          FACTOR = 1.
        ELSE
          FACTOR = 1.E-3
        END IF
      IF ( IAPROC .EQ. NAPLOG ) WRITE (NDSO,920)
!
! 2.b Save MAPSTA
!
      ALLOCATE ( MAPTST(NY,NX) )
      MAPTST  = MAPSTA 

#ifdef W3_MEMCHECK
       WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 2b'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
!
! 2.c MPP preparation
! 2.c.1 Set simple counters and variables
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 10")
      CALL SET_UP_NSEAL_NSEALM(NSEALout, NSEALMout)
      NSEAL=NSEALout
      NSEALM=NSEALMout

#ifdef W3_MEMCHECK
       WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 2c'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'After set up of NSEAL, NSEAL=', NSEAL
     WRITE(740+IAPROC,*) 'After set up of NSEAL, NSEALM=', NSEALM
     WRITE(740+IAPROC,*) 'NSEA=', NSEA, ' NSPEC=', NSPEC
     FLUSH(740+IAPROC)
#endif
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 11")

#ifdef W3_DIST
        IF ( NSEA .LT. NAPROC ) GOTO 820
        IF ((LPDLIB .eqv. .FALSE.).or.(GTYPE .NE. UNGTYPE)) THEN
          IF ( NSPEC .LT. NAPROC ) GOTO 821
        END IF
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Before PDLIB related allocations'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
     IF ((IAPROC .LE. NAPROC).and.(GTYPE .eq. UNGTYPE)) THEN
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'After test 1'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
       IF (FSNIMP .or. FSTOTALIMP) THEN
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Before BLOCK_SOLVER_INIT'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
         CALL BLOCK_SOLVER_INIT()
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'After BLOCK_SOLVER_INIT'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("After BLOCK_SOLVER_INIT")
#endif
#ifdef W3_PDLIB
       ELSE IF (FSTOTALEXP) THEN
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Before AC_tot allocation'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
         allocate(AC_tot(NSPEC, npa), stat=istat)
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'After AC_tot allocation'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
       ENDIF
     END IF
#endif
#ifdef W3_MEMCHECK
       WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 2d'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 12")
!
!
! 2.c.2 Allocate arrays
!
      IF ( IAPROC .LE. NAPROC ) THEN
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Calling W3DIMW at W3INIT, case 1'
     FLUSH(740+IAPROC)
#endif
          CALL W3DIMW ( IMOD, NDSE, NDST )
        ELSE
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Calling W3DIMW at W3INIT, case 2'
     FLUSH(740+IAPROC)
#endif
          CALL W3DIMW ( IMOD, NDSE, NDST, .FALSE. )
        END IF
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) ' 1: NSEAL=', NSEAL
     WRITE(740+IAPROC,*) ' maxval(UST)=', maxval(UST)
     FLUSH(740+IAPROC)
#endif
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("After W3DIMW")
#endif
      CALL W3DIMA ( IMOD, NDSE, NDST )
      CALL W3DIMI ( IMOD, NDSE, NDST , FLAGSTIDEIN )
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 13")
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("After W3DIMI")
#endif

#ifdef W3_MEMCHECK
      WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 3'
      call getMallocInfo(mallinfos)
      call printMallInfo(IAPROC,mallInfos)
#endif
!
! 2.c.3 Calculated expected number of prop. calls per processor
!
      NTTOT  = 0
      DO IK=1, NK
        NTLOC  = 1 + INT(DTMAX/(DTCFL*SIG(IK)/SIG(1))-0.001)
        NTTOT  = NTTOT + NTLOC*NTH
        END DO
      NTTARG = 1 + (NTTOT-1)/NAPROC
      NTTARG = NTTARG + INT(DTMAX/(DTCFL*SIG(NK)/SIG(1))-0.001)
      NTTMAX = NTTARG + 5
!
! 2.c.4 Initialize IAPPRO
!
      IAPPRO = 1
      ALLOCATE ( NT(NSPEC) )
      NT     = NTTOT
#ifdef W3_DIST
    IF ((LPDLIB .eqv. .FALSE.).or.(GTYPE .NE. UNGTYPE)) THEN
#endif
!
#ifdef W3_DIST
      DO
#endif
!
! 2.c.5 First sweep filling IAPPRO
!
#ifdef W3_DIST
        DO IP=1, NAPROC
          ISTEP  = IP
          ISP    = 0
          NT(IP) = 0
          DO J=1, 1+NSPEC/NAPROC
            ISP    = ISP + ISTEP
            IF ( MOD(J,2) .EQ. 1 ) THEN
                ISTEP  = 2*(NAPROC-IP) + 1
              ELSE
                ISTEP  = 2*IP - 1
              END IF
            IF ( ISP .LE. NSPEC ) THEN
                IK     = 1 + (ISP-1)/NTH
                NTLOC  = 1 + INT(DTMAX/(DTCFL*SIG(IK)/SIG(1))-0.001)
                IF ( NT(IP)+NTLOC .LE. NTTARG ) THEN
                    IAPPRO(ISP) = IP
                    NT(IP)      = NT(IP) + NTLOC
                  ELSE
                    IAPPRO(ISP) = -1
                 END IF
              END IF
            END DO 
          END DO
#endif
!
! 2.c.6 Second sweep filling IAPPRO
!
#ifdef W3_DIST
        DO IP=1, NAPROC
          IF ( NT(IP) .LT. NTTARG ) THEN
              DO ISP=1, NSPEC
                IF ( IAPPRO(ISP) .EQ. -1 ) THEN
                    IK     = 1 + (ISP-1)/NTH
                    NTLOC  = 1 + INT(DTMAX/(DTCFL*SIG(IK)/SIG(1))-0.001)
                  IF ( NT(IP)+NTLOC .LE. NTTARG ) THEN
                        IAPPRO(ISP) = IP
                        NT(IP)      = NT(IP) + NTLOC
                     END IF
                  END IF
                END DO
            END IF
          END DO
#endif
!
! 2.c.7 Check if all served
!
#ifdef W3_DIST
        IF ( MINVAL(IAPPRO(1:NSPEC)) .GT. 0 ) THEN
            EXIT
          ELSE
            NTTARG = NTTARG + 1
            IF ( NTTARG .GE. NTTMAX ) EXIT
            IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,8028)
          END IF
#endif
!
#ifdef W3_DIST
        END DO
      END IF
#endif
!
!!/DEBUGMPI     CALL TEST_MPI_STATUS("Case 14")
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("After Case 14")
#endif
! 2.c.8 Test output
!
#ifdef W3_T
      WRITE (NDST,9020)
      DO IP=1, NAPROC
        WRITE (NDST,9021) IP, NT(IP), NTTARG
        END DO
#endif
!
#ifdef W3_T
      WRITE (NDST,9025)
      DO IK=NK, 1, -1
        WRITE (NDST,9026) IK, (IAPPRO(ITH+(IK-1)*NTH),ITH=1,MIN(24,NTH))
        IF ( NTH .GT. 24 ) WRITE (NDST,9027)                       &
                                 (IAPPRO(ITH+(IK-1)*NTH),ITH=25,NTH)
        END DO
#endif
!
! 2.c.9 Test if any spectral points are left out
!
#ifdef W3_DIST
    IF ((LPDLIB .eqv. .FALSE.).or.(GTYPE .NE. UNGTYPE)) THEN
      DO ISP=1, NSPEC
        IF ( IAPPRO(ISP) .EQ. -1. ) GOTO 829
        END DO
    END IF
#endif
!
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 4'
     FLUSH(740+IAPROC)
#endif
      DEALLOCATE ( NT )
!
! 3.  Model initialization ------------------------------------------- /
! 3.a Read restart file
!
      VA(:,:) = 0.
#ifdef W3_DEBUGMPI
     CALL TEST_MPI_STATUS("Case 15")
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 4.0'
     WRITE(740+IAPROC,*) ' 1: min/max/sum(VA)=', minval(VA), maxval(VA), sum(VA)
     WRITE(740+IAPROC,*) ' 1: NSEAL=', NSEAL
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Before W3IORS call")
#endif
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) ' After ALL_VA_INTEGRAL_PRINT'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("Before W3IORS")
#endif
      CALL W3IORS ( 'READ', NDS(6), SIG(NK), IMOD)
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("After W3IORS")
#endif
#ifdef W3_MEMCHECK
      WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 3a'
      call getMallocInfo(mallinfos)
      call printMallInfo(IAPROC,mallInfos)
#endif

#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) ' 2: min/max/sum(VA)=', minval(VA), maxval(VA), sum(VA)
     WRITE(740+IAPROC,*) ' 2: NSEAL=', NSEAL
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After W3IORS call")
#endif
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 4.1'
     WRITE(740+IAPROC,*) '    sum(VA)=', sum(VA)
     FLUSH(740+IAPROC)
#endif
      FLCOLD = RSTYPE.LE.1  .OR. RSTYPE.EQ.4
      IF ( IAPROC .EQ. NAPLOG ) THEN
          IF (RSTYPE.EQ.0) THEN
              WRITE (NDSO,930) 'cold start (idealized).'
            ELSE IF ( RSTYPE .EQ. 1 ) THEN
              WRITE (NDSO,930) 'cold start (wind).'
            ELSE IF ( RSTYPE .EQ. 4 ) THEN
              WRITE (NDSO,930) 'cold start (calm).'
            ELSE
              WRITE (NDSO,930) 'full restart.'
            END IF
        END IF
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 4.2'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3INIT, step 4.2")
#endif
#endif
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("After restart inits")
#endif

!
! 3.b Compare MAPSTA from grid and restart
!
      DO IX=1, NX
        DO IY=1, NY
          IF ( ABS(MAPSTA(IY,IX)).EQ.2 .OR.                           &
               ABS(MAPTST(IY,IX)).EQ.2 ) THEN
              MAPSTA(IY,IX) = SIGN ( MAPTST(IY,IX) , MAPSTA(IY,IX) )
            END IF
          END DO
        END DO

#ifdef W3_MEMCHECK
      WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 3b'
      call getMallocInfo(mallinfos)
      call printMallInfo(IAPROC,mallInfos)
#endif
!
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 4.3'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3INIT, step 4.3")
#endif
#endif
!
! 3.b2 Set MAPSTA associated to PDLIB
!
#ifdef W3_PDLIB
      IF (GTYPE .eq. UNGTYPE) THEN
        CALL PDLIB_MAPSTA_INIT(IMOD)
      END IF
#endif
!
! 3.c Initialization from wind fields
!
      FLIWND = RSTYPE.EQ.1
#ifdef W3_T
      IF ( FLIWND ) WRITE (NDST,9030)
#endif
!
! 3.d Initialization with calm conditions
!
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 5'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3INIT, step 5")
#endif
#endif
      IF ( RSTYPE .EQ. 4 ) THEN
          VA(:,:) = 0.
#ifdef W3_T
          WRITE (NDST,9031)
#endif
        END IF

#ifdef W3_MEMCHECK
       WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 4'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
! 3.e Prepare propagation scheme
!
      IF ( .NOT. FLCUR ) FLCK = .FALSE.
#ifdef W3_PDLIB
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT definition of FSREFR and FRFREQ'
     WRITE(740+IAPROC,*) 'FSTOTALIMP=', FSTOTALIMP
     WRITE(740+IAPROC,*) 'FSREFRACTION=', FSREFRACTION
     WRITE(740+IAPROC,*) 'FSFREQSHIFT=', FSFREQSHIFT
     WRITE(740+IAPROC,*) 'Before FLCTH=', FLCTH, 'FLCK=', FLCK
#endif
        IF (FSTOTALIMP .and. FSREFRACTION) THEN
          FLCTH = .FALSE.
        END IF
        IF (FSTOTALIMP .and. FSFREQSHIFT) THEN
          FLCK = .FALSE.
        END IF
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) ' After FLCTH=', FLCTH, 'FLCK=', FLCK
#endif
#endif
!
! 4.  Set-up output times -------------------------------------------- *
! 4.a Unpack ODAT
!
      DO J=1, NOTYPE
        J0 = (J-1)*5
        TONEXT(1,J) =        ODAT(J0+1) 
        TONEXT(2,J) =        ODAT(J0+2)
        DTOUT (  J) = REAL ( ODAT(J0+3) )
        TOLAST(1,J) =        ODAT(J0+4) 
        TOLAST(2,J) =        ODAT(J0+5)
      END DO
!
! J=8, second stream of restart files
        J=8
        J0 = (J-1)*5
      IF(ODAT(J0+1) .NE. 0) THEN
        TONEXT(1,J) =        ODAT(J0+1) 
        TONEXT(2,J) =        ODAT(J0+2)
        DTOUT (  J) = REAL ( ODAT(J0+3) )
        TOLAST(1,J) =        ODAT(J0+4) 
        TOLAST(2,J) =        ODAT(J0+5)
        FLOUT(8) = .TRUE.
      ELSE
        FLOUT(8) = .FALSE.
      END IF
!
! 4.b Check if output available
!
      FLOUT(1) = .FALSE.
      FLOGRD   = FLGRD
      FLOGD    = FLGD
      DO J=1, NOGRP
        DO K=1, NGRPP
          FLOUT(1) = FLOUT(1) .OR. FLOGRD(J,K)
        END DO
      END DO
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 6'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3INIT, step 6")
#endif
#endif
!
      FLOUT(7) = .FALSE.
      FLOGR2   = FLGR2
      FLOG2    = FLG2
      DO J=1, NOGRP
        DO K=1, NGRPP
          FLOUT(7) = FLOUT(7) .OR. FLOGR2(J,K)
        END DO
      END DO
!
      FLOUT(2) = NPT .GT. 0
!
      FLOUT(3) = .TRUE.
!
      FLOUT(4) = .TRUE.
!
      FLOUT(5) = FLBPO
      IF ( FLBPO ) THEN
          CALL W3DMO5 ( IMOD, NDSE, NDST, 4 )
        ELSE
          DTOUT(5) = 0.
        END IF
!
      IX0    = MAX (  1, IPRT(1) )
      IXN    = MIN ( NX, IPRT(2) )
      IXS    = MAX (  1, IPRT(3) )
      IY0    = MAX (  1, IPRT(4) )
      IYN    = MIN ( NY, IPRT(5) )
      IYS    = MAX (  1, IPRT(6) )
      FLFORM = PRTFRM
      FLOUT(6) = IX0.LE.IXN .AND. IY0.LE.IYN
!
! 4.c Get first time per output and overall.
!
      TOFRST(1) = -1
      TOFRST(2) =  0
!
!      WRITE(*,*) 'We set NOTYPE=0 just for DEBUGGING'
!      NOTYPE=0 ! ONLY FOR DEBUGGING PURPOSE
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 7'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3INIT, step 7")
#endif
#endif
#ifdef W3_DEBUGINIT
      WRITE(*,*) 'Starting the NOTYPE loop, takes time'
#endif
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("Before NOTYPE loop")
#endif
      DO J=1, NOTYPE
!
! ... check time step  
!
        DTOUT(J) = MAX ( 0. , DTOUT(J) )
        FLOUT(J) = FLOUT(J) .AND. ( DTOUT(J) .GT. 0.5 )
!
! ... get first time 
!
        IF ( FLOUT(J) ) THEN
#ifdef W3_NL5
       IF (J .EQ. 2) TOSNL5 = TONEXT(:, 2)
#endif
            TOUT = TONEXT(:,J)
            TLST = TOLAST(:,J)
!
            DO
              DTTST   = DSEC21 ( TIME , TOUT )
              IF ( ( J.NE.4 .AND. DTTST.LT.0. ) .OR.                  &
                   ( J.EQ.4 .AND. DTTST.LE.0. ) ) THEN
                  CALL TICK21 ( TOUT, DTOUT(J) )
                ELSE
                  EXIT
                END IF
              END DO
!
! ... reset first time
!
            TONEXT(:,J) = TOUT
!
! ... check last time
!
            DTTST  = DSEC21 ( TOUT , TLST )
            IF ( DTTST.LT.0.) FLOUT(J) = .FALSE.
!
! ... check overall first time
!
            IF ( FLOUT(J) ) THEN
                IF ( TOFRST(1).EQ.-1 ) THEN
                    TOFRST = TOUT
                  ELSE
                    DTTST  = DSEC21 ( TOUT , TOFRST )
                    IF ( DTTST.GT.0.) THEN
                        TOFRST = TOUT
                      END IF
                  END IF
              END IF
!
          END IF
!
        END DO
!
! J=8, second stream of restart files
!
      J=8
!
! ... check time step  
!
        DTOUT(J) = MAX ( 0. , DTOUT(J) )
        FLOUT(J) = FLOUT(J) .AND. ( DTOUT(J) .GT. 0.5 )
!
! ... get first time 
!
        IF ( FLOUT(J) ) THEN
            TOUT = TONEXT(:,J)
            TLST = TOLAST(:,J)
!
            DO
              DTTST   = DSEC21 ( TIME , TOUT )
              IF ( ( J.NE.4 .AND. DTTST.LT.0. ) .OR.                  &
                   ( J.EQ.4 .AND. DTTST.LE.0. ) ) THEN
                  CALL TICK21 ( TOUT, DTOUT(J) )
                ELSE
                  EXIT
                END IF
              END DO
!
! ... reset first time
!
            TONEXT(:,J) = TOUT
!
! ... check last time
!
            DTTST  = DSEC21 ( TOUT , TLST )
            IF ( DTTST.LT.0.) FLOUT(J) = .FALSE.
!
! ... check overall first time
!
            IF ( FLOUT(J) ) THEN
                IF ( TOFRST(1).EQ.-1 ) THEN
                    TOFRST = TOUT
                  ELSE
                    DTTST  = DSEC21 ( TOUT , TOFRST )
                    IF ( DTTST.GT.0.) THEN
                        TOFRST = TOUT
                      END IF
                  END IF
              END IF
!
          END IF
! END J=8
!
!
#ifdef W3_MEMCHECK
       WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 5'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

#ifdef W3_DEBUGINIT
      WRITE(*,*) 'Ending the NOTYPE loop, takes time'
#endif
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("After NOTYPE loop")
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 8'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3INIT, step 8.1")
#endif
#endif
!
! 4.d Preprocessing for point output.
!
      IF ( FLOUT(2) ) CALL W3IOPP ( NPT, XPT, YPT, PNAMES, IMOD )
!
#ifdef W3_T
      WRITE (NDST,9040)
      DO J=1, 5
        WRITE (NDST,9041) TONEXT(1,J),TONEXT(2,J),DTOUT(J),FLOUT(J)
        END DO
      WRITE (NDST,9042)
      WRITE (NDST,9043) TOFRST
#endif
!
! 5.  Define wavenumber grid ----------------------------------------- *
! 5.a Calculate depth
! 
#ifdef W3_T
      ALLOCATE ( MAPOUT(NX,NY), XOUT(NX,NY) )
      XOUT = -1.
#endif
!
      MAPTST = MOD(MAPST2/2,2)
      MAPST2 = MAPST2 - 2*MAPTST
#ifdef W3_PDLIB
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Before INIT_GET_JSEA_ISPROC call'
     WRITE(740+IAPROC,*) 'allocated(ISEA_TO_JSEA)=', allocated(ISEA_TO_JSEA)
     WRITE(740+IAPROC,*) 'NAPROC=', NAPROC
     FLUSH(740+IAPROC)
#endif
#endif

!
!Li   For multi-resolution SMC grid, these 1-NX and 1-NY nested loops
!Li   may miss the refined cells as they are not 1-1 corresponding to
!Li   the (Nx,NY) regular grid.  The loop is now modified to run over
!Li   full NSEA points.   JGLi24Jan2012
!Li   DO IY=1, NY
!Li     DO IX=1, NX
!Li       ISEA   = MAPFS(IY,IX)
#ifdef W3_DEBUGSTP
    WRITE(740+IAPROC,*) 'Debugging the SETUP / WLV'
#endif
      DO ISEA=1, NSEA
#ifdef W3_DEBUGSTP
    WRITE(740+IAPROC,*) 'ISEA/WLV/ZB=', ISEA, WLV(ISEA), ZB(ISEA)
#endif
        IX = MAPSF(ISEA,1)
        IY = MAPSF(ISEA,2)
#ifdef W3_T
       MAPOUT(IX,IY) = MAPSTA(IY,IX)
#endif
!Li     IF ( ISEA .NE. 0) THEN
          WLVeff=WLV(ISEA)
#ifdef W3_SETUP
       IF (DO_CHANGE_WLV) THEN
         WLVeff=WLVeff + ZETA_SETUP(ISEA)
       END IF
#endif
          DW(ISEA) = MAX ( 0. , WLVeff-ZB(ISEA) )
#ifdef W3_T
         XOUT(IX,IY) = DW(ISEA)
#endif
          IF ( WLVeff-ZB(ISEA) .LE.0. ) THEN
            MAPTST(IY,IX) = 1
            MAPSTA(IY,IX) = -ABS(MAPSTA(IY,IX))
!!/DEBUGINIT     WRITE(740+IAPROC,*) 'ISEA=', ISEA, ' JSEA=', JSEA
!!/DEBUGINIT     WRITE(740+IAPROC,*) 'NSEA=', NSEA, ' NSEAL=', NSEAL
!!/DEBUGINIT     WRITE(740+IAPROC,*) 'IAPROC=', IAPROC, ' ISPROC=', ISPROC
!!/DEBUGINIT     FLUSH(740+IAPROC)
          END IF
!Li     END IF
      END DO
!Li   END DO
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        WLVeff=WLV(ISEA)
#ifdef W3_SETUP
       IF (DO_CHANGE_WLV) THEN
         WLVeff=WLVeff + ZETA_SETUP(ISEA)
       END IF
#endif
        DW(ISEA) = MAX ( 0. , WLVeff-ZB(ISEA) )
        IF ( WLVeff-ZB(ISEA) .LE.0. ) THEN
!!/DEBUGINIT     WRITE(740+IAPROC,*) 'ISEA=', ISEA, ' JSEA=', JSEA
!!/DEBUGINIT     WRITE(740+IAPROC,*) 'NSEA=', NSEA, ' NSEAL=', NSEAL
!!/DEBUGINIT     WRITE(740+IAPROC,*) 'IAPROC=', IAPROC, ' ISPROC=', ISPROC
!!/DEBUGINIT     FLUSH(740+IAPROC)
          VA(:,JSEA) = 0.
        END IF
      END DO
#ifdef W3_DEBUGSTP
    FLUSH(740+IAPROC)
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 9'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3INIT, step 8.2")
#endif
#endif

!
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 9.1'
     WRITE(740+IAPROC,*) ' allocated(MAPTST)=', allocated(MAPTST)
     WRITE(740+IAPROC,*) 'NY=', NY, ' NX=', NX
     FLUSH(740+IAPROC)
#endif
      MAPST2 = MAPST2 + 2*MAPTST
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 9.2'
     FLUSH(740+IAPROC)
#endif
!
      DEALLOCATE ( MAPTST )
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 9.3'
     FLUSH(740+IAPROC)
#endif

#ifdef W3_MEMCHECK
       WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 6'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 9.4'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_T
      WRITE (NDST,9050)
      NX0    = 1
      DO
        NXN    = MIN ( NX0+NXS-1 , NX )
        CALL PRTBLK (NDST, NX, NY, NX, XOUT, MAPOUT, 0, 0.,        &
                     NX0, NXN, 1, 1, NY, 1, 'Depth', 'm')
        IF ( NXN .NE. NX ) THEN
            NX0    = NX0 + NXS
          ELSE
            EXIT
          END IF
        END DO
      DEALLOCATE ( MAPOUT, XOUT )
#endif
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("Before section 5.b")
#endif
!
! 5.b Fill wavenumber and group velocity arrays.
!
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 9.5'
     FLUSH(740+IAPROC)
#endif
      DO IS=0, NSEA
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'IS=', IS
     FLUSH(740+IAPROC)
#endif
        IF (IS.GT.0) THEN
          DEPTH  = MAX ( DMIN , DW(IS) )
        ELSE
          DEPTH = DMIN
          END IF 
!
#ifdef W3_T1
        WRITE (NDST,9051) IS, DEPTH
#endif
!
        DO IK=0, NK+1
!
!         Calculate wavenumbers and group velocities.
          CALL WAVNU1(SIG(IK),DEPTH,WN(IK,IS),CG(IK,IS))
!
#ifdef W3_T1
          WRITE (NDST,9052) IK, TPI/SIG(IK), WN(IK,IS), CG(IK,IS)
#endif
!
          END DO
        END DO
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 9.6'
     FLUSH(740+IAPROC)
#endif
!
! Commented by FA with version 4.12 
!      DO IK=1, NK
!        CG(IK,0) = CG(IK,1)
!        WN(IK,0) = WN(IK,1)
!        END DO
!
! 6.  Initialize arrays ---------------------------------------------- /
!     Some initialized in W3IORS
!
      UA     = 0.
      UD     = 0.
      U10    = 0.
      U10D   = 0.
!
      AS     = UNDEF
!
      AS    (0) = 0.
      DW    (0) = 0.
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 9.7'
     FLUSH(740+IAPROC)
#endif
!
! 7.  Write info to log file ----------------------------------------- /
!
      IF ( IAPROC .EQ. NAPLOG ) THEN
!
          WRITE (NDSO,970) GNAME
          IF (   FLLEV    ) WRITE (NDSO,971) 'Prescribed'
          IF (.NOT. FLLEV ) WRITE (NDSO,971) 'No'
          IF (   FLCUR    ) WRITE (NDSO,972) 'Prescribed'
          IF (.NOT. FLCUR ) WRITE (NDSO,972) 'No'
          IF (   FLWIND   ) WRITE (NDSO,973) 'Prescribed'
          IF (.NOT. FLWIND) WRITE (NDSO,973) 'No'
          IF (   FLICE    ) WRITE (NDSO,974) 'Prescribed'
          IF (.NOT. FLICE ) WRITE (NDSO,974) 'No'
          IF (   FLTAUA   ) WRITE (NDSO,988) 'Prescribed'
          IF (.NOT. FLTAUA) WRITE (NDSO,988) 'No'
          IF (   FLRHOA   ) WRITE (NDSO,989) 'Prescribed'
          IF (.NOT. FLRHOA) WRITE (NDSO,989) 'No'
!
          IF (   FLMDN    ) WRITE (NDSO,9972) 'Prescribed'
          IF (.NOT. FLMDN ) WRITE (NDSO,9972) 'No'
          IF (   FLMTH    ) WRITE (NDSO,9971) 'Prescribed'
          IF (.NOT. FLMTH ) WRITE (NDSO,9971) 'No'
          IF (   FLMVS    ) WRITE (NDSO,9970) 'Prescribed'
          IF (.NOT. FLMVS ) WRITE (NDSO,9970) 'No'

          IF (   FLIC1    ) WRITE (NDSO,9973) 'Prescribed'
          IF (.NOT. FLIC1 ) WRITE (NDSO,9973) 'No'
          IF (   FLIC2    ) WRITE (NDSO,9974) 'Prescribed'
          IF (.NOT. FLIC2 ) WRITE (NDSO,9974) 'No'
          IF (   FLIC3    ) WRITE (NDSO,9975) 'Prescribed'
          IF (.NOT. FLIC3 ) WRITE (NDSO,9975) 'No'
          IF (   FLIC4    ) WRITE (NDSO,9976) 'Prescribed'
          IF (.NOT. FLIC4 ) WRITE (NDSO,9976) 'No'
          IF (   FLIC5    ) WRITE (NDSO,9977) 'Prescribed'
          IF (.NOT. FLIC5 ) WRITE (NDSO,9977) 'No'

          IF ( FLOUT(1) ) THEN
              WRITE (NDSO,975)
              DO J=1,NOGRP
              DO K=1,NGRPP
                IF ( FLOGRD(J,K) ) WRITE (NDSO,976) IDOUT(J,K)
                END DO
                END DO
            END IF
!
          IF ( FLOUT(7) ) THEN
              WRITE (NDSO,987)
              DO J=1,NOGRP
              DO K=1,NGRPP
                IF ( FLOGR2(J,K) ) WRITE (NDSO,976) IDOUT(J,K)
                END DO
                END DO
            END IF
!
          IF ( FLOUT(2) ) THEN
              WRITE (NDSO,977) NOPTS
              IF ( NOPTS .EQ. 0 ) THEN
                  WRITE (NDSO,978)
                ELSE
                  IF ( FLAGLL ) THEN
                      WRITE (NDSO,979)
                    ELSE
                      WRITE (NDSO,985)
                    END IF
                  DO IP=1, NOPTS
                    IF ( FLAGLL ) THEN
                        WRITE (NDSO,980) IP, FACTOR*PTLOC(1,IP),      &
                                         FACTOR*PTLOC(2,IP), PTNME(IP)
                      ELSE
                        WRITE (NDSO,986) IP, FACTOR*PTLOC(1,IP),      &
                                         FACTOR*PTLOC(2,IP), PTNME(IP)
                      END IF
                    END DO
                END IF
            END IF
!
          CALL STME21 ( TIME , DTME21 )
          WRITE (NDSO,981) DTME21
          IF (FLLEV) THEN
              CALL STME21 ( TLEV , DTME21 )
              WRITE (NDSO,982) DTME21
            END IF
          IF (FLICE) THEN
              CALL STME21 ( TICE , DTME21 )
              WRITE (NDSO,983) DTME21
            END IF
          IF (FLRHOA) THEN
              CALL STME21 ( TRHO , DTME21 )
              WRITE (NDSO,990) DTME21
            END IF
!
          WRITE (NDSO,984)
!
        END IF
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 9.8'
     FLUSH(740+IAPROC)
#endif
!
      IF ( NOPTS .EQ. 0 ) FLOUT(2) = .FALSE.

#ifdef W3_MEMCHECK
       WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_INIT SECTION 7'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 9.9'
     FLUSH(740+IAPROC)
#endif
!
! Boundary set up for the directions
!
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3INIT, step 8.3")
#endif
#endif
!!/PDLIB         CALL VA_SETUP_IOBPD
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3INIT, step 8.4")
#endif
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'W3INIT, aft BLOCK_SOLVER_INIT, step 9.10'
     FLUSH(740+IAPROC)
#endif
!
! 8.  Final MPI set up ----------------------------------------------- /
!
#ifdef W3_MPI
      CALL W3MPII ( IMOD )
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'After W3MPII'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
      CALL W3MPIO ( IMOD )
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'After W3MPIO'
     FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
      IF ( FLOUT(2) ) CALL W3MPIP ( IMOD )
#endif
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'After W3MPIP'
     FLUSH(740+IAPROC)
#endif
!
#ifdef W3_PDLIB
#ifdef W3_DEBUGINIT
         CALL PRINT_WN_STATISTIC("W3INIT leaving")
#endif
#endif
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("Leaving W3INIT")
#endif
      RETURN
!
! Escape locations read errors :
!
#ifdef W3_DIST
  820 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,8020) NSEA, NAPROC
      CALL EXTCDE ( 820 )
#endif
!
#ifdef W3_DIST
  821 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,8021) NSPEC, NAPROC
      CALL EXTCDE ( 821 )
#endif
!
#ifdef W3_DIST
  829 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,8029)
      CALL EXTCDE ( 829 )
#endif

!
  888 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,8000) IERR
      CALL EXTCDE ( 1 )
!
  889 CONTINUE
! === no process number filtering for test file !!! ===
      WRITE (NDSE,8001) IERR
      CALL EXTCDE ( 2 )
!
! Formats
!
  900 FORMAT ( ' WAVEWATCH III log file            ',                 &
               '                     version ',A/                     &
               ' ==================================',                 &
               '==================================='/                 &
               50X,'date : ',A10/50X,'time :  ',A8)
  920 FORMAT (/' Model definition file read.')
  930 FORMAT ( ' Restart file read; ',A)
!
  970 FORMAT (/' Grid name : ',A)
  971 FORMAT (/' ',A,' water levels.')
  972 FORMAT ( ' ',A,' curents.')
  973 FORMAT ( ' ',A,' winds.')
  974 FORMAT ( ' ',A,' ice fields.')
  988 FORMAT ( ' ',A,' momentum')
  989 FORMAT ( ' ',A,' air density')
  9972 FORMAT( ' ',A,' mud density.')
  9971 FORMAT( ' ',A,' mud thickness.')
  9970 FORMAT( ' ',A,' mud viscosity.')
  9973 FORMAT( ' ',A,' ice parameter 1')
  9974 FORMAT( ' ',A,' ice parameter 2')
  9975 FORMAT( ' ',A,' ice parameter 3')
  9976 FORMAT( ' ',A,' ice parameter 4')
  9977 FORMAT( ' ',A,' ice parameter 5')

!
  975 FORMAT (/' Gridded output fields : '/                           &
               '--------------------------------------------------')
  976 FORMAT ( '     ',A)
!
  977 FORMAT (/' Point output requested for',I6,' points : '/         &
               '------------------------------------------')
  978 FORMAT (/'      Point output disabled')
  979 FORMAT                                                     &
        (/'      point  |  longitude  |   latitude  |  name  '/  &
     '     --------|-------------|-------------|----------------')
  985 FORMAT                                                     &
        (/'      point  |      X      |      Y      |  name  '/  &
     '     --------|-------------|-------------|----------------')
  980 FORMAT ( 5X,I5,'   |',2(F10.2,'   |'),2X,A)
  986 FORMAT ( 5X,I5,'   |',2(F8.1,'E3   |'),2X,A)
!
  981 FORMAT (/' Initial time     : ',A)
  982 FORMAT ( ' Water level time : ',A)
  983 FORMAT ( ' Ice field time   : ',A)
  990 FORMAT ( ' Air density time : ',A)
!
  984 FORMAT (//                                                      &
        37X,'  |         input         |     output    |'/            &
        37X,'  |-----------------------|---------------|'/            &
         2X,'   step | pass |    date      time   |',                 &
              ' b w l c t r i i1 i5 d | g p t r b f c |'/             &
         2X,'--------|------|---------------------|',                 &
            '-------------------|---------------|'/                   &
         2X,'--------+------+---------------------+',                 &
            '-------------------+---------------+')
  987 FORMAT (/' Coupling output fields : '/                          &
               '--------------------------------------------------')
!
 8000 FORMAT (/' *** WAVEWATCH III ERROR IN W3INIT : '/               &
               '     ERROR IN OPENING LOG FILE'/                      &
               '     IOSTAT =',I5/)
 8001 FORMAT (/' *** WAVEWATCH III ERROR IN W3INIT : '/               &
               '     ERROR IN OPENING TEST FILE'/                     &
               '     IOSTAT =',I5/)
 8002 FORMAT (/' *** WAVEWATCH III WARNING IN W3INIT : '/             &
               '     SIGNIFICANT PART OF RESOURCES RESERVED FOR',     &
                   ' OUTPUT :',F6.1,'%'/)
#ifdef W3_DIST
 8020 FORMAT (/' *** WAVEWATCH III ERROR IN W3INIT : '/         &
         '     NUMBER OF SEA POINTS LESS THAN NUMBER OF PROC.'/ &
         '     NSEA, NAPROC =',2I8/)
 8021 FORMAT (/' *** WAVEWATCH III ERROR IN W3INIT : '/         &
    '     NUMBER OF SPECTRAL POINTS LESS THAN NUMBER OF PROC.'/ &
         '     NSPEC, NAPROC =',2I8/)
 8028 FORMAT (/' *** WAVEWATCH III WARNING IN W3INIT : '/       &
         '     INCREASING TARGET IN MPP PROPAGATION MAP.'/      &
         '     IMBALANCE BETWEEN OVERALL AND CFL TIME STEPS'/)
 8029 FORMAT (/' *** WAVEWATCH III ERROR IN W3INIT : '/         &
         '     SOMETHING WRONG WITH MPP PROPAGATION MAP.'/      &
         '     CALL HENDRIK !!!'/)
#endif
!
#ifdef W3_T
 9000 FORMAT ( 'TEST W3INIT: MOD. NR. AND FILE EXT.: ',I4,' [',A,']')
 9001 FORMAT ( '             NR. OF PROCESSORS     : ',3I4/        &
               '             ASSIGNED PROCESSORS     ',9I4)
 9002 FORMAT ( '             DATA SET NUMBERS      : ',4I4)
 9003 FORMAT ( '             LOG FILE              : [',A,']'/     &
               '             TEST FILE             : [',A,']')
#endif
!
#ifdef W3_T
 9020 FORMAT (' TEST W3INIT : IP, NTTOT, NTTARG :')
 9021 FORMAT ( '         ',3I8)
 9025 FORMAT (' TEST W3INIT : MPP PROPAGATION MAP SPECTRAL COMP.')
 9026 FORMAT (4X,I4,2X,24I4)
 9027 FORMAT (10X,24I4)
#endif
!
#ifdef W3_T
 9030 FORMAT (' TEST W3INIT : INITIALIZATION USING WINDS, ',       &
               'PERFORMED IN W3WAVE')
 9031 FORMAT (' TEST W3INIT : STARTING FROM CALM CONDITIONS')
#endif
!
#ifdef W3_T
 9040 FORMAT (' TEST W3INIT : OUTPUT DATA, FIRST TIME, STEP, FLAG')
 9041 FORMAT ('              ',I9.8,I7.6,F8.1,3X,L1)
 9042 FORMAT (' TEST W3INIT : FIRST TIME :')
 9043 FORMAT ('              ',I9.8,I7.6)
#endif
!
#ifdef W3_T
 9050 FORMAT (' TEST W3INIT : INITIAL DEPTHS')
#endif
#ifdef W3_T1
 9051 FORMAT (' TEST W3INIT : ISEA =',I6,'  DEPTH =',F7.1,         &
              '  IK, T, K, CG :')
 9052 FORMAT ('               ',I3,F8.2,F8.4,F8.2)
#endif
!/
!/ End of W3INIT ----------------------------------------------------- /
!/
      END SUBROUTINE W3INIT
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3MPII ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         11-May-2007 |
!/                  +-----------------------------------+
!/
!/    04-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    13-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/                  Taken out of W3WAVE.
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    13-Jun-2006 : Splitting STORE in G/SSTORE.        ( version 3.09 )
!/    11-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/
!  1. Purpose :
!
!     Perform initializations for MPI version of model.
!     Data transpose only.
!
!  2. Method :
!
!     Some derived data types are defined.  All communiction in
!     W3GATH, W3SCAT and W3WAVE are initialized so that all
!     communication can be performed with single MPI_STARTALL,
!     MPI_TESTALL and MPI_WAITALL calls.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!
!      MPI_TYPE_VECTOR, MPI_TYPE_COMMIT
!                Subr. mpif.h   MPI derived data type routines.
!
!      MPI_SEND_INIT, MPI_RECV_INIT
!                Subr. mpif.h   MPI persistent communication calls.
!     ----------------------------------------------------------------
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
!     None.
!
!  7. Remarks :
!
!     - Basic MPP set up partially performed in W3INIT.
!     - Each processor has to be able to send out individual error
!       messages in this routine !
!     - No testing on IMOD, since only called by W3INIT.
!     - In version 3.09 STORE was split into a send and receive 
!       buffer, to avoid/reduce possible conflicts between the FORTRAN
!       and MPI standards when a gather is posted in a given buffer
!       right after a send is completed.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   MPI communication calls.
!
!       !/S     Subroutine tracing,
!       !/T     Test output, general.
!       !/MPIT  Test output, MPI communications details.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE W3GDATMD, ONLY: NSEA
      USE W3ADATMD, ONLY: NSEALM
      USE W3GDATMD, ONLY: GTYPE, UNGTYPE
      USE CONSTANTS, ONLY: LPDLIB
#ifdef W3_MPI
      USE W3GDATMD, ONLY: NSPEC
      USE W3WDATMD, ONLY: VA
      USE W3ADATMD, ONLY: MPI_COMM_WAVE, WW3_FIELD_VEC,         &
                          WW3_SPEC_VEC, IAPPRO, WADATS,         &
                          NRQSG1, IRQSG1, NRQSG2, IRQSG2,       &
                          GSTORE, SSTORE, MPIBUF, BSTAT,        &
                          BISPL, ISPLOC, IBFLOC, NSPLOC
#endif
      USE W3ODATMD, ONLY: NDST, NAPROC, IAPROC
!/
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
      INTEGER                 :: NXXXX
#ifdef W3_MPI
      INTEGER                 :: IERR_MPI, ISP, IH, ITARG,       &
                                 IERR1, IERR2, IP
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3MPII')
#endif
!
! 1.  Set up derived data types -------------------------------------- /
!
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3MPII, step 1'
      FLUSH(740+IAPROC)
#endif
      NXXXX  = NSEALM * NAPROC
!
#ifdef W3_MPI
      CALL MPI_TYPE_VECTOR ( NSEALM, 1, NAPROC, MPI_REAL,        &
                             WW3_FIELD_VEC, IERR_MPI )
#endif
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3MPII, step 1'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
      CALL MPI_TYPE_VECTOR ( NSEALM, 1, NSPEC, MPI_REAL,         &
                             WW3_SPEC_VEC, IERR_MPI )
#endif
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3MPII, step 1'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
      CALL MPI_TYPE_COMMIT ( WW3_FIELD_VEC, IERR_MPI )
#endif
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3MPII, step 1'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
      CALL MPI_TYPE_COMMIT ( WW3_SPEC_VEC, IERR_MPI )
#endif
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3MPII, step 1'
      FLUSH(740+IAPROC)
#endif
!
#ifdef W3_MPIT
      WRITE (NDST,9010) WW3_FIELD_VEC, WW3_SPEC_VEC
#endif
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3MPII, step 1'
      FLUSH(740+IAPROC)
#endif
!
#ifdef W3_MPI
      IF( IAPROC .GT. NAPROC ) THEN
          NSPLOC = 0
          NRQSG1 = 0
          NRQSG2 = 0
#endif
#ifdef W3_MPIT
          WRITE (NDST,9011)
#endif
#ifdef W3_MPI
          RETURN
        END IF
#endif
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3MPII, step 1'
      FLUSH(740+IAPROC)
#endif
!
! 2.  Set up scatters and gathers for W3WAVE ------------------------- /
!     ( persistent communication calls )
!
#ifdef W3_DIST
   IF ((LPDLIB .eqv. .FALSE.).or.(GTYPE .NE. UNGTYPE)) THEN
#endif
#ifdef W3_MPI
      NSPLOC = 0
      DO ISP=1, NSPEC
        IF ( IAPPRO(ISP) .EQ. IAPROC ) NSPLOC = NSPLOC + 1
        END DO
#endif
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3MPII, step 1'
      FLUSH(740+IAPROC)
#endif
!
#ifdef W3_MPI
      NRQSG1 = NSPEC - NSPLOC
      ALLOCATE ( WADATS(IMOD)%IRQSG1(MAX(1,NRQSG1),2) )
      IRQSG1 => WADATS(IMOD)%IRQSG1
      IH     = 0
#endif
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3MPII, step 1'
      FLUSH(740+IAPROC)
#endif
!
#ifdef W3_MPIT
      WRITE (NDST,9021)
#endif
#ifdef W3_DEBUGINIT
          WRITE(*,*) 'Before VA MPI_SEND/RECV_INIT inits'
#endif
#ifdef W3_MPI
      DO ISP=1, NSPEC
        IF ( IAPPRO(ISP) .NE. IAPROC ) THEN
            ITARG  = IAPPRO(ISP) - 1
            IH     = IH + 1
            CALL MPI_SEND_INIT ( VA(ISP,1), 1, WW3_SPEC_VEC,     &
                 ITARG, ISP, MPI_COMM_WAVE, IRQSG1(IH,1), IERR1 )
            CALL MPI_RECV_INIT ( VA(ISP,1), 1, WW3_SPEC_VEC,     &
                 ITARG, ISP, MPI_COMM_WAVE, IRQSG1(IH,2), IERR2 )
#endif
#ifdef W3_MPIT
            WRITE (NDST,9022) IH, ISP, ITARG+1,                 &
                   IRQSG1(IH,1), IERR1, IRQSG1(IH,2), IERR2
#endif
#ifdef W3_MPI
          END IF
        END DO
#endif
#ifdef W3_DEBUGINIT
           WRITE(*,*) 'After VA MPI_SEND/RECV_INIT inits'
#endif
#ifdef W3_MPIT
      WRITE (NDST,9023)
      WRITE (NDST,9020) NRQSG1
#endif
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3MPII, step 1'
      FLUSH(740+IAPROC)
#endif
!
! 3.  Set up scatters and gathers for W3SCAT and W3GATH -------------- /
!     Also set up buffering of data.
!
#ifdef W3_MPI
      NRQSG2 = MAX( 1 , NAPROC-1 )
      ALLOCATE ( WADATS(IMOD)%IRQSG2(NRQSG2*NSPLOC,2),           &
                 WADATS(IMOD)%GSTORE(NAPROC*NSEALM,MPIBUF),      &
                 WADATS(IMOD)%SSTORE(NAPROC*NSEALM,MPIBUF) )
      NRQSG2 = NAPROC - 1
#endif
!
#ifdef W3_MPI
      IRQSG2 => WADATS(IMOD)%IRQSG2
      GSTORE => WADATS(IMOD)%GSTORE
      SSTORE => WADATS(IMOD)%SSTORE
#endif
!
#ifdef W3_MPI
      IH     = 0
      ISPLOC = 0
      IBFLOC = 0
      WADATS(IMOD)%GSTORE = 0.
      WADATS(IMOD)%SSTORE = 0.
#endif
!
! 3.a Loop over local spectral components
!
#ifdef W3_MPIT
      WRITE (NDST,9031)
#endif
!
#ifdef W3_MPI
      DO ISP=1, NSPEC
        IF ( IAPPRO(ISP) .EQ. IAPROC ) THEN
#endif
!
#ifdef W3_MPI
            ISPLOC = ISPLOC + 1
            IBFLOC = IBFLOC + 1
            IF ( IBFLOC .GT. MPIBUF ) IBFLOC = 1
#endif
!
! 3.b Loop over non-local processes
!
#ifdef W3_MPI
            DO IP=1, NAPROC
              IF ( IP .NE. IAPROC ) THEN
#endif
!
#ifdef W3_MPI
                  ITARG  = IP - 1
                  IH     = IH + 1
#endif
!
#ifdef W3_MPI
                  CALL MPI_RECV_INIT                             &
                     ( WADATS(IMOD)%GSTORE(IP,IBFLOC), 1,        &
                       WW3_FIELD_VEC, ITARG, ISP, MPI_COMM_WAVE, &
                       IRQSG2(IH,1), IERR2 )
                  CALL MPI_SEND_INIT                             &
                     ( WADATS(IMOD)%SSTORE(IP,IBFLOC), 1,        &
                       WW3_FIELD_VEC, ITARG, ISP, MPI_COMM_WAVE, &
                       IRQSG2(IH,2), IERR2 )
#endif
#ifdef W3_MPIT
                  WRITE (NDST,9032) IH, ISP, ITARG+1, IBFLOC,   &
                         IRQSG2(IH,1), IERR1, IRQSG2(IH,2), IERR2
#endif
!
! ... End of loops
!
#ifdef W3_MPI
                END IF
              END DO
#endif
!
#ifdef W3_MPI
          END IF
        END DO
#endif
!
#ifdef W3_MPIT
      WRITE (NDST,9033)
      WRITE (NDST,9030) NSPLOC, NRQSG2, IH
#endif
!
! 4.  Initialize buffer management ----------------------------------- /
!
#ifdef W3_MPI
      BSTAT  = 0
      BISPL  = 0
      ISPLOC = 0
      IBFLOC = 0
#endif
!
#ifdef W3_DIST
   END IF
#endif
      RETURN
!
! Format statements
!
#ifdef W3_MPIT
 9010 FORMAT ( ' TEST W3MPII: DATA TYPES DEFINED'/     &
               '              WW3_FIELD_VEC : ',I10/   &
               '              WW3_SPEC_VEC  : ',I10)
 9011 FORMAT ( ' TEST W3MPII: NO COMPUTATIONS ON THIS NODE')
#endif
!
#ifdef W3_MPIT
 9020 FORMAT ( ' TEST W3MPII: W3WAVE COMM. SET UP FINISHED'/    &
               '              NRQSG1        : ',I10)
 9021 FORMAT (/' TEST W3MPII: COMMUNICATION CALLS FOR W3WAVE '/ &
       ' +------+------+------+--------------+--------------+'/ &
       ' |  IH  |  ISP | TARG |    SCATTER   |    GATHER    |'/ &
       ' |      |      |      |   handle err |   handle err |'/ &
       ' +------+------+------+--------------+--------------+')
 9022 FORMAT ( ' |',3(I5,' |'),2(I9,I4,' |'))
 9023 FORMAT (                                                  &
       ' +------+------+------+--------------+--------------+'/)
#endif
!
#ifdef W3_MPIT
 9030 FORMAT ( ' TEST W3MPII: GATH/SCAT COMM. SET UP FINISHED'/ &
               '              NSPLOC        : ',I10/            &
               '              NRQSG2        : ',I10/            &
               '              TOTAL REQ.    : ',I10/)
 9031 FORMAT (/' TEST W3MPII: COMM. CALLS FOR W3GATH/W3SCAT '/  &
               ' +------+------+------+------+--------------+', &
               '--------------+'/                               &
               ' |  IH  |  ISP | TARG | IBFR |     GATHER   |', &
               '    SCATTER   |'/                               &
               ' |      |      |      |      |   handle err |', &
               '   handle err |'/                               &
               ' +------+------+------+------+--------------+', &
               '--------------+')
 9032 FORMAT ( ' |',4(I5,' |'),2(I9,I4,' |'))
 9033 FORMAT ( ' +------+------+------+------+--------------+', &
               '--------------+'/)
#endif
!/
!/ End of W3MPII ----------------------------------------------------- /
!/
      END SUBROUTINE W3MPII
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3MPIO ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         11-Nov-2015 |
!/                  +-----------------------------------+
!/
!/    17-Mar-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    11-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    20-Aug-2003 : Output server options added.        ( version 3.04 )
!/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/                  Taken out of W3WAVE.
!/    03-Jan-2005 : Add US2x to MPI communication.      ( version 3.06 )
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    21-Jul-2005 : Add output fields.                  ( version 3.07 )
!/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    02-Aug-2006 : W3MPIP split off.                   ( version 3.10 )
!/    02-Apr-2007 : Add partitioned field data.         ( version 3.11 )
!/                  Add user-defined field data.
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
!/    25-Dec-2012 : Modify field output MPI for new     ( version 4.11 )
!/                  structure and smaller memory footprint.
!/    02-Jul-2013 : Bug fix MPI_FLOAT -> MPI_REAL.      ( version 4.11 )
!/    11-Nov-2015 : Added ICEF                          ( version 5.08 )
!/
!  1. Purpose :
!
!     Prepare MPI persistent communication needed for WAVEWATCH I/O
!     routines.
!
!  2. Method :
!
!     Create handles as needed.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3XDMA    Subr. W3ADATMD Dimension expanded output arrays.
!      W3SETA    Subr.    "     Set pointers for output arrays
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!
!      MPI_SEND_INIT, MPI_RECV_INIT
!                Subr. mpif.h   MPI persistent communication calls.
!     ----------------------------------------------------------------
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
!  7. Remarks :
!
!     - The communication as set up in W3MPII uses tags with number
!       ranging from 1 through NSPEC. New and unique tags for IO
!       related communication are assigned here dynamically.
!     - No testing on IMOD, since only called by W3INIT.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/MPI   MPI communication calls.
!
!       !/S     Enable subroutine tracing.
!       !/MPIT  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
#ifdef W3_MPI
      USE W3ADATMD, ONLY: W3XDMA, W3SETA, W3XETA
      USE W3IORSMD, ONLY: OARST
#endif
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      USE W3GDATMD, ONLY: NSEA
      USE W3ADATMD, ONLY: NSEALM
#ifdef W3_MPI
      USE W3GDATMD, ONLY: NX, NSPEC, MAPFS, E3DF, P2MSF, US3DF, USSPF
      USE W3WDATMD, ONLY: VA, UST, USTDIR, ASF, FPIS, ICEF
      USE W3ADATMD, ONLY: MPI_COMM_WAVE, WW3_FIELD_VEC
      USE W3ADATMD, ONLY: HS, WLM, T02
#endif


#ifdef W3_MPI
      USE W3ADATMD, ONLY: T0M1, THM, THS, FP0, THP0, FP1, THP1,   &
                          DTDYN, FCUT, SPPNT, ABA, ABD, UBA, UBD,&
                          SXX, SYY, SXY, USERO, PHS, PTP, PLP,   &
                          PDIR, PSI, PWS, PWST, PNR, PHIAW, PHIOC,&
                          TUSX, TUSY, TAUWIX, TAUWIY, TAUOX,     &
                          TAUOY, USSX, USSY, MSSX, MSSY, MSSD,   &
                          MSCX, MSCY, MSCD, PRMS, TPMS, CHARN,   &
                          TWS, TAUWNX, TAUWNY, BHD, CGE,         &
                          CFLXYMAX, CFLTHMAX, CFLKMAX, WHITECAP, &
                          BEDFORMS, PHIBBL, TAUBBL, T01,         &
                          P2SMS, US3D, EF,  TH1M, STH1M, TH2M,   &
                          STH2M, HSIG, PHICE, TAUICE, USSP,      &
                          STMAXE, STMAXD, HMAXE, HCMAXE, HMAXD,  &
                          HCMAXD, QP, PTHP0, PQP, PPE, PGW, PSW, &
                          PTM1, PT1, PT2, PEP, WBT, CX, CY,      &
                          TAUOCX, TAUOCY, WNMEAN
#endif

#ifdef W3_MPI
      USE W3GDATMD, ONLY: NK
      USE W3ODATMD, ONLY: NDST, IAPROC, NAPROC, NTPROC, FLOUT,   &
                          NAPFLD, NAPPNT, NAPRST, NAPBPT, NAPTRK,&
                          NOGRP, NGRPP, NOGE, FLOGRR
      USE W3ODATMD, ONLY: OUTPTS, NRQGO, NRQGO2, IRQGO, IRQGO2,  &
                          FLOGRD, NRQPO, NRQPO2, IRQPO1, IRQPO2, &
                          NOPTS, IPTINT, NRQRS, IRQRS, NBLKRS,   &
                          RSBLKS, IRQRSS, VAAUX, NRQBP, NRQBP2,  &
                          IRQBP1, IRQBP2, NFBPO, NBO2, ISBPO,    &
                          ABPOS, NRQTR, IRQTR, IT0PNT, IT0TRK,   &
                          IT0PRT, NOSWLL, NOEXTR, NDSE, IOSTYP,  &
                          FLOGR2
      USE W3PARALL, ONLY : INIT_GET_JSEA_ISPROC
#endif
      USE W3GDATMD, ONLY: GTYPE, UNGTYPE
      USE CONSTANTS, ONLY: LPDLIB
!/
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
#ifdef W3_MPI
      INTEGER                 :: IK, IFJ
      INTEGER                 :: IH, IT0, IROOT, IT, IERR, I0,   &
                                 IFROM, IX(4), IY(4), IS(4),     &
                                 IP(4), I, J, JSEA, ITARG, IB,   &
                                 JSEA0, JSEAN, NSEAB, IBOFF,     &
                                 ISEA, ISPROC, K, NRQMAX
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT
#endif
#ifdef W3_MPI
      LOGICAL                 :: FLGRDALL(NOGRP,NGRPP)
      LOGICAL                 :: FLGRDARST(NOGRP,NGRPP)
#endif
#ifdef W3_MPIT
      CHARACTER(LEN=5)      :: STRING
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3MPIO')
#endif
!
! 1.  Set-up for W3IOGO ---------------------------------------------- /
!
#ifdef W3_MPI
      DO J=1, NOGRP
        DO K=1, NGRPP
          FLGRDALL (J,K) =  (FLOGRD(J,K) .OR. FLOGR2(J,K))
          FLGRDARST(J,K) =  (FLGRDALL(J,K) .OR. FLOGRR(J,K))
          END DO
        END DO
#endif
!
#ifdef W3_MPI
      NRQGO  = 0
      NRQGO2 = 0
      IT0    = NSPEC
      IROOT  = NAPFLD - 1
#endif
!
!
#ifdef W3_MPI
      IF ((FLOUT(1) .OR. FLOUT(7)).and.(.not. LPDLIB .or.       &
          (GTYPE .ne. UNGTYPE).or. .TRUE.)) THEN
#endif
!
! NRQMAX is the maximum number of output fields that require MPI communication,
! aimed to gather field values stored in each processor into one processor in
! charge of model output; for each of such fields, this routine requires one
! call to MPI_SEND_INIT and MPI_RECV_INIT storing the communication request
! handles in the vectors IRQGO and IRQGO2 respectively.
! NRQMAX is calculated as the sum of all fields described before (Hs)
!    + 2 or 3 component fields (CUR) + 3 component fields + extra fields
! For group 1 fields except ICEF, all processors contain information on all
! grid points because they are input fields, and therefore this MPI
! communication is not necessary and they do not contribute to NRQMAX.
!
#ifdef W3_MPI
          ! Calculation of NRQMAX splitted by output groups and field type
          !       scalar                2-comp   3-comp
          NRQMAX =   1                +    0  +    0  +  &  ! group 1 
                    18                +    0  +    0  +  &  ! group 2
                     0                +    0  +    0  +  &  ! group 3 (extra contributions below)
             2+(NOGE(4)-2)*(NOSWLL+1) +    0  +    0  +  &  ! group 4
                    11                +    3  +    1  +  &  ! group 5
                    12                +    7  +    1  +  &  ! group 6 (extra contributions below)
                     5                +    4  +    1  +  &  ! group 7
                     5                +    2  +    0  +  &  ! group 8
                     5                +    0  +    0  +  &  ! group 9
                NOEXTR                +    0  +    0        ! group 10

          ! Extra contributions to NRQMAX from group 3
          DO IFJ=1,5
            IF ( FLGRDALL( 3,IFJ)) NRQMAX = NRQMAX +               &
                                    E3DF(3,IFJ) - E3DF(2,IFJ) + 1
            END DO
          ! Extra contributions to NRQMAX from group 6
          IF ( FLGRDALL( 6,9)) NRQMAX = NRQMAX +               &
                                          P2MSF(3) - P2MSF(2) + 1
          IF ( FLGRDALL( 6, 8) ) NRQMAX = NRQMAX + 2*NK
          IF ( FLGRDALL( 6,12) ) NRQMAX = NRQMAX + 2*NK
#endif
!
#ifdef W3_MPI
          IF ( NRQMAX .GT. 0 ) THEN 
              ALLOCATE ( OUTPTS(IMOD)%OUT1%IRQGO(NRQMAX) )
              ALLOCATE ( OUTPTS(IMOD)%OUT1%IRQGO2(NRQMAX*NAPROC) )
            END IF
          IRQGO  => OUTPTS(IMOD)%OUT1%IRQGO
          IRQGO2 => OUTPTS(IMOD)%OUT1%IRQGO2
#endif
!
! 1.a Sends of fields
!
#ifdef W3_MPI
          IH     = 0
#endif
!
#ifdef W3_MPI
          IF ( IAPROC .LE. NAPROC ) THEN
              IT     = IT0
#endif
#ifdef W3_MPIT
              WRITE (NDST,9010) '(SEND)'
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 1, 9) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (ICEF (IAPROC), 1, WW3_FIELD_VEC,    &
                                IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 1/09', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 1) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (HS   (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 2) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (WLM  (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/02', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 3) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (T02  (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/03', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 4) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (T0M1  (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/04', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 5) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (T01  (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/05', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 6) .OR. FLGRDALL( 2,18) ) THEN
                  ! TP output shares FP0 internal field with FP
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (FP0  (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/06', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 7) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (THM  (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/07', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 8) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (THS  (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/09', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 9) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (THP0 (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/09', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 10) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (HSIG (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/10', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 11) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (STMAXE (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/11', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 12) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (STMAXD (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/12', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 13) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (HMAXE (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/13', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 14) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (HCMAXE (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/14', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 15) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (HMAXD (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/15', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 16) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (HCMAXD (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/16', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 17) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (WBT  (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/17', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 2, 19) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (WNMEAN(1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/19', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 3, 1) ) THEN 
                  DO IK=E3DF(2,1),E3DF(3,1)
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_SEND_INIT (EF(1,IK),NSEALM , MPI_REAL, IROOT, &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'EF', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                 END IF
#endif
!      
#ifdef W3_MPI
              IF ( FLGRDALL( 3, 2) ) THEN 
                  DO IK=E3DF(2,2),E3DF(3,2)
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_SEND_INIT (TH1M(1,IK),NSEALM , MPI_REAL, IROOT, &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'TH1M', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                 END IF
#endif
!      
#ifdef W3_MPI
              IF ( FLGRDALL( 3, 3) ) THEN 
                  DO IK=E3DF(2,3),E3DF(3,3)
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_SEND_INIT (STH1M(1,IK),NSEALM , MPI_REAL, IROOT, &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'STH1M', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                 END IF
#endif
!       
#ifdef W3_MPI
              IF ( FLGRDALL( 3, 4) ) THEN 
                  DO IK=E3DF(2,4),E3DF(3,4)
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_SEND_INIT (TH2M(1,IK),NSEALM , MPI_REAL, IROOT, &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'TH2M', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                 END IF
#endif
!      
#ifdef W3_MPI
              IF ( FLGRDALL( 3, 5) ) THEN 
                  DO IK=E3DF(2,5),E3DF(3,5)
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_SEND_INIT (STH2M(1,IK),NSEALM , MPI_REAL, IROOT, &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'STH2M', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                 END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4, 1) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PHS(1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4, 2) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PTP(1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/02', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4, 3) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PLP(1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/03', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4, 4) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PDIR(1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/04', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4, 5) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PSI(1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/05', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4, 6) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PWS(1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/06', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4, 7) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PTHP0(1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/07', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4, 8) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PQP (1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/08', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4, 9) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PPE (1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/09', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4,10) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PGW (1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/10', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4,11) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PSW (1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/11', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4,12) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PTM1(1,K),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/12', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4,13) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PT1 (1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/13', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4,14) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PT2 (1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/14', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4,15) ) THEN
                DO K=0, NOSWLL
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PEP (1,K),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/15', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4,16) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PWST (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/16', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 4,17) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PNR  (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/17', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 5, 1) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (UST   (IAPROC), 1, WW3_FIELD_VEC,      &
                       IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (USTDIR(IAPROC), 1, WW3_FIELD_VEC,       &
                       IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (ASF   (IAPROC), 1, WW3_FIELD_VEC,       &
                       IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF 
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 5, 2) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (CHARN(1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/02', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 5, 3) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (CGE  (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/03', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 5, 4) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PHIAW(1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/04', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 5, 5) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TAUWIX(1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/05', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_SEND_INIT (TAUWIY(1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/05', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 5, 6) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TAUWNX(1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/06', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_SEND_INIT (TAUWNY(1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/06', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 5, 7) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (WHITECAP(1,1),NSEALM , MPI_REAL, IROOT,&
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/07', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 5, 8) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (WHITECAP(1,2),NSEALM , MPI_REAL, IROOT,&
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/08', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 5, 9) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (WHITECAP(1,3),NSEALM , MPI_REAL, IROOT,&
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/09', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 5,10) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (WHITECAP(1,4),NSEALM , MPI_REAL, IROOT,&
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/10', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 5, 11) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TWS(1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/11', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 6, 1) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (SXX   (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (SYY   (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (SXY   (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 6, 2) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TAUOX (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/02', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TAUOY (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/02', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 6, 3) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (BHD(1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/03', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 6, 4) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PHIOC (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/04', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 6, 5) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TUSX  (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/05', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TUSY  (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/05', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 6, 6) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (USSX  (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/06', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (USSY  (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/06', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 6, 7) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PRMS  (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/07', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TPMS  (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/07', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 6, 8) ) THEN
                  DO IK=1,2*NK
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_SEND_INIT (US3D(1,IK),NSEALM , MPI_REAL, IROOT,  &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'US3D ', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                END IF
#endif
!      
#ifdef W3_MPI
             IF ( FLGRDALL( 6, 9) ) THEN     
                      DO K=P2MSF(2),P2MSF(3)
                        IH     = IH + 1
                        IT     = IT + 1
      CALL MPI_SEND_INIT (P2SMS(1,K),NSEALM , MPI_REAL, IROOT,  &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'P2SMS', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                        END DO
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 6,10) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TAUICE (1,1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/10', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TAUICE (1,2),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/10', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 6,11) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PHICE (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/11', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 6, 12) ) THEN
                  DO IK=1,2*NK
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_SEND_INIT (USSP(1,IK),NSEALM , MPI_REAL, IROOT,  &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'USSP ', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                END IF 
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 6, 13) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TAUOCX(1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/13', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TAUOCY(1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/13', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 7, 1) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (ABA   (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (ABD   (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 7, 2) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (UBA   (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/02', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (UBD   (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/02', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 7, 3) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (BEDFORMS(1,1),NSEALM , MPI_REAL,      &
                         IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/03', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (BEDFORMS(1,2),NSEALM , MPI_REAL,      &
                         IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/03', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (BEDFORMS(1,3),NSEALM , MPI_REAL,      &
                         IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/03', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 7, 4) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (PHIBBL(1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/04', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 7, 5) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TAUBBL(1,1),NSEALM , MPI_REAL,        &
                         IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/05', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (TAUBBL(1,2),NSEALM , MPI_REAL,        &
                         IROOT, IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/05', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 8, 1) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (MSSX  (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
       CALL MPI_SEND_INIT (MSSY  (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 8, 2) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (MSCX  (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/02', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (MSCY  (1),NSEALM , MPI_REAL, IROOT,   &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/02', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 8, 3) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (MSSD  (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/03', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 8, 4) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (MSCD  (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/04', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 8, 5) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (QP    (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/05', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 9, 1) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (DTDYN(1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 9/01', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 9, 2) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (FCUT (1),NSEALM , MPI_REAL, IROOT,    &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 9/02', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 9, 3) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (CFLXYMAX(1),NSEALM , MPI_REAL, IROOT, &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 9/03', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 9, 4) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (CFLTHMAX(1),NSEALM , MPI_REAL, IROOT, &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 9/04', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              IF ( FLGRDALL( 9, 5) ) THEN
                  IH     = IH + 1
                  IT     = IT + 1
      CALL MPI_SEND_INIT (CFLKMAX(1),NSEALM , MPI_REAL, IROOT,  &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 9/05', IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              DO I=1, NOEXTR
                IF ( FLGRDALL(10, I) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_SEND_INIT (USERO(1,I),NSEALM , MPI_REAL, IROOT,  &
                                IT, MPI_COMM_WAVE, IRQGO(IH), IERR)
#endif
#ifdef W3_MPIT
                   WRITE (STRING,'(A3,I2.2)') '10/', I
      WRITE (NDST,9011) IH, STRING, IROOT, IT, IRQGO(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
                END DO
#endif
!
#ifdef W3_MPI
               NRQGO  = IH
#endif
#ifdef W3_MPIT
              WRITE (NDST,9012)
              WRITE (NDST,9013) NRQGO, NRQMAX
#endif
!
#ifdef W3_MPI
            END IF
#endif
!
#ifdef W3_MPI
          IF ( NRQGO .GT. NRQMAX ) THEN
              WRITE (NDSE,1010) NRQGO, NRQMAX
              CALL EXTCDE (10)
            END IF
#endif
!
#ifdef W3_MPI
          IF ( IAPROC .EQ. NAPFLD ) THEN
#endif
!
! 1.b Setting up expanded arrays
!
#ifdef W3_MPI
              IF (NAPFLD .EQ. NAPRST) THEN
                CALL W3XDMA ( IMOD, NDSE, NDST, FLGRDARST )
              ELSE
                CALL W3XDMA ( IMOD, NDSE, NDST, FLGRDALL )
              ENDIF
#endif
!
! 1.c Receives of fields
!
#ifdef W3_MPI
              CALL W3XETA ( IMOD, NDSE, NDST )
#endif
#ifdef W3_MPIT
              WRITE (NDST,9010) '(RECV)'
#endif
!
#ifdef W3_MPI
              IH     = 0
#endif
!
#ifdef W3_MPI
              DO I0=1, NAPROC
                IT     = IT0
                IFROM  = I0 - 1
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 1, 9) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (ICEF (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 1/09', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 1) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (HS   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 2) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (WLM  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/02', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 3) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (T02  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/03', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 4) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (T0M1  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/04', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 5) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (T01(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/05', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 6) .OR. FLGRDALL( 2,18) ) THEN
                    ! TP output shares FP0 internal field with FP
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (FP0  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/06', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 7) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (THM  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/07', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 8) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (THS  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/08', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 9) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (THP0 (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/09', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 10) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (HSIG (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/10', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 11) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (STMAXE (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/11', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 12) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (STMAXD(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/12', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 13) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (HMAXE (I0),1,WW3_FIELD_VEC, IFROM, IT, &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/13', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 14) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (HCMAXE(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/14', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 15) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (HMAXD (I0),1,WW3_FIELD_VEC, IFROM, IT, &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/15', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 16) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (HCMAXD(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/16', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 17) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (WBT(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/17', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 2, 19) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (WNMEAN(I0),1,WW3_FIELD_VEC, IFROM, IT, &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 2/19', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 3, 1) ) THEN 
                    DO IK=E3DF(2,1),E3DF(3,1)
                      IH     = IH + 1
                      IT     = IT + 1
      CALL MPI_RECV_INIT (EF(I0,IK),1,WW3_FIELD_VEC, IFROM, IT,&
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'EF', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                      END DO
                    END IF
#endif
!       
#ifdef W3_MPI
                IF ( FLGRDALL( 3, 2) ) THEN 
                    DO IK=E3DF(2,2),E3DF(3,2)
                      IH     = IH + 1
                      IT     = IT + 1
      CALL MPI_RECV_INIT (TH1M(I0,IK),1,WW3_FIELD_VEC, IFROM, IT,&
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'TH1M', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                      END DO
                    END IF
#endif
!      
#ifdef W3_MPI
                IF ( FLGRDALL( 3, 3) ) THEN 
                    DO IK=E3DF(2,3),E3DF(3,3)
                      IH     = IH + 1
                      IT     = IT + 1
      CALL MPI_RECV_INIT (STH1M(I0,IK),1,WW3_FIELD_VEC, IFROM, IT,&
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'STH1M', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                      END DO
                    END IF
#endif
!       
#ifdef W3_MPI
                IF ( FLGRDALL( 3, 4) ) THEN 
                    DO IK=E3DF(2,4),E3DF(3,4)
                      IH     = IH + 1
                      IT     = IT + 1
      CALL MPI_RECV_INIT (TH2M(I0,IK),1,WW3_FIELD_VEC, IFROM, IT,&
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'TH2M', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                      END DO
                    END IF
#endif
!      
#ifdef W3_MPI
               IF ( FLGRDALL( 3, 5) ) THEN 
                    DO IK=E3DF(2,5),E3DF(3,5)
                      IH     = IH + 1
                      IT     = IT + 1
      CALL MPI_RECV_INIT (STH2M(I0,IK),1,WW3_FIELD_VEC, IFROM, IT,&
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'STH2M', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                      END DO
                    END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4, 1) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PHS(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4, 2) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PTP(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/02', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4, 3) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PLP(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/03', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4, 4) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PDIR(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/04', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4, 5) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PSI(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/05', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4, 6) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PWS(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/06', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4, 7) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PTHP0(I0,K),1,WW3_FIELD_VEC, IFROM, IT,&
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/07', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4, 8) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PQP(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/08', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4, 9) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PPE(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/09', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4,10) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PGW(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/10', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4,11) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PSW(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/11', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4,12) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PTM1(I0,K),1,WW3_FIELD_VEC, IFROM, IT,&
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/12', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4,13) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PT1(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/13', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4,14) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PT2(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/14', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4,15) ) THEN
                  DO K=0, NOSWLL
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PEP(I0,K),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/15', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4,16) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PWST (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/16', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 4,17) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PNR  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 4/17', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 5, 1) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (UST   (I0), 1, WW3_FIELD_VEC, IFROM,   &
                             IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (USTDIR(I0), 1, WW3_FIELD_VEC, IFROM,   &
                             IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (ASF   (I0), 1, WW3_FIELD_VEC, IFROM,   &
                             IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 5, 2) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (CHARN(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/02', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 5, 3) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (CGE  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/03', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 5, 4) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PHIAW(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/04', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 5, 5) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TAUWIX(I0),1,WW3_FIELD_VEC, IFROM, IT, &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/05', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TAUWIY(I0),1,WW3_FIELD_VEC, IFROM, IT, &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/05', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 5, 6) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TAUWNX(I0),1,WW3_FIELD_VEC, IFROM, IT, &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/06', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TAUWNY(I0),1,WW3_FIELD_VEC, IFROM, IT, &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/06', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 5, 7) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (WHITECAP(I0,1),1,WW3_FIELD_VEC, IFROM,  &
                               IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/07', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 5, 8) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (WHITECAP(I0,2),1,WW3_FIELD_VEC, IFROM,  &
                               IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/08', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 5, 9) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (WHITECAP(I0,3),1,WW3_FIELD_VEC, IFROM,  &
                               IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/09', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 5,10) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (WHITECAP(I0,4),1,WW3_FIELD_VEC, IFROM,  &
                               IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/10', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 5,11) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TWS(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 5/11', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 6, 1) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (SXX   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (SYY   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (SXY   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 6, 2) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TAUOX (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/02', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TAUOY (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/02', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 6, 3) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (BHD(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/03', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 6, 4) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PHIOC (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/04', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 6, 5) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TUSX  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/05', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TUSY  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/05', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 6, 6) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (USSX  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/06', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (USSY  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/06', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 6, 7) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PRMS  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/07', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TPMS  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/07', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 6, 8) ) THEN
                    DO IK=1,2*NK
                      IH     = IH + 1
                      IT     = IT + 1
      CALL MPI_RECV_INIT (US3D(I0,IK),1,WW3_FIELD_VEC, IFROM, IT, &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'US3D ', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                      END DO
                  END IF
#endif
!      
#ifdef W3_MPI
                IF (  FLGRDALL( 6, 9) ) THEN
                      DO K=P2MSF(2),P2MSF(3)
                        IH     = IH + 1
                        IT     = IT + 1
      CALL MPI_RECV_INIT (P2SMS(I0,K),1,WW3_FIELD_VEC, IFROM, IT, &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'P3SMS', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                        END DO
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 6,10) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TAUICE (I0,1),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/10', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TAUICE (I0,2),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/10', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 6,11) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PHICE (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/11', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 6, 12) ) THEN
                    DO IK=1,2*NK
                      IH     = IH + 1
                      IT     = IT + 1
      CALL MPI_RECV_INIT (USSP(I0,IK),1,WW3_FIELD_VEC, IFROM, IT, &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, 'USSP ', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                      END DO
                  END IF  
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 6, 13) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TAUOCX(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/13', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TAUOCY(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 6/13', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 7, 1) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (ABA   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (ABD   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 7, 2) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (UBA   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/02', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (UBD   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/02', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 7, 3) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (BEDFORMS(I0,1),1,WW3_FIELD_VEC, IFROM,  &
                           IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/03', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (BEDFORMS(I0,2),1,WW3_FIELD_VEC, IFROM,  &
                           IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/03', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (BEDFORMS(I0,3),1,WW3_FIELD_VEC, IFROM,  &
                           IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/03', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 7, 4) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (PHIBBL(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/04', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 7, 5) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TAUBBL(I0,1),1,WW3_FIELD_VEC, IFROM,    &
                           IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/05', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (TAUBBL(I0,2),1,WW3_FIELD_VEC, IFROM,    &
                           IT, MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 7/05', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 8, 1) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (MSSX  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (MSSY  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 8, 2) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (MSCX  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/02', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (MSCY  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/02', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 8, 3) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (MSSD  (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/03', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 8, 4) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (MSCD (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/04', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 8, 5) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (QP   (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 8/05', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 9, 1) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (DTDYN(I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 9/01', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 9, 2) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (FCUT (I0),1,WW3_FIELD_VEC, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 9/02', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 9, 3) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (CFLXYMAX(I0),1,WW3_FIELD_VEC, IFROM, IT,&
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 9/03', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 9, 4) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (CFLTHMAX(I0),1,WW3_FIELD_VEC, IFROM, IT,&
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 9/04', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                IF ( FLGRDALL( 9, 5) ) THEN
                    IH     = IH + 1
                    IT     = IT + 1
      CALL MPI_RECV_INIT (CFLKMAX(I0),1,WW3_FIELD_VEC, IFROM, IT, &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9011) IH, ' 9/05', IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                DO I=1, NOEXTR
                  !WRITE(740+IAPROC,*) 'SECOND : I=', I, ' / ', NOEXTR, ' val=', FLGRDALL(10, I)
                  IF ( FLGRDALL(10, I) ) THEN
                      IH     = IH + 1
                      IT     = IT + 1
      CALL MPI_RECV_INIT (USERO(I0,I),1,WW3_FIELD_VEC, IFROM, IT, &
                               MPI_COMM_WAVE, IRQGO2(IH), IERR )
#endif
#ifdef W3_MPIT
                     WRITE (STRING,'(A3,I2.2)') '10/', I
      WRITE (NDST,9011) IH, STRING, IFROM, IT, IRQGO2(IH), IERR
#endif
#ifdef W3_MPI
                    END IF
                  END DO
#endif
!
#ifdef W3_MPI
                END DO
#endif
!
#ifdef W3_MPI
               NRQGO2 = IH
#endif
#ifdef W3_MPIT
              WRITE (NDST,9012)
              WRITE (NDST,9014) NRQGO2, NRQMAX*NAPROC
#endif
!
#ifdef W3_MPI
              CALL W3SETA ( IMOD, NDSE, NDST )
#endif
!
#ifdef W3_MPI
            END IF
#endif
!
#ifdef W3_MPI
          IF ( NRQGO2 .GT. NRQMAX*NAPROC ) THEN
              WRITE (NDSE,1011) NRQGO2, NRQMAX*NAPROC
              CALL EXTCDE (11)
            END IF
#endif
!
#ifdef W3_MPI
      END IF
#endif
!
! 2.  Set-up for W3IORS ---------------------------------------------- /
! 2.a General preparations
!
#ifdef W3_MPI
      NRQRS  = 0
      IH     = 0
      IROOT  = NAPRST - 1
#endif
!
#ifdef W3_MPI
      IF ( FLOUT(4) .OR. FLOUT(8) ) THEN
          IF (OARST) THEN
            ALLOCATE ( OUTPTS(IMOD)%OUT4%IRQRS(34*NAPROC) )
          ELSE
            ALLOCATE ( OUTPTS(IMOD)%OUT4%IRQRS(3*NAPROC) )
          ENDIF
          IRQRS  => OUTPTS(IMOD)%OUT4%IRQRS
#endif
!
! 2.b Fields at end of file (always)
!
#ifdef W3_MPIT
          WRITE (NDST,9020)
#endif
!
#ifdef W3_MPI
          IF ( IAPROC.NE.NAPRST .AND. IAPROC.LE.NAPROC ) THEN
#endif
!
#ifdef W3_MPI
              IH     = IH + 1
              IT     = IT0 + 1
              CALL MPI_SEND_INIT (UST (IAPROC), 1, WW3_FIELD_VEC, &
                       IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'S U*', IROOT, IT, IRQRS(IH), IERR
#endif
!
#ifdef W3_MPI
              IH     = IH + 1
              IT     = IT0 + 2
              CALL MPI_SEND_INIT (USTDIR(IAPROC), 1, WW3_FIELD_VEC, &
                       IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'S UD', IROOT, IT, IRQRS(IH), IERR
#endif
!
#ifdef W3_MPI
              IH     = IH + 1
              IT     = IT0 + 3
              CALL MPI_SEND_INIT (FPIS(IAPROC), 1, WW3_FIELD_VEC, &
                       IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'S FP', IROOT, IT, IRQRS(IH), IERR
#endif
!
#ifdef W3_MPI
          ELSE IF ( IAPROC .EQ. NAPRST ) THEN
              DO I0=1, NAPROC
                IFROM  = I0 - 1
                IF ( I0 .NE. IAPROC ) THEN
#endif
!
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT0 + 1
                    CALL MPI_RECV_INIT (UST (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R U*', IFROM, IT, IRQRS(IH), IERR
#endif
!
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT0 + 2
                    CALL MPI_RECV_INIT (USTDIR(I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R UD', IFROM, IT, IRQRS(IH), IERR
#endif
!
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT0 + 3
                    CALL MPI_RECV_INIT (FPIS(I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R FP', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                END IF
              END DO
            END IF
#endif
!
#ifdef W3_MPI
          IF (OARST) THEN
              IF ( FLOGRR( 1, 2) ) THEN
                IH     = IH + 1
                IT     = IT0 + 4
                CALL MPI_SEND_INIT (CX(IAPROC), 1, WW3_FIELD_VEC,   &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S CX', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                IH     = IH + 1
                IT     = IT0 + 5
                CALL MPI_SEND_INIT (CY(IAPROC), 1, WW3_FIELD_VEC,   &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S CY', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 1, 9) ) THEN
                IH     = IH + 1
                IT     = IT0 + 6
                CALL MPI_SEND_INIT (ICEF(IAPROC), 1, WW3_FIELD_VEC, &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S IF', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 2, 1) ) THEN
                IH     = IH + 1
                IT     = IT0 + 7
                CALL MPI_SEND_INIT (HS   (1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S HS', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 2, 2) ) THEN
                IH     = IH + 1
                IT     = IT0 + 8
                CALL MPI_SEND_INIT (WLM  (1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S WL', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 2, 4) ) THEN
                IH     = IH + 1
                IT     = IT0 + 9
                CALL MPI_SEND_INIT (T0M1(1), NSEALM, MPI_REAL,      &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S T0', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              ENDIF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 2, 5) ) THEN
                IH     = IH + 1
                IT     = IT0 + 10
                CALL MPI_SEND_INIT (T01 (1), NSEALM, MPI_REAL,      &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S T1', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              ENDIF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 2, 6) ) THEN
                IH     = IH + 1
                IT     = IT0 + 11
                CALL MPI_SEND_INIT (FP0  (1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S FP', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 2, 7) ) THEN
                IH     = IH + 1
                IT     = IT0 + 12
                CALL MPI_SEND_INIT (THM  (1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S TH', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 2, 19) ) THEN
                IH     = IH + 1
                IT     = IT0 + 13
                CALL MPI_SEND_INIT (WNMEAN(1), NSEALM, MPI_REAL,    &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S WM', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 5, 2) ) THEN
                IH     = IH + 1
                IT     = IT0 + 14
                CALL MPI_SEND_INIT (CHARN(1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S CH', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              ENDIF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 5, 5) ) THEN
                IH     = IH + 1
                IT     = IT0 + 15
                CALL MPI_SEND_INIT (TAUWIX(1), NSEALM, MPI_REAL,    &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S WX', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                IH     = IH + 1
                IT     = IT0 + 16
                CALL MPI_SEND_INIT (TAUWIY(1), NSEALM, MPI_REAL,    &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S WY', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 5, 11) ) THEN
                IH     = IH + 1
                IT     = IT0 + 17
                CALL MPI_SEND_INIT (TWS  (1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S TS', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 6, 2) ) THEN
                IH     = IH + 1
                IT     = IT0 + 18
                CALL MPI_SEND_INIT (TAUOX(1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S OX', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                IH     = IH + 1
                IT     = IT0 + 19
                CALL MPI_SEND_INIT (TAUOY(1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S OY', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 6, 3) ) THEN
                IH     = IH + 1
                IT     = IT0 + 20
                CALL MPI_SEND_INIT (BHD  (1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S BH', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 6, 4) ) THEN
                IH     = IH + 1
                IT     = IT0 + 21
                CALL MPI_SEND_INIT (PHIOC(1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S PH', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 6, 5) ) THEN
                IH     = IH + 1
                IT     = IT0 + 22
                CALL MPI_SEND_INIT (TUSX (1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S UX', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                IH     = IH + 1
                IT     = IT0 + 23
                CALL MPI_SEND_INIT (TUSY (1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S UY', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 6, 6) ) THEN
                IH     = IH + 1
                IT     = IT0 + 24
                CALL MPI_SEND_INIT (USSX (1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S SX', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                IH     = IH + 1
                IT     = IT0 + 25
                CALL MPI_SEND_INIT (USSY (1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S SY', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 6,10) ) THEN
                IH     = IH + 1
                IT     = IT0 + 26
                CALL MPI_SEND_INIT (TAUICE(1,1), NSEALM, MPI_REAL,  &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S I1', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                IH     = IH + 1
                IT     = IT0 + 27
                CALL MPI_SEND_INIT (TAUICE(1,2), NSEALM, MPI_REAL,  &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S I2', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 6,13) ) THEN
                IH     = IH + 1
                IT     = IT0 + 28
                CALL MPI_SEND_INIT (TAUOCX(1), NSEALM, MPI_REAL,    &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S TX', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                IH     = IH + 1
                IT     = IT0 + 29
                CALL MPI_SEND_INIT (TAUOCY(1), NSEALM, MPI_REAL,    &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S TY', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 7, 2) ) THEN
                IH     = IH + 1
                IT     = IT0 + 30
                CALL MPI_SEND_INIT (UBA  (1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S BA', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                IH     = IH + 1
                IT     = IT0 + 31
                CALL MPI_SEND_INIT (UBD  (1), NSEALM, MPI_REAL,     &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S BD', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 7, 4) ) THEN
                IH     = IH + 1
                IT     = IT0 + 32
                CALL MPI_SEND_INIT (PHIBBL(1), NSEALM, MPI_REAL,    &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S PB', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
              IF ( FLOGRR( 7, 5) ) THEN
                IH     = IH + 1
                IT     = IT0 + 33
                CALL MPI_SEND_INIT (TAUBBL(1,1), NSEALM, MPI_REAL,  &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S T1', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                IH     = IH + 1
                IT     = IT0 + 34
                CALL MPI_SEND_INIT (TAUBBL(1,2), NSEALM, MPI_REAL,  &
                         IROOT, IT, MPI_COMM_WAVE, IRQRS(IH), IERR)
#endif
#ifdef W3_MPIT
        WRITE (NDST,9021) IH, 'S T2', IROOT, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
              END IF
#endif
!
#ifdef W3_MPI
            IF ( IAPROC .EQ. NAPRST ) THEN
                IF (NAPRST .NE. NAPFLD) CALL W3XDMA ( IMOD, NDSE, NDST, FLOGRR )
                CALL W3XETA ( IMOD, NDSE, NDST )
#endif
!
#ifdef W3_MPI
                DO I0=1, NAPROC
                  IFROM  = I0 - 1
                  IF ( FLOGRR( 1, 2) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 4
                    CALL MPI_RECV_INIT (CX   (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R CX', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IT0 + 5
                    IT     = IT + 1
                    CALL MPI_RECV_INIT (CY   (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R CY', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 1, 9) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 6
                    CALL MPI_RECV_INIT (ICEF (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R IF', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 2, 1) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 7
                    CALL MPI_RECV_INIT (HS   (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R HS', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 2, 2) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 8
                    CALL MPI_RECV_INIT (WLM  (I0),1,WW3_FIELD_VEC,  &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R WL', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 2, 4) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 9
                    CALL MPI_RECV_INIT (T0M1(I0),1,WW3_FIELD_VEC,  &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R T0', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  ENDIF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 2, 5) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 10
                    CALL MPI_RECV_INIT (T01 (I0),1,WW3_FIELD_VEC,  &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R T1', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  ENDIF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 2, 6) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 11
                    CALL MPI_RECV_INIT (FP0  (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R FP', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 2, 7) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 12
                    CALL MPI_RECV_INIT (THM  (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R TH', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 2, 19) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 13
                    CALL MPI_RECV_INIT (WNMEAN(I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R WM', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 5, 2) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 14
                    CALL MPI_RECV_INIT (CHARN(I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R CH', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  ENDIF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 5, 5) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 15
                    CALL MPI_RECV_INIT (TAUWIX(I0),1,WW3_FIELD_VEC,&
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R WX', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT0 + 16
                    CALL MPI_RECV_INIT (TAUWIY(I0),1,WW3_FIELD_VEC,&
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R WY', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 5,11) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 17
                    CALL MPI_RECV_INIT (TWS  (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R TS', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 6, 2) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 18
                    CALL MPI_RECV_INIT (TAUOX(I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R OX', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT0 + 19
                    CALL MPI_RECV_INIT (TAUOY(I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R OY', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 6, 3) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 20
                    CALL MPI_RECV_INIT (BHD  (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R BH', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 6, 4) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 21
                    CALL MPI_RECV_INIT (PHIOC(I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R PH', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 6, 5) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 22
                    CALL MPI_RECV_INIT (TUSX (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R UX', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT0 + 23
                    CALL MPI_RECV_INIT (TUSY (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R UY', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 6, 6) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 24
                    CALL MPI_RECV_INIT (USSX (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R SX', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT0 + 25
                    CALL MPI_RECV_INIT (USSY (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R SY', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 6,10) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 26
                    CALL MPI_RECV_INIT (TAUICE(I0,1),1,WW3_FIELD_VEC,&
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R I1', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT0 + 27
                    CALL MPI_RECV_INIT (TAUICE(I0,2),1,WW3_FIELD_VEC,&
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R I2', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 6,13) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 28
                    CALL MPI_RECV_INIT (TAUOCX(I0),1,WW3_FIELD_VEC,&
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R SX', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT0 + 29
                    CALL MPI_RECV_INIT (TAUOCY(I0),1,WW3_FIELD_VEC,&
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R SY', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 7, 2) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 30
                    CALL MPI_RECV_INIT (UBA  (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R BA', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT0 + 31
                    CALL MPI_RECV_INIT (UBD  (I0),1,WW3_FIELD_VEC, &
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R BD', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 7, 4) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 32
                    CALL MPI_RECV_INIT (PHIBBL(I0),1,WW3_FIELD_VEC,&
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R PB', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
#endif
!
#ifdef W3_MPI
                  IF ( FLOGRR( 7, 5) ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 33
                    CALL MPI_RECV_INIT (TAUBBL(I0,1),1,WW3_FIELD_VEC,&
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R T1', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT0 + 34
                    CALL MPI_RECV_INIT (TAUBBL(I0,2),1,WW3_FIELD_VEC,&
                       IFROM, IT, MPI_COMM_WAVE, IRQRS(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9021) IH, 'R T2', IFROM, IT, IRQRS(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
                END DO
#endif
!
#ifdef W3_MPI
                CALL W3SETA ( IMOD, NDSE, NDST )
            END IF
          END IF
#endif
!
#ifdef W3_MPI
          NRQRS  = IH
          IF (OARST) THEN
            IT0    = IT0 + 34
          ELSE
            IT0    = IT0 + 3
          ENDIF
#endif
!
#ifdef W3_MPIT
          WRITE (NDST,9022)
          WRITE (NDST,9023) NRQRS
#endif
!
! 2.c Data server mode
!
#ifdef W3_MPI
          IF ( IOSTYP .GT. 0 ) THEN
#endif
!
#ifdef W3_MPI
              NBLKRS = 10
              RSBLKS = MAX ( 5 , NSEALM/NBLKRS )
              IF ( NBLKRS*RSBLKS .LT. NSEALM ) RSBLKS = RSBLKS + 1
              NBLKRS = 1 + (NSEALM-1)/RSBLKS
#endif
!
#ifdef W3_MPIT
              WRITE (NDST,9025) RSBLKS, NBLKRS
#endif
#ifdef W3_MPI
              IH     = 0
#endif
!
#ifdef W3_MPI
              IF ((.NOT. LPDLIB).OR.(GTYPE .NE. UNGTYPE)) THEN
              IF ( IAPROC .NE. NAPRST ) THEN
#endif
!
#ifdef W3_MPI
                  ALLOCATE ( OUTPTS(IMOD)%OUT4%IRQRSS(NBLKRS) )
                  IRQRSS => OUTPTS(IMOD)%OUT4%IRQRSS
#endif
!
#ifdef W3_MPI
                  DO IB=1, NBLKRS
                    IH     = IH + 1
                    IT     = IT0 + 3 + IB
                    JSEA0  = 1 + (IB-1)*RSBLKS
                    JSEAN  = MIN ( NSEALM , IB*RSBLKS )
                    NSEAB  = 1 + JSEAN - JSEA0
                    CALL MPI_SEND_INIT (VA(1,JSEA0), NSPEC*NSEAB,&
                         MPI_REAL, IROOT, IT, MPI_COMM_WAVE,    &
                         IRQRSS(IH), IERR )
#endif
#ifdef W3_MPIT
                    WRITE (NDST,9026) IH, 'S', IB, IROOT, IT,   &
                           IRQRSS(IH), IERR, NSEAB
#endif
#ifdef W3_MPI
                    END DO
#endif
!
#ifdef W3_MPI
                ELSE
#endif
!
#ifdef W3_MPI
                  ALLOCATE                                       &
                 ( OUTPTS(IMOD)%OUT4%IRQRSS(NAPROC*NBLKRS) ,     &
                   OUTPTS(IMOD)%OUT4%VAAUX(NSPEC,2*RSBLKS,NAPROC) )
#endif
!
#ifdef W3_MPI
                  IRQRSS => OUTPTS(IMOD)%OUT4%IRQRSS
                  VAAUX  => OUTPTS(IMOD)%OUT4%VAAUX
                  DO IB=1, NBLKRS
                    IT     = IT0 + 3 + IB
                    JSEA0  = 1 + (IB-1)*RSBLKS
                    JSEAN  = MIN ( NSEALM , IB*RSBLKS )
                    NSEAB  = 1 + JSEAN - JSEA0
                    DO I0=1, NAPROC
                      IF ( I0 .NE. NAPRST ) THEN
                          IH     = IH + 1
                          IFROM  = I0 - 1
                          IBOFF  = MOD(IB-1,2)*RSBLKS
                          CALL MPI_RECV_INIT (VAAUX(1,1+IBOFF,I0),&
                               NSPEC*NSEAB, MPI_REAL, IFROM, IT,  &
                               MPI_COMM_WAVE, IRQRSS(IH), IERR )
#endif
#ifdef W3_MPIT
                          WRITE (NDST,9026) IH, 'R', IB, IFROM, &
                                 IT, IRQRSS(IH), IERR, NSEAB
#endif
#ifdef W3_MPI
                        END IF
                      END DO
                    END DO
#endif
!
#ifdef W3_MPI
                END IF
                END IF
#endif
!
#ifdef W3_MPIT
              WRITE (NDST,9027)
              WRITE (NDST,9028) IH
#endif
#ifdef W3_MPI
              IT0    = IT0 + NBLKRS
#endif
!
#ifdef W3_MPI
            END IF
#endif
!
#ifdef W3_MPI
        END IF
#endif
!
! 3.  Set-up for W3IOBC ( SENDs ) ------------------------------------ /
!
#ifdef W3_MPI
      NRQBP  = 0
      NRQBP2 = 0
      IH     = 0
      IT     = IT0
      IROOT  = NAPBPT - 1
#endif
!
#ifdef W3_MPI
      IF ( FLOUT(5) ) THEN
          ALLOCATE ( OUTPTS(IMOD)%OUT5%IRQBP1(NBO2(NFBPO)),      &
                     OUTPTS(IMOD)%OUT5%IRQBP2(NBO2(NFBPO)) )
          IRQBP1 => OUTPTS(IMOD)%OUT5%IRQBP1
          IRQBP2 => OUTPTS(IMOD)%OUT5%IRQBP2
#endif
!
! 3.a Loops over files and points
!
#ifdef W3_MPIT
          WRITE (NDST,9030) 'MPI_SEND_INIT'
#endif
!
#ifdef W3_MPI
          DO J=1, NFBPO
            DO I=NBO2(J-1)+1, NBO2(J)
#endif
!
#ifdef W3_MPI
               IT     = IT + 1
#endif
!
! 3.b Residence processor of point
!
#ifdef W3_MPI
              ISEA   = ISBPO(I)
              CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
#endif
!
! 3.c If stored locally, send data
!
#ifdef W3_MPI
              IF ( IAPROC .EQ. ISPROC ) THEN
                  IH     = IH + 1
                  CALL MPI_SEND_INIT (VA(1,JSEA),NSPEC,MPI_REAL, &
                       IROOT, IT, MPI_COMM_WAVE, IRQBP1(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9031) IH, I, J, IROOT, IT, IRQBP1(IH), IERR
#endif
#ifdef W3_MPI
                END IF
#endif
!
#ifdef W3_MPI
              END DO
            END DO
#endif
!
! ... End of loops 4.a
!
#ifdef W3_MPI
          NRQBP  = IH
#endif
!
#ifdef W3_MPIT
          WRITE (NDST,9032)
          WRITE (NDST,9033) NRQBP
#endif
!
! 3.d Set-up for W3IOBC ( RECVs ) ------------------------------------ /
!
#ifdef W3_MPI
          IF ( IAPROC .EQ. NAPBPT ) THEN
#endif
!
#ifdef W3_MPI
              IH     = 0
              IT     = IT0
#endif
!
! 3.e Loops over files and points
!
#ifdef W3_MPIT
              WRITE (NDST,9030) 'MPI_RECV_INIT'
#endif
!
#ifdef W3_MPI
              DO J=1, NFBPO
                DO I=NBO2(J-1)+1, NBO2(J)
#endif
!
! 3.f Residence processor of point
!
#ifdef W3_MPI
                  ISEA   = ISBPO(I)
                  CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
#endif
!
! 3.g Receive in correct array
!
#ifdef W3_MPI
                  IH     = IH + 1
                  IT     = IT + 1
                  ITARG  = ISPROC - 1
                  CALL MPI_RECV_INIT (ABPOS(1,IH),NSPEC,MPI_REAL,&
                       ITARG, IT, MPI_COMM_WAVE, IRQBP2(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9031) IH, I, J, ITARG, IT, IRQBP2(IH), IERR
#endif
!
#ifdef W3_MPI
                  END DO
                END DO
#endif
!
#ifdef W3_MPI
              NRQBP2 = IH
#endif
!
! ... End of loops 4.e
!
#ifdef W3_MPIT
              WRITE (NDST,9032)
              WRITE (NDST,9033) NRQBP2
#endif
!
#ifdef W3_MPI
            END IF
#endif
!
#ifdef W3_MPI
          IT0    = IT0 + NBO2(NFBPO)
#endif
!
#ifdef W3_MPI
        END IF
#endif
!
#ifdef W3_MPIT
      WRITE (NDST,*)
#endif
!
! 4.  Set-up for W3IOTR ---------------------------------------------- /
!
#ifdef W3_MPI
      IH     = 0
      IROOT  = NAPTRK - 1
#endif
!
#ifdef W3_MPI
      IF ( FLOUT(3) ) THEN
#endif
!
! 4.a U*
!
#ifdef W3_MPIT
          WRITE (NDST,9040)
#endif
!
#ifdef W3_MPI
          IF ( IAPROC .NE. NAPTRK ) THEN
              ALLOCATE ( OUTPTS(IMOD)%OUT3%IRQTR(2) )
              IRQTR  => OUTPTS(IMOD)%OUT3%IRQTR
              IH     = IH + 1
              IT     = IT0 + 1
              CALL MPI_SEND_INIT (UST   (IAPROC),1,WW3_FIELD_VEC,&
                   IROOT, IT, MPI_COMM_WAVE, IRQTR(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9041) IH, 'S U*', IROOT, IT, IRQTR(IH), IERR
#endif
#ifdef W3_MPI
              IH     = IH + 1
              IT     = IT0 + 2
              CALL MPI_SEND_INIT (USTDIR(IAPROC),1,WW3_FIELD_VEC,&
                   IROOT, IT, MPI_COMM_WAVE, IRQTR(IH), IERR )
#endif
#ifdef W3_MPIT
      WRITE (NDST,9041) IH, 'S U*', IROOT, IT, IRQTR(IH), IERR
#endif
#ifdef W3_MPI
            ELSE
              ALLOCATE ( OUTPTS(IMOD)%OUT3%IRQTR(2*NAPROC) )
              IRQTR  => OUTPTS(IMOD)%OUT3%IRQTR
              DO I0=1, NAPROC
                IFROM  = I0 - 1 
                IF ( I0 .NE. IAPROC ) THEN
                    IH     = IH + 1
                    IT     = IT0 + 1
                    CALL MPI_RECV_INIT(UST   (I0),1,WW3_FIELD_VEC,&
                         IFROM,IT,MPI_COMM_WAVE, IRQTR(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9041) IH, 'R U*', IFROM, IT, IRQTR(IH), IERR
#endif
#ifdef W3_MPI
                    IH     = IH + 1
                    IT     = IT0 + 2
                    CALL MPI_RECV_INIT(USTDIR(I0),1,WW3_FIELD_VEC,&
                         IFROM,IT,MPI_COMM_WAVE, IRQTR(IH), IERR)
#endif
#ifdef W3_MPIT
      WRITE (NDST,9041) IH, 'R U*', IFROM, IT, IRQTR(IH), IERR
#endif
#ifdef W3_MPI
                  END IF
                END DO
            END IF
#endif
!
#ifdef W3_MPI
          NRQTR  = IH 
          IT0    = IT0 + 2
#endif
!
#ifdef W3_MPIT
          WRITE (NDST,9042)
          WRITE (NDST,9043) NRQTR 
#endif
!
#ifdef W3_MPI
        END IF
#endif
!
! 5.  Set-up remaining counters -------------------------------------- /
!
#ifdef W3_MPI
      IT0PRT = IT0
      IT0PNT = IT0PRT + 2*NAPROC
      IT0TRK = IT0PNT + 5000
#endif
!
      RETURN
!
!     Formats :
!
#ifdef W3_MPI
  1010 FORMAT (/' *** ERROR W3MPIO : ARRAY IRQGO TOO SMALL *** '/)
  1011 FORMAT (/' *** ERROR W3MPIO : ARRAY IRQGO2 TOO SMALL *** '/)
#endif
!
#ifdef W3_MPIT
 9010 FORMAT (/' TEST W3MPIO: COMMUNICATION CALLS FOR W3IOGO ',A/ &
               ' +------+-------+------+------+--------------+'/ &
               ' |  IH  |   ID  | TARG |  TAG |   handle err |'/ &
               ' +------+-------+------+------+--------------+')
 9011 FORMAT ( ' |',I5,' | ',A5,' |',2(I5,' |'),I9,I4,' |')
 9012 FORMAT ( ' +------+-------+------+------+--------------+')
 9013 FORMAT ( ' TEST W3MPIO: NRQGO :',2I10)
 9014 FORMAT ( ' TEST W3MPIO: NRQGO2:',2I10)
#endif
!
#ifdef W3_MPIT
 9020 FORMAT (/' TEST W3MPIO: COMM. CALLS FOR W3IORS (F)'/      &
         ' +------+------+------+------+--------------+'/       &
         ' |  IH  |  ID  | TARG |  TAG |   handle err |'/       &
         ' +------+------+------+------+--------------+')
 9021 FORMAT ( ' |',I5,' | ',A4,' |',2(I5,' |'),I9,I4,' |')
 9022 FORMAT ( ' +------+------+------+------+--------------+')
 9023 FORMAT ( ' TEST W3MPIO: NRQRS :',I10)
#endif
!
#ifdef W3_MPIT
 9025 FORMAT (/' TEST W3MPIO: COMM. CALLS FOR W3IORS (S)'/      &
               '              BLOCK SIZE / BLOCKS : ',2I6/      &
     ' +------+------+------+------+--------------+---------+'/ &
     ' |  IH  |  ID  | TARG |  TAG |   handle err | spectra |'/ &
     ' +------+------+------+------+--------------+---------+')
 9026 FORMAT (                                                  &
        ' |',I5,' | ',A1,I3,' |',2(I5,' |'),I9,I4,' |',I8,' |')
 9027 FORMAT (                                                  &
     ' +------+------+------+------+--------------+---------+')
 9028 FORMAT ( ' TEST W3MPIO: IHMAX :',I10)
#endif
!
#ifdef W3_MPIT
 9030 FORMAT (/' TEST W3MPIO: ',A,' CALLS FOR W3IOBC'/          &
         ' +------+------+---+------+------+--------------+'/   &
         ' |  IH  | IPT  | F | TARG |  TAG |   handle err |'/   &
         ' +------+------+---+------+------+--------------+')
 9031 FORMAT ( ' |',2(I5,' |'),I2,' |',2(I5,' |'),I9,I4,' |')
 9032 FORMAT (                                                  &
         ' +------+------+---+------+------+--------------+')
 9033 FORMAT ( ' TEST W3MPIO: NRQBC :',I10)
 9034 FORMAT ( ' TEST W3MPIO: TOTAL :',I10)
#endif
!
#ifdef W3_MPIT
 9040 FORMAT (/' TEST W3MPIO: COMMUNICATION CALLS FOR W3IOTR'/  &
         ' +------+------+------+------+--------------+'/       &
         ' |  IH  |  ID  | TARG |  TAG |   handle err |'/       &
         ' +------+------+------+------+--------------+')
 9041 FORMAT ( ' |',I5,' | ',A4,' |',2(I5,' |'),I9,I4,' |')
 9042 FORMAT (                                                  &
         ' +------+------+------+------+--------------+')
 9043 FORMAT ( ' TEST W3MPIO: NRQTR :',I10)
#endif
!/
!/ End of W3MPIO ----------------------------------------------------- /
!/
      END SUBROUTINE W3MPIO
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3MPIP ( IMOD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         30-Oct-2009 |
!/                  +-----------------------------------+
!/
!/    02-Aug-2006 : Origination.                        ( version 3.10 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/
!  1. Purpose :
!
!     Prepare MPI persistent communication needed for WAVEWATCH I/O
!     routines.
!
!  2. Method :
!
!     Create handles as needed.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!
!      MPI_SEND_INIT, MPI_RECV_INIT
!                Subr. mpif.h   MPI persistent communication calls.
!     ----------------------------------------------------------------
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
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/MPI   MPI communication calls.
!
!       !/S     Enable subroutine tracing.
!       !/MPIT  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
#ifdef W3_MPI
      USE W3SERVMD, ONLY: EXTCDE
#endif
!/
#ifdef W3_MPI
      USE W3GDATMD, ONLY: NX, NY, NSPEC, MAPFS
      USE W3WDATMD, ONLY: VA
      USE W3ADATMD, ONLY: MPI_COMM_WAVE, SPPNT
      USE W3ODATMD, ONLY: NDST, NDSE, IAPROC, NAPROC, NAPPNT, FLOUT
      USE W3ODATMD, ONLY: OUTPTS, NRQPO, NRQPO2, IRQPO1, IRQPO2, &
                          NOPTS, IPTINT, IT0PNT, IT0TRK, O2IRQI
      USE W3PARALL, ONLY: INIT_GET_JSEA_ISPROC
#endif
!/
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
#ifdef W3_MPI
      INTEGER                 :: IH, IROOT, I, J, IT, IT0, JSEA, &
                                 IERR, ITARG, IX(4), IY(4),      &
                                 K, IS(4), IP(4)
#endif
        INTEGER                 :: itout
#ifdef W3_S
      INTEGER, SAVE           :: IENT
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3MPIP')
#endif
!
#ifdef W3_MPI
      IF ( O2IRQI ) THEN
          WRITE (NDSE,1001)
          CALL EXTCDE (1)
        END IF
#endif
!
! 1.  Set-up for W3IOPE/O ( SENDs ) ---------------------------------- /
!
#ifdef W3_MPI
      NRQPO  = 0
      NRQPO2 = 0
      IH     = 0
      IT0    = IT0PNT
      IROOT  = NAPPNT - 1
#endif
!
#ifdef W3_MPI
      ALLOCATE ( OUTPTS(IMOD)%OUT2%IRQPO1(4*NOPTS),              &
                 OUTPTS(IMOD)%OUT2%IRQPO2(4*NOPTS) )
      IRQPO1 => OUTPTS(IMOD)%OUT2%IRQPO1
      IRQPO2 => OUTPTS(IMOD)%OUT2%IRQPO2
      O2IRQI = .TRUE.
#endif
!
! 1.a Loop over output locations
!
#ifdef W3_MPIT
      WRITE (NDST,9010) 'MPI_SEND_INIT'
#endif
!
#ifdef W3_MPI
      DO I=1, NOPTS
        DO K=1,4
          IX(K)=IPTINT(1,K,I)
          IY(K)=IPTINT(2,K,I)
          END DO
#endif
! 1.b Loop over corner points
!
#ifdef W3_MPI
        DO J=1, 4
#endif
!
#ifdef W3_MPI
          IT     = IT0 + (I-1)*4 + J
          IS(J)  = MAPFS (IY(J),IX(J))
          IF ( IS(J) .EQ. 0 ) THEN
              JSEA   = 0
              IP(J)  = NAPPNT
            ELSE
              CALL INIT_GET_JSEA_ISPROC(IS(J), JSEA, IP(J))
            END IF
#endif
!
! 1.c Send if point is stored here
!
#ifdef W3_MPI
          IF ( IP(J) .EQ. IAPROC ) THEN
              IH     = IH + 1
              CALL MPI_SEND_INIT ( VA(1,JSEA), NSPEC, MPI_REAL, &
                   IROOT, IT, MPI_COMM_WAVE, IRQPO1(IH), IERR )
#endif
#ifdef W3_MPIT
              WRITE (NDST,9011) IH,I,J, IROOT,IT, IRQPO1(IH), IERR
#endif
#ifdef W3_MPI
            END IF
#endif
!
! ... End of loop 1.b
!
#ifdef W3_MPI
          END DO
#endif
!
! ... End of loop 1.a
!
#ifdef W3_MPI
        END DO
#endif
!
#ifdef W3_MPI
      NRQPO  = IH
#endif
!
#ifdef W3_MPIT
      WRITE (NDST,9012)
      WRITE (NDST,9013) NRQPO
#endif
!
! 1.d Set-up for W3IOPE/O ( RECVs ) ---------------------------------- /
!
#ifdef W3_MPI
      IF ( IAPROC .EQ. NAPPNT ) THEN
#endif
!
#ifdef W3_MPI
          IH     = 0
#endif
!
! 2.e Loop over output locations
!
#ifdef W3_MPIT
          WRITE (NDST,9010) 'MPI_RECV_INIT'
#endif
!
#ifdef W3_MPI
          DO I=1, NOPTS
            DO K=1,4
              IX(K)=IPTINT(1,K,I)  
              IY(K)=IPTINT(2,K,I)
              END DO 
#endif
!
#ifdef W3_MPI
            DO J=1, 4
#endif
!
#ifdef W3_MPI
              IT     = IT0 + (I-1)*4 + J
              IS(J)  = MAPFS (IY(J),IX(J))
              IF ( IS(J) .EQ. 0 ) THEN
                  JSEA   = 0
                  IP(J)  = NAPPNT
                ELSE
                  CALL INIT_GET_JSEA_ISPROC(IS(J), JSEA, IP(J))
                END IF
#endif
!
! 1.g Receive in correct array
!
#ifdef W3_MPI
              IH     = IH + 1
              ITARG  = IP(J) - 1
              CALL MPI_RECV_INIT ( SPPNT(1,1,J), NSPEC, MPI_REAL, &
                   ITARG, IT, MPI_COMM_WAVE, IRQPO2(IH), IERR )
#endif
#ifdef W3_MPIT
              WRITE (NDST,9011) IH,I,J, ITARG,IT, IRQPO2(IH), IERR
#endif
!
! ... End of loop 1.f
!
#ifdef W3_MPI
              END DO
#endif
!
! ... End of loop 1.e
!
#ifdef W3_MPI
            END DO
#endif
!
#ifdef W3_MPI
          NRQPO2 = NOPTS*4
#endif
!
#ifdef W3_MPIT
          WRITE (NDST,9012)
          WRITE (NDST,9014) NRQPO2
#endif
!
#ifdef W3_MPI
        END IF
#endif
!
!
#ifdef W3_MPI
      IT0    = IT0 + 8*NOPTS
#endif
!
! 1.h Base tag number for track output
!
#ifdef W3_MPI
      IT0TRK = IT0
#endif
!
      RETURN
!
!     Formats :
!
#ifdef W3_MPI
  1001 FORMAT (/' *** ERROR W3MPIP : ARRAYS ALREADY ALLOCATED *** '/)
#endif
!
#ifdef W3_MPIT
 9010 FORMAT (/' TEST W3MPIP: ',A,' CALLS FOR W3IOPO'/          &
         ' +------+------+---+------+------+--------------+'/   &
         ' |  IH  | IPT  | J | TARG |  TAG |   handle err |'/   &
         ' +------+------+---+------+------+--------------+')
 9011 FORMAT ( ' |',2(I5,' |'),I2,' |',2(I5,' |'),I9,I4,' |')
 9012 FORMAT (                                                  &
         ' +------+------+---+------+------+--------------+')
 9013 FORMAT ( ' TEST W3MPIP: NRQPO :',I10)
 9014 FORMAT ( ' TEST W3MPIP: TOTAL :',I10)
#endif
!/
!/ End of W3MPIP ----------------------------------------------------- /
!/
      END SUBROUTINE W3MPIP
!/
!/ End of module W3INITMD -------------------------------------------- /
!/
      END MODULE W3INITMD
