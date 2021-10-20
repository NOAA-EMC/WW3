#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE WMINITMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    13-Jun-2005 : Origination.                        ( version 3.07 )
!/                  See subroutine for update log.
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    16-Aug-2010 : Adding NTRMAX to unify NTRACE.      ( version 3.14 )
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    05-Sep-2011 : Distribute HQFAC anf HPFAC to idle processors for
!/                  use in WMGRIDMD.                    ( version 4.05 )
!/    07-Mar-2012 : Adding TNAMES to avoid read warn.   ( version 4.07 )
!/                  Adjust allocation INPMAP and IDINP.
!/    12-Mar-2012 : Fixing format 9061.                 ( version 3.14 )
!/                  Use MPI_COMM_NULL for checks instead of fixed '-1'.
!/    28-Jul-2012 : Initialize FLGR2 properly.          ( version 4.08 )
!/                  Tom Durrant's fix, but moved to allocation.
!/    28-Nov-2012 : Bug fix: Distribute to idle processors the grid data
!/                  required for regridding.            ( version 4.08 )
!/                  (T. J. Campbell, NRL)
!/    02-Sep-2012 : Set up for > 999 test files.        ( version 4.10 )
!/                  Set up output for  > 999 procs.
!/    03-Sep-2012 : Output of initilization time.       ( version 4.10 )
!/                  Switch test file on/off (TSTOUT)
!/    18-Dec-2013 : Adding error checking for FLAGLL    ( version 4.16 )
!/    28-Jan-2014 : Add memory hwm to profiling.        ( version 5.00 )
!/    04-Feb-2014 : Switched clock to DATE_AND_TIME     ( version 4.18 )
!/                  (A. Chawla and Mark Szyszka) 
!/    27-May-2014 : Bug fix prf file name.              ( version 5.02 )
!/    20-Jan-2017 : Update to new W3GSRUMD APIs         ( version 6.02 )
!/    20-Jan-2017 : Modify input forcing flags to support coupler input.
!/                  Add ESMF override for STIME & ETIME ( version 6.02 )
!/                  (T. J. Campbell, NRL)
!/    15-May-2018 : Update namelist                     ( version 6.05 )
!/    22-Mar-2021 : Add momentum and air density input  ( version 7.13 )
!/
!/    Copyright 2009-2014 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Initialization of the multi-grid wave model. As a preparation
!     for coupled modeling, all initialization, including the 
!     processing of the input file has ben included in the routine.
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      NTRMAX    Int.  Local    Maximum number of subroutine trace
!                               printouts (NTRACE in subr. ITRACE).
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      WMINIT    Subr. Public   Wave model initialization.
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
!     See subroutine documentation.
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      PUBLIC
!/
      INTEGER, PRIVATE        :: NTRMAX = 1000
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE WMINIT ( IDSI, IDSO, IDSS, IDST, IDSE, IFNAME,       &
                          MPI_COMM, PREAMB )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    13-Jun-2005 : Origination.                        ( version 3.07 )
!/    28-Dec-2005 : Add static nesting.                 ( version 3.08 )
!/    25-May-2006 : Add overlapping grids.              ( version 3.09 )
!/    26-Jun-2006 : Add output type 6.                  ( version 3.09 )
!/    29-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    09-Aug-2006 : Unified point output added.         ( version 3.10 )
!/    14-Oct-2006 : Adding separate input grids.        ( version 3.10 )
!/    03-Nov-2006 : Adding wave field separation.       ( version 3.10 )
!/    02-Feb-2007 : Adding FLAGST initialization.       ( version 3.10 )
!/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    16-Aug-2010 : Adding NTRMAX to unify NTRACE.    ( version 3.14.5 )
!/    21-Sep-2010 : Adding coupling output              ( version 3.14-Ifremer)
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    28-Jul-2012 : Initialize FLGR2 properly.          ( version 4.08 )
!/                  Tom Durant's fix, but moved to allocation.
!/    28-Nov-2012 : Bug fix: Distribute to idle processors the grid data
!/                  required for regridding.            ( version 4.08 )
!/                  (T. J. Campbell, NRL)
!/    02-Sep-2012 : Set up for > 999 test files.        ( version 4.10 )
!/                  Set up output for  > 999 procs.
!/    03-Sep-2012 : Output of initilization time.       ( version 4.10 )
!/                  Switch test file on/off (TSTOUT)
!/    28-Nov-2012 : Bug fix: Distribute to idle processors the grid data
!/                  required for regridding.            ( version 4.08 )
!/                  (T. J. Campbell, NRL)
!/    15-Apr-2013 : Changes the reading of output fields( version 4.10 )
!/                  (F. Ardhuin) 
!/    28-Jan-2014 : Add memory hwm to profiling.        ( version 5.00 )
!/    27-May-2014 : Bug fix prf file name.              ( version 5.02 )
!/    17-Sep-2014 : Read mod_def before inp file        ( version 5.03 )
!/    20-Jan-2017 : Update to new W3GSRUMD APIs         ( version 6.02 )
!/    20-Jan-2017 : Modify input forcing flags to support coupler input.
!/                  Add ESMF override for STIME & ETIME ( version 6.02 )
!/                  (T. J. Campbell, NRL)
!/    28-Oct-2020 : Add SMCTYPE for SMC sub-grid.  JGLi ( version 7.13 )
!/    22-Mar-2021 : Add momentum and air density input  ( version 7.13 )
!/
!  1. Purpose :
!
!     Initialize multi-grid version of WAVEWATCH III.
!
!  2. Method :
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IDSI    Int.   I   Unit number for input file.
!       IDSO    Int.   I   Unit number for output file.
!       IDSS    Int.   I   Unit number for "screen" output. Switch off
!                          by setting equal to IDSO.
!       IDST    Int.   I   Unit number for test output.
!       IDSE    Int.   I   Unit number for error output.
!       IFNAME  Char   I   File name for input file.
!     MPI_COMM  Int.   I   MPI communicator to be used.
!       PREAMB  Char   I   File name preamble (optional).
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3NMOD    Subr. W3GDATMD Data structure initialization.
!      W3DIMX    Subr.   Id.    Set grid arrays.
!      W3DIMS    Subr.   Id.    Set grid arrays.
!      W3SETG    Subr.   Id.    Point to grid/model.
!      W3NDAT    Subr. W3WDATMD Data structure initialization.
!      W3SETW    Subr.   Id.    Point to grid/model.
!      W3NAUX    Subr. W3ADATMD Data structure initialization.
!      W3SETA    Subr.   Id.    Point to grid/model.
!      W3NOUT    Subr. W3ODATMD Data structure initialization.
!      W3SETO    Subr.   Id.    Point to grid/model.
!      W3NINP    Subr. W3IDATMD Data structure initialization.
!      W3SETI    Subr.   Id.    Point to grid/model.
!      W3DIMI    Subr.   Id.    Allocate grid/model.
!      WMNDAT    Subr. WMMDATMD Data structure initialization.
!      WMSETM    Subr.   Id.    Point to grid/model.
!      WMDIMD    Subr.   Id.    Allocate array space.
!      W3FLDO    Subr. W3FLDSMD Open input data file.
!      W3IOGR    Subr. W3IOGRMD Reading of model definition file.
!      W3INIT    Subr. W3INITMD Model intiailization.
!      WMGLOW    Subr. WMGRIDMD Lower rank grid dependencies.
!      WMGEQL    Subr.   Id.    Same rank grid dependencies.
!      WMGHGH    Subr.   Id.    Higher rank grid dependencies.
!      RESPEC    Subr.   Id.    Spectral conversion flags.
!      WMIOBS    Subr. WMINIOMD Stage boundary data.
!      WMIOBG    Subr.   Id.    Gather boundary data.
!      WMIOBF    Subr.   Id.    Finalize staging in WMIOBS.
!      WMUINI    Subr. WMUNITMD Initialize dynamic unit assignment,
!      WMUDMP    Subr.   Id.    Dump dynamic unit data,
!      WMUSET    Subr.   Id.    Set unit number data.
!      WMUGET    Subr.   Id.    Get a unit number.
!      WMUINQ    Subr.   Id.    Update unit number info.
!      WMIOPP    Subr. WMIOPOMD Initialize unified point output.
!      ITRACE    Subr. W3SERVMD Initialize subroutine tracing.
!      STRACE    Subr.   Id.    Subroutine tracing.
!      EXTCDE    Subr.   Id.    Program abort.
!      WWDATE    Subr.   Id.    System date.
!      WWTIME    Subr.   Id.    System time.
!      NEXTLN    Subr.   Id.    Find next input line in file.
!      PRINIT    Subr.   Id.    Profiling routine ( !/MPRF )
!      PRTIME    Subr.   Id.    Profiling routine ( !/MPRF )
!      STME21    Subr. W3TIMEMD Convert time to string.
!      DSEC21    Func.   Id.    Difference between times.
!      TICK21    Subr.   Id.    Advance the clock.
!      W3READFLGRD Subr. W3IOGOMD Reads flags or namelist for output fields
!
!      MPI_COMM_SIZE, CALL MPI_COMM_RANK, MPI_BARRIER, MPI_COMM_GROUP,
!      MPI_GROUP_INCLUDE, MPI_COMM_CREATE, MPI_GROUP_FREE, MPI_BCAST
!                Subr. mpif.h   Standard MPI routines.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3MLTI    Prog.   N/A    Multi-grid model driver.
!      ....                     Any coupled model.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     See formats 1000 and following, or escape locations 2000 and
!     following.
!
!  7. Remarks :
!
!     - When running regtests in cases where disk is non-local 
!       (i.e. NFS used), there can be a huge improvment in compute
!       time by using /var/tmp/ for log files. 
!       See commented line at "OPEN (MDSO,FILE=..."
!
!     - IDFLDS dimensioning is hardwired as IDFLDS(-7:9) where lowest possible 
!       value of JFIRST is JFIRST=-7
!
!  8. Structure :
!
!     --------------------------------------------------------------
!      1.  Multi-grid model intializations
!        a Unit numbers
!        b Subroutine tracing                            ( ITRACE )
!        c Input file
!        d Log and test files
!        e Initial and test output
!      2.  Set-up of data structures and I/O
!        a Get number of grids
!        b Set up data structures
!                 ( W3NMOD, W3NDAT, W3NAUX, W3NOUT, W3NINP, WMNDAT )
!        c Set up I/O for individual models
!      3.  Get individual grid information
!        a Read data
!        b Assign input file numbers.
!        c Set rank and group data
!        d Unified point output file.                    ( W3IOGR )
!        e Output
!      4.  Model run time information and settings
!      5.  Output requests
!        a Loop over types for unified output
!        ---------------------------------------------------
!        b Process standard line
!        c Type 1: fields of mean wave parameters
!        d Type 2: point output
!        e Type 3: track output
!        f Type 4: restart files (no additional data)
!        g Type 5: nesting data (no additional data)
!        h Type 6: wave field data (dummy for now)
!        i Set all grids to unified output
!        ---------------------------------------------------
!        j Endless loop for correcting output per grid
!        ---------------------------------------------------
!          Test grid name and output number
!        k Process standard line
!        l Type 1: fields of mean wave parameters
!        m Type 2: point output
!        n Type 3: track output
!        o Type 6: partitioning output
!        p Type 7: coupling output
!        ---------------------------------------------------
!      6.  Read moving grid data
!      7.  Work load distribution
!        a Initialize arrays
!        b Set communicators and ALLPRC array
!        c Set MODMAP and LOADMP arrays
!        d Warnings
!      8.  Actual initializations
!        a Loop over models for per-model initialization
!           1 Wave model                                 ( W3INIT )
!           2 Data files                                 ( W3FLDO )
!           3 Grid status indicator and model times
!           3 Grid data for processors that are NOT used.
!           5 Test output
!        b Input data files.
!        c Inter model initialization
!           1 Set spectral conversion flags              ( WMRSPC )
!           2 Prepare unified point output               ( WMIOPO )
!           3 Relation to lower ranked grids
!                                ( WMGLOW, WMIOBS, WMIOBG, WMIOBF )
!           4 Relation to same ranked grids              ( WMGEQL )
!           5 Relation to higher ranked grids            ( WMGHGH )
!           6 Output
!     --------------------------------------------------------------
!
!  9. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   Id.
!                            
!       !/MGW   Moving grid wind correction.
!       !/MGP   Moving grid propagation correction.
!
!       !/O10   Enable output identifying start and end of routine
!      
!       !/S     Enable subroutine tracing.
!       !/T     Enable test output.
!       !/MPRF  Profiling.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
!/
      USE W3GDATMD, ONLY: W3NMOD, W3DIMX, W3DIMS, W3SETG
      USE W3WDATMD, ONLY: W3NDAT, W3SETW
      USE W3ADATMD, ONLY: W3NAUX, W3SETA
      USE W3ODATMD, ONLY: W3NOUT, W3SETO
      USE W3IDATMD, ONLY: W3NINP, W3SETI, W3DIMI
      USE WMMDATMD, ONLY: WMNDAT, WMSETM, WMDIMD
!
      USE W3FLDSMD, ONLY: W3FLDO
      USE W3IOGOMD, ONLY: W3READFLGRD 
      USE W3IOGRMD, ONLY: W3IOGR
      USE W3INITMD, ONLY: W3INIT
      USE WMGRIDMD, ONLY: WMRSPC, WMGLOW, WMGEQL, WMGHGH, WMSMCEQL
      USE WMINIOMD, ONLY: WMIOBS, WMIOBG, WMIOBF
      USE WMIOPOMD, ONLY: WMIOPP
!/
      USE W3SERVMD, ONLY: ITRACE, EXTCDE, WWDATE, WWTIME, NEXTLN
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
#ifdef W3_MPRF
      USE W3TIMEMD, ONLY: PRINIT, PRTIME
#endif
      USE W3TIMEMD, ONLY: STME21, DSEC21, TICK21, TDIFF
      USE WMUNITMD, ONLY: WMUINI, WMUDMP, WMUSET, WMUGET, WMUINQ
!/
      USE W3GDATMD, ONLY: GTYPE, NX, NY, FILEXT, NSEA, FLAGST, GRIDS
#ifdef W3_SMC
      USE W3GDATMD, ONLY: NCel, NUFc, NVFc, NRLv, NBSMC 
      USE W3GDATMD, ONLY: NARC, NBAC, NSPEC, SMCTYPE 
#endif
#ifdef W3_MPI
      USE W3GDATMD, ONLY: FLAGLL, ICLOSE, GSU, X0, Y0, SX, SY,   &
                          XGRD, YGRD, DXDP, DXDQ, DYDP, DYDQ,    &
                          HQFAC, HPFAC, MAPSTA, MAPST2,          &
                          GRIDSHIFT, NSEAL, NK, NTH, XFR, FR1,   &
                          TH, DTMAX, DTCFL
      USE W3GSRUMD
#endif
      USE W3WDATMD, ONLY: TIME
      USE W3ADATMD, ONLY: WADATS
      USE W3IDATMD, ONLY: INFLAGS1, INFLAGS2, INPUTS, IINIT,  &
                          JFIRST
      USE W3ODATMD, ONLY: NOGRP, NGRPP, FLOUT, TONEXT, FLBPI,  &
                          FLBPO, NFBPO, NBI, NDS, IAPROC,     &
                          NAPFLD, NAPPNT, NAPTRK, NAPBPT,     &
                          NAPPRT, NAPROC, FNMPRE, OUTPTS, NDST, NDSE, &
                          NOPTS, IOSTYP, UNIPTS, UPPROC, DTOUT,       &
                          TOLAST, NOTYPE
      USE WMMDATMD, ONLY: MDSI, MDSO, MDSS, MDST, MDSE, MDSF, MDSUP,  &
                          IMPROC, NMPROC, NMPSCR, NMPERR,     &
                          NMPLOG, NMPUPT, STIME, ETIME, NMV, NMVMAX,  &
                          TMV, AMV, DMV, NRGRD, NRINP, NRGRP, GRANK,  &
                          GRGRP, INGRP, GRDHGH, GRDEQL, GRDLOW,       &
                          ALLPRC, MODMAP, TSYNC, TMAX, TOUTP, TDATA,  &
                          GRSTAT, DTRES, BCDUMP, FLGHG1, FLGHG2,      &
                          INPMAP, IDINP, NGRPSMC
      USE WMMDATMD, ONLY: CLKDT1, CLKDT2, CLKFIN
#ifdef W3_MPI
      USE WMMDATMD, ONLY: MPI_COMM_MWAVE, MPI_COMM_GRD,          &
                          MPI_COMM_BCT, CROOT, FBCAST
#endif
#ifdef W3_MPRF
      USE WMMDATMD, ONLY: MDSP
#endif
      USE W3INITMD, ONLY: WWVER
      USE W3ODATMD, ONLY:  OFILES
! 
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
      INTEGER, INTENT(IN)        :: IDSI, IDSO, IDSS, IDST, IDSE,     &
                                    MPI_COMM
      CHARACTER*(*), INTENT(IN)  :: IFNAME
      CHARACTER*(*), INTENT(IN), OPTIONAL :: PREAMB
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: MDSE2, IERR, I, J, NMOVE, TTIME(2),  &
                                 ILOOP, MDSI2, SCRATCH, RNKMIN,       &
                                 RNKMAX, RNKTMP, GRPMIN, GRPMAX, II,  &
                                 NDSREC, NDSFND, NPTS, JJ, IP1, IPN,  &
                                 MPI_COMM_LOC, NMPSC2, JJJ, TOUT(2),  &
                                 TLST(2), NCPROC, NPOUTT, NAPLOC,     &
                                 NAPRES, NAPADD, NAPBCT, IFI, IFJ, IW,&
                                 IFT
      INTEGER                 :: STMPT(2), ETMPT(2)
#ifdef W3_MPI
      INTEGER                 :: IERR_MPI, BGROUP, LGROUP, IROOT
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      INTEGER, ALLOCATABLE    :: MDS(:,:), NTRACE(:,:), ODAT(:,:),    &
                                 TMPRNK(:), TMPGRP(:), NINGRP(:),     &
                                 TMOVE(:,:), LOADMP(:,:), IPRT(:,:),  &
                                 NDPOUT(:), OUTFF(:,:)
      REAL                    :: DTTST, XX, YY

#ifdef W3_MPRF
      REAL                    :: PRFT0, PRFTN
      REAL(KIND=8)            :: get_memory
#endif
      REAL, ALLOCATABLE       :: X(:), Y(:), AMOVE(:), DMOVE(:),      &
                                 RP1(:), RPN(:)
      LOGICAL                 :: FLT, TFLAGI, TFLAGS(-7:14), PSHARE
      LOGICAL, ALLOCATABLE    :: FLGRD(:,:,:), FLRBPI(:), BCDTMP(:),   &
                                 USEINP(:), LPRT(:), FLGR2(:,:,:),     &
                                 FLGD(:,:), FLG2(:,:), FLG2D(:,:),     &
                                 FLG1D(:), CPLINP(:)
      CHARACTER(LEN=1)        :: COMSTR
      CHARACTER(LEN=3)        :: IDSTR(9), IDTST
      CHARACTER(LEN=5)        :: STOUT, OUTSTR(6)
      CHARACTER(LEN=6)        :: ACTION(11), YESXX, XXXNO
      CHARACTER(LEN=8)        :: LFILE, STTIME
#ifdef W3_SHRD
      CHARACTER(LEN=9)        :: TFILE
#endif
      CHARACTER(LEN=13)       :: STDATE, MN, TNAMES(9)
      CHARACTER(LEN=40)       :: PN
      CHARACTER(LEN=13),                                              &
                  ALLOCATABLE :: INAMES(:,:), MNAMES(:)
      CHARACTER(LEN=40),                                              &
                  ALLOCATABLE :: PNAMES(:)
      CHARACTER(LEN=12)       :: FORMAT
#ifdef W3_DIST
      CHARACTER(LEN=18)       :: TFILE
#endif
#ifdef W3_MPRF
      CHARACTER(LEN=18)       :: PFILE
#endif

      CHARACTER(LEN=13)       :: IDFLDS(-7:9)
      CHARACTER(LEN=23)       :: DTME21
      CHARACTER(LEN=30)       :: IDOTYP(8)
      CHARACTER(LEN=80)       :: TNAME
      CHARACTER(LEN=80)       :: LINE
      CHARACTER(LEN=80)       :: LINEIN
      CHARACTER(LEN=8)        :: WORDS(6)

      TYPE OT2TPE
        INTEGER                    :: NPTS
        REAL, POINTER              :: X(:), Y(:)
        CHARACTER(LEN=40), POINTER :: PNAMES(:)
      END TYPE OT2TPE
!
      TYPE(OT2TPE), ALLOCATABLE    :: OT2(:)
!/
!/ ------------------------------------------------------------------- /
!/

      DATA IDFLDS / 'ice param. 1 ' , 'ice param. 2 ' ,               &
                    'ice param. 3 ' , 'ice param. 4 ' ,               &
                    'ice param. 5 ' ,                                 &
                    'mud density  ' , 'mud thkness  ' ,               &
                    'mud viscos.  ' ,                                 &
                    'water levels ' , 'currents     ' ,               &
                    'winds        ' , 'ice fields   ' ,               &
                    'momentum     ' , 'air density  ' ,               &
                    'mean param.  ' , '1D spectra   ' ,               &
                    '2D spectra   ' /
!
      DATA IDOTYP / 'Fields of mean wave parameters' ,                &
                    'Point output                  ' ,                &
                    'Track point output            ' ,                &
                    'Restart files                 ' ,                &
                    'Nesting data                  ' ,                &
                    'Separated wave field data     ' ,                &
                    'Fields for coupling           ' ,                &
                    'Restart files second request  '/
!
      DATA IDSTR  / 'LEV', 'CUR', 'WND', 'ICE', 'TAU', 'RHO',         &
                    'DT0', 'DT1', 'DT2' /
!
      DATA YESXX  / 'YES/--' /
      DATA XXXNO  / '---/NO' /
!
#ifdef W3_MPRF
      CALL PRINIT
      CALL PRTIME ( PRFT0 )
#endif
!
      CALL DATE_AND_TIME ( VALUES=CLKDT1 )
!
      MPI_COMM_LOC   = MPI_COMM
#ifdef W3_MPI
      MPI_COMM_MWAVE = MPI_COMM
      CALL MPI_COMM_SIZE ( MPI_COMM_MWAVE, NMPROC, IERR_MPI )
      CALL MPI_COMM_RANK ( MPI_COMM_MWAVE, IMPROC, IERR_MPI )
      IMPROC = IMPROC + 1
#endif
!
      IF ( PRESENT(PREAMB) ) FNMPRE = PREAMB
!/
!/ ------------------------------------------------------------------- /
! 1.  Multi-grid model intializations
! 1.a Unit numbers
!     Initialize dynamic assignment, errors and test to stdout
!
      CALL WMUINI ( 6, 6 )
!
! ... Identify reserved unit numbers
!
      CALL WMUSET ( 6,6,  5, .TRUE., 'SYS', 'stdin', 'Standart input' )
      CALL WMUSET ( 6,6,  6, .TRUE., 'SYS', 'stdout','Standart output')
!
#ifdef W3_NL2
      CALL WMUSET (6,6,103, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,104, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,105, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,106, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,107, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,108, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,109, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,110, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,111, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,112, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,113, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,114, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,117, .TRUE., 'FIX', DESC='Reserved SNL2' )
#endif
!
! ... Unit numbers from parameter list
!     Dynamic scripture updated per file
!
      MDSI   = IDSI
      MDSO   = IDSO
      MDSS   = IDSS
      MDST   = IDST
      MDSE   = IDSE
!
      IF ( IMPROC .EQ. NMPERR ) THEN
          MDSE2  = MDSE
        ELSE
          MDSE2  = -1
        END IF
!
! 1.b Subroutine tracing
!
      CALL ITRACE ( MDST, NTRMAX )
!
#ifdef W3_O10
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,900)
#endif
!
! 1.c Input file
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                      &
          WRITE (MDSS,910)  IFNAME, MDSI
!
      OPEN (MDSI,FILE=TRIM(FNMPRE)//IFNAME,STATUS='OLD',ERR=2000,      &
            IOSTAT=IERR)
      REWIND (MDSI)
      READ (MDSI,'(A)',END=2001,ERR=2002) COMSTR
      IF (COMSTR.EQ.' ') COMSTR = '$'
      CALL WMUSET ( MDSS, MDSS, MDSI, .TRUE., 'INP',                  &
                    TRIM(FNMPRE)//IFNAME, 'Model control input file')
!
! 1.d Log and test files
!
      LFILE  = 'log.mww3'
      IW     = 1 + INT ( LOG10 ( REAL(NMPROC) + 0.5 ) )
      IW     = MAX ( 3 , MIN ( 9 , IW ) ) 
      WRITE (FORMAT,'(A5,I1.1,A1,I1.1,A4)') '(A4,I',IW,'.',IW,',A5)'
#ifdef W3_SHRD
       TFILE  = 'test.mww3'
#endif
#ifdef W3_DIST
       WRITE (TFILE,FORMAT) 'test', IMPROC, '.mww3'
#endif
#ifdef W3_MPRF
       WRITE (PFILE,FORMAT) 'prf.', IMPROC, '.mww3'
#endif
!
      IF ( IMPROC .EQ. NMPLOG ) THEN
          OPEN (MDSO,FILE=TRIM(FNMPRE)//LFILE,ERR=2010,IOSTAT=IERR)
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,911)  LFILE, MDSO
          CALL WMUSET ( MDSS, MDSS, MDSO, .TRUE., 'OUT',              &
                        TRIM(FNMPRE)//LFILE, 'Log file')
        ELSE
          CALL WMUSET ( MDSS, MDSS, MDSO, .TRUE., 'XXX',              &
                       'Log file on other processors')
        END IF
!
      IF ( MDST.NE.MDSO .AND. MDST.NE.MDSS .AND. TSTOUT ) THEN
          IFT    = LEN_TRIM(TFILE)
          OPEN (MDST,FILE=TRIM(FNMPRE)//TFILE(:IFT),ERR=2011,IOSTAT=IERR)
          CALL WMUSET ( MDSS, MDST, MDST, .TRUE., 'OUT',              &
                        TRIM(FNMPRE)//TFILE(:IFT), 'Test output file')
        END IF
!
#ifdef W3_MPRF
      IFT    = LEN_TRIM(PFILE)
      CALL WMUGET ( MDSS, MDST, MDSP, 'OUT' )
      CALL WMUSET ( MDSS, MDST, MDSP, .TRUE., 'OUT',            &
                    TRIM(FNMPRE)//PFILE(:IFT), 'Profiling file')
      OPEN (MDSP,FILE=TRIM(FNMPRE)//PFILE(:IFT),ERR=2011,IOSTAT=IERR)
#endif
!
! 1.e Initial and test output
!
#ifdef W3_S
      CALL STRACE (IENT, 'WMINIT')
#endif
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,912) COMSTR
!
      IF ( IMPROC .EQ. NMPLOG ) THEN
          CALL WWDATE ( STDATE )
          CALL WWTIME ( STTIME )
          WRITE (MDSO,901) WWVER, STDATE, STTIME
        END IF
!
#ifdef W3_T
      WRITE(MDST,9000) IDSI, IDSO, IDSS, IDST, IDSE, IFNAME
#endif
!
! 2.  Set-up of data structures and I/O  ----------------------------- /
! 2.a Get number of grids
!     Note: grid for consolidated point output always generated.
!     Processor set as in W3INIT to minimize communication in WMIOPO
!
      CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
      READ (MDSI,*,END=2001,ERR=2002) NRGRD, NRINP, UNIPTS,           &
                                      IOSTYP, UPPROC, PSHARE
      IOSTYP = MAX ( 0 , MIN ( 3 , IOSTYP ) )
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
          WRITE (MDSS,920) NRGRD
          IF ( NRINP .EQ. 0 ) THEN
              WRITE (MDSS,921)
            ELSE
              WRITE (MDSS,922) NRINP
            END IF
          IF ( UNIPTS ) THEN
              WRITE (MDSS,923) YESXX
            ELSE
              WRITE (MDSS,923) XXXNO
            END IF
          WRITE (MDSS,1923) IOSTYP
          IF ( UNIPTS ) THEN
              IF ( UPPROC ) THEN
                  WRITE (MDSS,2923) YESXX
                ELSE
                  WRITE (MDSS,2923) XXXNO
                END IF
            END IF
          IF ( IOSTYP.GT.1 .AND. PSHARE ) THEN
              WRITE (MDSS,3923) YESXX
            ELSE IF ( IOSTYP.GT. 1 ) THEN
              WRITE (MDSS,3923) XXXNO
            END IF
        END IF
!
      IF ( NRGRD .LT. 1 ) GOTO 2020
      IF ( NRINP .LT. 0 ) GOTO 2021
      IF ( NRINP.EQ.0 .AND. .NOT.UNIPTS ) NRINP = -1
!
! 2.b Set up data structures
!
      CALL W3NMOD ( NRGRD, MDSE2, MDST, NRINP )
      CALL W3NDAT (        MDSE2, MDST )
      CALL W3NAUX (        MDSE2, MDST )
      CALL W3NOUT (        MDSE2, MDST )
      CALL W3NINP (        MDSE2, MDST ) 
      CALL WMNDAT (        MDSE2, MDST ) 
!
! 2.c Set up I/O for individual models (initial)
!
      ALLOCATE ( MDS(13,NRGRD), NTRACE(2,NRGRD), ODAT(40,0:NRGRD),    &
           FLGRD(NOGRP,NGRPP,NRGRD), OT2(0:NRGRD), FLGD(NOGRP,NRGRD), &
           MDSF(-NRINP:NRGRD,JFIRST:9), IPRT(6,NRGRD), LPRT(NRGRD),   &
           FLGR2(NOGRP,NGRPP,NRGRD),FLG2D(NOGRP,NGRPP), FLG1D(NOGRP), &
           FLG2(NOGRP,NRGRD),OUTFF(7,0:NRGRD))
!
      MDS    = -1
      MDSF   = -1
      FLGR2  = .FALSE.
      FLG2   = .FALSE.
      LPRT   = .FALSE.
      IPRT   = 0
!
! ... Fixed and recycleable unit numbers.      
!
      CALL WMUGET ( MDSE, MDST, NDSREC, 'INP' )
      CALL WMUSET ( MDSE, MDST, NDSREC, .TRUE., 'I/O', NAME='...',    &
                    DESC='Recyclable I/O (mod_def etc.)' )
      CALL WMUGET ( MDSE, MDST, SCRATCH, 'SCR' )
      CALL WMUSET ( MDSE, MDST, SCRATCH, .TRUE., DESC='Scratch file', &
                    NAME=TRIM(FNMPRE)//'ww3_multi.scratch' )
!
      IF(MDST.EQ.NDSREC)THEN
         IF   ( IMPROC .EQ. NMPERR )                                  &
            WRITE(MDSE,'(A,I8)')'RECYCLABLE UNIT NUMBERS AND '&
            //'TEST OUTPUT UNIT NUMBER ARE THE SAME : ',MDST
         CALL EXTCDE ( 15 )
      ENDIF

      DO I=1, NRGRD
        MDS   ( 2,I) =  6
        MDS   ( 3,I) = MDST
        MDS   ( 4,I) =  6
        MDS   ( 5,I) = NDSREC
        MDS   ( 6,I) = NDSREC
        NTRACE( 1,I) = MDST
        NTRACE( 2,I) = NTRMAX
        END DO
!
#ifdef W3_T
      WRITE (MDST,9020) 'INITIAL'
      DO I=1, NRGRD
        WRITE (MDST,9021) I, MDS(:,I), NTRACE(:,I)
        END DO
#endif
!
! 3.  Get individual grid information -------------------------------- /
!
!     Version 3.07: For now we simply read the input data flags,
!                   skip the homogeneous option. Later on, we want
!                   to have the options to use input from common
!                   sources, and from communication rather than
!                   files.
!
      ALLOCATE ( INAMES(2*NRGRD,JFIRST:9), MNAMES(-NRINP:2*NRGRD),   &
                 TMPRNK(2*NRGRD), TMPGRP(2*NRGRD), NINGRP(2*NRGRD),  &
                 RP1(2*NRGRD), RPN(2*NRGRD), BCDTMP(NRGRD+1:2*NRGRD) )
      ALLOCATE ( GRANK(NRGRD), GRGRP(NRGRD), USEINP(NRINP) )
      ALLOCATE ( CPLINP(NRINP) )
      GRANK  = -1
      GRGRP  = -1
      USEINP = .FALSE.
      CPLINP = .FALSE.
!
! 3.a Read data
!
#ifdef W3_T
      WRITE (MDST,9030)
#endif
!
! 3.a.1 Input grids
!
      DO I=1, NRINP
!
        CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
        CALL W3SETI ( -I, MDSE, MDST )
        INFLAGS1 = .FALSE.
        READ (MDSI,*,END=2001,ERR=2002) MNAMES(-I), INFLAGS1(JFIRST:9)
!
        END DO
!
! 3.a.2 Unified point output grid.
!
      IF ( UNIPTS ) THEN
!
          CALL W3SETI ( 0, MDSE, MDST )
          CALL W3SETO ( 0, MDSE, MDST )
          INFLAGS1 = .FALSE.
          NDST   = MDST
          NDSE   = MDSE
!
          CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
          READ (MDSI,*,END=2001,ERR=2002) MNAMES(0)
!
          IF ( IOSTYP .LE. 1 ) THEN
              NMPUPT = MAX(1,NMPROC-2)
            ELSE
              NMPUPT = NMPROC
            END IF
!
        END IF
!
! 3.a.3 Read wave grids
!
      DO I=NRGRD+1, 2*NRGRD
        CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
        READ (MDSI,*,END=2001,ERR=2002) MNAMES(I), TNAMES(:),         &
              TMPRNK(I), TMPGRP(I), RP1(I), RPN(I), BCDTMP(I)
        INAMES(I,:) = TNAMES(:)
        RP1(I) = MAX ( 0. , MIN ( 1. , RP1(I) ) )
        RPN(I) = MAX ( RP1(I) , MIN ( 1. , RPN(I) ) )
        END DO
!
! 3.a.4 Sort wave grids
!
      RNKTMP = MINVAL ( TMPRNK(NRGRD+1:2*NRGRD) )
      I      = 0
!
      DO
        DO J=NRGRD+1, 2*NRGRD
          IF ( TMPRNK(J) .EQ. RNKTMP ) THEN
              I      = I + 1
              CALL W3SETI ( I, MDSE, MDST )
              INFLAGS1      = .FALSE.
#ifdef W3_MGW
              INFLAGS1(10)   = .TRUE.
#endif
#ifdef W3_MGP
              INFLAGS1(10)   = .TRUE.
#endif
              INAMES(I,:)= INAMES(J,:)
              MNAMES(I)  = MNAMES(J)
              TMPRNK(I)  = TMPRNK(J)
              TMPGRP(I)  = TMPGRP(J)
              RP1(I)     = RP1(J)
              RPN(I)     = RPN(J)
              BCDUMP(I)  = BCDTMP(J)
#ifdef W3_T
              WRITE (MDST,9031) I, MNAMES(I), INFLAGS1, TMPRNK(I),    &
                                   TMPGRP(I), RP1(I), RPN(I)
#endif
            END IF
          END DO
        IF ( I .EQ. NRGRD ) EXIT
        RNKTMP = RNKTMP + 1
        END DO
!
! 3.a.5 Set input flags
!
      ALLOCATE ( INPMAP(NRGRD,JFIRST:10), IDINP(-NRINP:NRGRD,JFIRST:10) )
      INPMAP = 0
      IDINP  = '---'
!
      DO I=1, NRGRD
         CALL W3SETI ( I, MDSE, MDST )
         DO J=JFIRST, 9
            IF ( INAMES(I,J) .EQ. 'native' ) THEN
            ! *** forcing input from file & defined on the native grid ***
                INFLAGS1(J) = .TRUE.
              ELSE
                INFLAGS1(J) = .FALSE.
                IF ( INAMES(I,J)(1:4) .EQ. 'CPL:' ) THEN
                    IF ( INAMES(I,J)(5:) .EQ. 'native' ) THEN
                      ! *** forcing input from CPL & defined on the native grid ***
                        INFLAGS1(J) = .TRUE.
                        INPMAP(I,J) = -999
                      ELSE
                      ! *** forcing input from CPL & defined on an input grid ***
                        DO JJ=1, NRINP
                          IF ( MNAMES(-JJ) .EQ. INAMES(I,J)(5:) ) THEN
                              INPMAP(I,J) = -JJ
                              EXIT
                            END IF
                          END DO
                        IF ( INPMAP(I,J) .EQ. 0 ) GOTO 2030
                        IF ( .NOT. INPUTS(INPMAP(I,J))%INFLAGS1(J) ) GOTO 2031
                        USEINP(-INPMAP(I,J)) = .TRUE.
                        CPLINP(-INPMAP(I,J)) = .TRUE.
                      END IF
                  ELSE IF ( INAMES(I,J) .NE. 'no' ) THEN
                    ! *** forcing input from file & defined on an input grid ***
                    DO JJ=1, NRINP
                       IF ( MNAMES(-JJ) .EQ. INAMES(I,J) ) THEN
                          INPMAP(I,J) = JJ
                          INFLAGS2(J) = .TRUE.
                          EXIT
                       END IF
                    END DO
                    IF ( INPMAP(I,J) .EQ. 0 ) GOTO 2030
                    IF ( .NOT. INPUTS(-INPMAP(I,J))%INFLAGS1(J) ) GOTO 2031
                    USEINP(INPMAP(I,J)) = .TRUE.
                  END IF
              END IF
!        INFLAGS2 is initial value of INFLAGS1. Unlike INFLAGS1, 
!           it does not change during the simulation
         IF(.NOT. INFLAGS2(J)) INFLAGS2(J)=INFLAGS1(J)
         END DO !         DO J=JFIRST, 9
      END DO !      DO I=1, NRGRD
!
      DO I=1, NRINP
        IF ( .NOT.USEINP(I) .AND.                                     &
             MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
            II     = LEN_TRIM(MNAMES(-I))
            WRITE (MDSE,1032) MNAMES(-I)(1:II)
          END IF
        END DO
!
! 3.b Assign input file unit numbers
!
      DO I=-NRINP, NRGRD
        IF ( I .EQ. 0 ) CYCLE
        CALL W3SETI ( I, MDSE, MDST )
        DO J=JFIRST, 9
          IF ( I .GE. 1 ) THEN
              IF ( INPMAP(I,J) .LT. 0 ) CYCLE
            END IF
          IF ( INFLAGS1(J) ) THEN
              CALL WMUGET ( MDSE, MDST, NDSFND, 'INP' )
              CALL WMUSET ( MDSE, MDST, NDSFND, .TRUE.,               &
                            DESC='Input data file' )
              MDSF(I,J) = NDSFND
            END IF
          END DO
        END DO
!
#ifdef W3_T
      WRITE (MDST,9022)
      DO I=-NRINP, NRGRD
        IF ( I .EQ. 0 ) CYCLE
        WRITE (MDST,9021) I, MDSF(I,JFIRST:9)
        END DO
#endif
!
! 3.c Set rank and group data
!
#ifdef W3_T
      WRITE (MDST,9032)
#endif
!
      RNKMAX = MAXVAL ( TMPRNK(1:NRGRD) ) + 1
      RNKTMP = 0
!
      DO
        RNKMIN = MINVAL ( TMPRNK(1:NRGRD) )
        IF ( RNKMIN .EQ. RNKMAX ) EXIT
        RNKTMP = RNKTMP + 1
        DO I=1, NRGRD
          IF ( TMPRNK(I) .EQ. RNKMIN ) THEN
              GRANK(I)  = RNKTMP
              TMPRNK(I) = RNKMAX
            END IF
          END DO
        END DO
!
#ifdef W3_T
      DO I=1, NRGRD
        WRITE (MDST,9033) I, MNAMES(I), GRANK(I)
        END DO
#endif
!
      RNKMAX = RNKTMP
      GRPMAX = MAXVAL ( TMPGRP(1:NRGRD) ) + 1
      NRGRP  = 0
      NINGRP = 0
!
      DO RNKTMP=1, RNKMAX
        DO
          GRPMIN = GRPMAX
          DO I=1, NRGRD
            IF ( GRANK(I) .EQ. RNKTMP )                               &
                 GRPMIN = MIN ( GRPMIN , TMPGRP(I) )
            END DO
          IF ( GRPMIN .EQ. GRPMAX ) EXIT
          NRGRP  = NRGRP + 1
          DO I=1, NRGRD
            IF ( GRANK(I).EQ.RNKTMP .AND. GRPMIN.EQ.TMPGRP(I) ) THEN
                GRGRP(I)  = NRGRP
                TMPGRP(I) = GRPMAX 
                NINGRP(NRGRP) = NINGRP(NRGRP) + 1
              END IF
            END DO
          END DO
        END DO
!
#ifdef W3_T
      WRITE (MDST,9034) NRGRP
      DO I=1, NRGRD
        WRITE (MDST,9033) I, MNAMES(I), GRGRP(I)
        END DO
      WRITE (MDST,9035) NINGRP(1:NRGRP)
#endif
!
      ALLOCATE ( INGRP(NRGRP,0:MAXVAL(NINGRP(:NRGRP))) )
      DEALLOCATE ( TMPRNK, TMPGRP, NINGRP, BCDTMP )
      INGRP = 0
!
      DO I=1, NRGRD
        INGRP(GRGRP(I),0) = INGRP(GRGRP(I),0) + 1
        INGRP(GRGRP(I),INGRP(GRGRP(I),0)) = I
        END DO
!
#ifdef W3_T
      WRITE (MDST,9036)
      DO J=1, NRGRP
        WRITE (MDST,9037) J, INGRP(J,:INGRP(J,0))
        END DO
#endif
!
!
! 3.d Unified point output
!
#ifdef W3_MPRF
      CALL PRTIME ( PRFTN )
      WRITE (MDSP,990) PRFT0, PRFTN, get_memory(), 'START Sec. 8.b'
      PRFT0  = PRFTN
#endif
!
      IF ( UNIPTS ) THEN
!
          J      = LEN_TRIM(MNAMES(0))
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
              WRITE (MDSS,986) MNAMES(0)(1:J)
              WRITE (MDSS,987)
            END IF
!
          CALL W3IOGR ( 'GRID', NDSREC, 0, MNAMES(0)(1:J) )
!
        END IF
!
! 3.e Output
!
      IF ( NRINP .GT. 0 ) THEN
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,924)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,924)
          DO I=1, NRINP
            IF ( .NOT. USEINP(I) ) CYCLE
            CALL W3SETI ( -I, MDSE, MDST )
            ACTION(1:6) = '---   '
            DO J=JFIRST, 6
              IF ( INFLAGS1(J) ) ACTION(J) = ' X    '
              END DO
            ACTION(7:9) = '-     '
            IF ( INFLAGS1(7) ) ACTION(7) = '1     '
            IF ( INFLAGS1(8) ) ACTION(8) = '2     '
            IF ( INFLAGS1(9) ) ACTION(9) = '3     '
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                WRITE (MDSS,925) I, MNAMES(-I), ACTION(JFIRST:9)
            IF ( NMPLOG .EQ. IMPROC )                                 &
                WRITE (MDSO,925) I, MNAMES(-I), ACTION(JFIRST:9)
            END DO
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,926)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,926)
        END IF
!
      IF ( UNIPTS ) THEN
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,927)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,927)
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
                WRITE (MDSS,928) MNAMES(0)
          IF ( NMPLOG .EQ. IMPROC )                                   &
                WRITE (MDSO,928) MNAMES(0)
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,929)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,929)
        END IF
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,930)
      IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,930)
      DO I=1, NRGRD
        CALL W3SETI ( I, MDSE, MDST )
        ACTION(1:6) = '---   '
        DO J=JFIRST, 6
          IF ( INFLAGS1(J) .AND. INPMAP(I,J) .EQ. 0 ) THEN
              ACTION(J) = 'native'
            ELSE IF ( INFLAGS1(J) .AND. INPMAP(I,J) .EQ. -999 ) THEN
              ACTION(J) = 'native'
            ELSE IF ( INPMAP(I,J) .GT. 0 ) THEN
              ACTION(J) = MNAMES(-INPMAP(I,J))
            ELSE IF ( INPMAP(I,J) .LT. 0 ) THEN
              ACTION(J) = MNAMES( INPMAP(I,J))
            END IF
          END DO
        ACTION(7:11) = '-     '
        IF ( INFLAGS1(7) ) ACTION(7) = '1     '
        IF ( INFLAGS1(8) ) ACTION(8) = '2     '
        IF ( INFLAGS1(9) ) ACTION(9) = '3     '
        IF ( INFLAGS1(10) ) THEN
            ACTION(10) = 'yes   '
          ELSE
            ACTION(10) = 'no    '
          END IF
        IF ( BCDUMP(I) ) ACTION(11) = 'y     '
        IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                    &
            WRITE (MDSS,931) I, MNAMES(I), ACTION(1:10), GRANK(I),     &
                             GRGRP(I), ACTION(11)
        IF ( NMPLOG .EQ. IMPROC )                                     &
            WRITE (MDSO,931) I, MNAMES(I), ACTION(1:10), GRANK(I),     &
                             GRGRP(I), ACTION(11)
      END DO
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,932)
      IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,932)
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                      &
          WRITE (MDSS,933) 'Group information'
      IF ( NMPLOG .EQ. IMPROC )                                       &
          WRITE (MDSO,933) 'Group information'
      DO J=1, NRGRP
        WRITE (LINE(1:6),'(1X,I3,2X)') J
        JJJ    = 6
        DO JJ=1, INGRP(J,0)
          IF ( JJJ .GT. 60 ) THEN
              IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )              &
                                        WRITE (MDSS,934) LINE(1:JJJ)
              IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,934) LINE(1:JJJ)
              LINE(1:6) = '      '
              JJJ       = 6
            END IF 
          WRITE (LINE(JJJ+1:JJJ+3),'(I3)') INGRP(J,JJ)
!
          LINE(JJJ+4:JJJ+5) = ' ('
          WRITE (LINE(JJJ+6:JJJ+11),'(F6.4)') RP1(INGRP(J,JJ))
          LINE(JJJ+12:JJJ+12) = '-'
          WRITE (LINE(JJJ+13:JJJ+18),'(F6.4)') RPN(INGRP(J,JJ))
          LINE(JJJ+19:JJJ+19) = ')'
          JJJ    = JJJ + 19
!
          END DO
        IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                    &
                                  WRITE (MDSS,934) LINE(1:JJJ)
        IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,934) LINE(1:JJJ)
        END DO
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,935)
      IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,935)
!
! 4.  Model run time information etc. -------------------------------- /
!
!     Version 3.07: Same for all grids, diversify later ....
!     If invoked as ESMF Component, then STIME and ETIME are set
!     in WMESMFMD from the external clock.
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,940)
!
      CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
      IF (IS_ESMF_COMPONENT) THEN
          READ (MDSI,*,END=2001,ERR=2002) STMPT, ETMPT
        ELSE
          READ (MDSI,*,END=2001,ERR=2002) STIME, ETIME
        END IF
!
      CALL STME21 ( STIME , DTME21 )
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,941) DTME21
      CALL STME21 ( ETIME , DTME21 )
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,942) DTME21
!
      DO I=1, NRGRD
        CALL W3SETW ( I, MDSE, MDST )
        TIME   = STIME
        END DO 
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,943)
!              
      CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
      READ (MDSI,*,END=2001,ERR=2002) FLGHG1, FLGHG2
      FLGHG2 = FLGHG1 .AND. FLGHG2
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
          IF ( FLGHG1 ) THEN
              WRITE (MDSS,944) YESXX
            ELSE
              WRITE (MDSS,944) XXXNO
            END IF
          IF ( FLGHG2 ) THEN
              WRITE (MDSS,945) YESXX
            ELSE
              WRITE (MDSS,945) XXXNO
            END IF
        END IF
!
! 5.  Output requests ------------------------------------------------ /
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,950)
      NPTS   = 0
!
! 5.a Loop over types for unified output
!
      NOTYPE = 6
!!/COU      NOTYPE = 7
      DO J=1, NOTYPE
!
! 5.b Process standard line
!
        CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
!
        IF(J .LE. 2) THEN
          WORDS(1:6)=''
          READ (MDSI,'(A)') LINEIN
          READ(LINEIN,*,iostat=ierr) WORDS
!        
          IF(J .LE. 1) THEN
            READ(WORDS( 1 ), * ) ODAT(1,1)
            READ(WORDS( 2 ), * ) ODAT(2,1)
            READ(WORDS( 3 ), * ) ODAT(3,1)
            READ(WORDS( 4 ), * ) ODAT(4,1)
            READ(WORDS( 5 ), * ) ODAT(5,1)
          ELSE
            READ(WORDS( 1 ), * ) ODAT(6,1)
            READ(WORDS( 2 ), * ) ODAT(7,1)
            READ(WORDS( 3 ), * ) ODAT(8,1)
            READ(WORDS( 4 ), * ) ODAT(9,1)
            READ(WORDS( 5 ), * ) ODAT(10,1)
          END IF

          IF (WORDS(6) .NE. '0' .AND. WORDS(6) .NE. '1') THEN
            OUTFF(J,1)=0
          ELSE
            READ(WORDS( 6 ), * ) OUTFF(J,1)
!            print*,' Number of data: ', 6
          END IF
! CHECKPOINT
        ELSE IF(J .EQ. 4) THEN
            WORDS(1:6)=''
            READ (MDSI,'(A)') LINEIN
            READ(LINEIN,*,iostat=ierr) WORDS
!
            READ(WORDS( 1 ), * ) ODAT(16,1)
            READ(WORDS( 2 ), * ) ODAT(17,1)
            READ(WORDS( 3 ), * ) ODAT(18,1)
            READ(WORDS( 4 ), * ) ODAT(19,1)
            READ(WORDS( 5 ), * ) ODAT(20,1)
            IF (WORDS(6) .EQ. 'T') THEN
              CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
              READ (MDSI,*,END=2001,ERR=2002)(ODAT(I,1),I=5*(8-1)+1,5*8)
              END IF
        ELSE
          READ (MDSI,*,END=2001,ERR=2002)(ODAT(I,1),I=5*(J-1)+1,5*J)
          OUTFF(J,1) = 0
        END IF
!
        OUTPTS(1)%OFILES(J)=OUTFF(J,1)
!         
!
        ODAT(5*(J-1)+3,1) = MAX ( 0 , ODAT(5*(J-1)+3,1) )
!
        IF ( ODAT(5*(J-1)+3,1) .NE. 0 ) THEN
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                WRITE (MDSS,951) J, IDOTYP(J)
            TTIME(1) = ODAT(5*(J-1)+1,1)
            TTIME(2) = ODAT(5*(J-1)+2,1)
            CALL STME21 ( TTIME , DTME21 )
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                 WRITE (MDSS,952) DTME21
            TTIME(1) = ODAT(5*(J-1)+4,1)
            TTIME(2) = ODAT(5*(J-1)+5,1)
            CALL STME21 ( TTIME , DTME21 )
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                 WRITE (MDSS,953) DTME21
            TTIME(1) = 0
            TTIME(2) = 0
            DTTST    = REAL ( ODAT(5*(J-1)+3,1) )
            CALL TICK21 ( TTIME , DTTST  )
            CALL STME21 ( TTIME , DTME21 )
            IF ( ( ODAT(5*(J-1)+1,1) .NE. ODAT(5*(J-1)+4,1) .OR.      &
                   ODAT(5*(J-1)+2,1) .NE. ODAT(5*(J-1)+5,1) ) .AND.   &
                   MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                DO I=1, 18
                  IF ( DTME21(I:I).NE.'0' .AND.                       &
                       DTME21(I:I).NE.'/' .AND.                       &
                       DTME21(I:I).NE.' ' .AND.                       &
                       DTME21(I:I).NE.':' ) EXIT
                  DTME21(I:I) = ' '
                  END DO
                WRITE (MDSS,954) DTME21(1:19)

              END IF
            IF ( J .EQ. 1 ) THEN
!
! 5.c Type 1: fields of mean wave parameters
!
              FLGRD(:,:,:)=.FALSE. ! Initialize FLGRD 
              CALL W3READFLGRD ( MDSI, MDSS, MDSO, MDSE2, COMSTR, FLG1D,    &
                                 FLG2D, IMPROC, NMPSCR, IERR )
              FLGRD(:,:,1)=FLG2D
              FLGD(:,1)   =FLG1D
!
            ELSE IF ( J .EQ. 2 ) THEN
!
! 5.d Type 2: point output
!
                DO ILOOP=1, 2
                  IF ( ILOOP .EQ. 1 ) THEN
                      MDSI2  = MDSI
                      IF ( IMPROC .EQ. 1 ) OPEN                       &
                         (SCRATCH,FILE=TRIM(FNMPRE)//'ww3_multi.scratch')
                    ELSE
                      MDSI2  = SCRATCH
#ifdef W3_MPI
                      CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
#endif
                      OPEN                                            &
                         (SCRATCH,FILE=TRIM(FNMPRE)//'ww3_multi.scratch')
                      REWIND (SCRATCH)
                      IF (NPTS.GT.0) THEN
                         ALLOCATE ( X(NPTS), Y(NPTS), PNAMES(NPTS) )
                      ELSE
                         GOTO 2054
                      END IF
                    END IF
!
                  NPTS = 0
                  DO
                    CALL NEXTLN ( COMSTR , MDSI2 , MDSE2 )
                    READ (MDSI2,*,END=2001,ERR=2002) XX, YY, PN
!
                    IF ( ILOOP.EQ.1 .AND. IMPROC.EQ.1 ) THEN
                        BACKSPACE (MDSI)
                        READ (MDSI,'(A)') LINE
                        WRITE (SCRATCH,'(A)') LINE
                      END IF
!
                    IF ( PN .EQ. 'STOPSTRING' ) EXIT
!
                    NPTS = NPTS + 1
                    IF ( ILOOP .EQ. 1 ) CYCLE
!
                    X(NPTS)      = XX
                    Y(NPTS)      = YY
                    PNAMES(NPTS) = PN
                    IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                        IF ( NPTS .EQ. 1 ) THEN
                            WRITE (MDSS,957)       XX, YY, PN
                          ELSE
                            WRITE (MDSS,958) NPTS, XX, YY, PN
                          END IF
                      END IF
!
                    END DO
!
                  IF ( IMPROC.EQ.1 .AND. ILOOP.EQ.1 ) CLOSE (SCRATCH)
                  END DO
!
                IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC .AND.        &
                     NPTS.EQ.0 ) WRITE (MDSS,959)
                IF ( IMPROC .EQ. 1 ) THEN
#ifdef W3_MPI
                    CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
#endif
                    CLOSE (SCRATCH,STATUS='DELETE')
                  ELSE
                    CLOSE (SCRATCH)
#ifdef W3_MPI
                    CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
#endif
                  END IF
!
              ELSE IF ( J .EQ. 3 ) THEN
!
! 5.e Type 3: track output
!
                CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
                READ (MDSI,*,END=2001,ERR=2002) TFLAGI
                IF ( .NOT. TFLAGI ) MDS(11,:) = -MDS(11,:)
                IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                    IF ( .NOT. TFLAGI ) THEN
                        WRITE (MDSS,960) 'input', 'UNFORMATTED'
                      ELSE
                        WRITE (MDSS,960) 'input', 'FORMATTED'
                      END IF
                  END IF
!
              ELSE IF ( J .EQ. 4 ) THEN
!
! 5.f Type 4: restart files (no additional data)
!
              ELSE IF ( J .EQ. 5 ) THEN
!
! 5.g Type 5: nesting data (no additional data)
!
              ELSE IF ( J .EQ. 6 ) THEN
!
! 5.h Type 6: partitioned wave field data
!
                CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
                READ (MDSI,*,END=2001,ERR=2002) IPRT(:,1), LPRT(1)
                IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                    WRITE (MDSS,961) IPRT(:,1)
                    IF ( .NOT. LPRT(1) ) THEN
                        WRITE (MDSS,960) 'output', 'UNFORMATTED'
                      ELSE
                        WRITE (MDSS,960) 'output', 'FORMATTED'
                      END IF
                  END IF

!!/COU! NOT YET IMPLEMENTED
!
!!/COU              ELSE IF ( J .EQ. 7 ) THEN
!
! 5.i Type 7: coupling
!
!!/COU              CALL W3READFLGRD ( MDSI, MDSS, MDSO, MDSE2, COMSTR, FLG1D,    &
!!/COU                                 FLG2D, IMPROC, NMPSCR, IERR )
!!/COU              FLGR2(:,:,1)=FLG2D
!!/COU              FLG2(:,1)   =FLG1D
!
! ... End of output type selecttion ELSE IF
!
              END IF
!
! ... End of IF in 5.b
!
          END IF
!
! ... End of loop in 5.a
!
        END DO
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! Checkpoint
       J=8
        IF ( ODAT(5*(J-1)+3,1) .NE. 0 ) THEN
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                WRITE (MDSS,951) J, IDOTYP(J)
            TTIME(1) = ODAT(5*(J-1)+1,1)
            TTIME(2) = ODAT(5*(J-1)+2,1)
            CALL STME21 ( TTIME , DTME21 )
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                 WRITE (MDSS,952) DTME21
            TTIME(1) = ODAT(5*(J-1)+4,1)
            TTIME(2) = ODAT(5*(J-1)+5,1)
            CALL STME21 ( TTIME , DTME21 )
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                 WRITE (MDSS,953) DTME21
            TTIME(1) = 0
            TTIME(2) = 0
            DTTST    = REAL ( ODAT(5*(J-1)+3,1) )
            CALL TICK21 ( TTIME , DTTST  )
            CALL STME21 ( TTIME , DTME21 )
            IF ( ( ODAT(5*(J-1)+1,1) .NE. ODAT(5*(J-1)+4,1) .OR.      &
                   ODAT(5*(J-1)+2,1) .NE. ODAT(5*(J-1)+5,1) ) .AND.   &
                   MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                DO I=1, 18
                  IF ( DTME21(I:I).NE.'0' .AND.                       &
                       DTME21(I:I).NE.'/' .AND.                       &
                       DTME21(I:I).NE.' ' .AND.                       &
                       DTME21(I:I).NE.':' ) EXIT
                  DTME21(I:I) = ' '
                  END DO
                WRITE (MDSS,954) DTME21(1:19)
              END IF
        END IF
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
! 5.i Set all grids to unified output
!
      IF ( UNIPTS ) THEN
        ODAT(6:10,0) = ODAT(6:10,1)
        ODAT( 8 , 1) = 0
        OUTPTS(1)%OFILES(1)   = OUTFF(1,1)
        END IF
!
      DO I=2, NRGRD
        ODAT(:,I)  = ODAT(:,1)
        OUTFF(:,I) = OUTFF(:,1)
        OUTPTS(I)%OFILES(:)=OUTFF(:,1)
        FLGD(:,I) = FLGD(:,1)
        FLGRD(:,:,I) = FLGRD(:,:,1)
        FLG2(:,I) = FLG2(:,1)
        FLGR2(:,:,I) = FLGR2(:,:,1)
        IPRT(:,I)  = IPRT(:,1)
        LPRT(I)    = LPRT(1)
        END DO
!
      IF ( NPTS.EQ.0 .OR. ODAT(8,0).EQ.0 ) UNIPTS = .FALSE.
      IF ( UNIPTS ) THEN
          IF ( ( NPTS.EQ.0 .OR.  ODAT(8,0).EQ.0 ) .AND.               &
              IMPROC.EQ.NMPERR ) WRITE (MDSE,1050)
          IF ( NPTS.EQ.0 .OR. ODAT(8,0).EQ.0 ) UNIPTS = .FALSE.
          OT2(0)%NPTS = NPTS
          ALLOCATE (OT2(0)%X(NPTS),OT2(0)%Y(NPTS),OT2(0)%PNAMES(NPTS))
          OT2(0)%X      = X
          OT2(0)%Y      = Y
          OT2(0)%PNAMES = PNAMES
          DO I=1, NRGRD
            OT2(I)%NPTS = 0
            ALLOCATE (OT2(I)%X(1),OT2(I)%Y(1),OT2(I)%PNAMES(1))
            END DO
        ELSE
          DO I=1, NRGRD
            OT2(I)%NPTS = NPTS
            IF ( NPTS .EQ. 0 ) THEN
                ALLOCATE (OT2(I)%X(1),OT2(I)%Y(1),OT2(I)%PNAMES(1))
              ELSE
                ALLOCATE (OT2(I)%X(NPTS),OT2(I)%Y(NPTS),              &
                          OT2(I)%PNAMES(NPTS))
                OT2(I)%X      = X
                OT2(I)%Y      = Y
                OT2(I)%PNAMES = PNAMES
              END IF
            END DO
        END IF
!
! 5.j Endless loop for correcting output per grid
!
      DO
        CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
        READ (MDSI,*,END=2001,ERR=2002) MN, J
!
! 5.j.1 Bail out loop for output type 0
!
        IF ( J .EQ. 0 ) EXIT
!
! 5.j.2 Find the grid number
!
        II     = LEN_TRIM(MN)
        DO I=1, NRGRD
          IF ( MN(:II) .EQ. MNAMES(I)(1:II) ) EXIT
          END DO
!
        IF ( I .GT. NRGRD ) GOTO 2051
        IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                    &
            WRITE (MDSS,962) MN(1:II), I
!
! 5.j.3 Check the output type
!
        IF ( J.LT.0 .OR. J.GT. NOTYPE ) GOTO 2052
        IF ( J.EQ.2 .AND. UNIPTS ) GOTO 2053
        IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                    &
            WRITE (MDSS,951) J, IDOTYP(J)
!
! 5.k Process standard line
!
        CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
        IF(J .LE. 2) THEN
          OUTFF(J,I)=0
          WORDS(1:6) =''
          READ (MDSI,'(A)') LINEIN
          READ(LINEIN,*,iostat=ierr) WORDS
          IF(J .EQ. 1) THEN
            READ(WORDS( 1 ), * ) ODAT(1,I)
            READ(WORDS( 2 ), * ) ODAT(2,I)
            READ(WORDS( 3 ), * ) ODAT(3,I)
            READ(WORDS( 4 ), * ) ODAT(4,I)
            READ(WORDS( 5 ), * ) ODAT(5,I)
          ELSE
            READ(WORDS( 1 ), * ) ODAT(6,I)
            READ(WORDS( 2 ), * ) ODAT(7,I)
            READ(WORDS( 3 ), * ) ODAT(8,I)
            READ(WORDS( 4 ), * ) ODAT(9,I)
            READ(WORDS( 5 ), * ) ODAT(10,I)
          END IF
          IF (WORDS(6) .NE. '0' .AND. WORDS(6) .NE. '1') THEN
            OUTFF(J,I)=0
          ELSE
            READ(WORDS( 6 ), * ) OUTFF(J,I)
          END IF
!
        ELSE
          READ (MDSI,*,END=2001,ERR=2002)(ODAT(II,I),II=5*(J-1)+1,5*J)
          OUTFF(J,I) = 0
        END IF
!         
        OUTPTS(I)%OFILES(J)=OUTFF(J,I)
!
        ODAT(5*(J-1)+3,I) = MAX ( 0 , ODAT(5*(J-1)+3,I) )
! 
        IF ( ODAT(5*(J-1)+3,I) .NE. 0 ) THEN
            TTIME(1) = ODAT(5*(J-1)+1,I)
            TTIME(2) = ODAT(5*(J-1)+2,I)
            CALL STME21 ( TTIME , DTME21 )
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                 WRITE (MDSS,952) DTME21
            TTIME(1) = ODAT(5*(J-1)+4,I)
            TTIME(2) = ODAT(5*(J-1)+5,I)
            CALL STME21 ( TTIME , DTME21 )
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                 WRITE (MDSS,953) DTME21
            TTIME(1) = 0
            TTIME(2) = 0
            DTTST    = REAL ( ODAT(5*(J-1)+3,I) )
            CALL TICK21 ( TTIME , DTTST  )
            CALL STME21 ( TTIME , DTME21 )
            IF ( ( ODAT(5*(J-1)+1,I) .NE. ODAT(5*(J-1)+4,I) .OR.      &
                   ODAT(5*(J-1)+2,I) .NE. ODAT(5*(J-1)+5,I) ) .AND.   &
                   MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                DO II=1, 18
                  IF ( DTME21(II:II).NE.'0' .AND.                     &
                       DTME21(II:II).NE.'/' .AND.                     &
                       DTME21(II:II).NE.' ' .AND.                     &
                       DTME21(II:II).NE.':' ) EXIT
                  DTME21(II:II) = ' '
                  END DO
                WRITE (MDSS,954) DTME21(1:19)
              END IF
!
            IF ( J .EQ. 1 ) THEN
!
! 5.l Type 1: fields of mean wave parameters
!
                CALL W3READFLGRD ( MDSI, MDSS, MDSO, MDSE2, COMSTR,    &
                                 FLG1D, FLG2D, IMPROC, NMPSCR, IERR )
                FLGD(:,I)    = FLG1D
                FLGRD(:,:,I) = FLG2D
!
              ELSE IF ( J .EQ. 2 ) THEN
!
! 5.m Type 2: point output
!
                DO ILOOP=1, 2
                  IF ( ILOOP .EQ. 1 ) THEN
                      MDSI2  = MDSI
                      IF ( IMPROC .EQ. 1 ) OPEN                       &
                         (SCRATCH,FILE=TRIM(FNMPRE)//'ww3_multi.scratch')
                    ELSE
                      MDSI2  = SCRATCH
#ifdef W3_MPI
                      CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
#endif
                      OPEN                                            &
                         (SCRATCH,FILE=TRIM(FNMPRE)//'ww3_multi.scratch')
                      REWIND (SCRATCH)
                      DEALLOCATE ( OT2(I)%X, OT2(I)%Y, OT2(I)%PNAMES )
                      ALLOCATE ( OT2(I)%X(OT2(I)%NPTS),               &
                                 OT2(I)%Y(OT2(I)%NPTS),               &
                                 OT2(I)%PNAMES(OT2(I)%NPTS) )
                    END IF
!
                  OT2(I)%NPTS = 0
                  DO
                    CALL NEXTLN ( COMSTR , MDSI2 , MDSE2 )
                    READ (MDSI2,*,END=2001,ERR=2002) XX, YY, PN
!
                    IF ( ILOOP.EQ.1 .AND. IMPROC.EQ.1 ) THEN
                        BACKSPACE (MDSI)
                        READ (MDSI,'(A)') LINE
                        WRITE (SCRATCH,'(A)') LINE
                      END IF
!
                    IF ( PN .EQ. 'STOPSTRING' ) EXIT
!
                    OT2(I)%NPTS = OT2(I)%NPTS + 1
                    IF ( ILOOP .EQ. 1 ) CYCLE
!
                    OT2(I)%X(OT2(I)%NPTS)      = XX
                    OT2(I)%Y(OT2(I)%NPTS)      = YY
                    OT2(I)%PNAMES(OT2(I)%NPTS) = PN
                    IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                        IF ( OT2(I)%NPTS .EQ. 1 ) THEN
                            WRITE (MDSS,957)              XX, YY, PN
                          ELSE
                            WRITE (MDSS,958) OT2(I)%NPTS, XX, YY, PN
                          END IF
                      END IF
!
                    END DO
!
                  IF ( IMPROC.EQ.1 .AND. ILOOP.EQ.1 ) CLOSE (SCRATCH)
                  END DO
!
                IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC .AND.        &
                     OT2(I)%NPTS.EQ.0 ) WRITE (MDSS,959)
                IF ( IMPROC .EQ. 1 ) THEN
#ifdef W3_MPI
                    CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
#endif
                    CLOSE (SCRATCH,STATUS='DELETE')
                  ELSE
                    CLOSE (SCRATCH)
#ifdef W3_MPI
                    CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
#endif
                  END IF
!
              ELSE IF ( J .EQ. 3 ) THEN
!
! 5.n Type 3: track output
!
                CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
                READ (MDSI,*,END=2001,ERR=2002) TFLAGI
                IF ( TFLAGI ) THEN
                    MDS(11,I) =  ABS(MDS(11,I))
                  ELSE
                    MDS(11,I) = -ABS(MDS(11,I))
                  END IF
                IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                    IF ( .NOT. TFLAGI ) THEN
                        WRITE (MDSS,960) 'input', 'UNFORMATTED'
                      ELSE
                        WRITE (MDSS,960) 'input', 'FORMATTED'
                      END IF
                  END IF
!
              ELSE IF ( J .EQ. 6 ) THEN
!
! 5.o Type 6: partitioned wave field data
!
                CALL NEXTLN ( COMSTR , MDSI , MDSE2 )
                READ (MDSI,*,END=2001,ERR=2002) IPRT(:,I), LPRT(I)
                IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                    WRITE (MDSS,961) IPRT(:,I)
                    IF ( .NOT. LPRT(I) ) THEN
                        WRITE (MDSS,960) 'output', 'UNFORMATTED'
                      ELSE
                        WRITE (MDSS,960) 'output', 'FORMATTED'
                      END IF
                  END IF
!
              END IF
            ELSE IF ( J .EQ. 7 ) THEN
!
! 5.p Type 7: coupling fields
!
                CALL W3READFLGRD ( MDSI, MDSS, MDSO, MDSE2, COMSTR,    &
                                 FLG1D, FLG2D, IMPROC, NMPSCR, IERR )
                FLG2(:,I)    = FLG1D
                FLGR2(:,:,I) = FLG2D
!
          ELSE
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,963)
          END IF
!
! ... End of loop in 5.j
!
        END DO
!
#ifdef W3_T
      DO I=1, NRGRD
          WRITE (MDST,9050) I
          WRITE (MDST,9051) ODAT(:,I)
          WRITE (MDST,9051) OUTFF(:,I)
          WRITE (MDST,9052) FLGRD(:,:,I)
        END DO
#endif
!
! 6.  Read moving grid data ------------------------------------------ /
!
!     Only a single set of data are provided to be applied to all
!     the grids, because this is only intended for test cases. 
!     For true implementations, the jumping grid will be used.
!
      IF ( INFLAGS1(10) ) THEN
!
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
              WRITE (MDSS,965)
              WRITE (MDSS,966) 'Continuous grid movement data'
            END IF
!
#ifdef W3_MPI
          CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
#endif
          DO ILOOP=1, 2
            IF ( ILOOP .EQ. 1 ) THEN
                MDSI2  = MDSI
                IF ( IMPROC .EQ. 1 )                                  &
                    OPEN (SCRATCH,FILE=TRIM(FNMPRE)//'ww3_shel.scratch')
              ELSE
                MDSI2  = SCRATCH
#ifdef W3_MPI
                CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
#endif
                OPEN (SCRATCH,FILE=TRIM(FNMPRE)//'ww3_shel.scratch')
                REWIND (SCRATCH)
                ALLOCATE ( TMOVE(2,NMOVE), AMOVE(NMOVE), DMOVE(NMOVE) )
                IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )            &
                    WRITE (MDSS,967) NMOVE, 'MOV'
              END IF
!
            NMOVE  = 0
            DO
              CALL NEXTLN ( COMSTR , MDSI2 , MDSE2 )
              READ (MDSI2,*,END=2001,ERR=2002) IDTST
!
              IF ( ILOOP.EQ.1 .AND. IMPROC.EQ.1 ) THEN
                  BACKSPACE (MDSI)
                  READ (MDSI,'(A)') LINE
                  WRITE (SCRATCH,'(A)') LINE
                END IF
!
              IF ( IDTST .EQ. 'STP' ) EXIT
              IF ( IDTST .NE. 'MOV' ) CYCLE
!
              NMOVE  = NMOVE + 1
              IF ( ILOOP .EQ. 1 ) CYCLE
!
              BACKSPACE (MDSI2)
              READ (MDSI2,*,END=2001,ERR=2002) IDTST, TTIME, XX, YY
              TMOVE(:,NMOVE) = TTIME
              AMOVE(NMOVE)   = XX
              DMOVE(NMOVE)   = YY
              IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )              &
                  WRITE (MDSS,968) NMOVE, TMOVE(:,NMOVE),             &
                                   AMOVE(NMOVE), DMOVE(NMOVE)
!
              END DO
!
            IF ( IMPROC.EQ.1 .AND. ILOOP.EQ.1 ) CLOSE (SCRATCH)
            END DO
!
          IF ( IMPROC .EQ. 1 ) THEN
#ifdef W3_MPI
              CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
#endif
              CLOSE (SCRATCH,STATUS='DELETE')
            ELSE
              CLOSE (SCRATCH)
#ifdef W3_MPI
              CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
#endif
            END IF
!
#ifdef W3_T
          WRITE (MDST,9060)
          DO I=1, NMOVE
            WRITE (MDST,9061) I, TMOVE(:,I), AMOVE(I), DMOVE(I)
            END DO
#endif
!
          IF ( NMOVE .EQ. 0 ) GOTO 2060
!
          NMVMAX = NMOVE
          DO I=1, NRGRD
            CALL W3SETG ( I, MDSE, MDST )
            CALL WMSETM ( I, MDSE, MDST )
            NMV    = NMOVE
            CALL WMDIMD ( I, MDSE, MDST, 0 )
            DO II=1, NMV
              TMV(:,4,II) = TMOVE(:,II)
              AMV(II,4)   = AMOVE(II)
              DMV(II,4)   = DMOVE(II)
              END DO
            END DO
!
        END IF
!
! 7.  Work load distribution ----------------------------------------- /
! 7.a Initialize arrays
!
!     *******************************************************
!     *** NOTE : OUTPUT PROCESSOR ASSIGNMENT NEEDS TO BE  ***
!     ***        CONSISTENT WITH ASSIGNMENT IN W3INIT.    ***
!     *******************************************************
!
      ALLOCATE ( ALLPRC(NMPROC,NRGRD) , MODMAP(NMPROC,NRGRP) ,        &
                 LOADMP(NMPROC,NRGRP) )
!
      ALLPRC = 0
      MODMAP = 0
      LOADMP = 0
!
! 7.b Determine number of output processors
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,970)
!
      NCPROC = NMPROC
      UPPROC = UPPROC .AND. UNIPTS .AND. IOSTYP.GT.1
!
! 7.b.1 Unified point output
!
      IF ( UNIPTS ) THEN
          IF ( NMPROC.GE.10 .AND. UPPROC ) THEN
              NCPROC = NMPROC - 1
            ELSE
              IF ( UPPROC .AND. MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) &
                  WRITE (MDSS,971) 'Separate process for point' //    &
                                   ' output disabled.'
              UPPROC = .FALSE.
            END IF
          IF ( NMPUPT .EQ. IMPROC ) THEN
              II     = LEN_TRIM(MNAMES(0))
              CALL WMUGET ( MDSS, MDST, MDSUP, 'OUT' )
              CALL WMUSET ( MDSS, MDST, MDSUP, .TRUE., 'OUT',         &
                           TRIM(FNMPRE)//'out_pnt.'//MNAMES(0)(1:II),  &
                           'Unified point output')
            END IF
        END IF
!
      IF ( UPPROC .AND. MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )         &
           WRITE (MDSS,972) NMPUPT
!
! 7.b.2 Other output
!
      ALLOCATE ( NDPOUT(NRGRD) )
      NDPOUT = 0
!
      IF ( IOSTYP .GT. 1 ) THEN
          DO I=1, NRGRD
            IF ( ODAT( 3,I) .GT. 0 ) NDPOUT(I) = NDPOUT(I) + 1
            IF ( ODAT(13,I) .GT. 0 ) NDPOUT(I) = NDPOUT(I) + 1
            IF ( ODAT(28,I) .GT. 0 ) NDPOUT(I) = NDPOUT(I) + 1
            IF ( ODAT( 8,I) .GT. 0 .OR.  ODAT(18,I) .GT. 0 .OR.       &
                 ODAT(23,I) .GT. 0 ) NDPOUT(I) = NDPOUT(I) + 1
            IF ( IOSTYP .EQ. 2 ) NDPOUT(I) = MIN ( 1 , NDPOUT(I) )
            END DO
        END IF
!
! ..... Reduce IOSTYP if not enough resources to run IOSTYP = 3
!
      IF ( IOSTYP.EQ.3 .AND.                                          &
             ( ( .NOT.PSHARE .AND. 4*SUM(NDPOUT).GT.NCPROC )          &
           .OR.( PSHARE .AND. 4*MAXVAL(NDPOUT).GT.NCPROC ) ) ) THEN
          DO I=1, NRGRD
            NDPOUT(I) = MIN ( 1 , NDPOUT(I) )
            END DO
          IOSTYP = 2
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,971) 'Separate processes for output' //     &
                                   ' types disabled.'
        END IF
!
! ..... Force sharing of output processes if not enough resources
!
      IF ( IOSTYP.GT.1 .AND. .NOT.PSHARE .AND.                        &
           4*SUM(NDPOUT).GT.NCPROC ) THEN
          PSHARE = .TRUE.
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,971) 'Grids sharing output processes.'
        END IF
!
! ..... Disable output processes if not enough resources
!
      IF ( IOSTYP.GT.1 .AND. 4*MAXVAL(NDPOUT).GT.NCPROC ) THEN
          NDPOUT = 0
          IOSTYP = 1
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,971) 'Separate processes for output' //     &
                               ' disabled.'
        END IF
!
! ..... Number of output processes (except for unified point output)
!
      NPOUTT = 0
      IF ( IOSTYP .GT. 1 ) THEN
          IF ( PSHARE ) THEN
              NPOUTT = MAXVAL(NDPOUT)
            ELSE
              NPOUTT = SUM(NDPOUT)
            END IF
        END IF
      NCPROC = NCPROC - NPOUTT
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
          IF ( NPOUTT .EQ. 0 ) THEN
              WRITE (MDSS,971) 'No (other) dedicated output processes.'
            ELSE
              WRITE (MDSS,973) NCPROC+1, NCPROC+NPOUTT, NPOUTT
            END IF
        END IF
!
! 7.c Set communicators and ALLPRC array
!
#ifdef W3_T
      WRITE (MDST,9070)
#endif
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,974)
      IF ( NMPLOG.EQ.IMPROC ) WRITE (MDSO,1974)
!
#ifdef W3_MPI
      CALL MPI_COMM_GROUP ( MPI_COMM_MWAVE, BGROUP, IERR_MPI )
#endif
      ALLOCATE ( TMPRNK(NMPROC) )
      NAPRES = NCPROC
!
      DO I=1, NRGRD
!
        IP1    = MAX( 1 , MIN ( NCPROC , 1+NINT(REAL(NCPROC)*RP1(I)) ) )
        IPN    = MAX( IP1 , MIN ( NCPROC , NINT(REAL(NCPROC)*RPN(I)) ) )
        OUTSTR = '-----'
!
        CALL WMSETM ( I, MDSE, MDST )
        NAPLOC = 1 + IPN - IP1
        NAPADD = NAPLOC
#ifdef W3_MPI
        CROOT  = IP1
        FBCAST = NAPLOC .NE. NCPROC
        FBCAST = NAPLOC .NE. NCPROC .OR.                         &
                     ( IOSTYP.GT.1 .AND. .NOT.PSHARE )
#endif
        DO J=IP1, IPN
          TMPRNK(1+J-IP1) = J - 1
          END DO
!
        IF ( IOSTYP .GT. 1 ) THEN
            IF ( PSHARE ) NAPRES = NCPROC
            DO J=1, NDPOUT(I)
              NAPADD = NAPADD + 1
              TMPRNK(NAPADD) = NAPRES
              NAPRES = NAPRES + 1
              END DO
          END IF
!
        IF ( UPPROC ) THEN
            NAPADD = NAPADD + 1
            TMPRNK(NAPADD) = NMPROC - 1
          END IF
!
#ifdef W3_MPI
        CALL MPI_GROUP_INCL ( BGROUP, NAPADD, TMPRNK, LGROUP,    &
                              IERR_MPI )
        CALL MPI_COMM_CREATE ( MPI_COMM_MWAVE, LGROUP,           &
                               MPI_COMM_GRD, IERR_MPI )
        CALL MPI_GROUP_FREE ( LGROUP, IERR_MPI )
#endif
!
        DO II=IP1, IPN
          ALLPRC(II,I) = 1 + II - IP1
          END DO
        II     = II - IP1
!
        IF ( PSHARE .OR. I.EQ.1 ) THEN
            NAPADD = NCPROC
          ELSE
            NAPADD = NCPROC + SUM(NDPOUT(1:I-1))
          END IF
        IF ( IOSTYP .GT. 1 ) THEN
            DO J=1, NDPOUT(I)
              NAPADD = NAPADD + 1
              II     = II + 1
              ALLPRC(NAPADD,I) = II
              END DO
          END IF
!
        IF ( UPPROC ) THEN
            II     = II + 1
            ALLPRC(NMPROC,I) = II
          END IF
!
#ifdef W3_T
        WRITE (MDST,9071) I, ALLPRC(:,I)
#endif
!
! ... output
!
!
        IF ( IOSTYP .LE. 1 ) THEN
!
            IF ( ODAT( 3,I) .GT. 0 ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(MAX(1,NAPLOC-1))+1
                OUTSTR(1) = STOUT
              END IF
            IF ( ODAT( 8,I) .GT. 0 .OR. UNIPTS ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(MAX(1,NAPLOC-2))+1
                OUTSTR(2) = STOUT
              END IF
            IF ( ODAT(13,I) .GT. 0 ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(MAX(1,NAPLOC-5))+1
                OUTSTR(3) = STOUT
              END IF
            IF ( ODAT(18,I) .GT. 0 ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(NAPLOC)+1
                OUTSTR(4) = STOUT
              END IF
            IF ( ODAT(23,I) .GT. 0 ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(MAX(1,NAPLOC-3))+1
                OUTSTR(5) = STOUT
              END IF
            IF ( ODAT(28,I) .GT. 0 ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(MAX(1,NAPLOC-4))+1
                OUTSTR(6) = STOUT
              END IF
!
          ELSE
!
            IF ( UNIPTS ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(II) + 1
                OUTSTR(2) = STOUT
                IF ( UPPROC ) II = II - 1
              END IF
!
            IF ( IOSTYP .EQ. 2 ) THEN
!
                WRITE (STOUT,'(I5.5)') TMPRNK(II) + 1
                IF ( ODAT( 3,I) .GT. 0 ) OUTSTR(1) = STOUT
                IF ( ODAT( 8,I) .GT. 0 .OR.                           &
                                    ( UNIPTS .AND. .NOT.UPPROC ) )    &
                                         OUTSTR(2) = STOUT
                IF ( ODAT(13,I) .GT. 0 ) OUTSTR(3) = STOUT
                IF ( ODAT(18,I) .GT. 0 ) OUTSTR(4) = STOUT
                IF ( ODAT(23,I) .GT. 0 ) OUTSTR(5) = STOUT
                IF ( ODAT(28,I) .GT. 0 ) OUTSTR(6) = STOUT
!
              ELSE IF ( IOSTYP .EQ. 3 ) THEN
!
                IF ( ODAT( 3,I).GT.0 ) THEN
                    WRITE (STOUT,'(I5.5)') TMPRNK(II) + 1
                    OUTSTR(1) = STOUT
                    II        = II - 1
                  END IF
                IF ( ODAT(13,I).GT.0 ) THEN
                    WRITE (STOUT,'(I5.5)') TMPRNK(II) + 1
                    OUTSTR(3) = STOUT
                    II        = II - 1
                  END IF
                IF ( ODAT(28,I).GT.0 ) THEN
                    WRITE (STOUT,'(I5.5)') TMPRNK(II) + 1
                    OUTSTR(6) = STOUT
                    II        = II - 1
                  END IF
                WRITE (STOUT,'(I5.5)') TMPRNK(II) + 1
                IF ( ODAT( 8,I) .GT. 0 ) OUTSTR(2) = STOUT
                IF ( ODAT(18,I) .GT. 0 ) OUTSTR(4) = STOUT
                IF ( ODAT(23,I) .GT. 0 ) OUTSTR(5) = STOUT
!
              END IF
!
          END IF
!
        IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                    &
            WRITE (MDSS,975) MNAMES(I), IP1, IPN, OUTSTR
        IF ( NMPLOG .EQ. IMPROC )                                     &
            WRITE (MDSO,1975)MNAMES(I), IP1, IPN, OUTSTR
!
#ifdef W3_MPI
        IF ( FBCAST ) THEN
            TMPRNK(1) = IP1 - 1
            NAPBCT    = 1
            DO J=1, NMPROC
              IF ( ALLPRC(J,I) .EQ. 0 ) THEN
                  NAPBCT = NAPBCT + 1
                  TMPRNK(NAPBCT) = J - 1
                END IF
              END DO
            CALL MPI_GROUP_INCL ( BGROUP, NAPBCT, TMPRNK,       &
                                  LGROUP, IERR_MPI )
            CALL MPI_COMM_CREATE ( MPI_COMM_MWAVE, LGROUP,      &
                                   MPI_COMM_BCT, IERR_MPI )
            CALL MPI_GROUP_FREE ( LGROUP, IERR_MPI )
         END IF
#endif
!
        END DO
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
          WRITE (MDSS,976)
          IF ( UNIPTS ) WRITE (MDSS,977) NMPUPT
          WRITE (MDSS,*) 
        END IF
!
      IF ( NMPLOG .EQ. IMPROC ) THEN
          WRITE (MDSO,1976)
          IF ( UNIPTS ) WRITE (MDSO,1977) NMPUPT
          WRITE (MDSO,*) 
        END IF
!
      DEALLOCATE ( TMPRNK, NDPOUT )
!
! 7.d Set MODMAP and LOADMP arrays
!
      DO JJ=1, NRGRP
        DO II=1, INGRP(JJ,0)
          I      = INGRP(JJ,II)
          DO J=1, NMPROC
            IF ( ALLPRC(J,I) .NE. 0 ) THEN
                LOADMP(J,JJ) = LOADMP(J,JJ) + 1
                IF ( LOADMP(J,JJ) .EQ. 1 ) THEN
                    MODMAP(J,JJ) = I
                  ELSE
                    MODMAP(J,JJ) = -1
                  END IF
              END IF
            END DO
          END DO
        END DO
!
#ifdef W3_T
      WRITE (MDST,8042)
      DO J=1, NRGRP
        WRITE (MDST,8044) J, MODMAP(:,J)
        END DO
      WRITE (MDST,8043)
      DO J=1, NRGRP
        WRITE (MDST,8044) J, LOADMP(:,J)
        END DO
#endif
!
! 7.e Warnings
!
      IF ( NMPROC .GT. 1 ) THEN
          DO I=1, NRGRP
            IP1    = MINVAL ( LOADMP(:NCPROC,I) )
            IPN    = MAXVAL ( LOADMP(:NCPROC,I) )
            IF ( IP1.NE.IPN .AND. IMPROC.EQ.NMPERR )                  &
                WRITE (MDSE,1040) I, IP1, IPN
            END DO
        END IF
!
      DEALLOCATE ( RP1, RPN, LOADMP )
!
! 7.f Reset NMPSCR to first processor of first rank 1 grid
!
#ifdef W3_MPI
      CALL WMSETM ( INGRP(1,1), MDSE, MDST )
      NMPSCR = CROOT
#endif
!
#ifdef W3_MPI
      CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
#endif
!
! 8.  Actual initializations ----------------------------------------- /
!
#ifdef W3_MPRF
      CALL PRTIME ( PRFTN )
      WRITE (MDSP,990) PRFT0, PRFTN, get_memory(), 'START Sec. 8'
      PRFT0  = PRFTN
#endif
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,980)
      ALLOCATE ( TSYNC(2,0:NRGRD), TMAX(2,NRGRD), TOUTP(2,0:NRGRD),   &
                 TDATA(2,NRGRD), GRSTAT(NRGRD), DTRES(NRGRD) )
!
      TSYNC(1,:) = -1
      TSYNC(2,:) =  0
      TMAX (1,:) = -1
      TMAX (2,:) =  0
      TOUTP(1,:) = -1
      TOUTP(2,:) =  0
      TDATA(1,:) = -1
      TDATA(2,:) =  0
      GRSTAT     = 99
!
! 8.a Loop over models for per-model initialization
!
#ifdef W3_T
      WRITE (MDST,9080)
#endif
#ifdef W3_MPRF
      CALL PRTIME ( PRFTN )
      WRITE (MDSP,990) PRFT0, PRFTN, get_memory(), 'START Sec. 8.a'
      PRFT0  = PRFTN
#endif
!
      DO I=1, NRGRD
        J      = LEN_TRIM(MNAMES(I))
        DO NMPSC2=1, NMPROC
          IF ( ALLPRC(NMPSC2,I) .EQ. 1 ) EXIT
          END DO
        IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC )                    &
             WRITE (MDSS,981) I, MNAMES(I)(1:J)
!
#ifdef W3_MPI
        CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
#endif
!
! 8.a.1 Wave model initialization (NOTE: sets all grid pointers)
! ..... Initial output file hook up
!
        CALL WMSETM ( I, MDSE, MDST )
#ifdef W3_MPI
        MPI_COMM_LOC = MPI_COMM_GRD
        IF ( MPI_COMM_LOC .EQ. MPI_COMM_NULL ) CYCLE
#endif
!
        CALL WMUGET ( MDSE, MDST, NDSFND, 'OUT' )
        CALL WMUSET ( MDSE, MDST, NDSFND, .TRUE., DESC='Log file' )
        MDS( 1,I) = NDSFND
!
! ... this one overwrites the combined setting MDS( 3,I) = MDST above
!
!       CALL WMUGET ( MDSE, MDST, NDSFND, 'OUT' )
!       CALL WMUSET ( MDSE, MDST, NDSFND, .TRUE., DESC='Test output' )
!       MDS( 3,I) = NDSFND
!
        DO J=1, 6
          IF ( J.EQ.4 .OR. J.EQ.5 ) CYCLE
          IF ( ODAT(5*(J-1)+3,I) .GT. 0 ) THEN
              CALL WMUGET ( MDSE, MDST, NDSFND, 'OUT' )
              CALL WMUSET ( MDSE, MDST, NDSFND, .TRUE.,              &
                            DESC='Raw output file' )
              SELECT CASE (J)
                CASE (1)
                  MDS(7,I) = NDSFND
                CASE (2)
                  MDS(8,I) = NDSFND
                CASE (3)
                  MDS(12,I) = NDSFND
                  CALL WMUGET ( MDSE, MDST, NDSFND, 'INP' )
                  CALL WMUSET ( MDSE, MDST, NDSFND, .TRUE.,           &
                                DESC='Input data file' )
                  MDS(11,I) = NDSFND
                CASE (6)
                  MDS(13,I) = NDSFND
              END SELECT
            END IF
          END DO
!
        CALL WMUGET ( MDSE, MDST, NDSFND, 'INP' )
        CALL WMUSET ( MDSE, MDST, NDSFND, .TRUE.,                     &
                      DESC='Input data file' )
        MDS(9,I) = NDSFND
!
        IF ( ODAT(5*(5-1)+3,I) .GT. 0 ) THEN
            CALL WMUGET ( MDSE, MDST, NDSFND, 'OUT', 9 )
            MDS(10,I) = NDSFND
            DO II=0, 8
              CALL WMUSET ( MDSE, MDST, NDSFND+II, .TRUE.,            &
                            DESC='Raw output file' )
              END DO
          END IF
!
! ..... Model initialization
!
        IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC ) WRITE (MDSS,982)

        CALL W3INIT ( I, .TRUE., MNAMES(I), MDS(:,I), NTRACE(:,I),    &
                      ODAT(:,I),                          &
                      FLGRD(:,:,I),FLGR2(:,:,I),FLGD(:,I),FLG2(:,I),  &
                      OT2(I)%NPTS, OT2(I)%X, OT2(I)%Y, OT2(I)%PNAMES, &
                      IPRT(:,I), LPRT(I), MPI_COMM_LOC)
!
! ..... Finalize I/O file hook up
!
        II     = LEN_TRIM(FILEXT)
        JJ     = LEN_TRIM(FNMPRE)
        CALL WMUINQ ( MDSE, MDST, MDS(1,I) )
        IF ( MDS(3,I) .NE. MDST ) CALL WMUINQ ( MDSE, MDST, MDS(3,I) )
!
        IF ( MDS(7,I) .NE. -1 ) THEN
            IF ( IAPROC .EQ. NAPFLD ) THEN
                TNAME  = TRIM(FNMPRE)//'out_grd.' // FILEXT(:II)
                CALL WMUSET ( MDSE,MDST, MDS(7,I), .TRUE., NAME=TNAME )
              ELSE
                CALL WMUSET ( MDSE,MDST, MDS(7,I), .FALSE. )
                MDS(7,I) = -1
              END IF
          END IF
!
        IF ( MDS(8,I) .NE. -1 ) THEN
            IF ( IAPROC .EQ. NAPPNT ) THEN
                TNAME  = TRIM(FNMPRE)//'out_pnt.' // FILEXT(:II)
                CALL WMUSET ( MDSE,MDST, MDS(8,I), .TRUE., NAME=TNAME )
              ELSE
                CALL WMUSET ( MDSE,MDST, MDS(8,I), .FALSE. )
                MDS(8,I) = -1
              END IF
          END IF
!
        IF ( MDS(9,I) .NE. -1 ) THEN
            IF ( FLBPI ) THEN
                TNAME  = TRIM(FNMPRE)//'nest.' // FILEXT(:II)
                CALL WMUSET ( MDSE, MDST, MDS(9,I), .TRUE., NAME=TNAME )
              ELSE
                CALL WMUSET ( MDSE, MDST, MDS(9,I), .FALSE. )
                MDS(9,I) = -1
              END IF
          END IF
!
        IF ( MDS(10,I) .NE. -1 ) THEN
            IF ( FLBPO .AND. IAPROC.EQ.NAPBPT ) THEN
                TNAME  = TRIM(FNMPRE)//'nestN.' // FILEXT(:II)
                DO J=0, NFBPO-1
                  WRITE (TNAME(JJ+5:JJ+5),'(I1)') J + 1
                  CALL WMUSET ( MDSE, MDST, MDS(10,I)+J, .TRUE.,      &
                                NAME=TNAME )
                  END DO
                DO J=NFBPO, 8
                  CALL WMUSET ( MDSE,MDST, MDS(10,I)+J, .FALSE. )
                  END DO
              ELSE
                DO J=0, 8
                  CALL WMUSET ( MDSE,MDST, MDS(10,I)+J, .FALSE. )
                  END DO
                MDS(10,I) = -1
              END IF
          END IF
!
        IF ( MDS(11,I) .NE. -1 ) THEN
            TNAME  = TRIM(FNMPRE)//'track_i.' // FILEXT(:II)
            CALL WMUSET ( MDSE,MDST, MDS(11,I), .TRUE., NAME=TNAME )
          END IF
!
        IF ( MDS(12,I) .NE. -1 ) THEN
            IF ( IAPROC .EQ. NAPTRK ) THEN
                TNAME  = TRIM(FNMPRE)//'track_o.' // FILEXT(:II)
                CALL WMUSET ( MDSE,MDST, MDS(12,I), .TRUE., NAME=TNAME )
              ELSE
                CALL WMUSET ( MDSE,MDST, MDS(12,I), .FALSE. )
                MDS(12,I) = -1
              END IF
          END IF
!
        IF ( MDS(13,I) .NE. -1 ) THEN
            IF ( IAPROC .EQ. NAPPRT ) THEN
                TNAME  = TRIM(FNMPRE)//'partition.' // FILEXT(:II)
                CALL WMUSET ( MDSE,MDST, MDS(13,I), .TRUE., NAME=TNAME )
              ELSE
                CALL WMUSET ( MDSE,MDST, MDS(13,I), .FALSE. )
                MDS(13,I) = -1
              END IF
          END IF
!
#ifdef W3_T
        WRITE (MDST,9081) I, TIME
#endif
!
! 8.a.2 Data file initialization (forcing)
!
        IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC ) WRITE (MDSS,983)
        CALL W3SETI ( I, MDSE, MDST )
!
!!Li  Stop modifying GTYPE from input forcing file.  JGLi08Apr2021.
        JJJ = GTYPE
!
! ..... regular input files
!
        DO J=JFIRST, 6
          IF ( INFLAGS1(J) ) THEN
              IDINP(I,J) = IDSTR(J)
              IF ( INPMAP(I,J) .LT. 0 ) CYCLE
              CALL W3FLDO ('READ', IDINP(I,J), MDSF(I,J), MDST, MDSE2,&
!!Li                        NX, NY, GTYPE, IERR, MNAMES(I),           &
                            NX, NY,   JJJ, IERR, MNAMES(I),           &
                            TRIM(FNMPRE) )
              IF ( IERR .NE. 0 ) GOTO 2080
!
!!Li   Print a warning message when GTYPE not matching forcing field one. 
              IF ( (JJJ .NE. GTYPE) .AND. (IMPROC .EQ. NMPSC2) )       &
                 WRITE (MDSE, *) ' *** Warning: grid', I, ' GTYPE=',  & 
                 GTYPE, ' not matching field', J, ' grid type', JJJ
!
              IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC )              &
                   WRITE (MDSS,985) IDFLDS(J)
            ELSE
              IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC )              &
                   WRITE (MDSS,984) IDFLDS(J)
            END IF
          END DO
!
! ..... assimilation data files
!
!       version 3.07: Data assimilation part ignored for now ....
!
! ..... finalize file info data base
!
        DO J=JFIRST, 9
          IF ( MDSF(I,J) .NE. -1 ) CALL WMUINQ ( MDSE, MDST, MDSF(I,J) )
          END DO
!
! ..... Adjust input flags for other than native or CPL input,
!       and initialize input arrays one set at a time as needed.
!
        IF ( SIZE(INFLAGS1) .NE. SIZE(TFLAGS) ) THEN
          WRITE (MDSE,'(/2A)') ' *** ERROR WMINIT: ', &
                 'SIZE(INFLAGS1).NE.SIZE(TFLAGS) ***'
          CALL EXTCDE ( 999 )
        END IF
        IF ( SIZE(INFLAGS2) .NE. SIZE(TFLAGS) ) THEN
          WRITE (MDSE,'(/2A)') ' *** ERROR WMINIT: ', &
                 'SIZE(INFLAGS2).NE.SIZE(TFLAGS) ***'
          CALL EXTCDE ( 999 )
        END IF

        TFLAGS = INFLAGS1
!
        DO J=JFIRST, 9
          IF ( INPMAP(I,J) .NE. 0 ) THEN
!
              TFLAGS(J) = .TRUE.
              INFLAGS1     = .FALSE.
              INFLAGS1(J)  = .TRUE.
              IINIT     = .FALSE.
              CALL W3DIMI ( I, MDSE, MDST )
!
              IF ( J.EQ.2 ) ALLOCATE ( WADATS(I)%CA0(NSEA) ,          &
                                       WADATS(I)%CAI(NSEA) ,          &
                                       WADATS(I)%CD0(NSEA) ,          &
                                       WADATS(I)%CDI(NSEA) )
!
              IF ( J.EQ.3 ) ALLOCATE ( WADATS(I)%UA0(NSEA) ,          &
                                       WADATS(I)%UAI(NSEA) ,          &
                                       WADATS(I)%UD0(NSEA) ,          &
                                       WADATS(I)%UDI(NSEA) ,          &
                                       WADATS(I)%AS0(NSEA) ,          &
                                       WADATS(I)%ASI(NSEA) )
!
              IF ( J.EQ.5 ) ALLOCATE ( WADATS(I)%MA0(NSEA) ,          &
                                       WADATS(I)%MAI(NSEA) ,          &
                                       WADATS(I)%MD0(NSEA) ,          &
                                       WADATS(I)%MDI(NSEA) )
!
              IF ( J.EQ.6 ) ALLOCATE ( WADATS(I)%RA0(NSEA) ,          &
                                       WADATS(I)%RAI(NSEA) )
!
          END IF !  IF ( INPMAP(I,J) .NE. 0 ) THEN
        END DO !  DO J=JFIRST, 9
!
        INFLAGS1  = TFLAGS
        CALL W3SETI ( I, MDSE, MDST )
        CALL W3SETA ( I, MDSE, MDST )
!
! 8.a.3 Status indicator and model times
!
        DO J=1, NOTYPE
          IF ( FLOUT(J) ) THEN
              IF ( TOUTP(1,I) .EQ. -1 ) THEN
                  TOUTP(:,I) = TONEXT(:,J)
                ELSE
                  DTTST  = DSEC21 ( TOUTP(:,I), TONEXT(:,J) )
                  IF ( DTTST .LT. 0. ) TOUTP(:,I) = TONEXT(:,J)
                ENDIF
            END IF
          END DO
!
! CHECKPOINT
        J=8
        IF ( FLOUT(J) ) THEN
           IF ( TOUTP(1,I) .EQ. -1 ) THEN
              TOUTP(:,I) = TONEXT(:,J)
           ELSE
              DTTST  = DSEC21 ( TOUTP(:,I), TONEXT(:,J) )
              IF ( DTTST .LT. 0. ) TOUTP(:,I) = TONEXT(:,J)
           ENDIF
        END IF
!
!
        GRSTAT(I) =  0
        TSYNC(:,I) = TIME(:)
!
#ifdef W3_SMC
 !!Li   Check GTYPE values after initialization.  JGLi08Apr2021
        IF( IMPROC .EQ. CROOT ) WRITE(MDSE,*) " GRID CROOT GTYPE", &
            I, CROOT, GRIDS(I)%GTYPE
#endif
!
#ifdef W3_T
        WRITE (MDST,9082) GRSTAT(I), TOUTP(:,I), TSYNC(:,I)
#endif
!
        END DO   !! 8.a I-NRGRD loop
!
#ifdef W3_MPI
      CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
      DO I=1, NRGRD
        CALL WMSETM ( I, MDSE, MDST )
        CALL W3SETG ( I, MDSE, MDST )
        CALL W3SETO ( I, MDSE, MDST )
        IF ( FBCAST .AND. MPI_COMM_BCT.NE.MPI_COMM_NULL ) THEN
            CALL MPI_BCAST ( TOUTP(1,I), 2, MPI_INTEGER, 0,      &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( TSYNC(1,I), 2, MPI_INTEGER, 0,      &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( GRSTAT(I), 1, MPI_INTEGER, 0,       &
                             MPI_COMM_BCT, IERR_MPI )
#endif
!
! 8.a.4 Grid sizes etc. for processors that are not used.
!
#ifdef W3_MPI
            CALL MPI_BCAST ( FLAGLL,1, MPI_LOGICAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( GTYPE, 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( ICLOSE,1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NX   , 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NY   , 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( X0   , 1, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( SX   , 1, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( Y0   , 1, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( SY   , 1, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NSEA , 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NSEAL, 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( DTMAX, 1, MPI_REAL, 0,              &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( DTCFL, 1, MPI_REAL, 0,              &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( FILEXT, 10, MPI_CHARACTER, 0,       &
                             MPI_COMM_BCT, IERR_MPI )
            IF ( MPI_COMM_GRD .EQ. MPI_COMM_NULL )               &
                 CALL W3DIMX  ( I, NX, NY, NSEA, MDSE, MDST      &
#endif
#ifdef W3_SMC
 !!  SMC grid related variables are not needed beyond MPI_COMM_GRD
 !!  so all dimensions are minimised to 1.  JGLi29Mar2021
#endif
#ifdef W3_MPI
#ifdef W3_SMC
 !!Li        , NCel, NUFc, NVFc, NRLv, NBSMC  &
 !!Li        , NARC, NBAC, NSPEC              &
             , 1, 1, 1, 1, 1, 1, 1, 1         &
#endif
                )
            CALL MPI_BCAST ( HQFAC, NX*NY, MPI_REAL, 0,          &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( HPFAC, NX*NY, MPI_REAL, 0,          &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( XGRD, NX*NY, MPI_REAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( YGRD, NX*NY, MPI_REAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            IF ( MPI_COMM_GRD .EQ. MPI_COMM_NULL )               &
                 GSU = W3GSUC( .FALSE., FLAGLL, ICLOSE,          &
                               XGRD, YGRD )
            CALL MPI_BCAST ( DXDP, NX*NY, MPI_REAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( DXDQ, NX*NY, MPI_REAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( DYDP, NX*NY, MPI_REAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( DYDQ, NX*NY, MPI_REAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( MAPSTA, NX*NY, MPI_INTEGER, 0,      &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( MAPST2, NX*NY, MPI_INTEGER, 0,      &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( GRIDSHIFT, 1, MPI_DOUBLE_PRECISION, 0, &
                             MPI_COMM_BCT, IERR_MPI )
#endif
!
#ifdef W3_MPI
            CALL MPI_BCAST ( NK   , 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NTH  , 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( XFR  , 1, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( FR1  , 1, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            IF ( MPI_COMM_GRD .EQ. MPI_COMM_NULL )               &
                 CALL W3DIMS ( I, NK, NTH, MDSE, MDST )
            CALL MPI_BCAST ( TH , NTH, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
#endif
!
#ifdef W3_MPI
            CALL MPI_BCAST ( NAPROC,1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NAPPNT,1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NBI  , 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
#endif
!
#ifdef W3_MPI
            CALL MPI_BCAST ( FLOUT,  8, MPI_LOGICAL, 0,          &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( DTOUT , 8, MPI_REAL, 0,             &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( TONEXT,16, MPI_INTEGER, 0,          &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( TOLAST,16, MPI_INTEGER, 0,          &
                             MPI_COMM_BCT, IERR_MPI )
#endif
!
#ifdef W3_MPI
          END IF
        END DO
      CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
#endif
!
      DO I=1, NRGRD
        IF ( ALLPRC(IMPROC,I) .EQ. 0 ) THEN
            CALL W3SETO ( I, MDSE, MDST )
            IAPROC = -1
          END IF
        END DO
!
! 8.a.5 Test output
!
#ifdef W3_T
      WRITE (MDST,9020) 'AFTER SETUP'
      DO I=1, NRGRD
        WRITE (MDST,9021) I, MDS(:,I), NTRACE(:,I)
        END DO
#endif
!
! 8.a.6 Check for coordinate system
!
      DO I=1, NRGRD-1
        IF ( GRIDS(I)%FLAGLL .NEQV. GRIDS(I+1)%FLAGLL ) GOTO 2070
        END DO
!
! 8.b Input files
!
#ifdef W3_MPRF
      CALL PRTIME ( PRFTN )
      WRITE (MDSP,990) PRFT0, PRFTN, get_memory(), 'START Sec. 8.c'
      PRFT0  = PRFTN
#endif
!
      DO I=1, NRINP
!
        IF ( .NOT. USEINP(I) ) CYCLE
!
        J      = LEN_TRIM(MNAMES(-I))
        IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC ) THEN
            WRITE (MDSS,988) I, MNAMES(-I)(1:J)
            WRITE (MDSS,987)
          END IF
!
        CALL W3IOGR ( 'GRID', NDSREC, -I, MNAMES(-I)(1:J) )
        CALL W3DIMI ( -I, MDSE, MDST )
!
        IF ( CPLINP(I) ) CYCLE
!
        DO J=JFIRST, 6
          IF ( INFLAGS1(J) ) THEN
              IDINP(-I,J) = IDSTR(J)
              CALL W3FLDO ('READ', IDINP(-I,J), MDSF(-I,J), MDST,     &
                            MDSE2, NX, NY, GTYPE, IERR,               &
                            MNAMES(-I), TRIM(FNMPRE) )
              IF ( IERR .NE. 0 ) GOTO 2080
              IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC )              &
                   WRITE (MDSS,985) IDFLDS(J)
            ELSE
              IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC )              &
                   WRITE (MDSS,984) IDFLDS(J)
            END IF
          END DO
!
! Skipping assimilation input files for now.
!
          DO J=JFIRST, 9
            IF ( MDSF(-I,J) .NE. -1 ) CALL WMUINQ                     &
                                         ( MDSE, MDST, MDSF(-I,J) )
            END DO
!
        END DO
!
      DO I=1, NRGRD
        DO J=JFIRST, 9
          IF  ( INPMAP(I,J).LT.0 .AND. INPMAP(I,J).NE.-999) IDINP(I,J) = IDINP( INPMAP(I,J),J)
          !IF ( INPMAP(I,J) .LT. 0 ) IDINP(I,J) = IDINP( INPMAP(I,J),J)
          IF ( INPMAP(I,J) .GT. 0 ) IDINP(I,J) = IDINP(-INPMAP(I,J),J)
          END DO
        END DO
!
      DEALLOCATE ( USEINP )
      DEALLOCATE ( CPLINP )
!
! 8.c Inter model initialization
!
#ifdef W3_MPRF
      CALL PRTIME ( PRFTN )
      WRITE (MDSP,990) PRFT0, PRFTN, get_memory(), 'START Sec. 8.d'
      PRFT0  = PRFTN
#endif

! 8.c.1 Spectral conversion flags and source term flags
!
      CALL WMRSPC
!
      DO I=1, NRGRD
        CALL W3SETG ( I, MDSE, MDST )
        FLAGST = .TRUE.
        END DO
!
! 8.c.2 Relation to lower ranked grids
!       Includes update of unit numbers, and bound. data initialization.
!
      ALLOCATE ( FLRBPI(NRGRD) )
      CALL WMGLOW ( FLRBPI )
!
! ..... At this point the grid-search-utility (GSU) object for grids
!       that do not belong to this processor is no longer needed.
!
#ifdef W3_MPI
      DO I=1, NRGRD
        CALL WMSETM ( I, MDSE, MDST )
        CALL W3SETG ( I, MDSE, MDST )
#endif
! the next line (with the W3GSUD call) removed Jan 8 2013. 
! ...ref: personal communication, 
! ...email from Rogers to Alves, Campbell, Tolman, Chawla Dec 13 2012.
! REMOVED  !/MPI        IF ( MPI_COMM_GRD .EQ. MPI_COMM_NULL ) CALL W3GSUD( GSU )
#ifdef W3_MPI
        END DO
#endif
!
! ..... Unit numbers
!

      DO I=1, NRGRD
!
        CALL W3SETG ( I, MDSE, MDST )
        CALL W3SETO ( I, MDSE, MDST )
!
        IF ( BCDUMP(I) .AND. FLRBPI(I) ) THEN
            IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1080) I
            IF ( IMPROC .EQ. NMPLOG ) WRITE (MDSO,1082) I
            BCDUMP(I) = .FALSE.
          END IF
!
        IF ( BCDUMP(I) .AND. NBI.EQ.0 ) THEN
            IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1081) I
            IF ( IMPROC .EQ. NMPLOG ) WRITE (MDSO,1082) I
            BCDUMP(I) = .FALSE.
          END IF
!
#ifdef W3_SHRD
        IF ( .NOT. FLRBPI(I) .AND. FLBPI ) THEN
#endif
#ifdef W3_MPI
        IF ( .NOT. FLRBPI(I) .AND. FLBPI .AND.                   &
              MPI_COMM_GRD .NE. MPI_COMM_NULL) THEN
#endif
            CALL WMUSET ( MDSE, MDST, NDS(9), .FALSE. )
            IF ( BCDUMP(I) .AND. IAPROC.EQ.NAPBPT ) THEN
                J            = LEN_TRIM(FILEXT)
                TNAME(1:5)   = 'nest.'
                TNAME(6:5+J) = FILEXT(1:J)
                J      = J + 5
                CALL WMUGET ( MDSE, MDST, NDS(9), 'OUT' )
                CALL WMUSET ( MDSE, MDST, NDS(9), .TRUE.,             &
                              NAME=TRIM(FNMPRE)//TNAME(1:J),           &
                              DESC='Output data file (nest dump)' )
                MDS(9,I) = NDSFND
              ELSE
                NDS(9) = -1
              END IF
          END IF
!
        END DO
!
! ..... Data initialization
!
      DO I=1, NRGRD
#ifdef W3_MPI
        CALL WMSETM ( I, MDSE, MDST )
        IF ( MPI_COMM_GRD .NE. MPI_COMM_NULL ) CALL WMIOBS ( I )
#endif
#ifdef W3_SHRD
        CALL WMIOBS ( I )
#endif
        END DO
!
      DO I=1, NRGRD
#ifdef W3_MPI
        CALL WMSETM ( I, MDSE, MDST )
        IF ( MPI_COMM_GRD .NE. MPI_COMM_NULL ) CALL WMIOBG ( I )
#endif
#ifdef W3_SHRD
        CALL WMIOBG ( I )
#endif
        END DO
!
#ifdef W3_MPI
      DO I=1, NRGRD
        CALL WMSETM ( I, MDSE, MDST )
        IF ( MPI_COMM_GRD .NE. MPI_COMM_NULL ) CALL WMIOBF ( I )
        END DO
#endif
!
! 8.c.3 Relation to same ranked grids
!
#ifdef W3_SMC
 !!  Check whether there is a SMC grid group.   JGLi12Apr2021
      NGRPSMC = 0 
      DO JJ=1, NRGRP
         J = 0
         DO II=1, INGRP(JJ,0)
            I = INGRP(JJ,II)
            IF( GRIDS(I)%GTYPE .EQ. SMCTYPE ) J = J + 1 
         ENDDO
         IF( J .GT. 1 )  NGRPSMC = JJ 
      ENDDO
      IF( IMPROC.EQ.NMPERR )  WRITE (MDSE,*) " NGRPSMC =", NGRPSMC
 
 !!  Equal ranked SMC grid group uses its own sub.   JGLi12Apr2021
      IF( NGRPSMC .GT. 0 ) THEN
          CALL WMSMCEQL
      ELSE
#endif
!
      CALL WMGEQL
!
#ifdef W3_SMC
      ENDIF
#endif
!
! 8.c.4 Relation to higher ranked grids
!
      IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC ) WRITE (MDSS,938) &
           'Computing relation to higher ranked grids'
      CALL WMGHGH
      IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC ) WRITE (MDSS,938) &
           'Finished computing relation to higher ranked grids'
!
! 8.c.5 Unified point output
!
      IF ( UNIPTS ) THEN
!
          OUTPTS(0)%TONEXT(1,2) =        ODAT( 6,0)
          OUTPTS(0)%TONEXT(2,2) =        ODAT( 7,0)
          OUTPTS(0)%DTOUT (  2) = REAL ( ODAT( 8,0) )
          OUTPTS(0)%TOLAST(1,2) =        ODAT( 9,0)
          OUTPTS(0)%TOLAST(2,2) =        ODAT(10,0)
          OUTPTS(0)%OFILES(1)   =        OUTFF(1,1)
          OUTPTS(0)%OFILES(2)   =        OUTFF(2,1)
!
          TOUT   = OUTPTS(0)%TONEXT(:,2)
          TLST   = OUTPTS(0)%TOLAST(:,2)
!
          DO
            DTTST   = DSEC21 ( STIME , TOUT )
            IF ( DTTST .LT. 0 ) THEN
                CALL TICK21 ( TOUT, OUTPTS(0)%DTOUT(2) )
              ELSE
                EXIT
              END IF
            END DO
!
          OUTPTS(0)%TONEXT(:,2) = TOUT
!
          DTTST  = DSEC21 ( TOUT , TLST )
          IF ( DTTST .LT. 0. ) THEN
              UNIPTS = .FALSE.
            ELSE
              CALL WMIOPP ( OT2(0)%NPTS, OT2(0)%X, OT2(0)%Y,          &
                            OT2(0)%PNAMES )
            END IF
!
#ifdef W3_MPI
          DO I=1, NRGRD
            CALL WMSETM ( I, MDSE, MDST )
            CALL W3SETG ( I, MDSE, MDST )
            CALL W3SETO ( I, MDSE, MDST )
            IF ( FBCAST .AND. MPI_COMM_BCT.NE.MPI_COMM_NULL ) THEN
                CALL MPI_BCAST ( NOPTS, 1, MPI_INTEGER, 0,       &
                                 MPI_COMM_BCT, IERR_MPI )
              END IF
            END DO
#endif
!
        END IF
!
! 8.c.6 Output
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                      &
          WRITE (MDSS,938) 'Additional group information'
!
      IF ( MAXVAL(GRDLOW(:,0)) .GT. 0 ) THEN
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,933) 'Lower rank grid dependence'
          IF ( NMPLOG .EQ. IMPROC )                                   &
              WRITE (MDSO,933) 'Lower rank grid dependence'
          DO I=1, NRGRD
            WRITE (LINE(1:6),'(1X,I3,2X)') I
            JJJ    = 6
            IF ( GRDLOW(I,0) .NE. 0 ) THEN
                DO J=1, GRDLOW(I,0)
                  WRITE (LINE(JJJ+1:JJJ+3),'(I3)') GRDLOW(I,J)
                  JJJ    = JJJ + 3
                  END DO
              ELSE IF ( FLRBPI(I) ) THEN
                JJJ    = 21
                LINE(7:JJJ) = ' Data from file'
              ELSE
                JJJ    = 22
                LINE(7:JJJ) = ' No dependencies'
              END IF
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                WRITE(MDSS,934) LINE(1:JJJ)
            IF ( NMPLOG .EQ. IMPROC ) WRITE(MDSO,934) LINE(1:JJJ)
            END DO
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,935)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,935)
        ELSE
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,937) 'No lower rank grid dependencies'
          IF ( NMPLOG .EQ. IMPROC )                                   &
              WRITE (MDSO,937) 'No lower rank grid dependencies'
        END IF
      DEALLOCATE ( FLRBPI )
!
      IF ( MAXVAL(GRDEQL(:,0)) .GT. 0 ) THEN
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,933) 'Same rank grid dependence'
          IF ( NMPLOG .EQ. IMPROC )                                   &
              WRITE (MDSO,933) 'Same rank grid dependence'
          DO I=1, NRGRD
            WRITE (LINE(1:6),'(1X,I3,2X)') I
            JJJ    = 6
            IF ( GRDEQL(I,0) .NE. 0 ) THEN
                DO J=1, GRDEQL(I,0)
                  WRITE (LINE(JJJ+1:JJJ+3),'(I3)') GRDEQL(I,J)
                  JJJ    = JJJ + 3
                  END DO
              ELSE
                JJJ    = 22
                LINE(7:JJJ) = ' No dependencies'
              END IF
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                WRITE(MDSS,934) LINE(1:JJJ)
            IF ( NMPLOG .EQ. IMPROC ) WRITE(MDSO,934) LINE(1:JJJ)
            END DO
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,935)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,935)
        ELSE
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,937) 'No same rank grid dependencies'
          IF ( NMPLOG .EQ. IMPROC )                                   &
              WRITE (MDSO,937) 'No same rank grid dependencies'
        END IF
!
      IF ( MAXVAL(GRDHGH(:,0)) .GT. 0 ) THEN
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,933) 'Higher rank grid dependence'
          IF ( NMPLOG .EQ. IMPROC )                                   &
              WRITE (MDSO,933) 'Higher rank grid dependence'
          DO I=1, NRGRD
            WRITE (LINE(1:6),'(1X,I3,2X)') I
            JJJ    = 6
            IF ( GRDHGH(I,0) .NE. 0 ) THEN
                DO J=1, GRDHGH(I,0)
                  WRITE (LINE(JJJ+1:JJJ+3),'(I3)') GRDHGH(I,J)
                  JJJ    = JJJ + 3
                  END DO
              ELSE
                JJJ    = 22
                LINE(7:JJJ) = ' No dependencies'
              END IF
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                WRITE(MDSS,934) LINE(1:JJJ)
            IF ( NMPLOG .EQ. IMPROC ) WRITE(MDSO,934) LINE(1:JJJ)
            END DO
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,935)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,935)
        ELSE
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,937) 'No higher rank grid dependencies'
          IF ( NMPLOG .EQ. IMPROC )                                   &
              WRITE (MDSO,937) 'No higher rank grid dependencies'
        END IF
!
#ifdef W3_T
      WRITE (MDST,9083)
      DO I=-NRINP, NRGRD
        WRITE (MDST,9084) I, IDINP(I,:)
        END DO
#endif
!
!    Test output of connected units (always)
!
      CALL WMUSET ( MDSE, MDST, SCRATCH, .FALSE. )
      IF ( TSTOUT ) CALL WMUDMP ( MDST, 0 )
!
      DEALLOCATE ( MDS, NTRACE, ODAT, FLGRD, FLGR2, FLGD, FLG2, INAMES,&
                   MNAMES                                              &
                   ,OUTFF )
!
#ifdef W3_MPI
      CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
#endif
!
      CALL DATE_AND_TIME ( VALUES=CLKDT2 )
      CLKFIN = TDIFF ( CLKDT1,CLKDT2 )
!
#ifdef W3_MPRF
      CALL PRTIME ( PRFTN )
      WRITE (MDSP,990) PRFT0, PRFTN, get_memory(), 'END'
#endif
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,998)
#ifdef W3_O10
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,999)
#endif
!!!!!/MPI CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
!!!!!/MPI CALL MPI_FINALIZE  ( IERR_MPI )
!!!!!/MPI stop 'Ending in wminitmd, case 1'
!
      RETURN
!
! Escape locations read errors :
!
 2000 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1000) IFNAME, IERR
      CALL EXTCDE ( 2000 )
      RETURN
!
 2001 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1001)
      CALL EXTCDE ( 2001 )
      RETURN
!
 2002 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1002) IERR
      CALL EXTCDE ( 2002 )
      RETURN
!
 2010 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1010) IERR
      CALL EXTCDE ( 2010 )
      RETURN
!
 2011 CONTINUE
! === no process number filtering for test file !!! ===
      WRITE (MDSE,1011) IERR
      CALL EXTCDE ( 2011 )
      RETURN
!
 2020 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1020) 
      CALL EXTCDE ( 2020 )
      RETURN
!
 2021 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1021) 
      CALL EXTCDE ( 2021 )
      RETURN
!
 2030 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1030) MNAMES(I), INAMES(I,J)
      CALL EXTCDE ( 2030 )
      RETURN
!
 2031 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1031) INAMES(I,J), J
      CALL EXTCDE ( 2031 )
      RETURN
!
!2050 CONTINUE
!     IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1040)
!     CALL EXTCDE ( 2050 )
!     RETURN
!
 2051 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1051) MN(:II)
      CALL EXTCDE ( 2051 )
      RETURN
!
 2052 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1052) J
      CALL EXTCDE ( 2052 )
      RETURN
!
 2053 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1053)
      CALL EXTCDE ( 2053 )
      RETURN
!
 2054 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1054)
      CALL EXTCDE ( 2054 )
      RETURN
!
 2060 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1060)
      CALL EXTCDE ( 2060 )
      RETURN
!
 2070 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1070)
      CALL EXTCDE ( 2070 )
      RETURN
!
 2080 CONTINUE
      CALL EXTCDE ( 2080 )
      RETURN
!
! Formats
!
  900 FORMAT ( ' ========== STARTING MWW3 INITIALIZATION (WMINIT) =', &
               '============================'/)
  901 FORMAT ( ' WAVEWATCH III log file            ',                 &
               '                     version ',A/                     &
               ' ==================================',                 &
               '==================================='/                 &
               ' multi-grid model driver                          ',  &
               'date : ',A10/50X,'time :  ',A8)
!
  910 FORMAT ( '  Opening input file ',A,' (unit number',I3,')')
  911 FORMAT ( '  Opening output file ',A,' (unit number',I3,')')
  912 FORMAT (/'  Comment character : ''',A,'''')
!
  920 FORMAT (/'  Number of grids          :',I3)
  921 FORMAT ( '  No input data grids.')
  922 FORMAT ( '  Input data grids         :',I3)
  923 FORMAT ( '  Single point output file : ',A)
 1923 FORMAT (/'  Output server type       :',I3)
 2923 FORMAT ( '  Single point output proc : ',A)
 3923 FORMAT ( '  Grids share output procs : ',A)
!
  924 FORMAT (/'  Input grid information : '/                         &
               '  nr extension  lev.   cur.   wind   ice    tau',     &
               '    rho    data'/    &
               ' ----------------------------------------------',     &
               '--------------')
  925 FORMAT (1X,I3,1X,A10,6(1X,A6),3(1X,A1))
  926 FORMAT ( ' ----------------------------------------------',     &
               '--------------')
!
  927 FORMAT (/'  Grid for point output : '/                          &
               '  nr extension  '/ ' ---------------')
  928 FORMAT (5X,A10)
  929 FORMAT ( ' ---------------')
!
  930 FORMAT (/'  Wave grid information : '/                          &
               '  nr extension  lev.   cur.   wind   ice    tau',     &
               '    rho    data   move1 rnk grp dmp'/                 &
               ' ----------------------------------------------',     &
               '-----------------------------------')
  931 FORMAT (1X,I3,1X,A10,6(1X,A6),3(1X,A1),2X,A4,2I4,3X,A1)
  932 FORMAT ( ' -----------------------------------------------',    &
               '-----------------------------------'/)
  933 FORMAT ( '  ',A,' : '/                                          &
               '  nr   grids (part of comm.)'/                        &
               ' -----------------------------------------------',    &
               '---------------------')
  934 FORMAT (A)
  935 FORMAT ( ' -----------------------------------------------',    &
               '---------------------'/)
  936 FORMAT (/'  ',A,' : '/                                          &
               '  nr   Depends on '/                                  &
               ' -----------------------------------------------',    &
               '---------------------')
  937 FORMAT ( '  ',A/)
  938 FORMAT (/'  ',A/)
!
  940 FORMAT (/'  Time interval : '/                                  &
               ' --------------------------------------------------')
  941 FORMAT ( '       Starting time : ',A)
  942 FORMAT ( '       Ending time   : ',A/)
  943 FORMAT (/'  Model settings : '/                                 &
               ' --------------------------------------------------')
  944 FORMAT ( '       Masking computation in nesting : ',A)
  945 FORMAT ( '       Masking output in nesting      : ',A/)
!
  950 FORMAT (/'  Output requests : (ALL GRIDS) '/                    &
               ' ==================================================')
  951 FORMAT (/'       Type',I2,' : ',A/                              &
               '      -----------------------------------------')
  952 FORMAT ( '            From     : ',A)
  953 FORMAT ( '            To       : ',A)
  954 FORMAT ( '            Interval : ',A/)
  955 FORMAT ( '            Fields   : ',A)
  956 FORMAT ( '                       ',A)
  957 FORMAT ( '            Point  1 : ',2E14.6,2X,A)             
  958 FORMAT ( '              ',I6,' : ',2E14.6,2X,A)
  959 FORMAT ( '            No points defined')
  960 FORMAT ( '            The file with ',A,' data is ',A,'.')
  961 FORMAT ( '            IX fls   : ',3I6/                         &
               '            IY fls   : ',3I6)
  962 FORMAT (/'  Output request for model ',A,' (nr',I3,') '/        &
               ' ==================================================')
  963 FORMAT ( '            Output disabled')
!
  965 FORMAT (/'  Grid movement data (!/MGP, !/MGW): '/               &
               ' --------------------------------------------------')
  966 FORMAT ( '       ',A)
  967 FORMAT ( '       ',I6,2X,A)
  968 FORMAT ( '          ',I6,I11.8,I7.6,2F8.2)
!
  970 FORMAT(//'  Assigning resources : '/                            &
               ' --------------------------------------------------')
  971 FORMAT ( '       ',A)
  972 FORMAT ( '       Process ',I5.5,' reserved for all point output.')
  973 FORMAT ( '       Processes ',I5.5,' through ',I5.5,' [',I3,']', &
               ' reserved for output.')
  974 FORMAT (/                                                       &
        5X,'  grid           comp.      grd    pnt    trk    rst    bpt    prt'/     &
        5X,' ------------------------------------------------------', &
           '-------------')
  975 FORMAT (5X,'  ',A10,2X,I5.5,'-',I5.5,6(2x,A5))
  976 FORMAT(5X,' -------------------------------------------------', &
                '------------------')
  977 FORMAT (5X,'    Unified point output at ',I5.5)
 1974 FORMAT ('  Resource assignement (processes) : '/                &
           '  grid           comp.      grd    pnt    trk    rst    bpt    prt'/     &
           ' ------------------------------------------------------', &
           '-------------')
 1975 FORMAT ('  ',A10,2X,I5.5,'-',I5.5,6(2x,A5))
 1976 FORMAT (' ---------------------------------------------------', &
              '----------------')
 1977 FORMAT ('    Unified point output at ',I5.5)
!
  980 FORMAT(//'  Initializations :'/                                 &
               ' --------------------------------------------------')
  981 FORMAT ( '       Model number',I3,' [',A,']')
  982 FORMAT ( '          Initializing wave model ...')
  983 FORMAT ( '          Initializing model input ...')
  984 FORMAT ( '            ',A,': file not needed')
  985 FORMAT ( '            ',A,': file OK')
  986 FORMAT ( '       Unified point output [',A,']')
  987 FORMAT ( '          Initializing grids ...')
  988 FORMAT ( '       Input data grid',I3,' [',A,']')
!
#ifdef W3_MPRF
  990 FORMAT (1X,3F12.3,' WMINIT',1X,A)
#endif
!
  998 FORMAT ( '  Running the model :'/                               &
               ' --------------------------------------------------'/)
  999 FORMAT ( ' ========== END OF MWW3 INITIALIZATION (WMINIT) ===', &
               '============================'/)
!
 1000 FORMAT (/' *** WAVEWATCH III ERROR IN WMINIT : *** '/           &
               '     ERROR IN OPENING INPUT FILE ',A/                 &
               '     IOSTAT =',I5/)
!
 1001 FORMAT (/' *** WAVEWATCH III ERROR IN WMINIT : *** '/           &
               '     PREMATURE END OF INPUT FILE'/)
! 
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN WMINIT : *** '/           &
               '     ERROR IN READING FROM INPUT FILE'/               &
               '     IOSTAT =',I5/)
 1010 FORMAT (/' *** WAVEWATCH III ERROR IN WMINIT : *** '/           &
               '     ERROR IN OPENING LOG FILE'/                      &
               '     IOSTAT =',I5/)
 1011 FORMAT (/' *** WAVEWATCH III ERROR IN WMINIT : *** '/           &
               '     ERROR IN OPENING TEST FILE'/                     &
               '     IOSTAT =',I5/)
 1020 FORMAT (/' *** WAVEWATCH III ERROR IN WMINIT : *** '/           &
               '     ILLEGAL NUMBER OF GRIDS ( < 1 ) '/)
 1021 FORMAT (/' *** WAVEWATCH III ERROR IN WMINIT : *** '/           &
               '     ILLEGAL NUMBER OF INPUT GRIDS ( < 0 ) '/)
 1030 FORMAT (/' *** WAVEWATCH III ERROR IN WMINIT : *** '/           &
               '     INPUT GRID NAME NOT FOUND '/                     &
               '     WAVE GRID  : ',A/                                &
               '     INPUT NAME : ',A/)
 1031 FORMAT (/' *** WAVEWATCH III ERROR IN WMINIT : *** '/           &
               '     REQUESTED INPUT TYPE NOT FOUND IN INPUT GRID '/  &
               '     INPUT GRID : ',A/                                &
               '     INPUT TYPE : ',I8/)
 1032 FORMAT (/' *** WAVEWATCH III WARNING IN WMINIT : *** '/         &
               '     INPUT GRID ',A,' NOT USED '/)
 1040 FORMAT ( ' *** WAVEWATCH III WARNING IN W3MLTI : ***'/          &
               '     POSSIBLE LOAD IMBALANCE GROUP',I3,' :',2I6/)
!1040 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/            &
!              '     ILLEGAL TIME INTERVAL'/)
 1050 FORMAT (/' *** WAVEWATCH III WARNING IN W3MLTI : ***'/          &
               '     UNIFIED POINT OUTPUT BUT NO OUTPUT'/             &
               '     UNIFIED POINT OUTPUT DISABLED'/)
 1051 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/            &
               '     ILLEGAL MODEL ID [',A,']'/)
 1052 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/            &
               '     ILLEGAL OUTPUT TYPE',I10/)
 1053 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/            &
         '     OUTPUT POINTS FOR INDIVIDUAL GRIDS CANNOT BE DEFINED'/ &
               '     WHEN UNIFIED POINT OUTPUT IS REQUESTED'/)
 1054 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/            &
         '     POINT OUTPUT ACTIVATED, BUT NO POINTS DEFINED'/)
 1060 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/            &
               '     NO MOVING GRID DATA PRESENT'/)
 1070 FORMAT (/' *** WAVEWATCH III ERROR IN WMINIT : ***'/            &
               '     ALL GRIDS ARE NOT USING THE SAME COORDINATE SYSTEM'/)
 1080 FORMAT (/' *** BOUNDARY DATA READ, WILL NOT DUMP, GRID :',I4,   &
               ' ***')
 1081 FORMAT (/' *** NO BOUNDARY DATA TO DUMP, GRID :',I4,' ***')
 1082 FORMAT ( '  No boundary data dump for grid',I3/)
!
#ifdef W3_T
 9000 FORMAT ( ' TEST WMINIT : UNIT NUMBERS    : ',5I6/            &
               '               INPUT FILE NAME : ',A)
#endif
!
#ifdef W3_T
 9020 FORMAT ( ' TEST WMINIT : UNIT NUMBERS FOR GRIDS (',A,')'/    &
                               15X,'GRID MDS(1-13)',43X,'NTRACE')
 9021 FORMAT (14X,16I4)
 9022 FORMAT ( ' TEST WMINIT : UNIT NUMBERS FOR INTPUT FILES'/     &
                               15X,'GRID MDSF(JFIRST-9)')
 9030 FORMAT ( ' TEST WMINIT : FILE EXTENSIONS, INPUT FLAGS,',     &
               ' RANK AND GROUP, PROC RANGE')
 9031 FORMAT ( '            ',I3,1X,A,20L2,2I4,2F6.2)
 9032 FORMAT ( ' TEST WMINIT : PROCESSED RANK NUMBERS')
 9033 FORMAT ( '             ',I3,1X,A,1X,I4)
 9034 FORMAT ( ' TEST WMINIT : NUMBER OF GROUPS :',I4)
 9035 FORMAT ( ' TEST WMINIT : SIZE OF GROUPS :',20I3)
 9036 FORMAT ( ' TEST WMINIT : GROUP SIZE AND COMPONENTS :')
 9037 FORMAT ( '             ',2I3,':',20I3)
#endif
!
#ifdef W3_T
 9050 FORMAT ( ' TEST WMINIT : GRID NUMBER',I3,' =================')
 9051 FORMAT ( ' TEST WMINIT : ODAT   : ',I9.8,I7.6,I7,I9.8,I7.6,  &
                                  5(/24X,I9.8,I7.6,I7,I9.8,I7.6) )
 9053 FORMAT ( ' TEST WMINITNML : OUTFF   : ',I9.8 &
                                  5(/24X,I9.8) )
 9052 FORMAT ( ' TEST WMINIT : FLGRD  : ',5(5L2,1X)/24X,5(5L2,1X))
#endif
!
#ifdef W3_T
 9060 FORMAT ( ' TEST WMINIT : GRID MOVEMENT DATA')
 9061 FORMAT ( '             ',I8.8,I7,1X,2F8.2)
#endif
!
#ifdef W3_T
 9070 FORMAT ( ' TEST WMINIT : ALLPRC ')
 9071 FORMAT ( ' ',I3,'  : ',250I3)
 8042 FORMAT ( ' TEST WMINIT : MODMAP ')
 8043 FORMAT ( ' TEST WMINIT : LOADMP ')
 8044 FORMAT ( '        ',I3,'  : ',250I2)
#endif
!
#ifdef W3_T
 9080 FORMAT ( ' TEST WMINIT : MODEL INITIALIZATION')
 9081 FORMAT ( '               MODEL AND TIME   :',I4,I10.8,I8.6)
 9082 FORMAT ( '               STATUS AND TIMES :',I4,3(I10.8,I8.6))
 9083 FORMAT ( ' TEST WMINIT : IDINP AFTER INITIALIZATION :')
 9084 FORMAT ( '               ',I4,17(2X,A3))
#endif
!/
!/ End of WMINIT ----------------------------------------------------- /
!/
      END SUBROUTINE WMINIT






!/ ------------------------------------------------------------------- /
      SUBROUTINE WMINITNML ( IDSI, IDSO, IDSS, IDST, IDSE, IFNAME,       &
                          MPI_COMM, PREAMB )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    13-Jun-2005 : Origination.                        ( version 3.07 )
!/    28-Dec-2005 : Add static nesting.                 ( version 3.08 )
!/    25-May-2006 : Add overlapping grids.              ( version 3.09 )
!/    26-Jun-2006 : Add output type 6.                  ( version 3.09 )
!/    29-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    09-Aug-2006 : Unified point output added.         ( version 3.10 )
!/    14-Oct-2006 : Adding separate input grids.        ( version 3.10 )
!/    03-Nov-2006 : Adding wave field separation.       ( version 3.10 )
!/    02-Feb-2007 : Adding FLAGST initialization.       ( version 3.10 )
!/    21-Jun-2007 : Dedicated output processes.         ( version 3.11 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    16-Aug-2010 : Adding NTRMAX to unify NTRACE.    ( version 3.14.5 )
!/    21-Sep-2010 : Adding coupling output              ( version 3.14-Ifremer)
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    28-Jul-2012 : Initialize FLGR2 properly.          ( version 4.08 )
!/                  Tom Durant's fix, but moved to allocation.
!/    28-Nov-2012 : Bug fix: Distribute to idle processors the grid data
!/                  required for regridding.            ( version 4.08 )
!/                  (T. J. Campbell, NRL)
!/    02-Sep-2012 : Set up for > 999 test files.        ( version 4.10 )
!/                  Set up output for  > 999 procs.
!/    03-Sep-2012 : Output of initilization time.       ( version 4.10 )
!/                  Switch test file on/off (TSTOUT)
!/    28-Nov-2012 : Bug fix: Distribute to idle processors the grid data
!/                  required for regridding.            ( version 4.08 )
!/                  (T. J. Campbell, NRL)
!/    15-Apr-2013 : Changes the reading of output fields( version 4.10 )
!/                  (F. Ardhuin) 
!/    28-Jan-2014 : Add memory hwm to profiling.        ( version 5.00 )
!/    27-May-2014 : Bug fix prf file name.              ( version 5.02 )
!/    17-Sep-2014 : Read mod_def before inp file        ( version 5.03 )
!/    17-Feb-2016 : New version from namelist use       ( version 5.11 )
!/    20-Jan-2017 : Update to new W3GSRUMD APIs         ( version 6.02 )
!/    20-Jan-2017 : Modify input forcing flags to support coupler input.
!/                  Add ESMF override for STIME & ETIME ( version 6.02 )
!/                  (T. J. Campbell, NRL)
!/    15-May-2018 : Update namelist                     ( version 6.05 )
!/    22-Mar-2021 : Add momentum and air density input  ( version 7.13 )
!/
!  1. Purpose :
!
!     Initialize multi-grid version of WAVEWATCH III.
!
!  2. Method :
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IDSI    Int.   I   Unit number for input file.
!       IDSO    Int.   I   Unit number for output file.
!       IDSS    Int.   I   Unit number for "screen" output. Switch off
!                          by setting equal to IDSO.
!       IDST    Int.   I   Unit number for test output.
!       IDSE    Int.   I   Unit number for error output.
!       IFNAME  Char   I   File name for input file.
!      MPI_COMM Int.   I   MPI communicator to be used.
!       PREAMB  Char   I   File name preamble (optiona).
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3NMOD    Subr. W3GDATMD Data structure initialization.
!      W3DIMX    Subr.   Id.    Set grid arrays.
!      W3DIMS    Subr.   Id.    Set grid arrays.
!      W3SETG    Subr.   Id.    Point to grid/model.
!      W3NDAT    Subr. W3WDATMD Data structure initialization.
!      W3SETW    Subr.   Id.    Point to grid/model.
!      W3NAUX    Subr. W3ADATMD Data structure initialization.
!      W3SETA    Subr.   Id.    Point to grid/model.
!      W3NOUT    Subr. W3ODATMD Data structure initialization.
!      W3SETO    Subr.   Id.    Point to grid/model.
!      W3NINP    Subr. W3IDATMD Data structure initialization.
!      W3SETI    Subr.   Id.    Point to grid/model.
!      W3DIMI    Subr.   Id.    Allocate grid/model.
!      WMNDAT    Subr. WMMDATMD Data structure initialization.
!      WMSETM    Subr.   Id.    Point to grid/model.
!      WMDIMD    Subr.   Id.    Allocate array space.
!      W3FLDO    Subr. W3FLDSMD Open input data file.
!      W3IOGR    Subr. W3IOGRMD Reading of model definition file.
!      W3INIT    Subr. W3INITMD Model intiailization.
!      WMGLOW    Subr. WMGRIDMD Lower rank grid dependencies.
!      WMGEQL    Subr.   Id.    Same rank grid dependencies.
!      WMGHGH    Subr.   Id.    Higher rank grid dependencies.
!      RESPEC    Subr.   Id.    Spectral conversion flags.
!      WMIOBS    Subr. WMINIOMD Stage boundary data.
!      WMIOBG    Subr.   Id.    Gather boundary data.
!      WMIOBF    Subr.   Id.    Finalize staging in WMIOBS.
!      WMUINI    Subr. WMUNITMD Initialize dynamic unit assignment,
!      WMUDMP    Subr.   Id.    Dump dynamic unit data,
!      WMUSET    Subr.   Id.    Set unit number data.
!      WMUGET    Subr.   Id.    Get a unit number.
!      WMUINQ    Subr.   Id.    Update unit number info.
!      WMIOPP    Subr. WMIOPOMD Initialize unified point output.
!      ITRACE    Subr. W3SERVMD Initialize subroutine tracing.
!      STRACE    Subr.   Id.    Subroutine tracing.
!      EXTCDE    Subr.   Id.    Program abort.
!      WWDATE    Subr.   Id.    System date.
!      WWTIME    Subr.   Id.    System time.
!      PRINIT    Subr.   Id.    Profiling routine ( !/MPRF )
!      PRTIME    Subr.   Id.    Profiling routine ( !/MPRF )
!      STME21    Subr. W3TIMEMD Convert time to string.
!      DSEC21    Func.   Id.    Difference between times.
!      TICK21    Subr.   Id.    Advance the clock.
!      W3READFLGRD Subr. W3IOGOMD Reads flags or namelist for output fields
!
!      MPI_COMM_SIZE, CALL MPI_COMM_RANK, MPI_BARRIER, MPI_COMM_GROUP,
!      MPI_GROUP_INCLUDE, MPI_COMM_CREATE, MPI_GROUP_FREE, MPI_BCAST
!                Subr. mpif.h   Standard MPI routines.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3MLTI    Prog.   N/A    Multi-grid model driver.
!      ....                     Any coupled model.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     See formats 1000 and following, or escape locations 2000 and
!     following.
!
!  7. Remarks :
!
!     - When running regtests in cases where disk is non-local 
!       (i.e. NFS used), there can be a huge improvment in compute
!       time by using /var/tmp/ for log files. 
!       See commented line at "OPEN (MDSO,FILE=..."
!
!     - IDFLDS dimensioning is hardwired as IDFLDS(-7:9) where lowest possible 
!       value of JFIRST is JFIRST=-7
!
!  8. Structure :
!
!     --------------------------------------------------------------
!      1.  Multi-grid model intializations
!        a Unit numbers
!        b Subroutine tracing                            ( ITRACE )
!        c Input file
!        d Log and test files
!        e Initial and test output
!      2.  Set-up of data structures and I/O
!        a Get number of grids
!        b Set up data structures
!                 ( W3NMOD, W3NDAT, W3NAUX, W3NOUT, W3NINP, WMNDAT )
!        c Set up I/O for individual models
!      3.  Get individual grid information
!        a Read data
!        b Assign input file numbers.
!        c Set rank and group data
!        d Unified point output file.                    ( W3IOGR )
!        e Output
!      4.  Model run time information and settings
!      5.  Output requests
!        a Loop over types for unified output
!        ---------------------------------------------------
!        b Process standard line
!        c Type 1: fields of mean wave parameters
!        d Type 2: point output
!        e Type 3: track output
!        f Type 4: restart files (no additional data)
!        g Type 5: nesting data (no additional data)
!        h Type 6: wave field data (dummy for now)
!        i Set all grids to unified output
!        ---------------------------------------------------
!        j Endless loop for correcting output per grid
!        ---------------------------------------------------
!          Test grid name and output number
!        k Process standard line
!        l Type 1: fields of mean wave parameters
!        m Type 2: point output
!        n Type 3: track output
!        o Type 6: partitioning output
!        p Type 7: coupling output
!        ---------------------------------------------------
!      6.  Read moving grid data
!      7.  Work load distribution
!        a Initialize arrays
!        b Set communicators and ALLPRC array
!        c Set MODMAP and LOADMP arrays
!        d Warnings
!      8.  Actual initializations
!        a Loop over models for per-model initialization
!           1 Wave model                                 ( W3INIT )
!           2 Data files                                 ( W3FLDO )
!           3 Grid status indicator and model times
!           3 Grid data for processors that are NOT used.
!           5 Test output
!        b Input data files.
!        c Inter model initialization
!           1 Set spectral conversion flags              ( WMRSPC )
!           2 Prepare unified point output               ( WMIOPO )
!           3 Relation to lower ranked grids
!                                ( WMGLOW, WMIOBS, WMIOBG, WMIOBF )
!           4 Relation to same ranked grids              ( WMGEQL )
!           5 Relation to higher ranked grids            ( WMGHGH )
!           6 Output
!     --------------------------------------------------------------
!
!  9. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   Id.
!                            
!       !/MGW   Moving grid wind correction.
!       !/MGP   Moving grid propagation correction.
!
!       !/O10   Enable output identifying start and end of routine
!      
!       !/S     Enable subroutine tracing.
!       !/T     Enable test output.
!       !/MPRF  Profiling.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
!/
      USE W3GDATMD, ONLY: W3NMOD, W3DIMX, W3DIMS, W3SETG
      USE W3WDATMD, ONLY: W3NDAT, W3SETW
      USE W3ADATMD, ONLY: W3NAUX, W3SETA
      USE W3ODATMD, ONLY: W3NOUT, W3SETO
      USE W3ODATMD, ONLY:  OFILES
      USE W3IDATMD, ONLY: W3NINP, W3SETI, W3DIMI
      USE WMMDATMD, ONLY: WMNDAT, WMSETM, WMDIMD
!
      USE W3FLDSMD, ONLY: W3FLDO
      USE W3IOGOMD, ONLY: W3READFLGRD, W3FLGRDFLAG
      USE W3IOGRMD, ONLY: W3IOGR
      USE W3INITMD, ONLY: W3INIT
      USE WMGRIDMD, ONLY: WMRSPC, WMGLOW, WMGEQL, WMGHGH, WMSMCEQL
      USE WMINIOMD, ONLY: WMIOBS, WMIOBG, WMIOBF
      USE WMIOPOMD, ONLY: WMIOPP
!/
      USE W3SERVMD, ONLY: ITRACE, EXTCDE, NEXTLN, WWDATE, WWTIME
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
#ifdef W3_MPRF
      USE W3TIMEMD, ONLY: PRINIT, PRTIME
#endif
      USE W3TIMEMD, ONLY: STME21, DSEC21, TICK21, TDIFF
      USE WMUNITMD, ONLY: WMUINI, WMUDMP, WMUSET, WMUGET, WMUINQ
!/
      USE W3GDATMD, ONLY: GTYPE, NX, NY, FILEXT, NSEA, FLAGST, GRIDS
#ifdef W3_SMC
      USE W3GDATMD, ONLY: NCel, NUFc, NVFc, NRLv, NBSMC 
      USE W3GDATMD, ONLY: NARC, NBAC, NSPEC, SMCTYPE 
#endif
#ifdef W3_MPI
      USE W3GDATMD, ONLY: FLAGLL, ICLOSE, GSU, X0, Y0, SX, SY,   &
                          XGRD, YGRD, DXDP, DXDQ, DYDP, DYDQ,    &
                          HQFAC, HPFAC, MAPSTA, MAPST2,          &
                          GRIDSHIFT, NSEAL, NK, NTH, XFR, FR1,   &
                          TH, DTMAX, DTCFL
      USE W3GSRUMD
#endif
      USE W3WDATMD, ONLY: TIME
      USE W3ADATMD, ONLY: WADATS
      USE W3IDATMD, ONLY: INFLAGS1, INPUTS, IINIT,      &
                          JFIRST, INFLAGS2
      USE W3ODATMD, ONLY: NOGRP, NGRPP, FLOUT, TONEXT, FLBPI,  &
                          FLBPO, NFBPO, NBI, NDS, IAPROC,     &
                          NAPFLD, NAPPNT, NAPTRK,  NAPBPT,     &
                          NAPPRT, NAPROC, FNMPRE, OUTPTS, NDST, NDSE, &
                          NOPTS, IOSTYP, UNIPTS, UPPROC, DTOUT,       &
                          TOLAST, NOTYPE
      USE WMMDATMD, ONLY: MDSI, MDSO, MDSS, MDST, MDSE, MDSF, MDSUP,  &
                          IMPROC, NMPROC, NMPSCR, NMPERR,     &
                          NMPLOG, NMPUPT, STIME, ETIME, NMV, NMVMAX,  &
                          TMV, AMV, DMV, NRGRD, NRINP, NRGRP, GRANK,  &
                          GRGRP, INGRP, GRDHGH, GRDEQL, GRDLOW,       &
                          ALLPRC, MODMAP, TSYNC, TMAX, TOUTP, TDATA,  &
                          GRSTAT, DTRES, BCDUMP, FLGHG1, FLGHG2,      &
                          INPMAP, IDINP, NGRPSMC
      USE WMMDATMD, ONLY: CLKDT1, CLKDT2, CLKFIN
#ifdef W3_MPI
      USE WMMDATMD, ONLY: MPI_COMM_MWAVE, MPI_COMM_GRD,          &
                          MPI_COMM_BCT, CROOT, FBCAST
#endif
#ifdef W3_MPRF
      USE WMMDATMD, ONLY: MDSP
#endif
      USE W3INITMD, ONLY: WWVER
      USE W3NMLMULTIMD
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
      INTEGER, INTENT(IN)        :: IDSI, IDSO, IDSS, IDST, IDSE,     &
                                    MPI_COMM
      CHARACTER*(*), INTENT(IN)  :: IFNAME
      CHARACTER*(*), INTENT(IN), OPTIONAL :: PREAMB
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      TYPE(NML_DOMAIN_T)                      :: NML_DOMAIN
      TYPE(NML_INPUT_GRID_T), ALLOCATABLE     :: NML_INPUT_GRID(:) 
      TYPE(NML_MODEL_GRID_T), ALLOCATABLE     :: NML_MODEL_GRID(:) 
      TYPE(NML_OUTPUT_TYPE_T), ALLOCATABLE    :: NML_OUTPUT_TYPE(:)
      TYPE(NML_OUTPUT_DATE_T), ALLOCATABLE    :: NML_OUTPUT_DATE(:)
      TYPE(NML_HOMOG_COUNT_T)                 :: NML_HOMOG_COUNT
      TYPE(NML_HOMOG_INPUT_T), ALLOCATABLE    :: NML_HOMOG_INPUT(:)
!
      TYPE OT2TPE
        INTEGER                    :: NPTS
        REAL, POINTER              :: X(:), Y(:)
        CHARACTER(LEN=40), POINTER :: PNAMES(:)
      END TYPE OT2TPE
!
      TYPE(OT2TPE), ALLOCATABLE    :: OT2(:)
!
      INTEGER                 :: MDSE2, IERR, I,J,K, N_MOV, N_TOT,     &
                                 SCRATCH, RNKMIN, RNKMAX, RNKTMP,      &
                                 GRPMIN, GRPMAX, II, NDSREC, NDSFND,   &
                                 NPTS, JJ, IP1, IPN, MPI_COMM_LOC,     &
                                 NMPSC2, JJJ, NCPROC, NPOUTT, NAPLOC,  &
                                 NAPRES, NAPADD, NAPBCT, IFI, IFJ, IW, &
                                 IFT, ILOOP 
!
      INTEGER                 :: TTIME(2), TOUT(2), STMPT(2), ETMPT(2),&
                                 TLST(2)
#ifdef W3_MPI
      INTEGER                 :: IERR_MPI, BGROUP, LGROUP, IROOT
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!
      INTEGER, ALLOCATABLE    :: MDS(:,:), NTRACE(:,:), ODAT(:,:),     &
                                 TMPRNK(:), TMPGRP(:), NINGRP(:),      &
                                 TMOVE(:,:), LOADMP(:,:), IPRT(:,:),   &
                                 NDPOUT(:)                             &
                                 ,OUTFF(:,:)
!
      REAL                    :: DTTST, XX, YY
#ifdef W3_MPRF
      REAL                    :: PRFT0, PRFTN
      REAL(KIND=8)            :: get_memory
#endif
!
      REAL, ALLOCATABLE       :: X(:), Y(:), AMOVE(:), DMOVE(:),       &
                                 RP1(:), RPN(:)
!
      LOGICAL                 :: FLT, TFLAGI, TFLAGS(-7:14), PSHARE
      LOGICAL, ALLOCATABLE    :: FLGRD(:,:,:), FLRBPI(:), BCDTMP(:),   &
                                 USEINP(:), LPRT(:), FLGR2(:,:,:),     &
                                 FLGD(:,:), FLG2(:,:), FLG2D(:,:),     &
                                 FLG1D(:), CPLINP(:)
!
      CHARACTER(LEN=1)        :: COMSTR
      CHARACTER(LEN=256)      :: TMPLINE, TEST
      CHARACTER(LEN=3)        :: IDSTR(-7:9), IDTST
      CHARACTER(LEN=5)        :: STOUT, OUTSTR(6)
      CHARACTER(LEN=6)        :: YESXX, XXXNO
      CHARACTER(LEN=6),                                                &
                  ALLOCATABLE :: ACTION(:)
      CHARACTER(LEN=8)        :: LFILE, STTIME
#ifdef W3_SHRD
      CHARACTER(LEN=9)        :: TFILE
#endif
      CHARACTER(LEN=13)       :: STDATE, MN, TNAMES(9)
      CHARACTER(LEN=40)       :: PN
      CHARACTER(LEN=13),                                               &
                  ALLOCATABLE :: INAMES(:,:), MNAMES(:)
      CHARACTER(LEN=40),                                               &
                  ALLOCATABLE :: PNAMES(:)
      CHARACTER(LEN=12)       :: FORMAT
#ifdef W3_DIST
      CHARACTER(LEN=18)       :: TFILE
#endif
#ifdef W3_MPRF
      CHARACTER(LEN=18)       :: PFILE
#endif
      CHARACTER(LEN=13)       :: IDFLDS(-7:9)
      CHARACTER(LEN=23)       :: DTME21
      CHARACTER(LEN=30)       :: IDOTYP(8)
      CHARACTER(LEN=80)       :: TNAME, LINE
      CHARACTER(LEN=1024)     :: FLDOUT
!

!/
!/ ------------------------------------------------------------------- /
!/

      DATA IDFLDS / 'ice param. 1 ' , 'ice param. 2 ' ,               &
                    'ice param. 3 ' , 'ice param. 4 ' ,               &
                    'ice param. 5 ' ,                                 &
                    'mud density  ' , 'mud thkness  ' ,               &
                    'mud viscos.  ' ,                                 &
                    'water levels ' , 'currents     ' ,               &
                    'winds        ' , 'ice fields   ' ,               &
                    'momentum     ' , 'air density  ' ,               &
                    'mean param.  ' , '1D spectra   ' ,               &
                    '2D spectra   ' /
!
      DATA IDOTYP / 'Fields of mean wave parameters' ,                &
                    'Point output                  ' ,                &
                    'Track point output            ' ,                &
                    'Restart files                 ' ,                &
                    'Nesting data                  ' ,                &
                    'Separated wave field data     ' ,                &
                    'Fields for coupling           ' ,                &
                    'Restart files second request  '/
!
      DATA IDSTR  / 'IC1', 'IC2', 'IC3', 'IC4', 'IC5',                &
                    'MDN', 'MTH', 'MVS', 'LEV', 'CUR',                &
                    'WND', 'ICE', 'TAU', 'RHO', 'DT0',                &
                    'DT1', 'DT2' /
!
      DATA YESXX  / 'YES/--' /
      DATA XXXNO  / '---/NO' /
!
#ifdef W3_MPRF
      CALL PRINIT
      CALL PRTIME ( PRFT0 )
#endif
!
      CALL DATE_AND_TIME ( VALUES=CLKDT1 )
!
      MPI_COMM_LOC   = MPI_COMM
#ifdef W3_MPI
      MPI_COMM_MWAVE = MPI_COMM
      CALL MPI_COMM_SIZE ( MPI_COMM_MWAVE, NMPROC, IERR_MPI )
      CALL MPI_COMM_RANK ( MPI_COMM_MWAVE, IMPROC, IERR_MPI )
      IMPROC = IMPROC + 1
#endif
!
      IF ( PRESENT(PREAMB) ) FNMPRE = PREAMB
!/
!/ ------------------------------------------------------------------- /
! 1.  Multi-grid model intializations
! 1.a Unit numbers
!     Initialize dynamic assignment, errors and test to stdout
!
      CALL WMUINI ( 6, 6 )
!
! ... Identify reserved unit numbers
!
      CALL WMUSET ( 6,6,  5, .TRUE., 'SYS', 'stdin', 'Standart input' )
      CALL WMUSET ( 6,6,  6, .TRUE., 'SYS', 'stdout','Standart output')
!
#ifdef W3_NL2
      CALL WMUSET (6,6,103, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,104, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,105, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,106, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,107, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,108, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,109, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,110, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,111, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,112, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,113, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,114, .TRUE., 'FIX', DESC='Reserved SNL2' )
      CALL WMUSET (6,6,117, .TRUE., 'FIX', DESC='Reserved SNL2' )
#endif
!
! ... Unit numbers from parameter list
!     Dynamic scripture updated per file
!
      MDSI   = IDSI
      MDSO   = IDSO
      MDSS   = IDSS
      MDST   = IDST
      MDSE   = IDSE
!
      COMSTR = '$'
!
      IF ( IMPROC .EQ. NMPERR ) THEN
          MDSE2  = MDSE
        ELSE
          MDSE2  = -1
        END IF
!
! 1.b Subroutine tracing
!
      CALL ITRACE ( MDST, NTRMAX )
!
#ifdef W3_O10
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,900)
#endif
!
! 1.c Input file
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                      &
          WRITE (MDSS,910)  IFNAME, MDSI
!
      ! process ww3_multi namelist input
      CALL W3NMLMULTIDEF (MPI_COMM, MDSI, TRIM(FNMPRE)//IFNAME, NML_DOMAIN, IERR)
      ALLOCATE(NML_INPUT_GRID(NML_DOMAIN%NRINP))
      ALLOCATE(NML_MODEL_GRID(NML_DOMAIN%NRGRD)) 
      ALLOCATE(NML_OUTPUT_TYPE(NML_DOMAIN%NRGRD))
      ALLOCATE(NML_OUTPUT_DATE(NML_DOMAIN%NRGRD))
!
      CALL W3NMLMULTICONF (MPI_COMM, MDSI, TRIM(FNMPRE)//IFNAME, &
                          NML_DOMAIN, NML_INPUT_GRID, NML_MODEL_GRID, NML_OUTPUT_TYPE, &
                          NML_OUTPUT_DATE, NML_HOMOG_COUNT, NML_HOMOG_INPUT, IERR) 
      IF (IERR.NE.0) THEN
        WRITE (*,'(2A)') 'ERROR: error occured while processing ', IFNAME
        CALL EXIT (IERR)
      END IF


      CALL WMUSET ( MDSS, MDSS, MDSI, .TRUE., 'INP',                  &
                    TRIM(FNMPRE)//IFNAME, 'Model control input file')
!
! 1.d Log and test files
!
      LFILE  = 'log.mww3'
      IW     = 1 + INT ( LOG10 ( REAL(NMPROC) + 0.5 ) )
      IW     = MAX ( 3 , MIN ( 9 , IW ) ) 
      WRITE (FORMAT,'(A5,I1.1,A1,I1.1,A4)') '(A4,I',IW,'.',IW,',A5)'
#ifdef W3_SHRD
       TFILE  = 'test.mww3'
#endif
#ifdef W3_DIST
       WRITE (TFILE,FORMAT) 'test', IMPROC, '.mww3'
#endif
#ifdef W3_MPRF
       WRITE (PFILE,FORMAT) 'prf.', IMPROC, '.mww3'
#endif
!
      IF ( IMPROC .EQ. NMPLOG ) THEN
          OPEN (MDSO,FILE=TRIM(FNMPRE)//LFILE,ERR=2010,IOSTAT=IERR)
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,911)  LFILE, MDSO
          CALL WMUSET ( MDSS, MDSS, MDSO, .TRUE., 'OUT',              &
                        TRIM(FNMPRE)//LFILE, 'Log file')
        ELSE
          CALL WMUSET ( MDSS, MDSS, MDSO, .TRUE., 'XXX',              &
                       'Log file on other processors')
        END IF
!
      IF ( MDST.NE.MDSO .AND. MDST.NE.MDSS .AND. TSTOUT ) THEN
          IFT    = LEN_TRIM(TFILE)
          OPEN (MDST,FILE=TRIM(FNMPRE)//TFILE(:IFT),ERR=2011,IOSTAT=IERR)
          CALL WMUSET ( MDSS, MDST, MDST, .TRUE., 'OUT',              &
                        TRIM(FNMPRE)//TFILE(:IFT), 'Test output file')
        END IF
!
#ifdef W3_MPRF
      IFT    = LEN_TRIM(PFILE)
      CALL WMUGET ( MDSS, MDST, MDSP, 'OUT' )
      CALL WMUSET ( MDSS, MDST, MDSP, .TRUE., 'OUT',            &
                    TRIM(FNMPRE)//PFILE(:IFT), 'Profiling file')
      OPEN (MDSP,FILE=TRIM(FNMPRE)//PFILE(:IFT),ERR=2011,IOSTAT=IERR)
#endif
!
! 1.e Initial and test output
!
#ifdef W3_S
      CALL STRACE (IENT, 'WMINITNML')
#endif
!
      IF ( IMPROC .EQ. NMPLOG ) THEN
          CALL WWDATE ( STDATE )
          CALL WWTIME ( STTIME )
          WRITE (MDSO,901) WWVER, STDATE, STTIME
        END IF
!
#ifdef W3_T
      WRITE(MDST,9000) IDSI, IDSO, IDSS, IDST, IDSE, IFNAME
#endif
!
! 2.  Set-up of data structures and I/O  ----------------------------- /
! 2.a Get number of grids
!     Note: grid for consolidated point output always generated.
!     Processor set as in W3INIT to minimize communication in WMIOPO
!
      NRINP = NML_DOMAIN%NRINP
      NRGRD = NML_DOMAIN%NRGRD
      UNIPTS = NML_DOMAIN%UNIPTS
      IOSTYP = NML_DOMAIN%IOSTYP
      UPPROC = NML_DOMAIN%UPPROC
      PSHARE = NML_DOMAIN%PSHARE

      IOSTYP = MAX ( 0 , MIN ( 3 , IOSTYP ) )
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
          WRITE (MDSS,920) NRGRD
          IF ( NRINP .EQ. 0 ) THEN
              WRITE (MDSS,921)
            ELSE
              WRITE (MDSS,922) NRINP
            END IF
          IF ( UNIPTS ) THEN
              WRITE (MDSS,923) YESXX
            ELSE
              WRITE (MDSS,923) XXXNO
            END IF
          WRITE (MDSS,1923) IOSTYP
          IF ( UNIPTS ) THEN
              IF ( UPPROC ) THEN
                  WRITE (MDSS,2923) YESXX
                ELSE
                  WRITE (MDSS,2923) XXXNO
                END IF
            END IF
          IF ( IOSTYP.GT.1 .AND. PSHARE ) THEN
              WRITE (MDSS,3923) YESXX
            ELSE IF ( IOSTYP.GT. 1 ) THEN
              WRITE (MDSS,3923) XXXNO
            END IF
        END IF
!
      IF ( NRGRD .LT. 1 ) GOTO 2020
      IF ( NRINP .LT. 0 ) GOTO 2021
      IF ( NRINP.EQ.0 .AND. .NOT.UNIPTS ) NRINP = -1
!
! 2.b Set up data structures
!
      CALL W3NMOD ( NRGRD, MDSE2, MDST, NRINP )
      CALL W3NDAT (        MDSE2, MDST )
      CALL W3NAUX (        MDSE2, MDST )
      CALL W3NOUT (        MDSE2, MDST )
      CALL W3NINP (        MDSE2, MDST ) 
      CALL WMNDAT (        MDSE2, MDST ) 
!
! 2.c Set up I/O for individual models (initial)
!
      ALLOCATE ( MDS(13,NRGRD), NTRACE(2,NRGRD), ODAT(40,0:NRGRD),    &
           FLGRD(NOGRP,NGRPP,NRGRD), OT2(0:NRGRD), FLGD(NOGRP,NRGRD), &
           MDSF(-NRINP:NRGRD,JFIRST:9), IPRT(6,NRGRD), LPRT(NRGRD),   &
           FLGR2(NOGRP,NGRPP,NRGRD),FLG2D(NOGRP,NGRPP), FLG1D(NOGRP), &
           FLG2(NOGRP,NRGRD)                                          &
           ,OUTFF(7,0:NRGRD))
!
      MDS    = -1
      MDSF   = -1
      FLGR2  = .FALSE.
      IPRT   = 0
!
! ... Fixed and recycleable unit numbers.      
!
      CALL WMUGET ( MDSE, MDST, NDSREC, 'INP' )
      CALL WMUSET ( MDSE, MDST, NDSREC, .TRUE., 'I/O', NAME='...',    &
                    DESC='Recyclable I/O (mod_def etc.)' )
      CALL WMUGET ( MDSE, MDST, SCRATCH, 'SCR' )
      CALL WMUSET ( MDSE, MDST, SCRATCH, .TRUE., DESC='Scratch file', &
                    NAME=TRIM(FNMPRE)//'ww3_multi.scratch' )
!
      IF(MDST.EQ.NDSREC)THEN
         IF   ( IMPROC .EQ. NMPERR )                                  &
            WRITE(MDSE,'(A,I8)')'RECYCLABLE UNIT NUMBERS AND '&
            //'TEST OUTPUT UNIT NUMBER ARE THE SAME : ',MDST
         CALL EXTCDE ( 15 )
      ENDIF

      DO I=1, NRGRD
        MDS   ( 2,I) =  6
        MDS   ( 3,I) = MDST
        MDS   ( 4,I) =  6
        MDS   ( 5,I) = NDSREC
        MDS   ( 6,I) = NDSREC
        NTRACE( 1,I) = MDST
        NTRACE( 2,I) = NTRMAX
        END DO
!
#ifdef W3_T
      WRITE (MDST,9020) 'INITIAL'
      DO I=1, NRGRD
        WRITE (MDST,9021) I, MDS(:,I), NTRACE(:,I)
        END DO
#endif
!
! 3.  Get individual grid information -------------------------------- /
!
!     Version 3.07: For now we simply read the input data flags,
!                   skip the homogeneous option. Later on, we want
!                   to have the options to use input from common
!                   sources, and from communication rather than
!                   files.
!
      ALLOCATE ( INAMES(2*NRGRD,-7:9), MNAMES(-NRINP:2*NRGRD),     &
                 TMPRNK(2*NRGRD), TMPGRP(2*NRGRD), NINGRP(2*NRGRD),    &
                 RP1(2*NRGRD), RPN(2*NRGRD), BCDTMP(NRGRD+1:2*NRGRD))
      ALLOCATE ( GRANK(NRGRD), GRGRP(NRGRD), USEINP(NRINP) )
      ALLOCATE ( CPLINP(NRINP) )
      GRANK  = -1
      GRGRP  = -1
      USEINP = .FALSE.
      CPLINP = .FALSE.
!
! 3.a Read data
!
#ifdef W3_T
      WRITE (MDST,9030)
#endif
!
! 3.a.1 Input grids
!
      DO I=1, NRINP
!
        CALL W3SETI ( -I, MDSE, MDST )
        INFLAGS1 = .FALSE.
        MNAMES(-I) = NML_INPUT_GRID(I)%NAME 
        INFLAGS1(-7) = NML_INPUT_GRID(I)%FORCING%ICE_PARAM1
        INFLAGS1(-6) = NML_INPUT_GRID(I)%FORCING%ICE_PARAM2
        INFLAGS1(-5) = NML_INPUT_GRID(I)%FORCING%ICE_PARAM3
        INFLAGS1(-4) = NML_INPUT_GRID(I)%FORCING%ICE_PARAM4
        INFLAGS1(-3) = NML_INPUT_GRID(I)%FORCING%ICE_PARAM5
        INFLAGS1(-2) = NML_INPUT_GRID(I)%FORCING%MUD_DENSITY
        INFLAGS1(-1) = NML_INPUT_GRID(I)%FORCING%MUD_THICKNESS
        INFLAGS1(0) = NML_INPUT_GRID(I)%FORCING%MUD_VISCOSITY
        INFLAGS1(1) = NML_INPUT_GRID(I)%FORCING%WATER_LEVELS
        INFLAGS1(2) = NML_INPUT_GRID(I)%FORCING%CURRENTS
        INFLAGS1(3) = NML_INPUT_GRID(I)%FORCING%WINDS
        INFLAGS1(4) = NML_INPUT_GRID(I)%FORCING%ICE_CONC
        INFLAGS1(5) = NML_INPUT_GRID(I)%FORCING%ATM_MOMENTUM
        INFLAGS1(6) = NML_INPUT_GRID(I)%FORCING%AIR_DENSITY
        INFLAGS1(7) = NML_INPUT_GRID(I)%ASSIM%MEAN
        INFLAGS1(8) = NML_INPUT_GRID(I)%ASSIM%SPEC1D
        INFLAGS1(9) = NML_INPUT_GRID(I)%ASSIM%SPEC2D
      END DO
!
! 3.a.2 Unified point output grid.
!
      IF ( UNIPTS ) THEN
!
          CALL W3SETI ( 0, MDSE, MDST )
          CALL W3SETO ( 0, MDSE, MDST )
          INFLAGS1 = .FALSE.
          NDST   = MDST
          NDSE   = MDSE
!
          MNAMES(0) = NML_OUTPUT_TYPE(1)%POINT%NAME
!
          IF ( IOSTYP .LE. 1 ) THEN
              NMPUPT = MAX(1,NMPROC-2)
            ELSE
              NMPUPT = NMPROC
            END IF
!
        END IF
!
! 3.a.3 Read wave grids
!
      DO I=1,NRGRD
        MNAMES(NRGRD+I) = NML_MODEL_GRID(I)%NAME
        INAMES(NRGRD+I,-7) = NML_MODEL_GRID(I)%FORCING%ICE_PARAM1
        INAMES(NRGRD+I,-6) = NML_MODEL_GRID(I)%FORCING%ICE_PARAM2
        INAMES(NRGRD+I,-5) = NML_MODEL_GRID(I)%FORCING%ICE_PARAM3
        INAMES(NRGRD+I,-4) = NML_MODEL_GRID(I)%FORCING%ICE_PARAM4
        INAMES(NRGRD+I,-3) = NML_MODEL_GRID(I)%FORCING%ICE_PARAM5
        INAMES(NRGRD+I,-2) = NML_MODEL_GRID(I)%FORCING%MUD_DENSITY
        INAMES(NRGRD+I,-1) = NML_MODEL_GRID(I)%FORCING%MUD_THICKNESS
        INAMES(NRGRD+I,0) = NML_MODEL_GRID(I)%FORCING%MUD_VISCOSITY
        INAMES(NRGRD+I,1) = NML_MODEL_GRID(I)%FORCING%WATER_LEVELS
        INAMES(NRGRD+I,2) = NML_MODEL_GRID(I)%FORCING%CURRENTS
        INAMES(NRGRD+I,3) = NML_MODEL_GRID(I)%FORCING%WINDS
        INAMES(NRGRD+I,4) = NML_MODEL_GRID(I)%FORCING%ICE_CONC
        INAMES(NRGRD+I,5) = NML_MODEL_GRID(I)%FORCING%ATM_MOMENTUM
        INAMES(NRGRD+I,6) = NML_MODEL_GRID(I)%FORCING%AIR_DENSITY
        INAMES(NRGRD+I,7) = NML_MODEL_GRID(I)%ASSIM%MEAN
        INAMES(NRGRD+I,8) = NML_MODEL_GRID(I)%ASSIM%SPEC1D
        INAMES(NRGRD+I,9) = NML_MODEL_GRID(I)%ASSIM%SPEC2D
        TMPRNK(NRGRD+I) = NML_MODEL_GRID(I)%RESOURCE%RANK_ID
        TMPGRP(NRGRD+I) = NML_MODEL_GRID(I)%RESOURCE%GROUP_ID
        RP1(NRGRD+I) = NML_MODEL_GRID(I)%RESOURCE%COMM_FRAC(1)
        RPN(NRGRD+I) = NML_MODEL_GRID(I)%RESOURCE%COMM_FRAC(2)
        BCDTMP(NRGRD+I) = NML_MODEL_GRID(I)%RESOURCE%BOUND_FLAG
!
        RP1(NRGRD+I) = MAX ( 0. , MIN ( 1. , RP1(NRGRD+I) ) )
        RPN(NRGRD+I) = MAX ( RP1(NRGRD+I) , MIN ( 1. , RPN(NRGRD+I) ) )
        END DO
!
! 3.a.4 Sort wave grids
!
      RNKTMP = MINVAL ( TMPRNK(NRGRD+1:2*NRGRD) )
      I      = 0
!
      DO
        DO J=NRGRD+1, 2*NRGRD
          IF ( TMPRNK(J) .EQ. RNKTMP ) THEN
              I      = I + 1
              CALL W3SETI ( I, MDSE, MDST )
              INFLAGS1      = .FALSE.
#ifdef W3_MGW
              INFLAGS1(10)   = .TRUE.
#endif
#ifdef W3_MGP
              INFLAGS1(10)   = .TRUE.
#endif
              INAMES(I,:)= INAMES(J,:)
              MNAMES(I)  = MNAMES(J)
              TMPRNK(I)  = TMPRNK(J)
              TMPGRP(I)  = TMPGRP(J)
              RP1(I)     = RP1(J)
              RPN(I)     = RPN(J)
              BCDUMP(I)  = BCDTMP(J)
#ifdef W3_T
              WRITE (MDST,9031) I, MNAMES(I), INFLAGS1, TMPRNK(I),    &
                                   TMPGRP(I), RP1(I), RPN(I)
#endif
            END IF
          END DO
        IF ( I .EQ. NRGRD ) EXIT
        RNKTMP = RNKTMP + 1
        END DO
!
! 3.a.5 Set input flags
!
      ALLOCATE ( INPMAP(NRGRD,JFIRST:10), IDINP(-NRINP:NRGRD,JFIRST:10) )
      INPMAP = 0
      IDINP  = '---'
!
      DO I=1, NRGRD
         CALL W3SETI ( I, MDSE, MDST )
         DO J=JFIRST, 9
            IF ( INAMES(I,J) .EQ. 'native' ) THEN
            ! *** forcing input from file & defined on the native grid ***
                INFLAGS1(J) = .TRUE.
              ELSE
                INFLAGS1(J) = .FALSE.
                IF ( INAMES(I,J)(1:4) .EQ. 'CPL:' ) THEN
                    IF ( INAMES(I,J)(5:) .EQ. 'native' ) THEN
                      ! *** forcing input from CPL & defined on the native grid ***
                        INFLAGS1(J) = .TRUE.
                        INPMAP(I,J) = -999
                      ELSE
                      ! *** forcing input from CPL & defined on an input grid ***
                        DO JJ=1, NRINP
                          IF ( MNAMES(-JJ) .EQ. INAMES(I,J)(5:) ) THEN
                              INPMAP(I,J) = -JJ
                              EXIT
                            END IF
                          END DO
                        IF ( INPMAP(I,J) .EQ. 0 ) GOTO 2030
                        IF ( .NOT. INPUTS(INPMAP(I,J))%INFLAGS1(J) ) GOTO 2031
                        USEINP(-INPMAP(I,J)) = .TRUE.
                        CPLINP(-INPMAP(I,J)) = .TRUE.
                      END IF
                  ELSE IF ( INAMES(I,J) .NE. 'no' ) THEN
                    ! *** forcing input from file & defined on an input grid ***
                    DO JJ=1, NRINP
                       IF ( MNAMES(-JJ) .EQ. INAMES(I,J) ) THEN
                          INPMAP(I,J) = JJ
                          INFLAGS2(J) = .TRUE.
                          EXIT
                       END IF
                    END DO
                    IF ( INPMAP(I,J) .EQ. 0 ) GOTO 2030
                    IF ( .NOT. INPUTS(-INPMAP(I,J))%INFLAGS1(J) ) GOTO 2031
                    USEINP(INPMAP(I,J)) = .TRUE.
                  END IF
              END IF
!        INFLAGS2 is initial value of INFLAGS1. Unlike INFLAGS1, 
!           it does not change during the simulation
         IF(.NOT. INFLAGS2(J)) INFLAGS2(J)=INFLAGS1(J)
         END DO !         DO J=JFIRST, 9
      END DO !      DO I=1, NRGRD
!
      DO I=1, NRINP
        IF ( .NOT.USEINP(I) .AND.                                     &
             MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
            II     = LEN_TRIM(MNAMES(-I))
            WRITE (MDSE,1032) MNAMES(-I)(1:II)
          END IF
        END DO
!
! 3.b Assign input file unit numbers
!
      DO I=-NRINP, NRGRD
        IF ( I .EQ. 0 ) CYCLE
        CALL W3SETI ( I, MDSE, MDST )
        DO J=JFIRST, 9
          IF ( I .GE. 1 ) THEN
              IF ( INPMAP(I,J) .LT. 0 ) CYCLE
            END IF
          IF ( INFLAGS1(J) ) THEN
              CALL WMUGET ( MDSE, MDST, NDSFND, 'INP' )
              CALL WMUSET ( MDSE, MDST, NDSFND, .TRUE.,               &
                            DESC='Input data file' )
              MDSF(I,J) = NDSFND
            END IF
          END DO
        END DO
!
#ifdef W3_T
      WRITE (MDST,9022)
      DO I=-NRINP, NRGRD
        IF ( I .EQ. 0 ) CYCLE
        WRITE (MDST,9021) I, MDSF(I,JFIRST:9)
        END DO
#endif
!
! 3.c Set rank and group data
!
#ifdef W3_T
      WRITE (MDST,9032)
#endif
!
      RNKMAX = MAXVAL ( TMPRNK(1:NRGRD) ) + 1
      RNKTMP = 0
!
      DO
        RNKMIN = MINVAL ( TMPRNK(1:NRGRD) )
        IF ( RNKMIN .EQ. RNKMAX ) EXIT
        RNKTMP = RNKTMP + 1
        DO I=1, NRGRD
          IF ( TMPRNK(I) .EQ. RNKMIN ) THEN
              GRANK(I)  = RNKTMP
              TMPRNK(I) = RNKMAX
            END IF
          END DO
        END DO
!
#ifdef W3_T
      DO I=1, NRGRD
        WRITE (MDST,9033) I, MNAMES(I), GRANK(I)
        END DO
#endif
!
      RNKMAX = RNKTMP
      GRPMAX = MAXVAL ( TMPGRP(1:NRGRD) ) + 1
      NRGRP  = 0
      NINGRP = 0
!
      DO RNKTMP=1, RNKMAX
        DO
          GRPMIN = GRPMAX
          DO I=1, NRGRD
            IF ( GRANK(I) .EQ. RNKTMP )                               &
                 GRPMIN = MIN ( GRPMIN , TMPGRP(I) )
            END DO
          IF ( GRPMIN .EQ. GRPMAX ) EXIT
          NRGRP  = NRGRP + 1
          DO I=1, NRGRD
            IF ( GRANK(I).EQ.RNKTMP .AND. GRPMIN.EQ.TMPGRP(I) ) THEN
                GRGRP(I)  = NRGRP
                TMPGRP(I) = GRPMAX 
                NINGRP(NRGRP) = NINGRP(NRGRP) + 1
              END IF
            END DO
          END DO
        END DO
!
#ifdef W3_T
      WRITE (MDST,9034) NRGRP
      DO I=1, NRGRD
        WRITE (MDST,9033) I, MNAMES(I), GRGRP(I)
        END DO
      WRITE (MDST,9035) NINGRP(1:NRGRP)
#endif
!
      ALLOCATE ( ACTION(JFIRST:11) )
      ALLOCATE ( INGRP(NRGRP,0:MAXVAL(NINGRP(:NRGRP))) )
      DEALLOCATE ( TMPRNK, TMPGRP, NINGRP, BCDTMP )
      INGRP = 0
!
      DO I=1, NRGRD
        INGRP(GRGRP(I),0) = INGRP(GRGRP(I),0) + 1
        INGRP(GRGRP(I),INGRP(GRGRP(I),0)) = I
        END DO
!
#ifdef W3_T
      WRITE (MDST,9036)
      DO J=1, NRGRP
        WRITE (MDST,9037) J, INGRP(J,:INGRP(J,0))
        END DO
#endif
!
!
! 3.d Unified point output
!
#ifdef W3_MPRF
      CALL PRTIME ( PRFTN )
      WRITE (MDSP,990) PRFT0, PRFTN, get_memory(), 'START Sec. 8.b'
      PRFT0  = PRFTN
#endif
!
      IF ( UNIPTS ) THEN
!
          J      = LEN_TRIM(MNAMES(0))
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
              WRITE (MDSS,986) MNAMES(0)(1:J)
              WRITE (MDSS,987)
            END IF
!
          CALL W3IOGR ( 'GRID', NDSREC, 0, MNAMES(0)(1:J) )
!
        END IF
!
! 3.e Output
!
      IF ( NRINP .GT. 0 ) THEN
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,924)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,924)
          DO I=1, NRINP
            IF ( .NOT. USEINP(I) ) CYCLE
            CALL W3SETI ( -I, MDSE, MDST )
            ACTION(1:6) = '---   '
            DO J=JFIRST, 6
              IF ( INFLAGS1(J) ) ACTION(J) = ' X    '
              END DO
            ACTION(7:9) = '-     '
            IF ( INFLAGS1(7) ) ACTION(7) = '1     '
            IF ( INFLAGS1(8) ) ACTION(8) = '2     '
            IF ( INFLAGS1(9) ) ACTION(9) = '3     '
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                WRITE (MDSS,925) I, MNAMES(-I), ACTION(JFIRST:9)
            IF ( NMPLOG .EQ. IMPROC )                                 &
                WRITE (MDSO,925) I, MNAMES(-I), ACTION(JFIRST:9)
            END DO
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,926)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,926)
        END IF
!
      IF ( UNIPTS ) THEN
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,927)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,927)
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
                WRITE (MDSS,928) MNAMES(0)
          IF ( NMPLOG .EQ. IMPROC )                                   &
                WRITE (MDSO,928) MNAMES(0)
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,929)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,929)
        END IF
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,930)
      IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,930)
      DO I=1, NRGRD
        CALL W3SETI ( I, MDSE, MDST )
        ACTION(1:6) = '---   '
        DO J=JFIRST, 6
          IF ( INFLAGS1(J) .AND. INPMAP(I,J) .EQ. 0 ) THEN
              ACTION(J) = 'native'
            ELSE IF ( INFLAGS1(J) .AND. INPMAP(I,J) .EQ. -999 ) THEN
              ACTION(J) = 'native'
            ELSE IF ( INPMAP(I,J) .GT. 0 ) THEN
              ACTION(J) = MNAMES(-INPMAP(I,J))
            ELSE IF ( INPMAP(I,J) .LT. 0 ) THEN
              ACTION(J) = MNAMES( INPMAP(I,J))
            END IF
          END DO
        ACTION(7:11) = '-     '
        IF ( INFLAGS1(7) ) ACTION(7) = '1     '
        IF ( INFLAGS1(8) ) ACTION(8) = '2     '
        IF ( INFLAGS1(9) ) ACTION(9) = '3     '
        IF ( INFLAGS1(10) ) THEN
            ACTION(10) = 'yes   '
          ELSE
            ACTION(10) = 'no    '
          END IF
        IF ( BCDUMP(I) ) ACTION(11) = 'y     '
        IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                    &
            WRITE (MDSS,931) I, MNAMES(I), ACTION(1:10), GRANK(I),     &
                             GRGRP(I), ACTION(11)
        IF ( NMPLOG .EQ. IMPROC )                                     &
            WRITE (MDSO,931) I, MNAMES(I), ACTION(1:10), GRANK(I),     &
                             GRGRP(I), ACTION(11)
      END DO
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,932)
      IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,932)
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                      &
          WRITE (MDSS,933) 'Group information'
      IF ( NMPLOG .EQ. IMPROC )                                       &
          WRITE (MDSO,933) 'Group information'
      DO J=1, NRGRP
        WRITE (LINE(1:6),'(1X,I3,2X)') J
        JJJ    = 6
        DO JJ=1, INGRP(J,0)
          IF ( JJJ .GT. 60 ) THEN
              IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )              &
                                        WRITE (MDSS,934) LINE(1:JJJ)
              IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,934) LINE(1:JJJ)
              LINE(1:6) = '      '
              JJJ       = 6
            END IF 
          WRITE (LINE(JJJ+1:JJJ+3),'(I3)') INGRP(J,JJ)
!
          LINE(JJJ+4:JJJ+5) = ' ('
          WRITE (LINE(JJJ+6:JJJ+11),'(F6.4)') RP1(INGRP(J,JJ))
          LINE(JJJ+12:JJJ+12) = '-'
          WRITE (LINE(JJJ+13:JJJ+18),'(F6.4)') RPN(INGRP(J,JJ))
          LINE(JJJ+19:JJJ+19) = ')'
          JJJ    = JJJ + 19
!
          END DO
        IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                    &
                                  WRITE (MDSS,934) LINE(1:JJJ)
        IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,934) LINE(1:JJJ)
        END DO
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,935)
      IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,935)
!
! 4.  Model run time information etc. -------------------------------- /
!
!     Version 3.07: Same for all grids, diversify later ....
!     If invoked as ESMF Component, then STIME and ETIME are set
!     in WMESMFMD from the external clock.
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,940)
!              
      IF (IS_ESMF_COMPONENT) THEN
          READ(NML_DOMAIN%START, *) STMPT 
          READ(NML_DOMAIN%STOP, *) ETMPT
        ELSE
          READ(NML_DOMAIN%START, *) STIME 
          READ(NML_DOMAIN%STOP, *) ETIME
        END IF
      CALL STME21 ( STIME , DTME21 )
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,941) DTME21
      CALL STME21 ( ETIME , DTME21 )
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,942) DTME21
!
      DO I=1, NRGRD
        CALL W3SETW ( I, MDSE, MDST )
        TIME   = STIME
        END DO 
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,943)
!              
      FLGHG1 = NML_DOMAIN%FLGHG1
      FLGHG2 = NML_DOMAIN%FLGHG2
      FLGHG2 = FLGHG1 .AND. FLGHG2
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
          IF ( FLGHG1 ) THEN
              WRITE (MDSS,944) YESXX
            ELSE
              WRITE (MDSS,944) XXXNO
            END IF
          IF ( FLGHG2 ) THEN
              WRITE (MDSS,945) YESXX
            ELSE
              WRITE (MDSS,945) XXXNO
            END IF
        END IF
!
! 5.  Output requests ------------------------------------------------ /
!
      OT2(:)%NPTS = 0
      ILOOP = 0
!
! 5.a Loop over types
!
      DO I=1, NRGRD
        IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,950) TRIM(MNAMES(NRGRD+I))
        NOTYPE = 6
!!/COU      NOTYPE = 7

        READ(NML_OUTPUT_DATE(I)%FIELD%START, *)   ODAT(1,I), ODAT(2,I)
        READ(NML_OUTPUT_DATE(I)%FIELD%STRIDE, *)  ODAT(3,I)
        READ(NML_OUTPUT_DATE(I)%FIELD%STOP, *)    ODAT(4,I), ODAT(5,I)
        READ(NML_OUTPUT_DATE(I)%FIELD%OUTFFILE, *)  OUTFF(1,I)
        READ(NML_OUTPUT_DATE(I)%POINT%START, *)   ODAT(6,I), ODAT(7,I)
        READ(NML_OUTPUT_DATE(I)%POINT%STRIDE, *)  ODAT(8,I)
        READ(NML_OUTPUT_DATE(I)%POINT%STOP, *)    ODAT(9,I), ODAT(10,I)
        READ(NML_OUTPUT_DATE(I)%POINT%OUTFFILE, *)  OUTFF(2,I)
        READ(NML_OUTPUT_DATE(I)%TRACK%START, *)   ODAT(11,I), ODAT(12,I)
        READ(NML_OUTPUT_DATE(I)%TRACK%STRIDE, *)  ODAT(13,I)
        READ(NML_OUTPUT_DATE(I)%TRACK%STOP, *)    ODAT(14,I), ODAT(15,I)
        READ(NML_OUTPUT_DATE(I)%RESTART%START, *)   ODAT(16,I), ODAT(17,I)
        READ(NML_OUTPUT_DATE(I)%RESTART%STRIDE, *)  ODAT(18,I)
        READ(NML_OUTPUT_DATE(I)%RESTART%STOP, *)    ODAT(19,I), ODAT(20,I)
        READ(NML_OUTPUT_DATE(I)%RESTART2%START, *)   ODAT(36,I), ODAT(37,I)
        READ(NML_OUTPUT_DATE(I)%RESTART2%STRIDE, *)  ODAT(38,I)
        READ(NML_OUTPUT_DATE(I)%RESTART2%STOP, *)    ODAT(39,I), ODAT(40,I)
        READ(NML_OUTPUT_DATE(I)%BOUNDARY%START, *)   ODAT(21,I), ODAT(22,I)
        READ(NML_OUTPUT_DATE(I)%BOUNDARY%STRIDE, *)  ODAT(23,I)
        READ(NML_OUTPUT_DATE(I)%BOUNDARY%STOP, *)    ODAT(24,I), ODAT(25,I)
        READ(NML_OUTPUT_DATE(I)%PARTITION%START, *)   ODAT(26,I), ODAT(27,I)
        READ(NML_OUTPUT_DATE(I)%PARTITION%STRIDE, *)  ODAT(28,I)
        READ(NML_OUTPUT_DATE(I)%PARTITION%STOP, *)    ODAT(29,I), ODAT(30,I)
!!/COU! NOT YET IMPLEMENTED
!!/COU        READ(NML_OUTPUT_DATE(I)%COUPLING%START, *)   ODAT(31,I), ODAT(32,I)
!!/COU        READ(NML_OUTPUT_DATE(I)%COUPLING%STRIDE, *)  ODAT(33,I)
!!/COU        READ(NML_OUTPUT_DATE(I)%COUPLING%STOP, *)    ODAT(34,I), ODAT(35,I)

        ! set the time stride at 0 or more
        ODAT(3,I) = MAX ( 0 , ODAT(3,I) )
        ODAT(8,I) = MAX ( 0 , ODAT(8,I) )
        ODAT(13,I) = MAX ( 0 , ODAT(13,I) )
        ODAT(18,I) = MAX ( 0 , ODAT(18,I) )
        ODAT(23,I) = MAX ( 0 , ODAT(23,I) )
        ODAT(28,I) = MAX ( 0 , ODAT(28,I) )
!!/COU        ODAT(33,I) = MAX ( 0 , ODAT(33,I) )
        ODAT(38,I) = MAX ( 0 , ODAT(38,I) )

        ! define the time of the output point grid (index 0) as the   &
        ! time of the first grid which should be the larger one by convention
        ODAT(6:10,0) = ODAT(6:10,1)

        ! allocate pointers to minimum value if no output point
        IF ( ODAT(8,I) .EQ. 0 ) THEN
          ALLOCATE ( OT2(I)%X(1), OT2(I)%Y(1), OT2(I)%PNAMES(1) )
        END IF


        DO J=1, NOTYPE
!
! 5.b Process standard line
!
          OUTPTS(I)%OFILES(J)=OUTFF(J,I)              
          IF ( ODAT(5*(J-1)+3,I) .NE. 0 ) THEN
              IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                  WRITE (MDSS,951) J, IDOTYP(J)
              TTIME(1) = ODAT(5*(J-1)+1,I)
              TTIME(2) = ODAT(5*(J-1)+2,I)
              CALL STME21 ( TTIME , DTME21 )
              IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                   WRITE (MDSS,952) DTME21
              TTIME(1) = ODAT(5*(J-1)+4,I)
              TTIME(2) = ODAT(5*(J-1)+5,I)
              CALL STME21 ( TTIME , DTME21 )
              IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                   WRITE (MDSS,953) DTME21
              TTIME(1) = 0
              TTIME(2) = 0
              DTTST    = REAL ( ODAT(5*(J-1)+3,I) )
              CALL TICK21 ( TTIME , DTTST  )
              CALL STME21 ( TTIME , DTME21 )
              IF ( ( ODAT(5*(J-1)+1,I) .NE. ODAT(5*(J-1)+4,I) .OR.      &
                     ODAT(5*(J-1)+2,I) .NE. ODAT(5*(J-1)+5,I) ) .AND.   &
                     MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                  DO II=1, 18
                    IF ( DTME21(II:II).NE.'0' .AND.                     &
                         DTME21(II:II).NE.'/' .AND.                     &
                         DTME21(II:II).NE.' ' .AND.                     &
                         DTME21(II:II).NE.':' ) EXIT
                    DTME21(II:II) = ' '
                    END DO
                  WRITE (MDSS,954) DTME21(1:19)
                END IF
!
              IF ( J .EQ. 1 ) THEN
!
! 5.c Type 1: fields of mean wave parameters
!
                FLGRD(:,:,I)=.FALSE. ! Initialize FLGRD
                FLDOUT = NML_OUTPUT_TYPE(I)%FIELD%LIST
                CALL W3FLGRDFLAG ( MDSS, MDSO, MDSE2, FLDOUT, FLG1D,       &
                                   FLG2D, IMPROC, NMPSCR, IERR )
                FLGRD(:,:,I)=FLG2D
                FLGD(:,I)   =FLG1D
!
              ELSE IF ( J .EQ. 2 ) THEN
!
! 5.d Type 2: point output
!
                ! if the output is 0, the output is disabled
                IF (UNIPTS) THEN
                  IF ( ODAT(8,0).EQ.0 .AND. IMPROC.EQ.NMPERR ) WRITE (MDSE,1050)
                  IF ( ODAT(8,0).EQ.0 ) UNIPTS = .FALSE.
                END IF

                ! if the point file is not set
                IF ( TRIM(NML_OUTPUT_TYPE(I)%POINT%FILE).EQ.'unset' ) THEN
                  ! and if output also disabled, cycle to the next output type J
                  IF ( ODAT(8,I).EQ.0 ) THEN
                    ALLOCATE ( OT2(I)%X(1), OT2(I)%Y(1), OT2(I)%PNAMES(1) )
                    CYCLE
                  ! and if output still enabled, stop
                  ELSE
                    GOTO 2055
                  END IF
                END IF

                ! if the unified point is already defined, cycle to the next output type J
                IF ( UNIPTS .AND. ILOOP.NE.0 ) CYCLE
!
                IF ( UNIPTS .AND. I.GE.2 ) THEN
                  DO K=1,I-1
                    IF ( NML_OUTPUT_TYPE(K)%POINT%FILE.NE.NML_OUTPUT_TYPE(I)%POINT%FILE ) GOTO 2053
                  END DO
                END IF
                OPEN (MDSI, file=TRIM(FNMPRE)//TRIM(NML_OUTPUT_TYPE(I)%POINT%FILE), &
                      FORM='FORMATTED', STATUS='OLD', ERR=2104, IOSTAT=IERR)

                ! first loop to count the number of points
                ! second loop to allocate the array and store the points
                OT2(I)%NPTS = 0
                DO ILOOP=1,2
                  REWIND (MDSI)
!
                  IF ( ILOOP.EQ.2) THEN
                    IF ( OT2(I)%NPTS.GT.0 ) THEN
                      ALLOCATE ( OT2(I)%X(OT2(I)%NPTS),               &
                                 OT2(I)%Y(OT2(I)%NPTS),               &
                                 OT2(I)%PNAMES(OT2(I)%NPTS) )
                      OT2(I)%NPTS = 0  ! reset it to use it as a counter for loop 2
                    ELSE
                      ALLOCATE ( OT2(I)%X(1), OT2(I)%Y(1), OT2(I)%PNAMES(1) )
                      GOTO 2054
                    END IF
                  END IF
!
                  DO
                    READ (MDSI,*,ERR=2004,IOSTAT=IERR) TMPLINE
                    ! if end of file or stopstring, then exit
                    IF ( IERR.NE.0 .OR. INDEX(TMPLINE,"STOPSTRING").NE.0 ) EXIT
                    ! leading blanks removed and placed on the right
                    TEST = ADJUSTL ( TMPLINE )
                    IF ( TEST(1:1).EQ.COMSTR .OR. LEN_TRIM(TEST).EQ.0 ) THEN
                      ! if comment or blank line, then skip
                      CYCLE
                    ELSE
                      ! otherwise, backup to beginning of line
                      BACKSPACE ( MDSI, ERR=2004, IOSTAT=IERR)
                      READ (MDSI,*,ERR=2004,IOSTAT=IERR) XX, YY, PN
                    ENDIF
                    OT2(I)%NPTS = OT2(I)%NPTS + 1
                    IF ( ILOOP .EQ. 1 ) CYCLE
                    IF ( ILOOP .EQ. 2 ) THEN
                      OT2(I)%X(OT2(I)%NPTS)      = XX
                      OT2(I)%Y(OT2(I)%NPTS)      = YY
                      OT2(I)%PNAMES(OT2(I)%NPTS) = PN 
                      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                        IF ( OT2(I)%NPTS .EQ. 1 ) THEN
                          WRITE (MDSS,957)              XX, YY, PN
                        ELSE
                          WRITE (MDSS,958) OT2(I)%NPTS, XX, YY, PN
                        END IF
                      END IF
                    END IF ! ILOOP.EQ.2
                  END DO ! end of file                      
                END DO ! ILOOP
                CLOSE(MDSI)
!
                IF ( UNIPTS .AND. OT2(0)%NPTS.EQ.0 .AND. OT2(I)%NPTS.GT.0 ) THEN
                  ! copy points to point grid number 0
                  OT2(0)%NPTS = OT2(I)%NPTS
                  ALLOCATE (OT2(0)%X(OT2(0)%NPTS), OT2(0)%Y(OT2(0)%NPTS), OT2(0)%PNAMES(OT2(0)%NPTS))
                  OT2(0)%X(:)      = OT2(I)%X(:)
                  OT2(0)%Y(:)      = OT2(I)%Y(:)
                  OT2(0)%PNAMES(:) = OT2(I)%PNAMES(:)
                  ! define all the other grids to empty output point
                  DO K=1, NRGRD
                    OT2(K)%NPTS = 0
                    ALLOCATE (OT2(K)%X(1),OT2(K)%Y(1),OT2(K)%PNAMES(1))
                  END DO
                END IF
!
              ELSE IF ( J .EQ. 3 ) THEN
!
! 5.e Type 3: track output
!
                TFLAGI = NML_OUTPUT_TYPE(I)%TRACK%FORMAT
                IF ( TFLAGI ) THEN
                    MDS(11,I) =  ABS(MDS(11,I))
                  ELSE
                    MDS(11,I) = -ABS(MDS(11,I))
                  END IF
                IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                    IF ( .NOT. TFLAGI ) THEN
                        WRITE (MDSS,960) 'input', 'UNFORMATTED'
                      ELSE
                        WRITE (MDSS,960) 'input', 'FORMATTED'
                      END IF
                  END IF
!
              ELSE IF ( J .EQ. 4 ) THEN
!
! 5.f Type 4: restart files (no additional data)
!
              ELSE IF ( J .EQ. 5 ) THEN
!
! 5.g Type 5: nesting data (no additional data)
!
              ELSE IF ( J .EQ. 6 ) THEN
!
! 5.h Type 6: partitioned wave field data
!
                IPRT(1,I) = NML_OUTPUT_TYPE(I)%PARTITION%X0
                IPRT(2,I) = NML_OUTPUT_TYPE(I)%PARTITION%XN
                IPRT(3,I) = NML_OUTPUT_TYPE(I)%PARTITION%NX
                IPRT(4,I) = NML_OUTPUT_TYPE(I)%PARTITION%Y0
                IPRT(5,I) = NML_OUTPUT_TYPE(I)%PARTITION%YN
                IPRT(6,I) = NML_OUTPUT_TYPE(I)%PARTITION%NY
                LPRT(I) = NML_OUTPUT_TYPE(I)%PARTITION%FORMAT
                IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                    WRITE (MDSS,961) IPRT(:,I)
                    IF ( .NOT. LPRT(I) ) THEN
                        WRITE (MDSS,960) 'output', 'UNFORMATTED'
                      ELSE
                        WRITE (MDSS,960) 'output', 'FORMATTED'
                      END IF
                  END IF

!!/COU! NOT YET IMPLEMENTED
!
!!/COU              ELSE IF ( J .EQ. 7 ) THEN
!
! 5.i Type 7: coupling
!
!!/COU              FLDOUT = NML_OUTPUT_TYPE(I)%COUPLING%LIST
!!/COU              CALL W3FLGRDFLAG ( MDSS, MDSO, MDSE2, FLDOUT, FLG1D,       &
!!/COU                                 FLG2D, IMPROC, NMPSCR, IERR )
!!/COU              FLGR2(:,:,I)=FLG2D
!!/COU              FLG2(:,I)   =FLG1D
!
! ... End of output type selecttion ELSE IF
!
              END IF
!
! ... End of IF in 5.b
!
          END IF
!
! ... End of loop J on NOTYPE in 5.a
!
        END DO
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!  Checkpoint
        J=8
            !OUTPTS(I)%FLOUT(8)=.FALSE.
          IF ( ODAT(5*(J-1)+3,I) .NE. 0 ) THEN
             !OUTPTS(I)%FLOUT(8)=.TRUE.
              IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                  WRITE (MDSS,951) J, IDOTYP(J)
              TTIME(1) = ODAT(5*(J-1)+1,I)
              TTIME(2) = ODAT(5*(J-1)+2,I)
              CALL STME21 ( TTIME , DTME21 )
              IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                   WRITE (MDSS,952) DTME21
              TTIME(1) = ODAT(5*(J-1)+4,I)
              TTIME(2) = ODAT(5*(J-1)+5,I)
              CALL STME21 ( TTIME , DTME21 )
              IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                   WRITE (MDSS,953) DTME21
              TTIME(1) = 0
              TTIME(2) = 0
              DTTST    = REAL ( ODAT(5*(J-1)+3,I) )
              CALL TICK21 ( TTIME , DTTST  )
              CALL STME21 ( TTIME , DTME21 )
              IF ( ( ODAT(5*(J-1)+1,I) .NE. ODAT(5*(J-1)+4,I) .OR.      &
                     ODAT(5*(J-1)+2,I) .NE. ODAT(5*(J-1)+5,I) ) .AND.   &
                     MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
                  DO II=1, 18
                    IF ( DTME21(II:II).NE.'0' .AND.                     &
                         DTME21(II:II).NE.'/' .AND.                     &
                         DTME21(II:II).NE.' ' .AND.                     &
                         DTME21(II:II).NE.':' ) EXIT
                    DTME21(II:II) = ' '
                    END DO
                  WRITE (MDSS,954) DTME21(1:19)
                END IF
            !ELSE
            !OUTPTS(I)%FLOUT(8) = .FALSE.
          END IF
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
! ... End of loop I on NRGRD in 5.a
!
      END DO
!
#ifdef W3_T
      DO I=1, NRGRD
          WRITE (MDST,9050) I
          WRITE (MDST,9053) ODAT(:,I)
          WRITE (MDST,9052) FLGRD(:,:,I)
        END DO
#endif
!
! 6.  Read moving grid data ------------------------------------------ /
!
!     Only a single set of data are provided to be applied to all
!     the grids, because this is only intended for test cases. 
!     For true implementations, the jumping grid will be used.
!
      IF ( INFLAGS1(10) ) THEN
!
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
              WRITE (MDSS,965)
              WRITE (MDSS,966) 'Continuous grid movement data'
            END IF
!
          N_MOV  = NML_HOMOG_COUNT%N_MOV
          N_TOT = NML_HOMOG_COUNT%N_TOT

          IF ( N_MOV .EQ. 0 ) GOTO 2060
          IF ( N_MOV .GT. 99 ) GOTO 2061

          ALLOCATE ( TMOVE(2,N_MOV), AMOVE(N_MOV), DMOVE(N_MOV) )
!
          DO I=1,N_TOT
            READ(NML_HOMOG_INPUT(I)%NAME,*) IDTST
              SELECT CASE (IDTST)
              CASE ('MOV')
                READ(NML_HOMOG_INPUT(I)%DATE,*) TMOVE(:,I)
                AMOVE(I) = NML_HOMOG_INPUT(I)%VALUE1
                DMOVE(I) = NML_HOMOG_INPUT(I)%VALUE2
                IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )             &
                  WRITE (MDSS,968) I, TMOVE(:,I), AMOVE(I), DMOVE(I)
              CASE DEFAULT
                GOTO 2062
              END SELECT
          END DO
!
          NMVMAX = N_MOV
          DO I=1, NRGRD
            CALL W3SETG ( I, MDSE, MDST )
            CALL WMSETM ( I, MDSE, MDST )
            NMV    = N_MOV
            CALL WMDIMD ( I, MDSE, MDST, 0 )
            DO II=1, NMV
              TMV(:,4,II) = TMOVE(:,II)
              AMV(II,4)   = AMOVE(II)
              DMV(II,4)   = DMOVE(II)
              END DO
            END DO
!
        END IF
!
! 7.  Work load distribution ----------------------------------------- /
! 7.a Initialize arrays
!
!     *******************************************************
!     *** NOTE : OUTPUT PROCESSOR ASSIGNMENT NEEDS TO BE  ***
!     ***        CONSISTENT WITH ASSIGNMENT IN W3INIT.    ***
!     *******************************************************
!
      ALLOCATE ( ALLPRC(NMPROC,NRGRD) , MODMAP(NMPROC,NRGRP) ,        &
                 LOADMP(NMPROC,NRGRP) )
!
      ALLPRC = 0
      MODMAP = 0
      LOADMP = 0
!
! 7.b Determine number of output processors
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,970)
!
      NCPROC = NMPROC
      UPPROC = UPPROC .AND. UNIPTS .AND. IOSTYP.GT.1
!
! 7.b.1 Unified point output
!
      IF ( UNIPTS ) THEN
          IF ( NMPROC.GE.10 .AND. UPPROC ) THEN
              NCPROC = NMPROC - 1
            ELSE
              IF ( UPPROC .AND. MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) &
                  WRITE (MDSS,971) 'Separate process for point' //    &
                                   ' output disabled.'
              UPPROC = .FALSE.
            END IF
          IF ( NMPUPT .EQ. IMPROC ) THEN
              II     = LEN_TRIM(MNAMES(0))
              CALL WMUGET ( MDSS, MDST, MDSUP, 'OUT' )
              CALL WMUSET ( MDSS, MDST, MDSUP, .TRUE., 'OUT',         &
                           TRIM(FNMPRE)//'out_pnt.'//MNAMES(0)(1:II),  &
                           'Unified point output')
            END IF
        END IF
!
      IF ( UPPROC .AND. MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )         &
           WRITE (MDSS,972) NMPUPT
!
! 7.b.2 Other output
!
      ALLOCATE ( NDPOUT(NRGRD) )
      NDPOUT = 0
!
      IF ( IOSTYP .GT. 1 ) THEN
          DO I=1, NRGRD
            IF ( ODAT( 3,I) .GT. 0 ) NDPOUT(I) = NDPOUT(I) + 1
            IF ( ODAT(13,I) .GT. 0 ) NDPOUT(I) = NDPOUT(I) + 1
            IF ( ODAT(28,I) .GT. 0 ) NDPOUT(I) = NDPOUT(I) + 1
!xxx
! Checkpoint 
            IF ( ODAT(38,I) .GT. 0 ) NDPOUT(I) = NDPOUT(I) + 1
!xxx
            IF ( ODAT( 8,I) .GT. 0 .OR.  ODAT(18,I) .GT. 0 .OR.       &
                 ODAT(23,I) .GT. 0 )          &
                                           NDPOUT(I) = NDPOUT(I) + 1
            IF ( IOSTYP .EQ. 2 ) NDPOUT(I) = MIN ( 1 , NDPOUT(I) )
            END DO
        END IF
!
! ..... Reduce IOSTYP if not enough resources to run IOSTYP = 3
!
      IF ( IOSTYP.EQ.3 .AND.                                          &
             ( ( .NOT.PSHARE .AND. 4*SUM(NDPOUT).GT.NCPROC )          &
           .OR.( PSHARE .AND. 4*MAXVAL(NDPOUT).GT.NCPROC ) ) ) THEN
          DO I=1, NRGRD
            NDPOUT(I) = MIN ( 1 , NDPOUT(I) )
            END DO
          IOSTYP = 2
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,971) 'Separate processes for output' //     &
                                   ' types disabled.'
        END IF
!
! ..... Force sharing of output processes if not enough resources
!
      IF ( IOSTYP.GT.1 .AND. .NOT.PSHARE .AND.                        &
           4*SUM(NDPOUT).GT.NCPROC ) THEN
          PSHARE = .TRUE.
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,971) 'Grids sharing output processes.'
        END IF
!
! ..... Disable output processes if not enough resources
!
      IF ( IOSTYP.GT.1 .AND. 4*MAXVAL(NDPOUT).GT.NCPROC ) THEN
          NDPOUT = 0
          IOSTYP = 1
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,971) 'Separate processes for output' //     &
                               ' disabled.'
        END IF
!
! ..... Number of output processes (except for unified point output)
!
      NPOUTT = 0
      IF ( IOSTYP .GT. 1 ) THEN
          IF ( PSHARE ) THEN
              NPOUTT = MAXVAL(NDPOUT)
            ELSE
              NPOUTT = SUM(NDPOUT)
            END IF
        END IF
      NCPROC = NCPROC - NPOUTT
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
          IF ( NPOUTT .EQ. 0 ) THEN
              WRITE (MDSS,971) 'No (other) dedicated output processes.'
            ELSE
              WRITE (MDSS,973) NCPROC+1, NCPROC+NPOUTT, NPOUTT
            END IF
        END IF
!
! 7.c Set communicators and ALLPRC array
!
#ifdef W3_T
      WRITE (MDST,9070)
#endif
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,974)
      IF ( NMPLOG.EQ.IMPROC ) WRITE (MDSO,1974)
!
#ifdef W3_MPI
      CALL MPI_COMM_GROUP ( MPI_COMM_MWAVE, BGROUP, IERR_MPI )
#endif
      ALLOCATE ( TMPRNK(NMPROC) )
      NAPRES = NCPROC
!
      DO I=1, NRGRD
!
        IP1    = MAX( 1 , MIN ( NCPROC , 1+NINT(REAL(NCPROC)*RP1(I)) ) )
        IPN    = MAX( IP1 , MIN ( NCPROC , NINT(REAL(NCPROC)*RPN(I)) ) )
        OUTSTR = '-----'
!
        CALL WMSETM ( I, MDSE, MDST )
        NAPLOC = 1 + IPN - IP1
        NAPADD = NAPLOC
#ifdef W3_MPI
        CROOT  = IP1
        FBCAST = NAPLOC .NE. NCPROC
        FBCAST = NAPLOC .NE. NCPROC .OR.                         &
                     ( IOSTYP.GT.1 .AND. .NOT.PSHARE )
#endif
        DO J=IP1, IPN
          TMPRNK(1+J-IP1) = J - 1
          END DO
!
        IF ( IOSTYP .GT. 1 ) THEN
            IF ( PSHARE ) NAPRES = NCPROC
            DO J=1, NDPOUT(I)
              NAPADD = NAPADD + 1
              TMPRNK(NAPADD) = NAPRES
              NAPRES = NAPRES + 1
              END DO
          END IF
!
        IF ( UPPROC ) THEN
            NAPADD = NAPADD + 1
            TMPRNK(NAPADD) = NMPROC - 1
          END IF
!
#ifdef W3_MPI
        CALL MPI_GROUP_INCL ( BGROUP, NAPADD, TMPRNK, LGROUP,    &
                              IERR_MPI )
        CALL MPI_COMM_CREATE ( MPI_COMM_MWAVE, LGROUP,           &
                               MPI_COMM_GRD, IERR_MPI )
        CALL MPI_GROUP_FREE ( LGROUP, IERR_MPI )
#endif
!
        DO II=IP1, IPN
          ALLPRC(II,I) = 1 + II - IP1
          END DO
        II     = II - IP1
!
        IF ( PSHARE .OR. I.EQ.1 ) THEN
            NAPADD = NCPROC
          ELSE
            NAPADD = NCPROC + SUM(NDPOUT(1:I-1))
          END IF
        IF ( IOSTYP .GT. 1 ) THEN
            DO J=1, NDPOUT(I)
              NAPADD = NAPADD + 1
              II     = II + 1
              ALLPRC(NAPADD,I) = II
              END DO
          END IF
!
        IF ( UPPROC ) THEN
            II     = II + 1
            ALLPRC(NMPROC,I) = II
          END IF
!
#ifdef W3_T
        WRITE (MDST,9071) I, ALLPRC(:,I)
#endif
!
! ... output
!
!
        IF ( IOSTYP .LE. 1 ) THEN
!
            IF ( ODAT( 3,I) .GT. 0 ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(MAX(1,NAPLOC-1))+1
                OUTSTR(1) = STOUT
              END IF
            IF ( ODAT( 8,I) .GT. 0 .OR. UNIPTS ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(MAX(1,NAPLOC-2))+1
                OUTSTR(2) = STOUT
              END IF
            IF ( ODAT(13,I) .GT. 0 ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(MAX(1,NAPLOC-5))+1
                OUTSTR(3) = STOUT
              END IF
            IF ( ODAT(18,I) .GT. 0 ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(NAPLOC)+1
                OUTSTR(4) = STOUT
              END IF
            IF ( ODAT(23,I) .GT. 0 ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(MAX(1,NAPLOC-3))+1
                OUTSTR(5) = STOUT
              END IF
            IF ( ODAT(28,I) .GT. 0 ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(MAX(1,NAPLOC-4))+1
                OUTSTR(6) = STOUT
              END IF
!
          ELSE
!
            ! set last proc for point and disable point for the grid
            IF ( UNIPTS ) THEN
                WRITE (STOUT,'(I5.5)') TMPRNK(II) + 1
                OUTSTR(2) = STOUT
                ODAT(8,I) = 0
                IF ( UPPROC ) II = II - 1
              END IF
!
            IF ( IOSTYP .EQ. 2 ) THEN
!
                WRITE (STOUT,'(I5.5)') TMPRNK(II) + 1
                IF ( ODAT( 3,I) .GT. 0 ) OUTSTR(1) = STOUT
                IF ( ODAT( 8,I) .GT. 0 .OR.                           &
                                    ( UNIPTS .AND. .NOT.UPPROC ) )    &
                                         OUTSTR(2) = STOUT
                IF ( ODAT(13,I) .GT. 0 ) OUTSTR(3) = STOUT
                IF ( ODAT(18,I) .GT. 0 ) OUTSTR(4) = STOUT
                IF ( ODAT(23,I) .GT. 0 ) OUTSTR(5) = STOUT
                IF ( ODAT(28,I) .GT. 0 ) OUTSTR(6) = STOUT
!
              ELSE IF ( IOSTYP .EQ. 3 ) THEN
!
                IF ( ODAT( 3,I).GT.0 ) THEN
                    WRITE (STOUT,'(I5.5)') TMPRNK(II) + 1
                    OUTSTR(1) = STOUT
                    II        = II - 1
                  END IF
                IF ( ODAT(13,I).GT.0 ) THEN
                    WRITE (STOUT,'(I5.5)') TMPRNK(II) + 1
                    OUTSTR(3) = STOUT
                    II        = II - 1
                  END IF
                IF ( ODAT(28,I).GT.0 ) THEN
                    WRITE (STOUT,'(I5.5)') TMPRNK(II) + 1
                    OUTSTR(6) = STOUT
                    II        = II - 1
                  END IF
                WRITE (STOUT,'(I5.5)') TMPRNK(II) + 1
                IF ( ODAT( 8,I) .GT. 0 ) OUTSTR(2) = STOUT
                IF ( ODAT(18,I) .GT. 0 ) OUTSTR(4) = STOUT
                IF ( ODAT(23,I) .GT. 0 ) OUTSTR(5) = STOUT
!
              END IF
!
          END IF
!
        IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                    &
            WRITE (MDSS,975) MNAMES(I), IP1, IPN, OUTSTR
        IF ( NMPLOG .EQ. IMPROC )                                     &
            WRITE (MDSO,1975)MNAMES(I), IP1, IPN, OUTSTR
!
#ifdef W3_MPI
        IF ( FBCAST ) THEN
            TMPRNK(1) = IP1 - 1
            NAPBCT    = 1
            DO J=1, NMPROC
              IF ( ALLPRC(J,I) .EQ. 0 ) THEN
                  NAPBCT = NAPBCT + 1
                  TMPRNK(NAPBCT) = J - 1
                END IF
              END DO
            CALL MPI_GROUP_INCL ( BGROUP, NAPBCT, TMPRNK,       &
                                  LGROUP, IERR_MPI )
            CALL MPI_COMM_CREATE ( MPI_COMM_MWAVE, LGROUP,      &
                                   MPI_COMM_BCT, IERR_MPI )
            CALL MPI_GROUP_FREE ( LGROUP, IERR_MPI )
         END IF
#endif
!
        END DO
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) THEN
          WRITE (MDSS,976)
          IF ( UNIPTS ) WRITE (MDSS,977) NMPUPT
          WRITE (MDSS,*) 
        END IF
!
      IF ( NMPLOG .EQ. IMPROC ) THEN
          WRITE (MDSO,1976)
          IF ( UNIPTS ) WRITE (MDSO,1977) NMPUPT
          WRITE (MDSO,*) 
        END IF
!
      DEALLOCATE ( TMPRNK, NDPOUT )
!
! 7.d Set MODMAP and LOADMP arrays
!
      DO JJ=1, NRGRP
        DO II=1, INGRP(JJ,0)
          I      = INGRP(JJ,II)
          DO J=1, NMPROC
            IF ( ALLPRC(J,I) .NE. 0 ) THEN
                LOADMP(J,JJ) = LOADMP(J,JJ) + 1
                IF ( LOADMP(J,JJ) .EQ. 1 ) THEN
                    MODMAP(J,JJ) = I
                  ELSE
                    MODMAP(J,JJ) = -1
                  END IF
              END IF
            END DO
          END DO
        END DO
!
#ifdef W3_T
      WRITE (MDST,8042)
      DO J=1, NRGRP
        WRITE (MDST,8044) J, MODMAP(:,J)
        END DO
      WRITE (MDST,8043)
      DO J=1, NRGRP
        WRITE (MDST,8044) J, LOADMP(:,J)
        END DO
#endif
!
! 7.e Warnings
!
      IF ( NMPROC .GT. 1 ) THEN
          DO I=1, NRGRP
            IP1    = MINVAL ( LOADMP(:NCPROC,I) )
            IPN    = MAXVAL ( LOADMP(:NCPROC,I) )
            IF ( IP1.NE.IPN .AND. IMPROC.EQ.NMPERR )                  &
                WRITE (MDSE,1040) I, IP1, IPN
            END DO
        END IF
!
      DEALLOCATE ( RP1, RPN, LOADMP )
!
! 7.f Reset NMPSCR to first processor of first rank 1 grid
!
#ifdef W3_MPI
      CALL WMSETM ( INGRP(1,1), MDSE, MDST )
      NMPSCR = CROOT
#endif
!
#ifdef W3_MPI
      CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
#endif
!
! 8.  Actual initializations ----------------------------------------- /
!
#ifdef W3_MPRF
      CALL PRTIME ( PRFTN )
      WRITE (MDSP,990) PRFT0, PRFTN, get_memory(), 'START Sec. 8'
      PRFT0  = PRFTN
#endif
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,980)
      ALLOCATE ( TSYNC(2,0:NRGRD), TMAX(2,NRGRD), TOUTP(2,0:NRGRD),   &
                 TDATA(2,NRGRD), GRSTAT(NRGRD), DTRES(NRGRD) )
!
      TSYNC(1,:) = -1
      TSYNC(2,:) =  0
      TMAX (1,:) = -1
      TMAX (2,:) =  0
      TOUTP(1,:) = -1
      TOUTP(2,:) =  0
      TDATA(1,:) = -1
      TDATA(2,:) =  0
      GRSTAT     = 99
!
! 8.a Loop over models for per-model initialization
!
#ifdef W3_T
      WRITE (MDST,9080)
#endif
#ifdef W3_MPRF
      CALL PRTIME ( PRFTN )
      WRITE (MDSP,990) PRFT0, PRFTN, get_memory(), 'START Sec. 8.a'
      PRFT0  = PRFTN
#endif
!
      DO I=1, NRGRD
        J      = LEN_TRIM(MNAMES(I))
        DO NMPSC2=1, NMPROC
          IF ( ALLPRC(NMPSC2,I) .EQ. 1 ) EXIT
          END DO
        IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC )                    &
             WRITE (MDSS,981) I, MNAMES(I)(1:J)
!
#ifdef W3_MPI
        CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
#endif
!
! 8.a.1 Wave model initialization (NOTE: sets all grid pointers)
! ..... Initial output file hook up
!
        CALL WMSETM ( I, MDSE, MDST )
#ifdef W3_MPI
        MPI_COMM_LOC = MPI_COMM_GRD
        IF ( MPI_COMM_LOC .EQ. MPI_COMM_NULL ) CYCLE
#endif
!
        CALL WMUGET ( MDSE, MDST, NDSFND, 'OUT' )
        CALL WMUSET ( MDSE, MDST, NDSFND, .TRUE., DESC='Log file' )
        MDS( 1,I) = NDSFND
!
! ... this one overwrites the combined setting MDS( 3,I) = MDST above
!
!       CALL WMUGET ( MDSE, MDST, NDSFND, 'OUT' )
!       CALL WMUSET ( MDSE, MDST, NDSFND, .TRUE., DESC='Test output' )
!       MDS( 3,I) = NDSFND
!
        DO J=1, 6
          IF ( J.EQ.4 .OR. J.EQ.5 ) CYCLE
          IF ( ODAT(5*(J-1)+3,I) .GT. 0 ) THEN
              CALL WMUGET ( MDSE, MDST, NDSFND, 'OUT' )
              CALL WMUSET ( MDSE, MDST, NDSFND, .TRUE.,              &
                            DESC='Raw output file' )
              SELECT CASE (J)
                CASE (1)
                  MDS(7,I) = NDSFND
                CASE (2)
                  MDS(8,I) = NDSFND
                CASE (3)
                  MDS(12,I) = NDSFND
                  CALL WMUGET ( MDSE, MDST, NDSFND, 'INP' )
                  CALL WMUSET ( MDSE, MDST, NDSFND, .TRUE.,           &
                                DESC='Input data file' )
                  MDS(11,I) = NDSFND
                CASE (6)
                  MDS(13,I) = NDSFND
              END SELECT
            END IF
          END DO
!
        CALL WMUGET ( MDSE, MDST, NDSFND, 'INP' )
        CALL WMUSET ( MDSE, MDST, NDSFND, .TRUE.,                     &
                      DESC='Input data file' )
        MDS(9,I) = NDSFND
!
        IF ( ODAT(5*(5-1)+3,I) .GT. 0 ) THEN
            CALL WMUGET ( MDSE, MDST, NDSFND, 'OUT', 9 )
            MDS(10,I) = NDSFND
            DO II=0, 8
              CALL WMUSET ( MDSE, MDST, NDSFND+II, .TRUE.,            &
                            DESC='Raw output file' )
              END DO
          END IF
!
! ..... Model initialization
!
        IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC ) WRITE (MDSS,982)
!
        CALL W3INIT ( I, .TRUE., MNAMES(I), MDS(:,I), NTRACE(:,I), ODAT(:,I), &
                      FLGRD(:,:,I),FLGR2(:,:,I),FLGD(:,I),FLG2(:,I),  &
                      OT2(I)%NPTS, OT2(I)%X, OT2(I)%Y, OT2(I)%PNAMES, &
                      IPRT(:,I), LPRT(I), MPI_COMM_LOC )
!
! ..... Finalize I/O file hook up
!
        II     = LEN_TRIM(FILEXT)
        JJ     = LEN_TRIM(FNMPRE)
        CALL WMUINQ ( MDSE, MDST, MDS(1,I) )
        IF ( MDS(3,I) .NE. MDST ) CALL WMUINQ ( MDSE, MDST, MDS(3,I) )
!
        IF ( MDS(7,I) .NE. -1 ) THEN
            IF ( IAPROC .EQ. NAPFLD ) THEN
                TNAME  = TRIM(FNMPRE)//'out_grd.' // FILEXT(:II)
                CALL WMUSET ( MDSE,MDST, MDS(7,I), .TRUE., NAME=TNAME )
              ELSE
                CALL WMUSET ( MDSE,MDST, MDS(7,I), .FALSE. )
                MDS(7,I) = -1
              END IF
          END IF
!
        IF ( MDS(8,I) .NE. -1 ) THEN
            IF ( IAPROC .EQ. NAPPNT ) THEN
                TNAME  = TRIM(FNMPRE)//'out_pnt.' // FILEXT(:II)
                CALL WMUSET ( MDSE,MDST, MDS(8,I), .TRUE., NAME=TNAME )
              ELSE
                CALL WMUSET ( MDSE,MDST, MDS(8,I), .FALSE. )
                MDS(8,I) = -1
              END IF
          END IF
!
        IF ( MDS(9,I) .NE. -1 ) THEN
            IF ( FLBPI ) THEN
                TNAME  = TRIM(FNMPRE)//'nest.' // FILEXT(:II)
                CALL WMUSET ( MDSE, MDST, MDS(9,I), .TRUE., NAME=TNAME )
              ELSE
                CALL WMUSET ( MDSE, MDST, MDS(9,I), .FALSE. )
                MDS(9,I) = -1
              END IF
          END IF
!
        IF ( MDS(10,I) .NE. -1 ) THEN
            IF ( FLBPO .AND. IAPROC.EQ.NAPBPT ) THEN
                TNAME  = TRIM(FNMPRE)//'nestN.' // FILEXT(:II)
                DO J=0, NFBPO-1
                  WRITE (TNAME(JJ+5:JJ+5),'(I1)') J + 1
                  CALL WMUSET ( MDSE, MDST, MDS(10,I)+J, .TRUE.,      &
                                NAME=TNAME )
                  END DO
                DO J=NFBPO, 8
                  CALL WMUSET ( MDSE,MDST, MDS(10,I)+J, .FALSE. )
                  END DO
              ELSE
                DO J=0, 8
                  CALL WMUSET ( MDSE,MDST, MDS(10,I)+J, .FALSE. )
                  END DO
                MDS(10,I) = -1
              END IF
          END IF
!
        IF ( MDS(11,I) .NE. -1 ) THEN
            TNAME  = TRIM(FNMPRE)//'track_i.' // FILEXT(:II)
            CALL WMUSET ( MDSE,MDST, MDS(11,I), .TRUE., NAME=TNAME )
          END IF
!
        IF ( MDS(12,I) .NE. -1 ) THEN
            IF ( IAPROC .EQ. NAPTRK ) THEN
                TNAME  = TRIM(FNMPRE)//'track_o.' // FILEXT(:II)
                CALL WMUSET ( MDSE,MDST, MDS(12,I), .TRUE., NAME=TNAME )
              ELSE
                CALL WMUSET ( MDSE,MDST, MDS(12,I), .FALSE. )
                MDS(12,I) = -1
              END IF
          END IF
!
        IF ( MDS(13,I) .NE. -1 ) THEN
            IF ( IAPROC .EQ. NAPPRT ) THEN
                TNAME  = TRIM(FNMPRE)//'partition.' // FILEXT(:II)
                CALL WMUSET ( MDSE,MDST, MDS(13,I), .TRUE., NAME=TNAME )
              ELSE
                CALL WMUSET ( MDSE,MDST, MDS(13,I), .FALSE. )
                MDS(13,I) = -1
              END IF
          END IF
!
#ifdef W3_T
        WRITE (MDST,9081) I, TIME
#endif
!
! 8.a.2 Data file initialization (forcing)
!
        IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC ) WRITE (MDSS,983)
        CALL W3SETI ( I, MDSE, MDST )
!
! ..... regular input files
!
        DO J=JFIRST, 6
          IF ( INFLAGS1(J) ) THEN
              IDINP(I,J) = IDSTR(J)
              IF ( INPMAP(I,J) .LT. 0 ) CYCLE
              CALL W3FLDO ('READ', IDINP(I,J), MDSF(I,J), MDST, MDSE2,&
                            NX, NY, GTYPE, IERR, MNAMES(I),           &
                            TRIM(FNMPRE) )
              IF ( IERR .NE. 0 ) GOTO 2080
              IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC )              &
                   WRITE (MDSS,985) IDFLDS(J)
            ELSE
              IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC )              &
                   WRITE (MDSS,984) IDFLDS(J)
            END IF
          END DO
!
! ..... assimilation data files
!
!       version 3.07: Data assimilation part ignored for now ....
!
! ..... finalize file info data base
!
        DO J=JFIRST, 9
          IF ( MDSF(I,J) .NE. -1 ) CALL WMUINQ ( MDSE, MDST, MDSF(I,J) )
          END DO
!
! ..... Adjust input flags for other than native or CPL input,
!       and initialize input arrays one set at a time as needed.
!
        IF ( SIZE(INFLAGS1) .NE. SIZE(TFLAGS) ) THEN
          WRITE (MDSE,'(/2A)') ' *** ERROR WMINITNML: ', &
                 'SIZE(INFLAGS1).NE.SIZE(TFLAGS) ***'
          CALL EXTCDE ( 999 )
        END IF
        IF ( SIZE(INFLAGS2) .NE. SIZE(TFLAGS) ) THEN
          WRITE (MDSE,'(/2A)') ' *** ERROR WMINITNML: ', &
                 'SIZE(INFLAGS2).NE.SIZE(TFLAGS) ***'
          CALL EXTCDE ( 999 )
        END IF

        TFLAGS = INFLAGS1
!
        DO J=JFIRST, 9
          IF ( INPMAP(I,J) .NE. 0 ) THEN
!
              TFLAGS(J) = .TRUE.
              INFLAGS1     = .FALSE.
              INFLAGS1(J)  = .TRUE.
              IINIT     = .FALSE.
              CALL W3DIMI ( I, MDSE, MDST )
!
              IF ( J.EQ.2 ) ALLOCATE ( WADATS(I)%CA0(NSEA) ,          &
                                       WADATS(I)%CAI(NSEA) ,          &
                                       WADATS(I)%CD0(NSEA) ,          &
                                       WADATS(I)%CDI(NSEA) )
!
              IF ( J.EQ.3 ) ALLOCATE ( WADATS(I)%UA0(NSEA) ,          &
                                       WADATS(I)%UAI(NSEA) ,          &
                                       WADATS(I)%UD0(NSEA) ,          &
                                       WADATS(I)%UDI(NSEA) ,          &
                                       WADATS(I)%AS0(NSEA) ,          &
                                       WADATS(I)%ASI(NSEA) )
!
              IF ( J.EQ.5 ) ALLOCATE ( WADATS(I)%MA0(NSEA) ,          &
                                       WADATS(I)%MAI(NSEA) ,          &
                                       WADATS(I)%MD0(NSEA) ,          &
                                       WADATS(I)%MDI(NSEA) )
!
              IF ( J.EQ.6 ) ALLOCATE ( WADATS(I)%RA0(NSEA) ,          &
                                       WADATS(I)%RAI(NSEA) )
!
            END IF
          END DO
!
        INFLAGS1  = TFLAGS
        CALL W3SETI ( I, MDSE, MDST )
        CALL W3SETA ( I, MDSE, MDST )
!
! 8.a.3 Status indicator and model times
!
        DO J=1, NOTYPE
          IF ( FLOUT(J) ) THEN
              IF ( TOUTP(1,I) .EQ. -1 ) THEN
                  TOUTP(:,I) = TONEXT(:,J)
                ELSE
                  DTTST  = DSEC21 ( TOUTP(:,I), TONEXT(:,J) )
                  IF ( DTTST .LT. 0. ) TOUTP(:,I) = TONEXT(:,J)
                ENDIF
            END IF
          END DO
!
! Checkpoint
        J=8
            OUTPTS(I)%FLOUT(8)=.FALSE.
          IF ( ODAT(5*(J-1)+3,I) .NE. 0 ) THEN
             OUTPTS(I)%FLOUT(8)=.TRUE.
          ELSE
              OUTPTS(I)%FLOUT(8)=.FALSE.
          ENDIF

          IF ( FLOUT(J) ) THEN
              IF ( TOUTP(1,I) .EQ. -1 ) THEN
                  TOUTP(:,I) = TONEXT(:,J)
                ELSE
                  DTTST  = DSEC21 ( TOUTP(:,I), TONEXT(:,J) )
                  IF ( DTTST .LT. 0. ) TOUTP(:,I) = TONEXT(:,J)
                ENDIF
            END IF
!
!
        GRSTAT(I) =  0
        TSYNC(:,I) = TIME(:)
!
#ifdef W3_T
        WRITE (MDST,9082) GRSTAT(I), TOUTP(:,I), TSYNC(:,I)
#endif
!
        END DO
!
#ifdef W3_MPI
      CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
      DO I=1, NRGRD
        CALL WMSETM ( I, MDSE, MDST )
        CALL W3SETG ( I, MDSE, MDST )
        CALL W3SETO ( I, MDSE, MDST )
        IF ( FBCAST .AND. MPI_COMM_BCT.NE.MPI_COMM_NULL ) THEN
            CALL MPI_BCAST ( TOUTP(1,I), 2, MPI_INTEGER, 0,      &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( TSYNC(1,I), 2, MPI_INTEGER, 0,      &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( GRSTAT(I), 1, MPI_INTEGER, 0,       &
                             MPI_COMM_BCT, IERR_MPI )
#endif
!
! 8.a.4 Grid sizes etc. for processors that are not used.
!
#ifdef W3_MPI
            CALL MPI_BCAST ( FLAGLL,1, MPI_LOGICAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( GTYPE, 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( ICLOSE,1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NX   , 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NY   , 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( X0   , 1, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( SX   , 1, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( Y0   , 1, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( SY   , 1, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NSEA , 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NSEAL, 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( DTMAX, 1, MPI_REAL, 0,              &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( DTCFL, 1, MPI_REAL, 0,              &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( FILEXT, 10, MPI_CHARACTER, 0,       &
                             MPI_COMM_BCT, IERR_MPI )
            IF ( MPI_COMM_GRD .EQ. MPI_COMM_NULL )               &
                 CALL W3DIMX  ( I, NX, NY, NSEA, MDSE, MDST      &
#endif
#ifdef W3_SMC
 !!  SMC grid related variables are not needed beyond MPI_COMM_GRD
 !!  so all dimensions are minimised to 1.  JGLi29Mar2021
#endif
#ifdef W3_MPI
#ifdef W3_SMC
 !!Li        , NCel, NUFc, NVFc, NRLv, NBSMC  &
 !!Li        , NARC, NBAC, NSPEC              &
             , 1, 1, 1, 1, 1, 1, 1, 1         &
#endif
                )
            CALL MPI_BCAST ( HQFAC, NX*NY, MPI_REAL, 0,          &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( HPFAC, NX*NY, MPI_REAL, 0,          &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( XGRD, NX*NY, MPI_REAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( YGRD, NX*NY, MPI_REAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            IF ( MPI_COMM_GRD .EQ. MPI_COMM_NULL )               &
                 GSU = W3GSUC( .FALSE., FLAGLL, ICLOSE,          &
                               XGRD, YGRD )
            CALL MPI_BCAST ( DXDP, NX*NY, MPI_REAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( DXDQ, NX*NY, MPI_REAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( DYDP, NX*NY, MPI_REAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( DYDQ, NX*NY, MPI_REAL, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( MAPSTA, NX*NY, MPI_INTEGER, 0,      &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( MAPST2, NX*NY, MPI_INTEGER, 0,      &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( GRIDSHIFT, 1, MPI_DOUBLE_PRECISION, 0, &
                             MPI_COMM_BCT, IERR_MPI )
#endif
!
#ifdef W3_MPI
            CALL MPI_BCAST ( NK   , 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NTH  , 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( XFR  , 1, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( FR1  , 1, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            IF ( MPI_COMM_GRD .EQ. MPI_COMM_NULL )               &
                 CALL W3DIMS ( I, NK, NTH, MDSE, MDST )
            CALL MPI_BCAST ( TH , NTH, MPI_REAL   , 0,           &
                             MPI_COMM_BCT, IERR_MPI )
#endif
!
#ifdef W3_MPI
            CALL MPI_BCAST ( NAPROC,1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NAPPNT,1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( NBI  , 1, MPI_INTEGER, 0,           &
                             MPI_COMM_BCT, IERR_MPI )
#endif
!
#ifdef W3_MPI
            CALL MPI_BCAST ( FLOUT,  8, MPI_LOGICAL, 0,          &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( DTOUT , 8, MPI_REAL, 0,             &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( TONEXT,16, MPI_INTEGER, 0,          &
                             MPI_COMM_BCT, IERR_MPI )
            CALL MPI_BCAST ( TOLAST,16, MPI_INTEGER, 0,          &
                             MPI_COMM_BCT, IERR_MPI )
#endif
!
#ifdef W3_MPI
          END IF
        END DO
      CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
#endif
!
      DO I=1, NRGRD
        IF ( ALLPRC(IMPROC,I) .EQ. 0 ) THEN
            CALL W3SETO ( I, MDSE, MDST )
            IAPROC = -1
          END IF
        END DO
!
! 8.a.5 Test output
!
#ifdef W3_T
      WRITE (MDST,9020) 'AFTER SETUP'
      DO I=1, NRGRD
        WRITE (MDST,9021) I, MDS(:,I), NTRACE(:,I)
        END DO
#endif
!
! 8.a.6 Check for coordinate system
!
      DO I=1, NRGRD-1
        IF ( GRIDS(I)%FLAGLL .NEQV. GRIDS(I+1)%FLAGLL ) GOTO 2070
        END DO
!
! 8.b Input files
!
#ifdef W3_MPRF
      CALL PRTIME ( PRFTN )
      WRITE (MDSP,990) PRFT0, PRFTN, get_memory(), 'START Sec. 8.c'
      PRFT0  = PRFTN
#endif
!
      DO I=1, NRINP
!
        IF ( .NOT. USEINP(I) ) CYCLE
!
        J      = LEN_TRIM(MNAMES(-I))
        IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC ) THEN
            WRITE (MDSS,988) I, MNAMES(-I)(1:J)
            WRITE (MDSS,987)
          END IF
!
        CALL W3IOGR ( 'GRID', NDSREC, -I, MNAMES(-I)(1:J) )
        CALL W3DIMI ( -I, MDSE, MDST )
!
        IF ( CPLINP(I) ) CYCLE
!
        DO J=JFIRST, 6
          IF ( INFLAGS1(J) ) THEN
              IDINP(-I,J) = IDSTR(J)
              CALL W3FLDO ('READ', IDINP(-I,J), MDSF(-I,J), MDST,     &
                            MDSE2, NX, NY, GTYPE, IERR,               &
                            MNAMES(-I), TRIM(FNMPRE) )
              IF ( IERR .NE. 0 ) GOTO 2080
              IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC )              &
                   WRITE (MDSS,985) IDFLDS(J)
            ELSE
              IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC )              &
                   WRITE (MDSS,984) IDFLDS(J)
            END IF
          END DO
!
! Skipping assimilation input files for now.
!
          DO J=JFIRST, 9
            IF ( MDSF(-I,J) .NE. -1 ) CALL WMUINQ                     &
                                         ( MDSE, MDST, MDSF(-I,J) )
            END DO
!
        END DO
!
      DO I=1, NRGRD
        DO J=JFIRST, 9
          IF ( INPMAP(I,J) .LT. 0 ) IDINP(I,J) = IDINP( INPMAP(I,J),J)
          IF ( INPMAP(I,J) .GT. 0 ) IDINP(I,J) = IDINP(-INPMAP(I,J),J)
          END DO
        END DO
!
      DEALLOCATE ( USEINP )
      DEALLOCATE ( CPLINP )
!
! 8.c Inter model initialization
!
#ifdef W3_MPRF
      CALL PRTIME ( PRFTN )
      WRITE (MDSP,990) PRFT0, PRFTN, get_memory(), 'START Sec. 8.d'
      PRFT0  = PRFTN
#endif

! 8.c.1 Spectral conversion flags and source term flags
!
      CALL WMRSPC
!
      DO I=1, NRGRD
        CALL W3SETG ( I, MDSE, MDST )
        FLAGST = .TRUE.
        END DO
!
! 8.c.2 Relation to lower ranked grids
!       Includes update of unit numbers, and bound. data initialization.
!
      ALLOCATE ( FLRBPI(NRGRD) )
      CALL WMGLOW ( FLRBPI )
!
! ..... At this point the grid-search-utility (GSU) object for grids
!       that do not belong to this processor is no longer needed.
!
#ifdef W3_MPI
      DO I=1, NRGRD
        CALL WMSETM ( I, MDSE, MDST )
        CALL W3SETG ( I, MDSE, MDST )
#endif
! the next line (with the W3GSUD call) removed Jan 8 2013. 
! ...ref: personal communication, 
! ...email from Rogers to Alves, Campbell, Tolman, Chawla Dec 13 2012.
! REMOVED  !/MPI        IF ( MPI_COMM_GRD .EQ. MPI_COMM_NULL ) CALL W3GSUD( GSU )
#ifdef W3_MPI
        END DO
#endif
!
! ..... Unit numbers
!

      DO I=1, NRGRD
!
        CALL W3SETG ( I, MDSE, MDST )
        CALL W3SETO ( I, MDSE, MDST )
!
        IF ( BCDUMP(I) .AND. FLRBPI(I) ) THEN
            IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1080) I
            IF ( IMPROC .EQ. NMPLOG ) WRITE (MDSO,1082) I
            BCDUMP(I) = .FALSE.
          END IF
!
        IF ( BCDUMP(I) .AND. NBI.EQ.0 ) THEN
            IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1081) I
            IF ( IMPROC .EQ. NMPLOG ) WRITE (MDSO,1082) I
            BCDUMP(I) = .FALSE.
          END IF
!
#ifdef W3_SHRD
        IF ( .NOT. FLRBPI(I) .AND. FLBPI ) THEN
#endif
#ifdef W3_MPI
        IF ( .NOT. FLRBPI(I) .AND. FLBPI .AND.                   &
              MPI_COMM_GRD .NE. MPI_COMM_NULL) THEN
#endif
            CALL WMUSET ( MDSE, MDST, NDS(9), .FALSE. )
            IF ( BCDUMP(I) .AND. IAPROC.EQ.NAPBPT ) THEN
                J            = LEN_TRIM(FILEXT)
                TNAME(1:5)   = 'nest.'
                TNAME(6:5+J) = FILEXT(1:J)
                J      = J + 5
                CALL WMUGET ( MDSE, MDST, NDS(9), 'OUT' )
                CALL WMUSET ( MDSE, MDST, NDS(9), .TRUE.,             &
                              NAME=TRIM(FNMPRE)//TNAME(1:J),           &
                              DESC='Output data file (nest dump)' )
                MDS(9,I) = NDSFND
              ELSE
                NDS(9) = -1
              END IF
          END IF
!
        END DO
!
! ..... Data initialization
!
      DO I=1, NRGRD
#ifdef W3_MPI
        CALL WMSETM ( I, MDSE, MDST )
        IF ( MPI_COMM_GRD .NE. MPI_COMM_NULL ) CALL WMIOBS ( I )
#endif
#ifdef W3_SHRD
        CALL WMIOBS ( I )
#endif
        END DO
!
      DO I=1, NRGRD
#ifdef W3_MPI
        CALL WMSETM ( I, MDSE, MDST )
        IF ( MPI_COMM_GRD .NE. MPI_COMM_NULL ) CALL WMIOBG ( I )
#endif
#ifdef W3_SHRD
        CALL WMIOBG ( I )
#endif
        END DO
!
#ifdef W3_MPI
      DO I=1, NRGRD
        CALL WMSETM ( I, MDSE, MDST )
        IF ( MPI_COMM_GRD .NE. MPI_COMM_NULL ) CALL WMIOBF ( I )
        END DO
#endif
!
! 8.c.3 Relation to same ranked grids
!
#ifdef W3_SMC
 !!  Check whether there is a SMC grid group.  JGLi12Apr2021
      NGRPSMC = 0 
      DO JJ=1, NRGRP
         J = 0
         DO II=1, INGRP(JJ,0)
            I = INGRP(JJ,II)
            IF( GRIDS(I)%GTYPE .EQ. SMCTYPE ) J = J + 1 
         ENDDO
         IF( J .GT. 1 )  NGRPSMC = JJ 
      ENDDO
      IF( IMPROC.EQ.NMPERR )  WRITE (MDSE,*) " NGRPSMC =", NGRPSMC
 
 !!  Equal ranked SMC grid group uses its own sub.   JGLi12Apr2021
      IF( NGRPSMC .GT. 0 ) THEN
          CALL WMSMCEQL
      ELSE
#endif
!
      CALL WMGEQL
!
#ifdef W3_SMC
      ENDIF
#endif
!
! 8.c.4 Relation to higher ranked grids
!
      IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC ) WRITE (MDSS,938) &
           'Computing relation to higher ranked grids'
      CALL WMGHGH
      IF ( MDSS.NE.MDSO .AND. NMPSC2.EQ.IMPROC ) WRITE (MDSS,938) &
           'Finished computing relation to higher ranked grids'
!
! 8.c.5 Unified point output
!
      IF ( UNIPTS ) THEN
!
          OUTPTS(0)%TONEXT(1,2) =        ODAT( 6,0)
          OUTPTS(0)%TONEXT(2,2) =        ODAT( 7,0)
          OUTPTS(0)%DTOUT (  2) = REAL ( ODAT( 8,0) )
          OUTPTS(0)%TOLAST(1,2) =        ODAT( 9,0)
          OUTPTS(0)%TOLAST(2,2) =        ODAT(10,0)
!
          TOUT   = OUTPTS(0)%TONEXT(:,2)
          TLST   = OUTPTS(0)%TOLAST(:,2)
!
          DO
            DTTST   = DSEC21 ( STIME , TOUT )
            IF ( DTTST .LT. 0 ) THEN
                CALL TICK21 ( TOUT, OUTPTS(0)%DTOUT(2) )
              ELSE
                EXIT
              END IF
            END DO
!
          OUTPTS(0)%TONEXT(:,2) = TOUT
!
          DTTST  = DSEC21 ( TOUT , TLST )
          IF (( DTTST .LT. 0. ) .OR. ( ODAT(8,0) .EQ. 0 )) THEN
              UNIPTS = .FALSE.
            ELSE
              CALL WMIOPP ( OT2(0)%NPTS, OT2(0)%X, OT2(0)%Y,          &
                            OT2(0)%PNAMES )
            END IF
!
#ifdef W3_MPI
          DO I=1, NRGRD
            CALL WMSETM ( I, MDSE, MDST )
            CALL W3SETG ( I, MDSE, MDST )
            CALL W3SETO ( I, MDSE, MDST )
            IF ( FBCAST .AND. MPI_COMM_BCT.NE.MPI_COMM_NULL ) THEN
                CALL MPI_BCAST ( NOPTS, 1, MPI_INTEGER, 0,       &
                                 MPI_COMM_BCT, IERR_MPI )
              END IF
            END DO
#endif
!
        END IF
!
! 8.c.6 Output
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                      &
          WRITE (MDSS,938) 'Additional group information'
!
      IF ( MAXVAL(GRDLOW(:,0)) .GT. 0 ) THEN
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,933) 'Lower rank grid dependence'
          IF ( NMPLOG .EQ. IMPROC )                                   &
              WRITE (MDSO,933) 'Lower rank grid dependence'
          DO I=1, NRGRD
            WRITE (LINE(1:6),'(1X,I3,2X)') I
            JJJ    = 6
            IF ( GRDLOW(I,0) .NE. 0 ) THEN
                DO J=1, GRDLOW(I,0)
                  WRITE (LINE(JJJ+1:JJJ+3),'(I3)') GRDLOW(I,J)
                  JJJ    = JJJ + 3
                  END DO
              ELSE IF ( FLRBPI(I) ) THEN
                JJJ    = 21
                LINE(7:JJJ) = ' Data from file'
              ELSE
                JJJ    = 22
                LINE(7:JJJ) = ' No dependencies'
              END IF
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                WRITE(MDSS,934) LINE(1:JJJ)
            IF ( NMPLOG .EQ. IMPROC ) WRITE(MDSO,934) LINE(1:JJJ)
            END DO
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,935)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,935)
        ELSE
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,937) 'No lower rank grid dependencies'
          IF ( NMPLOG .EQ. IMPROC )                                   &
              WRITE (MDSO,937) 'No lower rank grid dependencies'
        END IF
      DEALLOCATE ( FLRBPI )
!
      IF ( MAXVAL(GRDEQL(:,0)) .GT. 0 ) THEN
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,933) 'Same rank grid dependence'
          IF ( NMPLOG .EQ. IMPROC )                                   &
              WRITE (MDSO,933) 'Same rank grid dependence'
          DO I=1, NRGRD
            WRITE (LINE(1:6),'(1X,I3,2X)') I
            JJJ    = 6
            IF ( GRDEQL(I,0) .NE. 0 ) THEN
                DO J=1, GRDEQL(I,0)
                  WRITE (LINE(JJJ+1:JJJ+3),'(I3)') GRDEQL(I,J)
                  JJJ    = JJJ + 3
                  END DO
              ELSE
                JJJ    = 22
                LINE(7:JJJ) = ' No dependencies'
              END IF
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                WRITE(MDSS,934) LINE(1:JJJ)
            IF ( NMPLOG .EQ. IMPROC ) WRITE(MDSO,934) LINE(1:JJJ)
            END DO
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,935)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,935)
        ELSE
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,937) 'No same rank grid dependencies'
          IF ( NMPLOG .EQ. IMPROC )                                   &
              WRITE (MDSO,937) 'No same rank grid dependencies'
        END IF
!
      IF ( MAXVAL(GRDHGH(:,0)) .GT. 0 ) THEN
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,933) 'Higher rank grid dependence'
          IF ( NMPLOG .EQ. IMPROC )                                   &
              WRITE (MDSO,933) 'Higher rank grid dependence'
          DO I=1, NRGRD
            WRITE (LINE(1:6),'(1X,I3,2X)') I
            JJJ    = 6
            IF ( GRDHGH(I,0) .NE. 0 ) THEN
                DO J=1, GRDHGH(I,0)
                  WRITE (LINE(JJJ+1:JJJ+3),'(I3)') GRDHGH(I,J)
                  JJJ    = JJJ + 3
                  END DO
              ELSE
                JJJ    = 22
                LINE(7:JJJ) = ' No dependencies'
              END IF
            IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                &
                WRITE(MDSS,934) LINE(1:JJJ)
            IF ( NMPLOG .EQ. IMPROC ) WRITE(MDSO,934) LINE(1:JJJ)
            END DO
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,935)
          IF ( NMPLOG .EQ. IMPROC ) WRITE (MDSO,935)
        ELSE
          IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC )                  &
              WRITE (MDSS,937) 'No higher rank grid dependencies'
          IF ( NMPLOG .EQ. IMPROC )                                   &
              WRITE (MDSO,937) 'No higher rank grid dependencies'
        END IF
!
#ifdef W3_T
      WRITE (MDST,9083)
      DO I=-NRINP, NRGRD
        WRITE (MDST,9084) I, IDINP(I,:)
        END DO
#endif
!
!    Test output of connected units (always)
!
      CALL WMUSET ( MDSE, MDST, SCRATCH, .FALSE. )
      IF ( TSTOUT ) CALL WMUDMP ( MDST, 0 )
!
      DEALLOCATE ( MDS, NTRACE, ODAT, FLGRD, FLGR2, FLGD, FLG2, INAMES,&
                   MNAMES )
!
#ifdef W3_MPI
      CALL MPI_BARRIER ( MPI_COMM_MWAVE, IERR_MPI )
#endif
!
      CALL DATE_AND_TIME ( VALUES=CLKDT2 )
      CLKFIN = TDIFF ( CLKDT1,CLKDT2 )
!
#ifdef W3_MPRF
      CALL PRTIME ( PRFTN )
      WRITE (MDSP,990) PRFT0, PRFTN, get_memory(), 'END'
#endif
!
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,998)
#ifdef W3_O10
      IF ( MDSS.NE.MDSO .AND. NMPSCR.EQ.IMPROC ) WRITE (MDSS,999)
#endif
!!!!!/MPI CALL MPI_BARRIER (MPI_COMM_MWAVE,IERR_MPI)
!!!!!/MPI CALL MPI_FINALIZE  ( IERR_MPI )
!!!!!/MPI stop
!
      RETURN
!
! Escape locations read errors :
!
 2003 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1003)
      CALL EXTCDE ( 2003 )
      RETURN
!
 2104 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1104) IERR
      CALL EXTCDE ( 1104 )
      RETURN
!
 2004 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1004) IERR
      CALL EXTCDE ( 2004 )
      RETURN
!
 2010 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1010) IERR
      CALL EXTCDE ( 2010 )
      RETURN
!
 2011 CONTINUE
! === no process number filtering for test file !!! ===
      WRITE (MDSE,1011) IERR
      CALL EXTCDE ( 2011 )
      RETURN
!
 2020 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1020) 
      CALL EXTCDE ( 2020 )
      RETURN
!
 2021 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1021) 
      CALL EXTCDE ( 2021 )
      RETURN
!
 2030 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1030) MNAMES(I), INAMES(I,J)
      CALL EXTCDE ( 2030 )
      RETURN
!
 2031 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1031) INAMES(I,J), J
      CALL EXTCDE ( 2031 )
      RETURN
!
!2050 CONTINUE
!     IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1040)
!     CALL EXTCDE ( 2050 )
!     RETURN
!
 2051 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1051) MN(:II)
      CALL EXTCDE ( 2051 )
      RETURN
!
 2052 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1052) J
      CALL EXTCDE ( 2052 )
      RETURN
!
 2053 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1053)
      CALL EXTCDE ( 2053 )
      RETURN
!
 2054 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1054)
      CALL EXTCDE ( 2054 )
      RETURN
!
 2055 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1055)
      CALL EXTCDE ( 2055 )
      RETURN
!
 2060 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1060)
      CALL EXTCDE ( 2060 )
      RETURN
!
 2061 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1061) IDTST, N_MOV
      CALL EXTCDE ( 2061 )
      RETURN
!
 2062 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1062) IDTST
      CALL EXTCDE ( 2062 )
      RETURN
!
 2070 CONTINUE
      IF ( IMPROC .EQ. NMPERR ) WRITE (MDSE,1070)
      CALL EXTCDE ( 2070 )
      RETURN
!
 2080 CONTINUE
      CALL EXTCDE ( 2080 )
      RETURN
!
! Formats
!
  900 FORMAT ( ' ========== STARTING MWW3 INITIALIZATION (WMINITNML) =', &
               '============================'/)
  901 FORMAT ( ' WAVEWATCH III log file            ',                 &
               '                     version ',A/                     &
               ' ==================================',                 &
               '==================================='/                 &
               ' multi-grid model driver                          ',  &
               'date : ',A10/50X,'time :  ',A8)
!
  910 FORMAT ( '  Opening input file ',A,' (unit number',I3,')')
  911 FORMAT ( '  Opening output file ',A,' (unit number',I3,')')
  912 FORMAT (/'  Comment character : ''',A,'''')
!
  920 FORMAT (/'  Number of grids          :',I3)
  921 FORMAT ( '  No input data grids.')
  922 FORMAT ( '  Input data grids         :',I3)
  923 FORMAT ( '  Single point output file : ',A)
 1923 FORMAT (/'  Output server type       :',I3)
 2923 FORMAT ( '  Single point output proc : ',A)
 3923 FORMAT ( '  Grids share output procs : ',A)
!
  924 FORMAT (/'  Input grid information : '/                         &
               '  nr extension  lev.   cur.   wind   ice    tau',     &
               '    rho    data'/    &
               ' ----------------------------------------------',     &
               '---------------')
  925 FORMAT (1X,I3,1X,A10,6(1X,A6),3(1X,A1))
  926 FORMAT ( ' ----------------------------------------------',     &
               '---------------')
!
  927 FORMAT (/'  Grid for point output : '/                          &
               '  nr extension  '/ ' ---------------')
  928 FORMAT (5X,A10)
  929 FORMAT ( ' ---------------')
!
  930 FORMAT (/'  Wave grid information : '/                          &
               '  nr extension  lev.   cur.   wind   ice    tau',     &
               '    rho    data   move1 rnk grp dmp'/                 &
               ' -----------------------------------------------',    &
               '-----------------------------------')
  931 FORMAT (1X,I3,1X,A10,6(1X,A6),3(1X,A1),2X,A4,2I4,3X,A1)
  932 FORMAT ( ' -----------------------------------------------',    &
               '-----------------------------------'/)
  933 FORMAT ( '  ',A,' : '/                                          &
               '  nr   grids (part of comm.)'/                        &
               ' -----------------------------------------------',    &
               '---------------------')
  934 FORMAT (A)
  935 FORMAT ( ' -----------------------------------------------',    &
               '---------------------'/)
  936 FORMAT (/'  ',A,' : '/                                          &
               '  nr   Depends on '/                                  &
               ' -----------------------------------------------',    &
               '---------------------')
  937 FORMAT ( '  ',A/)
  938 FORMAT (/'  ',A/)
!
  940 FORMAT (/'  Time interval : '/                                  &
               ' --------------------------------------------------')
  941 FORMAT ( '       Starting time : ',A)
  942 FORMAT ( '       Ending time   : ',A/)
  943 FORMAT (/'  Model settings : '/                                 &
               ' --------------------------------------------------')
  944 FORMAT ( '       Masking computation in nesting : ',A)
  945 FORMAT ( '       Masking output in nesting      : ',A/)
!
  950 FORMAT (/'  Output requests : (',A,') '/                        &
               ' ==================================================')
  951 FORMAT (/'       Type',I2,' : ',A/                              &
               '      -----------------------------------------')
  952 FORMAT ( '            From     : ',A)
  953 FORMAT ( '            To       : ',A)
  954 FORMAT ( '            Interval : ',A/)
  955 FORMAT ( '            Fields   : ',A)
  956 FORMAT ( '                       ',A)
  957 FORMAT ( '            Point  1 : ',2E14.6,2X,A)             
  958 FORMAT ( '              ',I6,' : ',2E14.6,2X,A)
  959 FORMAT ( '            No points defined')
  960 FORMAT ( '            The file with ',A,' data is ',A,'.')
  961 FORMAT ( '            IX fls   : ',3I6/                         &
               '            IY fls   : ',3I6)
  962 FORMAT (/'  Output request for model ',A,' (nr',I3,') '/        &
               ' ==================================================')
  963 FORMAT ( '            Output disabled')
!
  965 FORMAT (/'  Grid movement data (!/MGP, !/MGW): '/               &
               ' --------------------------------------------------')
  966 FORMAT ( '       ',A)
  967 FORMAT ( '       ',I6,2X,A)
  968 FORMAT ( '          ',I6,I11.8,I7.6,2F8.2)
!
  970 FORMAT(//'  Assigning resources : '/                            &
               ' --------------------------------------------------')
  971 FORMAT ( '       ',A)
  972 FORMAT ( '       Process ',I5.5,' reserved for all point output.')
  973 FORMAT ( '       Processes ',I5.5,' through ',I5.5,' [',I3,']', &
               ' reserved for output.')
  974 FORMAT (/                                                       &
        5X,'  grid           comp.      grd    pnt    trk    rst    bpt    prt'/     &
        5X,' ------------------------------------------------------', &
           '-------------')
  975 FORMAT (5X,'  ',A10,2X,I5.5,'-',I5.5,6(2x,A5))
  976 FORMAT(5X,' -------------------------------------------------', &
                '------------------')
  977 FORMAT (5X,'    Unified point output at ',I5.5)
 1974 FORMAT ('  Resource assignement (processes) : '/                &
           '  grid           comp.      grd    pnt    trk    rst    bpt    prt'/     &
           ' ------------------------------------------------------', &
           '-------------')
 1975 FORMAT ('  ',A10,2X,I5.5,'-',I5.5,6(2x,A5))
 1976 FORMAT (' ---------------------------------------------------', &
              '----------------')
 1977 FORMAT ('    Unified point output at ',I5.5)
!
  980 FORMAT(//'  Initializations :'/                                 &
               ' --------------------------------------------------')
  981 FORMAT ( '       Model number',I3,' [',A,']')
  982 FORMAT ( '          Initializing wave model ...')
  983 FORMAT ( '          Initializing model input ...')
  984 FORMAT ( '            ',A,': file not needed')
  985 FORMAT ( '            ',A,': file OK')
  986 FORMAT ( '       Unified point output [',A,']')
  987 FORMAT ( '          Initializing grids ...')
  988 FORMAT ( '       Input data grid',I3,' [',A,']')
!
#ifdef W3_MPRF
  990 FORMAT (1X,3F12.3,' WMINITNML',1X,A)
#endif
!
  998 FORMAT ( '  Running the model :'/                               &
               ' --------------------------------------------------'/)
  999 FORMAT ( ' ========== END OF MWW3 INITIALIZATION (WMINITNML) ===', &
               '============================'/)
!
 1003 FORMAT (/' *** WAVEWATCH III ERROR IN WMINITNML : *** '/         &
               '     PREMATURE END OF POINT FILE'/)
!
 1104 FORMAT (/' *** WAVEWATCH III ERROR IN WMINITNML : *** '/         &
               '     ERROR IN OPENING POINT FILE'/                     &
               '     IOSTAT =',I5/)
!
 1004 FORMAT (/' *** WAVEWATCH III ERROR IN WMINITNML : *** '/         &
               '     ERROR IN READING FROM POINT FILE'/                &
               '     IOSTAT =',I5/)
 1010 FORMAT (/' *** WAVEWATCH III ERROR IN WMINITNML : *** '/         &
               '     ERROR IN OPENING LOG FILE'/                       &
               '     IOSTAT =',I5/)
 1011 FORMAT (/' *** WAVEWATCH III ERROR IN WMINITNML : *** '/         &
               '     ERROR IN OPENING TEST FILE'/                      &
               '     IOSTAT =',I5/)
 1020 FORMAT (/' *** WAVEWATCH III ERROR IN WMINITNML : *** '/         &
               '     ILLEGAL NUMBER OF GRIDS ( < 1 ) '/)
 1021 FORMAT (/' *** WAVEWATCH III ERROR IN WMINITNML : *** '/         &
               '     ILLEGAL NUMBER OF INPUT GRIDS ( < 0 ) '/)
 1030 FORMAT (/' *** WAVEWATCH III ERROR IN WMINITNML : *** '/         &
               '     INPUT GRID NAME NOT FOUND '/                      &
               '     WAVE GRID  : ',A/                                 &
               '     INPUT NAME : ',A/)
 1031 FORMAT (/' *** WAVEWATCH III ERROR IN WMINITNML : *** '/         &
               '     REQUESTED INPUT TYPE NOT FOUND IN INPUT GRID '/   &
               '     INPUT GRID : ',A/                                 &
               '     INPUT TYPE : ',I8/)
 1032 FORMAT (/' *** WAVEWATCH III WARNING IN WMINITNML : *** '/       &
               '     INPUT GRID ',A,' NOT USED '/)
 1040 FORMAT ( ' *** WAVEWATCH III WARNING IN W3MLTI : ***'/           &
               '     POSSIBLE LOAD IMBALANCE GROUP',I3,' :',2I6/)
!1040 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/             &
!              '     ILLEGAL TIME INTERVAL'/)
 1050 FORMAT (/' *** WAVEWATCH III WARNING IN W3MLTI : ***'/           &
               '     UNIFIED POINT OUTPUT BUT NO OUTPUT'/              &
               '     UNIFIED POINT OUTPUT DISABLED'/)
 1051 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/             &
               '     ILLEGAL MODEL ID [',A,']'/)
 1052 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/             &
               '     ILLEGAL OUTPUT TYPE',I10/)
 1053 FORMAT (/' *** WAVEWATCH III WARNING IN W3MLTI : ***'/           &
         '     OUTPUT POINTS FOR INDIVIDUAL GRIDS CANNOT BE DEFINED'/  &
               '     WHEN UNIFIED POINT OUTPUT IS REQUESTED'/)
 1054 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/             &
         '     POINT OUTPUT ACTIVATED, BUT NO POINTS DEFINED'/)
 1055 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/             &
         '     POINT OUTPUT ACTIVATED, BUT NO FILE DEFINED'/)
 1060 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/             &
               '     NO MOVING GRID DATA PRESENT'/)
 1061 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/             &
               '     TOO MANY HOMOGENEOUS FIELDS : ',A,1X,I4/)
 1062 FORMAT (/' *** WAVEWATCH III ERROR IN W3MLTI : ***'/             &
               '     HOMOGENEOUS NAME NOT RECOGNIZED : ', A/)
 1070 FORMAT (/' *** WAVEWATCH III ERROR IN WMINITNML : ***'/          &
               '     ALL GRIDS ARE NOT USING THE SAME COORDINATE SYSTEM'/)
 1080 FORMAT (/' *** BOUNDARY DATA READ, WILL NOT DUMP, GRID :',I4,    &
               ' ***')
 1081 FORMAT (/' *** NO BOUNDARY DATA TO DUMP, GRID :',I4,' ***')
 1082 FORMAT ( '  No boundary data dump for grid',I3/)
!
#ifdef W3_T
 9000 FORMAT ( ' TEST WMINITNML : UNIT NUMBERS    : ',5I6/          &
               '               INPUT FILE NAME : ',A)
#endif
!
#ifdef W3_T
 9020 FORMAT ( ' TEST WMINITNML : UNIT NUMBERS FOR GRIDS (',A,')'/  &
                               15X,'GRID MDS(1-13)',43X,'NTRACE')
 9021 FORMAT (14X,16I4)
 9022 FORMAT ( ' TEST WMINITNML : UNIT NUMBERS FOR INTPUT FILES'/   &
                               15X,'GRID MDSF(JFIRST-9)')
 9030 FORMAT ( ' TEST WMINITNML : FILE EXTENSIONS, INPUT FLAGS,',   &
               ' RANK AND GROUP, PROC RANGE')
 9031 FORMAT ( '            ',I3,1X,A,20L2,2I4,2F6.2)
 9032 FORMAT ( ' TEST WMINITNML : PROCESSED RANK NUMBERS')
 9033 FORMAT ( '             ',I3,1X,A,1X,I4)
 9034 FORMAT ( ' TEST WMINITNML : NUMBER OF GROUPS :',I4)
 9035 FORMAT ( ' TEST WMINITNML : SIZE OF GROUPS :',20I3)
 9036 FORMAT ( ' TEST WMINITNML : GROUP SIZE AND COMPONENTS :')
 9037 FORMAT ( '             ',2I3,':',20I3)
#endif
!
#ifdef W3_T
 9050 FORMAT ( ' TEST WMINITNML : GRID NUMBER',I3,' =================')
 9051 FORMAT ( ' TEST WMINITNML : ODAT   : ',I9.8,I7.6,I7,I9.8,I7.6,  &
                                  5(/24X,I9.8,I7.6,I7,I9.8,I7.6) )
 9052 FORMAT ( ' TEST WMINITNML : FLGRD  : ',5(5L2,1X)/24X,5(5L2,1X))
#endif
!
#ifdef W3_T
 9060 FORMAT ( ' TEST WMINITNML : GRID MOVEMENT DATA')
 9061 FORMAT ( '             ',I8.8,I7,1X,2F8.2)
#endif
!
#ifdef W3_T
 9070 FORMAT ( ' TEST WMINITNML : ALLPRC ')
 9071 FORMAT ( ' ',I3,'  : ',250I3)
 8042 FORMAT ( ' TEST WMINITNML : MODMAP ')
 8043 FORMAT ( ' TEST WMINITNML : LOADMP ')
 8044 FORMAT ( '        ',I3,'  : ',250I2)
#endif
!
#ifdef W3_T
 9080 FORMAT ( ' TEST WMINITNML : MODEL INITIALIZATION')
 9081 FORMAT ( '               MODEL AND TIME   :',I4,I10.8,I8.6)
 9082 FORMAT ( '               STATUS AND TIMES :',I4,3(I10.8,I8.6))
 9083 FORMAT ( ' TEST WMINITNML : IDINP AFTER INITIALIZATION :')
 9084 FORMAT ( '               ',I4,17(2X,A3))
#endif
!/
!/ End of WMINITNML ----------------------------------------------------- /
!/
      END SUBROUTINE WMINITNML





!/
!/ End of module WMINITMD -------------------------------------------- /
!/
      END MODULE WMINITMD
