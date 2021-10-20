#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3WAVEMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    04-Feb-2000 : Origination.                        ( version 2.00 )
!/                  For upgrades see subroutines.
!/    14-Feb-2000 : Exact-NL added.                     ( version 2.01 )
!/    05-Jan-2001 : Bug fix to allow model to run       ( version 2.05 )
!/                  without output.
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    09-Feb-2001 : Third propagation scheme added.     ( version 2.08 )
!/    23-Feb-2001 : Check for barrier after source
!/                  terms added ( W3NMIN ).     ( delayed version 2.07 )
!/    16-Mar-2001 : Fourth propagation scheme added.    ( version 2.09 )
!/    30-Mar-2001 : Sub-grid obstacles added.           ( version 2.10 )
!/    23-May-2001 : Clean up and bug fixes.             ( version 2.11 )
!/    10-Dec-2001 : Sub-grid obstacles for UQ schemes.  ( version 2.14 )
!/    11-Jan-2002 : Sub-grid ice.                       ( version 2.15 )
!/    24-Jan-2002 : Zero time step dor data ass.        ( version 2.17 )
!/    18-Feb-2002 : Point output diagnostics added.     ( version 2.18 )
!/    30-Apr-2002 : Add field output types 17-18.       ( version 2.20 )
!/    09-May-2002 : Switch clean up.                    ( version 2.21 )
!/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
!/    26-Dec-2002 : Moving grid version.                ( version 3.02 )
!/    01-Aug-2003 : Moving grid GSE correction.         ( version 3.03 )
!/    20-Aug-2003 : Output server options added.        ( version 3.04 )
!/    07-Oct-2003 : Output options for NN training.     ( version 3.05 )
!/    29-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/                  W3INIT, W3MPII-O and WWVER moved to w3initmd.ftn
!/    04-Feb-2005 : Add STAMP to par list of W3WAVE.    ( version 3.07 )
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    28-Jun-2005 : Adding map recalc for W3ULEV call.  ( version 3.07 )
!/    07-Sep-2005 : Updated boundary conditions.        ( version 3.08 )
!/                  Fix NRQSG1/2 = 0 array bound issue.
!/    13-Jun-2006 : Split STORE in G/SSTORE             ( version 3.09 )
!/    26-Jun-2006 : Add output type 6.                  ( version 3.09 )
!/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    18-Oct-2006 : Partitioned spectral data output.   ( version 3.10 )
!/    02-Feb-2007 : Add FLAGST test.                    ( version 3.10 )
!/    02-Apr-2007 : Add partitioned field data.         ( version 3.11 )
!/    07-May-2007 : Bug fix SKIP_O treatment.           ( version 3.11 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    08-Oct-2007 : Adding AS CX-Y to W3SRCE par. list. ( version 3.13 )
!/    22-Feb-2008 : Initialize VGX-Y properly.          ( version 3.13 )
!/    10-Apr-2008 : Bug fix writing log file (MPI).     ( version 3.13 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    29-Mar-2010 : Adding coupling, ice in W3SRCE.     ( version 3.14_SHOM )
!/    16-May-2010 : Adding transparencies in W3SCRE     ( version 3.14_SHOM )
!/    23-Jun-2011 : Movable bed bottom friction BT4     ( version 4.04 )
!/    03-Nov-2011 : Shoreline reflection on unst. grids ( version 4.04 )
!/    02-Jul-2011 : Update for PALM coupling            ( version 4.07 )
!/    06-Mar-2012 : Initializing ITEST as needed.       ( version 4.07 )
!/    02-Jul-2012 : Update for PALM coupling            ( version 4.07 )
!/    02-Sep-2012 : Clean up of open BC for UG grids    ( version 4.08 )
!/    03-Sep-2012 : Fix format 902.                     ( version 4.10 )
!/    07-Dec-2012 : Wrap W3SRCE with TMPn to limit WARN ( version 4.OF )
!/    10-Dec-2012 : Modify field output MPI for new     ( version 4.OF )
!/                  structure and smaller memory footprint.
!/    12-Dec-2012 : Adding SMC grid.  JG_Li             ( version 4.08 )
!/    26-Dec-2012 : Move FIELD init. to W3GATH.         ( version 4.OF )
!/    16-Sep-2013 : Add Arctic part for SMC grid.       ( version 4.11 )
!/    11-Nov-2013 : SMC and rotated grid incorporated in the main
!/                  trunk                               ( version 4.13 )
!/    14-Nov-2013 : Remove orphaned work arrays.        ( version 4.13 )
!/    27-Nov-2013 : Fixes for OpenMP versions.          ( version 4.15 )
!/    23-May-2014 : Adding ice fluxes to W3SRCE         ( version 5.01 )
!/    27-May-2014 : Move to OMPG/X switch.              ( version 5.02 )
!/    24-Apr-2015 : Adding OASIS coupling calls         ( version 5.07 )
!/                  (M. Accensi & F. Ardhuin, IFREMER)
!/    27-Aug-2015 : Update for ICEH, ICEF               ( version 5.08 )
!/    14-Sep-2018 : Remove PALM implementation          ( version 6.06 )
!/    15-Sep-2020 : Bugfix FIELD allocation. Remove     ( version 7.11 )
!/                  defunct OMPX switches.
!/    22-Mar-2021 : Update TAUA, RHOA                   ( version 7.13 )
!/    06-May-2021 : Use ARCTC and SMCTYPE options. JGLi ( version 7.13 )
!/    19-Jul-2021 : Momentum and air density support    ( version 7.xx )
!/
!/    Copyright 2009-2014 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!  2. Variables and types :
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3WAVE    Subr. Public   Actual wave model.
!      W3GATH    Subr. Public   Data transpose before propagation.
!      W3SCAT    Subr. Public   Data transpose after propagation.
!      W3NMIN    Subr. Public   Calculate minimum number of sea
!                               points per processor.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETx    Subr. W3xDATMD Point to data structure.
!
!      W3UCUR    Subr. W3UPDTMD Interpolate current fields in time.
!      W3UWND    Subr. W3UPDTMD Interpolate wind fields in time.
!      W3UINI    Subr. W3UPDTMD Update initial conditions if init.
!                               with initial wind conditions.
!      W3UBPT    Subr. W3UPDTMD Update boundary points.
!      W3UICE    Subr. W3UPDTMD Update ice coverage.
!      W3ULEV    Subr. W3UPDTMD Transform the wavenumber grid.
!      W3DDXY    Subr. W3UPDTMD Calculate dirivatives of the depth.
!      W3DCXY    Subr. W3UPDTMD Calculate dirivatives of the current.
!
!      W3MAPn    Subr. W3PROnMD Preparation for  ropagation schemes.
!      W3XYPn    Subr. W3PROnMD Longitude-latitude ("XY") propagation.
!      W3KTPn    Subr. W3PROnMD Intra-spectral ("k-theta") propagation.
!
!      W3SRCE    Subr. W3SRCEMD Source term integration and calculation.
!
!      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
!      W3OUTG    Subr. W3IOGOMD Generate gridded output fields.
!      W3IOGO    Subr. W3IOGOMD Read/write gridded output.
!      W3IOPE    Subr. W3IOPOMD Extract point output.
!      W3IOPO    Subr. W3IOPOMD Read/write point output.
!      W3IOTR    Subr. W3IOTRMD Process spectral output along tracks.
!      W3IORS    Subr. W3IORSMD Read/write restart files.
!      W3IOBC    Subr. W3IOBCMD Read/write boundary conditions.
!      W3CPRT    Subr. W3IOSFMD Partition spectra.
!      W3IOSF    Subr.   Id.    Write partitioned spectral data.
!
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      WWTIME    Subr.   Id.    System time in readable format.
!      EXTCDE    Subr.   Id.    Program abort.
!
!      TICK21    Subr. W3TIMEMD Advance the clock.
!      DSEC21    Func.   Id.    Difference between times.
!      STME21    Subr.   Id.    Time in readable format.
!
!      MPI_BARRIER, MPI_STARTALL, MPI_WAITALL
!                Subr.          Basic MPI routines.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!  6. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/DIST  Id.
!       !/MPI   Id.
!       !/OMPG  Id.
!
!       !/PR1   First order propagation schemes.
!       !/PR2   ULTIMATE QUICKEST scheme.
!       !/PR3   Averaged ULTIMATE QUICKEST scheme.
!       !/SMC   UNO2 scheme on SMC grid.
!
!       !/S     Enable subroutine tracing.
!       !/T     Test output.
!       !/MPIT  Test output for MPI specific code.
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_MPI
      USE W3ADATMD, ONLY: MPIBUF
#endif
!
      PUBLIC
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3WAVE ( IMOD, ODAT, TEND, STAMP, NO_OUT &
#ifdef W3_OASIS
                  ,ID_LCOMM, TIMEN                 &
#endif
                         )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    17-Mar-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    05-Jan-2001 : Bug fix to allow model to run       ( version 2.05 )
!/                  without output.
!/    24-Jan-2001 : Flat grid version.                  ( version 2.06 )
!/    09-Feb-2001 : Third propagation scheme added.     ( version 2.08 )
!/    23-Feb-2001 : Check for barrier after source
!/                  terms added ( W3NMIN ).     ( delayed version 2.07 )
!/    16-Mar-2001 : Fourth propagation scheme added.    ( version 2.09 )
!/    30-Mar-2001 : Sub-grid obstacles added.           ( version 2.10 )
!/    23-May-2001 : Barrier added for dry run, changed  ( version 2.10 )
!/                  declaration of FLIWND.
!/    10-Dec-2001 : Sub-grid obstacles for UQ schemes.  ( version 2.14 )
!/    11-Jan-2002 : Sub-grid ice.                       ( version 2.15 )
!/    24-Jan-2002 : Zero time step dor data ass.        ( version 2.17 )
!/    09-May-2002 : Switch clean up.                    ( version 2.21 )
!/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
!/    26-Dec-2002 : Moving grid version.                ( version 3.02 )
!/    01-Aug-2003 : Moving grid GSE correction.         ( version 3.03 )
!/    07-Oct-2003 : Output options for NN training.     ( version 3.05 )
!/    29-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    04-Feb-2005 : Add STAMP to par list.              ( version 3.07 )
!/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
!/    28-Jun-2005 : Adding map recalc for W3ULEV call.  ( version 3.07 )
!/    07-Sep-2005 : Updated boundary conditions.        ( version 3.08 )
!/    26-Jun-2006 : Add output type 6.                  ( version 3.09 )
!/    04-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    18-Oct-2006 : Partitioned spectral data output.   ( version 3.10 )
!/    02-Feb-2007 : Add FLAGST test.                    ( version 3.10 )
!/    02-Apr-2007 : Add partitioned field data.         ( version 3.11 )
!/                  Improve MPI_WAITALL call tests/allocations.
!/    07-May-2007 : Bug fix SKIP_O treatment.           ( version 3.11 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    08-Oct-2007 : Adding AS CX-Y to W3SRCE par. list. ( version 3.13 )
!/    22-Feb-2008 : Initialize VGX-Y properly.          ( version 3.13 )
!/    10-Apr-2008 : Bug fix writing log file (MPI).     ( version 3.13 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    31-Mar-2010 : Add reflections                     ( version 3.14.4 )
!/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.4 )
!/                  (A. Roland and F. Ardhuin)
!/    06-Mar-2011 : Output of max. CFL (F.Ardhuin)      ( version 3.14.4 )
!/    05-Apr-2011 : Implement iteration for DTMAX <1s   ( version 3.14.4 )
!/    02-Jul-2012 : Update for PALM coupling            ( version 4.07 )
!/    02-Sep-2012 : Clean up of open BC for UG grids    ( version 4.08 )
!/    03-Sep-2012 : Fix format 902.                     ( version 4.10 )
!/    10-Dec-2012 : Modify field output MPI for new     ( version 4.OF )
!/                  structure and smaller memory footprint.
!/    16-Nov-2013 : Allows reflection on curvi. grids   ( version 4.13 )
!/    27-Nov-2013 : Fixes for OpenMP versions.          ( version 4.15 )
!/    23-May-2014 : Adding ice fluxes to W3SRCE         ( version 5.01 )
!/    27-May-2014 : Move to OMPG/X switch.              ( version 5.02 )
!/    24-Apr-2015 : Adding OASIS coupling calls         ( version 5.07 )
!/                  (M. Accensi & F. Ardhuin, IFREMER)
!/    27-Aug-2015 : Update for ICEH, ICEF               ( version 5.10 )
!/    31-Mar-2016 : Current option for smc grid.        ( version 5.18 )
!/    06-Jun-2018 : Add PDLIB/MEMCHECK/SETUP/NETCDF_QAD/TIMING
!/                  OASIS/DEBUGINIT/DEBUGSRC/DEBUGRUN/DEBUGCOH
!/                  DEBUGIOBP/DEBUGIOBC                 ( version 6.04 )
!/    14-Sep-2018 : Remove PALM implementation          ( version 6.06 )
!/    25-Sep-2020 : Oasis coupling at T+0               ( version 7.10 )
!/    22-Mar-2021 : Update TAUA, RHOA                   ( version 7.13 )
!/    06-May-2021 : Use ARCTC and SMCTYPE options. JGLi ( version 7.13 )
!/
!  1. Purpose :
!
!     Run WAVEWATCH III for a given time interval.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number.
!       TEND    I.A.   I   Ending time of integration.
!       STAMP   Log.   I   WRITE(*,*)time stamp (optional, defaults to T).
!       NO_OUT  Log.   I   Skip output (optional, defaults to F).
!                          Skip at ending time only!
!     ----------------------------------------------------------------
!
!     Local parameters : Flags
!     ----------------------------------------------------------------
!       FLOUTG  Log.  Flag for running W3OUTG.
!       FLPART  Log.  Flag for running W3CPRT.
!       FLZERO  Log.  Flag for zero time interval.
!       FLAG0   Log.  Flag for processors without tasks.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!     Any program shell or integrated model which uses WAVEWATCH III.
!
!  6. Error messages :
!
!  7. Remarks :
!
!     - Currents are updated before winds as currents are used in wind
!       and USTAR processing.
!     - Ice and water levels can be updated only once per call.
!     - If ice or water level time are undefined, the update
!       takes place asap, otherwise around the "half-way point"
!       betweem the old and new times.
!     - To increase accuracy, the calculation of the intra-spectral
!       propagation is performed in two parts around the spatial propagation.
!
!  8. Structure :
!
!     -----------------------------------------------------------
!       0.  Initializations
!         a Point to data structures
!         b Subroutine tracing
!         c Local parameter initialization
!         d Test output
!       1.  Check the consistency of the input.
!         a Ending time versus initial time.
!         b Water level time.
!         c Current time interval.
!         d Wind time interval.
!         e Ice time.
!       2.  Determine next time from ending and output
!           time and get corresponding time step.
!       3.  Loop over time steps (see below).
!       4.  Perform output to file if requested.
!         a Check if time is output time.
!         b Processing and MPP preparations.  ( W3CPRT, W3OUTG )
!         c Reset next output time.
!        -------------- loop over output types ------------------
!         d Perform output.                           ( W3IOxx )
!         e Update next output time.
!        -------------------- end loop --------------------------
!       5.  Update log file.
!       6.  If time is not ending time, branch back to 2.
!     -----------------------------------------------------------
!
!      Section 3.
!     ----------------------------------------------------------
!       3.1  Interpolate winds and currents. ( W3UCUR, W3DCXY )
!                                                    ( W3UWND )
!                                                    ( W3UINI )
!       3.2  Update boundary conditions.     ( W3IOBC, W3UBPT )
!       3.3  Update ice coverage (if new ice map).   ( W3UICE )
!       3.4  Transform grid (if new water level).    ( W3ULEV )
!       3.5  Update maps and dirivatives.    ( W3MAPn, W3DDXY )
!                                            ( W3NMIN, W3UTRN )
!            Update grid advection vector.
!       3.6  Perform propagation
!          a Preparations.
!          b Intra spectral part 1.                  ( W3KTPn )
!          c Longitude-latitude       ( W3GATH, W3XYPn W3SCAT )
!          b Intra spectral part 2.                  ( W3KTPn )
!       3.7  Calculate and integrate source terms.   ( W3SRCE )
!       3.8  Update global time step.
!     ----------------------------------------------------------
!
!  9. Switches :
!
!     See module documentation.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
!/
      USE W3GDATMD
      USE W3WDATMD
      USE W3ADATMD
      USE W3IDATMD
      USE W3ODATMD
!/
      USE W3UPDTMD
      USE W3SRCEMD
#ifdef W3_PR1
      USE W3PRO1MD
#endif
#ifdef W3_PR2
      USE W3PRO2MD
#endif
#ifdef W3_PR3
      USE W3PRO3MD
#endif
#ifdef W3_SMC
      USE W3PSMCMD
#endif
!
#ifdef W3_PR1
      USE W3PROFSMD
#endif
#ifdef W3_PR2
      USE W3PROFSMD
#endif
#ifdef W3_PR3
      USE W3PROFSMD
#endif
!/
      USE W3TRIAMD
      USE W3IOGRMD
      USE W3IOGOMD
      USE W3IOPOMD
      USE W3IOTRMD
      USE W3IORSMD
      USE W3IOBCMD
      USE W3IOSFMD
#ifdef W3_PDLIB
      USE PDLIB_W3PROFSMD, only : APPLY_BOUNDARY_CONDITION_VA
      USE PDLIB_W3PROFSMD, only : PDLIB_W3XYPUG, PDLIB_W3XYPUG_BLOCK_IMPLICIT, PDLIB_W3XYPUG_BLOCK_EXPLICIT
      USE PDLIB_W3PROFSMD, only : ALL_VA_INTEGRAL_PRINT, ALL_VAOLD_INTEGRAL_PRINT, ALL_FIELD_INTEGRAL_PRINT
      USE W3PARALL, only : PDLIB_NSEAL, PDLIB_NSEALM
      USE yowNodepool, only: npa, iplg
#endif
!/
      USE W3SERVMD
      USE W3TIMEMD
#ifdef W3_IC3
      USE W3SIC3MD
#endif
#ifdef W3_IS2
      USE W3SIS2MD
#endif
#ifdef W3_UOST
      USE W3UOSTMD, ONLY: UOST_SETGRID
#endif
      USE W3PARALL, ONLY : INIT_GET_ISEA
#ifdef W3_MEMCHECK
      USE MallocInfo_m
#endif
#ifdef W3_SETUP
      USE W3WAVSET, only : WAVE_SETUP_COMPUTATION
#endif
!/NETCDF_QAD      USE W3NETCDF, only : OUTPUT_NETCDF_QUICK_AND_DIRTY

#ifdef W3_OASIS
      USE W3OACPMD, ONLY: ID_OASIS_TIME, CPLT0
#endif
#ifdef W3_OASOCM
      USE W3OGCMMD, ONLY: SND_FIELDS_TO_OCEAN
#endif
#ifdef W3_OASACM
      USE W3AGCMMD, ONLY: SND_FIELDS_TO_ATMOS
#endif
#ifdef W3_OASICM
      USE W3IGCMMD, ONLY: SND_FIELDS_TO_ICE
#endif

#ifdef W3_PDLIB
      USE PDLIB_FIELD_VEC, only : DO_OUTPUT_EXCHANGES
#endif
#ifdef W3_TIMINGS
    USE W3PARALL, only : PRINT_MY_TIME
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
      INTEGER, INTENT(IN)           :: IMOD, TEND(2),ODAT(35)
      LOGICAL, INTENT(IN), OPTIONAL :: STAMP, NO_OUT
#ifdef W3_OASIS
 INTEGER, INTENT(IN), OPTIONAL :: ID_LCOMM
 INTEGER, INTENT(IN), OPTIONAL :: TIMEN(2)
#endif
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters :
!/
#ifdef W3_T
      INTEGER                 :: ILEN
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      INTEGER                 :: IP
      INTEGER                 :: TCALC(2), IT, IT0, NT, ITEST,        &
                                 ITLOC, ITLOCH, NTLOC, ISEA, JSEA,    &
                                 IX, IY, ISPEC, J, TOUT(2), TLST(2),  &
                                 REFLED(6), IK, ITH, IS, NKCFL
      INTEGER                 :: ISP, IP_glob
      INTEGER                 :: TTEST(2),DTTEST
      REAL                    :: ICEDAVE
!
#ifdef W3_MPI
      LOGICAL                 :: SBSED
#endif
#ifdef W3_SEC1
      INTEGER                 :: ISEC1
#endif
#ifdef W3_SBS
      INTEGER                 :: JJ, NDSOFLG
#endif
#ifdef W3_MPI
      INTEGER                 :: IERR_MPI, NRQMAX
      INTEGER, ALLOCATABLE    :: STATCO(:,:), STATIO(:,:)
#endif
      INTEGER                 :: IXrel
      REAL                    :: DTTST, DTTST1, DTTST2, DTTST3,       &
                                 DTL0, DTI0, DTR0, DTI10, DTI50,      &
                                 DTGA, DTG, DTGpre, DTRES,            &
                                 FAC, VGX, VGY, FACK, FACTH,          &
                                 FACX, XXX, REFLEC(4),                &
                                 DELX, DELY, DELA, DEPTH, D50, PSIC
     REAL                     :: VSioDummy(NSPEC), VDioDummy(NSPEC), VAoldDummy(NSPEC)
     LOGICAL                  :: SHAVETOTioDummy
#ifdef W3_SEC1
     REAL                    :: DTGTEMP
#endif
!
      REAL, ALLOCATABLE       :: FIELD(:)
      REAL                    :: TMP1(4), TMP2(3), TMP3(2), TMP4(2)
#ifdef W3_IC3
 REAL, ALLOCATABLE       :: WN_I(:)
#endif
#ifdef W3_REFRX
 REAL, ALLOCATABLE       :: CIK(:)
#endif
!
! Orphaned arrays from old data structure
!
      REAL, ALLOCATABLE       :: TAUWX(:), TAUWY(:)
!
      LOGICAL                 :: FLACT, FLZERO, FLFRST, FLMAP, TSTAMP,&
                                 SKIP_O, FLAG_O, FLDDIR, READBC,      &
                                 FLAG0 = .FALSE., FLOUTG, FLPFLD,     &
                                 FLPART, LOCAL, FLOUTG2
!
#ifdef W3_MPI
      LOGICAL                 :: FLGMPI(0:8)
#endif
#ifdef W3_IC3
      REAL                    :: FIXEDVISC,FIXEDDENS,FIXEDELAS
      REAL                    :: USE_CHENG, USE_CGICE, HICE
#endif
      LOGICAL                 :: UGDTUPDATE    ! true if time step should be updated for UG schemes
      CHARACTER(LEN=8)        :: STTIME
      CHARACTER(LEN=21)       :: IDACT
      CHARACTER(LEN=13)       :: OUTID
      CHARACTER(LEN=23)       :: IDTIME
      INTEGER eIOBP
      INTEGER ITH_F
#ifdef W3_PDLIB
     REAL ::             VS_SPEC(NSPEC)
     REAL ::             VD_SPEC(NSPEC)
#endif

!
#ifdef W3_SBS
      CHARACTER(LEN=30)       :: FOUTNAME
#endif
!
#ifdef W3_T
     REAL             :: INDSORT(NSEA), DTCFL1(NSEA)
#endif
!/
#ifdef W3_SMC
  !Li   Temperature spectra for Arctic boundary update.
      REAL, ALLOCATABLE       :: BACSPEC(:)
      REAL                    :: BACANGL

#endif

!/ ------------------------------------------------------------------- /
! 0.  Initializations
!
! 0.a Set pointers to data structure
!
#ifdef W3_COU
      SCREEN   =  333
#endif
!
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3WAVE, step 1'
#endif
#ifdef W3_DEBUGSRC
      WRITE(740+IAPROC,*) 'Step 1 : max(UST)=', maxval(UST)
#endif
#ifdef W3_DEBUGINIT
      FLUSH(740+IAPROC)
#endif
      IF ( IOUTP  .NE. IMOD ) CALL W3SETO ( IMOD, NDSE, NDST )
      IF ( IGRID  .NE. IMOD ) CALL W3SETG ( IMOD, NDSE, NDST )
      IF ( IWDATA .NE. IMOD ) CALL W3SETW ( IMOD, NDSE, NDST )
      IF ( IADATA .NE. IMOD ) CALL W3SETA ( IMOD, NDSE, NDST )
      IF ( IIDATA .NE. IMOD ) CALL W3SETI ( IMOD, NDSE, NDST )
#ifdef W3_UOST
     CALL UOST_SETGRID(IMOD)
#endif

#ifdef W3_DEBUGRUN
      DO JSEA = 1, NSEAL
        DO IS = 1, NSPEC
          IF (VA(IS, JSEA) .LT. 0.) THEN
            WRITE(740+IAPROC,*) 'NEGATIVE ACTION 1', IS, JSEA, VA(IS,JSEA)
            CALL FLUSH(740+IAPROC)
            CALL EXTCDE(666)
          ENDIF
        ENDDO
      ENDDO
      IF (SUM(VA) .NE. SUM(VA)) THEN
        WRITE(740+IAPROC,*) 'NAN in ACTION 1', SUM(VA)
        CALL FLUSH(740+IAPROC)
        CALL EXTCDE(666)
      ENDIF
#endif


#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3WAVEMD, step 1")
#endif
#ifdef W3_DEBUGIOBP
         IF (NX .ge. 10210) WRITE(*,*) 'CRIT 1:', MAPSTA(1,10210), IOBP(10210)
#endif
#endif

!
      ALLOCATE(TAUWX(NSEAL), TAUWY(NSEAL))
#ifdef W3_REFRX
      ALLOCATE(CIK(NSEAL))
#endif
!
      IF ( PRESENT(STAMP) ) THEN
          TSTAMP = STAMP
        ELSE
          TSTAMP = .TRUE.
        END IF
!
      IF ( PRESENT(NO_OUT) ) THEN
          SKIP_O = NO_OUT
        ELSE
          SKIP_O = .FALSE.
        END IF
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3WAVE, step 2'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3WAVEMD, step 2")
#endif
#endif
!
! 0.b Subroutine tracing
!
#ifdef W3_S
      CALL STRACE (IENT, 'W3WAVE')
#endif
!
!
! 0.c Local parameter initialization
!
      IPASS  = IPASS + 1
      IDACT  = '                 '
      OUTID  = '           '
      FLACT  = ITIME .EQ. 0
      FLMAP  = ITIME .EQ. 0
      FLDDIR = ITIME .EQ. 0 .AND. ( FLCTH .OR. FSREFRACTION        &
          .OR. FLCK .OR. FSFREQSHIFT )
!
      FLPFLD = .FALSE.
      DO J=1,NOGE(4)
        FLPFLD = FLPFLD .OR. FLOGRD(4,J) .OR. FLOGR2(4,J)
        END DO
!
      IF ( IAPROC .EQ. NAPLOG ) BACKSPACE ( NDSO )
!
      IF ( FLCOLD ) THEN
          DTDYN = 0.
          FCUT  = SIG(NK) * TPIINV
        END IF
!
      IF( GTYPE .EQ. SMCTYPE ) THEN
             J = 1
#ifdef W3_SMC
 !!Li   Use sea point only field for SMC grid.
        ALLOCATE ( FIELD(NCel) )
#endif
      ELSE
             ALLOCATE ( FIELD(1-NY:NY*(NX+2)) )
      ENDIF
!
      LOCAL   = IAPROC .LE. NAPROC
      UGDTUPDATE = .FALSE.
      IF (FLAGLL) THEN
        FACX   =  1./(DERA * RADIUS)
      ELSE
        FACX   =  1.
        END IF
!
#ifdef W3_SBS
      NDSOFLG = 99
#endif
#ifdef W3_MPI
      SBSED = .FALSE.
#endif
#ifdef W3_SBS
      SBSED = .TRUE.
#endif
!
      TAUWX  = 0.
      TAUWY  = 0.
!
! 0.d Test output
!
#ifdef W3_T
      ILEN   = LEN_TRIM(FILEXT)
      WRITE (NDST,9000) IMOD, FILEXT(:ILEN), TEND
#endif
!
! 1.  Check the consistency of the input ----------------------------- /
! 1.a Ending time versus initial time
!
      DTTST  = DSEC21 ( TIME , TEND )
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) '1 : DTTST=', DTTST, TIME, TEND
#endif
      FLZERO = DTTST .EQ. 0.
#ifdef W3_T
      WRITE (NDST,9010) DTTST, FLZERO
#endif
      IF ( DTTST .LT. 0. ) THEN
          IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000)
          CALL EXTCDE ( 1 )
        END IF
!
! 1.b Water level time
!
      IF ( FLLEV ) THEN
          IF ( TLEV(1) .GE. 0. ) THEN
              DTL0   = DSEC21 ( TLEV , TLN )
            ELSE
              DTL0   = 1.
            END IF
#ifdef W3_T
          WRITE (NDST,9011) DTL0
#endif
          IF ( DTL0 .LT. 0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001)
              CALL EXTCDE ( 2 )
            END IF
        ELSE
          DTL0   = 0.
        END IF
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3WAVE, step 4'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3WAVEMD, step 4")
#endif
#endif
!
! 1.c Current interval
!
      IF ( FLCUR ) THEN
          DTTST1 = DSEC21 ( TC0 , TCN )
          DTTST2 = DSEC21 ( TC0 , TIME )
          DTTST3 = DSEC21 ( TEND , TCN )
#ifdef W3_T
          WRITE (NDST,9012) DTTST1, DTTST2, DTTST3
#endif
          IF ( DTTST1.LT.0. .OR. DTTST2.LT.0. .OR. DTTST3.LT.0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002)
              CALL EXTCDE ( 3 )
            END IF
          IF ( DTTST2.EQ.0..AND. ITIME.EQ.0 ) THEN
              IDACT(7:7) = 'F'
              TOFRST = TIME
            END IF
        END IF
!
! 1.d Wind interval
!
      IF ( FLWIND ) THEN
          DTTST1 = DSEC21 ( TW0 , TWN )
          DTTST2 = DSEC21 ( TW0 , TIME )
          DTTST3 = DSEC21 ( TEND , TWN )
#ifdef W3_T
          WRITE (NDST,9013) DTTST1, DTTST2, DTTST3
#endif
          IF ( DTTST1.LT.0. .OR. DTTST2.LT.0. .OR. DTTST3.LT.0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1003)
              CALL EXTCDE ( 4 )
            END IF
          IF ( DTTST2.EQ.0..AND. ITIME.EQ.0 ) THEN
              IDACT(3:3) = 'F'
              TOFRST = TIME
            END IF
        END IF
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3WAVE, step 5'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3WAVEMD, step 5")
#endif
#endif
!
! 1.e Ice concentration interval
!
      IF ( FLICE ) THEN
          IF ( TICE(1) .GE. 0 ) THEN
              DTI0   = DSEC21 ( TICE , TIN )
            ELSE
              DTI0   = 1.
            END IF
#ifdef W3_T
          WRITE (NDST,9014) DTI0
#endif
          IF ( DTI0 .LT. 0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1004)
              CALL EXTCDE ( 5 )
            END IF
        ELSE
          DTI0   = 0.
        END IF
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'W3WAVE, step 6'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3WAVEMD, step 6")
#endif
#endif
!
! 1.f Momentum interval
!
      IF ( FLTAUA ) THEN
          DTTST1 = DSEC21 ( TU0 , TUN )
          DTTST2 = DSEC21 ( TU0 , TIME )
          DTTST3 = DSEC21 ( TEND , TUN )
#ifdef W3_T
          WRITE (NDST,9017) DTTST1, DTTST2, DTTST3
#endif
          IF ( DTTST1.LT.0. .OR. DTTST2.LT.0. .OR. DTTST3.LT.0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1007)
              CALL EXTCDE ( 3 )
            END IF
          IF ( DTTST2.EQ.0..AND. ITIME.EQ.0 ) THEN
              IDACT(9:9) = 'F'
              TOFRST = TIME
            END IF
        END IF
!
! 1.g Air density time
!
      IF ( FLRHOA ) THEN
          DTTST1 = DSEC21 ( TU0 , TUN )
          DTTST2 = DSEC21 ( TU0 , TIME )
          DTTST3 = DSEC21 ( TEND , TUN )
#ifdef W3_T
          WRITE (NDST,9018) DTTST1, DTTST2, DTTST3
#endif
          IF ( DTTST1.LT.0. .OR. DTTST2.LT.0. .OR. DTTST3.LT.0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1008)
              CALL EXTCDE ( 2 )
            END IF
          IF ( DTTST2.EQ.0..AND. ITIME.EQ.0 ) THEN
              IDACT(11:11) = 'F'
              TOFRST = TIME
            END IF
        END IF
!
! 1.e Ice thickness interval
!
      IF ( FLIC1 ) THEN
          IF ( TIC1(1) .GE. 0 ) THEN
              DTI10   = DSEC21 ( TIC1 , TI1 )
            ELSE
              DTI10   = 1.
            END IF
#ifdef W3_T
          WRITE (NDST,9015) DTI10
#endif
          IF ( DTI10 .LT. 0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1005)
              CALL EXTCDE ( 5 )
            END IF
        ELSE
          DTI10   = 0.
        END IF
!
! 1.e Ice floe interval
!
#ifdef W3_IS2
      IF ( FLIC5 ) THEN
          IF ( TIC5(1) .GE. 0 ) THEN
              DTI50   = DSEC21 ( TIC5 , TI5 )
            ELSE
              DTI50   = 1.
            END IF
#ifdef W3_T
          WRITE (NDST,9016) DTI50
#endif
          IF ( DTI50 .LT. 0. ) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1006)
              CALL EXTCDE ( 5 )
            END IF
        ELSE
          DTI50   = 0.
        END IF
#endif
!
! 2.  Determine next time from ending and output --------------------- /
!     time and get corresponding time step.
!
      FLFRST = .TRUE.
      DO
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'First entry in the TIME LOOP'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("First entry in the TIME LOOP")
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.1'
        FLUSH(740+IAPROC)
#endif
!      DO JSEA = 1, NSEAL
!        DO IS = 1, NSPEC
!          IF (VA(IS, JSEA) .LT. 0.) THEN
!            WRITE(740+IAPROC,*) 'TEST W3WAVE 2', VA(IS,JSEA)
!            CALL FLUSH(740+IAPROC)
!          ENDIF
!        ENDDO
!      ENDDO
!      IF (SUM(VA) .NE. SUM(VA)) THEN
!        WRITE(740+IAPROC,*) 'NAN in ACTION 2', IX, IY, SUM(VA)
!        CALL FLUSH(740+IAPROC)
!        STOP
!      ENDIF


#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "W3WAVEMD, step 6.1")
#endif
#endif
!
!
! 2.a Pre-calculate table for IC3 ------------------------------------ /
#ifdef W3_IC3
        USE_CHENG=IC3PARS(9)
        IF( USE_CHENG==1.0 )THEN
           FIXEDVISC=IC3PARS(14)
           FIXEDDENS=IC3PARS(15)
           FIXEDELAS=IC3PARS(16)
           IF ( (FIXEDVISC.LT.0.0).OR.(FIXEDDENS.LT.0.0) .OR. &
              (FIXEDELAS.LT.0.0) ) THEN
               IF ( IAPROC .EQ. NAPERR )                          &
               WRITE(NDSE,*)'Cheng method requires stationary',   &
                            ' and uniform rheology from namelist.'
               CALL EXTCDE(2)
           END IF
           IF (CALLEDIC3TABLE==0) THEN
             CALL IC3TABLE_CHENG(FIXEDVISC,FIXEDDENS,FIXEDELAS)
             CALLEDIC3TABLE = 1
           ENDIF
        ENDIF
#endif

! 2.b Update group velocity and wavenumber from ice parameters ------- /
!     from W3SIC3MD module. ------------------------------------------ /
!     Note: "IF FLFRST" can be added for efficiency, but testing req'd

         JSEA=1 ! no switch (intentional)

#ifdef W3_IC3
        USE_CGICE=IC3PARS(12)
        IF ( USE_CGICE==1.0 ) THEN
          IF ( IAPROC .EQ. NAPERR ) WRITE(SCREEN,920)
#endif

#ifdef W3_IC3
          DO JSEA=1,NSEAL
#endif
#ifdef W3_DIST
           ISEA   = IAPROC + (JSEA-1)*NAPROC
#endif
#ifdef W3_SHRD
           ISEA   = JSEA
#endif
#ifdef W3_IC3
            ALLOCATE(WN_I(SIZE(WN(:,ISEA))))
            WN_I(:) = 0.
            DEPTH  = MAX( DMIN , DW(ISEA) )
            IX     = MAPSF(ISEA,1)
            IY     = MAPSF(ISEA,2)
#endif

! 2.b.1 Using Cheng method: requires stationary/uniform rheology.
!       However, ice thickness may be input by either method

#ifdef W3_IC3
            IF ( USE_CHENG==1.0 ) THEN
               IF (FLIC1) THEN
                  HICE=ICEP1(IX,IY)
               ELSEIF (IC3PARS(13).GE.0.0)THEN
                  HICE=IC3PARS(13)
               ELSE
                  IF ( IAPROC .EQ. NAPERR )                       &
                  WRITE(NDSE,*)'ICE THICKNESS NOT AVAILABLE ',    &
                               'FOR CG CALC'
                  CALL EXTCDE(2)
               ENDIF
               IF (HICE > 0.0) THEN ! non-zero ice
                  CALL W3IC3WNCG_CHENG(WN(:,ISEA),WN_I(:),        &
                    CG(:,ISEA),HICE,FIXEDVISC,                    &
                    FIXEDDENS, FIXEDELAS, DEPTH)
               END IF ! non-zero ice
#endif

#ifdef W3_IC3
            ELSE ! not using Cheng method
#endif
! 2.b.2 If not using Cheng method: require FLIC1 to FLIC4 (not strictly
!       necesssary, but makes code simpler)

#ifdef W3_IC3
               IF (FLIC1.AND.FLIC2.AND.FLIC3.AND.FLIC4) THEN
                  IF (ICEP1(IX,IY)>0.0) THEN ! non-zero ice
                     CALL W3IC3WNCG_V1(WN(:,ISEA),WN_I(:),        &
                       CG(:,ISEA),ICEP1(IX,IY),ICEP2(IX,IY),      &
                       ICEP3(IX,IY),ICEP4(IX,IY),DEPTH)
                  END IF ! non-zero ice
               ELSE
                  IF ( IAPROC .EQ. NAPERR )                       &
                  WRITE(NDSE,*)'ICE PARAMETERS NOT AVAILABLE ',   &
                               'FOR CG CALC'
                  CALL EXTCDE(2)
               END IF
            ENDIF ! IF USE_CHENG...
#endif

#ifdef W3_IC3
            DEALLOCATE(WN_I)
          END DO ! DO JSEA=1,NSEAL
        END IF !  IF USE_CGICE ...
#endif
!
        IF ( TOFRST(1) .GT. 0 ) THEN
            DTTST  = DSEC21 ( TEND , TOFRST )
          ELSE
            DTTST  = 0.
          ENDIF
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) '2 : DTTST=', DTTST, TEND, TOFRST
#endif
!
        IF ( DTTST.GE.0. ) THEN
            TCALC = TEND
          ELSE
            TCALC = TOFRST
          END IF
!
        DTTST  = DSEC21 ( TIME , TCALC )
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) '3 : DTTST=', DTTST, TEND, TOFRST
#endif
        NT     = 1 + INT ( DTTST / DTMAX - 0.001 )
        DTGA   = DTTST / REAL(NT)
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'DTTST=', DTTST, ' NT=', NT
#endif
        IF ( DTTST .EQ. 0. ) THEN
            IT0    = 0
            IF ( .NOT.FLZERO ) ITIME  = ITIME - 1
            NT     = 0
          ELSE
            IT0    = 1
          END IF

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

!
#ifdef W3_T
        WRITE (NDST,9020) IT0, NT, DTGA
#endif
!
! ==================================================================== /
!
! 3.  Loop over time steps
!
        DTRES  = 0.

#ifdef W3_DEBUGRUN
      DO JSEA = 1, NSEAL
        DO IS = 1, NSPEC
          IF (VA(IS, JSEA) .LT. 0.) THEN
            WRITE(740+IAPROC,*) 'TEST W3WAVE 3', VA(IS,JSEA)
            CALL FLUSH(740+IAPROC)
          ENDIF
        ENDDO
      ENDDO
      IF (SUM(VA) .NE. SUM(VA)) THEN
        WRITE(740+IAPROC,*) 'NAN in ACTION 3', IX, IY, SUM(VA)
        CALL FLUSH(740+IAPROC)
        STOP
      ENDIF
        WRITE(740+IAPROC,*) 'IT0=', IT0, ' NT=', NT
        FLUSH(740+IAPROC)
#endif
!
        DO IT=IT0, NT
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("Begin of IT loop")
#endif
#ifdef W3_SETUP
     CALL WAVE_SETUP_COMPUTATION
#endif
! copy old values
#ifdef W3_PDLIB
     DO IP=1,NSEAL
       DO ISPEC=1,NSPEC
         VAOLD(ISPEC,IP)=VA(ISPEC,IP)
       END DO
     END DO
#endif
!
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Beginning time loop")
#endif
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("After assigning VAOLD")
#endif
!
#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 0'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
          ITIME  = ITIME + 1
!
          DTG    = REAL(NINT(DTGA+DTRES+0.0001))
          DTRES  = DTRES + DTGA - DTG
          IF ( ABS(DTRES) .LT. 0.001 ) DTRES  = 0.
          CALL TICK21 ( TIME , DTG )
!
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'DTGA=', DTGA, ' DTRES=', DTRES
      WRITE(740+IAPROC,*) 'DTG 1 : DTG=', DTG
      FLUSH(740+IAPROC)
#endif
!
#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 1'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

          IF ( TSTAMP .AND. SCREEN.NE.NDSO .AND. IAPROC.EQ.NAPOUT ) THEN
              CALL WWTIME ( STTIME )
              CALL STME21 ( TIME , IDTIME )
              WRITE (SCREEN,950) IDTIME, STTIME
            END IF

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 2'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

#ifdef W3_DEBUGRUN
      DO JSEA = 1, NSEAL
        DO IS = 1, NSPEC
          IF (VA(IS, JSEA) .LT. 0.) THEN
            WRITE(740+IAPROC,*) 'TEST W3WAVE 4', VA(IS,JSEA)
            CALL FLUSH(740+IAPROC)
          ENDIF
        ENDDO
      ENDDO
      IF (SUM(VA) .NE. SUM(VA)) THEN
        WRITE(740+IAPROC,*) 'NAN in ACTION 4', IX, IY, SUM(VA)
        CALL FLUSH(740+IAPROC)
        STOP
      ENDIF
#endif
!
          VGX = 0.
          VGY = 0.
          IF(INFLAGS1(10)) THEN
              DTTST1 = DSEC21 ( TIME, TGN )
              DTTST2 = DSEC21 ( TG0, TGN )
              FAC    = DTTST1 / MAX ( 1. , DTTST2 )
              VGX    = (FAC*GA0+(1.-FAC)*GAN) *                       &
                            COS(FAC*GD0+(1.-FAC)*GDN)
              VGY    = (FAC*GA0+(1.-FAC)*GAN) *                       &
                            SIN(FAC*GD0+(1.-FAC)*GDN)
            END IF
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("After VGX/VGY assignation")
#endif
!
#ifdef W3_T
        WRITE (NDST,9021) ITIME, IT, TIME, FLMAP, FLDDIR,          &
                          VGX, VGY, DTG, DTRES
#endif
#ifdef W3_DEBUGSRC
      WRITE(740+IAPROC,*) 'DTG 2 : DTG=', DTG
      WRITE(740+IAPROC,*) 'max(UST)=', maxval(UST)
      FLUSH(740+IAPROC)
#endif
!
! 3.1 Interpolate winds, currents, and momentum.
!     (Initialize wave fields with winds)
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'FLCUR=', FLCUR
        FLUSH(740+IAPROC)
#endif
#ifdef W3_DEBUGDCXDX
       WRITE(740+IAPROC,*) 'Debug DCXDX FLCUR=', FLCUR
#endif
#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 3a '
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

          IF ( FLCUR  ) THEN
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.4'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Before UCUR")
#endif
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.4.1'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("W3WAVE, step 6.4.1")
#endif

#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.4.2 before W3UCUR'
        FLUSH(740+IAPROC)
#endif
            CALL W3UCUR ( FLFRST )
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.4.1 after W3UCUR'
        FLUSH(740+IAPROC)
#endif

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 3b '
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
            IF (GTYPE .EQ. SMCTYPE) THEN
              IX = 1
#ifdef W3_SMC
 !!Li  Use new sub for DCXDX/Y and DCYDX/Y assignment.  
         CALL SMCDCXY 
#endif
            ELSE IF (GTYPE .EQ. UNGTYPE) THEN
#ifdef W3_DEBUGDCXDX
       WRITE(740+IAPROC,*) 'Before call to UG_GRADIENT for assigning DCXDX/DCXDY array'
#endif
              CALL UG_GRADIENTS(CX, DCXDX, DCXDY)
              CALL UG_GRADIENTS(CY, DCYDX, DCYDY)
              CALL GET_INTERFACE
              UGDTUPDATE=.TRUE.
              CFLXYMAX = 0.
            ELSE
              CALL W3DZXY(CX(1:UBOUND(CX,1)),'m/s',DCXDX, DCXDY) !CX GRADIENT
              CALL W3DZXY(CY(1:UBOUND(CY,1)),'m/s',DCYDX, DCYDY) !CY GRADIENT
            ENDIF  !! End GTYPE 
!
#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 4'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
            ELSE IF ( FLFRST ) THEN
              UGDTUPDATE=.TRUE.
              CFLXYMAX = 0.
              CX = 0.
              CY = 0.
              END IF ! FLCUR
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("After CX/CY assignation")
#endif
!
#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 5'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

          IF ( FLWIND ) THEN
            IF ( FLFRST ) ASF = 1.
            CALL W3UWND ( FLFRST, VGX, VGY )
          ELSE IF ( FLFRST ) THEN
            U10    = 0.01
            U10D   = 0.
            UST    = 0.05
            USTDIR = 0.05
          END IF

!      DO JSEA = 1, NSEAL
!        DO IS = 1, NSPEC
!          IF (VA(IS, JSEA) .LT. 0.) THEN
!            WRITE(740+IAPROC,*) 'TEST W3WAVE 5', VA(IS,JSEA)
!            CALL FLUSH(740+IAPROC)
!          ENDIF
!        ENDDO
!      ENDDO
!      IF (SUM(VA) .NE. SUM(VA)) THEN
!        WRITE(740+IAPROC,*) 'NAN in ACTION 5', IX, IY, SUM(VA)
!        CALL FLUSH(740+IAPROC)
!        STOP
!      ENDIF

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 6'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("After U10, etc. assignation")
#endif
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.5'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Before call to W3UINI")
#endif
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("Before call W3UINI")
#endif
          IF ( FLIWND .AND. LOCAL ) CALL W3UINI ( VA )
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.5.1 DTG=', DTG
        FLUSH(740+IAPROC)
#endif
!
          IF ( FLTAUA ) THEN
            CALL W3UTAU ( FLFRST )
          ELSE IF ( FLFRST ) THEN
            TAUA    = 0.01
            TAUADIR = 0.
          END IF
!
          IF ( FLRHOA ) THEN
            CALL W3URHO ( FLFRST )
          ELSE IF ( FLFRST ) THEN
            RHOAIR = DAIR
          END IF
!
! 3.2 Update boundary conditions if boundary flag is true (FLBPI)
!
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Before boundary update")
#endif
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("Before boundary update")
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'FLBPI=', FLBPI
        WRITE(740+IAPROC,*) 'LOCAL=', LOCAL
        FLUSH(740+IAPROC)
#endif
#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 7'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

          IF ( FLBPI .AND. LOCAL ) THEN
!
              DO
                IF ( TBPIN(1) .EQ. -1 ) THEN
                    READBC = .TRUE.
                    IDACT(1:1) = 'F'
                ELSE
                    READBC = DSEC21(TIME,TBPIN).LT.0.
                    IF (READBC.AND.IDACT(1:1).EQ.' ') IDACT(1:1) = 'X'
                END IF
                FLACT  = READBC .OR. FLACT
#ifdef W3_DEBUGIOBC
     WRITE(740+IAPROC,*) 'READBC=', READBC
     FLUSH(740+IAPROC)
#endif

                IF ( READBC ) THEN
#ifdef W3_DEBUGIOBC
       WRITE(740+IAPROC,*) 'Before call to W3IOBC'
       FLUSH(740+IAPROC)
#endif
                  CALL W3IOBC ( 'READ', NDS(9), TBPI0, TBPIN,       &
                                ITEST, IMOD )
#ifdef W3_DEBUGIOBC
       WRITE(740+IAPROC,*) 'After call to W3IOBC'
       WRITE(740+IAPROC,*) 'ITEST=', ITEST
       FLUSH(740+IAPROC)
#endif
                  IF ( ITEST .NE. 1 ) CALL W3UBPT
                ELSE
                  ITEST  = 0
                END IF
                IF ( ITEST .LT. 0 ) IDACT(1:1) = 'L'
                IF ( ITEST .GT. 0 ) IDACT(1:1) = ' '
                IF ( .NOT. (READBC.AND.FLBPI) ) EXIT
                END DO

          END IF

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 7'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif


#ifdef W3_PDLIB
          CALL APPLY_BOUNDARY_CONDITION_VA
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After FLBPI and LOCAL")
#endif
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("After FLBPI and LOCAL")
#endif

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 8'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
! 3.3.1 Update ice coverage (if new ice map).
!     Need to be run on output nodes too, to update MAPSTx
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'FLICE=', FLICE
        WRITE(740+IAPROC,*) 'DTI0=', DTI0
        FLUSH(740+IAPROC)
#endif
          IF ( FLICE .AND. DTI0.NE.0. ) THEN
!
              IF ( TICE(1).GE.0 ) THEN
                  IF ( DTI0 .LT. 0. ) THEN
                      IDACT(13:13) = 'B'
                    ELSE
                      DTTST  = DSEC21 ( TIME, TIN )
                      IF ( DTTST .LE. 0.5*DTI0 ) IDACT(13:13) = 'U'
                    END IF
                ELSE
                  IDACT(13:13) = 'I'
                END IF
!
              IF ( IDACT(13:13).NE.' ' ) THEN
                  CALL W3UICE ( VA, VA )
                  DTI0   = 0.
                  FLACT  = .TRUE.
                  FLMAP  = .TRUE.
              END IF
          END IF
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After FLICE and DTI0")
#endif
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("After FLICE and DTI0")
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.7 DTG=', DTG
        FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGIOBP
         IF (NX .ge. 10210) WRITE(*,*) 'Before W3ULEV:', MAPSTA(1,10210), IOBP(10210)
#endif
#endif

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 9'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
! 3.3.2 Update ice thickness
!
          IF ( FLIC1 .AND. DTI10.NE.0. ) THEN
!
              IF ( TIC1(1).GE.0 ) THEN
                  IF ( DTI10 .LT. 0. ) THEN
                      IDACT(15:15) = 'B'
                    ELSE
                      DTTST  = DSEC21 ( TIME, TI1 )
                      IF ( DTTST .LE. 0.5*DTI10 ) IDACT(15:15) = 'U'
                    END IF
                ELSE
                  IDACT(15:15) = 'I'
                END IF

!
              IF ( IDACT(15:15).NE.' ' ) THEN
                  CALL W3UIC1 ( FLFRST )
                  DTI10   = 0.
                  FLACT  = .TRUE.
                  FLMAP  = .TRUE.
                END IF
!
            END IF

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 10'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

!
! 3.3.3 Update ice floe diameter
!
#ifdef W3_IS2
          IF ( FLIC5 .AND. DTI50.NE.0. ) THEN
#endif
!
#ifdef W3_IS2
              IF ( TIC5(1).GE.0 ) THEN
                  IF ( DTI50 .LT. 0. ) THEN
                      IDACT(18:18) = 'B'
                    ELSE
                      DTTST  = DSEC21 ( TIME, TI5 )
                      IF ( DTTST .LE. 0.5*DTI50 ) IDACT(18:18) = 'U'
                    END IF
                ELSE
                  IDACT(18:18) = 'I'
                END IF
#endif
!
#ifdef W3_IS2
              IF ( IDACT(18:18).NE.' ' ) THEN
               CALL W3UIC5( FLFRST )
                  DTI50   = 0.
                  FLACT  = .TRUE.
                  FLMAP  = .TRUE.
                END IF
#endif
!
#ifdef W3_IS2
            END IF
#endif

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 11a'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
! 3.4 Transform grid (if new water level).
!
!          write(740+IAPROC,*) 'TEST ARON', FLLEV, DTL0, TLEV(1), IDACT(5:5), DSEC21 ( TIME, TLN ), TIME, TLN
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'FLLEV=', FLLEV, ' DTL0=', DTL0
        FLUSH(740+IAPROC)
#endif
          IF ( FLLEV .AND. DTL0 .NE.0. ) THEN
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'Before time works'
        FLUSH(740+IAPROC)
#endif
              IF ( TLEV(1) .GE. 0 ) THEN
                  IF ( DTL0 .LT. 0. ) THEN
                      IDACT(5:5) = 'B'
                    ELSE
                      DTTST  = DSEC21 ( TIME, TLN )
                      IF ( DTTST .LE. 0.5*DTL0 ) IDACT(5:5) = 'U'
                    END IF
                ELSE
                  IDACT(5:5) = 'I'
                END IF
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'After time works'
        FLUSH(740+IAPROC)
#endif
!
              IF ( IDACT(5:5).NE.' ' ) THEN

#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'Before W3ULEV'
        FLUSH(740+IAPROC)
#endif
                  CALL W3ULEV ( VA, VA )
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'After W3ULEV'
        FLUSH(740+IAPROC)
#endif

                  UGDTUPDATE=.TRUE.
                  CFLXYMAX = 0.
                  DTL0   = 0.
                  FLACT  = .TRUE.
                  FLMAP  = .TRUE.
                  FLDDIR = FLDDIR .OR.  FLCTH .OR. FSREFRACTION        &
                        .OR. FLCK .OR. FSFREQSHIFT
              END IF
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'After IDACT if test'
        FLUSH(740+IAPROC)
#endif
          END IF
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'After FLLEV test'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After FFLEV and DTL0")
#endif
#ifdef W3_DEBUGIOBP
         IF (NX .ge. 10210) WRITE(*,*) ' After W3ULEV:', MAPSTA(1,10210), IOBP(10210)
#endif
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("After FFLEV and DTL0")
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'FLMAP=', FLMAP
        FLUSH(740+IAPROC)
#endif

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 11b'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

!
! 3.5 Update maps and derivatives.
!
          IF ( FLMAP ) THEN
            IF ( GTYPE .NE. SMCTYPE ) THEN
#ifdef W3_PR1
              CALL W3MAP1 ( MAPSTA )
#endif
#ifdef W3_PR2
              CALL W3MAP2
#endif
#ifdef W3_PR3
              CALL W3MAP3
#endif
              CALL W3UTRN ( TRNX, TRNY )
#ifdef W3_PR3
              CALL W3MAPT
#endif
            END IF  !! GTYPE 
              CALL W3NMIN ( MAPSTA, FLAG0 )
              IF ( FLAG0 .AND. IAPROC.EQ.NAPERR ) WRITE (NDSE,1030) IMOD
              FLMAP  = .FALSE.
          END IF
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.1 DTG=', DTG
        FLUSH(740+IAPROC)
#endif
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.2 DTG=', DTG
        WRITE(740+IAPROC,*) 'FLDDIR=', FLDDIR
        FLUSH(740+IAPROC)
#endif
          IF ( FLDDIR ) THEN
            IF (GTYPE .EQ. SMCTYPE) THEN
              IX = 1
#ifdef W3_SMC
 !!Li  Use new sub for DDDX and DDDY assignment.  
         CALL SMCDHXY 
#endif
            ELSE IF (GTYPE .EQ. UNGTYPE) THEN
              CALL UG_GRADIENTS(DW, DDDX, DDDY)
            ELSE
              CALL W3DZXY(DW(1:UBOUND(DW,1)),'m',DDDX,DDDY)
            END IF
            FLDDIR = .FALSE.
          END IF

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 12'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.3 DTG=', DTG
        FLUSH(740+IAPROC)
#endif
!
!         Calculate PHASE SPEED GRADIENT.
          DCDX = 0.
          DCDY = 0.
#ifdef W3_REFRX
   CIK  = 0.
!
          IF (GTYPE .NE. UNGTYPE) THEN
            DO IK=0,NK+1
               CIK = SIG(IK) / WN(IK,1:NSEA)
               CALL W3DZXY(CIK,'m/s',DCDX(IK,:,:),DCDY(IK,:,:))
            END DO
          ELSE
            WRITE (NDSE,1040)
            CALL EXTCDE(2)
     !      CALL UG_GRADIENTS(CMN, DCDX, DCDY) !/ Stefan, to be confirmed!
          END IF
#endif
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.4'
        FLUSH(740+IAPROC)
#endif
!
          FLIWND = .FALSE.
          FLFRST = .FALSE.
!
#ifdef W3_PDLIB
#ifdef W3_DEBUGSRC
          WRITE(740+IAPROC,*) 'ITIME=', ITIME, ' IT=', IT
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA before W3SRCE_IMP_PRE")
          CALL ALL_FIELD_INTEGRAL_PRINT(VSTOT, "VSTOT before W3SRCE_IMP_PRE")
          CALL ALL_FIELD_INTEGRAL_PRINT(VDTOT, "VDTOT before W3SRCE_IMP_PRE")
          IF (DEBUG_NODE .le. NSEAL) THEN
            WRITE(740+IAPROC,*) '     Values for DEBUG_NODE=', DEBUG_NODE
            WRITE(740+IAPROC,*) '     sum(VA)=', sum(VA(:,DEBUG_NODE))
            WRITE(740+IAPROC,*) '     sum(VSTOT)=', sum(VSTOT(:,DEBUG_NODE))
            WRITE(740+IAPROC,*) '     sum(VDTOT)=', sum(VDTOT(:,DEBUG_NODE))
          END IF
#endif
       IF (IT .eq. 0) THEN
         DTGpre = 1.
       ELSE
         DTGpre = DTG
       END IF
#endif

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 13'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
        IF ( FLSOU .and. LPDLIB) THEN
!
#ifdef W3_OMP0
!$OMP PARALLEL DO PRIVATE (JSEA,ISEA,IX,IY) SCHEDULE (DYNAMIC,1)
#endif
          D50=0.0002
          REFLEC(:)=0.
          REFLED(:)=0
          PSIC=0.
#ifdef W3_PDLIB
          VSTOT = 0.
          VDTOT = 0.
#endif

          DO JSEA=1, NSEAL
            CALL INIT_GET_ISEA(ISEA, JSEA)
            IX     = MAPSF(ISEA,1)
            IY     = MAPSF(ISEA,2)
            DELA=1.
            DELX=1.
            DELY=1.
#ifdef W3_REF1
                IF (GTYPE.EQ.RLGTYPE) THEN
                  DELX=SX*CLATS(ISEA)/FACX
                  DELY=SY/FACX
                  DELA=DELX*DELY
                END IF
                IF (GTYPE.EQ.CLGTYPE) THEN
! Maybe what follows works also for RLGTYPE ... to be verified
                  DELX=HPFAC(IY,IX)/ FACX
                  DELY=HQFAC(IY,IX)/ FACX
                  DELA=DELX*DELY
                END IF
#endif
!
#ifdef W3_REF1
       REFLEC=REFLC(:,ISEA)
       REFLEC(4)=BERG(ISEA)*REFLEC(4)
       REFLED=REFLD(:,ISEA)
#endif
#ifdef W3_BT4
        D50=SED_D50(ISEA)
        PSIC=SED_PSIC(ISEA)
#endif
#ifdef W3_REF1
       REFLEC=REFLC(:,ISEA)
       REFLEC(4)=BERG(ISEA)*REFLEC(4)
       REFLED=REFLD(:,ISEA)
#endif
#ifdef W3_BT4
        D50=SED_D50(ISEA)
        PSIC=SED_PSIC(ISEA)
#endif
!
#ifdef W3_DEBUGRUN
        DO IS = 1, NSPEC
          IF (VA(IS, JSEA) .LT. 0.) THEN
            WRITE(740+IAPROC,*) 'TEST W3WAVE 7', VA(IS,JSEA)
            CALL FLUSH(740+IAPROC)
          ENDIF
        ENDDO
#endif
!
            IF ( MAPSTA(IY,IX) .EQ. 1 .AND. FLAGST(ISEA)) THEN
              IF (FSSOURCE) THEN
#ifdef W3_PDLIB
#ifdef W3_DEBUGSRC
         IF (IX .eq. DEBUG_NODE) THEN
           WRITE(740+IAPROC,*) 'NODE_SRCE_IMP_PRE : IX=', IX, ' JSEA=', JSEA
         END IF
      WRITE(740+IAPROC,*) 'IT/IX/IY/IMOD=', IT, IX, IY, IMOD
      WRITE(740+IAPROC,*) 'ISEA/JSEA=', ISEA, JSEA
      WRITE(740+IAPROC,*) 'Before sum(VA)=', sum(VA(:,JSEA))
      FLUSH(740+IAPROC)
#endif
                CALL W3SRCE(srce_imp_pre, IT, JSEA, IX, IY, IMOD, &
                   VAoldDummy, VA(:,JSEA),                        &
                   VSTOT(:,JSEA), VDTOT(:,JSEA), SHAVETOT(JSEA),  &
                   ALPHA(1:NK,JSEA), WN(1:NK,ISEA),               &
                   CG(1:NK,ISEA), DW(ISEA), U10(ISEA),            &
                   U10D(ISEA), AS(ISEA), UST(ISEA),               &
                   USTDIR(ISEA), CX(ISEA), CY(ISEA),              &
                   ICE(ISEA), ICEH(ISEA), ICEF(ISEA),             &
                   ICEDMAX(ISEA),                                 &
                   REFLEC, REFLED, DELX, DELY, DELA,              &
                   TRNX(IY,IX), TRNY(IY,IX), BERG(ISEA),          &
                   FPIS(ISEA), DTDYN(JSEA),                       &
                   FCUT(JSEA), DTGpre, TAUWX(JSEA), TAUWY(JSEA),  &
                   TAUOX(JSEA), TAUOY(JSEA), TAUWIX(JSEA),        &
                   TAUWIY(JSEA), TAUWNX(JSEA),                    &
                   TAUWNY(JSEA),  PHIAW(JSEA), CHARN(JSEA),       &
                   TWS(JSEA), PHIOC(JSEA), TMP1, D50, PSIC, TMP2, &
                   PHIBBL(JSEA), TMP3, TMP4, PHICE(JSEA),         &
                   TAUOCX(JSEA), TAUOCY(JSEA), WNMEAN(JSEA),      &
                   RHOAIR(ISEA), ASF(ISEA))
#ifdef W3_DEBUGSRC
      WRITE(740+IAPROC,*) 'After sum(VA)=', sum(VA(:,JSEA))
      WRITE(740+IAPROC,*) '   sum(VSTOT)=', sum(VSTOT(:,JSEA))
      WRITE(740+IAPROC,*) '   sum(VDTOT)=', sum(VDTOT(:,JSEA))
      WRITE(740+IAPROC,*) '     SHAVETOT=', SHAVETOT(JSEA)
      FLUSH(740+IAPROC)
#endif
#endif
              ENDIF
            ELSE
              UST   (ISEA) = UNDEF
              USTDIR(ISEA) = UNDEF
              DTDYN (JSEA) = UNDEF
              FCUT  (JSEA) = UNDEF
            END IF
          END DO ! JSEA
        END IF ! PDLIB
#ifdef W3_PDLIB
#ifdef W3_DEBUGSRC
          WRITE(740+IAPROC,*) 'ITIME=', ITIME, ' IT=', IT
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA after W3SRCE_IMP_PRE")
          CALL ALL_FIELD_INTEGRAL_PRINT(VSTOT, "VSTOT after W3SRCE_IMP_PRE")
          CALL ALL_FIELD_INTEGRAL_PRINT(VDTOT, "VDTOT after W3SRCE_IMP_PRE")
          IF (DEBUG_NODE .le. NSEAL) THEN
            WRITE(740+IAPROC,*) '     Values for DEBUG_NODE=', DEBUG_NODE
            WRITE(740+IAPROC,*) '     sum(VA)=', sum(VA(:,DEBUG_NODE))
            WRITE(740+IAPROC,*) '     sum(VSTOT)=', sum(VSTOT(:,DEBUG_NODE))
            WRITE(740+IAPROC,*) '     sum(VDTOT)=', sum(VDTOT(:,DEBUG_NODE))
          END IF
#endif
#endif

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 14'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

          IF ( FLZERO ) THEN
#ifdef W3_T
              WRITE (NDST,9022)
#endif
              GOTO 400
            END IF
          IF ( IT.EQ.0 ) THEN
            DTG = 1.
!            DTG = 60.
            GOTO 370
          END IF
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.5'
        WRITE(740+IAPROC,*) 'FLDRY=', FLDRY
        FLUSH(740+IAPROC)
#endif
          IF ( FLDRY .OR. IAPROC.GT.NAPROC ) THEN
#ifdef W3_T
              WRITE (NDST,9023)
#endif
#ifdef W3_DEBUGRUN
              WRITE(740+IAPROC,*) 'Jump to 380'
              FLUSH(740+IAPROC)
#endif
              GOTO 380
          END IF
!
! Estimation of the local maximum CFL for XY propagation
!
#ifdef W3_T
           WRITE(NDSE,*) 'Computing CFLs .... ',NSEAL
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'FLOGRD(9,3) = ', FLOGRD(9,3)
        WRITE(740+IAPROC,*) 'UGDTUPDATE=', UGDTUPDATE
        FLUSH(740+IAPROC)
#endif
                IF ( FLOGRD(9,3).AND. UGDTUPDATE ) THEN
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.6'
        FLUSH(740+IAPROC)
#endif
                  IF (FSTOTALIMP .eqv. .FALSE.) THEN
                      NKCFL=NK
#ifdef W3_T
                   NKCFL=1
#endif
!
#ifdef W3_OMPG
!$OMP PARALLEL DO PRIVATE (JSEA,ISEA) SCHEDULE (DYNAMIC,1)
#endif
!
                      DO JSEA=1, NSEAL
                        CALL INIT_GET_ISEA(ISEA, JSEA)
#ifdef W3_PR3
                      IF (GTYPE .EQ. UNGTYPE) THEN
                        IF ( FLOGRD(9,3) ) THEN
#endif
#ifdef W3_T
                          IF (MOD(ISEA,100).EQ.0) WRITE(NDSE,*) 'COMPUTING CFL FOR NODE:',ISEA
#endif
#ifdef W3_PDLIB
                      IF (.NOT. LPDLIB) THEN
#endif
#ifdef W3_PR3
                          CALL W3CFLUG ( ISEA, NKCFL, FACX, FACX, DTG,  &
                                         MAPFS,  CFLXYMAX(JSEA), VGX, VGY )
#endif
#ifdef W3_PDLIB
                      ENDIF
#endif
#ifdef W3_PR3
                        END IF
                      ELSE
                        CALL W3CFLXY ( ISEA, DTG, MAPSTA, MAPFS,      &
                                       CFLXYMAX(JSEA), VGX, VGY )
                      END IF
#endif
                      END DO
!
#ifdef W3_OMPG
!$OMP END PARALLEL DO
#endif
!
                    END IF
                END IF
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.8.7'
        FLUSH(740+IAPROC)
#endif

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 15'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
#ifdef W3_DEBUGRUN
      DO JSEA = 1, NSEAL
        DO IS = 1, NSPEC
          IF (VA(IS, JSEA) .LT. 0.) THEN
            WRITE(740+IAPROC,*) 'TEST W3WAVE 8', VA(IS,JSEA)
            CALL FLUSH(740+IAPROC)
          ENDIF
        ENDDO
      ENDDO
      IF (SUM(VA) .NE. SUM(VA)) THEN
        WRITE(740+IAPROC,*) 'NAN in ACTION 6 ', IX, IY, SUM(VA)
        CALL FLUSH(740+IAPROC)
        STOP
      ENDIF
#endif

!
#ifdef W3_T
       IF (GTYPE .EQ. UNGTYPE) THEN
         IF ( FLOGRD(9,3) ) THEN
           DTCFL1(:)=1.
           DO JSEA=1,NSEAL
             INDSORT(JSEA)=FLOAT(JSEA)
             DTCFL1(JSEA)=DTG/CFLXYMAX(JSEA)
           END DO
           CALL SSORT1 (DTCFL1, INDSORT, NSEAL, 2)
           IF ( IAPROC .EQ. NAPERR ) WRITE(NDSE,*) 'Nodes requesting smallest timesteps:'
           IF ( IAPROC .EQ. NAPERR ) WRITE(NDSE,'(A,10I10)')   'Nodes      ',NINT(INDSORT(1:10))
           IF ( IAPROC .EQ. NAPERR ) WRITE(NDSE,'(A,10F10.2)') 'time steps ',DTCFL1(1:10)
           DO JSEA = 1, MIN(NSEAL,200)
             ISEA   = NINT(INDSORT(JSEA))            ! will not work with MPI
             IX     = MAPSF(ISEA,1)
             IF (JSEA.EQ.1) &
               WRITE(995,*) '       IP  dtmax_exp(ip)        x-coord        y-coord        z-coord'
             WRITE(995,'(I10,F10.2,3F10.4)') IX,  DTCFL1(JSEA), XYB(IX,1), XYB(IX,2), XYB(IX,3)
           END DO ! JSEA
           CLOSE(995)
         END IF
       END IF
#endif

!
! 3.6 Perform Propagation = = = = = = = = = = = = = = = = = = = = = = =
! 3.6.1 Preparations
!
#ifdef W3_SEC1
      DTGTEMP=DTG
      DTG=DTG/NITERSEC1
      DO ISEC1=1,NITERSEC1
#endif
          NTLOC  = 1 + INT( DTG/DTCFLI - 0.001 )
#ifdef W3_SEC1
    IF ( IAPROC .EQ. NAPOUT )    WRITE(NDSE,'(A,I4,A,I4)') '   SUBSECOND STEP:',ISEC1,' out of ',NITERSEC1
#endif
!
          FACTH  = DTG / (DTH*REAL(NTLOC))
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, DTCFLI=', DTCFLI
        WRITE(740+IAPROC,*) 'W3WAVE, DTG=', DTG
        WRITE(740+IAPROC,*) 'W3WAVE, DTH=', DTH
        WRITE(740+IAPROC,*) 'W3WAVE, NTLOC=', NTLOC
        FLUSH(740+IAPROC)
#endif
          FACK   = DTG / REAL(NTLOC)

          TTEST(1) = TIME(1)
          TTEST(2) = 0
          DTTEST = DSEC21(TTEST,TIME)
          ITLOCH = ( NTLOC + 1 - MOD(NINT(DTTEST/DTG),2) ) / 2
!
! 3.6.2 Intra-spectral part 1
!
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Before intraspectral part 1")
#endif
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("Before intraspectral")
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.10'
        WRITE(740+IAPROC,*) 'FLCTH=', FLCTH, ' FLCK=', FLCK
        FLUSH(740+IAPROC)
#endif
          IF ( FLCTH .OR. FLCK ) THEN
              DO ITLOC=1, ITLOCH
!
#ifdef W3_OMPG
!$OMP PARALLEL PRIVATE (JSEA,ISEA,IX,IY,DEPTH,IXrel)
!$OMP DO SCHEDULE (DYNAMIC,1)
#endif
!
#ifdef W3_DEBUGRUN
                WRITE(740+IAPROC,*) ' ITLOC=', ITLOC
                WRITE(740+IAPROC,*) ' 1: Before call to W3KTP1 / W3KTP2 / W3KTP3'
#endif
                DO JSEA=1, NSEAL
                  CALL INIT_GET_ISEA(ISEA, JSEA)
                  IX     = MAPSF(ISEA,1)
                  IY     = MAPSF(ISEA,2)

#ifdef W3_DEBUGRUN
                  IF (JSEA == DEBUG_NODE) WRITE(*,*) 'W3WAVE TEST', SUM(VA(:,JSEA))
#endif
                  IF ( GTYPE .EQ. UNGTYPE ) THEN
                    IF (IOBP(ISEA) .NE. 1) CYCLE
                  ENDIF

                  IF ( MAPSTA(IY,IX) .EQ. 1 ) THEN
                           DEPTH  = MAX ( DMIN , DW(ISEA) )
                           IF (LPDLIB) THEN
                             IXrel = JSEA
                           ELSE
                             IXrel = IX
                           END IF
!
                      IF( GTYPE .EQ. SMCTYPE ) THEN
                          J = 1
#ifdef W3_SMC
  !!Li    Refraction and GCT in theta direction is done by rotation.
                      CALL W3KRTN ( ISEA, FACTH, FACK, CTHG0S(ISEA), &
                           CG(:,ISEA), WN(:,ISEA), DEPTH,            &
                           DHDX(ISEA), DHDY(ISEA), DHLMT(:,ISEA),    &
                           CX(ISEA), CY(ISEA), DCXDX(IY,IX),         &
                           DCXDY(IY,IX), DCYDX(IY,IX), DCYDY(IY,IX), & 
                           DCDX(:,IY,IX), DCDY(:,IY,IX), VA(:,JSEA) )  
#endif
!
                      ELSE
                          J = 1
!
#ifdef W3_PR1
                      CALL W3KTP1 ( ISEA, FACTH, FACK, CTHG0S(ISEA), &
                           CG(:,ISEA), WN(:,ISEA), DEPTH,            &
                           DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
                           CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
                           DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
                           DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA))
#endif
#ifdef W3_PR2
                      CALL W3KTP2 ( ISEA, FACTH, FACK, CTHG0S(ISEA), &
                           CG(:,ISEA), WN(:,ISEA), DEPTH,            &
                           DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
                           CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
                           DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
                           DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA))
#endif
#ifdef W3_PR3
                      CALL W3KTP3 ( ISEA, FACTH, FACK, CTHG0S(ISEA), &
                           CG(:,ISEA), WN(:,ISEA), DEPTH,            &
                           DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
                           CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
                           DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
                           DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA), &
                           CFLTHMAX(JSEA), CFLKMAX(JSEA) )  
#endif
!
                      END IF  !!  GTYPE  
!
                    END IF
                  END DO
!
#ifdef W3_OMPG
!$OMP END DO
!$OMP END PARALLEL
#endif
!
              END DO
          END IF

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 16'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
  CALL ALL_VA_INTEGRAL_PRINT(IMOD, "Before spatial advection")
#endif
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("Before spatial advection")
#endif
!
! 3.6.3 Longitude-latitude
!       (time step correction in routine)
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12'
        WRITE(740+IAPROC,*) 'FSN=', FSN
        WRITE(740+IAPROC,*) 'FSPSI=', FSPSI
        WRITE(740+IAPROC,*) 'FSFCT=', FSFCT
        WRITE(740+IAPROC,*) 'FSNIMP=', FSNIMP
        WRITE(740+IAPROC,*) 'FLCTH=', FLCTH
        WRITE(740+IAPROC,*) 'FSREFRACTION=', FSREFRACTION
        WRITE(740+IAPROC,*) 'FLCK=', FLCK
        WRITE(740+IAPROC,*) 'FSFREQSHIFT=', FSFREQSHIFT
        WRITE(740+IAPROC,*) 'FLSOU=', FLSOU
        WRITE(740+IAPROC,*) 'FSSOURCE=', FSSOURCE
        WRITE(740+IAPROC,*) 'FSTOTALIMP=', FSTOTALIMP
        WRITE(740+IAPROC,*) 'FSTOTALEXP=', FSTOTALEXP
        WRITE(740+IAPROC,*) 'FLCUR=', FLCUR
        WRITE(740+IAPROC,*) 'PDLIB=', LPDLIB
        WRITE(740+IAPROC,*) 'GTYPE=', GTYPE
        WRITE(740+IAPROC,*) 'UNGTYPE=', UNGTYPE
        WRITE(740+IAPROC,*) 'NAPROC=', NAPROC, 'NTPROC=', NTPROC
        WRITE(740+IAPROC,*) 'FLCX=', FLCX, ' FLCY=', FLCY
        FLUSH(740+IAPROC)
#endif
!
!/NETCDF_QAD      CALL OUTPUT_NETCDF_QUICK_AND_DIRTY(IMOD, DTG)
!
        IF (GTYPE .EQ. UNGTYPE) THEN
          IF (FLAGLL) THEN
            FACX   =  1./(DERA * RADIUS)
          ELSE
            FACX   =  1.
          END IF
        END IF
        IF ((GTYPE .EQ. UNGTYPE) .and. LPDLIB) THEN
!
#ifdef W3_PDLIB
       IF ((FSTOTALIMP .eqv. .FALSE.).and.(FLCX .or. FLCY)) THEN
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12.1'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
         DO ISPEC=1,NSPEC
           CALL PDLIB_W3XYPUG ( ISPEC, FACX, FACX, DTG,           &
                                VGX, VGY, UGDTUPDATE )
         END DO
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12.2'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
       END IF
#endif
!
#ifdef W3_PDLIB
       IF (FSTOTALIMP .and. (IT .ne. 0)) THEN
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12.3A'
        WRITE(*,*), 'W3WAVE, step 6.12.3A'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
         CALL PDLIB_W3XYPUG_BLOCK_IMPLICIT (FACX, FACX, DTG, VGX, VGY)
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12.4A'
        WRITE(*,*), 'W3WAVE, step 6.12.4A'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
       ELSE IF(FSTOTALEXP .and. (IT .ne. 0)) THEN
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12.3B'
        WRITE(*,*), 'W3WAVE, step 6.12.3B'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
         CALL PDLIB_W3XYPUG_BLOCK_EXPLICIT(FACX, FACX, DTG, VGX, VGY)
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.12.4B'
        WRITE(*,*), 'W3WAVE, step 6.12.4B'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
       ENDIF
#endif
        ELSE
          IF (FLCX .or. FLCY) THEN
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.13'
        FLUSH(740+IAPROC)
#endif
!
#ifdef W3_MPI
       IF ( NRQSG1 .GT. 0 ) THEN
         CALL MPI_STARTALL (NRQSG1, IRQSG1(1,1), IERR_MPI)
         CALL MPI_STARTALL (NRQSG1, IRQSG1(1,2), IERR_MPI)
       END IF
#endif
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.14'
        FLUSH(740+IAPROC)
#endif
!
! Initialize FIELD variable
             FIELD = 0.
!
            DO ISPEC=1, NSPEC
              IF ( IAPPRO(ISPEC) .EQ. IAPROC ) THEN
!
                IF( GTYPE .EQ. SMCTYPE ) THEN
                    IX = 1
#ifdef W3_SMC
 !!Li   Use SMC sub to gether field
                   CALL W3GATHSMC ( ISPEC, FIELD )
#endif
                ELSE IF (.NOT.LPDLIB ) THEN
                  CALL W3GATH ( ISPEC, FIELD )
                END IF   !! GTYPE
!
                IF (GTYPE .EQ. SMCTYPE) THEN
                  IX = 1
#ifdef W3_SMC
   !!Li   Propagation on SMC grid uses UNO2 scheme.
             CALL W3PSMC ( ISPEC, DTG, FIELD )
#endif
!
                ELSE IF (GTYPE .EQ. UNGTYPE) THEN
                  IX = 1
#ifdef W3_MPI
             IF (.NOT. LPDLIB) THEN
#endif
#ifdef W3_PR1
               CALL W3XYPUG ( ISPEC, FACX, FACX, DTG,           &
                              FIELD, VGX, VGY, UGDTUPDATE )
#endif
#ifdef W3_PR2
               CALL W3XYPUG ( ISPEC, FACX, FACX, DTG,           &
                              FIELD, VGX, VGY, UGDTUPDATE )
#endif
#ifdef W3_PR3
               CALL W3XYPUG ( ISPEC, FACX, FACX, DTG,           &
                              FIELD, VGX, VGY, UGDTUPDATE )
#endif
#ifdef W3_MPI
             END IF
#endif
!
                ELSE
                  IX = 1
#ifdef W3_PR1
             CALL W3XYP1 ( ISPEC, DTG, MAPSTA, FIELD, VGX, VGY )
#endif
#ifdef W3_PR2
             CALL W3XYP2 ( ISPEC, DTG, MAPSTA, MAPFS, FIELD, VGX, VGY )
#endif
#ifdef W3_PR3
             CALL W3XYP3 ( ISPEC, DTG, MAPSTA, MAPFS, FIELD, VGX, VGY )
#endif
!
                END IF   !! GTYPE
!
                IF( GTYPE .EQ. SMCTYPE ) THEN
                  IX = 1
#ifdef W3_SMC
 !!Li   Use SMC sub to scatter field
             CALL W3SCATSMC ( ISPEC, MAPSTA, FIELD )
#endif
                ELSE IF (.NOT.LPDLIB ) THEN
                  CALL W3SCAT ( ISPEC, MAPSTA, FIELD )
                END IF   !! GTYPE

              END IF
            END DO
!
#ifdef W3_MPI
       IF ( NRQSG1 .GT. 0 ) THEN
         ALLOCATE ( STATCO(MPI_STATUS_SIZE,NRQSG1) )
         CALL MPI_WAITALL (NRQSG1, IRQSG1(1,1), STATCO, &
                           IERR_MPI)
         CALL MPI_WAITALL (NRQSG1, IRQSG1(1,2), STATCO, &
                           IERR_MPI)
         DEALLOCATE ( STATCO )
         END IF
#endif

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE TIME LOOP 17'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
!Li   Initialise IK IX IY in case ARC option is not used to avoid warnings.
              IK=1
              IX=1
              IY=1
#ifdef W3_SMC
  !Li    Find source boundary spectra and assign to SPCBAC
        IF( ARCTC ) THEN
 
        DO IK = 1, NBAC
           IF( IK .LE. (NBAC-NBGL) ) THEN
                   IY = ICLBAC(IK)
           ELSE
                   IY = NGLO + IK 
           ENDIF 
 
  !Li    Work out root PE (ISPEC) and JSEA numbers for IY 
#ifdef W3_DIST
            ISPEC = MOD( IY-1, NAPROC ) 
             JSEA = 1 + (IY - ISPEC - 1)/NAPROC
#endif
#ifdef W3_SHRD
            ISPEC = 0 
             JSEA = IY 
#endif
#endif
!
#ifdef W3_SMC
 !!Li   Assign boundary cell spectra. 
              IF( IAPROC .EQ. ISPEC+1 ) THEN
                   SPCBAC(:,IK)=VA(:,JSEA)
              ENDIF
#endif
!
#ifdef W3_SMC
 !!Li   Broadcast local SPCBAC(:,IK) to all other PEs.
#ifdef W3_MPI
         CALL MPI_BCAST(SPCBAC(1,IK),NSPEC,MPI_REAL,ISPEC,MPI_COMM_WAVE,IERR_MPI)
         CALL MPI_BARRIER (MPI_COMM_WAVE,IERR_MPI)
#endif
#endif
!
#ifdef W3_SMC
        END DO   !! Loop IK ends.
#endif
!
#ifdef W3_SMC
 !!Li    Update Arctic boundary cell spectra if within local range
           ALLOCATE ( BACSPEC(NSPEC) )
        DO IK = 1, NBAC
           IF( IK .LE. (NBAC-NBGL) ) THEN
                   IX = NGLO + IK 
                   BACANGL = ANGARC(IK)
           ELSE
                   IX = ICLBAC(IK)
                   BACANGL = - ANGARC(IK)
           ENDIF 

 !!Li    Work out boundary PE (ISPEC) and JSEA numbers for IX 
#ifdef W3_DIST
            ISPEC = MOD( IX-1, NAPROC ) 
             JSEA = 1 + (IX - ISPEC - 1)/NAPROC
#endif
#ifdef W3_SHRD
            ISPEC = 0 
             JSEA = IX 
#endif
#endif
!
#ifdef W3_SMC
              IF( IAPROC .EQ. ISPEC+1 ) THEN
                   BACSPEC = SPCBAC(:,IK)
                  
                   CALL w3acturn( NTH, NK, BACANGL, BACSPEC )

                   VA(:,JSEA) = BACSPEC 
 !!Li              WRITE(NDSE,*) "IAPROC, IX, JSEAx, IK=", IAPROC, IX, JSEA, IK 
              ENDIF

        END DO  !! Loop IK ends.
        DEALLOCATE ( BACSPEC )

        ENDIF  !! ARCTC
#endif
!
! End of test FLCX.OR.FLCY
          END IF
!
        END IF

#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
        CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After spatial advection")
#endif
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("After spatial advection")
#endif
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.16'
        WRITE(740+IAPROC,*) 'NTLOC=', NTLOC
        WRITE(740+IAPROC,*) 'ITLOCH=', ITLOCH
        FLUSH(740+IAPROC)
#endif
!
! 3.6.4 Intra-spectral part 2
!
          IF ( FLCTH .OR. FLCK ) THEN
              DO ITLOC=ITLOCH+1, NTLOC
!
#ifdef W3_OMPG
!$OMP PARALLEL PRIVATE (JSEA,ISEA,IX,IY,DEPTH,IXrel)
!$OMP DO SCHEDULE (DYNAMIC,1)
#endif
!
#ifdef W3_DEBUGRUN
                WRITE(740+IAPROC,*) ' ITLOC=', ITLOC
                WRITE(740+IAPROC,*) ' 2: Before call to W3KTP1 / W3KTP2 / W3KTP3'
#endif
                DO JSEA = 1, NSEAL

                  CALL INIT_GET_ISEA(ISEA, JSEA)
                  IX     = MAPSF(ISEA,1)
                  IY     = MAPSF(ISEA,2)
#ifdef W3_DEBUGRUN
                  IF (JSEA == DEBUG_NODE) WRITE(*,*) 'W3WAVE TEST', SUM(VA(:,JSEA))
#endif
                  DEPTH  = MAX ( DMIN , DW(ISEA) )

                  IF ( GTYPE .EQ. UNGTYPE ) THEN
                    IF (IOBP(ISEA) .NE. 1) CYCLE
                  ENDIF

                  IF ( MAPSTA(IY,IX) .EQ. 1 ) THEN
                           IF (LPDLIB) THEN
                             IXrel = JSEA
                           ELSE
                             IXrel = IX
                           END IF
!
                      IF( GTYPE .EQ. SMCTYPE ) THEN
                          J = 1
#ifdef W3_SMC
 !!Li    Refraction and GCT in theta direction is done by rotation.
                      CALL W3KRTN ( ISEA, FACTH, FACK, CTHG0S(ISEA), &
                           CG(:,ISEA), WN(:,ISEA), DEPTH,            &
                           DHDX(ISEA), DHDY(ISEA), DHLMT(:,ISEA),    &
                           CX(ISEA), CY(ISEA), DCXDX(IY,IX),         &
                           DCXDY(IY,IX), DCYDX(IY,IX), DCYDY(IY,IX), & 
                           DCDX(:,IY,IX), DCDY(:,IY,IX), VA(:,JSEA) )  
#endif
!
                      ELSE
                          J = 1
#ifdef W3_PR1
                      CALL W3KTP1 ( ISEA, FACTH, FACK, CTHG0S(ISEA),       &
                           CG(:,ISEA), WN(:,ISEA), DEPTH,                  &
                           DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
                           CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
                           DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
                           DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA))
#endif
#ifdef W3_PR2
                      CALL W3KTP2 ( ISEA, FACTH, FACK, CTHG0S(ISEA),       &
                           CG(:,ISEA), WN(:,ISEA), DEPTH,                  &
                           DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
                           CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
                           DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
                           DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA))
#endif
#ifdef W3_PR3
                      CALL W3KTP3 ( ISEA, FACTH, FACK, CTHG0S(ISEA),       &
                           CG(:,ISEA), WN(:,ISEA), DEPTH,                  &
                           DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
                           CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
                           DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
                           DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA), &
                           CFLTHMAX(JSEA), CFLKMAX(JSEA) )
#endif
!
                      END IF  !! GTYPE 
!
                    END IF
                  END DO
!
#ifdef W3_OMPG
!$OMP END DO
!$OMP END PARALLEL
#endif
!
                END DO
            END IF
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
       CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After intraspectral adv.")
#endif
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("fter intraspectral adv.")
#endif
!
          UGDTUPDATE = .FALSE.
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.17'
        WRITE(740+IAPROC,*) 'FSSOURCE=', FSSOURCE
        FLUSH(740+IAPROC)
#endif
!
! 3.6 End propapgation  = = = = = = = = = = = = = = = = = = = = = = = =

! 3.7 Calculate and integrate source terms.
!
  370     CONTINUE
          IF ( FLSOU ) THEN
!
            D50=0.0002
            REFLEC(:)=0.
            REFLED(:)=0
            PSIC=0.
#ifdef W3_PDLIB
#ifdef W3_DEBUGSRC
          WRITE(740+IAPROC,*) 'ITIME=', ITIME, ' IT=', IT
          CALL ALL_VAOLD_INTEGRAL_PRINT("VAOLD before W3SRCE_IMP_POST")
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA before W3SRCE_IMP_POST")
          IF (DEBUG_NODE .le. NSEAL) THEN
            WRITE(740+IAPROC,*) '     Values for DEBUG_NODE=', DEBUG_NODE
            WRITE(740+IAPROC,*) '     sum(VA)=', sum(VA(:,DEBUG_NODE))
            WRITE(740+IAPROC,*) '     sum(VAOLD)=', sum(VAOLD(:,DEBUG_NODE))
            WRITE(740+IAPROC,*) '     sum(VSTOT)=', sum(VSTOT(:,DEBUG_NODE))
            WRITE(740+IAPROC,*) '     sum(VDTOT)=', sum(VDTOT(:,DEBUG_NODE))
          END IF
#endif
#endif
!
#ifdef W3_OMPG
!$OMP PARALLEL PRIVATE (JSEA,ISEA,IX,IY,DELA,DELX,DELY,        &
!$OMP&                  REFLEC,REFLED,D50,PSIC,TMP1,TMP2,TMP3,TMP4)
!$OMP DO SCHEDULE (DYNAMIC,1)
#endif
!
              DO JSEA=1, NSEAL
                CALL INIT_GET_ISEA(ISEA, JSEA)
                IX     = MAPSF(ISEA,1)
                IY     = MAPSF(ISEA,2)
                DELA=1.
                DELX=1.
                DELY=1.
#ifdef W3_REF1
                IF (GTYPE.EQ.RLGTYPE) THEN
                  DELX=SX*CLATS(ISEA)/FACX
                  DELY=SY/FACX
                  DELA=DELX*DELY
                END IF
                IF (GTYPE.EQ.CLGTYPE) THEN
! Maybe what follows works also for RLGTYPE ... to be verified
                  DELX=HPFAC(IY,IX)/ FACX
                  DELY=HQFAC(IY,IX)/ FACX
                  DELA=DELX*DELY
                END IF
#endif
!
#ifdef W3_REF1
          REFLEC=REFLC(:,ISEA)
          REFLEC(4)=BERG(ISEA)*REFLEC(4)
          REFLED=REFLD(:,ISEA)
#endif
#ifdef W3_BT4
           D50=SED_D50(ISEA)
           PSIC=SED_PSIC(ISEA)
#endif

#ifdef W3_DEBUGRUN
          IF (JSEA == DEBUG_NODE) WRITE(*,*) 'W3WAVE TEST', ISEA, JSEA, SUM(VA(:,JSEA))
#endif

                IF ( MAPSTA(IY,IX) .EQ. 1 .AND. FLAGST(ISEA)) THEN
                     TMP1   = WHITECAP(JSEA,1:4)
                     TMP2   = BEDFORMS(JSEA,1:3)
                     TMP3   = TAUBBL(JSEA,1:2)
                     TMP4   = TAUICE(JSEA,1:2)
#ifdef W3_PDLIB
              IF (FSSOURCE) THEN
                   CALL W3SRCE(srce_imp_post,IT,JSEA,IX,IY,IMOD,  &
                      VAOLD(:,JSEA), VA(:,JSEA),                  &
                      VSTOT(:,JSEA),VDTOT(:,JSEA),SHAVETOT(JSEA), &
                      ALPHA(1:NK,JSEA), WN(1:NK,ISEA),            &
                      CG(1:NK,ISEA), DW(ISEA), U10(ISEA),         &
                      U10D(ISEA), AS(ISEA), UST(ISEA),            &
                      USTDIR(ISEA), CX(ISEA), CY(ISEA),           &
                      ICE(ISEA), ICEH(ISEA), ICEF(ISEA),          &
                      ICEDMAX(ISEA),                              &
                      REFLEC, REFLED, DELX, DELY, DELA,           &
                      TRNX(IY,IX), TRNY(IY,IX), BERG(ISEA),       &
                      FPIS(ISEA), DTDYN(JSEA),                    &
                      FCUT(JSEA), DTG, TAUWX(JSEA), TAUWY(JSEA),  &
                      TAUOX(JSEA), TAUOY(JSEA), TAUWIX(JSEA),     &
                      TAUWIY(JSEA), TAUWNX(JSEA),                 &
                      TAUWNY(JSEA),  PHIAW(JSEA), CHARN(JSEA),    &
                      TWS(JSEA),PHIOC(JSEA), TMP1, D50, PSIC, TMP2,&
                      PHIBBL(JSEA), TMP3, TMP4, PHICE(JSEA),      &
                      TAUOCX(JSEA), TAUOCY(JSEA), WNMEAN(JSEA),   &
                      RHOAIR(ISEA), ASF(ISEA))
              ELSE
#endif
                       CALL W3SRCE(srce_direct, IT, JSEA, IX, IY, IMOD, &
                            VAoldDummy, VA(:,JSEA),                     &
                            VSioDummy, VDioDummy, SHAVETOTioDummy,      &
                            ALPHA(1:NK,JSEA), WN(1:NK,ISEA),            &
                            CG(1:NK,ISEA), DW(ISEA), U10(ISEA),         &
                            U10D(ISEA),                                 &
#ifdef W3_FLX5
                            TAUA(ISEA), TAUADIR(ISEA),                  &
#endif
                            AS(ISEA), UST(ISEA),                        &
                            USTDIR(ISEA), CX(ISEA), CY(ISEA),           &
                            ICE(ISEA), ICEH(ISEA), ICEF(ISEA),          &
                            ICEDMAX(ISEA),                              &
                            REFLEC, REFLED, DELX, DELY, DELA,           &
                            TRNX(IY,IX), TRNY(IY,IX), BERG(ISEA),       &
                            FPIS(ISEA), DTDYN(JSEA),                    &
                            FCUT(JSEA), DTG, TAUWX(JSEA), TAUWY(JSEA),  &
                            TAUOX(JSEA), TAUOY(JSEA), TAUWIX(JSEA),     &
                            TAUWIY(JSEA), TAUWNX(JSEA),                 &
                            TAUWNY(JSEA),  PHIAW(JSEA), CHARN(JSEA),    &
                            TWS(JSEA), PHIOC(JSEA), TMP1, D50, PSIC,TMP2,&
                            PHIBBL(JSEA), TMP3, TMP4 , PHICE(JSEA),     &
                            TAUOCX(JSEA), TAUOCY(JSEA), WNMEAN(JSEA),   &
                            RHOAIR(ISEA), ASF(ISEA))
#ifdef W3_PDLIB
              END IF
#endif
                     WHITECAP(JSEA,1:4) = TMP1
                     BEDFORMS(JSEA,1:3) = TMP2
                     TAUBBL(JSEA,1:2) = TMP3
                     TAUICE(JSEA,1:2) = TMP4
                ELSE
                    UST   (ISEA) = UNDEF
                    USTDIR(ISEA) = UNDEF
                    DTDYN (JSEA) = UNDEF
                    FCUT  (JSEA) = UNDEF
!                    VA(:,JSEA)  = 0.
                END IF
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'RET: min/max/sum(VA)=',minval(VA(:,JSEA)),maxval(VA(:,JSEA)),sum(VA(:,JSEA))
#endif
              END DO
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'min/max/sum(VAtot)=', minval(VA), maxval(VA), sum(VA)
        FLUSH(740+IAPROC)
#endif

#ifdef W3_DEBUGRUN
      DO JSEA = 1, NSEAL
        DO IS = 1, NSPEC
          IF (VA(IS, JSEA) .LT. 0.) THEN
            WRITE(740+IAPROC,*) 'TEST W3WAVE 9', VA(IS,JSEA)
            CALL FLUSH(740+IAPROC)
          ENDIF
        ENDDO
      ENDDO
      IF (SUM(VA) .NE. SUM(VA)) THEN
        WRITE(740+IAPROC,*) 'NAN in ACTION 7', IX, IY, SUM(VA)
        CALL FLUSH(740+IAPROC)
        STOP
      ENDIF
#endif
!
#ifdef W3_OMPG
!$OMP END DO
!$OMP END PARALLEL
#endif
!
#ifdef W3_PDLIB
#ifdef W3_DEBUGSRC
          WRITE(740+IAPROC,*) 'ITIME=', ITIME, ' IT=', IT
          CALL ALL_VAOLD_INTEGRAL_PRINT("VAOLD after W3SRCE_IMP_PRE_POST")
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA after W3SRCE_IMP_PRE_POST")
          IF (DEBUG_NODE .le. NSEAL) THEN
            WRITE(740+IAPROC,*) '     Values for DEBUG_NODE=', DEBUG_NODE
            WRITE(740+IAPROC,*) '     sum(VA)=', sum(VA(:,DEBUG_NODE))
            WRITE(740+IAPROC,*) '     min/max(VA)=', minval(VA(:,DEBUG_NODE)), maxval(VA(:,DEBUG_NODE))
          END IF
#endif
#endif

!
! This barrier is from older code versions. It has been removed in 3.11
! to optimize IO2/3 settings. May be needed on some systems still
!
!!/MPI              IF (FLAG0) CALL MPI_BARRIER (MPI_COMM_WCMP,IERR_MPI)
!!/MPI            ELSE
!!/MPI              CALL MPI_BARRIER (MPI_COMM_WCMP,IERR_MPI)
!
            END IF
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.18'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
          CALL ALL_VA_INTEGRAL_PRINT(IMOD, "After source terms")
#endif
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("After source terms")
#endif
!
! End of interations for DTMAX < 1s
!
#ifdef W3_SEC1
       IF (IT.EQ.0) EXIT
       END DO
       IF (IT.GT.0) DTG=DTGTEMP
#endif
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.19'
        FLUSH(740+IAPROC)
      DO JSEA = 1, NSEAL
        DO IS = 1, NSPEC
          IF (VA(IS, JSEA) .LT. 0.) THEN
            WRITE(740+IAPROC,*) 'TEST W3WAVE 10', VA(IS,JSEA)
            CALL FLUSH(740+IAPROC)
          ENDIF
        ENDDO
     ENDDO
      IF (SUM(VA) .NE. SUM(VA)) THEN
        WRITE(740+IAPROC,*) 'NAN in ACTION 8', IX, IY, SUM(VA)
        CALL FLUSH(740+IAPROC)
        STOP
      ENDIF
#endif
!
! 3.8 Update global time step.
!     (Branch point FLDRY, IT=0)
!
  380     CONTINUE
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.20'
        FLUSH(740+IAPROC)
#endif
          IF (IT.NE.NT) THEN
              DTTST  = DSEC21 ( TIME , TCALC )
              DTG    = DTTST / REAL(NT-IT)
            END IF
!
          IF ( FLACT .AND. IT.NE.NT .AND. IAPROC.EQ.NAPLOG ) THEN
              CALL STME21 ( TIME , IDTIME )
              IF ( IDLAST .NE. TIME(1) ) THEN
                  WRITE (NDSO,900) ITIME, IPASS, IDTIME(01:19),       &
                                   IDACT, OUTID
                  IDLAST = TIME(1)
                ELSE
                  WRITE (NDSO,901) ITIME, IPASS, IDTIME(12:19),       &
                                   IDACT, OUTID
                END IF
              FLACT  = .FALSE.
              IDACT  = '         '
          END IF
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.21'
        FLUSH(740+IAPROC)
#endif
!
#ifdef W3_PDLIB
#ifdef W3_DEBUGCOH
            CALL ALL_VA_INTEGRAL_PRINT(IMOD, "end of time loop")
#endif
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("end of time loop")
#endif
!
!
        END DO

#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.21.1'
        FLUSH(740+IAPROC)
#endif
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("W3WAVE, step 6.21.1")
#endif
!
#ifdef W3_T
      WRITE (NDST,9030)
#endif
#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE END TIME LOOP'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
!     End of loop over time steps
! ==================================================================== /
!
  400 CONTINUE
!
! 4.  Perform output to file if requested ---------------------------- /
! 4.a Check if time is output time
!     Delay if data assimilation time.
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.21.2'
        FLUSH(740+IAPROC)
#endif
!
        IF ( TOFRST(1)  .EQ. -1 ) THEN
            DTTST  = 1.
          ELSE
            DTTST   = DSEC21 ( TIME, TOFRST )
          END IF
!
        IF ( TDN(1)  .EQ. -1 ) THEN
            DTTST1 = 1.
          ELSE
            DTTST1  = DSEC21 ( TIME, TDN )
          END IF
!
        DTTST2 = DSEC21 ( TIME, TEND )
        FLAG_O = .NOT.SKIP_O .OR. ( SKIP_O .AND. DTTST2.NE.0. )
!
#ifdef W3_T
        WRITE (NDST,9040) TOFRST, TDN, DTTST, DTTST1, FLAG_O
#endif
!
#ifdef W3_DEBUGRUN
        WRITE(740+IAPROC,*) 'W3WAVE, step 6.21.3'
        FLUSH(740+IAPROC)
#endif
        IF ( DTTST.LE.0. .AND. DTTST1.NE.0. .AND. FLAG_O ) THEN
#ifdef W3_DEBUGRUN
            WRITE(740+IAPROC,*) 'W3WAVE, step 6.21.4'
            FLUSH(740+IAPROC)
#endif
!
#ifdef W3_T
          WRITE (NDST,9041)
#endif
!
! 4.b Processing and MPP preparations
!
            IF ( FLOUT(1) ) THEN
                FLOUTG = DSEC21(TIME,TONEXT(:,1)).EQ.0.
              ELSE
                FLOUTG = .FALSE.
              END IF
!
            IF ( FLOUT(7) ) THEN
                FLOUTG2 = DSEC21(TIME,TONEXT(:,7)).EQ.0.
              ELSE
                FLOUTG2 = .FALSE.
              END IF
!
          FLPART = .FALSE.
          IF ( FLOUT(1) .AND. FLPFLD )                               &
               FLPART = FLPART .OR. DSEC21(TIME,TONEXT(:,1)).EQ.0.
#ifdef W3_DEBUGRUN
            WRITE(740+IAPROC,*) 'W3WAVE, step 6.21.7'
            FLUSH(740+IAPROC)
#endif
          IF ( FLOUT(6) )                                            &
               FLPART = FLPART .OR. DSEC21(TIME,TONEXT(:,6)).EQ.0.
#ifdef W3_DEBUGRUN
            WRITE(740+IAPROC,*) 'W3WAVE, step 6.21.8'
            FLUSH(740+IAPROC)
#endif
!
#ifdef W3_T
            WRITE (NDST,9042) LOCAL, FLPART, FLOUTG
#endif
!
            IF ( LOCAL .AND. FLPART ) CALL W3CPRT ( IMOD )
            IF ( LOCAL .AND. (FLOUTG .OR. FLOUTG2) )                   &
                 CALL W3OUTG ( VA, FLPFLD, FLOUTG, FLOUTG2 )
!
#ifdef W3_MPI
            FLGMPI = .FALSE.
            NRQMAX = 0
#endif
!
#ifdef W3_MPI
     IF ( ( (DSEC21(TIME,TONEXT(:,1)).EQ.0.) .AND. FLOUT(1) ) .OR. &
          (  (DSEC21(TIME,TONEXT(:,7)).EQ.0.) .AND. FLOUT(7) .AND. &
             SBSED ) ) THEN
       IF (.NOT. LPDLIB .or. (GTYPE.ne.UNGTYPE)) THEN
         IF (NRQGO.NE.0 ) THEN
#endif
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'BEFORE STARTALL NRQGO.NE.0 , step 0', &
                 NRQGO, IRQGO, GTYPE, UNGTYPE, .NOT. LPDLIB .or. (GTYPE.ne.UNGTYPE)
      FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
           CALL MPI_STARTALL ( NRQGO, IRQGO , IERR_MPI )
#endif
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'AFTER STARTALL NRQGO.NE.0, step 0'
      FLUSH(740+IAPROC)
#endif

#ifdef W3_MPI
           FLGMPI(0) = .TRUE.
           NRQMAX    = MAX ( NRQMAX , NRQGO )
#endif
#ifdef W3_MPIT
          WRITE (NDST,9043) '1a', NRQGO, NRQMAX, NAPFLD
#endif
#ifdef W3_MPI
         END IF
#endif
!
#ifdef W3_MPI
         IF (NRQGO2.NE.0 ) THEN
#endif
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'BEFORE STARTALL NRQGO2.NE.0, step 0', &
              NRQGO2, IRQGO2, GTYPE, UNGTYPE, .NOT. LPDLIB .or. (GTYPE.ne.UNGTYPE)
      FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
           CALL MPI_STARTALL ( NRQGO2, IRQGO2, IERR_MPI )
#endif
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'AFTER STARTALL NRQGO2.NE.0, step 0'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
           FLGMPI(1) = .TRUE.
           NRQMAX    = MAX ( NRQMAX , NRQGO2 )
#endif
#ifdef W3_MPIT
          WRITE (NDST,9043) '1b', NRQGO2, NRQMAX, NAPFLD
#endif
#ifdef W3_MPI
         END IF
       ELSE
#endif
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'BEFORE DO_OUTPUT_EXCHANGES, step 0'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_PDLIB
       CALL DO_OUTPUT_EXCHANGES(IMOD)
#endif
#ifdef W3_MPI
       END IF
     END IF
#endif

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE AFTER TIME LOOP 1'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

!
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'After DO_OUTPUT_EXCHANGES, step 1'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
            IF ( FLOUT(2) .AND. NRQPO.NE.0 ) THEN
                IF ( DSEC21(TIME,TONEXT(:,2)).EQ.0. ) THEN
                    CALL MPI_STARTALL ( NRQPO, IRQPO1, IERR_MPI )
                    FLGMPI(2) = .TRUE.
                    NRQMAX    = MAX ( NRQMAX , NRQPO )
#endif
#ifdef W3_MPIT
                    WRITE (NDST,9043) '2 ', NRQPO, NRQMAX, NAPPNT
#endif
#ifdef W3_MPI
                  END IF
              END IF
#endif
!
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'After DO_OUTPUT_EXCHANGES, step 2'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
            IF ( FLOUT(4) .AND. NRQRS.NE.0 ) THEN
                IF ( DSEC21(TIME,TONEXT(:,4)).EQ.0. ) THEN
                    CALL MPI_STARTALL ( NRQRS, IRQRS , IERR_MPI )
                    FLGMPI(4) = .TRUE.
                    NRQMAX    = MAX ( NRQMAX , NRQRS )
#endif
#ifdef W3_MPIT
                    WRITE (NDST,9043) '4 ', NRQRS, NRQMAX, NAPRST
#endif
#ifdef W3_MPI
                  END IF
              END IF
#endif
!
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'After DO_OUTPUT_EXCHANGES, step 2'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
            IF ( FLOUT(8) .AND. NRQRS.NE.0 ) THEN
                IF ( DSEC21(TIME,TONEXT(:,8)).EQ.0. ) THEN
                    CALL MPI_STARTALL ( NRQRS, IRQRS , IERR_MPI )
                    FLGMPI(8) = .TRUE.
                    NRQMAX    = MAX ( NRQMAX , NRQRS )
#endif
#ifdef W3_MPIT
                    WRITE (NDST,9043) '8 ', NRQRS, NRQMAX, NAPRST
#endif
#ifdef W3_MPI
                  END IF
              END IF
#endif
!
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'After DO_OUTPUT_EXCHANGES, step 3'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
            IF ( FLOUT(5) .AND. NRQBP.NE.0 ) THEN
                IF ( DSEC21(TIME,TONEXT(:,5)).EQ.0. ) THEN
                    CALL MPI_STARTALL ( NRQBP , IRQBP1, IERR_MPI )
                    FLGMPI(5) = .TRUE.
                    NRQMAX    = MAX ( NRQMAX , NRQBP )
#endif
#ifdef W3_MPIT
                    WRITE (NDST,9043) '5a', NRQBP, NRQMAX, NAPBPT
#endif
#ifdef W3_MPI
                  END IF
              END IF
#endif
!
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'After DO_OUTPUT_EXCHANGES, step 4'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
            IF ( FLOUT(5) .AND. NRQBP2.NE.0 .AND.                &
                 IAPROC.EQ.NAPBPT) THEN
                IF ( DSEC21(TIME,TONEXT(:,5)).EQ.0. ) THEN
                    CALL MPI_STARTALL (NRQBP2,IRQBP2,IERR_MPI)
                    NRQMAX    = MAX ( NRQMAX , NRQBP2 )
#endif
#ifdef W3_MPIT
                    WRITE (NDST,9043) '5b', NRQBP2, NRQMAX, NAPBPT
#endif
#ifdef W3_MPI
                  END IF
              END IF
#endif
!
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'After DO_OUTPUT_EXCHANGES, step 5'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_MPI
           IF ( NRQMAX .NE. 0 ) ALLOCATE                         &
                                 ( STATIO(MPI_STATUS_SIZE,NRQMAX) )
#endif

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE AFTER TIME LOOP 2'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

!
! 4.c Reset next output time

#ifdef W3_DEBUGRUN
      IF (MINVAL(VA) .LT. 0.) THEN
        WRITE(740+IAPROC,*) 'TEST W3WAVE 12', SUM(VA), MINVAL(VA), MAXVAL(VA)
        CALL FLUSH(740+IAPROC)
        STOP
      ENDIF
      IF (SUM(VA) .NE. SUM(VA)) THEN
        WRITE(740+IAPROC,*) 'NAN in ACTION 9', IX, IY, SUM(VA)
        CALL FLUSH(740+IAPROC)
        STOP
      ENDIF
#endif
!
            TOFRST(1) = -1
            TOFRST(2) =  0
!
            DO J=1, NOTYPE
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'NOTYPE, J=', J
      FLUSH(740+IAPROC)
#endif

              IF ( FLOUT(J) ) THEN
!
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'Matching FLOUT(J)'
      FLUSH(740+IAPROC)
#endif
!
! 4.d Perform output
!
#ifdef W3_NL5
              IF (J .EQ. 2) TOSNL5  = TONEXT(:, 2)
#endif
                  TOUT(:) = TONEXT(:,J)
                  DTTST   = DSEC21 ( TIME, TOUT )
!
                  IF ( DTTST .EQ. 0. ) THEN
                      IF ( ( J .EQ. 1 )              &
#ifdef W3_SBS
                           .OR. ( J .EQ. 7 )         &
#endif
                                        ) THEN
                          IF ( IAPROC .EQ. NAPFLD ) THEN
#ifdef W3_MPI
                              IF ( FLGMPI(1) ) CALL MPI_WAITALL  &
                                 ( NRQGO2, IRQGO2, STATIO, IERR_MPI )
                              FLGMPI(1) = .FALSE.
#endif
!
#ifdef W3_SBS
                              IF ( J .EQ. 1 ) THEN
#endif
                                CALL W3IOGO( 'WRITE', NDS(7), ITEST, IMOD )
#ifdef W3_SBS
                              ENDIF
#endif
!
#ifdef W3_SBS
 !
 !     Generate output flag file for fields and SBS coupling.
 !
                              JJ = LEN_TRIM ( FILEXT )
                              CALL STME21 ( TIME, IDTIME )
                              FOUTNAME = 'Field_done.' // IDTIME(1:4) &
                                       // IDTIME(6:7) // IDTIME(9:10) &
                                      // IDTIME(12:13) // '.' // FILEXT(1:JJ)
#endif
!
#ifdef W3_SBS
                              OPEN( UNIT=NDSOFLG, FILE=FOUTNAME)
                              CLOSE( NDSOFLG )
#endif
                            END IF
!
                        ELSE IF ( J .EQ. 2 ) THEN
!
!   Point output
!
                          IF ( IAPROC .EQ. NAPPNT ) THEN
!
!   Gets the necessary spectral data
!
                            CALL W3IOPE ( VA )
                            CALL W3IOPO ( 'WRITE', NDS(8), ITEST, IMOD )
                            END IF
!
                        ELSE IF ( J .EQ. 3 ) THEN
!
! Track output
!
                          CALL W3IOTR ( NDS(11), NDS(12), VA, IMOD )
                        ELSE IF ( J .EQ. 4 ) THEN
                          CALL W3IORS ('HOT', NDS(6), XXX, IMOD, FLOUT(8) )
                          ITEST = RSTYPE
                        ELSE IF ( J .EQ. 5 ) THEN
                          IF ( IAPROC .EQ. NAPBPT ) THEN
#ifdef W3_MPI
                              IF (NRQBP2.NE.0) CALL MPI_WAITALL  &
                                ( NRQBP2, IRQBP2,STATIO, IERR_MPI )
#endif
                              CALL W3IOBC ( 'WRITE', NDS(10),         &
                                            TIME, TIME, ITEST, IMOD )
                            END IF
                        ELSE IF ( J .EQ. 6 ) THEN
                          CALL W3IOSF ( NDS(13), IMOD )
#ifdef W3_OASIS
                      ELSE IF ( J .EQ. 7 ) THEN
                        !
                        ! Send variables to atmospheric or ocean circulation or ice model
                        !
                        IF (DTOUT(7).NE.0) THEN
                          IF ( (MOD(ID_OASIS_TIME,NINT(DTOUT(7))) .EQ. 0 ) .AND. &
                               (DSEC21 (TIME00, TIME) .GT. 0.0) ) THEN 
                            IF ( (CPLT0 .AND. (DSEC21 (TIME, TIMEN) .GT. 0.0)) .OR. &
                                  .NOT. CPLT0 ) THEN
                              IF (CPLT0) ID_OASIS_TIME = NINT(DSEC21 ( TIME00 , TIME ))

#endif
#ifdef W3_OASACM
                             CALL SND_FIELDS_TO_ATMOS()
#endif
#ifdef W3_OASOCM
                             CALL SND_FIELDS_TO_OCEAN()
#endif
#ifdef W3_OASICM
                             CALL SND_FIELDS_TO_ICE()
#endif
#ifdef W3_OASIS
                              IF (.NOT. CPLT0) ID_OASIS_TIME = NINT(DSEC21 ( TIME00 , TIME ))
                            ENDIF
                          ENDIF
                        ENDIF
#endif
                        END IF
!
                      CALL TICK21 ( TOUT, DTOUT(J) )
                      TONEXT(:,J) = TOUT
                      TLST        = TOLAST(:,J)
                      DTTST       = DSEC21 ( TOUT , TLST )
                      FLOUT(J)    = DTTST.GE.0.
                      IF ( FLOUT(J) ) THEN
                          OUTID(2*J-1:2*J-1) = 'X'
#ifdef W3_OASIS
                          IF ( (DTOUT(7).NE.0) .AND.           &
                               (DSEC21(TIME,TIME00).EQ.0 .OR.  &
                                DSEC21(TIME,TIMEEND).EQ.0) ) OUTID(13:13) = ' '
#endif
                        ELSE
                          OUTID(2*J-1:2*J-1) = 'L'
                        END IF
                    END IF
!
! 4.e Update next output time
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


! If there is a second stream of restart files then J=8 and FLOUT(8)=.TRUE.
            J=8
            IF ( FLOUT(J) ) THEN
!
#ifdef W3_DEBUGRUN
      WRITE(740+IAPROC,*) 'Matching FLOUT(J)'
      FLUSH(740+IAPROC)
#endif
!
! 4.d Perform output
!
              TOUT(:) = TONEXT(:,J)
              DTTST   = DSEC21 ( TIME, TOUT )
              IF ( DTTST .EQ. 0. ) THEN
                CALL W3IORS ('HOT', NDS(6), XXX, IMOD, FLOUT(8) )
                 ITEST = RSTYPE
                 CALL TICK21 ( TOUT, DTOUT(J) )
                 TONEXT(:,J) = TOUT
                 TLST        = TOLAST(:,J)
                 DTTST       = DSEC21 ( TOUT , TLST )
                 FLOUT(J)    = DTTST.GE.0.
                 IF ( FLOUT(J) ) THEN
                   OUTID(2*J-1:2*J-1) = 'X'
#ifdef W3_OASIS
                     IF ( (DTOUT(7).NE.0) .AND.           &
                        (DSEC21(TIME,TIME00).EQ.0 .OR.  &
                        DSEC21(TIME,TIMEEND).EQ.0) ) OUTID(13:13) = ' '
#endif
                  ELSE
                    OUTID(2*J-1:2*J-1) = 'L'
                  END IF
              END IF
!
! 4.e Update next output time
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
            END IF
!        END OF CHECKPOINT
!
#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE AFTER TIME LOOP 3'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

!
#ifdef W3_MPI
            IF ( FLGMPI(0) ) CALL MPI_WAITALL                    &
                             ( NRQGO, IRQGO , STATIO, IERR_MPI )
            IF ( FLGMPI(2) ) CALL MPI_WAITALL                    &
                             ( NRQPO, IRQPO1, STATIO, IERR_MPI )
            IF ( FLGMPI(4) ) CALL MPI_WAITALL                    &
                             ( NRQRS, IRQRS , STATIO, IERR_MPI )
            IF ( FLGMPI(8) ) CALL MPI_WAITALL                    &
                             ( NRQRS, IRQRS , STATIO, IERR_MPI )
            IF ( FLGMPI(5) ) CALL MPI_WAITALL                    &
                             ( NRQBP, IRQBP1, STATIO, IERR_MPI )
            IF ( NRQMAX .NE. 0 ) DEALLOCATE ( STATIO )
#endif
!
#ifdef W3_T
          WRITE (NDST,9044)
#endif
!
! This barrier is from older code versions. It has been removed in 3.11
! to optimize IO2/3 settings. May be needed on some systems still
!
!!/MPI            IF (FLDRY) CALL MPI_BARRIER (MPI_COMM_WAVE,IERR_MPI)
!
          END IF
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("Before update log file")
#endif

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE AFTER TIME LOOP 4'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

!
! 5.  Update log file ------------------------------------------------ /

!      IF (MINVAL(VA) .LT. 0.) THEN
!        WRITE(740+IAPROC,*) 'TEST W3WAVE 13', SUM(VA), MINVAL(VA), MAXVAL(VA)
!        CALL FLUSH(740+IAPROC)
!        STOP
!      ENDIF
!
        IF ( IAPROC.EQ.NAPLOG ) THEN
!
            CALL STME21 ( TIME , IDTIME )
            IF ( FLCUR ) THEN
                DTTST  = DSEC21 ( TIME , TCN )
                IF ( DTTST .EQ. 0. ) IDACT(7:7) = 'X'
              END IF
            IF ( FLWIND ) THEN
                DTTST  = DSEC21 ( TIME , TWN )
                IF ( DTTST .EQ. 0. ) IDACT(3:3) = 'X'
              END IF
            IF ( FLTAUA ) THEN
                DTTST  = DSEC21 ( TIME , TUN )
                IF ( DTTST .EQ. 0. ) IDACT(9:9) = 'X'
              END IF
            IF ( FLRHOA ) THEN
                DTTST  = DSEC21 ( TIME , TRN )
                IF ( DTTST .EQ. 0. ) IDACT(11:11) = 'X'
              END IF
            IF ( TDN(1) .GT. 0  ) THEN
                DTTST  = DSEC21 ( TIME , TDN )
                IF ( DTTST .EQ. 0. ) IDACT(21:21) = 'X'
              END IF
!
            IF ( IDLAST.NE.TIME(1) ) THEN
                WRITE (NDSO,900) ITIME, IPASS, IDTIME(1:19),          &
                                 IDACT, OUTID
                IDLAST = TIME(1)
              ELSE
                WRITE (NDSO,901) ITIME, IPASS, IDTIME(12:19),         &
                                 IDACT, OUTID
              END IF
!
          END IF
!
        IDACT  = '         '
        OUTID  = '           '
        FLACT  = .FALSE.
!
! 6.  If time is not ending time, branch back to 2 ------------------- /
!
        DTTST  = DSEC21 ( TIME, TEND )
        IF ( DTTST .EQ. 0. ) EXIT
#ifdef W3_TIMINGS
         CALL PRINT_MY_TIME("Continuing the loop")
#endif
      END DO

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE AFTER TIME LOOP 5'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!

      IF ( TSTAMP .AND. SCREEN.NE.NDSO .AND. IAPROC.EQ.NAPOUT ) THEN
         CALL WWTIME ( STTIME )
         WRITE (SCREEN,951) STTIME
      END IF

      IF ( IAPROC .EQ. NAPLOG ) WRITE (NDSO,902)
!
      DEALLOCATE(FIELD)
      DEALLOCATE(TAUWX, TAUWY)
!
#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'WW3_WAVE END W3WAVE'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif
!
      RETURN
!
! Formats
!
  900 FORMAT (4X,I6,'|',I6,'| ', A19  ,' | ',A,' | ',A,' |')
  901 FORMAT (4X,I6,'|',I6,'| ',11X,A8,' | ',A,' | ',A,' |')
  902 FORMAT (2X,'--------+------+---------------------+'             &
                ,'-------------------+---------------+')
!
#ifdef W3_IC3
  920 FORMAT ('     Updating k and Cg from ice param. 1,2,3,4.'/)
#endif
  950 FORMAT ('  WAVEWATCH III calculating for ',A,' at ',A)
  951 FORMAT ('  WAVEWATCH III reached the end of a computation',     &
               ' loop at ',A)
 1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     ENDING TIME BEFORE STARTING TIME '/)
 1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     NEW WATER LEVEL BEFORE OLD WATER LEVEL '/)
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     ILLEGAL CURRENT INTERVAL '/)
 1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     ILLEGAL WIND INTERVAL '/)
 1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     NEW ICE FIELD BEFORE OLD ICE FIELD '/)
 1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     NEW IC1 FIELD BEFORE OLD IC1 FIELD '/)
 1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     NEW ATM MOMENTUM BEFORE OLD ATM MOMENTUM '/)
 1008 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     NEW AIR DENSITY BEFORE OLD AIR DENSITY '/)
#ifdef W3_IS2
 1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     NEW IC5 FIELD BEFORE OLD IC5 FIELD '/)
#endif
 1030 FORMAT (/' *** WAVEWATCH III WARING IN W3WAVE :'/               &
               '     AT LEAST ONE PROCESSOR HAS 0 ACTIVE POINTS',     &
                   ' IN GRID',I3)
#ifdef W3_REFRX
 1040 FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                &
               '     EXPERIMENTAL FEATURE !/REFRX NOT FULLY IMPLEMENTED.'/)
#endif
!
#ifdef W3_T
 9000 FORMAT (                                                     &
   '============================================================', &
   '===================='/                                         &
   ' TEST W3WAVE : RUN MODEL',I3,' FILEXT [',A,                    &
                    '] UP TO ',I8.8,I7.6 /                         &
   '====================',                                         &
 '============================================================')
 9010 FORMAT (' TEST W3WAVE : DT INT. =',F12.1,'   FLZERO = ',L1)
 9011 FORMAT (' TEST W3WAVE : DT LEV. =',F12.1)
 9012 FORMAT (' TEST W3WAVE : DT CUR. =',F12.1/                    &
              '                        ',F12.1/                    &
              '                        ',F12.1)
 9013 FORMAT (' TEST W3WAVE : DT WIND =',F12.1/                    &
              '                        ',F12.1/                    &
              '                        ',F12.1)
 9014 FORMAT (' TEST W3WAVE : DT ICE  =',F12.1)
 9015 FORMAT (' TEST W3WAVE : DT IC1  =',F12.1)
 9016 FORMAT (' TEST W3WAVE : DT IC5  =',F12.1)
 9017 FORMAT (' TEST W3WAVE : DT TAU  =',F12.1)
 9018 FORMAT (' TEST W3WAVE : DT RHO  =',F12.1)
 9020 FORMAT (' TEST W3WAVE : IT0, NT, DTG :',2I4,F8.1)
 9021 FORMAT (' TEST W3WAVE : ITIME etc',I6,I4,I10.8,I7.6,1X,2L1,  &
                                         2F6.2,F7.1,F6.2)
 9022 FORMAT (' TEST W3WAVE : SKIP TO 400 IN 3.5')
 9023 FORMAT (' TEST W3WAVE : SKIP TO 380 IN 3.5')
 9030 FORMAT (' TEST W3WAVE : END OF COMPUTATION LOOP')
 9040 FORMAT (' TEST W3WAVE : CHECKING FOR OUTPUT'/                &
              '               TOFRST           :',I9.8,I7.6/       &
              '               TND              :',I9.8,I7.6/       &
              '               DTTST[1], FLAG_O :',2F8.1,L4)
 9041 FORMAT (' TEST W3WAVE : PERFORMING OUTPUT')
 9042 FORMAT (' TEST W3WAVE : OUTPUT COMPUTATION FLAGS: ',3L2)
#endif
#ifdef W3_MPIT
 9043 FORMAT (' TEST W3WAVE : TYPE, NRQ, NRQMAX, NA : ',A2,3I6)
#endif
#ifdef W3_T
 9044 FORMAT (' TEST W3WAVE : END OF OUTPUT')
#endif
!/
!/ End of W3WAVE ----------------------------------------------------- /
!/
      END SUBROUTINE W3WAVE
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3GATH ( ISPEC, FIELD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         26-Dec-2012 |
!/                  +-----------------------------------+
!/
!/    04-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    13-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    29-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    13-Jun-2006 : Split STORE in G/SSTORE             ( version 3.09 )
!/    26-Dec-2012 : Move FIELD init. to W3GATH.         ( version 4.OF )
!/
!  1. Purpose :
!
!     Gather spectral bin information into a propagation field array.
!
!  2. Method :
!
!     Direct copy or communication calls (MPP version).
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       ISPEC   Int.   I   Spectral bin considered.
!       FIELD   R.A.   O   Full field to be propagated.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!
!      MPI_STARTALL, MPI_WAITALL
!                Subr. mpif.h   MPI persistent comm. routines (!/MPI).
!     ----------------------------------------------------------------
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
!     - The field is extracted but not converted.
!     - MPI version requires posing of send and receive calls in
!       W3WAVE to match local calls.
!     - MPI version does not require an MPI_TESTALL call for the
!       posted gather operation as MPI_WAITALL is mandatory to
!       reset persistent communication for next time step.
!     - MPI version allows only two new pre-fetch postings per
!       call to minimize chances to be slowed down by gathers that
!       are not yet needed, while maximizing the pre-loading
!       during the early (low-frequency) calls to the routine
!       where the amount of calculation needed for proagation is
!       the largest.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/SHRD  Switch for message passing method.
!     !/MPI   Id.
!
!     !/S     Enable subroutine tracing.
!     !/MPIT  MPI test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      USE W3GDATMD, ONLY: NSPEC, NX, NY, NSEA, NSEAL, MAPSF, DMIN
      USE W3PARALL, ONLY: INIT_GET_ISEA
      USE W3WDATMD, ONLY: A => VA
#ifdef W3_MPI
      USE W3ADATMD, ONLY: MPIBUF, BSTAT, IBFLOC, ISPLOC, BISPL, &
                          NSPLOC, NRQSG2, IRQSG2, GSTORE
      USE W3ODATMD, ONLY: NDST, IAPROC, NAPROC, NOTYPE
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
      INTEGER, INTENT(IN)     :: ISPEC
      REAL, INTENT(OUT)       :: FIELD(1-NY:NY*(NX+2))
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_SHRD
      INTEGER                 :: ISEA, IXY
#endif
#ifdef W3_MPI
      INTEGER                 :: STATUS(MPI_STATUS_SIZE,NSPEC),  &
                                 IOFF, IERR_MPI, JSEA, ISEA,     &
                                 IXY, IS0, IB0, NPST, J
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT
#endif
#ifdef W3_MPIT
      CHARACTER(LEN=15)       :: STR(MPIBUF), STRT
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3GATH')
#endif
!
       FIELD  = 0.
!
! 1.  Shared memory version ------------------------------------------ /
!
#ifdef W3_SHRD
      DO ISEA=1, NSEA
        IXY        = MAPSF(ISEA,3)
        FIELD(IXY) = A(ISPEC,ISEA)
        END DO
#endif
!
#ifdef W3_SHRD
      RETURN
#endif
!
! 2.  Distributed memory version ( MPI ) ----------------------------- /
! 2.a Update counters
!
#ifdef W3_MPI
      ISPLOC = ISPLOC + 1
      IBFLOC = IBFLOC + 1
      IF ( IBFLOC .GT. MPIBUF ) IBFLOC = 1
#endif
!
#ifdef W3_MPIT
      IF ( ISPLOC .EQ. 1 ) THEN
          STR = '--------------+'
          WRITE (NDST,9000) STR
        END IF
      STR    = '              |'
      STRT   = STR(IBFLOC)
      STRT(9:9) = 'A'
#endif
!
! 2.b Check status of present buffer
! 2.b.1 Scatter (send) still in progress, wait to end
!
#ifdef W3_MPI
      IF ( BSTAT(IBFLOC) .EQ. 2 ) THEN
          IOFF =  1 + (BISPL(IBFLOC)-1)*NRQSG2
          IF ( NRQSG2 .GT. 0 ) CALL                              &
               MPI_WAITALL ( NRQSG2, IRQSG2(IOFF,2),             &
                             STATUS, IERR_MPI )
          BSTAT(IBFLOC) = 0
#endif
#ifdef W3_MPIT
          STRT(13:13) = 'S'
#endif
#ifdef W3_MPI
        END IF
#endif
!
! 2.b.2 Gather (recv) not yet posted, post now
!
#ifdef W3_MPI
      IF ( BSTAT(IBFLOC) .EQ. 0 ) THEN
          BSTAT(IBFLOC) = 1
          BISPL(IBFLOC) = ISPLOC
          IOFF =  1 + (ISPLOC-1)*NRQSG2
          IF ( NRQSG2 .GT. 0 ) CALL                              &
               MPI_STARTALL ( NRQSG2, IRQSG2(IOFF,1), IERR_MPI )
#endif
#ifdef W3_MPIT
          STRT(10:10) = 'g'
#endif
#ifdef W3_MPI
        END IF
#endif
!
! 2.c Put local spectral densities in store
!
#ifdef W3_MPI
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        GSTORE(ISEA,IBFLOC) = A(ISPEC,JSEA)
        END DO
#endif
!
! 2.d Wait for remote spectral densities
!
#ifdef W3_MPI
      IOFF =  1 + (BISPL(IBFLOC)-1)*NRQSG2
      IF ( NRQSG2 .GT. 0 ) CALL                                  &
           MPI_WAITALL ( NRQSG2, IRQSG2(IOFF,1), STATUS, IERR_MPI )
#endif
!
#ifdef W3_MPIT
      STRT(11:11) = 'G'
      WRITE (STRT(1:7),'(I2,I5)') BSTAT(IBFLOC), ISPLOC
      STR(IBFLOC) = STRT
#endif
!
! 2.e Convert storage array to field.
!
#ifdef W3_MPI
      DO ISEA=1, NSEA
        IXY        = MAPSF(ISEA,3)
        FIELD(IXY) = GSTORE(ISEA,IBFLOC)
        END DO
#endif
!
! 2.f Pre-fetch data in available buffers
!
#ifdef W3_MPI
      IS0    = ISPLOC
      IB0    = IBFLOC
      NPST   = 0
#endif
!
#ifdef W3_MPI
      DO J=1, MPIBUF-1
        IS0    = IS0 + 1
        IF ( IS0 .GT. NSPLOC ) EXIT
        IB0    = 1 + MOD(IB0,MPIBUF)
        IF ( BSTAT(IB0) .EQ. 0 ) THEN
            BSTAT(IB0) = 1
            BISPL(IB0) = IS0
            IOFF       = 1 + (IS0-1)*NRQSG2
            IF ( NRQSG2 .GT. 0 ) CALL                            &
                 MPI_STARTALL ( NRQSG2, IRQSG2(IOFF,1), IERR_MPI )
            NPST       = NPST + 1
#endif
#ifdef W3_MPIT
            STRT        = STR(IB0)
            STRT(10:10) = 'g'
            WRITE (STRT(1:7),'(I2,I5)') BSTAT(IB0), BISPL(IB0)
            STR(IB0)    = STRT
#endif
#ifdef W3_MPI
          END IF
        IF ( NPST .GE. 2 ) EXIT
        END DO
#endif
!
! 2.g Test output
!
#ifdef W3_MPIT
      DO IB0=1, MPIBUF
        STRT   = STR(IB0)
        IF ( STRT(2:2) .EQ. ' ' ) THEN
            IF ( BSTAT(IB0) .EQ. 0 ) THEN
                WRITE (STRT(1:2),'(I2)') BSTAT(IB0)
              ELSE
                WRITE (STRT(1:7),'(I2,I5)') BSTAT(IB0), BISPL(IB0)
              END IF
            STR(IB0) = STRT
          END IF
        END DO
      WRITE (NDST,9010) ISPLOC, STR
#endif
!
#ifdef W3_MPI
      RETURN
#endif
!
! Formats
!
#ifdef W3_MPIT
 9000 FORMAT ( ' TEST OF BUFFER MANAGEMENT MPI :'/              &
               ' -------------------------------'/              &
      '      RECORDS ALTERNATELY WRITTEN BY W3GATH AND W3SCAT'/ &
      '      FRIST COLLUMN  : LOCAL ISPEC'/                     &
      '      OTHER COLLUMNS : BUFFER STATUS INDICATOR '/        &
      '                        0 : INACTIVE'/                   &
      '                        1 : RECEIVING'/                  &
      '                        2 : SENDING'/                    &
      '                       LOCAL ISPEC FOR BUFFER'/          &
      '                       A  : ACTIVE BUFFER'/              &
      '                       g/G: START/FINISH RECIEVE'/       &
      '                       s/S: START/FINISH SEND'/          &
      ' +-----+',8A15)
 9010 FORMAT ( ' |',I4,' |',8A15)
#endif
!/
!/ End of W3GATH ----------------------------------------------------- /
!/
      END SUBROUTINE W3GATH
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3SCAT ( ISPEC, MAPSTA, FIELD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         13-Jun-2006 |
!/                  +-----------------------------------+
!/
!/    04-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
!/    13-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Major changes to logistics.
!/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    07-Sep-2005 : Updated boundary conditions.        ( version 3.08 )
!/    13-Jun-2006 : Split STORE in G/SSTORE             ( version 3.09 )
!/
!  1. Purpose :
!
!     'Scatter' data back to spectral storage after propagation.
!
!  2. Method :
!
!     Direct copy or communication calls (MPP version).
!     See also W3GATH.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       ISPEC   Int.   I   Spectral bin considered.
!       MAPSTA  I.A.   I   Status map for spatial grid.
!       FIELD   R.A.   I   Full field to be propagated.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!
!      MPI_STARTALL, MPI_WAITALL, MPI_TESTALL
!                Subr. mpif.h   MPI persistent comm. routines (!/MPI).
!     ----------------------------------------------------------------
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
!     - The field is put back but not converted !
!     - MPI persistent communication calls initialize in W3MPII.
!     - See W3GATH and W3MPII for additional comments on data
!       buffering.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/SHRD  Switch for message passing method.
!     !/MPI   Id.
!
!     !/S     Enable subroutine tracing.
!     !/MPIT  MPI test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      USE W3GDATMD, ONLY: NSPEC, NX, NY, NSEA, NSEAL, MAPSF
      USE W3WDATMD, ONLY: A => VA
#ifdef W3_MPI
      USE W3ADATMD, ONLY: MPIBUF, BSTAT, IBFLOC, ISPLOC, BISPL, &
                          NSPLOC, NRQSG2, IRQSG2, SSTORE
#endif
      USE W3ODATMD, ONLY: NDST
#ifdef W3_MPI
      USE W3ODATMD, ONLY: IAPROC, NAPROC
#endif
      USE CONSTANTS, ONLY : LPDLIB
      USE W3PARALL, only: INIT_GET_ISEA
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
      INTEGER, INTENT(IN)     :: ISPEC, MAPSTA(NY*NX)
      REAL, INTENT(IN)        :: FIELD(1-NY:NY*(NX+2))
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_SHRD
      INTEGER                 :: ISEA, IXY
#endif
#ifdef W3_MPI
      INTEGER                 :: ISEA, IXY, IOFF, IERR_MPI, J,   &
                                 STATUS(MPI_STATUS_SIZE,NSPEC),  &
                                 JSEA, IB0
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT
#endif
#ifdef W3_MPIT
      CHARACTER(LEN=15)       :: STR(MPIBUF), STRT
#endif
#ifdef W3_MPI
      LOGICAL                 :: DONE
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3SCAT')
#endif
!
! 1.  Shared memory version ------------------------------------------ *
!
#ifdef W3_SHRD
      DO ISEA=1, NSEA
        IXY           = MAPSF(ISEA,3)
        IF ( MAPSTA(IXY) .GE. 1 ) A(ISPEC,ISEA) = FIELD(IXY)
        END DO
#endif
!
#ifdef W3_SHRD
      RETURN
#endif
!
! 2.  Distributed memory version ( MPI ) ----------------------------- *
! 2.a Initializations
!
#ifdef W3_MPIT
      DO IB0=1, MPIBUF
        STR(IB0) = '              |'
        END DO
#endif
!
#ifdef W3_MPIT
      STRT   = STR(IBFLOC)
      STRT(9:9) = 'A'
#endif
!
! 2.b Convert full grid to sea grid, active points only
!
#ifdef W3_MPI
      DO ISEA=1, NSEA
        IXY    = MAPSF(ISEA,3)
        IF ( MAPSTA(IXY) .GE. 1 ) SSTORE(ISEA,IBFLOC) = FIELD(IXY)
        END DO
#endif
!
! 2.c Send spectral densities to appropriate remote
!
#ifdef W3_MPI
      IOFF   = 1 + (ISPLOC-1)*NRQSG2
      IF ( NRQSG2 .GT. 0 ) CALL                                  &
           MPI_STARTALL ( NRQSG2, IRQSG2(IOFF,2), IERR_MPI )
      BSTAT(IBFLOC) = 2
#endif
#ifdef W3_MPIT
      STRT(12:12) = 's'
      WRITE (STRT(1:7),'(I2,I5)') BSTAT(IBFLOC), ISPLOC
      STR(IBFLOC) = STRT
#endif
!
! 2.d Save locally stored results
!
#ifdef W3_MPI
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        IXY    = MAPSF(ISEA,3)
        IF (MAPSTA(IXY) .GE. 1) A(ISPEC,JSEA) = SSTORE(ISEA,IBFLOC)
        END DO
#endif
!
! 2.e Check if any sends have finished
!
#ifdef W3_MPI
      IB0    = IBFLOC
#endif
!
#ifdef W3_MPI
      DO J=1, MPIBUF
        IB0    = 1 + MOD(IB0,MPIBUF)
        IF ( BSTAT(IB0) .EQ. 2 ) THEN
            IOFF   = 1 + (BISPL(IB0)-1)*NRQSG2
            IF ( NRQSG2 .GT. 0 ) THEN
               CALL MPI_TESTALL ( NRQSG2, IRQSG2(IOFF,2), DONE,  &
                                 STATUS, IERR_MPI )
              ELSE
                DONE   = .TRUE.
              END IF
            IF ( DONE .AND. NRQSG2.GT.0 ) CALL                   &
                     MPI_WAITALL ( NRQSG2, IRQSG2(IOFF,2),       &
                                   STATUS, IERR_MPI )
            IF ( DONE ) THEN
                BSTAT(IB0) = 0
#endif
#ifdef W3_MPIT
                STRT        = STR(IB0)
                WRITE (STRT(1:7),'(I2,I5)') BSTAT(IB0), BISPL(IB0)
                STRT(13:13) = 'S'
                STR(IB0)    = STRT
#endif
#ifdef W3_MPI
              END IF
          END IF
        END DO
#endif
!
! 2.f Last component, finish message passing, reset buffer control
!
#ifdef W3_MPI
      IF ( ISPLOC .EQ. NSPLOC ) THEN
#endif
!
#ifdef W3_MPI
          DO IB0=1, MPIBUF
            IF ( BSTAT(IB0) .EQ. 2 ) THEN
                IOFF   = 1 + (BISPL(IB0)-1)*NRQSG2
                IF ( NRQSG2 .GT. 0 ) CALL                        &
                     MPI_WAITALL ( NRQSG2, IRQSG2(IOFF,2),       &
                                   STATUS, IERR_MPI )
                BSTAT(IB0) = 0
#endif
#ifdef W3_MPIT
                STRT        = STR(IB0)
                WRITE (STRT(1:7),'(I2,I5)') BSTAT(IB0), BISPL(IB0)
                STRT(13:13) = 'S'
                STR(IB0)    = STRT
#endif
#ifdef W3_MPI
              END IF
            END DO
#endif
!
#ifdef W3_MPI
          ISPLOC = 0
          IBFLOC = 0
#endif
!
#ifdef W3_MPI
        END IF
#endif
!
! 2.g Test output
!
#ifdef W3_MPIT
      DO IB0=1, MPIBUF
        STRT   = STR(IB0)
        IF ( STRT(2:2) .EQ. ' ' ) THEN
            IF ( BSTAT(IB0) .EQ. 0 ) THEN
                WRITE (STRT(1:2),'(I2)') BSTAT(IB0)
              ELSE
                WRITE (STRT(1:7),'(I2,I5)') BSTAT(IB0), BISPL(IB0)
              END IF
            STR(IB0) = STRT
          END IF
        END DO
#endif
!
#ifdef W3_MPIT
      WRITE (NDST,9000) STR
#endif
!
#ifdef W3_MPIT
      IF ( ISPLOC .EQ. 0 ) THEN
          DO IB0=1, MPIBUF
            STR(IB0) = '--------------+'
            END DO
          WRITE (NDST,9010) STR
          WRITE (NDST,*)
        END IF
#endif
!
#ifdef W3_MPI
      RETURN
#endif
!
! Formats
!
#ifdef W3_MPIT
 9000 FORMAT ( ' |     |',8A15)
 9010 FORMAT ( ' +-----+',8A15)
#endif
!/
!/ End of W3SCAT ----------------------------------------------------- /
!/
      END SUBROUTINE W3SCAT
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3NMIN ( MAPSTA, FLAG0 )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         28-Dec-2004 |
!/                  +-----------------------------------+
!/
!/    23-Feb-2001 : Origination.                        ( version 2.07 )
!/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/
!  1. Purpose :
!
!     Check minimum number of active sea points at given processor to
!     evaluate the need for a MPI_BARRIER call.
!
!  2. Method :
!
!     Evaluate mapsta.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       MAPSTA  I.A.   I   Status map for spatial grid.
!       FLAG0   log.   O   Flag to identify 0 as minimum.
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
!     !/S     Enable subroutine tracing.
!     !/T     Test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSF
      USE W3ODATMD, ONLY: NDST, NAPROC
      USE W3PARALL, ONLY: INIT_GET_JSEA_ISPROC
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: MAPSTA(NY*NX)
      LOGICAL, INTENT(OUT)    :: FLAG0
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NMIN, IPROC, NLOC, ISEA, IXY
      INTEGER                 :: JSEA, ISPROC
#ifdef W3_S
      INTEGER, SAVE           :: IENT
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3NMIN')
#endif
!
      NMIN   = NSEA
!
      DO IPROC=1, NAPROC
        NLOC   = 0
        DO ISEA=1, NSEA
          CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
          IF (ISPROC .eq. IPROC) THEN
            IXY    = MAPSF(ISEA,3)
            IF ( MAPSTA(IXY) .EQ. 1 ) NLOC = NLOC + 1
          END IF
        END DO
#ifdef W3_SMC
 !!Li   For SMC grid, local sea points are equally NSEA/NAPROC
 !!Li   so the NLOC is overwirte by a constant.
        NLOC = NSEA/NAPROC
#endif
!
#ifdef W3_T
        WRITE (NDST,9000) IPROC, NLOC
#endif
        NMIN   = MIN ( NMIN , NLOC )
        END DO
!
      FLAG0  = NMIN .EQ. 0
#ifdef W3_T
      WRITE (NDST,9001) NMIN, FLAG0
#endif
!
      RETURN
!
! Formats
!
#ifdef W3_T
 9000 FORMAT ( ' TEST W3NMIN : IPROC =',I3,'  NLOC =',I5)
 9001 FORMAT ( ' TEST W3NMIN : NMIN =',I5,'  FLAG0 =',L2)
#endif
!/
!/ End of W3NMIN ----------------------------------------------------- /
!/
      END SUBROUTINE W3NMIN
!/
!/ End of module W3WAVEMD -------------------------------------------- /
!/
      END MODULE W3WAVEMD
