!> @file
!> @brief Contains module WMMDATMD.
!>
!> @author H. L. Tolman @date 22-Mar-2021
!>

#include "w3macros.h"
!
!/ ------------------------------------------------------------------- /
!>
!> @brief Define data structures to set up wave model dynamic data for
!>  several models simultaneously.
!>
!> @author H. L. Tolman @date 22-Mar-2021
!>
MODULE WMMDATMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         22-Mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    13-Jun-2005 : Origination.                       ( version 3.07 )
  !/    16-Dec-2005 : Add staging of boundary data.      ( version 3.08 )
  !/                  Add HGSTGE data.                   ( version 3.08 )
  !/    27-Jan-2006 : Adding static nests.               ( version 3.08 )
  !/    24-Mar-2006 : Add EQSTGE data.                   ( version 3.09 )
  !/    25-May-2006 : Add STIME in BPSTGE.               ( version 3.09 )
  !/    29-May-2006 : Adding overlapping grids.          ( version 3.09 )
  !/                  Fixing boundary data (buffering).
  !/    18-Jul-2006 : Adding input grids.                ( version 3.09 )
  !/    09-Aug-2006 : Adding unified point output.       ( version 3.10 )
  !/    06-Oct-2006 : Adding separate input grids.       ( version 3.10 )
  !/    12-Jan-2007 : Add FLSTI and FLLSTL.              ( version 3.10 )
  !/    22-Jan-2007 : Add NAVMAX.                        ( version 3.10 )
  !/    29-May-2009 : Preparing distribution version.    ( version 3.14 )
  !/    03-Sep-2012 : Add clock parameters (init.).      ( version 4.10 )
  !/    04-Feb-2014 : Switched to DATE_AND_TIME param.   ( version 4.18 )
  !/    10-Dec-2014 : Add checks for allocate status     ( version 5.04 )
  !/    28-Sep-2016 : Adjust MTAG? values so that MPI tags used fit
  !/                  within allowed bounds.             ( version 5.15 )
  !/    06-Jun-2018 : add subroutine INIT_GET_JSEA_ISPROC_GLOB/add PDLIB
  !/                                                     ( version 6.04 )
  !/    22-Mar-2021 : Support for air density input      ( version 7.13 )
  !/    28-Oct-2021 : Add SMC grid group indicator. JGLi ( version 7.13 )
  !/
  !/    Copyright 2009-2012 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Define data structures to set up wave model dynamic data for
  !     several models simultaneously.
  !
  !  2. Variables and types :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      NMDATA    Int.  Public   Number of models in array dim.
  !      IMDATA    Int.  Public   Selected model for output, init. at -1.
  !
  !      MDSI      Int.  Public   Unit number for input file.
  !      MDSO      Int.  Public   Unit number for output (log file).
  !      MDSS      Int.  Public   Unit number for output (screen).
  !      MDST      Int.  Public   Unit number for test output.
  !      MDSE      Int.  Public   Unit number for error output.
  !                               These outputs correspond to similar
  !                               unit numbers as defined per grid, but
  !                               are used for multi-grid routines
  !                               only.
  !      MDSP      Int.  Public   Unit number for profiling.
  !      MDSUP     Int.  Public   Unit number for unified point output.
  !      MDSUPA    Int.  Public   Unit number for unified point output.
  !                               ASCII
  !      MDSF      I.A.  Public   Unit numbers for input files.
  !
  !      NMPROC    Int.  Public   Number of processors (for total multi-
  !                               grid model).
  !      IMPROC    Int.  Public   Corresponding actual processor number.
  !      NMPLOG, NMPSCR, NMPTST, NMPERR, NMPUPT
  !                Int.  Public   Processors in NMPROC designated for
  !                               the above output units numbers.
  !
  !      STIME     I.A.  Public   Model run starting time.
  !      ETIME     I.A.  Public   Model run ending time.
  !      TSYNC     I.A.  Public   Synchronization time for grids.
  !      TMAX      I.A.  Public   Maximum next time per grid.
  !      TOUTP     I.A.  Public   Next output time for grids.
  !      TDATA     I.A.  Public   Time for which data is available.
  !
  !      NRGRD     Int.  Public   Number of grids.
  !      NRINP     Int.  Public   Number of input grids.
  !      NRGRP     Int.  Public   Number of groups.
  !      NGRPSMC   Int.  Public   SMC grid group number, one of 0:NRGRP.
  !      NMVMAX    Int.  Public   Number of moving grid data.
  !      GRANK     I.A.  Public   Rank number for grid.
  !      GRGRP     I.A.  Public   Group number for grid.
  !      INGRP     I.A.  Public   Grids in group, element 0 is number.
  !      GRDHGH, GRDEQL, GRDLOW
  !                I.A.  Public   Dependent grids with higher, same or
  !                               lower rank number, element 0 is number.
  !      ALLPRC    I.A.  Public   Map of processors in MPI_COMM_MWAVE for
  !                               all individual grids.
  !      MODMAP    I.A.  Public   Map which model is running where in
  !                               MPI_COMM_MWAVE each group.
  !      GRSTAT    I.A.  Public   Grid computation status indicator.
  !      DTRES     R.A.  Public   Residual of time step.
  !      NBI2G     I.A.  Public   Map cross-referencing how many spectra
  !                               echo grid provides to boundary cond. for
  !                               other grids.
  !      RESPEC    L.A.  Public   Map for need to convert spectra between
  !                               grids.
  !      BCDUMP    L.A.  Public   Flag for dumping internal bound. data.
  !      INPMAP    I.A.  Public   Map for expternal input grids.
  !      IDINP     C.A.  Public   Input field identifiers.
  !
  !      CLKDT1, CLKDT2, CLKDT3, CLKFIN
  !                Int.  Public   Global wall clock parameters,
  !
  !      MPI_COMM_MWAVE
  !                Int.  Public   MPI communicator.             ( !/MPI )
  !      MTAGn     Int.  Public   "Zero" tag number for MPI     ( !/MPI )
  !      MTAG_UB   Int.  Public   Upper-bound for MPI tags      ( !/MPI )
  !      NBISTA    I.A.  Public   Status for gathering input boundary
  !                               data.                         ( !/MPI )
  !      HGHSTA    I.A.  Public   Status for gathering high resolution
  !                               data.                         ( !/MPI )
  !      EQLSTA    I.A.  Public   Status for gathering data fro equally
  !                               ranked grids.                 ( !/MPI )
  !
  !      FLGBDI    Log.  Public   Flag for intitialization of boundry
  !                               distance maps.
  !      FLGHGn    Log.  Public   Flags for using mask for computations
  !                               and output for areas of grid overlap.
  !      IFLSTI    L.A.  Public   FLags for last ice per grid.
  !      IFLSTL    L.A.  Public   FLags for last level per grid.
  !      IFLSTR    L.A.  Public   FLags for last air density per grid.
  !
  !      MDATA     TYPE  Public   Data structure for grid dependent data.
  !      MDATAS    MDATA Public   Array of data structures.
  !
  !      BPST      TYPE  Public   Data structure for staging boundary
  !                               data.
  !      BPSTGE    BPST  Public   Array of data structures.
  !
  !      HGST      TYPE  Public   Data structure for staging 2-way
  !                               nesting data.
  !      HGSTGE    HGST  Public   Array of data structures.
  !
  !      EQST      TYPE  Public   Data structure for staging equal grid
  !                               reconcilliation data.
  !      EQSTGE    EQST  Public   Array of data structures.
  !     ----------------------------------------------------------------
  !
  !     All elements of MDATA are aliased to pointers with the same
  !     name. These pointers are defined as :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      NBI2S     I.A.  Public   Source information of boundary input
  !                               data (grid number and sea counter).
  !      MAPBDI    R.A.  Public   Map with distances to boundary.
  !      MAPODI    R.A.  Public   idem, open edges of grids.
  !      NRUPTS    Int.  Public   Number of unified output points.
  !      UPTMAP    I.A.  Public   Mapping of unified points to grids.
  !      MAPMSK    I.A.  Public   Mask corresponding to FLGHGn above.
  !      MINIT, MSKINI, FLDATn
  !                Log.  Public   Flags for array initializations.
  !      FLLSTI    Log.  Public   FLag for last ice per grid.
  !      FLLSTL    Log.  Public   FLag for last level per grid.
  !      FLLSTR    Log.  Public   FLag for last air density per grid.
  !
  !      NMV       Int.  Public   Number of moving grid data.
  !      TMV       I.A.  Public   Moving grid times.
  !      AMV       R.A.  Public   Moving grid velocities.
  !      DMV       R.A.  Public   Moving grid directions.
  !
  !      RCLD      I.A.  Public   Record length for data assimilation.
  !      NDT       I.A.  Public   Number of data for data assimilation.
  !      DATAn     R.A.  Public   Assimilation data.
  !
  !   MPI_COMM_GRD Int.  Public   Communicator for grid       ( !/MPI )
  !   MPI_COMM_BCT Int.  Public   Communicator for broadcast  ( !/MPI )
  !      CROOT     Int.  Public   "root" for MPI_COMM_GRD in
  !                               MPI_COMM_MWAVE              ( !/MPI )
  !      FBCAST    Log.  Public   FLag for need of broadcasting data
  !                               to processors that are not in the
  !                               communicator                ( !/MPI )
  !      NRQBPG    Int.  Public   Number of request handles    ( !/MPI )
  !      IRQBPG    I.A.  Public   Request handles.             ( !/MPI )
  !      NRQHGG    Int.  Public   Number of request handles    ( !/MPI )
  !      IRQHGG    I.A.  Public   Request handles.             ( !/MPI )
  !      NRQEQG    Int.  Public   Number of request handles    ( !/MPI )
  !      IRQEQG    I.A.  Public   Request handles.             ( !/MPI )
  !     ----------------------------------------------------------------
  !
  !     Elements of the structure BPTS are
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      NRQBPS    Int.  Public   Number of request handles    ( !/MPI )
  !      IRQBPS    I.A.  Public   Request handles.             ( !/MPI )
  !      VTIME     I.A.  Public   Valid time of data.
  !      STIME     I.A.  Public   Buffer for time for sending. ( !/MPI )
  !      SBPI      R.A.  Public   Spectral data storage.
  !      TSTORE    R.A.  Public   Spectral data buffer.        ( !/MPI )
  !      INIT      Log.  Public   Flag for array allocation.
  !     ----------------------------------------------------------------
  !
  !     Elements of the structure HGST are
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      NRQHGS    Int.  Public   Number of request handles    ( !/MPI )
  !      IRQHGS    I.A.  Public   Request handles.             ( !/MPI )
  !      NRQOUT    Int.  Public   Number of local spectra.     ( !/MPI )
  !      OUTDAT    I.A.  Public   Corresponding data.          ( !/MPI )
  !      NTOT, NREC, NRC1, NSND, NSN1, NSMX
  !                Int.  Public   Counters for total data, send and
  !                               received data with and without
  !                               masking.
  !      VTIME     I.A.  Public   Valid time of data.
  !      LJSEA     I.A.  Public   Local sea point counters.
  !      NRAVG     I.A.  Public   Number of points in averaging.
  !      IMPSRC    I.A.  Public   Source processor for data,
  !      ITAG      I.A.  Public   Communication tag.
  !      ISEND     I.A.  Public   Composite of all deta needed for send.
  !      WGHT      R.A.  Public   Weihts in averaging.
  !      SHGH      R.A.  Public   Staging area for spectra.
  !      TSTORE    R.A.  Public   Staging area for spectra to be send
  !                               out                         ( !/MPI )
  !      INIT      Log.  Public   Flag for array allocation.
  !     ----------------------------------------------------------------
  !
  !     Elements of the structure EQST are
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      NRQEQS    Int.  Public   Number of request handles    ( !/MPI )
  !      IRQEQS    I.A.  Public   Request handles.             ( !/MPI )
  !      NRQOUT    Int.  Public   Number of local spectra.     ( !/MPI )
  !      OUTDAT    I.A.  Public   Corresponding data.          ( !/MPI )
  !      NTOT, NREC, NSND, NAVMAX
  !                Int.  Public   Counters for total data, send and
  !                               received data.
  !      VTIME     I.A.  Public   Valid time of data.
  !      I/JSEA    I.A.  Public   Sea point counters.
  !      NAVG      I.A.  Public   Number of spectra in averaging.
  !      RIP       I.A.  Public   Processor (receiving).
  !      RTG       I.A.  Public   Tag number (receiving).
  !      SIS,SJS   I.A.  Public   Sea point counter (sending).
  !      SI1/2     I.A.  Public   Storage array counters (sending).
  !      SIP       I.A.  Public   Processor (sending).
  !      STG       I.A.  Public   Tag (sending).
  !      SEQL      R.A.  Public   Staging array.
  !      WGHT      R.A.  Public   Weight between grids.
  !      WAVG      R.A.  Public   Weight within grid.
  !      TSTORE    R.A.  Public   Staging area for spectra to be send
  !                               out                         ( !/MPI )
  !      INIT      Log.  Public   Flag for array allocation.
  !     ----------------------------------------------------------------
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      WMNDAT    Subr. Public   Set number of grids/models.
  !      WMDIMD    Subr. Public   Set dimensions of arrays (data).
  !      WMDIMM    Subr. Public   Set dimensions of arrays.
  !      WMSETM    Subr. Public   Point to selected grid / model.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      W3SETG    Subr. W3GDATMD Point to proper model grid.
  !      STRACE    Subr. W3SERVMD Subroutine tracing.
  !      EXTCDE    Subr.   Id.    Abort program with exit code.
  !     ----------------------------------------------------------------
  !
  !  5. Remarks :
  !
  !     - The number of grids is taken from W3GDATMD, and needs to be
  !       set first with W3DIMG.
  !
  !  6. Switches :
  !
  !     !/S    Enable subroutine tracing.
  !     !/T    Enable test output
  !
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  !/ Specify default accessibility
  !/
  PUBLIC
  !/
  !/ Module private variable for checking error returns
  !/
  INTEGER, PRIVATE        :: ISTAT         !< ISTAT check error returns
  !/
  !/ Conventional declarations
  !/
  INTEGER                 :: NMDATA = -1   !< NMDATA
  INTEGER                 :: IMDATA = -1   !< IMDATA
  INTEGER                 :: MDSI = 8   !< MDSI
  INTEGER                 :: MDSO = 9   !< MDSO
  INTEGER                 :: MDSS = 6   !< MDSS
  INTEGER                 :: MDST = 6   !< MDST
  INTEGER                 :: MDSE = 6   !< MDSE
  INTEGER                 :: MDSUP      !< MDSUP
#ifdef W3_ASCII
  INTEGER                 :: MDSUPA     !< MDSUPA
#endif
  INTEGER                 :: NMPROC = 1   !< NMPROC
  INTEGER                 :: IMPROC = 1   !< IMPROC
  INTEGER                 :: NMPLOG = 1   !< NMPLOG
  INTEGER                 :: NMPSCR = 1   !< NMPSCR
  INTEGER                 :: NMPTST = 1   !< NMPTST
  INTEGER                 :: NMPERR = 1   !< NMPERR
  INTEGER                 :: NMPUPT = 1   !< NMPUPT
  INTEGER                 :: STIME(2)   !< STIME
  INTEGER                 :: ETIME(2)   !< ETIME
  INTEGER                 :: NRGRD   !< NRGRD
  INTEGER                 :: NRINP   !< NRINP
  INTEGER                 :: NRGRP   !< NRGRP
  INTEGER                 :: NMVMAX   !< NMVMAX
  INTEGER                 :: NGRPSMC   !< NGRPSMC

  INTEGER                 :: CLKDT1(8)   !< CLKDT1
  INTEGER                 :: CLKDT2(8)   !< CLKDT2
  INTEGER                 :: CLKDT3(8)   !< CLKDT3

#ifdef W3_MPRF
  INTEGER                 :: MDSP   !< MDSP
#endif
#ifdef W3_MPI
  INTEGER                 :: MPI_COMM_MWAVE    !< MPI_COMM_MWAVE
  INTEGER, PARAMETER      :: MTAGB = 0   !< MTAGB
  INTEGER, PARAMETER      :: MTAG0 = 1000   !< MTAG0
  INTEGER, PARAMETER      :: MTAG1 = 40000    !< MTAG1
  INTEGER, PARAMETER      :: MTAG2 = 1000000   !< MTAG2
  INTEGER, PARAMETER      :: MTAG_UB = 2**21-1 !< MPI_TAG_UB on Cray XC40
#endif

  INTEGER, ALLOCATABLE    :: MDSF(:,:)   !< MDSF
  INTEGER, ALLOCATABLE    :: GRANK(:)   !< GRANK
  INTEGER, ALLOCATABLE    :: GRGRP(:)   !< GRGRP
  INTEGER, ALLOCATABLE    :: INGRP(:,:)   !< INGRP
  INTEGER, ALLOCATABLE    :: GRDHGH(:,:)   !< GRDHGH
  INTEGER, ALLOCATABLE    :: GRDEQL(:,:)   !< GRDEQL

  INTEGER, ALLOCATABLE    :: GRDLOW(:,:)   !< GRDLOW
  INTEGER, ALLOCATABLE    :: ALLPRC(:,:)   !< ALLPRC
  INTEGER, ALLOCATABLE    :: MODMAP(:,:)   !< MODMAP
  INTEGER, ALLOCATABLE    :: TSYNC(:,:)   !< TSYNC
  INTEGER, ALLOCATABLE    :: TMAX(:,:)   !< TMAX
  INTEGER, ALLOCATABLE    :: TOUTP(:,:)   !< TOUTP
  INTEGER, ALLOCATABLE    :: TDATA(:,:)   !< TDATA
  INTEGER, ALLOCATABLE    :: GRSTAT(:)   !< GRSTAT
  INTEGER, ALLOCATABLE    :: NBI2G(:,:)   !< NBI2G
  INTEGER, ALLOCATABLE    :: INPMAP(:,:)   !< INPMAP

#ifdef W3_MPI
  INTEGER, ALLOCATABLE    :: NBISTA(:)   !< NBISTA
  INTEGER, ALLOCATABLE    :: HGHSTA(:)   !< HGHSTA
  INTEGER, ALLOCATABLE    :: EQLSTA(:)   !< EQLSTA
#endif

  REAL                    :: CLKFIN   !< CLKFIN
  REAL,    ALLOCATABLE    :: DTRES(:)   !< DTRES
  LOGICAL                 :: FLGBDI=.FALSE.   !< FLGBDI
  LOGICAL                 :: FLGHG1   !< FLGHG1
  LOGICAL                 :: FLGHG2   !< FLGHG2
  LOGICAL, ALLOCATABLE    :: RESPEC(:,:)   !< RESPEC
  LOGICAL, ALLOCATABLE    :: BCDUMP(:)   !< BCDUMP
  LOGICAL, ALLOCATABLE    :: IFLSTI(:)   !< IFLSTI
  LOGICAL, ALLOCATABLE    :: IFLSTL(:)   !< IFLSTL
  LOGICAL, ALLOCATABLE    :: IFLSTR(:)   !< IFLSTR
  CHARACTER(LEN=3), ALLOCATABLE :: IDINP(:,:)   !< IDINP
  !/
  !/ Data structures
  !/
  TYPE MDATA
    INTEGER               :: RCLD(3)   !< RCLD
    INTEGER               :: NDT(3)   !< NDT
    INTEGER               :: NMV   !< NMV
    INTEGER               :: NRUPTS   !< NRUPTS

#ifdef W3_MPI
    INTEGER               :: MPI_COMM_GRD   !< MPI_COMM_GRD
    INTEGER               :: MPI_COMM_BCT   !< MPI_COMM_BCT
    INTEGER               :: CROOT   !< CROOT
    INTEGER               :: NRQBPG   !< NRQBPG
    INTEGER               :: NRQHGG   !< NRQHGG
    INTEGER               :: NRQEQG   !< NRQEQG
#endif
    INTEGER, POINTER      :: TMV(:,:,:)   !< TMV
    INTEGER, POINTER      :: NBI2S(:,:)   !< NBI2S
    INTEGER, POINTER      :: MAPMSK(:,:)   !< MAPMSK
    INTEGER, POINTER      :: UPTMAP(:)   !< UPTMAP

#ifdef W3_MPI
    INTEGER, POINTER      :: IRQBPG(:)   !< IRQBPG
    INTEGER, POINTER      :: IRQHGG(:)   !< IRQHGG
    INTEGER, POINTER      :: IRQEQG(:)   !< IRQEQG
#endif
    REAL, POINTER         :: DATA0(:,:)   !< DATA0
    REAL, POINTER         :: DATA1(:,:)   !< DATA1
    REAL, POINTER         :: DATA2(:,:)   !< DATA2
    REAL, POINTER         :: AMV(:,:)   !< AMV
    REAL, POINTER         :: DMV(:,:)   !< DMV

    REAL, POINTER         :: MAPBDI(:,:)   !< MAPBDI
    REAL, POINTER         :: MAPODI(:,:)   !< MAPODI
#ifdef W3_PDLIB
    INTEGER, POINTER      :: SEA_IPGL(:)   !< SEA_IPGL
    INTEGER, POINTER      :: SEA_IPGL_TO_PROC(:)   !< SEA_IPGL_TO_PROC
#endif
    LOGICAL               :: MINIT   !< MINIT
    LOGICAL               :: MSKINI   !< MSKINI
    LOGICAL               :: FLLSTL   !< FLLSTL
    LOGICAL               :: FLLSTR   !< FLLSTR
    LOGICAL               :: FLLSTI   !< FLLSTI
    LOGICAL               :: FLDAT0   !< FLDAT0
    LOGICAL               :: FLDAT1   !< FLDAT1
    LOGICAL               :: FLDAT2   !< FLDAT2

#ifdef W3_MPI
    LOGICAL                :: FBCAST   !< FBCAST
#endif
  END TYPE MDATA

  !

  TYPE BPST
#ifdef W3_MPI
    INTEGER                :: NRQBPS   !< NRQBPS
    INTEGER                :: STIME(2)   !< STIME
#endif
    INTEGER                 :: VTIME(2)   !< VTIME
#ifdef W3_MPI
    INTEGER, POINTER        :: IRQBPS(:)   !< IRQBPS
#endif
    REAL, POINTER           :: SBPI(:,:)   !< SBPI
#ifdef W3_MPI
    REAL, POINTER           :: TSTORE(:,:)   !< TSTORE
#endif
    LOGICAL                 :: INIT   !< INIT
  END TYPE BPST
  !
  TYPE HGST
    INTEGER                 :: VTIME(2)   !< VTIME
    INTEGER                 :: NTOT   !< NTOT
    INTEGER                 :: NREC   !< NREC
    INTEGER                 :: NRC1   !< NRC1
    INTEGER                 :: NSND   !< NSND
    INTEGER                 :: NSN1   !< NSN1
    INTEGER                 :: NSMX   !< NSMX
    INTEGER                 :: XTIME(2)   !< XTIME

#ifdef W3_MPI
    INTEGER                 :: NRQHGS   !< NRQHGS
    INTEGER                 :: NRQOUT   !< NRQOUT
#endif
    INTEGER, POINTER        :: LJSEA(:)   !< LJSEA
    INTEGER, POINTER        :: NRAVG(:)   !< NRAVG
    INTEGER, POINTER        :: IMPSRC(:,:)   !< IMPSRC
    INTEGER, POINTER        :: ITAG(:,:)   !< ITAG
    INTEGER, POINTER        :: ISEND(:,:)   !< ISEND
#ifdef W3_MPI
    INTEGER, POINTER        :: IRQHGS(:)   !< IRQHGS
    INTEGER, POINTER        :: OUTDAT(:,:)   !< OUTDAT
#endif
    REAL, POINTER           :: WGTH(:,:)   !< WGTH
    REAL, POINTER           :: SHGH(:,:,:)   !< SHGH
#ifdef W3_MPI
    REAL, POINTER            :: TSTORE(:,:)   !< TSTORE
#endif
    LOGICAL                  :: INIT   !< INIT
  END TYPE HGST

  !

  TYPE EQST
    INTEGER                 :: VTIME(2)   !< VTIME
    INTEGER                 :: NTOT   !< NTOT
    INTEGER                 :: NREC   !< NREC
    INTEGER                 :: NSND   !< NSND
    INTEGER                 :: NAVMAX   !< NAVMAX
#ifdef W3_MPI
    INTEGER                 :: NRQEQS   !< NRQEQS
    INTEGER                 :: NRQOUT   !< NRQOUT
#endif
    INTEGER, POINTER        :: ISEA(:)   !< ISEA
    INTEGER, POINTER        :: JSEA(:)   !< JSEA
    INTEGER, POINTER        :: NAVG(:)   !< NAVG
    INTEGER, POINTER        :: RIP(:,:)   !< RIP
    INTEGER, POINTER        :: RTG(:,:)   !< RTG
    INTEGER, POINTER        :: SIS(:)   !< SIS
    INTEGER, POINTER        :: SJS(:)   !< SJS
    INTEGER, POINTER        :: SI1(:)   !< SI1
    INTEGER, POINTER        :: SI2(:)   !< SI2
    INTEGER, POINTER        :: SIP(:)   !< SIP
    INTEGER, POINTER        :: STG(:)   !< STG

#ifdef W3_MPI
    INTEGER, POINTER        :: IRQEQS(:)   !< IRQEQS
    INTEGER, POINTER        :: OUTDAT(:,:)   !< OUTDAT
#endif
    REAL, POINTER           :: SEQL(:,:,:)   !< SEQL
    REAL, POINTER           :: WGHT(:)   !< WGHT
    REAL, POINTER           :: WAVG(:,:)   !< WAVG
#ifdef W3_MPI
    REAL, POINTER            :: TSTORE(:,:)   !< TSTORE
#endif
    LOGICAL                  :: INIT   !< INIT
  END TYPE EQST
  !/
  !/ Data storage
  !/
  TYPE(MDATA), TARGET, ALLOCATABLE  :: MDATAS(:)   !< MDATAS
  TYPE(BPST),  TARGET, ALLOCATABLE  :: BPSTGE(:,:)   !< BPSTGE
  TYPE(HGST),  TARGET, ALLOCATABLE  :: HGSTGE(:,:)   !< HGSTGE
  TYPE(EQST),  TARGET, ALLOCATABLE  :: EQSTGE(:,:)   !< EQSTGE
  !/
  !/ Data aliasses for structure MDATA(S)
  !/
  INTEGER, POINTER           :: RCLD(:)   !< RCLD
  INTEGER, POINTER           :: NDT(:)   !< NDT
  INTEGER, POINTER           :: NMV   !< NMV
  INTEGER, POINTER           :: TMV(:,:,:)   !< TMV
  INTEGER, POINTER           :: NBI2S(:,:)   !< NBI2S
  INTEGER, POINTER           :: MAPMSK(:,:)   !< MAPMSK
  INTEGER, POINTER           :: UPTMAP(:)   !< UPTMAP
#ifdef W3_MPI
  INTEGER, POINTER           :: MPI_COMM_GRD   !< MPI_COMM_GRD
  INTEGER, POINTER           :: MPI_COMM_BCT   !< MPI_COMM_BCT
  INTEGER, POINTER           :: CROOT   !< CROOT
#endif
  REAL, POINTER              :: DATA0(:,:)   !< DATA0
  REAL, POINTER              :: DATA1(:,:)   !< DATA1
  REAL, POINTER              :: DATA2(:,:)   !< DATA2
  REAL, POINTER              :: AMV(:,:)   !< AMV
  REAL, POINTER              :: DMV(:,:)   !< DMV

  REAL, POINTER              :: MAPBDI(:,:)   !< MAPBDI
  REAL, POINTER              :: MAPODI(:,:)   !< MAPODI
#ifdef W3_PDLIB
  INTEGER, POINTER           :: SEA_IPGL(:)   !< SEA_IPGL
  INTEGER, POINTER           :: SEA_IPGL_TO_PROC(:)   !< SEA_IPGL_TO_PROC
#endif
  LOGICAL, POINTER           :: MINIT   !< MINIT
  LOGICAL, POINTER           :: FLLSTL   !< FLLSTL
  LOGICAL, POINTER           :: FLLSTR   !< FLLSTR
  LOGICAL, POINTER           :: FLLSTI   !< FLLSTI
  LOGICAL, POINTER           :: FLDAT0   !< FLDAT0
  LOGICAL, POINTER           :: FLDAT1   !< FLDAT1
  LOGICAL, POINTER           :: FLDAT2   !< FLDAT2

#ifdef W3_MPI
  LOGICAL, POINTER           :: FBCAST   !< FBCAST
#endif
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Set up the number of grids to be used.
  !>
  !> @details Use data stored in NGRIDS in W3GDATMD.
  !>
  !> @param[in] NDSE Error output unit number.
  !> @param[in] NDST Test output unit number.
  !>
  !> @author H. L. Tolman  @date  22-Mar-2021
  !>
  SUBROUTINE WMNDAT ( NDSE, NDST )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 !
    !/                  +-----------------------------------+
    !/
    !/    22-Feb-2005 : Origination.                        ( version 3.07 )
    !/    16-Dec-2005 : Add staging of boundary data.       ( version 3.08 )
    !/                  Add HGSTGE data.                    ( version 3.08 )
    !/    24-Mar-2006 : Add EQSTGE data.                    ( version 3.09 )
    !/    25-May-2006 : Add STIME in BPSTGE.                ( version 3.09 )
    !/    12-Jan-2007 : Add FLSTI and FLLSTL.               ( version 3.10 )
    !/    22-Jan-2007 : Add NAVMAX.                         ( version 3.10 )
    !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
    !/    22-Mar-2021 : Support for air density input       ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Set up the number of grids to be used.
    !
    !  2. Method :
    !
    !     Use data stored in NGRIDS in W3GDATMD.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NDSE    Int.   I   Error output unit number.
    !       NDST    Int.   I   Test output unit number.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     See module documentation.
    !
    !  5. Called by :
    !
    !     Any program that uses this grid structure.
    !
    !  6. Error messages :
    !
    !     - Error checks on previous setting of variable NGRIDS.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/S    Enable subroutine tracing.
    !     !/T    Enable test output
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD, ONLY: NGRIDS
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
    INTEGER, INTENT(IN)     :: NDSE, NDST
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: I, J
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'WMNDAT')
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Test input and module status
    !
    IF ( NGRIDS .EQ. -1 ) THEN
      WRITE (NDSE,1001) NGRIDS
      CALL EXTCDE (1)
    END IF
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Set variable and allocate arrays
    !
    ALLOCATE ( MDATAS(NGRIDS), BPSTGE(NGRIDS,NGRIDS),               &
         HGSTGE(NGRIDS,NGRIDS), EQSTGE(NGRIDS,NGRIDS),        &
         BCDUMP(NRGRD), IFLSTI(NRINP), IFLSTL(NRINP),         &
         IFLSTR(NRINP), STAT=ISTAT )
    CHECK_ALLOC_STATUS ( ISTAT )
#ifdef W3_MPI
    ALLOCATE ( NBISTA(NGRIDS), HGHSTA(NGRIDS), EQLSTA(NGRIDS), &
         STAT=ISTAT )
    CHECK_ALLOC_STATUS ( ISTAT )
#endif
    NMDATA = NGRIDS
    !
    ! -------------------------------------------------------------------- /
    ! 3.  Initialize parameters
    !
#ifdef W3_MPI
    NBISTA = 0
    HGHSTA = 0
    EQLSTA = 0
#endif
    !
    IFLSTI = .FALSE.
    IFLSTL = .FALSE.
    IFLSTR = .FALSE.
    !
    DO I=1, NGRIDS
      MDATAS(I)%MINIT  = .FALSE.
      MDATAS(I)%MSKINI = .FALSE.
      MDATAS(I)%FLDAT0 = .FALSE.
      MDATAS(I)%FLDAT1 = .FALSE.
      MDATAS(I)%FLDAT2 = .FALSE.
#ifdef W3_MPI
      MDATAS(I)%MPI_COMM_GRD = -99
      MDATAS(I)%MPI_COMM_BCT = -99
#endif
      DO J=1, NGRIDS
        BPSTGE(I,J)%VTIME(1) = -1
        BPSTGE(I,J)%VTIME(2) =  0
#ifdef W3_MPI
        BPSTGE(I,J)%STIME(1) = -1
        BPSTGE(I,J)%STIME(2) =  0
#endif
        BPSTGE(I,J)%INIT     = .FALSE.
#ifdef W3_MPI
        BPSTGE(I,J)%NRQBPS   = 0
#endif
        HGSTGE(I,J)%VTIME(1) = -1
        HGSTGE(I,J)%VTIME(2) =  0
        HGSTGE(I,J)%XTIME(1) = -1
        HGSTGE(I,J)%XTIME(2) =  0
        HGSTGE(I,J)%NTOT     =  0
        HGSTGE(I,J)%NREC     =  0
        HGSTGE(I,J)%NRC1     =  0
        HGSTGE(I,J)%NSND     =  0
        HGSTGE(I,J)%NSN1     =  0
        HGSTGE(I,J)%NSMX     =  0
#ifdef W3_MPI
        HGSTGE(I,J)%NRQHGS   =  0
        HGSTGE(I,J)%NRQOUT   =  0
#endif
        HGSTGE(I,J)%INIT     = .FALSE.
        EQSTGE(I,J)%VTIME(1) = -1
        EQSTGE(I,J)%VTIME(2) =  0
        EQSTGE(I,J)%NTOT     =  0
        EQSTGE(I,J)%NREC     =  0
        EQSTGE(I,J)%NSND     =  0
        EQSTGE(I,J)%NAVMAX   =  1
#ifdef W3_MPI
        EQSTGE(I,J)%NRQEQS   =  0
        EQSTGE(I,J)%NRQOUT   =  0
#endif
        EQSTGE(I,J)%INIT     = .FALSE.
      END DO
    END DO
    !
#ifdef W3_T
    WRITE (NDST,9000) NGRIDS
#endif
    !
    RETURN
    !
    ! Formats
    !
1001 FORMAT (/' *** ERROR WMNDAT : NGRIDS NOT YET SET *** '/         &
         '                    NGRIDS = ',I10/                   &
         '                    RUN W3NMOD FIRST'/)
    !
#ifdef W3_T
9000 FORMAT (' TEST WMNDAT : SETTING UP FOR ',I4,' GRIDS')
#endif
    !/
    !/ End of WMNDAT ----------------------------------------------------- /
    !/
  END SUBROUTINE WMNDAT
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Initialize an individual data grid at the proper dimensions.
  !>
  !> @details Allocate directly into the structure array. Note that
  !>  this cannot be done through the pointer alias!
  !>
  !> @param[in] IMOD Model number to point to.
  !> @param[in] NDSE Error output unit number.
  !> @param[in] NDST Test output unit number.
  !> @param[in] J    Data set [1,2,3].
  !>
  !> @author H. L. Tolman  @date 10-Dec-2014
  !>
  SUBROUTINE WMDIMD ( IMOD, NDSE, NDST, J )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         10-Dec-2014 !
    !/                  +-----------------------------------+
    !/
    !/    22-Feb-2005 : Origination.                        ( version 3.07 )
    !/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
    !/
    !  1. Purpose :
    !
    !     Initialize an individual data grid at the proper dimensions.
    !
    !  2. Method :
    !
    !     Allocate directly into the structure array. Note that
    !     this cannot be done through the pointer alias!
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IMOD    Int.   I   Model number to point to.
    !       NDSE    Int.   I   Error output unit number.
    !       NDST    Int.   I   Test output unit number.
    !       J       Int.   I   Data set [1,2,3].
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
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     - Check on input parameters.
    !     - Check on previous allocation.
    !
    !  7. Remarks :
    !
    !     - WMSETM needs to be called after allocation to point to
    !       proper allocated arrays.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/S    Enable subroutine tracing.
    !     !/T    Enable test output
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD, ONLY: NGRIDS, IGRID, W3SETG
    USE W3ODATMD, ONLY: NAPROC
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)           :: IMOD, NDSE, NDST, J
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: JGRID
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'WMDIMD')
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Test input and module status
    !
    IF ( NGRIDS .EQ. -1 ) THEN
      WRITE (NDSE,1001)
      CALL EXTCDE (1)
    END IF
    !
    IF ( IMOD.LT.1 .OR. IMOD.GT.NMDATA ) THEN
      WRITE (NDSE,1002) IMOD, NMDATA
      CALL EXTCDE (2)
    END IF
    !
    IF ( MDATAS(IMOD)%MINIT ) THEN
      WRITE (NDSE,1003)
      CALL EXTCDE (3)
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9000) IMOD
#endif
    !
    JGRID  = IGRID
    IF ( JGRID .NE. IMOD ) CALL W3SETG ( IMOD, NDSE, NDST )
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Allocate arrays
    !
    IF ( J .EQ. 0 ) THEN
      ALLOCATE ( MDATAS(IMOD)%TMV(2,-7:4,NMV) ,            &
           MDATAS(IMOD)%AMV(NMV,-7:4)   ,            &
           MDATAS(IMOD)%DMV(NMV,-7:4)   , STAT=ISTAT )
      CHECK_ALLOC_STATUS ( ISTAT )
    END IF
    !
    IF ( J .EQ. 1 ) THEN
      IF ( FLDAT0 ) DEALLOCATE ( MDATAS(IMOD)%DATA0 )
      ALLOCATE ( MDATAS(IMOD)%DATA0(RCLD(J),NDT(J)), STAT=ISTAT )
      CHECK_ALLOC_STATUS ( ISTAT )
      FLDAT0 = .TRUE.
    END IF
    !
    IF ( J .EQ. 2 ) THEN
      IF ( FLDAT1 ) DEALLOCATE ( MDATAS(IMOD)%DATA1 )
      ALLOCATE ( MDATAS(IMOD)%DATA1(RCLD(J),NDT(J)), STAT=ISTAT )
      CHECK_ALLOC_STATUS ( ISTAT )
      FLDAT1 = .TRUE.
    END IF
    !
    IF ( J .EQ. 3 ) THEN
      IF ( FLDAT2 ) DEALLOCATE ( MDATAS(IMOD)%DATA2 )
      ALLOCATE ( MDATAS(IMOD)%DATA2(RCLD(J),NDT(J)), STAT=ISTAT )
      CHECK_ALLOC_STATUS ( ISTAT )
      FLDAT2 = .TRUE.
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9001)
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 3.  Point to allocated arrays
    !
    CALL WMSETM ( IMOD, NDSE, NDST )
    !
    IF ( J .EQ. 0 ) THEN
      TMV    = 0
      AMV    = 0.
      DMV    = 0.
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9002)
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 5.  Restore previous grid setting if necessary
    !
    IF ( JGRID .NE. IMOD ) CALL W3SETG ( JGRID, NDSE, NDST )
    !
    RETURN
    !
    ! Formats
    !
1001 FORMAT (/' *** ERROR WMDIMD : GRIDS NOT INITIALIZED *** '/      &
         '                    RUN W3NMOD FIRST '/)
1002 FORMAT (/' *** ERROR WMDIMD : ILLEGAL MODEL NUMBER *** '/       &
         '                    IMOD   = ',I10/                   &
         '                    NMDATA = ',I10/)
1003 FORMAT (/' *** ERROR WMDIMD : ARRAY(S) ALREADY ALLOCATED *** ')
    !
#ifdef W3_T
9000 FORMAT (' TEST WMDIMD : MODEL ',I4,' DIM. AT ',2I5,I7)
9001 FORMAT (' TEST WMDIMD : ARRAYS ALLOCATED')
9002 FORMAT (' TEST WMDIMD : POINTERS RESET')
#endif
    !/
    !/ End of WMDIMD ----------------------------------------------------- /
    !/
  END SUBROUTINE WMDIMD
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Initialize an individual data grid at the proper dimensions.
  !>
  !> @details Allocate directly into the structure array. Note that
  !>  this cannot be done through the pointer alias!
  !>
  !> @param[in] IMOD Model number to point to.
  !> @param[in] NDSE Error output unit number.
  !> @param[in] NDST Test output unit number.
  !>
  !> @author H. L. Tolman  @date 22-Feb-2005
  !>
  SUBROUTINE WMDIMM  ( IMOD, NDSE, NDST )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Feb-2005 !
    !/                  +-----------------------------------+
    !/
    !/    22-Feb-2005 : Origination.                        ( version 3.07 )
    !/
    !  1. Purpose :
    !
    !     Initialize an individual data grid at the proper dimensions.
    !
    !  2. Method :
    !
    !     Allocate directly into the structure array. Note that
    !     this cannot be done through the pointer alias!
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IMOD    Int.   I   Model number to point to.
    !       NDSE    Int.   I   Error output unit number.
    !       NDST    Int.   I   Test output unit number.
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
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     - Check on input parameters.
    !     - Check on previous allocation.
    !
    !  7. Remarks :
    !
    !     - WMSETM needs to be called after allocation to point to
    !       proper allocated arrays.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/S    Enable subroutine tracing.
    !     !/T    Enable test output
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD, ONLY: NGRIDS, IGRID, W3SETG
    USE W3ODATMD, ONLY: NAPROC
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)           :: IMOD, NDSE, NDST
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: JGRID
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'WMDIMM')
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Test input and module status
    !
    IF ( NGRIDS .EQ. -1 ) THEN
      WRITE (NDSE,1001)
      CALL EXTCDE (1)
    END IF
    !
    IF ( IMOD.LT.1 .OR. IMOD.GT.NMDATA ) THEN
      WRITE (NDSE,1002) IMOD, NMDATA
      CALL EXTCDE (2)
    END IF
    !
    IF ( MDATAS(IMOD)%MINIT ) THEN
      WRITE (NDSE,1003)
      CALL EXTCDE (3)
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9000) IMOD
#endif
    !
    JGRID  = IGRID
    IF ( JGRID .NE. IMOD ) CALL W3SETG ( IMOD, NDSE, NDST )
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Allocate arrays
    !
    !     ALLOCATE ( MDATAS(IMOD)%...
    !
#ifdef W3_T
    WRITE (NDST,9001)
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 3.  Point to allocated arrays
    !
    CALL WMSETM ( IMOD, NDSE, NDST )
    !
#ifdef W3_T
    WRITE (NDST,9002)
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 4.  Update flag
    !
    MINIT  = .TRUE.
    !
#ifdef W3_T
    WRITE (NDST,9003)
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 5.  Restore previous grid setting if necessary
    !
    IF ( JGRID .NE. IMOD ) CALL W3SETG ( JGRID, NDSE, NDST )
    !
    RETURN
    !
    ! Formats
    !
1001 FORMAT (/' *** ERROR WMDIMM : GRIDS NOT INITIALIZED *** '/      &
         '                    RUN W3NMOD FIRST '/)
1002 FORMAT (/' *** ERROR WMDIMM : ILLEGAL MODEL NUMBER *** '/       &
         '                    IMOD   = ',I10/                   &
         '                    NMDATA = ',I10/)
1003 FORMAT (/' *** ERROR WMDIMM : ARRAY(S) ALREADY ALLOCATED *** ')
    !
#ifdef W3_T
9000 FORMAT (' TEST WMDIMM : MODEL ',I4,' DIM. AT ',2I5,I7)
9001 FORMAT (' TEST WMDIMM : ARRAYS ALLOCATED')
9002 FORMAT (' TEST WMDIMM : POINTERS RESET')
9003 FORMAT (' TEST WMDIMM : FLAGS SET')
#endif
    !/
    !/ End of WMDIMM ----------------------------------------------------- /
    !/
  END SUBROUTINE WMDIMM
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Select one of the WAVEWATCH III grids / models.
  !>
  !> @details Point pointers to the proper variables in the proper element of
  !>  the GRIDS array.
  !>
  !> @param[in] IMOD Model number to point to.
  !> @param[in] NDSE Error output unit number.
  !> @param[in] NDST Test output unit number.
  !>
  !> @author H. L. Tolman  @date 22-Mar-2021
  !>
  SUBROUTINE WMSETM ( IMOD, NDSE, NDST )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 !
    !/                  +-----------------------------------+
    !/
    !/    13-Jun-2005 : Origination.                       ( version 3.07 )
    !/    22-Mar-2021 : Support for air density input      ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Select one of the WAVEWATCH III grids / models.
    !
    !  2. Method :
    !
    !     Point pointers to the proper variables in the proper element of
    !     the GRIDS array.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IMOD    Int.   I   Model number to point to.
    !       NDSE    Int.   I   Error output unit number.
    !       NDST    Int.   I   Test output unit number.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     See module documentation.
    !
    !  5. Called by :
    !
    !     Many subroutines in the WAVEWATCH system.
    !
    !  6. Error messages :
    !
    !     Checks on parameter list IMOD.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/S    Enable subroutine tracing.
    !     !/T    Enable test output
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
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
    INTEGER, INTENT(IN)     :: IMOD, NDSE, NDST
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'WMSETM')
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Test input and module status
    !
    IF ( NMDATA .EQ. -1 ) THEN
      WRITE (NDSE,1001)
      CALL EXTCDE (1)
    END IF
    !
    IF ( IMOD.LT.1 .OR. IMOD.GT.NMDATA ) THEN
      WRITE (NDSE,1002) IMOD, NMDATA
      CALL EXTCDE (2)
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9000) IMOD
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Set model numbers
    !
    IMDATA = IMOD
    !
    ! -------------------------------------------------------------------- /
    ! 3.  Set pointers
    !
    !
    NMV    => MDATAS(IMOD)%NMV
    TMV    => MDATAS(IMOD)%TMV
    AMV    => MDATAS(IMOD)%AMV
    DMV    => MDATAS(IMOD)%DMV
#ifdef W3_MPI
    MPI_COMM_GRD => MDATAS(IMOD)%MPI_COMM_GRD
    MPI_COMM_BCT => MDATAS(IMOD)%MPI_COMM_BCT
    CROOT  => MDATAS(IMOD)%CROOT
    FBCAST => MDATAS(IMOD)%FBCAST
#endif
    RCLD   => MDATAS(IMOD)%RCLD
    NDT    => MDATAS(IMOD)%NDT
    DATA0  => MDATAS(IMOD)%DATA0
    DATA1  => MDATAS(IMOD)%DATA1
    DATA2  => MDATAS(IMOD)%DATA2
    NBI2S  => MDATAS(IMOD)%NBI2S
    MAPMSK => MDATAS(IMOD)%MAPMSK
    MINIT  => MDATAS(IMOD)%MINIT
    FLLSTL => MDATAS(IMOD)%FLLSTL
    FLLSTI => MDATAS(IMOD)%FLLSTI
    FLLSTR => MDATAS(IMOD)%FLLSTR
    MAPBDI => MDATAS(IMOD)%MAPBDI
    MAPODI => MDATAS(IMOD)%MAPODI
#ifdef W3_PDLIB
    SEA_IPGL => MDATAS(IMOD)%SEA_IPGL
    SEA_IPGL_TO_PROC => MDATAS(IMOD)%SEA_IPGL_TO_PROC
#endif
    UPTMAP => MDATAS(IMOD)%UPTMAP
    !
    RETURN
    !
    ! Formats
    !
1001 FORMAT (/' *** ERROR WMSETM : GRIDS NOT INITIALIZED *** '/      &
         '                    RUN W3NMOD FIRST '/)
1002 FORMAT (/' *** ERROR WMSETM : ILLEGAL MODEL NUMBER *** '/       &
         '                    IMOD   = ',I10/                   &
         '                    NMDATA = ',I10/)
    !
#ifdef W3_T
9000 FORMAT (' TEST WMSETM : MODEL ',I4,' SELECTED')
#endif
    !/
    !/ End of WMSETM ----------------------------------------------------- /
    !/
  END SUBROUTINE WMSETM
  !**********************************************************************
  !*                                                                    *
  !**********************************************************************
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Introduce mapping for ISPROC and JSEA when using PDLIB
  !>  in the Multigrid approach.
  !>
  !> @param[in]   ISEA
  !> @param[in]   J
  !> @param[out]  JSEA
  !> @param[out]  ISPROC
  !>
  !> @author Aron Roland  @date 14-Jun-2018
  !>
  SUBROUTINE INIT_GET_JSEA_ISPROC_GLOB(ISEA, J, JSEA, ISPROC)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           Aron Roland             |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         14-Jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/    06-Jun-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Introduce mapping for ISPROC and JSEA when using PDLIB
    !               in the Multigrid approach
    !
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
    USE CONSTANTS, ONLY: LPDLIB
    USE W3ODATMD, ONLY: OUTPTS
    USE W3GDATMD, ONLY: GTYPE, GRIDS, UNGTYPE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    integer, intent(in) :: ISEA, J
    integer, intent(out) :: JSEA, ISPROC
    integer nb
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
#ifdef W3_S
    CALL STRACE (IENT, 'INIT_GET_JSEA_ISPROC_GLOB')
#endif
    IF (.NOT. LPDLIB) THEN
      nb=OUTPTS(J)%NAPROC
      JSEA   = 1 + (ISEA-1)/nb
      ISPROC=1
#ifdef W3_DIST
      ISPROC = MDATAS(J)%CROOT - 1 + ISEA - (JSEA-1)*nb
#endif
    ELSE
#ifdef W3_PDLIB
      IF (GRIDS(J)%GTYPE .ne. UNGTYPE) THEN
        nb=OUTPTS(J)%NAPROC
        JSEA   = 1 + (ISEA-1)/nb
        ISPROC = MDATAS(J)%CROOT - 1 + ISEA - (JSEA-1)*nb
      ELSE
        JSEA   = MDATAS(J)%SEA_IPGL(ISEA)
        ISPROC = MDATAS(J)%SEA_IPGL_TO_PROC(ISEA)
      ENDIF
#endif
    ENDIF
    !/
    !/ End of INIT_GET_JSEA_ISPROC_GLOB  ---------------------------------- /
    !/
  END SUBROUTINE INIT_GET_JSEA_ISPROC_GLOB
  !/
  !/ End of module WMMDATMD -------------------------------------------- /
  !/
END MODULE WMMDATMD
