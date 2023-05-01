#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE  W3IORSMDOLD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                      FORTRAN 2003 |
!/                  | Last update :         03-Aug-2020 |
!/                  +-----------------------------------+
!/
!/    See subroutine for update log.
!/
!  1. Purpose :
!
!     Read/write restart files.
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      VERINI    C*10  Private  Restart file version number.
!      IDSTR     C*26  Private  Restart file UD string.
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3IORS    Subr. Public   Read/write restart files.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETO, W3SETG, W3SETW, W3DIMW
!                Subr. W3xDATMD Manage data structures.
!      STRACE    Subr. W3SERVMD Subroutine tracing.            (!/S)
!      EXTCDE    Subr. W3SERVMD Abort program with exit code.
!      MPI_STARTALL, MPI_WAITALL                              (!/MPI)
!                Subr.          MPI persistent communication routines
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!  6. Switches :
!
!     See also routine.
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      PUBLIC
!/
!/ Private parameter statements (ID strings)
!/
      CHARACTER(LEN=10), PARAMETER, PRIVATE :: VERINI = 'III  5.10 '
      CHARACTER(LEN=26), PARAMETER, PRIVATE ::                        &
                               IDSTR = 'WAVEWATCH III RESTART FILE'
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
!JDM      SUBROUTINE W3IORSOLD ( INXOUT, NDSR, DUMFPI, IMOD, FLRSTRT )
      SUBROUTINE W3IORSOLD ( INXOUT, NDSR, DUMFPI, RSTYPE, IMOD )

!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         05-Jun-2018 |
!/                  +-----------------------------------+
!/
!/    12-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
!/    27-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    30-Apr-2002 : Add ice for transparencies.         ( version 2.20 )
!/    13-Nov-2002 : Add stress as vector.               ( version 3.00 )
!/    19-Aug-2003 : Output server options added.        ( version 3.04 )
!/    09-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    24-Jun-2005 : Adding MAPST2.                      ( version 3.07 )
!/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    05-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
!/    08-May-2007 : Starting from calm as an option.    ( version 3.11 )
!/    17-May-2007 : Adding NTPROC/NAPROC separation.    ( version 3.11 )
!/    22-Jun-2007 : Dedicated output processes.         ( version 3.11 )
!/    15-Apr-2008 : Clean up for distribution.          ( version 3.14 )
!/    21-Apr-2008 : Remove PGI bug internal files.      ( version 3.14 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Output file name with 3 digit id.   ( version 3.14 )
!/                  (W. E. Rogers, NRL)
!/    14-Nov-2013 : Remove cold start init. UST(DIR).   ( version 4.13 )
!/    31-May-2016 : Optimize restart file size for un-  ( version 5.10 )
!/                  structured grid and restart read.
!/                  (M. Ward, NCI, S. Zieger, BOM)
!/    10-Mar-2017 : File access mode changed to 'STREAM'( version 6.02 )
!/                  (S. Zieger, BOM)
!/    09-Aug-2017 : Bug fix for MPI restart read issue  ( version 6.02 )
!/                  (T. Campbell, NRL)
!/    05-Jun-2018 : Add PDLIB/TIMINGS/DEBUGIO           ( version 6.04 )
!/                  DEBUGINIT/MPI
!/    19-Dec-2019 : Optional second stream of           ( version 7.00 )
!/                  restart files
!/                  (Roberto Padilla-Hernandez & J.H. Alves)
!/    03-Aug-2020 : Updates so that it can be used to generate HWRF IC 
!/         This routine was created by taking the production/GFSv16
!/    18-Sep-2020 : Updates for compiling, needed to hardcode variables 
!/         and change the interface to match the older style
!/         Also make sure that NSEAL is not used, as it is 0 use NSEA
!branch and processing it with the following switches: 
!F90 NCO NOGRB SHRD SCRIP SCRIPNC WRST NC4 PR3 UQ FLX0 SEED ST4 STAB0
!NL1 BT1 DB1 MLIM TR0 BS0 XX0 RWND WNX1 WNT1 CRX1 CRT1 O0 O1 O2 O3 O4 O5
!O6 O7 O14 O15 IC0 IS0 REF0 
!/
!/    Copyright 2009-2013 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Reads/writes restart files.
!
!  2. Method :
!
!     The file is opened within the routine, the name is pre-defined
!     and the unit number is given in the parameter list. The restart
!     file is written using UNFORMATTED write statements. The routine
!     generates new names when called more than once. File names are :
!
!                                 restart000.FILEXT
!                                 restart001.FILEXT
!                                 restart002.FILEXT etc.
!
!     Optionally, a second stream of restart files is generated given
!     a secondary stride definad by an additional start/end time line
!     triggered by an optional argument added to the end of the stan-
!     dard restart request line (a sixth argument flag set to T). File
!     names include a time-tag prefix:
!
!                                 YYYYMMDD.HHMMSS.restart.FILEXT
!
!     The file to be read thus always is unnumbered, whereas all
!     written files are automatically numbered.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       INXOUT  C*(*)  I   Test string for read/write, valid are:
!                          'READ' Reading of a restart file.
!                          'HOT'  Writing a full restart from the model.
!                          'COLD' Writing a cold start file.
!                          'WIND' Initialize fields using first wind
!                                 field.
!                          'CALM' Starting from calm conditions.
!       NDSR    Int.  I/O  File unit number.
!       DUMFPI  Real   I   Dummy values for FPIS for cold start.
!       RSTYPE  Int.   O   Type of input field,
!                           0 : cold start,
!                           1 : cold start with fetch-limited spectra,
!                           2 : full restart,
!                           3 : for writing file.
!                           4 : starting from calm.
!       IMOD    Int.   I   Optional grid number, defaults to 1.
!       FLRSTRT LOGIC  I    OTIONAL TRUE: A second request for restart files
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
!      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
!      WW3_STRT  Prog.   N/A    Initial conditions program.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       Tests on INXOUT, file status and on array dimensions.
!
!  7. Remarks :
!
!     - MAPSTA is dumped as it contains information on inactive points.
!       Note that the original MAPSTA is dumped in the model def. file
!       for use in the initial conditions (and output) programs.
!     - Note that MAPSTA and MAPST2 data is combinded in the file.
!     - The depth is recalculated in a write to avoid floating point
!       errors in W3STRT.
!     - Fields and field info read by all, written by las processor
!       only.
!     - The MPP version of the model will perform a gather here to
!       maximize hiding of communication with IO.
!
!  8. Structure :
!
!     +---------------------------------------------------------------+
!     | initialisations                                               |
!     | test INXOUT                                                   |
!     | open file                                                     |
!     +---------------------------------------------------------------|
!     |                             WRITE ?                           |
!     | Y                                                           N |
!     |-------------------------------|-------------------------------|
!     | Write identifiers and         | Write identifiers and         |
!     |   dimensions.                 |   dimensions.                 |
!     |                               | Check ident. and dimensions.  |
!     +-------------------------------+-------------------------------|
!     |                       Full restart ?                          |
!     | Y                                                           N |
!     |-------------------------------|-------------------------------|
!     | read/write/test time          |                               |
!     +-------------------------------+-------------------------------|
!     |                             WRITE ?                           |
!     | Y                                                           N |
!     |-------------------------------|-------------------------------|
!     |          TYPE = 'WIND' ?      |          TYPE = 'WIND' ?      |
!     | Y                           N | Y                           N |
!     |---------------|---------------|---------------|---------------|
!     | close file    | write spectra | gen. fetch-l. | read spectra  |
!     | RETURN        |               |   spectra.    |               |
!     |---------------+---------------+---------------+---------------|
!     |                             WRITE ?                           |
!     | Y                                                           N |
!     |-------------------------------|-------------------------------|
!     |          TYPE = 'FULL' ?      |          TYPE = 'FULL' ?      |
!     | Y                           N | Y                           N |
!     |---------------|---------------|---------------|---------------|
!     | write level & | ( prep. level | read level &  | initalize l.& |
!     |   (ice) map & |   for test    |   (ice) map.& |   times       |
!     |   times       |   output )    |   times       | ( no ice )    |
!     +---------------+---------------+---------------+-------------- +
!
!  9. Switches :
!
!     !/SEED  Linear input / seeding option.
!     !/LNx
!
!     !/SHRD  Switch for shared / distributed memory architecture.
!     !/DIST  Id.
!     !/MPI   Id.
!
!     !/S     Enable subroutine tracing.
!     !/T     Enable test output
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: W3SETG, W3SETREF !JDM, RSTYPE
      USE W3ODATMD, ONLY: W3SETO
!/
      USE W3GDATMD, ONLY: NX, NY, NSEA, NSPEC, MAPSTA, MAPST2, &
                          GNAME, FILEXT, GTYPE, UNGTYPE
!JDM      USE W3TRIAMD, ONLY: SETUGIOBP
      USE W3WDATMD
      USE W3IDATMD, ONLY: WXN, WYN, W3SETI
!JDM      USE W3IDATMD, ONLY: WXNwrst, WYNwrst
      USE W3ODATMD, ONLY: NDSE, NDST, IAPROC, NAPROC, NAPERR, NAPRST, &
                          IFILE => IFILE4, FNMPRE, NTPROC, IOSTYP
!/
      USE W3SERVMD, ONLY: EXTCDE
      USE CONSTANTS,ONLY: file_endian
!JDM      USE CONSTANTS, only: LPDLIB
!JDM      USE W3PARALL, ONLY: INIT_GET_ISEA, INIT_GET_JSEA_ISPROC
      USE W3GDATMD, ONLY: NK, NTH
!!!!!/PDLIB    USE PDLIB_FIELD_VEC!, only : UNST_PDLIB_READ_FROM_FILE, UNST_PDLIB_WRITE_TO_FILE
!
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER                       :: NDSR
!      INTEGER, INTENT(IN)           :: NDSR
      INTEGER, INTENT(IN), OPTIONAL :: IMOD
      REAL, INTENT(INOUT)           :: DUMFPI
      CHARACTER, INTENT(IN)         :: INXOUT*(*)
      INTEGER, INTENT(OUT)          :: RSTYPE !JDM add back to argument list
      LOGICAL  :: FLRSTRT=.FALSE. !JDM set to false
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER, PARAMETER      :: LRB = 4
!
      INTEGER                 :: IGRD, I, J, LRECL, NSIZE, IERR,      &
                                 NSEAT, MSPEC, TTIME(2), ISEA, JSEA,  &
                                 NREC, NPART, IPART, IX, IY, IXL, IP, &
                                 NPRTX2, NPRTY2, IYL
      INTEGER, ALLOCATABLE    :: MAPTMP(:,:)
      INTEGER(KIND=8)         :: RPOS
      REAL(KIND=LRB), ALLOCATABLE :: WRITEBUFF(:)
 
      LOGICAL                 :: WRITE, IOSFLG
      CHARACTER(LEN=4)        :: TYPE
      CHARACTER(LEN=10)       :: VERTST
!      CHARACTER(LEN=21)       :: FNAME
      CHARACTER(LEN=100)       :: FNAME
      CHARACTER(LEN=26)       :: IDTST
      CHARACTER(LEN=30)       :: TNAME
      CHARACTER(LEN=15)       :: TIMETAG
!   Parameters added for backwards porting 
       REAL, ALLOCATABLE      :: WXNwrst(:,:),WYNwrst(:,:)
     LOGICAL                  :: LPDLIB = .FALSE.
!/
!/ ------------------------------------------------------------------- /
!/
!
! Constant NDSR for using mpiifort in ZEUS ... paralell runs crashing
!  because compiler doesn't accept reciclyng of UNIT for FORMATTED or
!  UNFORMATTED files in OPEN
!
!     NDSR = 525
 
      IOSFLG = IOSTYP .GT. 0
!
! test parameter list input ------------------------------------------ *
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
      CALL W3SETI ( IGRD, NDSE, NDST )
!
      IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'HOT'  .AND.               &
          INXOUT.NE.'COLD' .AND. INXOUT.NE.'WIND' .AND.               &
          INXOUT.NE.'CALM' ) THEN
          IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,900) INXOUT
          CALL EXTCDE ( 1 )
        END IF
!
      WRITE = INXOUT .NE. 'READ'
      IF ( INXOUT .EQ. 'HOT' ) THEN
          TYPE   = 'FULL'
        ELSE
          TYPE   = INXOUT
        END IF
!
! initializations ---------------------------------------------------- *
!
      IF ( .NOT.DINIT ) THEN
          IF ( IAPROC .LE. NAPROC ) THEN
              CALL W3DIMW ( IMOD, NDSE, NDST )
            ELSE
              CALL W3DIMW ( IMOD, NDSE, NDST, .FALSE. )
            END IF
        END IF
!
      IF ( IAPROC .LE. NAPROC ) VA(:,0) = 0.
! JDM Added for backwards porting 
      ALLOCATE(WXNwrst(NX,NY),WYNwrst(NX,NY))

!
      LRECL  = MAX ( LRB*NSPEC ,                                      &
                     LRB*(6+(25/LRB)+(9/LRB)+(29/LRB)+(3/LRB)) )
      NSIZE  = LRECL / LRB
!     --- Allocate buffer array with zeros (used to
!         fill bytes up to size LRECL). ---
      ALLOCATE(WRITEBUFF(NSIZE))
      WRITEBUFF(:) = 0.
!
! open file ---------------------------------------------------------- *
!
      I      = LEN_TRIM(FILEXT)
      J      = LEN_TRIM(FNMPRE)
!
!CHECKPOINT
      !IF ( PRESENT(FLRSTRT) .AND. FLRSTRT) THEN
      IF ( FLRSTRT) THEN
          WRITE(TIMETAG,"(i8.8,'.'i6.6)")TIME(1),TIME(2)
!         FNAME=TIMETAG//'.restart.'//FILEXT(:I)
          FNAME='restart_wave/'//TIMETAG//'.restart.'//FILEXT(:I)
      ELSE
         IF ( IFILE.EQ.0 ) THEN
            FNAME  = 'restart.'//FILEXT(:I)
         ELSE
            FNAME  = 'restartNNN.'//FILEXT(:I)
            IF ( WRITE .AND. IAPROC.EQ.NAPRST )                         &
               WRITE (FNAME(8:10),'(I3.3)') IFILE
        END IF
      END IF
 
      IFILE  = IFILE + 1
!
 
      IF(NDST.EQ.NDSR)THEN
         IF ( IAPROC .EQ. NAPERR )                                    &
            WRITE(NDSE,'(A,I8)')'UNIT NUMBERS OF RESTART FILE AND '&
            //'TEST OUTPUT ARE THE SAME : ',NDST
         CALL EXTCDE ( 15 )
      ENDIF
 
      IF ( WRITE ) THEN
          IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST )                    &
          print*,"FNAME-- ",FNMPRE(:J)//trim(FNAME)
          OPEN (NDSR,FILE=FNMPRE(:J)//trim(FNAME),FORM='UNFORMATTED', convert=file_endian,       &
                ACCESS='STREAM',ERR=800,IOSTAT=IERR)
        ELSE
          OPEN (NDSR,FILE=FNMPRE(:J)//trim(FNAME),FORM='UNFORMATTED', convert=file_endian,      &
                ACCESS='STREAM',ERR=800,IOSTAT=IERR,                  &
                STATUS='OLD',ACTION='READ')
        END IF
!
! test info ---------------------------------------------------------- *
!
      IF ( WRITE ) THEN
!
          IF ( IAPROC .EQ. NAPRST ) THEN
!           Because data has mixed data types we do not know how many
!           bytes remain to fill up to LRECL. ---
!           --- Make the entire record zero ---
            WRITEBUFF(:) = 0.
            WRITE (NDSR,POS=1) WRITEBUFF
!           --- Replace zeros with data ---
            WRITE (NDSR,POS=1) IDSTR, VERINI, GNAME, TYPE, NSEA, NSPEC
          END IF
          RSTYPE = 3
!
        ELSE
          READ (NDSR,POS=1,ERR=802,IOSTAT=IERR)                       &
            IDTST, VERTST, TNAME, TYPE, NSEAT, MSPEC
!
          IF ( IDTST .NE. IDSTR ) THEN
              IF ( IAPROC .EQ. NAPERR )                               &
                  WRITE (NDSE,901) IDTST, IDSTR
              CALL EXTCDE ( 10 )
            END IF
          IF ( VERTST .NE. VERINI ) THEN
              IF ( IAPROC .EQ. NAPERR )                               &
                  WRITE (NDSE,902) VERTST, VERINI
              CALL EXTCDE ( 11 )
            END IF
          IF ( TNAME .NE. GNAME ) THEN
              IF ( IAPROC .EQ. NAPERR )                               &
                  WRITE (NDSE,903) TNAME, GNAME
            END IF
          IF (TYPE.NE.'FULL' .AND. TYPE.NE.'COLD' .AND.               &
              TYPE.NE.'WIND' .AND. TYPE.NE.'CALM' ) THEN
              IF ( IAPROC .EQ. NAPERR )                               &
                  WRITE (NDSE,904) TYPE
              CALL EXTCDE ( 12 )
            END IF
          IF (NSEAT.NE.NSEA .OR. NSPEC.NE.MSPEC) THEN
              IF ( IAPROC .EQ. NAPERR )                               &
                  WRITE (NDSE,905) MSPEC, NSEAT, NSPEC, NSEA
              CALL EXTCDE ( 13 )
            END IF
          IF (TYPE.EQ.'FULL') THEN
              RSTYPE = 2
            ELSE IF (TYPE.EQ.'WIND') THEN
              RSTYPE = 1
            ELSE IF (TYPE.EQ.'CALM') THEN
              RSTYPE = 4
            ELSE
              RSTYPE = 0
            END IF
!
        END IF
!
  100 CONTINUE
!
! TIME if required --------------------------------------------------- *
!
      IF (TYPE.EQ.'FULL') THEN
          RPOS  = 1_8 + LRECL*(2-1_8)
          IF ( WRITE ) THEN
              IF ( IAPROC .EQ. NAPRST ) THEN
                WRITEBUFF(:) = 0.
                WRITE (NDSR,POS=RPOS) WRITEBUFF
                WRITE (NDSR,POS=RPOS) TIME
              END IF
            ELSE
              READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR) TTIME
              IF (TIME(1).NE.TTIME(1) .OR. TIME(2).NE.TTIME(2)) THEN
                  IF ( IAPROC .EQ. NAPERR )                           &
                      WRITE (NDSE,906) TTIME, TIME
                  CALL EXTCDE ( 20 )
                END IF
            END IF
!
        END IF
!
! Spectra ------------------------------------------------------------ *
!          ( Bail out if write for TYPE.EQ.'WIND' )
!
      IF ( WRITE ) THEN
          IF ( TYPE.EQ.'WIND' .OR. TYPE.EQ.'CALM' ) THEN
              IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST ) THEN
                CLOSE ( NDSR )
              END IF
              RETURN
            ELSE IF ( IAPROC.LE.NAPROC .OR. IAPROC.EQ. NAPRST ) THEN
!
! Original non-server version writing of spectra
!
              IF ( .NOT.IOSFLG .OR. (NAPROC.EQ.1.AND.NAPRST.EQ.1) ) THEN
                  DO JSEA=1, NSEA
                    ISEA  = JSEA    !JDM CALL INIT_GET_ISEA(ISEA, JSEA)
                    NREC   = ISEA + 2
                    RPOS  = 1_8 + LRECL*(NREC-1_8)
                    WRITEBUFF(:) = 0.
                    WRITEBUFF(1:NSPEC) = VA(1:NSPEC,JSEA)
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
                    END DO
!
! I/O server version writing of spectra ( !/MPI )
!
 
!
                END IF
!
            END IF
        ELSE
!
! Reading spectra
!
          IF ( TYPE.EQ.'WIND' .OR. TYPE.EQ.'CALM' ) THEN
          ELSE
            IF (LPDLIB .and. (GTYPE.eq.UNGTYPE)) THEN
            ELSE
              VA = 0.
              DO JSEA=1, NSEA
                ISEA  = JSEA    !JDM CALL INIT_GET_ISEA(ISEA, JSEA)
                NREC   = ISEA + 2
                RPOS   = 1_8 + LRECL*(NREC-1_8)
                READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                         (VA(I,JSEA),I=1,NSPEC)
                ENDDO
            END IF
          END IF
        END IF
 
!AR: Must be checked better ... will do that when cleaning debugging switches!
        VA = MAX(0.,VA)
!
! Water level etc. if required --------------------------------------- *
!     ( For cold start write test output and cold start initialize
!       water levels. Note that MAPSTA overwrites the one read from the
!       model definition file, so that it need not be initialized. )
!
      NREC   = NSEA + 3
      NPART  = 1 + (NSEA-1)/NSIZE
      NPRTX2 = 1 + (NX-1)/NSIZE
      NPRTY2 = 1 + (NY-1)/NSIZE
!
      IF ( WRITE ) THEN
!
          IF (TYPE.EQ.'FULL') THEN
!
              IF ( IAPROC .EQ. NAPRST ) THEN
!
                  RPOS  = 1_8 + LRECL*(NREC-1_8)
                  WRITEBUFF(:) = 0.
                  WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
                  WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)           &
                          TLEV, TICE
                  DO IPART=1,NPART
                    NREC  = NREC + 1
                    RPOS  = 1_8 + LRECL*(NREC-1_8)
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
                          (WLV(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
                                          MIN(NSEA,IPART*NSIZE))
                    END DO
                  DO IPART=1,NPART
                    NREC  = NREC + 1
                    RPOS  = 1_8 + LRECL*(NREC-1_8)
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
                          (ICE(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
                                          MIN(NSEA,IPART*NSIZE))
                  END DO
                  DO IX=1, NX
                    DO IPART=1,NPRTY2
                      NREC  = NREC + 1
                      RPOS  = 1_8 + LRECL*(NREC-1_8)
                      WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
                      WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)       &
                          (WXN(IX,IYL),IYL=1+(IPART-1)*NSIZE,         &
                                         MIN(NY,IPART*NSIZE))
                    END DO
                  END DO
                  DO IX=1, NX
                    DO IPART=1,NPRTY2
                      NREC  = NREC + 1
                      RPOS  = 1_8 + LRECL*(NREC-1_8)
                      WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
                      WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)       &
                          (WYN(IX,IYL),IYL=1+(IPART-1)*NSIZE,         &
                                         MIN(NY,IPART*NSIZE))
                    END DO
                  END DO
                  ALLOCATE ( MAPTMP(NY,NX) )
                  MAPTMP = MAPSTA + 8*MAPST2
                  DO IY=1, NY
                    DO IPART=1,NPRTX2
                      NREC  = NREC + 1
                      RPOS  = 1_8 + LRECL*(NREC-1_8)
                      WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)       &
                             WRITEBUFF
                      WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)       &
                            (MAPTMP(IY,IXL),IXL=1+(IPART-1)*NSIZE,    &
                                                MIN(NX,IPART*NSIZE))
                      END DO
                    END DO
                  DEALLOCATE ( MAPTMP )
                  DO IPART=1,NPART
                    NREC  = NREC + 1
                    RPOS  = 1_8 + LRECL*(NREC-1_8)
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
                          (UST(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
                                          MIN(NSEA,IPART*NSIZE))
                    END DO
                  DO IPART=1,NPART
                    NREC  = NREC + 1
                    RPOS  = 1_8 + LRECL*(NREC-1_8)
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
                          (USTDIR(ISEA),ISEA=1+(IPART-1)*NSIZE,       &
                                          MIN(NSEA,IPART*NSIZE))
                    END DO
                  DO IPART=1,NPART
                    NREC  = NREC + 1
                    RPOS  = 1_8 + LRECL*(NREC-1_8)
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
                          (ASF(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
                                          MIN(NSEA,IPART*NSIZE))
                    END DO
                  DO IPART=1,NPART
                    NREC  = NREC + 1
                    RPOS  = 1_8 + LRECL*(NREC-1_8)
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
                    WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
                          (FPIS(ISEA),ISEA=1+(IPART-1)*NSIZE,         &
                                          MIN(NSEA,IPART*NSIZE))
                    END DO
                END IF
            END IF
        ELSE
          IF (TYPE.EQ.'FULL') THEN
              RPOS = 1_8 + LRECL*(NREC-1_8)
              READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)                &
                      TLEV, TICE
              DO IPART=1,NPART
                NREC  = NREC + 1
                RPOS = 1_8 + LRECL*(NREC-1_8)
                READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                      (WLV(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
                                      MIN(NSEA,IPART*NSIZE))
                END DO
              DO IPART=1,NPART
                NREC  = NREC + 1
                RPOS = 1_8 + LRECL*(NREC-1_8)
                READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                      (ICE(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
                                      MIN(NSEA,IPART*NSIZE))
              END DO
              DO IX=1, NX
               DO IPART=1,NPRTY2
                NREC  = NREC + 1
                RPOS = 1_8 + LRECL*(NREC-1_8)
                READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                      (WXNwrst(IX,IYL),IYL=1+(IPART-1)*NSIZE,         &
                                      MIN(NY,IPART*NSIZE))
               END DO
              END DO
              DO IX=1, NX
               DO IPART=1,NPRTY2
                NREC  = NREC + 1
                RPOS = 1_8 + LRECL*(NREC-1_8)
                READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                      (WYNwrst(IX,IYL),IYL=1+(IPART-1)*NSIZE,         &
                                      MIN(NY,IPART*NSIZE))
               END DO
              END DO
              ALLOCATE ( MAPTMP(NY,NX) )
              DO IY=1, NY
                DO IPART=1,NPRTX2
                  NREC  = NREC + 1
                  RPOS  = 1_8 + LRECL*(NREC-1_8)
                  READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)            &
                        (MAPTMP(IY,IXL),IXL=1+(IPART-1)*NSIZE,        &
                                            MIN(NX,IPART*NSIZE))
                  END DO
                END DO
              MAPSTA = MOD(MAPTMP+2,8) - 2
              MAPST2 = (MAPTMP-MAPSTA) / 8
              DEALLOCATE ( MAPTMP )
!
! Updates reflections maps:
!
              IF (GTYPE.EQ.UNGTYPE) THEN
                !CALL SETUGIOBP   !JDM compiler errors
                ENDIF
!
              DO IPART=1,NPART
                NREC  = NREC + 1
                RPOS  = 1_8 + LRECL*(NREC-1_8)
                READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                      (UST(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
                                      MIN(NSEA,IPART*NSIZE))
                END DO
              DO IPART=1,NPART
                NREC  = NREC + 1
                RPOS  = 1_8 + LRECL*(NREC-1_8)
                READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                      (USTDIR(ISEA),ISEA=1+(IPART-1)*NSIZE,           &
                                      MIN(NSEA,IPART*NSIZE))
                END DO
              DO IPART=1,NPART
                NREC  = NREC + 1
                RPOS  = 1_8 + LRECL*(NREC-1_8)
                READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                      (ASF(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
                                      MIN(NSEA,IPART*NSIZE))
                END DO
              DO IPART=1,NPART
                NREC  = NREC + 1
                RPOS  = 1_8 + LRECL*(NREC-1_8)
                READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                      (FPIS(ISEA),ISEA=1+(IPART-1)*NSIZE,             &
                                      MIN(NSEA,IPART*NSIZE))
                END DO
            ELSE
              TLEV(1) = -1
              TLEV(2) =  0
              TICE(1) = -1
              TICE(2) =  0
              TIC1(1) = -1
              TIC1(2) =  0
              TIC5(1) = -1
              TIC5(2) =  0
              WXNwrst =  0.
              WYNwrst =  0.
              WLV     =  0.
              ICE     =  0.
              ASF     =  1.
              FPIS    =  DUMFPI
            END IF
        END IF
!
! Close file --------------------------------------------------------- *
!
      IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST ) THEN
        CLOSE ( NDSR )
      END IF
!
      IF (ALLOCATED(WXNwrst)) DEALLOCATE(WXNwrst)
      IF (ALLOCATED(WYNwrst)) DEALLOCATE(WYNwrst)

      IF (ALLOCATED(WRITEBUFF)) DEALLOCATE(WRITEBUFF)
!
      RETURN
!
! Escape locations read errors :
!
  800 CONTINUE
      TYPE   = 'CALM'
      RSTYPE = 4
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,990) TYPE, IERR
      GOTO 100
!
  801 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,991)
      CALL EXTCDE ( 30 )
!
  802 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,992) IERR
      CALL EXTCDE ( 31 )
!
  803 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,993) IERR, RPOS
      CALL EXTCDE ( 31 )
!
! Formats
!
  900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
               '     ILLEGAL INXOUT VALUE: ',A/)
  901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
               '     ILLEGAL IDSTR, READ : ',A/                       &
               '                   CHECK : ',A/)
  902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
               '     ILLEGAL VERINI, READ : ',A/                      &
               '                    CHECK : ',A/)
  903 FORMAT (/' *** WAVEWATCH III WARNING IN W3IORS :'/              &
               '     ILLEGAL GNAME, READ : ',A/                       &
               '                   CHECK : ',A/)
  904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
               '     ILLEGAL TYPE : ',A/)
  905 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
               '     CONFLICTING NSPEC, NSEA GRID : ',2I8/            &
               '                         EXPECTED : ',2I8/)
  906 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
               '     CONFLICTING TIMES: FILE : ',I10.8,I8.6/          &
               '                       MODEL : ',I10.8,I8.6/)
!
  990 FORMAT (/' *** WAVEWATCH III WARNING IN W3IORS : '/             &
               '     NO READABLE RESTART FILE, ',                     &
                    'INITIALIZE WITH ''',A,''' INSTEAD'/              &
               '     IOSTAT =',I5/)
  991 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS : '/               &
               '     PREMATURE END OF FILE'/)
  992 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS : '/               &
               '     ERROR IN READING FROM FILE'/                     &
               '     IOSTAT =',I5/)
  993 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS : '/               &
               '     ERROR IN WRITING TO FILE'/                       &
               '     IOSTAT =',I5,', POS =',I11 /)
!
!/
!/ End of W3IORSOLD ----------------------------------------------------- /
!/
      END SUBROUTINE W3IORSOLD
!/
!/ End of module W3IORSMDOLD -------------------------------------------- /
!/
      END MODULE W3IORSMDOLD
