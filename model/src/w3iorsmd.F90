!> @file
!> @brief Read/write restart files.
!>
!> @author H. L. Tolman  @date 22-Mar-2021
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Read/write restart files.
!>
!> @author H. L. Tolman  @date 22-Mar-2021
!>
MODULE W3IORSMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |                      FORTRAN 2003 |
  !/                  | Last update :         22-Mar-2021 |
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
  ! Add fields needed for OASIS coupling in restart
  LOGICAL :: OARST
  !/
  !/ Private parameter statements (ID strings)
  !/
  CHARACTER(LEN=10), PARAMETER, PRIVATE :: VERINI = '2024-04-26'
  CHARACTER(LEN=26), PARAMETER, PRIVATE ::                        &
       IDSTR = 'WAVEWATCH III RESTART FILE'
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Reads/writes restart files.
  !>
  !> @details
  !> @verbatim
  !>     The file is opened within the routine, the name is pre-defined
  !>     and the unit number is given in the parameter list. The restart
  !>     file is written using UNFORMATTED write statements. The routine
  !>     generates new names when called more than once. File names are :
  !>
  !>                                 restart000.FILEXT
  !>                                 restart001.FILEXT
  !>                                 restart002.FILEXT etc.
  !>
  !>     Optionally, a second stream of restart files is generated given
  !>     a secondary stride definad by an additional start/end time line
  !>     triggered by an optional argument added to the end of the stan-
  !>     dard restart request line (a sixth argument flag set to T). File
  !>     names include a time-tag prefix:
  !>
  !>                                YYYYMMDD.HHMMSS.restart.FILEXT
  !>
  !>     The file to be read thus always is unnumbered, whereas all
  !>     written files are automatically numbered.
  !> @endverbatim
  !>
  !> @param[in]    INXOUT   Test string for read/write.
  !> @param[inout] NDSR     File unit number.
  !> @param[in]    DUMFPI   Dummy values for FPIS for cold start.
  !> @param[in]    IMOD     Optional grid number, defaults to 1.
  !> @param[in]    FLRSTRT  A second request for restart files (optional TRUE).
  !>
  !> @author H. L. Tolman  @date 22-Mar-2021
  !>
  SUBROUTINE W3IORS ( INXOUT, NDSR, DUMFPI, IMOD, FLRSTRT )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 |
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
    !/    25-Sep-2020 : Extra fields for coupled restart    ( version 7.10 )
    !/    22-Mar-2021 : Add new coupling fields in restart  ( version 7.13 )
    !/    18-May-2021 : Read by default all extra restart   ( version 7.13 )
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
    USE W3GDATMD, ONLY: W3SETG, W3SETREF, RSTYPE
    USE W3ODATMD, ONLY: W3SETO
    USE W3ADATMD, ONLY: W3SETA, W3XETA, NSEALM
    USE W3ADATMD, ONLY: CX, CY, HS, WLM, T0M1, T01, FP0, THM, CHARN,&
         TAUWIX, TAUWIY, TWS, TAUOX, TAUOY, BHD,     &
         PHIOC, TUSX, TUSY, USSX, USSY, TAUICE,      &
         UBA, UBD, PHIBBL, TAUBBL, TAUOCX, TAUOCY,   &
         WNMEAN
    !/
    USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, NSPEC, MAPSTA, MAPST2, &
         GNAME, FILEXT, GTYPE, UNGTYPE
    USE W3TRIAMD, ONLY: SET_UG_IOBP
    USE W3WDATMD
#ifdef W3_WRST
    USE W3IDATMD, ONLY: WXN, WYN, W3SETI
    USE W3IDATMD, ONLY: WXNwrst, WYNwrst
#endif
    USE W3ODATMD, ONLY: NDSE, NDST, IAPROC, NAPROC, NAPERR, NAPRST, &
         IFILE => IFILE4, FNMPRE, NTPROC, IOSTYP,    &
         FLOGRR, NOGRP, NGRPP, SCREEN
#ifdef W3_MPI
    USE W3ODATMD, ONLY: NRQRS, NBLKRS, RSBLKS, IRQRS, IRQRSS,  &
         VAAUX
    USE W3ADATMD, ONLY: MPI_COMM_WCMP
#endif
    !/
    USE W3SERVMD, ONLY: EXTCDE
    USE CONSTANTS, only: LPDLIB, file_endian
    USE W3PARALL, ONLY: INIT_GET_ISEA, INIT_GET_JSEA_ISPROC
    USE W3GDATMD, ONLY: NK, NTH
#ifdef W3_TIMINGS
    USE W3PARALL, ONLY: PRINT_MY_TIME
#endif
#ifdef W3_PDLIB
    USE PDLIB_FIELD_VEC
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
    INTEGER                       :: NDSR
    !      INTEGER, INTENT(IN)           :: NDSR
    INTEGER, INTENT(IN), OPTIONAL :: IMOD
    REAL, INTENT(INOUT)           :: DUMFPI
    CHARACTER, INTENT(IN)         :: INXOUT*(*)
    LOGICAL, INTENT(IN),OPTIONAL  :: FLRSTRT
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER, PARAMETER      :: LRB = 4
    !
    INTEGER                 :: IGRD, I, J, LRECL, NSIZE, IERR,      &
         NSEAT, MSPEC, TTIME(2), ISEA, JSEA,  &
         NREC, NPART, IPART, IX, IY, IXL, IP, &
         NPRTX2, NPRTY2, IYL, ITMP
    INTEGER, ALLOCATABLE    :: MAPTMP(:,:)
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_MPI
    INTEGER                 :: IERR_MPI, IH, IB, ISEA0, ISEAN, &
         NRQ, NSEAL_MIN
#endif
    INTEGER(KIND=8)         :: RPOS
#ifdef W3_MPI
    INTEGER, ALLOCATABLE    :: STAT1(:,:), STAT2(:,:)
    REAL, ALLOCATABLE       :: VGBUFF(:), VLBUFF(:)
#endif
    REAL(KIND=LRB), ALLOCATABLE :: WRITEBUFF(:), TMP(:), TMP2(:)

    LOGICAL                 :: WRITE, IOSFLG
    LOGICAL                 :: FLOGOA(NOGRP,NGRPP)
    LOGICAL                 :: NDSROPN
    CHARACTER(LEN=4)        :: TYPE
    CHARACTER(LEN=10)       :: VERTST
    !      CHARACTER(LEN=21)       :: FNAME
    CHARACTER(LEN=40)       :: FNAME
    CHARACTER(LEN=26)       :: IDTST
    CHARACTER(LEN=30)       :: TNAME
    CHARACTER(LEN=15)       :: TIMETAG
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3IORS')
#endif
    !
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
#ifdef W3_WRST
    CALL W3SETI ( IGRD, NDSE, NDST )
#endif
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
#ifdef W3_T
    WRITE (NDST,9000) INXOUT, WRITE, NTPROC, NAPROC, IAPROC, NAPRST
#endif
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
    !
    LRECL  = MAX ( LRB*NSPEC ,                                      &
         LRB*(6+(25/LRB)+(9/LRB)+(29/LRB)+(3/LRB)) )
    NSIZE  = LRECL / LRB
    !     --- Allocate buffer array with zeros (used to
    !         fill bytes up to size LRECL). ---
    ALLOCATE(WRITEBUFF(NSIZE))
    WRITEBUFF(:) = 0.
    !
    !     Allocate memory to receive fields needed for coupling
    IF (OARST) THEN
      ALLOCATE(TMP(NSEA))
      ALLOCATE(TMP2(NSEA))
    ENDIF
    !
    ! open file ---------------------------------------------------------- *
    !
    I      = LEN_TRIM(FILEXT)
    J      = LEN_TRIM(FNMPRE)
    !
    !CHECKPOINT RESTART FILE
    ITMP=0
    IF ( PRESENT(FLRSTRT) ) THEN
      IF (FLRSTRT) THEN
        WRITE(TIMETAG,"(i8.8,'.'i6.6)")TIME(1),TIME(2)
        FNAME=TIMETAG//'.restart.'//FILEXT(:I)
        ITMP=1
      END IF
    END IF
    IF(ITMP.NE.1)THEN ! FNAME is not set above, so do it here
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
#ifdef W3_T
    WRITE (NDST,9001) FNAME, LRECL
#endif
    !

    IF(NDST.EQ.NDSR)THEN
      IF ( IAPROC .EQ. NAPERR )                                    &
           WRITE(NDSE,'(A,I8)')'UNIT NUMBERS OF RESTART FILE AND '&
           //'TEST OUTPUT ARE THE SAME : ',NDST
      CALL EXTCDE ( 15 )
    ENDIF

    IF ( WRITE ) THEN
      IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST )                    &
           OPEN (NDSR,FILE=FNMPRE(:J)//FNAME,form='UNFORMATTED', convert=file_endian,       &
           ACCESS='STREAM',ERR=800,IOSTAT=IERR)
    ELSE
      OPEN (NDSR,FILE=FNMPRE(:J)//FNAME,form='UNFORMATTED', convert=file_endian,       &
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
        WRITE (NDSR,POS=1) IDSTR, VERINI, GNAME, TYPE, NSEA,      &
             NSPEC, FLOGRR
      END IF
      RSTYPE = 3
      !
    ELSE
      READ (NDSR,POS=1,ERR=802,IOSTAT=IERR)                       &
           IDTST, VERTST, TNAME, TYPE, NSEAT, MSPEC, FLOGOA
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

      IF (.NOT. WRITE .AND. OARST .AND. IAPROC .EQ. NAPROC) THEN
        DO I=1, NOGRP
          DO J=1, NGRPP
            IF (FLOGRR(I,J) .AND. .NOT. FLOGOA(I,J)) THEN
              WRITE(SCREEN,1000) I, J
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      !
    END IF
    !
100 CONTINUE
    !
#ifdef W3_T
    WRITE (NDST,9002) IDSTR, VERINI, GNAME, TYPE,                &
         NSEA, NSEAL, NSPEC
#endif
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
#ifdef W3_T
      WRITE (NDST,9003) TIME
    ELSE
      WRITE (NDST,9004)
#endif
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
#ifdef W3_T
        WRITE (NDST,9005) TYPE
#endif
        ! Clean up file handles and allocated arrays
        INQUIRE (UNIT=NDSR, OPENED=NDSROPN)
        IF (NDSROPN)              CLOSE(NDSR)
        IF (ALLOCATED(WRITEBUFF)) DEALLOCATE(WRITEBUFF)
        IF (ALLOCATED(TMP))       DEALLOCATE(TMP)
        IF (ALLOCATED(TMP2))      DEALLOCATE(TMP2)

        RETURN
      ELSE IF ( IAPROC.LE.NAPROC .OR. IAPROC.EQ. NAPRST ) THEN
        !
        ! Original non-server version writing of spectra
        !
        IF ( .NOT.IOSFLG .OR. (NAPROC.EQ.1.AND.NAPRST.EQ.1) ) THEN
#ifdef W3_MPI
          DO JSEA=1, NSEAL
            CALL INIT_GET_ISEA(ISEA, JSEA)
            NREC   = ISEA + 2
            RPOS  = 1_8 + LRECL*(NREC-1_8)
            WRITEBUFF(:) = 0.
            WRITEBUFF(1:NSPEC) = VA(1:NSPEC,JSEA)
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
          END DO
#else
          DO JSEA=1, NSEA
            ISEA = JSEA
            NREC   = ISEA + 2
            RPOS  = 1_8 + LRECL*(NREC-1_8)
            WRITEBUFF(:) = 0.
            WRITEBUFF(1:NSPEC) = VA(1:NSPEC,JSEA)
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
          END DO
#endif
          !
          ! I/O server version writing of spectra ( !/MPI )
          !
#ifdef W3_MPI
        ELSE
          !
          IF (LPDLIB) THEN
#endif
#ifdef W3_TIMINGS
            CALL PRINT_MY_TIME("Before UNST_PDLIB_WRITE_TO_FILE")
#endif
#ifdef W3_PDLIB
            CALL UNST_PDLIB_WRITE_TO_FILE(NDSR)
#endif
#ifdef W3_TIMINGS
            CALL PRINT_MY_TIME("After UNST_PDLIB_WRITE_TO_FILE")
#endif
#ifdef W3_MPI
          ELSE

            IF ( IAPROC .NE. NAPRST ) THEN
              NRQ    = 1
            ELSE IF ( NAPRST .LE. NAPROC ) THEN
              NRQ    = NAPROC - 1
            ELSE
              NRQ    = NAPROC
            END IF
            !
            ALLOCATE ( STAT1(MPI_STATUS_SIZE,NRQ) )
            IF ( IAPROC .EQ. NAPRST ) CALL MPI_STARTALL    &
                 ( NRQ, IRQRSS, IERR_MPI )
            !
            DO IB=1, NBLKRS
              ISEA0  = 1 + (IB-1)*RSBLKS*NAPROC
              ISEAN  = MIN ( NSEA , IB*RSBLKS*NAPROC )
              !
              IF ( IAPROC .EQ. NAPRST ) THEN
                !
                IH     = 1 + NRQ * (IB-1)
                CALL MPI_WAITALL                         &
                     ( NRQ, IRQRSS(IH), STAT1, IERR_MPI )
                IF ( IB .LT. NBLKRS ) THEN
                  IH     = 1 + NRQ * IB
                  CALL MPI_STARTALL                    &
                       ( NRQ, IRQRSS(IH), IERR_MPI )
                END IF
                !
                DO ISEA=ISEA0, ISEAN
                  NREC   = ISEA + 2
                  CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, IP)
                  RPOS   = 1_8 + LRECL*(NREC-1_8)
                  WRITEBUFF(:) = 0.
                  IF ( IP .EQ. NAPRST ) THEN
                    WRITEBUFF(1:NSPEC) = VA(1:NSPEC,JSEA)
                  ELSE
                    JSEA   = JSEA - 2*((IB-1)/2)*RSBLKS
                    WRITEBUFF(1:NSPEC) = VAAUX(1:NSPEC,JSEA,IP)
                  END IF
                  WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) &
                       WRITEBUFF
                END DO
                !
              ELSE
                !
                CALL MPI_STARTALL                        &
                     ( 1, IRQRSS(IB), IERR_MPI )
                CALL MPI_WAITALL                         &
                     ( 1, IRQRSS(IB), STAT1, IERR_MPI )
                !
              END IF
            END DO
            !
            DEALLOCATE ( STAT1 )
          END IF
#endif
          !
        END IF
        !
      END IF
    ELSE
      !
      ! Reading spectra
      !
      IF ( TYPE.EQ.'WIND' .OR. TYPE.EQ.'CALM' ) THEN
#ifdef W3_T
        WRITE (NDST,9020) TYPE
#endif
      ELSE
        IF (LPDLIB) THEN
#ifdef W3_TIMINGS
          CALL PRINT_MY_TIME("Before UNST_PDLIB_READ_FROM_FILE")
#endif
#ifdef W3_PDLIB
          CALL UNST_PDLIB_READ_FROM_FILE(NDSR)
#endif
#ifdef W3_TIMINGS
          CALL PRINT_MY_TIME("After UNST_PDLIB_READ_FROM_FILE")
#endif
        ELSE
#ifdef W3_MPI
          NSEAL_MIN = 1 + (NSEA-NAPROC)/NAPROC
          IF ( NAPROC.GT.1 ) THEN
            !/ ----------- Large number of small-sized record reads will tend ---- *
            !/             to perform badly on most file systems. We read this part
            !/             using streams and scatter the results using MPI.
            !/                                                      ( M. WARD, NCI )
            !
            !              Begin computational proc. only section ---------------- *
            IF ( IAPROC.LE.NAPROC ) THEN
              !
              !              Main loop --------------------------------------------- *
              ALLOCATE( VGBUFF( NSIZE * NAPROC ) )
              ALLOCATE( VLBUFF( NSIZE ) )
              !
              DO JSEA = 1, NSEAL_MIN
                !                Read NAPROC records into buffer VGBUFF. ------------- *
                IF ( IAPROC .EQ. NAPROC ) THEN
                  RPOS = 1_8 + (2 + (JSEA - 1_8) * NAPROC) * LRECL
                  READ(NDSR, POS=RPOS,ERR=802,IOSTAT=IERR) VGBUFF(:)
                ELSE
                  VGBUFF(:) = 0.
                END IF
                !                Distribute one record to each rank.
                CALL MPI_SCATTER(VGBUFF, NSIZE, MPI_REAL,             &
                     VLBUFF, NSIZE, MPI_REAL,             &
                     NAPROC-1, MPI_COMM_WCMP, IERR        )
                !                Transfer the spectral content of VLBUFF to VA. ------ *
                VA(1:NSPEC,JSEA) = VLBUFF(1:NSPEC)
              END DO
              !
              !              Include remainder values (switch to record format) ---- *
              JSEA = NSEAL_MIN + 1
              IF ( JSEA.EQ.NSEAL ) THEN
                CALL INIT_GET_ISEA(ISEA, JSEA)
                NREC = ISEA + 2
                RPOS = 1_8 + LRECL*(NREC-1_8)
                READ (NDSR, POS=RPOS, ERR=802, IOSTAT=IERR)          &
                     (VA(I,JSEA), I=1,NSPEC)
              END IF
              !
              DEALLOCATE( VGBUFF )
              DEALLOCATE( VLBUFF )
              !
              !              End computational proc. only section ------------------ *
            END IF
            !
          ELSE
#endif
            VA = 0.
            DO JSEA=1, NSEA
              CALL INIT_GET_ISEA(ISEA, JSEA)
              NREC   = ISEA + 2
              RPOS   = 1_8 + LRECL*(NREC-1_8)
              READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                   (VA(I,JSEA),I=1,NSPEC)
            ENDDO
#ifdef W3_MPI
          END IF
#endif
        END IF
      END IF
    END IF

    VA = MAX(0.,VA)
    !
#ifdef W3_T
    WRITE (NDST,9006)
#endif
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
#ifdef W3_MPI
          if (associated(irqrs)) then
            ALLOCATE ( STAT2(MPI_STATUS_SIZE,NRQRS) )
            CALL MPI_WAITALL                               &
                 ( NRQRS, IRQRS , STAT2, IERR_MPI )
            DEALLOCATE ( STAT2 )
          end if
#endif
          !
          RPOS  = 1_8 + LRECL*(NREC-1_8)
          WRITEBUFF(:) = 0.
          WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
          WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)           &
               TLEV, TICE, TRHO, TIC1, TIC5
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

#ifdef W3_WRST
          ! The WRST switch saves the values of wind in the
          ! restart file and then uses the wind for the first
          ! time step here.  This is needed when coupling with
          ! an atm model that does not have 10m wind speeds at
          ! initialization.  If there is no restart, wind is zero
#endif

#ifdef W3_WRST
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
#endif
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
          IF (OARST) THEN
#ifdef W3_MPI
            CALL W3XETA ( IGRD, NDSE, NDST )
#endif
            !
            IF ( FLOGRR(1,2) ) THEN
              WRITE(NDSR,ERR=803,IOSTAT=IERR) CX(1:NSEA)
              WRITE(NDSR,ERR=803,IOSTAT=IERR) CY(1:NSEA)
            ENDIF
            IF ( FLOGRR(1,12) )                                 &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) ICEF(1:NSEA)
            IF ( FLOGRR(2,1) )                                  &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) HS(1:NSEA)
            IF ( FLOGRR(2,2) )                                  &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) WLM(1:NSEA)
            IF ( FLOGRR(2,4) )                                  &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) T0M1(1:NSEA)
            IF ( FLOGRR(2,5) )                                  &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) T01(1:NSEA)
            IF ( FLOGRR(2,6) )                                  &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) FP0(1:NSEA)
            IF ( FLOGRR(2,7) )                                  &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) THM(1:NSEA)
            IF ( FLOGRR(2,19) )                                 &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) WNMEAN(1:NSEA)
            IF ( FLOGRR(5,2) )                                  &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) CHARN(1:NSEA)
            IF ( FLOGRR(5,5) ) THEN
              WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUWIX(1:NSEA)
              WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUWIY(1:NSEA)
            ENDIF
            IF ( FLOGRR(5,11) )                                 &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) TWS(1:NSEA)
            IF ( FLOGRR(6,2) ) THEN
              WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUOX(1:NSEA)
              WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUOY(1:NSEA)
            ENDIF
            IF ( FLOGRR(6,3) )                                  &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) BHD(1:NSEA)
            IF ( FLOGRR(6,4) )                                  &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) PHIOC(1:NSEA)
            IF ( FLOGRR(6,5) ) THEN
              WRITE(NDSR,ERR=803,IOSTAT=IERR) TUSX(1:NSEA)
              WRITE(NDSR,ERR=803,IOSTAT=IERR) TUSY(1:NSEA)
            ENDIF
            IF ( FLOGRR(6,6) ) THEN
              WRITE(NDSR,ERR=803,IOSTAT=IERR) USSX(1:NSEA)
              WRITE(NDSR,ERR=803,IOSTAT=IERR) USSY(1:NSEA)
            ENDIF
            IF ( FLOGRR(6,10) ) THEN
              WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUICE(1:NSEA,1)
              WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUICE(1:NSEA,2)
            ENDIF
            IF ( FLOGRR(6,13) ) THEN
              WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUOCX(1:NSEA)
              WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUOCY(1:NSEA)
            ENDIF
            IF ( FLOGRR(7,2) ) THEN
              WRITE(NDSR,ERR=803,IOSTAT=IERR) UBA(1:NSEA)
              WRITE(NDSR,ERR=803,IOSTAT=IERR) UBD(1:NSEA)
            ENDIF
            IF ( FLOGRR(7,4) )                                  &
                 WRITE(NDSR,ERR=803,IOSTAT=IERR) PHIBBL(1:NSEA)
            IF ( FLOGRR(7,5) ) THEN
              WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUBBL(1:NSEA,1)
              WRITE(NDSR,ERR=803,IOSTAT=IERR) TAUBBL(1:NSEA,2)
            ENDIF
            !
#ifdef W3_MPI
            CALL W3SETA ( IGRD, NDSE, NDST )
#endif
          ENDIF
#ifdef W3_T
          WRITE (NDST,9007)
        ELSE
          DO ISEA=1, NSEA
            WLV(ISEA) = 0.
            ICE(ISEA) = 0.
          END DO
          WRITE (NDST,9008)
#endif
        END IF
      END IF
    ELSE
      IF (TYPE.EQ.'FULL') THEN
        RPOS = 1_8 + LRECL*(NREC-1_8)
        READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)                &
             TLEV, TICE, TRHO, TIC1, TIC5
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
#ifdef W3_WRST
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
#endif
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
          !AR: not needed since already initialized on w3iogr                CALL SET_UG_IOBP
#ifdef W3_REF1
        ELSE
          CALL W3SETREF
#endif
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
        IF (OARST) THEN
          IF ( FLOGOA(1,2) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) CX(1:NSEA)
            READ (NDSR,ERR=802,IOSTAT=IERR) CY(1:NSEA)
          ENDIF
          IF ( FLOGOA(1,12) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) ICEF(1:NSEA)
          ENDIF
          IF ( FLOGOA(2,1) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) HS(I) = TMP(J)
            ENDDO
          ENDIF
          IF ( FLOGOA(2,2) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) WLM(I) = TMP(J)
            ENDDO
          ENDIF
          IF ( FLOGOA(2,4) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) T0M1(I) = TMP(J)
            ENDDO
          ENDIF
          IF ( FLOGOA(2,5) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) T01(I) = TMP(J)
            ENDDO
          ENDIF
          IF ( FLOGOA(2,6) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) FP0(I) = TMP(J)
            ENDDO
          ENDIF
          IF ( FLOGOA(2,7) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) THM(I) = TMP(J)
            ENDDO
          ENDIF
          IF ( FLOGOA(2,19) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) WNMEAN(I) = TMP(J)
            ENDDO
          ENDIF
          IF ( FLOGOA(5,2) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) CHARN(I) = TMP(J)
            ENDDO
          ENDIF
          IF ( FLOGOA(5,5) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) THEN
                TAUWIX(I) = TMP(J)
                TAUWIY(I) = TMP2(J)
              ENDIF
            ENDDO
          ENDIF
          IF ( FLOGOA(5,11) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) TWS(I) = TMP(J)
            ENDDO
          ENDIF
          IF ( FLOGOA(6,2) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) THEN
                TAUOX(I) = TMP(J)
                TAUOY(I) = TMP2(J)
              ENDIF
            ENDDO
          ENDIF
          IF ( FLOGOA(6,3) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) BHD(I) = TMP(J)
            ENDDO
          ENDIF
          IF ( FLOGOA(6,4) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) PHIOC(I) = TMP(J)
            ENDDO
          ENDIF
          IF ( FLOGOA(6,5) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) THEN
                TUSX(I) = TMP(J)
                TUSY(I) = TMP2(J)
              ENDIF
            ENDDO
          ENDIF
          IF ( FLOGOA(6,6) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) THEN
                USSX(I) = TMP(J)
                USSY(I) = TMP2(J)
              ENDIF
            ENDDO
          ENDIF
          IF ( FLOGOA(6,10) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) THEN
                TAUICE(I,1) = TMP(J)
                TAUICE(I,2) = TMP2(J)
              ENDIF
            ENDDO
          ENDIF
          IF ( FLOGOA(6,13) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) THEN
                TAUOCX(I) = TMP(J)
                TAUOCY(I) = TMP2(J)
              ENDIF
            ENDDO
          ENDIF
          IF ( FLOGOA(7,2) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) THEN
                UBA(I) = TMP(J)
                UBD(I) = TMP2(J)
              ENDIF
            ENDDO
          ENDIF
          IF ( FLOGOA(7,4) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) PHIBBL(I) = TMP(J)
            ENDDO
          ENDIF
          IF ( FLOGOA(7,5) ) THEN
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP(1:NSEA)
            READ (NDSR,ERR=802,IOSTAT=IERR) TMP2(1:NSEA)
            DO I=1, NSEALM
              J = IAPROC + (I-1)*NAPROC
              IF (J .LE. NSEA) THEN
                TAUBBL(I,1) = TMP(J)
                TAUBBL(I,2) = TMP2(J)
              ENDIF
            ENDDO
          ENDIF
        ENDIF
#ifdef W3_T
        WRITE (NDST,9007)
#endif
      ELSE
        TLEV(1) = -1
        TLEV(2) =  0
        TICE(1) = -1
        TICE(2) =  0
        TRHO(1) = -1
        TIC1(1) = -1
        TIC1(2) =  0
        TIC5(1) = -1
        TIC5(2) =  0
#ifdef W3_WRST
        WXNwrst =  0.
        WYNwrst =  0.
#endif
        WLV     =  0.
        ICE     =  0.
        ASF     =  1.
        FPIS    =  DUMFPI

        ! Initialize coupled fields if no restart is present
        IF (OARST) THEN
          CX      = 0.
          CY      = 0.
          ICEF    = 0.
          HS      = 0.
          WLM     = 0.
          T0M1    = 0.
          T01     = 0.
          FP0     = 1.
          THM     = 0.
          WNMEAN  = 0.
          CHARN   = 0.0185
          TAUWIX  = 0.
          TAUWIY  = 0.
          TWS     = 0.
          TAUOX   = 0.
          TAUOY   = 0.
          BHD     = 0.
          PHIOC   = 0.
          TUSX    = 0.
          TUSY    = 0.
          USSX    = 0.
          USSY    = 0.
          TAUOCX  = 0.
          TAUOCY  = 0.
          TAUICE  = 0.
          UBA     = 0.
          UBD     = 0.
          PHIBBL  = 0.
          TAUBBL  = 0.
        ENDIF
#ifdef W3_T
        WRITE (NDST,9008)
#endif
      END IF
    END IF
    !
    ! Close file --------------------------------------------------------- *
    !
    IF (WRITE) THEN
      IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST ) THEN
        CLOSE ( NDSR )
      END IF
    ELSE
      CLOSE ( NDSR )
    END IF
    !
    IF (ALLOCATED(WRITEBUFF)) DEALLOCATE(WRITEBUFF)
    IF (ALLOCATED(TMP))  DEALLOCATE(TMP)
    IF (ALLOCATED(TMP2)) DEALLOCATE(TMP2)
    !
    RETURN
    !
    ! Escape locations read errors :
    !
800 CONTINUE
#ifdef W3_LN0
    TYPE   = 'WIND'
    RSTYPE = 1
#endif
#ifdef W3_SEED
    TYPE   = 'CALM'
    RSTYPE = 4
#endif
#ifdef W3_LN1
    TYPE   = 'CALM'
    RSTYPE = 4
#endif
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
1000 FORMAT (/' *** WAVEWATCH III WARNING IN W3IORS : '/             &
         '     REQUESTED EXTRA RESTART GROUP',I2,' FIELD',I2, / &
         '     IS NOT PRESENT IN THE RESTART FILE.'/            &
         '     THIS MAY CAUSE INSTABILITIES IN COUPLED CONFIGURATIONS')
    !
    !
#ifdef W3_T
9000 FORMAT (' TEST W3IORS : TEST PARAMETERS :'/                  &
         '      INXOUT : ',A,/                                &
         '       WRITE : ',L10/                               &
         '      NTPROC : ',I10/                               &
         '      NAPROC : ',I10/                               &
         '      IAPROC : ',I10/                               &
         '      NAPRST : ',I10)
9001 FORMAT ('      FNAME  : ',A/                                 &
         '       LRECL : ',I10)
9002 FORMAT ('       IDSTR : ',A/                                 &
         '      VERINI : ',A/                                 &
         '       GNAME : ',A/                                 &
         '        TYPE : ',A/                                 &
         '        NSEA : ',I10/                               &
         '       NSEAL : ',I10/                               &
         '       NSPEC : ',I10)
9003 FORMAT (' TEST W3IORS :',I10.8,I8.6,' UTC')
9004 FORMAT (' TEST W3IORS : TIME NOT AVAILABLE ')
9005 FORMAT (' TEST W3IORS : NO SPECTRA, TYPE=''',A,''' ')
9006 FORMAT (' TEST W3IORS : SPECTRA PROCESSED ')
9007 FORMAT (' TEST W3IORS : WATER LEVELS ETC. PROCESSED ')
9008 FORMAT (' TEST W3IORS : WATER LEVELS ETC. PROCESSED (DUMMY)')
    !
9020 FORMAT (' TEST W3IORS : RSTYPE = ',A,', PERFORMED BY W3INIT')
#endif
    !/
    !/ End of W3IORS ----------------------------------------------------- /
    !/
  END SUBROUTINE W3IORS
  !/
  !/ End of module W3IORSMD -------------------------------------------- /
  !/
END MODULE W3IORSMD
