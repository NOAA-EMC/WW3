!> @file
!> @brief Gridded output of mean wave parameters.
!>
!> @author H. L. Tolman  @date 22-Mar-2021
!>

#include "w3macros.h"

!>
!> @brief Gridded output of mean wave parameters.
!>
!> @author H. L. Tolman  @date 22-Mar-2021
!>
!/ ------------------------------------------------------------------- /
MODULE W3IOGOMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         02-Mar-2024 |
  !/                  +-----------------------------------+
  !/
  !/    04-Jan-2001 : Origination.                        ( version 2.00 )
  !/    23-Apr-2002 : Clean up.                           ( version 2.19 )
  !/    29-Apr-2002 : Add output parameters 17-18.        ( version 2.20 )
  !/    30-May-2002 : Switch clean up.                    ( version 2.21 )
  !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
  !/    25-Oct-2004 : Multiple grid version.              ( version 3.06 )
  !/    27-Jun-2005 : Adding MAPST2.                      ( version 3.07 )
  !/    21-Jul-2005 : Adding output fields 19-21.         ( version 3.07 )
  !/    23-Apr-2006 : Filter for directional spread.      ( version 3.09 )
  !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
  !/    05-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
  !/    02-Apr-2007 : Adding partitioned output.          ( version 3.11 )
  !/                  Adding user slots for outputs.
  !/    08-Oct-2007 : Adding ST3 source term option.      ( version 3.13 )
  !/                  ( F. Ardhuin )
  !/    05-Mar-2008 : Added NEC sxf90 compiler directives
  !/                  (Chris Bunney, UK Met Office)       ( version 3.13 )
  !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  !/    13-Sep-2009 : Add coupling option                 ( version 3.14 )
  !/    10-Mar-2009 : Add second order pressure           ( version 3.14 )
  !/    15-Sep-2010 : Adding ST4 source term option.      ( version 3.14 )
  !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
  !/                  (W. E. Rogers & T. J. Campbell, NRL)
  !/    05-Feb-2011 : Implement unstructured grid        ( version 3.14.3 )
  !/                  (A. Roland and F. Ardhuin)
  !/    12-Jun-2012 : Add /RTD option or rotated grid option.
  !/                  (Jian-Guo Li)                       ( version 4.06 )
  !/    25-Dec-2012 : New output structure and smaller    ( version 4.11 )
  !/                  memory footprint.
  !/    15-Apr-2013 : New subroutine to read param. names ( version 4.11 )
  !/    21-Aug-2013 : Bug correction in W3IOGO: UBR, ABR  ( version 4.11 )
  !/    11-Nov-2013 : SMC and rotated grid incorporated in the main
  !/                  trunk                               ( version 4.13 )
  !/    31-Jan-2014 : Bug fix warning output (Tolman).    ( version 4.18 )
  !/    10-Feb-2014 : Bug correction for US3D: div. by df ( version 4.18 )
  !/    30-Apr-2014 : Add th2m and sth2m calculation      ( version 5.01 )
  !/    27-May-2014 : Switch to OMPG switch.              ( version 5.02 )
  !/    27-Aug-2015 : Add ICEF,ICEH as output fields      ( version 5.10 )
  !/    01-Mar-2018 : Removed RTD code (now used in post  ( version 6.02 )
  !/                  processing code)
  !/    05-Jun-2018 : Add DEBUGSTP/SETUP                  ( version 6.04 )
  !/    22-Aug-2018 : Add WBT output parameter            ( version 6.06 )
  !/    25-Sep-2019 : Corrected th2m and sth2m            ( version 6.07 )
  !/                  calculations. (J Dykes, NRL)
  !/    04-Oct-2019 : Optional one file per output stride ( version 7.00 )
  !/                  (Roberto Padilla-Hernandez & J.H. Alves)
  !/    03-Nov-2020 : Factored out NAME matching into     ( version 7.12 )
  !/                  seperate subroutine. (C. Bunney)
  !/    15-Jan-2021 : Added TP output based on exsiting   ( version 7.12 )
  !/                  FP internal field. (C. Bunney)
  !/    22-Mar-2021 : Add extra coupling fields as output ( version 7.13 )
  !/    21-Jul-2022 : Correct FP0 calc for peak energy in ( version 7.14 )
  !/                  min/max freq band (B. Pouliot, CMC)
  !/    02-Mar-2024 : Add skweness and EM bias varaible   ( version 7.xx )
  !/
  !/    Copyright 2009-2024 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Gridded output of mean wave parameters.
  !
  !  2. Variables and types :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      VEROGR    C*10  Private  Gridded output file version number.
  !      IDSTR     C*30  Private  Gridded output file ID string.
  !     ----------------------------------------------------------------
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      W3OUTG    Subr. Public   Calculate mean parameters.
  !      W3IOGO    Subr. Public   IO to raw gridded fields file.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      W3SETO    Subr. W3ODATMD Point to data structure.
  !      W3SETG    Subr. W3GDATMD Point to data structure.
  !      W3SETW    Subr. W3WDATMD Point to data structure.
  !      W3SETA    Subr. W3ADATMD Point to data structure.
  !      W3XETA    Subr. W3ADATMD Point to data structure.
  !      W3DIMW    Subr. W3WDATMD Allocate data structure.
  !      W3DIMA    Subr. W3ADATMD Allocate data structure.
  !      STRACE    Subr. W3SERVMD Subroutine tracing.           ( !/S )
  !      EXTCDE    Subr. W3SERVMD Program abort with exit code.
  !     ----------------------------------------------------------------
  !
  !  5. Remarks :
  !
  !     - The different output fields are not folded in with this module
  !       due to the different requirements for a element '0' in some of
  !       the fields.
  !
  !  6. Switches :
  !
  !       !/SHRD  Switch for shared / distributed memory architecture.
  !       !/DIST  Id.
  !
  !       !/OMPG  OpenMP compiler directive for loop splitting.
  !
  !       !/O8    Filter for low wave heights ( HSMIN )
  !       !/O9    Negative wave height alowed, other mean parameters will
  !             not be correct.
  !
  !       !/ST0   No source terms.
  !       !/ST1   Source term set 1 (WAM equiv.)
  !       !/ST2   Source term set 2 (Tolman and Chalikov)
  !       !/ST3   Source term set 3 (WAM 4+)
  !       !/ST4   Source term set 4 (Ardhuin et al. 2009, 2010)
  !       !/ST6   Source term set 6 (BYDRZ)
  !
  !       !/S     Enable subroutine tracing.
  !       !/T     Test output.
  !
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
#ifdef W3_S
  USE W3SERVMD, ONLY : STRACE
#endif
  !/
  PUBLIC
  CHARACTER(LEN=1024)                   :: FLDOUT
  !/
  !/ Private parameter statements (ID strings)
  !/
  CHARACTER(LEN=10), PARAMETER, PRIVATE :: VEROGR = '2019-10-04'
  CHARACTER(LEN=30), PARAMETER, PRIVATE ::                        &
       IDSTR = 'WAVEWATCH III GRID OUTPUT FILE'
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Updates the flags for output parameters based on the mod_def file
  !>  this is to prevent the allocation of big 3D arrays when not requested.
  !>
  !> @param[in]    NDSO   Output file logical unit number.
  !> @param[in]    NDSEN  Error output file logical unit number.
  !> @param[inout] FLGRD  1D array of flags for groups.
  !> @param[inout] FLGR2  1D array of flags for groups.
  !> @param[inout] FLGD   2D array of flags.
  !> @param[inout] FLG2   2D array of flags.
  !>
  !> @author F. Ardhuin  @date 15-Apr-2013
  !>
  SUBROUTINE W3FLGRDUPDT ( NDSO, NDSEN, FLGRD, FLGR2, FLGD, FLG2 )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           F. Ardhuin              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         15-Apr-2013 |
    !/                  +-----------------------------------+
    !/
    !/    15-Apr-2013 : Origination.                        ( version 4.10 )
    !/
    !  1. Purpose :
    !
    !     Updates the flags for output parameters based on the mod_def file
    !     this is to prevent the allocation of big 3D arrays when not requested
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NDSO      Int.   I   Output file logical unit number
    !       NDSEN     R.A.   I   Error output file logical unit number
    !       FLGD,FLG2 L.A.   O   1D array of flags for groups
    !       FLGRD     L.A.   O   2D array of flags
    !       FLGR2     L.A.   O   2D array of flags
    !     ----------------------------------------------------------------
    !
    !
    !  4. Subroutines used :
    !
    !     None
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3INIT    Subr.   N/A
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     None.
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
    USE CONSTANTS
    USE W3GDATMD, ONLY: E3DF, P2MSF, US3DF, USSPF
    USE W3ODATMD, ONLY: NOGRP, NGRPP
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: NDSO, NDSEN
    LOGICAL, INTENT(INOUT)  :: FLGRD(NOGRP,NGRPP), FLGD(NOGRP),     &
         FLGR2(NOGRP,NGRPP), FLG2(NOGRP)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER             :: I
    CHARACTER(LEN=10)  :: VARNAME1(5),VARNAME2(5)
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3FLGRDUPDT')
#endif
    !
    VARNAME1(1) = 'EF';    VARNAME2(1) = 'E3D'
    VARNAME1(2) = 'TH1M';  VARNAME2(2) = 'TH1MF'
    VARNAME1(3) = 'STH1M'; VARNAME2(3) = 'STH1MF'
    VARNAME1(4) = 'TH2M';  VARNAME2(4) = 'TH2MF'
    VARNAME1(5) = 'STH2M'; VARNAME2(5) = 'STH2MF'

    DO I=1,5
      IF (E3DF(1,I).LE.0.OR.E3DF(3,I).LT.E3DF(2,I)) THEN
        IF (FLGRD(3,I).OR.FLGR2(3,I)) THEN
          WRITE(NDSEN,1008) VARNAME1(I),VARNAME2(I)
        END IF
        FLGRD(3,I)=.FALSE.
        FLGR2(3,I)=.FALSE.
      END IF
    END DO
    IF (US3DF(1).LE.0.OR.US3DF(3).LT.US3DF(2)) THEN
      IF (FLGRD(6,8).OR.FLGR2(6,8)) THEN
        WRITE(NDSEN,1008) 'USF','US3D'
      END IF
      FLGRD(6,8)=.FALSE.
      FLGR2(6,8)=.FALSE.
    END IF
    IF (USSPF(1).LE.0.OR.USSPF(2).LE.0) THEN
      IF (FLGRD(6,12).OR.FLGR2(6,12)) THEN
        WRITE(NDSEN,1008) 'USP','USSP'
      END IF
      FLGRD(6,12)=.FALSE.
      FLGR2(6,12)=.FALSE.
    END IF
    IF (P2MSF(1).LE.0.OR.P2MSF(3).LT.P2MSF(2)) THEN
      IF (FLGRD(6,9).OR.FLGR2(6,9)) THEN
        WRITE(NDSEN,1008) 'P2L','P2SF'
      END IF
      FLGRD(6,9)=.FALSE.
      FLGR2(6,9)=.FALSE.
    END IF
    !
    FLGD(3) = .FALSE.
    FLG2(3) = .FALSE.
    IF(ANY(FLGRD(3,:))) FLGD(3)=.TRUE.
    IF(ANY(FLGR2(3,:))) FLG2(3)=.TRUE.
    FLGD(6) = .FALSE.
    FLG2(6) = .FALSE.
    IF(ANY(FLGRD(6,:))) FLGD(6)=.TRUE.
    IF(ANY(FLGR2(6,:))) FLG2(6)=.TRUE.
    !
    RETURN
    !
1008 FORMAT (/' *** WAVEWATCH III WARNING  : '/                       &
         '     PARAMETER ',A,' not allowed: need to set',        &
         ' parameter ',A,' in OUTS namelist (in ww3_grid.inp)'   &
         ' with proper bounds' )
    !
  END SUBROUTINE W3FLGRDUPDT
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Fills in FLG1D and FLG2D arrays from ASCII input file.
  !>
  !> @param[in] NDSI    Input file logical unit number.
  !> @param[in] NDSO    Output file logical unit number.
  !> @param[in] NDSS    Screen file logical unit number.
  !> @param[in] NDSEN   Error output file logical unit number.
  !> @param[in] COMSTR  Comment string, usually '$'.
  !> @param[out] FLG1D  1D array of flags for groups.
  !> @param[out] FLG2D  2D array of flags.
  !> @param[in] IAPROC  Index of current processor.
  !> @param[in] NAPOUT  Index of processor for output (screen).
  !> @param[out] IERR   Error message number.
  !>
  !> @author F. Ardhuin  @date 25-Sep-2020
  !>
  SUBROUTINE W3READFLGRD ( NDSI , NDSO, NDSS, NDSEN, COMSTR,      &
       FLG1D, FLG2D, IAPROC, NAPOUT, IERR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           F. Ardhuin              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         25-Sep-2020 |
    !/                  +-----------------------------------+
    !/
    !/    15-Apr-2013 : Origination.                        ( version 4.10 )
    !/    31-Jan-2014 : Bug fix warning output (Tolman).    ( version 4.18 )
    !/    30-Apr-2014 : Add th2m and sth2m calculation      ( version 5.01 )
    !/    25-Sep-2020 : Calculate FLG1D for any processor   ( version 7.10 )
    !/    03-Nov-2020 : Factored out NAME matching into     ( version 7.12 )
    !/                  seperate subroutine (C. Bunney)
    !/
    !  1. Purpose :
    !
    !     Fills in FLG1D and FLG2D arrays from ASCII input file
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NDSI    Int.   I   Input file logical unit number
    !       NDSO    Int.   I   Output file logical unit number
    !       NDSS    Int.   I   Screen file logical unit number
    !       NDSEN   R.A.   I   Error output file logical unit number
    !       COMSTR  Char   I   Comment string, usually '$'
    !       FLG1D   L.A.   O   1D array of flags for groups
    !       FLG2D   L.A.   O   2D array of flags
    !       IAPROC  Int.   I   index of current processor
    !       NAPOUT  Int.   I   index of processor for output (screen)
    !       IERR    Int.   O   Error message number
    !     ----------------------------------------------------------------
    !
    !
    !  4. Subroutines used :
    !
    !     None
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WW3_SHEL  Prog.   N/A    Actual wave model program
    !      WW3_OUTF  Prog.   N/A    Output postprocessor.
    !      WW3_OUNF  Prog.   N/A    NetCDF output postprocessor.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     None.
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
    USE CONSTANTS
    USE W3GDATMD, ONLY: US3DF, USSPF
    USE W3ODATMD, ONLY: NOGRP, NGRPP, NOGE, IDOUT
    USE W3SERVMD, ONLY: NEXTLN, STRSPLIT, STR_TO_UPPER
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: NDSI, NDSO, NDSS, NDSEN, IAPROC, NAPOUT
    INTEGER, INTENT(OUT)    :: IERR
    CHARACTER(LEN=1)        :: COMSTR
    LOGICAL, INTENT(OUT)    :: FLG2D(NOGRP,NGRPP), FLG1D(NOGRP)
    CHARACTER(LEN=100)      :: OUT_NAMES(100), TESTSTR
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER             :: IFI, IFJ, IOUT
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    CHARACTER(LEN=1)    :: AFLG
    LOGICAL             :: FLT, NAMES
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3READFLGRD')
#endif
    !
    !
    ! 1.  Initialize flags -------------------------------------- *
    !
    IERR=0
    FLG2D(:,:)=.FALSE. ! Initialize FLG2D
    FLG1D(:)=.FALSE. ! Initialize FLOG
    NAMES =.FALSE.
    !
    DO IFI=1,NOGRP ! Loop over field output groups
      !
      CALL NEXTLN ( COMSTR , NDSI , NDSEN )
      READ (NDSI,*,END=2001,ERR=2002) AFLG
      IF (AFLG.EQ.'T') THEN
        FLG1D(IFI)=.TRUE.
      ELSE IF (AFLG.EQ.'F') THEN
        FLG1D(IFI)=.FALSE.
      ELSE IF (AFLG.EQ.'N') THEN
        NAMES=.TRUE.
        EXIT
      ELSE
        IERR=1
        GOTO 2005
      END IF
      IF ( FLG1D (IFI) ) THEN ! Skip if group not requested
        CALL NEXTLN ( COMSTR , NDSI , NDSEN )
        READ (NDSI,'(A)',END=2001,ERR=2006,IOSTAT=IERR)              &
             FLDOUT
        OUT_NAMES(:)=''
        CALL STRSPLIT(FLDOUT,OUT_NAMES)
        IFJ=0
        DO WHILE (len_trim(OUT_NAMES(IFJ+1)).NE.0)
          IFJ=IFJ+1
          IF ( OUT_NAMES(IFJ) .EQ. 'T' )                            &
               FLG2D(IFI,IFJ)=.TRUE.
        ENDDO
        IF ( IAPROC .EQ. NAPOUT .AND. IFJ .LT. NOGE(IFI) ) WRITE(NDSEN,1007) IFI
      ENDIF
    END DO
    !
    IF (NAMES) THEN
      !
      ! 2. Reads and splits list of output field names
      !
      CALL NEXTLN ( COMSTR , NDSI , NDSEN )
      READ (NDSI,'(A)',END=2001,ERR=2003,IOSTAT=IERR) FLDOUT
      OUT_NAMES(:)=''
      CALL STRSPLIT(FLDOUT,OUT_NAMES)
      IOUT=0
      DO WHILE (len_trim(OUT_NAMES(IOUT+1)).NE.0)
        CALL STR_TO_UPPER(OUT_NAMES(IOUT+1))
        !
        ! 2. Matches names with expected ...
        !
        TESTSTR=OUT_NAMES(IOUT+1)
        CALL W3FLDTOIJ(TESTSTR, IFI, IFJ, IAPROC, NAPOUT, NDSEN)

        IF(IFI .NE. -1) THEN
          FLG2D(IFI, IFJ) = .TRUE.
        ENDIF
        !
        IOUT=IOUT+1
        !
      END DO
      !
    END IF
    !
    FLT    = .TRUE.
    DO IFI=1, NOGRP
      IF ( IAPROC .EQ. NAPOUT ) THEN
        DO IFJ=1, NGRPP
          IF ( FLG2D(IFI,IFJ) ) THEN
            IF ( FLT ) THEN
              WRITE (NDSO,1945) IDOUT(IFI,IFJ)
              FLT    = .FALSE.
            ELSE
              WRITE (NDSO,1946) IDOUT(IFI,IFJ)
            END IF
          END IF
        END DO
      END IF
      IF(ANY(FLG2D(IFI,:))) FLG1D(IFI)=.TRUE. !Update FLG1D
    END DO
    IF ( IAPROC .EQ. NAPOUT ) THEN
      IF ( FLT ) WRITE (NDSO,1945) 'no fields defined'
    END IF
    !
    RETURN
    !
2001 CONTINUE
    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSEN,1001)
    RETURN
2002 CONTINUE
    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSEN, 1002) IFI, IERR
    RETURN
2003 CONTINUE
    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSEN, 1003) IERR
    RETURN
    !2004 CONTINUE ! replaced by warning in code ....
2005 CONTINUE
    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSEN, 1005) AFLG
    RETURN
2006 CONTINUE
    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSEN, 1006) IFI,IERR
    RETURN
    !
1945 FORMAT ( '            Fields   : ',A)
1946 FORMAT ( '                       ',A)
    !
1001 FORMAT (/' *** WAVEWATCH III ERROR  : '/                         &
         '     PREMATURE END OF INPUT FILE'/)
    !
1002 FORMAT (/' *** WAVEWATCH III ERROR  : '/                         &
         '     ERROR IN READING OUTPUT FIELDS GROUP FLAGS ',     &
         I2, /, '     IOSTAT =',I5/)
    !
1003 FORMAT (/' *** WAVEWATCH III ERROR  : '/                         &
         '     ERROR READING OUTPUT FIELD NAMES FROM INPUT FILE'/&
         '     IOSTAT =',I5/)
    !
1005 FORMAT (/' *** WAVEWATCH III ERROR    : '/                       &
         '     WAS EXPECTING "T" "F" or "N", but found "',A,'".'/)
    !
1006 FORMAT (/' *** WAVEWATCH III ERROR  : '/                         &
         '     ERROR IN READING OUTPUT FIELDS FLAGS FOR GROUP ', &
         I2, /, '     IOSTAT =',I5/)
    !
1007 FORMAT (/' *** WAVEWATCH III WARNING  : '/                       &
         '     NUMBER OF REQUESTED OUTPUT FIELD FLAGS IN GROUP ',&
         I2, /,' LESS THAN AVAILABLE, CHECK DOCS FOR MORE OPTIONS')
    !
  END SUBROUTINE W3READFLGRD

  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Fills in FLG1D and FLG2D arrays from ASCII input file.
  !>
  !> @param[in] NDSO    Output file logical unit number.
  !> @param[in] NDSS    Screen file logical unit number.
  !> @param[in] NDSEN   Error output file logical unit number.
  !> @param[in] FLDOUT  List of field names.
  !> @param[out] FLG1D  1D array of flags for groups.
  !> @param[out] FLG2D  2D array of flags.
  !> @param[in] IAPROC  Index of current processor.
  !> @param[in] NAPOUT  Index of processor for output (screen).
  !> @param[out] IERR   Error message number.
  !>
  !> @author F. Ardhuin  @date 25-Sep-2020
  !>
  SUBROUTINE W3FLGRDFLAG ( NDSO, NDSS, NDSEN, FLDOUT,      &
       FLG1D, FLG2D, IAPROC, NAPOUT, IERR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           F. Ardhuin              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         25-Sep-2020 |
    !/                  +-----------------------------------+
    !/
    !/    15-Apr-2013 : Origination.                        ( version 4.10 )
    !/    31-Jan-2014 : Bug fix warning output (Tolman).    ( version 4.18 )
    !/    30-Apr-2014 : Add th2m and sth2m calculation      ( version 5.01 )
    !/    17-Feb-2016 : New version for namelist use        ( version 5.11 )
    !/    25-Sep-2020 : Calculate FLG1D for any processor   ( version 7.10 )
    !/    03-Nov-2020 : Factored out NAME matching into     ( version 7.12 )
    !/                  seperate subroutine (C. Bunney)
    !/
    !  1. Purpose :
    !
    !     Fills in FLG1D and FLG2D arrays from ASCII input file
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NDSO    Int.   I   Output file logical unit number
    !       NDSS    Int.   I   Screen file logical unit number
    !       NDSEN   R.A.   I   Error output file logical unit number
    !       FLDOUT  Cha.   I   List of field names
    !       FLG1D   L.A.   O   1D array of flags for groups
    !       FLG2D   L.A.   O   2D array of flags
    !       IAPROC  Int.   I   index of current processor
    !       NAPOUT  Int.   I   index of processor for output (screen)
    !       IERR    Int.   O   Error message number
    !     ----------------------------------------------------------------
    !
    !
    !  4. Subroutines used :
    !
    !     None
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WW3_SHEL  Prog.   N/A    Actual wave model program
    !      WW3_OUTF  Prog.   N/A    Output postprocessor.
    !      WW3_OUNF  Prog.   N/A    NetCDF output postprocessor.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     None.
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
    USE CONSTANTS
    USE W3ODATMD, ONLY: NOGRP, NGRPP, IDOUT
    USE W3SERVMD, ONLY: STRSPLIT, STR_TO_UPPER
    USE W3GDATMD, ONLY: US3DF, USSPF
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: NDSO, NDSS, NDSEN, IAPROC, NAPOUT
    CHARACTER(1024), INTENT(IN)   :: FLDOUT
    INTEGER, INTENT(OUT)    :: IERR
    LOGICAL, INTENT(OUT)    :: FLG2D(NOGRP,NGRPP), FLG1D(NOGRP)
    CHARACTER(LEN=100)      :: OUT_NAMES(100), TESTSTR
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER             :: I, IFI, IFJ, IOUT
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    LOGICAL             :: FLT
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3FLGRDFLAG')
#endif
    !
    !
    ! 1.  Initialize flags -------------------------------------- *
    !
    IERR=0
    FLG2D(:,:)=.FALSE. ! Initialize FLG2D
    FLG1D(:)=.FALSE. ! Initialize FLOG
    !
    ! 2. Splits list of output field names
    !
    OUT_NAMES(:)=''
    CALL STRSPLIT(FLDOUT,OUT_NAMES)
    IOUT=0
    DO WHILE (len_trim(OUT_NAMES(IOUT+1)).NE.0)
      CALL STR_TO_UPPER(OUT_NAMES(IOUT+1))
      !
      ! 2. Matches names with expected ...
      !
      TESTSTR=OUT_NAMES(IOUT+1)
      CALL W3FLDTOIJ(TESTSTR, IFI, IFJ, IAPROC, NAPOUT, NDSEN)

      IF(IFI .NE. -1) THEN
        FLG2D(IFI, IFJ) = .TRUE.
      ENDIF
      !
      IOUT=IOUT+1
      !
    END DO
    !
    FLT    = .TRUE.
    DO IFI=1, NOGRP
      IF ( IAPROC .EQ. NAPOUT ) THEN
        DO IFJ=1, NGRPP
          IF ( FLG2D(IFI,IFJ) ) THEN
            IF ( FLT ) THEN
              WRITE (NDSO,1945) IDOUT(IFI,IFJ)
              FLT    = .FALSE.
            ELSE
              WRITE (NDSO,1946) IDOUT(IFI,IFJ)
            END IF
          END IF
        END DO
      ENDIF
      IF(ANY(FLG2D(IFI,:))) FLG1D(IFI)=.TRUE. !Update FLG1D
    END DO
    IF ( IAPROC .EQ. NAPOUT ) THEN
      IF ( FLT ) WRITE (NDSO,1945) 'no fields defined'
    ENDIF
    !
    RETURN
    !
1945 FORMAT ( '            Fields   : ',A)
1946 FORMAT ( '                       ',A)
    !
    ! 1004 FORMAT (/' *** WAVEWATCH III WARNING  : '/                       &
    !               '     REQUESTED OUTPUT FIELD ',A,' WAS NOT RECOGNIZED.'/)
    !!
    ! 1008 FORMAT (/' *** WAVEWATCH III WARNING  : '/                       &
    !               '     PARAMETER ',A,' not allowed: need to set',        &
    !               ' parameter ',A,' in OUTS namelist (in ww3_grid.inp)')
    !
  END SUBROUTINE W3FLGRDFLAG

  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Returns the group/field (I/J) indices for a named output field.
  !>
  !> @param[in]  FLD     Field names.
  !> @param[out] I       Output group number (IFI).
  !> @param[out] J       Output field number (IFJ).
  !> @param[in]  IAPROC  Index of current processor.
  !> @param[in]  NAPOUT  Index of processor for output (screen).
  !> @param[in]  NDSEN   Error output file logical unit number.
  !>
  !> @author C. Bunney  @date 22-Mar-2021
  !>
  SUBROUTINE W3FLDTOIJ(FLD, I, J, IAPROC, NAPOUT, NDSEN)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |            C. Bunney              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    03-Nov-2020 : Origination.                        ( version 7.12 )
    !/    22-Mar-2021 : Add extra coupling fields as output ( version 7.13 )
    !
    !  1. Purpose :
    !
    !     Returns the group/field (I/J) indices for a named output field.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       FLD     Cha.   I   Field names
    !       I       Int.   O   Output group number (IFI)
    !       J       Int.   O   Output field number (IFJ)
    !       IAPROC  Int.   I   index of current processor
    !       NAPOUT  Int.   I   index of processor for output (screen)
    !       NDSEN   R.A.   I   Error output file logical unit number
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD, ONLY: US3DF, USSPF
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    CHARACTER(LEN=*), INTENT(IN) :: FLD
    INTEGER, INTENT(IN) :: IAPROC, NAPOUT, NDSEN
    INTEGER, INTENT(OUT) :: I, J

    I = -1
    J = -1

    SELECT CASE(TRIM(FLD(1:6)))
      !
      ! Group 1
      !
    CASE('DPT')
      I = 1
      J = 1
    CASE('CUR')
      I = 1
      J = 2
    CASE('WND')
      I = 1
      J = 3
    CASE('AST')
      I = 1
      J = 4
    CASE('WLV')
      I = 1
      J = 5
    CASE('ICE')
      I = 1
      J = 6
    CASE('IBG')
      I = 1
      J = 7
    CASE('TAU')
      I = 1
      J = 8
    CASE('RHO')
      I = 1
      J = 9
#ifdef W3_BT4
    CASE('D50')
      I = 1
      J = 10
#endif
#ifdef W3_IS2
    CASE('IC1')
      I = 1
      J = 11
    CASE('IC5')
      I = 1
      J = 12
#endif
      ! Group 2
      !
#ifdef W3_OASACM
    CASE('AHS')
      I = 2
      J = 1
#endif
#ifdef W3_OASOCM
    CASE('OHS')
      I = 2
      J = 1
#endif
    CASE('HS')
      I = 2
      J = 1
    CASE('LM')
      I = 2
      J = 2
    CASE('T02')
      I = 2
      J = 3
    CASE('T0M1')
      I = 2
      J = 4
    CASE('T01')
      I = 2
      J = 5
    CASE('FP')
      I = 2
      J = 6
    CASE('DIR')
      I = 2
      J = 7
    CASE('SPR')
      I = 2
      J = 8
    CASE('DP')
      I = 2
      J = 9
    CASE('HIG')
      I = 2
      J = 10
    CASE('MXE')
      I = 2
      J = 11
    CASE('MXES')
      I = 2
      J = 12
    CASE('MXH')
      I = 2
      J = 13
    CASE('MXHC')
      I = 2
      J = 14
    CASE('SDMH')
      I = 2
      J = 15
    CASE('SDMHC')
      I = 2
      J = 16
    CASE('WBT')
      I = 2
      J = 17
    CASE('TP') ! Uses FP0 internally, as per FP
      I = 2
      J = 18
    CASE('WNM')
      I = 2
      J = 19
#ifdef W3_OASOCM
    CASE('THM')
      I = 2
      J = 20
#endif
      !
      ! Group 3
      !
    CASE('EF')
      I = 3
      J = 1
    CASE('TH1M')
      I = 3
      J = 2
    CASE('STH1M')
      I = 3
      J = 3
    CASE('TH2M')
      I = 3
      J = 4
    CASE('STH2M')
      I = 3
      J = 5
    CASE('WN')
      I = 3
      J = 6
      !
      ! Group 4
      !
    CASE('PHS')
      I = 4
      J = 1
    CASE('PTP')
      I = 4
      J = 2
    CASE('PLP')
      I = 4
      J = 3
    CASE('PDIR')
      I = 4
      J = 4
    CASE('PSPR')
      I = 4
      J = 5
    CASE('PWS')
      I = 4
      J = 6
    CASE('PDP')
      I = 4
      J = 7
    CASE('PQP')
      I = 4
      J = 8
    CASE('PPE')
      I = 4
      J = 9
    CASE('PGW')
      I = 4
      J = 10
    CASE('PSW')
      I = 4
      J = 11
    CASE('PTM10')
      I = 4
      J = 12
    CASE('PT01')
      I = 4
      J = 13
    CASE('PT02')
      I = 4
      J = 14
    CASE('PEP')
      I = 4
      J = 15
    CASE('TWS')
      I = 4
      J = 16
    CASE('PNR')
      I = 4
      J = 17
      !
      ! Group 5
      !
    CASE('UST')
      I = 5
      J = 1
#ifdef W3_OASACM
    CASE('ACHA')
      I = 5
      J = 2
#endif
#ifdef W3_OASOCM
    CASE('OCHA')
      I = 5
      J = 2
#endif
    CASE('CHA')
      I = 5
      J = 2
    CASE('CGE')
      I = 5
      J = 3
    CASE('FAW')
      I = 5
      J = 4
    CASE('TAW')
      I = 5
      J = 5
    CASE('TWA')
      I = 5
      J = 6
    CASE('WCC')
      I = 5
      J = 7
    CASE('WCF')
      I = 5
      J = 8
    CASE('WCH')
      I = 5
      J = 9
    CASE('WCM')
      I = 5
      J = 10
    CASE('FWS')
      I = 5
      J = 11
      !
      ! Group 6
      !
    CASE('SXY')
      I = 6
      J = 1
    CASE('TWO')
      I = 6
      J = 2
    CASE('BHD')
      I = 6
      J = 3
    CASE('FOC')
      I = 6
      J = 4
    CASE('TUS')
      I = 6
      J = 5
    CASE('USS')
      I = 6
      J = 6
    CASE('P2S')
      I = 6
      J = 7
    CASE('USF')
      IF (US3DF(1).GE.1) THEN
        I = 6
        J = 8
      ELSE
        IF ( IAPROC .EQ. NAPOUT ) WRITE(NDSEN,1008) 'USF','US3D'
      END IF
    CASE('P2L')
      I = 6
      J = 9
    CASE('TWI')
      I = 6
      J = 10
    CASE('FIC')
      I = 6
      J = 11
    CASE('USP')
      IF (USSPF(1).GE.1) THEN
        I = 6
        J = 12
      ELSE
        IF ( IAPROC .EQ. NAPOUT ) WRITE(NDSEN,1008) 'USP','USSP'
      END IF
    CASE('TOC')
      I = 6
      J = 13
      !
      ! Group 7
      !
    CASE('ABR')
      I = 7
      J = 1
    CASE('UBR')
      I = 7
      J = 2
    CASE('BED')
      I = 7
      J = 3
    CASE('FBB')
      I = 7
      J = 4
    CASE('TBB')
      I = 7
      J = 5
      !
      ! Group 8
      !
    CASE('MSS')
      I = 8
      J = 1
    CASE('MSC')
      I = 8
      J = 2
    CASE('MSD')
      I = 8
      J = 3
    CASE('MCD')
      I = 8
      J = 4
    CASE('QP')
      I = 8
      J = 5
    CASE('QKK')
      I = 8
      J = 6
    CASE('SKW')
      I = 8
      J = 7
    CASE('EMB')
      I = 8
      J = 8
    CASE('EMC')
      I = 8
      J = 9
      !
      ! Group 9
      !
    CASE('DTD')
      I = 9
      J = 1
    CASE('FC')
      I = 9
      J = 2
    CASE('CFX')
      I = 9
      J = 3
    CASE('CFD')
      I = 9
      J = 4
    CASE('CFK')
      I = 9
      J = 5
      !
      ! Group 10
      !
    CASE('U1')
      I = 10
      J = 1
    CASE('U2')
      I = 10
      J = 1
      ! Not found:
#ifdef W3_COU
    CASE('DRY')
#endif
    CASE('UNSET')
    CASE DEFAULT
      I = -1
      J = -1
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSEN,1004) TRIM(FLD)
    END SELECT

1004 FORMAT (/' *** WAVEWATCH III WARNING  : '/                       &
         '     REQUESTED OUTPUT FIELD ',A,' WAS NOT RECOGNIZED.'/)
    !
1008 FORMAT (/' *** WAVEWATCH III WARNING  : '/                       &
         '     PARAMETER ',A,' not allowed: need to set',        &
         ' parameter ',A,' in OUTS namelist (in ww3_grid.inp)')
    !
  END SUBROUTINE W3FLDTOIJ

  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Fill necessary arrays with gridded data for output.
  !>
  !> @param[in] A        Input spectra, left in par list to changeshape.
  !> @param[in] FLPART   Flag for filling fields with partition data.
  !> @param[in] FLOUTG   Flag for file field output.
  !> @param[in] FLOUTG2  Flag for coupling field output.
  !>
  !> @author H. L. Tolman  @date 10-Apr-2015
  !>
  SUBROUTINE W3OUTG ( A, FLPART, FLOUTG, FLOUTG2 )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         10-Apr-2015 |
    !/                  +-----------------------------------+
    !/
    !/    10-Dec-1998 : Distributed FORTRAN 77 version.     ( version 1.18 )
    !/    04-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/                  Major changes to logistics.
    !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
    !/    19-Oct-2004 : Multiple grid version.              ( version 3.06 )
    !/    21-Jul-2005 : Adding output fields 19-21.         ( version 3.07 )
    !/    23-Apr-2006 : Filter for directional spread.      ( version 3.09 )
    !/    02-Apr-2007 : Adding partitioned output.          ( version 3.11 )
    !/                  Adding user slots for outputs.
    !/    08-Oct-2007 : Adding ST3 source term option.      ( version 3.13 )
    !/                  ( F. Ardhuin )
    !/    05-Mar-2008 : Added NEC sxf90 compiler directives
    !/                  (Chris Bunney, UK Met Office)       ( version 3.13 )
    !/    25-Dec-2012 : New output structure and smaller    ( version 4.11 )
    !/                  memory footprint.
    !/    10-Feb-2014 : Bug correction for US3D: div. by df ( version 4.18 )
    !/    30-Apr-2014 : Add th2m and sth2m calculation      ( version 5.01 )
    !/    27-May-2014 : Switch to OMPG switch.              ( version 5.02 )
    !/    10-Apr-2015 : Remove unused variables             ( version 5.08 )
    !/    10-Jan-2017 : Separate Stokes drift calculation   ( version 6.01 )
    !/    01-Mar-2018 : Removed RTD code (now used in post  ( version 6.02 )
    !/                  processing code)
    !/    22-Aug-2018 : Add WBT parameter                   ( version 6.06 )
    !/    25-Sep-2019 : Corrected th2m and sth2m            ( version 6.07 )
    !/                  calculations. (J Dykes, NRL)
    !/
    !  1. Purpose :
    !
    !     Fill necessary arrays with gridded data for output.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.   I   Input spectra. Left in par list to change
    !                          shape.
    !       FLPART  Log.   I   Flag for filling fields with part. data.
    !       FLOUTG  Log.   I   Flag for file field output
    !       FLOUTG2 Log.   I   Flag for coupling field output
    !     ----------------------------------------------------------------
    !
    !     Locally saved parameters
    !     ----------------------------------------------------------------
    !       HSMIN   Real  Filter level in Hs for calculation of mean
    !                     wave parameters.
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
    !     None.
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
    !     !/OMPG  OpenMP compiler directive for loop splitting.
    !
    !     !/O8    Filter for low wave heights ( HSMIN )
    !     !/O9    Negative wave height alowed, other mean parameters will
    !             not be correct.
    !
    !     !/ST0   No source terms.
    !     !/ST1   Source term set 1 (WAM equiv.)
    !     !/ST2   Source term set 2 (Tolman and Chalikov)
    !     !/ST3   Source term set 3 (WAM 4+)
    !     !/ST6   Source term set 6 (BYDRZ)
    !
    !     !/S     Enable subroutine tracing.
    !     !/T     Test output.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD
    USE W3WDATMD, ONLY: UST, FPIS
    USE W3ADATMD, ONLY: CG, WN, DW
    USE W3ADATMD, ONLY: HS, WLM, T02, T0M1, T01, FP0,               &
         THM, THS, THP0
    USE W3ADATMD, ONLY: ABA, ABD, UBA, UBD, FCUT, SXX,              &
         SYY, SXY, PHS, PTP, PLP, PDIR, PSI, PWS,    &
         PWST, PNR, USERO, TUSX, TUSY, PRMS, TPMS,   &
         USSX, USSY, MSSX, MSSY, MSSD, MSCX, MSCY,   &
         MSCD, CHARN,                                &
         BHD, CGE, P2SMS, US3D, EF, TH1M, STH1M,     &
         TH2M, STH2M, HSIG, STMAXE, STMAXD,          &
         HCMAXE, HMAXE, HCMAXD, HMAXD, USSP, QP, PQP,&
         PTHP0, PPE, PGW, PSW, PTM1, PT1, PT2, PEP,  &
         WBT, QKK
    USE W3ODATMD, ONLY: NDST, UNDEF, IAPROC, NAPROC, NAPFLD,        &
         ICPRT, DTPRT, WSCUT, NOSWLL, FLOGRD, FLOGR2,&
         NOGRP, NGRPP
    USE W3ADATMD, ONLY: NSEALM
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    USE W3PARALL, ONLY : INIT_GET_ISEA
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)        :: A(NTH,NK,0:NSEAL)
    LOGICAL, INTENT(IN)     :: FLPART, FLOUTG, FLOUTG2
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IK, ITH, JSEA, ISEA, IX, IY,         &
         IKP0(NSEAL), NKH(NSEAL),             &
         I, J, LKMS, HKMS, ITL
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: FXPMC, FACTOR, FACTOR2, EBAND, FKD,  &
         AABS, UABS,                          &
         XL, XH, XL2, XH2, EL, EH, DENOM, KD, &
         M1, M2, MA, MB, MC, STEX, STEY, STED
    REAL                    :: ET(NSEAL), EWN(NSEAL), ETR(NSEAL),   &
         ETX(NSEAL), ETY(NSEAL), AB(NSEAL),   &
         ETXX(NSEAL), ETYY(NSEAL), ETXY(NSEAL),&
         ABX(NSEAL), ABY(NSEAL),ET02(NSEAL),  &
         EBD(NK,NSEAL), EC(NSEAL),            &
         ABR(NSEAL), UBR(NSEAL), UBS(NSEAL),  &
         ABX2(NSEAL), ABY2(NSEAL),            &
         AB2X(NSEAL), AB2Y(NSEAL),            &
         ABST(NSEAL), ABXX(NSEAL),            &
         ABYY(NSEAL), ABXY(NSEAL),            &
         ABYX(NSEAL), EET1(NSEAL),            &
         ETUSCX(NSEAL), ETUSCY(NSEAL),        &
         ETMSSL(NSEAL), ETMSSCL(NSEAL),       &
         ETTPMM(NSEAL), ETF(NSEAL),           &
         ET1(NSEAL), ABX2M(NSEAL),            &
         ABY2M(NSEAL), ABXM(NSEAL),           &
         ABYM(NSEAL), ABXYM(NSEAL),           &
         MSSXM(NSEAL), MSSYM(NSEAL),          &
         MSSXTM(NSEAL), MSSYTM(NSEAL),        &
         MSSXYM(NSEAL), THMP(NSEAL),          &
         T02P(NSEAL), NV(NSEAL), NS(NSEAL),   &
         NB(NSEAL), MODE(NSEAL),              &
         MU(NSEAL), NI(NSEAL), STMAXEL(NSEAL),&
         PHI(21,NSEAL),PHIST(NSEAL),         &
         EBC(NK,NSEAL), ABP(NSEAL),           &
         STMAXDL(NSEAL), TLPHI(NSEAL),        &
         WL02X(NSEAL), WL02Y(NSEAL),          &
         ALPXT(NSEAL), ALPYT(NSEAL),          &
         ALPXY(NSEAL), SCREST(NSEAL),         &
         QK1(NSEAL), QK2(NSEAL)
    REAL                       USSCO, FT1
    REAL, SAVE              :: HSMIN = 0.01
    LOGICAL                 :: FLOLOC(NOGRP,NGRPP)
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3OUTG')
#endif
    DO I=1,NOGRP
      DO J=1,NGRPP
        FLOLOC(I,J) =   &
             ((FLOUTG.AND.FLOGRD(I,J)).OR.(FLOUTG2.AND.FLOGR2(I,J)))
      END DO
    END DO
    !
    FXPMC  = 0.66 * GRAV / 28.
    HSMIN  = HSMIN
    FT1    =  0.3333 * SIG(NK)**2 * DTH * SIG(NK)
    !
    ! 1.  Initialize storage arrays -------------------------------------- *
    !
    ET     = 0.
    ET02   = 0.
    EWN    = 0.
    ETR    = 0.
    ET1    = 0.
    EET1   = 0.
    ETX    = 0.
    ETY    = 0.
    ETXX   = 0.
    ETYY   = 0.
    ETXY   = 0.
    ABR    = 0.
    ABA    = 0.
    ABD    = 0.
    UBR    = 0.
    UBA    = 0.
    UBD    = 0.
    UBS    = 0.
    SXX    = 0.
    SYY    = 0.
    SXY    = 0.
    USSX   = 0.
    USSY   = 0.
    TUSX   = 0.
    TUSY   = 0.
    MSSX   = 0.
    MSSY   = 0.
    MSSD   = 0.
    MSCX   = 0.
    MSCY   = 0.
    MSCD   = 0.
    PRMS   = 0.
    TPMS   = 0.
    ETUSCY = 0.
    ETUSCY = 0.
    ETMSSL = 0.
    ETMSSCL= 0.
    ETTPMM = 0.
    EBD    = 0.
    EC     = 0.
    ETF    = 0.
    EBC    = 0.
    BHD = 0.
    MSSXM = 0.
    MSSYM = 0.
    MSSXTM = 0.
    MSSYTM = 0.
    MSSXYM = 0.
    PHI    = 0.
    PHIST  = 0.
    TLPHI  = 0.
    STMAXEL = 0.
    STMAXDL = 0.
    QK2 = 0.
    !
    HS     = UNDEF
    WLM    = UNDEF
    T0M1   = UNDEF
    T01    = UNDEF
    T02    = UNDEF
    FP0    = UNDEF
    THM    = UNDEF
    THS    = UNDEF
    THP0   = UNDEF
    HSIG   = UNDEF
    WL02X  = UNDEF
    WL02Y  = UNDEF
    ALPXY  = UNDEF
    ALPXT  = UNDEF
    ALPYT  = UNDEF
    QKK    = UNDEF
    THMP = UNDEF
    T02P = UNDEF
    SCREST = UNDEF
    NV = UNDEF
    NS = UNDEF
    NB = UNDEF
    MU = UNDEF
    NI = UNDEF
    MODE = UNDEF
    STMAXE = UNDEF
    STMAXD = UNDEF
    HCMAXE = UNDEF
    HMAXE = UNDEF
    HCMAXD = UNDEF
    HMAXD = UNDEF
    QP    = UNDEF
    WBT    = UNDEF
    !
    ! 2.  Integral over discrete part of spectrum ------------------------ *
    !
    DO IK=1, NK
      !
      ! 2.a Initialize energy in band
      !
      AB     = 0.
      ABX    = 0.
      ABY    = 0.
      ABX2   = 0.
      ABY2   = 0.
      AB2X   = 0.
      AB2Y   = 0.
      ABXX   = 0.
      ABYY   = 0.
      ABXY   = 0.
      ABYX   = 0.
      ABST   = 0.
      QK1    = 0.
      !
      ! 2.b Integrate energy in band
      !
      DO ITH=1, NTH
        !
#ifdef W3_OMPG
        !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,FACTOR)
#endif
        !
        DO JSEA=1, NSEAL
          NKH(JSEA)  = MIN ( NK ,   &
               INT(FACTI2+FACTI1*LOG(MAX(1.E-7,FCUT(JSEA)))) )
          AB (JSEA)  = AB (JSEA) + A(ITH,IK,JSEA)
          ABX(JSEA)  = ABX(JSEA) + A(ITH,IK,JSEA)*ECOS(ITH)
          ABY(JSEA)  = ABY(JSEA) + A(ITH,IK,JSEA)*ESIN(ITH)
          ! These are the integrals with cos^2 and sin^2
          ABX2(JSEA) = ABX2(JSEA) + A(ITH,IK,JSEA)*EC2(ITH)
          ABY2(JSEA) = ABY2(JSEA) + A(ITH,IK,JSEA)*ES2(ITH)
          ! Using trig identities to represent cos2theta and sin2theta.
          AB2X(JSEA) = AB2X(JSEA) + A(ITH,IK,JSEA)*(2*EC2(ITH) - 1)
          AB2Y(JSEA) = AB2Y(JSEA) + A(ITH,IK,JSEA)*(2*ESC(ITH))
          ABYX(JSEA) = ABYX(JSEA) + A(ITH,IK,JSEA)*ESC(ITH)
          IF (ITH.LE.NTH/2) THEN
            ABST(JSEA) = ABST(JSEA) +                               &
                 A(ITH,IK,JSEA)*A(ITH+NTH/2,IK,JSEA)
            QK1 (JSEA) = QK1(JSEA) + (A(ITH,IK,JSEA)+A(ITH+NTH/2,IK,JSEA))**2
          END IF
          CALL INIT_GET_ISEA(ISEA, JSEA)
          FACTOR     = MAX ( 0.5 , CG(IK,ISEA)/SIG(IK)*WN(IK,ISEA) )
          ABXX(JSEA) = ABXX(JSEA) + ((1.+EC2(ITH))*FACTOR-0.5) *    &
               A(ITH,IK,JSEA)
          ABYY(JSEA) = ABYY(JSEA) + ((1.+ES2(ITH))*FACTOR-0.5) *    &
               A(ITH,IK,JSEA)
          ABXY(JSEA) = ABXY(JSEA) + ESC(ITH)*FACTOR * A(ITH,IK,JSEA)
        END DO
        !
#ifdef W3_OMPG
        !$OMP END PARALLEL DO
#endif
        !
      END DO
      !
      ! 2.c Finalize integration over band and update mean arrays
      !
      !
#ifdef W3_OMPG
      !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,FACTOR,FACTOR2,MA,MC,MB,KD,FKD,USSCO,M1,M2)
#endif
      !
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        FACTOR       = DDEN(IK) / CG(IK,ISEA)
        EBD(IK,JSEA) = AB(JSEA) * FACTOR            ! this is E(f)*df
        ET (JSEA)    = ET (JSEA) + EBD(IK,JSEA)
#ifdef W3_IG1
        IF (IK.EQ.NINT(IGPARS(5))) HSIG(JSEA) = 4*SQRT(ET(JSEA))
#endif
        ETF(JSEA)  = ETF(JSEA) + EBD(IK,JSEA) * CG(IK,ISEA)
        EWN(JSEA)  = EWN(JSEA) + EBD(IK,JSEA) / WN(IK,ISEA)
        ETR(JSEA)  = ETR(JSEA) + EBD(IK,JSEA) / SIG(IK)
        ET1(JSEA)  = ET1(JSEA) + EBD(IK,JSEA) * SIG(IK)
        !          EET1(JSEA) = EET1(JSEA)+ EBD(IK,JSEA)**2 * SIG(IK)
        EET1(JSEA) = EET1(JSEA)+ EBD(IK,JSEA)**2 * SIG(IK)/DSII(IK)
        ET02(JSEA) = ET02(JSEA)+ EBD(IK,JSEA) * SIG(IK)**2
        ETX(JSEA)  = ETX(JSEA) + ABX(JSEA) * FACTOR
        ETY(JSEA)  = ETY(JSEA) + ABY(JSEA) * FACTOR
        TUSX(JSEA) = TUSX(JSEA) + ABX(JSEA)*FACTOR               &
             *GRAV*WN(IK,ISEA)/SIG(IK)
        TUSY(JSEA)  = TUSY(JSEA) + ABY(JSEA)*FACTOR               &
             *GRAV*WN(IK,ISEA)/SIG(IK)
        ETXX(JSEA) = ETXX(JSEA) + ABX2(JSEA) * FACTOR* WN(IK,ISEA)**2
        ! NB:     QK1 (JSEA) = QK1(JSEA) + A(ITH,IK,JSEA)**2
        QK2 (JSEA) = QK2 (JSEA) + QK1(JSEA)  * FACTOR* SIG(IK) /WN(IK,ISEA)
        ETYY(JSEA) = ETYY(JSEA) + ABY2(JSEA) * FACTOR* WN(IK,ISEA)**2
        ETXY(JSEA) = ETXY(JSEA) + ABYX(JSEA) * FACTOR* WN(IK,ISEA)**2
        IF (SIG(IK)*0.5*(1+XFR).LT.0.4*TPI) THEN
          ETMSSL(JSEA)  = ETMSSL(JSEA) + AB(JSEA)*FACTOR           &
               *WN(IK,ISEA)**2
        ELSE
          IF (SIG(MAX(IK-1,1))*0.5*(1+XFR).LT.0.4*TPI) THEN
            ETMSSL(JSEA)  = ETMSSL(JSEA) + AB(JSEA)*FACTOR         &
                 *(SIG(IK)*0.5*(1+1/XFR)-(0.4*TPI))/DSII(IK)     &
                 *WN(IK,ISEA)**2
            FACTOR2       = SIG(IK)**5/(GRAV**2)/DSII(IK)
            ETMSSCL(JSEA) = AB(JSEA)*FACTOR*FACTOR2
          END IF
        END IF
        !
        UBS(JSEA) = UBS(JSEA) + AB(JSEA) * SIG(IK)**2
        !
        !   2nd order equivalent surface pressure spectral density at K=0
        !   this is used for microseismic or microbarom sources
        !   Finite water depth corrections (Ardhuin & Herbers 2013) are not
        !   included here.
        !
        FACTOR2 = DTH*2/(TPI**2)                        &
             * SIG(IK)                             &
             * (TPI*SIG(IK)/CG(IK,ISEA))**2        &  ! Jacobian^2 to get E(f,th) from A(k,th)
             * ABST(JSEA)
        !
        !   Integration over seismic radian frequency : *2*dsigma
        !
        PRMS(JSEA)  = PRMS(JSEA) + FACTOR2 * 2 * DSII(IK)
        IF ( FLOLOC (6, 9).AND.(IK.GE.P2MSF(2).AND.IK.LE.P2MSF(3)))   &
             P2SMS(JSEA,IK) = FACTOR2 * 2 * TPI
        IF (FACTOR2 .GT. ETTPMM(JSEA)) THEN
          ETTPMM(JSEA) = FACTOR2
          TPMS(JSEA) = TPI/SIG(IK)
        END IF

        !
        ! Directional moments in the last freq. band
        !
        IF (IK.EQ.NK) THEN
          FACTOR2       = SIG(IK)**5/(GRAV**2)/DSII(IK)
          ETUSCX(JSEA)  = ABX(JSEA)*FACTOR*FACTOR2
          ETUSCY(JSEA)  = ABY(JSEA)*FACTOR*FACTOR2
          !
          !     NB: the slope PDF is proportional to ell1=ETYY*EC2-2*ETXY*ECS+ETYY*ES2 = A*EC2-2*B*ECS+C*ES2
          !     This is an ellipse equation with axis direction given by dir=0.5*ATAN2(-2.*ETXY,ETYY-ETXX)
          !
          MA  = ABX2(JSEA) * FACTOR * FACTOR2
          MC  = ABY2(JSEA) * FACTOR * FACTOR2
          MB  = ABYX(JSEA) * FACTOR * FACTOR2
          !
          ! Old definitions:  MSCX(JSEA)  = ABX2(JSEA) * FACTOR * FACTOR2
          !                   MSCY(JSEA)  = ABY2(JSEA) * FACTOR * FACTOR2
          MSCD(JSEA)=0.5*ATAN2(2*MB,MA-MC)

          MSCX(JSEA)= MA*COS(MSCD(JSEA))**2   &
               +2*MB*SIN(MSCD(JSEA))*COS(MSCD(JSEA))+MA*SIN(MSCD(JSEA))**2
          MSCY(JSEA)= MC*COS(MSCD(JSEA))**2   &
               -2*MB*SIN(MSCD(JSEA))*COS(MSCD(JSEA))+MA*SIN(MSCD(JSEA))**2
        END IF
        !
        ! Deep water limits
        !
        KD    = MAX ( 0.001 , WN(IK,ISEA) * DW(ISEA) )
        IF ( KD .LT. 6. ) THEN
          FKD       = FACTOR / SINH(KD)**2
          ABR(JSEA) = ABR(JSEA) + AB(JSEA) * FKD
          ABA(JSEA) = ABA(JSEA) + ABX(JSEA) * FKD
          ABD(JSEA) = ABD(JSEA) + ABY(JSEA) * FKD
          UBR(JSEA) = UBR(JSEA) + AB(JSEA) * SIG(IK)**2 * FKD
          UBA(JSEA) = UBA(JSEA) + ABX(JSEA) * SIG(IK)**2 * FKD
          UBD(JSEA) = UBD(JSEA) + ABY(JSEA) * SIG(IK)**2 * FKD
          USSCO=FKD*SIG(IK)*WN(IK,ISEA)*COSH(2.*KD)
          BHD(JSEA) = BHD(JSEA) +                             &
               GRAV*WN(IK,ISEA) * EBD(IK,JSEA) / (SINH(2.*KD))
        ELSE
          USSCO=FACTOR*SIG(IK)*2.*WN(IK,ISEA)
        END IF
        !
        ABXX(JSEA)   = MAX ( 0. , ABXX(JSEA) ) * FACTOR
        ABYY(JSEA)   = MAX ( 0. , ABYY(JSEA) ) * FACTOR
        ABXY(JSEA)   = ABXY(JSEA) * FACTOR
        SXX(JSEA)    = SXX(JSEA)  + ABXX(JSEA)
        SYY(JSEA)    = SYY(JSEA)  + ABYY(JSEA)
        SXY(JSEA)    = SXY(JSEA)  + ABXY(JSEA)
        EBD(IK,JSEA) = EBD(IK,JSEA) / DSII(IK)
        !
        IF ( FLOLOC( 3, 1).AND.(IK.GE.E3DF(2,1).AND.IK.LE.E3DF(3,1)))   &
             EF(JSEA,IK)  = EBD(IK,JSEA) * TPI
        !
        USSX(JSEA)  = USSX(JSEA) + ABX(JSEA)*USSCO
        USSY(JSEA)  = USSY(JSEA) + ABY(JSEA)*USSCO
        !
        ! Fills the 3D Stokes drift spectrum array
        !  ! The US3D Stokes drift specrum array is now calculated in a
        !  subroutine and called at the end of this subroutine
        !          IF ( FLOLOC( 6, 8).AND.(IK.GE.US3DF(2).AND.IK.LE.US3DF(3) ))   THEN
        !            US3D(JSEA,IK)    =  ABX(JSEA)*USSCO/(DSII(IK)*TPIINV)
        !            US3D(JSEA,NK+IK) =  ABY(JSEA)*USSCO/(DSII(IK)*TPIINV)
        !          END IF
        IF ( FLOLOC( 3, 2).AND.(IK.GE.E3DF(2,2).AND.IK.LE.E3DF(3,2)))  &
             TH1M(JSEA,IK)= MOD ( 630. - RADE*ATAN2(ABY(JSEA),ABX(JSEA)) , 360. )
        M1 = SQRT(ABX(JSEA)**2+ABY(JSEA)**2)/MAX(1E-20,AB(JSEA))
        IF ( FLOLOC( 3, 3).AND.(IK.GE.E3DF(2,3).AND.IK.LE.E3DF(3,3)))  &
             STH1M(JSEA,IK)= SQRT(ABS(2.*(1-M1)))*RADE
        IF ( FLOLOC( 3, 4).AND.(IK.GE.E3DF(2,4).AND.IK.LE.E3DF(3,4)))  &
             TH2M(JSEA,IK)= MOD ( 270. - RADE*0.5*ATAN2(ABY2(JSEA),AB2X(JSEA)) , 180. )
        M2 = SQRT(AB2X(JSEA)**2+AB2Y(JSEA)**2)/MAX(1E-20,AB(JSEA))
        IF ( FLOLOC( 3, 5).AND.(IK.GE.E3DF(2,5).AND.IK.LE.E3DF(3,5)))  &
             STH2M(JSEA,IK)= SQRT(ABS(0.5*(1-M2)))*RADE
      END DO
      !
#ifdef W3_OMPG
      !$OMP END PARALLEL DO
#endif
      !
    END DO
    !
    ! Start of Space-Time Extremes Section
    IF ( ( STEXU .GT. 0. .AND. STEYU .GT. 0. ) &
         .OR. ( STEDU .GT. 0. ) ) THEN
      !  Space-Time extremes
      !    (for references:
      !     - Krogstad et al, OMAE 2004
      !     - Baxevani and Rychlik, OE 2006
      !     - Adler and Taylor, 2007
      !     - Fedele, JPO 2012
      !     - Fedele et al, OM 2013
      !     - Benetazzo et al, JPO 2015)
      !
      !  Compute spectral parameters wrt the mean wave direction
      !  (no tail contribution - Prognostic)
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        IX     = MAPSF(ISEA,1)
        IY     = MAPSF(ISEA,2)
        IF ( MAPSTA(IY,IX) .GT. 0 ) THEN
          IF ( ABS(ETX(JSEA))+ABS(ETY(JSEA)) .GT. 1.E-7 ) THEN
            THMP(JSEA) = ATAN2(ETY(JSEA),ETX(JSEA))
          END IF
        END IF
      END DO
      !
      DO IK=1, NK
        !
        ABX2M = 0.
        ABY2M = 0.
        ABXM = 0.
        ABYM = 0.
        ABXYM = 0.
        !
        DO ITH=1, NTH
          !
#ifdef W3_OMPG
          !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
          !
          DO JSEA=1, NSEAL
            ABX2M(JSEA) = ABX2M(JSEA) + A(ITH,IK,JSEA)*                &
                 (ECOS(ITH)*COS(THMP(JSEA))+ESIN(ITH)*SIN(THMP(JSEA)))**2
            ABY2M(JSEA) = ABY2M(JSEA) + A(ITH,IK,JSEA)*                &
                 (ESIN(ITH)*COS(THMP(JSEA))-ECOS(ITH)*SIN(THMP(JSEA)))**2
            ABXM(JSEA)  = ABXM(JSEA) + A(ITH,IK,JSEA)*                 &
                 (ECOS(ITH)*COS(THMP(JSEA))+ESIN(ITH)*SIN(THMP(JSEA)))
            ABYM(JSEA)  = ABYM(JSEA) + A(ITH,IK,JSEA)*                 &
                 (ESIN(ITH)*COS(THMP(JSEA))-ECOS(ITH)*SIN(THMP(JSEA)))
            ABXYM(JSEA) = ABXYM(JSEA) + A(ITH,IK,JSEA)*                &
                 (ECOS(ITH)*COS(THMP(JSEA))+ESIN(ITH)*SIN(THMP(JSEA)))*   &
                 (ESIN(ITH)*COS(THMP(JSEA))-ECOS(ITH)*SIN(THMP(JSEA)))
          END DO
          !
#ifdef W3_OMPG
          !$OMP END PARALLEL DO
#endif
          !
        END DO
        !
#ifdef W3_OMPG
        !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,FACTOR)
#endif
        !
        DO JSEA=1, NSEAL
          CALL INIT_GET_ISEA(ISEA, JSEA)
          FACTOR       = DDEN(IK) / CG(IK,ISEA)
          MSSXM(JSEA)  = MSSXM(JSEA) + ABX2M(JSEA)*FACTOR*             &
               WN(IK,ISEA)**2
          MSSYM(JSEA)  = MSSYM(JSEA) + ABY2M(JSEA)*FACTOR*             &
               WN(IK,ISEA)**2
          MSSXTM(JSEA)  = MSSXTM(JSEA) + ABXM(JSEA)*FACTOR*WN(IK,ISEA)* &
               SIG(IK)
          MSSYTM(JSEA)  = MSSYTM(JSEA) + ABYM(JSEA)*FACTOR*WN(IK,ISEA)* &
               SIG(IK)
          MSSXYM(JSEA)  = MSSXYM(JSEA) + ABXYM(JSEA)*FACTOR*           &
               WN(IK,ISEA)**2
        END DO
        !
#ifdef W3_OMPG
        !$OMP END PARALLEL DO
#endif
        !
      END DO

#ifdef W3_OMPG
      !$OMP PARALLEL DO PRIVATE(JSEA,STEX,STEY,STED,ITL,IK)
#endif
      !
      DO JSEA=1, NSEAL
        !
        !  Mean wave period (no tail contribution - Prognostic)
        IF ( ET02(JSEA) .GT. 1.E-7 ) THEN
          T02P(JSEA) = TPI * SQRT(ET(JSEA) / ET02(JSEA) )
        END IF
        !
        !  Mean wavelength and mean crest length (02) for space-time extremes
        IF ( MSSXM(JSEA) .GT. 1.E-7 ) THEN
          WL02X(JSEA) = TPI * SQRT(ET(JSEA) / MSSXM(JSEA))
        END IF
        IF ( MSSYM(JSEA) .GT. 1.E-7 ) THEN
          WL02Y(JSEA) = TPI * SQRT(ET(JSEA) / MSSYM(JSEA))
        END IF
        !
        !  Irregularity parameters for space-time extremes
        IF ((MSSXM(JSEA) .GT. 1.E-7) .AND. (ET02(JSEA) .GT. 1.E-7)) THEN
          ALPXT(JSEA) = MSSXTM(JSEA) / (SQRT(MSSXM(JSEA) * ET02(JSEA)))
        ENDIF
        IF ((MSSYM(JSEA) .GT. 1.E-7) .AND. (ET02(JSEA) .GT. 1.E-7)) THEN
          ALPYT(JSEA) = MSSYTM(JSEA) / (SQRT(MSSYM(JSEA) * ET02(JSEA)))
        ENDIF
        IF ((MSSXM(JSEA) .GT. 1.E-7) .AND. (MSSYM(JSEA) .GT. 1.E-7)) THEN
          ALPXY(JSEA) = MSSXYM(JSEA) / (SQRT(MSSXM(JSEA) * MSSYM(JSEA)))
        ENDIF
        !
        !  Short-crestedness parameter
        IF (MSSXM(JSEA) .GT. 1.E-7)  THEN
          SCREST(JSEA) = SQRT(MSSYM(JSEA)/MSSXM(JSEA))
        END IF
        !
        !  Space domain size (user-defined or default)
        IF ( STEXU .GT. 0 .AND. STEYU .GT. 0 ) THEN
          STEX = STEXU
          STEY = STEYU
        ELSE
          STEX = 0.
          STEY = 0.
        END IF
        !
        !  Time domain size (user-defined or default)
        IF ( STEDU .GT. 0 ) THEN
          STED = STEDU
        ELSE
          STED = 0.
        END IF
        !
        !  Average numbers of waves in the space-time domain (Volume+Sides+Borders)
        IF ((WL02X(JSEA) .GT. 1.E-7) .AND. (WL02Y(JSEA) .GT. 1.E-7)    &
             .AND. (T02P(JSEA) .GT. 1.E-7)) THEN
          NV(JSEA) = TPI*(STEX*STEY*STED)/                             &
               (WL02X(JSEA)*WL02Y(JSEA)*T02P(JSEA))  *                    &
               SQRT(1-ALPXT(JSEA)**2-ALPYT(JSEA)**2  -                    &
               ALPXY(JSEA)**2+2*ALPXT(JSEA)*ALPYT(JSEA)*ALPXY(JSEA))
          NS(JSEA) = SQRT(TPI)*((STEX*STED)/(WL02X(JSEA)*T02P(JSEA)) * &
               SQRT(1-ALPXT(JSEA)**2) +                                   &
               (STEY*STED)/(WL02Y(JSEA)*T02P(JSEA)) *                     &
               SQRT(1-ALPYT(JSEA)**2) +                                   &
               (STEX*STEY)/(WL02X(JSEA)*WL02Y(JSEA)) *                    &
               SQRT(1-ALPXY(JSEA)**2))
          NB(JSEA) = STEX/WL02X(JSEA) + STEY/WL02Y(JSEA) +             &
               STED/T02P(JSEA)
        END IF
        !
        ! Integral measure of wave steepness (Fedele & Tayfun, 2009) MU, as a
        ! function of the spectral width parameter NI (Longuet-Higgins, 1985)
        IF (ET1(JSEA) .GT. 1.E-7) THEN
          NI(JSEA) = SQRT(ET(JSEA)*ET02(JSEA)/ET1(JSEA)**2 - 1)
        ENDIF
        IF (ET(JSEA) .GT. 1.E-7) THEN
          MU(JSEA) = ET1(JSEA)**2/GRAV * (ET(JSEA))**(-1.5) *          &
               (1-NI(JSEA)+NI(JSEA)**2)
        ENDIF
        !
        ! Mode of the Adler&Taylor distribution
        ! (normalized on the standard deviation = Hs/4)
        ! Time extremes
        IF ((STEX .EQ. 0) .AND. (STEY .EQ. 0)) THEN
          MODE(JSEA) = SQRT(2.*LOG(NB(JSEA)))
          ! Space extremes (strictly for STEX*STEY >> WL02X*WL02Y)
        ELSEIF (STED .EQ. 0) THEN
          MODE(JSEA) = SQRT(2.*LOG(NS(JSEA))+LOG(2.*LOG(NS(JSEA))+     &
               LOG(2.*LOG(NS(JSEA)))))
          ! Space-time extremes (strictly for STEX*STEY >> WL02X*WL02Y)
        ELSEIF ((WL02X(JSEA) .GT. 1.E-7) .AND. (WL02Y(JSEA) .GT. 1.E-7) &
             .AND. (T02P(JSEA) .GT. 1.E-7)) THEN
          MODE(JSEA) = SQRT(2.*LOG(NV(JSEA))+2.*LOG(2.*LOG(NV(JSEA))+  &
               2.*LOG(2.*LOG(NV(JSEA)))))
        ENDIF
        !
        ! Expected maximum sea surface elevation in the ST domain - nonlinear
        ! (in meters, Hs/4=SQRT(ET(JSEA)))
        STMAXE(JSEA) = SQRT(ET(JSEA)) *                                &
             ( MODE(JSEA)+0.5*MU(JSEA)*MODE(JSEA)**2 +                  &
             0.5772*(1+MU(JSEA)*MODE(JSEA)) /                           &
             (MODE(JSEA)-(2*NV(JSEA)*MODE(JSEA)+NS(JSEA)) /             &
             (NV(JSEA)*MODE(JSEA)**2+NS(JSEA)*MODE(JSEA)+NB(JSEA))) )
        !
        ! Standard deviation of the maximum sea surface elevation in ST domain
        !  - nonlinear (in meters, Hs/4=SQRT(ET(JSEA)))
        STMAXD(JSEA) =  SQRT(ET(JSEA)) *                               &
             ( PI*(1+MU(JSEA)*MODE(JSEA))/SQRT(6.) /                    &
             (MODE(JSEA)-(2*NV(JSEA)*MODE(JSEA)+NS(JSEA)) /             &
             (NV(JSEA)*MODE(JSEA)**2+NS(JSEA)*MODE(JSEA)+NB(JSEA))) )
        !
        ! Autocovariance (time) function (normalized on the maximum, i.e. total
        ! variance)
        IF (T02P(JSEA) .GT. 1.E-7) THEN
          TLPHI(JSEA) = 0.3*T02P(JSEA)
          DO ITL = 1, 21
            DO IK = 1, NK-3, 4
              PHI(ITL,JSEA) = PHI(ITL,JSEA) +                          &
                   (XFR**3*EBD(IK+3,JSEA)*COS(XFR**3*SIG(IK)*TLPHI(JSEA))+    &
                   XFR**2*EBD(IK+2,JSEA)*COS(XFR**2*SIG(IK)*TLPHI(JSEA))+     &
                   XFR*EBD(IK+1,JSEA)*COS(XFR*SIG(IK)*TLPHI(JSEA)) +          &
                   EBD(IK,JSEA)*COS(SIG(IK)*TLPHI(JSEA)))*DSII(IK)
            ENDDO
            TLPHI(JSEA) = TLPHI(JSEA) + T02P(JSEA)/20.
          ENDDO
          PHI(:,JSEA) = PHI(:,JSEA)/ET(JSEA)
          !
          ! First minimum of the autocovariance function (absolute value)
          PHIST(JSEA) = ABS(MINVAL(PHI(:,JSEA),1))
        ENDIF
        !
        ! Wave height of the wave with the maximum expected crest height
        ! and corresponding standard deviation
        ! (according to Boccotti Quasi-Determinism theory - linear)
        STMAXEL(JSEA) = SQRT(ET(JSEA)) * ( MODE(JSEA)+0.5772 /         &
             (MODE(JSEA)-(2*NV(JSEA)*MODE(JSEA)+NS(JSEA)) /             &
             (NV(JSEA)*MODE(JSEA)**2+NS(JSEA)*MODE(JSEA)+NB(JSEA))) )
        STMAXDL(JSEA) = SQRT(ET(JSEA)) *                               &
             ( PI/SQRT(6.) /                                            &
             (MODE(JSEA)-(2*NV(JSEA)*MODE(JSEA)+NS(JSEA)) /             &
             (NV(JSEA)*MODE(JSEA)**2+NS(JSEA)*MODE(JSEA)+NB(JSEA))) )
        HCMAXE(JSEA) = STMAXEL(JSEA)*(1+PHIST(JSEA))
        HCMAXD(JSEA) = STMAXDL(JSEA)*(1+PHIST(JSEA))
        ! Maximum expected wave height and corresponding standard deviation
        ! (according to Boccotti Quasi-Determinism theory - linear)
        HMAXE(JSEA) = STMAXEL(JSEA)*SQRT(2*(1+PHIST(JSEA)))
        HMAXD(JSEA) = STMAXDL(JSEA)*SQRT(2*(1+PHIST(JSEA)))
      ENDDO
      !
#ifdef W3_OMPG
      !$OMP END PARALLEL DO
#endif
      !

      ! End of Space-Time Extremes Section
    ENDIF
    !
    ! 3.  Finalize computation of mean parameters ------------------------ *
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,EBAND)
#endif
    !
    DO JSEA=1, NSEAL
      CALL INIT_GET_ISEA(ISEA, JSEA)
      !
      ! 3.a Directional mss parameters
      !     NB: the slope PDF is proportional to ell1=ETYY*EC2-2*ETXY*ECS+ETXX*ES2 = C*EC2-2*B*ECS+A*ES2
      !     This is an ellipse equation with axis direction given by dir=0.5*ATAN2(2.*ETXY,ETXX-ETYY)
      !     From matlab script: t0=0.5*(atan2(2.*B,A-C));
      !     From matlab script: A2=A.*cos(t0).^2+2.*B.*sin(t0).*cos(t0)+A.*cos(t0).^2+C.*sin(t0)^2;
      !     From matlab script: C2=C.*cos(t0)^2-2.*B.*sin(t0).*cos(t0)+A.*sin(t0).^2;
      MSSD(JSEA)=0.5*(ATAN2(2*ETXY(JSEA),ETXX(JSEA)-ETYY(JSEA)))
      MSSX(JSEA)  = ETXX(JSEA)*COS(MSSD(JSEA))**2   &
           +2*ETXY(JSEA)*SIN(MSSD(JSEA))*COS(MSSD(JSEA))+ETYY(JSEA)*SIN(MSSD(JSEA))**2
      MSSY(JSEA)  = ETYY(JSEA)*COS(MSSD(JSEA))**2   &
           -2*ETXY(JSEA)*SIN(MSSD(JSEA))*COS(MSSD(JSEA))+ETXX(JSEA)*SIN(MSSD(JSEA))**2
      !
      ! 3.b Add tail
      !     ( DTH * SIG absorbed in FTxx )

      EBAND     = AB(JSEA) / CG(NK,ISEA)           ! EBAND is E(sigma)/sigma for the last frequency band
      ET (JSEA) = ET (JSEA) + FTE  * EBAND
      EWN(JSEA) = EWN(JSEA) + FTWL * EBAND
      ETF(JSEA) = ETF(JSEA) + GRAV * FTTR * EBAND  ! this is the integral of CgE in deep water
      ETR(JSEA) = ETR(JSEA) + FTTR * EBAND
      ET1(JSEA) = ET1(JSEA) + FT1  * EBAND
      !        EET1(JSEA)= EET1(JSEA) + FT1  * EBAND**2 : this was not correct. Actually tail may not be needed for Qp.
      ET02(JSEA)= ET02(JSEA)+ EBAND* 0.5 * SIG(NK)**4 * DTH
      ETX(JSEA) = ETX(JSEA) + FTE * ABX(JSEA) / CG(NK,ISEA)
      ETY(JSEA) = ETY(JSEA) + FTE * ABY(JSEA) / CG(NK,ISEA)
      SXX(JSEA) = SXX(JSEA) + FTE * ABXX(JSEA) / CG(NK,ISEA)
      SYY(JSEA) = SYY(JSEA) + FTE * ABYY(JSEA) / CG(NK,ISEA)
      SXY(JSEA) = SXY(JSEA) + FTE * ABXY(JSEA) / CG(NK,ISEA)
      !
      ! Tail for surface stokes drift is commented out: very sensitive to tail power
      !
      !       USSX(JSEA)  = USSX(JSEA) + 2*GRAV*ETUSCX(JSEA)/SIG(NK)
      !       USSY(JSEA)  = USSY(JSEA) + 2*GRAV*ETUSCY(JSEA)/SIG(NK)
      UBS(JSEA) = UBS(JSEA) + FTWL * EBAND/GRAV
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    SXX    = SXX * DWAT * GRAV
    SYY    = SYY * DWAT * GRAV
    SXY    = SXY * DWAT * GRAV
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,IX,IY)
#endif
    !
    DO JSEA=1, NSEAL
      CALL INIT_GET_ISEA(ISEA, JSEA)
      IX     = MAPSF(ISEA,1)
      IY     = MAPSF(ISEA,2)
      IF ( MAPSTA(IY,IX) .GT. 0 ) THEN
#ifdef W3_O9
        IF ( ET(JSEA) .GE. 0. ) THEN
#endif
          HS (JSEA) = 4. * SQRT ( ET(JSEA) )
#ifdef W3_O9
        ELSE
          HS (JSEA) = - 4. * SQRT ( -ET(JSEA) )
        END IF
#endif
        IF ( ET(JSEA) .GT. 1.E-7 ) THEN
          QP(JSEA) = ( 2. / ET(JSEA)**2 ) * EET1(JSEA)
          WLM(JSEA) = EWN(JSEA) / ET(JSEA) * TPI
          T0M1(JSEA) = ETR(JSEA) / ET(JSEA) * TPI
          THS(JSEA) = RADE * SQRT ( MAX ( 0. , 2. * ( 1. - SQRT ( &
               MAX(0.,(ETX(JSEA)**2+ETY(JSEA)**2)/ET(JSEA)**2) ) ) ) )
          IF ( THS(JSEA) .LT. 0.01*RADE*DTH ) THS(JSEA) = 0.
          ! NB:           QK1 (JSEA) = QK1(JSEA) + A(ITH,IK,JSEA)**2
          !               QK2 (JSEA) = QK2 (JSEA) + QK1(JSEA)  * FACTOR* SIG(IK) /WN(IK,ISEA)
          QKK (JSEA) = SQRT(0.5*QK2 (JSEA))/ET(JSEA)
        ELSE
          WLM(JSEA) = 0.
          T0M1(JSEA) = TPI / SIG(NK)
          THS(JSEA) = 0.
        END IF
        IF ( ABS(ETX(JSEA))+ABS(ETY(JSEA)) .GT. 1.E-7 ) THEN
          THM(JSEA) = ATAN2(ETY(JSEA),ETX(JSEA))
        ELSE
          THM(JSEA) = 0.
        END IF
        ABR(JSEA) = SQRT ( 2. * MAX ( 0. , ABR(JSEA) ) )
        IF ( ABR(JSEA) .GE. 1.E-7 ) THEN
          ABD(JSEA) = ATAN2(ABD(JSEA),ABA(JSEA))
        ELSE
          ABD(JSEA) = 0.
        ENDIF
        ABA(JSEA) = ABR(JSEA)
        UBR(JSEA) = SQRT ( 2. * MAX ( 0. , UBR(JSEA) ) )
        IF ( UBR(JSEA) .GE. 1.E-7 ) THEN
          UBD(JSEA) = ATAN2(UBD(JSEA),UBA(JSEA))
        ELSE
          UBD(JSEA) = 0.
        ENDIF
        UBA(JSEA) = UBR(JSEA)
        CGE(JSEA) = DWAT*GRAV*ETF(JSEA)
        IF ( ET02(JSEA) .GT. 1.E-7  .AND.  ET(JSEA) .GT. 0 ) THEN
          T02(JSEA) = TPI * SQRT(ET(JSEA) / ET02(JSEA) )
          T01(JSEA) = TPI * ET(JSEA) / ET1(JSEA)
        ELSE
          T02(JSEA) = TPI / SIG(NK)
          T01(JSEA)= T02(JSEA)
        ENDIF
        !
        !  Add here USERO(JSEA,1) ...
        !
      END IF
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    ! 3.b Clean-up small values if !/O8 switch selected
    !
#ifdef W3_O8
    DO JSEA=1, NSEAL
      IF ( HS(JSEA).LE.HSMIN .AND. HS(JSEA).NE.UNDEF) THEN
        WLM(JSEA) = UNDEF
        T02(JSEA) = UNDEF
        T0M1(JSEA) = UNDEF
        THM(JSEA) = UNDEF
        THS(JSEA) = UNDEF
      END IF
    END DO
#endif
    !
    ! 4.  Peak frequencies and directions -------------------------------- *
    ! 4.a Initialize
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
    !
    DO JSEA=1, NSEAL
      EC  (JSEA) = EBD(NK,JSEA)
      FP0 (JSEA) = UNDEF
      IKP0(JSEA) = NK
      THP0(JSEA) = UNDEF
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    ! 4.b Discrete peak frequencies
    !
    DO IK=NK-1, 1, -1
      !
#ifdef W3_OMPG
      !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
      !
      DO JSEA=1, NSEAL
        IF ( EC(JSEA) .LT. EBD(IK,JSEA) ) THEN
          EC  (JSEA) = EBD(IK,JSEA)
          IKP0(JSEA) = IK
        END IF
      END DO
      !
#ifdef W3_OMPG
      !$OMP END PARALLEL DO
#endif
      !
    END DO
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
    !
    DO JSEA=1, NSEAL
      IF ( IKP0(JSEA) .NE. NK ) FP0(JSEA) = SIG(IKP0(JSEA)) * TPIINV
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    ! 4.c Continuous peak frequencies
    !
    XL     = 1./XFR - 1.
    XH     =  XFR - 1.
    XL2    = XL**2
    XH2    = XH**2
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA,EL,EH,DENOM)
#endif
    !
    DO JSEA=1, NSEAL
      IF ( IKP0(JSEA) .NE. NK ) THEN
        IF ( IKP0(JSEA) .EQ. 1 ) THEN
          EL = - EBD(IKP0(JSEA), JSEA)
        ELSE
          EL = EBD(IKP0(JSEA)-1, JSEA) - EBD(IKP0(JSEA), JSEA)
        END IF

        EH = EBD(IKP0(JSEA)+1, JSEA) - EBD(IKP0(JSEA), JSEA)

        DENOM  = XL*EH - XH*EL
        FP0(JSEA) = FP0 (JSEA) * ( 1. + 0.5 * ( XL2*EH - XH2*EL )   &
             / SIGN ( MAX(ABS(DENOM),1.E-15) , DENOM ) )
      END IF
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    ! 4.d Peak directions
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
    !
    DO JSEA=1, NSEAL
      ETX(JSEA) = 0.
      ETY(JSEA) = 0.
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    DO ITH=1, NTH
      !
#ifdef W3_OMPG
      !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
      !
      DO JSEA=1, NSEAL
        IF ( IKP0(JSEA) .NE. NK ) THEN
          ETX(JSEA) = ETX(JSEA) + A(ITH,IKP0(JSEA),JSEA)*ECOS(ITH)
          ETY(JSEA) = ETY(JSEA) + A(ITH,IKP0(JSEA),JSEA)*ESIN(ITH)
        END IF
      END DO
      !
#ifdef W3_OMPG
      !$OMP END PARALLEL DO
#endif
      !
    END DO
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
    !
    DO JSEA=1, NSEAL
      IF ( ABS(ETX(JSEA))+ABS(ETY(JSEA)) .GT. 1.E-7 .AND.           &
           FP0(JSEA).NE.UNDEF )                                     &
           THP0(JSEA) = ATAN2(ETY(JSEA),ETX(JSEA))
      ETX(JSEA) = 0.
      ETY(JSEA) = 0.
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
    !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,IX,IY)
#endif
    !
    DO JSEA =1, NSEAL
      CALL INIT_GET_ISEA(ISEA, JSEA)
      IX          = MAPSF(ISEA,1)
      IY          = MAPSF(ISEA,2)
      IF ( MAPSTA(IY,IX) .LE. 0 ) THEN
        FP0 (JSEA) = UNDEF
        THP0(JSEA) = UNDEF
      END IF
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    ! 5.  Test output (local to MPP only)
    !
#ifdef W3_T
    WRITE (NDST,9050)
    DO JSEA =1, NSEAL
      CALL INIT_GET_ISEA(ISEA, JSEA)
      IX     = MAPSF(ISEA,1)
      IY     = MAPSF(ISEA,2)
      IF ( HS(JSEA) .EQ. UNDEF ) THEN
        WRITE (NDST,9051) ISEA, IX, IY
      ELSE IF ( WLM(JSEA) .EQ. UNDEF ) THEN
        WRITE (NDST,9052) ISEA, IX, IY, HS(JSEA)
      ELSE IF ( FP0(JSEA) .EQ. UNDEF ) THEN
        WRITE (NDST,9053) ISEA, IX, IY, HS(JSEA), WLM(JSEA),   &
             T0M1(JSEA), RADE*THM(JSEA), THS(JSEA)
      ELSE
        WRITE (NDST,9054) ISEA, IX, IY, HS(JSEA), WLM(JSEA),   &
             T0M1(JSEA), RADE*THM(JSEA), THS(JSEA), FP0(JSEA),&
             THP0(JSEA)
      END IF
    END DO
#endif
    !
    ! 6.  Fill arrays wth partitioned data
    !
    IF ( FLPART ) THEN
      !
      ! 6.a Initializations
      !
      PHS    = UNDEF
      PTP    = UNDEF
      PLP    = UNDEF
      PDIR   = UNDEF
      PSI    = UNDEF
      PWS    = UNDEF
      PWST   = UNDEF
      PNR    = UNDEF
      PTHP0  = UNDEF
      PQP    = UNDEF
      PPE    = UNDEF
      PGW    = UNDEF
      PSW    = UNDEF
      PTM1   = UNDEF
      PT1    = UNDEF
      PT2    = UNDEF
      PEP    = UNDEF
      !
      ! 6.b Loop over local sea points
      !
#ifdef W3_OMPG
      !$OMP PARALLEL DO PRIVATE(ISEA,JSEA,IX,IY,I,J)
#endif
      !
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        IX          = MAPSF(ISEA,1)
        IY          = MAPSF(ISEA,2)
        !
        IF ( MAPSTA(IY,IX).GT.0 ) THEN
          I         = ICPRT(JSEA,2)
          PNR(JSEA) = MAX ( 0. , REAL(ICPRT(JSEA,1)-1) )
          IF ( ICPRT(JSEA,1).GE.1 ) PWST(JSEA) = DTPRT(6,I)
        END IF
        !
        IF ( MAPSTA(IY,IX).GT.0 .AND. ICPRT(JSEA,1).GT.1 ) THEN
          I      = ICPRT(JSEA,2) + 1
          IF ( DTPRT(6,I) .GE. WSCUT ) THEN
            PHS(JSEA,0) = DTPRT(1,I)
            PTP(JSEA,0) = DTPRT(2,I)
            PLP(JSEA,0) = DTPRT(3,I)
            ! (PDIR is already in degrees nautical - convert back to
            !  Cartesian in radians to maintain internal convention)
            IF(DTPRT(4,I) .NE. UNDEF) THEN
              PDIR(JSEA,0) = (270. - DTPRT(4,I)) * DERA
            ENDIF
            PSI(JSEA,0) = DTPRT(5,I)
            PWS(JSEA,0) = DTPRT(6,I)
            ! (PTHP0 is already in degrees nautical - convert back to
            !  Cartesian in radians to maintain internal convention)
            IF(DTPRT(7,I) .NE. UNDEF) THEN
              PTHP0(JSEA,0) = (270. - DTPRT(7,I)) * DERA
            ENDIF
            PSW(JSEA,0) = DTPRT(8,I)
            PPE(JSEA,0) = DTPRT(9,I)
            PQP(JSEA,0) = DTPRT(10,I)
            PGW(JSEA,0) = DTPRT(11,I)
            PTM1(JSEA,0) = DTPRT(12,I)
            PT1(JSEA,0) = DTPRT(13,I)
            PT2(JSEA,0) = DTPRT(14,I)
            PEP(JSEA,0) = DTPRT(15,I)
            I      = I + 1
          END IF
          DO J=1, NOSWLL
            IF ( I .GT.  ICPRT(JSEA,2)+ICPRT(JSEA,1)-1 ) EXIT
            PHS(JSEA,J) = DTPRT(1,I)
            PTP(JSEA,J) = DTPRT(2,I)
            PLP(JSEA,J) = DTPRT(3,I)
            ! (PDIR is already in degrees nautical - convert back to
            !  Cartesian in radians to maintain internal convention)
            IF(DTPRT(4,I) .NE. UNDEF) THEN
              PDIR(JSEA,J) = (270. - DTPRT(4,I)) * DERA
            ENDIF
            PSI(JSEA,J) = DTPRT(5,I)
            PWS(JSEA,J) = DTPRT(6,I)
            ! (PTHP0 is already in degrees nautical - convert back to
            !  Cartesian in radians to maintain internal convention)
            IF(DTPRT(7,I) .NE. UNDEF) THEN
              PTHP0(JSEA,J) = (270. - DTPRT(7,I)) * DERA
            ENDIF
            PSW(JSEA,J) = DTPRT(8,I)
            PPE(JSEA,J) = DTPRT(9,I)
            PQP(JSEA,J) = DTPRT(10,I)
            PGW(JSEA,J) = DTPRT(11,I)
            PTM1(JSEA,J) = DTPRT(12,I)
            PT1(JSEA,J) = DTPRT(13,I)
            PT2(JSEA,J) = DTPRT(14,I)
            PEP(JSEA,J) = DTPRT(15,I)
            I      = I + 1
          END DO
        END IF
        !
      END DO
      !
#ifdef W3_OMPG
      !$OMP END PARALLEL DO
#endif
      !

    END IF

    IF (FLOLOC( 6, 8)) THEN
      CALL CALC_U3STOKES(A,1)
    END IF
    !
    IF (FLOLOC( 6, 12)) THEN
      CALL CALC_U3STOKES(A,2)
    ENDIF
    !
    IF (FLOLOC( 8, 7).OR.FLOLOC( 8, 8).OR.FLOLOC( 8, 9)) THEN
      CALL SKEWNESS(A)
    END IF
    
    !
    ! Dominant wave breaking probability
    !
    IF (FLOLOC(2, 17)) CALL CALC_WBT(A)
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9050 FORMAT (' TEST W3OUTG : ISEA, IX, IY, HS, L, Tm, THm, THs',     &
         ', FP0, THP0')
9051 FORMAT (2X,I8,2I8)
9052 FORMAT (2X,I8,2I8,F6.2)
9053 FORMAT (2X,I8,2I8,F6.2,F7.1,F6.2,2F6.1)
9054 FORMAT (2X,I8,2I8,F6.2,F7.1,F6.2,2F6.1,F6.3,F6.0)
#endif

    !/
    !/ End of W3OUTG ----------------------------------------------------- /
    !/
  END SUBROUTINE W3OUTG
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief  Read/write gridded output.
  !>
  !> @details Fields in file are determined by flags in FLOGRD in W3ODATMD.
  !>
  !> @param[inout] INXOUT  Test string for read/write.
  !> @param[inout] NDSOG   File unit number.
  !> @param[inout] IOTST   Test indictor for reading.
  !> @param[inout] IMOD    Model number for W3GDAT etc.
  !>
  !> @author H. L. Tolman  @date 22-Mar-2021
  !>
  SUBROUTINE W3IOGO ( INXOUT, NDSOG, IOTST, IMOD &
#ifdef W3_ASCII
                      ,NDSOA &
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
    !/    04-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/                  Major changes to logistics.
    !/    24-Jan-2001 : Flat grid version (formats only)    ( version 2.06 )
    !/    23-Apr-2002 : Clean up                            ( version 2.19 )
    !/    29-Apr-2002 : Add output types 17-18.             ( version 2.20 )
    !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
    !/    25-Oct-2004 : Multiple grid version.              ( version 3.06 )
    !/    27-Jun-2005 : Adding MAPST2.                      ( version 3.07 )
    !/    21-Jul-2005 : Adding output fields 19-21.         ( version 3.07 )
    !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
    !/    05-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
    !/    02-Apr-2007 : Adding partitioned output.          ( version 3.11 )
    !/                  Adding user slots for outputs.
    !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
    !/                  (W. E. Rogers & T. J. Campbell, NRL)
    !/    31-Oct-2010 : Implement unstructured grids        ( version 3.14 )
    !/                  (A. Roland and F. Ardhuin)
    !/    05-Feb-2011 : Renumbering of output fields        ( version 3.14 )
    !/                  (F. Ardhuin)
    !/    25-Dec-2012 : New output structure and smaller    ( version 4.11 )
    !/                  memory footprint.
    !/    21-Aug-2013 : Added missing cos,sin for UBA, ABA  ( version 4.11 )
    !/    27-Nov-2013 : Management of coupling output       ( version 4.18 )
    !/    01-Mar-2018 : Removed RTD code (now used in post  ( version 6.02 )
    !/                  processing code)
    !/    25-Aug-2018 : Add WBT parameter                   ( version 6.06 )
    !/    22-Mar-2021 : Add extra coupling fields as output ( version 7.13 )
    !/    07-Mar-2024 : Add Skewness parameters             ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Read/write gridded output.
    !
    !  2. Method :
    !
    !     Fields in file are determined by flags in FLOGRD in W3ODATMD.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       INXOUT  C*(*)  I   Test string for read/write, valid are:
    !                          'READ' and 'WRITE'.
    !       NDSOG   Int.   I   File unit number.
    !       IOTST   Int.   O   Test indictor for reading.
    !                           0 : Fields read.
    !                          -1 : Past end of file.
    !       IMOD    Int.   I   Model number for W3GDAT etc.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !       See module documentation above.
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
    !      WW3_OUTF  Prog.   N/A    Ouput postprocessor.
    !      WW3_GRIB  Prog.   N/A    Ouput postprocessor.
    !      GX_OUTF   Prog.   N/A    Ouput postprocessor.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !       Tests on INXOUT, file status and on array dimensions.
    !
    !  7. Remarks :
    !
    !     - MAPSTA is dumped as it contains information on the ice edge.
    !       Dynamic ice edges require MAPSTA to be dumped every time step.
    !     - The output file has the pre-defined name 'out_grd.FILEXT'.
    !     - The current components CX and CY are written to out_grd as
    !       components, but converted to magnitude and direction in most
    !       gridded and point output post-processors (except gx_outf).
    !     - All written direction are in degrees, nautical convention,
    !       but in reading, all is convered back to radians and cartesian
    !       conventions.
    !     - Before writing, wind and current directions are converted,
    !       wave directions are already in correct convention (see W3OUTG).
    !     - In MPP version of model data is supposed to be gatherd at the
    !       correct processor before the routine is called.
    !     - In MPP version routine is called by only one process, therefore
    !       no test on process for error messages is needed.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/ST1   First source term package (WAM3).
    !     !/ST2   Second source term package (TC96).
    !     !/S     Enable subroutine tracing.
    !     !/T     Test output.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD
    !/
    USE W3WDATMD, ONLY: W3SETW, W3DIMW
    USE W3ADATMD, ONLY: W3SETA, W3DIMA, W3XETA
    USE W3ODATMD, ONLY: W3SETO
    !/
    USE W3WDATMD, ONLY: TIME, DINIT, WLV, ICE, ICEF, ICEH, BERG,    &
         UST,  USTDIR, ASF, RHOAIR
    USE W3ADATMD, ONLY: AINIT, DW, UA, UD, AS, CX, CY, WN,          &
         TAUA, TAUADIR
    USE W3ADATMD, ONLY: HS, WLM, T02, T0M1, T01, FP0, THM, THS, THP0,&
         WBT, WNMEAN
    USE W3ADATMD, ONLY: DTDYN, FCUT, ABA, ABD, UBA, UBD, SXX, SYY, SXY,&
         PHS, PTP, PLP, PDIR, PSI, PWS, PWST, PNR,    &
         PTHP0, PQP, PPE, PGW, PSW, PTM1, PT1, PT2,  &
         PEP, USERO, TAUOX, TAUOY, TAUWIX, TAUWIY,    &
         PHIAW, PHIOC, TUSX, TUSY, PRMS, TPMS,        &
         USSX, USSY, MSSX, MSSY, MSSD, MSCX, MSCY,    &
         MSCD, QP, TAUWNX, TAUWNY, CHARN, TWS, BHD,   &
         PHIBBL, TAUBBL, WHITECAP, BEDFORMS, CGE, EF, &
         CFLXYMAX, CFLTHMAX, CFLKMAX, P2SMS, US3D,    &
         TH1M, STH1M, TH2M, STH2M, HSIG, PHICE, TAUICE,&
         STMAXE, STMAXD, HMAXE, HCMAXE, HMAXD, HCMAXD,&
         USSP, TAUOCX, TAUOCY, QKK, SKEW, EMBIA1, EMBIA2
    !/
    USE W3ODATMD, ONLY: NOGRP, NGRPP, IDOUT, UNDEF, NDST, NDSE,     &
         FLOGRD, IPASS => IPASS1, WRITE => WRITE1,   &
         FNMPRE, NOSWLL, NOEXTR
    !/
    USE W3SERVMD, ONLY: EXTCDE
    USE W3ODATMD, only : IAPROC
    USE W3ODATMD, ONLY :  OFILES
#ifdef W3_SETUP
    USE W3WDATMD, ONLY: ZETA_SETUP
#endif
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(INOUT)        :: IOTST
    INTEGER, INTENT(IN)           :: NDSOG
    INTEGER, INTENT(IN), OPTIONAL :: IMOD
    CHARACTER, INTENT(IN)         :: INXOUT*(*)
    CHARACTER(LEN=15) :: TIMETAG
#ifdef W3_ASCII
    INTEGER, INTENT(IN), OPTIONAL :: NDSOA
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IGRD, IERR, I, J, IX, IY, MOGRP,     &
         MGRPP, ISEA, MOSWLL, IK, IFI, IFJ    &
         ,IFILOUT
    INTEGER, ALLOCATABLE    :: MAPTMP(:,:)
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: AUX1(NSEA), AUX2(NSEA),              &
         AUX3(NSEA), AUX4(NSEA)
#ifdef W3_SMC
    REAL                    :: UDARC
#endif
    CHARACTER(LEN=30)       :: IDTST, TNAME
    CHARACTER(LEN=10)       :: VERTST
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3IOGO')
#endif
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
    CALL W3SETA ( IGRD, NDSE, NDST )
#ifdef W3_MPI
    CALL W3XETA ( IGRD, NDSE, NDST )
#endif
    CALL W3SETW ( IGRD, NDSE, NDST )
    !
    IPASS  = IPASS + 1
    IOTST  = 0
    !
    IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE' ) THEN
      WRITE (NDSE,900) INXOUT
      CALL EXTCDE ( 1 )
    END IF
    !
    IF ( IPASS.EQ.1 .AND. OFILES(1) .EQ. 0) THEN
      WRITE  = INXOUT.EQ.'WRITE'
    ELSE
      IF ( WRITE .AND. INXOUT.EQ.'READ' ) THEN
        WRITE (NDSE,901) INXOUT
        CALL EXTCDE ( 2 )
      END IF
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9000) IPASS, INXOUT, WRITE, NDSOG, IGRD, FILEXT
#endif
    !
    !
    ! open file ---------------------------------------------------------- *
    ! ( IPASS = 1 )
    !
    IF ( IPASS.EQ.1 .AND. OFILES(1) .EQ. 0) THEN
      I      = LEN_TRIM(FILEXT)
      J      = LEN_TRIM(FNMPRE)
      !
#ifdef W3_T
      WRITE (NDST,9001) FNMPRE(:J)//'out_grd.'//FILEXT(:I)
#endif
      IF ( WRITE ) THEN
        OPEN (NDSOG,FILE=FNMPRE(:J)//'out_grd.'//FILEXT(:I),    &
             form ='UNFORMATTED', convert=file_endian,ERR=800,IOSTAT=IERR)
#ifdef W3_ASCII
        OPEN (NDSOA,FILE=FNMPRE(:J)//'out_grd.'//FILEXT(:I)//'.txt',    &
             form ='FORMATTED',ERR=800,IOSTAT=IERR)
#endif
      ELSE
        OPEN (NDSOG,FILE=FNMPRE(:J)//'out_grd.'//FILEXT(:I),    &
             form='UNFORMATTED', convert=file_endian,ERR=800,IOSTAT=IERR,STATUS='OLD')
      END IF
      !
      REWIND ( NDSOG )
      !
      ! test info --------------------------------------------------------- *
      ! ( IPASS = 1 )
      !
      IF ( WRITE ) THEN
        WRITE (NDSOG)                                           &
             IDSTR, VEROGR, GNAME, NOGRP, NGRPP, NSEA, NX, NY,     &
             UNDEF, NOSWLL
#ifdef W3_ASCII
        WRITE (NDSOA,*)                                           &
             'IDSTR, VEROGR, GNAME, NOGRP, NGRPP, NSEA, NX, NY,     &
             UNDEF, NOSWLL:',                                     &
             IDSTR, VEROGR, GNAME, NOGRP, NGRPP, NSEA, NX, NY,     &
             UNDEF, NOSWLL
#endif
      ELSE
        READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)                &
             IDTST, VERTST, TNAME, MOGRP, MGRPP, NSEA, NX, NY,     &
             UNDEF, MOSWLL
        !
        IF ( IDTST .NE. IDSTR ) THEN
          WRITE (NDSE,902) IDTST, IDSTR
          CALL EXTCDE ( 20 )
        END IF
        IF ( VERTST .NE. VEROGR ) THEN
          WRITE (NDSE,903) VERTST, VEROGR
          CALL EXTCDE ( 21 )
        END IF
        IF ( NOGRP .NE. MOGRP .OR. NGRPP .NE. MGRPP ) THEN
          WRITE (NDSE,904) MOGRP, MGRPP, NOGRP, NGRPP
          CALL EXTCDE ( 22 )
        END IF
        IF ( TNAME .NE. GNAME ) THEN
          WRITE (NDSE,905) TNAME, GNAME
        END IF
        IF ( NOSWLL .NE. MOSWLL ) THEN
          WRITE (NDSE,906) MOSWLL, NOSWLL
          CALL EXTCDE ( 24 )
        END IF
        !
      END IF
      !
#ifdef W3_T
      WRITE (NDST,9002) IDSTR, VEROGR, GNAME, NSEA, NX, NY,    &
           UNDEF
#endif
      !
    END IF
    !
    !  IN CASE OF GENERATION OF A NEW FILE OUTPUT EVERY DELTA OUTPUT
    ! open file ---------------------------------------------------------- *
    ! ( IPASS = 1 )
    !
    IF ( IPASS.GE.1 .AND. OFILES(1) .EQ. 1) THEN
      WRITE  = INXOUT.EQ.'WRITE'
    ELSE
      IF ( WRITE .AND. INXOUT.EQ.'READ' ) THEN
        WRITE (NDSE,901) INXOUT
        CALL EXTCDE ( 2 )
      END IF
    END IF

    !
    IF ( IPASS.GE.1 .AND. OFILES(1) .EQ. 1) THEN
      I      = LEN_TRIM(FILEXT)
      J      = LEN_TRIM(FNMPRE)
      !
      ! Create TIMETAG for file name using YYYYMMDD.HHMMS prefix
      WRITE(TIMETAG,"(i8.8,'.'i6.6)")TIME(1),TIME(2)
#ifdef W3_T
      WRITE (NDST,9001) FNMPRE(:J)//TIMETAG//'.out_grd.'//FILEXT(:I)
#endif
      IF ( WRITE ) THEN
        OPEN (NDSOG,FILE=FNMPRE(:J)//TIMETAG//'.out_grd.'  &
             //FILEXT(:I),form='UNFORMATTED', convert=file_endian,ERR=800,IOSTAT=IERR)
#ifdef W3_ASCII
        OPEN (NDSOA,FILE=FNMPRE(:J)//TIMETAG//'.out_grd.'  &
             //FILEXT(:I)//'.txt',form='FORMATTED',ERR=800,IOSTAT=IERR)
#endif
      ELSE
        OPEN (NDSOG,FILE=FNMPRE(:J)//'out_grd.'//FILEXT(:I),    &
             form='UNFORMATTED', convert=file_endian,ERR=800,IOSTAT=IERR,STATUS='OLD')
      END IF
      !
      REWIND ( NDSOG )
      !
      ! test info --------------------------------------------------------- *
      ! ( IPASS >= 1 & OFILES(1) = 1)
      !
      IF ( WRITE ) THEN
        WRITE (NDSOG)                                           &
             IDSTR, VEROGR, GNAME, NOGRP, NGRPP, NSEA, NX, NY,     &
             UNDEF, NOSWLL
#ifdef W3_ASCII
        WRITE (NDSOA,*)                                           &
             'IDSTR, VEROGR, GNAME, NOGRP, NGRPP, NSEA, NX, NY,     &
             UNDEF, NOSWLL:',                                     &
             IDSTR, VEROGR, GNAME, NOGRP, NGRPP, NSEA, NX, NY,     &
             UNDEF, NOSWLL
#endif
      ELSE
        READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)                &
             IDTST, VERTST, TNAME, MOGRP, MGRPP, NSEA, NX, NY,     &
             UNDEF, MOSWLL
        !
        IF ( IDTST .NE. IDSTR ) THEN
          WRITE (NDSE,902) IDTST, IDSTR
          CALL EXTCDE ( 20 )
        END IF
        IF ( VERTST .NE. VEROGR ) THEN
          WRITE (NDSE,903) VERTST, VEROGR
          CALL EXTCDE ( 21 )
        END IF
        IF ( NOGRP .NE. MOGRP .OR. NGRPP .NE. MGRPP ) THEN
          WRITE (NDSE,904) MOGRP, MGRPP, NOGRP, NGRPP
          CALL EXTCDE ( 22 )
        END IF
        IF ( TNAME .NE. GNAME ) THEN
          WRITE (NDSE,905) TNAME, GNAME
        END IF
        IF ( NOSWLL .NE. MOSWLL ) THEN
          WRITE (NDSE,906) MOSWLL, NOSWLL
          CALL EXTCDE ( 24 )
        END IF
        !
      END IF
      !
#ifdef W3_T
      WRITE (NDST,9002) IDSTR, VEROGR, GNAME, NSEA, NX, NY,    &
           UNDEF
#endif
      !
    END IF
    !
    ! TIME and flags ----------------------------------------------------- *
    !
    IF ( WRITE ) THEN
      WRITE (NDSOG)                            TIME, FLOGRD
#ifdef W3_ASCII
      WRITE (NDSOA,*)                         'TIME, FLOGRD:', &
                                               TIME, FLOGRD
#endif
    ELSE
      READ (NDSOG,END=803,ERR=802,IOSTAT=IERR) TIME, FLOGRD
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9003) TIME, FLOGRD
#endif
    !
    ! MAPSTA ------------------------------------------------------------- *
    !
    ALLOCATE ( MAPTMP(NY,NX) )
    IF ( WRITE ) THEN
      MAPTMP = MAPSTA + 8*MAPST2
      WRITE (NDSOG)                                               &
           ((MAPTMP(IY,IX),IX=1,NX),IY=1,NY)
#ifdef W3_ASCII
      WRITE (NDSOA,*) 'MAPSTA:',                                  &
           ((MAPTMP(IY,IX),IX=1,NX),IY=1,NY)
#endif
    ELSE
      READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)                    &
           ((MAPTMP(IY,IX),IX=1,NX),IY=1,NY)
      MAPSTA = MOD(MAPTMP+2,8) - 2
      MAPST2 = (MAPTMP-MAPSTA) / 8
    END IF
    DEALLOCATE ( MAPTMP )
    !
    ! Fields ---------------------------------------------- *
    !
    ! Initialization ---------------------------------------------- *
    !
    IF ( WRITE ) THEN
      DO ISEA=1, NSEA
        IF ( MAPSTA(MAPSF(ISEA,2),MAPSF(ISEA,1)) .LT. 0 ) THEN
          !
          IF ( FLOGRD( 2, 2) ) WLM   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 3) ) T02   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 4) ) T0M1  (ISEA) = UNDEF
          IF ( FLOGRD( 2, 5) ) T01   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 6) .OR. FLOGRD( 2,18) )                 &
               FP0   (ISEA) = UNDEF  ! FP or TP
          IF ( FLOGRD( 2, 7) ) THM   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 8) ) THS   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 9) ) THP0  (ISEA) = UNDEF
          UST   (ISEA) = UNDEF
          USTDIR(ISEA) = UNDEF
          IF ( FLOGRD( 2,10) ) HSIG  (ISEA) = UNDEF
          IF ( FLOGRD( 2,11) ) STMAXE(ISEA) = UNDEF
          IF ( FLOGRD( 2,12) ) STMAXD(ISEA) = UNDEF
          IF ( FLOGRD( 2,13) ) HMAXE (ISEA) = UNDEF
          IF ( FLOGRD( 2,14) ) HCMAXE(ISEA) = UNDEF
          IF ( FLOGRD( 2,15) ) HMAXD (ISEA) = UNDEF
          IF ( FLOGRD( 2,16) ) HCMAXD(ISEA) = UNDEF
          IF ( FLOGRD( 2,17) ) WBT   (ISEA) = UNDEF
          IF ( FLOGRD( 2,19) ) WNMEAN(ISEA) = UNDEF
          !
          IF ( FLOGRD( 3, 1) ) EF   (ISEA,:) = UNDEF
          IF ( FLOGRD( 3, 2) ) TH1M (ISEA,:) = UNDEF
          IF ( FLOGRD( 3, 3) ) STH1M(ISEA,:) = UNDEF
          IF ( FLOGRD( 3, 4) ) TH2M (ISEA,:) = UNDEF
          IF ( FLOGRD( 3, 5) ) STH2M(ISEA,:) = UNDEF
          !
          IF ( FLOGRD( 4, 1) ) PHS (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 2) ) PTP (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 3) ) PLP (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 4) ) PDIR (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 5) ) PSI (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 6) ) PWS (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 7) ) PTHP0(ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 8) ) PQP (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 9) ) PPE(ISEA,:)  = UNDEF
          IF ( FLOGRD( 4,10) ) PGW(ISEA,:)  = UNDEF
          IF ( FLOGRD( 4,11) ) PSW (ISEA,:) = UNDEF
          IF ( FLOGRD( 4,12) ) PTM1(ISEA,:) = UNDEF
          IF ( FLOGRD( 4,13) ) PT1 (ISEA,:) = UNDEF
          IF ( FLOGRD( 4,14) ) PT2 (ISEA,:) = UNDEF
          IF ( FLOGRD( 4,15) ) PEP (ISEA,:) = UNDEF
          IF ( FLOGRD( 4,16) ) PWST(ISEA  ) = UNDEF
          IF ( FLOGRD( 4,17) ) PNR (ISEA  ) = UNDEF
          !
          IF ( FLOGRD( 5, 2) ) CHARN (ISEA) = UNDEF
          IF ( FLOGRD( 5, 3) ) CGE   (ISEA) = UNDEF
          IF ( FLOGRD( 5, 4) ) PHIAW (ISEA) = UNDEF
          IF ( FLOGRD( 5, 5) ) THEN
            TAUWIX(ISEA) = UNDEF
            TAUWIY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 5, 6) ) THEN
            TAUWNX(ISEA) = UNDEF
            TAUWNY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 5, 7) ) WHITECAP(ISEA,1) = UNDEF
          IF ( FLOGRD( 5, 8) ) WHITECAP(ISEA,2) = UNDEF
          IF ( FLOGRD( 5, 9) ) WHITECAP(ISEA,3) = UNDEF
          IF ( FLOGRD( 5,10) ) WHITECAP(ISEA,4) = UNDEF
          !
          IF ( FLOGRD( 6, 1) ) THEN
            SXX   (ISEA) = UNDEF
            SYY   (ISEA) = UNDEF
            SXY   (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 2) ) THEN
            TAUOX (ISEA) = UNDEF
            TAUOY (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 3) ) BHD(ISEA) = UNDEF
          IF ( FLOGRD( 6, 4) ) PHIOC (ISEA) = UNDEF
          IF ( FLOGRD( 6, 5) ) THEN
            TUSX  (ISEA) = UNDEF
            TUSY  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 6) ) THEN
            USSX  (ISEA) = UNDEF
            USSY  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 7) ) THEN
            PRMS  (ISEA) = UNDEF
            TPMS  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 8) ) US3D(ISEA,:) = UNDEF
          IF ( FLOGRD( 6, 9) ) P2SMS(ISEA,:) = UNDEF
          IF ( FLOGRD( 6, 10) ) TAUICE(ISEA,:) = UNDEF
          IF ( FLOGRD( 6, 11) ) PHICE(ISEA) = UNDEF
          IF ( FLOGRD( 6, 12) ) USSP(ISEA,:) = UNDEF
          IF ( FLOGRD( 6, 13) ) THEN
            TAUOCX(ISEA) = UNDEF
            TAUOCY(ISEA) = UNDEF
          END IF
          !
          IF ( FLOGRD( 7, 1) ) THEN
            ABA   (ISEA) = UNDEF
            ABD   (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 7, 2) ) THEN
            UBA   (ISEA) = UNDEF
            UBD   (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 7, 3) ) BEDFORMS(ISEA,:) = UNDEF
          IF ( FLOGRD( 7, 4) ) PHIBBL(ISEA) = UNDEF
          IF ( FLOGRD( 7, 5) ) TAUBBL(ISEA,:) = UNDEF
          !
          IF ( FLOGRD( 8, 1) ) THEN
            MSSX  (ISEA) = UNDEF
            MSSY  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 8, 2) ) THEN
            MSCX  (ISEA) = UNDEF
            MSCY  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 8, 3) ) MSSD (ISEA) = UNDEF
          IF ( FLOGRD( 8, 4) ) MSCD (ISEA) = UNDEF
          IF ( FLOGRD( 8, 5) ) QP   (ISEA) = UNDEF
          IF ( FLOGRD( 8, 6) ) QKK  (ISEA) = UNDEF
          IF ( FLOGRD( 8, 7) ) SKEW (ISEA) = UNDEF
          IF ( FLOGRD( 8, 8) ) EMBIA1(ISEA) = UNDEF
          IF ( FLOGRD( 8, 9) ) EMBIA2(ISEA) = UNDEF
          !
          IF ( FLOGRD( 9, 1) ) DTDYN (ISEA) = UNDEF
          IF ( FLOGRD( 9, 2) ) FCUT  (ISEA) = UNDEF
          IF ( FLOGRD( 9, 3) ) CFLXYMAX(ISEA) = UNDEF
          IF ( FLOGRD( 9, 4) ) CFLTHMAX(ISEA) = UNDEF
          IF ( FLOGRD( 9, 5) ) CFLKMAX(ISEA) = UNDEF
          !
        END IF
        !
        IF ( MAPSTA(MAPSF(ISEA,2),MAPSF(ISEA,1)) .EQ. 2 ) THEN
          !
          IF ( FLOGRD( 5, 4) ) PHIAW (ISEA) = UNDEF
          IF ( FLOGRD( 5, 5) ) THEN
            TAUWIX(ISEA) = UNDEF
            TAUWIY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 5, 6) ) THEN
            TAUWNX(ISEA) = UNDEF
            TAUWNY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 5, 7) ) WHITECAP(ISEA,1) = UNDEF
          IF ( FLOGRD( 5, 8) ) WHITECAP(ISEA,2) = UNDEF
          IF ( FLOGRD( 5, 9) ) WHITECAP(ISEA,3) = UNDEF
          IF ( FLOGRD( 5,10) ) WHITECAP(ISEA,4) = UNDEF
          !
          IF ( FLOGRD( 6, 2) )THEN
            TAUOX (ISEA) = UNDEF
            TAUOY (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 4) ) PHIOC (ISEA) = UNDEF
          !
          IF ( FLOGRD( 7, 3) ) BEDFORMS(ISEA,:) = UNDEF
          IF ( FLOGRD( 7, 4) ) PHIBBL(ISEA) = UNDEF
          IF ( FLOGRD( 7, 5) ) TAUBBL(ISEA,:) = UNDEF
          !
        END IF
        !
      END DO
      !
    ELSE
      IF (.NOT.DINIT) CALL W3DIMW ( IGRD, NDSE, NDST, .TRUE. )
      IF (.NOT.AINIT) CALL W3DIMA ( IGRD, NDSE, NDST, .TRUE. )
    END IF
    !
    ! Actual output  ---------------------------------------------- *
    DO IFI=1, NOGRP
      DO IFJ=1, NGRPP

        IF ( FLOGRD(IFI,IFJ) ) THEN
          !
#ifdef W3_T
          WRITE (NDST,9010) FLOGRD(IFI,IFJ), IDOUT(IFI,IFJ)
#endif
          !
          IF ( WRITE ) THEN
            !
            !     Section 1)
            !
            IF ( IFI .EQ. 1 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) DW(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'DW:', DW(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) CX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'CX:', CX(1:NSEA)
#endif
              WRITE ( NDSOG ) CY(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'CY:', CY(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 3 ) THEN
              DO ISEA=1, NSEA
#ifdef W3_SMC
                !!Li  Rotate map-east wind in Arctic part back to local east.  JGLi02Feb2016
                IF( ARCTC .AND. (ISEA .GT. NGLO) ) THEN
                  UDARC = UD(ISEA) - ANGARC(ISEA - NGLO)*DERA
                  UD(ISEA) = MOD(TPI + UDARC, TPI)
                ENDIF
#endif
                IF (UA(ISEA) .NE.UNDEF) THEN
                  AUX1(ISEA) = UA(ISEA)*COS(UD(ISEA))
                  AUX2(ISEA) = UA(ISEA)*SIN(UD(ISEA))
                ELSE
                  AUX1(ISEA) = UNDEF
                  AUX2(ISEA) = UNDEF
                END IF
              END DO
              WRITE ( NDSOG ) AUX1
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'AUX1 (UA*cos(UD)):', AUX1
#endif
              WRITE ( NDSOG ) AUX2
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'AUX2 (UA*sin(UD)):', AUX2
#endif
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) AS(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'AS:', AS(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) WLV(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'WLV:', WLV(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 6 ) THEN
              WRITE ( NDSOG ) ICE(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'ICE:', ICE(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 7 ) THEN
              WRITE ( NDSOG ) BERG(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'BERG:', BERG(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 8 ) THEN
              DO ISEA=1, NSEA
#ifdef W3_SMC
                !!Li  Rotate map-east momentum in Arctic part back to local east.  JGLi02Feb2016
                IF( ARCTC .AND. (ISEA .GT. NGLO) ) THEN
                  UDARC = TAUADIR(ISEA) - ANGARC(ISEA - NGLO)*DERA
                  TAUADIR(ISEA) = MOD(TPI + UDARC, TPI)
                ENDIF
#endif
                IF (TAUA(ISEA) .NE.UNDEF) THEN
                  AUX1(ISEA) = TAUA(ISEA)*COS(TAUADIR(ISEA))
                  AUX2(ISEA) = TAUA(ISEA)*SIN(TAUADIR(ISEA))
                ELSE
                  AUX1(ISEA) = UNDEF
                  AUX2(ISEA) = UNDEF
                END IF
              END DO
              WRITE ( NDSOG ) AUX1
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'AUX1 (TAUA*cos(TAUADIR)):', AUX1
#endif
              WRITE ( NDSOG ) AUX2
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'AUX2 (TAUA*sin(TAUADIR)):', AUX2
#endif
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 9 ) THEN
              WRITE ( NDSOG ) RHOAIR(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'RHOAIR:', RHOAIR(1:NSEA)
#endif
#ifdef W3_BT4
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 10 ) THEN
              WRITE ( NDSOG ) SED_D50(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'SED_D50:', SED_D50(1:NSEA)
#endif
#endif
#ifdef W3_IS2
            ELSE IF (IFI .EQ. 1 .AND. IFJ .EQ. 11 ) THEN
              WRITE (NDSOG ) ICEH(1:NSEA)
#ifdef W3_ASCII
              WRITE (NDSOA,* ) 'ICEH:', ICEH(1:NSEA)
#endif
            ELSE IF (IFI .EQ. 1 .AND. IFJ .EQ. 12 ) THEN
              WRITE (NDSOG ) ICEF(1:NSEA)
#ifdef W3_ASCII
              WRITE (NDSOA,* ) 'ICEF:', ICEF(1:NSEA)
#endif
#endif
#ifdef W3_SETUP
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 13 ) THEN
              WRITE ( NDSOG ) ZETA_SETUP(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'ZETA_SETUP:', ZETA_SETUP(1:NSEA)
#endif
#endif

              !
              !     Section 2)
              !
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) HS(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'HS:', HS(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) WLM(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'WLM:', WLM(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) T02(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'T02:', T02(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) T0M1(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'T0M1:', T0M1(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) T01(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'T01:', T01(1:NSEA)
#endif
            ELSE IF ( (IFI .EQ. 2 .AND. IFJ .EQ. 6) .OR.         &
                 (IFI .EQ. 2 .AND. IFJ .EQ. 18) ) THEN
              ! Note: TP output is derived from FP field.
              WRITE ( NDSOG ) FP0(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'FP0:', FP0(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 7 ) THEN
              WRITE ( NDSOG ) THM(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'THM:', THM(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 8 ) THEN
              WRITE ( NDSOG ) THS(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'THS:', THS(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 9 ) THEN
              WRITE ( NDSOG ) THP0(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'THP0:', THP0(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 10 ) THEN
              WRITE ( NDSOG ) HSIG(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'HSIG:', HSIG(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 11 ) THEN
              WRITE ( NDSOG ) STMAXE(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'STMAXE:', STMAXE(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 12 ) THEN
              WRITE ( NDSOG ) STMAXD(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'STMAXD:', STMAXD(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 13 ) THEN
              WRITE ( NDSOG ) HMAXE(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'HMAXE:', HMAXE(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 14 ) THEN
              WRITE ( NDSOG ) HCMAXE(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'HCMAXE:', HCMAXE(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 15 ) THEN
              WRITE ( NDSOG ) HMAXD(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'HMAXD:', HMAXD(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 16 ) THEN
              WRITE ( NDSOG ) HCMAXD(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'HCMAXD:', HCMAXD(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 17 ) THEN
              WRITE ( NDSOG ) WBT(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'WBT:', WBT(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 19 ) THEN
              WRITE ( NDSOG ) WNMEAN(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'WNMEAN:', WNMEAN(1:NSEA)
#endif
              !
              !     Section 3)
              !
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) EF(1:NSEA,E3DF(2,1):E3DF(3,1))
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'EF:', EF(1:NSEA,E3DF(2,1):E3DF(3,1))
#endif
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) TH1M(1:NSEA,E3DF(2,2):E3DF(3,2))
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TH1M:', TH1M(1:NSEA,E3DF(2,2):E3DF(3,2))
#endif
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) STH1M(1:NSEA,E3DF(2,3):E3DF(3,3))
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'STH1M:', STH1M(1:NSEA,E3DF(2,3):E3DF(3,3))
#endif
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) TH2M(1:NSEA,E3DF(2,4):E3DF(3,4))
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TH2M:', TH2M(1:NSEA,E3DF(2,4):E3DF(3,4))
#endif
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) STH2M(1:NSEA,E3DF(2,5):E3DF(3,5))
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'STH2M:', STH2M(1:NSEA,E3DF(2,5):E3DF(3,5))
#endif
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 6) THEN
              WRITE ( NDSOG ) WN(1:NK,1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'WN:', WN(1:NK,1:NSEA)
#endif
              !
              !     Section 4)
              !
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) PHS(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PHS:', PHS(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) PTP(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PTP:', PTP(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) PLP(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PLP:', PLP(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) PDIR(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PDIR:', PDIR(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) PSI(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PSI:', PSI(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 6 ) THEN
              WRITE ( NDSOG ) PWS(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PWS:', PWS(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 7 ) THEN
              WRITE ( NDSOG ) PTHP0(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PTHP0:', PTHP0(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 8  ) THEN
              WRITE ( NDSOG ) PQP(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PQP:', PQP(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 9  ) THEN
              WRITE ( NDSOG ) PPE(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PPE:', PPE(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 10 ) THEN
              WRITE ( NDSOG ) PGW(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PGW:', PGW(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 11 ) THEN
              WRITE ( NDSOG ) PSW(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PSW:', PSW(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 12 ) THEN
              WRITE ( NDSOG ) PTM1(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PTM1:', PTM1(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 13 ) THEN
              WRITE ( NDSOG ) PT1(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PT1:', PT1(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 14 ) THEN
              WRITE ( NDSOG ) PT2(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PT2:', PT2(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 15 ) THEN
              WRITE ( NDSOG ) PEP(1:NSEA,0:NOSWLL)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PEP:', PEP(1:NSEA,0:NOSWLL)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 16 ) THEN
              WRITE ( NDSOG ) PWST(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PWST:', PWST(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 17 ) THEN
              WRITE ( NDSOG ) PNR(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PNR:', PNR(1:NSEA)
#endif
              !
              !     Section 5)
              !
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 1 ) THEN
              DO ISEA=1, NSEA
                IX     = MAPSF(ISEA,1)
                IY     = MAPSF(ISEA,2)
                IF ( MAPSTA(IY,IX) .EQ. 1 ) THEN
                  AUX1(ISEA) = UST(ISEA) * ASF(ISEA) *        &
                       COS(USTDIR(ISEA))
                  AUX2(ISEA) = UST(ISEA) * ASF(ISEA) *        &
                       SIN(USTDIR(ISEA))
                ELSE
                  AUX1(ISEA) = UNDEF
                  AUX2(ISEA) = UNDEF
                END IF
              END DO
              WRITE ( NDSOG ) AUX1
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'AUX1 (UST*ASF*cos(USTDIR)):', AUX1
#endif
              WRITE ( NDSOG ) AUX2
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'AUX2 (UST*ASF*sin(USTDIR)):', AUX2
#endif
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) CHARN(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'CHARN:', CHARN(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) CGE(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'CGE:', CGE(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) PHIAW(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PHIAW:', PHIAW(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) TAUWIX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TAUWIX:', TAUWIX(1:NSEA)
#endif
              WRITE ( NDSOG ) TAUWIY(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TAUWIY:', TAUWIY(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 6 ) THEN
              WRITE ( NDSOG ) TAUWNX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TAUWNX:', TAUWNX(1:NSEA)
#endif
              WRITE ( NDSOG ) TAUWNY(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TAUWNY:', TAUWNY(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 7 ) THEN
              WRITE ( NDSOG ) WHITECAP(1:NSEA,1)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'WHITECAP(1):', WHITECAP(1:NSEA,1)
#endif
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 8 ) THEN
              WRITE ( NDSOG ) WHITECAP(1:NSEA,2)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'WHITECAP(2):', WHITECAP(1:NSEA,2)
#endif
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 9 ) THEN
              WRITE ( NDSOG ) WHITECAP(1:NSEA,3)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'WHITECAP(3):', WHITECAP(1:NSEA,3)
#endif
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 10 ) THEN
              WRITE ( NDSOG ) WHITECAP(1:NSEA,4)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'WHITECAP(4):', WHITECAP(1:NSEA,4)
#endif
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 11 ) THEN
              WRITE ( NDSOG ) TWS(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TWS:', TWS(1:NSEA)
#endif
              !
              !     Section 6)
              !
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) SXX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'SXX:', SXX(1:NSEA)
#endif
              WRITE ( NDSOG ) SYY(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'SYY:', SYY(1:NSEA)
#endif
              WRITE ( NDSOG ) SXY(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'SXY:', SXY(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) TAUOX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TAUOX:', TAUOX(1:NSEA)
#endif
              WRITE ( NDSOG ) TAUOY(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TAUOY:', TAUOY(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 3  ) THEN
              WRITE ( NDSOG ) BHD(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'BHD:', BHD(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) PHIOC(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PHIOC:', PHIOC(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) TUSX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TUSX:', TUSX(1:NSEA)
#endif
              WRITE ( NDSOG ) TUSY(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TUSY:', TUSY(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 6 ) THEN
              WRITE ( NDSOG ) USSX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'USSX:', USSX(1:NSEA)
#endif
              WRITE ( NDSOG ) USSY(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'USSY:', USSY(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 7 ) THEN
              WRITE ( NDSOG ) PRMS(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PRMS:', PRMS(1:NSEA)
#endif
              WRITE ( NDSOG ) TPMS(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TPMS:', TPMS(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 8 ) THEN
              WRITE ( NDSOG ) US3D(1:NSEA,   US3DF(2):US3DF(3))
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'US3D:', US3D(1:NSEA,   US3DF(2):US3DF(3))
#endif
              WRITE ( NDSOG ) US3D(1:NSEA,NK+US3DF(2):NK+US3DF(3))
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'US3D+NK:', US3D(1:NSEA,NK+US3DF(2):NK+US3DF(3))
#endif
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ.  9 ) THEN
              WRITE ( NDSOG ) P2SMS(1:NSEA,P2MSF(2):P2MSF(3))
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'P2SMS:', P2SMS(1:NSEA,P2MSF(2):P2MSF(3))
#endif
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 10 ) THEN
              WRITE ( NDSOG ) TAUICE(1:NSEA,1)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TAUICE(1):', TAUICE(1:NSEA,1)
#endif
              WRITE ( NDSOG ) TAUICE(1:NSEA,2)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TAUICE(2):', TAUICE(1:NSEA,2)
#endif
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 11 ) THEN
              WRITE ( NDSOG ) PHICE(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PHICE:', PHICE(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 12 ) THEN
              WRITE ( NDSOG ) USSP(1:NSEA,   1:USSPF(2))
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'USSP:', USSP(1:NSEA,   1:USSPF(2))
#endif
              WRITE ( NDSOG ) USSP(1:NSEA,NK+1:NK+USSPF(2))
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'USSP:', USSP(1:NSEA,NK+1:NK+USSPF(2))
#endif
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 13 ) THEN
              WRITE ( NDSOG ) TAUOCX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TAUOCX:', TAUOCX(1:NSEA)
#endif
              WRITE ( NDSOG ) TAUOCY(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TAUOCY:', TAUOCY(1:NSEA)
#endif
              !
              !     Section 7)
              !
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 1 ) THEN
              DO ISEA=1, NSEA
                IF ( ABA(ISEA) .NE. UNDEF ) THEN
                  AUX1(ISEA) = ABA(ISEA)*COS(ABD(ISEA))
                  AUX2(ISEA) = ABA(ISEA)*SIN(ABD(ISEA))
                ELSE
                  AUX1(ISEA) = UNDEF
                  AUX2(ISEA) = UNDEF
                END IF
              END DO
              WRITE ( NDSOG ) AUX1
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'AUX1 (ABA*cos(ABD)):', AUX1
#endif
              WRITE ( NDSOG ) AUX2
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'AUX2 (ABA*sin(ABD)):', AUX2
#endif
              !WRITE ( NDSOG ) ABA(1:NSEA)
              !WRITE ( NDSOG ) ABD(1:NSEA)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 2 ) THEN
              DO ISEA=1, NSEA
                IF ( UBA(ISEA) .NE. UNDEF ) THEN
                  AUX1(ISEA) = UBA(ISEA)*COS(UBD(ISEA))
                  AUX2(ISEA) = UBA(ISEA)*SIN(UBD(ISEA))
                ELSE
                  AUX1(ISEA) = UNDEF
                  AUX2(ISEA) = UNDEF
                END IF
              END DO
              WRITE ( NDSOG ) AUX1
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'AUX1 (UBA*cos(UBD)):', AUX1
#endif
              WRITE ( NDSOG ) AUX2
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'AUX2 (UBA*sin(UBD)):', AUX2
#endif
              !                    WRITE ( NDSOG ) UBA(1:NSEA)
              !                    WRITE ( NDSOG ) UBD(1:NSEA)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) BEDFORMS(1:NSEA,1)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'BEDFORMS(1):', BEDFORMS(1:NSEA,1)
#endif
              WRITE ( NDSOG ) BEDFORMS(1:NSEA,2)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'BEDFORMS(2):', BEDFORMS(1:NSEA,2)
#endif
              WRITE ( NDSOG ) BEDFORMS(1:NSEA,3)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'BEDFORMS(3):', BEDFORMS(1:NSEA,3)
#endif
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) PHIBBL(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'PHIBBL:', PHIBBL(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) TAUBBL(1:NSEA,1)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TAUBBL(1):', TAUBBL(1:NSEA,1)
#endif
              WRITE ( NDSOG ) TAUBBL(1:NSEA,2)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'TAUBBL(2):', TAUBBL(1:NSEA,2)
#endif
              !
              !     Section 8)
              !Skewness
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) MSSX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'MSSX:', MSSX(1:NSEA)
#endif
              WRITE ( NDSOG ) MSSY(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'MSSY:', MSSY(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) MSCX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'MSCX:', MSCX(1:NSEA)
#endif
              WRITE ( NDSOG ) MSCY(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'MSCY:', MSCY(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) MSSD(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'MSSD:', MSSD(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) MSCD(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'MSCD:', MSCD(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) QP(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'QP:', QP(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 6 ) THEN
              WRITE ( NDSOG ) QKK(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'QKK:', QKK(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 7 ) THEN
              WRITE ( NDSOG ) SKEW(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'SKW:', SKEW(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 8 ) THEN
              WRITE ( NDSOG ) EMBIA1(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'EMB:', EMBIA1(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 9 ) THEN
              WRITE ( NDSOG ) EMBIA2(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'EMC:', EMBIA2(1:NSEA)
#endif
              !
              !     Section 9)
              !
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) DTDYN(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'DTDYN:', DTDYN(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) FCUT(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'FCUT:', FCUT(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) CFLXYMAX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'CFLXYMAX:', CFLXYMAX(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) CFLTHMAX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'CFLTHMAX:', CFLTHMAX(1:NSEA)
#endif
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) CFLKMAX(1:NSEA)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'CFLMAX:', CFLKMAX(1:NSEA)
#endif
              !
              !     Section 10)
              !
            ELSE IF ( IFI .EQ. 10 ) THEN
              WRITE ( NDSOG ) USERO(1:NSEA,IFJ)
#ifdef W3_ASCII
              WRITE ( NDSOA,* ) 'USER0:', USERO(1:NSEA,IFJ)
#endif
              !
            END IF
            !
          ELSE
            !
            !     Start of reading ......
            !
            !     Section 1)
            !
            IF ( IFI .EQ. 1 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) DW(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) CX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) CY(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) UA(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) UD(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) AS(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) WLV(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 6 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ICE(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 7 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) BERG(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 8 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) TAUA(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) TAUADIR(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 9 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) RHOAIR(1:NSEA)
#ifdef W3_BT4
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 10 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) SED_D50(1:NSEA)
#endif
#ifdef W3_IS2
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 11 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ICEH(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 12 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ICEF(1:NSEA)
#endif
#ifdef W3_SETUP
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 13 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ZETA_SETUP(1:NSEA)
#endif
              !
              !     Section 2)
              !
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) HS(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) WLM(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) T02(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) T0M1(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) T01(1:NSEA)
            ELSE IF ( (IFI .EQ. 2 .AND. IFJ .EQ. 6) .OR.       &
                 (IFI .EQ. 2 .AND. IFJ .EQ. 18) ) THEN
              ! Note: TP output is derived from FP field.
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) FP0(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 7 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) THM(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 8 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) THS(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 9 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   THP0(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 10 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   HSIG(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 11 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   STMAXE(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 12 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   STMAXD(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 13 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   HMAXE(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 14 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   HCMAXE(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 15 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   HMAXD(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 16 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   HCMAXD(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 17 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) WBT(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 19 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   WNMEAN(1:NSEA)
              !
              !     Section 3)
              !
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   EF(1:NSEA,E3DF(2,1):E3DF(3,1))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TH1M(1:NSEA,E3DF(2,2):E3DF(3,2))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   STH1M(1:NSEA,E3DF(2,3):E3DF(3,3))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TH2M(1:NSEA,E3DF(2,4):E3DF(3,4))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   STH2M(1:NSEA,E3DF(2,5):E3DF(3,5))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 6) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
                   WN(1:NK,1:NSEA)
              !
              !     Section 4)
              !
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PHS(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PTP(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PLP(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PDIR(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PSI(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 6 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PWS(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 7 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PTHP0(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 8  ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PQP(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 9  ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PPE(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 10 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PGW(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 11 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PSW(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 12 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PTM1(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 13 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PT1(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 14 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PT2(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 15 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PEP(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 16) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PWST(1:NSEA)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 17) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) PNR(1:NSEA)
              !
              !     Section 5)
              !
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)          &
                   UST(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)          &
                   USTDIR(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   CHARN(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) CGE(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PHIAW(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUWIX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUWIY(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 6 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUWNX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUWNY(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 7 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   WHITECAP(1:NSEA,1)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 8 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   WHITECAP(1:NSEA,2)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 9 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   WHITECAP(1:NSEA,3)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 10 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   WHITECAP(1:NSEA,4)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 11 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TWS(1:NSEA)
              !
              !     Section 6)
              !
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) SXX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) SYY(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) SXY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUOX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUOY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   BHD(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PHIOC(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TUSX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TUSY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 6 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   USSX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   USSY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 7 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PRMS(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TPMS(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 8 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
                   US3D(1:NSEA,US3DF(2):US3DF(3))
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
                   US3D(1:NSEA,NK+US3DF(2):NK+US3DF(3))
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ.  9 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)     &
                   P2SMS(1:NSEA,P2MSF(2):P2MSF(3))
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 10 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUICE(1:NSEA,1)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUICE(1:NSEA,2)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 11 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PHICE(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 12 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
                   USSP(1:NSEA,1:USSPF(2))
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
                   USSP(1:NSEA,NK+1:NK+USSPF(2))
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 13 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUOCX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUOCY(1:NSEA)

              !
              !     Section 7)
              !
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ABA(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ABD(1:NSEA)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) UBA(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) UBD(1:NSEA)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   BEDFORMS(1:NSEA,1)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   BEDFORMS(1:NSEA,2)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   BEDFORMS(1:NSEA,3)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PHIBBL(1:NSEA)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUBBL(1:NSEA,1)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUBBL(1:NSEA,2)
              !
              !     Section 8)
              !
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   MSSX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   MSSY(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   MSCX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   MSCY(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   MSSD(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   MSCD(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) QP(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 6 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) QKK(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 7 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) SKEW(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 8 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) EMBIA1(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 9 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) EMBIA2(1:NSEA)
              !
              !     Section 9)
              !
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   DTDYN(1:NSEA)
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   FCUT(1:NSEA)
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   CFLXYMAX(1:NSEA)
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   CFLTHMAX(1:NSEA)
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   CFLKMAX(1:NSEA)
              !
              !     Section 10)
              !
            ELSE IF ( IFI .EQ. 10 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   USERO(1:NSEA,IFJ)
            END IF
            !
            ! End of test on WRITE/READ:
            !
          END IF
          !
          ! End of test on  FLOGRD(IFI,IFJ):
          !
        END IF
        !
        ! End of IFI and IFJ loops
        !
      END DO
    END DO
    !
    ! Flush the buffers for write
    !
    IF ( WRITE ) CALL FLUSH ( NDSOG )
    !
    IF(OFILES(1) .EQ. 1) CLOSE(NDSOG)
    !
#ifdef W3_MPI
    CALL W3SETA ( IGRD, NDSE, NDST )
#endif
    !
    RETURN
    !
    ! Escape locations read errors
    !
800 CONTINUE
    WRITE (NDSE,1000) IERR
    CALL EXTCDE ( 41 )
    !
801 CONTINUE
    WRITE (NDSE,1001)
    CALL EXTCDE ( 42 )
    !
802 CONTINUE
    WRITE (NDSE,1002) IERR
    CALL EXTCDE ( 43 )
    !
803 CONTINUE
    IOTST  = -1
#ifdef W3_T
    WRITE (NDST,9020)
#endif
    RETURN
    !
    ! Formats
    !
900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
         '     ILEGAL INXOUT VALUE: ',A/)
901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
         '     MIXED READ/WRITE, LAST REQUEST: ',A/)
902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
         '     ILEGAL IDSTR, READ : ',A/                        &
         '                  CHECK : ',A/)
903 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
         '     ILEGAL VEROGR, READ : ',A/                       &
         '                   CHECK : ',A/)
904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
         '     DIFFERENT NUMBER OF FIELDS, FILE :',I8,I8/       &
         '                              PROGRAM :',I8,I8/)
905 FORMAT (/' *** WAVEWATCH III WARNING IN W3IOGO :'/              &
         '     ILEGAL GNAME, READ : ',A/                        &
         '                  CHECK : ',A/)
906 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
         '     ILEGAL NOSWLL, READ : ',I4/                      &
         '                   CHECK : ',I4/)
    !
    !  999 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
    !               '     PLEASE UPDATE FIELDS !!! '/)
    !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO : '/               &
         '     ERROR IN OPENING FILE'/                          &
         '     IOSTAT =',I5/)
1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO : '/               &
         '     PREMATURE END OF FILE'/)
1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO : '/               &
         '     ERROR IN READING FROM FILE'/                     &
         '     IOSTAT =',I5/)
    !
#ifdef W3_T
9000 FORMAT (' TEST W3IOGO : IPASS =',I4,' INXOUT = ',A,          &
         ' WRITE = ',L1,' UNIT =',I3/                         &
         '               IGRD =',I3,' FEXT = ',A)
9001 FORMAT (' TEST W3IOGO : OPENING NEW FILE [',A,']')
9002 FORMAT (' TEST W3IOGO : TEST PARAMETERS:'/                   &
         '       IDSTR :  ',A/                                &
         '      VEROGR :  ',A/                                &
         '       GNAME :  ',A/                                &
         '        NSEA :',I6/                                 &
         '       NX,NY : ',I9,I12/                            &
         '       UNDEF : ',F8.2)
9003 FORMAT (' TEST W3IOGO : TIME  :',I9.8,I7.6/                  &
         '               FLAGS :',20L2,1X,20L2/               &
         '                      ',20L2,2X,20L2/               &
         '                      ',20L2,2X,20L2/               &
         '                      ',20L2,2X,20L2/               &
         '                      ',20L2,2X,20L2)
9010 FORMAT (' TEST W3IOGO : PROC = ',L1,' FOR ',A)
9020 FORMAT (' TEST W3IOGO : END OF FILE REACHED')
#endif
    !/
    !/ End of W3IOGO ----------------------------------------------------- /
    !/
  END SUBROUTINE W3IOGO
  !/
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Output Stokes drift related parameters.
  !>
  !> @details This code is built for the purpose of outputting Stokes
  !>  drift related parameters that can be utilized to obtain full
  !>  Stokes drift profiles external to the wave model.
  !>
  !> @param[in] A  Input spectra, left in par list to change shape.
  !> @param[in] USS_SWITCH  Switch if computing US3D (spectral) or USSP (partitions).
  !>
  !> @author H. L. Tolman  @date 10-Jan-2017
  !>
  SUBROUTINE CALC_U3STOKES ( A , USS_SWITCH )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         10-Jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    10-Jan-2017 : Separate Stokes drift calculation  ( version 6.01 )
    !/
    !  1. Purpose :
    !
    !     This code is built for the purpose of outputting Stokes drift
    !       related parameters that can be utilized to obtain full
    !       Stokes drift profiles external to the wave model.
    !
    !     Option 1: USS_SWITCH == 1
    !               This method is for outputing the Stokes drift frequency
    !               spectrum for spectral frequency bands as defined by the
    !               WW3 computation spectral frequency grid.
    !               Output Quantity: Stokes drift frequency spectrum [m/s/Hz]
    !                                X and Y componenets.
    !
    !     Option 2: USS_SWITCH == 2
    !               This method is for outputing the surface Stokes drift
    !               for a specified frequency partition/band of the
    !               wave spectrum.  These partitions do not need to be
    !               matched to WW3's computation spectral frequency grid,
    !               and will rather sum the contributions of the WW3 bands
    !               into the output partition.  The partitions are defined
    !               in the ww3_grid.inp namelist section.
    !               Output Quantity: Stokes drift surface velocity [m/s]
    !                                X and Y components
    !                                For each partition (up to 25).
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.   I   Input spectra. Left in par list to change
    !                          shape.
    !      USS_SWITCH  I   I   Switch if computing US3D (spectral) or USSP
    !                           (partitions)
    !     ----------------------------------------------------------------
    !
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
    !     None.
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
    !     !/OMPG  OpenMP compiler directive for loop splitting.
    !
    !     !/S     Enable subroutine tracing.
    !     !/T     Test output.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS, ONLY: TPIINV, GRAV, TPI
    USE W3GDATMD,  ONLY: DDEN, DSII, XFR, SIG, NK, NTH, NSEAl,    &
         ECOS, ESIN, US3DF, USSPF, USSP_WN
    USE W3ADATMD,  ONLY: CG, WN, DW
    USE W3ADATMD,  ONLY: USSX, USSY,  US3D, USSP
    USE W3ODATMD, ONLY: IAPROC, NAPROC
    USE W3PARALL, ONLY: INIT_GET_ISEA
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)        :: A(NTH,NK,0:NSEAL)
    INTEGER, INTENT(IN)     :: USS_SWITCH
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IK, ITH, ISEA, JSEA
    INTEGER                 :: IKST, IKFI, IB
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: FACTOR, FKD,KD
    REAL                    :: ABX(NSEAL), ABY(NSEAL), USSCO
    REAL                    :: MINDIFF
    INTEGER                 :: Spc2Bnd(NK)
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'CALC_U3STOKES')
#endif
    !
    ! 1.  Initialize storage arrays -------------------------------------- *
    !
    ! 2.  Integral over discrete part of spectrum ------------------------ *
    !
    !Two options ----------------------------------------------------|
    ! USS_SWITCH == 1 -> Old option, Stokes drift integrated in same |
    !                    wavenumber bands as model integrates.       |
    ! USS_SWITCH == 2 -> New option, Stokes drift integrated in a    |
    !                    defined number (NP) of user specified       |
    !                    partitions, where NP and the frequency      |
    !                    ranges for each partition can be user       |
    !                    defined at run-time.                        |
    !----------------------------------------------------------------|

    if (USS_SWITCH==1) then
      IKST=US3DF(2)!Start at US3DF(2)
      IKFI=US3DF(3)!End at US3DF(3)
    ELSEif (USS_SWITCH==2) then
      IKST=1  ! Start at 1
      IKFI=NK ! End at NK
    ENDIF

    ! Initialize US3D/USSP
    IF (USS_SWITCH.eq.1) then
      US3D(:,:)=0.0
    ELSEIF (USS_SWITCH.eq.2) then
      USSP(:,:)=0.0
    ENDIF
    DO IK=IKST,IKFI   !1, NK
      !
      ! 2.a Initialize energy in band
      !
      ABX    = 0.
      ABY    = 0.
      !
      ! 2.b Integrate energy in band
      !
      DO ITH=1, NTH
        !
#ifdef W3_OMPG
        !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
        !
        DO JSEA=1, NSEAL
          ABX(JSEA)  = ABX(JSEA) + A(ITH,IK,JSEA)*ECOS(ITH)
          ABY(JSEA)  = ABY(JSEA) + A(ITH,IK,JSEA)*ESIN(ITH)
        END DO
        !
#ifdef W3_OMPG
        !$OMP END PARALLEL DO
#endif
        !
      END DO
      !
      ! 2.c Finalize integration over band and update mean arrays
      !
      !
#ifdef W3_OMPG
      !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,FACTOR,KD,FKD,USSCO,MINDIFF,IB)
#endif
      !
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        FACTOR       = DDEN(IK) / CG(IK,ISEA)
        !
        ! Deep water limits
        !
        KD    = MAX ( 0.001 , WN(IK,ISEA) * DW(ISEA) )
        IF ( KD .LT. 6. ) THEN
          FKD       = FACTOR / SINH(KD)**2
          USSCO=FKD*SIG(IK)*WN(IK,ISEA)*COSH(2.*KD)
        ELSE
          USSCO=FACTOR*SIG(IK)*2.*WN(IK,ISEA)
        END IF
        !
        !
        !USSX(JSEA)  = USSX(JSEA) + ABX(JSEA)*USSCO
        !USSY(JSEA)  = USSY(JSEA) + ABY(JSEA)*USSCO
        !
        ! Fills the 3D Stokes drift spectrum array or surface Stokes partitions
        !
        IF (USS_SWITCH==1) THEN
          !Old method fills into WW3 bands
          IF (IK.GE.US3DF(2).and.IK.LE.US3DF(3)) then
            US3D(JSEA,IK)    =  ABX(JSEA)*USSCO/(DSII(IK)*TPIINV)
            US3D(JSEA,NK+IK) =  ABY(JSEA)*USSCO/(DSII(IK)*TPIINV)
          ENDIF
        ELSEIF (USS_SWITCH==2) THEN
          ! Match each spectral component to the nearest partition
          MINDIFF=1.e8
          Spc2BND(IK) = 1
          MINDIFF=abs(USSP_WN(1)-WN(IK,ISEA))
          DO IB=2,USSPF(2)
            IF (MinDiff .gt. abs(USSP_WN(IB)-WN(IK,ISEA))) then
              Spc2BND(IK) = IB
              MinDiff = abs(USSP_WN(IB)-WN(IK,ISEA))
            ENDIF
          ENDDO
          !Put spectral energey into whichever band central wavenumber fits in
          USSP(JSEA,Spc2Bnd(IK))    =  USSP(JSEA,Spc2Bnd(IK)) + ABX(JSEA)*USSCO
          USSP(JSEA,NK+Spc2BND(IK)) =  USSP(JSEA,NK+Spc2Bnd(IK)) + ABY(JSEA)*USSCO
        ENDIF
      END DO
#ifdef W3_OMPG
      !$OMP END PARALLEL DO
#endif
    END DO
    !
    RETURN
    !
    !/ End of CALC_U3STOKES
    !----------------------------------------------------- /
    !/
  END SUBROUTINE CALC_U3STOKES
  !/
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Estimate the dominant wave breaking probability b_T.
  !>
  !> @details Estimate the dominant wave breaking probability b_T based on
  !>  the empirical parameterization proposed by Babanin et al. (2001).
  !>
  !> @verbatim
  !>     From their Fig. 12, we have
  !>
  !>         b_T = 85.1 * [(εp - 0.055) * (1 + H_s/d)]^2.33,
  !>
  !>     where ε is the significant steepness of the spectral peak, H_s is
  !>     the significant wave height, d is the water depth.
  !>
  !>     For more details, please see
  !>         Banner et al.  2000: JPO,      30,  3145 -  3160.
  !>         Babanin et al. 2001: JGR, 106(C6), 11569 - 11676.
  !> @endverbatim
  !>
  !> @param[in] A  Input wave action spectra N(j, θ, k).
  !>
  !> @author Q. Liu  @date 24-Aug-2018
  !>
  SUBROUTINE CALC_WBT (A)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           Q. Liu                  |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         24-Aug-2018 |
    !/                  +-----------------------------------+
    !/
    !/    24-Aug-2018 : Origination.                       ( version 6.06 )
    !/
    !  1. Purpose :
    !
    !     Estimate the dominant wave breaking probability b_T based on
    !     the empirical parameterization proposed by Babanin et al. (2001).
    !     From their Fig. 12, we have
    !
    !         b_T = 85.1 * [(εp - 0.055) * (1 + H_s/d)]^2.33,
    !
    !     where ε is the significant steepness of the spectral peak, H_s is
    !     the significant wave height, d is the water depth.
    !
    !     For more details, please see
    !         Banner et al.  2000: JPO,      30,  3145 -  3160.
    !         Babanin et al. 2001: JGR, 106(C6), 11569 - 11676.
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.   I   Input wave action spectra N(j, θ, k)
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3OUTG    Subr. Public   Calculate mean parameters.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     None.
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
    USE W3DISPMD, ONLY: WAVNU1
    USE W3ADATMD, ONLY: U10, U10D, WBT
    USE W3ADATMD, ONLY: CG, WN, DW
    USE W3GDATMD, ONLY: NK, NTH, NSEAL, SIG, ESIN, ECOS, DTH, DSII, &
         FTE, XFR, MAPSF, MAPSTA, DMIN
    USE W3GDATMD, ONLY: BTBETA
    USE W3PARALL, ONLY: INIT_GET_ISEA
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)     :: A  (NTH, NK, 0:NSEAL)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !
    INTEGER              :: FPOPT = 0
    !
    INTEGER              :: IK, ITH, ISEA, JSEA, IKM, IKL, IKH, IX, IY
    REAL                 :: TDPT, TU10, TUDIR, SINU, COSU, TC, TFORCE
    REAL                 :: ESIG(NK) ! E(σ)
    REAL                 :: FACTOR, ET, HS, ETP, HSP, SIGP, KP, &
         CGP, WSTP
    REAL                 :: XL, XH, XL2, XH2, EL, EH, DENOM
    REAL                 :: TWBT
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'CALC_WBT')
#endif
    !
    DO JSEA = 1, NSEAL
      ! JSEA 2 ISEA
      CALL INIT_GET_ISEA(ISEA, JSEA)
      !
      ! check the status of this grid point [escape if this point is excluded]
      !
      IX = MAPSF(ISEA,1)
      IY = MAPSF(ISEA,2)
      IF ( MAPSTA(IY,IX) .LE. 0 ) CYCLE
      !
      ! Wind info. is required to select wind sea partition from the wave
      ! spectrum. Two wind velocities are availabe:
      ! - U10 & U10D   (w3adatmd)
      ! - UST & USTDIR (w3wdatmd)
      !     * U10D & USTDIR are not really the same when swell are present.
      !
      ! Following Janssen et al. (1989) and Bidlot (2001), spectral components
      ! are considered to be subject to local wind forcing when
      !
      !          c / [U cos(θ - φ)] < β,
      !
      ! where c is the phase velocity c = σ/k, φ is the wind direction, U is
      ! the wind speed U10, (sometimes approximated by U10≅ 28 * ust), β is
      ! the constant forcing parameter with β∈ [1.0, 2.0]. By default, we use
      ! β = 1.2(Bidlot 2001).
      !
      TDPT  = MAX(DW(ISEA), DMIN)          ! water depth d
      TU10  = U10(ISEA)                    ! wind velocity U10
      TUDIR = U10D(ISEA)                   ! wind direction φ (rad)
      SINU  = SIN(TUDIR)                   ! sinφ
      COSU  = COS(TUDIR)                   ! cosφ
      !
      ESIG  = 0.                           ! E(σ)
      ET    = 0.                           ! ΣE(σ)δσ
      ETP   = 0.                           ! ΣE(σ)δσ at peak only
      !
      DO IK = 1, NK
        TC     = SIG(IK) / WN(IK, ISEA)  ! phase velocity c=σ/k
        FACTOR = SIG(IK) / CG(IK, ISEA)  ! σ / cg
        FACTOR = FACTOR * DTH            ! σ / cg * δθ
        !
        DO ITH = 1, NTH
          TFORCE = TC - TU10 * (COSU*ECOS(ITH)+SINU*ESIN(ITH)) &
               * BTBETA

          IF (TFORCE .LT. 0.) THEN ! wind sea component
            ESIG(IK) = ESIG(IK) + A(ITH, IK, JSEA) * FACTOR
          ENDIF
        ENDDO ! ITH
        !
      ENDDO ! IK
      !
      ! ESIG is E(σ) of the wind sea after filtration of any background swell.
      ! Now we need to get Hs & σp for the wind sea spectrum.
      ! FTE    = 0.25 * SIG(NK) * DTH * SIG(NK) [ww3_grid.ftn]
      !
      ET = SUM(ESIG * DSII)
      ET = ET + ESIG(NK) * FTE / (DTH * SIG(NK))  ! FTE: add tail
      HS = 4. * SQRT(MAX(0., ET))                 ! Hs
      !
      ! Get σp from E(σ)
      !
      ! Here we have tried three different ways to calculate FP:
      !
      ! FPOPT = 0: fp defined by Young (1999, p. 239)
      ! FPOPT = 1: parabolic fit around the discrete peak frequency, as used
      !            by ww3_outp
      ! FPOPT = 2: discrete peak frequency
      !
      ! When the discrete peak frequency is used:
      ! * For XFR = 1.1, the **discrete** peak region [0.7σp, 1.3σp] will be
      !     {0.75, 0.83, 0.91, 1., 1.1, 1.21, 1.33}σp,
      ! * and for XFR = 1.07, the **discrete** peak region becomes
      !     {0.71, 0.76, 0.82, 0.87, 0.93, 1., 1.07, 1.14, 1.23, 1.31}σp.
      !
      ! Thus, a good approximation to the range [0.7σp, 1.3σp] is guranteed
      ! by each XFR. I however found using the discrete peak frequency yielded
      ! step-wise results. According to my test, the smoothest results were
      ! obtained with FPOPT = 0. For simplicity, the δσ values (DSII) are
      ! not modified.
      !
      IKM   = MAXLOC(ESIG, 1)                     ! index for σp
      !
      IF (FPOPT .EQ. 0) THEN
        !             FP defined in Ian's book
        SIGP = SUM(ESIG**4. * SIG(1:NK) * DSII) /  &
             MAX(1E-10, SUM(ESIG**4. * DSII))
        !
      ELSE IF (FPOPT .EQ. 1) THEN
        !             Parabolic fit around the discrete peak (ww3_outp.ftn)
        XL    = 1./XFR - 1.
        XH    = XFR - 1.
        XL2   = XL**2.
        XH2   = XH**2.
        IKL   = MAX (  1 , IKM-1 )
        IKH   = MIN ( NK , IKM+1 )
        EL    = ESIG(IKL) - ESIG(IKM)
        EH    = ESIG(IKH) - ESIG(IKM)
        DENOM = XL*EH - XH*EL
        SIGP  = SIG(IKM) * (1. + 0.5 * ( XL2*EH - XH2*EL) &
             / SIGN (MAX(ABS(DENOM), 1.E-15), DENOM)) ! σp
        !
      ELSE IF (FPOPT .EQ. 2) THEN
        !             Discrete peak (Give stepwise results, not used by default)
        SIGP  = SIG(IKM)
      ENDIF
      !
      ! kp from σp (linear dispersion)
      !
      ! N(k, θ) at first step is zero →  σp=0 → floating divided by zero error
      IF (SIGP < 1E-6) SIGP = SIG(NK)     ! Hsp & b_T will be still 0.
      !
      CALL WAVNU1 (SIGP, TDPT, KP, CGP)
      !
      !                         { /1.3σp         }1/2
      ! peak wave height Hp = 4 { |      E(σ) dσ }
      !                         { /0.7σp         }
      !
      DO IK = 1, NK
        IF ( (SIG(IK) >= 0.7 * SIGP) .AND. &
             (SIG(IK) <= 1.3 * SIGP) ) THEN
          ETP = ETP + ESIG(IK) * DSII(IK)
        ENDIF
      ENDDO ! IK
      HSP = 4. * SQRT(MAX(0., ETP))
      !
      ! significant steepness of the peak region εp
      !
      WSTP = 0.5 * KP * HSP
      !
      ! Dominant wave breaking b_T
      !
      TWBT = 85.1 * (MAX(0.0, WSTP - 0.055) * (1 + HS/TDPT))**2.33
      WBT(JSEA) = MIN(1.0, TWBT)
      !
    ENDDO ! JSEA
    !/
    !/ End of  CALC_WBT -------------------------------------------------- /
    !/
  END SUBROUTINE CALC_WBT
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief  Computation of second order harmonics and
  !>         relevant tables for the altimeter corrections
  !>
  !> @param[in]    NKHF   Extended number of frequencies.
  !> @param[out]   FAC0   2nd order coef correction.
  !> @param[out]   FAC1   2nd order coef correction.
  !> @param[out]   FAC2   2nd order coef correction.
  !> @param[out]   FAC3   2nd order coef correction.
  !>
  !> @author P. Janssen  @date 29-Mar-2024
  !>
      SUBROUTINE SECONDHH(NKHF,FAC0,FAC1,FAC2,FAC3)
!----------------------------------------------------------------

!**** *SECONDHH* - COMPUTATION OF SECOND ORDER HARMONICS AND
!                  RELEVANT TABLES FOR THE ALTIMETER CORRECTIONS.

!     P.A.E.M. JANSSEN

!     PURPOSE.
!     ---------

!          COMPUTE SECOND HARMONICS

!**   INTERFACE.
!     ----------

!          *CALL* *SECONDHH*

!     METHOD.
!     -------

!          SEE REFERENCE.

!     EXTERNALS.
!     ----------

!         VMIN_D
!         VPLUS_D          

!     REFERENCES.
!     -----------

!          V E ZAKHAROV(1967)

!-------------------------------------------------------------------

!-------------------------------------------------------------------
USE CONSTANTS, ONLY: GRAV, TPI
USE W3GDATMD,  ONLY: NK, NTH, XFR, SIG, TH, DTH, ECOS, ESIN
      IMPLICIT NONE
 !     REAL(KIND=4) :: VMIN_D,VPLUS_D



      INTEGER, INTENT(IN) :: NKHF
      REAL(KIND=4), DIMENSION(NTH,NTH,NKHF,NKHF), INTENT(OUT)  :: FAC0, FAC1, FAC2, FAC3
      REAL(KIND=4), PARAMETER   :: FRATIO = 1.1


      INTEGER :: M, K1, M1, K2, M2

      REAL(KIND=4), PARAMETER :: DEL1=1.0E-8
      REAL(KIND=4), PARAMETER :: ZCONST = 0.0281349

      !REAL(KIND=4) :: VMIN_D, VPLUS_D
      REAL(KIND=4) :: CO1
      REAL(KIND=4) :: XK1, XK1SQ, XK2, XK2SQ, XK3
      REAL(KIND=4) :: COSDIFF
      REAL(KIND=4) :: X12, X13, X32, OM1, OM2, OM3, F1, F2, F3
      REAL(KIND=4) :: VM, VP
      REAL(KIND=4) :: DELOM1, DELOM2
      REAL(KIND=4) :: DELOM321, DELOM312
      REAL(KIND=4) :: C22, S22

      REAL(KIND=4), DIMENSION(NTH,NTH,NKHF,NKHF) :: B
      REAL(KIND=4), DIMENSION(:), ALLOCATABLE:: FAK, SIGHF, DFIMHF



 
!-----------------------------------------------------------------------




!*    1. INITIALISE RELEVANT QUANTITIES.

      ALLOCATE(FAK(NKHF))
      ALLOCATE(SIGHF(NKHF))
      ALLOCATE(DFIMHF(NKHF))

      SIGHF(1)  = SIG(1)
      DO M=2,NKHF
        SIGHF(M) = XFR*SIGHF(M-1)
      ENDDO

      DO M=1,NKHF
         FAK(M) = (SIGHF(M))**2/GRAV
      ENDDO

      CO1 = 0.5*(XFR-1.)*DTH
      DFIMHF(1) = CO1*SIGHF(1)
      DO M=2,NKHF-1
         DFIMHF(M)=CO1*(SIGHF(M)+SIGHF(M-1))
      ENDDO
      DFIMHF(NKHF)=CO1*SIGHF(NKHF-1)

      DO M2=1,NKHF
        XK2 = FAK(M2)
        XK2SQ = FAK(M2)**2
        DO  M1=1,NKHF
          XK1 = FAK(M1)
          XK1SQ = FAK(M1)**2
          DO K1=1,NTH
            DO K2=1,NTH
              COSDIFF = COS(TH(K1)-TH(K2))
              X12 = XK1*XK2*COSDIFF
              XK3 = XK1SQ + XK2SQ +2.0*X12 +DEL1
              XK3 = SQRT(XK3)
              X13 = XK1SQ+X12
              X32 = X12+XK2SQ
              OM1 = SQRT(GRAV*XK1)
              OM2 = SQRT(GRAV*XK2)
              OM3 = SQRT(GRAV*XK3)
              F1 = SQRT(XK1/(2.0*OM1))
              F2 = SQRT(XK2/(2.0*OM2))
              F3 = SQRT(XK3/(2.0*OM3))
              VM = TPI*VMIN_D(XK3,XK1,XK2,X13,X32,X12,OM3,OM1,OM2)
              VP = TPI*VPLUS_D(-XK3,XK1,XK2,-X13,-X32,X12,OM3,OM1,OM2)
              DELOM1 = OM3-OM1-OM2+DEL1
              DELOM2 = OM3+OM1+OM2+DEL1
              FAC0(K1,K2,M1,M2) = -F3/(F1*F2)*(VM/(DELOM1)+             &
     &                            VP/(DELOM2))
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      DO M2=1,NKHF
        XK2 = FAK(M2)
        XK2SQ = FAK(M2)**2
        DO  M1=1,NKHF
          XK1 = FAK(M1)
          XK1SQ = FAK(M1)**2
          DO K1=1,NTH
            DO K2=1,NTH
              COSDIFF = COS(TH(K1)-TH(K2))
              X12 = XK1*XK2*COSDIFF
              XK3 = XK1SQ + XK2SQ - 2.*X12 + DEL1
              XK3 = SQRT(XK3)
              X13 = XK1SQ-X12
              X32 = X12-XK2SQ
              OM1 = SQRT(GRAV*XK1)
              OM2 = SQRT(GRAV*XK2)
              OM3 = SQRT(GRAV*XK3)+DEL1
              F1 = SQRT(XK1/(2.0*OM1))
              F2 = SQRT(XK2/(2.0*OM2))
              F3 = SQRT(ABS(XK3)/(2.0*OM3))
              VM = TPI*VMIN_D(XK1,XK3,XK2,X13,X12,X32,OM1,OM3,OM2)
              VP = TPI*VMIN_D(XK2,-XK3,XK1,-X32,X12,-X13,OM2,OM3,OM1)
              DELOM321 = OM3+OM2-OM1+DEL1
              DELOM312 = OM3+OM1-OM2+DEL1
              B(K1,K2,M1,M2) = -F3/(F1*F2)*(VM/(DELOM321)+              &
     &                         VP/(DELOM312))
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      DO M2=1,NKHF
        XK2SQ = FAK(M2)**2
        DO M1=1,NKHF
          XK1SQ = FAK(M1)**2
          DO K2=1,NTH
            DO K1=1,NTH
              C22 = FAC0(K1,K2,M1,M2)+B(K1,K2,M1,M2)
              S22 = B(K1,K2,M1,M2)-FAC0(K1,K2,M1,M2)
              FAC1(K1,K2,M1,M2) =                                       &
     &             (XK1SQ*ECOS(K1)**2 + XK2SQ*ECOS(K2)**2)*C22        &
     &             -FAK(M1)*FAK(M2)*ECOS(K1)*ECOS(K2)*S22
              FAC2(K1,K2,M1,M2) =                                       &
     &             (XK1SQ*ESIN(K1)**2 + XK2SQ*ESIN(K2)**2)*C22        &
     &             -FAK(M1)*FAK(M2)*ESIN(K1)*ESIN(K2)*S22
              FAC3(K1,K2,M1,M2) =                                       &
     &             (XK1SQ*ESIN(K1)*ECOS(K1) +                         &
     &              XK2SQ*ESIN(K2)*ECOS(K2))*C22                      &
     &             -FAK(M1)*FAK(M2)*ECOS(K1)*ESIN(K2)*S22
              FAC0(K1,K2,M1,M2) = C22
            ENDDO
          ENDDO
        ENDDO
      ENDDO


     CONTAINS

!-----------------------------------------------------------------------

     REAL(KIND=4) FUNCTION VMIN_D(XI,XJ,XK,XIJ,XIK,XJK,XOI,XOJ,XOK)
     
!     PETER JANSSEN

!     PURPOSE.
!     --------

!              GIVES NONLINEAR TRANSFER COEFFICIENT FOR THREE
!              WAVE INTERACTIONS OF DEEP-WATER WAVES IN THE
!              IDEAL CASE OF NO CURRENT. (CF.ZAKHAROV)

!     INTERFACE.
!     ----------
!              *VMIN_D(XI,XJ,XK)*
!                      *XI*  - WAVE NUMBER
!                      *XJ*  - WAVE NUMBER
!                      *XK*  - WAVE NUMBER
!     METHOD.
!     -------
!              NONE

!     EXTERNALS.
!     ----------
!              NONE.


!***  1. DETERMINE NONLINEAR TRANSFER.
!     --------------------------------
      IMPLICIT NONE
      REAL, INTENT(IN) :: XI, XJ, XK, XIJ, XIK, XJK, XOI, XOJ, XOK
      REAL :: RI, RJ, RK, OI, OJ, OK, SQIJK, SQIKJ, SQJKI

      RI=ABS(XI)+DEL1
      RJ=ABS(XJ)+DEL1
      RK=ABS(XK)+DEL1
      OI=XOI+DEL1
      OJ=XOJ+DEL1
      OK=XOK+DEL1
      SQIJK=SQRT(OI*OJ*RK/(OK*RI*RJ))
      SQIKJ=SQRT(OI*OK*RJ/(OJ*RI*RK))
      SQJKI=SQRT(OJ*OK*RI/(OI*RJ*RK))
      VMIN_D=ZCONST*( (XIJ-RI*RJ)*SQIJK + (XIK-RI*RK)*SQIKJ             &
     &                + (XJK+RJ*RK)*SQJKI )

      END FUNCTION VMIN_D      

!-----------------------------------------------------------------------

      REAL(KIND=4) FUNCTION VPLUS_D(XI,XJ,XK,XIJ,XIK,XJK,XOI,XOJ,XOK)

!***  *VPLUS_D*  DETERMINES THE NONLINEAR TRANSFER COEFFICIENT FOR THREE
!                WAVE INTERACTIONS OF DEEP-WATER WAVES.

!     PETER JANSSEN

!     PURPOSE.
!     --------

!              GIVES NONLINEAR TRANSFER COEFFICIENT FOR THREE
!              WAVE INTERACTIONS OF GRAVITY-CAPILLARY WAVES IN THE
!              IDEAL CASE OF NO CURRENT. (CF.ZAKHAROV)

!     INTERFACE.
!     ----------
!              *VPLUS_D(XI,XJ,XK)*
!                        *XI*  - WAVE NUMBER
!                        *XJ*  - WAVE NUMBER
!                        *XK*  - WAVE NUMBER
!     METHOD.
!     -------
!              NONE

!     EXTERNALS.
!     ----------
!              NONE.



!***  1. DETERMINE NONLINEAR TRANSFER.
!     --------------------------------

      IMPLICIT NONE
      REAL, INTENT(IN) :: XI, XJ, XK, XIJ, XIK, XJK, XOI, XOJ, XOK
      REAL :: RI, RJ, RK, OI, OJ, OK, SQIJK, SQIKJ, SQJKI

      RI=ABS(XI)+DEL1
      RJ=ABS(XJ)+DEL1
      RK=ABS(XK)+DEL1
      OI=XOI+DEL1
      OJ=XOJ+DEL1
      OK=XOK+DEL1
      SQIJK=SQRT(OI*OJ*RK/(OK*RI*RJ))
      SQIKJ=SQRT(OI*OK*RJ/(OJ*RI*RK))
      SQJKI=SQRT(OJ*OK*RI/(OI*RJ*RK))
      VPLUS_D=ZCONST*( (XIJ+RI*RJ)*SQIJK + (XIK+RI*RK)*SQIKJ            &
     &               + (XJK+RJ*RK)*SQJKI )

      END FUNCTION VPLUS_D
!     -----------------------------------------------------------------

      END SUBROUTINE SECONDHH
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief  Determines skewness paramters in order to obtain
  !>         correction on altimeter wave height
  !>
  !> @details Evaluate deviations from gaussianity following the work 
  !>          of Srokosz and Longuet-Higgins. For second order
  !>          corrections to surface elevation, the approach of
  !>          Zaharov has been used.
  !>
  !> @param[in]    NKHF   Extended number of frequencies.
  !> @param[out]   FAC0   2nd order coef correction.
  !> @param[out]   FAC1   2nd order coef correction.
  !> @param[out]   FAC2   2nd order coef correction.
  !> @param[out]   FAC3   2nd order coef correction.
  !>
  !> @author P. Janssen  @date 29-Mar-2024
  !>
      SUBROUTINE SKEWNESS(A)

!--------------------------------------------------------------------

!*****SKEWNESS** COMPUTES PARAMETERS OF THE NEARLY-GAUSSIAN
!             DISTRIBUTION OF OCEAN WAVES AT A FIXED GRID POINT.

!     P.JANSSEN JULY 1997

!     PURPOSE
!     -------
!             DETERMINES SKEWNESS PARAMETERS IN ORDER TO OBTAIN
!             CORRECTION ON ALTIMETER WAVE HEIGHT.

!     INTERFACE
!     ---------
!             *CALL* *SKEWNESS(IU06,F1,NCOLL,XKAPPA1,DELH_ALT)*



!     METHOD
!     ------
!             EVALUATE DEVIATIONS FROM GAUSSIANITY FOLLOWING THE WORK
!             OF SROKOSZ AND LONGUET-HIGGINS. FOR SECOND ORDER
!             CORRECTIONS TO SURFACE ELEVATION THE APPROACH OF
!             ZAKHAROV HAS BEEN USED.

!     EXTERNALS
!     ---------
!             NONE

!     REFERENCES
!     ----------
!             M.A. SROKOSZ, J.G.R.,91,995-1006(1986)
!             V.E. ZAKHAROV, HAMILTONIAN APPROACH(1967)
!--------------------------------------------------------------------



!--------------------------------------------------------------------
!      *TH*        REAL      DIRECTIONS IN RADIANS.
USE CONSTANTS, ONLY: GRAV, TPI, TPIINV
USE W3GDATMD,  ONLY: NK, NTH, XFR, SIG, DTH, ECOS, ESIN, NSEAL
USE W3PARALL,  ONLY: INIT_GET_ISEA
USE W3ADATMD,  ONLY: CG, SKEW, EMBIA1, EMBIA2


    IMPLICIT NONE

    REAL, INTENT(IN)        :: A(NTH,NK,0:NSEAL)

    INTEGER :: NKHF
    REAL(KIND=4), DIMENSION(:,:,:,:) , ALLOCATABLE:: FAC0,FAC1,FAC2,FAC3

    INTEGER :: M, K, M1, K1, M2, K2, I, J
    INTEGER :: MSTART, JSEA
   
    REAL(KIND=4) :: CONX, DELTA
    REAL(KIND=4) :: FH, DELF, XK1
    REAL(KIND=4) :: XPI, XPJ, XPK, XN, XFAC, CO1
    REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: F2
    REAL(KIND=4), DIMENSION(0:3,0:2,0:2) :: XMU, XLAMBDA
    REAL(KIND=4), DIMENSION(:) , ALLOCATABLE::  SIGHF, DFIMHF, FAK

! ----------------------------------------------------------------------

    NKHF=NK+13 ! same offset as in ECWAM 

    ALLOCATE(FAC0(NTH,NTH,NKHF,NKHF))
    ALLOCATE(FAC1(NTH,NTH,NKHF,NKHF))
    ALLOCATE(FAC2(NTH,NTH,NKHF,NKHF))
    ALLOCATE(FAC3(NTH,NTH,NKHF,NKHF))
      
    CALL SECONDHH(NKHF,FAC0,FAC1,FAC2,FAC3)

    ALLOCATE(F2(NTH,NKHF))
    ALLOCATE(SIGHF(NKHF), DFIMHF(NKHF), FAK(NKHF)) 

!     1. COMPUTATION OF FREQUENCY-DIRECTION INCREMENT
!     -----------------------------------------------

    MSTART = 1


#ifdef W3_OMPG
        !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
    DO JSEA=1, NSEAL
      XMU(:,:,:) = 0.0
      DO K=1,NTH
        DO M=1,NK
          CONX = TPIINV / SIG(M) * CG(M,JSEA)
          F2(K,M)=A(K,M,JSEA)/ CONX
          END DO
        END DO

      SIGHF(1)  = SIG(1)
      DO M=2,NKHF
        SIGHF(M) = XFR*SIGHF(M-1)
      ENDDO

      CO1 = 0.5*(XFR-1.)*DTH*TPIINV 
      DFIMHF(1) = CO1*SIGHF(1)        ! this is DF*DTH
      DO M=2,NKHF-1
        DFIMHF(M)=CO1*(SIGHF(M)+SIGHF(M-1))
      ENDDO
      DFIMHF(NKHF)=CO1*SIGHF(NKHF-1)

      DO M=1,NKHF
        FAK(M) = (SIGHF(M))**2/GRAV
      ENDDO

! Deals with the tail ... 
      DO M=NK+1,NKHF
        FH=(SIGHF(NK)/SIGHF(M))**5
        DO K=1,NTH
          F2(K,M)=F2(K,NK)*FH
        ENDDO
      ENDDO

!     2. COMPUTATION OF THE SKEWNESS COEFFICIENTS
!     --------------------------------------------

      DO M1=MSTART,NKHF
        DO M2=MSTART,NKHF
          DO K1=1,NTH
            DO K2=1,NTH
              DELF = DFIMHF(M1)*DFIMHF(M2)*F2( K1,M1)*F2(K2,M2)
              XMU(3,0,0) = XMU(3,0,0)+3.0*FAC0(K1,K2,M1,M2)*DELF
              XMU(1,2,0) = XMU(1,2,0)+FAC1(K1,K2,M1,M2)*DELF
              XMU(1,0,2) = XMU(1,0,2)+FAC2(K1,K2,M1,M2)*DELF
              XMU(1,1,1) = XMU(1,1,1)+FAC3(K1,K2,M1,M2)*DELF
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      DO K1=1,NTH
        DO M1=MSTART,NKHF
          XK1 = FAK(M1)**2
          DELF = DFIMHF(M1)*F2(K1,M1)
          XMU(2,0,0) = XMU(2,0,0) + DELF
          XMU(0,2,0) = XMU(0,2,0) + XK1*ECOS(K1)**2*DELF
          XMU(0,0,2) = XMU(0,0,2) + XK1*ESIN(K1)**2*DELF
          XMU(0,1,1) = XMU(0,1,1) + XK1*ECOS(K1)*ESIN(K1)*DELF
        ENDDO
      ENDDO


!     3. COMPUTATION OF THE NORMALISED SKEWNESS COEFFICIENTS
!     ------------------------------------------------------

      DO I=0,3
        XPI = 0.5*FLOAT(I)
        DO J=0,2
          XPJ = 0.5*FLOAT(J)
          DO K=0,2
            XPK = 0.5*FLOAT(K)
            XN = XMU(2,0,0)**XPI*XMU(0,2,0)**XPJ*XMU(0,0,2)**XPK  ! denom in Srokosz eq. 11
            IF (XN .NE. 0) THEN
              XLAMBDA(I,J,K) = XMU(I,J,K)/XN
            ELSE
              XLAMBDA(I,J,K) = 0
            END IF
          END DO
        END DO
      END DO
      IF ( XMU(2,0,0) .GT. 1.E-7 ) THEN
        SKEW(JSEA)=XLAMBDA(3,0,0)
        DELTA = ( XLAMBDA(1,2,0) + XLAMBDA(1,0,2)           &
                  - 2.0*XLAMBDA(0,1,1)*XLAMBDA(1,1,1) )/    &
                   (1.0 - XLAMBDA(0,1,1)**2)             ! this is called gamma eq. 20 
        EMBIA1(JSEA)=-0.125*DELTA                             ! EM Bias coefficient 
        EMBIA2(JSEA)=-0.125*XLAMBDA(3,0,0)/3.0           ! tracker bias (least squares only) 	
      END IF
    END DO  ! end of loop on JSEA
        !
#ifdef W3_OMPG
        !$OMP END PARALLEL DO
#endif

    DEALLOCATE(FAC0,FAC1,FAC2,FAC3)
    DEALLOCATE(F2,SIGHF,DFIMHF,FAK) 


      END SUBROUTINE SKEWNESS

END MODULE W3IOGOMD
