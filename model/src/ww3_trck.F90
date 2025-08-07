!> @file
!> @brief Contains program W3TRCK for converting track output.
!>
!> @author H. L. Tolman @date 05-Mar-2014
!
#include "w3macros.h"

!/ ------------------------------------------------------------------- /
!> @brief Convert direct access track output file to free-format
!>  readable sequential file.
!>
!> @details Info read from track_o.ww3, written to track.ww3.
!>
!> @author H. L. Tolman @date 05-Mar-2014
PROGRAM W3TRCK
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         05-Mar-2014 |
  !/                  +-----------------------------------+
  !/
  !/    14-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
  !/    21-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  !/    25-Jan-2001 : Flat grid version                   ( version 2.06 )
  !/    20-Aug-2003 : Sequential file version             ( version 3.04 )
  !/    29-Jun-2006 : Adding file name preamble.          ( version 3.09 )
  !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
  !/                  (W. E. Rogers & T. J. Campbell, NRL)
  !/    05-Mar-2014 : Now calls W3SETG for pointer def.   ( version 4.18 )
  !/    04-Jul-2025 : Remove labelled statements          ( version X.XX )
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Convert direct access track output file to free-format
  !     readable sequential file.
  !
  !  2. Method :
  !
  !     Info read from track_o.ww3, written to track.ww3.
  !
  !  3. Parameters :
  !
  !  4. Subroutines used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      W3NMOD    Subr. W3GDATMD Set number of model.
  !      W3NOUT    Subr. W3ODATMD Set number of model for output.
  !     ----------------------------------------------------------------
  !
  !  5. Called by :
  !
  !     None, stand-alone program.
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
  !       !/S    Enable subroutine tracing.
  !
  ! 10. Source code :
  !
  !/ ------------------------------------------------------------------- /
  USE W3GDATMD, ONLY : W3NMOD, W3SETG, FLAGLL, XFR
  USE W3ODATMD, ONLY : W3NOUT, W3SETO, FNMPRE
  USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE, EXTOPN, EXTIOF
#ifdef W3_S
  USE W3SERVMD, ONLY : STRACE
#endif
  USE W3TIMEMD, ONLY : STME21
  !
  USE W3ODATMD, ONLY: NDSO, NDSE, NDST
  use constants, only: file_endian
  !
  IMPLICIT NONE
  !/
  !/ ------------------------------------------------------------------- /
  !/ Local parameters
  !/
  CHARACTER*34, PARAMETER ::                                      &
       IDTST  = 'WAVEWATCH III TRACK OUTPUT SPECTRA'
  !
  INTEGER                 :: NDSI, NDSINP,                        &
       NDSOUT, NDSTRC, NTRACE, NK, NTH,     &
       NSPEC, IERR, MK, MTH,                &
       NREC, ILOC, ISPEC, TIME(2), TTST(2), &
       ILAST, NZERO, IK, ITH, IWZERO, ICH,  &
       IWDTH, J
#ifdef W3_S
  INTEGER, SAVE           :: IENT   = 0
#endif
  INTEGER                 :: LINELN = 81
  REAL                    :: TH1, DTH, X, Y, DW, CX, CY, WX, WY,  &
       UST, AS, VALUE
  REAL                    :: SCALE  = 0.001
  REAL                    :: FACTOR
  REAL, ALLOCATABLE       :: SIG(:), DSIP(:), SPEC(:,:)
  CHARACTER               :: COMSTR*1, IDSTR*34, TSTSTR*3,        &
       STIME*23, STRING*81, EMPTY*81,       &
       PART*9, ZEROS*9, TRCKID*32
  !
  DATA EMPTY(01:40) / '                                        ' /
  DATA EMPTY(41:81) / '                                         ' /
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !
  ! 1.a Initialize data structure
  !
  CALL W3NMOD ( 1, 6, 6 )
  CALL W3SETG ( 1, 6, 6 )
  CALL W3NOUT (    6, 6 )
  CALL W3SETO ( 1, 6, 6 )
  !
  ! 1.b IO set-up.
  !
  NDSI   = 10
  NDSINP = 11
  NDSOUT = 51
  !
  NDSTRC =  6
  NTRACE = 10
  CALL ITRACE ( NDSTRC, NTRACE )
  !
#ifdef W3_S
  CALL STRACE ( IENT, 'W3TRCK' )
#endif
  !
  WRITE (NDSO,900)
  !
  J      = LEN_TRIM(FNMPRE)
  OPEN (NDSI,FILE=FNMPRE(:J)//'ww3_trck.inp',STATUS='OLD',IOSTAT=IERR)
  IF (IERR.NE.0) CALL EXTOPN(NDSE,IERR, 'W3TRCK', 'INPUT', 5)
  READ (NDSI,'(A)',IOSTAT=IERR) COMSTR
  IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','INPUT',6)
  IF (COMSTR.EQ.' ') COMSTR = '$'
  WRITE (NDSO,901) COMSTR
  !
  CALL NEXTLN ( COMSTR , NDSI , NDSE )
  READ (NDSI,*,IOSTAT=IERR) NK, NTH
  IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','INPUT',6)
  NSPEC  = NK * NTH
  WRITE (NDSO,902) NK, NTH
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 2.  Open and test input data file
  !
  WRITE (NDSO,920)
  !
  OPEN (NDSINP,FILE=FNMPRE(:J)//'track_o.ww3',form='UNFORMATTED', convert=file_endian, &
       STATUS='OLD',IOSTAT=IERR)
  IF (IERR.NE.0) CALL EXTOPN(NDSE,IERR,'W3TRCK','INPUT DATA',1)
  READ (NDSINP,IOSTAT=IERR) IDSTR, FLAGLL, MK, MTH, XFR
  IF (IERR.GT.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','INPUT DATA',2)
  !
  IF ( FLAGLL ) THEN
    FACTOR  = 1.
  ELSE
    FACTOR  = 1.E-3
  END IF
  !
  IF ( IDSTR .NE. IDTST ) THEN
    WRITE (NDSE,1010) IDSTR, IDTST
    CALL EXTCDE ( 5 )
  END IF
  IF ( NK.NE.MK .OR. NTH.NE.MTH ) THEN
    WRITE (NDSE,1011) MK, MTH, NK, NTH
    CALL EXTCDE ( 6 )
  END IF

  ALLOCATE ( SIG(MK), DSIP(MK), SPEC(MK,MTH) )
  !
  READ (NDSINP,IOSTAT=IERR) TH1, DTH, SIG, DSIP
  IF (IERR.GT.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','INPUT DATA',2)
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 3.  Open output file and prepare
  !
  WRITE (NDSO,930)
  !
  OPEN (NDSOUT,FILE=FNMPRE(:J)//'track.ww3',FORM='FORMATTED',IOSTAT=IERR)
  IF (IERR.NE.0) CALL EXTOPN(NDSE,IERR,'W3TRCK','OUTPUT',3)
  !
  WRITE (NDSOUT,980,IOSTAT=IERR) IDSTR
  IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','OUTPUT',4)
  WRITE (NDSOUT,981,IOSTAT=IERR) MK, MTH, TH1, DTH
  IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','OUTPUT',4)
  WRITE (NDSOUT,982,IOSTAT=IERR) SIG
  IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','OUTPUT',4)
  WRITE (NDSOUT,983,IOSTAT=IERR) DSIP
  IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','OUTPUT',4)
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 4.  Process data
  !
  ILOC    = 0
  ISPEC   = 0
  READ (NDSINP,IOSTAT=IERR) TTST
  IF (IERR.GT.0) THEN
    CALL EXTIOF(NDSE,IERR,'W3TRCK','INPUT DATA',2)
  ELSE IF(IERR.EQ.0) THEN
    BACKSPACE (NDSINP)
    WRITE (NDSO,940)
    !
    DO
      !
      ! 4.a Read/write basic data
      !
      READ (NDSINP,IOSTAT=IERR) TIME, X, Y, TSTSTR,  &
           TRCKID
      IF (IERR.GT.0) THEN
        CALL EXTIOF(NDSE,IERR,'W3TRCK','INPUT DATA',2)
      ELSE IF(IERR.LT.0) THEN
        EXIT
      END IF
      IF ( FLAGLL ) THEN
        WRITE (NDSOUT,984,IOSTAT=IERR)                      &
             TIME, FACTOR*X, FACTOR*Y, TSTSTR, TRCKID
        IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','OUTPUT',4)
      ELSE
        WRITE (NDSOUT,974,IOSTAT=IERR)                      &
             TIME, FACTOR*X, FACTOR*Y, TSTSTR, TRCKID
        IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','OUTPUT',4)
      END IF
      !
      IF ( TIME(1).EQ.TTST(1) .AND. TIME(2).EQ.TTST(2) ) THEN
        ILOC = ILOC + 1
        IF ( TSTSTR .EQ. 'SEA' ) ISPEC = ISPEC + 1
      ENDIF
      IF ( TIME(1).NE.TTST(1) .OR. TIME(2).NE.TTST(2) ) THEN
        CALL STME21 ( TTST , STIME )
        WRITE (NDSO,941) STIME, ILOC, ISPEC
        ILOC    = 1
        ISPEC   = 0
        IF ( TSTSTR .EQ. 'SEA' ) ISPEC = ISPEC + 1
        TTST(1) = TIME(1)
        TTST(2) = TIME(2)
      ENDIF
      !
      ! 4.b Check if sea point
      !
      IF ( TSTSTR .NE. 'SEA' ) CYCLE
      !
      ! 4.c Read all data
      !
      READ (NDSINP,IOSTAT=IERR) DW, CX, CY, WX, WY, UST, AS,  &
           SPEC
      IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','INPUT DATA',2)
      IF ( UST .LT. 0. ) UST = -1.0
      !
      ! 4.d Write the basic stuff
      !
      WRITE (NDSOUT,985,IOSTAT=IERR)                          &
           DW, CX, CY, WX, WY, UST, AS, SCALE
      IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','OUTPUT',4)
      !
      ! 4.e Start of integer packing
      !
      STRING = EMPTY
      ILAST  = 0
      NZERO  = 0
      !
      ! 4.e.1 Loop over spectrum
      !
      DO IK=1, NK
        DO ITH=1, NTH
          VALUE  = MAX ( 0.1 , 1.1*SPEC(IK,ITH)/SCALE )
          IWDTH  = 2 + MAX( 0 , INT( ALOG10(VALUE) ) )
          !
          ! 4.e.2 Put value in string and test overflow
          !
          IF ( IWDTH .GT. 9 ) THEN
            IWDTH   = 9
            PART    = ' 99999999'
          ELSE
            WRITE (PART,987) NINT(SPEC(IK,ITH)/SCALE)
            IF ( PART(11-IWDTH:11-IWDTH) .EQ. ' ' )                 &
                 IWDTH   = IWDTH - 1
          ENDIF
          !
          ! 4.e.3 It's a zero, wait with writing
          !
          IF ( PART(8:9) .EQ. ' 0' ) THEN
            NZERO  = NZERO + 1
          ELSE
            !
            ! 4.e.4 It's not a zero, write unwritten zeros
            !
            IF ( NZERO .NE. 0 ) THEN
              IF ( NZERO .EQ. 1 ) THEN
                ZEROS  = '        0'
                IWZERO = 2
              ELSE
                WRITE (ZEROS,'(I7,A2)') NZERO, '*0'
                IWZERO = 4
                DO
                  ICH    = 10 - IWZERO
                  IF ( ZEROS(ICH:ICH) .NE. ' ' ) THEN
                    IWZERO = IWZERO + 1
                  ELSE
                    EXIT
                  ENDIF
                END DO
              ENDIF
              IF ( ILAST+IWZERO .GT. LINELN ) THEN
                WRITE (NDSOUT,986,IOSTAT=IERR)          &
                     STRING(2:ILAST)
                IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','OUTPUT',4)
                STRING = EMPTY
                ILAST  = 0
              ENDIF
              STRING(ILAST+1:ILAST+IWZERO) =                      &
                   ZEROS(10-IWZERO:9)
              ILAST  = ILAST + IWZERO
              NZERO  = 0
            ENDIF
            !
            ! 4.e.5 It's not a zero, put in string
            !
            IF ( ILAST+IWDTH .GT. LINELN ) THEN
              WRITE (NDSOUT,986,IOSTAT=IERR)              &
                   STRING(2:ILAST)
              IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','OUTPUT',4)
              STRING = EMPTY
              ILAST  = 0
            ENDIF
            !
            STRING(ILAST+1:ILAST+IWDTH) = PART(10-IWDTH:9)
            ILAST  = ILAST + IWDTH
            !
          ENDIF
          !
        END DO
      END DO
      !
      ! ..... End of loop over spectrum (4.e.1)
      !
      ! 4.e.6 Write trailing zeros
      !
      IF ( NZERO .NE. 0 ) THEN
        IF ( NZERO .EQ. 1 ) THEN
          ZEROS  = '        0'
          IWZERO = 2
        ELSE
          WRITE (ZEROS,'(I7,A2)') NZERO, '*0'
          IWZERO = 4
          DO
            ICH    = 10 - IWZERO
            IF ( ZEROS(ICH:ICH) .NE. ' ' ) THEN
              IWZERO = IWZERO + 1
            ELSE
              EXIT
            ENDIF
          END DO
        ENDIF
        IF ( ILAST+IWZERO .GT. LINELN ) THEN
          WRITE (NDSOUT,986,IOSTAT=IERR)                  &
               STRING(2:ILAST)
          IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','OUTPUT',4)
          STRING = EMPTY
          ILAST  = 0
        ENDIF
        STRING(ILAST+1:ILAST+IWZERO) = ZEROS(10-IWZERO:9)
        ILAST  = ILAST + IWZERO
        NZERO  = 0
      ENDIF
      !
      ! 4.e.7 Write last line
      !
      IF ( ILAST .NE. 0 ) THEN
        WRITE (NDSOUT,986,IOSTAT=IERR) STRING(2:ILAST)
        IF (IERR.NE.0) CALL EXTIOF(NDSE,IERR,'W3TRCK','OUTPUT',4)
      ENDIF
    END DO
  END IF
  !
  ! 4.f All data done, write last batch info
  !
  CALL STME21 ( TTST , STIME )
  WRITE (NDSO,941) STIME, ILOC, ISPEC
  !
  WRITE (NDSO,999)
  !
  ! Formats
  !
900 FORMAT (/15X,'    *** WAVEWATCH III Track output post.***    '/ &
       15X,'==============================================='/)
901 FORMAT ( '  Comment character is ''',A,''''/)
902 FORMAT ( '  Spectral grid size is ',I3,' by ',I3//              &
       '  Opening files : '/                              &
       ' -----------------------------------------------')
920 FORMAT ( '     Input file ...')
930 FORMAT ( '     Output file ...')
940 FORMAT (/'  Processing data : '/                                &
       ' -----------------------------------------------')
941 FORMAT ( '     ',A,' :',I6,' points and',I6,'  spectra.')
  !
980 FORMAT (A)
981 FORMAT (2I6,2E13.5)
982 FORMAT (7E11.4)
983 FORMAT (7E11.4)
984 FORMAT (I8.8,I7.6,2F9.3,2X,A3,2X,A32)
974 FORMAT (I8.8,I7.6,2(F9.2,'E3'),2X,A3,2X,A32)
985 FORMAT (F8.1,2F6.2,2F8.2,f9.5,f7.2,E12.5)
986 FORMAT (A)
987 FORMAT (I9)
  !
999 FORMAT (/'  End of program '/                                   &
       ' ========================================='/          &
       '         WAVEWATCH III Track output '/)
  !
1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRCK : '/               &
       '     UNEXPECTED ID STRING IN INPUT : ',A/             &
       '                         SHOULD BE : ',A/)
  !
1011 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRCK : '/               &
       '     UNEXPECTED SPECTRAL DIMENSIONS : ',2I4/          &
       '                          SHOULD BE : ',2I4/)
  !/
  !/ End of W3TRCK ----------------------------------------------------- /
  !/
END PROGRAM W3TRCK
