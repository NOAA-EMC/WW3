      PROGRAM W3ADC
C/
C/                  +-----------------------------------+
C/                  |           H. L. Tolman            |
C/                  |                        FORTRAN 77 |
C/                  | Last update :         03-Feb-2020 |
C/                  +-----------------------------------+
C/
C/    05-Jan-2001 : Origination
C/    03-Feb-2020 : Added ability to process multiple   ( version 7.00 )
C/                  switches on a single line. Chris Bunney, UKMO
C/
C/    Version to preprocess FORTRAN 90 free format code.
C/
C  1. Purpose :
C
C     Pre-processing of FORTRAN files by switching on and off of
C     selected lines and by including COMMONs.
C
C                                  - Based on ADCOM by N. Booij,
C                                    Delft University of Technology.
C                                  - Refinement of WWADC
C                                  - FORTRAN 90 compatible
C
C  2. Method :
C
C     From standard input the following data are read:
C       - Test indicator and compress indicator     ( 1 line  ).
C       - File name of the input and output code    ( 1 line  ).
C       - Switches to be turned on                  ( 1 line  ).
C       - Include string and file name              ( n lines ).
C
C     where :
C
C       test indicator : 0 : no test output
C                       >0 : (more) test output
C       compress ind.  : 0 : no file compression
C                        1 : Remove comments except for empty switches.
C                        2 : Remove all comment.
C
C     Output is read using free format, so quotes are needed around
C     strings. Echo and test output is send to the standard output
C     device. Switches can be up to NLSWTC characters long, and are
C     separated by spaces.
C
C  3. Parameters :
C
C     Data in PARAMETER statements :
C     ----------------------------------------------------------------
C       MMLOUT  Int.  Line length of output.
C       MMSWTC  Int.  Maximum number of switches.
C       MMSWLN  Int.  Maximum number of switches on a single line.
C       MMFILE  Int.  Maximum number of include files.
C       MMLINE  Int.  Maximum length of include files.
C     ----------------------------------------------------------------
C
C     Internal data :
C     ----------------------------------------------------------------
C       ITEST   Int.  Test indicator (see section 2).
C       ICOMP   Int.  Compression indicator (see section 2).
C       NSWTCH  Int.  Number of switches (total).
C       NLSWTC  Int.  Maximum length of switches.
C       NFILES  Int.  Number of include files.
C       NLINES  I.A.  Length of include files.
C       IDLEN   I.A.  Length of ID string for include file.
C       MMLNGT  Int.  Maximum length of include files.
C     ----------------------------------------------------------------
C
C  4. Subroutines used :
C
C     None.
C
C  5. Called by :
C
C     None (stand alone program).
C
C  6. Error messages :
C
C     - Checks on array sizes.
C     - Open errors on files.
C
C  7. Remarks :
C
C     - Switches are case-sensitive
C     - Switch in code has to be followed by space, forward slash (/) or
C       exclamation mark (!)
C     - Multiple switches can appear on a single line, seperated by 
C       a forward slash or exclamation mark. In this case all switches
C       need to be present in switch file for the line to be included.
C     - Switches can be used in include files, since include files are
C       are pre-processed before the actual file is processed. Includes
C       in include files, however, are not accepted.
C     - By dealing with swtiches before looking for include files,
C       the inclusion of files can be governed by switches.
C     - Variable record lengths used by looking for last non-blank.
C     - ID string for inclusion of file can be anything, and may
C       include blanks.
C
C  8. Structure :
C
C     =====================================================
C      Initializations
C      Read from standard input and proparations
C      ----------------------------------------------------
C        ITEST and ICOMP
C        Read names of input and output files and open
C        Read and process switches.
C        Process include files
C          ------------------------------------------------
C          Get and store ID string
C          Preprocess include file
C            ----------------------------------------------
C            Process switches
C            Check for keeping of line (compress)
C            Determine line lengths
C     =====================================================
C      Actual processing
C      For every line do
C      ----------------------------------------------------
C        Process switches
C        Check for include files
C        If no include
C        --------------------------------------------------
C          Check for keeping of line (compress)
C          Determine actual line length
C          Write line
C     =====================================================
C
C  9. Source code :
C
* ----------------------------------------------------------------------
*
      PARAMETER ( MMLOUT = 132 )
      PARAMETER ( MMFILE =  30 )
      PARAMETER ( MMSWTC =  52 )
      PARAMETER ( MMSWLN =   4 )
      PARAMETER ( MMLINE = 200 )
*
      INTEGER       NSWTCH, IDLEN(MMFILE), NLINES(MMFILE), LL, NSWLN,
     &              LENGTH(MMFILE,MMLINE), NINCF(MMFILE), LS(MMSWTC)
      LOGICAL       FLOLD, FLKEEP, FLINCL, FLSWTC, LSTEXC, NOWEXC,
     &              QUOTES
*
      CHARACTER*20  TEST0, TSTSTR
      CHARACTER*500 FNAMEI, FNAMEO, FNAMER
      CHARACTER*72  INSTR
      CHARACTER*176 NEWLNE, OLDLNE
      CHARACTER*200 SWTCHS
      CHARACTER*33  NOLINE
      CHARACTER     SWITCH*8, SW0*8
      DIMENSION     SWITCH(MMSWTC), TSTSTR(MMFILE), INSTR(MMFILE,MMLINE)
*
* initialisations
*
      NOLINE        = '.  .  .  .  .  .  .  .  .  .  .  '
*
* set test parameters --------------------------------------------------
*
      NLSWTC = LEN(SWITCH(1))
*
      READ (*,*) ITEST, ICOMP
      ITEST  = MAX ( ITEST , 0 )
      ICOMP  = MIN ( MAX(ICOMP,0) , 2 )
      WRITE (*,900) ITEST, ICOMP
*
* Read file names ------------------------------------------------------
*
      READ (*,*) FNAMEI, FNAMEO
      NDSIN  = 10
      NDSOUT = 11
      WRITE (*,910) FNAMEI, FNAMEO
*
      OPEN (NDSIN ,FILE=FNAMEI,ERR=801,STATUS='OLD')
      OPEN (NDSOUT,FILE=FNAMEO,ERR=802)
*
* Read switches --------------------------------------------------------
*
      READ (*,*) SWTCHS
      NSWTCH = 0
      FLOLD  = .FALSE.
*
      DO 100, I=1, 200
        IF (SWTCHS(I:I).NE.' ') THEN
            IF ( FLOLD ) THEN
                  J = J + 1
                  IF (J.GT.NLSWTC) THEN
                      WRITE (*,*) '*** ERROR: SWITCH TOO LONG'
                      STOP
                    ENDIF
                  SW0(J:J) = SWTCHS(I:I)
                ELSE
                  IF (NSWTCH+1.GT.MMSWTC) THEN
                      WRITE (*,*) '*** ERROR: TOO MANY SWITCHES'
                      STOP
                    ENDIF
                  NSWTCH   = NSWTCH + 1
                  J        = 1
                  SW0(1:1) = SWTCHS(I:I)
                  FLOLD    = .TRUE.
                ENDIF
          ELSE
            IF ( FLOLD ) THEN
                SWITCH(NSWTCH) = SW0(1:J)
                LS    (NSWTCH) = J
              ENDIF
            FLOLD  = .FALSE.
          ENDIF
  100   CONTINUE
*
      IF ( FLOLD ) THEN
          SWITCH(NSWTCH) = SW0(1:J)
          LS    (NSWTCH) = J
        ENDIF
*
      WRITE (*,920) NSWTCH
      IF (ITEST.EQ.0) THEN
          WRITE (*,921) (SWITCH(I),I=1,NSWTCH)
        ELSE
          DO 90, J=1, NSWTCH
            SW0    = SWITCH(J)
            WRITE (*,922) J, SW0(1:LS(J))
   90       CONTINUE
        ENDIF
*
* get include files ----------------------------------------------------
*
      NDSINC = 50
      NFILES = 0
      WRITE (*,930)
  110 CONTINUE
      READ (*,*,END=200,ERR=200) TEST0, FNAMER
      NFILES = NFILES + 1
      NINCF(NFILES) = 0
      IF (NFILES.GT.MMFILE) THEN
          WRITE (*,*) '*** ERROR: TOO MANY INCLUDE FILES'
          STOP
        ENDIF
      IF (ITEST.GE.3) WRITE (*,9930) TEST0, FNAMER
*
* store ID string and determine its length - - - - - - - - - - - - - - -
*
      TSTSTR(NFILES) = TEST0
      DO 120 J=LEN(TEST0),1,-1
        IDLEN(NFILES) = J
        IF (TEST0(J:J).NE.' ') GOTO 121
  120   CONTINUE
  121 CONTINUE
      IF (ITEST.GE.2) WRITE (*,9931) IDLEN(NFILES)
*
* (pre-) process include file  - - - - - - - - - - - - - - - - - - - - -
*
      ILINE1 = 0
      ILINE2 = 0
      NDSINC = NDSINC+1
      OPEN (NDSINC,FILE=FNAMER,ERR=803,STATUS='OLD')
      LSTEXC = .FALSE.
  130 CONTINUE
      READ (NDSINC,'(A)',END=190,ERR=190) NEWLNE
      OLDLNE = NEWLNE
      ILINE1 = ILINE1 + 1
*
* switches
*
      FLKEEP = .TRUE.
      FLSWTC = .FALSE.
*
      ! Rewrite for multiple switches on single line
      ! Chris Bunney, Feb 2020.
      NSWLN = 0
      DO 140
        IF(NSWLN .GT. MMSWLN) THEN
          WRITE(*,9950) ILINE1, TRIM(FNAMER), TRIM(OLDLNE)
          STOP
        ENDIF
        IF(NEWLNE(1:2) .EQ. '!/') THEN
          ! Potential switch
          FLSWTC = .FALSE.
          FLKEEP = .FALSE.
*
          ! Check if just a comment
          IF ( NEWLNE(3:3) .EQ. ' ' ) THEN
            FLSWTC = .TRUE.
            GOTO 142  ! Assumes no more switches
          ENDIF
*
          ! Check if is an activated switch:
          DO 141, I=1, NSWTCH
            SW0 = SWITCH(I)
            J = LS(I)
            IF(NEWLNE(3:2+J) .EQ. SW0(1:J)) THEN
*
              IF(NEWLNE(3+J:3+J) .EQ. ' ' .OR.        
     &             NEWLNE(3+J:3+J) .EQ. '!') THEN
                NEWLNE(1:MMLOUT) = NEWLNE(3+J:MMLOUT+3+J-1)
                FLSWTC = .TRUE.
                NSWLN = NSWLN + 1
                GOTO 140
              ENDIF
*
              IF(NEWLNE(3+J:3+J) .EQ. '/' ) THEN
                NEWLNE(1:MMLOUT) = NEWLNE(4+J:MMLOUT+4+J-1)
                FLSWTC = .TRUE.
                NSWLN = NSWLN + 1
                GOTO 140
              ENDIF
*
            ENDIF
 141      CONTINUE ! ENDDO
*
          ! No match found for switch - don't include line
          FLSWTC = .FALSE.
          GOTO 142
        ELSE
          ! No more switches, break out of do loop
          GOTO 142
        ENDIF
* 
 140  CONTINUE ! ENDDO
 142  CONTINUE ! ESCAPE
*
* keep line ...
*
      FLKEEP = FLKEEP .OR. FLSWTC
*
      IF (ICOMP.GE.1) FLKEEP = FLSWTC .OR. .NOT. (NEWLNE(1:1).EQ.'!')
      IF (ICOMP.GT.1) FLKEEP = FLKEEP .AND. (NEWLNE(1:2).NE.'!/')
*
      IF ( FLKEEP ) THEN
          DO 150 J=MMLOUT,1,-1
            LL     = J
            IF (NEWLNE(J:J).NE.' ') GOTO 151
 150        CONTINUE
 151      CONTINUE
*
          IF ( ICOMP.NE.0 .AND. .NOT.FLSWTC ) THEN
              QUOTES = .FALSE.
              DO 155 J=1,LL
                IF (.NOT.QUOTES .AND. NEWLNE(J-1:J-1).EQ.'!') GOTO 156
                IF (NEWLNE(J-1:J-1).EQ.'''') QUOTES = .NOT. QUOTES
 155            CONTINUE
 156          CONTINUE
              LL     = MIN ( J , LL )
            ENDIF
*
        ENDIF
*
      NOWEXC = NEWLNE(1:1).EQ.'!' .AND. LL.EQ.1
      FLKEEP = FLKEEP .AND. .NOT. (LSTEXC.AND.NOWEXC)
*
      IF (FLKEEP) THEN
          LSTEXC = NOWEXC
          ILINE2 = ILINE2 + 1
          IF (ILINE2.GT.MMLINE) THEN
              WRITE (*,*) '*** ERROR: TOO MANY INCLUDE LINES'
              STOP
            ENDIF
          INSTR(NFILES,ILINE2) = NEWLNE(1:MMLOUT)
*
* line length
*
          LENGTH(NFILES,ILINE2) = LL
        ENDIF
*
* next line
*
      GOTO 130
  190 CONTINUE
      NLINES(NFILES) = ILINE2
      WRITE (*,931) TEST0, FNAMER, ILINE1, ILINE2
*
      IF (ITEST.GE.2) THEN
          WRITE (*,9932) FNAMER
          DO 191, I=1,NLINES(NFILES)
            IF (ITEST.GE.3) THEN
                WRITE (*,9933) INSTR(NFILES,I)(1:LENGTH(NFILES,I)),
     &            ('.',J=LENGTH(NFILES,I)+1,72), '|'
              ELSE
                WRITE (*,9933) INSTR(NFILES,I)(1:72), '|'
              ENDIF
  191       CONTINUE
          WRITE (*,9934)
        ENDIF
*
* next include file
*
      GOTO 110
  200 CONTINUE
      IF (ITEST.GE.2) THEN
          WRITE (*,9999)
          STOP
        ENDIF
*
* Preprocessing finished, start processing file ------------------------
*
      NINP   = 0
      NOUT   = 0
      NINCL  = 0
      LSTEXC = .FALSE.
      IF (ITEST.GE.1) WRITE (*,9940)
  300 CONTINUE
      READ (NDSIN ,'(A)',END=400,ERR=400) NEWLNE
      NINP   = NINP + 1
*
      OLDLNE = NEWLNE
*
* switches
*
      FLKEEP = .TRUE.
      FLSWTC = .FALSE.
*
      ! Rewrite for multiple switches on single line
      ! Chris Bunney, Feb 2020.
      NSWLN = 0
      DO 310
        IF(NSWLN .GT. MMSWLN) THEN
          WRITE(*,9950) ILINE1, TRIM(FNAMEI), TRIM(OLDLNE)
          STOP
        ENDIF
        IF(NEWLNE(1:2) .EQ. '!/') THEN
          ! Potential switch
          FLSWTC = .FALSE.
          FLKEEP = .FALSE.
*
          ! Check if just a comment
          IF ( NEWLNE(3:3) .EQ. ' ' ) THEN
            FLSWTC = .TRUE.
            GOTO 312  ! Assumes no more switches
          ENDIF
*
          ! Check if is an activated switch:
          DO 311, I=1, NSWTCH
            SW0 = SWITCH(I)
            J = LS(I)
            IF(NEWLNE(3:2+J) .EQ. SW0(1:J)) THEN
*
              IF(NEWLNE(3+J:3+J) .EQ. ' ' .OR.        
     &             NEWLNE(3+J:3+J) .EQ. '!') THEN
                NEWLNE(1:MMLOUT) = NEWLNE(3+J:MMLOUT+3+J-1)
                FLSWTC = .TRUE.
                NSWLN = NSWLN + 1
                GOTO 310
              ENDIF
*
              IF(NEWLNE(3+J:3+J) .EQ. '/' ) THEN
                NEWLNE(1:MMLOUT) = NEWLNE(4+J:MMLOUT+4+J-1)
                FLSWTC = .TRUE.
                NSWLN = NSWLN + 1
                GOTO 310
              ENDIF
*
            ENDIF
 311      CONTINUE ! ENDDO
*
          ! No match found for switch - don't include line
          FLSWTC = .FALSE.
          GOTO 312
        ELSE
          ! No more switches, break out of do loop
          GOTO 312
        ENDIF
* 
 310  CONTINUE ! ENDDO
 312  CONTINUE ! ESCAPE
*
* include ???
*
      FLINCL = .FALSE.
      DO 330, J=1, NFILES
        IF (NEWLNE(1:IDLEN(J)).EQ.TSTSTR(J)(1:IDLEN(J))) THEN
            IF (ITEST.GE.1) THEN
                WRITE (*,9941) OLDLNE, NOLINE
                NOLINE(1 :32) = NOLINE( 2:33)
                NOLINE(33:33) = NOLINE( 3: 3)
              ENDIF
            DO 320, I=1,NLINES(J)
              WRITE (NDSOUT,'(A)') INSTR(J,I)(1:LENGTH(J,I))
  320         CONTINUE
            IF (ITEST.GE.1) THEN
                DO 321, I=1,NLINES(J)
                  WRITE (*,9941) NOLINE, INSTR(J,I)
                  NOLINE(1 :32) = NOLINE( 2:33)
                  NOLINE(33:33) = NOLINE( 3: 3)
  321             CONTINUE
              ENDIF
            FLINCL = .TRUE.
            NINCL  = NINCL + 1
            NINCF(J) = NINCF(J) + 1
            NOUT   = NOUT + NLINES(J)
            GOTO 331
          ENDIF
  330   CONTINUE
  331 CONTINUE
*
* keep line ...
*
      IF (.NOT.FLINCL) THEN
          FLKEEP = FLKEEP .OR. FLSWTC
*
          IF (ICOMP.GE.1) FLKEEP = FLSWTC .OR. .NOT.(NEWLNE(1:1).EQ.'!')
          IF (ICOMP.GT.1) FLKEEP = FLKEEP .AND. (NEWLNE(1:2).NE.'!/')
*
          IF ( FLKEEP ) THEN
              DO 350 J=MMLOUT,1,-1
                LL     = J
                IF (NEWLNE(J:J).NE.' ') GOTO 351
 350            CONTINUE
 351          CONTINUE
*
              IF ( ICOMP.NE.0 .AND. .NOT.FLSWTC ) THEN
                  QUOTES = .FALSE.
                  DO 355 J=1,LL
                    IF (.NOT.QUOTES .AND. NEWLNE(J-1:J-1).EQ.'!')
     &                    GOTO 356
                    IF (NEWLNE(J-1:J-1).EQ.'''') QUOTES = .NOT. QUOTES
 355                CONTINUE
 356              CONTINUE
                  LL     = MIN ( J , LL )
                ENDIF
*
            ENDIF
*
          NOWEXC = NEWLNE(1:1).EQ.'!' .AND. LL.EQ.1
          FLKEEP = FLKEEP .AND. .NOT. (LSTEXC.AND.NOWEXC)
*
          IF (FLKEEP) THEN
              LSTEXC = NOWEXC
*
* write line
*
              WRITE (NDSOUT,'(A)') NEWLNE(1:LL)
              NOUT   = NOUT + 1
              IF (ITEST.GE.1) WRITE (*,9941) OLDLNE, NEWLNE
*
            ELSE IF (ITEST.GE.1) THEN
              WRITE (*,9941) OLDLNE, NOLINE
              NOLINE(1 :32) = NOLINE( 2:33)
              NOLINE(33:33) = NOLINE( 3: 3)
            ENDIF
        ENDIF
*
* next line
*
      GOTO 300
  400 CONTINUE
*
* End of processing ----------------------------------------------------
*
      IF (ITEST.GE.1) WRITE (*,9942)
      WRITE (*,950) NINP, NOUT, NINCL
      IF (NINCL.NE.0) THEN
          WRITE (*,951)
          DO 410, J=1, NFILES
            IF (NINCF(J).NE.0) WRITE (*,952) NINCF(J), TSTSTR(J)
  410       CONTINUE
        ENDIF
*
      STOP ' '
*
* Error escape locations
*
  801 CONTINUE
      WRITE (*,*) '*** ERROR: OPENING INPUT FILE'
      STOP
*
  802 CONTINUE
      WRITE (*,*) '*** ERROR: OPENING OUTPUT FILE'
      STOP
*
  803 CONTINUE
      WRITE (*,*) '*** ERROR: OPENING INCLUDE FILE ', FNAMER
      STOP
*
* Formats
*
  900 FORMAT (/'  W3ADC, WAVEWATCH FORTRAN PREPROCESSING : '/
     &         ' =========================================='//
     &         '     ITEST =',I4/
     &         '     ICOMP =',I4)
  910 FORMAT ( '     INPUT FILE  : ',A/
     &         '     OUTPUT FILE : ',A)
  920 FORMAT ( '     NUMBER OF SWITCHES:',I4,'   SWITCHES :')
  921 FORMAT ( '        ',8(A,1X))
  922 FORMAT ( '     ',I4,'  >',A,'<')
  930 FORMAT (/'     INCLUDE FILES :'//
     &         '     ID_STRING             FILENAME    ',
     &         '                                 LINES '/
     &         '     ----------------------------------',
     &         '---------------------------------------')
  931 FORMAT ( '     ',A20,2X,A40,2X,2I4)
  950 FORMAT (/'     FINAL STATISTICS  : '/
     &         '        INPUT LINES    :',I6/
     &         '        OUTPUT LINES   :',I6/
     &         '        FILES INCLUDED :',I6,'  TOTAL')
  951 FORMAT ( '                         ------------------------',
     &                                      '--------------------')
  952 FORMAT ( '                        ',I6,2X,A)
*
 9930 FORMAT ( ' TEST W3ADC/3 : ',A,2X,A)
 9931 FORMAT ( ' TEST W3ADC/3 : ',I10)
 9932 FORMAT ( ' TEST W3ADC/2 : INCULDE FILE ',A/
     &         '   +------------------------------------',
     &         '------------------------------------+')
 9933 FORMAT ( '   |',73A)
 9934 FORMAT ( '   +------------------------------------',
     &         '------------------------------------+'/)
 9940 FORMAT (/' TEST W3ADC/1 '/
     &         '                 INPUT                  ',
     &         '                OUTPUT'/
     &         '   +---------------------------------+  ',
     &         '  +---------------------------------+')
 9941 FORMAT ( '   |',A33,'|    |',A33,'|')
 9942 FORMAT ( '   +---------------------------------+  ',
     &         '  +---------------------------------+')
*
 9950 FORMAT (/'*** ERROR: MAXIMUM NUMBER OF SWITCHES ON',
     &         ' INPUT LINE EXCEEDED                  ',/
     &         '       LINE NUMBER: ', I5, /
     &         '       FILENAME: ', A, /
     &         '       LINE: ', A//)
*
 9999 FORMAT ( ' TEST W3ADC/2 : PROGRAM ENDED DUE TO VALUE OF ITEST'/)
*
* End of W3ADC  --------------------------------------------------------
*
      END
