      PROGRAM W3LIST
C/
C/                  +-----------------------------------+
C/                  |           H. L. Tolman            |
C/                  |                        FORTRAN 77 |
C/                  | Last update :         17-Feb-1995 |
C/                  +-----------------------------------+
C/
C  1. Purpose :
C
C     Generate a line numbered source code listing, putting numbers
C     on source code lines only.
C
C     This is a modified version of w3prnt. See this program for
C     documentation.
C
C     Invert option disabled.
C     ALLNUM read from standard input after file name.
C
C  9. Source code :
C
* ----------------------------------------------------------------------
*
      INTEGER     MMLINE, NLPAGE, NOPAGE
*
      PARAMETER ( MMLINE = 50000 )
      PARAMETER ( NLPAGE =   60 )
      PARAMETER ( NOPAGE =   55 )
*
      INTEGER     NADD_1, NADD_2, N_HEAD, IERR, I, NL, NPAGE,
     &            IP, IL, IL1, ILN, ICHAR
      LOGICAL     ALLNUM
      CHARACTER   FILE*80, LINE*80, NOLINE*80, HEADER*79,
     &            FNAME*40, NULLST*1, PP*4
      DIMENSION   FILE(MMLINE)
*
* initialisations ------------------------------------------------------
*
      NOLINE(01:40) = '                                        '
      NOLINE(41:80) = '                                        '
      NULLST = ' '
      NADD_1 = ( NLPAGE - NOPAGE -1 ) / 2
      NADD_2 = NLPAGE - NOPAGE - 1 - NADD_1
*
* get file name --------------------------------------------------------
*
      READ (*,'(A)',ERR=800,IOSTAT=IERR) FNAME
      READ (*,*,ERR=800,IOSTAT=IERR) ALLNUM
      N_HEAD = LEN(FNAME)
      DO 100, I=N_HEAD, 1, -1
        IF ( FNAME(I:I) .NE. ' ' ) THEN
            N_HEAD = I
            GOTO 101
          ENDIF
  100   CONTINUE
  101 CONTINUE
      HEADER = NOLINE(1:79)
      HEADER(70-N_HEAD:69) = FNAME(1:N_HEAD)
      HEADER(72:75) = 'page'
*
* Open input file ------------------------------------------------------
*
      OPEN (10,FILE=FNAME,STATUS='OLD',ERR=802,IOSTAT=IERR)
      REWIND (10)
*
* Read input file ------------------------------------------------------
*
      NL     = 0
  200 CONTINUE
      READ (10,'(A)',END=201,ERR=803,IOSTAT=IERR) LINE
      NL     = NL + 1
      IF ( NL .GT. MMLINE ) THEN
          WRITE (*,*) '*** ERROR  : TOO MANY LINES'
          STOP ' '
        ENDIF
      FILE(NL) = LINE
      GOTO 200
*
  201 CONTINUE
*
* Numer of pages--------------------------------------------------------
*
      NPAGE  = 1 + (NL-1)/NOPAGE
      ILCODE = 0
*
* Open output file -----------------------------------------------------
*
      OPEN (11,FILE='w3list.out',ERR=804,IOSTAT=IERR)
*
* Loop over pages ------------------------------------------------------
*
      DO 400, IP=1, NPAGE, 1
*
        WRITE (PP,'(I4)') IP
        HEADER(76:79) = PP
        ILN    = IP * NOPAGE
        IL1    = ILN + 1 - NOPAGE
*
        WRITE (11,'(A)') HEADER
        DO 300, IL=1, NADD_1
          WRITE (11,'(A)')  NULLST
  300     CONTINUE
*
        DO 350, IL=IL1, ILN
          IF ( IL .LE. NL ) THEN
              LINE   = FILE(IL)
              DO 310, I=80, 1, -1
                IF ( LINE(I:I) .NE. ' ' ) THEN
                    ICHAR  = I
                    GOTO 311
                  ENDIF
  310           CONTINUE
  311         CONTINUE
              IF ( ICHAR .GT. 74 ) THEN
                  LINE(74:74) = '>'
                  ICHAR  = 74
                ENDIF
              IF ( LINE(1:1) .EQ. ' ' .OR. ALLNUM ) THEN
                  ILCODE = ILCODE + 1
                  WRITE (11,'(1X,I4,1X,A)')  ILCODE, LINE(1:ICHAR)
                ELSE
                  WRITE (11,'(6X,A)') LINE(1:ICHAR)
                ENDIF
            ENDIF
  350     CONTINUE
*
        IF ( IP .NE. NPAGE ) THEN
            DO 360, IL=1, NADD_2
              WRITE (11,'(A)')  NULLST
  360         CONTINUE
          ENDIF
*
  400   CONTINUE
*
* End of processing ----------------------------------------------------
*
      STOP
*
* Error escape locations
*
  800 CONTINUE
      WRITE (*,*) '*** w3list ERROR  : READING FILE NAME'
      WRITE (*,*) '           IOSTAT : ', IERR
      STOP
*
  801 CONTINUE
      WRITE (*,*) '*** w3list ERROR  : READING LINE NUMBER FLAG'
      WRITE (*,*) '           IOSTAT : ', IERR
      STOP
*
  802 CONTINUE
      WRITE (*,*) '*** w3list ERROR  : OPENING INPUT FILE'
      WRITE (*,*) '           IOSTAT : ', IERR
      STOP
*
  803 CONTINUE
      WRITE (*,*) '*** w3list ERROR  : READING FROM OUTPUT FILE'
      WRITE (*,*) '           IOSTAT : ', IERR
      STOP
*
  804 CONTINUE
      WRITE (*,*) '*** w3list ERROR  : OPENING INPUT FILE'
      WRITE (*,*) '           IOSTAT : ', IERR
      STOP
*
* Formats
*
*
* End of W3PRNT --------------------------------------------------------
*
      END
