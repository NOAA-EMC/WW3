      PROGRAM W3PRNT
C/
C/                  +-----------------------------------+
C/                  |           H. L. Tolman            |
C/                  |                        FORTRAN 77 |
C/                  | Last update :         08-Jul-1993 |
C/                  +-----------------------------------+
C/
C  1. Purpose :
C
C     Printing source codes and COMMON's including page numbers and
C     headers.
C
C  2. Method :
C
C     File name read from standard input.
C     Output send to w3prnt.out.
C
C  3. Parameters :
C
C     Data in PARAMETER statements :
C     ----------------------------------------------------------------
C       MMLINE  Int.  Maximum length of file.
C       NLPAGE  Int.  Length of page in terms of lines.
C       NOPAGE  Int.  Number of lines printed on page.
C       NUMB    Log.  Flag for line numbering.
C       INVERT  Log.  Flag for inverting print order of pages.
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
C     - File existence.
C     - File size.
C
C  7. Remarks :
C
C     - Only first 74 characters printed.
C     - If line is longer, last charected beomes break-off mark >.
C
C  8. Structure :
C
C     =====================================================
C       Prepagations
C       Get file name from standard input.
C       Open input file.
C       Read entire file.
C       Get number of pages.
C       Open output file.
C       Process pages.
C       ------------------------------------------------
C       ------------------------------------------------
C     =====================================================
C
C  9. Source code :
C
* ----------------------------------------------------------------------
*
      INTEGER     MMLINE, NLPAGE, NOPAGE
      LOGICAL     NUMB, INVERT
*
      PARAMETER ( MMLINE = 5000 )
      PARAMETER ( NLPAGE =   60 )
      PARAMETER ( NOPAGE =   55 )
*
      PARAMETER ( NUMB   = .TRUE. )
      PARAMETER ( INVERT = .FALSE. )
*
      INTEGER     NADD_1, NADD_2, N_HEAD, IERR, I, NL, NPAGE,
     &            IP, IP1, IPN, IPS, IL, IL1, ILN, ICHAR
      CHARACTER   FILE*80, LINE*80, NOLINE*80, HEADER*80,
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
      N_HEAD = LEN(FNAME)
      DO 100, I=N_HEAD, 1, -1
        IF ( FNAME(I:I) .NE. ' ' ) THEN
            N_HEAD = I
            GOTO 101
          ENDIF
  100   CONTINUE
  101 CONTINUE
      HEADER = NOLINE
      HEADER(71-N_HEAD:70) = FNAME(1:N_HEAD)
      HEADER(73:76) = 'page'
*
* Open input file ------------------------------------------------------
*
      OPEN (10,FILE=FNAME,STATUS='OLD',ERR=801,IOSTAT=IERR)
      REWIND (10)
*
* Read input file ------------------------------------------------------
*
      NL     = 0
  200 CONTINUE
      READ (10,'(A)',END=201,ERR=802,IOSTAT=IERR) LINE
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
* Numer of pages ----------------------------------------------------------------------
*
      NPAGE  = 1 + (NL-1)/NOPAGE
      IF ( INVERT ) THEN
          IP1    = NPAGE
          IPN    =   1
          IPS    =  -1
        ELSE
          IP1    =   1
          IPN    = NPAGE
          IPS    =   1
        ENDIF
*
* Open output file -----------------------------------------------------
*
      OPEN (11,FILE='w3prnt.out',ERR=803,IOSTAT=IERR)
*
* Loop over pages ------------------------------------------------------
*
      DO 400, IP=IP1, IPN, IPS
*
        WRITE (PP,'(I4)') IP
        HEADER(77:80) = PP
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
              IF ( NUMB ) THEN
                  WRITE (11,'(1X,I4,1X,A)')  IL, LINE(1:ICHAR)
                ELSE
                  WRITE (11,'(6X,A)') LINE(1:ICHAR)
                ENDIF
            ELSE
              WRITE (11,'(A)')  NULLST
            ENDIF
  350     CONTINUE
*
        IF ( IP .NE. IPN ) THEN
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
      WRITE (*,*) '*** ERROR  : READING FILE NAME'
      WRITE (*,*) '    IOSTAT : ', IERR
      STOP
*
  801 CONTINUE
      WRITE (*,*) '*** ERROR  : OPENING INPUT FILE'
      WRITE (*,*) '    IOSTAT : ', IERR
      STOP
*
  802 CONTINUE
      WRITE (*,*) '*** ERROR  : READING FROM OUTPUT FILE'
      WRITE (*,*) '    IOSTAT : ', IERR
      STOP
*
  803 CONTINUE
      WRITE (*,*) '*** ERROR  : OPENING INPUT FILE'
      WRITE (*,*) '    IOSTAT : ', IERR
      STOP
*
* Formats
*
*
* End of W3PRNT --------------------------------------------------------
*
      END
