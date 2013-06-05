C/ ------------------------------------------------------------------- /
      PROGRAM W3SPLT
C/
C/                  +-----------------------------------+
C/                  | WAVEWATCH III           NOAA/NCEP |
C/                  |           H. L. Tolman            |
C/                  |                        FORTRAN 77 |
C/                  | Last update :         19-May-1999 |
C/                  +-----------------------------------+
C/
C  1. Purpose :
C
C     Read a WAVEWATCH III version 1.17 point output data file and
C     produces a table of mean parameters for all individual wave
C     systems.
C
C  2. Method :
C
C     Partitioning as devised by Gerling and similar to used in
C     WAM model. From standard input the following data is read :
C
C        name of output point to be considered.
C        name of run of model.
C        name of input file.
C        logical to indentify FORMATTED input file.
C        name of output file.
C
C     All strings are read as characters, therefore no quotes are
C     needed.
C
C  3. Parameters :
C
C     Parameter statements
C     ----------------------------------------------------------------
C       NFR     Int.   Number of frequencies in spectrum.
C       NTH     Int.   Number of directions in spectrum.
C       NPMAX   Int.   Maximum number of peaks to be looked for.
C       NPTAB   Int.   Number of columns in table.
C       HSMIN   Real   Minimum wave height for includion in table.
C       HSDROP  Real   Minimum wave high for system to be considered
C                      as separate wave system.
C       DHSMAX  Real   Max. change in Hs for system to be considered
C                      related to previous time.
C       DTPAX   Real   Id. Tp.
C       DDMMAX  Real   Id. Dm.
C       DDWMAX  Real   Maximum differences in wind and wave direction
C                      for marking of system as under the influence
C                      of the local wind,
C       AGEMIN  Real   Id. wave age.
C     ----------------------------------------------------------------
C
C  4. Subroutines used :
C
C       MPARS   Calculate mean parameters
C       FNDPRT  Get mask for partition.
C       WAVNU2  Solve dispersion relation.
C
C  5. Called by :
C
C     None, main program,
C
C  6. Error messages :
C
C     See "Error escape locations" at end of code.
C
C  7. Remarks :
C
C  8. Structure :
C
C     See source code.
C
C 10. Source code :
C
C/ ------------------------------------------------------------------- /
C/ Parameter statements
C/
      INTEGER         NFR, NTH, NPMAX, NPTAB
      REAL            XFR, HSMIN, HSDROP,
     &                DHSMAX, DTPAX, DDMMAX, DDWMAX, AGEMIN
*
      PARAMETER     ( NFR    =  25    )
      PARAMETER     ( NTH    =  24    )
      PARAMETER     ( NPMAX  =  20    )
      PARAMETER     ( NPTAB  =   6    )
      PARAMETER     ( XFR    =   1.1  )
      PARAMETER     ( HSMIN  =   0.15 )
      PARAMETER     ( HSDROP =   0.05 )
      PARAMETER     ( DHSMAX =   1.5  )
      PARAMETER     ( DTPMAX =   1.5  )
      PARAMETER     ( DDMMAX =  15.   )
      PARAMETER     ( DDWMAX =  30.   )
      PARAMETER     ( AGEMIN =   0.8  )
*
      INTEGER         NDSI, NDSO, IERR, NFRF, NTHF, IFR, ITH
      INTEGER         NPNTS, IP, TIME(2), IREAD
      INTEGER         IFL, IFH, ITHL, ITHH
      INTEGER         NPEAK, NFRP(NPMAX), NTHP(NPMAX), IPNOW, IOUT, ITAB
      INTEGER         NZERO
      REAL            PI, RADE, FR(NFR), TH(NTH), DTH, DFR(NFR),
     &                TAILF, SPEC(NFR,NTH), W1(NFR,NTH), W2(NFR,NTH),
     &                SP1D(NFR), HS, TP, DM, XFRV, STMAX, HMAX
      REAL            HSTOT, HSP(NPMAX), TPP(NPMAX), DMP(NPMAX)
      REAL            HST(NPMAX,2), TPT(NPMAX,2), DMT(NPMAX,2)
      REAL            D, UA, UD, UABS, UDIR, DELDW, DELHS, DELTP, DELDM
      REAL            AFR, WN, CG, AGE, Y, X
      LOGICAL         FORMI, DATA, FLAG(NPMAX), HEADER
      CHARACTER       POINT*10, PID*10, FNAMEI*40, FNAMEO*40, IDSTR*21
      CHARACTER       MODEL*40, RUN*20, GNAME*30, IDLAT*1, IDLON*1
      CHARACTER*129   BLANK, TAIL, STRING
      CHARACTER*15    PART
C/
C/ ------------------------------------------------------------------- /
C/
*
* 1.  Initializations ------------------------------------------------ *
* 1.a Constants etc.
*
      NDSI   = 10
      NDSO   = 50
*
      PI     = 4. * ATAN(1.)
      RADE   = 180. / PI
      XFRV   = XFR
*
      IREAD  = 0
*
      TAIL (  1: 40) = '+-------+-----------+-----------------+-'
      TAIL ( 41: 80) = '----------------+-----------------+-----'
      TAIL ( 81:120) = '------------+-----------------+---------'
      TAIL (120:129) = '---------+'
      BLANK(  1: 40) = '| nn nn |      nn   |                 | '
      BLANK( 41: 80) = '                |                 |     '
      BLANK( 81:120) = '            |                 |         '
      BLANK(120:129) = '         |'
      STRING       = BLANK
*
      DO 100, IP=1, NPTAB
        HST(IP,1) = -1.
        TPT(IP,1) = -1.
        DMT(IP,1) = -1.
  100   CONTINUE
*
* 1.b Initial I/O
*
      WRITE (*,900)
      READ (*,'(A)') POINT
      READ (*,'(A)') RUN
      WRITE (*,901) POINT, RUN
      READ (*,'(A)') FNAMEI
      READ (*,*) FORMI
      WRITE (*,902) FNAMEI, FORMI
      READ (*,'(A)') FNAMEO
      WRITE (*,903) FNAMEO
*
* 1.c Open input file and process header
*
      IF ( FORMI ) THEN
          OPEN (NDSI,FILE=FNAMEI,STATUS='OLD',ERR=800,IOSTAT=IERR)
        ELSE
          OPEN (NDSI,FILE=FNAMEI,STATUS='OLD',
     &               FORM='UNFORMATTED',ERR=800,IOSTAT=IERR)
        ENDIF
      WRITE (*,904)
*
      IF ( FORMI ) THEN
          READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR)
     &          IDSTR, NFRF, NTHF, NPNTS, GNAME
        ELSE
          READ (NDSI , END=801,ERR=802,IOSTAT=IERR)
     &          IDSTR, NFRF, NTHF, NPNTS, GNAME
        ENDIF
*
      IF ( IDSTR .NE. 'WAVEWATCH III SPECTRA' ) GOTO 803
      IF ( NFR.NE.NFRF .OR. NTH.NE.NTHF ) GOTO 804
      WRITE (*,905)
*
      IF ( FORMI ) THEN
          READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) FR
        ELSE
          READ (NDSI , END=801,ERR=802,IOSTAT=IERR) FR
        ENDIF
      WRITE (*,906)
*
      IF ( FORMI ) THEN
          READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) TH
        ELSE
          READ (NDSI , END=801,ERR=802,IOSTAT=IERR) TH
        ENDIF
      WRITE (*,907)
*
      DTH    = 2. * PI / REAL(NTH)
      TAILF  = 0.25 * FR(NFR)
*
      DO 110, IFR=1, NFR
        DFR(IFR) = 0.5 * (XFR-1./XFR) * FR(IFR)
  110   CONTINUE
      DFR(NFR) = 0.5 * DFR(NFR)
      WRITE (*,908)
*
* 1.d Open output file and process header
*
      OPEN (NDSO,FILE=FNAMEO,ERR=810,IOSTAT=IERR)
      WRITE (*,909)
      HEADER = .TRUE.
*
* === LOOP OVER DATA ================================================= *
*
  200 CONTINUE
*
* 2.  Read next data ------------------------------------------------- *
*
      IF ( FORMI ) THEN
          READ (NDSI,*,END=888,ERR=802,IOSTAT=IERR) TIME
        ELSE
          READ (NDSI , END=888,ERR=802,IOSTAT=IERR) TIME
        ENDIF
*
      DATA   = .FALSE.
      DO 210, IP=1, NPNTS
        IF ( FORMI ) THEN
            READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR)
     &                                     PID, Y, X, D, UA, UD
          ELSE
            READ (NDSI , END=801,ERR=802,IOSTAT=IERR)
     &                                     PID, Y, X, D, UA, UD
          ENDIF
        IF ( PID .EQ. POINT ) THEN
            IF ( FORMI ) THEN
                READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) SPEC
              ELSE
                READ (NDSI , END=801,ERR=802,IOSTAT=IERR) SPEC
              ENDIF
            DATA   = .TRUE.
            UABS   = UA
            UDIR   = MOD( UD+180., 360. )
          ELSE
            IF ( FORMI ) THEN
                READ (NDSI,*,END=801,ERR=802,IOSTAT=IERR) W1
              ELSE
                READ (NDSI , END=801,ERR=802,IOSTAT=IERR) W1
              ENDIF
          ENDIF
  210   CONTINUE
*
      IF ( .NOT. DATA ) GOTO 805
      IREAD  = IREAD + 1
*
      IF ( HEADER ) THEN
          X      = MOD ( X+720. , 360. )
          IF ( X .LE. 180. ) THEN
              IDLON  = 'E'
            ELSE
              X      = 360. - X
              IDLON  = 'W'
            ENDIF
          IF ( ABS(Y) .LE. 0.0049 ) THEN
              IDLAT  = ' '
            ELSE IF ( Y .GT. 0. ) THEN
              IDLAT  = 'N'
            ELSE
              IDLAT  = 'S'
              X      = -X
            ENDIF
          WRITE (NDSO,950) POINT, Y, IDLAT, X, IDLON, GNAME, RUN
          WRITE (NDSO,951) TAIL
          WRITE (NDSO,952)
          WRITE (NDSO,951) TAIL
          HEADER = .FALSE.
        ENDIF
*
* 3.  Get overall wave height ---------------------------------------- *
*
      DO 310, ITH=1, NTH
        DO 300, IFR=1, NFR
          W1(IFR,ITH) = 1.
  300     CONTINUE
  310   CONTINUE
*
      CALL MPARS ( NFR, NTH, SPEC, W1, FR, TH, DFR, DTH, TAILF, XFRV,
     &             SP1D, HS, TP, DM )
*
      HSTOT  = HS
*
* 4.  Determine maxima ----------------------------------------------- *
*
      NPEAK  = 0
*
      DO 410, IFR=NFR, 1, -1
        IFL    = MAX ( 1 , IFR-1 )
        IFH    = MIN ( NFR , IFR+1 )
        DO 400, ITH=1, NTH
          ITHL   = 1 + MOD(NTH+ITH-2,NTH)
          ITHH   = 1 + MOD(ITH,NTH)
          IF ( SPEC(IFR,ITH) .GE. SPEC(IFL,ITH ) .AND.
     &         SPEC(IFR,ITH) .GE. SPEC(IFH,ITH ) .AND.
     &         SPEC(IFR,ITH) .GE. SPEC(IFL,ITHL) .AND.
     &         SPEC(IFR,ITH) .GE. SPEC(IFR,ITHL) .AND.
     &         SPEC(IFR,ITH) .GE. SPEC(IFH,ITHL) .AND.
     &         SPEC(IFR,ITH) .GE. SPEC(IFL,ITHH) .AND.
     &         SPEC(IFR,ITH) .GE. SPEC(IFR,ITHH) .AND.
     &         SPEC(IFR,ITH) .GE. SPEC(IFH,ITHH) .AND.
     &         SPEC(IFR,ITH) .GT. 0.             ) THEN
              NPEAK  = NPEAK + 1
              NFRP(NPEAK) = IFR
              NTHP(NPEAK) = ITH
            ENDIF
          W1(IFR,ITH) = 0.
  400     CONTINUE
  410   CONTINUE
*
      IF ( NPEAK .LE. NPMAX ) THEN
          WRITE (*,910) TIME, NPEAK
        ELSE
          NPEAK  = NPMAX
          WRITE (*,911) TIME, NPEAK
        ENDIF
*
* 5.  Process all partial fields ------------------------------------- *
*
      NZERO  = 0
*
      DO 500, IP=1, NPEAK
*
        CALL FNDPRT ( NFR, NTH, NFRP(IP), NTHP(IP), SPEC, W1, W2 )
*
        CALL MPARS ( NFR, NTH, SPEC, W2, FR, TH, DFR, DTH, TAILF, XFRV,
     &               SP1D, HSP(IP), TPP(IP), DMP(IP) )
        IF ( HSP(IP) .LE. HSDROP ) NZERO = NZERO + 1
*
  500   CONTINUE
*
      DO 510, IP=NPEAK+1, NPMAX
        HSP(IP) =    0.00
        TPP(IP) = -999.99
        DMP(IP) = -999.99
  510   CONTINUE
*
      DO 520, IP=1, NPTAB
        HST(IP,2) = HST(IP,1)
        TPT(IP,2) = TPT(IP,1)
        DMT(IP,2) = DMT(IP,1)
        HST(IP,1) = -1.
        TPT(IP,1) = -1.
        DMT(IP,1) = -1.
  520   CONTINUE
*
* 6.  Generate output table ------------------------------------------ *
* 6.a Time and overall wave height to string
*
      STRING = BLANK
*
      WRITE (STRING(3:4),'(I2)') MOD(TIME(1),100)
      WRITE (STRING(6:7),'(I2)') TIME(2)/10000
      IF ( HSTOT .GT. 0. ) WRITE (STRING(11:14),'(F4.1)') HSTOT
      WRITE (STRING(16:17),'(I2)') NPEAK - NZERO
      IF ( NPEAK.EQ.0 .OR. HSTOT.LT.0.1 ) GOTO 699
*
* 6.b Switch off peak with too low wave height
*
      DO 600, IP=1, NPEAK
        FLAG(IP) = HSP(IP) .GT. HSMIN
  600   CONTINUE
*
* 6.c Find next highest wave height
*
      IOUT   = 0
  601 CONTINUE
*
      HMAX   = 0.
      IPNOW  = 0
      DO 610, IP=1, NPEAK
        IF ( HSP(IP).GT.HMAX .AND. FLAG(IP) ) THEN
            IPNOW  = IP
            HMAX   = HSP(IP)
          ENDIF
  610   CONTINUE
*
* 6.d No more peaks, skip to output
*
      IF ( IPNOW .EQ. 0 ) GOTO 699
*
* 6.e Find matching field
*
      ITAB   = 0
*
      DO 620, IP=1, NPTAB
        IF ( TPT(IP,2) .GT. 0. ) THEN
            DELHS  = ABS ( HST(IP,2) - HSP(IPNOW) )
            DELTP  = ABS ( TPT(IP,2) - TPP(IPNOW) )
            DELDM  = ABS ( DMT(IP,2) - DMP(IPNOW) )
            IF ( DELDM .GT. 180. ) DELDM = 360. - DELDM
            IF ( DELHS.LT.DHSMAX .AND. 
     &           DELTP.LT.DTPMAX .AND. 
     &           DELDM.LT.DDMMAX ) ITAB = IP
          ENDIF
  620   CONTINUE
*
* 6.f No matching field, find empty fields
*
      IF ( ITAB .EQ. 0 ) THEN
          DO 630, IP=NPTAB, 1, -1
            IF ( TPT(IP,1).LT.0. .AND. TPT(IP,2).LT.0. ) ITAB = IP
  630       CONTINUE
        ENDIF
*
* 6.g Slot in table found, write
*
      IF ( ITAB .NE. 0 ) THEN
*
          WRITE (PART,'(2X,F4.1,F5.1,I4)')
     &           HSP(IPNOW), TPP(IPNOW), NINT(DMP(IPNOW))
          DELDW  = MOD ( ABS ( UDIR - DMP(IPNOW) ) , 360. )
          IF ( DELDW .GT. 180. ) DELDW = 360. - DELDW
          AFR    = 2.*PI/TPP(IPNOW)
          CALL WAVNU2 ( AFR, D, WN, CG, 1.E-5, 15, ICON )
          AGE    = UABS * WN / AFR
          IF ( DELDW.LT.DDWMAX .AND. AGE.GT.AGEMIN ) PART(1:1) = '*'
*
          STRING(5+ITAB*18:19+ITAB*18) = PART
*
          HST(ITAB,1) = HSP(IPNOW)
          TPT(ITAB,1) = TPP(IPNOW)
          DMT(ITAB,1) = DMP(IPNOW)
*
* 6.g No slot in table found, write
*
        ELSE
*
          IOUT   = IOUT + 1
          WRITE (STRING(19:19),'(I1)') IOUT
*
        ENDIF
*
      FLAG(IPNOW) = .FALSE.
      GOTO 601
*
* 6.h End of processing, write line in table
*
  699 CONTINUE
*
      WRITE (NDSO,951) STRING
*
* === BRANCH BACK IN DATA LOOP ======================================= *
*
      GOTO 200
*
*     Error escape locations ----------------------------------------- *
*
c     GOTO 888
*
  800 CONTINUE
      WRITE (*,1000) FNAMEI, IERR
      STOP
*
  801 CONTINUE
      WRITE (*,1001) FNAMEI
      STOP
*
  802 CONTINUE
      WRITE (*,1002) FNAMEI, IERR
      STOP
*
  803 CONTINUE
      WRITE (*,1003) IDSTR, 'WAVEWATCH III SPECTRA'
      STOP
*
  804 CONTINUE
      WRITE (*,1004) NFR, NTH, NFRF, NTHF
      STOP
*
  805 CONTINUE
      WRITE (*,1005) POINT
      STOP
*
  810 CONTINUE
      WRITE (*,1010) FNAMEO, IERR
      STOP
*
  888 CONTINUE
      WRITE (NDSO,951) TAIL
      WRITE (NDSO,953) HSDROP, HSMIN
      WRITE (*,999)
*
*     Format statements ---------------------------------------------- *
*
  900 FORMAT (/' Splitting WAVEWATCH III spectra : '/
     &         ' ----------------------------------')
  901 FORMAT ( '    Location    : ',A/
     &         '    Run ID      : ',A)
  902 FORMAT ( '    Input file  : ',A,'  FORMATTED : ',L1)
  903 FORMAT ( '    Output file : ',A)
*
  904 FORMAT (/'    Input file opened ...')
  905 FORMAT ( '       Header read and processed.')
  906 FORMAT ( '       Frequency info read.')
  907 FORMAT ( '       Direction info read.')
  908 FORMAT ( '       Bin sizes preprocessed.')
*
  909 FORMAT (/'    Output file opened ...')
  910 FORMAT ( '       Processing ',I8.8,' ',I6.6,
     &         '  Number of peaks :',I3)
  911 FORMAT ( '       Processing ',I8.8,' ',I6.6,
     &         '  Number of peaks :',I3,'  (TRUNCATED)')
*
  950 FORMAT ( '  Location : ',A,' (',F5.2,A,1X,F6.2,A,')'/
     &         '  Model    : ',A/
     &         '  Cycle    : ',A)
  951 FORMAT (1X,A)
  952 FORMAT (' | day & |  Hst  n x |     Hs   Tp dir |',
     &                              '     Hs   Tp dir |',
     &                              '     Hs   Tp dir |',
     &                              '     Hs   Tp dir |',
     &                              '     Hs   Tp dir |',
     &                              '     Hs   Tp dir |'/
     &        ' |  hour |  (m)  - - |    (m)  (s) (d) |',
     &                              '    (m)  (s) (d) |',
     &                              '    (m)  (s) (d) |',
     &                              '    (m)  (s) (d) |',
     &                              '    (m)  (s) (d) |',
     &                              '    (m)  (s) (d) |')
  953 FORMAT (
     &  75X,'Hst : Total sigificant wave height.'/
     &  75X,'n   : Number of fields with Hs > ',f6.2,
     &           ' in 2-D spectrum.'/
     &  75X,'x   : Number of fields with Hs > ',f6.2,
     &           ' not in table.'/
     &  75X,'Hs  : Significant wave height of separate wave field.'/
     &  75X,'Tp  : Peak period of separate wave field.'/
     &  75X,'dir : Mean direction of separate wave field.'/
     &  75X,'*   : Wave generation due to local wind probable.')
*
  999 FORMAT (/' End of program '/)
*
 1000 FORMAT (/' *** ERROR IN OPENING INPUT FILE ',A/
     &         '     IOSTAT = ',I8/)
 1001 FORMAT (/' *** PREMATURE END OF INPUT FILE ',A/)
 1002 FORMAT (/' *** ERROR IN READING FROM INPUT FILE ',A/
     &         '     IOSTAT = ',I8/)
 1003 FORMAT (/' *** UNEXPECTED HEADER IN FILE : '/
     &         '     READ     : ',A/
     &         '     EXPECTED : ',A)
 1004 FORMAT (/' *** UNEXPECTED SPECTRAL DIMENSIONS : '/
     &         '     IN PROGRAM :',2I6/
     &         '     IN FILE    :',2I6/)
 1005 FORMAT (/' *** NO DATA FOUND FOR POINT ',A/)
*
 1010 FORMAT (/' *** ERROR IN OPENING OUTPUT FILE ',A/
     &         '     IOSTAT = ',I8/)
C/
C/ End of W3SPLT ----------------------------------------------------- /
C/
      END
C/ ------------------------------------------------------------------- /
      SUBROUTINE MPARS ( NFR, NTH, SPEC, MASK, FREQ, DIR, DFR, DTH,
     &                   TAILF, XFR, SP1D, HS, TP, DM )
C/
C/                  +-----------------------------------+
C/                  | WAVEWATCH III           NOAA/NCEP |
C/                  |           H. L. Tolman            |
C/                  |                        FORTRAN 77 |
C/                  | Last update :         15-Oct-1998 |
C/                  +-----------------------------------+
C/
C  1. Purpose :
C
C     Calculate mean parameters of a spectrum using a mask.
C
C  2. Method :
C
C     Wave height HS : Integral over spectrum using par. tail.
C     Peak period TP : Parabolic fit to 1-D spectrum.
C     Mean direction DM : From first Fourier components.
C
C  3. Parameters :
C
C     Parameter list
C     ----------------------------------------------------------------
C       NFR     Int.   I   Number of frequencies.
C       NTH     Int.   I   Number of directions.
C       SPEC    R.A.   I   Spectrum.
C       MASK    R.A.   I   Mask for spectrumi (real weight function).
C       FREQ    R.A.   I   Frequencies.
C       DIR     R.A.   I   Directions (radians).
C       DFR     R.A.   I   Bind width info for all frequencies.
C       DTH     R.A.   I   Directionsl increment.
C       TAILF   Real   I   Tail factor.
C       XFR     Real   I   Frequency increment factor.
C       SP1D    R.A.   O   1-D spectrum.
C       HS      Real   O   Wave height              -999.99 if undef.
C       TP      Real   O   Peak wave period         -999.99 if undef.
C       DM      Real   O   Mean wave direction      -999.99 if undef.
C     ----------------------------------------------------------------
C
C  4. Subroutines used :
C
C       None.
C
C  5. Called by :
C
C       Any.
C
C 10. Source code :
C
C/ ------------------------------------------------------------------- /
C/ Parameter list
C/
      INTEGER         NFR, NTH
      REAL            SPEC(NFR,NTH), MASK(NFR,NTH), FREQ(NFR), DIR(NTH),
     &                DFR(NFR), DTH, TAILF, XFR, SP1D(NFR), HS, TP, DM
C/
C/ ------------------------------------------------------------------- /
C/ Local parameters
C/
      INTEGER         IFR, ITH, IMAX, ILOW, IHGH
      REAL            COSMOM, SINMOM, TOTMOM, COS0, SIN0, PI, RADE, EMAX
      REAL            XL, XH, XL2, XH2, EL, EH, DENOM, FP
C/
C/ ------------------------------------------------------------------- /
*
      PI     = 4. * ATAN(1.)
      RADE   = 180./PI
*
      XL     = 1./XFR - 1.
      XH     =  XFR - 1.
      XL2    = XL**2
      XH2    = XH**2
*
      COSMOM = 0.
      SINMOM = 0.
      TOTMOM = 0.
*
      DO 110, IFR=1, NFR
*
        SP1D(IFR) = 0.
        COS0      = 0.
        SIN0      = 0.
*
        DO 100, ITH=1, NTH
          SP1D(IFR) = SP1D(IFR) + MASK(IFR,ITH)*SPEC(IFR,ITH)
          COS0      = COS0 + MASK(IFR,ITH)*SPEC(IFR,ITH) * COS(DIR(ITH))
          SIN0      = SIN0 + MASK(IFR,ITH)*SPEC(IFR,ITH) * SIN(DIR(ITH))
  100     CONTINUE
*
        SP1D(IFR) = SP1D(IFR) * DTH
        COSMOM    = COSMOM + COS0 * DTH * DFR(IFR)
        SINMOM    = SINMOM + SIN0 * DTH * DFR(IFR)
        TOTMOM    = TOTMOM + SP1D(IFR)*DFR(IFR)
*
  110   CONTINUE
*
      TOTMOM = TOTMOM + SP1D(NFR)*TAILF
      TOTMOM = MAX ( 0. , TOTMOM )
*
* 2.  Mean parameters ------------------------------------------------ *
* 2.a Set to undefined
*
      HS     =    0.00
      TP     = -999.99
      DM     = -999.99
*
      IF ( TOTMOM .GT. 0. ) THEN
*
* 2.b Wave height
*
          HS     = 4. * SQRT(TOTMOM)
*
* 2.c Mean direction
*
          DM     = RADE * ATAN2(SINMOM,COSMOM)
          IF ( DM .LT. 0. ) DM = DM + 360.
*
* 2.d Peak period
*
          EMAX   = 0.
*
          DO 200, IFR=1, NFR
            IF ( SP1D(IFR) .GT. EMAX ) THEN
                EMAX   = SP1D(IFR)
                IMAX   = IFR
              ENDIF
  200       CONTINUE
*
          ILOW   = MAX (  1 , IMAX-1 )
          IHGH   = MIN ( NFR , IMAX+1 )
          EL     = SP1D(ILOW) - SP1D(IMAX)
          EH     = SP1D(IHGH) - SP1D(IMAX)
          DENOM  = XL*EH - XH*EL
          FP     = FREQ(IMAX) * ( 1. + 0.5 * ( XL2*EH - XH2*EL )
     &                 / SIGN ( MAX(ABS(DENOM),1.E-15) , DENOM ) )
*
          TP     = 1. / FP
*
        ENDIF
*
      RETURN
C/
C/ End of MPARS ------------------------------------------------------ /
C/
      END
C/ ------------------------------------------------------------------- /
      SUBROUTINE FNDPRT ( NFR, NTH, IFRC, ITHC, SPEC, W1, W2 )
C/
C/                  +-----------------------------------+
C/                  | WAVEWATCH III           NOAA/NCEP |
C/                  |           H. L. Tolman            |
C/                  |                        FORTRAN 77 |
C/                  | Last update :         09-Sep-1998 |
C/                  +-----------------------------------+
C/
C  1. Purpose :
C
C     Find partition starting at given peak and given mask.
C
C  2. Method :
C
C  3. Parameters :
C
C     Parameter list
C     ----------------------------------------------------------------
C       NFR     Int.   I   Number of frequencies.
C       NTH     Int.   I   Number of directions.
C       IFRC    Int.   I   Peak discrete frequency.
C       ITHC    Int.   I   Peak discrete direction.
C       SPEC    R.A.   I   Spectrum.
C       W1      R.A.  I/O  Map of bins used so far.
C       W2      R.A.   O   Map of bins used now.
C     ----------------------------------------------------------------
C
C  4. Subroutines used :
C
C       None.
C
C  5. Called by :
C
C       Any.
C
C 10. Source code :
C
C/ ------------------------------------------------------------------- /
C/ Parameter list
C/
      INTEGER         NFR, NTH, IFRC, ITHC
      REAL            SPEC(NFR,NTH), W1(NFR,NTH), W2(NFR,NTH)
C/
C/ ------------------------------------------------------------------- /
C/ Local parameters
C/
      INTEGER         IFR, ITH, IFRL, ITHL
      LOGICAL         CHANGE, ADD
C/
C/ ------------------------------------------------------------------- /
*
* 1.  Set up the W2 map ---------------------------------------------- *
*
      DO 110, IFR=1, NFR
        DO 100, ITH=1, NTH
          W2(IFR,ITH) = 0.
  100     CONTINUE
  110   CONTINUE
*
      IFRL   = MAX ( 1 , IFRC-1 )
      IFRH   = MIN ( NFR , IFRC+1 )
      ITHL   = 1 + MOD(NTH+ITHC-2,NTH)
      ITHH   = 1 + MOD(ITHC,NTH)
*
      DO 130, ITH=ITHC-1, ITHC+1
        ITHL = 1 + MOD(NTH+ITH-1,NTH)
        DO 120, IFR=IFRC-1, IFRC+1
          IFRL = MAX(1,MIN(NFR,IFR))
          IF ( W1(IFRL,ITHL) .LE. 0.5 ) W2(IFRL,ITHL) = 0.5
  120     CONTINUE
  130   CONTINUE
*
      IF ( W1(IFRC,ITHC) .LT. 0.25 ) W2(IFRC,ITHC) = 1.0
*
* 2.  Itterate search ------------------------------------------------ *
*
      NITT   = 0
*
* 2.a Branch point
*
  200 CONTINUE
      NITT   = NITT + 1
      CHANGE = .FALSE.
*
* 2.b Determine central points
*
      DO 240, IFR=1, NFR
        DO 230, ITH=1, NTH
*
          IF ( W2(IFR,ITH).EQ.0.5 .AND. W1(IFR,ITH).LT.0.5 ) THEN
              ADD    = .TRUE.
              DO 220, ITHR=ITH-1, ITH+1
                ITHL = 1 + MOD(NTH+ITHR-1,NTH)
                DO 210, IFRL=MAX(1,IFR-1), MIN(NFR,IFR+1)
                  IF ( W2 (IFRL,ITHL).EQ.0. .AND. 
     &                SPEC(IFRL,ITHL).GT.SPEC(IFR,ITH) ) ADD = .FALSE.
  210             CONTINUE
  220           CONTINUE
              IF ( ADD ) W2(IFR,ITH) = 1.
              CHANGE = CHANGE .OR. ADD
            ENDIF
*
  230     CONTINUE
  240   CONTINUE
*
* 2.c Determine central points
*
      DO 280, IFR=1, NFR
        DO 270, ITH=1, NTH
*
          IF ( W2(IFR,ITH).EQ.0. ) THEN
              ADD    = .FALSE.
              DO 260, ITHR=ITH-1, ITH+1
                ITHL = 1 + MOD(NTH+ITHR-1,NTH)
                DO 250, IFRL=MAX(1,IFR-1), MIN(NFR,IFR+1)
                  IF ( W2 (IFRL,ITHL).EQ.1. ) ADD = .TRUE.
  250             CONTINUE
  260           CONTINUE
              IF ( ADD ) W2(IFR,ITH) = 0.5
              CHANGE = CHANGE .OR. ADD
            ENDIF
*
  270     CONTINUE
  280   CONTINUE
*
* 2.d Branch back ?
*
      IF ( CHANGE .AND. NITT.LT.25 ) GOTO 200
*
* 3   Update the overall map ----------------------------------------- *
*
      DO 310, IFR=1, NFR
        DO 300, ITH=1, NTH
          W1(IFR,ITH) = W1(IFR,ITH) + W2(IFR,ITH)
  300     CONTINUE
  310   CONTINUE
*
      RETURN
C/
C/ End of FNDPRT ----------------------------------------------------- /
C/
      END
C/ ------------------------------------------------------------------- /
      SUBROUTINE WAVNU2 (W,H,K,CG,EPS,NMAX,ICON)
C/
C/                  +-----------------------------------+
C/                  | WAVEWATCH III           NOAA/NCEP |
C/                  |           H. L. Tolman            |
C/                  |                        FORTRAN 77 |
C/                  | Last update :         17-Jul-1990 |
C/                  +-----------------------------------+
C/
C  1. Purpose :
C
C     Calculation of wavenumber K from a given angular
C     frequency W and waterdepth H.
C
C  2. Method :
C
C     Used equation :
C                        2
C                       W  = G*K*TANH(K*H)
C
C     Because of the nature of the equation, K is calculated
C     with an itterative procedure.
C
C  3. Parameters :
C
C     Parameter list
C     ----------------------------------------------------------------
C       W       Real   I   Angular frequncy
C       H       Real   I   Waterdepth
C       K       Real   O   Wavenumber ( same sign as W )
C       CG      Real   O   Group velocity (same sign as W)
C       EPS     Real   I   Wanted max. difference between K and Kold
C       NMAX    Int.   I   Max number of repetitions in calculation
C       ICON    Int.   O   Contol counter ( See error messages )
C     ----------------------------------------------------------------
C
C  9. Switches :
C
C     C/S  Enable subroutine tracing.
C
C 10. Source code :
C/
C/ ------------------------------------------------------------------- /
C/ Parameter list
C/
      INTEGER         ICON, NMAX
      REAL            EPS, CG, K, H, W
C/
C/ ------------------------------------------------------------------- /
C/ Local parameters
C/
      INTEGER         I
C/S      INTEGER         IENT
      REAL            G, F, W0, FD, DIF, RDIF, KOLD
C/
C/ ------------------------------------------------------------------- /
C/
      DATA G / 9.81 /
*
*     Initialisations :
*
      CG   = 0
      KOLD = 0
      ICON = 0
      W0   = ABS(W)
*
*     1st approach :
*
      IF (W0.LT.SQRT(G/H)) THEN
          K = W0/SQRT(G*H)
        ELSE
          K = W0*W0/G
        END IF
*
*     Refinement :
*
      DO 7, I=1, NMAX
        DIF = ABS(K-KOLD)
      IF (K.NE.0) THEN
          RDIF = DIF/K
        ELSE
          RDIF = 0
        END IF
      IF (DIF .LT. EPS .AND. RDIF .LT. EPS) THEN
          ICON = 1
          GOTO 8
        ELSE
          KOLD = K
          F    = G*KOLD*TANH(KOLD*H)-W0**2
          IF (KOLD*H.GT.25) THEN
              FD = G*TANH(KOLD*H)
            ELSE
              FD = G*TANH(KOLD*H) + G*KOLD*H/((COSH(KOLD*H))**2)
            END IF
          K    = KOLD - F/FD
        END IF
7       CONTINUE
      DIF   = ABS(K-KOLD)
      RDIF  = DIF/K
      IF (DIF .LT. EPS .AND. RDIF .LT. EPS) ICON = 1
8     CONTINUE
      IF (2*K*H.GT.25) THEN
          CG = W0/K * 0.5
        ELSE
          CG = W0/K * 0.5*(1+(2*K*H/SINH(2*K*H)))
        END IF
      IF (W.LT.0.0) THEN
          K  = (-1)*K
          CG = CG*(-1)
        END IF
*
      RETURN
C/
C/ End of WAVNU2 ----------------------------------------------------- /
C/
      END
