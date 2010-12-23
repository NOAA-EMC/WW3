!/ ------------------------------------------------------------------- /
      PROGRAM PROCESS
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Oct-2010 |
!/                  +-----------------------------------+
!/
!/    13-Nov-2008 : Origination.
!/    04-Dec-2008 : Add depth to output file.
!/    22-Dec-2008 : Bug fix error test label 807.
!/    24-Dec-2008 : Allow larger maximum error.
!/    14-Jan-2009 : Increase accuracy of error.
!/    04-Feb-2010 : Fix FORMAT warnings.
!/    20-Oct-2010 : Fix bug in reading TX, TY from part?.ww3
!/    22-Oct-2010 : Adjust T0 for Great Lakes test case.
!/
!/    Copyright 2008-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Take standard raw output files from one or two model runs and
!     process to full set of test parameters. If two model runs are
!     given, errors of the first with respect to the second are
!     computed.
!
!  2. Method :
!
!     Read from files spec1.data, spec2.data, source1.data, 
!     source2.data, part1.data and part2.data. The files identified
!     as '2' are optional.
!
!  3. Parameters :
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      PRT2DS    Subr  W3ARRYMD Print plot of spectrum.
!      DSEC21    Subr  W3TIMEMD Difference in times.
!      WAVNU2    Sybr  W3DISPMD Solve dispersion relation.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     None, stand-alone program.
!
!  6. Error messages :
!
!     - See error escape locations at end of code.
!
!  7. Remarks :
!
!     - When run with one set of raw data, processed data files are
!       produced, when run with two sets of data, error data files
!       are produced.
!
!  8. Structure :
!
!     - See source code.
!
!  9. Switches :
!
!     - This is a true FORTRAN file without switches.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
      USE W3ARRYMD
      USE W3TIMEMD
      USE W3DISPMD
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETER statements
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NF, IERR, NFR, NTH, NRQ, TTST(2),    &
                                 NT, IRQ, J, I, NRP, IRP, NFT, NTT,   &
                                 NRT, IFR, ITH, T0(2), NTOT(14)
      INTEGER, ALLOCATABLE    :: TIME(:,:)
      REAL                    :: XFR, TX, TY, TD, TUA, TUD, TCA, TCD, &
                                 XP(6), DTH, SP1MAX, XX, YY, MM1MAX,  &
                                 RTIME, ESUM(14), ECOM, DTNORM
      REAL                    :: EHS(0:2), EFP(0:2), ETM(0:2),        &
                                 ESI(0:2), EAL, EF0, EBE, EF1, EG1,   &
                                 EN1, ET1, ES1, EF2, EG2, EN2
      REAL, ALLOCATABLE       :: FRQ(:), SIG(:), TH(:), TSP(:,:),     &
                                 XA(:), YA(:), DA(:), UA(:), UD(:),   &
                                 SPC(:,:,:,:), XXX(:), SRC(:,:,:,:),  &
                                 HS(:,:,:), FP(:,:,:), TM(:,:,:),     &
                                 SM(:,:,:), DFR(:), WN(:), CG(:),     &
                                 SPS(:,:,:,:), SP1(:,:,:),            &
                                 SS1(:,:,:), NL1(:,:,:), TH1(:,:,:),  &
                                 SI1(:,:,:), MM1(:,:,:), ALP(:,:),    &
                                 WN1(:,:), FR0(:,:)
      LOGICAL                 :: OK
      CHARACTER(LEN=10)       :: TLOC
      CHARACTER(LEN=12)       :: FNAME
      CHARACTER(LEN=21)       :: TSTR
      CHARACTER(LEN=21)       :: TST2
      CHARACTER(LEN=30)       :: GNAME, TNAME
      CHARACTER(LEN=144)      :: STRING
!/
!/ ------------------------------------------------------------------- /
!/
!     DATA T0 / 19680606 , 0 /
      DATA DTNORM / 90. /
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 0.  Initialization
!
      WRITE (*,900)
!
! 0.a Test existence of files
!
      WRITE (*,901)
!
      FNAME = 'spec1.data'
      INQUIRE (FILE=FNAME,EXIST=OK)
      IF ( OK ) THEN
          WRITE (*,902) FNAME
        ELSE
          GOTO 801
        END IF
!
      FNAME = 'source1.data'
      INQUIRE (FILE=FNAME,EXIST=OK)
      IF ( OK ) THEN
          WRITE (*,902) FNAME
        ELSE
          GOTO 801
        END IF
!
      FNAME = 'part1.data'
      INQUIRE (FILE=FNAME,EXIST=OK)
      IF ( OK ) THEN
          WRITE (*,902) FNAME
        ELSE
          GOTO 801
        END IF
!
      FNAME = 'spec2.data'
      INQUIRE (FILE=FNAME,EXIST=OK)
      IF ( OK ) THEN
          WRITE (*,902) FNAME
          NF     = 2
        ELSE
          NF     = 1
          WRITE (*,903)
        END IF
!
      IF ( NF .EQ. 2 ) THEN
!
          FNAME = 'source2.data'
          INQUIRE (FILE=FNAME,EXIST=OK)
          IF ( OK ) THEN
              WRITE (*,902) FNAME
            ELSE
              GOTO 801
            END IF
!
          FNAME = 'part2.data'
          INQUIRE (FILE=FNAME,EXIST=OK)
          IF ( OK ) THEN
              WRITE (*,902) FNAME
            ELSE
              GOTO 801
            END IF
!
        END IF
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Read files
! 1.a Get array s1zes from first file
!
      WRITE (*,910)
!
! 1.a.1 Open file
!
      FNAME = 'spec1.data'
      OPEN (10,FILE=FNAME,FORM='UNFORMATTED',ERR=802,IOSTAT=IERR)
!
! 1.a.2 Process header
!
      READ (10) TSTR, NFR, NTH, NRQ, GNAME
      WRITE (*,911) NFR, NTH, NRQ, GNAME
      ALLOCATE ( FRQ(NFR), SIG(NFR), TH(NTH) )
      READ (10) FRQ
      XFR    = FRQ(2) / FRQ(1)
      WRITE (*,912) FRQ(1), XFR
      READ (10) TH
!
! 1.a.3 Process rest of file
!
      NT     = 0
      ALLOCATE ( TSP(NFR,NTH) )
!
     DO
        READ (10,END=101,ERR=803,IOSTAT=IERR) TTST
!       WRITE (*,913) TTST
        DO IRQ=1, NRQ
          READ (10,ERR=803,IOSTAT=IERR) TLOC, TY, TX, TD
!         WRITE (*,914) TLOC, TX*1.E-3, TY*1.E-3, TD
          READ (10,ERR=803,IOSTAT=IERR) TSP
          END DO
        NT     = NT + NRQ
!
        END DO
!
  101 CONTINUE
!
      WRITE (*,915) NT
!
      ALLOCATE ( TIME(2,NT), XA(NT), YA(NT), DA(NT), UA(NT), UD(NT) )
      ALLOCATE ( SPC(NFR,NTH,NT,NF) , SRC(NFR,NTH,NT,NF) ,            &
                 SPS(NFR,NTH,NT,NF) )
!
      ALLOCATE ( HS(0:2,NT,NF), FP(0:2,NT,NF), TM(0:2,NT,NF),         &
                 SM(0:2,NT,NF) )
      HS     = -1.
      FP     = -1.
      TM     = -1.
      SM     = -1.
!
      ALLOCATE ( SP1(NFR,NT,NF) , SS1(NFR,NT,NF) , NL1(NFR,NT,NF) )
      ALLOCATE ( TH1(NFR,NT,NF) , SI1(NFR,NT,NF) , MM1(NFR,NT,NF) )
      TH1    = -181.
      SI1    = -1.
      ALLOCATE ( WN1(NFR,NT) )
!
      ALLOCATE ( ALP(NT,NF) , FR0(NT,NF) )
      ALP    = -1.
      FR0    = -1.
!
! 1.b Read spec1.data
!
      WRITE (*,916) FNAME
      REWIND (10)
      READ (10) TSTR, NFR, NTH, NRQ, GNAME
      READ (10) FRQ
      READ (10) TH
! 
      DO J=1, NT/NRQ
!
        READ (10,END=803,ERR=803,IOSTAT=IERR) TTST
!
        DO IRQ=1, NRQ
!
          I      = (J-1)*NRQ + IRQ
          TIME(:,I) = TTST
!
          READ (10,ERR=803,IOSTAT=IERR) TLOC, TY, TX, TD,            &
                                        TUA, TUD, TCA, TCD
          XA(I)  = TX
          YA(I)  = TY
          DA(I)  = TD
          UA(I)  = TUA
          UD(I)  = TUD
!
          READ (10,ERR=803,IOSTAT=IERR) SPC(:,:,I,1)
!         CALL  PRT2DS (6, NFR, NFR, NTH, SPC(:,:,I,1), FRQ, 'HZ',   &
!                       1., 0., 0.0001, 'F(f,t)', 'm2s', 'TEST')
!
          END DO
!
        END DO
!
      CLOSE (10)
!
! 1.c Read source1.data
!
      FNAME = 'source1.data'
      OPEN (10,FILE=FNAME,FORM='UNFORMATTED',ERR=802,IOSTAT=IERR)
      WRITE (*,916) FNAME
!
      READ (10,ERR=803,IOSTAT=IERR) TSTR, NFT, NTT, NRT
      IF ( NFR.NE.NFT .OR. NTH.NE.NTT .OR. NRQ.NE.NRT ) GOTO 804
      ALLOCATE ( XXX(NFR) )
      READ (10,ERR=803,IOSTAT=IERR) XXX
      DEALLOCATE ( XXX )
      ALLOCATE ( XXX(NTH) )
      READ (10,ERR=803,IOSTAT=IERR) XXX
      DEALLOCATE ( XXX )
!
      DO J=1, NT/NRQ
! 
        READ (10,END=803,ERR=803,IOSTAT=IERR) TTST
!
        DO IRQ=1, NRQ
!
          I      = (J-1)*NRQ + IRQ
          IF ( DSEC21(TTST,TIME(:,I)) .NE. 0. ) GOTO 806
!
          READ (10,ERR=803,IOSTAT=IERR) TLOC, TY, TX, TD,            &
                                        TUA, TUD, TCA, TCD
          IF ( ABS(TX-XA(I)).GT.60. .OR.                             &
               ABS(TY-YA(I)).GT.60. .OR.                             &
               ABS(TD-DA(I)).GT.0.06 .OR.                            &
               ABS(TUA-UA(I)).GT.0.06 .OR.                           &
               ABS(TUD-UD(I)).GT.0.06 ) GOTO 807
!
          READ (10,ERR=803,IOSTAT=IERR) SRC(:,:,I,1)
!         CALL  PRT2DS (6, NFR, NFR, NTH, SRC(:,:,I,1), FRQ, 'HZ',   &
!                       1., 0., 0.0001, 'Snl(f,t)', 'm2', 'TEST')
!
          END DO
!
        END DO 
!
      CLOSE (10)
!
! 1.d Read part1.data
!
      FNAME = 'part1.data'
      OPEN (10,FILE=FNAME,FORM='FORMATTED',ERR=802,IOSTAT=IERR)
      WRITE (*,916) FNAME
!
      DO I=1, NT
        READ (10,*,ERR=803,IOSTAT=IERR) TTST, TY, TX, TLOC, NRP, TD,  &
                                        TUA, TUD, TCA, TCD
        IF ( DSEC21(TTST,TIME(:,I)) .NE. 0. ) GOTO 806
        IF ( ABS(TX-XA(I)).GT.60. .OR.                                &
             ABS(TY-YA(I)).GT.60. .OR.                                &
             ABS(TD-DA(I)).GT.0.06 .OR.                               &
             ABS(TUA-UA(I)).GT.0.06 .OR.                              &
             ABS(TUD-UD(I)).GT.0.06 ) GOTO 807
        DO IRP=0, NRP
          READ (10,*,ERR=803,IOSTAT=IERR) J, XP
          IF ( IRP .LE. 2 ) THEN
              HS(IRP,I,1) = XP(1)
              FP(IRP,I,1) = 1./XP(2)
              TM(IRP,I,1) = XP(4)
              SM(IRP,I,1) = XP(5)
            END IF
          END DO
        END DO
!
      CLOSE (10)
!
! 1.e Read spec2.data
!
      IF ( NF .GT. 1 ) THEN
!
          FNAME = 'spec2.data'
          OPEN (10,FILE=FNAME,FORM='UNFORMATTED',ERR=802,IOSTAT=IERR)
          WRITE (*,916) FNAME
!
          READ (10) TSTR, NFT, NTT, NRT, TNAME
          IF ( NFR.NE.NFT .OR. NTH.NE.NTT .OR. NRQ.NE.NRT .OR.       &
               GNAME.NE.TNAME ) GOTO 805
          ALLOCATE ( XXX(NFR) )
          READ (10,ERR=803,IOSTAT=IERR) XXX
          DEALLOCATE ( XXX )
          ALLOCATE ( XXX(NTH) )
          READ (10,ERR=803,IOSTAT=IERR) XXX
          DEALLOCATE ( XXX )
! 
          DO J=1, NT/NRQ
!
            READ (10,END=803,ERR=803,IOSTAT=IERR) TTST
!
            DO IRQ=1, NRQ
!
              I      = (J-1)*NRQ + IRQ
              TIME(:,I) = TTST
!
              READ (10,ERR=803,IOSTAT=IERR) TLOC, TY, TX, TD,        &
                                            TUA, TUD, TCA, TCD
              IF ( ABS(TX-XA(I)).GT.60. .OR.                          &
                   ABS(TY-YA(I)).GT.60. .OR.                          &
                   ABS(TD-DA(I)).GT.0.06 .OR.                         &
                   ABS(TUA-UA(I)).GT.0.06 .OR.                        &
                   ABS(TUD-UD(I)).GT.0.06 ) GOTO 807
!
              READ (10,ERR=803,IOSTAT=IERR) SPC(:,:,I,2)
!         CALL  PRT2DS (6, NFR, NFR, NTH, SPC(:,:,I,2), FRQ, 'HZ',   &
!                       1., 0., 0.0001, 'F(f,t)', 'm2s', 'TEST')
!
              END DO
!
            END DO
!
          CLOSE (10)
!
! 1.f Read source2.data
!
          FNAME = 'source2.data'
          OPEN (10,FILE=FNAME,FORM='UNFORMATTED',ERR=802,IOSTAT=IERR)
          WRITE (*,916) FNAME
!
          READ (10,ERR=803,IOSTAT=IERR) TSTR, NFT, NTT, NRT
          IF ( NFR.NE.NFT .OR. NTH.NE.NTT .OR. NRQ.NE.NRT ) GOTO 804
          ALLOCATE ( XXX(NFR) )
          READ (10,ERR=803,IOSTAT=IERR) XXX
          DEALLOCATE ( XXX )
          ALLOCATE ( XXX(NTH) )
          READ (10,ERR=803,IOSTAT=IERR) XXX
          DEALLOCATE ( XXX )
!
          DO J=1, NT/NRQ
! 
            READ (10,END=803,ERR=803,IOSTAT=IERR) TTST
!
            DO IRQ=1, NRQ
!
          I      = (J-1)*NRQ + IRQ
          IF ( DSEC21(TTST,TIME(:,I)) .NE. 0. ) GOTO 806
!
              READ (10,ERR=803,IOSTAT=IERR) TLOC, TY, TX, TD,        &
                                            TUA, TUD, TCA, TCD
              IF ( ABS(TX-XA(I)).GT.60. .OR.                          &
                   ABS(TY-YA(I)).GT.60. .OR.                          &
                   ABS(TD-DA(I)).GT.0.06 .OR.                         &
                   ABS(TUA-UA(I)).GT.0.06 .OR.                        &
                   ABS(TUD-UD(I)).GT.0.06 ) GOTO 807
!
              READ (10,ERR=803,IOSTAT=IERR) SRC(:,:,I,2)
!         CALL  PRT2DS (6, NFR, NFR, NTH, SRC(:,:,I,2), FRQ, 'HZ',   &
!                       1., 0., 0.0001, 'Snl(f,t)', 'm2', 'TEST')
!
              END DO
!
            END DO 
!
          CLOSE (10)
!
! 1.g Read part2.data
!
          FNAME = 'part2.data'
          OPEN (10,FILE=FNAME,FORM='FORMATTED',ERR=802,IOSTAT=IERR)
          WRITE (*,916) FNAME
!
          DO I=1, NT
            READ (10,*,ERR=803,IOSTAT=IERR) TTST, TY, TX, TLOC, NRP,  &
                                            TD, TUA, TUD, TCA, TCD
            IF ( DSEC21(TTST,TIME(:,I)) .NE. 0. ) GOTO 806
            IF ( ABS(TX-XA(I)).GT.60. .OR.                            &
                 ABS(TY-YA(I)).GT.60. .OR.                            &
                 ABS(TD-DA(I)).GT.0.06 .OR.                           &
                 ABS(TUA-UA(I)).GT.0.06 .OR.                          &
                 ABS(TUD-UD(I)).GT.0.06 ) GOTO 807
            DO IRP=0, NRP
              READ (10,*,ERR=803,IOSTAT=IERR) J, XP
              IF ( IRP .LE. 2 ) THEN
                  HS(IRP,I,2) = XP(1)
                  FP(IRP,I,2) = 1./XP(2)
                  TM(IRP,I,2) = XP(4)
                  SM(IRP,I,2) = XP(5)
                END IF
              END DO
            END DO
!
          CLOSE (10)
!
        END IF
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2. Process data
!
      WRITE (*,920)
!
      ALLOCATE ( DFR(NFR), WN(NFR), CG(NFR) )
      DFR    = FRQ * 0.5 * (XFR-1./XFR)
      DTH    = TPI / REAL(NTH)
!
! 2.a Loop over points
!
      DO I=1, NT
!
! 2.b Set wavenumbers etc.
!
        DO J=1, NFR
          CALL WAVNU2 ( FRQ(J)*TPI, DA(I), WN(J), CG(J), 1.E-7,25,IERR)
          END DO
!
        WN1(:,I) = WN
!
! 2.c Loop over cases
!
        DO J=1, NF
!
! 2.c.1 Direct 1-D spectral data
!
          DO IFR=1, NFR
            SPS(IFR,:,I,J) = SPC(IFR,:,I,J) * WN(IFR)**2
            SP1(IFR,  I,J) = SUM ( SPC(IFR,:,I,J) ) * DTH
            SS1(IFR,  I,J) = SP1(IFR,I,J) * WN(IFR)**2
            NL1(IFR,  I,J) = SUM ( SRC(IFR,:,I,J) ) * DTH
            END DO
!
! 2.c.2 Dirived or limited range 1-D spectral data
!
          SP1MAX = MAXVAL(SP1(:,I,J)) * 0.001
!
          DO IFR=1, NFR
            IF ( SP1(IFR,I,J) .GT. SP1MAX ) THEN
                XX     = 0.
                YY     = 0.
                DO ITH=1, NTH
                  XX     = XX + SPC(IFR,ITH,I,J) * COS(TH(ITH))
                  YY     = YY + SPC(IFR,ITH,I,J) * SIN(TH(ITH))
                  END DO
                XX     = XX * DTH
                YY     = YY * DTH
                TH1(IFR,I,J) = ATAN2(YY,XX) * RADE
                TH1(IFR,I,J) = MOD ( TH1(IFR,I,J)+180. , 360. ) - 180.
                SI1(IFR,I,J) = SQRT ( MAX ( 0. , 2. * ( 1. - SQRT (   &
                  MAX(0.,(XX**2+YY**2)/SP1(IFR,I,J)**2) ) ) ) ) * RADE
              END IF
            END DO
!
          MM1(1,I,J) = NL1(1,I,J) * 0.5 * DFR(1)
          DO IFR=2, NFR
            MM1(IFR,I,J) = MM1(IFR-1,I,J) + 0.5*(FRQ(IFR)-FRQ(IFR-1)) &
                                    * (NL1(IFR-1,I,J)+NL1(IFR,I,J))
            END DO
!
! 2.c.3 Dirived or limited range 1-D spectral data
!
          ALP(I,J) = TPI**2 / GRAV**2 * SP1(NFR,I,J) / FRQ(NFR)**5
!
          MM1MAX = MAXVAL ( MM1(:,I,J) )
          DO IFR=1, NFR
            IF ( MM1(IFR,I,J) .EQ. MM1MAX ) EXIT
            END DO
          DO IFR=IFR, NFR
            IF ( MM1(IFR,I,J) .LT. 0. ) EXIT
            END DO
!
          IF ( IFR .LE. NFR ) THEN
              IF ( MM1(IFR-1,I,J)*MM1(IFR,I,J) .LT. 0. ) THEN
                  FR0(I,J) = FRQ(IFR-1) + (FRQ(IFR)-FRQ(IFR-1))       &
                     * MM1(IFR-1,I,J) / (MM1(IFR-1,I,J)-MM1(IFR,I,J))
                END IF
            END IF
!
! ... End loop 2.c
!
          END DO
!
! ... End loop 2.a
!
        END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3. Process errors
!
      IF ( NF .EQ. 2 ) THEN
!
          WRITE (*,930)
!
! 3.a Mean parameters
!
          EHS    = -1.
          EFP    = -1.
          ETM    = -1.
          ESI    = -1.
          EAL    = -1.
          EF0    = -1.
!
          ESUM   = 0.
          NTOT   = 0
!
          DO I=1, NT
!
            ESUM( 1) = ESUM( 1) + (HS(0,I,1)/HS(0,I,2)-1.)**2
            IF ( HS(1,I,2) .GT. 0.05 ) THEN
                ECOM   = MAX ( 0.  , HS(1,I,1) )
                ESUM( 2) = ESUM( 2) + (ECOM/HS(1,I,2)-1.)**2
                NTOT( 2) = NTOT( 2) + 1
              END IF
            IF ( HS(2,I,2) .GT. 0.05 ) THEN
                ECOM   = MAX ( 0.  , HS(2,I,1) )
                ESUM( 3) = ESUM( 3) + (ECOM/HS(2,I,2)-1.)**2
                NTOT( 3) = NTOT( 3) + 1
              END IF
!
            ESUM( 4) = ESUM( 4) + (FP(0,I,1)/FP(0,I,2)-1.)**2
            IF ( FP(1,I,2) .GT. 0.001 ) THEN
                ECOM   = MAX ( 0.  , FP(1,I,1) )
                ESUM( 5) = ESUM( 5) + (ECOM/FP(1,I,2)-1.)**2
                NTOT( 5) = NTOT( 5) + 1
              END IF
            IF ( FP(2,I,2) .GT. 0.001 ) THEN
                ECOM   = MAX ( 0.  , FP(2,I,1) )
                ESUM( 6) = ESUM( 6) + (ECOM/FP(2,I,2)-1.)**2
                NTOT( 6) = NTOT( 6) + 1
              END IF
!
            ESUM( 7) = ESUM( 7) + (TM(0,I,1)-TM(0,I,2))**2
            IF ( TM(1,I,2) .GT. 0.001 ) THEN
                IF ( TM(1,I,1) .LT. 0. ) THEN
                    ECOM = TM(1,I,2) + DTNORM
                  ELSE
                    ECOM = TM(1,I,1)
                  END IF
                ESUM( 8) = ESUM( 8) + (ECOM-TM(1,I,2))**2
                NTOT( 8) = NTOT( 8) + 1
              END IF
            IF ( TM(2,I,2) .GT. 0.001 ) THEN
                IF ( TM(2,I,1) .LT. 0. ) THEN
                    ECOM = TM(2,I,2) + DTNORM
                  ELSE
                    ECOM = TM(2,I,1)
                  END IF
                ESUM( 9) = ESUM( 9) + (ECOM-TM(2,I,2))**2
                NTOT( 9) = NTOT( 9) + 1
              END IF
!
            ESUM(10) = ESUM(10) + (SM(0,I,1)/SM(0,I,2)-1.)**2
            IF ( SM(1,I,2) .GT. 0.1 ) THEN
                ECOM   = MAX ( 0.  , SM(1,I,1) )
                ESUM(11) = ESUM(11) + (ECOM/SM(1,I,2)-1.)**2
                NTOT(11) = NTOT(11) + 1
              END IF
            IF ( SM(2,I,2) .GT. 0.1 ) THEN
                ECOM   = MAX ( 0.  , SM(2,I,1) )
                ESUM(12) = ESUM(12) + (ECOM/SM(2,I,2)-1.)**2
                NTOT(12) = NTOT(12) + 1
              END IF
!
            IF ( ALP(I,2) .GT. 0.001 ) THEN
                ESUM(13) = ESUM(13) + (ALP(I,1)/ALP(I,2)-1.)**2
                NTOT(13) = NTOT(13) + 1
              END IF
            IF ( FR0(I,2) .GT. 0.001 ) THEN
                ECOM   = MAX ( 0.  , FR0(I,1) )
                ESUM(14) = ESUM(14) + (ECOM/FR0(I,2)-1.)**2
                NTOT(14) = NTOT(14) + 1
              END IF
!
            END DO
!
          EHS(0) = SQRT( ESUM(1) / REAL(NT) ) * 100.
          IF ( NTOT(2) .GE. 1 )                                       &
              EHS(1) = SQRT( ESUM(2) / REAL(NTOT(2)) ) * 100.
          IF ( NTOT(3) .GE. 1 )                                       &
              EHS(2) = SQRT( ESUM(3) / REAL(NTOT(3)) ) * 100.
          EHS    = MIN ( 9999.99 , EHS )
!
          EFP(0) = SQRT( ESUM(4) / REAL(NT) ) * 100.
          IF ( NTOT(5) .GE. 1 )                                       &
              EFP(1) = SQRT( ESUM(5) / REAL(NTOT(5)) ) * 100.
          IF ( NTOT(6) .GE. 1 )                                       &
              EFP(2) = SQRT( ESUM(6) / REAL(NTOT(6)) ) * 100.
          EFP    = MIN ( 9999.99 , EFP )
!
          ETM(0) = SQRT( ESUM(7) / REAL(NT) ) / DTNORM * 100.
          IF ( NTOT(8) .GE. 1 )                                       &
              ETM(1) = SQRT( ESUM(8) / REAL(NTOT(8)) ) / DTNORM * 100.
          IF ( NTOT(9) .GE. 1 )                                       &
              ETM(2) = SQRT( ESUM(9) / REAL(NTOT(9)) ) / DTNORM * 100.
          ETM    = MIN ( 9999.99 , ETM )
!
          ESI(0) = SQRT( ESUM(10) / REAL(NT) ) * 100.
          IF ( NTOT(11) .GE. 1 )                                       &
              ESI(1) = SQRT( ESUM(11) / REAL(NTOT(11)) ) * 100.
          IF ( NTOT(12) .GE. 1 )                                       &
              ESI(2) = SQRT( ESUM(12) / REAL(NTOT(12)) ) * 100.
          ESI    = MIN ( 9999.99 , ESI )
!
          IF ( NTOT(13) .GE. 1 )                                       &
              EAL    = SQRT( ESUM(13) / REAL(NTOT(13)) ) * 100.
          EAL    = MIN ( 9999.99 , EAL )
          IF ( NTOT(14) .GE. 1 )                                       &
              EF0    = SQRT( ESUM(14) / REAL(NTOT(14)) ) * 100.
          EF0    = MIN ( 9999.99 , EF0 )
!
! 3.b f**-4 range
!
          EBE    = -1.
!
          ESUM   = 0.
          NTOT   = 0
!
          DO I=1, NT
!
            ESUM(2:3) = 0.
!
            DO IFR=1, NFR
              IF ( FRQ(IFR) .LT. 1.5*FP(1,I,2) ) CYCLE
              ESUM(2) = ESUM(2) + (SS1(IFR,I,1)-SS1(IFR,I,2))**2      &
                                            * DFR(IFR)
              ESUM(3) = ESUM(3) + SS1(IFR,I,2) * DFR(IFR)
              IF ( FRQ(IFR) .GT. 3.0*FP(1,I,2) ) EXIT
              END DO
!
            IF ( ESUM(3) .GT. 1.E-10 ) THEN
                ESUM(1) = ESUM(1) + SQRT(ESUM(2)) / ESUM(3)
                NTOT(1) = NTOT(1) + 1
              END IF
!
            END DO
!
          IF ( NTOT(1) .GE. 1 )                                       &
              EBE    = SQRT( ESUM(1) / REAL(NTOT(1)) ) * 100.
          EBE    = MIN ( 9999.9 , EBE )
!
! 3.c 1-D spectra, theta and sigma
!
          EF1    = -1.
          EG1    = -1.
          EN1    = -1.
          ET1    = -1.
          ES1    = -1.
!
          ESUM   = 0.
          NTOT   = 0
!
          DO I=1, NT
!
            ESUM(6:) = 0.
!
            DO IFR=1, NFR
!
              ESUM( 6) = ESUM( 6) + (SP1(IFR,I,1)-SP1(IFR,I,2))**2    &
                                              * DFR(IFR)
              ESUM( 7) = ESUM( 7) + (SS1(IFR,I,1)-SS1(IFR,I,2))**2    &
                                              * DFR(IFR)
              ESUM( 8) = ESUM( 8) + (NL1(IFR,I,1)-NL1(IFR,I,2))**2    &
                                              * DFR(IFR)
!
              ESUM( 9) = ESUM( 9) + SP1(IFR,I,2) * DFR(IFR)
              ESUM(10) = ESUM(10) + SS1(IFR,I,2) * DFR(IFR)
              ESUM(11) = ESUM(11) + NL1(IFR,I,2)**2 * DFR(IFR)
!
              IF ( TH1(IFR,I,1).GT.-180.5 .AND. TH1(IFR,I,2).GT.-180.5 ) THEN
                  ESUM(12) = ESUM(12) + DFR(IFR) *                    &
                           (TH1(IFR,I,1)-TH1(IFR,I,2))**2 
                  ESUM(13) = ESUM(13) + DFR(IFR) *                    &
                           (SI1(IFR,I,1)-SI1(IFR,I,2))**2 
                  ESUM(14) = ESUM(14) + DFR(IFR)
                END IF
!
              IF ( FRQ(IFR) .GT. 3.5*FP(1,I,2) ) EXIT
              END DO
!
            IF ( ESUM(9) .GT. 1.E-10 ) THEN
                ESUM(1) = ESUM(1) + SQRT(ESUM(6)) / ESUM(9)
                NTOT(1) = NTOT(1) + 1
              END IF
!
            IF ( ESUM(10) .GT. 1.E-10 ) THEN
                ESUM(2) = ESUM(2) + SQRT(ESUM(7)) / ESUM(10)
                NTOT(2) = NTOT(2) + 1
              END IF
!
            IF ( ESUM(11) .GT. 1.E-15 ) THEN
                ESUM(3) = ESUM(3) + SQRT(ESUM(8)) / SQRT(ESUM(11))
                NTOT(3) = NTOT(3) + 1
              END IF
!
            IF ( ESUM(14) .GT. 0.001 ) THEN
                ESUM(4) = ESUM(4) + SQRT(ESUM(12)) / (ESUM(14)*DTNORM)
                NTOT(4) = NTOT(4) + 1
                IF ( SM(0,I,2) .GT. 0.01 ) THEN
                    ESUM(5) = ESUM(5) + SQRT(ESUM(13)) /              &
                                                  (ESUM(14)*SM(0,I,2))
                    NTOT(5) = NTOT(5) + 1
                  END IF
              END IF
!
            END DO
!
          IF ( NTOT(1) .GE. 1 )                                       &
              EF1    = SQRT( ESUM(1) / REAL(NTOT(1)) ) * 100.
          EF1    = MIN ( 9999.99 , EF1 )
          IF ( NTOT(2) .GE. 1 )                                       &
              EG1    = SQRT( ESUM(2) / REAL(NTOT(2)) ) * 100.
          EG1    = MIN ( 9999.99 , EG1 )
          IF ( NTOT(3) .GE. 1 )                                       &
              EN1    = SQRT( ESUM(3) / REAL(NTOT(3)) ) * 100.
          EN1    = MIN ( 9999.99 , EN1 )
          IF ( NTOT(4) .GE. 1 )                                       &
              ET1    = SQRT( ESUM(4) / REAL(NTOT(4)) ) * 100.
          ET1    = MIN ( 9999.99 , ET1 )
          IF ( NTOT(5) .GE. 1 )                                       &
              ES1    = SQRT( ESUM(5) / REAL(NTOT(5)) ) * 100.
          ES1    = MIN ( 9999.99 , ES1 )
!
! 3.d 2-D spectra
!
          EF2    = -1.
          EG2    = -1.
          EN2    = -1.
!
          ESUM   = 0.
          NTOT   = 0
!
          DFR    = DFR * DTH
!
          DO I=1, NT
!
            ESUM(4:) = 0.
!
            DO IFR=1, NFR
              DO ITH=1, NTH
!
                ESUM(4) = ESUM(4) + DFR(IFR) *                        &
                        (SPC(IFR,ITH,I,1)-SPC(IFR,ITH,I,2))**2
                ESUM(5) = ESUM(5) + DFR(IFR) *                        &
                        (SPS(IFR,ITH,I,1)-SPS(IFR,ITH,I,2))**2
                ESUM(6) = ESUM(6) + DFR(IFR) *                        &
                        (SRC(IFR,ITH,I,1)-SRC(IFR,ITH,I,2))**2
!
                ESUM(7) = ESUM(7) + DFR(IFR) * SPC(IFR,ITH,I,2)
                ESUM(8) = ESUM(8) + DFR(IFR) * SPS(IFR,ITH,I,2)
                ESUM(9) = ESUM(9) + DFR(IFR) * SRC(IFR,ITH,I,2)**2
!
                END DO
              END DO
!
            IF ( ESUM(7) .GT. 1.E-10 ) THEN
                ESUM(1) = ESUM(1) + SQRT(ESUM(4)) / ESUM(7)
                NTOT(1) = NTOT(1) + 1
              END IF
!
            IF ( ESUM(8) .GT. 1.E-10 ) THEN
                ESUM(2) = ESUM(2) + SQRT(ESUM(5)) / ESUM(8)
                NTOT(2) = NTOT(2) + 1
              END IF
!
            IF ( ESUM(9) .GT. 1.E-15 ) THEN
                ESUM(3) = ESUM(3) + SQRT(ESUM(6)) / SQRT(ESUM(9))
                NTOT(3) = NTOT(3) + 1
              END IF
!
            END DO
!
          DFR    = DFR / DTH
!
          IF ( NTOT(1) .GE. 1 )                                       &
              EF2    = SQRT( ESUM(1) / REAL(NTOT(1)) ) * 100.
          EF2    = MIN ( 9999.99 , EF2 )
          IF ( NTOT(2) .GE. 1 )                                       &
              EG2    = SQRT( ESUM(2) / REAL(NTOT(2)) ) * 100.
          EG2    = MIN ( 9999.99 , EG2 )
          IF ( NTOT(3) .GE. 1 )                                       &
              EN2    = SQRT( ESUM(3) / REAL(NTOT(3)) ) * 100.
          EN2    = MIN ( 9999.99 , EN2 )
!
        END IF
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 4. Write output files
! 4.a One set of files, processed data file
!
      WRITE (STRING,'(144X)') 
!
      IF ( TIME(1,1) .LT. 20000101 ) THEN
          T0(1) = 19680606
        ELSE
          T0(1) = 20091006
        END IF
      T0(2) = 0
!
      IF ( NF .EQ. 1 ) THEN
!
! 4.a.1 Open the file
!
          FNAME  = 'all_data.ww3'
          OPEN (50,FILE=FNAME,FORM='FORMATTED',ERR=802,IOSTAT=IERR)
          WRITE (*,940) FNAME
          WRITE (50,1100) NT, NFR, NTH
!
! 4.a.2 One-D parameters
!
          DO I=1, NT
!
            RTIME  = DSEC21(T0,TIME(:,I)) / 3600.
            WRITE (STRING(1:48),1110) I, RTIME, XA(I)*1.E-3,          &
                                YA(I)*1.E-3, DA(I), UA(I), UD(I)-180.
!
            DO J=0, 2
              IF ( HS(J,I,1) .GE. 0. ) THEN
                  WRITE (STRING(50+J*6:54+J*6),'(F5.2)')              &
                         MIN ( 99.99 , HS(J,I,1) )
                ELSE
                  STRING(50+J*6:54+J*6) = ' NaN '
                END IF
              END DO
!
            DO J=0, 2
              IF ( FP(J,I,1) .GE. 0. ) THEN
                  WRITE (STRING(68+J*7:73+J*7),'(F6.4)')              &
                         MIN ( 9.9999 , FP(J,I,1) )
                ELSE
                  STRING(68+J*7:73+J*7) = '  NaN '
                END IF
              END DO
!
            DO J=0, 2
              IF ( TM(J,I,1) .GE. 0. ) THEN
                  WRITE (STRING(89+J*7:94+J*7),'(F6.1)')              &
                         MOD(360.+TM(J,I,1),360.) - 180.
                ELSE
                  STRING(89+J*7:94+J*7) = '  NaN '
                END IF
              END DO
!
            DO J=0, 2
              IF ( SM(J,I,1) .GE. 0. ) THEN
                  WRITE (STRING(110+J*7:115+J*7),'(F6.2)')            &
                         MIN ( 999.99 , SM(J,I,1) )
                ELSE
                  STRING(110+J*7:115+J*7) = '  NaN '
                END IF
              END DO
!
            WRITE (STRING(131:137),'(F7.5)') MIN ( 9.99999 , ALP(I,1) )
!
            IF ( FR0(I,1) .GE. 0. ) THEN
                WRITE (STRING(139:144),'(F6.4)') MIN(9.9999,FR0(I,1))
              ELSE
                STRING(139:144) = '  NaN '
              END IF
!
            WRITE (50,1111) STRING
!
            END DO
!
! 4.a.3 One-D spectra
!
          WRITE (50,1116) FRQ
          WRITE (50,1117) TH*RADE
!
          DO I=1, NT
!
            WRITE (50,1112) TIME(:,I)
            WRITE (50,1113) SP1(:,I,1)
            WRITE (50,1113) SS1(:,I,1)
!
            J      = 1
            WRITE (STRING,'(136X)') 
!
            DO IFR=1, NFR
              IF ( TH1(IFR,I,1) .GT. -180.5 ) THEN
                  WRITE (STRING(J:J+5),'(F6.1)') TH1(IFR,I,1)
                ELSE
                  STRING(J:J+5) = '  NaN '
                END IF
              J      = J + 7
              IF ( J .GT. 130 ) THEN
                  WRITE (50,1111) STRING(1:J-2)
                  J      = 1
                  WRITE (STRING,'(136X)') 
                END IF
              END DO
!
            IF ( J .GT. 1 ) WRITE (50,1111) STRING(1:J-2)
!
            J      = 1
            WRITE (STRING,'(136X)') 
!
            DO IFR=1, NFR
              IF ( SI1(IFR,I,1) .GE. 0. ) THEN
                  WRITE (STRING(J:J+5),'(F6.1)') SI1(IFR,I,1)
                ELSE
                  STRING(J:J+5) = '  NaN '
                END IF
              J      = J + 7
              IF ( J .GT. 130 ) THEN
                  WRITE (50,1111) STRING(1:J-2)
                  J      = 1
                  WRITE (STRING,'(136X)') 
                END IF
              END DO
!
            IF ( J .GT. 1 ) WRITE (50,1111) STRING(1:J-2)
!
            WRITE (50,1113) NL1(:,I,1)
            WRITE (50,1113) MM1(:,I,1)
!
            WRITE (50,1113) WN1(:,I)
!
            END DO
!
! 4.a.4 Two-D spectrum and source term
!
          DO I=1, NT
!
            WRITE (50,1112) TIME(:,I)
!
            DO ITH=1, NTH
              WRITE (50,1114) SPS(:,ITH,I,1)
              END DO
!
            DO ITH=1, NTH
              WRITE (50,1115) SRC(:,ITH,I,1)
              END DO
!
            END DO
!
          CLOSE (50)
!
! 4.b Two sets of files, error file
!
        ELSE
!
! 4.b.1 Open the file
!
          FNAME  = 'errors.ww3'
          OPEN (50,FILE=FNAME,FORM='FORMATTED',ERR=802,IOSTAT=IERR)
          WRITE (*,940) FNAME
!
! 4.a.2 Write the parameters
!
          WRITE (50,1200) EHS, EFP, ETM, ESI, EAL, EF0, EBE, EF1,     &
                          EG1, EN1, ET1, ES1, EF2, EG2, EN2
!
          CLOSE (50)
!
        END IF
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Normal end of program
!
      GOTO 888
!
! Error scape locations
!
  801 CONTINUE
      WRITE (*,1001) FNAME
      STOP
!
  802 CONTINUE
      WRITE (*,1002) FNAME, IERR
      STOP
!
  803 CONTINUE
      WRITE (*,1003) FNAME, IERR
      STOP
!
  804 CONTINUE
      WRITE (*,1004) NFR, NTH, NRQ, NFT, NTT, NRT
      STOP
!
  805 CONTINUE
      WRITE (*,1005) NFR, NTH, NRQ, GNAME, NRT, NTT, NRT, TNAME
      STOP
!
  806 CONTINUE
      WRITE (*,1006) TIME(:,I), TTST
      STOP
!
  807 CONTINUE
      WRITE (*,1007) XA(I), YA(I), DA(I), UA(I), UD(I),               &
                     TX, TY, TD, TUA, TUD
      WRITE (*,*) ABS(TX-XA(I)), ABS(TY-YA(I)), ABS(TD-DA(I)), &
                  ABS(TUA-UA(I)), ABS(TUD-UD(I))
      STOP
!
  888 CONTINUE
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Finalize program
!
      WRITE (*,999)
!
! Formats
!
  900 FORMAT (/' process.x: processing raw files and errors :'/       &
               ' --------------------------------------------')
  901 FORMAT ( ' Test existence of data files :')
  902 FORMAT ( '    File found : ',A)
  903 FORMAT ( '    Second set of files not found')
!
  910 FORMAT (/' Read files:')
  911 FORMAT ( '    NFR     = ',I6/                                   &
               '    NTH     = ',I6/                                   &
               '    NRQ     = ',I6/                                   &
               '    Grid    = ',A)
  912 FORMAT ( '    FR1     = ',F8.4/                                 &
               '    XFR     = ',F7.3)
  913 FORMAT ( '                TEST: TIME ',I8.8,I7.6)
  914 FORMAT ( '                TEST: LOC  [',A10,']',3F8.2)
  915 FORMAT ( '    NT      = ',I6/)
  916 FORMAT ( '    File    : ',A)
!
  920 FORMAT (/' Process raw data ... ')
!
  930 FORMAT (/' Process errors ... ')
!
  940 FORMAT (/' Process output file ',A,'  ... ')
!
  999 FORMAT (/' End of process.x'/)
!
 1001 FORMAT (/'    *** ERROR : FILE NOT FOUND : ',A,' ***'/)
 1002 FORMAT (/'    *** ERROR IN OPENENING FILE ***'/                 &
               '        FILE : ',A,'    IOSTAT = ',I10)
 1003 FORMAT (/'    *** ERROR IN READING FROM FILE ***'/              &
               '        FILE : ',A,'    IOSTAT = ',I10)
 1004 FORMAT (/'    *** ERROR : UNEXPECTED FILE CONTENT ***'/         &
               '        EXPECETED : ',3I6/                            &
               '        IN FILE   : ',3I6/)
 1005 FORMAT (/'    *** ERROR : UNEXPECTED FILE CONTENT ***'/         &
               '        EXPECETED : ',3I6,2X,A/                       &
               '        IN FILE   : ',3I6,2X,A/)
 1006 FORMAT (/'    *** ERROR : UNEXPECTED TIME CONTENT ***'/         &
               '        EXPECETED : ',I8.8,I7.6/                      &
               '        IN FILE   : ',I8.8,I7.6/)
 1007 FORMAT (/'    *** ERROR : UNEXPECTED ENV. CONTENT ***'/         &
               '        EXPECETED : ',5E10.3/                         &
               '        IN FILE   : ',5E10.3/)
!
 1100 FORMAT (1X,3I6)
!
 1110 FORMAT (I3,F7.2,2F8.2,F8.2,F7.2,F7.1)
 1111 FORMAT (A)
 1112 FORMAT (1X,I8.8,1X,I6.6)
 1113 FORMAT (11E12.4)
 1114 FORMAT (13E10.3)
 1115 FORMAT (12E11.3)
 1116 FORMAT (17F8.5)
 1117 FORMAT (17F8.1)
!
 1200 FORMAT ( 'Hs      : ',3F9.3/                                    &
               'fp      : ',3F9.3/                                    &
               'theta_m : ',3F9.3/                                    &
               'sig_th  : ',3F9.3/                                    &
               'alpha   : ',F9.3/                                     &
               'f0      : ',F9.3/                                     &
               'beta    : ',F9.3/                                     &
               'F(f)    : ',F9.3/                                     &
               'G(f)    : ',F9.3/                                     &
               'Snl(f)  : ',F9.3/                                     &
               'th(f)   : ',F9.3/                                     &
               'si(f)   : ',F9.3/                                     &
               'F(f,t)  : ',F9.3/                                     &
               'G(f,t)  : ',F9.3/                                     &
               'S(f,t)  : ',F9.3)
!/
!/ End of PROCESS ---------------------------------------------------- /
!/
      END PROGRAM PROCESS
