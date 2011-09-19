!/ ------------------------------------------------------------------- /
      PROGRAM DESCENT2
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         10-Sep-2010 |
!/                  +-----------------------------------+
!/
!/    30-Dec-2008 : Origination.
!/    14-Jan-2009 : Increase accuracy of error.
!/    08-Oct-2009 : Fix error in namlist writing in 3.c (init. JQ).
!/    09-Oct-2009 : Reduce search line to 12 points.
!/    24-Dec-2009 : Renormalize for switched-off scalings.
!/    13-Aug-2010 : Move from !/NLX to !/NL3.
!/    10-Sep-2010 : Expand format for m < -9.99.
!/
!/    Copyright 2008-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Set up computations for running search line.
!
!  2. Method :
!
!  3. Parameters :
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      QTEST     L.F.  QTOOLSMD Test validity of quadruplet layout.
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
      USE QTOOLSMD
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: IERR, NQ, NDSI, NDSE, NDSO, NDSS,    &
                                 LGEN, I, II, J, JJ, IQ, JQ, MQ,      &
                                 JQD, JQS
      REAL                    :: FACT, ERRC, ERRN, ERRP, DELTA,       &
                                 ENORM, PFAC, L, M, DT, FACTR
      REAL, ALLOCATABLE       :: MEMBER(:), PERT(:), ADELT(:), EVEC(:)
      LOGICAL, ALLOCATABLE    :: FLAG(:)
      CHARACTER(LEN=8)        :: FNAME
!/
!/ ------------------------------------------------------------------- /
!
! 0.  Initialization
!
      READ (*,*,ERR=801,IOSTAT=IERR) NQ, FACT
      WRITE (*,900) NQ, FACT
      NDSI    = 10
      NDSE    = 11
      NDSO    = 50
      NDSS    = 51
      LGEN   = 5*NQ + 2
      ALLOCATE ( MEMBER(0:LGEN) , PERT(0:LGEN) , ADELT(LGEN),         &
                 EVEC(LGEN), FLAG(LGEN) )
      READ (*,*,ERR=801,IOSTAT=IERR) FLAG
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Set up input file
!
      WRITE (*,910)
!
      OPEN (NDSI,FILE='mini_pop',STATUS='OLD',ERR=802,IOSTAT=IERR)
      READ (NDSI,*,END=803,ERR=803,IOSTAT=IERR) MEMBER
      ERRC   = MEMBER(0)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Set up output file
!
      WRITE (*,920)
!
      OPEN (NDSO,FILE='line_pop')
!
      IF ( NQ .EQ. 1 ) THEN
          WRITE (NDSO,922) MEMBER(0), MEMBER(1:7)
        ELSE
          WRITE (NDSO,923) MEMBER(0), MEMBER(1:5)
          DO I=2, NQ-1
            WRITE (NDSO,924) MEMBER((I-1)*5+1:I*5)
            END DO
          WRITE (NDSO,925) MEMBER((NQ-1)*5+1:NQ*5+2)
        END IF
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  Set up all perturbations
! 3.a Process existing file
!
      WRITE (*,930)
      ADELT  = 0.
      EVEC   = 0.
      II     = 1
!
      DO I=1, LGEN
!
        IF ( FLAG(I) ) THEN
!
            J     = 1 + MOD(I-1,5)
            IQ    = 1 + (I-1)/5
!
            SELECT CASE (J)
              CASE (1)
                DELTA = FACT * 0.1
              CASE (2)
                DELTA = FACT * 0.1
              CASE (3)
                DELTA = FACT * 45.
              CASE DEFAULT
                DELTA = FACT * MEMBER(I)
            END SELECT
!
            IF ( IQ .GT. NQ ) THEN
                DELTA = FACT * 8.
              END IF
!
            READ (NDSI,*,END=803,ERR=803,IOSTAT=IERR) PERT
            II     = II + 1
            ERRN   = PERT(0)
            IF ( ERRN .EQ. 999.999 ) THEN
                WRITE (FNAME,'(A4,I4.4)') 'err.', II
                OPEN (NDSE,FILE=FNAME,STATUS='OLD',ERR=802,IOSTAT=IERR)
                READ (NDSE,*,ERR=805,END=805) PERT(0)
                ERRN   = PERT(0)
                CLOSE (NDSE)
              END IF
!
            IF ( NQ .EQ. 1 ) THEN
                WRITE (NDSO,922) PERT(0), PERT(1:7)
              ELSE
                WRITE (NDSO,923) PERT(0), PERT(1:5)
                DO JJ=2, NQ-1
                  WRITE (NDSO,924) PERT((JJ-1)*5+1:JJ*5)
                  END DO
                WRITE (NDSO,925) PERT((NQ-1)*5+1:NQ*5+2)
              END IF
!
            READ (NDSI,*,END=803,ERR=803,IOSTAT=IERR) PERT
            II     = II + 1
            ERRP   = PERT(0)
            IF ( ERRP .EQ. 999.999 ) THEN
                WRITE (FNAME,'(A4,I4.4)') 'err.', II
                OPEN (NDSE,FILE=FNAME,STATUS='OLD',ERR=804,IOSTAT=IERR)
                READ (NDSE,*,ERR=805,END=805) PERT(0)
                ERRP   = PERT(0)
                CLOSE (NDSE)
              END IF
!
            IF ( NQ .EQ. 1 ) THEN
                WRITE (NDSO,922) PERT(0), PERT(1:7)
              ELSE
                WRITE (NDSO,923) PERT(0), PERT(1:5)
                DO JJ=2, NQ-1
                  WRITE (NDSO,924) PERT((JJ-1)*5+1:JJ*5)
                  END DO
                WRITE (NDSO,925) PERT((NQ-1)*5+1:NQ*5+2)
              END IF
!
            IF ( MIN(ERRN,ERRP) .LT. ERRC ) THEN
                IF ( ERRN .LT. ERRP ) THEN
                    DELTA  = -ABS(DELTA)
                  ELSE
                    DELTA  =  ABS(DELTA)
                  END IF
                ADELT(I) = DELTA
                EVEC (I) = ERRC - MIN(ERRN,ERRP)
              END IF
!
          END IF
!
        END DO
!
! 3.b Get norm of error increment vector
!
      ENORM  = 0.
      DO I=1, LGEN
        ENORM  = ENORM + EVEC(I)**2
        END DO
      ENORM  = SQRT(MAX(0.,ENORM))
!
      IF ( ENORM .GT. 0. ) THEN
!
          EVEC   = EVEC / ENORM
          ADELT  = ADELT * EVEC
!
! 3.c Make perturbed line
!
          JQD    = 0
          JQS    = 0
!
          DO IQ=1, NQ
            IF ( MEMBER((IQ-1)*5+4).GT.0. ) JQD = JQD + 1
            IF ( MEMBER((IQ-1)*5+5).GT.0. ) JQS = JQS + 1
            END DO
!
          IF ( JQD.NE.NQ .AND. JQD.GE.1 ) THEN
              FACTR  = REAL(NQ) / REAL(JQD)
              DO IQ=1, NQ
                MEMBER((IQ-1)*5+4) = FACTR * MEMBER((IQ-1)*5+4)
                END DO
            END IF
!
          IF ( JQS.NE.NQ .AND. JQS.GE.1 ) THEN
              FACTR  = REAL(NQ) / REAL(JQS)
              DO IQ=1, NQ
                MEMBER((IQ-1)*5+5) = FACTR * MEMBER((IQ-1)*5+5)
                END DO
            END IF
!
          PFAC   = 0.25
!         DO J=1, 16
          DO J=1, 12
            DO JJ=1, LGEN
              PERT(JJ) = MEMBER(JJ) + ADELT(JJ)*PFAC*REAL(J)
              END DO
            II    = II + 1
!
            MQ    = 0
            JQ    = 0
            DO IQ=1, NQ
              JJ     = (IQ-1)*5 + 1
              IF ( MEMBER(JJ+3).EQ.0. .AND.                           &
                   MEMBER(JJ+3).NE.PERT(JJ+3) )                       &
                   PERT(JJ+3) = PERT(JJ+3) + 1.E5
              IF ( MEMBER(JJ+4).EQ.0. .AND.                           &
                   MEMBER(JJ+4).NE.PERT(JJ+4) )                       &
                   PERT(JJ+4) = PERT(JJ+4) + 1.E4
              IF ( PERT(JJ+3) .LT. 1.E5 ) PERT(JJ+3) = 0.
              IF ( PERT(JJ+4) .LT. 1.E4 ) PERT(JJ+4) = 0.
              IF ( MAX(PERT(JJ),PERT(JJ+1)).GT.0.                     &
                 .AND.  QTEST(PERT(JJ),PERT(JJ+1),PERT(JJ+2),L,M,DT)  &
                 .AND.  MAX(PERT(JJ+3),PERT(JJ+4)).GT.1. ) MQ = MQ + 1
              END DO
!
            JQD    = 0
            JQS    = 0
!
            DO IQ=1, NQ
              IF ( PERT((IQ-1)*5+4).GT.0. ) JQD = JQD + 1
              IF ( PERT((IQ-1)*5+5).GT.0. ) JQS = JQS + 1
              END DO
!
            IF ( JQD.NE.NQ .AND. JQD.GE.1 ) THEN
                FACTR  = REAL(JQD) / REAL(NQ)
                DO IQ=1, NQ
                  PERT((IQ-1)*5+4) = FACTR * PERT((IQ-1)*5+4)
                  END DO
              END IF
!
            IF ( JQS.NE.NQ .AND. JQS.GE.1 ) THEN
                FACTR  = REAL(JQS) / REAL(NQ)
                DO IQ=1, NQ
                  PERT((IQ-1)*5+5) = FACTR * PERT((IQ-1)*5+5)
                  END DO
              END IF
!
            IF ( MQ .GE. 1 ) THEN
                PERT(0) = 999.999
                WRITE (FNAME,'(A4,I4.4)') 'snl.', II
                OPEN (NDSS,FILE=FNAME,ERR=804,IOSTAT=IERR)
                WRITE (NDSS,940) MQ, PERT(LGEN-1), PERT(LGEN)
                DO IQ=1, NQ
                  JJ     = (IQ-1)*5+1
                  IF ( MAX(PERT(JJ),PERT(JJ+1)).GT.0.                 &
                 .AND.  QTEST(PERT(JJ),PERT(JJ+1),PERT(JJ+2),L,M,DT)  &
                 .AND.  MAX(PERT(JJ+3),PERT(JJ+4)).GT.1. ) THEN
                      JQ     = JQ + 1
                      JJ     = (IQ-1)*5+4
                      IF ( MQ .EQ. 1 ) THEN
                          WRITE (NDSS,941) L, M, DT, PERT(JJ:JJ+1)
                        ELSE
                          IF ( JQ .EQ. 1 ) THEN
                              WRITE (NDSS,942) L, M, DT, PERT(JJ:JJ+1)
                            ELSE IF ( JQ .EQ. MQ ) THEN
                              WRITE (NDSS,944) L, M, DT, PERT(JJ:JJ+1)
                            ELSE
                              WRITE (NDSS,943) L, M, DT, PERT(JJ:JJ+1)
                            END IF
                        END IF
                    END IF
                  END DO
                CLOSE (NDSS)
              ELSE
                PERT(0) = 999.950
              END IF
!
            IF ( NQ .EQ. 1 ) THEN
                WRITE (NDSO,922) PERT(0), PERT(1:7)
              ELSE
                WRITE (NDSO,923) PERT(0), PERT(1:5)
                DO JJ=2, NQ-1
                  WRITE (NDSO,924) PERT((JJ-1)*5+1:JJ*5)
                  END DO
                WRITE (NDSO,925) PERT((NQ-1)*5+1:NQ*5+2)
              END IF
!

            END DO
!
        END IF
!
      CLOSE (NDSO)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Normal end of program
!
      GOTO 888
!
! Error scape locations
!
  801 CONTINUE
      WRITE (*,1001) IERR
      STOP 1001
!
  802 CONTINUE
      WRITE (*,1002) IERR
      STOP 1002
!
  803 CONTINUE
      WRITE (*,1003) IERR
      STOP 1003
!
  804 CONTINUE
      WRITE (*,1004) FNAME, IERR
      STOP 1004
!
  805 CONTINUE
      WRITE (*,1005) FNAME, IERR
      STOP 1005
!
  888 CONTINUE
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Finalize program
!
      WRITE (*,999)
!
! Formats
!
  900 FORMAT (/' descent2.x: generate partial derivatives    '/       &
               ' --------------------------------------------'/       &
               '    Number of quadruplets       : ',I4/               &
               '    Increment (factor)          : ',F8.3)
  910 FORMAT (/'    Reading base state ...')
  920 FORMAT ( '    Starting output files ...')
  922 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3,F7.2,F6.2)
  923 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3)
  924 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3)
  925 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3,F7.2,F6.2)
  930 FORMAT ( '    Generate perturbations ...')
  940 FORMAT (' &SNL3 NQDEF =',I3,', MSC =',F6.2,', NSC =',F6.2,' /')
  941 FORMAT (' &ANL3 QPARMS =',F6.3,',',F6.3,',',F6.1,2(',',E10.3),  &
              ' /')
  942 FORMAT (' &ANL3 QPARMS =',F6.3,',',F6.3,',',F6.1,2(',',E10.3),',')
  943 FORMAT (15X,F6.3,',',F6.3,',',F6.1,2(',',E10.3),',')
  944 FORMAT (15X,F6.3,',',F6.3,',',F6.1,2(',',E10.3),' /')
!
  999 FORMAT (/' End of descent2.x')
!
 1001 FORMAT (/' *** ERROR IN READING FORM STDIN ***'/                &
               '     IOSTAT = ',I8/)
 1002 FORMAT (/' *** ERROR IN OPENING INPUT FILE ***'/                &
               '     IOSTAT = ',I8/)
 1003 FORMAT (/' *** ERROR IN READING INPUT FILE ***'/                &
               '     IOSTAT = ',I8/)
 1004 FORMAT (/' *** ERROR IN OPENING FILE ',A,' ***'/                &
               '     IOSTAT = ',I8/)
 1005 FORMAT (/' *** ERROR IN READING FILE ',A,' ***'/                &
               '     IOSTAT = ',I8/)
!/
!/ End of DESCENT2 --------------------------------------------------- /
!/
      END PROGRAM DESCENT2
