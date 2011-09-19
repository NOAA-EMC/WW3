!/ ------------------------------------------------------------------- /
      PROGRAM DESCENT1
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         10-Sep-2010 |
!/                  +-----------------------------------+
!/
!/    29-Dec-2008 : Origination.
!/    14-Jan-2009 : Increase accuracy of error.
!/    20-Nov-2009 : Bug fix for m and n perturbation.
!/    21-Dec-2009 : Account for switched-off quadruplets, and
!/                  compare to orriginal member,
!/    24-Dec-2009 : Rescale deep and shallow separately.
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
!     Set up case for computation of partial derivatives.
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
!      QDIFFA    L.F.  QTOOLSMD Compare population members.
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
      INTEGER                 :: IERR, NQ, NDSI, NDSO, LGEN, I, J,    &
                                 IQ, II, JJ, JQD, JQS, JQ
      REAL                    :: FACT, L, M, DT, DELTA, FACTR
      REAL, ALLOCATABLE       :: MEMBER(:), PERT(:)
      LOGICAL, ALLOCATABLE    :: FLAG(:)
!/
!/ ------------------------------------------------------------------- /
!
! 0.  Initialization
!
      READ (*,*,ERR=801,IOSTAT=IERR) NQ, FACT
      WRITE (*,900) NQ, FACT
      NDSI    = 10
      NDSO    = 50
      LGEN   = 5*NQ + 2
      ALLOCATE ( MEMBER(0:LGEN) , PERT(0:LGEN) , FLAG(LGEN) )
      READ (*,*,ERR=801,IOSTAT=IERR) FLAG
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Read data from input file
!
      WRITE (*,910)
!
      OPEN (NDSI,FILE='descent',STATUS='OLD',ERR=802,IOSTAT=IERR)
!
      DO
        READ (NDSI,*,END=111,ERR=803,IOSTAT=IERR) PERT
        MEMBER = PERT
        END DO
  111 CONTINUE
!
      CLOSE (NDSI) 
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Set up output file
!
      WRITE (*,920)
!
      OPEN (NDSO,FILE='mini_pop')
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
!
      WRITE (*,930)
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
      DO I=1, LGEN
!
        IF ( FLAG(I) ) THEN
!
            JJ    = 1 + MOD(I-1,5)
            IQ    = 1 + (I-1)/5
!
            SELECT CASE (JJ)
              CASE (1)
                DELTA = FACT * 0.1
              CASE (2)
                DELTA = FACT * 0.1
              CASE (3)
                DELTA = FACT * 45.
              CASE (4)
                DELTA = FACT * MAX ( 1.E5 , MEMBER(I) )
              CASE (5)
                DELTA = FACT * MAX ( 1.E4 , MEMBER(I) )
              CASE DEFAULT
                DELTA = FACT * MEMBER(I)
            END SELECT
!
            IF ( IQ .GT. NQ ) THEN
                DELTA = FACT * 8.
              END IF
!
            DO J=1, 2
!
              PERT  = MEMBER
              DELTA = -DELTA
              PERT(I) = PERT(I) + DELTA
              IF ( JJ.EQ.4 .OR. JJ.EQ.5 ) PERT(I) = MAX ( 0. , PERT(I) )
              IF ( JJ.EQ.4 .AND. PERT(I).GT.0. )                      &
                                 PERT(I) = MAX ( 1.E5 , PERT(I) )
              IF ( JJ.EQ.5 .AND. PERT(I).GT.0. )                      &
                                 PERT(I) = MAX ( 1.E4 , PERT(I) )
!
              II     = (IQ-1)*5 + 1
              IF ( I .GE. LGEN-1 ) THEN
!
                  PERT(0) = 999.999
!
                ELSE
!
                  IF ( QTEST(PERT(II),PERT(II+1),PERT(II+2),L,M,DT)   &
                       .AND. MAX(PERT(II),PERT(II+1)) .GT. 0. ) THEN
                      PERT(0) = MEMBER(0)
                      IF ( QDIFFA(NQ,LGEN,PERT,MEMBER) ) THEN
                          PERT(0) = 999.999
                        END IF
                    ELSE
                      PERT(0) = 2.*MEMBER(0)
                    END IF
!
                END IF
!
              JQD    = 0
              JQS    = 0
!
              DO JQ=1, NQ
                IF ( PERT((JQ-1)*5+4).GT.0. ) JQD = JQD + 1
                IF ( PERT((JQ-1)*5+5).GT.0. ) JQS = JQS + 1
                END DO
!
              IF ( JQD.NE.NQ .AND. JQD.GE.1 ) THEN
                  FACTR  = REAL(JQD) / REAL(NQ)
                  DO JQ=1, NQ
                    PERT((JQ-1)*5+4) = FACTR * PERT((JQ-1)*5+4)
                    END DO
                END IF
!
              IF ( JQS.NE.NQ .AND. JQS.GE.1 ) THEN
                  FACTR  = REAL(JQS) / REAL(NQ)
                  DO JQ=1, NQ
                    PERT((JQ-1)*5+5) = FACTR * PERT((JQ-1)*5+5)
                    END DO
                END IF
!
              IF ( NQ .EQ. 1 ) THEN
                  WRITE (NDSO,922) PERT(0), PERT(1:7)
                ELSE
                  WRITE (NDSO,923) PERT(0), PERT(1:5)
                  DO II=2, NQ-1
                    WRITE (NDSO,924) PERT((II-1)*5+1:II*5)
                    END DO
                  WRITE (NDSO,925) PERT((NQ-1)*5+1:NQ*5+2)
                END IF
!
              END DO
!
          END IF
!
        END DO
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
  888 CONTINUE
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Finalize program
!
      WRITE (*,999)
!
! Formats
!
  900 FORMAT (/' descent1.x: generate partial derivatives    '/       &
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
!
  999 FORMAT (/' End of descent1.x')
!
 1001 FORMAT (/' *** ERROR IN READING FORM STDIN ***'/                &
               '     IOSTAT = ',I8/)
 1002 FORMAT (/' *** ERROR IN OPENING INPUT FILE ***'/                &
               '     IOSTAT = ',I8/)
 1003 FORMAT (/' *** ERROR IN READING INPUT FILE ***'/                &
               '     IOSTAT = ',I8/)
!/
!/ End of DESCENT1 --------------------------------------------------- /
!/
      END PROGRAM DESCENT1
