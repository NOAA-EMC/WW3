!/ ------------------------------------------------------------------- /
      PROGRAM CHCKGEN
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         13-Aug-2010 |
!/                  +-----------------------------------+
!/
!/    21-Dec-2008 : Origination.
!/    14-Jan-2009 : Increase accuracy of error.
!/    23-Nov-2009 : Add FLAGUQ testing.
!/    23-Jan-2010 : Add FLAGSC to FLAGUQ testing.
!/    13-Aug-2010 : Move from !/NLX to !/NL3.
!/
!/    Copyright 2008-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Check status of generation (computation of errors).
!
!  2. Method :
!
!     Read file, if error not yet computed check for file with error
!     otherwise make file with Snl setting.
!
!  3. Parameters :
!
!  4. Subroutines used :
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
      INTEGER                 :: NQ, NDSI, NDSO, NDST, IERR, LGEN,    &
                                 NREAD, NADD, NPROC, I, IQ, JQ, JQT, MQ
      REAL                    :: ERROR, L, M, DT
      REAL, ALLOCATABLE       :: DATA(:)
      LOGICAL                 :: FLAGOK, FLAGP(7), FLAGSC
      LOGICAL, ALLOCATABLE    :: FLAGUQ(:)
      CHARACTER(LEN=8)        :: FNAME
!/
!/ ------------------------------------------------------------------- /
!
! 0.  Initialization
!
      READ (*,*) NQ
      WRITE (*,900) NQ
      NDSI    = 10
      NDST    = 11
      NDSO    = 50
!
      ALLOCATE ( FLAGUQ(NQ) )
      DO IQ=1, NQ
        IF ( IQ .LT. NQ ) THEN
            READ (*,*) FLAGP(1:5)
          ELSE
            READ (*,*) FLAGP(1:7)
          END IF
        FLAGUQ(IQ) = .FALSE.
        DO I=1, 5
          FLAGUQ(IQ) = FLAGUQ(IQ) .OR. FLAGP(I)
          END DO
        END DO
      FLAGSC = FLAGP(6) .OR. FLAGP(7)
!
      WRITE (*,901)
      OPEN (NDSI,FILE='population',STATUS='OLD',ERR=801,IOSTAT=IERR)
      WRITE (*,902)
      OPEN (NDSO,FILE='population.updt',ERR=801,IOSTAT=IERR)
!
      LGEN   = 5*NQ + 2
      ALLOCATE ( DATA(LGEN) )
!
      NREAD  = 0
      NADD   = 0
      NPROC  = 0
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Loop to read data
!
      DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Read data and check error
!
        READ (NDSI,*,END=600,ERR=802,IOSTAT=IERR) ERROR, DATA
        NREAD  = NREAD + 1
        IF ( ERROR .EQ. 999.999 ) THEN
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  Check if error file is there and process
!
            WRITE (FNAME,'(A4,I4.4)') 'err.', NREAD
            INQUIRE (FILE=FNAME,EXIST=FLAGOK)
            IF ( FLAGOK ) THEN
                OPEN (NDST,FILE=FNAME,ERR=830,IOSTAT=IERR)
                READ (NDST,*,ERR=831,END=831,IOSTAT=IERR) ERROR
                CLOSE (NDST)
                NADD   = NADD + 1
              ELSE
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 4.  Make snl file as needed
!
                JQ     = 0
                JQT    = 0
                DO IQ=1, NQ
                  I      = (IQ-1)*5+1
                  IF ( QTEST(DATA(I),DATA(I+1),DATA(I+2),L,M,DT) .AND. &
                       ( DATA(I+3).GT.0. .OR. DATA(I+4).GT.0. ) .AND.  &
                        MAX(DATA(I),DATA(I+1)) .GT. 0. ) THEN
                      JQ = JQ + 1
                      IF ( FLAGUQ(IQ) ) JQT = JQT + 1
                    END IF
                  END DO
                MQ     = JQ
!
                IF ( JQT.EQ.0 .AND. .NOT.FLAGSC ) THEN
                    ERROR  = 999.950
                    WRITE (FNAME,'(A4,I4.4)') 'err.', NREAD
                    OPEN (NDST,FILE=FNAME,ERR=830,IOSTAT=IERR)
                    WRITE (NDST,945) ERROR, ERROR
                    CLOSE (NDST)
                    NADD   = NADD + 1
                  ELSE
                    WRITE (FNAME,'(A4,I4.4)') 'snl.', NREAD
                    OPEN (NDST,FILE=FNAME,ERR=830,IOSTAT=IERR)
                    WRITE (NDST,940) JQ, DATA(LGEN-1), DATA(LGEN)
                    JQ     = 0
                    DO IQ=1, NQ
                      I      = (IQ-1)*5+1
!                     IF ( QTEST(DATA(I),DATA(I+1),DATA(I+2),L,M,DT)  &
!                          .AND. ( DATA(I+3).GT.0. .OR.               &
!                                  DATA(I+4).GT.0. ) ) THEN
                      IF ( QTEST(DATA(I),DATA(I+1),DATA(I+2),L,M,DT) .AND. &
                           ( DATA(I+3).GT.0. .OR. DATA(I+4).GT.0. ) .AND.  &
                            MAX(DATA(I),DATA(I+1)) .GT. 0. ) THEN
                          JQ     = JQ + 1
                          I      = (IQ-1)*5+4
                          IF ( MQ .EQ. 1 ) THEN
                              WRITE (NDST,941) L, M, DT, DATA(I:I+1)
                            ELSE
                              IF ( JQ .EQ. 1 ) THEN
                                  WRITE (NDST,942) L, M, DT, DATA(I:I+1)
                                ELSE IF ( JQ .EQ. MQ ) THEN
                                  WRITE (NDST,944) L, M, DT, DATA(I:I+1)
                                ELSE
                                  WRITE (NDST,943) L, M, DT, DATA(I:I+1)
                                END IF
                            END IF
                        END IF
                      END DO
                    CLOSE (NDST)
                    NPROC = NPROC + 1
                  END IF
!
              END IF
          END IF
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 5.  Write to output file
!
        IF ( NQ .EQ. 1 ) THEN 
            WRITE (NDSO,950) ERROR, DATA(1:7)
          ELSE
            WRITE (NDSO,951) ERROR, DATA(1:5)
            DO I=2, NQ-1
              WRITE (NDSO,952) DATA((I-1)*5+1:I*5)
              END DO
            WRITE (NDSO,953) DATA((NQ-1)*5+1:NQ*5+2)
          END IF
!
! ... End loop 1.
!
        END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 6.  All work done
!
  600 CONTINUE
      WRITE (*,960) NREAD, NADD, NPROC
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
  830 CONTINUE
      WRITE (*,1030) FNAME, IERR
      STOP 1030
!
  831 CONTINUE
      WRITE (*,1031) FNAME, IERR
      STOP 1031
!
  888 CONTINUE
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Finalize program
!
      WRITE (*,999)
!
! Formats
!
  900 FORMAT (/' chckgen.x: check errors in generation :     '/       &
               ' --------------------------------------------'/       &
               '    Number of quadruplets  : ',I3)
  901 FORMAT ( '    Opening input file ...')
  902 FORMAT ( '    Opening output file ...')
  940 FORMAT (' &SNL3 NQDEF =',I3,', MSC =',F6.2,', NSC =',F6.2,' /')
  941 FORMAT (' &ANL3 QPARMS =',F6.3,',',F6.3,',',F6.1,2(',',E10.3),  &
              ' /')
  942 FORMAT (' &ANL3 QPARMS =',F6.3,',',F6.3,',',F6.1,2(',',E10.3),',')
  943 FORMAT (15X,F6.3,',',F6.3,',',F6.1,2(',',E10.3),',')
  944 FORMAT (15X,F6.3,',',F6.3,',',F6.1,2(',',E10.3),' /')
  945 FORMAT (F8.3,' -1., -1., -1., -1., -1., -1., -1., -1., -1.'/    &
              F8.3,' -1., -1., -1., -1., -1., -1., -1., -1., -1.,',   &
              ' -1., -1., -1., -1., -1., -1., -1., -1., -1., -1.')
  950 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3,F6.2,F6.2)
  951 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3)
  952 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3)
  953 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3,F6.2,F6.2)
  960 FORMAT ( '    Records read   : ',I6/                            &
               '    Errors added   : ',I6/                            &
               '    Missing errors : ',I6)
  999 FORMAT (/' End of chckgen.x')
!
 1001 FORMAT (/'   *** ERROR IN OPENING FILE ***'/                    &
               '       IOSTAT = ',I10/)
 1002 FORMAT (/'   *** ERROR IN READING INPUT FILE ***'/              &
               '       IOSTAT = ',I10/)
 1030 FORMAT (/'   *** ERROR IN OPENING FILE ',A,' ***'/              &
               '       IOSTAT = ',I10/)
 1031 FORMAT (/'   *** ERROR IN READING FILE ',A,' ***'/              &
               '       IOSTAT = ',I10/)
!/
!/ End of CHCKGEN ---------------------------------------------------- /
!/
      END PROGRAM CHCKGEN
