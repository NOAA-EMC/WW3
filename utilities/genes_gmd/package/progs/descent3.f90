!/ ------------------------------------------------------------------- /
      PROGRAM DESCENT3
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
      INTEGER                 :: IERR, NQ, NDSI, NDSE, NDSO, LGEN, I, II
      REAL, ALLOCATABLE       :: BEST(:), NEXT(:)
      CHARACTER(LEN=8)        :: FNAME
!/
!/ ------------------------------------------------------------------- /
!
! 0.  Initialization
!
      READ (*,*,ERR=801,IOSTAT=IERR) NQ
      WRITE (*,900) NQ
      NDSI    = 10
      NDSE    = 11
      NDSO    = 50
      LGEN   = 5*NQ + 2
      ALLOCATE ( BEST(0:LGEN) , NEXT(0:LGEN) )
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Set up input and test output file
!
      WRITE (*,910)
!
      OPEN (NDSI,FILE='line_pop',STATUS='OLD',ERR=802,IOSTAT=IERR)
      OPEN (NDSO,FILE='final_pop',ERR=802,IOSTAT=IERR)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Process input files
!
      WRITE (*,920)
      II     = 0
!
      DO
!
        READ (NDSI,*,END=222,ERR=803) NEXT
        II     = II + 1
write (*,*) ii, next
!
        IF ( II .EQ. 1 ) BEST = NEXT
!
        IF ( NEXT(0) .EQ. 999.999 ) THEN
            WRITE (FNAME,'(A4,I4.4)') 'err.', II
            OPEN (NDSE,FILE=FNAME,STATUS='OLD',ERR=804,IOSTAT=IERR)
            READ (NDSE,*,ERR=805,END=805) NEXT(0)
            CLOSE (NDSE)
          END IF
!
        IF ( NQ .EQ. 1 ) THEN
            WRITE (NDSO,922) NEXT(0), NEXT(1:7)
          ELSE
            WRITE (NDSO,923) NEXT(0), NEXT(1:5)
            DO I=2, NQ-1
              WRITE (NDSO,924) NEXT((I-1)*5+1:I*5)
              END DO
            WRITE (NDSO,925) NEXT((NQ-1)*5+1:NQ*5+2)
          END IF
!
        IF ( NEXT(0) .LT. BEST(0) ) BEST = NEXT
!
        END DO
!
  222 CONTINUE
!
      CLOSE (NDSO)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  Output best member
!
      OPEN (NDSO,FILE='best_member',ERR=802,IOSTAT=IERR)
      IF ( NQ .EQ. 1 ) THEN
          WRITE (NDSO,922) BEST(0), BEST(1:7)
        ELSE
          WRITE (NDSO,923) BEST(0), BEST(1:5)
          DO I=2, NQ-1
            WRITE (NDSO,924) BEST((I-1)*5+1:I*5)
            END DO
          WRITE (NDSO,925) BEST((NQ-1)*5+1:NQ*5+2)
        END IF
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
  900 FORMAT (/' descent3.x: get best member of line_pop     '/       &
               ' --------------------------------------------'/       &
               '    Number of quadruplets       : ',I4)
  910 FORMAT (/'    Setting up files ...')
  920 FORMAT ( '    Processing input files ...')
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
  999 FORMAT (/' End of descent3.x')
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
!/ End of DESCENT3 --------------------------------------------------- /
!/
      END PROGRAM DESCENT3
