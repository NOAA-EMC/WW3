!/ ------------------------------------------------------------------- /
      PROGRAM ERRTEST
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         14-Jan-2009 |
!/                  +-----------------------------------+
!/
!/    24-Nov-2008 : Origination.
!/    24-Dec-2008 : Add maximum value.
!/    14-Jan-2009 : Increase accuracy of error.
!/
!/    Copyright 2008-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Comnpute composite error for a single test case.
!
!  2. Method :
!
!     Read from std in error weights and errors, write output to 
!     std out.
!
!  3. Parameters :
!
!  4. Subroutines used :
!
!     None.
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
!     USE CONSTANTS
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETER statements
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: I, SUMI
      REAL                    :: WEIGHT(23), ERRORS(23), SUM, SUMW
!/
!/ ------------------------------------------------------------------- /
!/
!
! 0.  Initialization
!
      WRITE (*,900)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Read data from std input
!
      READ (*,*,ERR=801) WEIGHT
      READ (*,*,ERR=801) ERRORS
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Process errors
!
      SUM    = 0.
      SUMW   = 0.
      SUMI   = 0
!
      WRITE (*,910)
!
      DO I=1, 23
        IF ( ERRORS(I) .GE. 0. ) THEN
            SUM    = SUM + WEIGHT(I) * ERRORS(I)
            SUMW   = SUMW + WEIGHT(I)
            IF ( WEIGHT(I) .GT. 0. ) SUMI = SUMI + 1
          END IF
        WRITE (*,911) I, ERRORS(I), WEIGHT(I), SUM, SUMW, SUMI
        END DO
!
      IF ( SUMW .GT. 0. ) THEN
          SUM    = SUM / SUMW
          SUM    = MIN ( 9999.99 , SUM )
        ELSE
          GOTO 802
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
      WRITE (*,1001)
      STOP 1001
!
  802 CONTINUE
      WRITE (*,1002)
      STOP 1002
!
  888 CONTINUE
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Finalize program
!
      WRITE (*,999) SUM, SUMI
!
! Formats
!
  900 FORMAT (/' err_case.x: combining errors for test case :'/       &
               ' --------------------------------------------'/       &
               '    Reading data from std in')
!
  910 FORMAT ( '    Processing errors ....')
  911 FORMAT ( '    ',I3,4F9.3, I4)
!
  999 FORMAT (/' End of err_case.x'/1X,F9.3,I4)
!
 1001 FORMAT (/' *** ERROR IN READING FROM STD IN ***'/)
 1002 FORMAT (/' *** ERROR : ZERO SUM OF WEIGHTS ***'/)
!/
!/ End of ERRTEST ---------------------------------------------------- /
!/
      END PROGRAM ERRTEST
