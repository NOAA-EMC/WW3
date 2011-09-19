!/ ------------------------------------------------------------------- /
      PROGRAM ERRTOT
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
!     Comnpute composite error for all test cases.
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
      INTEGER                 :: I
      REAL                    :: ERROR, WGHT, SUM, SUMW
      CHARACTER(LEN=20)       :: TNAME
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
      SUM    = 0.
      SUMW   = 0.
!
      DO
        READ (*,*,END=111,ERR=801) ERROR, I, WGHT, TNAME
        SUM    = SUM  + ERROR*WGHT
        SUMW   = SUMW + WGHT
        WRITE (*,910) TNAME(1:10), ERROR, WGHT, SUM, SUMW
        END DO
!
  111 CONTINUE
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Final error
!
      IF ( SUMW .EQ. 0. ) THEN
          GOTO 802
        ELSE
          SUM    = SUM / SUMW
          SUM    = MIN ( 999.90 , SUM )
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
      WRITE (*,999) SUM
!
! Formats
!
  900 FORMAT (/' err_tot.x: combining errors for test case :'/       &
               ' -------------------------------------------')
!
  910 FORMAT (3X,A,4F9.3)
!
  999 FORMAT (/' End of err_tot.x'/1X,F9.3)
!
 1001 FORMAT (/' *** ERROR IN READING INPUT ***'/)
 1002 FORMAT (/' *** ERROR: ZERO SUM WEIGHT ***'/)
!/
!/ End of ERRTOT ----------------------------------------------------- /
!/
      END PROGRAM ERRTOT
