!/ ------------------------------------------------------------------- /
      PROGRAM ERRPAR
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         14-Jan-2009 |
!/                  +-----------------------------------+
!/
!/    24-Dec-2008 : Origination.
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
!     Comnpute composite error per parameter for a set of test cases.
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
      INTEGER                 :: NCASE, I, J
      REAL                    :: CWEIGHT, WEIGHT(23), ERRORS(23),     &
                                 E(19), W(19)
!/
!/ ------------------------------------------------------------------- /
!/
! 0.  Initialization
!
      WRITE (*,900)
      E      = 0.
      W      = 0.
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Read data from std input
!
      NCASE  = 0
!
      DO
        READ (*,*,END=300,ERR=300) CWEIGHT
        NCASE  = NCASE + 1
        WRITE (*,910) NCASE, CWEIGHT
        READ (*,*,ERR=801) WEIGHT
        READ (*,*,ERR=801) ERRORS
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Process case
!
        J      = 0
!
        DO I=1, 23
!
          SELECT CASE (I)
          CASE (2)
          CASE (5)
          CASE (8)
          CASE (11)
          CASE DEFAULT
            J      = J + 1
          END SELECT
!
          IF ( ERRORS(I).GE.0. .AND. WEIGHT(I).GT.0. ) THEN
              E(J) = E(J) + ERRORS(I)
              W(J) = W(J) + CWEIGHT
            END IF
!
          END DO
!
        END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  Final processing
!
  300 CONTINUE
!
      DO I=1, 19
        IF ( W(I) .EQ. 0. ) THEN
            E(I) = -1.
          ELSE
            E(I) = MIN ( 9999.99 , E(I)/W(I) )
          END IF
        END DO
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
  888 CONTINUE
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Finalize program
!
      WRITE (*,999) E
!
! Formats
!
  900 FORMAT (/' err_par.x: combining errors for test case :'/        &
               ' -------------------------------------------'/        &
               '    Reading data from std in')
  910 FORMAT ( '       Processing data set',I2,' weight is',f5.2)
  999 FORMAT (/' End of err_par.x'/1X,19F9.3)
!
 1001 FORMAT (/' *** ERROR IN READING FROM STD IN ***'/)
!/
!/ End of ERRPAR ----------------------------------------------------- /
!/
      END PROGRAM ERRPAR
