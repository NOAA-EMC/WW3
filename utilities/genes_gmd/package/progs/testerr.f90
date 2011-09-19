!/ ------------------------------------------------------------------- /
      PROGRAM TSTERR
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         31-Aug-2009 |
!/                  +-----------------------------------+
!/
!/    31-Aug-2009 : Origination.
!/
!/    Copyright 2009-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Determine test error to establish full computation or not.
!
!  2. Method :
!
!     Read pars from std in, write test error line to std out.
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
      REAL :: MIN_ERR_POP, MIN_ERR, MAX_ERR, ERR_FAC, TERR
!/
!/ ------------------------------------------------------------------- /
!/
! 0.  Initialization
!
      WRITE (*,900)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Read data from std input
!
      READ (*,*,END=801,ERR=801) MIN_ERR_POP, MIN_ERR, MAX_ERR, ERR_FAC
      WRITE (*,910) MIN_ERR_POP, MIN_ERR, MAX_ERR, ERR_FAC
!
! 111 CONTINUE
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Test error
!
      TERR  = ERR_FAC * MIN_ERR_POP
      TERR  = MIN ( TERR , MAX_ERR )
      TERR  = MAX ( TERR , MIN_ERR )
      WRITE (*,920) TERR
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
      WRITE (*,999)
!
! Formats
!
  900 FORMAT (/' testerr.x: getting error level for tests :'/         &
               ' ------------------------------------------')
!
  910 FORMAT ( '    min error population : ',F7.3/                    &
               '    minimum error        : ',F7.3/                    &
               '    maximum error        : ',F7.3/                    &
               '    error factor         : ',F7.3/)
!
  920 FORMAT ( '    test error           : ',F7.3)
!
  999 FORMAT (/' End of testerr.x'/1X,F9.3)
!
 1001 FORMAT (/' *** ERROR IN READING INPUT ***'/)
!/
!/ End of TSTERR ----------------------------------------------------- /
!/
      END PROGRAM TSTERR
