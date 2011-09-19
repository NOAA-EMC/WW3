!/ ------------------------------------------------------------------- /
      PROGRAM RESEED
!/
!/                  +-----------------------------------+
!/                  |                         NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         16-Dec-2008 |
!/                  +-----------------------------------+
!/
!/    16-Dec-2008 : Origination.                        ( version 1.00 )
!/    04-Feb-2010 : Fix declaration conflic.            ( version 1.01 )
!/
!/    Copyright 2008-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Generate a new seed number.
!
!  2. Method :
!
!     Use RAN3B from the random.f90 module.
!
!  3. Parameters :
!
!     None.
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      RAN3B     R.F.  RANDOM   Random number generator.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     None, stand-alone program.
!
!  6. Error messages :
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE RANDOM, ONLY: RAN2
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: ISEED
      REAL                    :: RAND
!/
!/ ------------------------------------------------------------------- /
!/
! 0.  Initialization
!
      READ (*,*) ISEED
      RAND   = RAN2(ISEED)
      WRITE (*,*) ISEED
!
!/
!/ End of RESEED ----------------------------------------------------- /
!/
      END PROGRAM RESEED
