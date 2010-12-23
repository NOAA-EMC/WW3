!/ ------------------------------------------------------------------- /
      MODULE RANDOM
!/
!/                  +-----------------------------------+
!/                  |                         NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         16-Dec-2008 |
!/                  +-----------------------------------+
!/
!/    16-Dec-2008 : Origination.                        ( version 2.00 )
!/
!/    Copyright 2008-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Random number generators from Numerical Recepies.
!     Ported from previous DIA studies.
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      RAN2      Subr. Public   RAndom number generator.
!      RAN3      Subr. Public   RAndom number generator.
!      RAN3B     Subr. Public   RAndom number generator.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!     None.
!
!  5. Remarks :
!
!  6. Switches :
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      PUBLIC
!/
!/ Private parameter statements (ID strings)
!/
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      REAL FUNCTION RAN2 (ISEED)
!/
!/                  +-----------------------------------+
!/                  |                         NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 77 |
!/                  | Last update :         16-Jun-1995 |
!/                  +-----------------------------------+
!/
!  1. Purpose :
!
!     Randum number generator 'RAN2' from numerical recipes, p.197.
!     See book for docimentation.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       ISEED   Int.  I/O  Seed. Reseeding on first call or negative
!                          seed number.
!     ----------------------------------------------------------------
!
! 10. Source code :
!
! ---------------------------------------------------------------------
!     
      PARAMETER ( M      = 714025 )
      PARAMETER ( IA     =   1366 )
      PARAMETER ( IC     = 150899 )
      PARAMETER ( RM     = 1./M   )
! 
      PARAMETER ( IRD    =     97 )
      DIMENSION   IR(IRD)
!
      SAVE IFF, IR, IY   
      DATA IFF  / 0 /
!
      IF ( ISEED.LT.0 .OR. IFF.EQ.0 ) THEN
          IFF    = 1
          ISEED  = - ABS(ISEED)
          ISEED  = MOD ( IC-ISEED , M )
          DO J=1, IRD
            ISEED  = MOD ( IA*ISEED+IC , M )
            IR(J)  = ISEED
            END DO
          ISEED  = MOD ( IA*ISEED+IC , M )
          IY     = ISEED 
        ENDIF    
!
      J      = 1 + (IRD*IY)/M
      IF ( J.GT.IRD .OR. J.LT.0 ) PAUSE
      IY     = IR(J) 
      RAN2   = IY*RM 
      ISEED  = MOD ( IA*ISEED+IC , M )
      IR(J)  = ISEED
!
      RETURN
!
! End of RAN2 ----------------------------------------------------------
!
      END FUNCTION RAN2
!/ ------------------------------------------------------------------- /
      REAL FUNCTION RAN3 (ISEED)
!/
!/                  +-----------------------------------+
!/                  |                         NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 77 |
!/                  | Last update :         16-Jun-1995 |
!/                  +-----------------------------------+
!/
!  1. Purpose :
!
!     Randum number generator 'RAN3' from numerical recipes, p.199.
!     See book for docimentation.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       ISEED   Int.  I/O  Seed. Reseeding on first call or negative
!                          seed number.
!     ----------------------------------------------------------------
!
! 10. Source code :
!
! ---------------------------------------------------------------------
!
      PARAMETER ( MBIG   = 1000000000 )
      PARAMETER ( MSEED  =    1618033 )
      PARAMETER ( MZ     =          0 )
      PARAMETER ( FAC    = 1./MBIG    )
!
      DIMENSION   MA(55)
!
      SAVE IFF, MA, INEXT, INEXTP
      DATA IFF  / 0 /
!
      IF ( ISEED.LT.0 .OR. IFF.EQ.0 ) THEN
          IFF    = 1
          MJ     = ABS ( MSEED - IABS(ISEED) )
          MJ     = MOD ( MJ , MBIG )
          MA(55) = MJ
          MK     = 1
          DO I=1, 54
            II     = MOD ( 21*I , 55 )
            MA(II) = MK
            MK     = MJ - MK
            IF ( MK .LT. MZ ) MK = MK + MBIG
            MJ     = MA(II)
            END DO
          DO K=1, 4
            DO I=1, 55
              MA(I)  = MA(I) - MA(1+MOD(I+30,55))
              IF ( MA(I) .LT. MZ ) MA(I) = MA(I) + MBIG
              END DO
            END DO
          INEXT  =  0
          INEXTP = 31
          ISEED  = 1
        ENDIF
!
      INEXT  = 1 + MOD(INEXT,55)
      INEXTP = 1 + MOD(INEXTP,55)
      MJ     = MA(INEXT) - MA(INEXTP)
      IF ( MJ .LT. MZ ) MJ = MJ + MBIG
      MA(INEXT) = MJ
      RAN3   = MJ * FAC
!
      RETURN
!
! End of RAN3 ----------------------------------------------------------
!
      END FUNCTION RAN3
!/ ------------------------------------------------------------------- /
      REAL FUNCTION RAN3B (ISEED)
!/
!/                  +-----------------------------------+
!/                  |                         NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 77 |
!/                  | Last update :         16-Jun-1995 |
!/                  +-----------------------------------+
!/
!  1. Purpose :
!
!     Duplicate copy of RAN3.
!
! 10. Source code :
!
! ---------------------------------------------------------------------
!
      PARAMETER ( MBIG   = 1000000000 )
      PARAMETER ( MSEED  =    1618033 )
      PARAMETER ( MZ     =          0 )
      PARAMETER ( FAC    = 1./MBIG    )
!
      DIMENSION   MA(55)
!
      SAVE IFF, MA, INEXT, INEXTP
      DATA IFF  / 0 /
!
      IF ( ISEED.LT.0 .OR. IFF.EQ.0 ) THEN
          IFF    = 1
          MJ     = ABS ( MSEED - IABS(ISEED) )
          MJ     = MOD ( MJ , MBIG )
          MA(55) = MJ
          MK     = 1
          DO I=1, 54
            II     = MOD ( 21*I , 55 )
            MA(II) = MK
            MK     = MJ - MK
            IF ( MK .LT. MZ ) MK = MK + MBIG
            MJ     = MA(II)
            END DO
          DO K=1, 4
            DO I=1, 55
              MA(I)  = MA(I) - MA(1+MOD(I+30,55))
              IF ( MA(I) .LT. MZ ) MA(I) = MA(I) + MBIG
              END DO
            END DO
          INEXT  =  0
          INEXTP = 31
          ISEED  = 1
        ENDIF
!
      INEXT  = 1 + MOD(INEXT,55)
      INEXTP = 1 + MOD(INEXTP,55)
      MJ     = MA(INEXT) - MA(INEXTP)
      IF ( MJ .LT. MZ ) MJ = MJ + MBIG
      MA(INEXT) = MJ
      RAN3B  = MJ * FAC
!
      RETURN
!
! End of RAN3B ---------------------------------------------------------
!
      END FUNCTION RAN3B
!/
!/ End of module RANDOM ---------------------------------------------- /
!/
      END MODULE RANDOM
