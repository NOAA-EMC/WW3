!/ ------------------------------------------------------------------- /
      MODULE CGAUSSMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         02-Jan-2009 |
!/                  +-----------------------------------+
!/
!/    02-Jan-2009 : Origination.                        ( version 1.00 )
!/
!/    Copyright 2009-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Scalar and vector cut-off Gauss disctributions.
!     Adopted from previous bbstat.f F77 codes.
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      NTABC     I.P.  Private  Interpolation table dimension.
!      XMAXC     R.P.  Private  Max value in interpolation table.
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      CGAUSS    R.F.  Public   1-D cut-off Gauss distribution.
!      SETCGS    Subr  Public   Initialize interpolation tables.
!      CGSP      R.F.  Public   Get P from X.
!      CGSX      R.F.  Public   Get X from P.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
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
!/ Private parameter statements
!/
      INTEGER, PARAMETER, PRIVATE :: NDIM1 = 700, NDIM2 = 700,        &
                                     NDIM3 = 500
      REAL, PARAMETER, PRIVATE    :: XMAX = 7., UMAX = 7.
      REAL, PRIVATE               :: DX, DU, DP,                      &
                    PROB(-NDIM1:NDIM1,0:NDIM2), XX(0:NDIM3,0:NDIM2)
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      REAL FUNCTION CGAUSS ( X, XC, STD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         02-Jan-2009 !
!/                  +-----------------------------------+
!/
!/    08-Mar-1996 : Last update to F77 version.
!/    02-Jan-2009 : Origination.                        ( version 1.00 )
!/
!  1. Purpose :
!
!     Rescaled normal distribution, cut-off below zero, 1-D.
!
!  2. Method :
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       X       Real   I   Parameter for which pdf is to be evaluated.
!       XC      Real   I   Central parameter value.
!       STD     Real   I   Standard deviation.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     None.
!
!  5. Called by :
!
!     Any.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(IN)        :: X, XC, STD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER, PARAMETER      :: NTAB = 600
      REAL, PARAMETER         :: XMAX = 6.
!
      INTEGER, SAVE           :: IPASS = 0
      REAL, SAVE              :: PIFAC, DX, PTAB(-NTAB:NTAB)
!
      INTEGER                 :: I, IT1
      REAL                    :: XY, PDFDX, STDN, XCN, PR, RD, XCHECK
!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
      IF ( IPASS .EQ. 0 ) THEN
!
! 0.a Constants
!
          IPASS  = 1
          PIFAC  = 1. / SQRT(2.*PI)
          DX     = XMAX / REAL(NTAB)
!
! 0.b Table
!
          PTAB(-NTAB) = 0.
          DO I=1-NTAB, NTAB
            XY     = (REAL(I)-0.5)*DX
            PDFDX  = PIFAC * EXP(-0.5*XY**2) * DX
            PTAB(I) = PTAB(I-1) + PDFDX
            END DO
!
        ENDIF
!
! -------------------------------------------------------------------- /
! 1.  Calculate pdf
! 1.a Pr of cut-off point
!
      STDN   = MAX ( 0.001 , STD )
      XCN    = -XC / STDN
!
      IT1    = INT(XC/DX)
      IF ( XC .LT. 0. ) IT1 = IT1 - 1
!
      IF ( IT1 .LT. -NTAB ) THEN
          PR     = 1.
        ELSE IF ( IT1 .GE. NTAB ) THEN
          PR     = 0.
        ELSE
          RD     = XC/DX - REAL(IT1)
          PR     = 1. - (1.-RD)*PTAB(IT1) - RD*PTAB(IT1+1)
        ENDIF
!
      PR     = MAX ( 0.01 , PR )
!
! 1.b Cut-off pdf
!
      XCHECK = MIN ( 10., (ABS(X-XC)/STDN) )
      CGAUSS = PIFAC * EXP ( -0.5*XCHECK**2 ) / PR / STDN
!
      RETURN
!/
!/ End of CGAUSS ----------------------------------------------------- /
!/
      END FUNCTION CGAUSS
!/ ------------------------------------------------------------------- /
      SUBROUTINE SETCGS ( NDST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         02-Jan-2009 !
!/                  +-----------------------------------+
!/
!/    11-Mar-1996 : Last update to F77 version.
!/    02-Jan-2009 : Origination.                        ( version 1.00 )
!/
!  1. Purpose :
!
!     Set-up interpolation tables for cut-off Gauss (1-D).
!
!  2. Method :
!
!     Using CGAUSS.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NDST    Int.   I   Unit number for test output. If .LE. 0
!                          no test output is generated.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     CGAUSS from this module.
!
!  5. Called by :
!
!     Any.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
!
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: NDST
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: J, I, IP, INC1, INC2, INC3
      REAL                    :: X, U, PDF, PDFL, PR, DENOM, DDX
!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
      DX     = XMAX / REAL(NDIM1)
      DU     = UMAX / REAL(NDIM2)
      DP     = 1. / REAL(NDIM3)
!
! -------------------------------------------------------------------- /
! 1.  Fill PROB array using CGAUSS
! 1.a First fill
!
      DO J=0, NDIM2
        U      = REAL(J) * DU
        X      = -XMAX + U
        PDF    = 0.
        IF ( X .GE. 0. ) PDF = CGAUSS ( X, U, 1. )
        PROB(-NDIM1,J) = 0.
        DO I=1-NDIM1, NDIM1
          X      = U + REAL(I)*DX
          PDFL   = PDF 
          IF ( X .GE. 0. ) PDF = CGAUSS ( X, U, 1. )
          PROB(I,J) = PROB(I-1,J) + 0.5*(PDF+PDFL) 
          END DO
        END DO
!
! 1.b Normalization
!
      DO J=0, NDIM2
        DO I=1-NDIM1,NDIM1
          PROB(I,J) = PROB(I,J) / PROB(NDIM1,J)
          END DO
        END DO
!
! -------------------------------------------------------------------- /
! 2.  X from given probability
!
      DO J=0, NDIM2
!
! 2.a Probability 0.
!
        DO IP=1-NDIM1, NDIM1
          IF ( PROB(IP,J) .NE. 0 ) EXIT
          END DO
!
        IP     = IP - 1
        XX(0,J) = REAL(IP) * DX
!
! 2.b Intermediate probabilities
!
        DO I=1, NDIM3-1
          PR     = REAL(I) * DP
          DO
            IF ( PROB(IP,J) .LT. PR ) THEN
                IP     = IP + 1
              ELSE
                EXIT
              END IF
            END DO
          DENOM   = PROB(IP,J) - PROB(IP-1,J)
          IF ( DENOM .GT. 1.E-10 ) THEN
              DDX    = ( PR - PROB(IP,J) ) / DENOM
            ELSE
              DDX    = 0.
            ENDIF
          XX(I,J) = DX * ( REAL(IP) + DDX )
          END DO
!
! 2.c Probability 1
!
        XX(NDIM3,J) = XMAX
!
        END DO
!
! -------------------------------------------------------------------- /
! 3.  Test output
!
      IF ( NDST .GT. 0 ) THEN
!
          WRITE (NDST,900) NDIM1, DX, XMAX, NDIM2, DU, UMAX,          &
                           NDIM3, DP, 1.
          INC1   = 1 + (NDIM1-1)/7
          INC2   = 1 + (NDIM2-1)/7
          INC3   = 1 + (NDIM3-1)/10
          WRITE (NDST,901) INC1, INC2, INC3
!
          WRITE (NDST,910) (I,I=0,NDIM2,INC2)
          DO I=-NDIM1, NDIM1, INC1
            WRITE (NDST,920) I, (PROB(I,J),J=0,NDIM2,INC2)
            END DO
!
          WRITE (NDST,911) (I,I=0,NDIM2,INC2)
          DO I=0, NDIM3, INC3
            WRITE (NDST,920) I, (XX(I,J),J=0,NDIM2,INC2)
            END DO
!
          WRITE (NDST,999)
!
!     Formats ( test output only )
!
  900 FORMAT ( ' TEST SETCGS : NDIM1, DX, XMAX       : ',I5,2F7.2/    &
               '               NDIM2, DU, UMAX       : ',I5,2F7.2/    &
               '               NDIM3, DP, PMAX       : ',I5,2F7.2)
  901 FORMAT ( '               INC IN ARRAYS BELOW   : ',3I5)
  910 FORMAT (/' Probability functions : '/8X,12I6)
  911 FORMAT (/' Offsets : '/8X,12I6)
  920 FORMAT (1X,I7,11F6.3)
  999 FORMAT (/' TEST SETCGS : END OF ROUTINE '/)
!
        ENDIF
!
      RETURN
!/
!/ End of SETCGS ----------------------------------------------------- /
!/
      END SUBROUTINE SETCGS
!/ ------------------------------------------------------------------- /
      REAL FUNCTION CGSP ( X, U, STD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         02-Jan-2009 !
!/                  +-----------------------------------+
!/
!/    11-Mar-1996 : Last update to F77 version.
!/    02-Jan-2009 : Origination.                        ( version 1.00 )
!/
!  1. Purpose :
!
!     Obtain P from interpolation tables.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       X       Real   I   Parameter for which pdf is to be evaluated.
!       U       Real   I   Central parameter value.
!       STD     Real   I   Standard deviation.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     None.
!
!  5. Called by :
!
!     Any, after SETCGS has been called.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(IN)        :: X, U, STD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: IU1, IU2, IX1, IX2
      REAL                    :: STDN, UN, XN, XU, RU1, RU2, RX1, RX2
!
! -------------------------------------------------------------------- /
! 0.  Check input
!
      STDN   = MAX(STD,1.E-5)
      UN     = MAX(U,0.)/STDN
      XN     = MAX(X,0.)/STDN - UN
!
! -------------------------------------------------------------------- /
! 1.  Interpolation factors
!
      IX1    = INT(XN/DX)
      IF ( XN .LT. 0. ) IX1 = IX1 - 1
!
      IF ( IX1 .LT. -NDIM1 ) THEN
          CGSP   = 0.
          RETURN
        ELSE IF ( IX1 .GE. NDIM1 ) THEN
          CGSP   = 1.
          RETURN
        ENDIF
!
      IX2    = IX1 + 1
      RX2    = REAL(IX1)*DX - XN
      RX1    = 1. - RX2
!
      IU1    = INT(UN/DU)
      IF ( IU1 .GE. NDIM2 ) THEN
          IU1    = NDIM2
          IU2    = NDIM2
          RU1    = 1.
          RU2    = 0.
        ELSE
          RU2    = MOD(XN,1.)
          RU1    = 1.-RU2
          IU2    = IU1 + 1
        ENDIF
!
! -------------------------------------------------------------------- /
! 2.  Interpolate
!
      CGSP   = RX1*RU1*PROB(IX1,IU1) + RX2*RU1*PROB(IX2,IU1) +        &
               RX1*RU2*PROB(IX1,IU2) + RX2*RU2*PROB(IX2,IU2)
!
      RETURN
!/
!/ End of CGSP ------------------------------------------------------- /
!/
      END FUNCTION CGSP
!/ ------------------------------------------------------------------- /
      REAL FUNCTION CGSX ( P, U, STD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         02-Jan-2009 !
!/                  +-----------------------------------+
!/
!/    11-Mar-1996 : Last update to F77 version.
!/    02-Jan-2009 : Origination.                        ( version 1.00 )
!/
!  1. Purpose :
!
!     Obtain P from interpolation tables.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       P       Real   I   Probability.
!       U       Real   I   Central parameter value.
!       STD     Real   I   Standard deviation.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     None.
!
!  5. Called by :
!
!     Any, after SETCGS has been called.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(IN)        :: P, U, STD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: IP1, IP2, IU1, IU2
      REAL                    :: PN, STDN, UN, RP1, RP2, XU, RU1, RU2
!
! -------------------------------------------------------------------- /
! 0.  Check input
!
      PN     = MIN ( 1. , MAX ( 0., P ) )
      STDN   = MAX ( 1.E-5, STD )
      UN     = MAX(U,0.)/STDN
!
! -------------------------------------------------------------------- /
! 1.  Interpolation factors
!
      IP1    = MIN ( INT(PN/DP) , NDIM3-1 )
      IP2    = IP1 + 1
      RP2    = PN/DP - REAL(IP1)
      RP1    = 1. - RP2
!
      IU1    = INT(UN/DU)
      IF ( IU1 .GE. NDIM2 ) THEN
          IU1    = NDIM2
          IU2    = NDIM2
          RU1    = 1.
          RU2    = 0.
        ELSE
          RU2    = MOD(PN,1.)
          RU1    = 1.-RU2
          IU2    = IU1 + 1
        ENDIF
!
! -------------------------------------------------------------------- /
! 2.  Interpolate
!
      CGSX   = U + STD*( RP1*RU1*XX(IP1,IU1) + RP2*RU1*XX(IP2,IU1) +  &
                         RP1*RU2*XX(IP1,IU2) + RP2*RU2*XX(IP2,IU2) )
!
      RETURN
!/
!/ End of CGSX ------------------------------------------------------- /
!/
      END FUNCTION CGSX
!/
!/ End of module CGAUSSMD -------------------------------------------- /
!/
      END MODULE CGAUSSMD
