!/ ------------------------------------------------------------------- /
      MODULE QTOOLSMD
!/
!/                  +-----------------------------------+
!/                  |                         NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         04-Mar-2010 |
!/                  +-----------------------------------+
!/
!/    20-Dec-2008 : Origination.                        ( version 1.00 )
!/    31-Dec-2008 : Adding QSORT and QDIFF.             ( version 1.01 )
!/    28-Sep-2009 : Adding QDIFA.                       ( version 1.02 )
!/    04-Mar-2010 : All lambda = mu test in QTEST.      ( version 1.03 )
!/
!/    Copyright 2008-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Tools to process quadruplets for GMD optimization package.
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
!      QTEST     L.F.  Public   Sort and test quadruplet.
!      QSORT     Subr  Public   Sort a set of quadruplets.
!      QDIFF     L.F.  Public   Check if (sorted) quadruplet strings
!                               are different (children).
!      QDIFFC    L.F.  Public   Check if (sorted) quadruplet strings
!                               are different (copied parents).
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
!/ Private parameter statements (ID strings)
!/
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      LOGICAL FUNCTION QTEST ( LI, MI, DTI, LO, MO, DTO )
!/
!/                  +-----------------------------------+
!/                  |                         NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         04-Mar-2010 !
!/                  +-----------------------------------+
!/
!/    20-Dec-2008 : Origination.                        ( version 1.00 )
!/    04-Mar-2010 : All lambda = mu test.               ( version 1.01 )
!/
!  1. Purpose :
!
!     Order quadruplet and check validity.
!
!  2. Method :
!
!     Ordering of Lambda and Mu, test is resonance can be satisfied.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       LI      Real   I   Input lambda.
!       MI      Real   I   Input mu.
!       DTI     Real   I   Input D theta
!       LO      Real   O   Output lambda.
!       MO      Real   O   Output mu.
!       DTO     Real   O   Output D theta
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
!  6. Error messages :
!
!     None.
!
!  7. Remarks :
!
!      - Deep water test only, reorder lambda mu and angle if needed.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
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
      REAL, INTENT(IN)        :: LI, MI, DTI
      REAL, INTENT(OUT)       :: LO, MO, DTO
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      REAL                    :: S1, S2, S3, S4, W0, W1, W2, W3, W4,  &
                                 ANG, DT3, DT4
!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
      QTEST  = .FALSE.
      LO     = 0.
      MO     = 0.
      DTO    = 0.
!
! -------------------------------------------------------------------- /
! 1.  Initial check
!
      IF ( MIN ( LI, MI ) .LT. 0. ) RETURN
      IF ( MAX ( LI, MI ) .GT. 0.50 ) RETURN
      IF ( LI .EQ. MI ) RETURN
!
! -------------------------------------------------------------------- /
! 2.  Simple 1 and 2 par quad
!
      IF ( DTI .LT. 0. ) THEN
          LO     = MAX ( LI, MI )
          MO     = MIN ( LI, MI )
          DTO    = DTI
          QTEST  = .TRUE.
        END IF
!
! -------------------------------------------------------------------- /
! 3.  3 par quad
!
      S1     = ( 1. + MI )
      S2     = ( 1. - MI )
      S3     = ( 1. + LI )
      S4     = ( 1. - LI )
!
      W1     = S1**2 / GRAV
      W2     = S2**2 / GRAV
      W3     = S3**2 / GRAV
      W4     = S4**2 / GRAV
!
      ANG    = DTI * DERA
      W0     = W1**2 + W2**2 + 2.*W1*W2*COS(ANG)
      W0     = SQRT ( MAX ( W0 , 0. ) )
!
      DT3    = (W3**2+W0**2-W4**2) / (2.*W0*W3)
      DT4    = (W4**2+W0**2-W3**2) / (2.*W0*W4)
!
      IF ( ABS(DT3).LE.1. .AND. ABS(DT4).LE.1. ) THEN
          QTEST  = .TRUE.
          IF ( MI .LE. LI ) THEN
              LO     = LI
              MO     = MI
              DTO    = DTI
            ELSE
              LO     = MI
              MO     = LI
              DT3    = ACOS(DT3) * RADE
              DT4    = ACOS(DT4) * RADE
              DTO    = DT3 + DT4
            END IF
!
        END IF 
!
! -------------------------------------------------------------------- /
! 4.  End of routine
!
      RETURN
!/
!/ End of QTEST ------------------------------------------------------ /
!/
      END FUNCTION QTEST
!/ ------------------------------------------------------------------- /
      SUBROUTINE QSORT ( NQ, LGEN, QINP, QOUT, FLAGOK, MQ )
!/
!/                  +-----------------------------------+
!/                  |                         NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         31-Dec-2008 !
!/                  +-----------------------------------+
!/
!/    31-Dec-2008 : Origination.                        ( version 1.00 )
!/
!  1. Purpose :
!
!     Order a set of quadruplets and return if one or more valid
!     quadruplets are found.
!
!  2. Method :
!
!     Ordering of Lambda and Mu, test is resonance can be satisfied.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NQ      Int.   I   Number of quadruplets.
!       LGEN    Int.   I   Lengt of genome.
!       QINP    R.A.   I   Input set of quadruplets.
!       QOUT    R.A.   O   Output set of quadruplets.
!       FLAGOK  Log.   O   Flag for at least one valid quadruplet set.
!       MQ      Real   O   Number of valid quadruplets (optional)
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!     
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      QTEST     L.F.  Local    Test validity of quadruplet layout.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any.
!
!  6. Error messages :
!
!     None.
!
!  7. Remarks :
!
!      - Deep water test only, reorder lambda mu and angle if needed.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
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
      INTEGER, INTENT(IN)     :: NQ, LGEN
      INTEGER, INTENT(OUT),OPTIONAL  :: MQ
      REAL, INTENT(IN)        :: QINP(0:LGEN)
      REAL, INTENT(OUT)       :: QOUT(0:LGEN)
      LOGICAL, INTENT(OUT)    :: FLAGOK
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: I
      REAL                    :: L, M, DT, C1, C2
      LOGICAL                 :: CHANGE
!
! -------------------------------------------------------------------- /
! 0.  Initializations
!
      FLAGOK  = .FALSE.
      QOUT    = QINP
      IF ( PRESENT(MQ) ) MQ = 0
!
! -------------------------------------------------------------------- /
! 1.  Check individual quadruplets
!
        DO I=1, NQ
          IF ( QOUT((I-1)*5+4).EQ.0. .AND.  QOUT(I*5).EQ.0. ) THEN
              QOUT((I-1)*5+1) = 0.999
              QOUT((I-1)*5+2) = 0.999
              QOUT((I-1)*5+3) = -1.
            ELSE
              IF ( QTEST ( QOUT((I-1)*5+1), QOUT((I-1)*5+2),      &
                           QOUT((I-1)*5+3), L, M, DT ) ) THEN
                  QOUT((I-1)*5+1) = L
                  QOUT((I-1)*5+2) = M
                  QOUT((I-1)*5+3) = DT
                  FLAGOK          = .TRUE.
                  IF ( PRESENT(MQ) ) MQ = MQ + 1
                ELSE
                  QOUT((I-1)*5+1) = 0.999
                  QOUT((I-1)*5+2) = 0.999
                  QOUT((I-1)*5+3) = -1.
                  QOUT((I-1)*5+4) = 0.
                  QOUT((I-1)*5+5) = 0.
                END IF
            END IF
          END DO
!
! -------------------------------------------------------------------- /
! 2.  Sort quaduplets
!
        DO
          CHANGE = .FALSE.
          DO I=1, NQ-1
            IF ( QOUT((I-1)*5+1) .GT. QOUT(I*5+1) ) THEN
                L      = QOUT((I-1)*5+1)
                M      = QOUT((I-1)*5+2)
                DT     = QOUT((I-1)*5+3)
                C1     = QOUT((I-1)*5+4)
                C2     = QOUT((I-1)*5+5)
                QOUT((I-1)*5+1:I*5) = QOUT(I*5+1:(I+1)*5)
                QOUT(I*5+1) = L
                QOUT(I*5+2) = M
                QOUT(I*5+3) = DT
                QOUT(I*5+4) = C1
                QOUT(I*5+5) = C2
                CHANGE  = .TRUE.
              END IF
            END DO
          IF ( .NOT. CHANGE ) EXIT
          END DO
!
! -------------------------------------------------------------------- /
! 3.  End of routine
!
      RETURN
!/
!/ End of QSORT ------------------------------------------------------ /
!/
      END SUBROUTINE QSORT
!/ ------------------------------------------------------------------- /
      LOGICAL FUNCTION QDIFF ( NQ, LGEN, Q1, Q2 )
!/
!/                  +-----------------------------------+
!/                  |                         NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         28-Sep-2009 !
!/                  +-----------------------------------+
!/
!/    31-Dec-2008 : Origination.                        ( version 1.00 )
!/    28-Sep-2009 : Relax criteria.                     ( version 1.01 )
!/
!  1. Purpose :
!
!     Check if members of population are differents.
!
!  2. Method :
!
!     Ordering of Lambda and Mu, test is resonance can be satisfied.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NQ      Int.   I   Number of quadruplets.
!       LGEN    Int.   I   Lengt of genome.
!       Q1/2    R.A.   I   Input set of members of population.
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
!  6. Error messages :
!
!     None.
!
!  7. Remarks :
!
!      - Deep water test only, reorder lambda mu and angle if needed.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
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
      INTEGER, INTENT(IN)     :: NQ, LGEN
      REAL, INTENT(IN)        :: Q1(0:LGEN), Q2(0:LGEN)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: I, IQ
!
      REAL                    :: TOLE = 0.0099
      REAL                    :: TOLL = 0.0049
      REAL                    :: TOLM = 0.0049
      REAL                    :: TOLD = 2.5
      REAL                    :: TOLC = 0.05
      REAL                    :: TOLA = 0.15
      REAL                    :: TOLB = 0.15
!/
! -------------------------------------------------------------------- /
! 0.  Initializations
!
      QDIFF  = .FALSE.
!
! -------------------------------------------------------------------- /
! 1.  Check bits and pieces
! 1.a Error
!
      IF ( ABS(1.-Q1(0)/Q2(0)) .GT. TOLE ) GOTO 222
!
! 1.b m and n
!
      I      = NQ*5
      IF ( ABS(Q1(I+1)-Q2(I+1)) .GT. TOLA ) GOTO 222
      IF ( ABS(Q1(I+2)-Q2(I+2)) .GT. TOLB ) GOTO 222
!
! 1.c quad pars
!
      DO IQ=1, NQ
        I      = (IQ-1)*5
        IF ( ABS(Q1(I+1)-Q2(I+1)) .GT. TOLL ) GOTO 222
        IF ( ABS(Q1(I+2)-Q2(I+2)) .GT. TOLM ) GOTO 222
        IF ( ABS(Q1(I+3)-Q2(I+3)) .GT. TOLD ) GOTO 222
        IF ( Q2(I+4) .GT. 1. ) THEN
            IF ( ABS(1.-Q1(I+4)/Q2(I+4)) .GT. TOLC ) GOTO 222
          ELSE
            IF ( Q1(I+4) .GT. 1. ) GOTO 222
          END IF
        IF ( Q2(I+5) .GT. 1. ) THEN
            IF ( ABS(1.-Q1(I+5)/Q2(I+5)) .GT. TOLC ) GOTO 222
          ELSE
            IF ( Q1(I+5) .GT. 1. ) GOTO 222
          END IF
        END DO
!
! 1.d end of routine for equal
!
      RETURN
!
! -------------------------------------------------------------------- /
! 2.  End of routine for difference
!
  222 CONTINUE
      QDIFF  = .TRUE.
!
      RETURN
!/
!/ End of QDIFF ------------------------------------------------------ /
!/
      END FUNCTION QDIFF
!/ ------------------------------------------------------------------- /
      LOGICAL FUNCTION QDIFFA ( NQ, LGEN, Q1, Q2 )
!/
!/                  +-----------------------------------+
!/                  |                         NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         28-Sep-2009 !
!/                  +-----------------------------------+
!/
!/    28-Sep-2009 : Origination (from QDIFF).           ( version 1.00 )
!/
!  1. Purpose :
!
!     QDIFF with absolute equality.
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
      INTEGER, INTENT(IN)     :: NQ, LGEN
      REAL, INTENT(IN)        :: Q1(0:LGEN), Q2(0:LGEN)
!/
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: I, IQ
!/
! -------------------------------------------------------------------- /
! 0.  Initializations
!
      QDIFFA = .FALSE.
!
! -------------------------------------------------------------------- /
! 1.  Check bits and pieces
! 1.a Error
!
      IF ( Q1(0) .NE. Q2(0) ) GOTO 222
!
! 1.b m and n
!
      I      = NQ*5
      IF ( Q1(I+1) .NE. Q2(I+1) ) GOTO 222
      IF ( Q1(I+2) .NE. Q2(I+2) ) GOTO 222
!
! 1.c quad pars
!
      DO IQ=1, NQ
        I      = (IQ-1)*5
        IF ( Q1(I+1) .NE. Q2(I+1) ) GOTO 222
        IF ( Q1(I+2) .NE. Q2(I+2) ) GOTO 222
        IF ( Q1(I+3) .NE. Q2(I+3) ) GOTO 222
        IF ( Q1(I+4) .NE. Q2(I+4) ) GOTO 222
        IF ( Q1(I+5) .NE. Q2(I+5) ) GOTO 222
        END DO
!
! 1.d end of routine for equal
!
      RETURN
!
! -------------------------------------------------------------------- /
! 2.  End of routine for difference
!
  222 CONTINUE
      QDIFFA = .TRUE.
!
      RETURN
!/
!/ End of QDIFFA ----------------------------------------------------- /
!/
      END FUNCTION QDIFFA
!/
!/ End of module QTOOLSMD -------------------------------------------- /
!/
      END MODULE QTOOLSMD
