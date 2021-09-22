#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3SDB1MD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           J. H. Alves             |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         29-May-2009 |
!/                  +-----------------------------------+
!/
!/    25-Apr-2007 : Origination of module.              ( version 3.11 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/
!/    Copyright 2009 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Dummy slot for bottom friction source term.
!
!  2. Variables and types :
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3SDB1    Subr. Public   Battjes and Janssen depth-induced
!                               breaking.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!     See subroutine documentation.
!
!  5. Remarks :
!
!  6. Switches :
!
!     See subroutine documentation.
!
!  7. Source code :
!/
!/ ------------------------------------------------------------------- /
!/
      PUBLIC
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3SDB1 (IX, A, DEPTH, EMEAN, FMEAN, WNMEAN, CG, LBREAK, S, D )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                        FORTRAN 90 |
!/                  |           J. H. Alves             |
!/                  |           H. L. Tolman            |
!/                  !           A. Roland               | 
!/                  | Last update :         08-Jun-2018 |
!/                  +-----------------------------------+
!/
!/    25-Apr-2007 : Origination of module.              ( version 3.11 )
!/    08-Jun-2018 : Add DEBUGDB1.                       ( version 6.04 )
!/    03-Apr-2019 : Rewrite in terms of energy density (A. Roland,version 6.07)
!/    03-Apr-2019 : Add Thornton & Guza, 1983          (A. Roland,version 6.07)
!/
!  1. Purpose :
!
!     Compute depth-induced breaking using Battjes and Janssen bore
!     model approach
!
!  2. Method : Battjes & Janssen (1978), 
! 
!           Sbr   = Dtot/Etot*WA = D * WA
!           Dtot  = 0.25*alpha*Qb*fm*Hmax²
!           fm    = sigma/2Pi
!           BB    = Hrms²/Hmax² = 8Etot/Hmax²
!           D     = Dtot/Etot = BJALFA * sigma / pi * Qb/BB = 2 * BJALFA * fm * Qb/BB
!
!           AR: only valid for Hrms .le. Hm, Qb .le. 1, otherwise, in the degenrative regime it is 
!           due to Qb > 1 that all wave are broken and Hrms .le. Hmax
!           MLIM can be used to enforce this conditions, source term will smoothly converge to this limit. 
!           
!     Where CDB   = SDBC1 = BJALFA (defaults to BJALFA = 1)
!                   modified via ww3_grid namelist parameter BJALFA
!           HM    = GAMMA * DEP
!           GAMMA = SDBC2 defaults to 0.73 (mean Battjes/Janssen value)
!                   modified via ww3_grid namelist parameter BJGAM
!
!     And QB is estimated by iterations using the nonlinear expression 
!
!           1 - QB = HRMS**2
!           ------   -------
!            ln QB    HM**2
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       A       R.A.  I   Action density spectrum (1-D)
!       EMEAN   Real  I   Mean wave energy.
!       FMEAN   Real  I   Mean wave frequency.
!       WNMEAN  Real  I   Mean wave number.
!       DEPTH   Real  I   Mean water depth.
!       S       R.A.  O   Source term (1-D version).
!       D       R.A.  O   Diagonal term of derivative (1-D version).
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!       STRACE   Subroutine tracing (!/S switch).
!
!  5. Called by :
!
!       W3SRCE   Source term integration.
!       W3EXPO   Point output post-processor.
!       GXEXPO   GrADS point output post-processor.
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!     - Note that the Miche criterion con influence wave growth.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S   Enable subroutine tracing.
!     !/Tn  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      USE CONSTANTS
      USE W3GDATMD, ONLY: NK, NTH, NSPEC, SDBC1, SDBC2, FDONLY, FSSOURCE, DDEN
      USE W3ODATMD, ONLY: NDST
      USE W3GDATMD, ONLY: SIG
      USE W3ODATMD, only : IAPROC
!/S      USE W3SERVMD, ONLY: STRACE
!/T0      USE W3ARRYMD, ONLY: PRT2DS
!/T1      USE W3ARRYMD, ONLY: OUTMAT
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IX ! Local grid number
      REAL, INTENT(IN)        :: A(NSPEC)
      REAL, INTENT(INOUT)     :: EMEAN, FMEAN, WNMEAN, DEPTH
      REAL, INTENT(OUT)       :: S(NSPEC), D(NSPEC)
      REAL, INTENT(IN)        :: CG(NK)
      LOGICAL, INTENT(OUT)    :: LBREAK
      INTEGER                 :: ITH, IK, IWB
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: IS
!/S      INTEGER, SAVE           :: IENT = 0
!/T0      INTEGER                 :: IK, ITH
      REAL*8                    :: HM, BB, ARG, Q0, QB, B, CBJ, HRMS, EB(NK)
      REAL*8                    :: AUX, CBJ2, RATIO, S0, S1, THR, BR1, BR2, FAK
      REAL                      :: ETOT, FMEAN2
!/T0      REAL                    :: DOUT(NK,NTH)
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3SDB1')
!
! 0.  Initialzations ------------------------------------------------- /
!     Never touch this 4 lines below ... otherwise my exceptionhandling will not work.

      THR = DBLE(1.E-15)
      IF (SUM(A) .LT. THR) RETURN

      S = 0.
      D = 0.
      IWB = 1
!
!/T      WRITE (NDST,9000) SDBC1, SDBC2, FDONLY
!
! 1.  Integral quantities. AR: make sure mean quantities are computed, need to move upward
!
      ETOT = 0.
      FMEAN2 = 0.
      DO IK=1, NK
        EB(IK) = 0.
        DO ITH=1, NTH
          EB(IK) = EB(IK) + A(ITH+(IK-1)*NTH) 
        END DO
      END DO
      DO IK=1, NK
        EB(IK) = EB(IK) * DDEN(IK) / CG(IK)
        ETOT  = ETOT  + EB(IK)
      END DO
      DO IK=1, NK
        FMEAN2 = FMEAN2 + EB(IK) * SIG(IK) 
      END DO
      FMEAN2 = FMEAN2 / ETOT * TPIINV
!
! 2do compute wlmean
!
! 1.a. Maximum wave height
! 1.a.1. Simple limit
!
!/DEBUGDB1      WRITE(740+IAPROC,*) 'FDONLY=', FDONLY
!/DEBUGDB1      WRITE(740+IAPROC,*) 'FSSOURCE=', FSSOURCE
!/DEBUGDB1      FLUSH(740+IAPROC)
      IF ( FDONLY ) THEN
          HM     = DBLE(SDBC2) * DBLE(DEPTH)
      ELSE
!
! 1.a.2. Miche style criterion
!
          HM     = DBLE(SDBC2) / DBLE(WNMEAN) * TANH ( DBLE(WNMEAN) * MAX(DEPTH,0.) )
      END IF
!
!AR: Add Dingemans ...
! 1.b. Hrms and ratio Hrms / Hmax
! 
      HRMS = DSQRT (8.d0 * DBLE(EMEAN))
      IF ( HM .GT. THR) THEN
        BB     = HRMS * HRMS / ( HM * HM )
        B      = DSQRT(BB) 
      ELSE
        BB     = 0.d0
        B      = 0.d0
      END IF
!
! 2. Fraction of breaking waves -------------------------------------- /
! 2.a. First guess breaking fraction
!
      IF ( B .LE. 0.5d0 ) THEN
          Q0     = 0.d0
      ELSE IF ( B .LE. 1.d0 ) THEN
          Q0     = ( 2.d0 * B - 1.d0 ) ** 2
      END IF
!
! 2.b. Iterate to obtain actual breaking fraction
!
      IF ( B .LE. 0.2d0 ) THEN
        QB     = 0.d0
      ELSE IF ( B .LT. 1.d0 ) THEN
        ARG    = EXP  (( Q0 - 1.d0 ) / BB )
        QB     = Q0 - BB * ( Q0 - ARG ) / ( BB - ARG )
        DO IS=1, 3
          QB     = EXP((QB-1.)/BB)
        END DO
      ELSE
        QB = 1.0 - THR
      END IF
!
! 3. Estimate the breaking coefficient ------------------------------- /
!
      CBJ  = 0  
      IF (IWB == 1) THEN
        IF ( ( BB .GT. THR) .AND. ( ABS ( BB - QB ) .GT. THR) ) THEN
          IF ( BB .LT. 1.0) THEN
            CBJ = 2 * DBLE(SDBC1) * QB * DBLE(FMEAN) / BB
          ELSE
            CBJ = 2 * DBLE(SDBC1) * DBLE(FMEAN) * BB ! AR: degenerative regime, all waves must be .le. Hmax, we just smoothly let the excessive energy vanish by * BB. 
          END IF
        ELSE
          CBJ = 0.d0
        ENDIF
        D = - CBJ
        S = D * A
      ELSE IF (IWB == 2) THEN 
        IF (ETOT .GT. THR) THEN
          HRMS = SQRT(8*EMEAN)
          FAK  = (1+4./SQRT(PI)*(B*BB+1.5*B)*exp(-BB)-ERF(B))
          CBJ  = -SDBC1*SQRT(PI)/16.*FMEAN*HRMS**3/DEPTH/ETOT
        ELSE
          CBJ  = 0.
        ENDIF
        D = - CBJ
        S = D * A 
      ENDIF

      IF (CBJ .GT. 0.) THEN
        LBREAK = .TRUE. 
      ELSE
        LBREAK = .FALSE. 
      ENDIF

!/DEBUGRUN      IF (IX == DEBUG_NODE) THEN
!/DEBUGRUN        WRITE(*,'(A200)') 'IX, DEPTH, CBJ, BB, QB, SDBC1, SDBC2, FMEAN, FMEAN2, HS'
!/DEBUGRUN        WRITE(*,'(I10,20F20.10)') IX, DEPTH, CBJ, BB, QB, SDBC1, SDBC2, FMEAN, FMEAN2, 4*SQRT(ETOT)
!/DEBUGRUN      ENDIF

!/DEBUGDB1      WRITE(740+IAPROC,*) 'CBJ=', CBJ
!/DEBUGDB1      FLUSH(740+IAPROC)
!
! ... Test output of arrays
!
!/T0      DO IK=1, NK
!/T0        DO ITH=1, NTH
!/T0          DOUT(IK,ITH) = D(ITH+(IK-1)*NTH)
!/T0          END DO
!/T0        END DO
!
!/T0      CALL PRT2DS (NDST, NK, NK, NTH, DOUT, SIG, '  ', 1.,    &
!/T0                         0.0, 0.001, 'Diag Sdb', ' ', 'NONAME')
!     
!/T1      CALL OUTMAT (NDST, D, NTH, NTH, NK, 'diag Sdb')
!  
      RETURN
!
! Formats   
!
!/T 9000 FORMAT (' TEST W3SDB1 : PARAMETERS :',2F7.3,L4)
!/
!/ End of W3SDB1 ----------------------------------------------------- /
!/
      END SUBROUTINE W3SDB1
!/
!/
!/ End of module W3SDB1MD -------------------------------------------- /
!/
      END MODULE W3SDB1MD
