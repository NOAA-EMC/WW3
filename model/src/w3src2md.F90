!> @file
!> @brief Contains MODULE W3SRC2MD.
!>
!> @author H. L. Tolman  @date 29-May-2009
!>

#include "w3macros.h"
!>
!> @brief Tolman and Chalikov (1996) input and dissipation source terms.
!>
!> @details Bundled with interpolation tables.
!>
!> @author H. L. Tolman  @date 29-May-2009
!>
!/ ------------------------------------------------------------------- /
MODULE W3SRC2MD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         29-May-2009 |
  !/                  +-----------------------------------+
  !/
  !/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  !/    21-Feb-2004 : Multiple model version.             ( version 3.06 )
  !/    03-Jul-2006 : Extract stress computation.         ( version 3.09 )
  !/    13-Apr-2007 : EMEAN in W3SPR2 par list.           ( version 3.11 )
  !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Tolman and Chalikov (1996) input and dissipation source terms.
  !     Bundled with interpolation tables.
  !
  !  2. Variables and types :
  !
  !      Interpolation tables :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      NRSIGA    I.P.  Public   Array dimension (SIGA).
  !      NRDRAG    I.P.  Public   Array dimension (drag coefficient).
  !      SIGAMX    R.P.  Public   Maximum nondiensional frequency.
  !      DRAGMX    R.P.  Public   Maximum drag coefficient.
  !      DSIGA     Real  Public   Table increment.
  !      DDRAG     Real  Public   Id.
  !      BETATB    R.A.  Public   Interpolation table.
  !     ----------------------------------------------------------------
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      W3SPR2    Subr. Public   Mean parameters from spectrum.
  !      W3SIN2    Subr. Public   Input source term.
  !      W3SDS2    Subr. Public   Dissipation source term.
  !      INPTAB    Subr. Public   Interpolation table for wind-wave
  !                               interaction parameter.
  !      W3BETA    R.F.  INPTAB   Id. function.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      STRACE    Subr. W3SERVMD Subroutine tracing.            ( !/S )
  !      PRT2DS    Subr. W3ARRYMD Print plot of spectra.        ( !/T0 )
  !      OUTMAT    Subr. W3WRRYMD Print out 2D matrix.          ( !/T1 )
  !      ...       Data  W3DISPMD Interpolation tables to solve
  !                               dispersion relation.
  !     ----------------------------------------------------------------
  !
  !  5. Remarks :
  !
  !  6. Switches :
  !
  !       !/S      Enable subroutine tracing.
  !       !/T(n)   Test output, see subroutines.
  !
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
  PUBLIC
  !/
  !/ Interpolation table
  !/
  INTEGER, PARAMETER, PRIVATE :: NRSIGA =  400
  INTEGER, PARAMETER, PRIVATE :: NRDRAG =   20
  REAL, PARAMETER, PRIVATE    :: SIGAMX =   40.
  REAL, PARAMETER, PRIVATE    :: DRAGMX =    1.E-2
  !
  REAL, PRIVATE           :: DSIGA, DDRAG,                        &
       BETATB(-NRSIGA:NRSIGA+1,NRDRAG+1)
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Calculate mean wave parameters for the use in the source term
  !>  routines (Tolman and Chalikov).
  !>
  !> @param[in]  A      Action density spectrum.
  !> @param[in]  CG     Group velocities.
  !> @param[in]  WN     Wavenumbers.
  !> @param[in]  DEPTH  Water depth.
  !> @param[in]  FPI    Peak input frequency.
  !> @param[in]  U      Wind speed.
  !> @param[in]  USTAR  Friction velocity.
  !> @param[out] EMEAN  Total energy (variance).
  !> @param[out] FMEAN  Mean frequency.
  !> @param[out] WNMEAN Mean wavenumber.
  !> @param[out] AMAX   Maximum of action spectrum.
  !> @param[out] ALFA   Phillips' constant.
  !> @param[out] FP     Peak frequency.
  !>
  !> @author H. L. Tolman
  !> @author D. Chalikov
  !> @date   13-Apr-2007
  !>
  SUBROUTINE W3SPR2 (A, CG, WN, DEPTH, FPI, U, USTAR,             &
       EMEAN, FMEAN, WNMEAN, AMAX, ALFA, FP )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |            D.Chalikov             |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         13-Apr-2007 |
    !/                  +-----------------------------------+
    !/
    !/    06-Dec-1996 : Final version 1.18 / FORTRAN 77 version.
    !/    16-Nov-1999 : Add itteration to section 5. for removal of W3APR2.
    !/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/    21-Dec-2004 : Multiple model version.             ( version 3.06 )
    !/    03-Jul-2006 : Extract stress computation.         ( version 3.09 )
    !/    13-Apr-2007 : EMEAN in parameter list.            ( version 3.11 )
    !
    !  1. Purpose :
    !
    !     Calculate mean wave parameters for the use in the source term
    !     routines. (Tolman and Chalikov)
    !
    !  2. Method :
    !
    !     See source term routines.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.  I   Action density spectrum.
    !       CG      R.A.  I   Group velocities.
    !       WN      R.A.  I   Wavenumbers.
    !       DEPTH   Real  I   Water depth.
    !       FPI     Real  I   Peak input frequency.
    !       U       Real  I   Wind speed.
    !       USTAR   Real  I   Friction velocity.
    !       EMEAN   Real  O   Total energy (variance).
    !       FMEAN   Real  O   Mean frequency.
    !       WNMEAN  Real  O   Mean wavenumber.
    !       AMAX    Real  O   Maximum of action spectrum.
    !       ALFA    R.A.  O   Phillips' constant.
    !       FP      Real  O   Peak frequency.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3SRCE    Subr. W3SRCEMD Source term integration.
    !      W3EXPO    Subr.   N/A    Point output post-processor.
    !      GXEXPO    Subr.   N/A    GrADS point output post-processor.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !       !/S      Enable subroutine tracing.
    !       !/T      Enable test output.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD, ONLY: NK, NTH, DTH, SIG, DDEN, FTE, FTF, FTWN,    &
         NITTIN, ZWIND, CINXSI
#ifdef W3_T
    USE W3ODATMD, ONLY: NDST
#endif

#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE W3DISPMD, ONLY: NAR1D, DFAC, N1MAX, ECG1, EWN1, DSIE
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)        :: A(NTH,NK), CG(NK), WN(NK), DEPTH,    &
         FPI, U, USTAR
    REAL, INTENT(OUT)       :: EMEAN, FMEAN, WNMEAN, AMAX,          &
         ALFA(NK), FP
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IK, ITH, I1, ITT
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: EBAND, FPISTR, EB(NK), UST
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3SPR2')
#endif
    !
    UST    = MAX ( 0.0001 , USTAR )
    !
    EMEAN  = 0.
    FMEAN  = 0.
    WNMEAN = 0.
    AMAX   = 0.
    !
    ! 1.  Integral over directions and maximum --------------------------- *
    !
    DO IK=1, NK
      EB(IK) = 0.
      DO ITH=1, NTH
        EB(IK) = EB(IK) + A(ITH,IK)
        AMAX   = MAX ( AMAX , A(ITH,IK) )
      END DO
    END DO
    !
    ! 2.  Integrate over directions -------------------------------------- *
    !
    DO IK=1, NK
      ALFA(IK) = 2. * DTH * SIG(IK) * EB(IK) * WN(IK)**3
      EB(IK)   = EB(IK) * DDEN(IK) / CG(IK)
      EMEAN    = EMEAN  + EB(IK)
      FMEAN    = FMEAN  + EB(IK) / SIG(IK)
      WNMEAN   = WNMEAN + EB(IK) / SQRT(WN(IK))
    END DO
    !
    ! 3.  Add tail beyond discrete spectrum and get mean pars ------------ *
    !     ( DTH * SIG absorbed in FTxx )
    !
    EBAND  = EB(NK) / DDEN(NK)
    EMEAN  = EMEAN  + EBAND * FTE
    FMEAN  = FMEAN  + EBAND * FTF
    WNMEAN = WNMEAN + EBAND * FTWN
    !
    FMEAN  = TPIINV * EMEAN / MAX ( 1.E-7 , FMEAN )
    WNMEAN = ( EMEAN / MAX ( 1.E-7 , WNMEAN ) )**2
    !
    ! 4.  Estimate peak frequency from FPI ------------------------------- *
    !
    FPISTR = MAX ( 0.008 , FPI * UST / GRAV )
    FP     = ( 3.6E-4 + 0.92*FPISTR - 6.3E-10/FPISTR**3 )/UST*GRAV
    FP     = FP * TPIINV
    !
    RETURN
    !/
    !/ End of W3SPR2 ----------------------------------------------------- /
    !/
  END SUBROUTINE W3SPR2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Calculate input source term.
  !>
  !> @param[in]  A    Action density spectrum (1-D).
  !> @param[in]  CG   Group velocities for k-axis of spectrum.
  !> @param[in]  K    Wavenumber for entire spectrum (1-D).
  !> @param[in]  U    Wind speed at reference height.
  !> @param[in]  UDIR Direction of U.
  !> @param[in]  CD   Drag coefficient at wind level ZWIND.
  !> @param[in]  Z0   Corresponding z0.
  !> @param[out] FPI  Input 'peak' frequency.
  !> @param[out] S    Source term (1-D version).
  !> @param[out] D    Diagonal term of derivative (1-D version).
  !>
  !> @author H. L. Tolman
  !> @author D. Chalikov
  !> @date   21-Feb-2004
  !>
  SUBROUTINE W3SIN2 ( A, CG, K, U, UDIR, CD, Z0, FPI, S, D )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |            D.Chalikov             |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         21-Feb-2004 |
    !/                  +-----------------------------------+
    !/
    !/    14-Jan-1997 : Final FORTRAN 77                    ( version 1.18 )
    !/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/    21-Feb-2004 : Multiple model version.             ( version 3.06 )
    !/
    !  1. Purpose :
    !
    !     Calculate input source term.
    !
    !  2. Method :
    !
    !     Tolman and Chalikov (1996), see manual.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.  I   Action density spectrum (1-D).
    !       CG      R.A.  I   Group velocities for k-axis of spectrum.
    !       K       R.A.  I   Wavenumber for entire spectrum (1-D).
    !       U       Real  I   Wind speed at reference height.
    !       UDIR    Real  I   Direction of U.
    !       CD      Real  I   Drag coefficient at wind level ZWIND.
    !       Z0      Real  I   Corresponding z0.
    !       FPI     R.A.  O   Input 'peak' frequency.
    !       S       R.A.  O   Source term (1-D version).
    !       D       R.A.  O   Diagonal term of derivative (1-D version).
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !      PRT2DS    Subr. W3ARRYMD Print plot of spectra.
    !      OUTMAT    Subr. W3WRRYMD Print out 2D matrix.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3SRCE    Subr. W3SRCEMD Source term integration.
    !      W3EXPO    Subr.   N/A    Point output post-processor.
    !      GXEXPO    Subr.   N/A    GrADS point output post-processor.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !     - Actual height of wind speed does not need to be 10 m, but is
    !       given by ZWIND.
    !     - Abs(cos) > 0.0087 to asure continuity in beta. Corresponds
    !       to shift of up to half a degree.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !       !/S      Enable subroutine tracing.
    !       !/T      Enable general test output.
    !       !/T0     Print arrays.
    !       !/T1     Calculation of diagonal within spectrum
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, XFR, DDEN, SIG, SIG2,       &
         ESIN, ECOS, FTE, FTTR, FPIMIN, ZWIND,       &
         FACTI1, FACTI2, FSWELL
#ifdef W3_T
    USE W3ODATMD, ONLY: NDST
#endif
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
#ifdef W3_T0
    USE W3ARRYMD, ONLY: PRT2DS
#endif
#ifdef W3_T1
    USE W3ARRYMD, ONLY: OUTMAT
#endif
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)        :: A(NSPEC), CG(NK), K(NSPEC), U, UDIR, &
         CD, Z0
    REAL, INTENT(OUT)       :: S(NSPEC), D(NSPEC), FPI
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IS, IK, IOMA, ICL, NKFILT, NKFIL2
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T0
    INTEGER       ITH
#endif
    REAL                    :: COSU, SINU, COSFAC, LAMBDA, ULAM,    &
         CLAM, OMA, M0, M1, RD1, RD2, BETA,   &
         FACLN1, FACLN2, USTAR, TRANS, FPISTR,&
         FP1STR, FP1, SIN1A(NK)
    REAL, PARAMETER         :: TRANSF = 0.75
    REAL, PARAMETER         :: PEAKFC = 0.8
#ifdef W3_T0
    REAL                    :: DOUT(NK,NTH)
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3SIN2')
#endif
    !
#ifdef W3_T
    WRITE (NDST,9000) DSIGA, DDRAG, U, UDIR*RADE, CD, Z0
#endif
    !
    ! 1.  Preparations
    !
    COSU   = COS(UDIR)
    SINU   = SIN(UDIR)
    !
    ! 2.  Loop over spectrum
    !
#ifdef W3_T2
    WRITE (NDST,9020)
#endif
    !
    FACLN1 = U / LOG(ZWIND/Z0)
    FACLN2 = LOG(Z0)
    !
    DO IS=1, NSPEC
      COSFAC = ECOS(IS)*COSU + ESIN(IS)*SINU
      COSFAC = SIGN ( MAX ( 0.0087 , ABS(COSFAC) ) , COSFAC )
      LAMBDA = TPI / ( K(IS) * ABS(COSFAC) )
      ULAM   = FACLN1 * ( LOG(LAMBDA) - FACLN2 )
      CLAM   = CD * ( U / ULAM )**2
      OMA    = K(IS) * ULAM * COSFAC / SIG2(IS)
      IOMA   = INT ( OMA/DSIGA ) +                                  &
           MIN ( 0 , INT ( SIGN ( -1.1 , OMA ) ) )
      ICL    = INT ( CLAM/DDRAG )
      RD1    = OMA/DSIGA - REAL(IOMA)
      RD2    = CLAM/DDRAG - REAL(ICL)
      IOMA   = MAX ( -NRSIGA , MIN ( NRSIGA , IOMA ) )
      ICL    = MAX ( 1 , MIN ( NRDRAG , ICL ) )
      BETA   = (1.-RD1) * (1.-RD2) * BETATB( IOMA , ICL )           &
           +    RD1   * (1.-RD2) * BETATB(IOMA+1, ICL )           &
           + (1.-RD1) *    RD2   * BETATB( IOMA ,ICL+1)           &
           +    RD1   *    RD2   * BETATB(IOMA+1,ICL+1)
      D(IS)  = BETA * SIG2(IS)
      S(IS)  = A(IS) * D(IS)
#ifdef W3_T2
      WRITE (NDST,9021) IS, COSFAC, LAMBDA, ULAM, CLAM*1.E3,    &
           OMA, BETA*1.E4
#endif
    END DO
    !
    ! 3.  Calculate FPI
    !
    DO IK=1, NK
      SIN1A(IK) = 0.
      DO IS=(IK-1)*NTH+1, IK*NTH
        SIN1A(IK) = SIN1A(IK) + MAX ( 0. , S(IS) )
      END DO
    END DO
    !
    M0     = 0.
    M1     = 0.
    DO IK=1, NK
      SIN1A(IK) = SIN1A(IK) * DDEN(IK) / ( CG(IK) * SIG(IK)**3 )
      M0        = M0 + SIN1A(IK)
      M1        = M1 + SIN1A(IK)/SIG(IK)
    END DO
    !
    SIN1A(NK) = SIN1A(NK) / DDEN(NK)
    M0        = M0 + SIN1A(NK) * FTE
    M1        = M1 + SIN1A(NK) * FTTR
    IF ( M1 .LT. 1E-20 ) THEN
      FPI    = XFR * SIG(NK)
    ELSE
      FPI    = M0 / M1
    END IF
    !
    ! 4.  Filter for swell
    !
    USTAR  = U * SQRT(CD)
    FPISTR = MAX ( FPIMIN , FPI * USTAR / GRAV )
    FP1STR = 3.6E-4 + 0.92*FPISTR - 6.3E-10/FPISTR**3
    FP1    = PEAKFC * FP1STR * GRAV / USTAR
    !
    NKFILT = MIN ( NK , INT(FACTI2+FACTI1*LOG(FP1)) )
    NKFIL2 = MIN ( NK , INT(FACTI2+FACTI1*LOG(TRANSF*FP1)) )
    NKFIL2 = MAX ( 0 , NKFIL2 )
    !
    DO IS=1, NKFIL2*NTH
      D(IS)  = MAX ( D(IS) , FSWELL*D(IS) )
      S(IS)  = A(IS) * D(IS)
    END DO
    !
    DO IK=NKFIL2+1, NKFILT
      TRANS  = ( SIG(IK)/FP1 - TRANSF ) / (1.-TRANSF)
      DO IS=(IK-1)*NTH+1, IK*NTH
        D(IS)  = (1.-TRANS)*MAX(D(IS),FSWELL*D(IS)) + TRANS*D(IS)
        S(IS)  = A(IS) * D(IS)
      END DO
    END DO
    !
    ! ... Test output of arrays
    !
#ifdef W3_T0
    DO IK=1, NK
      DO ITH=1, NTH
        DOUT(IK,ITH) = D(ITH+(IK-1)*NTH)
      END DO
    END DO
    CALL PRT2DS (NDST, NK, NK, NTH, DOUT, SIG(1:), '  ', 1.,    &
         0.0, 0.001, 'Diag Sin', ' ', 'NONAME')
#endif
    !
#ifdef W3_T1
    CALL OUTMAT (NDST, D, NTH, NTH, NK, 'diag Sin')
#endif
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT (' TEST W3SIN2 : DSIGA,DDRAG,U,UDIR,CD,Z0(IN) : '/    &
         '              ',F8.4,F9.6,F7.2,F6.1,F8.5,F8.5)
#endif
    !
#ifdef W3_T2
9020 FORMAT (' TEST W3SIN2 : IS, COS, LAMBDA, ULAM, CLAM*1E3, ', &
         'OMA, BETA*1E4')
9021 FORMAT (6X,I6,F7.2,1X,F6.1,2(1X,F5.2),2(1X,F6.2))
#endif
    !/
    !/ End of W3SIN2 ----------------------------------------------------- /
    !/
  END SUBROUTINE W3SIN2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Calculate whitecapping source term and diagonal term of derivative.
  !>
  !> @param[in]  A     Input action density spectrum.
  !> @param[in]  CG    Group velocity array.
  !> @param[in]  K     Wavenumber array.
  !> @param[in]  FPI   'Peak frequency' of input (rad/s).
  !> @param[in]  USTAR Friction velocity (m/s).
  !> @param[in]  ALFA  Phillips' constant.
  !> @param[out] S     Source term (1-D version).
  !> @param[out] D     Diagonal term of derivative (1-D version).
  !>
  !> @author H. L. Tolman  @date 21-Feb-2004
  !>
  SUBROUTINE W3SDS2 (A, CG, K, FPI, USTAR, ALFA, S, D)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         21-Feb-2004 |
    !/                  +-----------------------------------+
    !/
    !/    12-Jun-1996 : Final FORTRAN 77                    ( version 1.18 )
    !/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/    23-Apr-2002 : Erick Rogers' fix                   ( version 2.19 )
    !/    21-Feb-2004 : Multiple model version.             ( version 3.06 )
    !/
    !  1. Purpose :
    !
    !     Calculate whitecapping source term and diagonal term of der.
    !
    !  2. Method :
    !
    !     Tolman and Chalikov (1995).
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.   I   Input action density spectrum.
    !       CG      R.A.   I   Group velocity array.
    !       K       R.A.   I   Wavenumber array.
    !       FPI     Real   I   'Peak frequency' of input (rad/s).
    !       USTAR   Real   I   Friction velocity (m/s).
    !       ALFA    R.A.   I   Phillips' constant.
    !       S       R.A.   O   Source term (1-D version).
    !       D       R.A.   O   Diagonal term of derivative (1-D version).
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !      PRT2DS    Subr. W3ARRYMD Print plot of spectra.
    !      OUTMAT    Subr. W3WRRYMD Print out 2D matrix.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3SRCE    Subr. W3SRCEMD Source term integration.
    !      W3EXPO    Subr.   N/A    Point output post-processor.
    !      GXEXPO    Subr.   N/A    GrADS point output post-processor.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/S     Enable subroutine tracing.
    !     !/T     Enable general test output.
    !     !/T0    Print arrays.
    !     !/T1    Print filter and constituents.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD, ONLY: NK, NTH, SIG, DDEN, DTH, FTE, FPIMIN,       &
         FACTI1, FACTI2, XF1, XF2, XFH, SDSALN,      &
         CDSA0, CDSA1, CDSA2, CDSB0, CDSB1, CDSB2,   &
         CDSB3
#ifdef W3_T
    USE W3ODATMD, ONLY: NDST
#endif
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
#ifdef W3_T0
    USE W3ARRYMD, ONLY: PRT2DS
#endif
#ifdef W3_T1
    USE W3ARRYMD, ONLY: OUTMAT
#endif
    !
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)        :: A(NTH,NK), CG(NK), K(NK), FPI,       &
         USTAR, ALFA(NK)
    REAL, INTENT(OUT)       :: S(NTH,NK), D(NTH,NK)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IK, ITH, IKHW
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: FHW, XHW, FPIT, PHI, AF1, AF2,       &
         AFILT, BFILT, CDIST, FILT, POW,      &
         CDISH, CDISP, HW, EHIGH, EBD(NK)
#ifdef W3_T
    REAL          POWMAX
#endif
#ifdef W3_T0
    REAL             DOUT(NK,NTH)
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3SDS2')
#endif
    !
#ifdef W3_T
    WRITE (NDST,9000) FPI, USTAR
#endif
    !
    ! 1.  Preparations
    ! 1.a HW
    !
    FHW    = XFH*FPI
    XHW    = FACTI2 + FACTI1*LOG(FHW)
    IKHW   = MIN ( NK , INT ( XHW + 0.5 ) )
    DO IK=IKHW, NK
      EBD(IK) = 0.
      DO ITH=1, NTH
        EBD(IK) = EBD(IK) + A(ITH,IK)
      END DO
    END DO
    !
    IF ( FHW .LT. SIG(NK+1) ) THEN
      XHW    = 1. - MOD ( XHW + 0.5 , 1. )
      IF ( IKHW .EQ. NK ) XHW = MAX ( 0. , XHW - 0.5 )
      HW     = XHW * EBD(IKHW)*DDEN(IKHW)/CG(IKHW)
      DO IK=IKHW+1, NK
        HW     = HW + EBD(IK)*DDEN(IK)/CG(IK)
      END DO
      HW     = 4. * SQRT ( HW + EBD(NK)/CG(NK)*FTE )
    ELSE
      EHIGH  = EBD(NK)/CG(NK) * SIG(NK)*DTH * (SIG(NK)/FHW)**5
      HW     = 4. * SQRT ( 0.25 * FHW * EHIGH )
    END IF
    !
    ! 1.b PHI
    !
    FPIT   = MAX ( FPIMIN , FPI*TPIINV*USTAR/GRAV )
    PHI    = CDSB0 + CDSB1*FPIT + CDSB2/FPIT**CDSB3
    !
    ! 1.c Set-up filter
    !
    AF2    = XF2*FPI
    AF1    = XF1*FPI
    BFILT  = 1. / ( AF2 - AF1 )
    AFILT  = - BFILT * AF1
    !
    ! 1.d Constants
    !
    CDIST = - 2. * USTAR * HW * PHI
    CDISH = G2PI3I * USTAR**2
    CDISP = G1PI1I * USTAR
    !
    ! 2.  Combined diagonal factor
    !
#ifdef W3_T2
    WRITE (NDST,9020)
#endif
#ifdef W3_T
    POWMAX = 0.
#endif
    DO IK=1, NK
      FILT    = MIN ( 1., MAX ( 0. , AFILT + BFILT*SIG(IK) ))
      POW     = MIN ( 25. , CDSA1 / ( CDISP*SIG(IK) )**CDSA2 )
      IF ( FILT .GT. 0. ) THEN
        D(1,IK) = (1.-FILT)  * CDIST * K(IK)**2                   &
             - FILT * CDSA0 * CDISH * SIG(IK)**3             &
             * (ALFA(IK)/SDSALN)**POW
      ELSE
        D(1,IK) = (1.-FILT)  * CDIST * K(IK)**2
      END IF
#ifdef W3_T
      POWMAX = MAX(POW*FILT,POWMAX)
#endif
#ifdef W3_T2
      WRITE (NDST,9021) IK, FILT, ALFA(IK)/SDSALN,              &
           CDIST*PHI*K(IK)**2, CDSA0*CDISH*SIG(IK)**3         &
           * (ALFA(IK)/SDSALN)**POW, D(1,IK)
#endif
    END DO
    !
#ifdef W3_T
    WRITE (NDST,9010) AF1, AF2, AFILT, BFILT, POWMAX
#endif
    !
    ! 3.  2-D diagonal array
    !
    DO IK=1, NK
      DO ITH=2, NTH
        D(ITH,IK) = D(1,IK)
      END DO
    END DO
    !
    S = D * A
    !
    ! ... Test output of arrays
    !
#ifdef W3_T0
    DO IK=1, NK
      DO ITH=1, NTH
        DOUT(IK,ITH) = D(ITH,IK)
      END DO
    END DO
    CALL PRT2DS (NDST, NK, NK, NTH, DOUT, SIG(1:), '  ', 1.,    &
         0.0, 0.001, 'Diag Sds', ' ', 'NONAME')
#endif
    !
#ifdef W3_T1
    CALL OUTMAT (NDST, D, NTH, NTH, NK, 'diag Sds')
#endif
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT (' TEST W3SDS2 : FPI, USTAR           : ',2F8.3)
9010 FORMAT (' TEST W3SDS2 : AF1-2, A-BFILT, PMAX : ',4F7.3,E10.3)
#endif
#ifdef W3_T2
9020 FORMAT (' TEST W3SDS2 : IK, FILT, ALFA, DDST, DDSH, DDS')
9021 FORMAT ('           ',I6,2F7.3,3E11.3)
#endif
    !/
    !/ End of W3SDS2 ----------------------------------------------------- /
    !/
  END SUBROUTINE W3SDS2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Generate an interpolation table for the air-sea interaction
  !>  parameter of Chalikov and Belevich (1993).
  !>
  !> @details The size of the table is set in parameter statements,
  !>  the range is set by the input parameters of this routine. The first
  !>  counter of the table corresponds to the nondimensional frequency
  !>
  !> @verbatim
  !>                  SIGMA Ul
  !>        SIGA  =  ----------  COS ( THETA - THETA     )           (1)
  !>                     g                          wind
  !> @endverbatim
  !>
  !>  The second counter of the table represents the drag coefficient.
  !>  The maximum values of both parameters are passed to the routine
  !>  through the parameter list.
  !>
  !> @author H. L. Tolman  @date 21-Feb-2004
  !>
  SUBROUTINE INPTAB
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         21-Feb-2004 |
    !/                  +-----------------------------------+
    !/
    !/    03-Jun-1996 : Final version 1.18 / FORTRAN 77 version.
    !/    06-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/    21-Feb-2004 : Multiple model version.             ( version 3.06 )
    !/
    !  1. Purpose :
    !
    !     Generate an interpolation table for the air-sea interaction
    !     parameter of Chalikov and Belevich (1993).
    !
    !  2. Method :
    !
    !     The size of the table is set in parameter statements, the range
    !     is set by the input parameters of this routine. The first counter
    !     of the table corresponds to the nondimensional frequency
    !
    !                  SIGMA Ul
    !        SIGA  =  ----------  COS ( THETA - THETA     )           (1)
    !                     g                          wind
    !
    !     The second counter of the table represents the drag coefficient.
    !     The maximum values of both parameters are passed to the routine
    !     through the parameter list.
    !
    !  3. Parameters :
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !      W3BETA    Func. Internal Function to calculate the
    !                               interaction parameter.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3IOGR    Subr. W3IOGRMD Model definition IO routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/S   Enable subroutine tracing.
    !     !/T   Enable test output.
    !     !/T0  Print table.
    !     !/T1  Estimate maximum errors.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3ODATMD, ONLY: NDST
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: ISIGA, IDRAG
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T0
    INTEGER                 :: I1
#endif
#ifdef W3_T1
    INTEGER                 :: IE1
#endif
    REAL                    :: SIGA, DRAG
#ifdef W3_T0
    REAL                    :: BMIN, BMAX
#endif
#ifdef W3_T1
    REAL                    :: ENORM, ERR(NRDRAG)
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'INPTAB')
#endif
    !
    ! 1.  Determine range and increments of table ------------------------ *

    !
    DSIGA  = SIGAMX / REAL(NRSIGA)
    DDRAG  = DRAGMX / REAL(NRDRAG)
    !
#ifdef W3_T
    WRITE (NDST,9000) SIGAMX, DSIGA, DRAGMX, DDRAG
#endif
    !
    ! 2.  Fill table ----------------------------------------------------- *
    !
    DO ISIGA=-NRSIGA,NRSIGA+1
      SIGA   = REAL(ISIGA) * DSIGA
      DO IDRAG=1, NRDRAG+1
        DRAG   = REAL(IDRAG) * DDRAG
        BETATB(ISIGA,IDRAG) = W3BETA ( SIGA, DRAG , NDST )
      END DO
    END DO
    !
    ! 3.  Test output ---------------------------------------------------- *
    !
#ifdef W3_T0
    WRITE (NDST,9010)
    I1     = MIN (35,NRDRAG)
    DO ISIGA=-NRSIGA,NRSIGA
      SIGA   = REAL(ISIGA) * DSIGA
      BMIN   = 0.
      BMAX   = 0.
      DO IDRAG=1, NRDRAG
        BMIN   = MIN ( BMIN , BETATB(ISIGA,IDRAG) )
        BMAX   = MAX ( BMAX , BETATB(ISIGA,IDRAG) )
      END DO
      BMAX   = MAX ( BMAX , -BMIN )
      WRITE (NDST,9011) ISIGA, SIGA, BMAX,                      &
           (NINT(BETATB(ISIGA,IDRAG)/BMAX*100.),IDRAG=1,I1)
      IF (I1.LT.NRDRAG) WRITE (NDST,9012)                       &
           (NINT(BETATB(ISIGA,IDRAG)/BMAX*100.),IDRAG=I1+1,NRDRAG)
    END DO
#endif
    !
#ifdef W3_T1
    WRITE (NDST,9020)
    IE1    = MIN (30,NRDRAG-1)
    ENORM  = 1000. / ABS(BETATB(0,NRDRAG))
    DO ISIGA=-NRSIGA,NRSIGA
      SIGA   = REAL(ISIGA) * DSIGA
      IF ( ABS(SIGA) .LT. 5.01 ) THEN
        DO IDRAG=1, NRDRAG-1
          DRAG   = ( REAL(IDRAG) + 0.5 ) * DDRAG
          ERR(IDRAG) =  - W3BETA (SIGA,DRAG,NDST) + 0.5 *     &
               ( BETATB(ISIGA,IDRAG) + BETATB(ISIGA,IDRAG+1) )
        END DO
        WRITE (NDST,9021) ISIGA, SIGA,                        &
             (NINT(ENORM*ERR(IDRAG)),IDRAG=1,IE1)
        IF (IE1.LT.NRDRAG-1) WRITE (NDST,9022)                &
             (NINT(ENORM*ERR(IDRAG)),IDRAG=IE1+1,NRDRAG-1)
      ENDIF
    END DO
    !
    WRITE (NDST,9030)
    IE1    = MIN (30,NRDRAG)
    ENORM  = 1000. / ABS(BETATB(0,NRDRAG))
    DO ISIGA=-NRSIGA,NRSIGA-1
      SIGA   = ( REAL(ISIGA) + 0.5 ) * DSIGA
      IF ( ABS(SIGA) .LT. 5.01 ) THEN
        DO IDRAG=1, NRDRAG
          DRAG   = REAL(IDRAG) * DDRAG
          ERR(IDRAG) =  - W3BETA (SIGA,DRAG,NDST) + 0.5 *     &
               ( BETATB(ISIGA,IDRAG) + BETATB(ISIGA+1,IDRAG) )
        END DO
        WRITE (NDST,9031) ISIGA, SIGA,                        &
             (NINT(ENORM*ERR(IDRAG)),IDRAG=1,IE1)
        IF (IE1.LT.NRDRAG) WRITE (NDST,9032)                  &
             (NINT(ENORM*ERR(IDRAG)),IDRAG=IE1+1,NRDRAG)
      ENDIF
    END DO
#endif
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT ( ' TEST INPTAB : SIGAMX, DSIGA : ',F6.2,F8.2/        &
         '               DRAGMX, DDRAG : ',F8.4,F9.5)
#endif
    !
#ifdef W3_T0
9010 FORMAT (/' TEST INPTAB : TABLE, NORMALIZED WITH ',          &
         'BETATB(ISIGA,NRDRAG)'/                            &
         '               ISIGA, SIGA, BETA_MAX, TABLE (x100)')
9011 FORMAT (1X,I4,F7.2,F6.4,1X,35I3)
9012 FORMAT (19X,35I3)
#endif
    !
#ifdef W3_T1
9020 FORMAT (/' TEST INPTAB : ERROR DUE TO DRAG, NORMALIZED ',   &
         'WITH BETATB(ISIGA,NRDRAG)'/                       &
         '               ISIGA, SIGA, TABLE (x1000)')
9021 FORMAT (1X,I4,F7.2,35I3)
9022 FORMAT (12X,35I3)
9030 FORMAT (/' TEST INPTAB : ERROR DUE TO SIGA, NORMALIZED WITH ',  &
         'BETATB(ISIGA,NRDRAG)'/                            &
         '               ISIGA, SIGA, TABLE (x1000)')
9031 FORMAT (1X,I4,F7.2,35I3)
9032 FORMAT (12X,35I3)
#endif
    !/
    !/    Internal function W3BETA
    !/
  CONTAINS
    !/ ------------------------------------------------------------------- /
    !>
    !> @brief Calculate wind-wave interaction parameter beta.
    !>
    !> @param   OMA    Non-dimensional apparent frequency.
    !> @param   CL     Drag coefficient at height l.
    !> @param   NDST
    !> @returns W3BETA Wind-wave interaction parameter multiplied
    !>          by density ratio.
    !>
    !> @author  H. L. Tolman
    !> @author  D. Chalikov
    !> @date    21-Feb-2004
    !>
    REAL FUNCTION W3BETA ( OMA , CL , NDST )
      !/
      !/                  +-----------------------------------+
      !/                  | WAVEWATCH III           NOAA/NCEP |
      !/                  |           H. L. Tolman            |
      !/                  |            D.Chalikov             |
      !/                  |                        FORTRAN 90 |
      !/                  | Last update :         21-Feb-2004 |
      !/                  +-----------------------------------+
      !/
      !/    06-Dec-1996 : Final version 1.18 / FORTRAN 77 version.
      !/    06-Dec-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
      !/    21-Feb-2004 : Multiple model version.             ( version 3.06 )
      !/
      !  1. Purpose :
      !
      !     Calculate wind-wave interaction parameter beta.
      !
      !  2. Method :
      !
      !     Chalikov and Belevich (1992), see also manual.
      !
      !  3. Parameters :
      !
      !     Parameter list
      !     ----------------------------------------------------------------
      !       W3BETA  Real  O   Wind-wave interaction parameter multiplied
      !                         by density ratio.
      !       OMA     Real  I   Non-dimensional apparent frequency.
      !
      !                         OMA = OMEGA | U | cos(theta-theta ) / g
      !                                        l                 w
      !
      !       CL      Real  I   Drag coefficient at height l
      !     ----------------------------------------------------------------
      !
      !  4. Subroutines used :
      !
      !  5. Called by :
      !
      !  6. Error messages :
      !
      !  7. Remarks :
      !
      !  8. Structure :
      !
      !     See source code.
      !
      !  9. Switches :
      !
      !     !/S   Enable subroutine tracing.
      !     !/T0  Enable test output.
      !
      ! 10. Source code :
      !
      !/ ------------------------------------------------------------------- /
      IMPLICIT NONE
      !/
      !/ ------------------------------------------------------------------- /
      !/ Parameter list
      !/
      INTEGER, INTENT(IN)     :: NDST
      REAL, INTENT(IN)        :: OMA, CL
      !/
      !/ ------------------------------------------------------------------- /
      !/ Local parameters
      !/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      REAL                    :: OM1, OM2, A0, A1, A2, A3, A4, A5,    &
           A6, A7, A8, A9, A10
      !/
      !/ ------------------------------------------------------------------- /
      !/
#ifdef W3_S
      CALL STRACE (IENT, 'W3BETA')
#endif
      !
#ifdef W3_T0
      WRITE (NDST,9000) OMA, CL
#endif
      !
      ! calculate Omegas
      !
      OM1    =  1.075 +  75.*CL
      OM2    =  1.2   + 300.*CL
      !
      ! calculate factors a
      !
      A1     =  0.25  + 395.*CL
      A2     =  0.35  + 150.*CL
      A4     =  0.3   + 300.*CL
      A9     =  0.35  + 240.*CL
      A10    = -0.06  + 470.*CL
      !
      A5     =  A4 * OM1
      A0     =  0.25 * A5**2 / A4
      A3     = (A0-A2-A1) / (A0+A4+A5)
      A6     =  A0 * (1.-A3)
      A7     = (A9*(OM2-1)**2+A10) / (OM2-OM1)
      A8     =  A7 * OM1
      !
#ifdef W3_T0
      WRITE (NDST,9001) OM1, OM2
      WRITE (NDST,9002) A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10
#endif
      !
      ! calculate beta * 1.e4
      !
      IF  ( OMA .LT. -1. ) THEN
        W3BETA = -A1 * OMA**2 - A2
      ELSE IF (OMA .LT. OM1/2.) THEN
        W3BETA =  A3 * OMA * ( A4 * OMA - A5 ) - A6
      ELSE IF (OMA .LT. OM1) THEN
        W3BETA =       OMA * ( A4 * OMA - A5 )
      ELSE IF (OMA .LT. OM2) THEN
        W3BETA = A7 * OMA - A8
      ELSE
        W3BETA = A9 * (OMA-1.)**2 + A10
      END IF
      !
      ! beta * dwat / dair
      !
      W3BETA = W3BETA * 1.E-4
#ifdef W3_T0
      WRITE (NDST,9003) W3BETA
#endif
      !
      RETURN
      !
      ! Formats
      !
#ifdef W3_T0
9000  FORMAT ( ' TEST W3BETA : INPUT : ',2E10.3)
9001  FORMAT ( ' TEST W3BETA : OM1-2 : ',2E10.3)
9002  FORMAT ( ' TEST W3BETA : A0-10 : ',5E10.3/                   &
           '             ',6E10.3)
9003  FORMAT ( ' TEST W3BETA : BETA  : ',E10.3)
#endif
      !/
      !/ End of W3BETA ----------------------------------------------------- /
      !/
    END FUNCTION W3BETA
    !/
    !/ End of INPTAB ----------------------------------------------------- /
    !/
  END SUBROUTINE INPTAB
  !/
  !/ End of module W3SRC2MD -------------------------------------------- /
  !/
END MODULE W3SRC2MD
