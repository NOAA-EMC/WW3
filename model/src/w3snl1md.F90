!> @file
!> @brief Bundles routines calculate nonlinear wave-wave interactions
!>  according to the Discrete Interaction Approximation (DIA).
!>
!> @author H. L. Tolman
!> @date   03-Sep-2012
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Bundles routines to calculate nonlinear wave-wave interactions
!>  according to the Discrete Interaction Approximation (DIA) of
!>  Hasselmann et al. (JPO, 1985).
!>
!> @author H. L. Tolman
!> @date   03-Sep-2012
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>

MODULE W3SNL1MD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         28-Feb-2023 |
  !/                  +-----------------------------------+
  !/
  !/    04-Feb-2000 : Origination.                        ( version 2.00 )
  !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
  !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
  !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  !/    03-Sep-2012 : Clean up of test output T0, T1      ( version 4.07 )
  !/    28-Feb-2023 : Adds GQM separate routines          ( version 7.07 )
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Bundles routines calculate nonlinear wave-wave interactions
  !     according to the Discrete Interaction Approximation (DIA) of
  !     Hasselmann et al. (JPO, 1985).
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
  !      W3SNL1    Subr. Public   Calculate interactions.
  !      INSNL1    Subr. Public   Initialization routine.
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
  !       !/S      Enable subroutine tracing.
  !       !/T(n)   Test output, see subroutines.
  !
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  !/
  PUBLIC
  !/
  !/ These are the arrays and variables used for GQM method
  !/
  INTEGER              :: NCONF
  INTEGER, ALLOCATABLE :: K_IF2 (:,:,:) , K_IF3 (:,:,:) , K_1P2P(:,:,:) , &
       K_1P3M(:,:,:) , K_1P2M(:,:,:) , K_1P3P(:,:,:) , &
       K_1M2P(:,:,:) , K_1M3M(:,:,:) , K_1M2M(:,:,:) , &
       K_1M3P(:,:,:)
  INTEGER, ALLOCATABLE :: F_POIN(:) , T_POIN(:) , K_IF1(:) , K_1P(:,:) ,  &
       K_1M(:,:) , IDCONF(:,:)
  DOUBLE PRECISION, ALLOCATABLE :: F_COEF(:) , F_PROJ(:) , TB_SCA(:) , TB_V14(:)
  DOUBLE PRECISION, ALLOCATABLE :: TB_V24(:,:,:) , TB_V34(:,:,:) ,        &
       TB_TPM(:,:,:) , TB_TMP(:,:,:) , TB_FAC(:,:,:)
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /

!>
!> @brief Calculate nonlinear interactions and the diagonal term of
!>  its derivative.
!>
!> @param[in] A       Action spectrum A(ISP) as a function of
!>                    direction (rad) and wavenumber.
!> @param[in] CG      Group velocities (dimension NK).
!> @param[in] KDMEAN  Mean relative depth.
!> @param[out] S      Source term.
!> @param[out] D      Diagonal term of derivative.
!>
!> @author H. L. Tolman
!> @date   06-Jun-2018
!>
  SUBROUTINE W3SNL1 (A, CG, KDMEAN, S, D)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         06-Jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/    12-Jun-1996 : Final FORTRAN 77                    ( version 1.18 )
    !/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
    !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
    !/    03-Sep-2012 : Clean up of test output T0, T1      ( version 4.07 )
    !/    06-Jun-2018 : Add optional DEBUGSRC               ( version 6.04 )
    !/
    !  1. Purpose :
    !
    !     Calculate nonlinear interactions and the diagonal term of
    !     its derivative.
    !
    !  2. Method :
    !
    !     Discrete interaction approximation. (Hasselmann and Hasselmann
    !     1985; WAMDI group 1988)
    !
    !     The DIA is applied to the energy spectrum (instead of the action
    !     spectrum), for which is was originally developped. Because the
    !     frequency grid is invariant, the nonlinear interactions are
    !     calculated for the frequency spectrum, as in WAM. This requires
    !     only a single set of interpolation data which can be applied
    !     throughout the spatial domain. For deep water this is idenitical
    !     to a direct application to the wavenumber spectrum, for shallow
    !     water it is not. As the shallow water correction is nothing but
    !     a crude approximation, the choice between spectra is expected to
    !     be irrelevant.
    !
    !     The nonlinear interactions are calculated for two "mirror image"
    !     quadruplets as described in the manual. The central bin of these
    !     quadruples is placed on the discrete complonents of the spectrum,
    !     which requires interpolation to obtain other eneregy densities.
    !     The figure below defines the diferent basic counters and weights
    !     necessary for this interpolation.
    !
    !
    !               IFRM1  IFRM
    !                5        7    T |
    !          ITHM1  +------+     H +
    !                 |      |     E |      IFRP      IFRP1
    !                 |   \  |     T |       3           1
    !           ITHM  +------+     A +        +---------+  ITHP1
    !                6       \8      |        |         |
    !                                |        |  /      |
    !                           \    +        +---------+  ITHP
    !                                |      /4           2
    !                              \ |  /
    !          -+-----+------+-------#--------+---------+----------+
    !                              / |  \        FREQ.
    !                                |      \4           2
    !                           /    +        +---------+  ITHP
    !                                |        |  \      |
    !                6       /8      |        |         |
    !           ITHM  +------+       +        +---------+  ITHP1
    !                 |   \  |       |       3           1
    !                 |      |       |      IFRP      IFRP1
    !          ITHM1  +------+       +
    !                5        7      |
    !
    !     To create long vector loops and to efficiently deal with the
    !     closed nature of the directional space, the relative counters
    !     above are replaced by complete addresses stored in 32 arrays
    !     (see section 3 and INSNL1). The interaction are furthermore
    !     calucated for an extended spectrum, making it unnecessary to
    !     introduce extra weight factors for low and high frequencies.
    !     Therefore low and high frequencies are added to the local
    !     (auxiliary) spectrum as illustraed below.
    !
    !
    !              ^  +---+---------------------+---------+- NTH
    !              |  |   :                     :         |
    !                 |   :                     :         |
    !              d  | 2 :  original spectrum  :    1    |
    !              i  |   :                     :         |
    !              r  |   :                     :         |
    !                 +---+---------------------+---------+-  1
    !                            Frequencies -->     ^
    !         IFR =   0   1                    NFR   |  NFRHGH
    !                                                |
    !                                             NFRCHG
    !
    !     where : 1 : Extra tail added beyond NFR
    !             2 : Empty bins at low frequencies
    !
    !             NFRHGH = NFR + IFRP1 - IFRM1
    !             NFRCHG = NFR - IFRM1
    !
    !     All counters and arrays are set in INSNL1. See also section 3
    !     and section 8.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.  I   Action spectrum A(ISP) as a function of
    !                         direction (rad)  and wavenumber.
    !       CG      R.A.  I   Group velocities (dimension NK).
    !       KDMEAN  Real  I   Mean relative depth.
    !       S       R.A.  O   Source term.                           *)
    !       D       R.A.  O   Diagonal term of derivative.           *)
    !     ----------------------------------------------------------------
    !                             *) 1-D array with dimension NTH*NK
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
    !       None.
    !
    !  7. Remarks :
    !
    !       None.
    !
    !  8. Structure :
    !
    !     -------------------------------------------
    !      1.  Calculate proportionality constant.
    !      2.  Prepare auxiliary spectrum
    !      3.  Calculate (unfolded) interactions
    !        a Energy at interacting bins
    !        b Contribution to interactions
    !        c Fold interactions to side angles
    !      4.  Put source and diagonal term together
    !     -------------------------------------------
    !
    !  9. Switches :
    !
    !     !/S   Enable subroutine tracing.
    !     !/T   Enable general test output.
    !     !/T0  2-D print plot of source term.
    !     !/T1  Print arrays.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    USE CONSTANTS
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, FACHFE,                &
         KDCON, KDMN, SNLC1, SNLS1, SNLS2, SNLS3
    USE W3ADATMD, ONLY: NFR, NFRHGH, NFRCHG, NSPECX, NSPECY,        &
         IP11, IP12, IP13, IP14, IM11, IM12, IM13, IM14,   &
         IP21, IP22, IP23, IP24, IM21, IM22, IM23, IM24,   &
         IC11, IC12, IC21, IC22, IC31, IC32, IC41, IC42,   &
         IC51, IC52, IC61, IC62, IC71, IC72, IC81, IC82,   &
         DAL1, DAL2, DAL3, AF11,                           &
         AWG1, AWG2, AWG3, AWG4, AWG5, AWG6, AWG7, AWG8,   &
         SWG1, SWG2, SWG3, SWG4, SWG5, SWG6, SWG7, SWG8
#ifdef W3_T
    USE W3ODATMD, ONLY: NDST
#endif
#ifdef W3_T1
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
    REAL, INTENT(IN)        :: A(NSPEC), CG(NK), KDMEAN
    REAL, INTENT(OUT)       :: S(NSPEC), D(NSPEC)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: ITH, IFR, ISP
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: X, X2, CONS, CONX, FACTOR,           &
         E00, EP1, EM1, EP2, EM2,             &
         SA1A, SA1B, SA2A, SA2B
#ifdef W3_T0
    REAL                    :: SOUT(NK,NFR), DOUT(NK,NFR)
#endif
    REAL               ::  UE  (1-NTH:NSPECY), SA1 (1-NTH:NSPECX),  &
         SA2 (1-NTH:NSPECX), DA1C(1-NTH:NSPECX),  &
         DA1P(1-NTH:NSPECX), DA1M(1-NTH:NSPECX),  &
         DA2C(1-NTH:NSPECX), DA2P(1-NTH:NSPECX),  &
         DA2M(1-NTH:NSPECX), CON (      NSPEC )
    !/
    !/ ------------------------------------------------------------------- /
    !/
    ! initialisations
    !
#ifdef W3_S
    CALL STRACE (IENT, 'W3SNL1')
#endif
    !
    ! 1.  Calculate prop. constant --------------------------------------- *
    !
    X      = MAX ( KDCON*KDMEAN , KDMN )
    X2     = MAX ( -1.E15, SNLS3*X)
    CONS   = SNLC1 * ( 1. + SNLS1/X * (1.-SNLS2*X) * EXP(X2) )
    !
#ifdef W3_T
    WRITE (NDST,9000) KDMEAN, CONS
#endif
    !
    ! 2.  Prepare auxiliary spectrum and arrays -------------------------- *
    !
    DO IFR=1, NFR
      CONX = TPIINV / SIG(IFR) * CG(IFR)
      DO ITH=1, NTH
        ISP       = ITH + (IFR-1)*NTH
        UE (ISP) = A(ISP) / CONX
        CON(ISP) = CONX
      END DO
    END DO
    !
    DO IFR=NFR+1, NFRHGH
      DO ITH=1, NTH
        ISP      = ITH + (IFR-1)*NTH
        UE(ISP) = UE(ISP-NTH) * FACHFE
      END DO
    END DO
    !
    DO ISP=1-NTH, 0
      UE  (ISP) = 0.
      SA1 (ISP) = 0.
      SA2 (ISP) = 0.
      DA1C(ISP) = 0.
      DA1P(ISP) = 0.
      DA1M(ISP) = 0.
      DA2C(ISP) = 0.
      DA2P(ISP) = 0.
      DA2M(ISP) = 0.
    END DO
    !
    ! 3.  Calculate interactions for extended spectrum ------------------- *
    !
    DO ISP=1, NSPECX
      !
      ! 3.a Energy at interacting bins
      !
      E00    =        UE(ISP)
      EP1    = AWG1 * UE(IP11(ISP)) + AWG2 * UE(IP12(ISP))        &
           + AWG3 * UE(IP13(ISP)) + AWG4 * UE(IP14(ISP))
      EM1    = AWG5 * UE(IM11(ISP)) + AWG6 * UE(IM12(ISP))        &
           + AWG7 * UE(IM13(ISP)) + AWG8 * UE(IM14(ISP))
      EP2    = AWG1 * UE(IP21(ISP)) + AWG2 * UE(IP22(ISP))        &
           + AWG3 * UE(IP23(ISP)) + AWG4 * UE(IP24(ISP))
      EM2    = AWG5 * UE(IM21(ISP)) + AWG6 * UE(IM22(ISP))        &
           + AWG7 * UE(IM23(ISP)) + AWG8 * UE(IM24(ISP))
      !
      ! 3.b Contribution to interactions
      !
      FACTOR = CONS * AF11(ISP) * E00
      !
      SA1A   = E00 * ( EP1*DAL1 + EM1*DAL2 )
      SA1B   = SA1A - EP1*EM1*DAL3
      SA2A   = E00 * ( EP2*DAL1 + EM2*DAL2 )
      SA2B   = SA2A - EP2*EM2*DAL3
      !
      SA1 (ISP) = FACTOR * SA1B
      SA2 (ISP) = FACTOR * SA2B
      !
      DA1C(ISP) = CONS * AF11(ISP) * ( SA1A + SA1B )
      DA1P(ISP) = FACTOR * ( DAL1*E00 - DAL3*EM1 )
      DA1M(ISP) = FACTOR * ( DAL2*E00 - DAL3*EP1 )
      !
      DA2C(ISP) = CONS * AF11(ISP) * ( SA2A + SA2B )
      DA2P(ISP) = FACTOR * ( DAL1*E00 - DAL3*EM2 )
      DA2M(ISP) = FACTOR * ( DAL2*E00 - DAL3*EP2 )
      !
    END DO
    !
    ! 4.  Put source and diagonal term together -------------------------- *
    !
    DO ISP=1, NSPEC
      !
      S(ISP) = CON(ISP) * ( - 2. * ( SA1(ISP) + SA2(ISP) )       &
           + AWG1 * ( SA1(IC11(ISP)) + SA2(IC12(ISP)) )    &
           + AWG2 * ( SA1(IC21(ISP)) + SA2(IC22(ISP)) )    &
           + AWG3 * ( SA1(IC31(ISP)) + SA2(IC32(ISP)) )    &
           + AWG4 * ( SA1(IC41(ISP)) + SA2(IC42(ISP)) )    &
           + AWG5 * ( SA1(IC51(ISP)) + SA2(IC52(ISP)) )    &
           + AWG6 * ( SA1(IC61(ISP)) + SA2(IC62(ISP)) )    &
           + AWG7 * ( SA1(IC71(ISP)) + SA2(IC72(ISP)) )    &
           + AWG8 * ( SA1(IC81(ISP)) + SA2(IC82(ISP)) ) )
      !
      D(ISP) =  - 2. * ( DA1C(ISP) + DA2C(ISP) )                 &
           + SWG1 * ( DA1P(IC11(ISP)) + DA2P(IC12(ISP)) )     &
           + SWG2 * ( DA1P(IC21(ISP)) + DA2P(IC22(ISP)) )     &
           + SWG3 * ( DA1P(IC31(ISP)) + DA2P(IC32(ISP)) )     &
           + SWG4 * ( DA1P(IC41(ISP)) + DA2P(IC42(ISP)) )     &
           + SWG5 * ( DA1M(IC51(ISP)) + DA2M(IC52(ISP)) )     &
           + SWG6 * ( DA1M(IC61(ISP)) + DA2M(IC62(ISP)) )     &
           + SWG7 * ( DA1M(IC71(ISP)) + DA2M(IC72(ISP)) )     &
           + SWG8 * ( DA1M(IC81(ISP)) + DA2M(IC82(ISP)) )
      !
    END DO
    !
    ! ... Test output :
    !
#ifdef W3_T0
    DO IFR=1, NFR
      DO ITH=1, NTH
        ISP          = ITH + (IFR-1)*NTH
        SOUT(IFR,ITH) = S(ISP) * TPI * SIG(IFR) / CG(IFR)
        DOUT(IFR,ITH) = D(ISP)
      END DO
    END DO
    CALL PRT2DS (NDST, NK, NK, NTH, SOUT, SIG(1:), '  ', 1.,  &
         0.0, 0.001, 'Snl(f,t)', ' ', 'NONAME')
    CALL PRT2DS (NDST, NK, NK, NTH, DOUT, SIG(1:), '  ', 1.,  &
         0.0, 0.001, 'Diag Snl', ' ', 'NONAME')
#endif
    !
#ifdef W3_T1
    CALL OUTMAT (NDST, S, NTH, NTH, NK, 'Snl')
    CALL OUTMAT (NDST, D, NTH, NTH, NK, 'Diag Snl')
#endif
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT (' TEST W3SNL1 : KDMEAN, CONS :',F8.2,F8.1)
#endif
    !/
    !/ End of W3SNL1 ----------------------------------------------------- /
    !/
  END SUBROUTINE W3SNL1
!/ ------------------------------------------------------------------- /
!>
!> @brief Preprocessing for nonlinear interactions (weights).
!>
!> @param[in] IMOD  Model number.
!>
!> @author H. L. Tolman
!> @date   24-Dec-2004
!>
  SUBROUTINE INSNL1 ( IMOD )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         24-Dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    19-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
    !/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
    !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
    !/
    !  1. Purpose :
    !
    !     Preprocessing for nonlinear interactions (weights).
    !
    !  2. Method :
    !
    !     See W3SNL1.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IMOD    Int.  I   Model number.
    !     ----------------------------------------------------------------
    !
    !     Local variables
    !     ----------------------------------------------------------------
    !       ITHxn   Real  Directional indices.                 (relative)
    !       IFRxn   Real  Frequency indices.                   (relative)
    !       IT1     R.A.  Directional indices.                      (1-D)
    !       IFn     R.A.  Frequency indices.                        (1-D)
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
    !      W3IOGR    Subr. W3IOGRMD Model definition file processing.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     - Check on array dimensions for local arrays in W3SNL.
    !
    !  7. Remarks :
    !
    !     - Test output is generated through W3IOGR.
    !     - No testing of IMOD ir resetting of pointers.
    !
    !  8. Structure :
    !
    !     - See source code.
    !
    !  9. Switches :
    !
    !       !/S      Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, DTH, XFR, SIG, LAM
    USE W3ADATMD, ONLY: W3DMNL
    USE W3ADATMD, ONLY: NFR, NFRHGH, NFRCHG, NSPECX, NSPECY,        &
         IP11, IP12, IP13, IP14, IM11, IM12, IM13, IM14,   &
         IP21, IP22, IP23, IP24, IM21, IM22, IM23, IM24,   &
         IC11, IC12, IC21, IC22, IC31, IC32, IC41, IC42,   &
         IC51, IC52, IC61, IC62, IC71, IC72, IC81, IC82,   &
         DAL1, DAL2, DAL3, AF11,                           &
         AWG1, AWG2, AWG3, AWG4, AWG5, AWG6, AWG7, AWG8,   &
         SWG1, SWG2, SWG3, SWG4, SWG5, SWG6, SWG7, SWG8
    USE W3ODATMD, ONLY: NDST, NDSE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: IMOD
    !/
    !/ Local parameters
    !/
    INTEGER                 :: IFR, ITH, ISP, ITHP, ITHP1, ITHM,    &
         ITHM1,IFRP, IFRP1, IFRM, IFRM1
    INTEGER, ALLOCATABLE    :: IF1(:), IF2(:), IF3(:), IF4(:),      &
         IF5(:), IF6(:), IF7(:), IF8(:),      &
         IT1(:), IT2(:), IT3(:), IT4(:),      &
         IT5(:), IT6(:), IT7(:), IT8(:)
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: DELTH3, DELTH4, LAMM2, LAMP2, CTHP,  &
         WTHP, WTHP1, CTHM, WTHM, WTHM1,      &
         XFRLN, WFRP, WFRP1, WFRM, WFRM1, FR, &
         AF11A
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'INSNL1')
#endif
#ifdef W3_T
    WRITE (NDST,9000) IMOD
#endif
    !
    NFR     = NK
    !
    ! 1.  Internal angles of quadruplet.
    !
    LAMM2  = (1.-LAM)**2
    LAMP2  = (1.+LAM)**2
    DELTH3 = ACOS( (LAMM2**2+4.-LAMP2**2) / (4.*LAMM2) )
    DELTH4 = ASIN(-SIN(DELTH3)*LAMM2/LAMP2)
    !
    ! 2.  Lambda dependend weight factors.
    !
    DAL1   = 1. / (1.+LAM)**4
    DAL2   = 1. / (1.-LAM)**4
    DAL3   = 2. * DAL1 * DAL2
    !
    ! 3.  Directional indices.
    !
    CTHP   = ABS(DELTH4/DTH)
    ITHP   = INT(CTHP)
    ITHP1  = ITHP + 1
    WTHP   = CTHP - REAL(ITHP)
    WTHP1  = 1.- WTHP
    !
    CTHM   = ABS(DELTH3/DTH)
    ITHM   = INT(CTHM)
    ITHM1  = ITHM + 1
    WTHM   = CTHM - REAL(ITHM)
    WTHM1  = 1.- WTHM
    !
    ! 4.  Frequency indices.
    !
    XFRLN  = LOG(XFR)
    !
    IFRP   = INT( LOG(1.+LAM) / XFRLN )
    IFRP1  = IFRP + 1
    WFRP   = (1.+LAM - XFR**IFRP) / (XFR**IFRP1 - XFR**IFRP)
    WFRP1  = 1. - WFRP
    !
    IFRM   = INT( LOG(1.-LAM) / XFRLN )
    IFRM1  = IFRM - 1
    WFRM   = (XFR**IFRM -(1.-LAM)) / (XFR**IFRM - XFR**IFRM1)
    WFRM1  = 1. - WFRM
    !
    ! 5.  Range of calculations
    !
    NFRHGH = NFR + IFRP1 - IFRM1
    NFRCHG = NFR - IFRM1
    NSPECY = NFRHGH * NTH
    NSPECX = NFRCHG * NTH
    !
    ! 6.  Allocate arrays or check array sizes
    !
    CALL W3DMNL ( IMOD, NDSE, NDST, NSPEC, NSPECX )
    !
    ALLOCATE ( IF1(NFRCHG), IF2(NFRCHG), IF3(NFRCHG), IF4(NFRCHG),  &
         IF5(NFRCHG), IF6(NFRCHG), IF7(NFRCHG), IF8(NFRCHG),  &
         IT1(NTH), IT2(NTH), IT3(NTH), IT4(NTH),              &
         IT5(NTH), IT6(NTH), IT7(NTH), IT8(NTH) )
    !
    ! 7.  Spectral addresses
    !
    DO IFR=1, NFRCHG
      IF1(IFR) =           IFR+IFRP
      IF2(IFR) =           IFR+IFRP1
      IF3(IFR) = MAX ( 0 , IFR+IFRM  )
      IF4(IFR) = MAX ( 0 , IFR+IFRM1 )
      IF5(IFR) = MAX ( 0 , IFR-IFRP  )
      IF6(IFR) = MAX ( 0 , IFR-IFRP1 )
      IF7(IFR) =           IFR-IFRM
      IF8(IFR) =           IFR-IFRM1
    END DO
    !
    DO ITH=1, NTH
      IT1(ITH) = ITH + ITHP
      IT2(ITH) = ITH + ITHP1
      IT3(ITH) = ITH + ITHM
      IT4(ITH) = ITH + ITHM1
      IT5(ITH) = ITH - ITHP
      IT6(ITH) = ITH - ITHP1
      IT7(ITH) = ITH - ITHM
      IT8(ITH) = ITH - ITHM1
      IF ( IT1(ITH).GT.NTH) IT1(ITH) = IT1(ITH) - NTH
      IF ( IT2(ITH).GT.NTH) IT2(ITH) = IT2(ITH) - NTH
      IF ( IT3(ITH).GT.NTH) IT3(ITH) = IT3(ITH) - NTH
      IF ( IT4(ITH).GT.NTH) IT4(ITH) = IT4(ITH) - NTH
      IF ( IT5(ITH).LT. 1 ) IT5(ITH) = IT5(ITH) + NTH
      IF ( IT6(ITH).LT. 1 ) IT6(ITH) = IT6(ITH) + NTH
      IF ( IT7(ITH).LT. 1 ) IT7(ITH) = IT7(ITH) + NTH
      IF ( IT8(ITH).LT. 1 ) IT8(ITH) = IT8(ITH) + NTH
    END DO
    !
    DO ISP=1, NSPECX
      IFR       = 1 + (ISP-1)/NTH
      ITH       = 1 + MOD(ISP-1,NTH)
      IP11(ISP) = IT2(ITH) + (IF2(IFR)-1)*NTH
      IP12(ISP) = IT1(ITH) + (IF2(IFR)-1)*NTH
      IP13(ISP) = IT2(ITH) + (IF1(IFR)-1)*NTH
      IP14(ISP) = IT1(ITH) + (IF1(IFR)-1)*NTH
      IM11(ISP) = IT8(ITH) + (IF4(IFR)-1)*NTH
      IM12(ISP) = IT7(ITH) + (IF4(IFR)-1)*NTH
      IM13(ISP) = IT8(ITH) + (IF3(IFR)-1)*NTH
      IM14(ISP) = IT7(ITH) + (IF3(IFR)-1)*NTH
      IP21(ISP) = IT6(ITH) + (IF2(IFR)-1)*NTH
      IP22(ISP) = IT5(ITH) + (IF2(IFR)-1)*NTH
      IP23(ISP) = IT6(ITH) + (IF1(IFR)-1)*NTH
      IP24(ISP) = IT5(ITH) + (IF1(IFR)-1)*NTH
      IM21(ISP) = IT4(ITH) + (IF4(IFR)-1)*NTH
      IM22(ISP) = IT3(ITH) + (IF4(IFR)-1)*NTH
      IM23(ISP) = IT4(ITH) + (IF3(IFR)-1)*NTH
      IM24(ISP) = IT3(ITH) + (IF3(IFR)-1)*NTH
    END DO
    !
    DO ISP=1, NSPEC
      IFR       = 1 + (ISP-1)/NTH
      ITH       = 1 + MOD(ISP-1,NTH)
      IC11(ISP) = IT6(ITH) + (IF6(IFR)-1)*NTH
      IC21(ISP) = IT5(ITH) + (IF6(IFR)-1)*NTH
      IC31(ISP) = IT6(ITH) + (IF5(IFR)-1)*NTH
      IC41(ISP) = IT5(ITH) + (IF5(IFR)-1)*NTH
      IC51(ISP) = IT4(ITH) + (IF8(IFR)-1)*NTH
      IC61(ISP) = IT3(ITH) + (IF8(IFR)-1)*NTH
      IC71(ISP) = IT4(ITH) + (IF7(IFR)-1)*NTH
      IC81(ISP) = IT3(ITH) + (IF7(IFR)-1)*NTH
      IC12(ISP) = IT2(ITH) + (IF6(IFR)-1)*NTH
      IC22(ISP) = IT1(ITH) + (IF6(IFR)-1)*NTH
      IC32(ISP) = IT2(ITH) + (IF5(IFR)-1)*NTH
      IC42(ISP) = IT1(ITH) + (IF5(IFR)-1)*NTH
      IC52(ISP) = IT8(ITH) + (IF8(IFR)-1)*NTH
      IC62(ISP) = IT7(ITH) + (IF8(IFR)-1)*NTH
      IC72(ISP) = IT8(ITH) + (IF7(IFR)-1)*NTH
      IC82(ISP) = IT7(ITH) + (IF7(IFR)-1)*NTH
    END DO
    !
    DEALLOCATE ( IF1, IF2, IF3, IF4, IF5, IF6, IF7, IF8,  &
         IT1, IT2, IT3, IT4, IT5, IT6, IT7, IT8 )
    !
    ! 8.  Fill scaling array (f**11)
    !
    DO IFR=1, NFR
      AF11A  = (SIG(IFR)*TPIINV)**11
      DO ITH=1, NTH
        AF11(ITH+(IFR-1)*NTH) = AF11A
      END DO
    END DO
    !
    FR     = SIG(NFR)*TPIINV
    DO IFR=NFR+1, NFRCHG
      FR     = FR * XFR
      AF11A  = FR**11
      DO ITH=1, NTH
        AF11(ITH+(IFR-1)*NTH) = AF11A
      END DO
    END DO
    !
    ! 9.  Interpolation weights
    !
    AWG1   = WTHP  * WFRP
    AWG2   = WTHP1 * WFRP
    AWG3   = WTHP  * WFRP1
    AWG4   = WTHP1 * WFRP1
    AWG5   = WTHM  * WFRM
    AWG6   = WTHM1 * WFRM
    AWG7   = WTHM  * WFRM1
    AWG8   = WTHM1 * WFRM1
    !
    SWG1   = AWG1**2
    SWG2   = AWG2**2
    SWG3   = AWG3**2
    SWG4   = AWG4**2
    SWG5   = AWG5**2
    SWG6   = AWG6**2
    SWG7   = AWG7**2
    SWG8   = AWG8**2
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT (' TEST INSNL1 : IMOD :',I4)
#endif
    !/
    !/ End of INSNL1 ----------------------------------------------------- /
    !/
  END SUBROUTINE INSNL1

  !/ ------------------------------------------------------------------- /
  SUBROUTINE W3SNLGQM(A,CG,WN,DEPTH,TSTOTn,TSDERn)
    ! This and the following routines are adapted to WW3 from TOMAWAC qnlin3.f
    !***********************************************************************
    ! TOMAWAC   V6P1                                   24/06/2011
    !***********************************************************************
    !
    !brief    COMPUTES THE CONTRIBUTION OF THE NON-LINEAR INTERACTIONS
    !+                SOURCE TERM BETWEEN QUADRUPLETS USING THE GQM METHOD
    !+                ("GAUSSIAN QUADRATURE METHOD") PROPOSED BY LAVRENOV
    !+                (2001)
    !+
    !+            PROCEDURE SPECIFIC TO THE CASE WHERE THE FREQUENCIES
    !+                FOLLOW A GEOMETRICAL PROGRESSION AND THE DIRECTIONS
    !+                ARE EVENLY DISTRIBUTED OVER [0;2.PI].
    !
    !note     THIS SUBROUTINE USES THE OUTPUT FROM 'PRENL3' TO OPTIMISE
    !+          THE COMPUTATIONS FOR DIA.
    !
    !reference  LAVRENOV, I.V. (2001):
    !+           "EFFECT OF WIND WAVE PARAMETER FLUCTUATION ON THE NONLINEAR
    !+           SPECTRUM EVOLUTION". J. PHYS. OCEANOGR. 31, 861-873.
    !
    !history  E. GAGNAIRE-RENOU
    !+        04/2011
    !+        V6P1
    !+   CREATED
    !
    !history  G.MATTAROLO (EDF - LNHE)
    !+        24/06/2011
    !+        V6P1
    !+   Translation of French names of the variables in argument

    !
    !/ Warning, contrary to the DIA routine, there is no extension to frequencies below IK=1
    !/ as a result the first two frequencies are not fully treated.
    !==================================================================================
    !     This subroutine is same as qnlin3 in TOMWAC
    USE CONSTANTS, ONLY: TPI
    USE W3GDATMD,  ONLY: SIG, NK ,  NTH , DTH, XFR, FR1, GQTHRSAT, GQAMP

    IMPLICIT NONE

    REAL, intent(in) :: A(NTH,NK), CG(NK), WN(NK)
    REAL, intent(in) :: DEPTH
    REAL, intent(out) :: TSTOTn(NTH,NK), TSDERn(NTH,NK)

    INTEGER          :: ITH,IK,NT,NF
    REAL             :: q_dfac, SATVAL(NK), SUME, ACCVAL, ACCMAX, AMPFAC
    DOUBLE PRECISION :: RAISF, FREQ(NK)
    DOUBLE PRECISION :: TSTOT(NTH,NK) , TSDER(NTH,NK), F(NTH,NK)
    DOUBLE PRECISION :: TEMP

    !.....LOCAL VARIABLES
    INTEGER             JF    , JT    , JF1   , JT1  , IQ_OM2 &
         , JFM0  , JFM1  , JFM2  , JFM3  , IXF1 , IXF2   &
         , IXF3  , JFMIN , JFMAX , ICONF , LBUF
    INTEGER            KT1P  , KT1M  , JT1P  , JT1M  , KT1P2P, KT1P2M &
         , KT1P3P, KT1P3M, KT1M2P, KT1M2M, KT1M3P, KT1M3M &
         , JT1P2P, JT1P2M, JT1P3P, JT1P3M, JT1M2P, JT1M2M &
         , JT1M3P, JT1M3M
    DOUBLE PRECISION  V1_4  , V2_4  , V3_4  , Q_2P3M, Q_2M3P, FACTOR &
         , T_2P3M, T_2M3P, S_2P3M, S_2M3P, SCAL_T, T2P3M &
         , T2M3P , SP0   , SP1P  , SP1M  , SP1P2P, SP1P2M &
         , SP1P3P, SP1P3M, SP1M2P, SP1M2M, SP1M3P, SP1M3M &
         , CF0   , CP0   , CF1   , CP1   , CF2   , CP2   &
         , CF3   , CP3   , Q2PD0 , Q2PD1 , Q2PD2P, Q2PD3M &
         , Q2MD0 , Q2MD1 , Q2MD2M, Q2MD3P ,AUX00 , AUX01  &
         , AUX02 , AUX03 , AUX04 , AUX05 , SEUIL  &
         , AUX06 , AUX07 , AUX08 , AUX09 , AUX10 , FSEUIL

    NT = NTH
    NF = NK
    LBUF = 500
    SEUIL = 0.
    RAISF = XFR

    DO IK = 1,NK
      FREQ(IK) = FR1*RAISF**(IK-1)
    ENDDO

    DO ITH = 1,NTH
      DO IK = 1,NK
        ! F is the E(f,theta) spectrum ...
        F(ITH,IK) = DBLE(A(ITH,IK)*SIG(IK))*DBLE(TPI)/DBLE(CG(IK))
      ENDDO
    ENDDO
    !   CALL INSNLGQM
    ! it returns: F_POIN , T_POIN , F_COEF , F_PROJ, TB_SCA , K_IF1, K_1P, k_1M , K_IF2
    !             K_IF3, K_1P2P , K_1P3M , K_1P2M , K_1P3P , K_1M2P , K_1M3M ,  K_1M2M
    !             K_1M3P , TB_V14 , TB_FAC , TB_V24 , TB_V34 , TB_TMP , TB_TPM , IDCONF, NCONF
    !=======================================================================
    !     COMPUTES THE GENERALIZED MIN AND MAX FREQUENCIES : INSTEAD OF GOING
    !     FROM 1 TO NF IN FREQ(JF) FOR THE MAIN FREQUENCY, IT GOES FROM JFMIN
    !     TO JFMAX
    !     JFMIN IS GIVEN BY Fmin=FREQ(1) /Gamma_min
    !     JFMAX IS GIVEN BY Fmax=FREQ(NF)*Gamma_max
    !     TESTS HAVE SHOWN THAT IT CAN BE ASSUMED Gamma_min=1. (JFMIN=1) AND
    !     Gamma_max=1.3 (JFMAX>NF) TO OBTAIN IMPROVED RESULTS
    !     Note by Fabrice Ardhuin: this appears to give the difference in tail benaviour with Gerbrant's WRT
    !=======================================================================
    JFMIN=MAX(1-INT(LOG(1.0D0)/LOG(RAISF)),1)
    JFMAX=MIN(NF+INT(LOG(1.3D0)/LOG(RAISF)),NK)
    !
    !=======================================================================
    !     COMPUTES THE SPECTRUM THRESHOLD VALUES (BELOW WHICH QNL4 IS NOT
    !     CALCULATED). THE THRESHOLD IS SET WITHIN 0 AND 1.
    ! This was commented by FA
    !=======================================================================
    !        AUX00=0.0D0
    !        DO JF=1,NF
    !          DO JT=1,NT
    !            IF (F(JT,JF).GT.AUX00) AUX00=F(JT,JF)
    !          ENDDO
    !        ENDDO
    !        FSEUIL=AUX00*SEUIL

    TSTOT = 0.
    TSDER = 0.
    !=======================================================================
    ACCMAX=0.
    DO JF=JFMIN,JFMAX
      SUME=SUM(F(:,JF))*DTH
      SATVAL(JF) = SUME*FREQ(JF)**5
      ACCVAL = SUME*FREQ(JF)**4
      IF (ACCVAL.GT.ACCMAX) ACCMAX=ACCVAL
    END DO


    !     ==================================================
    !     STARTS LOOP 1 OVER THE SELECTED CONFIGURATIONS
    !     ==================================================
    DO ICONF=1,NCONF
      !       ---------selected configuration characteristics
      JF1   =IDCONF(ICONF,1)
      JT1   =IDCONF(ICONF,2)
      IQ_OM2=IDCONF(ICONF,3)
      !
      !       ---------Recovers V1**4=(f1/f0)**4
      V1_4  =TB_V14(JF1)
      !       ---------Recovers the shift of the frequency index on f1
      IXF1  =K_IF1(JF1)
      !       ---------Recovers the direction indexes for Delat1
      KT1P  =K_1P(JT1,JF1)
      KT1M  =K_1M(JT1,JF1)
      !       ---------Recovers V2**4=(f2/f0)**4 and V3**4=(f3/f0)**4
      V2_4  =TB_V24(IQ_OM2,JT1,JF1)
      V3_4  =TB_V34(IQ_OM2,JT1,JF1)
      !       ---------Recovers the frequency indexes shift on f2 and f3
      IXF2  =K_IF2 (IQ_OM2,JT1,JF1)
      IXF3  =K_IF3 (IQ_OM2,JT1,JF1)
      !       ---------Recovers the direction indexes shift
      KT1P2P=K_1P2P(IQ_OM2,JT1,JF1)
      KT1P2M=K_1P2M(IQ_OM2,JT1,JF1)
      KT1P3P=K_1P3P(IQ_OM2,JT1,JF1)
      KT1P3M=K_1P3M(IQ_OM2,JT1,JF1)
      KT1M2P=K_1M2P(IQ_OM2,JT1,JF1)
      KT1M2M=K_1M2M(IQ_OM2,JT1,JF1)
      KT1M3P=K_1M3P(IQ_OM2,JT1,JF1)
      KT1M3M=K_1M3M(IQ_OM2,JT1,JF1)
      !       ---------Recovers the coupling coefficients
      T2P3M =TB_TPM(IQ_OM2,JT1,JF1)
      T2M3P =TB_TMP(IQ_OM2,JT1,JF1)
      !       ---------Recovers the multiplicative factor of QNL4
      FACTOR=TB_FAC(IQ_OM2,JT1,JF1)

      !       = = = = = = = = = = = = = = = = = = = = = = = = =
      !       STARTS LOOP 2 OVER THE SPECTRUM FREQUENCIES
      !       = = = = = = = = = = = = = = = = = = = = = = = = =
      DO JF=JFMIN,JFMAX
        IF (SATVAL(JF).GT.GQTHRSAT) THEN
          !
          !.........Recovers the coefficient for the coupling factor
          !.........Computes the coupling coefficients for the case +Delta1 (SIG=1)
          SCAL_T=TB_SCA(LBUF+JF)*FACTOR
          T_2P3M=T2P3M*SCAL_T
          T_2M3P=T2M3P*SCAL_T
          !
          !.........Frequency indexes and coefficients
          JFM0=F_POIN(JF+LBUF)
          CF0 =F_COEF(JF+LBUF)
          CP0 =F_PROJ(JF+LBUF)
          JFM1=F_POIN(JF+IXF1)
          CF1 =F_COEF(JF+IXF1)
          CP1 =F_PROJ(JF+IXF1)
          JFM2=F_POIN(JF+IXF2)
          CF2 =F_COEF(JF+IXF2)
          CP2 =F_PROJ(JF+IXF2)
          JFM3=F_POIN(JF+IXF3)
          CF3 =F_COEF(JF+IXF3)
          CP3 =F_PROJ(JF+IXF3)
          !
          !         -------------------------------------------------
          !         STARTS LOOP 3 OVER THE SPECTRUM DIRECTIONS
          !         -------------------------------------------------
          DO JT=1,NT
            !
            !...........Direction indexes
            !           direct config (+delta1) (sig =1)
            JT1P  =T_POIN(JT+KT1P)
            JT1P2P=T_POIN(JT+KT1P2P)
            JT1P2M=T_POIN(JT+KT1P2M)
            JT1P3P=T_POIN(JT+KT1P3P)
            JT1P3M=T_POIN(JT+KT1P3M)
            !           image config (-delta1)
            JT1M  =T_POIN(JT+KT1M)
            JT1M2P=T_POIN(JT+KT1M2P)
            JT1M2M=T_POIN(JT+KT1M2M)
            JT1M3P=T_POIN(JT+KT1M3P)
            JT1M3M=T_POIN(JT+KT1M3M)
            !
            !           - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            !           STARTS LOOP 4 OVER THE MESH NODES
            !           - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            !
            SP0=F(JT,JFM0)*CF0
            !
            !              IF (SP0.GT.FSEUIL) THEN
            !
            !               Config. +Delta1 (SIG=1)
            !               =======================
            !...............Computes the spectrum values in 1, 2, 3
            SP1P  =F(JT1P  ,JFM1)*CF1
            SP1P2P=F(JT1P2P,JFM2)*CF2
            SP1P3M=F(JT1P3M,JFM3)*CF3
            SP1P2M=F(JT1P2M,JFM2)*CF2
            SP1P3P=F(JT1P3P,JFM3)*CF3
            !
            !...............Computes auxiliary products and variables
            AUX01=SP0*V1_4+SP1P
            AUX02=SP0*SP1P
            AUX03=SP1P2P*SP1P3M
            AUX04=SP1P2P*V3_4+SP1P3M*V2_4
            AUX05=SP1P2M*SP1P3P
            AUX06=SP1P2M*V3_4+SP1P3P*V2_4
            AUX07=AUX02*V3_4
            AUX08=AUX02*V2_4
            !
            !...............Computes the components of the transfer term
            S_2P3M=AUX03*AUX01-AUX02*AUX04
            S_2M3P=AUX05*AUX01-AUX02*AUX06
            Q_2P3M=T_2P3M*S_2P3M
            Q_2M3P=T_2M3P*S_2M3P
            AUX00 =Q_2P3M+Q_2M3P
            !
            !...............Computes the components of the derived terms (dQ/dF)
            Q2PD0 =T_2P3M*(AUX03*V1_4   - SP1P*AUX04)*CF0
            Q2PD1 =T_2P3M*(AUX03        - SP0 *AUX04)*CF1
            Q2PD2P=T_2P3M*(AUX01*SP1P3M - AUX07     )*CF2
            Q2PD3M=T_2P3M*(AUX01*SP1P2P - AUX08     )*CF3
            Q2MD0 =T_2M3P*(AUX05*V1_4   - SP1P*AUX06)*CF0
            Q2MD1 =T_2M3P*(AUX03        - SP0 *AUX06)*CF1
            Q2MD2M=T_2M3P*(AUX01*SP1P3P - AUX07     )*CF2
            Q2MD3P=T_2M3P*(AUX01*SP1P2M - AUX08     )*CF3
            AUX09=Q2PD0+Q2MD0
            AUX10=Q2PD1+Q2MD1
            !
            !...............Sum of Qnl4 term in the table TSTOT
            TSTOT(JT,JFM0    )=TSTOT(JT,JFM0    )+AUX00 *CP0
            TSTOT(JT1P,JFM1  )=TSTOT(JT1P,JFM1  )+AUX00 *CP1
            TSTOT(JT1P2P,JFM2)=TSTOT(JT1P2P,JFM2)-Q_2P3M*CP2
            TSTOT(JT1P2M,JFM2)=TSTOT(JT1P2M,JFM2)-Q_2M3P*CP2
            TSTOT(JT1P3M,JFM3)=TSTOT(JT1P3M,JFM3)-Q_2P3M*CP3
            TSTOT(JT1P3P,JFM3)=TSTOT(JT1P3P,JFM3)-Q_2M3P*CP3
            !
            !...............Sum of the term dQnl4/dF in the table TSDER
            TSDER(JT,JFM0)=TSDER(JT,JFM0)+AUX09 *CP0
            TSDER(JT1P,JFM1)=TSDER(JT1P,JFM1)+AUX10 *CP1
            TSDER(JT1P2P,JFM2)=TSDER(JT1P2P,JFM2)-Q2PD2P*CP2
            TSDER(JT1P2M,JFM2)=TSDER(JT1P2M,JFM2)-Q2MD2M*CP2
            TSDER(JT1P3M,JFM3)=TSDER(JT1P3M,JFM3)-Q2PD3M*CP3
            TSDER(JT1P3P,JFM3)=TSDER(JT1P3P,JFM3)-Q2MD3P*CP3
#ifdef W3_TGQM
            ! Test output to set up triplet method ...
            WRITE(994,'(5I3,3E12.3)') ICONF,JF,JT,JT,    JFM0,AUX00 *CP0, F(JT,JFM0),TSTOT(JT    ,JFM0)
            WRITE(994,'(5I3,3E12.3)') ICONF,JF,JT,JT1P,  JFM1,AUX00 *CP1, F(JT1P,JFM1),TSTOT(JT1P,JFM1)
            WRITE(994,'(5I3,3E12.3)') ICONF,JF,JT,JT1P2P,JFM2,-Q_2P3M*CP2,F(JT1P2P,JFM2),TSTOT(JT1P2P,JFM2)
            WRITE(994,'(5I3,3E12.3)') ICONF,JF,JT,JT1P2M,JFM2,-Q_2M3P*CP2,F(JT1P2M,JFM2),TSTOT(JT1P2M,JFM2)
            WRITE(994,'(5I3,3E12.3)') ICONF,JF,JT,JT1P3M,JFM2,-Q_2P3M*CP3,F(JT1P3M,JFM3),TSTOT(JT1P3M,JFM3)
            WRITE(994,'(5I3,3E12.3)') ICONF,JF,JT,JT1P3P,JFM2,-Q_2M3P*CP3,F(JT1P3P,JFM3),TSTOT(JT1P3P,JFM3)
            TEMP=(TB_TPM(IQ_OM2,JT1,JF1)*(( F(JT1P2P,JFM2)*CF2 *F(JT1P3M,JFM3)*CF3)* &
                 (F(JT,JFM0    )*CF0*TB_V14(JF1)+F(JT1P  ,JFM1)*CF1) &
                 -SP0*SP1P*(SP1P2P*V3_4+SP1P3M*V2_4))+T_2M3P*(AUX05*AUX01-AUX02*AUX06)) *CP0
            WRITE(995,'(3I3,3E12.3)') ICONF,JF,JT, F(JT,JFM0)
            TEMP=(Q_2P3M+Q_2M3P) *CP1
            WRITE(995,'(5I3,3E12.3)') ICONF,JF,JT,JT1P,  JFM1,AUX00 *CP1, F(JT1P,JFM1),TSTOT(JT1P,JFM1)
            WRITE(995,'(5I3,3E12.3)') ICONF,JF,JT,JT1P2P,JFM2,-Q_2P3M*CP2,F(JT1P2P,JFM2),TSTOT(JT1P2P,JFM2)
            WRITE(995,'(5I3,3E12.3)') ICONF,JF,JT,JT1P2M,JFM2,-Q_2M3P*CP2,F(JT1P2M,JFM2),TSTOT(JT1P2M,JFM2)
            WRITE(995,'(5I3,3E12.3)') ICONF,JF,JT,JT1P3M,JFM2,-Q_2P3M*CP3,F(JT1P3M,JFM3),TSTOT(JT1P3M,JFM3)
            WRITE(995,'(5I3,3E12.3)') ICONF,JF,JT,JT1P3P,JFM2,-Q_2M3P*CP3,F(JT1P3P,JFM3),TSTOT(JT1P3P,JFM3)
#endif
            !
            !               Config. -Delta1 (SIG=-1)
            !               ========================
            !...............Computes the spectrum values in 1, 2, 3
            SP1M  =F(JT1M  ,JFM1)*CF1
            SP1M2P=F(JT1M2P,JFM2)*CF2
            SP1M3M=F(JT1M3M,JFM3)*CF3
            SP1M2M=F(JT1M2M,JFM2)*CF2
            SP1M3P=F(JT1M3P,JFM3)*CF3
            !
            !...............Computes auxiliary products and variables
            AUX01=SP0*V1_4+SP1M
            AUX02=SP0*SP1M
            AUX03=SP1M2P*SP1M3M
            AUX04=SP1M2P*V3_4+SP1M3M*V2_4
            AUX05=SP1M2M*SP1M3P
            AUX06=SP1M2M*V3_4+SP1M3P*V2_4
            AUX07=AUX02*V3_4
            AUX08=AUX02*V2_4
            !
            !...............Computes the transfer term components
            S_2P3M=AUX03*AUX01-AUX02*AUX04
            S_2M3P=AUX05*AUX01-AUX02*AUX06
            Q_2P3M=T_2M3P*S_2P3M
            Q_2M3P=T_2P3M*S_2M3P
            AUX00 =Q_2P3M+Q_2M3P   ! Same as in +Delta1, can be commented out
            !
            !...............Computes the derived terms components (dQ/dF)
            Q2PD0 =T_2P3M*(AUX03*V1_4   - SP1M*AUX04)*CF0
            Q2PD1 =T_2P3M*(AUX03        - SP0 *AUX04)*CF1
            Q2PD2P=T_2P3M*(AUX01*SP1M3M - AUX07     )*CF2
            Q2PD3M=T_2P3M*(AUX01*SP1M2P - AUX08     )*CF3
            Q2MD0 =T_2M3P*(AUX05*V1_4   - SP1M*AUX06)*CF0
            Q2MD1 =T_2M3P*(AUX03        - SP0 *AUX06)*CF1
            Q2MD2M=T_2M3P*(AUX01*SP1M3P - AUX07     )*CF2
            Q2MD3P=T_2M3P*(AUX01*SP1M2M - AUX08     )*CF3
            AUX09=Q2PD0+Q2MD0
            AUX10=Q2PD1+Q2MD1
            !
            !...............Sum of Qnl4 term in the table TSTOT
            TSTOT(JT    ,JFM0)=TSTOT(JT    ,JFM0)+AUX00 *CP0
            TSTOT(JT1M  ,JFM1)=TSTOT(JT1M  ,JFM1)+AUX00 *CP1
            TSTOT(JT1M2P,JFM2)=TSTOT(JT1M2P,JFM2)-Q_2P3M*CP2
            TSTOT(JT1M2M,JFM2)=TSTOT(JT1M2M,JFM2)-Q_2M3P*CP2
            TSTOT(JT1M3M,JFM3)=TSTOT(JT1M3M,JFM3)-Q_2P3M*CP3
            TSTOT(JT1M3P,JFM3)=TSTOT(JT1M3P,JFM3)-Q_2M3P*CP3
            !
            !...............Sum of the term dQnl4/dF in the table TSDER
            TSDER(JT    ,JFM0)=TSDER(JT    ,JFM0)+AUX09 *CP0
            TSDER(JT1M  ,JFM1)=TSDER(JT1M  ,JFM1)+AUX10 *CP1
            TSDER(JT1M2P,JFM2)=TSDER(JT1M2P,JFM2)-Q2PD2P*CP2
            TSDER(JT1M2M,JFM2)=TSDER(JT1M2M,JFM2)-Q2MD2M*CP2
            TSDER(JT1M3M,JFM3)=TSDER(JT1M3M,JFM3)-Q2PD3M*CP3
            TSDER(JT1M3P,JFM3)=TSDER(JT1M3P,JFM3)-Q2MD3P*CP3
            !
#ifdef W3_TGQM
            WRITE(994,'(5I3,3E12.3)') ICONF,JF,JT,JT,    JFM0,AUX00 *CP0, F(JT,JFM0),TSTOT(JT    ,JFM0)
            WRITE(994,'(5I3,3E12.3)') ICONF,JF,JT,JT1M,  JFM1,AUX00 *CP1, F(JT1M,JFM1),TSTOT(JT1M,JFM1)
            WRITE(994,'(5I3,3E12.3)') ICONF,JF,JT,JT1M2P,JFM2,-Q_2P3M*CP2,F(JT1M2P,JFM2),TSTOT(JT1M2P,JFM2)
            WRITE(994,'(5I3,3E12.3)') ICONF,JF,JT,JT1M2M,JFM2,-Q_2M3P*CP2,F(JT1M2M,JFM2),TSTOT(JT1M2M,JFM2)
            WRITE(994,'(5I3,3E12.3)') ICONF,JF,JT,JT1M3M,JFM2,-Q_2P3M*CP3,F(JT1M3M,JFM3),TSTOT(JT1M3M,JFM3)
            WRITE(994,'(5I3,3E12.3)') ICONF,JF,JT,JT1M3P,JFM2,-Q_2M3P*CP3,F(JT1M3P,JFM3),TSTOT(JT1M3P,JFM3)
#endif
            !
            !              ENDIF ! this was the test on SEUIL
            !
          ENDDO
          !         -------------------------------------------------
          !         END OF LOOP 3 OVER THE SPECTRUM DIRECTIONS
          !         -------------------------------------------------
          !
        ENDIF ! End of test on saturation level
      ENDDO
      !       = = = = = = = = = = = = = = = = = = = = = = = = =
      !       END OF LOOP 2 OVER THE SPECTRUM FREQUENCIES
      !       = = = = = = = = = = = = = = = = = = = = = = = = =
      !
    ENDDO
    !     ==================================================
    !     END OF LOOP 1 OVER THE SELECTED CONFIGURATIONS
    !     ==================================================
    ! Applying WAM DEPTH SCALING ! to be added later ...
    !      CALL q_dscale(F,WN,SIG,DTH,NK,NTH,DEPTH,q_dfac)
    q_dfac=1

    ! Amplification inspired by Lavrenov 2001, eq 10.
    AMPFAC=GQAMP(4)*MIN(MAX(ACCMAX/GQAMP(2),1.)**GQAMP(1),GQAMP(3))
    !WRITE(991,*) ACCMAX,q_dfac,AMPFAC,GQAMP(1:3),SATVAL(10),SATVAL(30)

    ! Replacing Double Precision with Simple Real and scaling
    TSTOTn = TSTOT*q_dfac*AMPFAC
    TSDERn = TSDER*q_dfac*AMPFAC


    ! Converting Snl(theta,f) to Snl(theta,k)/sigma
    DO ITH = 1,NT
      DO IK = 1,NF
        TSTOTn(ITH,IK) = TSTOTn(ITH,IK)*CG(IK)/(TPI*SIG(IK))
      ENDDO
    ENDDO
    !CLOSE(994)
    !STOP
  END SUBROUTINE W3SNLGQM

  !/ ------------------------------------------------------------------- /
  FUNCTION COUPLE(XK1 ,YK1 ,XK2 ,YK2 ,XK3 ,YK3 ,XK4 ,YK4)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  | M. Benoit & E. Gagnaire-Renou     |
    !/                  | Last update :         20-Nov-2022 |
    !/                  +-----------------------------------+
    !/
    !/    19-Nov-2022 : Transfer from TOMAWAC code          ( version 7.xx )
    !/
    !  1. Purpose :
    !
    !     Computes the 4-wave coupling coefficient used in Snl4
    !
    !  2. Method :
    !
    !     Uses theoretical expression by Webb (1978)
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       XK1     Real  I   x component of k1 wavenumber ...
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      INNSLGQM  Subr. W3SNL2   Prepares source term integration.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !       None.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS, ONLY: GRAV
    !
    IMPLICIT NONE
    
    DOUBLE PRECISION, INTENT(IN)    :: XK1   , YK1   , XK2   , YK2
    DOUBLE PRECISION, INTENT(IN)    :: XK3   , YK3
    DOUBLE PRECISION, INTENT(IN)    :: XK4   , YK4
    DOUBLE PRECISION COUPLE
    !
    !.....LOCAL VARIABLES
    !     """"""""""""""""""
    DOUBLE PRECISION RK1   , RK2   , RK3   , RK4   , WK1   , WK2
    DOUBLE PRECISION WK3   , WK4   , S12   , S13   , S14   , S23
    DOUBLE PRECISION S24   , S34   , W1P2  , Q12   , W1M3  , Q13
    DOUBLE PRECISION W1M4  , Q14   , DDD   , COEF  , DENO13, NUME13
    DOUBLE PRECISION DENO14, NUME14, ZERO, PI

    !
    PI = ACOS(-1.)
    COEF=PI*GRAV*GRAV/4.D0
    ZERO=1.D-10
    !
    RK1=SQRT(XK1*XK1+YK1*YK1)
    RK2=SQRT(XK2*XK2+YK2*YK2)
    RK3=SQRT(XK3*XK3+YK3*YK3)
    RK4=SQRT(XK4*XK4+YK4*YK4)
    !
    WK1=SQRT(RK1)
    WK2=SQRT(RK2)
    WK3=SQRT(RK3)
    WK4=SQRT(RK4)
    !
    S12=XK1*XK2+YK1*YK2
    S13=XK1*XK3+YK1*YK3
    S14=XK1*XK4+YK1*YK4
    S23=XK2*XK3+YK2*YK3
    S24=XK2*XK4+YK2*YK4
    S34=XK3*XK4+YK3*YK4
    !
    W1P2=SQRT((XK1+XK2)*(XK1+XK2)+(YK1+YK2)*(YK1+YK2))
    W1M3=SQRT((XK1-XK3)*(XK1-XK3)+(YK1-YK3)*(YK1-YK3))
    W1M4=SQRT((XK1-XK4)*(XK1-XK4)+(YK1-YK4)*(YK1-YK4))
    Q12=(WK1+WK2)*(WK1+WK2)
    Q13=(WK1-WK3)*(WK1-WK3)
    Q14=(WK1-WK4)*(WK1-WK4)
    !
    !.....COMPUTES THE D COEFFICIENT OF WEBB (1978)
    !     """"""""""""""""""""""""""""""""""""""
    DDD=2.00D0*Q12*(RK1*RK2-S12)*(RK3*RK4-S34)/(W1P2-Q12) &
         +0.50D0*(S12*S34+S13*S24+S14*S23) &
         +0.25D0*(S13+S24)*Q13*Q13 &
         -0.25D0*(S12+S34)*Q12*Q12 &
         +0.25D0*(S14+S23)*Q14*Q14 &
         +2.50D0*RK1*RK2*RK3*RK4 &
         +Q12*Q13*Q14*(RK1+RK2+RK3+RK4)

    DENO13=W1M3-Q13
    NUME13=2.00D0*Q13*(RK1*RK3+S13)*(RK2*RK4+S24)
    IF (ABS(DENO13).LT.ZERO) THEN
      IF (ABS(NUME13).LT.ZERO) THEN
        WRITE(*,*) 'W3SNL2 error for coupling coefficient : (1-3)  0/0 !'
      ELSE
        WRITE(*,*) 'W3SNL2 error for coupling coefficient : (1-3) inifinte value'
      ENDIF
      WRITE(*,*) 'W3SNL2 error for coupling coefficient : (1-3) term not used'
    ELSE
      DDD=DDD+NUME13/DENO13
    ENDIF
    DENO14=W1M4-Q14
    NUME14=2.00D0*Q14*(RK1*RK4+S14)*(RK2*RK3+S23)
    IF (ABS(DENO14).LT.ZERO) THEN
      IF (ABS(NUME14).LT.ZERO) THEN
        WRITE(*,*) 'W3SNL2 error for coupling coefficient : (1-4)  0/0 !'
      ELSE
        WRITE(*,*) 'W3SNL2 error for coupling coefficient : (1-4) inifinte value'
      ENDIF
      WRITE(*,*) 'W3SNL2 error for coupling coefficient : (1-4) term not used'
    ELSE
      DDD=DDD+NUME14/DENO14
    ENDIF

    COUPLE=COEF*DDD*DDD/(WK1*WK2*WK3*WK4)
    !      RETURN
  END FUNCTION COUPLE

  !/ ------------------------------------------------------------------- /
  SUBROUTINE GAULEG (W_LEG ,X_LEG ,NPOIN)
    !/ ------------------------------------------------------------------- /
    !.....VARIABLES IN ARGUMENT
    !     """"""""""""""""""""
    IMPLICIT NONE
    INTEGER ,         INTENT(IN)    :: NPOIN
    DOUBLE PRECISION ,INTENT(INOUT) :: W_LEG(NPOIN) , X_LEG(NPOIN)
    !
    !.....LOCAL VARIABLES
    !     """""""""""""""""
    INTEGER           I, M, J
    DOUBLE PRECISION  EPS, Z, P1, P2, P3, PP, Z1, PI
    PARAMETER        (EPS=3.D-14)
    !
    PI = ACOS(-1.)
    M=(NPOIN+1)/2
    DO I=1,M
      Z=COS(PI*(DBLE(I)-0.25D0)/(DBLE(NPOIN)+0.5D0))
1     CONTINUE
      P1=1.0D0
      P2=0.0D0
      DO J=1,NPOIN
        P3=P2
        P2=P1
        P1=((2.D0*DBLE(J)-1.D0)*Z*P2-(DBLE(J)-1.D0)*P3)/DBLE(J)
      ENDDO
      PP=DBLE(NPOIN)*(Z*P1-P2)/(Z*Z-1.D0)
      Z1=Z
      Z=Z-P1/PP
      IF (ABS(Z-Z1).GT.EPS) GOTO 1
      X_LEG(I)=-Z
      X_LEG(NPOIN+1-I)=Z
      W_LEG(I)=2.D0/((1.D0-Z**2)*PP**2)
      W_LEG(NPOIN+1-I)=W_LEG(I)
    ENDDO
  END SUBROUTINE GAULEG

  !/ ------------------------------------------------------------------- /
  SUBROUTINE F1F1F1(F1SF,NF1,IQ_OM1)
    ! TOMAWAC   V6P3                                   15/06/2011
    !***********************************************************************
    !
    !brief   SUBROUTINE CALLED BY PRENL3
    !+         COMPUTES VALUES OF RATIO F1/F AS FUNCTION OF THE IQ_OM1
    !+         INDICATOR
    !
    !history  E. GAGNAIRE-RENOU
    !+        04/2011
    !+        V6P1
    !+   CREATED
    !
    !history  G.MATTAROLO (EDF - LNHE)
    !+        15/06/2011
    !+        V6P1
    !+   Translation of French names of the variables in argument
    !
    !history  E. GAGNAIRE-RENOU
    !+        12/03/2013
    !+        V6P3
    !+   Better formatted: WRITE(LU,*), etc.
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    INTEGER,          INTENT(IN)    :: IQ_OM1
    INTEGER,          INTENT(INOUT) :: NF1
    DOUBLE PRECISION, INTENT(INOUT) :: F1SF(*)
    !
    INTEGER I,M
    DOUBLE PRECISION RAISON
    !
    IF(IQ_OM1.EQ.1) THEN
      IF(NF1.NE.14) THEN
        WRITE(*,*) '#1 Incorrect value for NF1',NF1
      ENDIF
      F1SF( 1)=0.30D0
      F1SF( 2)=0.40D0
      F1SF( 3)=0.50D0
      F1SF( 4)=0.60D0
      F1SF( 5)=0.70D0
      F1SF( 6)=0.80D0
      F1SF( 7)=0.90D0
      F1SF( 8)=1.00D0
      F1SF( 9)=1.11D0
      F1SF(10)=1.25D0
      F1SF(11)=1.42D0
      F1SF(12)=1.67D0
      F1SF(13)=2.00D0
      F1SF(14)=2.50D0
      F1SF(15)=3.30D0
    ELSEIF(IQ_OM1.EQ.2) THEN
      IF (NF1.NE.26) THEN
        WRITE(*,*) '#2 Incorrect value for NF1', NF1
      ENDIF
      F1SF( 1)=0.32D0
      F1SF( 2)=0.35D0
      F1SF( 3)=0.39D0
      F1SF( 4)=0.44D0
      F1SF( 5)=0.50D0
      F1SF( 6)=0.56D0
      F1SF( 7)=0.63D0
      F1SF( 8)=0.70D0
      F1SF( 9)=0.78D0
      F1SF(10)=0.86D0
      F1SF(11)=0.92D0
      F1SF(12)=0.97D0
      F1SF(13)=1.00D0
      F1SF(14)=1.03D0
      F1SF(15)=1.08D0
      F1SF(16)=1.13D0
      F1SF(17)=1.20D0
      F1SF(18)=1.28D0
      F1SF(19)=1.37D0
      F1SF(20)=1.48D0
      F1SF(21)=1.50D0
      F1SF(22)=1.65D0
      F1SF(23)=1.85D0
      F1SF(24)=2.10D0
      F1SF(25)=2.40D0
      F1SF(26)=2.70D0
      F1SF(27)=3.20D0
    ELSEIF(IQ_OM1.EQ.3) THEN
      IF(NF1.NE.11) THEN
        WRITE(*,*) 'Incorrect value for NF1', NF1
      ENDIF
      F1SF( 1)=0.30D0
      F1SF( 2)=0.48D0
      F1SF( 3)=0.64D0
      F1SF( 4)=0.78D0
      F1SF( 5)=0.90D0
      F1SF( 6)=1.00D0
      F1SF( 7)=1.12D0
      F1SF( 8)=1.28D0
      F1SF( 9)=1.50D0
      F1SF(10)=1.80D0
      F1SF(11)=2.40D0
      F1SF(12)=3.40D0
    ELSEIF(IQ_OM1.EQ.4) THEN
      IF(NF1.NE.40) THEN
        WRITE(*,*) 'Incorrect value for NF1', NF1
      ENDIF
      NF1=20
      M=10
      RAISON=9.D0**(1.D0/DBLE(NF1))
      F1SF(M+1)=1.0D0/3.0D0
      NF1=2*M+NF1
      DO I=M+2,NF1+1
        F1SF(I)=F1SF(I-1)*RAISON
      ENDDO
      DO I=M,1,-1
        F1SF(I)=F1SF(I+1)/RAISON
      ENDDO
    ELSEIF(IQ_OM1.EQ.5) THEN
      RAISON=9.D0**(1.D0/DBLE(NF1))
      F1SF(1)=1.D0/3.D0
      DO I=2,NF1+1
        F1SF(I)=F1SF(I-1)*RAISON
      ENDDO
    ELSEIF(IQ_OM1.EQ.6) THEN
      RAISON=(3.D0-1.D0/3.D0)/DBLE(NF1)
      F1SF(1)=1.D0/3.D0
      DO I=2,NF1+1
        F1SF(I)=F1SF(I-1)+RAISON
      ENDDO
    ELSEIF(IQ_OM1.EQ.7) THEN
      IF(NF1.NE.20) THEN
        WRITE(*,*) 'Incorrect value for NF1', NF1
      ENDIF
      F1SF( 1)=1.D0/3.D0
      F1SF( 2)=0.40D0
      F1SF( 3)=0.46D0
      F1SF( 4)=0.52D0
      F1SF( 5)=0.60D0
      F1SF( 6)=0.70D0
      F1SF( 7)=0.79D0
      F1SF( 8)=0.86D0
      F1SF( 9)=0.92D0
      F1SF(10)=0.97D0
      F1SF(11)=1.00D0
      F1SF(12)=1.04D0
      F1SF(13)=1.10D0
      F1SF(14)=1.18D0
      F1SF(15)=1.28D0
      F1SF(16)=1.42D0
      F1SF(17)=1.60D0
      F1SF(18)=1.84D0
      F1SF(19)=2.14D0
      F1SF(20)=2.52D0
      F1SF(21)=3.00D0
    ENDIF
    !
  END SUBROUTINE F1F1F1
  !/ ------------------------------------------------------------------- /
  SUBROUTINE INSNLGQM
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |       E. Gagnaire-Renou &         |
    !/                  |       M. Benoit                   |
    !/                  |       S. Mostafa Siadatamousavi   |
    !/		    |       M. Beyramzadeh              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         20-Nov-2022 |
    !/                  +-----------------------------------+
    !/
    !/    20-Nov-2022 : Merging with NL2 in WW3.            ( version 7.00 )
    !/
    !  1. Purpose :
    !
    !     Preprocessing for nonlinear interactions (Xnl).
    !
    !  2. Method :
    !
    !     See Xnl documentation.
    !
    !  3. Parameters :
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module      Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD    Subroutine tracing.
    !                Subr. GAULEG      Gauss-Legendre weights
    !      xnl_init  Subr. m_constants Xnl initialization routine.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3IOGR    Subr. W3IOGRMD Model definition file management.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     - See source code.
    !
    !  9. Switches :
    !
    !       !/S      Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS, ONLY: GRAV
    USE W3GDATMD,  ONLY: NK , NTH , XFR , FR1, GQNF1, GQNT1, GQNQ_OM2, NLTAIL, GQTHRCOU

#ifdef W3_S
    CALL STRACE (IENT, 'INSNLGQM')
#endif
    IMPLICIT NONE
    !.....LOCAL VARIABLES
    INTEGER           JF    , JT    , JF1   , JT1   , NF1P1 , IAUX , NT , NF , IK
    INTEGER           IQ_TE1 , IQ_OM2 , LBUF , DIMBUF , IQ_OM1 , NQ_TE1 , NCONFM

    DOUBLE PRECISION  EPSI_A, AUX   , CCC   , DENO  , AAA   , DP2SG , TAILF
    DOUBLE PRECISION  V1    , V1_4  , DV1   , DTETAR , ELIM , RAISF
    DOUBLE PRECISION  V2    , V2_4  , V3    , V3_4
    DOUBLE PRECISION  W2    , W2_M  , W2_1  , W_MIL , W_RAD
    DOUBLE PRECISION  RK0   , XK0   , YK0   , RK1   , XK1   , YK1
    DOUBLE PRECISION  RK2   , XK2P  , YK2P  , XK2M  , YK2M
    DOUBLE PRECISION  RK3   , XK3P  , YK3P  , XK3M  , YK3M
    DOUBLE PRECISION  D01P  , C_D01P, S_D01P, D0AP  , C_D0AP, S_D0AP
    DOUBLE PRECISION  GA2P  , C_GA2P, S_GA2P, GA3P  , C_GA3P, S_GA3P, TWOPI, PI, SEUIL1 , SEUIL2 , SEUIL
    !
    !.....Variables related to the Gaussian quadratures
    DOUBLE PRECISION  W_CHE_TE1, W_CHE_OM2, C_LEG_OM2
    !
    !.....Variables related to the configuration selection
    DOUBLE PRECISION  TEST1 , TEST2
    DOUBLE PRECISION :: FREQ(NK)
    DOUBLE PRECISION, ALLOCATABLE :: F1SF(:) , X_CHE_TE1(:) , X_CHE_OM2(:) , X_LEG_OM2(:) , W_LEG_OM2(:) &
         ,  MAXCLA(:)

    PI = Acos(-1.)
    LBUF = 500
    DIMBUF = 2*LBUF+200
    TWOPI  = 2.*PI
    !
    ! Defines some threshold values for filtering (See Gagnaire-Renou Thesis,  p 52)
    !
    SEUIL1 = 1E10
    SEUIL2 = GQTHRCOU

    IF(GQNF1.EQ.14) IQ_OM1=1
    IF(GQNF1.EQ.26) IQ_OM1=2
    IF(GQNF1.EQ.11) IQ_OM1=3
    IF(GQNF1.EQ.40) IQ_OM1=4
    IF(GQNF1.EQ.11) IQ_OM1=3
    IF(GQNF1.EQ.40) IQ_OM1=4
    IF(GQNF1.EQ.20) IQ_OM1=7
    !
    ! Note by FA: not sure what the 5 and 6 cases correspond to
    !
    NQ_TE1 = GQNT1/2
    NCONFM = GQNF1*GQNT1*GQNQ_OM2

    RAISF = XFR
    NT = NTH
    NF = NK
    DTETAR = TWOPI/DBLE(NT)

    DO IK = 1,NK
      FREQ(IK) = FR1*RAISF**(IK-1)
    ENDDO

    TAILF = -NLTAIL

    !===============ALLOCATE MATRICES=============================================
    if (Allocated(K_IF2) ) then
      deallocate(K_IF2)
    endif
    ALLOCATE(K_IF2(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(K_IF3) ) then
      deallocate(K_IF3)
    endif
    ALLOCATE(K_IF3(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(K_1P2P) ) then
      deallocate(K_1P2P)
    endif
    ALLOCATE(K_1P2P(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(K_1P3M) ) then
      deallocate(K_1P3M)
    endif
    ALLOCATE(K_1P3M(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(K_1P2M) ) then
      deallocate(K_1P2M)
    endif
    ALLOCATE(K_1P2M(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(K_1P3P) ) then
      deallocate(K_1P3P)
    endif
    ALLOCATE(K_1P3P(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(K_1M2P) ) then
      deallocate(K_1M2P)
    endif
    ALLOCATE(K_1M2P(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(K_1M3M) ) then
      deallocate(K_1M3M)
    endif
    ALLOCATE(K_1M3M(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(K_1M2M) ) then
      deallocate(K_1M2M)
    endif
    ALLOCATE(K_1M2M(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(K_1M3P) ) then
      deallocate(K_1M3P)
    endif
    ALLOCATE(K_1M3P(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(TB_V24) ) then
      deallocate(TB_V24)
    endif
    ALLOCATE(TB_V24(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(TB_V34) ) then
      deallocate(TB_V34)
    endif
    ALLOCATE(TB_V34(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(TB_TPM) ) then
      deallocate(TB_TPM)
    endif
    ALLOCATE(TB_TPM(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(TB_TMP) ) then
      deallocate(TB_TMP)
    endif
    ALLOCATE(TB_TMP(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(TB_FAC) ) then
      deallocate(TB_FAC)
    endif
    ALLOCATE(TB_FAC(GQNQ_OM2,GQNT1,GQNF1))

    if (Allocated(K_IF1) ) then
      deallocate(K_IF1)
    endif
    ALLOCATE(K_IF1(GQNF1))

    if (Allocated(K_1P) ) then
      deallocate(K_1P)
    endif
    ALLOCATE(K_1P(GQNT1,GQNF1))

    if (Allocated(K_1M) ) then
      deallocate(K_1M)
    endif
    ALLOCATE(K_1M(GQNT1,GQNF1))

    if (Allocated(TB_V14) ) then
      deallocate(TB_V14)
    endif
    ALLOCATE(TB_V14(GQNF1))

    if (Allocated(IDCONF) ) then
      deallocate(IDCONF)
    endif
    ALLOCATE(IDCONF(NCONFM,3))

    !=======================================================================
    !     INITIALISATION OF AUXILIAIRY TABLES FOR SPECTRUM INTERPOLATION
    !=======================================================================
    if (Allocated(F_POIN) ) then
      deallocate(F_POIN)
    endif
    ALLOCATE(F_POIN(DIMBUF))

    if (Allocated(T_POIN) ) then
      deallocate(T_POIN)
    endif
    ALLOCATE(T_POIN(DIMBUF))

    if (Allocated(F_COEF) ) then
      deallocate(F_COEF)
    endif
    ALLOCATE(F_COEF(DIMBUF))

    if (Allocated(F_PROJ) ) then
      deallocate(F_PROJ)
    endif
    ALLOCATE(F_PROJ(DIMBUF))

    if (Allocated(TB_SCA) ) then
      deallocate(TB_SCA)
    endif
    ALLOCATE(TB_SCA(DIMBUF))


    F_POIN(:)=0
    T_POIN(:)=0
    F_COEF(:)=0.D0
    F_PROJ(:)=0.D0
    TB_SCA(:)=0.0D0

    DO JF=1,LBUF
      F_POIN(JF)=1
      F_COEF(JF)=0.0D0
      F_PROJ(JF)=0.0D0
    ENDDO
    DO JF=1,NF
      IAUX=LBUF+JF
      F_POIN(IAUX)=JF
      F_COEF(IAUX)=1.0D0
      F_PROJ(IAUX)=1.0D0
    ENDDO
    AUX=1.D0/RAISF**TAILF
    DO JF=1,LBUF
      IAUX=LBUF+NF+JF
      F_POIN(IAUX)=NF
      F_COEF(IAUX)=AUX**JF
      F_PROJ(IAUX)=0.0D0
    ENDDO
    !
    DO JT=LBUF,1,-1
      T_POIN(JT)=NT-MOD(LBUF-JT,NT)
    ENDDO
    DO JT=1,NT
      T_POIN(LBUF+JT)=JT
    ENDDO
    DO JT=1,LBUF
      T_POIN(LBUF+NT+JT)=MOD(JT-1,NT)+1
    ENDDO
    !======================================================================
    !
    !=======================================================================
    !     COMPUTES SCALE COEFFICIENTS FOR THE COUPLING COEFFICIENT
    !     Would be easier to pass these on from W3SRCE ???
    !=======================================================================
    DP2SG=TWOPI*TWOPI/GRAV
    DO JF=1,LBUF
      AUX=FREQ(1)/RAISF**(LBUF-JF+1)
      TB_SCA(JF)=(DP2SG*AUX**2)**6/(TWOPI**3*AUX)
    ENDDO
    DO JF=1,NF
      TB_SCA(LBUF+JF)=(DP2SG*FREQ(JF)**2)**6/(TWOPI**3*FREQ(JF))
    ENDDO
    DO JF=1,LBUF
      IAUX=LBUF+NF+JF
      AUX=FREQ(NF)*RAISF**JF
      TB_SCA(IAUX)=(DP2SG*AUX**2)**6/(TWOPI**3*AUX)
    ENDDO
    !=======================================================================
    !
    !=======================================================================
    !     COMPUTES VALUES FOR GAUSSIAN QUADRATURES
    !=======================================================================
    if (Allocated(X_CHE_TE1) ) then
      deallocate(X_CHE_TE1)
    endif
    ALLOCATE(X_CHE_TE1(1:NQ_TE1),X_CHE_OM2(1:GQNQ_OM2))

    if (Allocated(X_LEG_OM2) ) then
      deallocate(X_LEG_OM2)
    endif
    ALLOCATE(X_LEG_OM2(1:GQNQ_OM2),W_LEG_OM2(1:GQNQ_OM2))
    !
    !.....Abscissa and weight (constant) for Gauss-Chebyshev
    DO IQ_TE1=1,NQ_TE1
      X_CHE_TE1(IQ_TE1)=COS(PI*(DBLE(IQ_TE1)-0.5D0)/DBLE(NQ_TE1))
    ENDDO
    W_CHE_TE1=PI/DBLE(NQ_TE1)
    DO IQ_OM2=1,GQNQ_OM2
      X_CHE_OM2(IQ_OM2)=COS(PI*(DBLE(IQ_OM2)-0.5D0)/DBLE(GQNQ_OM2))
    ENDDO
    W_CHE_OM2=PI/DBLE(GQNQ_OM2)
    !
    !.....Abscissa et weight for Gauss-Legendre
    CALL GAULEG( W_LEG_OM2 , X_LEG_OM2 , GQNQ_OM2 )
    DO IQ_OM2=1,GQNQ_OM2
      X_LEG_OM2(IQ_OM2)=0.25D0*(1.D0+X_LEG_OM2(IQ_OM2))**2
    ENDDO
    !=======================================================================
    !
    !
    !=======================================================================
    !     COMPUTES VALUES OF RATIO F1/F AS FUNCTION OF THE IQ_OM1 INDICATOR
    !=======================================================================
    NF1P1=GQNF1+1
    if (Allocated(F1SF) ) then
      deallocate(F1SF)
    endif
    ALLOCATE(F1SF(1:NF1P1))

    CALL F1F1F1 ( F1SF  , GQNF1   , IQ_OM1)
    !=======================================================================
    !
    !     ==================================================
    !     STARTS LOOP 1 OVER THE RATIOS F1/F0
    !     ==================================================
    DO JF1=1,GQNF1
      !       ---------Computes and stores v1=f1/f0 and v1**4
      V1=(F1SF(JF1+1)+F1SF(JF1))/2.D0
      K_IF1(JF1)=NINT(DBLE(LBUF)+LOG(V1)/LOG(RAISF))
      V1_4=V1**4
      TB_V14(JF1)=V1_4
      !       ---------Computes and stores dv1=df1/f0
      DV1=F1SF(JF1+1)-F1SF(JF1)
      !       ---------Computes the A parameter
      AAA=((1.D0+V1)**4-4.D0*(1.D0+V1_4))/(8.D0*V1**2)
      !
      !       =================================================
      !       STARTS LOOP 2 OVER THE DELTA_1+ VALUES
      !       =================================================
      DO JT1=1,GQNT1
        !
        !......Computes the Delta1+ values (=Theta_1-Theta_0) between 0 and Pi.
        IF (JT1.LE.NQ_TE1) THEN
          !           ---------First interval : X from -1 to A
          IQ_TE1=JT1
          C_D01P=(-1.D0+AAA)/2.D0+(1.D0+AAA)/2.D0*X_CHE_TE1(IQ_TE1)
          CCC=DV1*SQRT((AAA-C_D01P)/(1.D0-C_D01P))*W_CHE_TE1
        ELSE
          !           ---------Second interval : X from A to 1
          IQ_TE1=JT1-NQ_TE1
          C_D01P=( 1.D0+AAA)/2.D0+(1.D0-AAA)/2.D0*X_CHE_TE1(IQ_TE1)
          CCC=DV1*SQRT((C_D01P-AAA)/(1.D0+C_D01P))*W_CHE_TE1
        ENDIF
        S_D01P=SQRT(1.D0-C_D01P*C_D01P)
        D01P  =ACOS(C_D01P)
        K_1P(JT1,JF1)=LBUF+NINT(D01P/DTETAR)
        K_1M(JT1,JF1)=LBUF-NINT(D01P/DTETAR)
        !
        !         ---------Computes Epsilon_a
        EPSI_A=2.D0*SQRT(1.D0+V1_4+2.D0*V1*V1*C_D01P)/(1.D0+V1)**2
        !         ---------Computes Delta_A+ and its cosinus
        C_D0AP=(1.D0-V1_4+0.25D0*EPSI_A**2*(1.D0+V1)**4) &
             /(EPSI_A*(1.D0+V1)**2)
        S_D0AP=SQRT(1.0D0-C_D0AP*C_D0AP)
        D0AP  = ACOS(C_D0AP)
        !
        !.......Integration over OMEGA2 depending on EPS_A
        IF (EPSI_A.LT.1.D0) THEN
          !        - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          !........Case of a single singularity (in OMEGA2-)
          !        - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          W2_M=0.5D0*(1.D0-EPSI_A/2.D0)
          W2_1=0.5D0
          !
          W_RAD=W2_1-W2_M
          C_LEG_OM2=SQRT(W_RAD)
          !
          !        ----------------------------------------------------
          !........STARTS LOOP 3 OVER OMEGA_2 (CASE Epsilon_A < 1)
          !........Case of a single singularity (in OMEGA2-)
          !........Integration over OMEGA2 via GAUSS-LEGENDRE quadrature
          !        ----------------------------------------------------
          DO IQ_OM2=1,GQNQ_OM2
            !             ---------Computes W2, V2, and V3
            W2=W2_M+W_RAD*X_LEG_OM2(IQ_OM2)
            V2=W2*(1.D0+V1)
            V2_4=V2**4
            TB_V24(IQ_OM2,JT1,JF1)=V2_4
            K_IF2 (IQ_OM2,JT1,JF1) = NINT(DBLE(LBUF) &
                 + LOG(V2)/LOG(RAISF))
            V3=1.D0+V1-V2
            V3_4=V3**4
            TB_V34(IQ_OM2,JT1,JF1)=V3_4
            K_IF3 (IQ_OM2,JT1,JF1) = NINT(DBLE(LBUF) &
                 + LOG(V3)/LOG(RAISF))
            !             ---------Computes Gamma_2+ et Gamma_3+ angles
            C_GA2P=(EPSI_A**2/4.D0+W2**4-(1.D0-W2)**4)/(EPSI_A*W2*W2)
            C_GA2P=MAX(MIN(C_GA2P,1.D0),-1.D0)
            S_GA2P=SQRT(1.D0-C_GA2P*C_GA2P)
            GA2P  =ACOS(C_GA2P)
            C_GA3P=(EPSI_A**2/4.D0-W2**4+(1.D0-W2)**4)/EPSI_A &
                 /(1.D0-W2)**2
            C_GA3P=MAX(MIN(C_GA3P,1.D0),-1.D0)
            S_GA3P=SQRT(1.D0-C_GA3P*C_GA3P)
            GA3P  =ACOS(C_GA3P)
            !             Shifting of the direction indexes - Config. +Delta1 (SIG=1)
            K_1P2P(IQ_OM2,JT1,JF1)=NINT(( D0AP+GA2P)/DTETAR &
                 +DBLE(LBUF))
            K_1P3M(IQ_OM2,JT1,JF1)=NINT(( D0AP-GA3P)/DTETAR &
                 +DBLE(LBUF))
            K_1P2M(IQ_OM2,JT1,JF1)=NINT(( D0AP-GA2P)/DTETAR &
                 +DBLE(LBUF))
            K_1P3P(IQ_OM2,JT1,JF1)=NINT(( D0AP+GA3P)/DTETAR &
                 +DBLE(LBUF))
            !             Shifting of the direction indexes - Config. -Delta1 (SIG=-1)
            K_1M2P(IQ_OM2,JT1,JF1)=NINT((-D0AP+GA2P)/DTETAR &
                 +DBLE(LBUF))
            K_1M3M(IQ_OM2,JT1,JF1)=NINT((-D0AP-GA3P)/DTETAR &
                 +DBLE(LBUF))
            K_1M2M(IQ_OM2,JT1,JF1)=NINT((-D0AP-GA2P)/DTETAR &
                 +DBLE(LBUF))
            K_1M3P(IQ_OM2,JT1,JF1)=NINT((-D0AP+GA3P)/DTETAR &
                 +DBLE(LBUF))
            !
            !.........Computes the coupling coefficients (only for Delta_1+ )
            RK0=1.D0
            RK1=V1*V1
            RK2=V2*V2
            RK3=(1.D0+V1-V2)**2
            XK0  = RK0
            YK0  = 0.0D0
            XK1  = RK1*C_D01P
            YK1  = RK1*S_D01P
            XK2P = RK2*(C_D0AP*C_GA2P-S_D0AP*S_GA2P)
            YK2P = RK2*(S_D0AP*C_GA2P+C_D0AP*S_GA2P)
            XK2M = RK2*(C_D0AP*C_GA2P+S_D0AP*S_GA2P)
            YK2M = RK2*(S_D0AP*C_GA2P-C_D0AP*S_GA2P)
            XK3P = RK3*(C_D0AP*C_GA3P-S_D0AP*S_GA3P)
            YK3P = RK3*(S_D0AP*C_GA3P+C_D0AP*S_GA3P)
            XK3M = RK3*(C_D0AP*C_GA3P+S_D0AP*S_GA3P)
            YK3M = RK3*(S_D0AP*C_GA3P-C_D0AP*S_GA3P)
            TB_TPM(IQ_OM2,JT1,JF1)=COUPLE( XK0   , YK0   , XK1   , YK1   , XK2P  , YK2P  , XK3M  , YK3M)
            TB_TMP(IQ_OM2,JT1,JF1)=COUPLE( XK0   , YK0   , XK1   , YK1   , XK2M  , YK2M  , XK3P  , YK3P)
            !
            !.........Computes the multiplicative coefficient for QNL4
            DENO=2.D0*SQRT( (0.5D0*(1.D0+EPSI_A/2.D0)-W2) &
                 *((W2-0.5D0)**2+0.25D0*(1.D0+EPSI_A)) &
                 *((W2-0.5D0)**2+0.25D0*(1.D0-EPSI_A)) )
            TB_FAC(IQ_OM2,JT1,JF1)=1.D0/(DENO*V1*W2*(1.D0-W2)) &
                 /(1.D0+V1)**5 * W_LEG_OM2(IQ_OM2)*C_LEG_OM2* CCC
          ENDDO
          !        -----------------------------------------------
          !........END OF THE LOOP 3 OVER OMEGA_2 (CASE Epsilon_A < 1)
          !        -----------------------------------------------
          !
        ELSE
          !        - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          !........STARTS LOOP 3 OVER OMEGA_2 (CASE Epsilon_A > 1)
          !........Case of two singularities (in OMEGA2- and OMEGA2_1)
          !........Integration over OMEGA2 via GAUSS-CHEBYSCHEV quadrature
          !        - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          W2_M=0.5D0*(1.D0-EPSI_A/2.D0)
          W2_1=0.5D0*(1.D0-SQRT(EPSI_A-1.D0))
          !
          W_MIL=(W2_M+W2_1)/2.D0
          W_RAD=(W2_1-W2_M)/2.D0
          !
          DO IQ_OM2=1,GQNQ_OM2
            !             ---------Computes W2, V2, and V3
            W2=W_MIL+W_RAD*X_CHE_OM2(IQ_OM2)
            V2=W2*(1.D0+V1)
            V2_4=V2**4
            TB_V24(IQ_OM2,JT1,JF1)=V2_4
            K_IF2 (IQ_OM2,JT1,JF1)=NINT(DBLE(LBUF) &
                 +LOG(V2)/LOG(RAISF))
            V3=1.D0+V1-V2
            V3_4=V3**4
            TB_V34(IQ_OM2,JT1,JF1)=V3_4
            K_IF3 (IQ_OM2,JT1,JF1)=NINT(DBLE(LBUF) &
                 +LOG(V3)/LOG(RAISF))
            !             ---------Computes Gamma_2+ et Gamma_3+ angles
            C_GA2P=(EPSI_A**2/4.D0+W2**4-(1.D0-W2)**4)/(EPSI_A*W2*W2)
            C_GA2P=MAX(MIN(C_GA2P,1.D0),-1.D0)
            S_GA2P=SQRT(1.D0-C_GA2P*C_GA2P)
            GA2P  =ACOS(C_GA2P)
            C_GA3P=(EPSI_A**2/4.D0-W2**4+(1.D0-W2)**4)/EPSI_A &
                 /(1.D0-W2)**2
            C_GA3P=MAX(MIN(C_GA3P,1.D0),-1.D0)
            S_GA3P=SQRT(1.D0-C_GA3P*C_GA3P)
            GA3P  =ACOS(C_GA3P)
            !             Shifts the direction indexes - Config. +Delta1 (SIG=1)
            K_1P2P(IQ_OM2,JT1,JF1)=NINT(( D0AP+GA2P)/DTETAR &
                 +DBLE(LBUF))
            K_1P3M(IQ_OM2,JT1,JF1)=NINT(( D0AP-GA3P)/DTETAR &
                 +DBLE(LBUF))
            K_1P2M(IQ_OM2,JT1,JF1)=NINT(( D0AP-GA2P)/DTETAR &
                 +DBLE(LBUF))
            K_1P3P(IQ_OM2,JT1,JF1)=NINT(( D0AP+GA3P)/DTETAR &
                 +DBLE(LBUF))
            !             Shifts the direction indexes - Config. -Delta1 (SIG=-1)
            K_1M2P(IQ_OM2,JT1,JF1)=NINT((-D0AP+GA2P)/DTETAR &
                 +DBLE(LBUF))
            K_1M3M(IQ_OM2,JT1,JF1)=NINT((-D0AP-GA3P)/DTETAR &
                 +DBLE(LBUF))
            K_1M2M(IQ_OM2,JT1,JF1)=NINT((-D0AP-GA2P)/DTETAR &
                 +DBLE(LBUF))
            K_1M3P(IQ_OM2,JT1,JF1)=NINT((-D0AP+GA3P)/DTETAR &
                 +DBLE(LBUF))
            !
            !.........Computes the coupling coefficients (only for Delta_1+ )
            RK0=1.D0
            RK1=V1*V1
            RK2=V2*V2
            RK3=(1.D0+V1-V2)**2
            XK0  = RK0
            YK0  = 0.0D0
            XK1  = RK1*C_D01P
            YK1  = RK1*S_D01P
            XK2P = RK2*(C_D0AP*C_GA2P-S_D0AP*S_GA2P)
            YK2P = RK2*(S_D0AP*C_GA2P+C_D0AP*S_GA2P)
            XK2M = RK2*(C_D0AP*C_GA2P+S_D0AP*S_GA2P)
            YK2M = RK2*(S_D0AP*C_GA2P-C_D0AP*S_GA2P)
            XK3P = RK3*(C_D0AP*C_GA3P-S_D0AP*S_GA3P)
            YK3P = RK3*(S_D0AP*C_GA3P+C_D0AP*S_GA3P)
            XK3M = RK3*(C_D0AP*C_GA3P+S_D0AP*S_GA3P)
            YK3M = RK3*(S_D0AP*C_GA3P-C_D0AP*S_GA3P)
            TB_TPM(IQ_OM2,JT1,JF1)=COUPLE( XK0   , YK0   , XK1   , YK1   , XK2P  , YK2P  , XK3M  , YK3M)
            TB_TMP(IQ_OM2,JT1,JF1)=COUPLE( XK0   , YK0   , XK1   , YK1   , XK2M  , YK2M  , XK3P  , YK3P)
            !
            !.........Computes the multiplicative coefficient for QNL4
            DENO=2.D0*SQRT( (0.5D0*(1.D0+EPSI_A/2.D0)-W2) &
                 *((W2-0.5D0)**2+0.25D0*(1.D0+EPSI_A)) &
                 *(0.5D0*(1.D0+SQRT(EPSI_A-1.D0))-W2) )
            TB_FAC(IQ_OM2,JT1,JF1)=1.D0/(DENO*V1*W2*(1.D0-W2)) &
                 /(1.D0+V1)**5 * W_CHE_OM2* CCC
            !
          ENDDO
          !        -----------------------------------------------
          !........END OF LOOP 3 OVER OMEGA_2 (CASE Epsilon_A > 1)
          !        -----------------------------------------------
          !
        ENDIF
      ENDDO
      !       =================================================
      !       END OF LOOP 2 OVER THE DELTA_1+ VALUES
      !       =================================================
      !
    ENDDO
    !     ==================================================
    !     END OF LOOP 1 OVER THE F1/F0 RATIOS
    !     ==================================================
    DEALLOCATE(F1SF)
    DEALLOCATE(X_CHE_TE1)
    DEALLOCATE(X_CHE_OM2)
    DEALLOCATE(X_LEG_OM2)
    DEALLOCATE(W_LEG_OM2)

    !     ===========================================================
    !     POST-PROCESSING TO ELIMINATE PART OF THE CONFIGURATIONS
    !     ===========================================================
    !
    !.....It looks, for every value of the ratio V1, for the maximum value
    !.....of FACTOR*COUPLING : it is stored in the local table NAXCLA(.)
    !     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ALLOCATE(MAXCLA(1:GQNF1))
    DO JF1=1,GQNF1
      AUX=0.0D0
      DO JT1=1,GQNT1
        DO IQ_OM2=1,GQNQ_OM2
          AUX=MAX(AUX,TB_FAC(IQ_OM2,JT1,JF1)*TB_TPM(IQ_OM2,JT1,JF1),TB_FAC(IQ_OM2,JT1,JF1)*TB_TMP(IQ_OM2,JT1,JF1))
        ENDDO
      ENDDO
      MAXCLA(JF1)=AUX
    ENDDO
    !
    !.....It looks for the max V1 value
    !     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    AUX=0.0D0
    DO JF1=1,GQNF1
      IF (MAXCLA(JF1).GT.AUX) AUX=MAXCLA(JF1)
    ENDDO

    TEST1=SEUIL1*AUX
    !
    !.....Set to zero the coupling coefficients not used
    !     """""""""""""""""""""""""""""""""""""""""""""""""""""
    NCONF=0
    DO JF1=1,GQNF1
      TEST2 =SEUIL2*MAXCLA(JF1)
      DO JT1=1,GQNT1
        DO IQ_OM2=1,GQNQ_OM2
          AAA=TB_FAC(IQ_OM2,JT1,JF1)*TB_TPM(IQ_OM2,JT1,JF1)
          CCC=TB_FAC(IQ_OM2,JT1,JF1)*TB_TMP(IQ_OM2,JT1,JF1)
          IF ((AAA.GT.TEST1.OR.AAA.GT.TEST2).OR. &
               (CCC.GT.TEST1.OR.CCC.GT.TEST2)) THEN
            NCONF=NCONF+1
            IDCONF(NCONF,1)=JF1
            IDCONF(NCONF,2)=JT1
            IDCONF(NCONF,3)=IQ_OM2
          ENDIF
#ifdef W3_TGQM
          WRITE(993,*) NCONF,JF1,JT1,IQ_OM2,AAA,CCC,(AAA.GT.TEST1.OR.AAA.GT.TEST2), &
               (CCC.GT.TEST1.OR.CCC.GT.TEST2)
#endif
        ENDDO
      ENDDO
    ENDDO
    DEALLOCATE(MAXCLA)
    !
    !..... counts the fraction of the eliminated configurations
    ELIM=(1.D0-DBLE(NCONF)/DBLE(NCONFM))*100.D0
#ifdef W3_TGQM
    WRITE(994,*) 'NCONF, ELIM FRACTION:',NCONF,ELIM
#endif
  END SUBROUTINE INSNLGQM
  !/
  !/ End of module W3SNL1MD -------------------------------------------- /
  !/
END MODULE W3SNL1MD
