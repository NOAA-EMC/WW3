#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3SIC5MD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |           E. Rogers               |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         19-May-2021 |
!/                  +-----------------------------------+
!/
!/    15-Mar-2016 : Origination.                        ( version 5.10 )
!/                                                      ( Q. Liu )
!/    15-Mar-2016 : Started from w3sic1/2/3/4 module    ( Q. Liu )
!/
!/    24-Apr-2017 : Adding more filters                 ( Q. Liu )
!/
!/    29-Apr-2017 : Introducing CMPLX_TANH2             ( Q. Liu )
!/
!/    02-Jun-2017 : Update to version 5.16              ( Q. Liu )
!/
!/    17-Jun-2017 : Remove some unnecessary lines       ( Q. Liu )
!/                 (cg_ice, detla function, complx_tanh etc.)
!/
!/    20-Aug-2018 : Ready to be merged to master (v6.06)( Q. Liu)
!/
!/    19-May-2021 : Incl. the RP and M2 model           ( Q. Liu)
!/
!/ 1. Purpose :
!     Calculate ice source term S_{ice} according to different ice
!     models:
!     * 'FS': the viscoelastic, extended Fox and Squire sea ice model
!             (Mosig et al. 2015)
!     * 'RP': the viscoelastic, Robinson and Palmer model (Mosig et al.
!             2015)
!     * 'M2': the order 3 power law model proposed by Meylan et al.
!             (2018)
!
!     Reference:
!     Mosig, J.E.M., F. Montiel, and V. A. Squire (2015):
!     Comparison of viscoelastic-type models for ocean wave attenuation
!     in ice-covered seas, J. Geophys. Res. Oceans, 120, 6072–6090,
!     doi:10.1002/2015JC010881.
!
!     Meylan, M.H., L. Bennetts, J. Mosig, W. Rogers, M. Doble, and
!     M. Peter (2018): Dispersion relations, power laws, and energy loss
!     for waves in the marginal ice zone. J. Geophys. Res. Oceans, 123,
!     3322–3335, https://doi.org/10.1002/2018JC013776.
!
!     Liu, Q., W. E. Rogers, A. Babanin, J. Li, and C. Guan (2020):
!     Spectral Modeling of Ice-Induced Wave Decay. J. Phys. Oceanogr.,
!     50 (6), 1583–1604.
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      KSP       Int.  Private  the kind parameter for single precision
!                               real variables
!      KDP       Int.  Private  Same as KSP but for double precision
!      KSPC      Int.  Private  the kind parameter for single precision
!                               complex variables
!      KDPC      Int.  Private  Same as KSPC but for double precision
!      ERRTOL    Real  Private  A real parameter used for "==" test
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3SIC5    Subr. Public   Ice source term
!      W3IC5WNCG Subr. Public   Wavenumber and group velocity of ice-
!                               coupled waves
!      FSDISP    Subr. Public   Solving the ice-coupled wave dispersion
!      BALANCING_MATRIX
!                Subr. Private  Balancing the matrix before we try to
!                               find its eigenvalues
!      EIG_HQR   Subr. Private  QR algorithm for real Hessenberg matrix
!                               (eigenvalues-finding algorithm)
!      POLYROOTS Subr. Private  Finding roots of a general polynomial
!      NR_CORR   Func. Private  Get the Newton-Raphson correction term
!                               for iteration
!      NR_ROOT   Func. Private  Newton-Raphson algorithm for solving
!                               the ice-coupled wave dispersion
!      CMPLX_SINH, CMPLX_COSH, CMPLX_TANH2
!                Func. Private  sinh, cosh, tanh for complex inputs
!      INIT_RANDOM_SEED
!                Subr. Private  Initialize the random seed based on
!                               the system's time
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!      See subroutine documentation
!
!  5. Remarks :
!
!  6. Switches :
!      See subroutine documentation
!
!  7. Source code:
!/
!/ ------------------------------------------------------------------- /
      IMPLICIT NONE
!/
      PUBLIC  :: W3SIC5, W3IC5WNCG, FSDISP
      PRIVATE :: BALANCING_MATRIX, EIG_HQR, POLYROOTS
      PRIVATE :: NR_CORR, NR_ROOT
      PRIVATE :: CMPLX_SINH, CMPLX_COSH, CMPLX_TANH2
      PRIVATE :: INIT_RANDOM_SEED
!/
      PRIVATE :: KSP, KDP, KSPC, KDPC, ERRTOL
!/ ------------------------------------------------------------------- /
!/ Parameter list
!     Kind for single- and double-precision real type
      INTEGER, PARAMETER :: KSP    = KIND(1.0)
      INTEGER, PARAMETER :: KDP    = KIND(1.0D0)
!
!     Kind for single- and double-precision complex type
      INTEGER, PARAMETER :: KSPC   = KIND((1.0, 1.0))
      INTEGER, PARAMETER :: KDPC   = KIND((1.0D0, 1.0D0))
      REAL, PARAMETER    :: ERRTOL = 1.E-12
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
!/
      SUBROUTINE W3SIC5 (A, DEPTH, CG, WN, IX, IY, S, D)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |           E. Rogers               |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         19-May-2021 |
!/                  +-----------------------------------+
!/
!/    23-Mar-2016 : Origination                         ( version 5.10 )
!/                                                      ( Q. Liu)
!/    23-Mar-2016 : Started from w3sic1/2/3/4 subr.     ( Q. Liu)
!/    05-Apr-2016 : Options for Cg_{ice} or Cg          ( Q. Liu)
!/    25-Apr-2017 : Add more filters                    ( Q. Liu)
!/    20-Aug-2018 : Ready to be merged to master (v6.06)( Q. Liu)
!/
!/    Copyright 2009 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!/ 1. Purpose :
!     Calculate ice source term S_{ice} according to 3 different sea ice
!     models (Mosig et al. 2015, Meylan et al. 2018, Liu et al. 2020)
!
!  2. Method :
!     Regarding i/o (general to all Sice modules): S_{ice} source term
!     is calculated using up to 5 parameters read from input files.
!     These parameters are allowed to vary in space and time.
!     The parameters control the exponential decay rate k_i.
!     Since there are 5 parameters, this permits description of
!     dependence of k_i on frequency or wavenumber.
!
!     Sea ice affects the wavenumber k of wind-generated ocean waves.
!     The ice-modified wavenumber can be expressed as a complex number
!     k = k_r + i * k_i, with the real part k_r representing impact of
!     the sea ice on the physical wavelength and propagation speeds,
!     producing something analogous to shoaling and refraction by
!     bathymetry, whereas the imaginary part of the complex
!     wavenumber, k_i, is an exponential decay coefficient
!     k_i(x,y,t,sigma) (depending on location, time and frequency,
!     respectively), representing wave attenuation, and can be
!     introduced in a wave model such as WW3 as S_ice/E=-2*Cg*k_i,
!     where S_ice is one of several dissipation mechanisms, along
!     with whitecapping, for example, S_ds=S_wc+S_ice+⋯. The k_r -
!     modified by ice would enter the model via the C calculations
!     on the left-hand side of the governing equation.The fundamentals
!     are straightforward, e.g. Rogers and Holland (2009 and
!     subsequent unpublished work) modified a similar model, SWAN
!     (Booij et al. 1999) to include the effects of a viscous mud
!     layer using the same approach (k = k_r + i*k_i) previously.
!
!     General approach is analogous to Rogers and Holland (2009)
!         approach for mud.
!     See text near their eq. 1 :
!       k        = k_r  +  i * k_i
!       eta(x,t) = Real( a * exp( i * ( k * x - sigma * t ) ) )
!       a        = a0 * exp( -k_i * x )
!       S / E    = -2 * Cg * k_i (see also Komen et al. (1994, pg. 170)
!
!     Following W3SBT1 as a guide, equation 1 of W3SBT1 says:
!         S = D * E
!     However, the code of W3SBT1 has
!         S = D * A
!     This leads me to believe that the calling routine is
!         expecting "S/sigma" not "S"
!     Thus we will use D = S/E = -2 * Cg * k_i
!        (see also documentation of W3SIC1)
!
!     Notes regarding numerics:
!     -------------------------
!     Experiments with constant k_i values suggest that results may be
!        dependent on resolution if insufficient resolution is used.
!        For detailed information, see documentation of W3SIC1.
!
!     Note regarding applicability/validity:
!     --------------------------------------
!     Similar to the Wang and Shen model used in w3sic3md, the 3 models
!     used here are empirical medium models as well which treat the sea
!     ice cover as a continuum and use 1/2 empirical rheological para-
!     meters, i.e., the effective shear modulus of ice G and the effec-
!     tive viscosity η to characterize sea ices of various type. Please
!     see the documentation of w3sic3md for a detailed discussion of
!     this kind of model.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       A       R.A.  I   Action density spectrum (1-D).
!       DEPTH   Real  I   Local water depth.
!       CG      R.A.  I   Group velocities.
!       WN      R.A.  I   Wavenumbers
!       IX,IY   I.S.  I   Grid indices.
!       S       R.A.  O   Source term (1-D version).
!       D       R.A.  O   Diagonal term of derivative (1-D version).
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing (!/S switch).
!      PRT2DS    Subr. W3ARRYMD Print plot output (!/T0 switch).
!      OUTMAT    Subr. W3ARRYMD Matrix output (!/T1 switch).
!      W3IC5WNCG Subr. /        Wavenumber and group velocity of ice-
!                               coupled waves
!     ----------------------------------------------------------------
!      * / means this module
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SRCE    Subr. W3SRCEMD Source term integration.
!      W3EXPO    Subr.   N/A    ASCII Point output post-processor.
!      W3EXNC    Subr.   N/A    NetCDF Point output post-processor.
!      GXEXPO    Subr.   N/A    GrADS point output post-processor.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     None.
!
!  7. Remarks :
!
!     If ice parameter 1 is zero, no calculations are made.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!     !/T   Enable general test output.
!     !/T0  2-D print plot of source term.
!     !/T1  Print arrays.
!
! 10. Source code :
!/ ------------------------------------------------------------------- /
!/
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
      USE CONSTANTS, ONLY: TPI
      USE W3SERVMD,  ONLY: EXTCDE
      USE W3ODATMD,  ONLY: NDSE, IAPROC, NAPROC, NAPERR
      USE W3GDATMD,  ONLY: NK, NTH, NSPEC, SIG, MAPWN, IC5PARS
      USE W3IDATMD,  ONLY: INFLAGS2, ICEP1, ICEP2, ICEP3, ICEP4, ICEI
!
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(IN)        :: CG(NK), WN(NK), A(NSPEC), DEPTH
      REAL, INTENT(OUT)       :: S(NSPEC), D(NSPEC)
      INTEGER, INTENT(IN)     :: IX, IY
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T0
      INTEGER                :: ITH
      REAL                   :: DOUT(NK,NTH)
#endif
!/
      REAL                    :: ICECOEF1, ICECOEF2, ICECOEF3, &
                                 ICECOEF4, ICECONC
      REAL, DIMENSION(NK)     :: D1D, WN_R, WN_I
!     REAL                    :: TWN_R, TWN_I
      INTEGER                 :: IK, IKTH
      LOGICAL                 :: NOICE
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3SIC5')
#endif
!
! 0.  Initializations ------------------------------------------------ /
      D        = 0.
      D1D      = 0.
      WN_R     = 0.
      WN_I     = 0.
!
      ICECOEF1 = 0.
      ICECOEF2 = 0.
      ICECOEF3 = 0.
      ICECOEF4 = 0.
      ICECONC  = 0.
!
! Set the ice parameters from input
      IF (INFLAGS2(-7)) THEN
          ICECOEF1 = ICEP1(IX, IY) ! ice thickness h_i
      ELSE
          IF ( IAPROC .EQ. NAPERR )                       &
          WRITE (NDSE,1001) 'ICE PARAMETER 1 (HICE)'
          CALL EXTCDE(2)
      ENDIF
!
      IF (INFLAGS2(-6)) THEN
          ICECOEF2 = ICEP2(IX, IY) ! effective viscosity of ice η
      ELSE
          IF ( IAPROC .EQ. NAPERR )                       &
          WRITE (NDSE,1001) 'ICE PARAMETER 2 (VISC)'
          CALL EXTCDE(2)
      ENDIF
!
      IF (INFLAGS2(-5)) THEN
          ICECOEF3 = ICEP3(IX, IY) ! density of ice ρ_i
      ELSE
          IF ( IAPROC .EQ. NAPERR )                       &
          WRITE (NDSE,1001) 'ICE PARAMETER 3 (DENS)'
          CALL EXTCDE(2)
      ENDIF
!
      IF (INFLAGS2(-4)) THEN
          ICECOEF4 = ICEP4(IX, IY) ! effective shear modulus of ice G
      ELSE
          IF ( IAPROC .EQ. NAPERR )                       &
          WRITE (NDSE,1001) 'ICE PARAMETER 4 (ELAS)'
          CALL EXTCDE(2)
      ENDIF
!
      IF (INFLAGS2(4)) ICECONC = ICEI(IX, IY) ! ice concentration
!
! 1. No ice --------------------------------------------------------- /
      NOICE = .FALSE.
!     Zero ice thickness
!     Very small ice thickness may cause problems in POLYROOTS because
!     the first coefficient C1 may be very close to zero. So we regard
!     cases where hice is less than 0.0001 as no ice.
!     IF (ICECOEF1 < ERRTOL) NOICE = .TRUE.
      IF (ICECOEF1 < 0.0001) NOICE = .TRUE.
!     zero ice concentration
      IF (INFLAGS2(4) .AND. ICECONC < ERRTOL) NOICE = .TRUE.
!
! Calculate the decay rate k_i
      IF ( NOICE ) THEN
          D1D = 0.
!
! 2. Ice ------------------------------------------------------------- /
      ELSE
!         W3IC5WNCG(WN_R, WN_I, CG, HICE, IVISC, RHOI, ISMODG, HWAT)
          CALL W3IC5WNCG(WN_R, WN_I, CG, ICECOEF1, ICECOEF2, &
                         ICECOEF3, ICECOEF4, DEPTH)
! recall that D=S/E=-2*Cg_{ice}*k_i
! In some cases, the FS model yields very large Cg_{ice}, which
! subquently may result in numerical failure due to the violation of CFL
! conditions, therefore we still use ice-free group velocity to advect
! wave packets.
!
          DO IK = 1, NK
              D1D(IK) = -2.0 * CG(IK) * WN_I(IK)
          END DO
      END IF
!
! 2.1 Fill diagonal matrix
      DO IKTH = 1, NSPEC
          D(IKTH) = D1D(MAPWN(IKTH))
      END DO

      S = D * A
!
! ... Test output of arrays
!
#ifdef W3_T0
      DO IK=1, NK
        DO ITH=1, NTH
          DOUT(IK,ITH) = D(ITH+(IK-1)*NTH)
          END DO
        END DO
#endif
!
#ifdef W3_T0
      CALL PRT2DS (NDST, NK, NK, NTH, DOUT, SIG(1:), '  ', 1.,    &
                         0.0, 0.001, 'Diag Sice', ' ', 'NONAME')
#endif
!
#ifdef W3_T1
      CALL OUTMAT (NDST, D, NTH, NTH, NK, 'diag Sice')
#endif
!
! Formats
!
 1001 FORMAT(/' *** WAVEWATCH III ERROR IN W3SIC5MD : '/   &
              '     ',A,' IS NOT DEFINED IN ww3_shel.inp.')

!/
!/ End of W3SIC5------------------------------------------------------ /
!/
      END SUBROUTINE W3SIC5
!/ ------------------------------------------------------------------- /
!/
      SUBROUTINE W3IC5WNCG(WN_R, WN_I, CG, HICE, IVISC, RHOI, ISMODG, &
                           HWAT)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |           E. Rogers               |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         25-Apr-2017 |
!/                  +-----------------------------------+
!/
!/    17-Apr-2016 : Origination                         ( version 5.10)
!/                                                      ( Q. Liu )
!/    17-Apr-2016 : Start from W3IC3WNCG_CHENG          ( Q. Liu )
!/
!/ 1. Purpose:
!     Calculation of complex wavenumber arrays for ice-coupled waves.
!
!     This also allows us to use Cg_ice in the advection part of the
!     radiative transfer energy equation (RTE). --- abandoned in the end
!
!  2. Method:
!     Using the  Fox-Squire dispersion relations to get (kr, ki) and
!     then get cg by cg = dσ / dk (here dk uses kr)
!
!  3. Parameters:
!
!     Parameter list:
!     ----------------------------------------------------------------
!     Name     Type   Intent   Description
!     ----------------------------------------------------------------
!     WN_R     R.A.   I/O      the real. part of the wave number
!     WN_I     R.A.   I/O      the imag. part of the wave number
!     CG       R.A.   I        group velocity (m s^{-1})
!     HICE     Real.  I        thickness of ice (m)
!     IVISC    Real.  I        viscosity parameter of ice (m^2 s^{-1})
!     RHOI     Real.  I        the density of ice (kg m^{-3})
!     ISMODG   Real.  I        effecitive shear modulus G of ice (Pa)
!     HWAT     Real.  I        water depth
!     ----------------------------------------------------------------
!     * the intent of WN_R/I must be inout
!     * CG is unchanged but still kept here because some legacy reasons.
!
!  4. Subroutines used:
!
!     Name      Type  Module   Description
!     ----------------------------------------------------------------
!     FSDISP    Subr. /        dispersion relations for ice-coupled waves
!     CGINICE5  Subr. /        group velocity for given (σ, kr) array
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Name      Type  Module   Description
!     ----------------------------------------------------------------
!     W3SIC5    Subr. Public   Ice source term
!     W3WAVE    Subr. W3WAVEMD WW3 integration
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
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
      USE CONSTANTS, ONLY: TPI
      USE W3GDATMD,  ONLY: NK, SIG
      USE W3ODATMD,  ONLY: NDSE, IAPROC, NAPROC, NAPERR
      USE W3SERVMD,  ONLY: EXTCDE
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
      REAL, INTENT(INOUT)   :: WN_R(:), WN_I(:)
      REAL, INTENT(IN)      :: CG(:)
      REAL, INTENT(IN)      :: HICE, IVISC, RHOI, ISMODG, HWAT
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
      REAL, ALLOCATABLE     :: SIGMA(:)
      INTEGER               :: KL, KU, IK
      REAL                  :: TWN_R, TWN_I
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3IC5WNCG')
#endif
!/
! Initialize SIGMA {in w3gdatmd: SIG (0: NK+1)}
      IF (ALLOCATED(SIGMA)) DEALLOCATE(SIGMA); ALLOCATE(SIGMA(SIZE(CG)))
      SIGMA = 0.

      IF (SIZE(WN_R, 1) .EQ. NK) THEN
          KL    = 1
          KU    = NK
          SIGMA = SIG(1:NK)
      ELSE IF (SIZE(WN_R,1) .EQ. NK+2) THEN
          KL    = 1
          KU    = NK+2
          SIGMA = SIG(0:NK+1)
      ELSE
          IF ( IAPROC .EQ. NAPERR ) WRITE(NDSE,900) 'W3IC5WNCG'
          CALL EXTCDE(3)
      END IF
!
! Fox-Squire dispersion
      DO IK = KL, KU
!         FSDISP(HICE, IVISC, RHOI, ISMODG, HWAT, WT, WNR, WNI)
          CALL FSDISP(HICE, IVISC, RHOI, ISMODG, HWAT, TPI/SIGMA(IK), &
                      TWN_R, TWN_I)
          WN_R(IK) = TWN_R
          WN_I(IK) = TWN_I
      END DO
!
      DEALLOCATE(SIGMA)
!
  900 FORMAT(/' *** WAVEWATCH III ERROR IN W3SIC5MD : '/   &
              '     Subr. ', A, ': Cannot determine bounds of&
                                   & wavenumber array.'/)
!/
!/ End of W3IC5WNCG -------------------------------------------------- /
!/
      END SUBROUTINE W3IC5WNCG
!/ ------------------------------------------------------------------- /
!/
      SUBROUTINE FSDISP(HICE, IVISC, RHOI, ISMODG, HWAT, WT, WNR, WNI)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         19-May-2021 |
!/                  +-----------------------------------+
!/
!/    17-Mar-2016 : Origination                         ( version 5.10)
!/                                                      ( Q. Liu)
!/    17-Mar-2016 : Start from the Matlab code `FoxSquire.m` (provided
!/                  by Prof. Vernon Squire from University of Otago)
!/                                                      ( Q. Liu)
!/    25-Apr-2017 : Add more filters                    ( Q. Liu)
!/
!/    19-May-2021 : Incl. RP and M2 ice models          ( Q. Liu)
!/
!  1. Purpose :
!
!     Calculate the complex wavenumber for waves in ice according to
!     three different sea ice models, i.e., FS, RP and M2 (see Liu et
!     al. 2020)
!
!  2. Method :
!     Mainly solving the dispersion relations of FS and RP models (
!     Eqs. (20, 24, 25)) in Mosig et al. (2015))
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     Name     Type   Intent   Description
!     ----------------------------------------------------------------
!     HICE     Real.  IN       thickness of ice (m)
!     IVISC    Real.  IN       viscosity parameter of ice (m^2 s^{-1})
!     RHOI     Real.  IN       the density of ice (kg m^{-3})
!     ISMODG   Real.  IN       effecitive shear modulus G of ice (Pa)
!     HWAT     Real.  IN       water depth
!     WT       Real.  IN       wave period (s; 1/freq)
!     WNR      Real.  Out      the real. part of the wave number
!     WNI      Real.  Out      the imag. part of the wave number
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      POLYROOTS Subr. /        Find the roots of a general polynomial
!      NR_ROOT   Func. /        Newton-Raphson root finding
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3IC5WNCG Subr. /        Wavenumber and group velocity of ice-
!                               coupled waves
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     See Format 1000, 1001, 1002
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      USE CONSTANTS, ONLY: GRAV, TPI
      USE W3DISPMD,  ONLY: WAVNU1
      USE W3SERVMD,  ONLY: EXTCDE
      USE W3ODATMD,  ONLY: NDSE, IAPROC, NAPROC, NAPERR
      USE W3GDATMD,  ONLY: IC5PARS
      USE W3GSRUMD,  ONLY: W3INAN
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
      REAL, INTENT(IN)      :: HICE, IVISC, RHOI, ISMODG, HWAT, WT
      REAL, INTENT(OUT)     :: WNR, WNI
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!
      REAL                  :: IC5MINIG, IC5MINWT, IC5MAXKRATIO, &
                               IC5MAXKI, IC5MINHW, IC5VEMOD
      REAL                  :: TISMODG, TWT, TRATIO, THW
      REAL, PARAMETER       :: NU = 0.3, RHOW = 1025.
!     COMPLEX               :: GV, C1
!     REAL                  :: SIGMA, C2, WNO, CGO, THKH,  &
      COMPLEX               :: GV, C1, C2
      REAL                  :: SIGMA, WNO, CGO, THKH,  &
                               RTRL(5), RTIM(5), RTANG(5)
      INTEGER               :: IREAL
!     COMPLEX(KDPC)         :: GUESS, CROOT, C1D
!     REAL(KDP)             :: C2D, HWATD
      COMPLEX(KDPC)         :: GUESS, CROOT, C1D, C2D
      REAL(KDP)             :: HWATD
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'FSDISP')
#endif
! Note, same as W3IC3WNCG_xx in w3sic3md :
!     HICE   →   ICE1
!     IVISC  →   ICE2
!     RHOI   →   ICE3
!     ISMODG →   ICE4
! 0.  Initializations ------------------------------------------------ *
! Set limiters
!
! When G = 0, the FS method does not provide a solution. It is not
! unexpected because the FS model is originally devised as a
! thin elastic plate model in which elasticity is necessary.
!
! The FS algorithm may also have issues for very short wave periods,
! shallow waters and low G (e.g., T~3 s, d~10 m, hi~0.5 m, G<10^6 Pa)
!
      IC5MINIG     = IC5PARS(1) ! Minimum G
      IC5MINWT     = IC5PARS(2) ! Minimum T
      IC5MAXKRATIO = IC5PARS(3) ! Maximum k_{ow}/k_r
      IC5MAXKI     = IC5PARS(4) ! Maximum k_i
      IC5MINHW     = IC5PARS(5) ! Minimum d
      IC5VEMOD     = IC5PARS(9) ! Model selected 1: EFS, 2: RP, 3: M2
!
      TISMODG  = MAX(IC5MINIG, ISMODG)
      TWT      = MAX(IC5MINWT, WT)
      THW      = MAX(IC5MINHW, HWAT)
!
! G <= 0. is not allowed
      IF (ABS(TISMODG) < ERRTOL) THEN
          IF ( IAPROC .EQ. NAPERR ) WRITE(NDSE, 1000) 'FSDISP'
          CALL EXTCDE(1)
      END IF
!
! σ = 2π / T
      SIGMA = TPI / TWT
!
      IF (ABS(IC5VEMOD - 1.) < ERRTOL) THEN
! Complex shear modulus Gv = G - i σ ρ η (EFS model)
          GV = CMPLX(TISMODG, -1. * SIGMA * RHOI * IVISC)
!
! -------------------------------------------------------------------- *
! Note that Eq. (24) in Mosig et al. (2015) can be written like below:
! (c1 * k^5 + c2 * k) * tanh(HWAT*k) - 1 = 0
! Most Important part of this module --------------------------------- *
          C1 = GV * HICE**3. / (6. * RHOW * SIGMA**2.)
!
! To be divided by (1-NU) or multiplied by (1+NU) ??
! Beam model: then multiplied by (1+ν)
! Plate model: then divided by (1-ν)
! The beam version is more theoretically (J.E.M. Mosig, personal
! communication, 2016), although there is only very marginal difference
! between this two version as (1+NU = 1.3 and 1/(1-NU) ~ 1.4)
          C1 = C1 * (1+NU)
!         C1 = C1 / (1-NU)
!
! C2
!         C2 = GRAV / SIGMA**2. - RHOI * HICE / RHOW
          C2 = CMPLX(GRAV / SIGMA**2. - RHOI * HICE / RHOW, 0.)
!
      ELSE IF (ABS(IC5VEMOD - 2.) < ERRTOL) THEN
! See Appendix of Liu et al. (2020) - RP model
          C1 = CMPLX(TISMODG * HICE**3. * (1+NU) / (6. * RHOW * SIGMA**2.), 0.)
          C2 = CMPLX(GRAV/SIGMA**2. - RHOI * HICE / RHOW,              &
                     -1. * IVISC / (RHOW * SIGMA))
!
      ELSE IF (ABS(IC5VEMOD - 3.) > ERRTOL) THEN
          WRITE(NDSE, 1003) 'FSDISP', IC5VEMOD
          CALL EXTCDE(4)
      END IF
! Use the dispersion in open water to get an approximation of
! tanh(HWAT * k). We can also roughly use the dispersion in deep
! water case, that is tanh(HWAT*k) ~ 1.
! Wavenumber in the open water
!     WAVNU1(SI, H, K, CG)
      CALL WAVNU1(SIGMA, THW, WNO, CGO)
      THKH = TANH(WNO * THW)
!
      IF (ABS(IC5VEMOD - 1.) < ERRTOL .OR. ABS(IC5VEMOD - 2.) < ERRTOL) THEN
! Get the first guess of the complex wavenumber
          CALL POLYROOTS(6, &
                         (/REAL(REAL(C1))*THKH, 0., 0., 0., REAL(REAL(C2))*THKH, -1./),&
                         RTRL, RTIM)
          RTANG = ATAN2(RTIM, RTRL)
!
! There should only be one real root in RTRL + i * RTIM because in
! this case (ivisc=0) the original viscoelastic-type model reduced to
! the thin elastic plate model which has only one real solution.
! Find its index ...
!
          IREAL = MINLOC(ABS(RTANG), DIM=1)
          IF (RTRL(IREAL) <= 0. .OR. ABS(RTIM(IREAL)) > ERRTOL) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE(NDSE, 1001) 'FSDISP'
              CALL EXTCDE(2)
          END IF
!
! Get the first guess for iteration
          GUESS = RTRL(IREAL) * EXP(CMPLX(0., 1E-6))
!
! Newton-Raphson method
! Turn c1, c2, hwat to be double
          C1D = C1; C2D = C2; HWATD = THW
          CROOT = NR_ROOT(C1D, C2D, HWATD, GUESS)
          WNR = REAL(REAL(CROOT))
          WNI = REAL(AIMAG(CROOT))
!
      ELSE IF (ABS(IC5VEMOD - 3.) < ERRTOL) THEN ! M2
! Model with Order 3 Power Law (section 6.2 in Meylan et al. (2018, JGR-Ocean))
! Based on my understanding, the wavelength does not change because
! the elasticity is not considered in this model
          WNR = WNO ! Open-water wavenumber
! Eq. (53) in Meylan et al. (2018)
          WNI = HICE * IVISC * SIGMA**3. / (RHOW * GRAV**2.)
      END IF
!
! RATIO Check
! Using the ratio k0 / kr as a basic check for the reliability of
! FSDISP. The FS dispersion relation can give a very different kr from
! k0, especially for small wave periods (k0/kr is as high as 100).
! From my tests, using IC5MAXKRATIO = 1000. can basically detect most
! spurious solutions (although not all of them)
!
! ISNAN Check
! Common ways used are:
! NAN = SQRT(-1.) or
! a /= a then a is NaN or
! ISNAN func (supported by gfortran & ifort)
!        --- ISNAN -> W3INAN because ISNAN is not supported by pgi
! For very few cases, we can get nan | negative ki | kr
!
! (N.B.) NaN problem solved by using CMPLX_TANH2
!
      TRATIO = WNO / WNR
      IF (W3INAN(WNR) .OR. W3INAN(WNI) .OR. WNR <= 0 .OR. WNI <= 0. &
          .OR. TRATIO >= IC5MAXKRATIO) THEN
          IF ( IAPROC .EQ. NAPERR )                       &
          WRITE(NDSE, 1002) 'FSDISP', HICE, IVISC, TISMODG, HWAT, TWT, &
                            WNO, WNR, WNI
          CALL EXTCDE(3)
      END IF
!
! Filter high ki
      WNI = MIN(IC5MAXKI, WNI)
!
! FORMAT
 1000 FORMAT(/' *** WAVEWATCH III ERROR IN W3SIC5MD : '/   &
              '     Subr. ', A, ': Zero shear modulus G is not allowed&
                                   & in the FS viscoelastic model'/)
!
 1001 FORMAT(/' *** WAVEWATCH III ERROR IN W3SIC5MD : '/   &
              '     Subr. ', A, ': get a bad first guess'/)
!
 1002 FORMAT(/' *** WAVEWATCH III ERROR IN W3SIC5MD : '/   &
              ' -----------------------------------------------------'/&
              '     Subr. ', A,'   : get NaN/NeG/Huge kr or ki for'   /&
              ' -----------------------------------------------------'/&
              '  Ice thickness     : ', F9.1, ' m'/         &
              '  Ice viscosity     : ', E9.2, ' m2/s'/      &
              '  Ice shear modulus : ', E9.2, ' Pa' /       &
              '  Water depth       : ', F9.1, ' m'/         &
              '  Wave period       : ', F10.2, ' s'/        &
              '  Wave number (Ko)  : ', F11.3, ' rad/m'/    &
              '  Wave number (Kr)  : ', F11.3, ' rad/m'/    &
              '  Attenu. Rate (Ki) : ', E9.2, ' /m'/)
!
 1003 FORMAT(/' *** WAVEWATCH III ERROR IN W3SIC5MD : '/   &
               '     Subr. ', A, ': Unknown VE model (', F5.0, ')'/)
!/
!/ End of FSDISP ----------------------------------------------------- /
!/
      END SUBROUTINE FSDISP
!/ ------------------------------------------------------------------- /
!/
      SUBROUTINE BALANCING_MATRIX(NMAT, MATRIX)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         15-Mar-2016 |
!/                  +-----------------------------------+
!/
!/    15-Mar-2016 : Origination                         ( version 5.10)
!/                                                      ( Q. Liu )
!/    15-Mar-2016 : Borrowed from Numerical Recipes in Fortran
!/                                                      ( Q. Liu )
!  1. Purpose :
!     Reducing the sensitivity of eigenvalues to rounding errors during
!     the execution of some algorithms.
!
!  2. Method :
!     The errors in the eigensystem found by a numerical procedure are
!     generally proportional to the Euclidean norm of the matrix, that
!     is, to the square root of the sum of the squares of the elements
!     (sqrt(sum(a_{i, j} ** 2.)). The idea of balancing is to use
!     similarity transformations to make corresponding rows and columns
!     of the matrix have comparable norms, thus reducing the overall
!     norm of the matrix while leaving the eigenvalues unchanged. Note
!     that the symmetric matrix is already balanced.
!
!     The output is matrix that is balanced in the norm given by
!     summing the absolute magnitudes of the matrix elements(
!     sum(abs(a_{i, j})) ). This is more efficient than using the
!     Euclidean norm, and equally effective: a large reduction in
!     one norm implies a large reduction in the other.
!
!     For the details of this method, please refer to
!     1) Numerical Recipes in Fortran 77 (Volume 1, 2nd Edition)
!        [Chapter 11.5 / subroutine balanc]
!     2) Numerical Recipes in Fortran 90 (Volume 2)
!        [Chapter B11 / subroutine balanc]
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     Name     Type   Intent   Description
!     ----------------------------------------------------------------
!     NMAT     Int.   I        The size of one dimension of MATRIX
!     MATRIX   R.A.   I/O      A matrix with the shape (NMAT, NMAT)
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing (!/S switch).
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      POLYROOTS Subr. /        Find the roots of polynomials
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     None.
!
!  7. Remarks:
!     Balancing only needs marginal computational efforts but can
!     substantially improve the accuracy of the eigenvalues computed
!     for a badly balanced matrix. It is therefore recommended that
!     you always balance nonsymmetric matrices.
!
!     Given a (NMAT, NMAT) MATRIX, this routine replaces it by a
!     balanced matrix with identical eigenvalues. A symmetric matrix is
!     already balanced and is unaffected by this procedure.
!
!  8. Structure :
!
!     See the source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
      INTEGER, INTENT(IN)    :: NMAT
      REAL, INTENT(INOUT)    :: MATRIX(NMAT, NMAT)
!/ ------------------------------------------------------------------- /
!/ Local parameter
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
! the parameter radx is the machine's floating-point radix
      REAL, PARAMETER        :: RADX = RADIX(MATRIX), &
                                SQRADX = RADX ** 2
      INTEGER                :: I, LAST
      REAL                   :: C, F, G, R, S
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      CALL STRACE (IENT, 'BALANCING_MATRIX')
#endif
!
      DO
          LAST = 1
          DO I = 1, NMAT
! Calculate row and column norms
              C = SUM( ABS(MATRIX(:, I)) ) - MATRIX(I, I)
              R = SUM( ABS(MATRIX(I, :)) ) - MATRIX(I, I)
! If both are non-zero
              IF (C /= 0.0 .AND. R /= 0.0) THEN
! Find the integer power of the machine radix that comes closest to
! balancing the matrix (get G, F from C, R)
                  G = R / RADX
                  F = 1.0
                  S = C + R
                  DO
                      IF (C >= G) EXIT
                      F = F * RADX
                      C = C * SQRADX
                  END DO
!
                  G = R * RADX
                  DO
                      IF (C <= G) EXIT
                      F = F / RADX
                      C = C / SQRADX
                  END DO
!
                  IF ( (C+R)/F < 0.95*S) THEN
                      LAST = 0
                      G = 1.0 / F
! Apply similarity tranformation
                      MATRIX(I, :) = MATRIX(I, :) * G
                      MATRIX(:, I) = MATRIX(:, I) * F
                  END IF
              END IF
          END DO
          IF (LAST /= 0) EXIT
      END DO
!/
!/ End of subroutine BALANCING_MATRIX -------------------------------- /
!/
      END SUBROUTINE BALANCING_MATRIX
!/ ------------------------------------------------------------------- /
!/
      SUBROUTINE EIG_HQR (NMAT, HMAT, EIGR, EIGI)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         17-Mar-2016 |
!/                  +-----------------------------------+
!/
!/    16-Mar-2016 : Origination                         ( version 5.10)
!/                                                      ( Q. Liu )
!/    16-Mar-2016 : Borrowed from Numerical Recipes in Fortran
!/                                                      ( Q. Liu )
!/    17-Mar-2016 : Update the NR code v2.08 to v2.10   ( Q. Liu )
!/
!  1. Purpose :
!
!     When we calculate the eigenvalues of a general matrix, we first
!     reduce the matrix to a simpler form (e.g., Hessenberg form) and
!     then we perform the iterative procedures.
!
!     A upper Hessenberg matrix has zeros everywhere below the diagnal
!     except for the first subdiagonal row. For example, in the 6x6
!     case, the non-zero elements are:
!                   |x x x x x x|
!                   |x x x x x x|
!                   |  x x x x x|
!                   |    x x x x|
!                   |      x x x|
!                   |        x x|
!
!     This subroutine uses QR algorithm to get the eigenvalues of a
!     Hessenberg matrix. So make sure the input array HMAT is a
!     Hessenberg-type matrix.
!
!  2. Method :
!     QR algorithm for real Hessenberg matrices.
!     (I did not understand this algorithm well, so I could not give
!      any detailed explanations)
!
!     For the details of this HQR method, please refer to
!     1) Numerical Recipes in Fortran 77 (Volume 1, 2nd Edition)
!        [Chapter 11.6 / subroutine hqr]
!     2) Numerical Recipes in Fortran 90 (Volume 2)
!        [Chapter B11 / subroutine hqr]
!
!     Note that there is a bug in the `hqr` subroutine in NR v2.08.
!     See http://numerical.recipes/latest-known-bugs.html. Please use
!     the updated code in NR v2.10.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     Name     Type   Intent   Description
!     ----------------------------------------------------------------
!     NMAT     Int.   I        the size of one dimension of HMAT
!     HMAT     R.A.   I/O      the Hessenberg-type matrix (NMAT, NMAT)
!     EIGR     R.A.   O        the real part of the N eigenvalues
!     EIGI     R.A.   O        the imag part of the N eigenvalues
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
!      POLYROOTS Subr. /        Find the roots of polynomials
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     See Format 1001
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      USE W3SERVMD, ONLY: EXTCDE
      USE W3ODATMD, ONLY: NDSE, IAPROC, NAPROC, NAPERR
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)  :: NMAT
      REAL, INTENT(INOUT)  :: HMAT(NMAT, NMAT)
      REAL, INTENT(OUT)    :: EIGR(NMAT), EIGI(NMAT)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
      INTEGER              :: I, ITS, K, L, M, NN, MNNK, IDIAG
      REAL                 :: ANORM, P, Q, R, S, T, U, V, W, X, Y, Z
      REAL                 :: PP(NMAT)
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'EIG_HQR')
#endif
!
! Compute matrix norm for possible use in locating single small
! subdiagonal element.
!
! Note the speciality of Hessenberg matrix :
! Elements below the diagonal are zeros except for the first
! subdiagonal row. It might be more accurate if we use a mask array
! to mask all zero elments.
!
      ANORM = SUM(ABS(HMAT))
      NN = NMAT
! Gets changed only by an exceptional shift.
      T = 0.0
! Begin search for next eigenvalue: "do while nn >= 1"
      DO
          IF (NN < 1) EXIT
          ITS=0
! Begin iteration
          ITERATE:DO
! Look for single small subdiagonal element.
              SMALL: DO L=NN, 2, -1
                  S = ABS( HMAT(L-1, L-1) ) + ABS( HMAT(L, L) )
!                  IF (S == 0.0) S = ANORM
                  IF (ABS(S) < ERRTOL) S = ANORM
!                  IF ( ABS(HMAT(L, L-1)) + S == S ) THEN
                  IF ( ABS(HMAT(L, L-1)) < ERRTOL ) THEN
                      HMAT(L, L-1) = 0.0
                      EXIT SMALL
                  END IF
              END DO SMALL
              X = HMAT(NN, NN)
! One root found
              IF (L == NN) THEN
                  EIGR(NN) = X + T
                  EIGI(NN) = 0.0
                  NN=NN-1
! Go back for next eigenvalue
                  EXIT ITERATE
              END IF
              Y = HMAT(NN-1, NN-1)
              W = HMAT(NN, NN-1) * HMAT(NN-1, NN)
! Two roots found . . .
              IF (L == NN-1) THEN
                  P = 0.5 * (Y - X)
                  Q = P**2 + W
                  Z = SQRT( ABS(Q) )
                  X = X + T
! . . . A real pair . . .
                  IF (Q >= 0.0) THEN
                      Z = P + SIGN(Z, P)
                      EIGR(NN) = X + Z
                      EIGR(NN-1) = EIGR(NN)
                      IF (Z /= 0.0) EIGR(NN) = X - W/Z
                      EIGI(NN) = 0.0
                      EIGI(NN-1) = 0.0
! . . . A complex pair
                  ELSE
                      EIGR(NN) = X + P
                      EIGR(NN-1) = EIGR(NN)
                      EIGI(NN) = Z
                      EIGI(NN-1) = -Z
                  END IF
                  NN=NN-2
! GO BACK FOR NEXT EIGENVALUE.
                  EXIT ITERATE
              END IF
! NO ROOTS FOUND. CONTINUE ITERATION.
              IF (ITS == 30) THEN
                  IF ( IAPROC .EQ. NAPERR ) WRITE(NDSE, 1001) 'EIG_HQR'
                  CALL EXTCDE(2)
              END IF
! FORM EXCEPTIONAL SHIFT.
              IF (ITS == 10 .OR. ITS == 20) THEN
                  T = T + X
! Add -X to the diagonal of HMAT
                  DO IDIAG = 1, NN
                      HMAT(IDIAG, IDIAG) = HMAT(IDIAG, IDIAG) + (-X)
                  END DO
                  S = ABS(HMAT(NN, NN-1)) + ABS(HMAT(NN-1, NN-2))
                  X = 0.75 * S
                  Y = X
                  W = -0.4375 * S**2
              END IF
              ITS = ITS + 1
! Form shift and then look for 2 consecutive small subdiagonal elements.
              DO M = NN-2, L, -1
                  Z = HMAT(M, M)
                  R = X - Z
                  S = Y - Z
! Equation (11.6.23).
                  P = (R * S - W) / HMAT(M+1, M) + HMAT(M, M+1)
                  Q = HMAT(M+1, M+1) - Z - R - S
                  R = HMAT(M+2, M+1)
! Scale to prevent overflow or underflow
                  S = ABS(P) + ABS(Q) + ABS(R)
                  P = P / S
                  Q = Q / S
                  R = R / S
                  IF (M == L) EXIT
                  U = ABS( HMAT(M, M-1) ) * ( ABS(Q) + ABS(R) )
                  V = ABS(P) * ( ABS(HMAT(M-1, M-1)) + ABS(Z) + &
                      ABS( HMAT(M+1, M+1) ))
! Equation (11.6.26)
                  IF (U+V == V) EXIT
              END DO
              DO I= M+2, NN
                  HMAT(I, I-2) = 0.0
                  IF (I /= M+2) HMAT(I, I-3)=0.0
              END DO
! Double QR step on rows L to NN and columns M to NN
              DO K=M, NN-1
                  IF (K /= M) THEN
! Begin setup of householder vector
                      P = HMAT(K, K-1)
                      Q = HMAT(K+1, K-1)
                      R = 0.0
                      IF (K /= NN-1) R = HMAT(K+2, K-1)
                      X = ABS(P) + ABS(Q) + ABS(R)
                      IF (X /= 0.0) THEN
! Scale to prevent overflow or underflow
                          P = P / X
                          Q = Q / X
                          R = R / X
                      END IF
                  END IF
                  S = SIGN(SQRT(P**2 + Q**2 + R**2), P)
                  IF (S /= 0.0) THEN
                      IF (K == M) THEN
                          IF (L /= M) HMAT(K, K-1) = -HMAT(K, K-1)
                      ELSE
                          HMAT(K, K-1) = -S * X
                      END IF
! Equations (11.6.24).
                      P = P + S
                      X = P / S
                      Y = Q / S
                      Z = R / S
                      Q = Q / P
! READY FOR ROW MODIFICATION.
                      R = R / P
                      PP(K:NN) = HMAT(K, K:NN) + Q * HMAT(K+1, K:NN)
                      IF (K /= NN-1) THEN
                          PP(K:NN) = PP(K:NN) + R * HMAT(K+2, K:NN)
                          HMAT(K+2, K:NN) = HMAT(K+2, K:NN) - &
                                             PP(K:NN)*Z
                      END IF
                      HMAT(K+1, K:NN) = HMAT(K+1, K:NN) - PP(K:NN) * Y
                      HMAT(K, K:NN) = HMAT(K, K:NN) - PP(K:NN) * X
! COLUMN MODIFICATION.
                      MNNK = MIN(NN, K+3)
                      PP(L:MNNK) = X * HMAT(L:MNNK, K) + Y * &
                                   HMAT(L:MNNK, K+1)
                      IF (K /= NN-1) THEN
                          PP(L:MNNK) = PP(L:MNNK) + Z*HMAT(L:MNNK, K+2)
                          HMAT(L:MNNK, K+2) = HMAT(L:MNNK, K+2) - &
                                              PP(L:MNNK) * R
                      END IF
                      HMAT(L:MNNK, K+1) = HMAT(L:MNNK, K+1) - &
                                          PP(L:MNNK) * Q
                      HMAT(L:MNNK, K) = HMAT(L:MNNK, K) - PP(L:MNNK)
                  END IF
              END DO
! GO BACK FOR NEXT ITERATION ON CURRENT EIGENEND DO VALUE.
          END DO ITERATE
      END DO
!
! Formats
 1001 FORMAT(/' *** WAVEWATCH III ERROR IN W3SIC5MD : '/  &
              '     Subr. ', A, ': TOO MANY ITERATIONS'/)
!/ ------------------------------------------------------------------- /
!/
!/ End of EIG_HQR ---------------------------------------------------- /
!/
      END SUBROUTINE EIG_HQR
!/ ------------------------------------------------------------------- /
!/
      SUBROUTINE POLYROOTS(NPC, PCVEC, RTRL, RTIM)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         16-Mar-2016 |
!/                  +-----------------------------------+
!/
!/    16-Mar-2016 : Origination                         ( version 5.10)
!/                                                      ( Q. Liu )
!/    16-Mar-2016 : Started from Numerical Recipes in Fortran
!/                                                      ( Q. Liu )
!/
!  1. Purpose :
!
!     Find the roots of arbitrary polynomials through finding the
!     eigenvalues of companion matrix.
!
!  2. Method :
!     Suppose we have a general polynomial, which reads
!     P(x) = c_n * x^n + c_{n-1} * x^{n-1} + ... + c_1 * x + c_0
!
!     Then finding the roots of P(x) is equivalent to find the eigen-
!     values of the special n x n companion matrix A
!         |  -c_{n-1}/c_n   -c_{n-2}/c_n   ...   -c_1/c_n   -c_0/c_n |
!         |  1              0              ...   0          0        |
!     A = |  0              1              ...   0          0        |
!         |  :              :                    :          :        |
!         |  0              0                    1          0        |
!
!     In fact, P(x) is the characteristic polynomial of matrix A, i.e.,
!     P(x) = det|A-xI| and x is the eigenvalues of A (this is a
!     Hessenberg matrix).
!
!     In this subrountine, we will use the two subroutines above
!     (BALANCING_MATRIX & EIG_HQR) to get the complex eigenvalues of
!     an abitrary Hessenberg matrix
!
!     For the details of this method, please refer to
!     1) Numerical Recipes in Fortran 77 (Volume 1, 2nd Edition)
!        [Chapter 9.5 / Eigenvalue Methods / subroutine zrhqr]
!     2) Numerical Recipes in Fortran 90 (Volume 2)
!        [Chapter B9 / subroutine zrhqr]
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     Name     Type   Intent   Description
!     ----------------------------------------------------------------
!     NPC      Int.   I        The # of the Polynomial coefficients
!                              (from c_n to c_0)
!     PCVEC    R.A.   I        The 1d vector for the Polynomial
!                              coefficients  [c_n, c_{n-1}, ..., c_0]
!     RTRL     R.A.   O        The real part of all of the roots
!                              shape: [NPC-1]
!     RTIM     R.A.   O        The real part of all of the roots
!                              shape: [NPC-1]
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name                Type    Module    Description
!     ------------------------------------- ---------------------------
!      STRACE              Subr.   W3SERVMD  Subroutine tracing.
!      BALANCING_MATRIX    Subr.   /         Balancing matrix
!      EIG_HQR             Subr.   /         Finding eigenvalues
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      FSDISP    Subr. /        Solving the dispersion relations
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     See Format 1001
!
!  7. Remarks :
!     The built-in MATLAB function <roots> uses the same method to
!     find roots of a general polynomial. But perhaps MATLAB uses
!     different methods to find eigenvalues of the companion matrix.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      USE W3SERVMD, ONLY: EXTCDE
      USE W3ODATMD, ONLY: NDSE, IAPROC, NAPROC, NAPERR
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
      INTEGER, INTENT(IN)   :: NPC
      REAL, INTENT(IN)      :: PCVEC(NPC)
      REAL, INTENT(OUT)     :: RTRL(NPC-1), RTIM(NPC-1)
!/
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      REAL                  :: HESS(NPC-1, NPC-1)
      INTEGER               :: J
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'POLYROOTS')
#endif
!
!
      IF (ABS(PCVEC(1)) < ERRTOL) THEN
          IF ( IAPROC .EQ. NAPERR ) WRITE(NDSE, 1001) 'POLYROOTS'
          CALL EXTCDE(2)
      END IF
!
! Generate the Hessenberg matrix
      HESS = 0.
      HESS(1, :) = -1 * PCVEC(2:) / PCVEC(1)
      DO J = 1, NPC-2
          HESS(J+1, J) = 1.
      END DO

! Balancing the matrix HESS
      CALL BALANCING_MATRIX(NPC-1, HESS)
! Eigenvalues of the matrix HESS
      CALL EIG_HQR(NPC-1, HESS, RTRL, RTIM)

! Formats
 1001 FORMAT(/' *** WAVEWATCH III ERROR IN W3SIC5MD : '/  &
              '     Subr. ', A, ': the coeff. of x^n must not be 0'/)
!/
!/ End of POLYROOTS -------------------------------------------------- /
!/
      END SUBROUTINE POLYROOTS
!/ ------------------------------------------------------------------- /
!/
      FUNCTION NR_CORR(K, C1, C2, H)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         19-May-2021 |
!/                  +-----------------------------------+
!/
!/    18-Mar-2016 : Origination.                        ( version 5.10 )
!/                                                      ( Q. Liu )
!/    18-Mar-2016 : Start from the Matlab code `FoxSquire.m` (provided
!/                  by Prof. Vernon Squire from University of Otago)
!/                                                      ( Q. Liu )
!/    24-Mar-2016 : Adding the cmplx_sinh/cosh/tanh     ( Q. Liu )
!/
!/    19-May-2021 : Change types of few input arguments ( Q. Liu )
!/
!  1. Purpose :
!
!     Calculate the corrected term in the Newton-Raphson root-finding
!     method (Must use double precision)
!
!  2. Method :
!     Suppose we want to find the root of f(x) = 0, then according to
!     the Newton-Raphson method, the root is iteratively updated by the
!     formula below:
!
!         x_{i+1} = x_{i} - f(x_{i}) / f'(x_{i}),
!
!     where f'(x) denotes the derivative of f(x). In this function,
!     our f(x) reads (see also subr. FSDISP)
!
!         f(x) = (c1 * k**4 + c2) * k * tanh(kH) -1
!
!     we finally will get the Newton-Raphson correted term, i.e.,
!
!         dx = f(x_{i}) / f'(x_{i})
!
!     For the details of this method, please refer to
!     1) Numerical Recipes in Fortran 77 (Volume 1, 2nd Edition)
!        Chapter 9.4
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     Name     Type      Intent   Description
!     ----------------------------------------------------------------
!     K        CMPL.(D)  I        complex wave number
!     C1       CMPL.(D)  I        C1 in FSDISP
!     C2       Real.(D)  I        C2 in FSDISP
!     H        Real.(D)  I        water depth
!     NR_CORR  CMPL.(D)  O        Newton-Raphson corrected term (DK)
!     ----------------------------------------------------------------
!     * (D) means double precision
!
!  4. Subroutines used :
!
!      Name        Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE      Subr. W3SERVMD Subroutine tracing.
!      CMPLX_SINH  Func. /         sinh for complex var.
!      CMPLX_COSH  Func. /         cosh for complex var.
!      CMPLX_TANH2 Func. /         tanh for complex var.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      NR_ROOT   Func. /        Newton-Raphson root finding.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!     COMPLEX(KDPC), INTENT(IN)     :: K, C1
!     REAL(KDP), INTENT(IN)         :: C2, H
      COMPLEX(KDPC), INTENT(IN)     :: K, C1, C2
      REAL(KDP), INTENT(IN)         :: H
      COMPLEX(KDPC)                 :: NR_CORR
!/
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
! A rough value to differentiate deep water case from finite water case
      REAL(KDP), PARAMETER          :: KH_LIM = 7.5
      COMPLEX(KDPC)                 :: LAM, LAMPR, FV, DF, TKH
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'NR_CORR')
#endif
! f(k) = (c1 * k**4 + c2) * k * tanh(k*H) - 1
!      = lam * k * tanh(k*H) - 1
!
      TKH   = K * H
      LAM   = C1 * K**4 + C2
! the derivative of (lam * k)
      LAMPR = 5 * C1 * K**4 + C2
!
      IF (REAL(REAL(TKH)) <= KH_LIM) THEN
!         KH is small enough
!         FV = LAM * K * SINH(K*H) - COSH(K*H)
!         DF = LAM * (K*H) * COSH(K*H) + (LAMPR - H) * SINH(K*H)
          FV = LAM * K * CMPLX_SINH(TKH) - CMPLX_COSH(TKH)
          DF = LAM * TKH * CMPLX_COSH(TKH) + (LAMPR-H) * CMPLX_SINH(TKH)
      ELSE
!         FV = LAM * K * TANH(K*H) - 1
!         DF = LAM * K * H + (LAMPR - H) * TANH(K*H)
!         DF = LAMPR * TANH(K*H) + LAM * K * H / (COSH(K*H) **2)
          FV = LAM * K * CMPLX_TANH2(TKH) - 1
          DF = LAMPR * CMPLX_TANH2(TKH) + LAM * TKH * &
               (1 - CMPLX_TANH2(TKH) ** 2.)
      END IF
!
      NR_CORR = FV / DF
!/
!/ End of NR_CORR ---------------------------------------------------- /
!/
      END FUNCTION NR_CORR
!/ ------------------------------------------------------------------- /
!/
      FUNCTION NR_ROOT(C1, C2, H, GUESS)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         19-May-2021 |
!/                  +-----------------------------------+
!/
!/    18-Mar-2016 : Origination.                        ( version 5.10 )
!/                                                      ( Q. Liu )
!/    18-Mar-2016 : Start from the Matlab code `FoxSquire.m` (provided
!/                  by Prof. Vernon Squire from University of Otago)
!/                                                      ( Q. Liu )
!/
!/    19-May-2021 : Change types of few input arguments ( Q. Liu )
!/
!  1. Purpose :
!
!     The iterative procedure of the Newton-Raphson method
!
!  2. Method :
!     See the document of Subr. NR_CORR (Must use double precision)
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     Name     Type      Intent  Description
!     ----------------------------------------------------------------
!     C1       CMPL.(D)  I       C1 in FS dipsersion relations
!                                See the doc. of subr. NR_CORR
!     C2       REAL (D)  I       C2 in FS dipsersion relations
!     H        REAL (D)  I       water depth
!     GUESS    CMPL.(D)  I       the first guess obtained from POLYROOTS
!     NR_ROOT  CMPL.(D)  O       the calculated complex wave number.
!     ----------------------------------------------------------------
!     * (D) means double precision
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      NR_CORR   Func. /        Newton-Raphson correction term
!      INIT_RANDOM_SEED
!                Subr. /        Initialize the random seed based on
!                               the system's time
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      FSDISP    Subr. /        Solve  FS dispersion relations
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      USE W3SERVMD, ONLY: EXTCDE
      USE W3ODATMD, ONLY: NDSE, IAPROC, NAPROC, NAPERR
      USE W3GDATMD, ONLY: IC5PARS
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!     COMPLEX(KDPC), INTENT(IN)     :: C1, GUESS
!     REAL(KDP), INTENT(IN)         :: C2, H
      COMPLEX(KDPC), INTENT(IN)     :: C1, GUESS, C2
      REAL(KDP), INTENT(IN)         :: H
      COMPLEX(KDPC)                 :: NR_ROOT
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      COMPLEX(KDPC)                 :: K0, K1, DK
      INTEGER                       :: ITER
      REAL                          :: TRANVAL
      REAL                          :: IC5MAXITER, IC5RKICK, IC5KFILTER
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'NR_ROOT')
#endif
!/ Set parameters
      IC5MAXITER = IC5PARS(6)
      IC5RKICK   = IC5PARS(7) ! 0: False, 1: True
      IC5KFILTER = IC5PARS(8)
!
      K0 = GUESS
      DK = NR_CORR(K0, C1, C2, H)
      K1 = K0 - DK
      ITER = 0

      IF (IC5RKICK > 0.5) CALL INIT_RANDOM_SEED()
!
      DO WHILE (ABS(DK) > ERRTOL)
          K0 = K1
          DK = NR_CORR(K0, C1, C2, H)
          K1 = K0 - DK
          ITER = ITER + 1
!
! Random kick to avoid converging to evanescent modes
! Note: do not use RAND(1) because it alway gives a same random no.
! The built in function of RAND is not available in <ifort>, use
! random_seed/number instead.
!
! Based on many tests, I found the random kick & the corridor excluded
! from imaginary axis are kind of helpful to avoid spurious solutions.
! However, it may also lead to no solutions returned, especially for
! high G and high T.
!
          IF (IC5RKICK > 0.5 .AND. ABS(REAL(K1)) < IC5KFILTER) THEN
!             K1 = K1 + 2*RAND(0)
              CALL RANDOM_NUMBER(TRANVAL)
              K1 = K1 + 2 * TRANVAL
          END IF
!
          IF (ITER >= IC5MAXITER) THEN
              IF ( IAPROC .EQ. NAPERR ) WRITE(NDSE, 1001) 'NR_ROOT'
              CALL EXTCDE(1)
          END IF
!
      END DO
!
      NR_ROOT = K1
!
! Formats
 1001 FORMAT(/' *** WAVEWATCH III ERROR IN W3SIC5MD : '/  &
              '     Subr. ', A, ': TOO MANY ITERATIONS'/)
!
!/
!/ End of NR_ROOT ---------------------------------------------------- /
!/
      END FUNCTION NR_ROOT
!/ ------------------------------------------------------------------- /
!/
      FUNCTION CMPLX_SINH(X)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         24-Mar-2016 |
!/                  +-----------------------------------+
!/
!/    24-Mar-2016 : Origination.                        ( version 5.10 )
!/                                                      ( Q. Liu )
!/
!  1. Purpose :
!
!     For a number of compilers, the built-in function sinh, cosh and
!     tanh do not support the complex inputs. So here I write an
!     external one.
!
!  2. Method :
!
!     sinh(x) = (e**x - e**(-x)) / 2 (The built in function exp supports
!     complex input)
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     Name     Type      Intent  Description
!     ----------------------------------------------------------------
!      X       CMPL(D)     I     a double-precision complex var.
!     ----------------------------------------------------------------
!      * Note, this subr. will be only called by NR_CORR,
!      so for simplicity, I only use double-precision complex var.
!      as input.
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
!      Name       Type  Module   Description
!     ----------------------------------------------------------------
!      NR_CORR    Subr. /        Newton-Raphson correction term.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      COMPLEX(KDPC), INTENT(IN)  :: X
      COMPLEX(KDPC)              :: CMPLX_SINH
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      CALL STRACE (IENT, 'CMPLX_SINH')
#endif
!/
      CMPLX_SINH = (EXP(X) - EXP(-X)) * 0.5
!/
!/ End of CMPLX_SINH ------------------------------------------------- /
!/
      END FUNCTION CMPLX_SINH
!/ ------------------------------------------------------------------- /
!/
      FUNCTION CMPLX_COSH(X)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         24-Mar-2016 |
!/                  +-----------------------------------+
!/
!/    24-Mar-2016 : Origination.                        ( version 5.10 )
!/                                                      ( Q. Liu )
!/
!  1. Purpose :
!
!     For a number of compilers, the built-in function sinh, cosh and
!     tanh do not support the complex inputs. So here I write an
!     external one.
!
!  2. Method :
!
!     cosh(x) = (e**x + e**(-x)) / 2 (The built in function exp supports
!     complex input)
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     Name     Type      Intent  Description
!     ----------------------------------------------------------------
!      X       CMPL(D)     I     a double-precision complex var.
!     ----------------------------------------------------------------
!      * Note, this subr. will be only called by NR_CORR,
!      so for simplicity, I only use double-precision complex var.
!      as input.
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
!      Name       Type  Module   Description
!     ----------------------------------------------------------------
!      NR_CORR    Subr. /        Newton-Raphson correction term.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      COMPLEX(KDPC), INTENT(IN)  :: X
      COMPLEX(KDPC)              :: CMPLX_COSH
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      CALL STRACE (IENT, 'CMPLX_COSH')
#endif
!/
      CMPLX_COSH = (EXP(X) + EXP(-X)) * 0.5
!/
!/ End of CMPLX_COSH ------------------------------------------------- /
!/
      END FUNCTION CMPLX_COSH
!/ ------------------------------------------------------------------- /
!/
      FUNCTION CMPLX_TANH2(X)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         24-Mar-2016 |
!/                  +-----------------------------------+
!/
!/    24-Mar-2016 : Origination.                        ( version 5.10 )
!/                                                      ( Q. Liu )
!/
!  1. Purpose :
!     We may encounter overflow error for the above tanh function as kh
!     becomes huge. This is another version of tanh function
!
!  2. Method :
!
!     See https://en.wikipedia.org/wiki/Hyperbolic_function
!     tanh(x) = (exp(x) - exp(-x)) / (exp(x) + exp(-x))
!             = (1 - exp(-2x)) / (1 + exp(-2x))
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     Name     Type      Intent  Description
!     ----------------------------------------------------------------
!      X       CMPL(D)     I     a double-precision complex var.
!     ----------------------------------------------------------------
!      * Note, this subr. will be only called by NR_CORR, so for
!        simplicity, I only use double-precision complex var. as input.
!
!  4. Subroutines used :

!      Name        Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE      Subr. W3SERVMD Subroutine tracing.
!      CMPLX_SINH  Func. /        sinh for complex var.
!      CMPLX_COSH  Func. /        cosh for complex var.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      NR_CORR   Subr. /        Newton-Raphson correction term.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!       Calculating tanh in this way may have problems when x ->
!       -inf. But in our cases x is alway >0.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      COMPLEX(KDPC), INTENT(IN)  :: X
      COMPLEX(KDPC)              :: CMPLX_TANH2
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      CALL STRACE (IENT, 'CMPLX_TANH2')
#endif
!/
      CMPLX_TANH2 = (1 - EXP(-2*X)) / (1 + EXP(-2*X))
!/
!/ End of CMPLX_TANH2 ------------------------------------------------ /
!/
      END FUNCTION CMPLX_TANH2
!/
!/ ------------------------------------------------------------------- /
!/
      SUBROUTINE INIT_RANDOM_SEED()
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Q. Liu                  |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         24-Mar-2016 |
!/                  +-----------------------------------+
!/
!/    24-Mar-2016 : Origination.                        ( version 5.10 )
!/                                                      ( Q. Liu )
!/    24-Mar-2016 : Borrowed from Fortran Wiki          ( Q. Liu )
!
!  1. Purpose :
!
!     Initialize the random seed based on the system's time.
!
!  2. Method :
!
!     See http://fortranwiki.org/fortran/show/random_seed
!
!  3. Parameters :
!
!  4. Subroutines used :
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      NR_ROOT   Func. /        Newton-Raphson root finding.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
      INTEGER                            :: I, N, CLOCK
      INTEGER, DIMENSION(:), ALLOCATABLE :: SEED
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      CALL STRACE (IENT, 'INIT_RANDOM_SEED')
#endif
!/
      CALL RANDOM_SEED(SIZE = N)
      ALLOCATE(SEED(N))
!
      CALL SYSTEM_CLOCK(COUNT=CLOCK)
!
      SEED = CLOCK + 37 * (/ (I - 1, I = 1, N) /)
      CALL RANDOM_SEED(PUT = SEED)
!
      DEALLOCATE(SEED)
!/
!/ End of INIT_RANDOM_SEED ------------------------------------------- /
!/
      END SUBROUTINE INIT_RANDOM_SEED
!/
!/ ------------------------------------------------------------------- /
!/
!/ End of module W3SIC5MD -------------------------------------------- /
!/
      END MODULE W3SIC5MD
!/ ------------------------------------------------------------------- /
