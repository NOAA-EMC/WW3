!> @file
!> @brief Calculate ice dissipation source term S_{ice}.
!>
!> @author E. Rogers
!> @author S. Zieger
!> @author F. Ardhuin
!> @author G. Boutin
!> @date   05-Jan-2018
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Calculate ice dissipation source term S_{ice}.
!>
!> @author E. Rogers
!> @author S. Zieger
!> @author F. Ardhuin
!> @author G. Boutin
!> @date   05-Jan-2018
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>
MODULE W3SIC2MD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           E. Rogers               |
  !/                  |           S. Zieger               |
  !/                  |     F. Ardhuin & G. Boutin        |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         05-Jan-2018 |
  !/                  +-----------------------------------+
  !/
  !/    10-Mar-2014 : Generalization with turbulent BL    ( version 5.01 )
  !/    05-Jan-2018 : Addition of floe size effect        ( version 6.04 )
  !/
  !/    For updates see W3SIC1 documentation.
  !/
  !  1. Purpose :
  !
  !     Calculate ice dissipation source term S_{ice}.
  !          Exponential decay rate according to Liu et al., which
  !          uses as input: 1) ice thickness, and 2) an eddy
  !          viscosity parameter. This method is non-uniform in
  !          frequency. This is discussed further below, in
  !          subroutine "LIU_REVERSE_DISPERSION".
  !
  !          Includes generalization by F. Ardhuin with viscous and tubulent
  !          boundary layers. That part is activating by setting namelist
  !          parameters that define the under-ice roughness and a friction
  !          coefficient. For example: &IC2 IC2TURB = 1. , IC2ROUGH =0.0001
  !
  !        References for Subtype 2:
  !              Liu et al.    1991: JGR 96 (C3), 4605-4621
  !              Liu and Mollo 1988: JPO 18       1720-1712
  !              Stopa et al.  2016: The Cryosphere
  !
  !  2. Variables and types :
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      W3SIC2                 Subr. Public   Ice source term.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !     See subroutine documentation.
  !
  !  5. Remarks :
  !
  !     Reference:Rogers, W.E. and M.D. Orzech, 2013: Implementation and
  !        Testing of Ice and Mud Source Functions in WAVEWATCH III(R),
  !        NRL/MR/7320--13-9462, 31pp.
  !        available from http://www7320.nrlssc.navy.mil/pubs.php
  !        Direct link:
  !        http://www7320.nrlssc.navy.mil/pubs/2013/rogers2-2013.pdf
  !
  !  6. Switches :
  !
  !     See subroutine documentation.
  !
  !  7. Source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  PUBLIC  :: W3SIC2
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief S_{ice} source term using 5 parameters read from input files.
  !>
  !> @param[in]  A      Action density spectrum (1-D).
  !> @param[in]  DEPTH  Local water depth.
  !> @param[in]  ICEH   Ice thickness.
  !> @param[in]  ICEF   Ice Floe diameter.
  !> @param[in]  CG     Group velocities.
  !> @param[in]  WN     Wavenumbers.
  !> @param[in]  IX     Grid index.
  !> @param[in]  IY     Grid index.
  !> @param[out] S      Source term (1-D version).
  !> @param[out] D      Diagonal term of derivative (1-D version).
  !> @param[in]  WN_R   Wavenumbers in ice.
  !> @param[in]  CG_ICE Group velocities in ice.
  !> @param[in]  ALPHA  Exponential decay rate of energy.
  !> @param[in]  R      Ratio of energy to wave energy without ice.
  !>
  !> @author E. Rogers
  !> @author S. Zieger
  !> @author F. Ardhuin
  !> @author G. Boutin
  !> @date   04-Jan-2018
  !>
  SUBROUTINE W3SIC2 (A, DEPTH, ICEH, ICEF, CG, WN, IX, IY, S, D, WN_R, &
       CG_ICE, ALPHA, R)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           E. Rogers               |
    !/                  |           S. Zieger               |
    !/                  |    F. Ardhuin & G. Boutin         |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         04-Jan-2018 |
    !/                  +-----------------------------------+
    !/
    !/    16-Oct-2012 : Origination.                        ( version 4.04 )
    !/                                                        (E. Rogers)
    !/    09-Oct-2013 : W3SIC1 SUBTYPE=2 outsourced to W3SIC2 (S. Zieger)
    !/    10-Mar-2014 : Generalization with turbulent BL    ( version 5.01 )
    !/    16-Feb-2016 : Passes ICEH as parameter            ( version 5.10 )
    !/    02-May-2016 : Call to Liu disp moved to w3srce    ( version 5.10 )
    !/    04-Jan-2018 : Includes floe size dependance       ( version 6.02 )
    !/        FIXME   : Move field input to W3SRCE and provide
    !/     (S.Zieger)   input parameter to W3SIC1 to make the subroutine
    !/                : versatile for point output processors ww3_outp
    !/                  and ww3_ounp.
    !/
    !/    Copyright 2009 National Weather Service (NWS),
    !/       National Oceanic and Atmospheric Administration.  All rights
    !/       reserved.  WAVEWATCH III is a trademark of the NWS.
    !/       No unauthorized use without permission.
    !/
    !  1. Purpose :
    !
    !     S_{ice} source term using 5 parameters read from input files.
    !     These parameters are allowed to vary in space and time.
    !     The parameters control the exponential decay rate k_i
    !     Since there are 5 parameters, this permits description of
    !     dependence of k_i on frequency or wavenumber.
    !
    !/ ------------------------------------------------------------------- /
    !
    !  2. Method :
    !
    !     Regarding i/o (general to all Sice modules): S_{ice} source term
    !     is calculated using up to 5 parameters read from input files.
    !     These parameters are allowed to vary in space and time.
    !     The parameters control the exponential decay rate k_i
    !     Since there are 5 parameters, this permits description of
    !     dependence of k_i on frequency or wavenumber.
    !
    !     Sea ice affects the wavenumber k of wind-generated ocean waves.
    !     The ice-modified wavenumber can be expressed as a complex number
    !     k = k_r + i*k_i, with the real part k_r representing impact of
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
    !     Please note that S is source term for action.
    !
    !     Notes regarding numerics:
    !     (Note by F. Ardhuin: these may not apply in version 5 thanks to splitting
    !                          of ice source terms and implicit integration in W3SRCE)
    !     Experiments with constant k_i values suggest that :
    !       for dx=20.0 km, k_i should not exceed 3.5e-6
    !      (assumes 2.7% Hs error in my particular test case is intolerable)
    !       for dx=5.0 km,  k_i should not exceed 2.0e-5
    !       for dx=2.5 km,  k_i should not exceed 5.0e-5
    !       for dx=1.0 km,  k_i should not exceed 2.0e-4
    !       for dx=0.35 km, error is less than 2.1% for all k_i tested
    !       for dx=0.10 km, error is less than 1.3% for all k_i tested
    !     "Ground truth" used for this is an exponential decay profile.
    !
    !      For reference, ACNFS is 1/12th deg, so delta_latitude=9.25 km.
    !
    !     {put more equations here}
    !
    !     The laminar to turbulent transition is described in
    !         Stopa et al. (The Cryosphere, 2016).
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.   I   Action density spectrum (1-D)
    !       DEPTH   Real   I   Local water depth
    !       ICEH    Real   I   Ice thickness
    !       CG      R.A.   I   Group velocities
    !       WN      R.A.   I   Wavenumbers
    !       IX,IY   I.S.   I   Grid indices
    !       S       R.A.   O   Source term (1-D version)
    !       D       R.A.   O   Diagonal term of derivative (1-D version)
    !       WN_R    R.A.   I   Wavenumbers in ice
    !       CG_ICE  R.A.   I   Group velocities in ice
    !       ALPHA   R.A.   I   Exponential decay rate of energy
    !       R       R.A.   I   Ratio of energy to wave energy without ice
    !       ICEF    Real   I   Ice Floe diameter
    !
    !       imported via module:
    !       ICEP2   R.A.   I   Eddy viscosity
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing (!/S switch).
    !      PRT2DS    Subr. W3ARRYMD Print plot output (!/T1 switch).
    !      OUTMAT    Subr. W3ARRYMD Matrix output (!/T2 switch).
    !     ----------------------------------------------------------------
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
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3ODATMD, ONLY: NDSE
    USE W3SERVMD, ONLY: EXTCDE
    USE W3DISPMD
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, MAPWN, IC2PARS, DDEN,  &
         FLAGLL, YGRD, GTYPE, RLGTYPE
    USE W3IDATMD, ONLY: INFLAGS2,ICEP1,ICEP2,ICEP3,ICEP4,ICEP5,ICEI
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
    REAL, INTENT(IN)       :: A(NSPEC), DEPTH, ICEH
    REAL, INTENT(IN)       :: CG(NK),   WN(NK)
    REAL, INTENT(OUT)      :: S(NSPEC), D(NSPEC)
    REAL, INTENT(IN)       :: ALPHA(NK) ! exponential (spatial) decay rate for energy (1/m)
    INTEGER, INTENT(IN)    :: IX, IY
    REAL, INTENT(IN)       :: WN_R(NK), CG_ICE(NK), R(NK)
    REAL, INTENT(IN)       :: ICEF ! Hypothesis: friction does not occur for broken ice

    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T0
    REAL                    :: DOUT(NK,NTH)
#endif
    INTEGER                 :: IKTH, IK
    REAL                    :: D1D(NK)        !In SBT1: D1D was named "CBETA"
    REAL                    :: ICECOEF1, ICECOEF2, ICECONC
    REAL, ALLOCATABLE       :: WN_I(:)  ! exponential (spatial) decay rate for amplitude (1/m)
    REAL                    :: VISCM=1.83E-6 ! molecular viscosity of water at freezing
    REAL                    :: PTURB, PVISC, DTURB, DVISC,   &
         SMOOTH, RE, UORB, AORB, EB,   &
         DELI1, DELI2, FW, XI, FTURB,  &
         CG_EFF(NK), WLG_R(NK), SMOOTH_DMAX(NK)
    INTEGER                 :: IND, ITH, IS
    LOGICAL                 :: NOICE=.FALSE.
    ! Warning, ALPHA = 2 * WN_I -> Makes WN_I useless, doesnt it ?
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3SIC2')
#endif
    !
    ! 0.  Initializations ------------------------------------------------ *
    !
    D        = 0.0
    !
    ALLOCATE(WN_I(NK))
    WN_I     = 0.0
    ICECOEF1 = ICEH
    ICECOEF2 = 0.0
    ICECONC  = 0.0
    CG_EFF = 0.
    SMOOTH_DMAX(:)=1.
    !
    IF (INFLAGS2(-7))ICECOEF1 = ICEH
    IF (INFLAGS2(-6))ICECOEF2 = ICEP2(IX,IY)
    IF (INFLAGS2(4))  ICECONC = ICEI(IX,IY)
    !
    !
    ! 1.  No ice --------------------------------------------------------- /
    !
    NOICE=.FALSE.
    IF (ICECOEF1==0.0) NOICE=.TRUE.
    IF (INFLAGS2(4).AND.(ICECONC==0.0)) NOICE=.TRUE.

    IF ( NOICE ) THEN
      D = 0.0
      !
      ! 2.  Ice ------------------------------------------------------------ /
    ELSE
      !
      ! 2.a Set constant(s) and write test output -------------------------- /
      !
      !         (none)
      !
#ifdef W3_T38
      WRITE (NDST,9000) DEPTH,ICECOEF1,ICECOEF2
#endif
      !
      ! 2.b Make calculations ---------------------------------------------- /

      !  ICECOEF1 = H_ICE
      !  ICECOEF2 = VISC
      !
      ! Branches out depending on choice of dispersion relation...
      ! by default IC2PARS(1)=0, and attenuation computed as described in Stopa et al. 2016
      !
      IF (IC2PARS(1).GT.0.5) THEN
        IF (.NOT.INFLAGS2(-7))THEN
          WRITE (NDSE,1001) 'ICE PARAMETER 1'
          CALL EXTCDE(2)
        ENDIF
        IF (.NOT.INFLAGS2(-6))THEN
          WRITE (NDSE,1001) 'ICE PARAMETER 2'
          CALL EXTCDE(2)
        ENDIF
        !
        WN_I(:) = 0.5 * ALPHA(:) !  ALPHA=2*WN_I
        DO IK=1, NK
          !            recall that D=S/E=-2*Cg*k_i
          !            Note: We should not use CG_ICE here unless CG_ICE is also
          !            used for advection in w3wavemd.ftn (see lines for IC3
          !            there).
          D1D(IK)= -2.0 * CG(IK) * WN_I(IK)
        END DO
        !
        ! Alternative by F.A.: generalization to a turbulent boundary layer
        !                      uses the ice-free dispersion, to be updated later
        !
      ELSE ! goes here if IC2PARS(1).LE.0.5 (this is the default behavior)
        IF (IC2PARS(2).GT.0.) THEN
          UORB=0.
          AORB=0.
          FTURB = IC2PARS(2)
          ! Special treatment in the southern ocean ...
          IF (IC2PARS(7).GT.0) THEN
            IF (YGRD(IY,IX).LT.0.AND.GTYPE.EQ.RLGTYPE.AND.FLAGLL) FTURB = IC2PARS(7)
          END IF
          DO IK=1, NK
            EB  = 0.
            DO ITH=1, NTH
              IS=ITH+(IK-1)*NTH
              EB  = EB  + A(IS)
            END DO
            !
            !  UORB and AORB are the variances of the orbital velocity and surface elevation
            ! of the water relative to the ice ... this is only correct if the ice layer
            ! does not move. This should is changed by taking into account DMAX when IC2DMAX > 0:
            !
#ifdef W3_IS2
            IF (IC2PARS(8).GT.0) THEN
              WLG_R(IK)=TPI/WN_R(IK)
              SMOOTH_DMAX(IK)= (0.5*(1+TANH((ICEF-IC2PARS(8)*WLG_R(IK))/(ICEF*0.5))))**2
            END IF
#endif
            !
            IF (R(IK).GT.1.) THEN
              UORB = UORB + EB * SMOOTH_DMAX(IK)* SIG(IK)**2 * DDEN(IK) / CG(IK) &
                   / (R(IK)*CG_ICE(IK)/CG(IK))
              AORB = AORB + EB * SMOOTH_DMAX(IK)             * DDEN(IK) / CG(IK) &
                   / (R(IK)*CG_ICE(IK)/CG(IK)) !deep water only
            ELSE
              UORB = UORB + EB * SMOOTH_DMAX(IK) *SIG(IK)**2 * DDEN(IK) / CG(IK)
              AORB = AORB + EB * SMOOTH_DMAX(IK)             * DDEN(IK) / CG(IK) !deep water only
            END IF

          END DO
          !
          AORB = 2*SQRT(AORB)  ! significant amplitude
          UORB = 2*SQRT(UORB)  ! significant amplitude

          RE = UORB*AORB / VISCM
          SMOOTH = 0.5*TANH((RE-IC2PARS(4))/IC2PARS(5))
          PTURB=(0.5+SMOOTH)
          PVISC=(0.5-SMOOTH)

          XI=(ALOG10(MAX(AORB/IC2PARS(3),3.))-ABMIN)/DELAB
          IND  = MIN (SIZEFWTABLE-1, INT(XI))
          DELI1= MIN (1. ,XI-FLOAT(IND))
          DELI2= 1. - DELI1
          FW =FWTABLE(IND)*DELI2+FWTABLE(IND+1)*DELI1
          DTURB= FTURB*FW*UORB/GRAV
        ELSE ! so case of IC2PARS(2).LE.0.
          DTURB = 0.
          PTURB = 0.
          PVISC = 1.
        END IF ! IF (IC2PARS(2).GT.0.)
        !
        DO IK=1, NK
          ! WN_R is used here but warning, this is only OK for unbroken ice
          DVISC = IC2PARS(6) * WN_R(IK) * SQRT(VISCM* SIG(IK) / 2.)
          D1D(IK) = -1.*(PTURB*MAX(DTURB*SIG(IK)**2,DVISC) + PVISC*DVISC) &
               *SMOOTH_DMAX(IK)
        END DO
      END IF !  IF (IC2PARS(1).GT.0.5)

      !
      ! 2.c Fill diagional matrix
      !
      DO IKTH=1, NSPEC
        D(IKTH) = D1D(MAPWN(IKTH))
      END DO
      !
    END IF !    IF ( NOICE ) THEN
    !
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
1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3SIC2 : '/               &
         '     ',A,' REQUIRED BUT NOT SELECTED'/)
    !
#ifdef W3_T38
9000 FORMAT (' TEST W3SIC2 : DEPTH,ICECOEF1  : ',2E10.3)
#endif
    !/
    !/ End of W3SIC2 ----------------------------------------------------- /
    !/
  END SUBROUTINE W3SIC2

  !/
  !/ End of module W3SIC2MD -------------------------------------------- /
  !/
END MODULE W3SIC2MD
