!> @file
!> @brief Calculate ice source term S_{ice} according to simple methods.
!>
!> @author C. Collins
!> @author E. Rogers
!> @date   21-Jan-2015
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Calculate ice source term S_{ice} according to simple methods.
!>
!> @details Attenuation is a function of frequency and specified directly
!>  by the user. Example: a function is based on an exponential fit to
!>  the empirical data of Wadhams et al. (1988).
!>
!> @author C. Collins
!> @author E. Rogers
!> @date   21-Jan-2015
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>
MODULE W3SIC4MD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           C. Collins              |
  !/                  |           E. Rogers               |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         21-Jan-2015 |
  !/                  +-----------------------------------+
  !/
  !/    For updates see W3SIC4 documentation.
  !/
  !  1. Purpose :
  !
  !     Calculate ice source term S_{ice} according to simple methods.
  !          Attenuation is a function of frequency and specified directly
  !          by the user. Example: a function is based on an exponential fit to
  !          the empirical data of Wadhams et al. (1988).
  !
  !  2. Variables and types :
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      W3SIC4    Subr. Public   ice source term.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !     See subroutine documentation.
  !
  !  5. Remarks :
  !
  !     Documentation of IC4:
  !         1) Collins and Rogers, NRL Memorandum report 2017
  !         ---> "A Source Term for Wave Attenuation by Sea
  !               Ice in WAVEWATCH III® : IC4"
  !         ---> describes original IC4 methods, 1 to 6
  !         2) Rogers et al., NRL Memorandum report 2018a
  !         ---> "Forecasting and hindcasting waves in and near the
  !              marginal ice zone: wave modeling and the ONR “Sea
  !              State” Field Experiment"
  !         ---> IC4 method 7 added
  !         2) Rogers et al., NRL Memorandum report 2018b
  !         ---> "Frequency Distribution of Dissipation of Energy of
  !               Ocean Waves by Sea Ice Using Data from Wave Array 3 of
  !               the ONR “Sea State” Field Experiment"
  !         ---> New recommendations for IC4 Method 2 (polynomial fit)
  !              and IC4 Method 6 (step function via namelist)
  !
  !     Other source material :
  !        *** Wadhams et al. JGR 1988
  !        *** Meylan et al. GRL 2014
  !        *** Kohout & Meylan JGR 2008 in Horvat & Tziperman Cryo. 2015
  !        *** Kohout et al. Nature 2014
  !        *** Doble et al. GRL 2015
  !        *** Rogers et al. JGR 2016
  !        *** Meylan et al. JGR 2018
  !        *** Yu et al. JGR 2019
  !        *** Liu et al. JPO 2020
  !        *** Rogers et al. CRST 2021 (RMK2021)
  !        *** Rogers et al. tech. rep. 2021 (RYW2021)
  !        *** Yu et al. CRST 2022
  !        *** Yu JMSE 2022
  !
  !  6. Switches :
  !
  !     See subroutine documentation.
  !
  !  7. Source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  PUBLIC :: W3SIC4
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief S_{ice} source term using 5 parameters read from input files.
  !>
  !> @param[in]  A      Action density spectrum (1-D).
  !> @param[in]  DEPTH  Local water depth.
  !> @param[in]  CG     Group velocities.
  !> @param[in]  IX     Grid index.
  !> @param[in]  IY     Grid index.
  !> @param[out] S      Source term (1-D version).
  !> @param[out] D      Diagonal term of derivative (1-D version).
  !>
  !> @author C. Collins
  !> @author E. Rogers
  !> @date   24-Feb-2017
  !>
  SUBROUTINE W3SIC4 (A, DEPTH, CG, IX, IY, S, D)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Collins              |
    !/                  |           E. Rogers               |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         24-Feb-2017 |
    !/                  +-----------------------------------+
    !/
    !/    03-Dec-2015 : Origination                         ( version 5.09 )
    !/                     (starting from IC1)                (C. Collins)
    !/    03-Dec-2015 : W3SIC4 created, Methods 1,2,3,4       (C. Collins)
    !/    21-Jan-2016 : IC4 added to NCEP repository          (E. Rogers)
    !/    27-Jan-2016 : Method 5 added (step function)        (E. Rogers)
    !/    08-Apr-2016 : Method 6 added (namelist step funct.) (E. Rogers)
    !/    24-Feb-2017 : Corrections to Methods 1,2,3,4        (E. Rogers)
    !/    13-Apr-2017 : Method 7 added (Doble et al. 2015)    (E. Rogers)
    !/    11-Jan-2024 : Method 8 added (Meylan et al. 2018)   (E. Rogers)
    !/    11-Jan-2024 : Method 9 added (Rogers et al., 2021)
    !/                                      denoted "RYW2021" (E. Rogers)
    !/
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
    !     Apply parametric/empirical functions, e.g. from the literature.
    !     1) Exponential fit to Wadhams et al. 1988, Table 2
    !     2) Polynomial fit, Eq. 3 from Meylan et al. 2014
    !     3) Quadratic fit to Kohout & Meylan'08 in Horvat & Tziperman'15
    !        Here, note that their eqn is given as ln(alpha)=blah, so we
    !        have alpha=exp(blah).
    !        Note from ER:
    !        This implementation has two things to keep in mind:
    !          1) This is a scattering model, applied as dissipation,
    !             which is not correct.
    !          2) This is not actually HT15! The alpha of HT15 has
    !             different meaning from alpha of CR17, as follows:
    !             HT15: decay is exp(-alpha*Lambda) where Lambda
    !                   is the number of floes encountered.
    !             CR17: decay is exp(-alpha*x)
    !             Thus, CR17's implementation of HT15 is equivalent to
    !             the actual HT15 only if one assumes one floe encountered
    !             per meter. This is very strong attenuation, as shown in
    !             Figure 3 of CR17! This problem might be fixed by computing
    !             an encounter interval length scale from an a_ice and d_ice
    !             provided by the user...or a length scale provided by the 
    !             user.
    !             See also: page 3 of Rogers et al. (RYW2021).
    !     4) Eq. 1 from Kohout et al. 2014
    !
    !     5) Simple step function for ki as a function of frequency
    !          with up to 4 "steps". Controlling parameters KIx and FCx are
    !          read in as input fields, so they may be nonstationary and
    !          non-uniform in the same manner that ice concentration and
    !          water levels may be nonstationary and non-uniform.
    !                                          444444444444
    !                               33333333333
    !                   222222222222
    !      1111111111111
    !                  ^            ^          ^
    !                  |            |          |
    !                  5            6          7
    !      Here, 1 indicates ki=KI1=ICECOEF1 (ICEP1)
    !            2 indicates ki=KI2=ICECOEF2 (ICEP2)
    !            3 indicates ki=KI3=ICECOEF3 (ICEP3)
    !            4 indicates ki=KI4=ICECOEF4 (ICEP4)
    !            5 indicates freq cutoff #1 =FC5=ICECOEF5 (ICEP5)
    !            6 indicates freq cutoff #2 =FC6=ICECOEF6 (MUDD)
    !            7 indicates freq cutoff #3 =FC7=ICECOEF7 (MUDT)
    !     freq cutoff is given in Hz, freq=1/T (not sigma)
    !     Examples using hindcast, inversion with uniform ki:
    !        5.1) Beaufort Sea, AWAC mooring, 2012, Aug 17 to 20
    !             0.0418 Hz to 0.15 Hz : ki=10e-6
    !             0.15 Hz to 0.175 Hz : ki=11e-6
    !             0.175 Hz to 0.25 Hz : ki=15e-6
    !             0.25 Hz to 0.5 Hz : ki=25e-6
    !        5.2) Beaufort Sea, AWAC mooring, 2012, Oct 27 to 30
    !             0.0418 Hz to 0.1 Hz : ki=5e-6
    !             0.1 Hz to 0.12 Hz : ki=7e-6
    !             0.12 Hz to 0.16 Hz : ki=15e-6
    !             0.16 Hz to 0.5 Hz : ki=100e-6
    !             ICEP1=KI1=5.0e-6
    !             ICEP2=KI2=7.0e-6
    !             ICEP3=KI3=15.0e-6
    !             ICEP4=KI4=100.0e-6
    !             ICEP5=FC5=0.10
    !             MUDD=FC6=0.12
    !             MUDT=FC7=0.16
    !             In terms of the 3-character IDs for "Homogeneous field
    !             data" in ww3_shel.inp, these are, respectively, IC1, IC2,
    !             IC3, IC4, IC5, MDN, MTH, and so this might look like:
    !                'IC1' 19680606 000000   5.0e-6
    !                'IC2' 19680606 000000   7.0e-6
    !                'IC3' 19680606 000000   15.0e-6
    !                'IC4' 19680606 000000   100.0e-6
    !                'IC5' 19680606 000000   0.10
    !                'MDN' 19680606 000000   0.12
    !                'MTH' 19680606 000000   0.16
    !
    !     6) Simple step function for ki as a function of frequency
    !          with up to 16 "steps". Controlling parameters KIx and FCx are
    !          read in as namelist parameters, so they are stationary and
    !          uniform. (If 16 steps is not enough, the number of steps can be
    !          increased at compile time by changing NIC4 in w3gdatmd.ftn.)
    !          The last non-zero FCx value should be a large number, e.g. 99 Hz
    !
    !                                          4444444444  <--- ki=ic4_ki(4)
    !                               3333333333             <--- ki=ic4_ki(3)
    !                   2222222222                         <--- ki=ic4_ki(2)
    !      11111111111                                     <--- ki=ic4_ki(1)
    !                 ^           ^           ^         ^
    !                 |           |           |         |
    !          ic4_fc(1)   ic4_fc(2)   ic4_fc(3) ic4_fc(4)=large number
    !       Example: Beaufort Sea, AWAC mooring, 2012, Oct 27 to 30
    !           &SIC4  IC4METHOD = 6,
    !                  IC4KI =    0.50E-05,   0.70E-05,   0.15E-04,
    !                             0.10E+00,   0.00E+00,   0.00E+00,
    !                             0.00E+00,   0.00E+00,   0.00E+00,
    !                             0.00E+00,
    !                  IC4FC =    0.100,      0.120,      0.160,
    !                             99.00,      0.000,      0.000,
    !                             0.000,      0.000,      0.000,
    !                             0.000
    !                             /
    !
    !     7) Doble et al. (GRL 2015), eq. 3. This is a function of ice
    !        thickness and wave period.
    !        ALPHA  = 0.2*(T^(-2.13)*HICE or
    !        ALPHA  = 0.2*(FREQ^2.13)*HICE
    !
    !     8) Meylan et al. (JGR 2018), eq. 48. "Model with Order 3 Power
    !        Law". The is denoted as the "M2" model by Liu et al. (JPO 2020)
    !        It is a function of ice thickness and wave period.
    !        ki  = ChfM2*h_ice*freq^3
    !        where ChfM2 is a coefficient of proportionality which formally
    !        includes viscosity, density, and gravity parameters, see
    !        Meylan et al. (JGR 2018) for details.
    !        ChfM2 has units of s3/m2
    !        It is equation 53 in Meylan et al. (2018) and equation 16 in
    !        Liu et al. (2020).
    !        This method is functionally the same as the "M2" model in IC5
    !        in WW3 (IC5 w/IC5VEMOD=3) and is redundantly included here as
    !        IC4M8 because it is in the same "family" as IC4M7 and IC4M9,
    !        being in the form of:
    !        ki=Chf * h_ice^m * freq^n .
    !        Calibrations:
    !        * Liu et al. has ChfM2=eta*(2*pi)^3/(1025*9.81^2)
    !        ** eta=14.0 for "Sikuliaq" case of Liu et al., so ChfM2=0.035
    !        ** eta=3.0 for "SIPEX" case of Liu et al., so ChfM2=0.0075
    !        * Rogers et al. (tech rep. 2021, "RYW2021") :
    !        ** Fit to Rogers et al. (CRST 2021 "RMK2021") ChfM2=0.059 (*SD*)
    !        suggested default is marked with "(*SD*)", for consistency
    !          with SWAN (v41.31AB or later)
    !
    !     9) Rogers et al. (tech. rep. 2021, "RYW2021"): the "monomial power
    !        fit" described in section 2.2.3. It is the general form above,
    !        ki=Chf * h_ice^m * freq^n but is constrained such that m=n/2-1.
    !        This constraint is derived by RYW2021 by invoking the scaling from
    !        Yu et al. (2019), which is based on Reynolds number with ice
    !        thickness as the relevant length scale.
    !        This is also given as equation 2 in Yu et al. (CRST 2022).
    !        Some calibrations are as follows:
    !        * RYW2021, calibration to RMK2021: Chf=2.9 and n=4.5  (*SD*)
    !        * Yu et al. (2022) calibration to RMK2021 : Chf=2.4 and n=4.46
    !          (noting that c_n=0.108 and Chf=c_n*(2*pi/sqrt(g))^n)
    !        * Yu (2022) adjusted the prior calibration to get better fit
    !          to higher frequency lab measurements and got:
    !          Chf=7.89 and n=4.8
    !        suggested default is marked with "(*SD*)", for consistency
    !          with SWAN (v41.31AB or later)
    !
    !     ------------------------------------------------------------------
    !
    !     For all methods, the user can specify namelist
    !     variables IC4FMIN and IC4KIBK such as:
    !     &SIC4 IC4METHOD = [...], IC4FMIN=0.08, IC4KIBK=1.0e-7, [...]
    !     This accomodates the situation where the empirically-derived
    !     dissipation is uncertain for the lowest frequencies, which can be
    !     the case if estimated dissipation rate is so small that it falls
    !     in the noise level for the estimation method. (This is common,
    !     since some ice types cause only very weak dissipation
    !     to low frequencies.) In the example above, the amplitude
    !     dissipation rate ki is set to some low background level
    !     dissipation IC4KIBK=1.0e-7 1/m when model frequency is less than
    !     0.08 Hz.
    !
    !     More verbose description of implementation of Sice in WW3:
    !      See documentation for IC1
    !
    !     Notes regarding numerics:
    !      See documentation for IC1
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.  I   Action density spectrum (1-D)
    !       DEPTH   Real  I   Local water depth
    !       CG      R.A.  I   Group velocities.
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
    !     For questions, comments and/or corrections, please refer to:
    !        Method 1 : C. Collins
    !        Method 2 : C. Collins
    !        Method 3 : C. Collins
    !        Method 4 : C. Collins
    !        Method 5 : E. Rogers
    !        Method 6 : E. Rogers
    !        Method 7 : E. Rogers
    !
    !     ALPHA = 2 * WN_I
    !     Though it may seem redundant/unnecessary to have *both* in the
    !       code, we do it this way to make the code easier to read and
    !       relate to other codes and source material, and hopefully avoid
    !       mistakes.
    !/ ------------------------------------------------------------------- /
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
    USE CONSTANTS, ONLY: TPI
    USE W3ODATMD, ONLY: NDSE
    USE W3SERVMD, ONLY: EXTCDE
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, MAPWN, IC4PARS, DDEN, &
                        IC4_KI, IC4_FC, IC4_CN, NIC4, IC4_FMIN,    &
                        IC4_KIBK
    USE W3IDATMD, ONLY: ICEP1, ICEP2, ICEP3, ICEP4, ICEP5, &
                        MUDT, MUDV, MUDD, INFLAGS2

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
    REAL, INTENT(IN)        :: CG(NK),   A(NSPEC), DEPTH
    REAL, INTENT(OUT)       :: S(NSPEC), D(NSPEC)
    INTEGER, INTENT(IN)     :: IX, IY
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T0
    INTEGER                 :: ITH
    REAL                    :: DOUT(NK,NTH)
#endif
    INTEGER                 :: IKTH, IK, ITH, IC4METHOD, IFC
    REAL                    :: D1D(NK), EB(NK)
    REAL                    :: ICECOEF1, ICECOEF2, ICECOEF3, &
                               ICECOEF4, ICECOEF5, ICECOEF6, &
                               ICECOEF7, ICECOEF8
    REAL                    :: CICE1,CICE2,CICE3,CICE4,CICE5 ! temporary variables
    REAL                    :: KI1,KI2,KI3,KI4,FC5,FC6,FC7
    REAL                    :: HS, EMEAN, HICE
    REAL                    :: Chf,mpow,npow
    REAL, ALLOCATABLE       :: WN_I(:)  ! exponential decay rate for amplitude
    REAL, ALLOCATABLE       :: ALPHA(:) ! exponential decay rate for energy
    REAL, ALLOCATABLE       :: FREQ(:) ! wave frequency
    REAL, ALLOCATABLE       :: MARG1(:), MARG2(:) ! Arguments for M2
    REAL, ALLOCATABLE       :: KARG1(:), KARG2(:), KARG3(:) !Arguments for M3
    LOGICAL                 :: NML_INPUT ! if using namelist input for M2

    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3SIC4')
#endif
    !
    ! 0.  Initializations ------------------------------------------------ *
    !
    D        = 0.0
    !
    ALLOCATE(WN_I(0:NK+1))
    ALLOCATE(ALPHA(0:NK+1))
    ALLOCATE(MARG1(0:NK+1))
    ALLOCATE(MARG2(0:NK+1))
    ALLOCATE(KARG1(0:NK+1))
    ALLOCATE(KARG2(0:NK+1))
    ALLOCATE(KARG3(0:NK+1))
    ALLOCATE(FREQ(0:NK+1))
    MARG1    = 0.0
    MARG2    = 0.0
    KARG1    = 0.0
    KARG2    = 0.0
    KARG3    = 0.0
    WN_I     = 0.0
    ALPHA    = 0.0
    ICECOEF1 = 0.0
    ICECOEF2 = 0.0
    ICECOEF3 = 0.0
    ICECOEF4 = 0.0
    ICECOEF5 = 0.0
    ICECOEF6 = 0.0
    ICECOEF7 = 0.0
    ICECOEF8 = 0.0
    HS       = 0.0
    HICE     = 0.0
    EMEAN    = 0.0
    FREQ=SIG/TPI
    !
    !     IF (.NOT.INFLAGS2(-7))THEN
    !        WRITE (NDSE,1001) 'ICE PARAMETER 1'
    !        CALL EXTCDE(201)
    !     ENDIF
    !
    !   We cannot remove the other use of INFLAGS below,
    !   because we would get 'array not allocated' error for the methods
    !   that don't use MUDV, etc. and don't have MUDV allocated.

    IF (INFLAGS2(-7)) ICECOEF1 = ICEP1(IX,IY) ! a.k.a. IC1
    IF (INFLAGS2(-6)) ICECOEF2 = ICEP2(IX,IY) ! etc.
    IF (INFLAGS2(-5)) ICECOEF3 = ICEP3(IX,IY)
    IF (INFLAGS2(-4)) ICECOEF4 = ICEP4(IX,IY)
    IF (INFLAGS2(-3)) ICECOEF5 = ICEP5(IX,IY)

    ! Borrow from Smud (error if BT8 or BT9)
#ifdef W3_BT8
    WRITE (NDSE,*) 'DUPLICATE USE OF MUD PARAMETERS'
    CALL EXTCDE(202)
#endif
#ifdef W3_BT9
    WRITE (NDSE,*) 'DUPLICATE USE OF MUD PARAMETERS'
    CALL EXTCDE(202)
#endif
    IF (INFLAGS2(-2)) ICECOEF6 = MUDD(IX,IY) ! a.k.a. MDN
    IF (INFLAGS2(-1)) ICECOEF7 = MUDT(IX,IY) ! a.k.a. MTH
    IF (INFLAGS2(0 )) ICECOEF8 = MUDV(IX,IY) ! a.k.a. MVS

    IC4METHOD = IC4PARS(1)
    !
#ifdef W3_T38
    !     Write test output ---------------------------------------------- /
    WRITE (NDST,9000) DEPTH,ICECOEF1,ICECOEF2,ICECOEF3,ICECOEF4
#endif
    !
    ! 1.  Make calculations ---------------------------------------------- /
    !
    ! 1.a Calculate WN_I

    SELECT CASE (IC4METHOD)

    CASE (1) ! IC4M1 : Exponential fit to Wadhams et al. 1988
      ALPHA = EXP(-ICECOEF1 * TPI / SIG - ICECOEF2)
      WN_I = 0.5 * ALPHA

    CASE (2) ! IC4M2 : Polynomial fit, Eq. 3 from Meylan et al. 2014
      !NB: Eq. 3 only includes T^2 and T^4 terms,
      !  which correspond to ICECOEF3, ICECOEF5, so in
      !  regtest: ICECOEF1=ICECOEF2=ICECOEF4=0

      NML_INPUT=.TRUE.
      IF (INFLAGS2(-7).OR.INFLAGS2(-6).OR.INFLAGS2(-5).OR. &
          INFLAGS2(-4).OR.INFLAGS2(-3)) NML_INPUT=.FALSE.

      IF(NML_INPUT)THEN ! get from namelist array

         CICE1=IC4_CN(1)
         CICE2=IC4_CN(2)
         CICE3=IC4_CN(3)
         CICE4=IC4_CN(4)
         CICE5=IC4_CN(5)

      ELSE ! get from input-field array (ICEP1 etc.)

         CICE1=ICECOEF1
         CICE2=ICECOEF2
         CICE3=ICECOEF3
         CICE4=ICECOEF4
         CICE5=ICECOEF5

      ENDIF

      ! CICE1 is C_{ice,1} in Collins and Rogers (2017), for example.
      MARG1 = CICE1 + CICE2*FREQ + CICE3*FREQ**2
      MARG2 = CICE4*FREQ**3 + CICE5*FREQ**4
      ALPHA = MARG1 + MARG2
      WN_I = 0.5 * ALPHA

    CASE (3) ! IC4M3 : Quadratic fit to Kohout & Meylan'08 in Horvat & Tziperman'15
      HICE=ICECOEF1 ! For this method, ICECOEF1=ice thickness
      KARG1 = -0.3203 + 2.058*HICE - 0.9375*(TPI/SIG)
      KARG2 = -0.4269*HICE**2 + 0.1566*HICE*(TPI/SIG)
      KARG3 =  0.0006 * (TPI/SIG)**2
      ALPHA  = EXP(KARG1 + KARG2 + KARG3)
      WN_I = 0.5 * ALPHA

    CASE (4) !Eq. 1 from Kohout et al. 2014
      !Calculate HS
      DO IK=1, NK
        EB(IK) = 0.
        DO ITH=1, NTH
          EB(IK) = EB(IK) + A(ITH+(IK-1)*NTH)
        END DO
      END DO
      DO IK=1, NK
        EB(IK) = EB(IK) * DDEN(IK) / CG(IK)
        EMEAN  = EMEAN + EB(IK)
      END DO
      HS = 4.*SQRT( MAX(0.,EMEAN) )
      ! If Hs < 3 m then do Hs dependent calc, otherwise dH/dx is a constant
      IF (HS <= 3) THEN
        WN_I=ICECOEF1 ! from: DHDX=ICECOEF1*HS and WN_I=DHDX/HS
      ELSE IF (HS > 3) THEN
        WN_I=ICECOEF2/HS ! from: DHDX=ICECOEF2 and WN_I=DHDX/HS
      END IF

    CASE (5) ! Simple step function (time- and/or space-varying)
      ! rename variables for clarity
      KI1=ICECOEF1
      KI2=ICECOEF2
      KI3=ICECOEF3
      KI4=ICECOEF4
      FC5=ICECOEF5
      FC6=ICECOEF6
      FC7=ICECOEF7
      IF((KI1.EQ.0.0).OR.(KI2.EQ.0.0).OR.(KI3.EQ.0.0).OR. &
           (KI4.EQ.0.0).OR.(FC5.EQ.0.0).OR.(FC6.EQ.0.0).OR. &
           (FC7.EQ.0.0))THEN
        WRITE (NDSE,1001)'ICE PARAMETERS'
        CALL EXTCDE(201)
      END IF
      DO IK=1, NK
        ! select ki
        IF(FREQ(IK).LT.FC5)THEN
          WN_I(IK)=KI1
        ELSEIF(FREQ(IK).LT.FC6)THEN
          WN_I(IK)=KI2
        ELSEIF(FREQ(IK).LT.FC7)THEN
          WN_I(IK)=KI3
        ELSE
          WN_I(IK)=KI4
        ENDIF
      END DO

    CASE (6) ! Simple step function (from namelist)

      ! error checking: require at least 3 steps
      IF((IC4_KI(1).EQ.0.0).OR.(IC4_KI(2).EQ.0.0).OR. &
           (IC4_KI(3).EQ.0.0).OR.(IC4_FC(1).EQ.0.0).OR. &
           (IC4_FC(2).EQ.0.0) )THEN
        WRITE (NDSE,1001)'ICE PARAMETERS'
        CALL EXTCDE(201)
      END IF

      DO IK=1, NK
        ! select ki
        DO IFC=1,NIC4
          IF(FREQ(IK).LT.IC4_FC(IFC))THEN
            WN_I(IK)=IC4_KI(IFC)
            EXIT
          END IF
        END DO
      END DO

    CASE (7) ! Doble et al. (GRL 2015)

      HICE=ICECOEF1 ! For this method, ICECOEF1=ice thickness
      DO IK=1,NK
        ALPHA(IK)  = 0.2*(FREQ(IK)**2.13)*HICE
      END DO
      WN_I= 0.5 * ALPHA

    CASE (8) ! Meylan et al. (JGR 2018), Liu et al. (JPO 2020)

      NML_INPUT=.TRUE.
      IF (INFLAGS2(-6)) NML_INPUT=.FALSE.

      IF(NML_INPUT)THEN ! get from namelist array

        Chf=IC4_CN(1) ! Denoted "ChfM2" in documentation

      ELSE ! get from input-field array (ICEP1 etc.)

        Chf=ICECOEF2 ! Denoted "ChfM2" in documentation

      ENDIF

      ! Rename variable, for clarity
      hice=ICECOEF1 ! For this method, ICECOEF1 is ice thickness

      DO IK=1,NK
        WN_I(IK)  = Chf*hice*(FREQ(IK)**3)
      END DO

    CASE (9) ! Rogers et al. (2021) (RYW2021), Yu et al. (JGR 2022)

      NML_INPUT=.TRUE.
      IF (INFLAGS2(-6).OR.INFLAGS2(-5)) NML_INPUT=.FALSE.

      IF(NML_INPUT)THEN ! get from namelist array

        Chf=IC4_CN(1) ! Denoted as same in documentation
        npow=IC4_CN(2) ! Denoted "n" in documentation

      ELSE ! get from input-field array (ICEP1 etc.)

        Chf=ICECOEF2 ! Denoted as same in documentation
        npow=ICECOEF3 ! Denoted "n" in documentation

      ENDIF

      ! Rename variable, for clarity
      hice=ICECOEF1 ! For this method, ICECOEF1 is ice thickness
      ! Compute
      mpow=0.5*npow-1.0 ! Denoted "m" in documentation
      DO IK=1,NK
        WN_I(IK)  = Chf*(hice**mpow)*(FREQ(IK)**npow)
      END DO

    CASE DEFAULT
      WN_I = ICECOEF1 !Default to IC1: Uniform in k

    END SELECT

    !
    ! 1.b Calculate DID
    !
    DO IK=1, NK
      !   SBT1 has: D1D(IK) = FACTOR *  MAX(0., (CG(IK)*WN(IK)/SIG(IK)-0.5) )
      !             recall that D=S/E=-2*Cg*k_i
      IF(FREQ(IK).LT.IC4_FMIN)WN_I(IK)=IC4_KIBK
      !           write(*,*)freq(ik),wn_i(ik),ICECOEF1,' % :: freq,ki,hice' ! temporary code: do not commit to repo uncommented
      D1D(IK) = -2. * CG(IK) * WN_I(IK)

    END DO

    !
    ! 1.c Fill diagional matrix
    !
    DO IKTH=1, NSPEC
      D(IKTH) = D1D(MAPWN(IKTH))
    END DO
    !
    !      END IF
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
1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3SIC4 : '/               &
        '     ',A,' REQUIRED BUT NOT SELECTED'/)
    !
#ifdef W3_T
9000 FORMAT (' TEST W3SIC4 : DEPTH,ICECOEF1  : ',2E10.3)
#endif
    !/
    !/ End of W3SIC4 --------------------------------------------------- /
    !/
  END SUBROUTINE W3SIC4
  !/
  !/ End of module W3SIC4MD ------------------------------------------ /
  !/
END MODULE W3SIC4MD
