!> @file
!> @brief Contains module W3STR1MD.
!>
!> @author A. J. van der Westhuysen @date 13-Jan-2013
!> @author A. Roland @date 22-Feb-2023
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Module for inclusion of triad nonlinear interaction
!>  according to Eldeberky's (1996) Lumped Triad Interaction (LTA)
!>  source term.
!>
!> @author A. J. van der Westhuysen @date 13-Jan-2013
!>
MODULE W3STR1MD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |     A. J. van der Westhuysen      |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         13-Jan-2013 |
  !/                  +-----------------------------------+
  !/
  !/    13 Jan-2013 : Origination, based on SWAN v40.91 code ( version 4.08 )
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Module for inclusion of triad nonlinear interaction according to
  !     Eldeberky's (1996) Lumped Triad Interaction (LTA) source term.
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
  !      W3STR1    Subr. Public   User supplied triad interactions.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      STRACE    Subr. W3SERVMD Subroutine tracing.
  !     ----------------------------------------------------------------
  !
  !  5. Remarks :
  !
  !     WAVEWATCH III is designed as a highly plug-compatible code.
  !     Source term modules can be included as self-contained modules,
  !     with limited changes needed to the interface of routine calls
  !     in W3SRCE, and in the point postprocessing programs only.
  !     Codes submitted for inclusion in WAVEWATCH III should be
  !     self-contained in the way described below, and might be
  !     provided with distributions fully integrated in the data
  !     structure, or as an optional version of this module to be
  !     included by the user.
  !
  !     Rules for preparing a module to be included in or distributed
  !     with WAVEWATCH III :
  !
  !      - Fully document the code following the outline given in this
  !        file, and according to all other WAVEWATCH III routines.
  !      - Provide a file with necessary modifications to W3SRCE and
  !        all other routines that require modification.
  !      - Provide a test case with expected results.
  !      - It is strongly recommended that the programming style used
  !        in WAVEWATCH III is followed, in particular
  !          a) for readability, write as if in fixed FORTRAN format
  !             regarding column use, even though all files are F90
  !             free format.
  !          b) I prefer upper case programming for permanent code,
  !             as I use lower case in debugging and temporary code.
  !
  !     This module needs to be self-contained in the following way.
  !
  !      a) All saved variables connected with this source term need
  !         to be declared in the module header. Upon acceptance as
  !         permanent code, they will be converted to the WAVEWATCH III
  !         dynamic data structure.
  !      b) Provide a separate computation and initialization routine.
  !         In the submission, the initialization should be called
  !         from the computation routine upon the first call to the
  !         routine. Upon acceptance as permanent code, the
  !         initialization routine will be moved to a more appropriate
  !         location in the code (i.e., being absorbed in ww3_grid or
  !         being moved to W3IOGR).
  !
  !     See notes in the file below where to add these elements.
  !
  !  6. Switches :
  !
  !     !/S  Enable subroutine tracing.
  !
  !  7. Source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !     *****************************************
  !     ***    Declare saved variables here   ***
  !     ***  public or private as appropriate ***
  !     *****************************************
  !
  PUBLIC
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Triad interaction source term computed using the Lumped
  !>  Triad Appproximation (LTA) of Eldeberky (1996).
  !>
  !> @verbatim
  !>     The parametrized biphase is given by:
  !>
  !>                                  0.2
  !>     beta = - pi/2 + pi/2 tanh ( ----- )
  !>                                   Ur
  !>
  !>     where Ur is the Ursell number.
  !>
  !>     The source term as function of frequency p is:
  !>
  !>             +      -
  !>     S(p) = S(p) + S(p)
  !>
  !>     in which
  !>
  !>      +
  !>     S(p) = alpha Cp Cg,p (R(p/2,p/2))**2 sin (|beta|) ( E(p/2)**2 -2 E(p) E(p/2) )
  !>
  !>      -          +
  !>     S(p) = - 2 S(2p)
  !>
  !>     with alpha a tunable coefficient and R(p/2,p/2) is the interaction
  !>     coefficient of which the expression can be found in Eldeberky (1996).
  !>
  !>     Note that a slightly adapted formulation of the LTA is used in
  !>     in the SWAN model:
  !>
  !>     - Only positive contributions to higher harmonics are considered
  !>       here (no energy is transferred to lower harmonics).
  !>
  !>     - The mean frequency in the expression of the Ursell number
  !>       is calculated according to the first order moment over the
  !>       zeroth order moment (personal communication, Y.Eldeberky, 1997).
  !>
  !>     - The interactions are calculated up to 2.5 times the mean
  !>       frequency only.
  !>
  !>     - Since the spectral grid is logarithmically distributed in frequency
  !>       space, the interactions between central bin and interacting bin
  !>       are interpolated such that the distance between these bins is
  !>       factor 2 (nearly).
  !>
  !>     - The interactions are calculated in terms of energy density
  !>       instead of action density. So the action density spectrum
  !>       is firstly converted to the energy density grid, then the
  !>       interactions are calculated and then the spectrum is converted
  !>       to the action density spectrum back.
  !> @endverbatim
  !>
  !> @param[in] A Action density spectrum (1-D)
  !> @param[in] CG Group velocities.
  !> @param[in] WN Wavenumbers.
  !> @param[in] DEPTH Mean water depth.
  !> @param[in] IX
  !> @param[out] S Source term (1-D version).
  !> @param[out] D Diagonal term of derivative (1-D version).
  !>
  !> @author A. J. van der Westhuysen  @date 13-Jan-2013
  !>
  SUBROUTINE W3STR1 (A, AOLD, CG, WN, DEPTH, IX, S, D)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |     A. J. van der Westhuysen      |
    !/                  |     A. Roland                     |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         13-Jan-2013 |
    !/                  +-----------------------------------+
    !/
    !/    13 Jan-2013 : Origination, based on SWAN v40.91 code ( version 4.08 )
    !/    05 Oct-2016 : Avoiding divide by zero for EMEAN      ( version 5.15 )
    !/    28 Feb-2023 : Improvement of efficiency and stability ( version 7.xx)
    !/
    !  1. Purpose :
    !
    !     Triad interaction source term computed using the Lumped Triad
    !     Appproximation (LTA) of Eldeberky (1996).
    !
    !  2. Method :
    !
    !     (Taken from SWAN v40.91, based on code by Marcel Zijlema, TU Delft)
    !
    !     The parametrized biphase is given by:
    !
    !                                  0.2
    !     beta = - pi/2 + pi/2 tanh ( ----- )
    !                                   Ur
    !
    !     where Ur is the Ursell number.
    !
    !     The source term as function of frequency p is:
    !
    !             +      -
    !     S(p) = S(p) + S(p)
    !
    !     in which
    !
    !      +
    !     S(p) = alpha Cp Cg,p (R(p/2,p/2))**2 sin (|beta|) ( E(p/2)**2 -2 E(p) E(p/2) )
    !
    !      -          +
    !     S(p) = - 2 S(2p)
    !
    !     with alpha a tunable coefficient and R(p/2,p/2) is the interaction
    !     coefficient of which the expression can be found in Eldeberky (1996).
    !
    !     Note that a slightly adapted formulation of the LTA is used in
    !     in the SWAN model:
    !
    !     - Only positive contributions to higher harmonics are considered
    !       here (no energy is transferred to lower harmonics).
    !
    !     - The mean frequency in the expression of the Ursell number
    !       is calculated according to the first order moment over the
    !       zeroth order moment (personal communication, Y.Eldeberky, 1997).
    !
    !     - The interactions are calculated up to 2.5 times the mean
    !       frequency only.
    !
    !     - Since the spectral grid is logarithmically distributed in frequency
    !       space, the interactions between central bin and interacting bin
    !       are interpolated such that the distance between these bins is
    !       factor 2 (nearly).
    !
    !     - The interactions are calculated in terms of energy density
    !       instead of action density. So the action density spectrum
    !       is firstly converted to the energy density grid, then the
    !       interactions are calculated and then the spectrum is converted
    !       to the action density spectrum back.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.  I   Action density spectrum (1-D)
    !       CG      R.A.  I   Group velocities.
    !       WN      R.A.  I   Wavenumbers.
    !       DEPTH   Real  I   Mean water depth.
    !       EMEAN   Real  I   Mean wave energy.
    !       FMEAN   Real  I   Mean wave frequency.
    !       S       R.A.  O   Source term (1-D version).
    !       D       R.A.  O   Diagonal term of derivative (1-D version).
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
    !       None.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     Determine resonance condition and the maximum discrete freq.
    !     for which the interactions are calculated.
    !
    !     If Ursell number larger than prescribed value compute interactions
    !        Calculate biphase
    !        Do for each direction
    !           Convert action density to energy density
    !           Do for all frequencies
    !             Calculate interaction coefficient and interaction factor
    !             Compute interactions and store results in matrix
    !
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS, ONLY: GRAV, PI, TPI
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, DTH, SIG, DDEN, FTE, FTF
    USE W3ODATMD, ONLY: NDSE
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)        :: CG(NK), WN(NK), DEPTH, A(NSPEC), AOLD(NSPEC) 
    INTEGER, INTENT(IN)     :: IX
    REAL, INTENT(OUT)       :: S(NSPEC), D(NSPEC)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    !     AUX1  :     auxiliary real
    !     AUX2  :     auxiliary real
    !     BIPH  :     parameterized biphase of the spectrum
    !     C0    :     phase velocity at central bin
    !     CM    :     phase velocity at interacting bin
    !     DEP   :     water depth
    !     DEP_2 :     water depth to power 2
    !     DEP_3 :     water depth to power 3
    !     E     :     energy density as function of frequency
    !     E0    :     energy density at central bin
    !     EM    :     energy density at interacting bin
    !     HS    :     significant wave height
    !     FT    :     auxiliary real indicating multiplication factor
    !                 for triad contribution
    !     I1    :     auxiliary integer
    !     I2    :     auxiliary integer
    !     ID    :     counter
    !     IDDUM :     loop counter in direction space
    !     IENT  :     number of entries
    !     II    :     loop counter
    !     IS    :     loop counter in frequency space
    !     ISM   :     negative range for IS
    !     ISM1  :     negative range for IS
    !     ISMAX :     maximum of the counter in frequency space for
    !                 which the triad interactions are calculated (cut-off)
    !     ISP   :     positive range for IS
    !     ISP1  :     positive range for IS
    !     RINT  :     interaction coefficient
    !     SA    :     interaction contribution of triad
    !     SIGPICG :   sigma times 2pi/Cg (a Jacobian for E(f) -> E(k))
    !     SINBPH:     absolute sine of biphase
    !     STRI  :     total triad contribution
    !     WISM  :     interpolation weight factor corresponding to lower harmonic
    !     WISM1 :     interpolation weight factor corresponding to lower harmonic
    !     WISP  :     interpolation weight factor corresponding to higher harmonic
    !     WISP1 :     interpolation weight factor corresponding to higher harmonic
    !     W0    :     radian frequency of central bin
    !     WM    :     radian frequency of interacting bin
    !     WN0   :     wave number at central bin
    !     WNM   :     wave number at interacting bin
    !     XIS   :     rate between two succeeding frequency counters
    !     XISLN :     log of XIS
    !
#ifdef W3_S
    INTEGER, SAVE :: IENT = 0
#endif
    INTEGER :: I1, I2, ID, IDDUM, II, IS, ISM, ISM1, ISMAX
    INTEGER :: ISP, ISP1, ITH, IK
    REAL    :: AUX1, AUX2, BIPH, C0, CM, DEP, DEP_2, DEP_3, E0, EM, HS
    REAL    :: FT, RINT, SIGPICG, SINBPH, STRI, WISM, WISM1, WISP
    REAL    :: WISP1, W0, WM, WN0, WNM, XIS, XISLN, EDM, ED0, G9DEP, STRI2
    REAL    :: E(NK), SA(NTH,100), SA2(NTH,100), A2(NSPEC), A3(NSPEC), HMAX
    REAL    :: EB(NK), EBAND, EMEAN, SIGM01, ED(NK)
!----- Temp (to be moved) -----
    REAL    :: EF(NK), JACEPS, DIFFSTR
    REAL    :: PTRIAD(5)
    REAL    :: URSELL, ALPHAR
!------------------------------
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
    CALL STRACE (IENT, 'W3STR1')
#endif

!AR: todo: check all PRX routines for differences, check original thesis of elderberky. 
!
! 1.  Integral over directions
!
    SIGM01 = 0.
    EMEAN  = 0.
    JACEPS = 1E-12

    HMAX   = DEPTH * 0.73

    DO IK=1, NK
      EB(IK) = 0.
      ED(IK) = 0.
      DO ITH=1, NTH
        EB(IK) = EB(IK) + A(ITH+(IK-1)*NTH)
        ED(IK) = ED(IK) + A(ITH+(IK-1)*NTH) * DDEN(IK) / CG(IK)
      END DO
    END DO
!
! 2.  Integrate over frequencies. 
!
    DO IK=1, NK
      EB(IK) = EB(IK) * DDEN(IK) / CG(IK)
      EMEAN  = EMEAN  + EB(IK)
      SIGM01  = SIGM01  + EB(IK)*SIG(IK)
    END DO
!
! 3.  Add tail beyond discrete spectrum
!     ( DTH * SIG(NK) absorbed in FTxx )
!
    EBAND  = EB(NK) / DDEN(NK)
    EMEAN  = EMEAN  + EBAND * FTE
    SIGM01 = SIGM01 + EBAND * FTF
!
! 4.  Final processing
!
    SIGM01 = SIGM01 / EMEAN

!---- Temporary parameters (to be replaced by namelists) -----

    PTRIAD(1) = 1. 
    PTRIAD(2) = 10.
    PTRIAD(3) = 10. ! not used 
    PTRIAD(4) = 0.2
    PTRIAD(5) = 0.01

    HS = 4.*SQRT( MAX(0.,EMEAN) )
    URSELL = (GRAV*HS)/(2.*SQRT(2.)*SIGM01**2*DEPTH**2)
!---------------------------------------------

    DEP   = DEPTH
    DEP_2 = DEP**2
    DEP_3 = DEP**3
    G9DEP = GRAV * DEP
!
!     --- compute some indices in sigma space
!
    I2     = INT (FLOAT(NK) / 2.)
    I1     = I2 - 1
    XIS    = SIG(I2) / SIG(I1)
    XISLN  = LOG( XIS )

    ISP    = INT( LOG(2.) / XISLN )
    ISP1   = ISP + 1
    WISP   = (2. - XIS**ISP) / (XIS**ISP1 - XIS**ISP)
    WISP1  = 1. - WISP

    ISM    = INT( LOG(0.5) / XISLN )
    ISM1   = ISM - 1
    WISM   = (XIS**ISM -0.5) / (XIS**ISM - XIS**ISM1)
    WISM1  = 1. - WISM

    E  = 0.
    SA = 0.
!
!     --- compute maximum frequency for which interactions are calculated
!
    ISMAX = 1
    DO IK = 1, NK
     IF ( SIG(IK) .LT. ( PTRIAD(2) * SIGM01) ) THEN
        ISMAX = IK
     ENDIF
    ENDDO
    ISMAX = MAX ( ISMAX , ISP1 )
!
!     --- compute 3-wave interactions
!
      IF (URSELL.GE.PTRIAD(5) ) THEN ! AR: No need for switching it off from my point of view!
!
!       --- calculate biphase
!
        BIPH   = (0.5*PI)*(TANH(PTRIAD(4)/URSELL)-1.)
        SINBPH = ABS(SIN(BIPH) )
        EF     = 0.

        DO ITH = 1, NTH
          DO IK = 1, NK
            E(IK)  = A(ITH+(IK-1)*NTH) * TPI * SIG(IK) / CG(IK)
            EF(IK) = EF(IK) + E(IK)        
          END DO
          DO IK = 1, ISMAX
            E0  = E(IK)
            ED0 = EB(IK) 
            W0  = SIG(IK)
            WN0 = WN(IK)
            C0  = W0 / WN0
            IF ( IK.GT.-ISM1 ) THEN
               EM  = WISM * E(IK+ISM1)   + WISM1 * E(IK+ISM)
               EDM = WISM * EB(IK+ISM1)  + WISM1 * EB(IK+ISM)
               WM  = WISM * SIG(IK+ISM1) + WISM1 * SIG(IK+ISM)
               WNM = WISM * WN(IK+ISM1)  + WISM1 * WN(IK+ISM)
               CM  = WM / WNM
            ELSE
               EM  = 0.
               EDM = 0.
               WM  = 0.
               WNM = 0.
               CM  = 0.
            END IF
            AUX1 = WNM**2 * ( G9DEP + 2*CM**2 ) 
            AUX2 = WN0*DEP* (G9DEP+(2./15.)*GRAV*DEP_3*WN0**2-(2./5.)*W0**2*DEP_2)
            RINT = AUX1 / AUX2
            FT   = PTRIAD(1) * C0 * CG(IK) * RINT**2 * SINBPH 
            SA(ITH,IK) = MAX(0.,FT * ( EM * EM - 2. * EM * E0)) ! 1/(m²*s²) * m4 = m²/s² !!! [m²/s]
          END DO
        END DO

        DO IK = 1, NK - 1 
          SIGPICG = SIG(IK)*TPI/CG(IK) ! 1/s * s/m = 1/m
          DO ITH = 1, NTH
            STRI = SA(ITH,IK) - 2 * (WISP *  SA(ITH,IK+ISP1) + WISP1 *  SA(ITH,IK+ISP))
            IF (A(ITH+(IK-1)*NTH) .gt. JACEPS) THEN
              D(ITH+(IK-1)*NTH) = STRI / ((A(ITH+(IK-1)*NTH)) * SIGPICG) 
              S(ITH+(IK-1)*NTH) = STRI / SIGPICG 
            ELSE
              D(ITH+(IK-1)*NTH) = 0.
              S(ITH+(IK-1)*NTH) = 0.
            ENDIF
          END DO
        END DO
      ELSE
        D = 0.
        S = 0.
      END IF

    !/
    !/ End of W3STR1 ----------------------------------------------------- /
    !/
  END SUBROUTINE W3STR1
  !/ ------------------------------------------------------------------- /
END MODULE W3STR1MD
