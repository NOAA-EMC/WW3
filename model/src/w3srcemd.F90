!> @file
!> @brief Source term integration routine.
!>
!> @author H. L. Tolman
!> @author F. Ardhuin
!> @date   22-Mar-2021
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /

!>
!> @brief Source term integration routine.
!>
!> @author H. L. Tolman
!> @author F. Ardhuin
!> @date   22-Mar-2021
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>
MODULE W3SRCEMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |            F. Ardhuin             |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         03-Nov-2023 |
  !/                  +-----------------------------------+
  !/
  !/    For updates see subroutine.
  !/
  !  1. Purpose :
  !
  !     Source term integration routine.
  !
  !  2. Variables and types :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      OFFSET    R.P.  Private  Offset in time integration scheme.
  !                               0.5 in original WAM, now 1.0
  !     ----------------------------------------------------------------
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      W3SRCE    Subr. Public   Calculate and integrate source terms.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !     See corresponding documentation of W3SRCE.
  !
  !  5. Remarks :
  !
  !  6. Switches :
  !
  !       See section 9 of W3SRCE.
  !
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  REAL, PARAMETER, PRIVATE:: OFFSET = 1.

  ! GPU Refactor - user settable tile size for controlling chunking of 
  ! seapoint loop in W3SRCE. To minimize memory usage and reduce cache misses
  ! when running solely on the CPU (including MPI), set to 1.
  ! For running on a GPU, set to a number large enough to populate all your
  ! available GPU threads (e.g. on a nVidia V100 this would be 5000 - 10000).
  ! This can be set via the runtime environment variable WW3_SRC_TILE_SIZE
  ! and defaults to 1.
  INTEGER :: CHUNKSIZE = 1
  !/
CONTAINS

  !>
  !> @brief Initialise some variables at startup
  !>
  !> At the moment, just reads WW3_SRC_TILE_SIZE environment variable
  !> if it exists to control the tile sized used for the source term
  !> module. Defaults to tile size = 1.
  !>
  !> @author C. Bunney
  !> @date   12-Oct-2023
  SUBROUTINE W3SRCE_INIT()
    USE W3ODATMD, ONLY: NDSE, NDSO

    IMPLICIT NONE
    CHARACTER(LEN=16) :: VAL
    INTEGER :: STAT
    CALL get_environment_variable("WW3_SRC_TILE_SIZE", VALUE=VAL, STATUS=STAT)
    IF(STAT .EQ. 0) THEN
      READ(VAL,*,IOSTAT=STAT) CHUNKSIZE
      IF(STAT .NE. 0) THEN
        WRITE(NDSE,*) "Error ",STAT, " parsing value for WW3_SRC_TILE_SIZE: ", TRIM(VAL)
        WRITE(NDSE,*) "Will default to size of 1"
      ELSE
        WRITE(NDSO,*) "Source term tile size set to: ", CHUNKSIZE 
      ENDIF
    ENDIF
  END SUBROUTINE W3SRCE_INIT

  !/ ------------------------------------------------------------------- /

  !>
  !> @brief Calculate and integrate source terms for a single grid point.
  !>
  !> @verbatim
  !>     Physics  : see manual and corresponding subroutines.
  !>
  !>     Numerics :
  !>
  !>     Dynamic-implicit integration of the source terms based on
  !>     WW-II (Tolman 1992). The dynamic time step is calculated
  !>     given a maximum allowed change of spectral densities for
  !>     frequencies / wavenumbers below the usual cut-off.
  !>     The maximum change is given by the minimum of a parametric
  !>     and a relative change. The parametric change relates to a
  !>     PM type equilibrium range
  !>
  !>                                -1  (2pi)**4       1
  !>       dN(k)     =  Xp alpha  pi   ---------- ------------
  !>            max                       g**2     k**3 sigma
  !>
  !>                              1                                     .
  !>                 =  FACP ------------                              (1)
  !>                          k**3 sigma                                .
  !>
  !>     where
  !>           alpha = 0.62e-4                       (set in W3GRID)
  !>           Xp      fraction of PM shape          (read in W3GRID)
  !>           FACP    combined factor               (set in W3GRID)
  !>
  !>     The maximum relative change is given as
  !>
  !>                           /            +-                  -+ \    .
  !>       dN(k)     =  Xr max | N(k) , max | Nx , Xfilt N(k)    | |   (2)
  !>            max            \            +-               max-+ /    .
  !>
  !>     where
  !>           Xr      fraction of relative change   (read in W3GRID)
  !>           Xfilt   filter level                  (read in W3GRID)
  !>           Nx      Maximum parametric change (1)
  !>                   for largest wavenumber.
  !> @endverbatim
  !>
  !> @param[in]    srce_call
  !> @param[in]    IT
  !> @param[in]    IMOD       Model number.
  !> @param[in]    SPECOLD     (REMOVED)
  !> @param[inout] SPEC       Spectrum (action) in 1-D form.
  !> @param[out]   VSIO (Optional)
  !> @param[out]   VDIO (Optional)
  !> @param[out]   SHAVEIO (Optional)
  !> @param[inout] ALPHA      Nondimensional 1-D spectrum corresponding
  !>                          to above full spectra (Phillip's const.).
  !> @param[inout] WN1        Discrete wavenumbers.
  !> @param[inout] CG1        Id. group velocities.
  !> @param[in]    CLATSL
  !> @param[in]    D_INP      Depth, compared to DMIN to get DEPTH.
  !> @param[in]    U10ABS     Wind speed at reference height.
  !> @param[in]    U10DIR     Id. wind direction.
  !> @param[inout] TAUA       Magnitude of total atmospheric stress.
  !> @param[inout] TAUADIR    Direction of atmospheric stress.
  !> @param[in]    AS         Air-sea temperature difference.
  !> @param[inout] USTAR      Friction velocity.
  !> @param[inout] USTDIR     Idem, direction.
  !> @param[in]    CX         Current velocity component.
  !> @param[in]    CY         Current velocity component.
  !> @param[in]    ICE        Sea ice concentration.
  !> @param[in]    ICEH       Sea ice thickness.
  !> @param[inout] ICEF       Sea ice floe diameter.
  !> @param[in]    ICEDMAX    Sea ice maximum floe diameter
  !> @param[in]    REFLEC     Reflection coefficients.
  !> @param[in]    REFLED     Reflection direction.
  !> @param[in]    TRNX       Grid transparency in X.
  !> @param[in]    TRNY       Grid transparency in Y.
  !> @param[in]    BERG       Iceberg damping coefficient.
  !> @param[inout] FPI        Peak-input frequency.
  !> @param[out]   DTDYN      Average dynamic time step.
  !> @param[out]   FCUT       Cut-off frequency for tail.
  !> @param[in]    DTG        Global time step.
  !> @param[inout] TAUWX(JSEA)
  !> @param[inout] TAUWY(JSEA)
  !> @param[inout] TAUOX
  !> @param[inout] TAUWIX
  !> @param[inout] TAUWIY
  !> @param[inout] TAUWNX
  !> @param[inout] TAUWNY
  !> @param[inout] PHIAW
  !> @param[inout] CHARN
  !> @param[inout] TWS
  !> @param[inout] PHIOC
  !> @param[inout] WCAP_COV   Whitecap coverage
  !> @param[inout] WCAP_THK   Whitecap foam thickness
  !> @param[inout] WCAP_BHS   Whitecap breaking sig wave height
  !> @param[inout] WCAP_MNT   Whitecap moment
  !> @param[in]    D50        Sand grain size.
  !> @param[in]    PSIC       Critical shields.
  !> @param[inout] BEDROUGH   Bedform roughness
  !> @param[inout] BEDRIPX    Bedform ripple wavelength (x)
  !> @param[inout] BEDRIPY    Bedform ripple wavelength (y)
  !> @param[inout] PHIBBL     Energy flux to BBL.
  !> @param[inout] TAUBBLX    X-momentum flux to BBL.
  !> @param[inout] TAUBBLY    Y-momentum flux to BBL.
  !> @param[inout] TAUICEX    X-momentum flux to sea ice.
  !> @param[inout] TAUICEY    Y-momentum flux to sea ice.
  !> @param[inout] PHICE      Energy flux to sea ice.
  !> @param[inout] TAUOCX     Total ocean momentum component.
  !> @param[inout] TAUOCY     Total ocean momentum component.
  !> @param[inout] WNMEAN     Mean wave number.
  !> @param[in]    DAIR       Air density.
  !> @param[in]    COEF
  !>
  !> @author H. L. Tolman
  !> @author F. Ardhuin
  !> @author A. Roland
  !> @author M. Dutour Sikiric
  !> @date   22-Mar-2021
  !>
  SUBROUTINE W3SRCE ( srce_call, IT, IMOD,         &
!!     SPECOLD, SPEC, VSIO, VDIO, SHAVEIO,         &  ! SPECOLD not used (removed) VSIO,VDIO,SHAVIO made optional
       SPEC,                                       &
       ALPHA, WN1, CG1, CLATSL,                    &
       D_INP, U10ABS, U10DIR,                      &
#ifdef W3_FLX5
       TAUA, TAUADIR,                              &
#endif
       AS, USTAR, USTDIR,                          &
       CX, CY, ICE, ICEH, ICEF, ICEDMAX,          &
#ifdef W3_REF1
       REFLEC, REFLED, TRNX, TRNY, BERG,           &
#endif
       FPI, DTDYN, FCUT, DTG, TAUWX,               &  
       TAUWY, TAUOX, TAUOY, TAUWIX, TAUWIY, TAUWNX,&
       TAUWNY, PHIAW, CHARN, TWS, PHIOC,           &
       WCAP_COV, WCAP_THK, WCAP_BHS, WCAP_MNT,     &
#ifdef W3_BT4       
       D50, PSIC, BEDROUGH, BEDRIPX, BEDRIPY,      &  
#endif       
       PHIBBL, TAUBBLX, TAUBBLY,                   &
       TAUICEX, TAUICEY,                           &
       PHICE, TAUOCX, TAUOCY, WNMEAN, DAIR, COEF,  &
       VSIO, VDIO, SHAVEIO) ! These now optionals
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |            F. Ardhuin             |
    !/                  |            A. Roland              |
    !/                  |            M. Dutour Sikiric      |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    06-Dec-1996 : Final FORTRAN 77                    ( version 1.18 )
    !/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/    14-Feb-2000 : Exact-NL added                      ( version 2.01 )
    !/    04-May-2000 : Non-central integration             ( version 2.03 )
    !/    02-Feb-2001 : Xnl version 3.0                     ( version 2.07 )
    !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
    !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
    !/    27-Nov-2002 : First version of VDIA and MDIA.     ( version 3.01 )
    !/    07-Oct-2003 : Output options for NN training.     ( version 3.05 )
    !/    24-Dec-2004 : Multiple model version.             ( version 3.06 )
    !/    23-Jun-2006 : Linear input added.                 ( version 3.09 )
    !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
    !/    04-Jul-2006 : Separation of stress computation.   ( version 3.09 )
    !/    16-Apr-2007 : Miche style limiter added.          ( version 3.11 )
    !/                  (J. H. Alves)
    !/    25-Apr-2007 : Battjes-Janssen Sdb added.          ( version 3.11 )
    !/                  (J. H. Alves)
    !/    09-Oct-2007 : Adding WAM 4+ and SB1 options.      ( version 3.13 )
    !/                  (F. Ardhuin)
    !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
    !/    19-Aug-2010 : Making treatment of 0 water depth   ( version 3.14.6 )
    !/                  consistent with the rest of the model.
    !/    31-Mar-2010 : Adding ice conc. and reflections    ( version 3.14.4 )
    !/    15-May-2010 : Adding transparencies               ( version 3.14.4 )
    !/    01-Jun-2011 : Movable bed bottom friction in BT4  ( version 4.01 )
    !/    01-Jul-2011 : Energy and momentum flux, friction  ( version 4.01 )
    !/    24-Aug-2011 : Uses true depth for depth-induced   ( version 4.04 )
    !/    16-Sep-2011 : Initialization of TAUWAX, TAUWAY    ( version 4.04 )
    !/     1-Dec-2011 : Adding BYDRZ source term package    ( version 4.04 )
    !/                  ST6 and optional Hwang (2011)
    !/                  stresses FLX4.
    !/    14-Mar-2012 : Update of BT4, passing PSIC         ( version 4.04 )
    !/    13-Jul-2012 : Move GMD (SNL3) and nonlinear filter (SNLS)
    !/                  from 3.15 (HLT).                    ( version 4.08 )
    !/    28-Aug-2013 : Corrected MLIM application          ( version 4.11 )
    !/    10-Sep-2013 : Special treatment for IG band       ( version 4.15 )
    !/    14-Nov-2013 : Make orphaned pars in par lst local ( version 4.13 )
    !/    17-Nov-2013 : Coupling fraction of ice-free       ( version 4.13 )
    !/                  surface to SIN and SDS. (S. Zieger)
    !/    01-Avr-2014 : Adding ice thickness and floe size  ( version 4.18 )
    !/    23-May-2014 : Adding ice fluxes to W3SRCE         ( version 5.01 )
    !/    27-Aug-2015 : Adding inputs to function W3SIS2    ( version 5.10 )
    !/    13-Dec-2015 : Implicit integration of Sice (F.A.) ( version 5.10 )
    !/    30-Jul-2017 : Adds TWS in interface               ( version 6.04 )
    !/    07-Jan-2018 : Allows variable ice scaling (F.A.)  ( version 6.04 )
    !/    01-Jan-2018 : Add implicit source term integration ( version 6.04)
    !/    01-Jan-2018 : within PDLIB (A. Roland, M. Dutour
    !/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version  6.06)
    !/    26-Aug-2018 : UOST (Mentaschi et al. 2015, 2018)  ( version 6.06 )
    !/    22-Mar-2021 : Add extra fields used in coupling   ( version 7.13 )
    !/    07-Jun-2021 : S_{nl5} GKE NL5 (Q. Liu)            ( version 7.13 )
    !/    19-Jul-2021 : Momentum and air density support    ( version 7.14 )
    !/    10-Oct-2023 : Major refactor - W3SRCE now processes
    !/                  all seapoints rather than single
    !/                  seapoint. C. Bunney; UKMO           ( version 7.14 )
    !/    03-Nov-2023 : Split WHITECAP into 4 separate      ( version 7.14 )
    !/                  variables and TAUBBL/TAUICE into
    !/                  X and Y components. C Bunney
    !/
    !/    Copyright 2009-2013 National Weather Service (NWS),
    !/       National Oceanic and Atmospheric Administration.  All rights
    !/       reserved.  WAVEWATCH III is a trademark of the NWS.
    !/       No unauthorized use without permission.
    !/
    !  1. Purpose :
    !
    !     Calculate and integrate source terms for all grid points.
    !
    !  2. Method :
    !
    !     Physics  : see manual and corresponding subroutines.
    !
    !     Numerics :
    !
    !     Dynamic-implicit integration of the source terms based on
    !     WW-II (Tolman 1992). The dynamic time step is calculated
    !     given a maximum allowed change of spectral densities for
    !     frequencies / wavenumbers below the usual cut-off.
    !     The maximum change is given by the minimum of a parametric
    !     and a relative change. The parametric change relates to a
    !     PM type equilibrium range
    !
    !                                -1  (2pi)**4       1
    !       dN(k)     =  Xp alpha  pi   ---------- ------------
    !            max                       g**2     k**3 sigma
    !
    !                              1                                     .
    !                 =  FACP ------------                              (1)
    !                          k**3 sigma                                .
    !
    !     where
    !           alpha = 0.62e-4                       (set in W3GRID)
    !           Xp      fraction of PM shape          (read in W3GRID)
    !           FACP    combined factor               (set in W3GRID)
    !
    !     The maximum relative change is given as
    !
    !                           /            +-                  -+ \    .
    !       dN(k)     =  Xr max | N(k) , max | Nx , Xfilt N(k)    | |   (2)
    !            max            \            +-               max-+ /    .
    !
    !     where
    !           Xr      fraction of relative change   (read in W3GRID)
    !           Xfilt   filter level                  (read in W3GRID)
    !           Nx      Maximum parametric change (1)
    !                   for largest wavenumber.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IX,IY   Int.   I   Discrete grid point counters.
    !       IMOD    Int.   I   Model number.
    !       SPEC    R.A.  I/O  Spectrum (action) in 1-D form.
    !       ALPHA   R.A.  I/O  Nondimenional 1-D spectrum corresponding
    !                          to above full spectra (Phillip's const.).
    !                          Calculated separately for numerical
    !                          economy on vector machine (W3SPR2).
    !       WN1     R.A.   I   Discrete wavenumbers.
    !       CG1     R.A.   I   Id. group velocities.
    !       D_INP   Real.  I   Depth. Compared to DMIN to get DEPTH.
    !       U10ABS  Real.  I   Wind speed at reference height.
    !       U10DIR  Real.  I   Id. wind direction.
    !       TAUA    Real.  I   Magnitude of total atmospheric stress ( !/FLX5 )
    !       TAUADIR Real.  I   Direction of atmospheric stress       ( !/FLX5 )
    !       AS      Real.  I   Air-sea temp. difference.      ( !/ST3 )
    !       USTAR   Real. !/O  Friction velocity.
    !       USTDIR  Real  !/O  Idem, direction.
    !       CX-Y    Real.  I   Current velocity components.   ( !/BS1 )
    !       ICE     Real   I   Sea ice concentration
    !       ICEH    Real   I   Sea ice thickness
    !       ICEF    Real  I/O  Sea ice maximum floe diameter  (updated)
    !       ICEDMAX Real  I/O  Sea ice maximum floe diameter
    !       BERG    Real   I   Iceberg damping coefficient    ( !/BS1 )
    !       REFLEC  R.A.   I   reflection coefficients        ( !/BS1 )
    !       REFLED  I.A.   I   reflection direction           ( !/BS1 )
    !       TRNX-Y  Real   I   Grid transparency in X and Y   ( !/BS1 )
    !       FPI     Real  I/O  Peak-input frequency.          ( !/ST2 )
    !      WCAP_COV R.A.  I/O  Whitecap coverage              ( !/ST4 )
    !      WCAP_THK R.A.  I/O  Whitecap foam thickness        ( !/ST4 )
    !      WCAP_BHS R.A.  I/O  Whitecap breaking sig. wv. ht. ( !/ST4 )
    !      WCAP_MNT R.A.  I/O  Whitecap moment
    !       DTDYN   Real   O   Average dynamic time step.
    !       FCUT    Real   O   Cut-off frequency for tail.
    !       DTG     Real   I   Global time step.
    !       D50     Real   I   Sand grain size                ( !/BT4 )
    !      BEDROUGH R.A.  I/O  Bedform roughness              ( !/BT4 )
    !       BEDRIPX R.A.  I/O  Bedform ripple wavelength (x)  ( !/BT4 )
    !       BEDRIPY R.A.  I/O  Bedform ripple wavelength (y)  ( !/BT4 )       
    !       PSIC    Real   I   Critical Shields               ( !/BT4 )
    !       PHIBBL  Real   O   Energy flux to BBL             ( !/BTx )
    !     TAUBBL[XY]R.A.   O   Momentum flux to BBL           ( !/BTx )
    !     TAUICE[XY]R.A.  I/O  Momentum flux to sea ice       ( !/ICx )
    !       PHICE   Real  I/O  Energy flux to sea ice         ( !/ICx )
    !      TAUOC[XY]Real   O   Total ocean momentum components
    !       WNMEAN  Real   O   Mean wave number
    !       DAIR    Real   I   Air density
    !     ----------------------------------------------------------------
    !       Note: several pars are set to I/O to avoid compiler warnings.
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3SPRn    Subr. W3SRCnMD Mean wave parameters for use in
    !                               source terms.
    !      W3FLXn    Subr. W3FLXnMD Flux/stress computation.
    !      W3SLNn    Subr. W3SLNnMD Linear input.
    !      W3SINn    Subr. W3SRCnMD Input source term.
    !      W3SNLn    Subr. W3SNLnMD Nonlinear interactions.
    !      W3SNLS    Subr. W3SNLSMD Nonlinear smoother.
    !      W3SDSn    Subr. W3SRCnMD Whitecapping source term
    !      W3SBTn    Subr. W3SBTnMD Bottom friction source term.
    !      W3SDBn    Subr. W3SBTnMD Depth induced breaking source term.
    !      W3STRn    Subr. W3STRnMD Triad interaction source term.
    !      W3SBSn    Subr. W3SBSnMD Bottom scattering source term.
    !      W3REFn    Subr. W3REFnMD Reflexions (shore, icebergs ...).
    !      STRACE    Subr. W3SERVMD Subroutine tracing (!/S)
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !       None.
    !
    !  7. Remarks :
    !
    !     - No testing is performed on the status of the grid point.
    !
    !  8. Structure :
    !
    !     -----------------------------------------------------------------
    !       1.   Preparations
    !         a  Set maximum change and wavenumber arrays.
    !         b  Prepare dynamic time stepping.
    !         c  Compute mean parameters.                       ( W3SPRn )
    !         d  Compute stresses (if posible).
    !         e  Prepare cut-off
    !         f  Test output for !/NNT option.
    !     --start-dynamic-integration-loop---------------------------------
    !       2.  Calculate source terms
    !         a Input.                                  ( W3SLNx, W3SINn )
    !         b Nonlinear interactions.                         ( W3SNLn )
    !         c Dissipation                                     ( W3SDSn )
    !           1 as included in source terms                   ( W3SDSn )
    !           2 optional dissipation due to different physics ( W3SWLn )
    !         d Bottom friction.                                ( W3SBTn )
    !       3.  Calculate cut-off frequencie(s)
    !       4.  Summation of source terms and diagonal term and time step.
    !       5.  Increment spectrum.
    !       6.  Add tail
    !         a Mean wave parameters and cut-off                ( W3SPRn )
    !         b 'Seeding' of spectrum.                          ( !/SEED )
    !         c Add tail
    !       7.  Check if integration complete.
    !     --end-dynamic-integration-loop-----------------------------------
    !       8.  Save integration data.
    !     -----------------------------------------------------------------
    !
    !  9. Switches :
    !
    !     !/FLX1  Wu (1980) stress computation.              ( Choose one )
    !     !/FLX2  T&C (1996) stress computation.
    !     !/FLX3  T&C (1996) stress computation with cap.
    !     !/FLX4  Hwang (2011) stress computation (2nd order).
    !     !/FLX5  Direct use of stress from atmoshperic model.
    !
    !     !/LN0   No linear input.                           ( Choose one )
    !
    !     !/ST0   No input and dissipation.                  ( Choose one )
    !     !/ST1   WAM-3 input and dissipation.
    !     !/ST2   Tolman and Chalikov (1996)  input and dissipation.
    !     !/ST3   WAM 4+ input and dissipation.
    !     !/ST4   Ardhuin et al. (2009, 2010)
    !     !/ST6   BYDB source terms after Babanin, Young, Donelan and Banner.
    !
    !     !/NL0   No nonlinear interactions.                 ( Choose one )
    !     !/NL1   Discrete interaction approximation.
    !     !/NL2   Exact nonlinear interactions.
    !     !/NL3   Generalized Multiple DIA.
    !     !/NL4   Two Scale Approximation
    !     !/NL5   Generalized Kinetic Equation.
    !     !/NLS   Nonlinear HF smoother.
    !
    !     !/BT0   No bottom friction.                        ( Choose one )
    !     !/BT1   JONSWAP bottom friction.
    !     !/BT4   Bottom friction using movable bed roughness
    !                  (Tolman 1994, Ardhuin & al. 2003)
    !     !/BT8   Muddy bed (Dalrymple & Liu).
    !     !/BT9   Muddy bed (Ng).
    !
    !     !/IC1   Dissipation via interaction with ice according to simple
    !             methods: 1) uniform in frequency or
    !     !/IC2            2) Liu et al. model
    !     !/IC3   Dissipation via interaction with ice according to a
    !             viscoelastic sea ice model (Wang and Shen 2010).
    !     !/IC4   Dissipation via interaction with ice as a function of freq.
    !             (empirical/parametric methods)
    !     !/IC5   Dissipation via interaction with ice according to a
    !             viscoelastic sea ice model (Mosig et al. 2015).
    !     !/DB0   No depth-limited breaking.                 ( Choose one )
    !     !/DB1   Battjes-Janssen depth-limited breaking.
    !
    !     !/TR0   No triad interactions.                     ( Choose one )
    !     !/TR1   Lumped Triad Approximation (LTA).
    !
    !     !/BS0   No bottom scattering.                      ( Choose one )
    !     !/BS1   Scattering term by Ardhuin and Magne (2007).
    !
    !     !/MLIM  Miche style limiter for shallow water and steepness.
    !
    !     !/SEED  'Seeding' of lowest frequency for suffuciently strong
    !             winds.
    !
    !     !/NNT   Write output to file test_data_NNN.ww3 for NN training.
    !
    !     !/S     Enable subroutine tracing.
    !     !/T     Enable general test output.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS, ONLY: DWAT, srce_imp_post, srce_imp_pre,         &
         srce_direct, GRAV, TPI, TPIINV
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, TH, DMIN, DTMAX,       &
         DTMIN, FACTI1, FACTI2, FACSD, FACHFA, FACP, &
         XFC, XFLT, XREL, XFT, FXFM, FXPM, DDEN,     &
         FTE, FTF, FHMAX, ECOS, ESIN, IICEDISP,      &
         ICESCALES, IICESMOOTH
    USE W3WDATMD, ONLY: TIME
    USE W3ODATMD, ONLY: NDSE, NDST, IAPROC
    USE W3IDATMD, ONLY: INFLAGS2
    USE W3DISPMD
#ifdef W3_T
    USE CONSTANTS, ONLY: RADE
#endif
#ifdef W3_REF1
    USE W3GDATMD, ONLY: IOBP, IOBPD, GTYPE, UNGTYPE, REFPARS
#endif
#ifdef W3_NNT
    USE W3ODATMD, ONLY: IAPROC, SCREEN, FNMPRE
#endif
#ifdef W3_FLD1
    USE W3FLD1MD, ONLY: W3FLD1
    USE W3GDATMD, ONLY: AALPHA
#endif
#ifdef W3_FLD2
    USE W3FLD2MD, ONLY: W3FLD2
    USE W3GDATMD, ONLY: AALPHA
#endif
#ifdef W3_FLX1
    USE W3FLX1MD
#endif
#ifdef W3_FLX2
    USE W3FLX2MD
#endif
#ifdef W3_FLX3
    USE W3FLX3MD
#endif
#ifdef W3_FLX4
    USE W3FLX4MD
#endif
#ifdef W3_FLX5
    USE W3FLX5MD
#endif
#ifdef W3_LN1
    USE W3SLN1MD
#endif
#ifdef W3_ST0
    USE W3SRC0MD
#endif
#ifdef W3_ST1
    USE W3SRC1MD
#endif
#ifdef W3_ST2
    USE W3SRC2MD
    USE W3GDATMD, ONLY : ZWIND
#endif
#ifdef W3_ST3
    USE W3SRC3MD
    USE W3GDATMD, ONLY : ZZWND, FFXFM, FFXPM
#endif
#ifdef W3_ST4
    USE W3SRC4MD, ONLY : W3SPR4, W3SIN4, W3SDS4
    USE W3GDATMD, ONLY : ZZWND, FFXFM, FFXPM, FFXFA, SINTAILPAR
#endif
#ifdef W3_ST6
    USE W3SRC6MD
    USE W3SWLDMD, ONLY : W3SWL6
    USE W3GDATMD, ONLY : SWL6S6
#endif
#ifdef W3_NL1
    USE W3SNL1MD
    USE W3GDATMD, ONLY: IQTPE
#endif
#ifdef W3_NL2
    USE W3SNL2MD
#endif
#ifdef W3_NL3
    USE W3SNL3MD
#endif
#ifdef W3_NL4
    USE W3SNL4MD
#endif
#ifdef W3_NL5
    USE W3SNL5MD
    USE W3TIMEMD, ONLY: TICK21
#endif
#ifdef W3_NLS
    USE W3SNLSMD
#endif
#ifdef W3_BT1
    USE W3SBT1MD
#endif
#ifdef W3_BT4
    USE W3SBT4MD
#endif
#ifdef W3_BT8
    USE W3SBT8MD
#endif
#ifdef W3_BT9
    USE W3SBT9MD
#endif
#ifdef W3_IC1
    USE W3SIC1MD
#endif
#ifdef W3_IC2
    USE W3SIC2MD
#endif
#ifdef W3_IC3
    USE W3SIC3MD
#endif
#ifdef W3_IC4
    USE W3SIC4MD
#endif
#ifdef W3_IC5
    USE W3SIC5MD
#endif
#ifdef W3_IS1
    USE W3SIS1MD
#endif
#ifdef W3_IS2
    USE W3SIS2MD
    USE W3GDATMD, ONLY : IS2PARS
#endif
#ifdef W3_DB1
    USE W3SDB1MD
#endif
#ifdef W3_TR1
    USE W3STR1MD
#endif
#ifdef W3_BS1
    USE W3SBS1MD
#endif
#ifdef W3_REF1
    USE W3REF1MD
#endif
#ifdef W3_IG1
    USE W3GDATMD, ONLY : IGPARS
#endif
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
#ifdef W3_NNT
    USE W3SERVMD, ONLY: EXTCDE
#endif
#ifdef W3_UOST
    USE W3UOSTMD, ONLY: UOST_SRCTRMCOMPUTE
#endif
#ifdef W3_PDLIB
    USE PDLIB_W3PROFSMD, ONLY : B_JAC, ASPAR_JAC, ASPAR_DIAG_ALL
    USE yowNodepool, ONLY: PDLIB_I_DIAG, PDLIB_SI, NP
    USE W3GDATMD, ONLY: B_JGS_LIMITER, FSSOURCE, optionCall
    USE W3GDATMD, ONLY: IOBP_LOC, IOBPD_LOC, B_JGS_LIMITER_FUNC
    USE W3WDATMD, ONLY: VA
    USE W3PARALL, ONLY: IMEM, LSLOC
#endif

    !GPU Refactor - extra imports
    USE W3GDATMD, ONLY: NSEA, NSEAL, MAPSTA, MAPSF, FLAGST, NX, NY
    USE W3PARALL, ONLY: INIT_GET_ISEA
    USE W3ADATMD, ONLY: NSEALM
    USE CONSTANTS, ONLY: UNDEF

    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !!REAL, intent(in)        :: SPECOLD(NSPEC)  ! Refactor: Not used.

    ! GPU Refactor: Scalar inputs
    INTEGER, INTENT(IN) ::     &
        srce_call,             &
        IT,                    &
        IMOD
    REAL, INTENT(IN) :: DTG

    ! GPU Refactor: Input arrays with dimension NSEA:
    REAL, INTENT(IN) ::        &
        D_INP(1:NSEA),         &
        CLATSL(1:NSEA),        &
        AS(1:NSEA),            &
        CX(1:NSEA),            &
        CY(1:NSEA),            &
        DAIR(1:NSEA),          &
        ICE(1:NSEA),           &
        ICEH(1:NSEA),          &
        COEF(1:NSEA),          &
        ICEDMAX(1:NSEA)

    REAL, INTENT(INOUT) ::     &
        WN1(0:NK+1,NSEA),      &  ! Note: 0:NK+1 to avoid temporary array
        CG1(0:NK+1,NSEA),      &  ! Note: 0:NK+1 to avoid temporary array
        U10ABS(1:NSEA),        &
        U10DIR(1:NSEA),        &
        USTAR(1:NSEA),         &
        USTDIR(1:NSEA),        &
        FPI(1:NSEA),           &
        ICEF(1:NSEA)

    ! GPU Refactor: Input arrays with dimension NSEAL(M):
    ! TODO: Slice all these to 1:NSEAL in calling routine?   
    REAL, INTENT(INOUT) ::     &
        SPEC(NSPEC,NSEALM),    &
        ALPHA(1:NK,1:NSEAL),   &
        WNMEAN(1:NSEAL),       &
        TAUWX(1:NSEAL),        &
        TAUWY(1:NSEAL),        &
        CHARN(1:NSEAL),        &
        PHIBBL(1:NSEAL),       &
        PHIAW(1:NSEAL),        &
        PHIOC(1:NSEAL),        &
        PHICE(1:NSEAL),        &
        TAUWIX(1:NSEAL),       &
        TAUWIY(1:NSEAL),       &
        TAUWNX(1:NSEAL),       &
        TAUWNY(1:NSEAL),       &
        TAUOX(1:NSEAL),        &
        TAUOY(1:NSEAL),        &
        TAUOCX(1:NSEAL),       &
        TAUOCY(1:NSEAL),       &
        TAUBBLX(1:NSEAL),      &
        TAUBBLY(1:NSEAL),      &
        TWS(1:NSEAL),          &
        TAUICEX(1:NSEAL),      &
        TAUICEY(1:NSEAL),      &
        WCAP_COV(1:NSEAL),     &    ! -|
        WCAP_THK(1:NSEAL),     &    !  | GPU Refactor: Split WHITECAPING into 4 separate
        WCAP_BHS(1:NSEAL),     &    !  | arrays (WCAP_*) to avoid temporaries when slicing
        WCAP_MNT(1:NSEAL)           ! -|
    
    REAL, INTENT(OUT) ::       &
        DTDYN(1:NSEAL),        &
        FCUT(1:NSEAL)
  
! GPU Refactor - inputs depending on compile switch:
#ifdef W3_REF1
    INTEGER, INTENT(IN) ::     &
        REFLED(6,1:NSEA)

    REAL, INTENT(IN) ::        &
        REFLEC(4,1:NSEA),      &
        BERG(1:NSEA)
    
    REAL, INTENT(IN) ::        &
        TRNX(NY,NX),           &
        TRNY(NY,NX)
#endif
#ifdef W3_FLX5
    REAL, INTENT(IN) ::        &
        TAUA(1:NSEA),          &
        TAUADIR(1:NSEA)
#endif
#ifdef W3_BT4
    REAL, INTENT(IN) ::        &
        D50(1:NSEA),           &
        PSIC(1:NSEA)
    REAL, INTENT(INOUT) ::     &
        BEDROUGH(1:NSEAL),     &
        BEDRIPX(1:NSEAL),      &
        BEDRIPY(1:NSEAL)
#endif

    ! GPU Refactor: optional inputs
    REAL, INTENT(OUT), OPTIONAL ::  &
        VSIO(NSPEC,NSEAL),     &
        VDIO(NSPEC,NSEAL)
    LOGICAL, INTENT(OUT), OPTIONAL :: SHAVEIO(NSEAL)

    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER :: IK, ITH, IS, IS0, NSTEPS,  &
         IKS1, IS1, NSPECH, IDT, IERR, ISP
    REAL :: AFILT, DAMAX, AFAC, &
         HDT, ZWND, TAUSCX, TAUSCY
    ! Scaling factor for SIN, SDS, SNL
    REAL :: ICESCALELN, ICESCALEIN, ICESCALENL, ICESCALEDS
    REAL :: WN_R(NK), CG_ICE(NK), ALPHA_LIU(NK), ICECOEF2, R(NK)
    DOUBLE PRECISION :: ATT, ISO
    REAL :: EBAND, DIFF, EFINISH, HSTOT, &
!         PHINL,       &    ! GPU Refactor: Computed but not actually used anywhere - removed
         FACTOR, FACTOR2, &
         MWXFINISH, MWYFINISH, A1BAND, B1BAND,     &
         COSI(2)
    REAL :: SPEC2(NSPEC), FRLOCAL, JAC2
    REAL :: DAM2(NSPEC)
    !REAL :: EB(NK)       ! GPU Refactor: removed - not used.
    LOGICAL :: SHAVE
    LOGICAL :: LBREAK     ! TODO - returned from W3SDB1 but never used. Make dummy?
    !LOGICAL, SAVE :: FIRST = .TRUE.  ! GPU Refactor: removed - not used.
    LOGICAL :: PrintDeltaSmDA
    REAL :: eInc1, eInc2, eVS, eVD, JAC
    REAL, SAVE :: TAUNUX, TAUNUY    ! TODO - returned from W3_FLD[12], but never used...
    LOGICAL, SAVE :: FLTEST = .FALSE., FLAGNN = .TRUE.
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters dependent on compile switch
    !/
#ifdef W3_PDLIB
    REAL :: DeltaSRC(NSPEC)
#endif

#ifdef W3_S
    INTEGER, SAVE :: IENT = 0
#endif

#ifdef W3_NNT
    INTEGER, SAVE :: NDSD = 89, NDSD2 = 88, J
    REAL :: QCERR  = 0.     !/XNL2 and !/NNT
    REAL :: FOUT(NK,NTH), SOUT(NK,NTH), DOUT(NK,NTH)
#endif

#ifdef W3_NL5
    REAL :: QR5KURT
    INTEGER, PARAMETER :: NL5_SELECT = 1
    REAL, PARAMETER :: NL5_OFFSET = 0.  ! explicit dyn.
#endif

#ifdef W3_SEED
    REAL :: UC, SLEV
#endif

#ifdef W3_MLIM
    REAL :: HM, EM
#endif

#ifdef W3_NNT
    REAL :: FACNN
#endif

#ifdef W3_T
    REAL :: DTRAW
#endif

#if defined(W3_IC1) || defined(W3_IC2) || defined(W3_IC3) || defined(W3_IC4) || defined(W3_IC5)
    REAL :: VSIC(NSPEC), VDIC(NSPEC)
#endif

#if defined(W3_IS1) || defined(W3_IS2)
    REAL :: VSIR(NSPEC), VDIR(NSPEC)
#endif

#ifdef W3_IS2
    REAL :: VDIR2(NSPEC)
    DOUBLE PRECISION :: SCATSPEC(NTH)
    REAL :: SCAT, SMOOTH_ICEDISP
#endif

#if defined (W3_ST1) || defined(W3_ST3) || defined(W3_ST4)
    REAL :: FH1, FH2
#endif

#ifdef W3_ST2
    REAL :: FHTRAN, DFH, FACDIA, FACPAR
#endif

#ifdef W3_PDLIB
    REAL :: PreVS, DVS, SIDT, FAKS, MAXDAC
#endif

#ifdef W3_NNT
    CHARACTER(LEN=17), SAVE :: FNAME = 'test_data_nnn.ww3'
#endif

    !/
    !/ ------------------------------------------------------------------- /
    !/ --- GPU REFACTOR --- !
    !/ New local parameters or existing variables that have had CHUNKSIZE
    !/ dimension added to allow for tiling.
    !/

    ! Exising locals with new CHUNKSIZE dimension
    REAL :: AMAX(CHUNKSIZE),            &
            EMEAN(CHUNKSIZE),           &
            FMEAN(CHUNKSIZE),           &
            FMEAN1(CHUNKSIZE),          &
            FMEANWS(CHUNKSIZE),         &
            CD(CHUNKSIZE),              &
            Z0(CHUNKSIZE),              &
            FHIGH(CHUNKSIZE),           &
            !!FHIGI(CHUNKSIZE) ! Not used &
            TAUWAX(CHUNKSIZE),          &
            TAUWAY(CHUNKSIZE),          &
            DEPTH(CHUNKSIZE),           &
            DT(CHUNKSIZE),              &
            DRAT(CHUNKSIZE)

    INTEGER :: & 
            NKH(CHUNKSIZE), NKH1(CHUNKSIZE)

    REAL :: DTTOT(CHUNKSIZE)
    REAL :: DAM (NSPEC,CHUNKSIZE),      &
            WN2 (NSPEC,CHUNKSIZE),      &
            SPECINIT(NSPEC,CHUNKSIZE)

    REAL :: VS(NSPEC, CHUNKSIZE), VD(NSPEC, CHUNKSIZE)

    REAL :: VSLN(NSPEC, CHUNKSIZE),   &
            VSIN(NSPEC, CHUNKSIZE), VDIN(NSPEC, CHUNKSIZE), &
            VSDS(NSPEC, CHUNKSIZE), VDDS(NSPEC, CHUNKSIZE), &
            VSNL(NSPEC, CHUNKSIZE), VDNL(NSPEC, CHUNKSIZE), &
            VSBT(NSPEC, CHUNKSIZE), VDBT(NSPEC, CHUNKSIZE)
#ifdef W3_TR1
    REAL :: VSTR(NSPEC, CHUNKSIZE), VDTR(NSPEC, CHUNKSIZE)
#endif
#ifdef W3_ST6
    REAL :: VSWL(NSPEC, CHUNKSIZE), VDWL(NSPEC, CHUNKSIZE)
#endif
#ifdef W3_DB1
    REAL :: VSDB(NSPEC, CHUNKSIZE), VDDB(NSPEC, CHUNKSIZE)
#endif
#ifdef W3_BS1
    REAL :: VSBS(NSPEC, CHUNKSIZE), VDBS(NSPEC, CHUNKSIZE)
#endif
#ifdef W3_UOST
    REAL :: VSUO(NSPEC, CHUNKSIZE), VDUO(NSPEC, CHUNKSIZE)
#endif
#ifdef W3_REF1
    REAL :: VREF(NSPEC, CHUNKSIZE)  ! TODO: Can we share this with other switches? I.e 
                                    ! VSTMP and VDTMP.
#endif
#if W3_ST3
      REAL :: FMEANS(CHUNKSIZE)
#endif
#if defined(W3_ST3) || defined(W3_ST4)
    LOGICAL :: LLWS(NSPEC,CHUNKSIZE)
#endif
#ifdef W3_ST4
    REAL :: DLWMEAN(CHUNKSIZE),         &
            FAGE(CHUNKSIZE)
    REAL :: BRLAMBDA(NSPEC, CHUNKSIZE)
#endif  
#if defined(W3_ST0) || defined(W3_ST1) || defined(W3_ST2) || \
    defined(W3_ST6) || defined(W3_FLX2) || defined(W3_FLX3)
    REAL :: FP(CHUNKSIZE)
#endif
#ifdef W3_NL5
    INTEGER :: QI5TSTART(2,CHUNKSIZE)
#endif

    ! New locals for chunking
    INTEGER :: CHUNK0, CHUNKN, NSEAC, I, ISEA, JSEA, CSEA
    INTEGER :: IX(CHUNKSIZE), IY(CHUNKSIZE)

    ! Refactor: New CHUNK sized arrays for storing contiguous data from
    ! full seapoint array (NSEA) on local seapoint array (NSEAL)
    ! TODO - HOIST THESE TO MODULE SCOPE (Heap allocation)?
    ! TODO - Not really needed for SHRD runs, where ISEA == JSEA...can we used pointers instead?
    REAL :: CG1_CHUNK(1:NK, CHUNKSIZE),  &
            WN1_CHUNK(1:NK, CHUNKSIZE),  &
            U10_CHUNK(CHUNKSIZE),        &
            U10D_CHUNK(CHUNKSIZE),       &
            UST_CHUNK(CHUNKSIZE),        &
            USTD_CHUNK(CHUNKSIZE),       &
            DAIR_CHUNK(CHUNKSIZE),       &
            ICE_CHUNK(CHUNKSIZE),        &
            ICEH_CHUNK(CHUNKSIZE),       &
            ICEF_CHUNK(CHUNKSIZE),       &
            ICEDMAX_CHUNK(CHUNKSIZE),    &
            COEF_CHUNK(CHUNKSIZE)

#if defined(W3_ST3) || defined(W3_ST4)
    REAL :: AS_CHUNK(CHUNKSIZE)
#endif
#if defined(W3_BS1) || defined(W3_REF1)
    REAL :: CX_CHUNK(CHUNKSIZE), CY_CHUNK(CHUNKSIZE)
#endif
#ifdef W3_REF1
    REAL :: TRNX_CHUNK(CHUNKSIZE),      &
            TRNY_CHUNK(CHUNKSIZE),      &
            REFLEC_CHUNK(4,CHUNKSIZE),  &
            BERG_CHUNK(CHUNKSIZE)
    INTEGER :: REFLED_CHUNK(6,CHUNKSIZE)
#endif
#ifdef W3_FLX5
    REAL :: TAUA_CHUNK(CHUNKSIZE), TAUADIR_CHUNK(CHUNKSIZE)
#endif
#if W3_BT4
    REAL :: D50_CHUNK(CHUNKSIZE),      &
            PSIC_CHUNK(CHUNKSIZE)
#endif
#ifdef W3_PDLIB
    REAL :: CLATSL_CHUNK(CHUNKSIZE)
#endif


    ! For masking points that have completed integration or are not seapoints.
    LOGICAL, ALLOCATABLE :: SRC_MASK(:)
    LOGICAL :: COMPLETE

    ! --- END GPU REFACTOR --- !

    !
    !/ -- End of variable delclarations
    !/ ------------------------------------------------------------------- /
    !/

#ifdef W3_S
    CALL STRACE (IENT, 'W3SRCE')
#endif

#ifdef W3_T
    FLTEST = .TRUE.
#endif

    ! GPU Refactor - allocate locals
    ALLOCATE(SRC_MASK(CHUNKSIZE))

    IKS1 = 1
#ifdef W3_IG1
    ! Does not integrate source terms for IG band if IGPARS(12) = 0.
    IF (NINT(IGPARS(12)).EQ.0) IKS1 = NINT(IGPARS(5))
#endif
    IS1=(IKS1-1)*NTH+1

    ! GPU refactor: initialise some (non chunked) fields:
    !               (Moved outside chunk loop)
    ! These are all variables dimensioned (NSEAL)
    IF(PRESENT(VDIO)) VDIO = 0.
    IF(PRESENT(VSIO)) VSIO = 0.
    DTDYN  = 0.
    PHIAW  = 0.
    TAUWIX = 0.
    TAUWIY = 0.
    TAUWNX = 0.
    TAUWNY = 0.
    TAUOCX = 0.
    TAUOCY = 0.
    TAUBBLX = 0.
    TAUBBLY = 0.
    PHIBBL = 0.
    !TAUICEX = 0.    ! GPU REFACTOR: Don't zero whole array here (for B4B purposes after refactor; zeroed later)
    !TAUICEY = 0.    ! GPU REFACTOR: Don't zero whole array here (for B4B purposes after refactor; zeroed later)
    !PHICE  = 0.    ! GPU REFACTOR: Don't zero whole array here (for B4B purposes after refactor; zeroed later)
    WNMEAN = 0.
    !CHARN  = 0.    ! GPU REFACTOR: Don't zero whole array here (for B4B purposes after refactor; zeroed later)
    TWS    = 0.

    ! Refactor notes: Zero ice chunks if ICE field never read in.
    ! INFLAGS2(4) is true if ice concentration read in for this simulation
    IF(.NOT. INFLAGS2(4)) THEN
      ICE_CHUNK(:) = 0.0 ! Ice never read in
    END IF

#ifdef W3_ST4
    WCAP_COV(:) = 0.
    WCAP_THK(:) = 0.
    WCAP_BHS(:) = 0.
    WCAP_MNT(:) = 0.
#endif

    ! ---------------------------------------------------------------------
    ! Start of loop over tiles:
    ! ---------------------------------------------------------------------
    CHUNKN = 0
    DO
      ! Get start and end indices of tile:
      CHUNK0 = CHUNKN + 1

#ifdef W3_PDLIB
      ! In W3SRCE, the loop is 1:NP for the src_inp_pre loop, rather than 1:NSEAL
      ! Should it be the same for srce_imp_post?    
      IF(srce_call .EQ. srce_imp_pre) THEN
        IF(CHUNK0 .GT. NP) EXIT
      ENDIF
#endif
      IF(CHUNK0 .GT. NSEAL) EXIT
      CHUNKN = MIN(NSEAL,CHUNK0 + CHUNKSIZE - 1)
      NSEAC = CHUNKN - CHUNK0 + 1

      !! GPU Refactor: Now in section 1
      ! DEPTH  = MAX ( DMIN , D_INP )
      ! DRAT = DAIR / DWAT
      
      !! GPU Refactor: Moved to section 4:
      !ICESCALELN = MAX(0.,MIN(1.,1.-ICE*ICESCALES(1)))
      !ICESCALEIN = MAX(0.,MIN(1.,1.-ICE*ICESCALES(2)))
      !ICESCALENL = MAX(0.,MIN(1.,1.-ICE*ICESCALES(3)))
      !ICESCALEDS = MAX(0.,MIN(1.,1.-ICE*ICESCALES(4)))

      !! Initialise source term arrays:
      VS   = 0.
      VD   = 0.
      VSBT = 0.
      VDBT = 0.

#if defined(W3_LN0) || defined(W3_LN1) || defined(W3_SEED)
      VSLN = 0.
#endif

#if defined(W3_ST0) || defined(W3_ST3) || defined(W3_ST4)
      VSIN(:,:) = 0.
      VDIN(:,:) = 0.
#endif

#if defined(W3_NL0) || defined(W3_NL1)
      VSNL = 0.
      VDNL = 0.
#endif

#ifdef W3_TR1
      VSTR = 0.
      VDTR = 0.
#endif

#if defined(W3_ST0) || defined(W3_ST4)
      VSDS = 0.
      VDDS = 0.
#endif

#ifdef W3_DB1
      VSDB = 0.
      VDDB = 0.
#endif

#if defined(W3_IC1) || defined(W3_IC2) || defined(W3_IC3) || defined(W3_IC4) || defined(W3_IC5)
      VSIC = 0.
      VDIC = 0.
#endif

#ifdef W3_UOST
      VSUO = 0.
      VDUO = 0.
#endif

#if defined(W3_IS1) || defined(W3_IS2)
      VSIR = 0.
      VDIR = 0.
#endif

#ifdef W3_IS2
      VDIR2 = 0.
#endif
      !
#ifdef W3_ST6
      VSWL = 0.
      VDWL = 0.
#endif

      ! Set ZWND depeding on source term package
#if defined(W3_ST0) || defined(W3_ST1) || defined(W3_ST6)
      ZWND = 10.
#endif

#if defined(W3_ST2)
      ZWND = ZWIND
#endif

#if defined(W3_ST4)
      ZWND = ZZWND
#endif

#ifdef W3_T
      ! TODO - move this (depth not calculated yet)
      !WRITE (NDST,9000)
      !WRITE (NDST,9001) DEPTH, U10ABS, U10D_CHUNK(CSEA)*RADE
#endif
      !
      ! 1.  Preparations --------------------------------------------------- *

#if MANM 
!$ACC KERNELS        
          !TODO: GPU notes: This will need DATA section to work
          !      efficiently with explicit transfers
#endif         
      ! GPU Refactor - gather full domain variables (defined with size
      ! NSEA) to local domain variables (define with size NSEAL)
      !
      ! Not technically required when running SHRD mode
      ! TODO: Look into workaround for this, use pointers instead?
      ! TODO - rewrite to loop over I for GPU, to avoid loop dependency.

      I = 1
      DO JSEA=CHUNK0,CHUNKN
        CALL INIT_GET_ISEA(ISEA, JSEA)  !!! TODO: Potentially slow! Precalculate?

        IX(I) = MAPSF(ISEA,1)
        IY(I) = MAPSF(ISEA,2)

        CG1_CHUNK(:,I) = CG1(1:NK,ISEA)
        WN1_CHUNK(:,I) = WN1(1:NK,ISEA)
        U10_CHUNK(I) = U10ABS(ISEA)
        U10D_CHUNK(I) = U10DIR(ISEA)
        UST_CHUNK(I) = USTAR(ISEA)
        USTD_CHUNK(I) = USTDIR(ISEA)
        DAIR_CHUNK(I) = DAIR(ISEA)
        COEF_CHUNK(I) = COEF(ISEA)   !! TODO: Only non-1 value if STAB2 set

#if defined(W3_ST3) || defined(W3_ST4)
        AS_CHUNK(I) = AS(ISEA)
#endif
#ifdef W3_FLX5
        TAUA_CHUNK(I) = TAUA(ISEA)
        TAUADIR_CHUNK(I) = TAUADIR(ISEA)
#endif
#if defined(W3_BS1) || defined(W3_REF1)
        CX_CHUNK(I) = CX(ISEA)
        CY_CHUNK(I) = CY(ISEA)
#endif
#ifdef W3_REF1
        TRNX_CHUNK(I) = TRNX(IY(I),IX(I))
        TRNY_CHUNK(I) = TRNY(IY(I),IX(I))
        REFLEC_CHUNK(:,I) = REFLEC(:,ISEA)
        REFLED_CHUNK(:,I) = REFLED(:,ISEA)
        BERG_CHUNK(I) = BERG(ISEA)
#endif
#ifdef W3_BT4
        D50_CHUNK(I) = D50(ISEA)
        PSIC_CHUNK(I) = PSIC(ISEA)
#endif
#ifdef W3_PDLIB
        CLATSL_CHUNK(I) = CLATSL(ISEA)
#endif
        DRAT(I) = DAIR(ISEA) / DWAT
        DEPTH(I)  = MAX(DMIN , D_INP(ISEA))
#ifdef W3_MLIM
        ! Do we want D_INP_CHUNK here for Miche Limiter?
        ! Maybe just calculated ISEA in place in this case?
        ! (That's what I've done - see MLIM section below)
#endif
        ! Only bother copying ice if ice field read in (INFLAGS(4) is TRUE):
        IF(INFLAGS2(4)) THEN
          ICE_CHUNK(I) = ICE(ISEA)
          ICEH_CHUNK(I) = ICEH(ISEA)
          ICEF_CHUNK(I) = ICEF(ISEA)
#ifdef W3_IS2
          ICEDMAX_CHUNK(I) = ICEDMAX(ISEA)
#endif
        ENDIF
        !
        ! 1.a Set maximum change and wavenumber arrays.
        !
        DO IK=1, NK
          DAM(1+(IK-1)*NTH,I) = FACP / ( SIG(IK) * WN1(IK,ISEA)**3 )
          WN2(1+(IK-1)*NTH,I) = WN1(IK,ISEA)
        END DO
        !
        DO IK=1, NK
          IS0    = (IK-1)*NTH
          DO ITH=2, NTH
            DAM(ITH+IS0,I) = DAM(1+IS0,I)
            WN2(ITH+IS0,I) = WN2(1+IS0,I)
          END DO
        END DO

        ! Set mask for computation of source terms based on MAPSTA
        ! and FLAGST. This originally is done in w3wavemd as a
        ! conditional statement around the W3SRCE call
        SRC_MASK(I) = .NOT. (MAPSTA(IY(I),IX(I)) .EQ. 1 .AND. FLAGST(ISEA))

        I = I + 1
      ENDDO ! Gather to local grid loop
#if MANM
!$ACC END KERNELS
#endif
      !
      ! 1.b Prepare dynamic time stepping
      !
      ! Refactor: Zero "chunksize" dimensioned variables
      DTTOT  = 0.
      NSTEPS = 0
      !PHINL  = 0.  ! Calculated value never used...ditch?
      TAUWAX = 0.
      TAUWAY = 0.  ! Only for ST3, ST4, ST6
      TAUSCX = 0.  ! Only for W3_BS1
      TAUSCY = 0.
      !
      ! TIME is updated in W3WAVEMD prior to the call of W3SCRE, we should
      ! move 'TIME' one time step backward (QL)
#ifdef W3_NL5
      !QI5TSTART = TIME
      !CALL TICK21 (QI5TSTART, -1.0 * DTG)
      QI5TSTART(:,1) = TIME
      CALL TICK21 (QI5TSTART(:,1), -1.0 * DTG)
      IF(NSEAC .GT. 1) THEN
        DO CSEA=2,NSEAC
          ! GPU Refactor - to avoid calling TICK21 unneccesarily in a loop:
          QI5TSTART(1:2,CSEA) = QI5TSTART(1:2,1)
        END DO
      END IF
#endif
      !
#ifdef W3_DEBUGSRC
      IF (IX(CSEA) .eq. DEBUG_NODE) THEN
        WRITE(740+IAPROC,*) 'W3SRCE start sum(SPEC)=', sum(SPEC)
        !WRITE(740+IAPROC,*) 'W3SRCE start sum(SPECOLD)=', sum(SPECOLD)
        WRITE(740+IAPROC,*) 'W3SRCE start sum(SPECINIT)=', sum(SPECINIT)
        WRITE(740+IAPROC,*) 'W3SRCE start sum(VSIO)=', sum(VSIO)
        WRITE(740+IAPROC,*) 'W3SRCE start sum(VDIO)=', sum(VDIO)
        WRITE(740+IAPROC,*) 'W3SRCE start USTAR=', UST_CHUNK(CSEA)
      END IF
#endif

#ifdef W3_ST4
      DLWMEAN(:) = 0.
      BRLAMBDA(:,:) = 0.
      !WHITECAP(:,:) = 0.      Don't zero here - is in seapoint loop
#endif
      !
      ! 1.c Set mean parameters
      !
      DO CSEA=1,NSEAC
        IF(SRC_MASK(CSEA)) CYCLE
        JSEA = CHUNK0 + CSEA - 1

        ! CB Refactor - zero CHARN element wise, rather than whole array (for b4b reproducibility)
        CHARN(JSEA) = 0.

#ifdef W3_ST0
        CALL W3SPR0 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), WNMEAN(JSEA), AMAX(CSEA))
        FP(CSEA) = 0.85 * FMEAN(CSEA)
#endif
#ifdef W3_ST1
        CALL W3SPR1 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), WNMEAN(JSEA), AMAX(CSEA))
        FP(CSEA)= 0.85 * FMEAN(CSEA)
#endif
#ifdef W3_ST2
        CALL INIT_GET_ISEA(ISEA, JSEA)  !! TODO - to keep FPI working
        CALL W3SPR2 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), DEPTH(CSEA), FPI(ISEA), U10_CHUNK(CSEA), UST_CHUNK(CSEA),  &
           EMEAN(CSEA), FMEAN(CSEA), WNMEAN(JSEA), AMAX(CSEA), ALPHA(:,JSEA), FP(CSEA) )
#endif
#ifdef W3_ST3
        TAUWX(JSEA)=0.
        TAUWY(JSEA)=0.
        IF ( IT .eq. 0 ) THEN
          LLWS(:,CSEA) = .TRUE.
          UST_CHUNK(CSEA)=0.
          USTD_CHUNK(CSEA)=0.
          CALL W3SPR3 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), FMEANS(CSEA), WNMEAN(JSEA), &
             AMAX(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA), UST_CHUNK(CSEA), USTD_CHUNK(CSEA),          &
             TAUWX(JSEA), TAUWY(JSEA), CD(CSEA), Z0(CSEA), CHARN(JSEA), LLWS(:,CSEA), FMEANWS(CSEA))
        ELSE
          CALL W3SPR3 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), FMEANS(CSEA), WNMEAN(JSEA), &
             AMAX(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA), UST_CHUNK(CSEA), USTD_CHUNK(CSEA),          &
             TAUWX(JSEA), TAUWY(JSEA), CD(CSEA), Z0(CSEA), CHARN(JSEA), LLWS(:,CSEA), FMEANWS(CSEA))
          CALL W3SIN3 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN2(:,CSEA), U10_CHUNK(CSEA), UST_CHUNK(CSEA), DRAT(CSEA), AS_CHUNK(CSEA),   &
             U10D_CHUNK(CSEA), Z0(CSEA), CD(CSEA), TAUWX(JSEA), TAUWY(JSEA), TAUWAX(CSEA), TAUWAY(CSEA),   &
             ICE_CHUNK(CSEA), VSIN(:,CSEA), VDIN(:,CSEA), LLWS(:,CSEA), IX(CSEA), IY(CSEA) )
        END IF
        CALL W3SPR3 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), FMEANS(CSEA), WNMEAN(JSEA), &
           AMAX(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA), UST_CHUNK(CSEA), USTD_CHUNK(CSEA),          &
           TAUWX(JSEA), TAUWY(JSEA), CD(CSEA), Z0(CSEA), CHARN(JSEA), LLWS(:,CSEA), FMEANWS(CSEA))
        TWS(JSEA) = 1./FMEANWS(CSEA)
#endif
#ifdef W3_ST4
        IF (SINTAILPAR(4).GT.0.5) THEN ! this is designed to keep the bug as an option
              TAUWX(JSEA)=0.
              TAUWY(JSEA)=0.
        END IF
        IF ( IT .EQ. 0 ) THEN
          LLWS(:,CSEA) = .TRUE.
          TAUWX(JSEA)=0.
          TAUWY(JSEA)=0.
          UST_CHUNK(CSEA)=0.
          USTD_CHUNK(CSEA)=0.
        ELSE
          CALL W3SPR4 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), FMEAN1(CSEA), WNMEAN(JSEA), &
             AMAX(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA),                           &
#ifdef W3_FLX5
             TAUA_CHUNK(CSEA), TAUADIR_CHUNK(CSEA), DAIR_CHUNK(CSEA),    &
#endif
             UST_CHUNK(CSEA), USTD_CHUNK(CSEA),                                  &
             TAUWX(JSEA), TAUWY(JSEA), CD(CSEA), Z0(CSEA), CHARN(JSEA), LLWS(:,CSEA), FMEANWS(CSEA), DLWMEAN(CSEA))
#endif

#if defined(W3_DEBUGSRC) && defined(W3_ST4)
          IF (IX(CSEA) == DEBUG_NODE) THEN
            WRITE(740+IAPROC,*) '1: out value USTAR=', UST_CHUNK(CSEA), ' USTDIR=', USTD_CHUNK(CSEA)
            WRITE(740+IAPROC,*) '1: out value EMEAN(JSEA)=', EMEAN(CSEA), ' FMEAN(JSEA)=', FMEAN(JSEA)
            WRITE(740+IAPROC,*) '1: out value FMEAN1(JSEA)=', FMEAN1(CSEA), ' WNMEAN(JSEA)=', WNMEAN(JSEA)
            WRITE(740+IAPROC,*) '1: out value CD=', CD(CSEA), ' Z0=', Z0(CSEA)
            WRITE(740+IAPROC,*) '1: out value ALPHA=', CHARN(JSEA), ' FMEANWS=', FMEANWS(CSEA)
          END IF
#endif

#ifdef W3_ST4
          IF (SINTAILPAR(4).GT.0.5) THEN
            CALL W3SIN4 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN2(:,CSEA), U10_CHUNK(CSEA), UST_CHUNK(CSEA), DRAT(CSEA), AS_CHUNK(CSEA),       &
              U10D_CHUNK(CSEA), Z0(CSEA), CD(CSEA), TAUWX(JSEA), TAUWY(JSEA), TAUWAX(CSEA), TAUWAY(CSEA),       &
              VSIN(:,CSEA), VDIN(:,CSEA), LLWS(:,CSEA), IX(CSEA), IY(CSEA), BRLAMBDA(:,CSEA) )
          END IF
        END IF  ! IT==0
#endif
#if defined(W3_DEBUGSRC) && defined(W3_ST4)
        IF (IX(CSEA) == DEBUG_NODE) THEN
          WRITE(740+IAPROC,*) '1: U10DIR=', U10D_CHUNK(CSEA), ' Z0=', Z0(CSEA), ' CHARN=', CHARN(JSEA)
          WRITE(740+IAPROC,*) '1: USTAR=', UST_CHUNK(CSEA), ' U10ABS=', U10_CHUNK(CSEA), ' AS=', AS_CHUNK(CSEA)
          WRITE(740+IAPROC,*) '1: DRAT=', DRAT(CSEA)
          WRITE(740+IAPROC,*) '1: TAUWX=', TAUWX(JSEA), ' TAUWY=', TAUWY(JSEA)
          WRITE(740+IAPROC,*) '1: TAUWAX=', TAUWAX, ' TAUWAY=', TAUWAY
          WRITE(740+IAPROC,*) '1: min(CG1)=', minval(CG1_CHUNK(:,CSEA)), ' max(CG1)=', maxval(CG1_CHUNK(:,CSEA))
          WRITE(740+IAPROC,*) '1: W3SIN4(min/max/sum)VSIN=', minval(VSIN(:,CSEA)), maxval(VSIN(:,CSEA)), sum(VSIN(:,CSEA))
          WRITE(740+IAPROC,*) '1: W3SIN4(min/max/sum)VDIN=', minval(VDIN(:,CSEA)), maxval(VDIN(:,CSEA)), sum(VDIN(:,CSEA))
        END IF
#endif

#ifdef W3_ST4
        CALL W3SPR4 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), &
          EMEAN(CSEA), FMEAN(CSEA), FMEAN1(CSEA), WNMEAN(JSEA), &
          AMAX(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA),        &
#ifdef W3_FLX5
          TAUA_CHUNK(CSEA), TAUADIR_CHUNK(CSEA), DAIR_CHUNK(CSEA),                    &
#endif
          UST_CHUNK(CSEA), USTD_CHUNK(CSEA),                                &
          TAUWX(JSEA), TAUWY(JSEA), CD(CSEA), Z0(CSEA), CHARN(JSEA), &
          LLWS(:,CSEA), FMEANWS(CSEA), DLWMEAN(CSEA))

        TWS(JSEA) = 1./FMEANWS(CSEA)
#endif
#ifdef W3_ST6
        CALL W3SPR6 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), WNMEAN(JSEA), AMAX(CSEA), FP(CSEA))
#endif

      END DO ! CSEA
      !
      ! 1.c2 Stores the initial data
      !
      SPECINIT(:,:NSEAC) = SPEC(:,CHUNK0:CHUNKN)
      !
      ! 1.d Stresses
      !
      DO CSEA = 1,NSEAC
        IF(SRC_MASK(CSEA)) CYCLE
        JSEA = CHUNK0 + CSEA - 1
#ifdef W3_FLX1
        CALL W3FLX1 ( ZWND, U10_CHUNK(CSEA), U10D_CHUNK(CSEA), UST_CHUNK(CSEA), USTD_CHUNK(CSEA), Z0(CSEA), CD(CSEA) )
#endif
#ifdef W3_FLX2
        CALL W3FLX2 ( ZWND, DEPTH(CSEA), FP(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA),            &
           UST_CHUNK(CSEA), USTD_CHUNK(CSEA), Z0(CSEA), CD(CSEA) )
#endif
#ifdef W3_FLX3
        CALL W3FLX3 ( ZWND, DEPTH(CSEA), FP(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA),            &
           UST_CHUNK(CSEA), USTD_CHUNK(CSEA), Z0(CSEA), CD(CSEA) )
#endif
#ifdef W3_FLX4
        CALL W3FLX4 ( ZWND, U10_CHUNK(CSEA), U10D_CHUNK(CSEA), UST_CHUNK(CSEA), USTD_CHUNK(CSEA), Z0(CSEA), CD(CSEA) )
#endif
#ifdef W3_FLX5
        CALL W3FLX5 ( ZWND, U10_CHUNK(CSEA), U10D_CHUNK(CSEA), TAUA_CHUNK(CSEA), TAUADIR_CHUNK(CSEA), DAIR_CHUNK(CSEA),  &
           UST_CHUNK(CSEA), USTD_CHUNK(CSEA), Z0(CSEA), CD(CSEA), CHARN(JSEA) )
#endif
      END DO !CSEA

      !
      ! 1.e Prepare cut-off beyond which the tail is imposed with a power law
      !
#ifdef W3_ST0
      FHIGH(1:NSEAC)  = SIG(NK)
#endif
#ifdef W3_ST1
      ! TODO: This is mostly same for ST1, ST3, ST4, ST6
      DO CSEA=1,NSEAC
        FH1 = FXFM * FMEAN(CSEA)
        FH2 = FXPM / UST_CHUNK(CSEA)
        FHIGH(CSEA)  = MAX ( FH1 , FH2 )
        IF (FLTEST) WRITE (NDST,9004) FH1*TPIINV, FH2*TPIINV, FHIGH(CSEA)*TPIINV
      END DO
#endif
#ifdef W3_ST2
      DO CSEA=1,NSEAC
        JSEA = CHUNK0 + CSEA - 1
        CALL INIT_GET_ISEA(ISEA, JSEA)  !! TODO - to keep FPI working
        FHIGH(CSEA) = XFC * FPI(ISEA)
      ENDDO
      !!FHIGH(1:NSEAC) = XFC * FPI(CSEA)  ! Have to do this explicitly above as FPI is NSEA and I/O
#endif
#ifdef W3_ST3
      FHIGH(1:NSEAC) = MAX(FFXFM * MAX(FMEAN(1:NSEAC),FMEANWS(1:NSEAC)), &
                   FFXPM / UST_CHUNK(1:NSEAC))
#endif
#ifdef W3_ST4
      ! Introduces a Long & Resio (JGR2007) type dependance on wave age
      ! !/ST4      FAGE   = FFXFA*TANH(0.3*U10ABS*FMEANWS*TPI/GRAV)
      FAGE(1:NSEAC) = 0.
      FHIGH(1:NSEAC) = MAX( (FFXFM + FAGE(1:NSEAC) ) * &
          MAX(FMEAN1(1:NSEAC),FMEANWS(1:NSEAC)), FFXPM / UST_CHUNK(1:NSEAC))
      !!FHIGI(1:NSEAC) = FFXFA * FMEAN1(1:NSEAC) ! Not used
#endif
#ifdef W3_ST6
      IF (FXFM .LE. 0) THEN
        FHIGH(1:NSEAC) = SIG(NK)
      ELSE
        FHIGH(1:NSEAC) = MAX(FXFM * FMEAN(1:NSEAC), FXPM / UST_CHUNK(1:NSEAC))
      ENDIF
#endif
      !
      ! 1.f Prepare output file for !/NNT option
      !     TODO: AT MOMENT THIS WILL ONLY WORK WITH CHUNKSIZE=1
#ifdef W3_NNT
      IF ( IT .EQ. 0 ) THEN
        J = LEN_TRIM(FNMPRE)
        WRITE (FNAME(11:13),'(I3.3)') IAPROC
        OPEN (NDSD,FILE=FNMPRE(:J)//FNAME,form='UNFORMATTED', convert=file_endian,   &
             ERR=800,IOSTAT=IERR)
        WRITE (NDSD,ERR=801,IOSTAT=IERR) NK, NTH
        WRITE (NDSD,ERR=801,IOSTAT=IERR) SIG(1:NK) * TPIINV
        OPEN (NDSD2,FILE=FNMPRE(:J)//'time.ww3',                &
             FORM='FORMATTED',ERR=800,IOSTAT=IERR)
      END IF
#endif
      !
      ! ... Branch point dynamic integration - - - - - - - - - - - - - - - -
      !
      DO
        !
        NSTEPS = NSTEPS + 1
        !
#ifdef W3_T
        WRITE (NDST,9020) NSTEPS, DTTOT
#endif
        !
        ! 2.  Calculate source terms ----------------------------------------- *
        !
        ! 2.a Input.
        !
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1
#ifdef W3_LN1
          CALL W3SLN1 (WN1_CHUNK(:,CSEA), FHIGH(CSEA), UST_CHUNK(CSEA), U10D_CHUNK(CSEA), &
             VSLN(:,CSEA) )
#endif
        ENDDO ! CSEA loop - W3LNx
        !
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1
#ifdef W3_ST1
          CALL W3SIN1 (SPEC(:,JSEA), WN2(:,CSEA), UST_CHUNK(CSEA), U10D_CHUNK(CSEA), VSIN(:,CSEA), VDIN(:,CSEA) )
#endif
#ifdef W3_ST2
          CALL INIT_GET_ISEA(ISEA, JSEA)  !! TODO - to keep FPI working
          CALL W3SIN2 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN2(:,CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA), CD(CSEA), Z0(CSEA),  &
             FPI(ISEA), VSIN(:,CSEA), VDIN(:,CSEA) )
#endif
#ifdef W3_ST3
          CALL W3SIN3 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN2(:,CSEA), U10_CHUNK(CSEA), &
             UST_CHUNK(CSEA), DRAT(CSEA), AS_CHUNK(CSEA), U10D_CHUNK(CSEA),            &
             Z0(CSEA), CD(CSEA), TAUWX(JSEA), TAUWY(JSEA), TAUWAX(CSEA), TAUWAY(CSEA), &
             ICE_CHUNK(CSEA), VSIN(:,CSEA), VDIN(:,CSEA), LLWS(:,CSEA), IX(CSEA), IY(CSEA) )
#endif
#ifdef W3_ST4
          ! TESTING!
          VSIN(:,CSEA)=0  ! Not needed?
          VDIN(:,CSEA)=0 
          BRLAMBDA(:,CSEA)=0   ! TODO: Shouldn't be needed
          ! END TESTING !
          CALL W3SIN4 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN2(:,CSEA), &
             U10_CHUNK(CSEA), UST_CHUNK(CSEA), DRAT(CSEA), AS_CHUNK(CSEA),       &
             U10D_CHUNK(CSEA), Z0(CSEA), CD(CSEA), TAUWX(JSEA), TAUWY(JSEA), &
             TAUWAX(CSEA), TAUWAY(CSEA),       &
             VSIN(:,CSEA), VDIN(:,CSEA), LLWS(:,CSEA), IX(CSEA), IY(CSEA), BRLAMBDA(:,CSEA) )
#endif

#if defined(W3_DEBUGSRC) && defined(W3_ST4)
          IF (IX(CSEA) == DEBUG_NODE) THEN
            WRITE(740+IAPROC,*) '2 : W3SIN4(min/max/sum)VSIN=', minval(VSIN(:,CSEA)), maxval(VSIN(:,CSEA)), sum(VSIN(:,CSEA))
            WRITE(740+IAPROC,*) '2 : W3SIN4(min/max/sum)VDIN=', minval(VDIN(:,CSEA)), maxval(VDIN(:,CSEA)), sum(VDIN(:,CSEA))
          END IF
#endif

#ifdef W3_ST6
          CALL W3SIN6 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN2(:,CSEA), U10_CHUNK(CSEA), UST_CHUNK(CSEA), USTD_CHUNK(CSEA), CD(CSEA), DAIR_CHUNK(CSEA), &
             TAUWX(JSEA), TAUWY(JSEA), TAUWAX(CSEA), TAUWAY(CSEA), VSIN(:,CSEA), VDIN(:,CSEA) )
#endif
        END DO ! CSEA; W3SINx
        !
        ! 2.b Nonlinear interactions.
        !
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1
#ifdef W3_NL1
          IF (IQTPE.GT.0) THEN
            CALL W3SNL1 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), &
              WNMEAN(JSEA)*DEPTH(CSEA), VSNL(:,CSEA), VDNL(:,CSEA) )
          ELSE
            CALL W3SNLGQM ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), &
              DEPTH(CSEA), VSNL(:,CSEA), VDNL(:,CSEA) )
          END IF
#endif
#ifdef W3_NL2
          CALL W3SNL2 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), DEPTH(CSEA), VSNL(:,CSEA), VDNL(:,CSEA) )
#endif
#ifdef W3_NL3
          CALL W3SNL3 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), DEPTH(CSEA), VSNL(:,CSEA), VDNL(:,CSEA) )
#endif
#ifdef W3_NL4
          CALL W3SNL4 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), DEPTH(CSEA), VSNL(:,CSEA), VDNL(:,CSEA) )
#endif
#ifdef W3_NL5
          CALL W3SNL5 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), FMEAN(CSEA), QI5TSTART(:,CSEA),          &
             U10_CHUNK(CSEA), U10D_CHUNK(CSEA), JSEA, VSNL(:,CSEA), VDNL(:,CSEA), QR5KURT)
#endif
        END DO ! CSEA; W3SNLx     
        !
#ifdef W3_PDLIB
        IF (.NOT. FSSOURCE .or. LSLOC) THEN
#endif
#ifdef W3_TR1
          DO CSEA=1,NSEAC
            IF(SRC_MASK(CSEA)) CYCLE
            JSEA = CHUNK0 + CSEA - 1
            ! Refactor: IX and SPECOLD not used in W3STR1 - removed.
            !CALL W3STR1 ( SPEC(:,JSEA), SPECOLD, CG1_CHUNK(:,CSEA),  &
            CALL W3STR1 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA),  &
              WN1_CHUNK(:,CSEA), DEPTH(CSEA), VSTR(:,CSEA), VDTR(:,CSEA) )
          END DO ! CSEA; W3STR1
#endif
#ifdef W3_PDLIB
        ENDIF
#endif
        !
        ! 2.c Dissipation... except for ST4
        ! 2.c1   as in source term package
        !
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1

#ifdef W3_ST1
          CALL W3SDS1 ( SPEC(:,JSEA), WN2(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), WNMEAN(JSEA), VSDS(:,CSEA), VDDS(:,CSEA) )
#endif
#ifdef W3_ST2
          CALL INIT_GET_ISEA(ISEA, JSEA)  !! TODO - to keep FPI working
          CALL W3SDS2 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), FPI(ISEA), UST_CHUNK(CSEA), ALPHA(:,JSEA), VSDS(:,CSEA), VDDS(:,CSEA) )
#endif
#ifdef W3_ST3
! IX/IY not used...
          CALL W3SDS3 ( SPEC(:,JSEA), WN1_CHUNK(:,CSEA), CG1_CHUNK(:,CSEA), EMEAN(CSEA), FMEANS(CSEA), WNMEAN(JSEA),  &
             UST_CHUNK(CSEA), USTD_CHUNK(CSEA), DEPTH(CSEA), VSDS(:,CSEA), VDDS(:,CSEA), IX(CSEA), IY(CSEA) )
#endif
#ifdef W3_ST4
! IX/IY not used...
          CALL W3SDS4 ( SPEC(:,JSEA), WN1_CHUNK(:,CSEA), CG1_CHUNK(:,CSEA), UST_CHUNK(CSEA), USTD_CHUNK(CSEA), DEPTH(CSEA), DAIR_CHUNK(CSEA), VSDS(:,CSEA),   &
             VDDS(:,CSEA), IX(CSEA), IY(CSEA), BRLAMBDA(:,CSEA), WCAP_COV(JSEA), WCAP_THK(JSEA), WCAP_MNT(JSEA), DLWMEAN(CSEA) )
#endif
#if defined(W3_DEBUGSRC) && defined(W3_ST4)
          IF (IX(CSEA) == DEBUG_NODE) THEN
            WRITE(740+IAPROC,*) '2 : W3SDS4(min/max/sum)VSDS=', minval(VSDS(:,CSEA)), maxval(VSDS(:,CSEA)), sum(VSDS(:,CSEA))
            WRITE(740+IAPROC,*) '2 : W3SDS4(min/max/sum)VDDS=', minval(VDDS(:,CSEA)), maxval(VDDS(:,CSEA)), sum(VDDS(:,CSEA))
          END IF
#endif

#ifdef W3_ST6
          CALL W3SDS6 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA),  VSDS(:,CSEA), VDDS(:,CSEA) )
#endif
        END DO ! CSEA; W3SDSx
        !     
#ifdef W3_PDLIB
        IF (.NOT. FSSOURCE .or. LSLOC) THEN
#endif
#ifdef W3_DB1
          DO CSEA=1,NSEAC
            IF(SRC_MASK(CSEA)) CYCLE
            JSEA = CHUNK0 + CSEA - 1

            ! Note: LBREAK not used in W3SRCE - can be dummy scalar
            ! IX only used for DEBUG
            CALL W3SDB1 ( IX(CSEA), SPEC(:,JSEA), DEPTH(CSEA), EMEAN(CSEA), FMEAN(CSEA), &
                WNMEAN(JSEA), CG1_CHUNK(:,CSEA), LBREAK, VSDB(:,CSEA), VDDB(:,CSEA) )
          END DO ! CSEA; W3SDBx
#endif
#ifdef W3_PDLIB
        ENDIF
#endif
        !
        ! 2.c2   optional dissipation parameterisations
        !
#ifdef W3_ST6
        IF (SWL6S6) THEN
          DO CSEA=1,NSEAC
            IF(SRC_MASK(CSEA)) CYCLE
            JSEA = CHUNK0 + CSEA - 1
            CALL W3SWL6 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), VSWL(:,CSEA), VDWL(:,CSEA) )
          ENDDO ! CSEA; W3SWL6
        END IF
#endif
        !
        ! 2.d Bottom interactions.
        !
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1
#ifdef W3_BT1
          CALL W3SBT1 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), &
              DEPTH(CSEA), VSBT(:,CSEA), VDBT(:,CSEA) )
#endif
#ifdef W3_BT4
! IX,IY not used
          CALL W3SBT4 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), &
              DEPTH(CSEA), D50_CHUNK(CSEA), PSIC_CHUNK(CSEA), TAUBBLX(JSEA), TAUBBLY(JSEA), &
              BEDROUGH(JSEA), BEDRIPX(JSEA), BEDRIPY(JSEA), VSBT(:,CSEA), VDBT(:,CSEA), IX(CSEA), IY(CSEA) )
#endif
#ifdef W3_BT8
          CALL W3SBT8 ( SPEC(:,JSEA), DEPTH(CSEA), VSBT(:,CSEA), VDBT(:,CSEA), IX(CSEA), IY(CSEA) )
#endif
#ifdef W3_BT9
          CALL W3SBT9 ( SPEC(:,JSEA), DEPTH(CSEA), VSBT(:,CSEA), VDBT(:,CSEA), IX(CSEA), IY(CSEA) )
#endif
        END DO ! CSEA; W3SBTx
!
#ifdef W3_BS1
          DO CSEA=1,NSEAC
            IF(SRC_MASK(CSEA)) CYCLE
            JSEA = CHUNK0 + CSEA - 1

            CALL W3SBS1 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), & 
                DEPTH(CSEA), CX_CHUNK(CSEA), CY_CHUNK(CSEA), TAUSCX, TAUSCY, VSBS(:,CSEA), VDBS(:,CSEA) ) ! TODO - TAUSC[XY] not used.
          END DO ! CSEA; W3SBSx
#endif
        !
        ! 2.e Unresolved Obstacles Source Term
        !
#ifdef W3_UOST
        ! UNRESOLVED OBSTACLES
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1
          CALL UOST_SRCTRMCOMPUTE(IX(CSEA), IY(CSEA), SPEC(:,JSEA), CG1_CHUNK(:,CSEA), DT(CSEA),            &
             U10_CHUNK(CSEA), U10D_CHUNK(CSEA), VSUO(:,CSEA), VDUO(:,CSEA))
        END DO ! CSEA; UOST
#endif
        !
        ! 2.g Dump training data if necessary
        !
#ifdef W3_NNT
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1

          WRITE (SCREEN,8888) TIME, DTTOT, FLAGNN, QCERR
          WRITE (NDSD2,8888) TIME, DTTOT, FLAGNN, QCERR
8888      FORMAT (1X,I8.8,1X,I6.6,F8.1,L2,F8.2)
          WRITE (NDSD,ERR=801,IOSTAT=IERR) IX(CSEA), IY(CSEA), TIME, NSTEPS,        &
             DTTOT, FLAGNN, DEPTH(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA)
        !
          IF ( FLAGNN ) THEN
            DO JSEA=CHUNK0,CHUNKN
              DO IK=1, NK
                FACNN  = TPI * SIG(IK) / CG1_CHUNK(IK,CSEA)
                DO ITH=1, NTH
                  IS     = ITH + (IK-1)*NTH
                  FOUT(IK,ITH) = SPEC(IS, JSEA) * FACNN
                  SOUT(IK,ITH) = VSNL(IS, JSEA) * FACNN
                  DOUT(IK,ITH) = VDNL(IS, JSEA)
                END DO
              END DO
              WRITE (NDSD,ERR=801,IOSTAT=IERR) FOUT
              WRITE (NDSD,ERR=801,IOSTAT=IERR) SOUT
              WRITE (NDSD,ERR=801,IOSTAT=IERR) DOUT
            END DO ! ISEA
          END IF
        END DO ! CSEA; training data
#endif
        !
        ! 3.  Set frequency cut-off ------------------------------------------ *
        !
        ! GPU Refactor - loop over seapoints in chunk:
        DO CSEA = 1,NSEAC
          ! GPU Refactor - don't integrate if timestep for this spectum
          ! is complete, or point is not active.
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1

#ifdef W3_ST2
          CALL INIT_GET_ISEA(ISEA, JSEA)  !! TODO - to keep FPI working
          FHIGH(CSEA)  = XFC * FPI(ISEA)
          IF ( FLTEST ) WRITE (NDST,9005) FHIGH*TPIINV
#endif
          NKH(CSEA)    = MIN (NK, INT(FACTI2+FACTI1*LOG(MAX(1.E-7,FHIGH(CSEA)))) )
          NKH1(CSEA)   = MIN (NK, NKH(CSEA)+1 )
          NSPECH = NKH1(CSEA)*NTH
#ifdef W3_T
          WRITE (NDST,9021) NKH(CSEA), NKH1(CSEA), NSPECH
#endif
          !
          ! 4.  Summation of source terms and diagonal term and time step ------ *
          !
          DT(CSEA) = MIN ( DTG-DTTOT(CSEA) , DTMAX )
          AFILT  = MAX ( DAM(NSPEC,CSEA) , XFLT*AMAX(CSEA) )
          !
          !     For input and dissipation calculate the fraction of the ice-free
          !     surface. In the presence of ice, the effective water surface
          !     is reduce to a fraction of the cell size free from ice, and so is
          !     input :
          !             SIN = (1-ICE)**ISCALEIN*SIN and SDS=(1-ICE)**ISCALEDS*SDS ------------------ *
          !     INFLAGS2(4) is true if ice concentration was ever read during
          !             this simulation
          IF ( INFLAGS2(4) ) THEN

            ! GPU Refactor: ICESCALExx calculations moved from start of routine.
            ICESCALELN = MAX(0.,MIN(1.,1.-ICE_CHUNK(CSEA)*ICESCALES(1)))
            ICESCALEIN = MAX(0.,MIN(1.,1.-ICE_CHUNK(CSEA)*ICESCALES(2)))
            ICESCALENL = MAX(0.,MIN(1.,1.-ICE_CHUNK(CSEA)*ICESCALES(3)))
            ICESCALEDS = MAX(0.,MIN(1.,1.-ICE_CHUNK(CSEA)*ICESCALES(4)))

            VSNL(1:NSPECH,CSEA) = ICESCALENL * VSNL(1:NSPECH,CSEA)
            VDNL(1:NSPECH,CSEA) = ICESCALENL * VDNL(1:NSPECH,CSEA)
            VSLN(1:NSPECH,CSEA) = ICESCALELN * VSLN(1:NSPECH,CSEA)
            VSIN(1:NSPECH,CSEA) = ICESCALEIN * VSIN(1:NSPECH,CSEA)
            VDIN(1:NSPECH,CSEA) = ICESCALEIN * VDIN(1:NSPECH,CSEA)
            VSDS(1:NSPECH,CSEA) = ICESCALEDS * VSDS(1:NSPECH,CSEA)
            VDDS(1:NSPECH,CSEA) = ICESCALEDS * VDDS(1:NSPECH,CSEA)
          END IF

#ifdef W3_PDLIB
          IF (B_JGS_LIMITER_FUNC == 2) THEN
            DO IK=1, NK
              JAC      = CG1_CHUNK(IK,CSEA)/CLATSL_CHUNK(CSEA)
              JAC2     = 1./TPI/SIG(IK)
              FRLOCAL  = SIG(IK)*TPIINV
#ifdef W3_ST6
              DAM2(1+(IK-1)*NTH) = 5E-7 * GRAV/FRLOCAL**4 * UST_CHUNK(CSEA) * FMEAN(CSEA) * DTMIN * JAC * JAC2
#else
              DAM2(1+(IK-1)*NTH) = 5E-7 * GRAV/FRLOCAL**4 * UST_CHUNK(CSEA) * MAX(FMEANWS(CSEA),FMEAN(CSEA)) * DTMIN * JAC * JAC2
#endif
              !FROM WWM:           5E-7  * GRAV/FR(IS)**4          * UST_CHUNK(CSEA) * MAX(FMEANWS(IP),FMEAN(IP)) * DT4S/PI2/SPSIG(IS)
            END DO
            DO IK=1, NK
              IS0  = (IK-1)*NTH
              DO ITH=2, NTH
                DAM2(ITH+IS0) = DAM2(1+IS0)
              END DO
            END DO
          ENDIF
#endif
          !
          DO IS=IS1, NSPECH
            VS(IS,CSEA) = VSLN(IS,CSEA) + VSIN(IS,CSEA) + VSNL(IS,CSEA)  &
                 + VSDS(IS,CSEA) + VSBT(IS,CSEA)
#ifdef W3_ST6
            VS(IS,CSEA) = VS(IS,CSEA) + VSWL(IS,CSEA)
#endif
#if defined(W3_TR1) && !defined(W3_PDLIB)
            VS(IS,CSEA) = VS(IS,CSEA) + VSTR(IS,CSEA)
#endif
#ifdef W3_BS1
            VS(IS,CSEA) = VS(IS,CSEA) + VSBS(IS,CSEA)
#endif
#ifdef W3_UOST
            VS(IS,CSEA) = VS(IS,CSEA) + VSUO(IS,CSEA)
#endif
            VD(IS,CSEA) =  VDIN(IS,CSEA) + VDNL(IS,CSEA)  &
                 + VDDS(IS,CSEA) + VDBT(IS,CSEA)
#ifdef W3_ST6
            VD(IS,CSEA) = VD(IS,CSEA) + VDWL(IS,CSEA)
#endif
#if defined(W3_TR1) && !defined(W3_PDLIB)
            VD(IS,CSEA) = VD(IS,CSEA) + VDTR(IS,CSEA)
#endif
#ifdef W3_BS1
            VD(IS,CSEA) = VD(IS,CSEA) + VDBS(IS,CSEA)
#endif
#ifdef W3_UOST
            VD(IS,CSEA) = VD(IS,CSEA) + VDUO(IS,CSEA)
#endif
            DAMAX = MIN ( DAM(IS,CSEA) , MAX ( XREL*SPECINIT(IS,CSEA) , AFILT ) )
            AFAC = 1. / MAX( 1.E-10 , ABS(VS(IS,CSEA)/DAMAX) )
#ifdef W3_NL5
            IF (NL5_SELECT .EQ. 1)  THEN
              DT(CSEA) = MIN ( DT(CSEA) , AFAC / ( MAX ( 1.E-10,             &
                   1. + NL5_OFFSET*AFAC*MIN(0.,VD(IS,CSEA)) ) ) )
            ELSE
#endif
              DT(CSEA) = MIN ( DT(CSEA) , AFAC / ( MAX ( 1.E-10,                  &
                   1. + OFFSET*AFAC*MIN(0.,VD(IS,CSEA)) ) ) )
#ifdef W3_NL5
            ENDIF
#endif
          END DO  ! end of loop on IS

          !
          DT(CSEA) = MAX ( 0.5, DT(CSEA) ) ! The hardcoded min. dt is a problem for certain cases e.g. laborotary scale problems.
          !
          DTDYN(JSEA) = DTDYN(JSEA) + DT(CSEA)
#ifdef W3_T
          DTRAW = DT(CSEA)
#endif
          IDT = 1 + INT ( 0.99*(DTG-DTTOT(CSEA))/DT(CSEA) ) ! number of iterations
          DT(CSEA) = (DTG-DTTOT(CSEA))/REAL(IDT)           ! actualy time step
          SHAVE = DT(CSEA).LT.DTMIN .AND. DT(CSEA).LT.DTG-DTTOT(CSEA)   ! limiter check ...
          IF(PRESENT(SHAVEIO)) SHAVEIO(JSEA) = SHAVE
          DT(CSEA) = MAX ( DT(CSEA) , MIN (DTMIN,DTG-DTTOT(CSEA)) ) ! override dt with input time step or last time step if it is bigger ... anyway the limiter is on!
          !
#ifdef W3_NL5
          DT(CSEA) = INT(DT(CSEA)) * 1.0
#endif
          IF (srce_call .eq. srce_imp_post) DT(CSEA) = DTG  ! for implicit part
#ifdef W3_NL5
          IF (NL5_SELECT .EQ. 1) THEN
            HDT    = NL5_OFFSET * DT(CSEA)
          ELSE
#endif
            HDT    = OFFSET * DT(CSEA)
#ifdef W3_NL5
          ENDIF
#endif
          DTTOT(CSEA)  = DTTOT(CSEA) + DT(CSEA)

#ifdef W3_DEBUGSRC
          IF (IX(CSEA) == DEBUG_NODE) WRITE(*,'(A20,2I10,5F20.10,L20)') 'TIMINGS 2', IDT, NSTEPS, DT(CSEA), DTMIN, DTDYN(JSEA), HDT, DTTOT(CSEA), SHAVE
          IF (IX(CSEA) == DEBUG_NODE) THEN
            WRITE(740+IAPROC,*) '1: min/max/sum(VS)=', minval(VS(:,CSEA)), maxval(VS(:,CSEA)), sum(VS(:,CSEA))
            WRITE(740+IAPROC,*) '1: min/max/sum(VD)=', minval(VD(:,CSEA)), maxval(VD(:,CSEA)), sum(VD(:,CSEA))
            WRITE(740+IAPROC,*) 'min/max/sum(VSIN)=', minval(VSIN(:,CSEA)), maxval(VSIN(:,CSEA)), sum(VSIN(:,CSEA))
            WRITE(740+IAPROC,*) 'min/max/sum(VDIN)=', minval(VDIN(:,CSEA)), maxval(VDIN(:,CSEA)), sum(VDIN(:,CSEA))
            WRITE(740+IAPROC,*) 'min/max/sum(VSLN)=', minval(VSLN(:,CSEA)), maxval(VSLN(:,CSEA)), sum(VSLN(:,CSEA))
            WRITE(740+IAPROC,*) 'min/max/sum(VSNL)=', minval(VSNL(:,CSEA)), maxval(VSNL(:,CSEA)), sum(VSNL(:,CSEA))
            WRITE(740+IAPROC,*) 'min/max/sum(VDNL)=', minval(VDNL(:,CSEA)), maxval(VDNL(:,CSEA)), sum(VDNL(:,CSEA))
            WRITE(740+IAPROC,*) 'min/max/sum(VSDS)=', minval(VSDS(:,CSEA)), maxval(VSDS(:,CSEA)), sum(VSDS(:,CSEA))
            WRITE(740+IAPROC,*) 'min/max/sum(VDDS)=', minval(VDDS(:,CSEA)), maxval(VDDS(:,CSEA)), sum(VDDS(:,CSEA))
#ifdef W3_ST6
            WRITE(740+IAPROC,*) 'min/max/sum(VSWL)=', minval(VSWL(:,CSEA)), maxval(VSWL(:,CSEA)), sum(VSWL(:,CSEA))
            WRITE(740+IAPROC,*) 'min/max/sum(VDWL)=', minval(VDWL(:,CSEA)), maxval(VDWL(:,CSEA)), sum(VDWL(:,CSEA))
#endif
#ifdef W3_DB1
            WRITE(740+IAPROC,*) 'min/max/sum(VSDB)=', minval(VSDB), maxval(VSDB), sum(VSDB)
            WRITE(740+IAPROC,*) 'min/max/sum(VDDB)=', minval(VDDB), maxval(VDDB), sum(VDDB)
#endif
#ifdef W3_TR1
            WRITE(740+IAPROC,*) 'min/max/sum(VSTR)=', minval(VSTR), maxval(VSTR), sum(VSTR)
            WRITE(740+IAPROC,*) 'min/max/sum(VDTR)=', minval(VDTR), maxval(VDTR), sum(VDTR)
#endif
#ifdef W3_BS1
            WRITE(740+IAPROC,*) 'min/max/sum(VSBS)=', minval(VSBS(:,CSEA)), maxval(VSBS(:,CSEA)), sum(VSBS(:,CSEA))
            WRITE(740+IAPROC,*) 'min/max/sum(VDBS)=', minval(VDBS(:,CSEA)), maxval(VDBS(:,CSEA)), sum(VDBS(:,CSEA))
#endif
            WRITE(740+IAPROC,*) 'min/max/sum(VSBT)=', minval(VSBT(:,CSEA)), maxval(VSBT(:,CSEA)), sum(VSBT(:,CSEA))
            WRITE(740+IAPROC,*) 'min/max/sum(VDBT)=', minval(VDBT(:,CSEA)), maxval(VDBT(:,CSEA)), sum(VDBT(:,CSEA))
          END IF
#endif

#ifdef W3_PDLIB
          IF (srce_call .eq. srce_imp_pre) THEN
            IF (LSLOC) THEN
              IF (IMEM == 1) THEN
                SIDT  = PDLIB_SI(JSEA) * DTG
                DO IK = 1, NK
                  JAC = CLATSL_CHUNK(CSEA)/CG1_CHUNK(IK,CSEA)
                  DO ITH = 1, NTH
                    ISP = ITH + (IK-1)*NTH
                    VD(ISP,CSEA) = MIN(0., VD(ISP,CSEA))
                    IF (B_JGS_LIMITER_FUNC == 2) THEN
                      MAXDAC = MAX(DAM(ISP,CSEA),DAM2(ISP))
                    ELSE
                      MAXDAC = DAM(ISP,CSEA)
                    ENDIF
                    FAKS   = DTG / MAX ( 1. , (1.-DTG*VD(ISP,CSEA)))
                    DVS    = VS(ISP,CSEA) * FAKS
                    IF (.NOT. B_JGS_LIMITER) THEN
                      DVS  = SIGN(MIN(MAXDAC,ABS(DVS)),DVS)
                    ENDIF
                    PreVS  = DVS / FAKS
                    eVS    = PreVS / CG1_CHUNK(IK,CSEA) * CLATSL_CHUNK(CSEA)
                    eVD    = MIN(0.,VD(ISP,CSEA))
                    B_JAC(ISP,JSEA) = B_JAC(ISP,JSEA) + SIDT * (eVS - eVD*SPEC(ISP,JSEA)*JAC)
                    ASPAR_JAC(ISP,PDLIB_I_DIAG(JSEA)) = ASPAR_JAC(ISP,PDLIB_I_DIAG(JSEA)) - SIDT * eVD
#ifdef W3_DB1
                    eVS = VSDB(ISP,CSEA) * JAC
                    eVD = MIN(0.,VDDB(ISP,CSEA))
                    IF (eVS .gt. 0.) THEN
                      evS = 2*evS
                      evD = -evD
                    ELSE
                      evS = -evS
                      evD = 2*evD
                    ENDIF
#endif
                    B_JAC(ISP,JSEA) = B_JAC(ISP,JSEA) + SIDT * eVS
                    ASPAR_JAC(ISP,PDLIB_I_DIAG(JSEA)) = ASPAR_JAC(ISP,PDLIB_I_DIAG(JSEA)) - SIDT * eVD

#ifdef W3_TR1
                    eVS = VSTR(ISP,CSEA) * JAC
                    eVD = VDTR(ISP,CSEA)
                    IF (eVS .gt. 0.) THEN
                      evS = 2*evS
                      evD = -evD
                    ELSE
                      evS = -evS
                      evD = 2*evD
                    ENDIF
#endif
                    B_JAC(ISP,JSEA) = B_JAC(ISP,JSEA) + SIDT * eVS
                    ASPAR_JAC(ISP,PDLIB_I_DIAG(JSEA)) = ASPAR_JAC(ISP,PDLIB_I_DIAG(JSEA)) - SIDT * eVD
                  END DO
                END DO

              ELSEIF (IMEM == 2) THEN

                SIDT   = PDLIB_SI(JSEA) * DTG
                DO IK=1,NK
                  JAC = CLATSL_CHUNK(CSEA)/CG1_CHUNK(IK,CSEA)
                  DO ITH=1,NTH
                    ISP=ITH + (IK-1)*NTH
                    VD(ISP,CSEA) = MIN(0., VD(ISP,CSEA))
                    IF (B_JGS_LIMITER_FUNC == 2) THEN
                      MAXDAC = MAX(DAM(ISP,CSEA),DAM2(ISP))
                    ELSE
                      MAXDAC = DAM(ISP,CSEA)
                    ENDIF
                    FAKS = DTG / MAX ( 1. , (1.-DTG*VD(ISP,CSEA)))
                    DVS = VS(ISP,CSEA) * FAKS
                    IF (.NOT. B_JGS_LIMITER) THEN
                      DVS = SIGN(MIN(MAXDAC,ABS(DVS)),DVS)
                    ENDIF
                    PreVS = DVS / FAKS
                    eVS = PreVS / CG1_CHUNK(IK,CSEA) * CLATSL_CHUNK(CSEA)
                    eVD = VD(ISP,CSEA)
#ifdef W3_DB1
                    eVS = eVS + DBLE(VSDB(ISP,CSEA)) * JAC
                    eVD = evD + MIN(0.,DBLE(VDDB(ISP,CSEA)))
                    B_JAC(ISP,JSEA) = B_JAC(ISP,JSEA) + SIDT * (eVS - eVD*VA(ISP,JSEA))
                    ASPAR_DIAG_ALL(ISP,JSEA) = ASPAR_DIAG_ALL(ISP,JSEA) - SIDT * eVD
#endif
                  END DO
                END DO
              ENDIF
            ENDIF

            PrintDeltaSmDA=.FALSE.
            IF (PrintDeltaSmDA .eqv. .TRUE.) THEN
              DO IS=1,NSPEC
                DeltaSRC(IS) = VSIN(IS,CSEA) - SPEC(IS,JSEA) * VDIN(IS,CSEA)
              END DO
              WRITE(740+IAPROC,*) 'min/max/sum(VSIN)=', minval(VSIN(:,CSEA)), maxval(VSIN(:,CSEA)), sum(VSIN(:,CSEA))
              WRITE(740+IAPROC,*) 'min/max/sum(DeltaIN)=', minval(DeltaSRC), maxval(DeltaSRC), sum(DeltaSRC)
            !
              DO IS=1,NSPEC
                DeltaSRC(IS) = VSNL(IS,CSEA) - SPEC(IS,JSEA) * VDNL(IS,CSEA)
              END DO
              WRITE(740+IAPROC,*) 'min/max/sum(VSNL)=', minval(VSNL(:,CSEA)), maxval(VSNL(:,CSEA)), sum(VSNL(:,CSEA))
            WRITE(740+IAPROC,*) 'min/max/sum(DeltaNL)=', minval(DeltaSRC), maxval(DeltaSRC), sum(DeltaSRC)
            !
              DO IS=1,NSPEC
                DeltaSRC(IS) = VSDS(IS,CSEA) - SPEC(IS,JSEA) * VDDS(IS,CSEA)
              END DO
              WRITE(740+IAPROC,*) 'min/max/sum(VSDS)=', minval(VSDS(:,CSEA)), maxval(VSDS(:,CSEA)), sum(VSDS(:,CSEA))
              WRITE(740+IAPROC,*) 'min/max/sum(DeltaDS)=', minval(DeltaSRC), maxval(DeltaSRC), sum(DeltaSRC)
            !
            !            DO IS=1,NSPEC
            !              DeltaSRC(IS) = VSIC(IS) - SPEC(IS, JSEA)*VDIC(IS)
            !            END DO
              WRITE(740+IAPROC,*) 'min/max/sum(DeltaDS)=', minval(DeltaSRC), maxval(DeltaSRC), sum(DeltaSRC)
            END IF

            IF (.not. LSLOC) THEN
              ! REFACTOR - TODO: INLINE THESE - they are very small subroutines (one liners almost)
              IF (optionCall .eq. 1) THEN
                CALL SIGN_VSD_PATANKAR_WW3(SPEC(:,JSEA),VS(:,CSEA),VD(:,CSEA))
              ELSE IF (optionCall .eq. 2) THEN
                CALL SIGN_VSD_SEMI_IMPLICIT_WW3(SPEC(:,JSEA),VS(:,CSEA),VD(:,CSEA))
              ELSE IF (optionCall .eq. 3) THEN
                CALL SIGN_VSD_SEMI_IMPLICIT_WW3(SPEC(:,JSEA),VS(:,CSEA),VD(:,CSEA))
              ENDIF
              IF (.not. LSLOC) THEN ! Refactor notes: This test was originally in W3SRCE
                IF(PRESENT(VSIO)) VSIO(:,JSEA) = VS(:,CSEA)
                IF(PRESENT(VDIO)) VDIO(:,JSEA) = VD(:,CSEA)
              END IF
            ENDIF

#ifdef W3_DEBUGSRC
            IF (IX(CSEA) == DEBUG_NODE) THEN
              WRITE(740+IAPROC,*) '     srce_imp_pre : SHAVE = ', SHAVE
              WRITE(740+IAPROC,*) '     srce_imp_pre : DT=', DT(CSEA), ' HDT=', HDT, 'DTG=', DTG
              WRITE(740+IAPROC,*) '     srce_imp_pre : sum(SPEC)=', sum(SPEC, JSEA)
              WRITE(740+IAPROC,*) '     srce_imp_pre : sum(VSTOT)=', sum(VS(:,CSEA))
              WRITE(740+IAPROC,*) '     srce_imp_pre : sum(VDTOT)=', sum(MIN(0. ,VD(:,CSEA)))
            END IF

            IF (IX(CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSIN(:,CSEA))
            IF (IX(CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VDIN(:,CSEA))
            IF (IX(CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSDS(:,CSEA))
            IF (IX(CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VDDS(:,CSEA))
            IF (IX(CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSNL(:,CSEA))
            IF (IX(CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VDNL(:,CSEA))
            IF (IX(CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSLN(:,CSEA))
            IF (IX(CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSBT(:,CSEA))
            IF (IX(CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VS(:,CSEA))
            IF (IX(CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VD(:,CSEA))
#endif
            !!RETURN ! return everything is done for the implicit ...

            ! GPU Refactor - don't return here. CYCLE instead as we need to process rest
            ! of seapoints in tile. A check on "srce_imp_pre" is now made after integration
            ! loop is complete.
            CYCLE

          END IF ! srce_imp_pre
! --end W3_PDLIB
#endif
          !
#ifdef W3_T
          WRITE (NDST,9040) DTRAW, DT(CSEA), SHAVE
#endif
          !
          ! 5.  Increment spectrum --------------------------------------------- *
          !
          IF (srce_call .eq. srce_direct) THEN
            IF ( SHAVE ) THEN
              DO IS=IS1, NSPECH
                eInc1 = VS(IS,CSEA) * DT(CSEA) / MAX ( 1. , (1.-HDT*VD(IS,CSEA)))
                eInc2 = SIGN ( MIN (DAM(IS,CSEA),ABS(eInc1)) , eInc1 )
                SPEC(IS,JSEA) = MAX ( 0. , SPEC(IS,JSEA)+eInc2 )
              END DO
            ELSE
              !
              DO IS=IS1, NSPECH
                eInc1 = VS(IS,CSEA) * DT(CSEA) / MAX ( 1. , (1.-HDT*VD(IS,CSEA)))
                SPEC(IS,JSEA) = MAX ( 0. , SPEC(IS,JSEA)+eInc1 )
              END DO
            END IF
            !
#ifdef W3_DB1
            DO IS=IS1, NSPECH
              eInc1 = VSDB(IS,CSEA) * DT(CSEA) / MAX ( 1. , (1.-HDT*VDDB(IS,CSEA)))
              SPEC(IS,JSEA) = MAX ( 0. , SPEC(IS,JSEA)+eInc1 )
            END DO
#endif
#ifdef W3_TR1
            DO IS=IS1, NSPECH
              eInc1 = VDTR(IS,CSEA) * DT(CSEA) / MAX ( 1. , (1.-HDT*VDTR(IS,CSEA)))
              SPEC(IS,JSEA) = MAX ( 0. , SPEC(IS,JSEA)+eInc1 )
            END DO
#endif

#ifdef W3_DEBUGSRC
            IF ((CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSIN(:,CSEA))
            IF ((CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VDIN(:,CSEA))
            IF ((CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSDS(:,CSEA))
            IF ((CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VDDS(:,CSEA))
            IF ((CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSNL(:,CSEA))
            IF ((CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VDNL(:,CSEA))
            IF ((CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSLN(:,CSEA))
            IF ((CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSBT(:,CSEA))
            IF ((CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VS(:,CSEA))
            IF ((CSEA) == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VD(:,CSEA))
            IF ((CSEA) == DEBUG_NODE) THEN
              WRITE(740+IAPROC,*) '     srce_direct : SHAVE = ', SHAVE
              WRITE(740+IAPROC,*) '     srce_direct : DT=', DT(CSEA), ' HDT=', HDT, 'DTG=', DTG
              WRITE(740+IAPROC,*) '     srce_direct : sum(SPEC)=', sum(SPEC,JSEA)
              WRITE(740+IAPROC,*) '     srce_direct : sum(VSTOT)=', sum(VS(:,CSEA))
              WRITE(740+IAPROC,*) '     srce_direct : sum(VDTOT)=', sum(MIN(0. ,VD(:,CSEA)))
            END IF
#endif
          END IF ! if src_direct

        END DO ! CSEA/JSEA loop (from section 3) ! TODO: quite a big loop. Split?

        ! GPU Refactor: Everything done for implicit (pre) source call.
        if(srce_call .eq. srce_imp_pre) goto 7777

        !
        ! 5.b  Computes
        !              atmos->wave flux PHIAW-------------------------------- *
        !              wave ->BBL  flux PHIBBL------------------------------- *
        !              wave ->ice  flux PHICE ------------------------------- *
        !

        !! GPU Refactor - loop over chunk
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1

          ! HDT Calculation copied from section 3 (to avoid making HDT an array)
#ifdef W3_NL5
          IF (NL5_SELECT .EQ. 1) THEN
            HDT = NL5_OFFSET * DT(CSEA)
          ELSE
#endif
            HDT = OFFSET * DT(CSEA)
#ifdef W3_NL5
          ENDIF
#endif

          WCAP_BHS(JSEA) = 0.
          HSTOT=0.  
          DO IK=IKS1, NK
            FACTOR = DDEN(IK)/CG1_CHUNK(IK,CSEA)             !Jacobian to get energy in band
            FACTOR2= FACTOR*GRAV*WN1_CHUNK(IK,CSEA)/SIG(IK)  ! coefficient to get momentum

            ! Wave direction is "direction to"
            ! therefore there is a PLUS sign for the stress
            DO ITH=1, NTH
              IS = (IK-1)*NTH + ITH
!!              COSI(1)=ECOS(IS)
!!              COSI(2)=ESIN(IS)  ! [Refactor] - not used?
              PHIAW(JSEA) = PHIAW(JSEA) + (VSIN(IS,CSEA))* DT(CSEA) * FACTOR                    &
                   / MAX ( 1. , (1.-HDT*VDIN(IS,CSEA))) ! semi-implict integration scheme

              PHIBBL(JSEA)= PHIBBL(JSEA) - (VSBT(IS,CSEA))* DT(CSEA) * FACTOR                    &
                   / MAX ( 1. , (1.-HDT*VDBT(IS,CSEA))) ! semi-implict integration scheme

!             ! PHINL is calculated but never used; I have commented out (Chris Bunney):
!              PHINL = PHINL + VSNL(IS,CSEA)* DT(CSEA) * FACTOR   &
!                   / MAX ( 1. , (1.-HDT*VDNL(IS,CSEA))) ! semi-implict integration scheme
              IF (VSIN(IS,CSEA).GT.0.) WCAP_BHS(JSEA) = WCAP_BHS(JSEA) + SPEC(IS,JSEA) * FACTOR
              HSTOT = HSTOT + SPEC(IS,JSEA) * FACTOR
            END DO
          END DO
          WCAP_BHS(JSEA) = 4. * SQRT(WCAP_BHS(JSEA))
          HSTOT = 4.*SQRT(HSTOT)
          TAUWIX(JSEA) = TAUWIX(JSEA) + TAUWX(JSEA) * DRAT(CSEA) * DT(CSEA)
          TAUWIY(JSEA) = TAUWIY(JSEA) + TAUWY(JSEA) * DRAT(CSEA) * DT(CSEA)
          TAUWNX(JSEA) = TAUWNX(JSEA) + TAUWAX(CSEA) * DRAT(CSEA) * DT(CSEA)
          TAUWNY(JSEA) = TAUWNY(JSEA) + TAUWAY(CSEA) * DRAT(CSEA) * DT(CSEA)
          ! MISSING: TAIL TO BE ADDED ?
          !
        ENDDO ! CSEA

#ifdef W3_NLS
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1
          CALL W3SNLS ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), DEPTH(CSEA), U10_CHUNK(CSEA), DT(CSEA), AA=SPEC(:,JSEA) )
        END DO ! CSEA; W3SNLS
#endif
        !
        ! 6.  Add tail ------------------------------------------------------- *
        !   a Mean parameters
        !
        !
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1

#ifdef W3_ST0
          CALL W3SPR0 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), WNMEAN(JSEA), AMAX(CSEA))
#endif
#ifdef W3_ST1
          CALL W3SPR1 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), WNMEAN(JSEA), AMAX(CSEA))
#endif
#ifdef W3_ST2
          CALL INIT_GET_ISEA(ISEA, JSEA)  !! TODO - to keep FPI working
          CALL W3SPR2 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), DEPTH(CSEA), FPI(ISEA), U10_CHUNK(CSEA), UST_CHUNK(CSEA),   &
             EMEAN(CSEA), FMEAN(CSEA), WNMEAN(JSEA), AMAX(CSEA), ALPHA(:,JSEA), FP(CSEA) )
#endif
#ifdef W3_ST3
          CALL W3SPR3 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), FMEANS(CSEA),  &
             WNMEAN(JSEA), AMAX(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA), UST_CHUNK(CSEA), USTD_CHUNK(CSEA), &
             TAUWX(JSEA), TAUWY(JSEA), CD(CSEA), Z0(CSEA), CHARN(JSEA), LLWS(:,CSEA), FMEANWS(CSEA))
#endif
#ifdef W3_ST4
          CALL W3SPR4 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), FMEAN1(CSEA), WNMEAN(JSEA),&
             AMAX(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA),                          &
#ifdef W3_FLX5
             TAUA_CHUNK(CSEA), TAUADIR_CHUNK(CSEA), DAIR_CHUNK(CSEA),                     &
#endif
             UST_CHUNK(CSEA), USTD_CHUNK(CSEA),                                 &
             TAUWX(JSEA), TAUWY(JSEA), CD(CSEA), Z0(CSEA), CHARN(JSEA), LLWS(:,CSEA), FMEANWS(CSEA), DLWMEAN(CSEA))
#endif
#ifdef W3_ST6
          CALL W3SPR6 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), FMEAN(CSEA), WNMEAN(JSEA), AMAX(CSEA), FP(CSEA))
#endif
        END DO ! CSEA; W3SPRx

        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1
#ifdef W3_FLX2
          CALL W3FLX2 ( ZWND, DEPTH(CSEA), FP(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA),           &
             UST_CHUNK(CSEA), USTD_CHUNK(CSEA), Z0(CSEA), CD(CSEA) )
#endif
#ifdef W3_FLX3
          CALL W3FLX3 ( ZWND, DEPTH(CSEA), FP(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA),           &
             UST_CHUNK(CSEA), USTD_CHUNK(CSEA), Z0(CSEA), CD(CSEA) )
#endif
        END DO ! CSEA; W3FLXx
!        
!        
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1

#ifdef W3_ST1
          FH1 = FXFM * FMEAN(CSEA)
          FH2 = FXPM / UST_CHUNK(CSEA)   ! GPU refactor - had to recalculate FH2 here
          FHIGH(CSEA)  = MIN ( SIG(NK) , MAX ( FH1 , FH2 ) )
          NKH(CSEA) = MAX ( 2 , MIN ( NKH1(CSEA) ,                  &
               INT ( FACTI2 + FACTI1*LOG(MAX(1.E-7,FHIGH(CSEA))) ) ) )
          !
          IF ( FLTEST ) WRITE (NDST,9060)                           &
               FH1*TPIINV, FH2*TPIINV, FHIGH(CSEA)*TPIINV, NKH(CSEA)
#endif
          !
#ifdef W3_ST2
          CALL INIT_GET_ISEA(ISEA, JSEA)  !! TODO - to keep FPI working
          FHTRAN = XFT*FPI(ISEA)
          FHIGH(CSEA)  = XFC*FPI(ISEA)
          DFH = FHIGH(CSEA) - FHTRAN
          NKH(CSEA) = MAX ( 1 ,                                        &
               INT ( FACTI2 + FACTI1*LOG(MAX(1.E-7,FHTRAN)) ) )

          IF ( FLTEST ) WRITE (NDST,9061) FHTRAN, FHIGH(CSEA), NKH(CSEA)
#endif
          !
#ifdef W3_ST3
          FH1 = FFXFM * FMEAN(CSEA)
          FH2 = FFXPM / UST_CHUNK(CSEA)
          FHIGH(CSEA)  = MIN ( SIG(NK) , MAX ( FH1 , FH2 ) )
          NKH(CSEA) = MAX ( 2 , MIN ( NKH1(CSEA) ,                           &
               INT ( FACTI2 + FACTI1*LOG(MAX(1.E-7,FHIGH(CSEA))) ) ) )
          !
          IF ( FLTEST ) WRITE (NDST,9062)                           &
               FH1*TPIINV, FH2*TPIINV, FHIGH(CSEA)*TPIINV, NKH(CSEA)
#endif
          !
#ifdef W3_ST4
          ! Introduces a Long & Resio (JGR2007) type dependance on wave age
          FAGE(CSEA) = FFXFA*TANH(0.3*U10_CHUNK(CSEA)*FMEANWS(CSEA)*TPI/GRAV)
          FH1 = (FFXFM+FAGE(CSEA)) * FMEAN1(CSEA)
          FH2 = FFXPM / UST_CHUNK(CSEA)
          FHIGH(CSEA) = MIN ( SIG(NK) , MAX ( FH1 , FH2 ) )
          NKH(CSEA) = MAX ( 2 , MIN ( NKH1(CSEA),                       &
               INT ( FACTI2 + FACTI1*LOG(MAX(1.E-7,FHIGH(CSEA))) ) ) )
#endif
          !
#ifdef W3_ST6
          IF (FXFM .LE. 0) THEN
            FHIGH(CSEA) = SIG(NK)
          ELSE
            FHIGH(CSEA) = MIN ( SIG(NK), MAX(FXFM * FMEAN(CSEA), FXPM / UST_CHUNK(CSEA)) )
          ENDIF
          NKH(CSEA) = MAX ( 2 , MIN ( NKH1(CSEA) ,                      &
               INT ( FACTI2 + FACTI1*LOG(MAX(1.E-7,FHIGH(CSEA))) ) ) )
          !
          IF ( FLTEST ) WRITE (NDST,9063) FHIGH(CSEA)*TPIINV, NKH(CSEA)
#endif
          !
          ! 6.b Limiter for shallow water or Miche style criterion
          !     Last time step ONLY !
          !     uses true depth (D_INP) instead of limited depth
          !
#ifdef W3_MLIM
          IF ( DTTOT(CSEA) .GE. 0.9999*DTG ) THEN
            ! Refactor - need ISEA here for D_INP. Impact is likely small, so 
            ! calculate rather than have an extra _CHUNK variable.
            CALL INIT_GET_ISEA(ISEA, JSEA)  !!! SLOW!

            HM     = FHMAX *TANH(WNMEAN(JSEA)*MAX(0.,D_INP(ISEA))) / MAX(1.E-4,WNMEAN(JSEA) )
            EM     = HM * HM / 16.
            IF ( EMEAN(CSEA).GT.EM .AND. EMEAN(CSEA).GT.1.E-30 ) THEN
              SPEC(:,JSEA)   = SPEC(:,JSEA) / EMEAN(CSEA) * EM
              EMEAN(CSEA)  = EM
            END IF
          END IF
#endif
          !
          ! 6.c Seeding of spectrum
          !     alpha = 0.005 , 0.5 in eq., 0.25 for directional distribution
          !
#ifdef W3_SEED
          DO IK=MIN(NK,NKH(CSEA)), NK
            UC = FACSD * GRAV / SIG(IK)
            SLEV = MIN ( 1. , MAX ( 0. , U10_CHUNK(CSEA)/UC-1. ) ) *      &
                 6.25E-4 / WN1_CHUNK(IK,CSEA)**3 / SIG(IK)
            IF (INFLAGS2(4)) SLEV=SLEV*(1-ICE_CHUNK(CSEA))
            DO ITH=1, NTH
              SPEC(ITH+(IK-1)*NTH,JSEA) = MAX ( SPEC(ITH+(IK-1)*NTH,JSEA) ,  &
                   SLEV * MAX ( 0. , COS(U10D_CHUNK(CSEA)-TH(ITH)) )**2 )
            END DO
          END DO
#endif
          !
          ! 6.d Add tail
          !
          DO IK=NKH(CSEA)+1, NK
#ifdef W3_ST2
            FACDIA = MAX ( 0. , MIN ( 1., (SIG(IK)-FHTRAN)/DFH) )
            FACPAR = MAX ( 0. , 1.-FACDIA )
#endif
            DO ITH=1, NTH
              SPEC(ITH+(IK-1)*NTH,JSEA) = SPEC(ITH+(IK-2)*NTH,JSEA) * FACHFA         &
#ifdef W3_ST2
                   * FACDIA + FACPAR * SPEC(ITH+(IK-1)*NTH,JSEA)            &
#endif
                   + 0.
            END DO
          END DO
        END DO ! CSEA/JSEA (section 6.a)
        !
        ! 6.e  Update wave-supported stress----------------------------------- *
        !
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          JSEA = CHUNK0 + CSEA - 1

#ifdef W3_ST3
          CALL W3SIN3 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN2(:,CSEA), & 
              U10_CHUNK(CSEA), UST_CHUNK(CSEA), DRAT(CSEA), AS_CHUNK(CSEA),      &
              U10D_CHUNK(CSEA), Z0(CSEA), CD(CSEA), TAUWX(JSEA), TAUWY(JSEA), &
              TAUWAX(CSEA), TAUWAY(CSEA), &
              ICE_CHUNK(CSEA), VSIN(:,CSEA), VDIN(:,CSEA), LLWS(:,CSEA), IX(CSEA), IY(CSEA) )
#endif
#ifdef W3_ST4
        CALL W3SIN4 ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN2(:,CSEA), &
             U10_CHUNK(CSEA), UST_CHUNK(CSEA), DRAT(CSEA), AS_CHUNK(CSEA),      &
             U10D_CHUNK(CSEA), Z0(CSEA), CD(CSEA), TAUWX(JSEA), TAUWY(JSEA), &
             TAUWAX(CSEA), TAUWAY(CSEA), &
             VSIN(:,CSEA), VDIN(:,CSEA), LLWS(:,CSEA), IX(CSEA), IY(CSEA), BRLAMBDA(:,CSEA) )
        IF (SINTAILPAR(4).LT.0.5) THEN 
          CALL W3SPR4 (SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), &
              FMEAN(CSEA), FMEAN1(CSEA), WNMEAN(JSEA), AMAX(CSEA), U10_CHUNK(CSEA), U10D_CHUNK(CSEA), &
#ifdef W3_FLX5
              TAUA_CHUNK(CSEA), TAUADIR_CHUNK(CSEA), DAIR_CHUNK(CSEA), &
#endif
              UST_CHUNK(CSEA), USTD_CHUNK(CSEA), &
              TAUWX(JSEA), TAUWY(JSEA), CD(CSEA), Z0(CSEA), CHARN(JSEA), &
              LLWS(:,CSEA), FMEANWS(CSEA), DLWMEAN(CSEA))
        ENDIF
#endif

        END DO ! CSEA; W3SINx

        !
        ! 7.  Check if integration complete ---------------------------------- *
        !
        ! Update QI5TSTART (Q. Liu)
#ifdef W3_NL5
        DO CSEA=1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          CALL TICK21(QI5TSTART(:,CSEA), DT(CSEA))
        END DO
#endif

        IF (srce_call .eq. srce_imp_post) THEN
          DTDYN(CHUNK0:CHUNKN) = DTDYN(CHUNK0:CHUNKN) / REAL(MAX(1,NSTEPS))
          EXIT
        ENDIF

! Note: Leaving these ACC statements here for now - will be useful for GPU port.
!$ACC DATA COPYOUT(COMPLETE)
!$ACC PARALLEL
        ! GPU refactor: Update source mask with seapoints that have completed
        ! timestepping:
        !!WHERE(DTTOT(:NSEAC) .GE. 0.9999*DTG) SRC_MASK(:NSEAC) = .TRUE.
        DO CSEA=1,NSEAC
          IF(DTTOT(CSEA) .GE. 0.9999*DTG .AND. .NOT. SRC_MASK(CSEA)) THEN
            ! Time stepping complete. Set mask to true and calculate DTDYN
            SRC_MASK(CSEA) = .TRUE.
            JSEA = CHUNK0 + CSEA - 1
            DTDYN(JSEA) = DTDYN(JSEA) / REAL(MAX(1,NSTEPS))
          END IF
        END DO

        COMPLETE = ALL(SRC_MASK(:NSEAC)) ! GPU Refactor - store in scalar and return
!$ACC END PARALLEL
!$ACC END DATA

        ! Complete is true if all _active_ points have finished integration loop
        IF(COMPLETE) THEN
          EXIT
        ENDIF

      END DO ! INTEGRATION LOOP

      ! Refactor - if implicit (pre), then we are all done - cycle chunk loop
      IF(srce_call .eq. srce_imp_pre) THEN
        GOTO 7777
      ENDIF

#ifdef W3_DEBUGSRC
      IF (IX(CSEA) .eq. DEBUG_NODE) THEN
        WRITE(740+IAPROC,*) 'NSTEPS=', NSTEPS
        WRITE(740+IAPROC,*) '1 : sum(SPEC)=', sum(SPEC)
      END IF
      WRITE(740+IAPROC,*) 'DT=', DT(CSEA), 'DTG=', DTG
#endif
      !
      ! ... End point dynamic integration - - - - - - - - - - - - - - - - - -
      !
      ! 8.  Save integration data ------------------------------------------ *
      !
      ! TODO: Need a better solution than recalculating.
      !       Integer source mask? 0 = active, 1=masked(or complete), 2=masked+complete
      I = 1
      DO JSEA=CHUNK0,CHUNKN
        ! TODO - THIS BLOCK TEMPORARY - NEED BETTER SOLUTION THAN RECALCULATING SRC_MASK
        CALL INIT_GET_ISEA(ISEA, JSEA)  !!! SLOW!
        SRC_MASK(I) = .NOT. (MAPSTA(IY(I),IX(I)) .EQ. 1 .AND. FLAGST(ISEA))
        I = I + 1
      ENDDO ! END TEMPORARY SOLUTION FOR SRC_MASK

      DO CSEA=1,NSEAC
        JSEA = CHUNK0 + CSEA - 1
        IF(SRC_MASK(CSEA)) CYCLE
        !DTDYN(JSEA)  = DTDYN(JSEA) / REAL(MAX(1,NSTEPS))  ! Refactor: Moved to section 7
        FCUT(JSEA)   = FHIGH(CSEA) * TPIINV
      ENDDO 
      !
      GOTO 888
      !
      ! Error escape locations
      !
#ifdef W3_NNT
800 CONTINUE
      WRITE (NDSE,8000) FNAME, IERR
      CALL EXTCDE (1)
      !
801 CONTINUE
      WRITE (NDSE,8001) IERR
      CALL EXTCDE (2)
#endif
      !
888 CONTINUE
      !
      ! 9.a  Computes PHIOC------------------------------------------ *
      !     The wave to ocean flux is the difference between initial energy
      !     and final energy, plus wind input plus the SNL flux to high freq.,
      !     minus the energy lost to the bottom boundary layer (BBL)
      !
#ifdef W3_DEBUGSRC
      IF (IX(CSEA) .eq. DEBUG_NODE) THEN
        WRITE(740+IAPROC,*) '2 : sum(SPEC)=', sum(SPEC)
      END IF
#endif
      !! GPU Refactor - loop over chunk elements
      DO CSEA = 1,NSEAC
        IF(SRC_MASK(CSEA)) CYCLE
        JSEA = CHUNK0 + CSEA - 1

        EFINISH  = 0.
        MWXFINISH  = 0.
        MWYFINISH  = 0.
        DO IK=1, NK
          EBAND = 0.
          A1BAND = 0.
          B1BAND = 0.
          DO ITH=1, NTH
            DIFF = SPECINIT(ITH+(IK-1)*NTH,CSEA)-SPEC(ITH+(IK-1)*NTH,JSEA)
            EBAND = EBAND + DIFF
            A1BAND = A1BAND + DIFF*ECOS(ITH)
            B1BAND = B1BAND + DIFF*ESIN(ITH)
          END DO
          EFINISH  = EFINISH  + EBAND * DDEN(IK) / CG1_CHUNK(IK,CSEA)
          MWXFINISH  = MWXFINISH  + A1BAND * DDEN(IK) / CG1_CHUNK(IK,CSEA)        &
               * WN1_CHUNK(IK,CSEA)/SIG(IK)
          MWYFINISH  = MWYFINISH  + B1BAND * DDEN(IK) / CG1_CHUNK(IK,CSEA)        &
               * WN1_CHUNK(IK,CSEA)/SIG(IK)
        END DO
        !
        ! Transformation in momentum flux in m^2 / s^2
        !
        TAUOX(JSEA) = (GRAV*MWXFINISH+TAUWIX(JSEA)-TAUBBLX(JSEA))/DTG
        TAUOY(JSEA) = (GRAV*MWYFINISH+TAUWIY(JSEA)-TAUBBLY(JSEA))/DTG
        TAUWIX(JSEA) = TAUWIX(JSEA)/DTG
        TAUWIY(JSEA) = TAUWIY(JSEA)/DTG
        TAUWNX(JSEA) = TAUWNX(JSEA)/DTG
        TAUWNY(JSEA) = TAUWNY(JSEA)/DTG
        TAUBBLX(JSEA) = TAUBBLX(JSEA)/DTG
        TAUBBLY(JSEA) = TAUBBLY(JSEA)/DTG
        TAUOCX(JSEA)= DAIR_CHUNK(CSEA)*COEF_CHUNK(CSEA)*COEF_CHUNK(CSEA)*UST_CHUNK(CSEA)*UST_CHUNK(CSEA)*COS(USTD_CHUNK(CSEA)) + DWAT*(TAUOX(JSEA)-TAUWIX(JSEA))
        TAUOCY(JSEA)= DAIR_CHUNK(CSEA)*COEF_CHUNK(CSEA)*COEF_CHUNK(CSEA)*UST_CHUNK(CSEA)*UST_CHUNK(CSEA)*SIN(USTD_CHUNK(CSEA)) + DWAT*(TAUOY(JSEA)-TAUWIY(JSEA))
        !
        ! Transformation in wave energy flux in W/m^2=kg / s^3
        !
        PHIOC(JSEA) = DWAT * GRAV * (EFINISH + PHIAW(JSEA) - PHIBBL(JSEA)) / DTG
        PHIAW(JSEA) = DWAT * GRAV * PHIAW(JSEA) / DTG
        ! PHINL is calculated but never used; I have commented out (Chris Bunney):
        !PHINL = DWAT * GRAV * PHINL / DTG  ! GPU Refactor: NOT USED ANYWHERE. REMOVE?
        PHIBBL(JSEA) = DWAT * GRAV * PHIBBL(JSEA) / DTG
      END DO ! CSEA/JSEA
      !
      ! 10.1  Adds ice scattering and dissipation: implicit integration---------------- *
      !     INFLAGS2(4) is true if ice concentration was ever read during
      !             this simulation
      !
#ifdef W3_DEBUGSRC
      IF (IX(CSEA) .eq. DEBUG_NODE) THEN
        WRITE(740+IAPROC,*) '3 : sum(SPEC)=', sum(SPEC)
      END IF
#endif
      !! GPU Refactor - loop over chunk elements
      !IF ( INFLAGS2(4).AND.ICE.GT.0 ) THEN  ! GPU refactor: have split this expression
      ! TODO: This is a very big loop (CSEA) - probably needs refactoring
      !       into smaller loops
      IF( INFLAGS2(4) ) THEN
        DO CSEA = 1,NSEAC
          IF(SRC_MASK(CSEA)) CYCLE
          ! GPU Refactor: Zero TAUICE and PHIICE here; for B4B reproducibility. Chris Bunney.
          TAUICEX(JSEA) = 0.
          TAUICEY(JSEA) = 0.
          PHICE(JSEA) = 0.

          IF(ICE_CHUNK(CSEA) .EQ. 0) THEN
#ifdef W3_IS2
            IF(IS2PARS(10).LT.0.5) THEN
              ICEF_CHUNK(CSEA) = 0.
            ENDIF
#endif
            CYCLE
          ENDIF

          JSEA = CHUNK0 + CSEA - 1

          IF (IICEDISP) THEN
            ICECOEF2 = 1E-6
            CALL LIU_FORWARD_DISPERSION (ICEH_CHUNK(CSEA),ICECOEF2,DEPTH(CSEA), &
                 SIG,WN_R,CG_ICE,ALPHA_LIU)
            !
            IF (IICESMOOTH) THEN
#ifdef W3_IS2
              DO IK=1,NK
                SMOOTH_ICEDISP=0.
                IF (IS2PARS(14)*(TPI/WN_R(IK)).LT.ICEF_CHUNK(CSEA)) THEN ! IF ICE IS NOT TOO MUCH BROKEN
                  SMOOTH_ICEDISP=TANH((ICEF_CHUNK(CSEA)-IS2PARS(14)*(TPI/WN_R(IK)))/(ICEF_CHUNK(CSEA)*IS2PARS(13)))
                END IF
                WN_R(IK)=WN1_CHUNK(IK,CSEA)*(1-SMOOTH_ICEDISP)+WN_R(IK)*(SMOOTH_ICEDISP)
              END DO
#endif
            END IF
          ELSE
            WN_R=WN1_CHUNK(:,CSEA)
            CG_ICE=CG1_CHUNK(:,CSEA)
          END IF
          !
          R(:)=1 ! TODO - MOVE THIS OUTSIDE LOOP?? ! In case IC2 is defined but not IS2  !!TODO - needs to be a chunk variable (or does it?)
          !
#ifdef W3_IC1
          CALL W3SIC1 ( SPEC(:,JSEA),DEPTH(CSEA), CG1_CHUNK(:,CSEA), IX(CSEA), IY(CSEA), VSIC, VDIC )
#endif
#ifdef W3_IS2
          CALL W3SIS2 ( SPEC(:,JSEA), DEPTH(CSEA), ICE_CHUNK(CSEA), ICEH_CHUNK(CSEA), ICEF_CHUNK(CSEA), ICEDMAX_CHUNK(CSEA), IX(CSEA), IY(CSEA), &
               VSIR, VDIR, VDIR2, WN1_CHUNK(:,CSEA), CG1_CHUNK(:,CSEA), WN_R, CG_ICE, R )
#endif
#ifdef W3_IC2
          CALL W3SIC2 ( SPEC(:,JSEA), DEPTH(CSEA), ICEH_CHUNK(CSEA), ICEF_CHUNK(CSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA),&
               IX(CSEA), IY(CSEA), VSIC, VDIC, WN_R, CG_ICE, ALPHA_LIU, R)
#endif
#ifdef W3_IC3
          CALL W3SIC3 ( SPEC(:,JSEA), DEPTH(CSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), IX(CSEA), IY(CSEA), VSIC, VDIC )
#endif
#ifdef W3_IC4
          CALL W3SIC4 ( SPEC(:,JSEA), DEPTH(CSEA), CG1_CHUNK(:,CSEA), IX(CSEA), IY(CSEA), VSIC, VDIC )
#endif
#ifdef W3_IC5
          CALL W3SIC5 ( SPEC(:,JSEA), DEPTH(CSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), IX(CSEA), IY(CSEA), VSIC, VDIC )
#endif
          !
#ifdef W3_IS1
          CALL W3SIS1 ( SPEC(:,JSEA), ICE_CHUNK(CSEA), VSIR )
#endif
          SPEC2 = SPEC(:,JSEA)
          !
          !!TAUICEX(JSEA) = 0.   ! ChrisB: GPU Refactor: Now zeroed in outer seapoint loop for B4B reproducibility
          !!TAUICEY(JSEA) = 0.   !  Ditto...
          !!PHICE(JSEA) = 0.     !  Ditto...
          DO IK=1,NK
            IS = 1+(IK-1)*NTH
            !
            ! First part of ice term integration: dissipation part
            !
            ATT=1.
#if defined(W3_IC1) || defined(W3_IC2) || defined(W3_IC3) || defined(W3_IC4) || defined(W3_IC5)
            ATT=EXP(ICE_CHUNK(CSEA)*VDIC(IS)*DTG)
#endif
#ifdef W3_IS1
            ATT=ATT*EXP(ICE_CHUNK(CSEA)*VDIR(IS)*DTG)
#endif
#ifdef W3_IS2
            ATT=ATT*EXP(ICE_CHUNK(CSEA)*VDIR2(IS)*DTG)
            IF (IS2PARS(2).EQ.0) THEN ! Reminder : IS2PARS(2) = IS2BACKSCAT
              !
              ! If there is not re-distribution in directions the scattering is just an attenuation
              !
              ATT=ATT*EXP((ICE_CHUNK(CSEA)*VDIR(IS))*DTG)
            END IF
#endif
            SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH,JSEA) = ATT*SPEC2(1+(IK-1)*NTH:NTH+(IK-1)*NTH)
            !
            ! Second part of ice term integration: scattering including re-distribution in directions
            !
#ifdef W3_IS2
            IF (IS2PARS(2).GE.0) THEN
              IF (IS2PARS(20).GT.0.5) THEN
                !
                ! Case of isotropic back-scatter: the directional spectrum is decomposed into
                !               - an isotropic part (ISO): eigenvalue of scattering is 0
                !               - the rest     (SPEC-ISO): eigenvalue of scattering is VDIR(IS)
                !
                SCAT = EXP(VDIR(IS)*IS2PARS(2)*DTG)
                ISO = SUM(SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH, JSEA))/NTH
                SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH, JSEA) = ISO &
                     +(SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH, JSEA)-ISO)*SCAT
              ELSE
                !
                ! General solution with matrix exponentials: same as bottom scattering, see Ardhuin & Herbers (JFM 2002)
                !
                SCATSPEC(1:NTH)=DBLE(SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH, JSEA))
                SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH, JSEA) =  &
                     REAL(MATMUL(IS2EIGVEC(:,:), EXP(IS2EIGVAL(:)*VDIR(IS)*DTG*IS2PARS(2)) &
                     *MATMUL(TRANSPOSE(IS2EIGVEC(:,:)),SCATSPEC)))
              END IF
            END IF
#endif
            !
            ! 10.2  Fluxes of energy and momentum due to ice effects
            !
            FACTOR = DDEN(IK)/CG1_CHUNK(IK,CSEA)                    !Jacobian to get energy in band
            FACTOR2= FACTOR*GRAV*WN1_CHUNK(IK,CSEA)/SIG(IK)         ! coefficient to get momentum
            DO ITH = 1,NTH
              IS = ITH+(IK-1)*NTH
              PHICE(JSEA) = PHICE(JSEA) + (SPEC(IS,JSEA)-SPEC2(IS)) * FACTOR
              !COSI(1)=ECOS(IS)
              !COSI(2)=ESIN(IS)  ! GPU Refactor : Not needed after TAUICE -> TAUICE[XY]
              TAUICEX(JSEA) = TAUICEX(JSEA) - (SPEC(IS,JSEA)-SPEC2(IS))*FACTOR2*ECOS(IS)
              TAUICEY(JSEA) = TAUICEY(JSEA) - (SPEC(IS,JSEA)-SPEC2(IS))*FACTOR2*ESIN(IS)
            END DO
          END DO
          PHICE(JSEA) = -1. * DWAT * GRAV * PHICE(JSEA) / DTG
          TAUICEX(JSEA) = TAUICEX(JSEA) / DTG
          TAUICEY(JSEA) = TAUICEY(JSEA) / DTG
        ENDDO ! CSEA/JSEA
      ELSE ! INPARS(4)
#ifdef W3_IS2
        IF (IS2PARS(10).LT.0.5) THEN
          ICEF_CHUNK(CSEA) = 0.
        ENDIF
#endif
      ENDIF ! INPARS(4)
      !
      !
      ! - - - - - - - - - - - - - - - - - - - - - -
      ! 11. Sea state dependent stress routine calls
      ! - - - - - - - - - - - - - - - - - - - - - -
      !Note the Sea-state dependent stress calculations are primarily for high-wind
      !conditions (>10 m/s).  It is not recommended to use these at lower wind
      !in their current state.
      !
      DO CSEA=1,NSEAC
        IF(SRC_MASK(CSEA)) CYCLE
        JSEA = CHUNK0 + CSEA - 1

#ifdef W3_DEBUGSRC
        IF (IX(CSEA) .eq. DEBUG_NODE) THEN
          WRITE(740+IAPROC,*) '4 : sum(SPEC)=', sum(SPEC(:,JSEA))
        END IF
#endif

          ! FLD1/2 requires the calculation of FPI:
#if defined (W3_FLD1) || defined(W3_FLD2)
        CALL INIT_GET_ISEA(ISEA, JSEA)  !! TODO - to keep FPI working
        CALL CALC_FPI(SPEC(:,JSEA), CG1_CHUNK(:,CSEA), FPI(ISEA), VSIN(:,CSEA) ) ! TODO - probably want to pass array of spec in future
#endif      
      !
#ifdef W3_FLD1
        IF (U10_CHUNK(CSEA).GT.10. .and. HSTOT.gt.0.5) then
          ! TODO - TAUNUX/Y not used. Remove?
          CALL W3FLD1 ( SPEC(:,JSEA),min(FPI(ISEA)/TPI,2.0),COEF_CHUNK(CSEA)*U10_CHUNK(CSEA)*COS(U10D_CHUNK(CSEA)),        &
             COEF_CHUNK(CSEA)*U10_CHUNK(CSEA)*Sin(U10D_CHUNK(CSEA)), ZWND, DEPTH(CSEA), 0.0, &
             DAIR_CHUNK(CSEA), UST_CHUNK(CSEA), USTD_CHUNK(CSEA), Z0(CSEA),TAUNUX,TAUNUY,CHARN(JSEA))
        ELSE
          CHARN(JSEA) = AALPHA
        ENDIF
#endif
#ifdef W3_FLD2
        IF (U10_CHUNK(CSEA).GT.10. .and. HSTOT.gt.0.5) then
          ! TODO - TAUNUX/Y not used. Remove?
          CALL W3FLD2 ( SPEC(:,JSEA),min(FPI(ISEA)/TPI,2.0),COEF_CHUNK(CSEA)*U10_CHUNK(CSEA)*COS(U10D_CHUNK(CSEA)),        &
             COEF_CHUNK(CSEA)*U10_CHUNK(CSEA)*Sin(U10D_CHUNK(CSEA)), ZWND, DEPTH(CSEA), 0.0, &
             DAIR_CHUNK(CSEA), UST_CHUNK(CSEA), USTD_CHUNK(CSEA), Z0(CSEA),TAUNUX,TAUNUY,CHARN(JSEA))
        ELSE
          CHARN(JSEA) = AALPHA
        ENDIF
#endif
      END DO ! CSEA
      !
      ! 12. includes shoreline reflection --------------------------------------------- *
      !
      DO CSEA=1,NSEAC
        IF(SRC_MASK(CSEA)) CYCLE
        JSEA = CHUNK0 + CSEA - 1

#ifdef W3_DEBUGSRC
        IF (IX(CSEA) .eq. DEBUG_NODE) THEN
          WRITE(740+IAPROC,*) '5 : sum(SPEC)=', sum(SPEC(:,JSEA))
        END IF
#endif

#ifdef W3_REF1
        ! NOTE : `REFLEC_CHUNK(4,CSEA) * BERG_CHUNK(CSEA)` calculation moved here from W3SRCE
        IF (REFLEC_CHUNK(1,CSEA) .GT. 0 .OR.  &
            REFLEC_CHUNK(2,CSEA) .GT. 0 .OR.  &
            (BERG_CHUNK(CSEA) .GT. 0 .AND. REFLEC_CHUNK(4,CSEA) * BERG_CHUNK(CSEA) .GT. 0) &
          ) THEN
          CALL W3SREF ( SPEC(:,JSEA), CG1_CHUNK(:,CSEA), WN1_CHUNK(:,CSEA), EMEAN(CSEA), &
             FMEAN(CSEA), DEPTH(CSEA), CX_CHUNK(CSEA), CY_CHUNK(CSEA),   &
             REFLEC_CHUNK(:,CSEA), REFLED_CHUNK(:,CSEA), TRNX_CHUNK(CSEA), TRNY_CHUNK(CSEA),  &
             BERG_CHUNK(CSEA), DTG, IX(CSEA), IY(CSEA), JSEA, VREF(:,CSEA) )
          IF (GTYPE.EQ.UNGTYPE.AND.REFPARS(3).LT.0.5) THEN
#ifdef W3_PDLIB
            IF (IOBP_LOC(JSEA).EQ.0) THEN
#else
            IF (IOBP(IX(CSEA)).EQ.0) THEN
#endif
              DO IK=1, NK
                DO ITH=1, NTH
                  ISP = ITH+(IK-1)*NTH
#ifdef W3_PDLIB
                  IF (IOBPD_LOC(ITH,JSEA).EQ.0) SPEC(ISP,JSEA) = DTG*VREF(ISP,CSEA)
#else
                  IF (IOBPD(ITH,IX(CSEA)).EQ.0) SPEC(ISP,JSEA) = DTG*VREF(ISP,CSEA)
#endif
                END DO
              END DO
            ELSE
              SPEC(:,JSEA) = SPEC(:,JSEA) + DTG * VREF(:,CSEA)
            ENDIF
          ELSE
            SPEC(:,JSEA) = SPEC(:,JSEA) + DTG * VREF(:,CSEA)
          END IF
        END IF
#endif

      !
#ifdef W3_DEBUGSRC
        IF (IX(CSEA) .eq. DEBUG_NODE) THEN
          WRITE(740+IAPROC,*) '6 : sum(SPEC)=', sum(SPEC,JSEA)
        END IF
#endif
      END DO ! CSEA

      !FIRST  = .FALSE.  ! Refactor: Never used.

      IF(IT.EQ.0) SPEC(:,CHUNK0:CHUNKN) = SPECINIT(:,:NSEAC)

      SPEC(:,CHUNK0:CHUNKN) = MAX(0., SPEC(:,CHUNK0:CHUNKN))
      !
7777  CONTINUE  ! GPU Refactor: Landing point for srce_imp_pre 
      !
      ! NEW GPU Refactored code:
      ! Write temporary local grid CHUNKED arrays back to full grid
      ! (INOUT/OUT variables only)::
      DO CSEA=1,NSEAC
        JSEA = CHUNK0 + CSEA - 1
        CALL INIT_GET_ISEA(ISEA, JSEA)

        ! Set output values in full grid
        USTAR(ISEA) = UST_CHUNK(CSEA)
        USTDIR(ISEA) = USTD_CHUNK(CSEA)
#ifdef W3_IS2
        ! Only copy ICEF back if ice field read in (INFLAGS(4) is TRUE):
        IF(INFLAGS2(4)) THEN
          ICEF(ISEA) = ICEF_CHUNK(CSEA)
        ENDIF
#endif

        ! This moved from W3WAVE (after W3SRCE call)
        ! TODO - check this is ok for implicit calls too...
        IF(.NOT. (MAPSTA(IY(CSEA),IX(CSEA)) .EQ. 1 .AND. FLAGST(ISEA))) THEN
          USTAR (ISEA) = UNDEF
          USTDIR(ISEA) = UNDEF
          DTDYN (JSEA) = UNDEF
          FCUT  (JSEA) = UNDEF
          !SPEC(:,JSEA)  = 0.  !! Do not zero spec if point not active.
        ENDIF
      ENDDO ! CSEA
    END DO !! CHUNK LOOP (CHUNK0,CHUNKN)
    RETURN
    !
    ! Formats
    !
#ifdef W3_NNT
8000 FORMAT (/' *** ERROR W3SRCE : ERROR IN OPENING FILE ',A,' ***'/ &
         '                    IOSTAT = ',I10/)
8001 FORMAT (/' *** ERROR W3SRCE : ERROR IN WRITING TO FILE ***'/    &
         '                    IOSTAT = ',I10/)
#endif
    !
#ifdef W3_T
9000 FORMAT (' TEST W3SRCE : COUNTERS   : NO LONGER AVAILABLE')
9001 FORMAT (' TEST W3SRCE : DEPTH      :',F8.1/                     &
         '               WIND SPEED :',F8.1/                     &
         '               WIND DIR   :',F8.1)
#endif
#ifdef W3_ST1
9004 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
         ' ------------- NEW DYNAMIC INTEGRATION LOOP',          &
         ' ------------- ')
#endif
#ifdef W3_ST2
9005 FORMAT (' TEST W3SRCE : FHIGH      : ',F8.4/                    &
         ' ------------- NEW DYNAMIC INTEGRATION LOOP',          &
         ' ------------- ')
#endif
#ifdef W3_ST3
9006 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
         ' ------------- NEW DYNAMIC INTEGRATION LOOP',          &
         ' ------------- ')
#endif
#ifdef W3_ST4
9006 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
         ' ------------- NEW DYNAMIC INTEGRATION LOOP',          &
         ' ------------- ')
#endif
    !
#ifdef W3_T
9020 FORMAT (' TEST W3SRCE : NSTEP : ',I4,'    DTTOT :',F6.1)
9021 FORMAT (' TEST W3SRCE : NKH (3X)   : ',2I3,I6)
9040 FORMAT (' TEST W3SRCE : DTRAW, DT, SHAVE :',2F6.1,2X,L1)
#endif
    !
#ifdef W3_ST1
9060 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
         '               NKH        : ',I3)
#endif
#ifdef W3_ST2
9061 FORMAT (' TEST W3SRCE : FHIGH (2X) : ',2F8.4/                   &
         '               NKH        : ',I3)
#endif
#ifdef W3_ST3
9062 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
         '               NKH        : ',I3)
#endif
#ifdef W3_ST4
9062 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
         '               NKH        : ',I3)
#endif
#ifdef W3_ST6
9063 FORMAT (' TEST W3SRCE : FHIGH      : ',F8.4/                    &
         '               NKH        : ',I3)
#endif
    !/
    !/ End of W3SRCE ----------------------------------------------------- /
    !/
  END SUBROUTINE W3SRCE
  !/ ------------------------------------------------------------------- /

  !>
  !> @brief Calculate equivalent peak frequency.
  !>
  !> @details Tolman and Chalikov (1996), equivalent peak frequency from source.
  !>
  !> @param[in]  A    Action density spectrum (1-D).
  !> @param[in]  CG   Group velocities for k-axis of spectrum.
  !> @param[out]  FPI  Input 'peak' frequency.
  !> @param[in] S    Source term (1-D version).
  !>
  !> @author Jessica Meixner
  !> @date   06-Jun-2018
  !>
  SUBROUTINE CALC_FPI( A, CG, FPI, S )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |          Jessica Meixner          |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         06-Jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/    06-Jul-2016 : Origination                         ( version 5.12 )
    !/    06-Jul-2016 : Add SUBROUTINE SIGN_VSD_SEMI_IMPLICIT_WW3
    !/                  Add optional DEBUGSRC/PDLIB           ( version 6.04 )
    !/
    !  1. Purpose :
    !
    !     Calculate equivalent peak frequency
    !
    !  2. Method :
    !
    !     Tolman and Chalikov (1996), equivalent peak frequency from source

    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.  I   Action density spectrum (1-D).
    !       CG      R.A.  I   Group velocities for k-axis of spectrum.
    !       FPI     R.A.  O   Input 'peak' frequency.
    !       S       R.A.  I   Source term (1-D version).
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
    !      W3SRCE Subr.
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
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, XFR, DDEN, SIG,FTE, FTTR
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)        :: A(NSPEC), CG(NK), S(NSPEC)
    REAL, INTENT(OUT)       :: FPI
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IS, IK
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    ::  M0, M1, SIN1A(NK)
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'CALC_FPI')
#endif
    !
    !     Calculate FPI: equivalent peak frequncy from wind source term
    !     input
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

  END SUBROUTINE CALC_FPI
  !/ ------------------------------------------------------------------- /!

  !>
  !> @brief Put source term in matrix same as done always.
  !>
  !> @param[in]    SPEC
  !> @param[inout] VS
  !> @param[inout] VD
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE SIGN_VSD_SEMI_IMPLICIT_WW3(SPEC, VS, VD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Put source term in matrix same as done always
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
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
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
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
    USE W3GDATMD, only : NTH, NK, NSPEC
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local PARAMETERs
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/

    INTEGER             :: ISP, ITH, IK, IS
    REAL, INTENT(IN)    :: SPEC(NSPEC)
    REAL, INTENT(INOUT) :: VS(NSPEC), VD(NSPEC)
#ifdef W3_S
    CALL STRACE (IENT, 'SIGN_VSD_SEMI_IMPLICIT_WW3')
#endif
    DO IS=1,NSPEC
      VD(IS) = MIN(0., VD(IS))
    END DO
  END SUBROUTINE SIGN_VSD_SEMI_IMPLICIT_WW3
  !/ ------------------------------------------------------------------- /

  !>
  !> @brief Put source term in matrix Patankar style (experimental).
  !>
  !> @param[in]    SPEC
  !> @param[inout] VS
  !> @param[inout] VD
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE SIGN_VSD_PATANKAR_WW3(SPEC, VS, VD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Put source term in matrix Patankar style (experimental)
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
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
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
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

    USE W3GDATMD, only : NTH, NK, NSPEC
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local PARAMETERs
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
    INTEGER             :: ISP, ITH, IK, IS
    REAL, INTENT(IN)    :: SPEC(NSPEC)
    REAL, INTENT(INOUT) :: VS(NSPEC), VD(NSPEC)
#ifdef W3_S
    CALL STRACE (IENT, 'SIGN_VSD_PATANKAR_WW3')
#endif
    DO IS=1,NSPEC
      VD(IS) = MIN(0., VD(IS))
      VS(IS) = MAX(0., VS(IS))
    END DO
  END SUBROUTINE SIGN_VSD_PATANKAR_WW3
  !/
  !/ End of module W3SRCEMD -------------------------------------------- /
  !/
END MODULE W3SRCEMD
