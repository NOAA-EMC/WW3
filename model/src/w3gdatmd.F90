#include "w3macros.h"
!/
!/ ------------------------------------------------------------------- /
!/ Macros for enabling test output
!/
#define TEST_W3GDATMD___disabled
#define TEST_W3GDATMD_W3NMOD___disabled
#define TEST_W3GDATMD_W3DIMX___disabled
#define TEST_W3GDATMD_W3DIMS___disabled
#define TEST_W3GDATMD_W3SETG___disabled
#define TEST_W3GDATMD_W3GNTX___disabled
#define TEST_W3GDATMD_W3DIMUG___disabled
#define TEST_W3GDATMD_W3SETREF___disabled
!/
!/ ------------------------------------------------------------------- /
      MODULE W3GDATMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  !           J. H. Alves             !
!/                  |            F. Ardhuin             |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         15-Apr-2020 |
!/                  +-----------------------------------+
!/
!/    24-Jun-2005 : Origination.                        ( version 3.07 )
!/    09-Nov-2005 : Remove soft boundary options.       ( version 3.08 )
!/    23-Jun-2006 : Add data for W3SLN1.                ( version 3.09 )
!/    18-Jul-2006 : Add input grids.                    ( version 3.10 )
!/    05-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
!/    02-Feb-2007 : Add FLAGST.                         ( version 3.10 )
!/    14-Apr-2007 : Add Miche style limiter.            ( version 3.11 )
!/                  ( J. H. Alves )
!/    25-Apr-2007 : Adding Battjes-Janssen Sdb.         ( version 3.11 )
!/                  ( J. H. Alves )
!/    06-Aug-2007 : Fixing SLNP !/SEED bug.             ( version 3.13 )
!/    18-Sep-2007 : Adding WAM4 source terms.           ( version 3.13 )
!/                  ( F. Ardhuin )
!/    15-Apr-2008 : Clean up for distribution.          ( version 3.14 )
!/    27-Jun-2008 : Expand WAM4 variants namelist       ( version 3.14 )
!/                  ( F. Ardhuin )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    29-Oct-2010 : Implement unstructured grids        ( version 3.14.1 )
!/                  (A. Roland and F. Ardhuin) 
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    23-Dec-2010 : Fix HPFAC and HQFAC by including the COS(YGRD)
!/                  factor with DXDP and DXDQ terms.    ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    05-Apr-2011 : Implement interations for DTMAX < 1s( version 3.14.1 )
!/                  (F. Ardhuin) 
!/    01-Jul-2011 : Movable bed bottom friction BT4     ( version 4.01 )
!/    03-Nov-2011 : Bug fix: GUGINIT initialization     ( version 4.04 )
!/    29-Nov-2011 : Adding ST6 source term option.      ( version 4.04 )
!/                  (S. Zieger)
!/    14-Mar-2012 : Add PSIC for BT4                    ( version 4.04 )
!/    12-Jun-2012 : Add /RTD option or rotated grid variables. 
!/                  (Jian-Guo Li)                       ( version 4.06 )
!/    13-Jul-2012 : Move data structures GMD (SNL3) and nonlinear
!/                  filter (SNLS) from 3.15 (HLT).      ( version 4.08 )
!/    03-Sep-2012 : Clean up of UG grids                ( version 4.08 )
!/    12-Dec-2012 : Adding SMC grid.  JG_Li             ( version 4.09 )
!/    16-Sep-2013 : Add Arctic part SMC grid.           ( version 4.11 )
!/    11-Nov-2013 : SMC and rotated grid incorporated in the main 
!/                  trunk                               ( version 4.13 )
!/    16-Nov-2013 : Allows reflection on curvi grids    ( version 4.14 )
!/    26-Jul-2013 : Adding IG waves                     ( version 4.16 )
!/    18-Dec-2013 : Moving FLAGLL into GRID TYPE        ( version 4.16 )
!/    11-Jun-2014 : Changed reflection for subgrid      ( version 5.01 )
!/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
!/    21-Aug-2015 : Add SMC FUNO3, FVERG options. JGLi  ( version 5.09 )
!/    04-May-2016 : Add IICEDISP                  GB&FA ( version 5.10 )
!/    20-Jan-2017 : Update to new W3GSRUMD APIs         ( version 6.02 )
!/    20-Jan-2017 : Change to preprocessor macros to enable test output.
!/                  (T.J. Campbell, NRL)                ( version 6.02 )
!/    20-Jan-2017 : Change calculation of curvilinear grid metric and
!/                  derivatives calculations to use W3GSRUMD:W3CGDM.
!/                  (T.J. Campbell, NRL)                ( version 6.02 )
!/    07-Jan-2018 : Generalizes ICE100WIND to ICESCALES ( version 6.04 )
!/    26-Mar-2018 : Add FSWND optional variable.  JGLi  ( version 6.02 )
!/    05-Jun-2018 : Add PDLIB/DEBUGINIT and implcit scheme parameters
!/                  for unstructured grids              ( version 6.04 ) 
!/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version 6.06 )
!/    20-Aug-2018:  Extra namelist variables for ST6    ( version 6.06)
!/                  (Q. Liu, UoM)
!/    26-Aug-2018 : UOST (Mentaschi et al. 2015, 2018)  ( version 6.06 )
!/    27-Aug-2018 : Add BTBETA parameter                ( version 6.06 )
!/    22-Feb-2020 : Add AIRGB and AIRCMIN               ( version 7.06 )
!/    15-Apr-2020 : Adds optional opt-out for CFL on BC ( version 7.08 )
!/    06-May-2021 : Add SMCTYPE, ARCTC options.   JGLi  ( version 7.12 )
!/    07-Jun-2021 : the GKE module (NL5, Q. Liu)        ( version 7.12 )
!/
!/
!/    Copyright 2009-2013 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Define data structures to set up wave model grids and aliases 
!     to use individual grids transparently. Also includes subroutines
!     to manage data structure and pointing to individual models.
!     Definition of grids and model set up.
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      NGRIDS    Int.  Public   Number of grids, initialized at -1 
!                               to check proper model initialization.
!      NAUXGR    Int.  Public   Auxiliary grids.
!      IGRID     Int.  Public   Selected spatial grid, init. at -1.
!      ISGRD     Int.  Public   Selected spectral grid, init. at -1.
!      IPARS     Int.  Public   Selected num. and ph. pars, init. at -1.
!      RLGTYPE   I.P.  Public   Named constant for rectilinear grid type
!      CLGTYPE   I.P.  Public   Named constant for curvilinear grid type
!      UNGTYPE   I.P.  Public   Named constant for Unstructured triangular grid
!      SMCTYPE   I.P.  Public   Named constant for unstructured SMC grid type
!      FLAGLL    Log.  Public   Flag to indicate coordinate system for all grids
!                               .TRUE.: Spherical (lon/lat in degrees)
!                               .FALSE.: Cartesian (meters)
!      GRID      TYPE  Public   Data structure defining grid.
!      GRIDS     GRID  Public   Array of grids.
!      SGRD      TYPE  Public   Data structure defining spectral grid.
!      SGRDS     GRID  Public   Array of spectral grids.
!      MPAR      TYPE  Public   Data structure with all other model
!                               parameters.
!      MPARS     GRID  Public   Array of MPAR.
!     ----------------------------------------------------------------
!
!     All elements of GRID are aliased to pointers with the same
!     name. These pointers are defined as :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      GTYPE     Int.  Public   Flag for type of grid
!                               RLGTYPE: Rectilinear grid
!                               CLGTYPE: Curvilinear grid
!                               UNGTYPE: Unstructured triangular grid                       
!                               SMCTYPE: Unstructured SMC grid
!      RSTYPE    Int.  Public   Integer identifyng restart type
!      ICLOSE    Int.  Public   Parameter indicating type of index closure of grid.
!                               ICLOSE_NONE: No grid closure
!                               ICLOSE_SMPL: Simple grid closure
!                                 Grid is periodic in the i-index and wraps at
!                                 I=NX+1. In other words, (NX+1,J) => (1,J).
!                               ICLOSE_TRPL: Tripole grid closure
!                                 Grid is periodic in the i-index and and wraps at
!                                 I=NX+1 and has closure at J=NY+1. In other words,
!                                 (NX+1,J<=NY) => (1,J) and
!                                 (I,NY+1) => (MOD(NX-I+1,NX)+1,NY). The tripole
!                                 closure requires that NX be even.
!      NX, NY    Int.  Public   Discrete dimensions of spatial grid.
!      NSEA(L)   Int.  Public   Number of sea points (local for MPP).
!      NU/VFc    Int.  Public   Number of U/V faces for SMC grid. 
!      NRLv      Int.  Public   Number of refined levels for SMC grid. 
!      NGLO      Int.  Public   Number of cells in global part for SMC grid. 
!      NARC      Int.  Public   Number of cells in Arctic part for SMC grid. 
!      NBAC      Int.  Public   Number of boundary cells in Arctic part.
!      NBGL      Int.  Public   Number of boundary cells in global part.
!      NBSMC     Int.  Public   Number of boundary cells for regional SMC grid.
!      TRFLAG    Int.  Public   Flag for use of transparencies
!                                0: No sub-grid obstacles.
!                                1: Obstructions at cell boundaries.
!                                2: Obstructions at cell centers.
!                                3: Like 1 with continuous ice.
!                                4: Like 2 with continuous ice.
!      MAPSTA    I.A.  Public   Grid status map.
!      MAPST2    I.A.  Public   Second grid status map.
!      MAPxx     I.A.  Public   Storage grid maps.
!      IJKCel    I.A.  Public   Cell info array for SMC grid.
!      IJKU/VFc  I.A.  Public   U/V-Face arrays for SMC grid.
!      NLv*      I.A.  Public   Cell, U/V-Face numbers of refine levels.
!      ICLBAC    I.A.  Public   Mapping index for Arctic boundary cells.
!      ISMCBP    I.A.  Public   List of SMC grid input boundary cell indexes.
!      SX,SY     Real  Public   Spatial (rectilinear) grid increments.
!      X0,Y0     Real  Public   Lower left corner of spatial (rectilinear) grid.
!      DTCFL     Real  Public   Maximum CFL time step X-Y propagation.
!      DTCFLI    Real  Public   Id. intra-spectral.
!      DTMAX     Real  Public   Maximum overall time step.
!      DTMIN     Real  Public   Minimum dynamic time step for source
!      NITERSEC1 Real  Public   Number of interations when DTMAX < 1s
!      DMIN      Real  Public   Minimum water depth.
!      CTMAX     Real  Public   Maximum CFL number for depth refr.
!      FICE0/N   Real  Public   Cut-off ice conc. for ice coverage.
!      FICEL     Real  Public   Length scale for sea ice damping
!      IICEHMIN  Real  Public   Minimum thickness of sea ice
!      IICEHDISP Real  Public   Minimum thickness of sea ice in the dispersion relation before relaxing the conv. criterion
!      IICEHFAC  Real  Public   Scale factor for sea ice thickness
!      IICEHINIT Real  Public   Initial value of ice thickness
!      ICESCALES R.A.  Publ.    Scaling coefficient for source terms in the presence of ice
!                               Default is 1.0, meaning that 100% ice 
!                               concentration result in zero source term
!                               If set to 0.0, then ice has no direct impact on Sln / Sin / Snl / Sds
!      IC3PARS   R.A.  Public   various parameters for use in IC4, handled as
!                               an array for simplicity
!      IC4_KI    R.A.  Public   KI (dissipation rate) values for use in IC4
!      IC4_FC    R.A.  Public   FC (frequency bin separators) for use in IC4
!      PFMOVE    Real  Public   Tunable parameter in GSE correction
!                               for moving grids.
!      GRIDSHIFT Real  Public   Grid offset for multi-grid w/SCRIP
!      CMPRTRCK  Log.  Public   True for traditional compression of track output
!      PoLat/Lon R.A.  Public   Rotated N-Pole standard latitude/longitude. 
!      AnglD     R.A.  Public   Rotation angle in degree to turn rotated grid
!                               back to standard grid.  JGLi12Jun2012
!      FLAGUNR   Log.  Public   True if rotating directions back to true north 
!      STEXU     Real  Public   Length-scale (X) for space-time extreme averaging
!      STEYU     Real  Public   Length-scale (Y) for space-time extreme averaging
!      STEDU     Real  Public   Time-scale for space-time extreme averaging
!      ZB        R.A.  Public   Bottom levels on storage grid.
!      CLATS(I)  R.A.  Public   (Inverse) cosine of latitude at sea points.
!      CTHG0S    R.A.  Public   Constant in great-circle refr. term at sea points.
!      TRNX/Y    R.A.  Public   Transparencies in X/Y for sub-grid
!      CTRNX/Y   R.A.  Public   Sub-grid transparencies for SMC grid.
!      ANGARC    R.A.  Public   Rotation angle in degree for Arctic cells. 
!      SPCBAC    R.A.  Public   Full 2-D spectra for Arctic boundary cells. 
!      X/YGRD    R.A.  Public   Spatial grid coordinate arrays.
!      SX/SYGRD  R.A.  Public   Spatial grid increment arrays.
!      GINIT     Log.  Public   Flag identifying grid initialization.
!      FLDRY     Log.  Public   Flag for 'dry' run (IO and data
!                               processing only).
!      FLCx      Log.  Public   Flags for prop. is different spaces.
!      FLSOU     Log.  Public   Flag for source term calculation.
!      FUNO3     Log.  Public   Flag for 3rd order UNO3 scheme on SMC grid. 
!      FVERG     Log.  Public   Flag for 1-2-1 averaging smoothing on SMC grid.
!      FSWND     Log.  Public   Flag for sea-point only wind input on SMC grid.
!      ARCTC     Log.  Public   Flag to include Arctic polar part on SMC grid.
!      FLAGST    L.A.  Public   Flag for source term computations 
!                               for individual grid points.
!      IICEDISP   Log.  Public   Flag for use of the ice covered dispertion relation.
!      IICESMOOTH Log.  Public   Flag to smooth the ice covered dispertion relation in broken ice.
!       
!
!      GNAME     C*30  Public   Grid name.
!      FILEXT    C*13  Public   Extension of WAVEWATCH III file names
!                               default in 'ww3'.
!      BTBETA    Real  Public   The constant used for separating wind sea
!                               and swell when we estimate WBT
!     ----------------------------------------------------------------
!
!     All elements of SGRD are aliased to pointers with the same
!     name. These pointers are defined as :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      NK        Int.  Public   Number of discrete wavenumbers.
!      NK2       Int.  Public   Extended wavenumber range.
!      NTH       Int.  Public   Number of discrete directions.
!      NSPEC     Int.  Public   Number of discrete spectral bins.
!      MAPxx     I.A.  Public   Spectral maps.
!      DTH       Real  Public   Directional increments (radians).
!      XFR       Real  Public   Frequency multiplication factor.
!      FR1       Real  Public   Lowest frequency                 (Hz)
!      FTE       Real  Public   Factor in tail integration energy.
!      FTF       Real  Public   Id. frequency.
!      FTWN      Real  Public   Id. wavenumber.
!      FTTR      Real  Public   Id. wave period.
!      FTWL      Real  Public   Id. wave length.
!      FACTIn    Real  Public   Factors for obtaining integer cut-off
!                               frequency.
!      FACHFx    Real  Public   Factor for tail.
!      TH        R.A   Public   Directions (radians).
!      ESIN      R.A   Public   Sine of discrete directions.
!      ECOS      R.A   Public   Cosine of discrete directions.
!      ES2, ESC, EC2
!                R.A   Public   Sine and cosine products
!      SIG       R.A   Public   Relative frequencies (invariant
!                                                     in grid). (rad)
!      SIG2      R.A   Public   Id. for full 2-D spectrum.
!      DSIP      R.A   Public   Frequency bandwidths (prop.)    (rad)
!      DSII      R.A   Public   Frequency bandwidths (int.)     (rad)
!      DDEN      R.A   Public   DSII * DTH * SIG (for integration
!                               based on energy)
!      DDEN2     R.A   Public   Idem, full spectrum.
!      SINIT     Log.  Public   Flag identifying grid initialization.
!     ----------------------------------------------------------------
!
!     The structure MPAR contains all other model parameters for
!     numerical methods and physical parameterizations. It contains
!     itself several structures as outlined below.
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      PINIT     Log.  Public   Flag identifying initialization.
!      NPARS     NPAR  Public   Numerical parameters,
!      PROPS     PROP  Public   Parameters propagatrion schemes.
!      SFLPS     SFLP  Public   Parameters for flux computation.
!      SLNPS     SLNP  Public   Parameters Sln.
!      SRCPS     SRCP  Public   Parameters Sin and Sds.
!      SNLPS     SNLP  Public   Parameters Snl.
!      SBTPS     SBTP  Public   Parameters Sbt.
!      SDBPS     SDBP  Public   Parameters Sdb.
!      STRPS     STRP  Public   Parameters Str.
!      SBSPS     SBSP  Public   Parameters Sbs.
!     ----------------------------------------------------------------
!
!     The structure NPAR contains numerical parameters and is aliased
!     as above:
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      FACP      Real  Public   Constant in maximum par. change in
!                               dynamic integration scheme (depends
!                               upon Xp).
!      XREL      Real  Public   Id. relative change.
!      XFLT      Real  Public   Id. filter level.
!      FXFM      Real  Public   Constant for mean frequency in
!                               cut-off.                       (!/ST1)
!      FXPM      Real  Public   Id. PM.
!      XFT       Real  Public   Constant for cut-off freq.     (!/ST2)
!      XFC       Real  Public   Id.
!      FACSD     Real  Public   Constant in seeding algorithm.
!      FHMAX     Real  Public   Hs/depth ratio in limiter     (!/MLIM)
!      RWINDC    Real  Public   Coefficient for current in relative 
!                               wind                          (!/RWND)
!      WWCOR     R.A.  Public   Wind correction factors       (!/WCOR)
!     ----------------------------------------------------------------
!
!     The structure PROP contains parameters for the propagation
!     schemes and is aliased as above:
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      DTME      Real  Public   Swell age in disp. corr.      (!/PR2)
!      CLATMN    Real  Public   Id. minimum cosine of lat.    (!/PR2)
!      DTMS      Real  Public   Swell age in disp. corr.      (!/SMC)
!
!      WDCG      Real  Public   Factors in width of av. Cg.   (!/PR3)
!      WDTH      Real  Public   Factors in width of av. Th.   (!/PR3)
!     ----------------------------------------------------------------
!
!     The structure SFLP contains parameters for the fluxes
!     and is aliased as above:
!     ----------------------------------------------------------------
!                                                            (!/FLX2)
!      NITTIN    Int.  Public   Number of itterations for drag calc.
!      CINXSI    Real  Public   Constant in parametric description
!                                                            (!/FLX3)
!      NITTIN    Int.  Public   Number of itterations for drag calc.
!      CAP_ID    Int   Public   Type of cap used.
!      CINXSI    Real  Public   Constant in parametric description
!      CD_MAX    Real  Public   Cap on Cd.
!                                                            (!/FLX4)
!      FLX4A0    Real  Public   Scaling value in parametric description
!     ----------------------------------------------------------------
!
!     The structure SLNP contains parameters for the linear input
!     source terms and is aliased as above:
!
!     ----------------------------------------------------------------
!                                                             (!/LN1)
!      SLNC1     Real  Public   Proportionality and other constants in
!                               input source term.
!      FSPM      Real  Public   Factor for fPM in filter.
!      FSHF      Real  Public   Factor for fh in filter.
!     ----------------------------------------------------------------
!
!     The structure SRCP contains parameters for the input and dis,
!     source terms and is aliased as above:
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      WWNMEANPTAIL R  Public   Power of tail for WNMEAN calculation
!      SSTXFTFTAIL  R  Public   Tail factor for  WNMEAN calculation
!                                                             (!/ST1)
!      SINC1     Real  Public   Proportionality and other constants in
!                               input source term.
!      SDSC1     Real  Public   Combined constant in dissipation
!                               source term.
!                                                             (!/ST2)
!      ZWIND     Real  Public   Height at which the wind is defined
!                               of drag.
!      FSWELL    Real  Public   Reduction factor of negative input
!                               for swell.
!      SHSTAB, OFSTAB, CCNG, CCPS, FFNG, FFPS
!                Real  Public   Factors in effective wind speed.
!      CDSAn     Real  Public   Constants in high-freq. dis.
!      SDSALN    Real  Public   Factor for nondimensional 1-D spectrum.
!      CDSBn     Real  Public   Constants in parameterization of PHI.
!      XFH       Real  Public   Constant for turbulent length scale.
!      XFn       Real  Public   Constants in combining low and high
!                               frequency dissipation.
!                                                             (!/ST3)
!      ZZWND     Real  Public   Height at which the wind is defined
!      AALPHA    Real  Public   Minimum value of charnock parameter
!      BBETA     Real  Public   Wind-wave coupling coefficient
!      ZZALP     Real  Public   Wave age tuning coefficient in Sin
!      TTAUWSHELTER Real  Public Sheltering coefficient for short waves
!      ZZ0MAX    Real  Public   Maximum value of air-side roughness
!      ZZ0RAT    Real  Public   ratio of roughness for mean and
!                               oscillatory flows
!      SSINTHP   Real  Public   Power in cosine of wind input
!      SSWELLF   R.A.  Public   Swell damping coefficients
!      SSDSCn    Real  Public   Dissipation parameters
!      SSDSBR    Real  Public   Threshold in saturation spectrum for Sds
!      SSDSP     Real  Public   Power of B(k) in Sds
!      WWNMEANP  Real  Public   Power that defines the mean wavenumber
!                               in Sds
!      SSTXFTF, SSTXFTWN Real  Public   Tail constants
!      SSDSC4,   Real  Public   Threshold shift in saturation diss.
!      SSDSC5,   Real  Public   Wave-turbulence dissipation factor
!      SSDSC6,   Real  Public   dissipation parameter
!      DDELTA1   Real  Public   Low-frequency dissipation coefficient
!                               in WAM4
!      DDELTA2   Real  Public   High-frequency dissipation coefficient
!                               in WAM4
!      SSDSDTH   Real  Public   Maximum angular sector for saturation
!                               spectrum
!      SSDSCOS   Real  Public   Power of cosine in saturation integral
!      SSDSISO   Int.  Public   Choice of definition of the isotropic
!                               saturation
!     ----------------------------------------------------------------
!
!     The structure SNLP contains parameters for the nonl. inter.
!     source term and is aliased as above:
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!                                                             (!/NL1)
!      SNLC1     Real  Public   Scaled proportionality constant.
!      LAM       Real  Public   Factor defining quadruplet.
!      KDCON     Real  Public   Conversion factor for relative depth.
!      KDMN      Real  Public   Minimum relative depth.
!      SNLSn     Real  Public   Constants in shallow water factor.
!                                                             (!/NL2)
!      IQTPE     Int.  Public   Type of depth treatment 
!                                1 : Deep water         
!                                2 : Deep water / WAM scaling
!                                3 : Finite water depth 
!      NDPTHS    Int.  Public   Number of depth for which integration
!                               space needs to be computed.
!      NLTAIL    Real  Public   Tail factor for parametric tail.
!      DPTHNL    R.A.  Public   Depths corresponding to NDPTHS.   
!                               *** NOTE: This array is not allocated
!                                         in the W3DIMP routine ***
!                                                             (!/NL3)
!      NFR       Int.  Public   Number of frequencies or wavenumbers
!                               in discrete spectral space (NFR=>NK).
!      NFRMIN    Int.  Public   Minimum discrete frequency in the
!                               expanded frequency space.
!      NFRMAX    Int.  Public   Idem maximum for first part.
!      NFRCUT    Int.  Public   Idem maximum for second part.
!      NTHMAX    Int.  Public   Extension of directional space.
!      NTHEXP    Int   Public   Number of bins in extended dir. space.
!      NSPMIN, NSPMAX, NSPMX2
!                Int.  Public   1D spectral space range.
!      FRQ       R.A.  Public   Expanded frequency range (Hz).
!      XSI       R.A.  Public   Expanded frequency range (rad/s).
!      NQA       Int.  Public   Number of actual quadruplets.
!      QST1      I.A.  Public   Spectral offsets for compuation of
!                               quadruplet spectral desnities.
!      QST2      R.A.  Public   Idem weights.
!      QST3      R.A.  Public   Proportionality constants and k factors
!                               in diagonal strength.
!      QST4      I.A.  Public   Spectral offsets for combining of
!                               interactions and diagonal.
!      QST5      R.A.  Public   Idem weights for interactions.
!      QST6      R.A.  Public   Idem weights for diagonal.
!      SNLNQ     Int.  Public   Number of quadruplet definitions.
!      SNLMSC    Real  Public   Tuning power 'deep' scaling.
!      SNLNSC    Real  Public   Tuning power 'shallow' scaling.
!      SNLSFD    Real  Public   'Deep' nondimensional filer freq.
!      SNLSFS    Real  Public   'Shallow' nondimensional filer freq.
!      SNLL      R.A.  Public   Array with lambda for quadruplet.
!      SNLM      R.A.  Public   Array with mu for quadruplet.
!      SNLT      R.A.  Public   Array with Dtheta for quadruplet.
!      SNLCD     R.A.  Public   Array with Cd for quadruplet.
!      SNLCS     R.A.  Public   Array with Cs for quadruplet.
!                                                             (!/NL4)
!      ITSA      Int.  Public   Integer indicating TSA (1) or FBI (0)
!      IALT      Int.  Public   Integer determining alternating looping
!                                                             (!/NL5)
!      QR5DPT    Real  Public   Water depth for the GKE module
!      QR5OML    Real  Public   λ cut off value for quasi-resonant quartets
!      QI5DIS    Int.  Public   Method to discretize continuous spectrum
!      QI5KEV    Int.  Public   GKE (GS13 or J03)
!      QI5NNZ    Int.  Public   # of interactive quadruplets
!      QI5IPL    Int.  Public   Interp. method to get C₄
!      QI5PMX    Int.  Public   Phase mixing related parameter
!                                                             (!/NLS)
!      NTHX      Int.  Public   Expanded discrete direction range.
!      NFRX      Int.  Public   Expanded discrete frequency range.
!      NSPL-H    Int.  Public   Range of 1D spectrum.
!      SNSST     R.A.  Public   Array with interpolation weights.
!      CNLSA     Real  Public   a34 in quadruplet definition.
!      CNLSC     Real  Public   C in Snl definition.
!      CNLSFM    Real  Public   Maximum relative spectral change.
!      CNLSC1/3  Real  Public   Constant in frequency filter.
!     ----------------------------------------------------------------
!
!     The structure SBTP contains parameters for the bottom friction
!     source term and is aliased as above:
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      SBTC1     Real  Public   Proportionality constant.    (!/BT1)
!      SBTCX     R.A.  Public   Parameters for bottom fric.  (!/BT4)
!     ----------------------------------------------------------------
!
!     The structure SDBP contains parameters for the depth incduced
!     breaking source term and is aliased as above:
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      SDBC1     Real  Public   Proportionality constant.    (!/DB1)
!      SDBC2     Real  Public   Hmax/d ratio.                (!/DB1)
!      FDONLY    Log.  Public   Flag for checking depth only (!/DB1)
!                               otherwise Miche criterion.
!     ----------------------------------------------------------------
!
!     The structure STRP contains parameters for the triad interaction
!     source term and is aliased as above:
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!     The structure SBSP contains parameters for the bottom scattering
!     source term and is aliased as above:
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!     The structure SICP contains parameters for arbitrary source 
!     term and is aliased as above:
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!     IS1C1      Real  Public   Scale factor for icecon.     (!/ISx)
!     IS1C2      Real  Public   Offset for ice concentration (!/ISx)
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3NMOD    Subr. Public   Set number of grids.
!      W3DIMX    Subr. Public   Set dimensions of spatial grid.
!      W3DIMS    Subr. Public   Set dimensions of spectral grid.
!      W3SETG    Subr. Public   Point to selected grid / model.
!      W3GNTX    Subr. Public   Construct grid arrays
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      EXTCDE    Subr. W3SERVMD Abort program with exit code.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!     - In model versions before 3.06 the parameters in the grid
!       structure were stored in the module W3IOGR.
!     - No subroutine DIMP is provided, instead, arrays are set
!       one-by-one in W3IOGR.
!
!  6. Switches :
!
!     See subroutine documentation.
!
!     !/PRn  Select propagation scheme
!     !/SMC  UNO2 propagation on SMC grid.   
!
!     !/LNn  Select source terms
!     !/STn
!     !/NLn
!     !/BTn
!     !/DBn
!     !/TRn
!     !/BSn
!     !/XXn
!
!     !/S    Enable subroutine tracing.
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
!/
!/ Required modules
!/
      USE W3GSRUMD
!/
!/ Specify default accessibility
!/
      PUBLIC
!/
!/ Module private variable for checking error returns
!/
      INTEGER, PRIVATE        :: ISTAT
!/
!/ Conventional declarations
!/
      INTEGER                 :: NGRIDS = -1, IGRID = -1, ISGRD = -1, &
                                 IPARS = -1, NAUXGR
!
#ifdef W3_IC4
      INTEGER, PARAMETER      :: NIC4=10
#endif
      INTEGER, PARAMETER      :: RLGTYPE = 1
      INTEGER, PARAMETER      :: CLGTYPE = 2
      INTEGER, PARAMETER      :: UNGTYPE = 3      
      INTEGER, PARAMETER      :: SMCTYPE = 4

      INTEGER, PARAMETER      :: ICLOSE_NONE = ICLO_NONE
      INTEGER, PARAMETER      :: ICLOSE_SMPL = ICLO_SMPL
      INTEGER, PARAMETER      :: ICLOSE_TRPL = ICLO_TRPL
!
! Dimensions of tables for pre-computing of dissipation 
!  
#ifdef W3_ST4
      INTEGER,    PARAMETER   :: NKHS=2000, NKD=1300
      INTEGER,    PARAMETER   :: NDTAB=2000          
#endif
!/
!/ Data structures
!/
!/ Grid type
      TYPE GRID          ! this is the geographical grid with all associated parameters 
        INTEGER          :: GTYPE
        INTEGER          :: RSTYPE = -1
        INTEGER          :: ICLOSE
        INTEGER          :: NX, NY, NSEA, NSEAL, TRFLAG
#ifdef W3_SEC1
     INTEGER          :: NITERSEC1
#endif
        INTEGER, POINTER :: MAPSTA(:,:), MAPST2(:,:),            &
                            MAPFS(:,:), MAPSF(:,:)
!
#ifdef W3_SMC
 !!Li     Cell and face arrays for SMC grid.  
        INTEGER          :: NCel, NUFc, NVFc, NRLv, MRFct 
        INTEGER          :: NGLO, NARC, NBGL, NBAC, NBSMC
        INTEGER, POINTER :: NLvCel(:), NLvUFc(:), NLvVFc(:) 
        INTEGER, POINTER :: IJKCel(:,:), IJKUFc(:,:), IJKVFc(:,:)
        INTEGER, POINTER :: ISMCBP(:),   ICLBAC(:)
#endif
!
        REAL             :: SX, SY, X0, Y0, DTCFL, DTCFLI, DTMAX,      &
                            DTMIN, DMIN, CTMAX, FICE0, FICEN, FICEL,   &
                            PFMOVE, STEXU, STEYU, STEDU, IICEHMIN,     &
                            IICEHINIT, ICESCALES(4), IICEHFAC, IICEHDISP, &
                            IICEDDISP, IICEFDISP, BTBETA, AAIRCMIN, AAIRGB

        REAL(8)          :: GRIDSHIFT ! see notes in WMGHGH

#ifdef W3_RTD
        REAL                  :: PoLat, PoLon   ! Rotated N-Pole lat/lon
        REAL, POINTER         :: AnglD(:)       ! Angle in degree
        LOGICAL               :: FLAGUNR
#endif

        REAL   , POINTER :: ZB(:)     ! BOTTOM GRID, DEFINED ON ISEA
        REAL   , POINTER :: CLATS(:)  ! COS(LAT), DEFINED ON SEA POINTS
        REAL   , POINTER :: CLATIS(:) ! INVERSE OF COS(LAT) DEFINED ON ISEA
        REAL   , POINTER :: CTHG0S(:) ! TAN(Y)/R, DEFINED ON ISEA

        REAL   , POINTER :: TRNX(:,:), TRNY(:,:) ! TRANSPARENCY INFORMATION ON IX,IY
#ifdef W3_SMC
        REAL, POINTER         :: CTRNX(:), CTRNY(:), CLATF(:)
#endif
        REAL   , POINTER :: SPCBAC(:,:), ANGARC(:)
        REAL   , POINTER :: XGRD(:,:), YGRD(:,:) ! X AND Y DEFINED ON IX,IY
        REAL   , POINTER :: DXDP(:,:), DXDQ(:,:) ! DX/DP & DX/DQ DEFINED ON IX,IY
        REAL   , POINTER :: DYDP(:,:), DYDQ(:,:) ! DY/DP & DY/DQ DEFINED ON IX,IY
        REAL   , POINTER :: DPDX(:,:), DPDY(:,:) ! DP/DX & DP/DY DEFINED ON IX,IY
        REAL   , POINTER :: DQDX(:,:), DQDY(:,:) ! DQ/DX & DQ/DY DEFINED ON IX,IY
        REAL   , POINTER :: GSQRT(:,:) ! SQRT(G) DEFINED ON IX,IY
        REAL   , POINTER :: HPFAC(:,:) ! H_P = SQRT(G_PP) DEFINED ON IX,IY
        REAL   , POINTER :: HQFAC(:,:) ! H_Q = SQRT(G_QQ) DEFINED ON IX,IY

        LOGICAL          :: GINIT, FLDRY, FLCX, FLCY, FLCTH, FLCK, FLSOU, IICEDISP,&
                            IICESMOOTH
        LOGICAL          :: FLAGLL
        LOGICAL          :: CMPRTRCK
        LOGICAL, POINTER :: FLAGST(:)
        CHARACTER(LEN=30):: GNAME
        CHARACTER(LEN=13):: FILEXT
        LOGICAL          :: GUGINIT
#ifdef W3_REF1
        REAL, POINTER    :: REFLC(:,:)  ! reflection coefficient
        INTEGER, POINTER :: REFLD(:,:)  ! reflection direction
#endif
        INTEGER          :: E3DF(3,5), P2MSF(3), US3DF(3), USSPF(2) ! freq. indices for 3D output
        REAL             :: USSP_WN(25) !Max set to 25 decay scales.
!
        TYPE(T_GSU) :: GSU ! Grid search utility object
!
        REAL                  :: FFACBERG    ! mutiplicative factor for iceberg mask
#ifdef W3_BT4
   REAL, POINTER         :: SED_D50(:), SED_PSIC(:) 
#endif
#ifdef W3_REF1
        LOGICAL, POINTER      :: RREF(:)
        REAL,    POINTER      :: REFPARS(:)
#endif
#ifdef W3_IG1
        REAL,    POINTER      :: IGPARS(:)
#endif
#ifdef W3_IC2
        REAL,    POINTER      :: IC2PARS(:)
#endif
#ifdef W3_IC3
        REAL,    POINTER      :: IC3PARS(:)
#endif
#ifdef W3_IC4
        INTEGER, POINTER      :: IC4PARS(:)
        REAL, POINTER         :: IC4_KI(:)
        REAL, POINTER         :: IC4_FC(:)
#endif
#ifdef W3_IC5
        REAL,    POINTER      :: IC5PARS(:)
#endif
#ifdef W3_IS2
        REAL,    POINTER      :: IS2PARS(:)
#endif
!
! unstructured data
!
        INTEGER               :: NTRI
        DOUBLE PRECISION, POINTER         :: XYB(:,:)
        INTEGER, POINTER      :: TRIGP(:,:)
#ifdef W3_PDLIB
        INTEGER               :: NBND_MAP
        INTEGER, POINTER      :: INDEX_MAP(:)
        INTEGER, POINTER      :: MAPSTA_LOC(:)
#endif
        REAL(8), POINTER      :: LEN(:,:),SI(:), IEN(:,:)

        REAL                  :: MAXX, MAXY, DXYMAX
        REAL, POINTER         :: ANGLE(:,:),ANGLE0(:,:)
        INTEGER               :: COUNTRI,COUNTOT,NNZ, NBEDGE
        INTEGER, POINTER      :: CCON(:), COUNTCON(:), IE_CELL(:), &
                                 POS_CELL(:), IOBP(:), IOBPD(:,:), IOBDP(:), IOBPA(:),   &
#ifdef W3_PDLIB
                          IOBPD_loc(:,:), IOBP_loc(:),                    &
#endif
                                 IAA(:), JAA(:), POSI(:,:), INDEX_CELL(:),       &
                                 I_DIAG(:), JA_IE(:,:,:)
        INTEGER, POINTER      :: EDGES(:,:), NEIGH(:,:)
        REAL(8), POINTER      :: TRIA(:)
        REAL, POINTER         :: CROSSDIFF(:,:)

#ifdef W3_UOST
         CHARACTER(LEN=256)      :: UOSTFILELOCAL, UOSTFILESHADOW
         LOGICAL, ALLOCATABLE    :: UOST_LCL_OBSTRUCTED(:,:), UOST_SHD_OBSTRUCTED(:,:)
         INTEGER*1, ALLOCATABLE  :: UOSTLOCALALPHA(:,:,:,:), UOSTLOCALBETA(:,:,:,:)
         INTEGER*1, ALLOCATABLE  :: UOSTSHADOWALPHA(:,:,:,:), UOSTSHADOWBETA(:,:,:,:)
         REAL*4, ALLOCATABLE     :: UOSTCELLSIZE(:,:,:)
         REAL                    :: UOSTABMULTFACTOR = 100
         REAL                    :: UOSTCELLSIZEFACTOR = 1000
         REAL                    :: UOSTLOCALFACTOR = 1
         REAL                    :: UOSTSHADOWFACTOR = 1
         LOGICAL                 :: UOSTENABLED = .true.
#endif

      END TYPE GRID
!
      TYPE SGRD   ! this is the spectral grid with all parameters that vary with freq. and direction
        INTEGER               :: NK=0, NK2=0, NTH=0, NSPEC=0
        INTEGER, POINTER      :: MAPWN(:), MAPTH(:)
        REAL                  :: DTH=0., XFR=0., FR1=0., FTE=0., FTF=0., FTWN=0., FTTR=0., &
                                 FTWL=0., FACTI1=0., FACTI2=0., FACHFA=0., FACHFE=0.
        REAL, POINTER         :: TH(:), ESIN(:), ECOS(:), ES2(:),     &
                                 ESC(:), EC2(:), SIG(:), SIG2(:),     &
                                 DSIP(:), DSII(:), DDEN(:), DDEN2(:)
        LOGICAL               :: SINIT=.FALSE.
      END TYPE SGRD
!
      TYPE NPAR
        REAL                  :: FACP, XREL, XFLT, FXFM, FXPM,        &
                                 XFT, XFC, FACSD, FHMAX
#ifdef W3_RWND
        REAL            :: RWINDC
#endif
#ifdef W3_WCOR
        REAL            :: WWCOR(2)
#endif
      END TYPE NPAR
!
      TYPE PROP
#ifdef W3_PR0
        REAL                  :: DUMMY
#endif
#ifdef W3_PR1
        REAL                  :: DUMMY
#endif
#ifdef W3_PR2
        REAL                  :: DTME, CLATMN
#endif
#ifdef W3_PR3
        REAL                  :: WDCG, WDTH
#endif
#ifdef W3_SMC
        REAL                  :: DTMS, Refran
        LOGICAL               :: FUNO3, FVERG, FSWND, ARCTC  
#endif
      END TYPE PROP
!
      TYPE FLDP
         REAL :: DUMMY
#ifdef W3_FLD1
        INTEGER               :: Tail_ID
        REAL                  :: Tail_Lev, TAIL_TRAN1, TAIL_TRAN2
#endif
#ifdef W3_FLD2
        INTEGER               :: Tail_ID
        REAL                  :: Tail_Lev, TAIL_TRAN1, TAIL_TRAN2
#endif
      END TYPE FLDP
      TYPE SFLP
#ifdef W3_FLX0
        REAL                  :: DUMMY
#endif
#ifdef W3_FLX1
        REAL                  :: DUMMY
#endif
#ifdef W3_FLX2
        INTEGER               :: NITTIN
        REAL                  :: CINXSI
#endif
#ifdef W3_FLX3
        INTEGER               :: NITTIN, CAP_ID
        REAL                  :: CINXSI, CD_MAX
#endif
#ifdef W3_FLX4
        REAL                  :: FLX4A0
#endif
      END TYPE SFLP
!
      TYPE SLNP
#ifdef W3_SEED
        REAL                  :: DUMMY
#endif
#ifdef W3_LN0
        REAL                  :: DUMMY
#endif
#ifdef W3_LN1
        REAL                  :: SLNC1, FSPM, FSHF
#endif
      END TYPE SLNP
!
      TYPE SRCP
        REAL                       :: WWNMEANPTAIL, SSTXFTFTAIL
#ifdef W3_ST1
        REAL                  :: SINC1, SDSC1
#endif
#ifdef W3_ST2
        REAL                  :: ZWIND, FSWELL, SHSTAB,               &
                                 OFSTAB, CCNG, CCPS, FFNG, FFPS,      &
                                 CDSA0, CDSA1, CDSA2, SDSALN,         &
                                 CDSB0, CDSB1, CDSB2, CDSB3, FPIMIN,  &
                                 XFH, XF1, XF2
#endif
#ifdef W3_ST3
        INTEGER               :: SSDSISO, SSDSBRFDF
        REAL                  :: AALPHA, BBETA, ZZ0MAX, ZZ0RAT, ZZALP,&
                                 SSINTHP, TTAUWSHELTER, SSWELLF(1:6), &
                                 SSDSC1, SSDSC2, SSDSC3, SSDSBR,      &
                                 SSDSP, WWNMEANP, SSTXFTF, SSTXFTWN,  &
                                 FFXPM, FFXFM,                        &
                                 SSDSC4, SSDSC5, SSDSC6, DDELTA1,     &
                                 DDELTA2, ZZWND
#endif
!
#ifdef W3_ST4
        INTEGER               :: SSWELLFPAR, SSDSISO, SSDSBRFDF
        INTEGER,  POINTER     :: IKTAB(:,:), SATINDICES(:,:)
        REAL,     POINTER     :: DCKI(:,:), SATWEIGHTS(:,:),CUMULW(:,:),QBI(:,:)
        REAL                  :: AALPHA, BBETA, ZZ0MAX, ZZ0RAT, ZZALP,&
                                 SSINTHP, TTAUWSHELTER, SSWELLF(1:7), &
                                 SSDSC(1:21), SSDSBR,                 &
                                 SSDSP, WWNMEANP, SSTXFTF, SSTXFTWN,  &
                                 FFXPM, FFXFM, FFXFA,   &
                                 SSDSBRF1, SSDSBRF2, SSDSBINT,SSDSBCK,&
                                 SSDSHCK, SSDSABK, SSDSPBK, SSINBR
        REAL                  :: ZZWND
        REAL                  :: SSDSCOS, SSDSDTH, SSDSBT, SSDSBM(0:4)
#endif
!
#ifdef W3_ST6
        REAL                  :: SIN6A0, SDS6A1, SDS6A2, SWL6B1, &
                                 SIN6WS, SIN6FC
        INTEGER               :: SDS6P1, SDS6P2
        LOGICAL               :: SDS6ET, SWL6S6, SWL6CSTB1
#endif
      END TYPE SRCP
!
      TYPE SNLP
#ifdef W3_NL0
        REAL                  :: DUMMY
#endif
#ifdef W3_NL1
        REAL                  :: SNLC1, LAM, KDCON, KDMN,             &
                                 SNLS1, SNLS2, SNLS3
#endif
#ifdef W3_NL2
        INTEGER               :: IQTPE, NDPTHS
        REAL                  :: NLTAIL
        REAL, POINTER         :: DPTHNL(:)
#endif
#ifdef W3_NL3
        INTEGER               :: NFRMIN, NFRMAX, NFRCUT, NTHMAX,      &
                                 NTHEXP, NSPMIN, NSPMAX, NSPMX2,      &
                                 NQA, SNLNQ
        INTEGER, POINTER      :: QST1(:,:,:), QST4(:,:,:)
        REAL                  :: SNLMSC, SNLNSC, SNLSFD, SNLSFS
        REAL, POINTER         :: FRQ(:), XSI(:),                      &
                                 QST2(:,:,:), QST3(:,:,:),            &
                                 QST5(:,:,:), QST6(:,:,:),            &
                                 SNLL(:), SNLM(:), SNLT(:),           &
                                 SNLCD(:), SNLCS(:)
#endif
#ifdef W3_NL4
        INTEGER               :: ITSA, IALT
#endif
#ifdef W3_NL5
        REAL                  :: QR5DPT, QR5OML
        INTEGER               :: QI5DIS, QI5KEV, QI5IPL, QI5PMX
        INTEGER(KIND=8)       :: QI5NNZ
#endif
#ifdef W3_NLS
        INTEGER               :: NTHX, NFRX, NSPL, NSPH
        REAL                  :: CNLSA, CNLSC, CNLSFM,                &
                                 CNLSC1, CNLSC2, CNLSC3
        REAL, POINTER         :: SNSST(:,:)
#endif

      END TYPE SNLP
!
      TYPE SBTP
#ifdef W3_BT0
        REAL                  :: DUMMY
#endif
#ifdef W3_BT1
        REAL                  :: SBTC1
#endif
#ifdef W3_BT4
        REAL                  :: SBTCX(10)
#endif
#ifdef W3_BT8
        REAL                  :: DUMMY
#endif
#ifdef W3_BT9
        REAL                  :: DUMMY
#endif
      END TYPE SBTP
!
      TYPE SDBP
#ifdef W3_DB0
        REAL                  :: DUMMY
#endif
#ifdef W3_DB1
        REAL                  :: SDBC1, SDBC2
        LOGICAL               :: FDONLY
#endif
      END TYPE SDBP

#ifdef W3_UOST
      TYPE UOSTP
        CHARACTER(LEN=256)    :: UOSTFILELOCAL, UOSTFILESHADOW
        REAL              :: UOSTFACTORLOCAL, UOSTFACTORSHADOW
      END TYPE UOSTP
#endif

!
      TYPE STRP
#ifdef W3_TR0
        REAL                  :: DUMMY
#endif
#ifdef W3_TR1
        REAL                  :: DUMMY
#endif
      END TYPE STRP
!
      TYPE SBSP
#ifdef W3_BS0
        REAL                  :: DUMMY
#endif
#ifdef W3_BS1
        REAL                  :: DUMMY
#endif
      END TYPE SBSP
!
      TYPE SICP
#ifdef W3_IS0
        REAL                  :: DUMMY
#endif
#ifdef W3_IS1
        REAL                  :: IS1C1, IS1C2
#endif
#ifdef W3_IS2
        REAL                  :: IS2C1, IS2C2
#endif
      END TYPE SICP
      
! specific type for unstructured scheme
      TYPE SCHM
         LOGICAL :: FSN          = .FALSE. 
         LOGICAL :: FSPSI        = .FALSE. 
         LOGICAL :: FSFCT        = .FALSE. 
         LOGICAL :: FSNIMP       = .FALSE. 
         LOGICAL :: FSTOTALIMP   = .FALSE. 
         LOGICAL :: FSTOTALEXP   = .FALSE. 
         LOGICAL :: FSREFRACTION = .FALSE. 
         LOGICAL :: FSFREQSHIFT  = .FALSE. 
         LOGICAL :: FSSOURCE     = .FALSE. 
         LOGICAL :: FSBCCFL      = .FALSE. 
         LOGICAL :: DO_CHANGE_WLV 
         REAL(8) :: SOLVERTHR_STP 
         REAL(8) :: CRIT_DEP_STP
         LOGICAL :: B_JGS_TERMINATE_MAXITER 
         LOGICAL :: B_JGS_TERMINATE_DIFFERENCE 
         LOGICAL :: B_JGS_TERMINATE_NORM 
         LOGICAL :: B_JGS_LIMITER 
         LOGICAL :: B_JGS_USE_JACOBI
         LOGICAL :: B_JGS_BLOCK_GAUSS_SEIDEL 
         INTEGER :: B_JGS_MAXITER 
         REAL*8  :: B_JGS_PMIN
         REAL*8  :: B_JGS_DIFF_THR 
         REAL*8  :: B_JGS_NORM_THR 
         INTEGER :: B_JGS_NLEVEL
         LOGICAL :: B_JGS_SOURCE_NONLINEAR 
      END TYPE SCHM    
!      
!
      TYPE MPAR
        LOGICAL               :: PINIT
        TYPE(NPAR)            :: NPARS
        TYPE(PROP)            :: PROPS
        TYPE(FLDP)            :: FLDPS
        TYPE(SFLP)            :: SFLPS
        TYPE(SLNP)            :: SLNPS
        TYPE(SRCP)            :: SRCPS
        TYPE(SNLP)            :: SNLPS
        TYPE(SBTP)            :: SBTPS
        TYPE(SDBP)            :: SDBPS
#ifdef W3_UOST
        TYPE(UOSTP)            :: UOSTPS
#endif
        TYPE(STRP)            :: STRPS
        TYPE(SBSP)            :: SBSPS
        TYPE(SICP)            :: SICPS
        TYPE(SCHM)            :: SCHMS
      END TYPE MPAR
!/
!/ Data storage
!/
      TYPE(GRID), TARGET, ALLOCATABLE :: GRIDS(:)
      TYPE(SGRD), TARGET, ALLOCATABLE :: SGRDS(:)
      TYPE(MPAR), TARGET, ALLOCATABLE :: MPARS(:)
!/
!/ Data aliases for structure GRID(S)
!/
      INTEGER, POINTER :: GTYPE
      INTEGER, POINTER :: RSTYPE
      INTEGER, POINTER :: ICLOSE
      INTEGER, POINTER        :: NX, NY, NSEA, NSEAL, TRFLAG
      INTEGER, POINTER        :: E3DF(:,:), P2MSF(:), US3DF(:), USSPF(:)
      REAL,    POINTER        :: USSP_WN(:)
#ifdef W3_REF1
      REAL,    POINTER        :: REFLC(:,:)
      INTEGER, POINTER        :: REFLD(:,:)
#endif
      INTEGER, POINTER        :: NBEDGE
      INTEGER, POINTER        :: EDGES(:,:), NEIGH(:,:)
!
! Variables for unstructured grids 
!
      INTEGER, POINTER        :: NTRI,COUNTRI,COUNTOT,NNZ
      INTEGER                 :: optionCall = 3 ! take care all other options are basically wrong
!  XYB may not be necessary now that we have XGRD and YGRD
!  but these XGRD and YGRD should probably be double precision
      DOUBLE PRECISION, POINTER  ::     XYB(:,:)   
      INTEGER, POINTER        :: TRIGP(:,:)
#ifdef W3_PDLIB
      INTEGER, POINTER        :: NBND_MAP
      INTEGER, POINTER        :: INDEX_MAP(:)
      INTEGER, POINTER        :: MAPSTA_LOC(:)
#endif
      REAL(8), POINTER        :: IEN(:,:), LEN(:,:), SI(:)
      REAL, POINTER           :: ANGLE(:,:),ANGLE0(:,:)
      INTEGER,  POINTER       :: CCON(:),  COUNTCON(:), IE_CELL(:),           &
                                 POS_CELL(:), IOBP(:), IOBPD(:,:), IOBDP(:), IOBPA(:),  &
#ifdef W3_PDLIB
                                 IOBPD_loc(:,:), IOBP_loc(:),          &
#endif
                                 IAA(:), JAA(:), POSI(:,:),                   &
                                 I_DIAG(:), JA_IE(:,:,:),                     &
                                 INDEX_CELL(:)
      REAL(8), POINTER        :: TRIA(:)
      REAL, POINTER           :: CROSSDIFF(:,:)
      REAL,POINTER            :: MAXX, MAXY, DXYMAX 
      LOGICAL, POINTER        :: GUGINIT       
!
      REAL,    POINTER        :: FFACBERG
#ifdef W3_REF1
      LOGICAL, POINTER        ::  RREF(:)
      REAL,    POINTER        :: REFPARS(:)
#endif
#ifdef W3_IG1
      REAL,    POINTER        :: IGPARS(:)
#endif
#ifdef W3_IC2
      REAL,    POINTER        :: IC2PARS(:)
#endif
#ifdef W3_IC3
      REAL,    POINTER        :: IC3PARS(:)
#endif
#ifdef W3_IC4
      INTEGER, POINTER        :: IC4PARS(:)
      REAL, POINTER           :: IC4_KI(:)
      REAL, POINTER           :: IC4_FC(:)
#endif
#ifdef W3_IC5
      REAL,    POINTER        :: IC5PARS(:)
#endif
#ifdef W3_IS2
      REAL,    POINTER        :: IS2PARS(:)
#endif
      INTEGER, POINTER        :: MAPSTA(:,:), MAPST2(:,:),            &
                                 MAPFS(:,:), MAPSF(:,:)
!
#ifdef W3_SMC
        INTEGER, POINTER :: NCel, NUFc, NVFc, NRLv, MRFct 
        INTEGER, POINTER :: NGLO, NARC, NBGL, NBAC, NBSMC
        INTEGER, POINTER :: NLvCel(:), NLvUFc(:), NLvVFc(:)
        INTEGER, POINTER :: IJKCel(:,:), IJKUFc(:,:), IJKVFc(:,:)
        INTEGER, POINTER :: ISMCBP(:),   ICLBAC(:)
#endif
!
#ifdef W3_SEC1
      INTEGER, POINTER        :: NITERSEC1
#endif
      REAL, POINTER           :: SX, SY, X0, Y0, DTCFL, DTCFLI, DTMAX, &
                                 DTMIN, DMIN, CTMAX, FICE0, FICEN,     &
                                 FICEL, PFMOVE, STEXU, STEYU, STEDU,   &
                                 IICEHMIN, IICEHINIT, ICESCALES(:),    &
                                 IICEHFAC, IICEHDISP, IICEDDISP, IICEFDISP, &
                                 BTBETA, AAIRCMIN, AAIRGB
      REAL(8),POINTER         :: GRIDSHIFT ! see notes in WMGHGH
#ifdef W3_RTD
        REAL, POINTER         :: PoLat, PoLon
        REAL, POINTER         :: AnglD(:)
        LOGICAL, POINTER      :: FLAGUNR
#endif
      REAL   , POINTER :: ZB(:), CLATS(:)
      REAL   , POINTER :: CLATIS(:) ! INVERSE OF COS(LAT) DEFINED ON ISEA
      REAL   , POINTER :: CTHG0S(:) ! TAN(Y)/R, DEFINED ON ISEA

      REAL   , POINTER :: TRNX(:,:), TRNY(:,:) ! TRANSPARENCY INFORMATION ON IX,IY
#ifdef W3_SMC
        REAL, POINTER :: CTRNX(:), CTRNY(:), CLATF(:)
#endif
      REAL   , POINTER :: SPCBAC(:,:), ANGARC(:)
      REAL   , POINTER :: XGRD(:,:), YGRD(:,:) ! X AND Y DEFINED ON IX,IY
      REAL   , POINTER :: DXDP(:,:), DXDQ(:,:) ! DX/DP & DX/DQ DEFINED ON IX,IY
      REAL   , POINTER :: DYDP(:,:), DYDQ(:,:) ! DY/DP & DY/DQ DEFINED ON IX,IY
      REAL   , POINTER :: DPDX(:,:), DPDY(:,:) ! DP/DX & DP/DY DEFINED ON IX,IY
      REAL   , POINTER :: DQDX(:,:), DQDY(:,:) ! DQ/DX & DQ/DY DEFINED ON IX,IY
      REAL   , POINTER :: GSQRT(:,:) ! SQRT(G) DEFINED ON IX,IY
      REAL   , POINTER :: HPFAC(:,:) ! H_P = SQRT(G_PP) DEFINED ON IX,IY
      REAL   , POINTER :: HQFAC(:,:) ! H_Q = SQRT(G_QQ) DEFINED ON IX,IY
#ifdef W3_BT4
        REAL, POINTER         :: SED_D50(:), SED_PSIC(:)
#endif

      LOGICAL, POINTER :: GINIT, FLDRY, FLCX, FLCY, FLCTH, FLCK, FLSOU, IICEDISP,&
                          IICESMOOTH
      LOGICAL, POINTER :: FLAGLL
      LOGICAL, POINTER :: CMPRTRCK
      LOGICAL, POINTER :: FLAGST(:)

      CHARACTER(LEN=30), POINTER :: GNAME
      CHARACTER(LEN=13), POINTER :: FILEXT

      TYPE(T_GSU), POINTER :: GSU ! Grid search utility object
!/
!/ Data aliasses for structure SGRD(S)
!/
      INTEGER, POINTER        :: NK, NK2, NTH, NSPEC
      INTEGER, POINTER        :: MAPWN(:), MAPTH(:)
      REAL, POINTER           :: DTH, XFR, FR1, FTE, FTF, FTWN, FTTR, &
                                 FTWL, FACTI1, FACTI2, FACHFA, FACHFE
      REAL, POINTER           :: TH(:), ESIN(:), ECOS(:), ES2(:),     &
                                 ESC(:), EC2(:), SIG(:), SIG2(:),     &
                                 DSIP(:), DSII(:), DDEN(:), DDEN2(:)
      LOGICAL, POINTER        :: SINIT
!/
!/ Data aliasses for structure MPAR(S)
!/
      LOGICAL, POINTER        :: PINIT
!/
!/ Data aliasses for structure NPAR(S)
!/
      REAL, POINTER           :: FACP, XREL, XFLT, FXFM, FXPM,        &
                                 XFT, XFC, FACSD, FHMAX
#ifdef W3_RWND
 REAL, POINTER           :: RWINDC
#endif
#ifdef W3_WCOR
 REAL, POINTER           :: WWCOR(:)
#endif
!/
!/ Data aliasses for structure PROP(S)
!/
#ifdef W3_PR2
      REAL, POINTER           :: DTME, CLATMN
#endif
#ifdef W3_PR3
      REAL, POINTER           :: WDCG, WDTH
#endif
#ifdef W3_SMC
      REAL, POINTER           :: DTMS, Refran
      LOGICAL, POINTER        :: FUNO3, FVERG, FSWND, ARCTC  
#endif
!/
!/ Data aliasses for structure FLDP(S)
!/
#ifdef W3_FLD1
     INTEGER, POINTER         :: TAIL_ID
     REAL, POINTER            :: TAIL_LEV, TAIL_TRAN1, TAIL_TRAN2
#endif
#ifdef W3_FLD2
     INTEGER, POINTER         :: TAIL_ID
     REAL, POINTER            :: TAIL_LEV, TAIL_TRAN1, TAIL_TRAN2
#endif
!/
!/ Data aliasses for structure SFLP(S)
!/
#ifdef W3_FLX2
      INTEGER, POINTER        :: NITTIN
      REAL, POINTER           :: CINXSI
#endif
#ifdef W3_FLX3
      INTEGER, POINTER        :: NITTIN, CAP_ID
      REAL, POINTER           :: CINXSI, CD_MAX
#endif
#ifdef W3_FLX4
      REAL, POINTER           :: FLX4A0
#endif
!/
!/ Data aliasses for structure SLNP(S)
!/
#ifdef W3_LN1
      REAL, POINTER           :: SLNC1, FSPM, FSHF
#endif
!/
!/ Data aliasses for structure SRCP(S)
!/
#ifdef W3_ST1
      REAL, POINTER           :: SINC1, SDSC1
#endif
#ifdef W3_ST2
      REAL, POINTER           :: ZWIND, FSWELL, SHSTAB,               &
                                 OFSTAB, CCNG, CCPS, FFNG, FFPS,      &
                                 CDSA0, CDSA1, CDSA2, SDSALN,         &
                                 CDSB0, CDSB1, CDSB2, CDSB3, FPIMIN,  &
                                 XFH, XF1, XF2
#endif
#ifdef W3_ST3
      REAL, POINTER           :: ZZWND, AALPHA, BBETA, ZZ0MAX, ZZ0RAT,&
                                 ZZALP, FFXFM, FFXPM,                 &
                                 SSINTHP, TTAUWSHELTER, SSWELLF(:),   &
                                 SSDSC1, SSDSC2, SSDSC3, SSDSBR,      &
                                 SSDSP, WWNMEANP, SSTXFTF, SSTXFTWN,  &
                                 SSDSC4, SSDSC5, SSDSC6, SSDSBT,      &
                                 DDELTA1, DDELTA2,                    &
                                 SSDSCOS, SSDSDTH, SSDSBM(:)
#endif
#ifdef W3_ST4
      INTEGER, POINTER        :: SSWELLFPAR, SSDSISO,SSDSBRFDF,       &
                                 IKTAB(:,:), SATINDICES(:,:),SSDSDIK
      REAL, POINTER           :: DCKI(:,:), SATWEIGHTS(:,:),CUMULW(:,:),QBI(:,:)
      REAL, POINTER           :: ZZWND, AALPHA, BBETA, ZZ0MAX, ZZ0RAT,&
                                 ZZALP, FFXFA,          &
                                 FFXFM, FFXPM, SSDSBRF1, SSDSBRF2,    &
                                 SSDSBINT, SSDSBCK, SSDSHCK, SSDSABK, &
                                 SSDSPBK, SSINBR,SSINTHP,TTAUWSHELTER,&
                                 SSWELLF(:), SSDSC(:), SSDSBR,        &
                                 SSDSP, WWNMEANP, SSTXFTF, SSTXFTWN,  &
                                 SSDSBT, SSDSCOS, SSDSDTH, SSDSBM(:)
#endif
#ifdef W3_ST6
      REAL, POINTER           :: SIN6A0, SDS6A1, SDS6A2, SWL6B1, &
                                 SIN6WS, SIN6FC
      INTEGER, POINTER        :: SDS6P1, SDS6P2
      LOGICAL, POINTER        :: SDS6ET, SWL6S6, SWL6CSTB1
#endif
      REAL, POINTER           :: WWNMEANPTAIL, SSTXFTFTAIL
!/
!/ Data aliasses for structure SNLP(S)
!/
#ifdef W3_NL1
      REAL, POINTER           :: SNLC1, LAM, KDCON, KDMN,             &
                                 SNLS1, SNLS2, SNLS3
#endif
#ifdef W3_NL2
      INTEGER, POINTER        :: IQTPE, NDPTHS
      REAL, POINTER           :: NLTAIL
      REAL, POINTER           :: DPTHNL(:)
#endif
#ifdef W3_NL3
      INTEGER, POINTER        :: NFRMIN, NFRMAX, NFRCUT, NTHMAX,      &
                                 NTHEXP, NSPMIN, NSPMAX, NSPMX2,      &
                                 NQA, SNLNQ
      INTEGER, POINTER        :: QST1(:,:,:), QST4(:,:,:)
      REAL, POINTER           :: SNLMSC, SNLNSC, SNLSFD, SNLSFS
      REAL, POINTER           :: FRQ(:), XSI(:),                      &
                                 QST2(:,:,:), QST3(:,:,:),            &
                                 QST5(:,:,:), QST6(:,:,:),            &
                                 SNLL(:), SNLM(:), SNLT(:),           &
                                 SNLCD(:), SNLCS(:)
#endif
#ifdef W3_NL4
      INTEGER, POINTER        :: ITSA, IALT
#endif
#ifdef W3_NL5
      REAL, POINTER           :: QR5DPT, QR5OML
      INTEGER, POINTER        :: QI5DIS, QI5KEV, QI5IPL, QI5PMX
      INTEGER(KIND=8), POINTER:: QI5NNZ
#endif
#ifdef W3_NLS
      INTEGER, POINTER        :: NTHX, NFRX, NSPL, NSPH
      REAL, POINTER           :: CNLSA, CNLSC, CNLSFM,                &
                                 CNLSC1, CNLSC2, CNLSC3, SNSST(:,:)
#endif
!/
!/ Data aliasses for structure SBTP(S)
!/
#ifdef W3_BT1
      REAL, POINTER           :: SBTC1
#endif
#ifdef W3_BT4
      REAL, POINTER           :: SBTCX(:)
#endif
!/
!/ Data aliasses for structure SDBP(S)
!/
#ifdef W3_DB1
      REAL, POINTER           :: SDBC1, SDBC2
      LOGICAL, POINTER        :: FDONLY
#endif
!/
#ifdef W3_UOST
!/ Data aliases for structure UOSTP(S)
      CHARACTER(LEN=:), POINTER   :: UOSTFILELOCAL, UOSTFILESHADOW
      REAL, POINTER           :: UOSTFACTORLOCAL, UOSTFACTORSHADOW
#endif
!/
!/ Data aliasing for structure SCHM(S)
      LOGICAL, POINTER  :: FSN,FSPSI,FSFCT,FSNIMP,FSTOTALIMP,FSTOTALEXP
      LOGICAL, POINTER  :: FSREFRACTION, FSFREQSHIFT, FSSOURCE, FSBCCFL
      LOGICAL, POINTER  :: DO_CHANGE_WLV
      REAL(8), POINTER  :: SOLVERTHR_STP
      REAL(8), POINTER  :: CRIT_DEP_STP
      LOGICAL, POINTER :: B_JGS_TERMINATE_MAXITER
      LOGICAL, POINTER :: B_JGS_TERMINATE_DIFFERENCE
      LOGICAL, POINTER :: B_JGS_TERMINATE_NORM
      LOGICAL, POINTER :: B_JGS_LIMITER
      LOGICAL, POINTER :: B_JGS_USE_JACOBI
      LOGICAL, POINTER :: B_JGS_BLOCK_GAUSS_SEIDEL
      INTEGER, POINTER :: B_JGS_MAXITER
      REAL(8), POINTER :: B_JGS_PMIN
      REAL(8), POINTER :: B_JGS_DIFF_THR
      REAL(8), POINTER :: B_JGS_NORM_THR
      INTEGER, POINTER :: B_JGS_NLEVEL
      LOGICAL, POINTER :: B_JGS_SOURCE_NONLINEAR
!/
!/ Data aliasing for structure SICP(S)
#ifdef W3_IS1
      REAL, POINTER           :: IS1C1, IS1C2
#endif
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3NMOD ( NUMBER, NDSE, NDST, NAUX )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         10-Dec-2014 !
!/                  +-----------------------------------+
!/
!/    24-Feb-2004 : Origination.                        ( version 3.06 )
!/    18-Jul-2006 : Add input grids.                    ( version 3.10 )
!/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
!/
!  1. Purpose :
!
!     Set up the number of grids to be used.
!
!  2. Method :
!
!     Store in NGRIDS and allocate GRIDS.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NUMBER  Int.   I   Number of grids to be used.
!       NDSE    Int.   I   Error output unit number.
!       NDST    Int.   I   Test output unit number.
!       NAUX    Int.   I   Number of auxiliary grids to be used.
!                          Grids -NAUX:NUBMER are defined, optional
!                          parameters.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!     Any program that uses this grid structure.
!
!  6. Error messages :
!
!     - Error checks on previous setting of variable.
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
!     !/S    Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)           :: NUMBER, NDSE, NDST
      INTEGER, INTENT(IN), OPTIONAL :: NAUX
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: I, NLOW
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3NMOD')
#endif
!
! -------------------------------------------------------------------- /
! 1.  Test input and module status
!
      IF ( NGRIDS .NE. -1 ) THEN
          WRITE (NDSE,1001) NGRIDS
          CALL EXTCDE (1)
        END IF
!
      IF ( NUMBER .LT. 1 ) THEN
          WRITE (NDSE,1002) NUMBER
          CALL EXTCDE (2)
        END IF
!
      IF ( PRESENT(NAUX) ) THEN
          NLOW   = -NAUX
        ELSE
          NLOW   = 1
        END IF
!
      IF ( NLOW .GT. 1 ) THEN
          WRITE (NDSE,1003) -NLOW
          CALL EXTCDE (3)
        END IF
!
! -------------------------------------------------------------------- /
! 1.  Set variable and allocate arrays
!
      NGRIDS = NUMBER
      NAUXGR = - NLOW
      ALLOCATE ( GRIDS(NLOW:NUMBER), &
                 SGRDS(NLOW:NUMBER), &
                 MPARS(NLOW:NUMBER), &
                 STAT=ISTAT )
      CHECK_ALLOC_STATUS ( ISTAT )
!
! -------------------------------------------------------------------- /
! 2.  Initialize GINIT and SINIT
!
      DO I=NLOW, NUMBER
        GRIDS(I)%GINIT  = .FALSE.
        GRIDS(I)%GUGINIT  = .FALSE.
        SGRDS(I)%SINIT  = .FALSE.
        MPARS(I)%PINIT  = .FALSE.
#ifdef W3_NL2
        MPARS(I)%SNLPS%NDPTHS = 0
#endif
        END DO
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3NMOD)
      WRITE (NDST,9000) NLOW, NGRIDS
#endif
!
      RETURN
!
! Formats
!
 1001 FORMAT (/' *** ERROR W3NMOD : GRIDS ALREADY INITIALIZED *** '/  &
               '                    NGRIDS = ',I10/)
 1002 FORMAT (/' *** ERROR W3NMOD : ILLEGAL NUMBER OF GRIDS *** '/    &
               '                    NUMBER = ',I10/)
 1003 FORMAT (/' *** ERROR W3NMOD : ILLEGAL NUMBER OF AUX GRIDS *** '/&
               '                    NUMBER = ',I10/)
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3NMOD)
 9000 FORMAT (' TEST W3NMOD : SETTING UP FOR GRIDS ',I3,           &
              ' THROUGH ',I3)
#endif
!/
!/ End of W3NMOD ----------------------------------------------------- /
!/
      END SUBROUTINE W3NMOD
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3DIMX  ( IMOD, MX, MY, MSEA, NDSE, NDST   & 
#ifdef W3_SMC
                    , MCel, MUFc, MVFc, MRLv, MBSMC    &
                    , MARC, MBAC, MSPEC                &
#endif
                         )
#ifdef W3_SMC
  !!Li    A few dimensional numbers for SMC grid.  
#endif
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         10-Dec-2014 |
!/                  +-----------------------------------+
!/
!/    24-Jun-2005 : Origination.                        ( version 3.07 )
!/    18-Jul-2006 : Add input grids.                    ( version 3.10 )
!/    05-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
!/    02-Feb-2007 : Add FLAGST.                         ( version 3.10 )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement unstructured grids        ( version 3.14.1)
!/    03-Sep-2012 : Clean up of UG grids                ( version 4.08 )
!/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
!/
!  1. Purpose :
!
!     Initialize an individual spatial grid at the proper dimensions.
!
!  2. Method :
!
!     Allocate directly into the structure array GRIDS. Note that
!     this cannot be done through the pointer alias!
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number to point to.
!       NDSE    Int.   I   Error output unit number.
!       NDST    Int.   I   Test output unit number.
!       MX, MY, MSEA       Like NX, NY, NSEA in data structure.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!       See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3IOGR    Subr. W3IOGRMD Model definition file IO program.
!      WW3_GRID  Prog.   N/A    Model set up program.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     - Check on input parameters.
!     - Check on previous allocation.
!
!  7. Remarks :
!
!     - Grid dimensions apre passed through parameter list and then 
!       locally stored to assure consistency between allocation and
!       data in structure.
!     - W3SETG needs to be called after allocation to point to 
!       proper allocated arrays.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S    Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IMOD, MX, MY, MSEA, NDSE, NDST
#ifdef W3_SMC
       INTEGER, INTENT(IN)     :: MCel, MUFc, MVFc, MRLv, MBSMC 
       INTEGER, INTENT(IN)     :: MARC, MBAC, MSPEC 
#endif
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_SMC
       INTEGER                 :: IARC, IBAC, IBSMC
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3DIMX')
#endif
!
! -------------------------------------------------------------------- /
! 1.  Test input and module status
!
      IF ( NGRIDS .EQ. -1 ) THEN
          WRITE (NDSE,1001)
          CALL EXTCDE (1)
        END IF
!
      IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NGRIDS ) THEN
          WRITE (NDSE,1002) IMOD, -NAUXGR, NGRIDS
          CALL EXTCDE (2)
        END IF
!
      IF ( MX.LT.3 .OR. (MY.LT.3.AND.GTYPE.NE.UNGTYPE) .OR. MSEA.LT.1 ) THEN
        WRITE (NDSE,1003) MX, MY, MSEA, GTYPE
        CALL EXTCDE (3)
        END IF
!
      IF ( GRIDS(IMOD)%GINIT ) THEN
          WRITE (NDSE,1004)
          CALL EXTCDE (4)
        END IF
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMX)
      WRITE (NDST,9000) IMOD, MX, MY, MSEA
#endif
!
! -------------------------------------------------------------------- /
! 2.  Allocate arrays
!
! NB: Some array start at 0 because MAPFS(IY,IX)=0 for missing points 
!
      ALLOCATE ( GRIDS(IMOD)%MAPSTA(MY,MX),  &
                 GRIDS(IMOD)%MAPST2(MY,MX),  &
                 GRIDS(IMOD)%MAPFS(MY,MX),   &
                 GRIDS(IMOD)%MAPSF(MSEA,3),  &
                 GRIDS(IMOD)%FLAGST(MSEA),   &
#ifdef W3_RTD
                 GRIDS(IMOD)%AnglD(MSEA),    &
#endif
                 GRIDS(IMOD)%ZB(MSEA),       &
                 GRIDS(IMOD)%CLATS(0:MSEA),  &
                 GRIDS(IMOD)%CLATIS(0:MSEA), &
                 GRIDS(IMOD)%CTHG0S(0:MSEA), &
                 GRIDS(IMOD)%TRNX(MY,MX),    &
                 GRIDS(IMOD)%TRNY(MY,MX),    &
                 GRIDS(IMOD)%XGRD(MY,MX),    &
                 GRIDS(IMOD)%YGRD(MY,MX),    &
                 GRIDS(IMOD)%DXDP(MY,MX),    &
                 GRIDS(IMOD)%DXDQ(MY,MX),    &
                 GRIDS(IMOD)%DYDP(MY,MX),    &
                 GRIDS(IMOD)%DYDQ(MY,MX),    &
                 GRIDS(IMOD)%DPDX(MY,MX),    &
                 GRIDS(IMOD)%DPDY(MY,MX),    &
                 GRIDS(IMOD)%DQDX(MY,MX),    &
                 GRIDS(IMOD)%DQDY(MY,MX),    &
                 GRIDS(IMOD)%GSQRT(MY,MX),   &
                 GRIDS(IMOD)%HPFAC(MY,MX),   &
                 GRIDS(IMOD)%HQFAC(MY,MX),   &
                 STAT=ISTAT                  )
      CHECK_ALLOC_STATUS ( ISTAT )
!!/DEBUGINIT         WRITE(740+IAPROC,*) 'After alocation of MAPST2, MY=', MY, ' MX=', MX
!!/DEBUGINIT         FLUSH(740+IAPROC)
#ifdef W3_BT4
    ALLOCATE ( GRIDS(IMOD)%SED_D50(0:MSEA), &
               GRIDS(IMOD)%SED_PSIC(0:MSEA),&
                 STAT=ISTAT                 )
      CHECK_ALLOC_STATUS ( ISTAT )
#endif
!
#ifdef W3_SMC
      ALLOCATE ( GRIDS(IMOD)%NLvCel(0:MRLv),     &
                 GRIDS(IMOD)%NLvUFc(0:MRLv),     &
                 GRIDS(IMOD)%NLvVFc(0:MRLv),     &
                 GRIDS(IMOD)%IJKCel(5, -9:MCel), &
                 GRIDS(IMOD)%IJKUFc(7,MUFc),     &
                 GRIDS(IMOD)%IJKVFc(8,MVFc),     &
                 GRIDS(IMOD)%CTRNX(-9:MCel),     &  
                 GRIDS(IMOD)%CTRNY(-9:MCel),     &
                 GRIDS(IMOD)%CLATF(MVFc),        &
                 STAT=ISTAT                      )
      CHECK_ALLOC_STATUS ( ISTAT )
#endif
!
#ifdef W3_SMC
 !! Arctic part related variables, declare minimum 1 element.
      IARC = MARC
      IF( MARC .LE. 1 ) IARC = 1
      IBAC = MBAC
      IF( MBAC .LE. 1 ) IBAC = 1
      IBSMC = MBSMC
      IF( MBSMC .LE. 1 ) IBSMC = 1
      ALLOCATE ( GRIDS(IMOD)%ICLBAC(IBAC),       &
                 GRIDS(IMOD)%ANGARC(IARC),       &
                 GRIDS(IMOD)%SPCBAC(MSPEC,IBAC), &
                 GRIDS(IMOD)%ISMCBP(IBSMC),      &
                 STAT=ISTAT                      )
      CHECK_ALLOC_STATUS ( ISTAT )
#endif
!
#ifdef W3_SMC
 !! All SMC grid related varialbes are initialised in case SMC  
 !! switch is selected but SMCTYPE is not used.  JGLi08Mar2021 
      GRIDS(IMOD)%NLvCel(:) = 0 
      GRIDS(IMOD)%NLvUFc(:) = 0
      GRIDS(IMOD)%NLvVFc(:) = 0 
      GRIDS(IMOD)%ISMCBP(:) = 0
      GRIDS(IMOD)%ICLBAC(:) = 0
      GRIDS(IMOD)%IJKCel(:,:) = 0 
      GRIDS(IMOD)%IJKUFc(:,:) = 0 
      GRIDS(IMOD)%IJKVFc(:,:) = 0 
      GRIDS(IMOD)%CTRNX(:)  = 0.0 
      GRIDS(IMOD)%CTRNY(:)  = 0.0
      GRIDS(IMOD)%CLATF(:)  = 0.0 
      GRIDS(IMOD)%ANGARC(:) = 0.0
#endif
!
      GRIDS(IMOD)%FLAGST = .TRUE.
      GRIDS(IMOD)%GINIT  = .TRUE.
      GRIDS(IMOD)%MAPSF(:,3)=0.
      GRIDS(IMOD)%CLATS(0)=1.
      GRIDS(IMOD)%CLATIS(0)=1.
      GRIDS(IMOD)%CTHG0S(0)=1.
!
#ifdef W3_REF1
      ALLOCATE ( GRIDS(IMOD)%RREF(4),     &
                 GRIDS(IMOD)%REFPARS(10), &
                 STAT=ISTAT               )
      CHECK_ALLOC_STATUS ( ISTAT )
#endif
!
#ifdef W3_REF1
      GRIDS(IMOD)%RREF(:)=.FALSE.
      GRIDS(IMOD)%REFPARS(:)=0.
#endif
!
#ifdef W3_REF1
! Memory footprint can be reduced by defining REFLC and REFLD only over nodes 
! where reflection can occur. 
      ALLOCATE ( GRIDS(IMOD)%REFLC(4,0:NSEA), &
                 GRIDS(IMOD)%REFLD(6,0:NSEA), &
                 STAT=ISTAT                    )
      CHECK_ALLOC_STATUS ( ISTAT )
#endif
#ifdef W3_IG1
       ALLOCATE ( GRIDS(IMOD)%IGPARS(12), STAT=ISTAT )
       CHECK_ALLOC_STATUS ( ISTAT )
#endif
#ifdef W3_IC2
       ALLOCATE ( GRIDS(IMOD)%IC2PARS(9), STAT=ISTAT )
       CHECK_ALLOC_STATUS ( ISTAT )
#endif
#ifdef W3_IC3
       ALLOCATE ( GRIDS(IMOD)%IC3PARS(16), STAT=ISTAT )
       CHECK_ALLOC_STATUS ( ISTAT )
#endif

#ifdef W3_IC4
       ALLOCATE ( GRIDS(IMOD)%IC4PARS(1), STAT=ISTAT )
       CHECK_ALLOC_STATUS ( ISTAT )
       ALLOCATE ( GRIDS(IMOD)%IC4_KI(NIC4), STAT=ISTAT )
       CHECK_ALLOC_STATUS ( ISTAT )
       ALLOCATE ( GRIDS(IMOD)%IC4_FC(NIC4), STAT=ISTAT )
       CHECK_ALLOC_STATUS ( ISTAT )
#endif
#ifdef W3_IC5
       ALLOCATE ( GRIDS(IMOD)%IC5PARS(9), STAT=ISTAT )
       CHECK_ALLOC_STATUS ( ISTAT )
#endif
#ifdef W3_IS2
       ALLOCATE ( GRIDS(IMOD)%IS2PARS(24), STAT=ISTAT )
       CHECK_ALLOC_STATUS ( ISTAT )
#endif
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMX)
      WRITE (NDST,9001)
#endif
!
#ifdef W3_REF1
      GRIDS(IMOD)%REFLC(1:4,0:NSEA)=0.
      GRIDS(IMOD)%REFLD(:,:)=0
#endif
#ifdef W3_IG1
      GRIDS(IMOD)%IGPARS(:)=0.
#endif
#ifdef W3_IC2
      GRIDS(IMOD)%IC2PARS(:)=0.
#endif
#ifdef W3_IS2
      GRIDS(IMOD)%IS2PARS(:)=0.
#endif
!
! -------------------------------------------------------------------- /
! 2.  Update counters in grid
!
      GRIDS(IMOD)%NX     = MX
      GRIDS(IMOD)%NY     = MY
      GRIDS(IMOD)%NSEA   = MSEA
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMX)
      WRITE (NDST,9002)
#endif
!
! -------------------------------------------------------------------- /
! 3.  Point to allocated arrays
!
      CALL W3SETG ( IMOD, NDSE, NDST )
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMX)
      WRITE (NDST,9003)
#endif
!
      RETURN
!
! Formats
!
 1001 FORMAT (/' *** ERROR W3DIMX : GRIDS NOT INITIALIZED *** '/      &
               '                    RUN W3NMOD FIRST '/)
 1002 FORMAT (/' *** ERROR W3DIMX : ILLEGAL MODEL NUMBER *** '/       &
               '                    IMOD   = ',I10/                   &
               '                    NAUXGR = ',I10/                   &
               '                    NGRIDS = ',I10/)
 1003 FORMAT (/' *** ERROR W3DIMX : ILLEGAL GRID DIMENSION(S) *** '/  &
               '                    INPUT = ',4I10 /)
 1004 FORMAT (/' *** ERROR W3DIMX : ARRAY(S) ALREADY ALLOCATED *** ')
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMX)
 9000 FORMAT (' TEST W3DIMX : MODEL ',I4,' DIM. AT ',2I5,I7)
 9001 FORMAT (' TEST W3DIMX : ARRAYS ALLOCATED')
 9002 FORMAT (' TEST W3DIMX : DIMENSIONS STORED')
 9003 FORMAT (' TEST W3DIMX : POINTERS RESET')
#endif
!/
!/ End of W3DIMX ----------------------------------------------------- /
!/
      END SUBROUTINE W3DIMX
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3DIMS  ( IMOD, MK, MTH, NDSE, NDST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         10-Dec-2014 !
!/                  +-----------------------------------+
!/
!/    19-Feb-2004 : Origination.                        ( version 3.06 )
!/    18-Jul-2006 : Add input grids.                    ( version 3.10 )
!/    05-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
!/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
!/
!  1. Purpose :
!
!     Initialize an individual spatial grid at the proper dimensions.
!
!  2. Method :
!
!     Allocate directly into the structure array GRIDS. Note that
!     this cannot be done through the pointer alias!
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number to point to.
!       NDSE    Int.   I   Error output unit number.
!       MK,MTH  Int.   I   Spectral dimensions.
!       NDST    Int.   I   Test output unit number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3IOGR    Subr. W3IOGRMD Model definition file IO program.
!      WW3_GRID  Prog.   N/A    Model set up program.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     - Check on input parameters.
!     - Check on previous allocation.
!
!  7. Remarks :
!
!     - Grid dimensions apre passed through parameter list and then 
!       locally stored to assure consistency between allocation and
!       data in structure.
!     - W3SETG needs to be called after allocation to point to 
!       proper allocated arrays.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S    Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_ST4
  USE CONSTANTS, ONLY: RADE
#endif
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IMOD, MK, MTH, NDSE, NDST
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER, SAVE           :: MK2, MSPEC
#ifdef W3_ST4
  INTEGER                  :: SDSNTH
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3DIMS')
#endif
!
! -------------------------------------------------------------------- /
! 1.  Test input and module status
!
      IF ( NGRIDS .EQ. -1 ) THEN
          WRITE (NDSE,1001)
          CALL EXTCDE (1)
        END IF
!
      IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NGRIDS ) THEN
          WRITE (NDSE,1002) IMOD, -NAUXGR, NGRIDS
          CALL EXTCDE (2)
        END IF
!
      IF ( MK.LT.3 .OR. MTH.LT.4 ) THEN
          WRITE (NDSE,1003) MK, MTH
          CALL EXTCDE (3)
        END IF
!
      IF ( SGRDS(IMOD)%SINIT ) THEN
          WRITE (NDSE,1004)
          CALL EXTCDE (4)
        END IF
!
      MK2    = MK + 2
      MSPEC  = MK * MTH
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMS)
      WRITE (NDST,9000) IMOD, MTH, MK, MK2, MSPEC
#endif
!
! -------------------------------------------------------------------- /
! 2.  Allocate arrays
!
      ALLOCATE ( SGRDS(IMOD)%MAPWN(MSPEC+MTH),                        &
                 SGRDS(IMOD)%MAPTH(MSPEC+MTH),                        &
                 SGRDS(IMOD)%TH(MTH),                                 &
                 SGRDS(IMOD)%ESIN(MSPEC+MTH),                         &
                 SGRDS(IMOD)%ECOS(MSPEC+MTH),                         &
                 SGRDS(IMOD)%ES2(MSPEC+MTH),                          &
                 SGRDS(IMOD)%ESC(MSPEC+MTH),                          &
                 SGRDS(IMOD)%EC2(MSPEC+MTH),                          &
                 SGRDS(IMOD)%SIG(0:MK+1),                             &
                 SGRDS(IMOD)%SIG2(MSPEC),                             &
                 SGRDS(IMOD)%DSIP(0:MK+1),                            &
                 SGRDS(IMOD)%DSII(MK),                                &
                 SGRDS(IMOD)%DDEN(MK),                                &
                 SGRDS(IMOD)%DDEN2(MSPEC),                            &
                 STAT=ISTAT                                           )
      CHECK_ALLOC_STATUS ( ISTAT )
      SGRDS(IMOD)%MAPWN(:)=0.
      SGRDS(IMOD)%MAPTH(:)=0.
      SGRDS(IMOD)%TH(:)=0.
      SGRDS(IMOD)%ESIN(:)=0.
      SGRDS(IMOD)%ECOS(:)=0.
      SGRDS(IMOD)%ES2(:)=0.
      SGRDS(IMOD)%ESC(:)=0.
      SGRDS(IMOD)%EC2(:)=0.
      SGRDS(IMOD)%SIG(:)=0.
      SGRDS(IMOD)%SIG2(:)=0.
      SGRDS(IMOD)%DSIP(:)=0.
      SGRDS(IMOD)%DSII(:)=0.
      SGRDS(IMOD)%DDEN(:)=0.
      SGRDS(IMOD)%DDEN2(:)=0.
#ifdef W3_ST4
      ALLOCATE ( MPARS(IMOD)%SRCPS%IKTAB(MK,NDTAB), &
                 MPARS(IMOD)%SRCPS%DCKI(NKHS,NKD),  &
                 MPARS(IMOD)%SRCPS%QBI(NKHS,NKD),   &
                 STAT=ISTAT                         )
      CHECK_ALLOC_STATUS ( ISTAT )
      SDSNTH  = MTH/2-1 !MIN(NINT(SSDSDTH/(DTH*RADE)),MTH/2-1)
      ALLOCATE( MPARS(IMOD)%SRCPS%SATINDICES(2*SDSNTH+1,MTH), &
                MPARS(IMOD)%SRCPS%SATWEIGHTS(2*SDSNTH+1,MTH), &
                MPARS(IMOD)%SRCPS%CUMULW(MSPEC,MSPEC),        &
                 STAT=ISTAT                                   )
      CHECK_ALLOC_STATUS ( ISTAT )
#endif
!
      SGRDS(IMOD)%SINIT  = .TRUE.
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMS)
      WRITE (NDST,9001)
#endif
!
! -------------------------------------------------------------------- /
! 3.  Point to allocated arrays
!
      CALL W3SETG ( IMOD, NDSE, NDST )
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMS)
      WRITE (NDST,9002)
#endif
!
! -------------------------------------------------------------------- /
! 4.  Update counters in grid
!
      NK     = MK
      NK2    = MK + 2
      NTH    = MTH
      NSPEC  = MK * MTH
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMS)
      WRITE (NDST,9003)
#endif
!
      RETURN
!
! Formats
!
 1001 FORMAT (/' *** ERROR W3DIMS : GRIDS NOT INITIALIZED *** '/      &
               '                    RUN W3NMOD FIRST '/)
 1002 FORMAT (/' *** ERROR W3DIMS : ILLEGAL MODEL NUMBER *** '/       &
               '                    IMOD   = ',I10/                   &
               '                    NAUXGR = ',I10/                   &
               '                    NGRIDS = ',I10/)
 1003 FORMAT (/' *** ERROR W3DIMS : ILLEGAL GRID DIMENSION(S) *** '/  &
               '                    INPUT = ',4I10/)
 1004 FORMAT (/' *** ERROR W3DIMS : ARRAY(S) ALREADY ALLOCATED *** ')
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMS)
 9000 FORMAT (' TEST W3DIMS : MODEL ',I4,' DIM. AT ',3I5,I7)
 9001 FORMAT (' TEST W3DIMS : ARRAYS ALLOCATED')
 9002 FORMAT (' TEST W3DIMS : POINTERS RESET')
 9003 FORMAT (' TEST W3DIMS : DIMENSIONS STORED')
#endif
!/
!/ End of W3DIMS ----------------------------------------------------- /
!/
      END SUBROUTINE W3DIMS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3SETG ( IMOD, NDSE, NDST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  !           J. H. Alves             !
!/                  |                        FORTRAN 90 |
!/                  | Last update :         03-Sep-2012 |
!/                  +-----------------------------------+
!/
!/    24-Jun-2005 : Origination.                        ( version 3.07 )
!/    09-Nov-2005 : Remove soft boundary options.       ( version 3.08 )
!/    23-Jun-2006 : Add data for W3SLN1.                ( version 3.09 )
!/    18-Jul-2006 : Add input grids.                    ( version 3.10 )
!/    05-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
!/    02-Feb-2007 : Add FLAGST.                         ( version 3.10 )
!/    14-Apr-2007 : Add Miche style limiter.            ( version 3.11 )
!/                  ( J. H. Alves )
!/    25-Apr-2007 : Adding Battjes-Janssen Sdb.         ( version 3.11 )
!/                  ( J. H. Alves )
!/    18-Sep-2007 : Adding WAM4 source terms.           ( version 3.13 )
!/                  ( F. Ardhuin )
!/    27-Jun-2008 : Expand WAM4 variants namelist       ( version 3.14 )
!/                  ( F. Ardhuin )
!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    13-Jul-2012 : Move data structures GMD (SNL3) and nonlinear
!/                  filter (SNLS) from 3.15 (HLT).      ( version 4.08 )
!/    03-Sep-2012 : Clean up of UG grids                ( version 4.08 )
!/
!  1. Purpose :
!
!     Select one of the WAVEWATCH III grids / models.
!
!  2. Method :
!
!     Point pointers to the proper variables in the proper element of
!     the GRIDS array.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number to point to.
!       NDSE    Int.   I   Error output unit number.
!       NDST    Int.   I   Test output unit number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!     Many subroutines in eth WAVEWATCH system.
!
!  6. Error messages :
!
!     Checks on parameter list IMOD.
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
!     !/PRn  Select propagation scheme
!
!     !/STn  Select source terms
!     !/NLn
!     !/BTn
!
!     !/S    Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IMOD, NDSE, NDST
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3SETG')
#endif
!
! -------------------------------------------------------------------- /
! 1.  Test input and module status
!
      IF ( NGRIDS .EQ. -1 ) THEN
          WRITE (NDSE,1001)
          CALL EXTCDE (1)
        END IF
!
      IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NGRIDS ) THEN
          WRITE (NDSE,1002) IMOD, -NAUXGR, NGRIDS
          CALL EXTCDE (2)
        END IF
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3SETG)
      WRITE (NDST,9000) IMOD
#endif
!
! -------------------------------------------------------------------- /
! 2.  Set model numbers
!
      IGRID  = IMOD
      ISGRD  = IMOD
      IPARS  = IMOD
!
! -------------------------------------------------------------------- /
! 3.  Set pointers in structure GRID
!
      GTYPE  => GRIDS(IMOD)%GTYPE
      RSTYPE => GRIDS(IMOD)%RSTYPE
      ICLOSE => GRIDS(IMOD)%ICLOSE
!
      NX     => GRIDS(IMOD)%NX
      NY     => GRIDS(IMOD)%NY
      NSEA   => GRIDS(IMOD)%NSEA
      NSEAL  => GRIDS(IMOD)%NSEAL
      TRFLAG => GRIDS(IMOD)%TRFLAG
      FLAGLL => GRIDS(IMOD)%FLAGLL
!
#ifdef W3_SMC
      NCel   => GRIDS(IMOD)%NCel 
      NUFc   => GRIDS(IMOD)%NUFc 
      NVFc   => GRIDS(IMOD)%NVFc 
      NRLv   => GRIDS(IMOD)%NRLv 
      MRFct  => GRIDS(IMOD)%MRFct 
#endif
!
#ifdef W3_SMC
      NGLO   => GRIDS(IMOD)%NGLO 
      NARC   => GRIDS(IMOD)%NARC 
      NBGL   => GRIDS(IMOD)%NBGL 
      NBAC   => GRIDS(IMOD)%NBAC 
      NBSMC  => GRIDS(IMOD)%NBSMC
#endif
!
      E3DF   => GRIDS(IMOD)%E3DF
      P2MSF  => GRIDS(IMOD)%P2MSF
      US3DF  => GRIDS(IMOD)%US3DF
      USSPF  => GRIDS(IMOD)%USSPF
      USSP_WN => GRIDS(IMOD)%USSP_WN
#ifdef W3_REF1
      REFLC  => GRIDS(IMOD)%REFLC
      REFLD  => GRIDS(IMOD)%REFLD
#endif
      FFACBERG => GRIDS(IMOD)%FFACBERG
#ifdef W3_REF1
      RREF   => GRIDS(IMOD)%RREF
      REFPARS=> GRIDS(IMOD)%REFPARS
#endif
#ifdef W3_IG1
      IGPARS => GRIDS(IMOD)%IGPARS
#endif
#ifdef W3_IC2
      IC2PARS => GRIDS(IMOD)%IC2PARS
#endif
#ifdef W3_IC3
      IC3PARS => GRIDS(IMOD)%IC3PARS
#endif
#ifdef W3_IC4
      IC4PARS => GRIDS(IMOD)%IC4PARS
      IC4_KI => GRIDS(IMOD)%IC4_KI
      IC4_FC => GRIDS(IMOD)%IC4_FC
#endif
#ifdef W3_IC5
      IC5PARS => GRIDS(IMOD)%IC5PARS
#endif
#ifdef W3_IS2
      IS2PARS => GRIDS(IMOD)%IS2PARS
#endif
      SX     => GRIDS(IMOD)%SX
      SY     => GRIDS(IMOD)%SY
      X0     => GRIDS(IMOD)%X0
      Y0     => GRIDS(IMOD)%Y0
!
      DTCFL  => GRIDS(IMOD)%DTCFL
      DTCFLI => GRIDS(IMOD)%DTCFLI
      DTMAX  => GRIDS(IMOD)%DTMAX
      DTMIN  => GRIDS(IMOD)%DTMIN
      DMIN   => GRIDS(IMOD)%DMIN
#ifdef W3_SEC1
      NITERSEC1  => GRIDS(IMOD)%NITERSEC1
#endif
      CTMAX  => GRIDS(IMOD)%CTMAX
      FICE0  => GRIDS(IMOD)%FICE0
      GRIDSHIFT  => GRIDS(IMOD)%GRIDSHIFT
      CMPRTRCK => GRIDS(IMOD)%CMPRTRCK
#ifdef W3_RTD
      PoLat  => GRIDS(IMOD)%PoLat
      PoLon  => GRIDS(IMOD)%PoLon
      FLAGUNR  => GRIDS(IMOD)%FLAGUNR
#endif
      FICEN  => GRIDS(IMOD)%FICEN
      FICEL  => GRIDS(IMOD)%FICEL
      IICEHMIN  => GRIDS(IMOD)%IICEHMIN
      IICEHDISP  => GRIDS(IMOD)%IICEHDISP
      IICEFDISP  => GRIDS(IMOD)%IICEFDISP
      IICEDDISP  => GRIDS(IMOD)%IICEDDISP
      IICEHFAC  => GRIDS(IMOD)%IICEHFAC
      IICEHINIT  => GRIDS(IMOD)%IICEHINIT
      ICESCALES  => GRIDS(IMOD)%ICESCALES
      PFMOVE => GRIDS(IMOD)%PFMOVE
      STEXU  => GRIDS(IMOD)%STEXU
      STEYU  => GRIDS(IMOD)%STEYU
      STEDU  => GRIDS(IMOD)%STEDU
      BTBETA => GRIDS(IMOD)%BTBETA
      AAIRGB => GRIDS(IMOD)%AAIRGB
      AAIRCMIN => GRIDS(IMOD)%AAIRCMIN
!
      GINIT  => GRIDS(IMOD)%GINIT
      GUGINIT  => GRIDS(IMOD)%GUGINIT      
      FLDRY  => GRIDS(IMOD)%FLDRY
      FLCX   => GRIDS(IMOD)%FLCX
      FLCY   => GRIDS(IMOD)%FLCY
      FLCTH  => GRIDS(IMOD)%FLCTH
      FLCK   => GRIDS(IMOD)%FLCK
      FLSOU  => GRIDS(IMOD)%FLSOU
      IICEDISP => GRIDS(IMOD)%IICEDISP
      IICESMOOTH => GRIDS(IMOD)%IICESMOOTH
!
      GNAME  => GRIDS(IMOD)%GNAME
      FILEXT => GRIDS(IMOD)%FILEXT
      XYB    => GRIDS(IMOD)%XYB
      TRIGP  => GRIDS(IMOD)%TRIGP
#ifdef W3_PDLIB
      NBND_MAP => GRIDS(IMOD)%NBND_MAP
      INDEX_MAP => GRIDS(IMOD)%INDEX_MAP
      MAPSTA_LOC => GRIDS(IMOD)%MAPSTA_LOC
#endif
      NTRI     => GRIDS(IMOD)%NTRI
      COUNTRI     => GRIDS(IMOD)%COUNTRI
      SI     => GRIDS(IMOD)%SI
      COUNTOT    => GRIDS(IMOD)%COUNTOT
      IEN     => GRIDS(IMOD)%IEN
      LEN     => GRIDS(IMOD)%LEN 
      ANGLE     => GRIDS(IMOD)%ANGLE 
      ANGLE0     => GRIDS(IMOD)%ANGLE0
      CCON     => GRIDS(IMOD)%CCON
      COUNTCON     => GRIDS(IMOD)%COUNTCON
      INDEX_CELL  => GRIDS(IMOD)%INDEX_CELL
      IE_CELL     => GRIDS(IMOD)%IE_CELL
      POS_CELL     => GRIDS(IMOD)%POS_CELL
      IOBP     => GRIDS(IMOD)%IOBP
      IAA      => GRIDS(IMOD)%IAA
      JAA      => GRIDS(IMOD)%JAA
      POSI     => GRIDS(IMOD)%POSI
      I_DIAG     => GRIDS(IMOD)%I_DIAG
      JA_IE     => GRIDS(IMOD)%JA_IE
      NBEDGE    => GRIDS(IMOD)%NBEDGE
      EDGES     => GRIDS(IMOD)%EDGES
      NEIGH     => GRIDS(IMOD)%NEIGH
      NNZ      => GRIDS(IMOD)%NNZ
      IOBPD     => GRIDS(IMOD)%IOBPD
      IOBDP     => GRIDS(IMOD)%IOBDP
      IOBPA     => GRIDS(IMOD)%IOBPA
#ifdef W3_PDLIB
      IOBP_loc     => GRIDS(IMOD)%IOBP_loc
      IOBPD_loc     => GRIDS(IMOD)%IOBPD_loc
#endif
      TRIA     => GRIDS(IMOD)%TRIA
      CROSSDIFF => GRIDS(IMOD)%CROSSDIFF
      MAXX     => GRIDS(IMOD)%MAXX
      MAXY     => GRIDS(IMOD)%MAXY
      DXYMAX   => GRIDS(IMOD)%DXYMAX      

!
      IF ( GINIT ) THEN
!
          MAPSTA => GRIDS(IMOD)%MAPSTA
          MAPST2 => GRIDS(IMOD)%MAPST2
          MAPFS  => GRIDS(IMOD)%MAPFS
          MAPSF  => GRIDS(IMOD)%MAPSF
          FLAGST => GRIDS(IMOD)%FLAGST
!
#ifdef W3_RTD
          AnglD  => GRIDS(IMOD)%AnglD
#endif
          ZB     => GRIDS(IMOD)%ZB
          CLATS  => GRIDS(IMOD)%CLATS
          CLATIS => GRIDS(IMOD)%CLATIS
          CTHG0S => GRIDS(IMOD)%CTHG0S
          TRNX   => GRIDS(IMOD)%TRNX
          TRNY   => GRIDS(IMOD)%TRNY
!
          XGRD   => GRIDS(IMOD)%XGRD
          YGRD   => GRIDS(IMOD)%YGRD
          DXDP   => GRIDS(IMOD)%DXDP
          DXDQ   => GRIDS(IMOD)%DXDQ
          DYDP   => GRIDS(IMOD)%DYDP
          DYDQ   => GRIDS(IMOD)%DYDQ
          DPDX   => GRIDS(IMOD)%DPDX
          DPDY   => GRIDS(IMOD)%DPDY
          DQDX   => GRIDS(IMOD)%DQDX
          DQDY   => GRIDS(IMOD)%DQDY
          GSQRT  => GRIDS(IMOD)%GSQRT
          HPFAC  => GRIDS(IMOD)%HPFAC
          HQFAC  => GRIDS(IMOD)%HQFAC
!
#ifdef W3_BT4
          SED_D50  => GRIDS(IMOD)%SED_D50
          SED_PSIC => GRIDS(IMOD)%SED_PSIC
#endif
!
#ifdef W3_SMC
          NLvCel => GRIDS(IMOD)%NLvCel
          NLvUFc => GRIDS(IMOD)%NLvUFc
          NLvVFc => GRIDS(IMOD)%NLvVFc
          IJKCel => GRIDS(IMOD)%IJKCel
          IJKUFc => GRIDS(IMOD)%IJKUFc
          IJKVFc => GRIDS(IMOD)%IJKVFc
          ISMCBP => GRIDS(IMOD)%ISMCBP
          CTRNX  => GRIDS(IMOD)%CTRNX
          CTRNY  => GRIDS(IMOD)%CTRNY
          CLATF  => GRIDS(IMOD)%CLATF
#endif
!
#ifdef W3_SMC
          ICLBAC => GRIDS(IMOD)%ICLBAC
          ANGARC => GRIDS(IMOD)%ANGARC
          SPCBAC => GRIDS(IMOD)%SPCBAC
#endif
!
          GSU  => GRIDS(IMOD)%GSU
!
        END IF
!
! -------------------------------------------------------------------- /
! 4.  Set pointers in structure SGRD
!
      NK     => SGRDS(IMOD)%NK
      NK2    => SGRDS(IMOD)%NK2
      NTH    => SGRDS(IMOD)%NTH
      NSPEC  => SGRDS(IMOD)%NSPEC
!
      DTH    => SGRDS(IMOD)%DTH
      XFR    => SGRDS(IMOD)%XFR
      FR1    => SGRDS(IMOD)%FR1
      FTE    => SGRDS(IMOD)%FTE
      FTF    => SGRDS(IMOD)%FTF
      FTWN   => SGRDS(IMOD)%FTWN
      FTTR   => SGRDS(IMOD)%FTTR
      FTWL   => SGRDS(IMOD)%FTWL
      FACTI1 => SGRDS(IMOD)%FACTI1
      FACTI2 => SGRDS(IMOD)%FACTI2
      FACHFA => SGRDS(IMOD)%FACHFA
      FACHFE => SGRDS(IMOD)%FACHFE
!
      SINIT  => SGRDS(IMOD)%SINIT
!
      IF ( SINIT ) THEN
!
          MAPWN  => SGRDS(IMOD)%MAPWN
          MAPTH  => SGRDS(IMOD)%MAPTH
!
          TH     => SGRDS(IMOD)%TH
          ESIN   => SGRDS(IMOD)%ESIN
          ECOS   => SGRDS(IMOD)%ECOS
          ES2    => SGRDS(IMOD)%ES2
          ESC    => SGRDS(IMOD)%ESC
          EC2    => SGRDS(IMOD)%EC2
          SIG    => SGRDS(IMOD)%SIG
          SIG2   => SGRDS(IMOD)%SIG2
          DSIP   => SGRDS(IMOD)%DSIP
          DSII   => SGRDS(IMOD)%DSII
          DDEN   => SGRDS(IMOD)%DDEN
          DDEN2  => SGRDS(IMOD)%DDEN2
!
        END IF
!
! -------------------------------------------------------------------- /
! 5.  Set pointers in structure MPAR
!
      PINIT  => MPARS(IMOD)%PINIT
!
!     Structure NPARS
!
      FACP   => MPARS(IMOD)%NPARS%FACP
      XREL   => MPARS(IMOD)%NPARS%XREL
      XFLT   => MPARS(IMOD)%NPARS%XFLT
      FXFM   => MPARS(IMOD)%NPARS%FXFM
      FXPM   => MPARS(IMOD)%NPARS%FXPM
      XFT    => MPARS(IMOD)%NPARS%XFT
      XFC    => MPARS(IMOD)%NPARS%XFC
      FACSD  => MPARS(IMOD)%NPARS%FACSD
      FHMAX  => MPARS(IMOD)%NPARS%FHMAX
#ifdef W3_RWND
    RWINDC => MPARS(IMOD)%NPARS%RWINDC
#endif
#ifdef W3_WCOR
    WWCOR  => MPARS(IMOD)%NPARS%WWCOR
#endif
!
!     Structure PROPS
!
#ifdef W3_PR2
      DTME   => MPARS(IMOD)%PROPS%DTME
      CLATMN => MPARS(IMOD)%PROPS%CLATMN
#endif
#ifdef W3_PR3
      WDCG   => MPARS(IMOD)%PROPS%WDCG
      WDTH   => MPARS(IMOD)%PROPS%WDTH
#endif
#ifdef W3_SMC
      DTMS   => MPARS(IMOD)%PROPS%DTMS
      Refran => MPARS(IMOD)%PROPS%Refran
      FUNO3  => MPARS(IMOD)%PROPS%FUNO3 
      FVERG  => MPARS(IMOD)%PROPS%FVERG 
      FSWND  => MPARS(IMOD)%PROPS%FSWND
      ARCTC  => MPARS(IMOD)%PROPS%ARCTC
#endif
!
!     Structure FLDP
!
#ifdef W3_FLD1
      TAIL_ID  => MPARS(IMOD)%FLDPS%TAIL_ID
      TAIL_LEV => MPARS(IMOD)%FLDPS%TAIL_LEV
      TAIL_TRAN1 => MPARS(IMOD)%FLDPS%TAIL_TRAN1
      TAIL_TRAN2 => MPARS(IMOD)%FLDPS%TAIL_TRAN2
#endif
#ifdef W3_FLD2
      TAIL_ID  => MPARS(IMOD)%FLDPS%TAIL_ID
      TAIL_LEV => MPARS(IMOD)%FLDPS%TAIL_LEV
      TAIL_TRAN1 => MPARS(IMOD)%FLDPS%TAIL_TRAN1
      TAIL_TRAN2 => MPARS(IMOD)%FLDPS%TAIL_TRAN2
#endif
!
!     Structure SFLPS
!
#ifdef W3_FLX2
      NITTIN => MPARS(IMOD)%SFLPS%NITTIN
      CINXSI => MPARS(IMOD)%SFLPS%CINXSI
#endif
#ifdef W3_FLX3
      NITTIN => MPARS(IMOD)%SFLPS%NITTIN
      CAP_ID => MPARS(IMOD)%SFLPS%CAP_ID
      CINXSI => MPARS(IMOD)%SFLPS%CINXSI
      CD_MAX => MPARS(IMOD)%SFLPS%CD_MAX
#endif
#ifdef W3_FLX4
      FLX4A0 => MPARS(IMOD)%SFLPS%FLX4A0
#endif
!
!     Structure SLNPS
!
#ifdef W3_LN1
      SLNC1  => MPARS(IMOD)%SLNPS%SLNC1
      FSPM   => MPARS(IMOD)%SLNPS%FSPM
      FSHF   => MPARS(IMOD)%SLNPS%FSHF
#endif
!
!     Structure SRCPS
!
      WWNMEANPTAIL=> MPARS(IMOD)%SRCPS%WWNMEANPTAIL
      SSTXFTFTAIL => MPARS(IMOD)%SRCPS%SSTXFTFTAIL
#ifdef W3_ST1
      SINC1  => MPARS(IMOD)%SRCPS%SINC1
      SDSC1  => MPARS(IMOD)%SRCPS%SDSC1
#endif
#ifdef W3_ST2
      ZWIND  => MPARS(IMOD)%SRCPS%ZWIND
      FSWELL => MPARS(IMOD)%SRCPS%FSWELL
      SHSTAB => MPARS(IMOD)%SRCPS%SHSTAB
      OFSTAB => MPARS(IMOD)%SRCPS%OFSTAB
      CCNG   => MPARS(IMOD)%SRCPS%CCNG
      CCPS   => MPARS(IMOD)%SRCPS%CCPS
      FFNG   => MPARS(IMOD)%SRCPS%FFNG
      FFPS   => MPARS(IMOD)%SRCPS%FFPS
      CDSA0  => MPARS(IMOD)%SRCPS%CDSA0
      CDSA1  => MPARS(IMOD)%SRCPS%CDSA1
      CDSA2  => MPARS(IMOD)%SRCPS%CDSA2
      SDSALN => MPARS(IMOD)%SRCPS%SDSALN
      CDSB0  => MPARS(IMOD)%SRCPS%CDSB0
      CDSB1  => MPARS(IMOD)%SRCPS%CDSB1
      CDSB2  => MPARS(IMOD)%SRCPS%CDSB2
      CDSB3  => MPARS(IMOD)%SRCPS%CDSB3
      FPIMIN => MPARS(IMOD)%SRCPS%FPIMIN
      XFH    => MPARS(IMOD)%SRCPS%XFH
      XF1    => MPARS(IMOD)%SRCPS%XF1
      XF2    => MPARS(IMOD)%SRCPS%XF2
#endif
!
#ifdef W3_ST3
      ZZWND  => MPARS(IMOD)%SRCPS%ZZWND
      AALPHA => MPARS(IMOD)%SRCPS%AALPHA
      BBETA  => MPARS(IMOD)%SRCPS%BBETA
      SSINTHP  => MPARS(IMOD)%SRCPS%SSINTHP
      ZZ0MAX  => MPARS(IMOD)%SRCPS%ZZ0MAX
      ZZ0RAT  => MPARS(IMOD)%SRCPS%ZZ0RAT
      ZZALP  => MPARS(IMOD)%SRCPS%ZZALP
      TTAUWSHELTER  => MPARS(IMOD)%SRCPS%TTAUWSHELTER
      SSWELLF  => MPARS(IMOD)%SRCPS%SSWELLF
      SSDSC1 => MPARS(IMOD)%SRCPS%SSDSC1
      WWNMEANP => MPARS(IMOD)%SRCPS%WWNMEANP
      FFXFM => MPARS(IMOD)%SRCPS%FFXFM
      FFXPM => MPARS(IMOD)%SRCPS%FFXPM
      DDELTA1 => MPARS(IMOD)%SRCPS%DDELTA1
      DDELTA2 => MPARS(IMOD)%SRCPS%DDELTA2
      SSTXFTF => MPARS(IMOD)%SRCPS%SSTXFTF
      SSTXFTWN => MPARS(IMOD)%SRCPS%SSTXFTWN
#endif
!
#ifdef W3_ST4
      ZZWND    => MPARS(IMOD)%SRCPS%ZZWND
      AALPHA   => MPARS(IMOD)%SRCPS%AALPHA
      BBETA    => MPARS(IMOD)%SRCPS%BBETA
      SSINTHP  => MPARS(IMOD)%SRCPS%SSINTHP
      ZZ0MAX   => MPARS(IMOD)%SRCPS%ZZ0MAX
      ZZ0RAT   => MPARS(IMOD)%SRCPS%ZZ0RAT
      ZZALP    => MPARS(IMOD)%SRCPS%ZZALP
      TTAUWSHELTER  => MPARS(IMOD)%SRCPS%TTAUWSHELTER
      SSWELLFPAR  => MPARS(IMOD)%SRCPS%SSWELLFPAR
      SSWELLF  => MPARS(IMOD)%SRCPS%SSWELLF
      SSDSC    => MPARS(IMOD)%SRCPS%SSDSC
      SSDSBR   => MPARS(IMOD)%SRCPS%SSDSBR
      SSDSBT   => MPARS(IMOD)%SRCPS%SSDSBT
      SSDSBRF1 => MPARS(IMOD)%SRCPS%SSDSBRF1
      SSDSBRF2 => MPARS(IMOD)%SRCPS%SSDSBRF2
      SSDSBRFDF  => MPARS(IMOD)%SRCPS%SSDSBRFDF
      SSDSBM   => MPARS(IMOD)%SRCPS%SSDSBM
      SSDSBCK  => MPARS(IMOD)%SRCPS%SSDSBCK
      SSDSABK  => MPARS(IMOD)%SRCPS%SSDSABK
      SSDSPBK  => MPARS(IMOD)%SRCPS%SSDSPBK
      SSDSHCK  => MPARS(IMOD)%SRCPS%SSDSHCK
      SSDSBINT => MPARS(IMOD)%SRCPS%SSDSBINT
      SSDSP    => MPARS(IMOD)%SRCPS%SSDSP
      WWNMEANP => MPARS(IMOD)%SRCPS%WWNMEANP
      FFXFM    => MPARS(IMOD)%SRCPS%FFXFM
      FFXFA    => MPARS(IMOD)%SRCPS%FFXFA
      FFXPM    => MPARS(IMOD)%SRCPS%FFXPM
      SSDSDTH  => MPARS(IMOD)%SRCPS%SSDSDTH
      SSTXFTF  => MPARS(IMOD)%SRCPS%SSTXFTF
      SSTXFTWN => MPARS(IMOD)%SRCPS%SSTXFTWN
      SSDSCOS  => MPARS(IMOD)%SRCPS%SSDSCOS
      SSDSISO  => MPARS(IMOD)%SRCPS%SSDSISO
      IKTAB    => MPARS(IMOD)%SRCPS%IKTAB  
      DCKI     => MPARS(IMOD)%SRCPS%DCKI  
      QBI      => MPARS(IMOD)%SRCPS%QBI  
      CUMULW   => MPARS(IMOD)%SRCPS%CUMULW
      SATINDICES    => MPARS(IMOD)%SRCPS%SATINDICES
      SATWEIGHTS   => MPARS(IMOD)%SRCPS%SATWEIGHTS
      SSINBR   => MPARS(IMOD)%SRCPS%SSINBR
#endif
!
#ifdef W3_ST6
      SIN6A0 => MPARS(IMOD)%SRCPS%SIN6A0
      SIN6WS => MPARS(IMOD)%SRCPS%SIN6WS
      SIN6FC => MPARS(IMOD)%SRCPS%SIN6FC
      SDS6ET => MPARS(IMOD)%SRCPS%SDS6ET
      SDS6A1 => MPARS(IMOD)%SRCPS%SDS6A1
      SDS6P1 => MPARS(IMOD)%SRCPS%SDS6P1
      SDS6A2 => MPARS(IMOD)%SRCPS%SDS6A2
      SDS6P2 => MPARS(IMOD)%SRCPS%SDS6P2
      SWL6S6 => MPARS(IMOD)%SRCPS%SWL6S6
      SWL6B1 => MPARS(IMOD)%SRCPS%SWL6B1
      SWL6CSTB1 => MPARS(IMOD)%SRCPS%SWL6CSTB1
#endif
!
!     Structure SRNLS
!
#ifdef W3_NL1
      SNLC1  => MPARS(IMOD)%SNLPS%SNLC1
      LAM    => MPARS(IMOD)%SNLPS%LAM
      KDCON  => MPARS(IMOD)%SNLPS%KDCON
      KDMN   => MPARS(IMOD)%SNLPS%KDMN
      SNLS1  => MPARS(IMOD)%SNLPS%SNLS1
      SNLS2  => MPARS(IMOD)%SNLPS%SNLS2
      SNLS3  => MPARS(IMOD)%SNLPS%SNLS3
#endif
#ifdef W3_NL2
      IQTPE  => MPARS(IMOD)%SNLPS%IQTPE
      NDPTHS => MPARS(IMOD)%SNLPS%NDPTHS
      NLTAIL => MPARS(IMOD)%SNLPS%NLTAIL
      IF ( NDPTHS .NE. 0 ) DPTHNL => MPARS(IMOD)%SNLPS%DPTHNL
#endif
#ifdef W3_NL3
      NFRMIN => MPARS(IMOD)%SNLPS%NFRMIN
      NFRMAX => MPARS(IMOD)%SNLPS%NFRMAX
      NFRCUT => MPARS(IMOD)%SNLPS%NFRCUT
      NTHMAX => MPARS(IMOD)%SNLPS%NTHMAX
      NTHEXP => MPARS(IMOD)%SNLPS%NTHEXP
      NSPMIN => MPARS(IMOD)%SNLPS%NSPMIN
      NSPMAX => MPARS(IMOD)%SNLPS%NSPMAX
      NSPMX2 => MPARS(IMOD)%SNLPS%NSPMX2
      FRQ    => MPARS(IMOD)%SNLPS%FRQ
      XSI    => MPARS(IMOD)%SNLPS%XSI
      NQA    => MPARS(IMOD)%SNLPS%NQA
      QST1   => MPARS(IMOD)%SNLPS%QST1
      QST2   => MPARS(IMOD)%SNLPS%QST2
      QST3   => MPARS(IMOD)%SNLPS%QST3
      QST4   => MPARS(IMOD)%SNLPS%QST4
      QST5   => MPARS(IMOD)%SNLPS%QST5
      QST6   => MPARS(IMOD)%SNLPS%QST6
      SNLNQ  => MPARS(IMOD)%SNLPS%SNLNQ
      SNLMSC => MPARS(IMOD)%SNLPS%SNLMSC
      SNLNSC => MPARS(IMOD)%SNLPS%SNLNSC
      SNLSFD => MPARS(IMOD)%SNLPS%SNLSFD
      SNLSFS => MPARS(IMOD)%SNLPS%SNLSFS
      SNLL   => MPARS(IMOD)%SNLPS%SNLL
      SNLM   => MPARS(IMOD)%SNLPS%SNLM
      SNLT   => MPARS(IMOD)%SNLPS%SNLT
      SNLCD  => MPARS(IMOD)%SNLPS%SNLCD
      SNLCS  => MPARS(IMOD)%SNLPS%SNLCS
#endif
#ifdef W3_NL4
      ITSA   => MPARS(IMOD)%SNLPS%ITSA
      IALT   => MPARS(IMOD)%SNLPS%IALT
#endif
#ifdef W3_NL5
      QR5DPT => MPARS(IMOD)%SNLPS%QR5DPT
      QR5OML => MPARS(IMOD)%SNLPS%QR5OML
      QI5DIS => MPARS(IMOD)%SNLPS%QI5DIS
      QI5KEV => MPARS(IMOD)%SNLPS%QI5KEV
      QI5NNZ => MPARS(IMOD)%SNLPS%QI5NNZ
      QI5IPL => MPARS(IMOD)%SNLPS%QI5IPL
      QI5PMX => MPARS(IMOD)%SNLPS%QI5PMX
#endif
#ifdef W3_NLS
      NTHX   => MPARS(IMOD)%SNLPS%NTHX
      NFRX   => MPARS(IMOD)%SNLPS%NFRX
      NSPL   => MPARS(IMOD)%SNLPS%NSPL
      NSPH   => MPARS(IMOD)%SNLPS%NSPH
      SNSST  => MPARS(IMOD)%SNLPS%SNSST
      CNLSA  => MPARS(IMOD)%SNLPS%CNLSA
      CNLSC  => MPARS(IMOD)%SNLPS%CNLSC
      CNLSFM => MPARS(IMOD)%SNLPS%CNLSFM
      CNLSC1 => MPARS(IMOD)%SNLPS%CNLSC1
      CNLSC2 => MPARS(IMOD)%SNLPS%CNLSC2
      CNLSC3 => MPARS(IMOD)%SNLPS%CNLSC3
#endif
!
!     Structure SBTPS
!
#ifdef W3_BT1
      SBTC1  => MPARS(IMOD)%SBTPS%SBTC1
#endif
#ifdef W3_BT4
      SBTCX  => MPARS(IMOD)%SBTPS%SBTCX
#endif
!
!     Structure SDBPS
!
#ifdef W3_DB1
      SDBC1  => MPARS(IMOD)%SDBPS%SDBC1
      SDBC2  => MPARS(IMOD)%SDBPS%SDBC2
      FDONLY => MPARS(IMOD)%SDBPS%FDONLY
#endif
!
!
#ifdef W3_UOST
      UOSTFILELOCAL => MPARS(IMOD)%UOSTPS%UOSTFILELOCAL
      UOSTFILESHADOW => MPARS(IMOD)%UOSTPS%UOSTFILESHADOW
      UOSTFACTORLOCAL => MPARS(IMOD)%UOSTPS%UOSTFACTORLOCAL
      UOSTFACTORSHADOW => MPARS(IMOD)%UOSTPS%UOSTFACTORSHADOW
#endif
!
!     Structure SICPS
!
#ifdef W3_IS1
      IS1C1  => MPARS(IMOD)%SICPS%IS1C1
      IS1C2  => MPARS(IMOD)%SICPS%IS1C2
#endif
!
!    Structure SCHM
       FSBCCFL => MPARS(IMOD)%SCHMS%FSBCCFL
       FSN => MPARS(IMOD)%SCHMS%FSN
       FSPSI => MPARS(IMOD)%SCHMS%FSPSI
       FSFCT => MPARS(IMOD)%SCHMS%FSFCT
       FSNIMP => MPARS(IMOD)%SCHMS%FSNIMP
       FSTOTALIMP => MPARS(IMOD)%SCHMS%FSTOTALIMP
       FSTOTALEXP => MPARS(IMOD)%SCHMS%FSTOTALEXP
       FSREFRACTION => MPARS(IMOD)%SCHMS%FSREFRACTION
       FSFREQSHIFT => MPARS(IMOD)%SCHMS%FSFREQSHIFT
       FSSOURCE => MPARS(IMOD)%SCHMS%FSSOURCE
       DO_CHANGE_WLV => MPARS(IMOD)%SCHMS%DO_CHANGE_WLV
       SOLVERTHR_STP => MPARS(IMOD)%SCHMS%SOLVERTHR_STP
       CRIT_DEP_STP => MPARS(IMOD)%SCHMS%CRIT_DEP_STP
       B_JGS_TERMINATE_MAXITER => MPARS(IMOD)%SCHMS%B_JGS_TERMINATE_MAXITER
       B_JGS_TERMINATE_DIFFERENCE => MPARS(IMOD)%SCHMS%B_JGS_TERMINATE_DIFFERENCE
       B_JGS_TERMINATE_NORM => MPARS(IMOD)%SCHMS%B_JGS_TERMINATE_NORM
       B_JGS_LIMITER => MPARS(IMOD)%SCHMS%B_JGS_LIMITER
       B_JGS_USE_JACOBI => MPARS(IMOD)%SCHMS%B_JGS_USE_JACOBI
       B_JGS_BLOCK_GAUSS_SEIDEL => MPARS(IMOD)%SCHMS%B_JGS_BLOCK_GAUSS_SEIDEL
       B_JGS_MAXITER => MPARS(IMOD)%SCHMS%B_JGS_MAXITER
       B_JGS_PMIN => MPARS(IMOD)%SCHMS%B_JGS_PMIN
       B_JGS_DIFF_THR => MPARS(IMOD)%SCHMS%B_JGS_DIFF_THR
       B_JGS_NORM_THR => MPARS(IMOD)%SCHMS%B_JGS_NORM_THR
       B_JGS_NLEVEL => MPARS(IMOD)%SCHMS%B_JGS_NLEVEL
       B_JGS_SOURCE_NONLINEAR => MPARS(IMOD)%SCHMS%B_JGS_SOURCE_NONLINEAR
      RETURN
!
! Formats
!
 1001 FORMAT (/' *** ERROR W3SETG : GRIDS NOT INITIALIZED *** '/      &
               '                    RUN W3NMOD FIRST '/)
 1002 FORMAT (/' *** ERROR W3SETG : ILLEGAL MODEL NUMBER *** '/       &
               '                    IMOD   = ',I10/                   &
               '                    NAUXGR = ',I10/                   &
               '                    NGRIDS = ',I10/)
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3SETG)
 9000 FORMAT (' TEST W3SETG : GRID/MODEL ',I4,' SELECTED')
#endif
!/
!/ End of W3SETG ----------------------------------------------------- /
!/
      END SUBROUTINE W3SETG
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3GNTX ( IMOD, NDSE, NDST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           T. J. Campbell          |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         20-Jul-2011 |
!/                  +-----------------------------------+
!/
!/    30-Oct-2009 : Origination.                        ( version 3.13 )
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid.   ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    23-Dec-2010 : Fix HPFAC and HQFAC by including the COS(YGRD)
!/                  factor with DXDP and DXDQ terms.    ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    20-Jul-2011 : HPFAC and HQFAC are now calculated using W3DIST.
!/                  Result should be very similar except near pole.
!/                  Due to precision issues, HPFAC and HQFAC revert
!/                  to SX and SY in case of regular grids.
!/                  (W. E. Rogers, NRL)                 ( version 3.14 )
!/    20-Jan-2017 : Update to new W3GSRUMD APIs         ( version 6.02 )
!/    20-Jan-2017 : Change calculation of curvilinear grid metric and
!/                  derivatives calculations to use W3GSRUMD:W3CGDM.
!/                  (T.J. Campbell, NRL)                ( version 6.02 )
!/
!  1. Purpose :
!
!     Construct required spatial grid quantities for curvilinear grids.
!
!  2. Method :
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number to point to.
!       NDSE    Int.   I   Error output unit number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!     Any program that uses this grid structure.
!
!  6. Error messages :
!
!     - Check on previous initialization of grids.
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
!     !/S    Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IMOD, NDSE, NDST
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER, PARAMETER :: NFD    = 4
      LOGICAL, PARAMETER :: PTILED = .FALSE.
      LOGICAL, PARAMETER :: QTILED = .FALSE.
      LOGICAL, PARAMETER :: IJG    = .FALSE.
      LOGICAL, PARAMETER :: SPHERE = .FALSE.
      INTEGER :: PRANGE(2), QRANGE(2)
      INTEGER :: LBI(2), UBI(2), LBO(2), UBO(2), ISTAT
      REAL   , ALLOCATABLE :: COSA(:,:)
#ifdef W3_S
      INTEGER, SAVE      :: IENT = 0
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3GNTX')
#endif
!
! -------------------------------------------------------------------- /
! 1.  Test input and module status
!
      IF ( NGRIDS .EQ. -1 ) THEN
          WRITE (NDSE,1001)
          CALL EXTCDE (1)
        END IF
!
      IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NGRIDS ) THEN
          WRITE (NDSE,1002) IMOD, -NAUXGR, NGRIDS
          CALL EXTCDE (2)
        END IF
!
      SELECT CASE ( GRIDS(IMOD)%GTYPE )
        CASE ( RLGTYPE )
        CASE ( CLGTYPE )
        CASE ( SMCTYPE )
        CASE DEFAULT
          WRITE (NDSE,1003) GRIDS(IMOD)%GTYPE
          CALL EXTCDE (3)
        END SELECT
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
      WRITE (NDST,9000) IMOD
#endif
!
! -------------------------------------------------------------------- /
! 2.  Create grid search utility object
!
      GRIDS(IMOD)%GSU = W3GSUC( IJG, FLAGLL, GRIDS(IMOD)%ICLOSE, &
                                GRIDS(IMOD)%XGRD, GRIDS(IMOD)%YGRD )
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
      CALL W3GSUP(GRIDS(IMOD)%GSU, NDST)
      WRITE (NDST,9001)
#endif
!
! -------------------------------------------------------------------- /
! 3.  Reset grid pointers
!
      CALL W3SETG ( IMOD, NDSE, NDST )
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
      WRITE (NDST,9002)
#endif
!
! -------------------------------------------------------------------- /
! 4.  Construct curvilinear grid derivatives and metric
!     Note that in the case of lon/lat grids, these quantities do not
!     include the spherical coordinate metric (SPHERE=.FALSE.).
!
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
      ALLOCATE ( COSA(NY,NX), STAT=ISTAT )
      CHECK_ALLOC_STATUS ( ISTAT )
#endif
      PRANGE = (/ 1,NX/)
      QRANGE = (/ 1,NY/)
      LBI = (/ 1, 1/)
      UBI = (/NY,NX/)
      LBO = (/ 1, 1/)
      UBO = (/NY,NX/)
      SELECT CASE ( GTYPE )
!!Li  SMC grid shares the settings with rectilinear grid. JGLi12Oct2020
        CASE ( RLGTYPE, SMCTYPE )
          CALL W3CGDM( IJG, FLAGLL, ICLOSE, PTILED, QTILED,            &
                       PRANGE, QRANGE, LBI, UBI, LBO, UBO, XGRD, YGRD, &
                       NFD=NFD, SPHERE=SPHERE, DX=SX, DY=SY,           &
                       DXDP=DXDP, DYDP=DYDP, DXDQ=DXDQ, DYDQ=DYDQ,     &
                       DPDX=DPDX, DPDY=DPDY, DQDX=DQDX, DQDY=DQDY,     &
                       HPFC=HPFAC, HQFC=HQFAC, GSQR=GSQRT,             &
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
                       COSA=COSA,                                      &
#endif
                       RC=ISTAT )
          IF ( ISTAT.NE.0 ) THEN
              WRITE (NDSE,1004) GTYPE
              CALL EXTCDE (4)
            END IF
        CASE ( CLGTYPE )
          CALL W3CGDM( IJG, FLAGLL, ICLOSE, PTILED, QTILED,            &
                       PRANGE, QRANGE, LBI, UBI, LBO, UBO, XGRD, YGRD, &
                       NFD=NFD, SPHERE=SPHERE,                         &
                       DXDP=DXDP, DYDP=DYDP, DXDQ=DXDQ, DYDQ=DYDQ,     &
                       DPDX=DPDX, DPDY=DPDY, DQDX=DQDX, DQDY=DQDY,     &
                       HPFC=HPFAC, HQFC=HQFAC, GSQR=GSQRT,             &
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
                       COSA=COSA,                                      &
#endif
                       RC=ISTAT )
          IF ( ISTAT.NE.0 ) THEN
              WRITE (NDSE,1004) GTYPE
              CALL EXTCDE (4)
            END IF
        END SELECT
!
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
      WRITE(NDST,'(A,2E14.6)')'HPFAC MIN/MAX:',MINVAL(HPFAC),MAXVAL(HPFAC)
      WRITE(NDST,'(A,2E14.6)')'HQFAC MIN/MAX:',MINVAL(HQFAC),MAXVAL(HQFAC)
      WRITE(NDST,'(A,2E14.6)')'GSQRT MIN/MAX:',MINVAL(GSQRT),MAXVAL(GSQRT)
      WRITE(NDST,'(A,2E14.6)')'DXDP  MIN/MAX:',MINVAL(DXDP),MAXVAL(DXDP)
      WRITE(NDST,'(A,2E14.6)')'DYDP  MIN/MAX:',MINVAL(DYDP),MAXVAL(DYDP)
      WRITE(NDST,'(A,2E14.6)')'DXDQ  MIN/MAX:',MINVAL(DXDQ),MAXVAL(DXDQ)
      WRITE(NDST,'(A,2E14.6)')'DYDQ  MIN/MAX:',MINVAL(DYDQ),MAXVAL(DYDQ)
      WRITE(NDST,'(A,2E14.6)')'DPDX  MIN/MAX:',MINVAL(DPDX),MAXVAL(DPDX)
      WRITE(NDST,'(A,2E14.6)')'DPDY  MIN/MAX:',MINVAL(DPDY),MAXVAL(DPDY)
      WRITE(NDST,'(A,2E14.6)')'DQDX  MIN/MAX:',MINVAL(DQDX),MAXVAL(DQDX)
      WRITE(NDST,'(A,2E14.6)')'DQDY  MIN/MAX:',MINVAL(DQDY),MAXVAL(DQDY)
      WRITE(NDST,'(A,2E14.6)')'COSA  MIN/MAX:',MINVAL(COSA),MAXVAL(COSA)
      WRITE (NDST,9003)
      DEALLOCATE ( COSA, STAT=ISTAT )
      CHECK_DEALLOC_STATUS ( ISTAT )
#endif
!
! Formats
!
 1001 FORMAT (/' *** ERROR W3GNTX : GRIDS NOT INITIALIZED *** '/      &
               '                    RUN W3NMOD FIRST '/)
 1002 FORMAT (/' *** ERROR W3GNTX : ILLEGAL MODEL NUMBER *** '/       &
               '                    IMOD   = ',I10/                   &
               '                    NAUXGR = ',I10/                   &
               '                    NGRIDS = ',I10/)
 1003 FORMAT (/' *** ERROR W3GNTX : UNSUPPORTED TYPE OF GRID *** '/   &
               '                    GTYPE  = ',I10/)
 1004 FORMAT (/' *** ERROR W3GNTX : ERROR OCCURED IN W3CGDM *** '/    &
               '                    GTYPE  = ',I10/)
!
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3GNTX)
 9000 FORMAT (' TEST W3GNTX : MODEL ',I4)
 9001 FORMAT (' TEST W3GNTX : SEARCH OBJECT CREATED')
 9002 FORMAT (' TEST W3GNTX : POINTERS RESET')
 9003 FORMAT (' TEST W3GNTX : GRID ARRAYS CONSTRUCTED')
#endif
!/
!/ End of W3GNTX ----------------------------------------------------- /
!/
      END SUBROUTINE W3GNTX
!/ ------------------------------------------------------------------- /   
      SUBROUTINE W3DIMUG  ( IMOD, MTRI, MX, COUNTOTA, NNZ, NDSE, NDST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |             F.ardhuin             |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         15-Mar-2007 !
!/                  +-----------------------------------+
!/
!/    15-Mar-2007 : Origination.                        ( version 3.14 )
!/    11-May-2015 : Updates to 2-ways nestings for UG   ( version 5.08 )
!/
!  1. Purpose :
!
!     Initialize an individual spatial grid at the proper dimensions.
!
!  2. Method :
!
!     Allocate directly into the structure array GRIDS. Note that
!     this cannot be done through the pointer alias!
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number to point to.
!       NDSE    Int.   I   Error output unit number.
!       NDST    Int.   I   Test output unit number.
!       MX, MTRI, MSEA       Like NX, NTRI, NSEA in data structure.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!       See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3IOGR    Subr. W3IOGRMD Model definition file IO program.
!      WW3_GRID  Prog.   N/A    Model set up program.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!     - Check on input parameters.
!     - Check on previous allocation.
!
!  7. Remarks :
!
!     - Grid dimensions apre passed through parameter list and then 
!       locally stored to assure consistency between allocation and
!       data in structure.
!     - W3SETG needs to be called after allocation to point to 
!       proper allocated arrays.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S    Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_MEMCHECK
      USE MallocInfo_m
#endif
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      IMPLICIT NONE
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IMOD, MTRI, MX, COUNTOTA, NNZ, NDSE, NDST
#ifdef W3_MEMCHECK
      type(MallInfo_t)        :: mallinfos
#endif
      INTEGER                 :: IAPROC = 1
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3DIMUG')
#endif
!
! -------------------------------------------------------------------- /
! 1.  Test input and module status
!
      IF ( NGRIDS .EQ. -1 ) THEN
          WRITE (NDSE,1001)
          CALL EXTCDE (1)
        END IF
!
      IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NGRIDS ) THEN
          WRITE (NDSE,1002) IMOD, NGRIDS
          CALL EXTCDE (2)
        END IF
      IF ( GRIDS(IMOD)%GUGINIT ) THEN
        WRITE (NDSE,1004)
        CALL EXTCDE (4)
        END IF
!
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMUG)
      WRITE (NDST,9000) IMOD, MX, MTRI
#endif

#ifdef W3_MEMCHECK
      WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_UG_ALLOCATE SECTION 1'
      call getMallocInfo(mallinfos)
      call printMallInfo(IAPROC,mallInfos)
#endif
!
! -------------------------------------------------------------------- /
! 2.  Allocate arrays
!
      ALLOCATE ( GRIDS(IMOD)%TRIGP(MTRI,3),                         &
                 GRIDS(IMOD)%XYB(MX,3),                             &
                 GRIDS(IMOD)%SI(MX),                                &
                 GRIDS(IMOD)%TRIA(MTRI),                            & 
                 GRIDS(IMOD)%CROSSDIFF(6,MTRI),                     &
                 GRIDS(IMOD)%IEN(MTRI,6),                           &
                 GRIDS(IMOD)%LEN(MTRI,3),                           &
                 GRIDS(IMOD)%ANGLE(MTRI,3),                         &
                 GRIDS(IMOD)%ANGLE0(MTRI,3),                        &
                 GRIDS(IMOD)%CCON(MX),                              &
                 GRIDS(IMOD)%COUNTCON(MX),                          &
                 GRIDS(IMOD)%INDEX_CELL(MX+1),                      &
                 GRIDS(IMOD)%IE_CELL(COUNTOTA),                     &
                 GRIDS(IMOD)%POS_CELL(COUNTOTA),                    &
                 GRIDS(IMOD)%IAA(NX+1),                             &
                 GRIDS(IMOD)%JAA(NNZ),                              &
                 GRIDS(IMOD)%POSI(3,COUNTOTA),                      &
                 GRIDS(IMOD)%I_DIAG(NX),                            &
                 GRIDS(IMOD)%JA_IE(3,3,MTRI),                       &
                 GRIDS(IMOD)%IOBP(MX),                              &
                 GRIDS(IMOD)%IOBPD(NTH,MX),                         &
                 GRIDS(IMOD)%IOBDP(MX),                             &
                 GRIDS(IMOD)%IOBPA(MX),                             &
                 STAT=ISTAT                                         )
      CHECK_ALLOC_STATUS ( ISTAT )
!
                 GRIDS(IMOD)%IOBP(:)=1
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMUG)
      WRITE (NDST,9001)
#endif
!
!some segmentation troubles can appear, they are related with the allocation of 
!normal(1st dimension) and the nesting of the triangulated grid.
! -------------------------------------------------------------------- /
! 3.  Point to allocated arrays
!
      CALL W3SETG ( IMOD, NDSE, NDST )
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMUG)
      WRITE (NDST,9002)
#endif
!
! -------------------------------------------------------------------- /
! 4.  Update counters in grid
!     Note that in the case of lon/lat grids, these quantities do not
!     include the spherical coordinate metric (SPHERE=.FALSE.).
!
      NTRI   = MTRI
      COUNTOT=COUNTOTA
      GRIDS(IMOD)%GUGINIT  = .TRUE.
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMUG)
      WRITE (NDST,9003)
#endif
#ifdef W3_MEMCHECK
      WRITE(740+IAPROC,*) 'memcheck_____:', 'WW3_UG_ALLOCATE SECTION 2'
      call getMallocInfo(mallinfos)
      call printMallInfo(IAPROC,mallInfos)
#endif

      RETURN
!
! Formats
!
 1001 FORMAT (/' *** ERROR W3DIMUG : GRIDS NOT INITIALIZED *** '/      &
               '                    RUN W3NMOD FIRST '/)
 1002 FORMAT (/' *** ERROR W3DIMUG : ILLEGAL MODEL NUMBER *** '/       &
               '                    IMOD   = ',I10/                   &
               '                    NGRIDS = ',I10/)
 1004 FORMAT (/' *** ERROR W3DIMUG : ARRAY(S) ALREADY ALLOCATED *** ')
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3DIMUG)
 9000 FORMAT (' TEST W3DIMUG: MODEL ',I4,' DIM. AT ',2I5,I7)
 9001 FORMAT (' TEST W3DIMUG : ARRAYS ALLOCATED')
 9002 FORMAT (' TEST W3DIMUG : POINTERS RESET')
 9003 FORMAT (' TEST W3DIMUG : DIMENSIONS STORED')
#endif
!/
!/ End of W3DIMUG ----------------------------------------------------- /
!/
      END SUBROUTINE W3DIMUG
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3SETREF
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           F. Ardhuin              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         13-Nov-2013 |
!/                  +-----------------------------------+
!/
!/    13-Nov-2013 : Origination.                        ( version 4.13 )
!/
!  1. Purpose :
!
!     Update reflection directions at shoreline. 
!
!  2. Method :
!
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       None
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      WW3_GRID  Prog. WW3_GRID Grid preprocessor
!      W3ULEV    Subr. W3UPDTMD Water level update
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
!       !/S      Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
#ifdef W3_S
   USE W3SERVMD, ONLY : STRACE
#endif
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER                 :: ISEA, IX, IY, IXY, IXN, IXP, IYN, IYP
      INTEGER                 :: J, K, NEIGH1(0:7)
      INTEGER                 :: ILEV, NLEV
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif

      REAL                    :: TRIX(NY*NX), TRIY(NY*NX), DX, DY,    &
                                 COSAVG, SINAVG, THAVG, ANGLES(0:7), CLAT
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3SETREF')
#endif
!
! 1.  Preparations --------------------------------------------------- *
!
#ifdef W3_REF1
      IF (REFPARS(2).GT.0) RREF(2)=.TRUE.
      IF (REFPARS(3).GT.0) RREF(3)=.TRUE.
      IF (REFPARS(4).GT.0) RREF(4)=.TRUE.
#endif
!
#ifdef W3_REF1
      DO IY=2, NY-1
        DO IX=2, NX-1
          IF (REFPARS(1).GT.0) RREF(1)=.TRUE.
!No reflection from artificial island on pole. 
          IF (FLAGLL.AND.(YGRD(IY,IX).GT.85)) RREF(1)=.FALSE.
          IF (MAPSTA(IY,IX).GT.0) THEN
!
! Prepares for reflection from subgrid islands
!
            IF (RREF(2)) &
              REFLC(2,MAPFS(IY,IX))= MAX((1. - TRNX(IY,IX)),(1.-TRNY(IY,IX)))
!
! Prepares for iceberg reflections
!
            IF (RREF(4)) &
              REFLC(4,MAPFS(IY,IX))= 1.
!
! resolved shoreline reflection
!
            IF (RREF(1)) THEN 
              REFLC(1,  MAPFS(IY,IX)) = 0.
              REFLD(1:6,MAPFS(IY,IX)) = 0
!
! Search for neighboring coastline.        3 2 1 
! around X. These are the neighbors of X:  4 X 0
!                                          5 6 7
!
!
              NEIGH1(0)=8*MAPST2(IY,IX+1)+MAPSTA(IY,IX+1)
              NEIGH1(1:3)=8*MAPST2(IY+1,IX+1:IX-1:-1)+MAPSTA(IY+1,IX+1:IX-1:-1)
              NEIGH1(4)=8*MAPST2(IY,IX-1)+MAPSTA(IY,IX-1)
              NEIGH1(5:7)=8*MAPST2(IY-1,IX-1:IX+1)+MAPSTA(IY-1,IX-1:IX+1)
!
! if one of the surrounding points is land: determines directions ... 
!              
              IF (MINVAL(ABS(NEIGH1)).EQ.0) THEN 
                IF ( FLAGLL ) THEN
                  CLAT   = COS(YGRD(IY,IX)*DERA)
                ELSE
                  CLAT = 1.
                  END IF
                ANGLES(0)= ATAN2(DYDP(IY,IX),DXDP(IY,IX)*CLAT)
                ANGLES(1)= ATAN2(DYDP(IY,IX)+DYDQ(IY,IX),(DXDP(IY,IX)+DXDQ(IY,IX))*CLAT)
                ANGLES(2)= ATAN2(DYDQ(IY,IX),DXDQ(IY,IX)*CLAT)
                ANGLES(3)= ATAN2(DYDQ(IY,IX)-DYDP(IY,IX),(DXDQ(IY,IX)-DXDP(IY,IX))*CLAT)
                ANGLES(4:7)= ANGLES(0:3)+PI
                IF ((NEIGH1(0).GE.1).AND.(NEIGH1(4).GE.1)) THEN 
                  REFLD(3,MAPFS(IY,IX))=0
                ELSE 
                  IF ((NEIGH1(0).GE.1).OR.(NEIGH1(4).GE.1)) REFLD(3,MAPFS(IY,IX))=1
                  END IF
                IF ((NEIGH1(2).EQ.1).AND.(NEIGH1(6).GE.1)) THEN 
                  REFLD(4,MAPFS(IY,IX))=0
                ELSE
                  IF ((NEIGH1(2).GE.1).OR.(NEIGH1(6).GE.1)) REFLD(4,MAPFS(IY,IX))=1
                  END IF
!
! Looks for a locally straight coast in all 8 orientations 
!
                J=0
                REFLD(1,MAPFS(IY,IX))=0
                COSAVG=0
                SINAVG=0
! Shore angle is corrected for grid rotation in w3ref1md.ftn with  REFLD(5:6,MAPFS(IY,IX)) 
                REFLD(5,MAPFS(IY,IX))= MOD(NTH+NINT(ANGLES(0)/TPI*NTH),NTH)
                REFLD(6,MAPFS(IY,IX))= MOD(NTH+NINT((ANGLES(2)/TPI-0.25)*NTH),NTH)
#endif
#ifdef W3_REFT
                    IF (IY.EQ.4) THEN 
                      WRITE(6,*) 'POINT (IX,IY):',IX,IY
                      WRITE(6,*) 'REFT:',NEIGH1(3),NEIGH1(2), NEIGH1(1) 
                      WRITE(6,*) 'REFT:',NEIGH1(4),1, NEIGH1(0) 
                      WRITE(6,*) 'REFT:',NEIGH1(5:7)
                      WRITE(6,*) 'ANG:',ANGLES(3)*RADE,ANGLES(2)*RADE, ANGLES(1)*RADE 
                      WRITE(6,*) 'ANG:',ANGLES(4)*RADE,1, ANGLES(0) *RADE
                      WRITE(6,*) 'ANG:',ANGLES(5:7)*RADE
                      WRITE(6,*) 'REFT:',XGRD(IY+1,IX-1:IX+1), YGRD(IY+1,IX-1:IX+1)
                      WRITE(6,*) 'REFT:',XGRD(IY,IX-1:IX+1) , YGRD(IY,IX-1:IX+1)
                      WRITE(6,*) 'REFT:',XGRD(IY-1,IX-1:IX+1), YGRD(IY-1,IX-1:IX+1)
                      WRITE(6,*) 'REFLD:',REFLD(3:6,MAPFS(IY,IX))
                    ENDIF
#endif
#ifdef W3_REF1
                DO K=0,7
                  IF (NEIGH1(K).EQ.0.AND.NEIGH1(MOD(K+7,8)).EQ.0 &
                    .AND.NEIGH1(MOD(K+1,8)).EQ.0 &
                    .AND.NEIGH1(MOD(K+4,8)).NE.0) THEN 
                    REFLC(1,MAPFS(IY,IX))= REFPARS(1)
!
! Defines direction index for specular reflection (normal to coast)
!
!  for example, if we have this layout   1 1 0
!  (NB: 1 is sea, 0 is land)             1 X 0
!                                        1 1 0
!
!  then there is only a coastline detection for K=0, giving J=1 
!  and the final result will be REFLD(1,MAPFS(IY,IX))=1
!  Namely, the direction TH(REFLD) is the direction pointing INTO the coast
!
                    REFLD(2,MAPFS(IY,IX))= 2
                    COSAVG=COSAVG+COS(ANGLES(K))  !ECOS(1+(K*NTH)/8)
                    SINAVG=SINAVG+SIN(ANGLES(K))  !ESIN(1+(K*NTH)/8)
                    J=J+1
                    ENDIF
                  END DO
                IF (J.GT.0) THEN 
                  IF (J.GT.1) REFLD(2,MAPFS(IY,IX))= 1
                  THAVG=ATAN2(SINAVG,COSAVG) 
#endif
#if defined(TEST_W3GDATMD) || defined(TEST_W3GDATMD_W3SETREF)
#ifdef W3_REF1
                  !WRITE (6,*) 'COASTAL REFLECTION:',IX,IY,   &
                  !SINAVG,COSAVG,THAVG/TPI,NINT(THAVG/TPI*NTH),MOD(NTH+NINT(THAVG/TPI*NTH),NTH)
#endif
#endif
#ifdef W3_REF1
                  REFLD(1,MAPFS(IY,IX))=1+MOD(NTH+NINT(THAVG/TPI*NTH),NTH)
                ELSE

!                             1 1 1 
! Looks for mild corners like 1 1 1
!                             1 0 0          
                  DO K=0,7
                    IF (NEIGH1(K).EQ.0.AND.NEIGH1(MOD(K+1,8)).EQ.0 &
                      .AND.NEIGH1(MOD(K+4,8)).NE.0) THEN 
                      REFLC(1,MAPFS(IY,IX))= REFPARS(1)
                      REFLD(1,MAPFS(IY,IX))= 1+MOD((K*NTH+(K+1)*NTH)/16,NTH)
                      REFLD(2,MAPFS(IY,IX))= 1
                      ENDIF
                    END DO
!                              1 1 1                        1 1 1
! Looks for sharp corners like 1 1 1 but not diagonals like 1 1 1
!                              1 0 1                        1 1 0          
                  IF (REFLC(1,MAPFS(IY,IX)).LE.0) THEN 
                    DO K=0,7,2
                      IF ( NEIGH1(K).EQ.0.AND.NEIGH1(MOD(K+4,8)).NE.0) THEN 
                        REFLC(1,MAPFS(IY,IX))= REFPARS(1)
                        REFLD(1,MAPFS(IY,IX))= 1+(K*NTH)/8
                        REFLD(2,MAPFS(IY,IX))= 0
                        !WRITE(6,*) 'NEIGH3:',IX,IY,K,NEIGH1,K*(NTH/8)
                        END IF
                      END DO
                    END IF
                  END IF
! End of test if surrounding point is land
                END IF
#endif
#ifdef W3_REFT
                IF (REFLC(1,MAPFS(IY,IX)).GT.0)  THEN 
                  WRITE (6,*) 'COAST DIRECTION AT POINT:',IX,IY,' IS ', &
                    REFLD(:,MAPFS(IY,IX)),TH(REFLD(1,MAPFS(IY,IX)))*360/TPI
                  ENDIF
#endif
#ifdef W3_REF1
! End of test if local point is sea
              END IF
            END IF
          END DO
        END DO
#endif
!
      RETURN
!
! Formats
!
!/
!/ End of W3SETREF ----------------------------------------------------- /
!/
      END SUBROUTINE W3SETREF
           
!/
!/ End of module W3GDATMD -------------------------------------------- /
!/
      END MODULE W3GDATMD
