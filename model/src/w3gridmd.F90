#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3GRIDMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |           J. H. Alves             |
!/                  |            F. Ardhuin             |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         27-May-2021 |
!/                  +-----------------------------------+
!/
!/    14-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
!/    27-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
!/                  Add UNFORMATTED bath file option.
!/                  Read options with namelists.
!/    14-Feb-2000 : Adding exact Snl                    ( version 2.01 )
!/    04-May-2000 : Non central source term int.        ( version 2.03 )
!/    24-Jan-2001 : Flat grid option.                   ( version 2.06 )
!/    02-Feb-2001 : Xnl version 3.0                     ( version 2.07 )
!/    09-Feb-2001 : Third propagation scheme added.     ( version 2.08 )
!/    27-Feb-2001 : O0 output switch added.             ( version 2.08 )
!/    16-Mar-2001 : Fourth propagation scheme added.    ( version 2.09 )
!/    29-Mar-2001 : Sub-grid island treatment.          ( version 2.10 )
!/    20-Jul-2001 : Clean up.                           ( version 2.11 )
!/    12-Sep-2001 : Clean up.                           ( version 2.13 )
!/    09-Nov-2001 : Clean up.                           ( version 2.14 )
!/    11-Jan-2002 : Sub-grid ice treatment.             ( version 2.15 )
!/    17-Jan-2002 : DSII bug fix.                       ( version 2.16 )
!/    09-May-2002 : Switch clean up.                    ( version 2.21 )
!/    26-Nov-2002 : Adding first version of NL-3/4.     ( version 3.01 )
!/                  Removed before distribution in 3.12.
!/    26-Dec-2002 : Relaxing CFL time step.             ( version 3.02 )
!/    01-Aug-2003 : Modify GSE correction for moving gr.( version 3.03 )
!/                  Add offset option for first direction.
!/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
!/    04-May-2005 : Allow active points at edge.        ( version 3.07 )
!/    07-Jul-2005 : Add MAPST2 and map processing.      ( version 3.07 )
!/    09-Nov-2005 : Remove soft boundary options.       ( version 3.08 )
!/    23-Jun-2006 : Adding alternative source terms.    ( version 3.09 )
!/                  Module W3SLN1MD, dummy for others.
!/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )
!/    28-Oct-2006 : Spectral partitioning.              ( version 3.09 )
!/    09-Jan-2007 : Correct edges of read mask.         ( version 3.10 )
!/    26-Mar-2007 : Add to spectral partitioning.       ( version 3.11 )
!/    14-Apr-2007 : Add Miche style limiter.            ( version 3.11 )
!/                  ( J. H. Alves )
!/    25-Apr-2007 : Battjes-Janssen Sdb added.          ( version 3.11 )
!/                  ( J. H. Alves )
!/    18-Sep-2007 : Adding WAM4 physics option.         ( version 3.13 )
!/                  ( F. Ardhuin )
!/    09-Oct-2007 : Adding bottom scattering SBS1.      ( version 3.13 )
!/                  ( F. Ardhuin )
!/    22-Feb-2008 : Initialize TRNX-Y properly.         ( version 3.13 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    23-Jul-2009 : Modification of ST3 namelist  .     ( version 3.14-SHOM )
!/    31-Mar-2010 : Addition of shoreline reflection    ( version 3.14-IFREMER )
!/    29-Jun-2010 : Adding Stokes drift profile output  ( version 3.14-IFREMER )
!/    30-Aug-2010 : Adding ST4 option                   ( version 3.14-IFREMER )

!/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
!/                  (W. E. Rogers & T. J. Campbell, NRL)
!/    29-Oct-2010 : Clean up of unstructured grids      ( version 3.14.4 )
!/                  (A. Roland and F. Ardhuin) 
!/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
!/                  specify index closure for a grid. Change GLOBAL
!/                  input in ww3_grid.inp to CSTRG.     ( version 3.14 )
!/                  (T. J. Campbell, NRL)
!/    25-Jun-2011 : Adding movable bed friction         ( version 4.01 )
!/    16-Sep-2011 : Clean up.                           ( version 4.05 )
!/    01-Dec-2011 : New namelist for reflection         ( version 4.05 )
!/    01-Mar-2012 : Bug correction for NLPROP in ST2    ( version 4.05 )
!/    12-Jun-2012 : Add /RTD rotated grid option. JGLi  ( version 4.06 ) 
!/    13-Jul-2012 : Move data structures GMD (SNL3) and nonlinear
!/                  filter (SNLS) from 3.15 (HLT).      ( version 4.07 )
!/    02-Sep-2012 : Clean up of reflection and UG grids ( version 4.08 )
!/    12-Dec-2012 : Adding SMC grid.  JG_Li             ( version 4.08 )
!/    19-Dec-2012 : Add NOSWLL as namelist variable.    ( version 4.OF )
!/    05-Mar-2013 : Adjusted default roughness for rocks( version 4.09 )
!/    01-Jun-2013 : Adding namelist for spectral output ( version 4.10 )
!/    12-Sep-2013 : Adding Arctic part for SMC grid.    ( version 4.11 )
!/    01-Nov-2013 : Changed UG list name to UNST        ( version 4.12 )
!/    11-Nov-2013 : Make SMC and RTD option compatible. ( version 4.13 )
!/    13-Nov-2013 : Moved out reflection to W3UPDTMD    ( version 4.12 )
!/    27-Jul-2013 : Adding free infragravity waves      ( version 4.15 )
!/    02-Dec-2013 : Update of ST4                       ( version 4.16 )
!/    16-Feb-2014 : Adds wind bias correction: WCOR     ( version 5.00 )
!/    10-Mar-2014 : Adding namelist for IC2             ( version 5.01 )
!/    29-May-2014 : Adding namelist for IC3             ( version 5.01 )
!/    15 Oct-2015 : Change SMC grid input files. JGLi   ( version 5.09 )
!/    10-Jan-2017 : Changes for US3D and USSP           ( version 6.01 )
!/    20-Jan-2017 : Bug fix for mask input from file.   ( version 6.02 )
!/    01-Mar-2018 : RTD poles info read from namelist   ( version 6.02 )
!/    14-Mar-2018 : Option to read UNST boundary file   ( version 6.02 )
!/    26-Mar-2018 : Sea-point only Wnd/Cur input. JGLi  ( version 6.02 )
!/    15-May-2018 : Dry sea points over zlim            ( version 6.04 )
!/    06-Jun-2018 : add Implicit grid parameters for unstructured grids
!/                  add DEBUGGRID/DEBUGSTP              ( version 6.04 )
!/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version 6.06 )
!/    20-Jun-2018 : Update of ST6  (Q. Liu)             ( version 6.06 )
!/    26-Aug-2018 : UOST (Mentaschi et al. 2015, 2018)  ( version 6.06 )
!/    27-Aug-2018 : Add WBT parameter                   ( version 6.06 )
!/    22-Jan-2020 : Update default values for IS2       ( version 7.05 ) 
!/    20-Feb-2020 : Include Romero's dissipation in ST4 ( version 7.06 )
!/    15-Apr-2020 : Adds optional opt-out for CFL on BC ( version 7.08 )
!/    18-Jun-2020 : Adds 360-day calendar option        ( version 7.08 )
!/    24-Jun-2020 : RTD output b. c. to rotated grid.   ( version 7.11 )
!/    05-Jan-2021 : Update SMC grid for multi-grid. JGLi( version 7.13 )
!/    27-May-2021 : Updates for IC5 (Q. Liu)            ( version 7.12 )
!/    27-May-2021 : Moved to a subroutine               ( version 7.13 )
!/    07-Jun-2021 : S_{nl} GKE NL5 (Q. Liu)             ( version 7.13 )
!/    19-Jul-2021 : Momentum and air density support    ( version 7.xx )
!/
!/    Copyright 2009-2013 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     "Grid" preprocessing subroutine, which writes a model definition
!     file containing the model parameter settigs and grid data.
!
!  2. Method :
!
!     Information is read from the file ww3_grid.inp (NDSI), or
!     preset in this subroutine. A model definition file mod_def.ww3 is
!     then produced by W3IOGR. Note that the name of the model
!     definition file is set in W3IOGR.
!
!  3. Parameters :
!
!     Local parameters.
!     ----------------------------------------------------------------
!       NDSI    Int.  Input unit number ("ww3_grid.inp").
!       NDSS    Int.  Scratch file.
!       NDSG    Int.  Grid unit ( may be NDSI )
!       NDSTR   Int.  Sub-grid unit ( may be NDSI or NDSG )
!       VSC     Real  Scale factor.
!       VOF     Real  Add offset.
!       ZLIM    Real  Limiting bottom depth, used to define land.
!       IDLA    Int.  Layout indicator used by INA2R.
!       IDFM    Int.  Id. FORMAT indicator.
!       RFORM   C*16  Id. FORMAT.
!       FNAME   C*60  File name with bottom level data.
!       FROM    C*4   Test string for open, 'UNIT' or 'FILE'
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3NMOD    Subr. W3GDATMD Set number of model.
!      W3SETG    Subr.   Id.    Point to selected model.
!      W3DIMS    Subr.   Id.    Set array dims for a spectral grid.
!      W3DIMX    Subr.   Id.    Set array dims for a spatial grid.
!      W3GRMP    Subr. W3GSRUMD Compute bilinear interpolation for point
!      W3NOUT    Subr. W3ODATMD Set number of model for output.
!      W3SETO    Subr.   Id.    Point to selected model for output.
!      W3DMO5    Subr.   Id.    Set array dims for output type 5.
!      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
!      STRACE    Subr.   Id.    Subroutine tracing.
!      NEXTLN    Subr.   Id.    Get next line from input file
!      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
!      DISTAB    Subr. W3DISPMD Make tables for solution of the
!                               dispersion relation.
!      READNL    Subr. Internal Read namelist.
!      INAR2R    Subr. W3ARRYMD Read in an REAL array.
!      PRTBLK    Subr.   Id.    Print plot of array.
!      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     ww3_grid program
!
!  6. Error messages :
!
!  7. Remarks :
!
!      Physical grid :
!     -----------------
!
!     The physical grid is defined by a grid counter IX defining the
!     discrete longitude and IY defining the discrete latitude as shown
!     below. For mathemathical convenience, these grid axes will
!     generally be denoted as the X and Y axes. Two-dimensional arrays
!     describing parameters on this grid are given as A(IY,IX).
!
!           IY=NY
!             ^  |      |      |      |      |      |            ^ N
!             |  |------|------|------|------|------|----        |
!             |  |  ::  |  25  |  26  |  27  |  28  |          --|--
!                |------|------|------|------|------|----        |
!           IY=3 |  ::  |  ::  |  9   |  10  |  11  |            |
!                |------|------|------|------|------|----
!           IY=2 |  ::  |   1  |   2  |  ::  |   3  |
!                |------|------|------|------|------|----
!           IY=1 |  ::  |  ::  |  ::  |  ::  |  ::  |
!                +------+------+------+------+------+----
!                  IX=1   IX=2   IX=3   IX=4   IX=5   ---> IX=NX
!
!                                        :: is a land point.
!
!     To reduce memory usage of the model, spectra are stored for sea
!     points only, in a one-dimensional grid with the length NSEA. This
!     grid is called the storage grid. The definition of the counter
!     in the storage grid is graphically depicted above. To transfer
!     data between the two grids, the maps MAPFS and MAPSF are
!     determined. MAPFS gives the counter of the storage grid ISEA
!     for every physical grid point (IY,IX), such that
!
!             MAPFS(IY,IX) = ISEA
!
!     ISEA = 0 corresponds to land points. The map MAPSF gives the grid
!     counters (IY,IX) for a given storage point ISEA.
!
!             MAPSF(ISEA,1) = IX
!             MAPSF(ISEA,2) = IY
!             MAPSF(ISEA,3) = IY+(IX-1)*NY  ( filled during reading )
!
!     Finally, a status maps MAPSTA and MAPST2 are determined, where
!     the status indicator ISTAT = MAPSTA(IY,IX) determines the type
!     of the grid point.
!
!         ISTAT  Means
!       ---------------------------------------------------
!           0    Point excluded from grid.
!        (-)1    Sea point
!        (-)2    "Active" boundary point (data prescribed)
!
!     For ISTAT=0, the secondary status counter ISTA2 is defined as
!
!         ISTA2  Means
!       ---------------------------------------------------
!           0    Land point.
!           1    Point excluded from grid.
!
!     Negative values of ISTAT identify points that are temporarily
!     taken out of the computation. For these points ISTA2 are
!     defined per bit
!
!         BIT    Means
!       ---------------------------------------------------
!          1     Ice flag (1 = ice coverage)
!          2     Dry flag (1 = dry point with depth 0)
!          3     Inferred land in multi-grid model.
!          4     Masking in multi-grid model.
!          5     land point flag for relocatable grid.
!
!      Thus ISTA2=0 for ISTAT<0 is in error, ISTA2=1 means ice cover,
!      ISTA2=3 means ice on dry point, etc.
!
!      Spectral grid :
!     -----------------
!
!     In the spectral grid (and in physical space in general),
!     the cartesian convention for directions is used, i.e., the
!     direction 0 corresponds to waves propagating in the positive
!     X-direction and 90 degr. corresponds to waves propagating in
!     the positive Y-direction. Similar definitions are used for the
!     internal description of winds and currents. Output can obviously
!     be transformed according to any preferred convention.
!
!          ITH=NTH
!             ^  |      |      |      |      |
!             |  |------|------|------|------|----
!             |  |      |      |      |      |      TH(3) = DTH*2.
!                |------|------|------|------|----
!          ITH=2 |      |      |      |      |      TH(2) = DTH
!                |------|------|------|------|----
!          ITH=1 |      |      |      |      |      TH(1) = 0.
!                +------+------+------+------+----
!                  IK=1   IK=2   IK=3   IK=4   ---> IK=NK
!
!     The spectral grid consists of NK wavenumbers. The first
!     wavenumber IK=1 corresponds to the longest wave. The wavenumber
!     grid varies in space, as given by an invariant relative freq.
!     grid and the local depth. The spectral grid furthermore contains
!     NTH directions, equally spaced over a full circle. the first
!     direction corresponds to the direction 0, etc.
!
! (Begin SMC description)
!
!      Spherical Multiple-Cell (SMC) grid 
!     -----------------------------------
!
!     SMC grid is a multi-resolution grid using cells of multiple times 
!     of each other.  It is similar to the lat-lon grid using rectangular 
!     cells but only cells at sea points are retained.  All land points 
!     have been removed from the model.  At high latitudes, cells are 
!     merged longitudinally to relax the CFL resctiction on time steps. 
!     Near coastlines, cells are divided into quarters in a few steps so 
!     that high resolution is achieved to refine coastlines and resolve 
!     small islands.  At present, three tiers of quarter cells are used.
!     For locating purpose, a usual x-y counter is setup by the smallest 
!     cell size and starting from the south-west corner of the usual 
!     rectuangular domain.  Each sea cell is then given a pair of x-y 
!     index, plus a pair of increments.  These four index are stored in 
!     the cell array IJKCel(NCel, 5), each row holds i, j, di, dj, ndps
!     where ndps is an integer depth in metre.  If precision higher than
!     a metre is required, it may use other unit (cm for instance) with a
!     conversion factor.  
!    
!     For transport calculation, two face arrays, IJKUFc(NUFc, 7) and 
!     IJKVFc(NVFc,8), are also created to store the neighbouring cell 
!     sequential numbers and the face location and size.  The 3 arrays 
!     are calculated outside the wave model and input from text files.
!
!     Boundary condition is added for SMC grid so that it can be used for
!     regional model as well.  Most of the original boundary settings
!     are reclaimed as long as the boundary condition file is provided
!     by a lat-lon grid WW3 model, which will set the interpolation
!     parameters in the boundary condition file.  The NBI number is
!     reset with an input value because the NX-Y double loop overcount
!     the boundary cells for merged cells in the SMC grid.  ISBPI
!     boundary cell mapping array is fine as MAPFS uses duplicated cell
!     number in any merged cell.  From there, all original NBI loops are 
!     reusable. 
!
!     The whole Arctic can be included in the SMC grid if ARCTC variable
!     is set to be .TRUE. within the SMC option.  The ARCTC option appends
!     the polar Arctic part above 86N to the existing SMC grid and uses
!     a map-east reference direction for this extra polar region.
!     Because the map-east direction changes with latitude and longitude
!     the wave spectra defined to the map-east direction could not be 
!     mixed up with the conventional spectra defined to the local east
!     direction.  A rotation sub is provided for convertion from one to
!     another.  Propagation part will be calculated together, including
!     the boundary cells.  The boundary cells are then updated by
!     assigning the corresponding inner cells to them after conversion.
!     Boundary cells are duplicated northmost 4 rows of the global part
!     and they can be excluded for source term and output if required.
!     For convenience, Arctic cellls are all base level cells and are
!     appended to the end of the global cells.  If refined cells were
!     used in the Arctic part, it would not be kept all together, making
!     the sub-loops much more complicated. If refined resolution cells
!     are required for a Arctic regional model, users may consider use
!     the rotated SMC grid options (RTD and SMC).
!
!     For more information about the SMC grid, please refer to  
!     Li, J.G. (2012) Propagation of Ocean Surface Waves on a Spherical
!     Multiple-Cell Grid.  J. Comput. Phys., 231, 8262-8277.  online at
!     http://dx.doi.org/10.1016/j.jcp.2012.08.007
!
! (End SMC description)
!
!     ICEWIND is the scale factor for reduction of wind input by ice 
!     concentration. Value specified corresponds to the fractional 
!     input for 100% ice concentration. Default is 1.0, meaning that
!     100% ice concentration result in zero wind input. 
!     Sin_in_ice=Sin_in_open_water * (1-ICE*ICEWIND)

!     -----------------------------------------------------------------*
!  8. Structure :
!
!     ----------------------------------------------------------------
!        1.   Set up grid storage structure.
!                               ( W3NMOD , W3NOUT , W3SETG , W3SETO )
!        2.a  I-O setup.
!          b  Print heading(s).
!        3.   Prepare int. table for dispersion relation   ( DISTAB )
!        4.   Read and process input file up to spectrum.
!          a  Get comment character
!          b  Name of grid
!          c  Define spectrum                              ( W3DIMS )
!        5.   Set-up discrete spectrum.
!          a  Directions.
!          b  Frequency for spectrum.
!        6.   Read and process input file up to numerical parameters
!          a  Set model flags and time steps
!          b  Set / select source term package
!          c  Pre-process namelists.
!          d  Wind input source term.
!          e  Nonlinear interactions.
!          f  Whitecapping term.
!          g  Bottom friction source term.
!          h  Depth indiced breaking source term.
!          i  Triad interaction source term.
!          j  Bottom scattering source term.
!          k  Undefined source term.
!          l  Set / select propagaton scheme
!          m  Parameters for propagation scheme.
!          n  Set misc. parameters (ice, seeding, ...)
!          o  End of namelist processing
!          p  Set various other variables
!        7.   Read and prepare grid.
!          a  Layout of grid
!          b  Storage of grid of grid
!          c  Read bottom depths
!          d  Set up temp map
!          e  Subgrid information
!            1 Info from input file
!            2 Open file and check if necessary
!            3 Read the data
!            4 Limit
!        8    Finalize status maps
!          a  Determine where to get the data
!             Get data in parts from input file
!             ----------------------------------------------------
!          b  Read and update TMPSTA with bound. and excl. points.
!          c  Finalize excluded points
!             ----------------------------------------------------
!             Read data from file
!             ----------------------------------------------------
!          d  Read data from file
!             ----------------------------------------------------
!          e  Get NSEA and other counters
!          f  Set up all maps                              ( W3DIMX )
!        9.   Prepare output boundary points.
!          a  Read
!          b  Update
!       10.   Write model definition file.                 ( W3IOGR )
!     ----------------------------------------------------------------
!
!  9. Switches :
!
!     !/FLX1  Stresses according to Wu (1980).
!     !/FLX2  Stresses according to T&C (1996).
!     !/FLX3  Stresses according to T&C (1996) with cap on Cd.
!     !/FLX4  Stresses according to Hwang (2011).
!     !/FLX5  Direct use of stress from atmospheric model/input file.
!
!     !/LN0   No linear input source term.
!     !/SEED  'Seeding' of lowest frequency for sufficiently strong
!             winds. Proxi for linear input.
!     !/LN1   Cavaleri and Melanotte-Rizzoli with Tolman filter.
!
!     !/ST0   No source terms included (input/dissipation)
!     !/ST1   WAM-3 physics package.
!     !/ST2   Tolman and Chalikov (1996) physics package.
!     !/ST3   WAM 4+ source terms from P.A.E.M. Janssen and J-R. Bidlot
!     !/ST4   Input and dissipation using saturation following Ardhuin et al. (2009,2010)
!             Filipot & Ardhuin (2010) or Romero (2019)
!     !/ST6   BYDRZ source term package featuring Donelan et al.
!             (2006) input and Babanin et al. (2001,2010) dissipation.
!
!     !/NL0   No nonlinear interactions.
!     !/NL1   Discrete interaction approximation (DIA).
!     !/NL2   Exact interactions (WRT).
!     !/NL3   Generalized Multiple DIA (GMD).
!     !/NL4   Two Scale Approximation
!     !/NL5   Generalized Kinetic Equation (GKE)
!     !/NLS   Snl based HF filter.
!
!     !/BT0   No bottom friction included.
!     !/BT1   JONSWAP bottom friction package.
!     !/BT4   SHOWEX bottom friction using movable bed roughness 
!                  (Tolman 1994, Ardhuin & al. 2003)
!
!     !/IC1   Sink term for interaction with ice (uniform k_i)
!     !/IC2   Sink term for under-ice boundary layer friction 
!                  (Liu et al.    1991: JGR 96 (C3), 4605-4621)
!                  (Liu and Mollo 1988: JPO 18       1720-1712)
!     !/IC3   Sink term for interaction with ice (Wang and Shen method)
!                  (Wang and Shen JGR 2010)
!     !/IC4   Sink term for empirical, frequency-dependent attenuation
!                   in ice (Wadhams et al. 1988: JGR 93 (C6) 6799-6818)
!     !/IC5   Sink term for interaction with ice (effective medium mod.)
!                  (Mosig et al. 2015, Meylan et al. 2018, Liu et al.
!                   2020)
!
!     !/UOST  Unresolved Obstacles Source Term (UOST), Mentaschi et al. 2015
!
!     !/DB0   No depth-induced breaking included.
!     !/DB1   Battjes-Janssen depth-limited breaking.
!     !/MLIM  Mich-style limiter.
!
!     !/TR0   No triad interactions included.
!
!     !/BS0   No bottom scattering included.
!     !/BS1   Routines from F. Ardhuin.
!
!     !/PR1   First order propagation scheme.
!     !/PR2   QUICKEST scheme with ULTIMATE limite and diffusion
!             correction for swell dispersion.
!     !/PR3   Averaging ULTIMATE QUICKEST scheme.
!
!     !/RTD   Rotated regular lat-lon grid. Special case is standard Polat=90.
!     !/SMC   Spherical Multiple-Cell grid, may includes the whole Arctic.   
!
!     !/MGG   GSE correction for moving grid.
!
!     !/S     Enable subroutine tracing.
!     !/T     Enable test output.
!     !/T0    Enable test output tables for boundary output.
!
!     !/O0    Print equivalent namelist setting to std out.
!     !/O1    Print tables with boundary points as part of output.
!     !/O2    Print MAPSTA as part of output.
!     !/O2a   Print land-sea mask in mask.ww3.
!     !/O2b   Print obstruction data.
!     !/O2c   Print extended status map.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
!/
      USE W3TRIAMD
      USE W3GSRUMD, ONLY: W3GRMP
      USE W3ODATMD, ONLY: W3NOUT, W3SETO, W3DMO5
      USE W3IOGRMD, ONLY: W3IOGR
      USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
#ifdef W3_RTD
      USE W3SERVMD, ONLY: W3EQTOLL, W3LLTOEQ
#endif
#ifdef W3_SMC
      USE W3SERVMD, ONLY: W3LLTOEQ 
#endif
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
      USE W3ARRYMD, ONLY: INA2R, INA2I
#ifdef W3_T
      USE W3ARRYMD, ONLY: PRTBLK
#endif
      USE W3DISPMD, ONLY: DISTAB
!/
      USE W3GDATMD
      USE W3ODATMD, ONLY: NDSE, NDST, NDSO
      USE W3ODATMD, ONLY: NBI, NBI2, NFBPO, NBO, NBO2, FLBPI, FLBPO,  &
                          IPBPO, ISBPO, XBPO, YBPO, RDBPO, FNMPRE,    &
                          IHMAX, HSPMIN, WSMULT, WSCUT, FLCOMB,       &
                          NOSWLL, PTMETH, PTFCUT
      USE W3TIMEMD, ONLY: CALTYPE
      USE W3NMLGRIDMD
#ifdef W3_SCRIP
      USE SCRIP_GRIDS, ONLY: GRID1_UNITS, GRID1_NAME, &
                             GRID1_CENTER_LON, GRID1_CENTER_LAT, &
                             GRID1_CORNER_LON, GRID1_CORNER_LAT, &
                             GRID1_MASK, GRID1_SIZE, GRID1_RANK, &
                             GRID1_IMASK, &
                             GRID1_CORNERS, GRID1_DIMS
      USE SCRIP_KINDSMOD
      USE WMSCRPMD
#endif
#ifdef W3_SCRIPNC
    USE NETCDF
#endif
!
#ifdef W3_NL3
      USE W3SNL3MD, ONLY: LAMMAX, DELTHM
#endif
#ifdef W3_NLS
      USE W3SNLSMD, ONLY: ABMAX
#endif
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      TYPE(NML_SPECTRUM_T)     :: NML_SPECTRUM
      TYPE(NML_RUN_T)          :: NML_RUN
      TYPE(NML_TIMESTEPS_T)    :: NML_TIMESTEPS
      TYPE(NML_GRID_T)         :: NML_GRID
      TYPE(NML_RECT_T)         :: NML_RECT
      TYPE(NML_CURV_T)         :: NML_CURV
      TYPE(NML_UNST_T)         :: NML_UNST
      TYPE(NML_SMC_T)          :: NML_SMC
      TYPE(NML_DEPTH_T)        :: NML_DEPTH
      TYPE(NML_MASK_T)         :: NML_MASK
      TYPE(NML_OBST_T)         :: NML_OBST
      TYPE(NML_SLOPE_T)        :: NML_SLOPE
      TYPE(NML_SED_T)          :: NML_SED
      TYPE(NML_INBND_COUNT_T)  :: NML_INBND_COUNT
      TYPE(NML_INBND_POINT_T), ALLOCATABLE  :: NML_INBND_POINT(:)
      TYPE(NML_EXCL_COUNT_T)   :: NML_EXCL_COUNT
      TYPE(NML_EXCL_POINT_T), ALLOCATABLE   :: NML_EXCL_POINT(:)
      TYPE(NML_EXCL_BODY_T), ALLOCATABLE    :: NML_EXCL_BODY(:)
      TYPE(NML_OUTBND_COUNT_T) :: NML_OUTBND_COUNT
      TYPE(NML_OUTBND_LINE_T), ALLOCATABLE  :: NML_OUTBND_LINE(:)
!
      INTEGER, PARAMETER      :: NFL = 6
      INTEGER                 :: NDSI, NDSI2, NDSS, NDSM, NDSG, NDSTR,&
                                 IERR, NDSTRC, NTRACE, ITH, IK, ITH0, &
                                 ISP, IYN(NFL), NRLIN, NRSRCE, NRNL,  &
                                 NRBT, NRDB, NRTR, NRBS, NRPROP,      &
                                 IDLA, IDFM, IX0, IXN, IX, IY, ISEA,  &
                                 IDX, IXO, IDY, IYO, IBA, NBA, ILOOP, &
                                 IFL, NBOTOT, NPO, IP, IX1, IX2, IY1, &
                                 IY2, J, JJ, IXR(4), IYR(4), ISEAI(4),&
                                 IST, NKI, NTHI, NRIC, NRIS, I, IDFT, &
                                 NSTAT, NBT, NLAND, NOSW, NMAPB, IMAPB
#ifdef W3_NL2
      INTEGER            :: IDEPTH
#endif
#ifdef W3_O1
      INTEGER             :: IBI, IP0, IPN, IPH, IPI
#endif
      INTEGER                 :: NCOL =  78
#ifdef W3_SMC
 !!Li     Offset to change Equator index = 0 to regular index JEQT
 !!Li     LvSMC  levels of refinded resolutions for SMC grid.  
 !!Li     NBISMC number of boundary point for regional SMC grid.  
 !!Li     ISHFT for SMC i-index from smc origin to regular grid west edge. 
 !!Li     SMC cell only subgrid obstruction array dimensions NCObst, JObs.
      INTEGER      :: JEQT, LvSMC, NBISMC, JS, NCObst, JObs, ISHFT
      INTEGER      :: NGUI, NGVJ,  NAUI, NAVJ 
#endif
!
#ifdef W3_O2
      INTEGER                 :: NMAP, IMAP
#endif
#ifdef W3_T
      INTEGER                 :: IX3, IY3
#endif
#ifdef W3_T0
      INTEGER                 :: IFILE
#endif
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!
      INTEGER, ALLOCATABLE    :: TMPSTA(:,:), TMPMAP(:,:), READMP(:,:)
#ifdef W3_T
      INTEGER, ALLOCATABLE    :: MAPOUT(:,:)
#endif
!
      REAL                    :: RXFR, RFR1, SIGMA, SXFR, FACHF,      &
                                 VSC, VSC0, VOF,                      &
                                 ZLIM, X, Y, XP,  XO0, YO0, DXO, DYO, &
                                 XO, YO, RD(4), RDTOT,                &
                                 FACTOR, RTH0, FMICHE, RWNDC,         &
                                 WCOR1, WCOR2
!
      CHARACTER(LEN=4)        :: GSTRG, CSTRG
!
! Variables used to allow spectral output on full grid
!
      INTEGER                 :: P2SF,I1P2SF,I2P2SF
      INTEGER                 :: E3D,I1E3D,I2E3D
      INTEGER                 :: US3D,I1US3D,I2US3D,                  &
                                 USSP, IUSSP,                         &
                                 TH1MF, I1TH1M, I2TH1M,               &
                                 STH1MF, I1STH1M, I2STH1M,            &
                                 TH2MF, I1TH2M, I2TH2M,               &
                                 STH2MF, I1STH2M, I2STH2M
      ! STK_WN are the decays for Stokes drift partitions
      REAL                    :: STK_WN(25)

#ifdef W3_DEBUGGRID
       INTEGER     :: nbCase1, nbCase2, nbCase3,           &
                      nbCase4, nbCase5, nbCase6,           &
                      nbCase7, nbCase8
       INTEGER     :: nbTMPSTA0, nbTMPSTA1, nbTMPSTA2
       INTEGER     :: IAPROC
#endif
!
#ifdef W3_LN1
      REAL                    :: CLIN, RFPM, RFHF
#endif
#ifdef W3_ST1
      REAL                    :: CINP, CDIS, APM
#endif
#ifdef W3_ST2
      REAL                    :: PHIMIN, FPIA, FPIB, DPHID
#endif
#ifdef W3_NL1
      REAL                    :: NLPROP
#endif
#ifdef W3_NL2
      REAL                    :: DPTFAC, DEPTHS(100)
#endif
#ifdef W3_NL3
      REAL                    :: QPARMS(500)
#endif
#ifdef W3_NLS
      REAL                    :: A34, FHFC, DNM, FC1, FC2, FC3
#endif
#ifdef W3_BT1
      REAL                    :: GAMMA
#endif
#ifdef W3_PR2
      REAL                    :: LATMIN
#endif
!
#ifdef W3_SMC
      REAL                    :: TRNMX, TRNMY
      INTEGER, ALLOCATABLE    :: NLvCelsk(:),  NLvUFcsk(:),  NLvVFcsk(:)
      INTEGER, ALLOCATABLE    :: IJKCelin(:,:),IJKUFcin(:,:),IJKVFcin(:,:)
      INTEGER, ALLOCATABLE    :: NBICelin(:),  IJKObstr(:,:)
      REAL                    :: PoLonAC, PoLatAC
      INTEGER, ALLOCATABLE    :: IJKCelAC(:,:),IJKUFcAC(:,:),IJKVFcAC(:,:)
      REAL,    ALLOCATABLE    :: XLONAC(:),YLATAC(:),ELONAC(:),ELATAC(:) 
#endif
!
#ifdef W3_RTD
      REAL, ALLOCATABLE       :: AnglDin(:,:),StdLon(:,:),StdLat(:,:)
 ! 1-dim boundary sectors
      REAL, ALLOCATABLE       :: BDYLON(:), BDYLAT(:),                &
                                 ELatbdy(:), ELonbdy(:), Anglbdy(:)
 ! If the destination grid for an output b.c. is rotated, its pole is:
      REAL                    :: bPolat, bPolon
!
#endif
      REAL, ALLOCATABLE       :: XGRDIN(:,:), YGRDIN(:,:)
      REAL, ALLOCATABLE       :: ZBIN(:,:), OBSX(:,:), OBSY(:,:)
      REAL, ALLOCATABLE       :: REFD(:,:), REFD2(:,:), REFS(:,:)
#ifdef W3_BT4
      REAL, ALLOCATABLE  :: SED_D50FILE(:,:), SED_POROFILE(:,:)
      LOGICAL            :: SEDMAPD50
      REAL               :: SED_D50_UNIFORM, SED_DSTAR, RIPFAC1, &
                            RIPFAC2, RIPFAC3, RIPFAC4, SIGDEPTH, &
                            BOTROUGHMIN, BOTROUGHFAC
#endif
!
      LOGICAL                 :: FLLIN, FLINDS, FLNL, FLBT, FLDB,     &
                                 FLTR, FLBS, FLPROP, FLREF,     &
                                 FIRST, CONNCT, FLNEW, INGRID,FLIC,   &
                                 FLIS, FLGNML
      LOGICAL                 :: FLTC96 = .FALSE.
      LOGICAL                 :: FLNMLO = .FALSE.
      LOGICAL                 :: FLSTB2 = .FALSE.
      LOGICAL                 :: FLST4  = .FALSE.
      LOGICAL                 :: FLST6  = .FALSE.

      REAL                    :: FACBERG, REFSLOPE
#ifdef W3_IS1
      REAL                    :: ISC1, ISC2
#endif
#ifdef W3_IS2
      REAL                    :: ISC1, IS2BACKSCAT, IS2C2, IS2C3,&
                                 IS2FRAGILITY, IS2DMIN, IS2DAMP, &
                                 IS2CONC, IS2CREEPB, IS2CREEPC,  &
                                 IS2CREEPD, IS2CREEPN, IS2BREAKE,&
                                 IS2WIM1, IS2BREAKF, IS2FLEXSTR, &
                                 IS2ANDISN, IS2ANDISE, IS2ANDISD
      LOGICAL                 :: IS2BREAK, IS2DISP, IS2DUPDATE,  &
                                 IS2ISOSCAT, IS2ANDISB
#endif
!
#ifdef W3_REF1
      REAL                    :: REFCOAST, REFFREQ, REFMAP,     &
                                 REFSUBGRID, REFRMAX, REFMAPD,  &
                                 REFICEBERG, REFCOSP_STRAIGHT,  &
                                 REFFREQPOW, REFUNSTSOURCE
#endif
!
#ifdef W3_IG1
  LOGICAL                 :: IGSWELLMAX, IGBCOVERWRITE
  INTEGER                 :: IGMETHOD, IGADDOUTP, IGSOURCE,      &
                             IGSOURCEATBP, IGSTERMS
  REAL                    :: IGMAXFREQ, IGMINDEP, IGMAXDEP,      &
                             IGKDMIN, IGFIXEDDEPTH, IGEMPIRICAL
#endif
!
#ifdef W3_IC2
  LOGICAL                 :: IC2DISPER
  REAL                    :: IC2TURB, IC2ROUGH, IC2REYNOLDS,      &
                             IC2SMOOTH, IC2VISC, IC2TURBS, IC2DMAX
#endif

#ifdef W3_IC3
  REAL                    :: IC2TURB, IC2ROUGH, IC2REYNOLDS,      &
                             IC2SMOOTH, IC2VISC, IC2TURBS,        &
                             IC3MAXTHK, IC3MAXCNC,                &
                             IC3HILIM, IC3KILIM,                  &
                             IC3VISC, IC3ELAS, IC3DENS, IC3HICE
  LOGICAL                 :: IC3CHENG,USECGICE
#endif

#ifdef W3_IC4
  INTEGER                 :: IC4METHOD
  REAL                    :: IC4KI(NIC4), IC4FC(NIC4)
#endif
!
#ifdef W3_IC5
  REAL                    :: IC5MINIG, IC5MINWT,                  &
                             IC5MAXKRATIO, IC5MAXKI, IC5MINHW,    &
                             IC5MAXITER, IC5RKICK, IC5KFILTER,    &
                             IC5VEMOD
  CHARACTER(LEN=4)        :: IC5MSTR(3) = (/' EFS', ' RP ', ' M2 '/)
#endif

      CHARACTER               :: COMSTR*1, PNAME*30, RFORM*16,        &
                                 FROM*4, FNAME*60, TNAME*60, LINE*80, &
                                 STATUS*20,FNAME2*60, PNAME2*40
      CHARACTER(LEN=6)        :: YESXNO(2)
#ifdef W3_FLX3
      CHARACTER(LEN=18)       :: TYPEID
#endif

#ifdef W3_SCRIP
 INTEGER :: NCID 
 INTEGER :: grid_size_dimid, grid_rank_dimid, grid_corners_dimid
 INTEGER :: grid_center_lat_varid, grid_center_lon_varid
 INTEGER :: grid_corner_lat_varid, grid_corner_lon_varid
 INTEGER :: grid_area_varid, grid_imask_varid 
 INTEGER :: grid_dims_varid
 REAL (SCRIP_R8) :: CONV_DX,CONV_DY,OFFSET
#endif

!/ ------------------------------------------------------------------- /
!/ Namelists
!/
      INTEGER                 :: FLAGTR, IHM
      REAL                    :: CFLTM, CICE0, CICEN, PMOVE, XFILT,    &
                                 LICE, XSEED, XR, HSPM, WSM, WSC, STDX,&
                                 STDY, STDT, ICEHMIN, ICEHFAC, ICEHINIT, &
                                 ICESLN, ICEWIND, ICESNL, ICESDS,        &
                                 ICEHDISP, ICEFDISP, ICEDDISP, BTBET
!
      REAL(8)                 :: GSHIFT ! see notes in WMGHGH
      LOGICAL                 :: FLC, ICEDISP, TRCKCMPR
      INTEGER                 :: PTM   ! Partitioning method
      REAL                    :: PTFC  ! Part. cut off freq (for method 5)
      REAL                    :: AIRCMIN, AIRGB
      CHARACTER               :: PMNAME*45, PMNAM2*45  ! Part. method desc.
#ifdef W3_FLD1
      INTEGER                 :: TAILTYPE
      REAL                    :: TAILLEV, TAILT1, TAILT2
#endif
#ifdef W3_FLD2
      INTEGER                 :: TAILTYPE
      REAL                    :: TAILLEV, TAILT1, TAILT2
#endif
#ifdef W3_FLX3
      INTEGER                 :: CTYPE
      REAL                    :: CDMAX
#endif
#ifdef W3_FLX4
      REAL                    :: CDFAC
#endif
#ifdef W3_ST2
      REAL                    :: ZWND, SWELLF, STABSH, STABOF,        &
                                 CNEG, CPOS, FNEG, FPOS
      REAL                    :: SDSA0, SDSA1, SDSA2,                 &
                                 SDSB0, SDSB1, SDSB2, SDSB3
#endif
#ifdef W3_ST3
      REAL                    :: ZWND, ALPHA0, Z0MAX, BETAMAX, SINTHP,&
                                 ZALP, SWELLF, FXPM3, FXFM3,          &
                                 WNMEANPTAIL, WNMEANP, STXFTF, STXFTWN
      REAL                    :: STXFTFTAIL, SDSC1,                   &
                                 SDSDELTA1, SDSDELTA2
#endif
!
#ifdef W3_ST4
      INTEGER                 :: SWELLFPAR, SDSISO, SDSBRFDF
      REAL 		   :: SDSBCHOICE
      REAL                    :: ZWND, ALPHA0, Z0MAX, BETAMAX, SINTHP,&
                                 ZALP, Z0RAT, TAUWSHELTER, SWELLF,    &
                                 SWELLF2,SWELLF3,SWELLF4, SWELLF5,    &
                                 SWELLF6, SWELLF7, FXPM3, FXFM3,      &
                                 WNMEANPTAIL, WNMEANP, STXFTF, STXFTFTAIL,       &
                                 STXFTWN, SINBR, FXFMAGE,             &
                                 SDSC2, SDSCUM, SDSC4, SDSC5, SDSC6, WHITECAPWIDTH, WHITECAPDUR, &
                                 SDSSTRAIN, SDSSTRAINA, SDSSTRAIN2,   &
                                 SDSBR, SDSP, SDSBT, SDS4A, SDKOF,    &
                                 SDSCOS, SDSDTH, SDSBCK, SDSABK,      &
                                 SDSPBK, SDSBINT, SDSHCK,             &               
                                 SDSBRF1,                             &
                                 SDSBM0, SDSBM1, SDSBM2, SDSBM3,      &
                                 SDSBM4, SDSFACMTF, SDSCUMP,  SDSNUW, &
                                 SDSL, SDSMWD, SDSMWPOW, SPMSS, SDSNMTF
#endif
!
#ifdef W3_ST6
      REAL                    :: SINA0, SINWS, SINFC,                 &
                                 SDSA1, SDSA2, SWLB1
      INTEGER                 :: SDSP1, SDSP2
      LOGICAL                 :: SDSET, CSTB1
#endif
!
#ifdef W3_NL1
      REAL                    :: LAMBDA, KDCONV, KDMIN,               &
                                 SNLCS1, SNLCS2, SNLCS3
#endif
#ifdef W3_NL2
      INTEGER                 :: IQTYPE, NDEPTH
      REAL                    :: TAILNL
#endif
#ifdef W3_NL3
      INTEGER                 :: NQDEF
      REAL                    :: MSC, NSC, KDFD, KDFS
#endif
#ifdef W3_NL4
      INTEGER                 :: INDTSA, ALTLP
#endif
#ifdef W3_NL5
      REAL                    :: NL5DPT, NL5OML
      INTEGER                 :: NL5DIS, NL5KEV, NL5IPL, NL5PMX
#endif
#ifdef W3_DB1
      REAL                    :: BJALFA, BJGAM
      LOGICAL                 :: BJFLAG
#endif
#ifdef W3_PR2
      REAL                    :: DTIME
#endif
!
#ifdef W3_SMC
      REAL                    :: DTIMS, CFLSM, RFMAXD, SYMR, YJ0R
      LOGICAL                 :: UNO3, AVERG, SEAWND, Arctic 
      CHARACTER               :: PNSMC*30 
#endif
!
#ifdef W3_PR3
      REAL                    :: WDTHCG, WDTHTH
#endif
           LOGICAL :: JGS_TERMINATE_MAXITER = .TRUE.
           LOGICAL :: JGS_TERMINATE_DIFFERENCE = .TRUE.
           LOGICAL :: JGS_TERMINATE_NORM = .TRUE.
           LOGICAL :: JGS_LIMITER = .FALSE.
           LOGICAL :: JGS_BLOCK_GAUSS_SEIDEL = .TRUE.
           LOGICAL :: JGS_USE_JACOBI = .TRUE.
           LOGICAL :: JGS_SOURCE_NONLINEAR = .FALSE.
           LOGICAL :: UGOBCAUTO = .FALSE.
           LOGICAL :: UGBCCFL   = .FALSE.
           LOGICAL :: EXPFSN    = .TRUE.
           LOGICAL :: EXPFSPSI  = .FALSE.
           LOGICAL :: EXPFSFCT  = .FALSE.
           LOGICAL :: IMPFSN    = .FALSE.
           LOGICAL :: EXPTOTAL  = .FALSE.
           LOGICAL :: IMPTOTAL  = .FALSE.
           LOGICAL :: IMPREFRACTION = .FALSE.
           LOGICAL :: IMPFREQSHIFT = .FALSE.
           LOGICAL :: IMPSOURCE = .FALSE.
           LOGICAL :: SETUP_APPLY_WLV = .FALSE.
           INTEGER :: JGS_MAXITER=100
           INTEGER :: nbSel
           INTEGER :: UNSTSCHEMES(4)
           INTEGER :: UNSTSCHEME
           INTEGER :: JGS_NLEVEL = 0
           REAL*8  :: JGS_PMIN = 0.
           REAL*8  :: JGS_DIFF_THR = 1.E-10
           REAL*8  :: JGS_NORM_THR = 1.E-20
           REAL*8  :: SOLVERTHR_SETUP = 1.E-20
           REAL*8  :: CRIT_DEP_SETUP = 0.
!
           CHARACTER               :: UGOBCFILE*60
           REAL                    :: UGOBCDEPTH 
           LOGICAL                 :: UGOBCOK

#ifdef W3_RTD
      REAL                    :: PLAT, PLON
      LOGICAL                 :: UNROT
 ! Poles of the output nested grids. May be a mix of rotated and standard
      REAL, DIMENSION(9)      :: BPLAT, BPLON
#endif
!
#ifdef W3_FLD1
      NAMELIST /FLD1/ TAILTYPE, TAILLEV, TAILT1, TAILT2
#endif
#ifdef W3_FLD2
      NAMELIST /FLD2/ TAILTYPE, TAILLEV, TAILT1, TAILT2
#endif
#ifdef W3_FLX3
      NAMELIST /FLX3/ CDMAX, CTYPE
#endif
#ifdef W3_FLX4
      NAMELIST /FLX4/ CDFAC
#endif
#ifdef W3_IC2
      NAMELIST /SIC2/  IC2DISPER, IC2TURB, IC2ROUGH, IC2REYNOLDS,      &
                      IC2SMOOTH, IC2VISC, IC2TURBS, IC2DMAX
#endif
#ifdef W3_IC3
      NAMELIST /SIC3/  IC3MAXTHK, IC2TURB, IC2ROUGH, IC2REYNOLDS,      &
                      IC2SMOOTH, IC2VISC, IC2TURBS, IC3MAXCNC,      &
                      IC3CHENG, USECGICE, IC3HILIM, IC3KILIM,      &
                      IC3VISC, IC3ELAS, IC3DENS, IC3HICE
#endif
#ifdef W3_IC4
      NAMELIST /SIC4/  IC4METHOD, IC4KI, IC4FC
#endif
#ifdef W3_IC5
      NAMELIST /SIC5/  IC5MINIG, IC5MINWT, IC5MAXKRATIO,        &
                       IC5MAXKI, IC5MINHW, IC5MAXITER, IC5RKICK,&
                       IC5KFILTER, IC5VEMOD
#endif
#ifdef W3_IG1
      NAMELIST /SIG1/  IGMETHOD, IGADDOUTP, IGSOURCE, IGBCOVERWRITE,   &
                      IGMAXFREQ, IGSTERMS, IGSWELLMAX,                &
                      IGSOURCEATBP, IGKDMIN, IGFIXEDDEPTH, IGEMPIRICAL 
#endif
#ifdef W3_LN1
      NAMELIST /SLN1/ CLIN, RFPM, RFHF
#endif
#ifdef W3_ST1
      NAMELIST /SIN1/ CINP
#endif
#ifdef W3_ST2
      NAMELIST /SIN2/ ZWND, SWELLF, STABSH, STABOF, CNEG, CPOS, FNEG
#endif
#ifdef W3_ST3
      NAMELIST /SIN3/ ZWND, ALPHA0, Z0MAX, BETAMAX, SINTHP, ZALP, &
                      SWELLF
#endif
#ifdef W3_ST4
      NAMELIST /SIN4/ ZWND, ALPHA0, Z0MAX, BETAMAX, SINTHP, ZALP, &
                      TAUWSHELTER, SWELLFPAR, SWELLF,                 &
                      SWELLF2, SWELLF3, SWELLF4, SWELLF5, SWELLF6,    &
                      SWELLF7, Z0RAT, SINBR
#endif
#ifdef W3_NL1
      NAMELIST /SNL1/ LAMBDA, NLPROP, KDCONV, KDMIN,                  &
                      SNLCS1, SNLCS2, SNLCS3
#endif
#ifdef W3_NL2
      NAMELIST /SNL2/ IQTYPE, TAILNL, NDEPTH
      NAMELIST /ANL2/ DEPTHS
#endif
#ifdef W3_NL3
      NAMELIST /SNL3/ NQDEF, MSC, NSC, KDFD, KDFS
      NAMELIST /ANL3/ QPARMS
#endif
#ifdef W3_NL4
      NAMELIST /SNL4/ INDTSA, ALTLP
#endif
#ifdef W3_NL5
      NAMELIST /SNL5/ NL5DPT, NL5OML, NL5DIS, NL5KEV, NL5IPL, NL5PMX
#endif
#ifdef W3_NLS
      NAMELIST /SNLS/ A34, FHFC, DNM, FC1, FC2, FC3
#endif
#ifdef W3_ST1
      NAMELIST /SDS1/ CDIS, APM
#endif
#ifdef W3_ST2
      NAMELIST /SDS2/ SDSA0, SDSA1, SDSA2, SDSB0, SDSB1, PHIMIN
#endif
#ifdef W3_ST3
      NAMELIST /SDS3/ SDSC1, WNMEANP, FXPM3, FXFM3, SDSDELTA1,        &
                      SDSDELTA2
#endif
#ifdef W3_ST4
      NAMELIST /SDS4/ SDSBCHOICE, WNMEANP, WNMEANPTAIL, FXPM3, FXFM3, &
                      FXFMAGE, SDSC2, SDSCUM, SDSSTRAIN, SDSSTRAINA,  &
                      SDSSTRAIN2, SDSC4, SDSFACMTF, SDSNMTF,SDSCUMP,  &
                      SDSC5, SDSC6, SDSBR, SDSBT, SDSP, SDSISO,       &
                      SDSBCK, SDSABK, SDSPBK, SDSBINT, SDSHCK,        &
                      SDSDTH, SDSCOS, SDSBRF1, SDSBRFDF,  SDSNUW,     &
                      SDSBM0, SDSBM1, SDSBM2, SDSBM3, SDSBM4,         &
                      WHITECAPWIDTH, WHITECAPDUR, SDSMWD, SDSMWPOW, SDKOF
#endif

#ifdef W3_ST6
      NAMELIST /SIN6/ SINA0, SINWS, SINFC
      NAMELIST /SDS6/ SDSET, SDSA1, SDSA2, SDSP1, SDSP2
      NAMELIST /SWL6/ SWLB1, CSTB1
#endif
#ifdef W3_BT1
      NAMELIST /SBT1/ GAMMA
#endif
#ifdef W3_BT4
      NAMELIST /SBT4/ SEDMAPD50, SED_D50_UNIFORM, RIPFAC1,            &
                      RIPFAC2, RIPFAC3, RIPFAC4, SIGDEPTH,            &
                      BOTROUGHMIN, BOTROUGHFAC
#endif
#ifdef W3_DB1
      NAMELIST /SDB1/ BJALFA, BJGAM, BJFLAG
#endif
#ifdef W3_UOST
     NAMELIST /UOST/ UOSTFILELOCAL, UOSTFILESHADOW,             &
                     UOSTFACTORLOCAL, UOSTFACTORSHADOW
#endif
!
#ifdef W3_PR1
      NAMELIST /PRO1/ CFLTM
#endif
#ifdef W3_PR2
      NAMELIST /PRO2/ CFLTM, DTIME, LATMIN
#endif
#ifdef W3_SMC
      NAMELIST /PSMC/ CFLSM, DTIMS, RFMAXD, Arctic, AVERG, UNO3, &
                      LvSMC, ISHFT, JEQT,   NBISMC, SEAWND 
#endif
! 
#ifdef W3_PR3
      NAMELIST /PRO3/ CFLTM, WDTHCG, WDTHTH
#endif
           NAMELIST /UNST/ UGOBCAUTO, UGOBCDEPTH, UGOBCFILE,          &
                           UGBCCFL, EXPFSN, EXPFSPSI, EXPFSFCT,       &
                           IMPFSN, IMPTOTAL, EXPTOTAL,                &
                           IMPREFRACTION, IMPFREQSHIFT,               &
                           IMPSOURCE,                                 &
                           JGS_TERMINATE_MAXITER,                     &
                           JGS_TERMINATE_DIFFERENCE,                  &
                           JGS_TERMINATE_NORM,                        &
                           JGS_LIMITER,                               &
                           JGS_USE_JACOBI,                            &
                           JGS_BLOCK_GAUSS_SEIDEL,                    &
                           JGS_MAXITER,                               &
                           JGS_PMIN,                                  &
                           JGS_DIFF_THR,                              &
                           JGS_NORM_THR,                              &
                           JGS_NLEVEL,                                &
                           JGS_SOURCE_NONLINEAR,                      &
                           SETUP_APPLY_WLV, SOLVERTHR_SETUP,          &
                           CRIT_DEP_SETUP
           NAMELIST /MISC/ CICE0, CICEN, LICE, XSEED, FLAGTR, XP, XR, & 
                      XFILT, PMOVE, IHM, HSPM, WSM, WSC, FLC, FMICHE, &
                      RWNDC, FACBERG, NOSW, GSHIFT, WCOR1, WCOR2,     &
                      STDX, STDY, STDT, ICEHMIN, ICEHINIT, ICEDISP,   &
                      ICESLN, ICEWIND, ICESNL, ICESDS, ICEHFAC,       &
                      ICEHDISP, ICEDDISP, ICEFDISP, CALTYPE,          &
                      TRCKCMPR, PTM, PTFC, BTBET
           NAMELIST /OUTS/ P2SF, I1P2SF, I2P2SF,                      &
                             US3D, I1US3D, I2US3D,                    &
                             USSP, IUSSP, STK_WN,                     &
                             E3D, I1E3D, I2E3D,                       &
                             TH1MF, I1TH1M, I2TH1M,                   &
                             STH1MF, I1STH1M, I2STH1M,                &
                             TH2MF, I1TH2M, I2TH2M,                   &
                             STH2MF, I1STH2M, I2STH2M
#ifdef W3_IS1
           NAMELIST /SIS1/ ISC1, ISC2
#endif
#ifdef W3_IS2
           NAMELIST /SIS2/ ISC1, IS2C2, IS2C3, IS2BACKSCAT, IS2ISOSCAT, IS2BREAK,  &
                           IS2DISP, IS2FRAGILITY, IS2CONC, IS2DMIN,    &
                           IS2DAMP, IS2DUPDATE, IS2CREEPB, IS2CREEPC,  &
                           IS2CREEPD, IS2CREEPN, IS2BREAKE, IS2BREAKF, &
                           IS2WIM1, IS2FLEXSTR, IS2ANDISB, IS2ANDISE, IS2ANDISD,   &
                           IS2ANDISN
#endif
#ifdef W3_REF1
           NAMELIST /REF1/ REFCOAST, REFFREQ, REFMAP,  REFMAPD,       &
                          REFSUBGRID, REFICEBERG,                     &
                          REFCOSP_STRAIGHT, REFSLOPE, REFRMAX,        &
                          REFFREQPOW, REFUNSTSOURCE
#endif
!/
#ifdef W3_RTD
      NAMELIST /ROTD/ PLAT, PLON, UNROT
! Poles of destination grids for boundary conditions output 
      NAMELIST /ROTB/ BPLAT, BPLON
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      DATA YESXNO / 'YES/--' , '---/NO' /

      CONTAINS

      SUBROUTINE W3GRID()

#ifdef W3_O0
      FLNMLO = .TRUE.
#endif
#ifdef W3_STAB2
      FLSTB2 = .TRUE.
#endif
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Set up grid storage structure
!
      CALL W3NMOD ( 1, 6, 6 )
      CALL W3SETG ( 1, 6, 6 )
      CALL W3NOUT (    6, 6 )
      CALL W3SETO ( 1, 6, 6 )
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  IO set-up.
!
#ifdef W3_DEBUGGRID
      IAPROC = 1
#endif
      NDSI   = 10
      NDSS   = 99
      NDSM   = 20
!
      INQUIRE(FILE=TRIM(FNMPRE)//"ww3_grid.nml", EXIST=FLGNML) 
      IF (FLGNML) THEN
         ! Read namelist
         CALL W3NMLGRID (NDSI, TRIM(FNMPRE)//'ww3_grid.nml', NML_SPECTRUM, NML_RUN,  &
                         NML_TIMESTEPS, NML_GRID, NML_RECT, NML_CURV,   &
                         NML_UNST, NML_SMC, NML_DEPTH, NML_MASK,        &
                         NML_OBST, NML_SLOPE, NML_SED, NML_INBND_COUNT, &
                         NML_INBND_POINT, NML_EXCL_COUNT,               &                  
                         NML_EXCL_POINT, NML_EXCL_BODY,                 &
                         NML_OUTBND_COUNT, NML_OUTBND_LINE, IERR)
      ELSE
        OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_grid.inp',STATUS='OLD',        &
              ERR=2000,IOSTAT=IERR)
      END IF
!
      NDSTRC =  6
      NTRACE =  10
      CALL ITRACE ( NDSTRC, NTRACE )
!
#ifdef W3_S
      CALL STRACE (IENT, 'W3GRID')
#endif
      WRITE (NDSO,900)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.a Interpolation table for dispersion relation.
!
      CALL DISTAB
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.b Table for friction factors
!
      CALL TABU_FW
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 4   Read and process input file up to spectrum
!

      IF (FLGNML) THEN
         ! grid name
         GNAME=TRIM(NML_GRID%NAME)
         WRITE (NDSO,902) GNAME

         ! spectrum parameters
         RXFR=NML_SPECTRUM%XFR
         RFR1=NML_SPECTRUM%FREQ1
         NKI=NML_SPECTRUM%NK
         NTHI=NML_SPECTRUM%NTH
         RTH0=NML_SPECTRUM%THOFF

      ELSE

        READ (NDSI,'(A)',END=2001,ERR=2002,IOSTAT=IERR) COMSTR
        IF (COMSTR.EQ.' ') COMSTR = '$'
        WRITE (NDSO,901) COMSTR
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
!
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=2001,ERR=2002) GNAME
        WRITE (NDSO,902) GNAME
!
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=2001,ERR=2002) RXFR, RFR1, NKI, NTHI, RTH0
      END IF


      NK     = NKI
      NK2    = NKI + 2
      NTH    = NTHI
      NSPEC  = NK * NTH
      XFR    = MAX ( RXFR , 1.00001 )
      FR1    = MAX ( RFR1 , 1.E-6 )
      DTH    = TPI / REAL(NTH)
      RTH0   = MAX ( -0.5 , MIN ( 0.5 , RTH0 ) )
      WRITE (NDSO,903) NTH, DTH*RADE
      WRITE (NDSO,904) 360./REAL(NTH)*RTH0
      WRITE (NDSO,905) NK, FR1, FR1*XFR**(NK-1), XFR
!
      CALL W3DIMS ( 1, NK, NTH, NDSE, NDST )
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 5.  Initialize spectral parameters.
! 5.a Directions :
!
      DO ITH=1, NTH
        TH  (ITH) = DTH * ( RTH0 + REAL(ITH-1) )
        ESIN(ITH) = SIN ( TH(ITH) )
        ECOS(ITH) = COS ( TH(ITH) )
        IF ( ABS(ESIN(ITH)) .LT. 1.E-5 ) THEN
          ESIN(ITH) = 0.
          IF ( ECOS(ITH) .GT. 0.5 ) THEN
            ECOS(ITH) =  1.
          ELSE
            ECOS(ITH) = -1.
            END IF
          END IF
        IF ( ABS(ECOS(ITH)) .LT. 1.E-5 ) THEN
          ECOS(ITH) = 0.
          IF ( ESIN(ITH) .GT. 0.5 ) THEN
            ESIN(ITH) =  1.
          ELSE
            ESIN(ITH) = -1.
            END IF
          END IF
        ES2 (ITH) = ESIN(ITH)**2
        EC2 (ITH) = ECOS(ITH)**2
        ESC (ITH) = ESIN(ITH)*ECOS(ITH)
        END DO
!
      DO IK=2, NK+1
        ITH0   = (IK-1)*NTH
        DO ITH=1, NTH
          ESIN(ITH0+ITH) = ESIN(ITH)
          ECOS(ITH0+ITH) = ECOS(ITH)
          ES2 (ITH0+ITH) = ES2 (ITH)
          EC2 (ITH0+ITH) = EC2 (ITH)
          ESC (ITH0+ITH) = ESC (ITH)
          END DO
        END DO
!
!   b Frequencies :
!
      SIGMA   = FR1 * TPI / XFR**2
      SXFR    = 0.5 * (XFR-1./XFR)
!
      DO IK=0, NK+1
        SIGMA    = SIGMA * XFR
        SIG (IK) = SIGMA
        DSIP(IK) = SIGMA * SXFR
        END DO
!
      DSII( 1) = 0.5 * SIG( 1) * (XFR-1.)
      DO IK=2, NK-1
        DSII(IK) = DSIP(IK)
        END DO
      DSII(NK) = 0.5 * SIG(NK) * (XFR-1.) / XFR
!
      DO IK=1, NK
        DDEN(IK) = DTH * DSII(IK) * SIG(IK)
        END DO
!
      DO ISP=1, NSPEC
        IK         = 1 + (ISP-1)/NTH
        SIG2 (ISP) = SIG (IK)
        DDEN2(ISP) = DDEN(IK)
        END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 6   Read and process input file up to numerical parameters
! 6.a Set model flags and time steps
!
      WRITE (NDSO,910)
      IF (FLGNML) THEN
        FLDRY=NML_RUN%FLDRY
        FLCX=NML_RUN%FLCX
        FLCY=NML_RUN%FLCY
        FLCTH=NML_RUN%FLCTH
        FLCK=NML_RUN%FLCK
        FLSOU=NML_RUN%FLSOU
      ELSE
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=2001,ERR=2002)                                 &
          FLDRY, FLCX, FLCY, FLCTH, FLCK, FLSOU
      END IF
!
      IYN = 2
      IF ( FLDRY ) IYN(1) = 1
      IF ( FLCX  ) IYN(2) = 1
      IF ( FLCY  ) IYN(3) = 1
      IF ( FLCTH ) IYN(4) = 1
      IF ( FLCK  ) IYN(5) = 1
      IF ( FLSOU ) IYN(6) = 1
!
      WRITE (NDSO,911) (YESXNO(IYN(IFL)),IFL=1,NFL)
!
      IF ( .NOT. (FLDRY.OR.FLCX.OR.FLCY.OR.FLCK.OR.FLCTH.OR.FLSOU) ) THEN
          WRITE (NDSE,1010)
          CALL EXTCDE ( 2 )
        END IF
!
      IF (FLGNML) THEN
        DTMAX=NML_TIMESTEPS%DTMAX
        DTCFL=NML_TIMESTEPS%DTXY
        DTCFLI=NML_TIMESTEPS%DTKTH
        DTMIN=NML_TIMESTEPS%DTMIN
      ELSE
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=2001,ERR=2002) DTMAX, DTCFL, DTCFLI, DTMIN
      END IF
#ifdef W3_SEC1
      IF (DTMAX.LT.1.) THEN 
        NITERSEC1=CEILING(1./DTMAX)
        WRITE (NDSO,913) NITERSEC1
      ELSE 
        NITERSEC1=1
        END IF
#endif

      DTMAX  = MAX ( 1. , DTMAX )
!
! Commented to allow very high resolution zooms
!
!      DTCFL  = MAX ( 1. , DTCFL  )
!      DTCFLI = MIN ( DTMAX , MAX ( 1. , DTCFLI ) )
      DTMIN  = MIN ( DTMAX , MAX ( 0. , DTMIN  ) )
      WRITE (NDSO,912) DTMAX, DTCFL, DTCFLI, DTMIN
!
! 6.b Set / select source term package
!
      NRLIN  = 0
      NRSRCE = 0
      NRNL   = 0
      NRBT   = 0
      NRIC   = 0
      NRIS   = 0
      NRDB   = 0
      NRTR   = 0
      NRBS   = 0
!
      FLLIN  = .TRUE.
      FLINDS = .TRUE.
      FLNL   = .TRUE.
      FLBT   = .TRUE.
      FLIC   = .FALSE.
      FLIS   = .FALSE.
      FLDB   = .TRUE.
      FLTR   = .TRUE.
      FLBS   = .TRUE.
      FLREF  = .FALSE.
!
#ifdef W3_LN0
      NRLIN  = NRLIN + 1
      FLLIN  = .FALSE.
#endif
#ifdef W3_SEED
      NRLIN  = NRLIN + 1
#endif
#ifdef W3_LN1
      NRLIN  = NRLIN + 1
#endif
!
#ifdef W3_ST0
      NRSRCE = NRSRCE + 1
      FLINDS = .FALSE.
#endif
#ifdef W3_ST1
      NRSRCE = NRSRCE + 1
#endif
#ifdef W3_ST2
      NRSRCE = NRSRCE + 1
      FLTC96 = .TRUE.
#endif
#ifdef W3_ST3
      NRSRCE = NRSRCE + 1
#endif
#ifdef W3_ST4
      NRSRCE = NRSRCE + 1
      FLST4  = .TRUE.
#endif
#ifdef W3_ST6
      NRSRCE = NRSRCE + 1
      FLST6  = .TRUE.
#endif
!
#ifdef W3_NL0
      NRNL   = NRNL + 1
      FLNL   = .FALSE.
#endif
#ifdef W3_NL1
      NRNL   = NRNL + 1
#endif
#ifdef W3_NL2
      NRNL   = NRNL + 1
#endif
#ifdef W3_NL3
      NRNL   = NRNL + 1
#endif
#ifdef W3_NL4
      NRNL   = NRNL + 1
#endif
#ifdef W3_NL5
      NRNL   = NRNL + 1
#endif
!
#ifdef W3_BT0
      NRBT   = NRBT + 1
      FLBT   = .FALSE.
#endif
#ifdef W3_BT1
      NRBT   = NRBT + 1
#endif
#ifdef W3_BT4
      NRBT   = NRBT + 1
#endif
#ifdef W3_BT8
      NRBT   = NRBT + 1
#endif
#ifdef W3_BT9
      NRBT   = NRBT + 1
#endif
!
#ifdef W3_IC1
      NRIC   = NRIC + 1
      FLIC   = .TRUE.
#endif
#ifdef W3_IC2
      NRIC   = NRIC + 1
      FLIC   = .TRUE.
#endif
#ifdef W3_IC3
      NRIC   = NRIC + 1
      FLIC   = .TRUE.
#endif
#ifdef W3_IC4
      NRIC   = NRIC + 1
      FLIC   = .TRUE.
#endif
#ifdef W3_IC5
      NRIC   = NRIC + 1
      FLIC   = .TRUE.
#endif
!
#ifdef W3_IS1
      NRIS   = NRIS + 1
      FLIS   = .TRUE.
#endif
#ifdef W3_IS2
      NRIS   = NRIS + 1
      FLIS   = .TRUE.
#endif
!
#ifdef W3_DB0
      NRDB   = NRDB + 1
      FLDB   = .FALSE.
#endif
#ifdef W3_DB1
      NRDB   = NRDB + 1
#endif
!
#ifdef W3_TR0
      NRTR   = NRTR + 1
      FLTR   = .FALSE.
#endif
#ifdef W3_TR1
      NRTR   = NRTR + 1
#endif
!
#ifdef W3_BS0
      NRBS   = NRBS + 1
      FLBS   = .FALSE.
#endif
#ifdef W3_BS1
      NRBS   = NRBS + 1
#endif
!
#ifdef W3_REF1
      FLREF   = .TRUE.
#endif
!
      IF ( .NOT.FLLIN .AND.  .NOT.FLINDS .AND.  .NOT.FLNL .AND.        &
           .NOT.FLBT  .AND.  .NOT.FLIC   .AND.  .NOT.FLIS .AND.        &
           .NOT.FLDB  .AND.  .NOT.FLTR   .AND.  .NOT.FLBS .AND.        &
           .NOT.FLREF .AND.  FLSOU ) THEN
          WRITE (NDSE,1020)
          CALL EXTCDE ( 10 )
        END IF
!
      IF ( ( FLLIN .OR. FLINDS .OR. FLNL .OR. FLBT .OR. FLDB .OR.     &
             FLTR .OR. FLBS .OR. FLREF .OR. FLIC )          &
             .AND. .NOT.FLSOU ) THEN
          WRITE (NDSE,1021)
        END IF
!
      IF ( NRLIN .NE. 1 ) THEN
          WRITE (NDSE,1022) NRLIN
          CALL EXTCDE ( 11 )
        END IF
!
      IF ( NRSRCE .NE. 1 ) THEN
          WRITE (NDSE,1023) NRSRCE
          CALL EXTCDE ( 12 )
        END IF
!
      IF ( NRNL .NE. 1 ) THEN
          WRITE (NDSE,1024) NRNL
          CALL EXTCDE ( 13 )
        END IF
!
      IF ( NRBT .NE. 1 ) THEN
          WRITE (NDSE,1025) NRBT
          CALL EXTCDE ( 14 )
        END IF
!
      IF ( NRDB .NE. 1 ) THEN
          WRITE (NDSE,1026) NRDB
          CALL EXTCDE ( 15 )
        END IF
!
      IF ( NRTR .NE. 1 ) THEN
          WRITE (NDSE,1027) NRTR
          CALL EXTCDE ( 16 )
        END IF
!
      IF ( NRBS .NE. 1 ) THEN
          WRITE (NDSE,1028) NRBS
          CALL EXTCDE ( 17 )
        END IF
!
      IF ( NRIC .GT. 1 ) THEN
          WRITE (NDSE,1034) NRIC
          CALL EXTCDE ( 19 )
        END IF
!
      IF ( NRIS .GT. 1 ) THEN
          WRITE (NDSE,1036) NRIS
          CALL EXTCDE ( 26 )
        END IF


!
! 6.c Read namelist file or Pre-process namelists into scratch file
!
      WRITE (NDSO,915)
      IF (FLGNML) THEN
        OPEN (NDSS,FILE=TRIM(FNMPRE)//TRIM(NML_GRID%NML),STATUS='OLD',FORM='FORMATTED')
      ELSE
        OPEN (NDSS,FILE=TRIM(FNMPRE)//'ww3_grid.scratch',FORM='FORMATTED')
        DO
          CALL NEXTLN ( COMSTR , NDSI , NDSE )
          READ (NDSI,'(A)',END=2001,ERR=2002) LINE
          IF ( LINE(1:16) .EQ. 'END OF NAMELISTS' ) THEN
            EXIT
          ELSE
            WRITE (NDSS,'(A)') LINE
          ENDIF
        END DO
      END IF
      WRITE (NDSO,916)
!
! 6.d Define Sin.
! 6.d.1 Stresses
!
#ifdef W3_FLX1
      WRITE (NDSO,810)
#endif
#ifdef W3_FLX2
      WRITE (NDSO,810)
#endif
!
#ifdef W3_FLX2
      CINXSI =    0.20
      NITTIN =    3
#endif
#ifdef W3_FLX3
      CINXSI =    0.20
      NITTIN =    3
      CDMAX  =    2.5E-3
      CTYPE  =    0
#endif
!
#ifdef W3_FLX3
      CALL READNL ( NDSS, 'FLX3', STATUS )
      WRITE (NDSO,810) STATUS
      CDMAX  = MAX ( 0. , CDMAX )
      IF ( CTYPE .EQ. 1 ) THEN
          TYPEID = 'hyperbolic tangent'
        ELSE
          CTYPE = 0
          TYPEID = 'discontinuous     '
        END IF
      WRITE (NDSO,811) CDMAX*1.E3, TYPEID
      CD_MAX =    CDMAX
      CAP_ID =    CTYPE
#endif
!
#ifdef W3_FLX4
      CDFAC  = 1.0
      CALL READNL ( NDSS, 'FLX4', STATUS )
      WRITE (NDSO,810) STATUS
      WRITE (NDSO,811) CDFAC
      FLX4A0 = CDFAC
#endif
#ifdef W3_FLX5
      WRITE (NDSO,810)
#endif
!
! 6.d.2 Linear input
!
#ifdef W3_LN0
      WRITE (NDSO,820)
#endif
#ifdef W3_SEED
      WRITE (NDSO,820)
#endif
!
#ifdef W3_LN1
      CLIN   = 80.
      RFPM   =  1.
      RFHF   =  0.5
#endif
!
#ifdef W3_LN1
      CALL READNL ( NDSS, 'SLN1', STATUS )
      WRITE (NDSO,820) STATUS
      CLIN   = MAX (0.,CLIN)
      RFPM   = MAX (0.,RFPM)
      RFHF   = MAX(0.,MIN (1.,RFHF))
      WRITE (NDSO,821) CLIN, RFPM, RFHF
      SLNC1  = CLIN * (DAIR/DWAT)**2 / GRAV**2
      FSPM   = RFPM
      FSHF   = RFHF
#endif
!
! 6.d.3 Exponential input
!
#ifdef W3_ST0
      WRITE (NDSO,920)
#endif
!
#ifdef W3_ST1
      CINP   =    0.25
#endif
#ifdef W3_ST2
      ZWND   =   10.
      SWELLF =    0.100
      STABSH =    1.38
      STABOF =   -0.01
      CNEG   =   -0.1
      CPOS   =    0.1
      FNEG   =  150.
#endif
!
#ifdef W3_ST3
      ZWND   =   10.
      ALPHA0 = 0.0095
      Z0MAX = 0.0    
      BETAMAX  = 1.2       !  default WAM4 / WAM4 + is 1.2 with rhow=1000
      SINTHP   = 2.     
      SWELLF = 0.         
      ZALP   = 0.0110
#endif
!
#ifdef W3_ST4
      ZWND   =   10.
      ALPHA0 = 0.0095
      Z0MAX = 0.0    
      Z0RAT = 0.04 
      BETAMAX   = 1.43     
      SINTHP    = 2.     
      SWELLF    = 0.66     
      SWELLFPAR = 1
      SWELLF2 = -0.018
      SWELLF3 = 0.022
      SWELLF4 = 1.5E5
      SWELLF5 = 1.2
      SWELLF6 = 0.
      SWELLF7 = 360000. 
      TAUWSHELTER = 0.3
      ZALP   = 0.006
      SINBR   = 0.
#endif
!
#ifdef W3_ST6
      SINA0  = 0.09
      SINWS  = 32.0
      SINFC  = 6.0
#endif
!
#ifdef W3_ST1
      CALL READNL ( NDSS, 'SIN1', STATUS )
      WRITE (NDSO,920) STATUS
      WRITE (NDSO,921) CINP
      SINC1  = 28. * CINP * DAIR / DWAT
#endif
!
#ifdef W3_ST2
      CALL READNL ( NDSS, 'SIN2', STATUS )
      WRITE (NDSO,920) STATUS
      IF ( SWELLF.LT.0. .OR. SWELLF.GT.1. ) SWELLF = 1.
      WRITE (NDSO,921) ZWND, SWELLF
      IF ( STABSH .LT. 0.1 ) STABSH = 1.
      IF ( CNEG*CPOS .EQ. 0. ) THEN
          CNEG   = 0.
          CPOS   = 0.
          FNEG   = 0.
          FPOS   = 0.
        ELSE
          CPOS   = - ABS(CPOS) * ABS(CNEG)/CNEG
          FNEG   = - MAX(1.,ABS(FNEG))
          FPOS   = FNEG * CNEG/CPOS
        END IF
#endif
#ifdef W3_STAB2
      WRITE (NDSO,1921) STABSH, STABOF, CNEG, CPOS, FNEG, FPOS
#endif
#ifdef W3_ST2
      ZWIND  = ZWND
      FSWELL = SWELLF
      SHSTAB = STABSH
      OFSTAB = STABOF
      CCNG   = CNEG
      CCPS   = CPOS
      FFNG   = FNEG
      FFPS   = FPOS
#endif
!
#ifdef W3_ST3
      CALL READNL ( NDSS, 'SIN3', STATUS )
      WRITE (NDSO,920) STATUS
      WRITE (NDSO,921) ALPHA0, BETAMAX, SINTHP, Z0MAX, ZALP, ZWND, &
           SWELLF
      ZZWND  = ZWND
      AALPHA = ALPHA0
      BBETA  = BETAMAX
      SSINTHP  = SINTHP
      ZZ0MAX  = Z0MAX
      ZZALP  = ZALP
      SSWELLF(1) = SWELLF
#endif
!
#ifdef W3_ST4
      CALL READNL ( NDSS, 'SIN4', STATUS )
      WRITE (NDSO,920) STATUS
      WRITE (NDSO,921) ALPHA0, BETAMAX, SINTHP, Z0MAX, ZALP, ZWND, TAUWSHELTER, &
           SWELLFPAR, SWELLF, SWELLF2, SWELLF3, SWELLF4, SWELLF5, &
           SWELLF6, SWELLF7, Z0RAT
      ZZWND  = ZWND
      AALPHA = ALPHA0
      BBETA  = BETAMAX
      SSINBR  = SINBR
      SSINTHP  = SINTHP
      ZZ0MAX  = Z0MAX
      ZZ0RAT  = Z0RAT
      ZZALP  = ZALP
      TTAUWSHELTER = TAUWSHELTER
      SSWELLF(1) = SWELLF
      SSWELLF(2) = SWELLF2
      SSWELLF(3) = SWELLF3
      SSWELLF(4) = SWELLF4
      SSWELLF(5) = SWELLF5
      SSWELLF(6) = SWELLF6
      SSWELLF(7) = SWELLF7
      SSWELLFPAR = SWELLFPAR
#endif
!
#ifdef W3_ST6
      CALL READNL ( NDSS, 'SIN6', STATUS )
      WRITE (NDSO,920) STATUS
      SIN6A0 = SINA0
      SIN6WS = SINWS
      SIN6FC = SINFC
      J = 1
      IF ( SIN6A0.LE.0. ) J = 2
      WRITE (NDSO,921) YESXNO(J), SIN6A0, SIN6WS, SIN6FC
#endif
!
! 6.e Define Snl.
!
#ifdef W3_NL0
      WRITE (NDSO,922)
#endif
!
#ifdef W3_NL1
      LAMBDA =  0.25
      IF ( FLTC96 ) THEN
          NLPROP =  1.00E7
        ELSE IF ( FLST4 ) THEN
          NLPROP =  2.50E7
        ELSE IF ( FLST6 ) THEN
          NLPROP =  3.00E7
        ELSE
          NLPROP =  2.78E7
        END IF
#endif
!
#ifdef W3_NL1
      KDCONV =  0.75
      KDMIN  =  0.50
      SNLCS1 =  5.5
      SNLCS2 =  0.833
      SNLCS3 = -1.25
#endif
!
#ifdef W3_NL1
      CALL READNL ( NDSS, 'SNL1', STATUS )
      WRITE (NDSO,922) STATUS
      WRITE (NDSO,923) LAMBDA, NLPROP, KDCONV, KDMIN,            &
                       SNLCS1, SNLCS2, SNLCS3
      SNLC1  = NLPROP / GRAV**4
      LAM    = LAMBDA
      KDCON  = KDCONV
      KDMN   = KDMIN
      SNLS1  = SNLCS1
      SNLS2  = SNLCS2
      SNLS3  = SNLCS3
#endif
!
#ifdef W3_ST0
      FACHF  = 5.
#endif
#ifdef W3_ST1
      FACHF  = 4.5
#endif
#ifdef W3_ST2
      FACHF  = 5.
#endif
#ifdef W3_ST3
      FACHF  = 5.
#endif
#ifdef W3_ST4
      FACHF  = 5.
#endif
#ifdef W3_ST6
      FACHF  = 5.
#endif
#ifdef W3_NL2
      IQTYPE =  2
      TAILNL = -FACHF
      NDEPTH =  0
#endif
#ifdef W3_NL3
      NQDEF  =  0
      MSC    =  0.
      NSC    = -3.5
      KDFD   =  0.20
      KDFS   =  5.00
#endif
#ifdef W3_NL4
      INDTSA =  1
      ALTLP  =  2
#endif
#ifdef W3_NL5
      NL5DPT = 3000.
      NL5OML = 0.10
      NL5DIS = 0
      NL5KEV = 0
      NL5IPL = 1
      NL5PMX = 100
#endif
#ifdef W3_NLS
      A34    =  0.05
      FHFC   =  1.E10
      DNM    =  0.25
      FC1    =  1.25
      FC2    =  1.50
      FC3    =  6.00
#endif
!
#ifdef W3_NL2
      CALL READNL ( NDSS, 'SNL2', STATUS )
      WRITE (NDSO,922) STATUS
      TAILNL = MIN ( MAX ( TAILNL, -5. ) , -4. )
      IF ( IQTYPE .EQ. 3 ) THEN
          WRITE (NDSO,923) 'Shallow water', TAILNL
        ELSE IF ( IQTYPE .EQ. 2 ) THEN
          WRITE (NDSO,923) 'Deep water with scaling', TAILNL
        ELSE
          WRITE (NDSO,923) 'Deep water', TAILNL
          IQTYPE = 1
        END IF
#endif
!
#ifdef W3_NL2
      IF ( IQTYPE .NE. 3 ) THEN
          NDEPTH = 1
          ALLOCATE ( MPARS(1)%SNLPS%DPTHNL(NDEPTH) )
          DPTHNL => MPARS(1)%SNLPS%DPTHNL
          DPTHNL = 1000.
        ELSE
          IF ( NDEPTH .EQ. 0 ) NDEPTH = 7
          NDEPTH = MAX ( 1 , NDEPTH )
          ALLOCATE ( MPARS(1)%SNLPS%DPTHNL(NDEPTH) )
          DPTHNL => MPARS(1)%SNLPS%DPTHNL
          DPTHNL(1) = 640.
          DPTHNL(NDEPTH) = 10.
          IF ( NDEPTH .GT. 1 ) THEN
              DPTFAC = (DPTHNL(NDEPTH)/DPTHNL(1))**(1./(REAL(NDEPTH-1)))
              DO IDEPTH=2, NDEPTH-1
                DPTHNL(IDEPTH) = DPTFAC*DPTHNL(IDEPTH-1)
                END DO
            END IF
          CALL READNL ( NDSS, 'ANL2', STATUS )
          WRITE (NDSO,1923) NDEPTH, DPTHNL(1:MIN(5,NDEPTH))
          IF (NDEPTH .GT. 5 )WRITE (NDSO,2923) DPTHNL(6:NDEPTH)
        END IF
      WRITE (NDST,*)
      IQTPE  = IQTYPE
      NDPTHS = NDEPTH
      NLTAIL = TAILNL
#endif
!
#ifdef W3_NL3
      CALL READNL ( NDSS, 'SNL3', STATUS )
      WRITE (NDSO,922) STATUS
#endif
!!/NL3      MSC    = MAX ( 0. , MIN ( 8. , MSC ) )  ! Disabled HLT ca. 2009
#ifdef W3_NL3
      KDFD   = MAX ( 0.001 , MIN ( 10. , KDFD ) )
      KDFS   = MAX ( KDFD , MIN ( 10. , KDFS ) )
      WRITE (NDSO,923) MSC, NSC, KDFD, KDFS
#endif
!
#ifdef W3_NL3
      NQDEF  = MAX ( 0 , NQDEF )
      IF ( NQDEF .EQ. 0 ) THEN
          NQDEF  = 1
          QPARMS(1:5) = [ 0.25 , 0.00, -1., 1.E7, 0.00 ]
        ELSE
          DO J=1, NQDEF
            QPARMS((J-1)*5+1:J*5) = [ 0.25, 0.00, -1., 1.E7, 1.E6 ]
            END DO
          CALL READNL ( NDSS, 'ANL3', STATUS )
        END IF
      DO J=1, NQDEF
        QPARMS((J-1)*5+1) = MAX(0.,MIN (LAMMAX,QPARMS((J-1)*5+1)))
        QPARMS((J-1)*5+2) = MAX(0.,MIN (QPARMS((J-1)*5+1),        &
                                        QPARMS((J-1)*5+2)))
        QPARMS((J-1)*5+3) = MIN (DELTHM,QPARMS((J-1)*5+3))
        QPARMS((J-1)*5+4) = MAX (0.,QPARMS((J-1)*5+4))
        QPARMS((J-1)*5+5) = MAX (0.,QPARMS((J-1)*5+5))
        END DO
      WRITE (NDSO,1923) NQDEF
      WRITE (NDSO,2923) QPARMS(1:NQDEF*5)
      WRITE (NDSO,*)
      SNLNQ  = NQDEF
      SNLMSC = MSC
      SNLNSC = NSC
      SNLSFD = SQRT ( KDFD * TANH(KDFD) )
      SNLSFS = SQRT ( KDFS * TANH(KDFS) )
      ALLOCATE ( MPARS(1)%SNLPS%SNLL(NQDEF),                     &
                 MPARS(1)%SNLPS%SNLM(NQDEF),                     &
                 MPARS(1)%SNLPS%SNLT(NQDEF),                     &
                 MPARS(1)%SNLPS%SNLCD(NQDEF),                    &
                 MPARS(1)%SNLPS%SNLCS(NQDEF) )
      SNLL   => MPARS(1)%SNLPS%SNLL
      SNLL   = QPARMS(1:NQDEF*5:5)
      SNLM   => MPARS(1)%SNLPS%SNLM
      SNLM   = QPARMS(2:NQDEF*5:5)
      SNLT   => MPARS(1)%SNLPS%SNLT
      SNLT   = QPARMS(3:NQDEF*5:5)
      SNLCD  => MPARS(1)%SNLPS%SNLCD
      SNLCD  = QPARMS(4:NQDEF*5:5)
      SNLCS  => MPARS(1)%SNLPS%SNLCS
      SNLCS  = QPARMS(5:NQDEF*5:5)
#endif
!
#ifdef W3_NL4
      CALL READNL ( NDSS, 'SNL4', STATUS )
      WRITE (NDSO,922) STATUS
      WRITE (NDSO,923) INDTSA, ALTLP
      ITSA = INDTSA
      IALT = ALTLP
#endif
!
#ifdef W3_NL5
      CALL READNL ( NDSS, 'SNL5', STATUS )
      WRITE (NDSO,922) STATUS
      NL5DPT = MAX(0., MIN(NL5DPT, 3000.))
      NL5DIS = MAX(0 , MIN(NL5DIS, 1))
      NL5KEV = MAX(0 , MIN(NL5KEV, 1))
      NL5IPL = MAX(0 , MIN(NL5IPL, 1))
      IF (NL5DIS .EQ. 1) NL5IPL = 0
      IF (NL5PMX .GT. 0) NL5PMX = MAX(10, NL5PMX)
      WRITE (NDSO,923) NL5DPT, NL5OML, NL5DIS, NL5KEV, NL5IPL, NL5PMX
      QR5DPT = NL5DPT
      QR5OML = NL5OML
      QI5DIS = NL5DIS
      QI5KEV = NL5KEV
      QI5IPL = NL5IPL
      QI5PMX = NL5PMX
#endif
!
#ifdef W3_NLS
      CALL READNL ( NDSS, 'SNLS', STATUS )
      WRITE (NDSO,9922) STATUS
      A34    = MAX ( 0. , MIN ( A34 , ABMAX ) )
      FHFC   = MAX ( 0. , FHFC )
      DNM    = MAX ( 0., DNM )
      WRITE (NDSO,9923) A34, (XFR-1.)*A34, FHFC, DNM, FC1, FC2, FC3
      CNLSA  = A34
      CNLSC  = FHFC
      CNLSFM = DNM
      CNLSC1 = FC1
      CNLSC2 = FC2
      CNLSC3 = FC3
#endif
!
! 6.f Define Sds.
!
#ifdef W3_ST0
      WRITE (NDSO,924)
#endif
!
#ifdef W3_ST1
      CDIS   = -2.36E-5
      APM    =  3.02E-3
#endif
#ifdef W3_ST2
      SDSA0  =  4.8
      SDSA1  =  1.7e-4
      SDSA2  =  2.0
      SDSB0  =  0.3e-3
      SDSB1  =  0.47
      PHIMIN =  0.003
      SDSALN =  0.002
      FPIMIN =  0.009
#endif
#ifdef W3_ST3
      SDSC1  = -2.1 !! This is Bidlot et al. 2005,  Otherwise WAM4 uses -4.5
      WNMEANP = 0.5 !! This is Bidlot et al. 2005,  Otherwise WAM4 uses -0.5
      FXFM3 = 2.5 
      FXPM3 = 4.
      WNMEANPTAIL = 0.5 
      SDSDELTA1 = 0.4 !! This is Bidlot et al. 2005,  Otherwise WAM4 uses 0.5
      SDSDELTA2 = 0.6 !! This is Bidlot et al. 2005,  Otherwise WAM4 uses 0.5
#endif
!
#ifdef W3_ST4
      WNMEANP = 0.5    ! taken from Bidlot et al. 2005
      FXFM3 = 2.5 
      FXFMAGE = 0. 
      FXPM3 = 4.
      WNMEANPTAIL = -0.5
      SDSBCHOICE =1 ! 1: Ardhuin et al., 2: Filipot & Ardhuin, 3: Romero 
      SDSC2     = -2.2E-5     ! -3.8 for Romero
      SDSCUM    = -0.40344
      SDSC4     = 1.
      SDSC5     = 0. 
      SDSNUW    = 0.
      SDSC6     = 0.3
      SDSBR     = 0.90E-3     ! 0.005 for Romero                                
      SDSBRFDF  = 0
      SDSBRF1   = 0.5
      SDSP      = 2.   ! this is now fixed in w3sds4, should be cleaned up 
      SDSDTH    = 80.
      SDSCOS    = 2.
      SDSISO    = 2
      SDSBM0    = 1.
      SDSBM1    = 0.
      SDSBM2    = 0.
      SDSBM3    = 0.
      SDSBM4    = 0.
      SDSBCK    = 0.
      SDSABK    = 1.5
      SDSPBK    = 4.
      SDSBINT   = 0.3
      SDSHCK    = 1.5
      WHITECAPWIDTH = 0.3
      SDSSTRAIN = 0.       
      SDSFACMTF =  400    ! MTF factor for Lambda , Romero (2019)
      SDSSTRAINA = 15.
      SDSSTRAIN2 = 0.
      WHITECAPDUR   = 0.56 ! breaking duration factor 
! b (strength of breaking)
      SDSBT     = 1.100E-3 ! B_T (sturation threshold for dissipation rate b) 
! Lambda parameters
      SDSL     = 3.5000e-05  ! L scaling 
! MTF
      SPMSS     = 0.5    ! cmss^SPMSS 
      SDSNMTF   = 1.5    ! MTF power 
      SDSCUMP   = 2.
! MW
      SDSMWD    = .9  ! new AFo
      SDSMWPOW  = 1.  ! (k )^pow
      SDKOF     = 3.  !  ko factor such that ko= g (SDKOF/(28 us))^2 
#endif
!
#ifdef W3_ST6
      SDSET  = .TRUE.
      SDSA1  = 4.75E-06
      SDSP1  = 4
      SDSA2  = 7.00E-05
      SDSP2  = 4
      CSTB1  = .FALSE.
      SWLB1  = 0.41E-02
#endif
!
#ifdef W3_ST1
      CALL READNL ( NDSS, 'SDS1', STATUS )
      WRITE (NDSO,924) STATUS
      WRITE (NDSO,925) CDIS, APM
      SDSC1  = TPI * CDIS / APM**2
#endif
!
#ifdef W3_ST2
      CALL READNL ( NDSS, 'SDS2', STATUS )
      WRITE (NDSO,924) STATUS
      IF ( PHIMIN .LE. 0. ) THEN
          SDSB2  = 0.
          SDSB3  = 0.
          PHIMIN = SDSB0 + SDSB1*FPIMIN
        ELSE
          FPIA   = ( PHIMIN - SDSB0 ) / SDSB1
          IF ( FPIA .LT. FPIMIN ) THEN
              SDSB3  = 4.
              SDSB2  = FPIMIN**SDSB3 * (PHIMIN-SDSB0-SDSB1*FPIMIN)
            ELSE
              FPIB   = MAX ( FPIA-0.0025 , FPIMIN )
              DPHID  = MAX ( PHIMIN - SDSB0 - SDSB1*FPIB , 1.E-15 )
              SDSB3  = MIN ( 10. , SDSB1*FPIB / DPHID )
              SDSB2  = FPIB**SDSB3 * DPHID
              FPIMIN = FPIB
            END IF
        END IF
      WRITE (NDSO,925) SDSA0, SDSA1, SDSA2,                      &
                       SDSB0, SDSB1, SDSB2, SDSB3, FPIMIN, PHIMIN
      CDSA0  = SDSA0
      CDSA1  = SDSA1
      CDSA2  = SDSA2
      CDSB0  = SDSB0
      CDSB1  = SDSB1
      CDSB2  = SDSB2
      CDSB3  = SDSB3
#endif
!
#ifdef W3_ST3
      CALL READNL ( NDSS, 'SDS3', STATUS )
      WRITE (NDSO,924) STATUS
      WRITE (NDSO,925) SDSC1, WNMEANP, SDSDELTA1,  &
                       SDSDELTA2
      SSDSC1   = SDSC1
      WWNMEANP   = WNMEANP
      FFXFM = FXFM3 * TPI
      FFXPM = FXPM3 * GRAV / 28.
      WWNMEANPTAIL   = WNMEANPTAIL
      DDELTA1   = SDSDELTA1
      DDELTA2   = SDSDELTA2
#endif
!
#ifdef W3_ST4
      CALL READNL ( NDSS, 'SDS4', STATUS )
      WRITE (NDSO,924) STATUS
      WRITE (NDSO,925) SDSC2, SDSBCK, SDSCUM, WNMEANP 
      SSDSC(1)   = REAL(SDSBCHOICE)
      SSDSC(2)   = SDSC2
      SSDSC(3)   = SDSCUM
      SSDSC(4)   = SDSC4
      SSDSC(5)   = SDSC5
      SSDSC(6)   = SDSC6
      SSDSC(7)   = WHITECAPWIDTH
      SSDSC(8)   = SDSSTRAIN   ! Straining constant ... 
      SSDSC(9)   = SDSL
      SSDSC(10)  = SDSSTRAINA*NTH/360. ! angle Aor enhanced straining
      SSDSC(11)  = SDSSTRAIN2  ! straining constant for directional part 
      SSDSC(12)  = SDSBT
      SSDSC(13)  = SDSMWD
      SSDSC(14)  = SPMSS
      SSDSC(15)  = SDSMWPOW 
      SSDSC(16)  = SDKOF
      SSDSC(17)  = WHITECAPDUR  
      SSDSC(18)  = SDSFACMTF
      SSDSC(19)  = SDSNMTF 
      SSDSC(20)  = SDSCUMP 
      SSDSC(21)  = SDSNUW 
#endif
!
#ifdef W3_ST4
      SSDSBR   = SDSBR
      SSDSBRF1 = SDSBRF1
      SSDSBRFDF= SDSBRFDF
      SSDSBM(0)   = SDSBM0
      SSDSBM(1)   = SDSBM1
      SSDSBM(2)   = SDSBM2
      SSDSBM(3)   = SDSBM3
      SSDSBM(4)   = SDSBM4
      SSDSBT   = SDSBT
      SSDSISO  = SDSISO
      SSDSCOS  = SDSCOS
      SSDSP    = SDSP
      SSDSDTH  = SDSDTH
      WWNMEANP   = WNMEANP
      FFXFM = FXFM3 * TPI
      FFXFA = FXFMAGE * TPI
      FFXPM = FXPM3 * GRAV / 28.
      WWNMEANPTAIL   = WNMEANPTAIL
      SSDSBCK   = SDSBCK
      SSDSABK   = SDSABK
      SSDSPBK   = SDSPBK
      SSDSBINT  = SDSBINT
      SSDSHCK   = SDSHCK
#endif
!
#ifdef W3_ST6
      CALL READNL ( NDSS, 'SDS6', STATUS )
      WRITE (NDSO,924) STATUS
      SDS6ET = SDSET
      SDS6A1 = SDSA1
      SDS6P1 = SDSP1
      SDS6A2 = SDSA2
      SDS6P2 = SDSP2
      J = 2
      IF (SDSET) J = 1
      WRITE (NDSO,925) YESXNO(J), YESXNO(3-J), SDS6A1, SDS6P1, SDS6A2, SDS6P2

      CALL READNL ( NDSS, 'SWL6', STATUS )
      WRITE (NDSO,937) STATUS
      J = 1
      SWL6S6 = SWLB1.GT.0.0
      IF (.NOT.SWL6S6) J  = 2
      SWL6B1 = SWLB1
      SWL6CSTB1 = CSTB1
      IF (CSTB1) THEN
         WRITE (NDSO,940) YESXNO(J), '(constant)           ' ,SWL6B1
      ELSE
         WRITE (NDSO,940) YESXNO(J), '(steepness dependent)' ,SWL6B1
      END IF
#endif
!
! 6.g Define Sbt.
!
#ifdef W3_BT0
      WRITE (NDSO,926)
#endif
#ifdef W3_BT4
      WRITE (NDSO,926)
#endif
!
#ifdef W3_BT1
      GAMMA  = -0.067
#endif
!
#ifdef W3_BT1
      CALL READNL ( NDSS, 'SBT1', STATUS )
      WRITE (NDSO,926) STATUS
      WRITE (NDSO,927) GAMMA
      SBTC1  = 2. * GAMMA / GRAV
#endif
!
#ifdef W3_BT4
      SEDMAPD50=.FALSE.
      SED_D50_UNIFORM=2.E-4  ! default grain size: medium sand 200 microns
      RIPFAC1=0.4    ! A1 in Ardhuin et al. 2003
      RIPFAC2=-2.5   ! A2 in Ardhuin et al. 2003
      RIPFAC3=1.2    ! A3 in Ardhuin et al. 2003
      RIPFAC4=0.05   ! A4 in Ardhuin et al. 2003
      SIGDEPTH=0.05
      BOTROUGHMIN=0.01
      BOTROUGHFAC=1.00
      CALL READNL ( NDSS, 'SBT4', STATUS )
      WRITE (NDSO,926) STATUS
      WRITE (NDSO,927) SEDMAPD50, SED_D50_UNIFORM, &
                       RIPFAC1,RIPFAC2,RIPFAC3,RIPFAC4,SIGDEPTH,  &
                       BOTROUGHMIN, BOTROUGHFAC
      SBTCX(1)=RIPFAC1
      SBTCX(2)=RIPFAC2
      SBTCX(3)=RIPFAC3
      SBTCX(4)=RIPFAC4
      SBTCX(5)=SIGDEPTH
      SBTCX(6)=BOTROUGHMIN
      SBTCX(7)=BOTROUGHFAC
#endif
!        
!
! 6.h Define Sdb.
!
#ifdef W3_DB0
      WRITE (NDSO,928)
#endif
!
#ifdef W3_DB1
      BJALFA = 1.
      BJGAM  = 0.73
      BJFLAG = .TRUE.
#endif
!
#ifdef W3_DB1
      CALL READNL ( NDSS, 'SDB1', STATUS )
      WRITE (NDSO,928) STATUS
      BJALFA = MAX ( 0. , BJALFA )
      BJGAM  = MAX ( 0. , BJGAM )
      WRITE (NDSO,929) BJALFA, BJGAM
      IF ( BJFLAG ) THEN
          WRITE (NDSO,*) '      Using Hmax/d ratio only.'
        ELSE
          WRITE (NDSO,*)                                       &
             '      Using Hmax/d in Miche style formulation.'
        END IF
      WRITE (NDSO,*)
      SDBC1  = BJALFA
      SDBC2  = BJGAM
      FDONLY = BJFLAG
#endif
!
!
#ifdef W3_UOST
      UOSTFILELOCAL = 'obstructions_local.'//ADJUSTL(TRIM(GNAME))//'.in'
      UOSTFILESHADOW = 'obstructions_shadow.'//ADJUSTL(TRIM(GNAME))//'.in'
      UOSTFACTORLOCAL = 1
      UOSTFACTORSHADOW = 1
      CALL READNL ( NDSS, 'UOST', STATUS )
      WRITE (NDSO,4500) STATUS
      WRITE (NDSO,4501) ADJUSTL(TRIM(UOSTFILELOCAL)), ADJUSTL(TRIM(UOSTFILESHADOW)), &
                UOSTFACTORLOCAL, UOSTFACTORSHADOW
#endif
!
! 6.i Define Str.
!
#ifdef W3_TR0
      WRITE (NDSO,930)
#endif
!
! 6.j Define Sbs.
!
#ifdef W3_BS0
      WRITE (NDSO,932)
#endif
#ifdef W3_BS1
      WRITE (NDSO,932)
#endif
!
! 6.k Define Sxx and Sic.
!
#ifdef W3_IC1
      WRITE (NDSO,935)
         WRITE(NDSO,'(A/A)')'    Sice will be calculated using ' &
            //'user-specified ki values.','        Required ' &
            //'field input: ice parameter 1.'
#endif
!
#ifdef W3_IC2
      WRITE (NDSO,935)
         WRITE(NDSO,'(A/A)')'    Sice will be calculated using ' &
            //'under-ice boundary layer method.','        Required '  &
            //'field input: ice parameters 1 and 2.'
#endif
!
#ifdef W3_IC3
      WRITE (NDSO,935)
      WRITE(NDSO,'(A/A)')'    Sice will be calculated using '&
            //'Wang and Shen method.','        '&
            //'Required field input: ice parameters 1, 2, 3 and 4.'
#endif
!
#ifdef W3_IC4
      WRITE (NDSO,935)
      WRITE(NDSO,'(A/A)')'    Sice will be calculated using '&
            //'Empirical method.','        '&
            //'Required field input: ice parameters (varies).'
#endif
!
#ifdef W3_IC5
      WRITE (NDSO,935)
      WRITE(NDSO,'(A/A/)')'    Sice will be calculated using '&
            //'effective medium models.','        '&
            //'Required field input: ice parameters 1, 2, 3 and 4.'
#endif
!
! 6.l Read unstructured data 
! initialisation of logical related to unstructured grid
       UGOBCAUTO = .TRUE.
       UGBCCFL = .TRUE.
       UGOBCDEPTH= -10.
       UGOBCOK = .FALSE.
       UGOBCFILE = 'unset'
       EXPFSN    = .TRUE.
       EXPFSPSI  = .FALSE.
       EXPFSFCT  = .FALSE.
       IMPFSN    = .FALSE.
       IMPTOTAL  = .FALSE.
       EXPTOTAL  = .FALSE. 
       IMPREFRACTION = .FALSE.
       IMPFREQSHIFT = .FALSE.
       IMPSOURCE = .FALSE.
       SETUP_APPLY_WLV = .FALSE.
       SOLVERTHR_SETUP=1E-14
       CRIT_DEP_SETUP=0.1
       JGS_TERMINATE_MAXITER = .TRUE.
       JGS_TERMINATE_DIFFERENCE = .TRUE.
       JGS_TERMINATE_NORM = .FALSE.
       JGS_LIMITER = .FALSE.
       JGS_BLOCK_GAUSS_SEIDEL = .TRUE.
       JGS_USE_JACOBI = .TRUE.
       JGS_MAXITER=100
       JGS_PMIN = 1
       JGS_DIFF_THR = 1.E-10
       JGS_NORM_THR = 1.E-20
       JGS_NLEVEL = 0
       JGS_SOURCE_NONLINEAR = .FALSE.
! read data from the unstructured devoted namelist
       CALL READNL ( NDSS, 'UNST', STATUS )

       B_JGS_USE_JACOBI = JGS_USE_JACOBI
       B_JGS_TERMINATE_MAXITER = JGS_TERMINATE_MAXITER
       B_JGS_TERMINATE_DIFFERENCE = JGS_TERMINATE_DIFFERENCE
       B_JGS_TERMINATE_NORM = JGS_TERMINATE_NORM
       B_JGS_LIMITER = JGS_LIMITER
       B_JGS_BLOCK_GAUSS_SEIDEL = JGS_BLOCK_GAUSS_SEIDEL
       B_JGS_MAXITER = JGS_MAXITER
       B_JGS_PMIN = JGS_PMIN
       B_JGS_DIFF_THR = JGS_DIFF_THR
       B_JGS_NORM_THR = JGS_NORM_THR
       B_JGS_NLEVEL = JGS_NLEVEL
       B_JGS_SOURCE_NONLINEAR = JGS_SOURCE_NONLINEAR

       IF ((EXPFSN .eqv. .FALSE.).and.(EXPFSPSI .eqv. .FALSE.)     &
           .and.(EXPFSFCT .eqv. .FALSE.)                           &
           .and.(IMPFSN .eqv. .FALSE.)                             &
           .and.(EXPTOTAL .eqv. .FALSE.)                           &
           .and.(IMPTOTAL .eqv. .FALSE.)) THEN
         EXPFSN=.TRUE. ! This is the default scheme ...
       END IF
       nbSel=0

       IF (EXPFSN) nbSel=nbSel+1
       IF (EXPFSPSI) nbSel=nbSel+1
       IF (EXPFSFCT) nbSel=nbSel+1
       IF (IMPFSN) nbSel=nbSel+1
       IF (IMPTOTAL) nbSel=nbSel+1
       IF (EXPTOTAL) nbSel=nbSel+1

       IF (GTYPE .EQ. UNGTYPE) THEN
         IF (nbSel .ne. 1) THEN
           WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN WW3_GRID:'
           IF (nbSel .gt. 1) THEN
             WRITE (NDSE,*) 'More than one scheme selected'
           ELSE IF (nbSel .eq. 0) THEN
             WRITE (NDSE,*) 'no scheme selected'
           END IF
           WRITE (NDSE,*)'Select only one of EXPFSN, EXPFSFCT, EXPFSPSI'
           WRITE (NDSE,*)'IMPFSN, IMPTOTAL'
           CALL EXTCDE ( 30 )
         END IF
       END IF
!
! 6.m Select propagation scheme
!
      WRITE (NDSO,950)
!
      NRPROP = 0
      FLPROP = .TRUE.
      PNAME  = '                              '
#ifdef W3_PR0
      PNAME  = 'Not defined                   '
      NRPROP = NRPROP + 1
      FLPROP = .FALSE.
#endif
#ifdef W3_PR1
      PNAME  = 'First order upstream          '
      NRPROP = NRPROP + 1
#endif
#ifdef W3_UQ
      PNAME  = '3rd order UQ'
#endif
#ifdef W3_UNO
      PNAME  = '2nd order UNO'
#endif
       J = LEN_TRIM(PNAME)
#ifdef W3_PR2
      PNAME  = PNAME(1:J)//' + GSE diffusion '
      NRPROP = NRPROP + 1
#endif
#ifdef W3_PR3
      PNAME  = PNAME(1:J)//' + GSE averaging '
      NRPROP = NRPROP + 1
#endif
!
#ifdef W3_SMC
      PNAME  = 'UNO2 on SMC grid + diffusion  '
#endif
!
      IF ( (FLCX.OR.FLCY.OR.FLCTH.OR.FLCK) .AND. .NOT. FLPROP ) THEN
          WRITE (NDSE,1030)
          CALL EXTCDE ( 20 )
        END IF
!
      IF ( .NOT.(FLCX.OR.FLCY.OR.FLCTH.OR.FLCK) .AND. FLPROP ) THEN
          WRITE (NDSE,1031)
        END IF
!
      IF ( NRPROP.EQ.0 ) THEN
          WRITE (NDSE,1032)
          CALL EXTCDE ( 21 )
        END IF
!
      IF ( NRPROP .GT. 1 ) THEN
          WRITE (NDSE,1033) NRPROP
          CALL EXTCDE ( 22 )
        END IF
!
! 6.m Parameters for propagation scheme
!
      WRITE (NDSO,951) PNAME
!
      CFLTM  =  0.7
!
#ifdef W3_PR2
      DTIME  =  0.
      LATMIN = 70.
#endif
!
#ifdef W3_SMC
 !!   Default values of SMC grid parameters.  JGLi06Apr2021
      NCel   =  1
      NUFc   =  1
      NVFc   =  1
      NGLO   =  1
      NARC   =  1
      NBGL   =  1
      NBAC   =  1
      LvSMC  =  1
      MRFct  =  1
      ISHFT  =  0
      JEQT   =  0
      NBISMC =  0
      CFLSM  =  0.7
      DTIMS  =  360.0
      RFMAXD =  36.0
      UNO3   = .FALSE.
      AVERG  = .TRUE.
      SEAWND = .FALSE.
      Arctic = .FALSE.
#endif
!
#ifdef W3_PR3
      WDTHCG = 1.5
      WDTHTH = WDTHCG
#endif
!
#ifdef W3_PR1
      CALL READNL ( NDSS, 'PRO1', STATUS )
      IF ( STATUS(18:18) .EQ. ':' ) STATUS(18:18) = ' '
      WRITE (NDSO,952) STATUS(1:18)
      CFLTM  = MAX ( 0. , CFLTM )
      WRITE (NDSO,953) CFLTM
#endif
!
#ifdef W3_PR2
      CALL READNL ( NDSS, 'PRO2', STATUS )
      IF ( STATUS(18:18) .EQ. ':' ) STATUS(18:18) = ' '
      WRITE (NDSO,952) STATUS(1:18)
      CFLTM  = MAX ( 0. , CFLTM )
      DTIME  = MAX ( 0. , DTIME )
      LATMIN = MIN ( 89. , ABS(LATMIN) )
      CLATMN = COS ( LATMIN * DERA )
      IF ( DTIME .EQ. 0. ) THEN
          WRITE (NDSO,953) CFLTM, LATMIN
        ELSE
          WRITE (NDSO,954) CFLTM, DTIME/3600., LATMIN
        END IF
      DTME   = DTIME
#endif
!
#ifdef W3_SMC
      CALL READNL ( NDSS, 'PSMC', STATUS )
      IF ( STATUS(18:18) .EQ. ':' ) STATUS(18:18) = ' '
      WRITE (NDSO,952) STATUS(1:18)
      CFLSM  = MAX ( 0. , CFLSM )
      DTIMS  = MAX ( 0. , DTIMS )
      RFMAXD = MIN ( 80.0, ABS(RFMAXD) )
      Refran = RFMAXD * DERA 
  !! Printing out SMC grid parameters. 
      WRITE (NDSO,1950)
      WRITE (NDSO,1951) PNSMC
      WRITE (NDSO,1953) CFLSM, DTIMS/3600., RFMAXD
#endif
!
#ifdef W3_SMC
      FUNO3  = UNO3
      FVERG  = AVERG
      FSWND  = SEAWND
      ARCTC  = Arctic
      NBSMC  = NBISMC
      IF( FUNO3 ) WRITE (NDSO,*)                &
        " Advection use 3rd order UNO3 instead of UNO2 scheme."
      IF( FVERG ) WRITE (NDSO,*)                &
        " Extra 1-2-1 average smoothing activated on SMC grid."
      IF( FSWND ) WRITE (NDSO,*)                & 
        " Sea-point only wind input is required for SMC grid. "
      IF( ARCTC ) WRITE (NDSO,*)                & 
        " Arctic polar part will be appended to this SMC grid."
      NRLv = LvSMC 
      WRITE (NDSO,4001) NRLv
      WRITE (NDSO,4002) JEQT
      WRITE (NDSO,4302) ISHFT
      WRITE (NDSO,4003) NBSMC
#endif
!
#ifdef W3_PR3
      CALL READNL ( NDSS, 'PRO3', STATUS )
      IF ( STATUS(18:18) .EQ. ':' ) STATUS(18:18) = ' '
#endif
       IF (GTYPE.NE.UNGTYPE) THEN
#ifdef W3_PR3
          WRITE (NDSO,952) STATUS(1:18)
      CFLTM  = MAX ( 0. , CFLTM )
          WRITE (NDSO,953) CFLTM, WDTHCG
      IF ( WDTHCG*(XFR-1.) .GT. 1. ) WRITE (NDSO,955) 1./(XFR-1.)
          WRITE (NDSO,954) WDTHTH
      IF ( WDTHTH*DTH .GT. 1. ) WRITE (NDSO,955) 1./DTH
          WRITE (NDSO,*)
#endif
       ENDIF
#ifdef W3_PR3
      WDCG   = WDTHCG
      WDTH   = WDTHTH
#endif
!
      CTMAX  = CFLTM
!
#ifdef W3_RTD
 ! Set/ read in rotation values - these will be written out
 ! later with the rest of the grid info
 ! Default is a non-rotated lat-lon grid
      PLAT = 90.
      PLON = -180.
      UNROT = .FALSE.
      CALL READNL ( NDSS, 'ROTD', STATUS )
      PLON = MOD( PLON + 180., 360. ) - 180.
 ! Ensure that a grid with pole at the geographic North is standard lat-lon
      IF ( PLAT == 90. .AND. ( PLON /= -180. .OR. UNROT ) ) THEN
         WRITE( NDSE, 1052 )
         CALL EXTCDE ( 33 )
       ENDIF
 ! Default poles of output b. c. are non-rotated: 
      BPLAT = 90.
      BPLON = -180.
      CALL READNL ( NDSS, 'ROTB', STATUS )
 ! A b. c. dest. grid with pole at the geographic North must be non-rotated
      DO I=1,9
        IF ( BPLAT(I) == 90. ) THEN
 ! Require BPLON(I) == -180., but don't blaim the user if BPLON(I) == 180.
          IF ( BPLON(I) == 180. ) BPLON(I) = -180.
          IF ( BPLON(I) == -180. ) CYCLE
          END IF
        IF ( BPLAT(I) < 90. ) CYCLE
        WRITE( NDSE, 1053 )
        CALL EXTCDE ( 34 )
       END DO
#endif
!
! 6.n Set miscellaneous parameters (ice, seeding, numerics ... )
!
      CICE0  = 0.5
      CICEN  = 0.5
      LICE   = 0.
      ICEHFAC= 1.0  
      ICEHMIN= 0.2  ! the 0.2 value is arbitrary and needs to be tuned.
      ICEHINIT= 0.5  
      ICESLN = 1.0
      ICEWIND= 1.0
      ICESNL = 1.0
      ICESDS = 1.0
      ICEHDISP= 0.6 ! Prevent from convergence crash in w3dispmd in the presence of ice, should be tuned
      ICEDDISP= 80
      ICEFDISP= 2
      GSHIFT = 0.0D0
      PMOVE  = 0.5
      XSEED  = 1.
      FLAGTR = 0
      XP     = 0.15
      XR     = 0.10
      XFILT  = 0.05
      IHM    = 100
      HSPM   = 0.05
      WSM    = 1.7
      WSC    = 0.333
      FLC    = .TRUE.
      TRCKCMPR = .TRUE.
      NOSW   = 5
!
! Gas fluxes 
!
      AIRCMIN   = 2.0  ! cmin for whitecap coverage and entrained air
      AIRGB     = 0.2  ! volume of entrained air constant (Deike et al. 2017)
!
#ifdef W3_NCO
! NCEP operations retains first three swell systems.
      NOSW=3
#endif
      PTM    = 1    ! Default to standard WW3 partitioning. C. Bunney
      PTFC   = 0.1  ! Part. method 5 cutoff freq default. C. Bunney
      FMICHE = 1.6
      RWNDC  = 1.
      WCOR1  = 99. 
      WCOR2  = 0.
      BTBET  = 1.2 ! β for c / [U cos(θ - φ)] < β
! Variables for Space-Time Extremes
!  Default negative values make w3iogomd switch off space-time extremes
!  forces user to provide NAMELIST if wanting to compute STE parameters
      STDX = -1. 
      STDY = -1. 
      STDT = -1. 
      ICEDISP = .FALSE.
      CALTYPE = 'standard'
! Variables for 3D array output
      E3D=0 
      I1E3D=1
      I2E3D=NK
      P2SF   = 0
      I1P2SF = 1	
      I2P2SF = 15
      US3D   = 0 
      I1US3D = 1
      I2US3D = NK
      USSP=0
      IUSSP=1
      STK_WN(:)=0.0
      STK_WN(1)=TPI/100. !Set default decay of 100 m for Stokes drift
      TH1MF=0 
      I1TH1M=1
      I2TH1M=NK
      STH1MF=0 
      I1STH1M=1
      I2STH1M=NK
      TH2MF=0
      I1TH2M=1
      I2TH2M=NK
      STH2MF=0 
      I1STH2M=1
      I2STH2M=NK
!
      FACBERG=1.
#ifdef W3_IS0
      WRITE (NDSO,944)
#endif
#ifdef W3_IS1
      ISC1 = 1.
      ISC2 = 0.
      CALL READNL ( NDSS, 'SIS1', STATUS )
      WRITE (NDSO,945) STATUS
      WRITE (NDSO,946) ISC1, ISC2
      IS1C1 = ISC1
      IS1C2 = ISC2
#endif
#ifdef W3_IS2
      ISC1 = 1.
      IS2C2 = 0.   ! 0.025
      IS2C3 = 0.   ! 2.4253
      IS2CONC = 0.   
      IS2BACKSCAT = 1.
      IS2BREAK = .FALSE.
      IS2BREAKF = 3.6
      IS2FLEXSTR=6.00E+05   ! value used in Ardhuin et al. 2020
      IS2ISOSCAT=.TRUE.     ! uses isotropic back-scatter
      IS2DISP=.FALSE. !not dispersion only attenuation following Liu disp. eq.
      IS2DUPDATE=.TRUE.
      IS2FRAGILITY=0.9
      IS2DMIN=20
      IS2DAMP=0.
      IS2CREEPB=0.
      IS2CREEPC=0.4     ! This gives an impact of break-up over a wider freq. range
#endif
!                            ! compared to the 0.2 value in Boutin et al. 2018
#ifdef W3_IS2
      IS2CREEPD=0.5
      IS2CREEPN=3.0
      IS2BREAKE=1.
      IS2WIM1=1.
      IS2ANDISB=.TRUE.  !anelastic instead of inelastic dissipation if IS2CREEPB>0
      IS2ANDISE=0.55    !energy of activation
      IS2ANDISD=2.0E-9  !see Ardhuin et al. 2020
      IS2ANDISN=1.      !dependency on stress. Equal to 1 normally?
      CALL READNL ( NDSS, 'SIS2', STATUS )
      WRITE (NDSO,947) STATUS
      WRITE (NDSO,2948) ISC1, IS2BACKSCAT, IS2ISOSCAT, IS2BREAK, IS2DUPDATE, IS2FLEXSTR, IS2DISP, &
        IS2DAMP, IS2FRAGILITY, IS2DMIN, IS2C2, IS2C3, IS2CONC, IS2CREEPB,&
        IS2CREEPC, IS2CREEPD, IS2CREEPN, IS2BREAKE, IS2BREAKF, IS2WIM1,  &
        IS2ANDISB, IS2ANDISE, IS2ANDISD, IS2ANDISN
#endif
!
#ifdef W3_REF1
      REFCOAST=0.
      REFMAP=0.
      REFMAPD=0.
      REFRMAX=1.
      REFFREQPOW=2.
      REFFREQ=0.
      REFCOSP_STRAIGHT=4.
      REFSLOPE=0.22
      REFSUBGRID=0.
      REFICEBERG=0.
      REFUNSTSOURCE=0.
#endif
!
#ifdef W3_REF1
      CALL READNL ( NDSS, 'REF1', STATUS )
      WRITE (NDSO,969) STATUS
#endif
!
#ifdef W3_IG1
       IGMETHOD = 2
       IGADDOUTP= 0 
       IGSOURCE = 2
       IGSTERMS = 0
       IGMAXFREQ=0.03
       IGSOURCEATBP = 0
       IGBCOVERWRITE = .TRUE.
       IGSWELLMAX = .TRUE.
       IGKDMIN = 1.1
       IGFIXEDDEPTH = 0.
       IGEMPIRICAL = 0.00125
#endif
!
#ifdef W3_IG1
      CALL READNL ( NDSS, 'SIG1 ', STATUS )
      WRITE (NDSO,970) STATUS
#endif
!
#ifdef W3_IC2
       IC2DISPER = .FALSE. 
       IC2TURB = 1.
       IC2TURBS = 0.
       IC2ROUGH = 0.01 
       IC2REYNOLDS = 1.5E5
       IC2SMOOTH = 2E5
       IC2VISC = 1.
       IC2DMAX = 0.
#endif
!
#ifdef W3_IC3
       IC3MAXTHK = 100.0
       IC3MAXCNC = 100.0
       IC2TURB = 2.0 ! from run_test example by F.A.
       IC2TURBS = 0.
       IC2ROUGH = 0.02  !  from run_test example by F.A. (alt:0.1)
       IC2REYNOLDS = 1.5E5
       IC2SMOOTH = 7.0E4
       IC2VISC = 2.0
       IC3CHENG = .TRUE.
       USECGICE = .FALSE.
       IC3HILIM = 100.0
       IC3KILIM = 100.0
       IC3HICE = -1.0
       IC3VISC = -2.0
       IC3DENS = -3.0
       IC3ELAS = -4.0
#endif
!fixme: if USECGICE = .TRUE., don't allow use of IC3MAXTHK<100.0

#ifdef W3_IC4
       IC4METHOD = 1 !switch for methods within IC4
       IC4KI=0.0
       IC4FC=0.0
#endif
!
#ifdef W3_IC5
       IC5MINIG      = 1.
       IC5MINWT      = 0.
       IC5MAXKRATIO  = 1E9
       IC5MAXKI      = 100.
       IC5MINHW      = 0.
       IC5MAXITER    = 100.
       IC5RKICK      = 0.
       IC5KFILTER    = 0.0025
       IC5VEMOD      = 3. ! 1: EFS, 2: RP, 3: M2 (default)
#endif
!
#ifdef W3_IC2
      CALL READNL ( NDSS, 'SIC2 ', STATUS )
      WRITE (NDSO,971) STATUS
#endif
!
#ifdef W3_IC3
      CALL READNL ( NDSS, 'SIC3 ', STATUS )
      WRITE (NDSO,971) STATUS
#endif
!
#ifdef W3_IC4
      CALL READNL ( NDSS, 'SIC4 ', STATUS )
      WRITE (NDSO,971) STATUS
#endif
!
#ifdef W3_IC5
      CALL READNL ( NDSS, 'SIC5 ', STATUS )
      IC5VEMOD = MIN(MAX(1., IC5VEMOD), 3.)
      WRITE (NDSO,971) STATUS
      WRITE (NDSO,2971) IC5MINIG, IC5MINWT, IC5MAXKRATIO,         &
                        IC5MAXKI, IC5MINHW, IC5MAXITER, IC5RKICK, &
                        IC5KFILTER, IC5MSTR(NINT(IC5VEMOD))
#endif
!
      CALL READNL ( NDSS, 'OUTS', STATUS )
      WRITE (NDSO,4970) STATUS
!
!
! output of frequency spectra, th1m ... 
!
      E3DF(1,1) = E3D
      E3DF(2,1) = MIN(MAX(1,I1E3D),NK)
      E3DF(3,1) = MIN(MAX(1,I2E3D),NK)
      E3DF(1,2) = TH1MF
      E3DF(2,2) = MIN(MAX(1,I1TH1M),NK)
      E3DF(3,2) = MIN(MAX(1,I2TH1M),NK)
      E3DF(1,3) = STH1MF
      E3DF(2,3) = MIN(MAX(1,I1STH1M),NK)
      E3DF(3,3) = MIN(MAX(1,I2STH1M),NK)
      E3DF(1,4) = TH2MF
      E3DF(2,4) = MIN(MAX(1,I1TH2M),NK)
      E3DF(3,4) = MIN(MAX(1,I2TH2M),NK)
      E3DF(1,5) = STH2MF
      E3DF(2,5) = MIN(MAX(1,I1STH2M),NK)
      E3DF(3,5) = MIN(MAX(1,I2STH2M),NK)
!
! output of microseismic source spectra
!
      P2MSF(1) = P2SF
      P2MSF(2) = MIN(MAX(1,I1P2SF),NK)
      P2MSF(3) = MIN(MAX(1,I2P2SF),NK)
!
! output of Stokes drift profile
!
      US3DF(1) = US3D
      US3DF(2) = MAX( 1 , MIN( NK, I1US3D) )
      US3DF(3) = MAX( 1 , MIN( NK, I2US3D) )
!
! output of Stokes drift partitions
!
      USSPF(1) = USSP
      USSPF(2) = MAX( 1 , MIN(25, IUSSP ) )
      IF (IUSSP.GT.25) THEN
         WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN ww3_grid:'
         WRITE(NDSE,*) "  Stokes drift partition outputs not    "
         WRITE(NDSE,*) "   intended for use with more than 25   "
         WRITE(NDSE,*) "   partitions.  Please reduce IUSSP     "
         WRITE(NDSE,*) "   specified in ww3_grid.inp to proceed "
         CALL EXTCDE( 31) 
      ENDIF

      DO J=1,USSPF(2)
         USSP_WN(j) = STK_WN(J)
      ENDDO
      
!
      WRITE (NDSO,4971) P2MSF(1:3)
      WRITE (NDSO,4972) US3DF(1:3)
      WRITE (NDSO,4973) E3DF(1:3,1)
      WRITE (NDSO,4974) USSPF(1:2)
      DO J=1,USSPF(2)
         WRITE(NDSO,4975) J,USSP_WN(J)
      ENDDO
!
      CALL READNL ( NDSS, 'MISC', STATUS )
      WRITE (NDSO,960) STATUS
!
      IF ( FLAGTR.LT.0 .OR. FLAGTR.GT.6 ) FLAGTR = 0
      CICEN  = MIN ( 1. , MAX ( 0. , CICEN ) )
      ICESLN  = MIN ( 1. , MAX ( 0. , ICESLN ) )
      ICEWIND = MIN ( 1. , MAX ( 0. , ICEWIND ) )
      ICESDS  = MIN ( 1. , MAX ( 0. , ICESDS ) )
      ICESNL  = MIN ( 1. , MAX ( 0. , ICESNL ) )
      FICEN  = CICEN
      GRIDSHIFT=GSHIFT
      ICESCALES(1)=ICESLN 
      ICESCALES(2)=ICEWIND
      ICESCALES(3)=ICESNL
      ICESCALES(4)=ICESDS
      CMPRTRCK=TRCKCMPR
      CICE0  = MIN ( CICEN , MAX ( 0. , CICE0 ) )
      FICEL  = LICE
      IICEHMIN  = ICEHMIN
      IICEHFAC  = ICEHFAC
      IICEHINIT  = ICEHINIT
      IICEDISP= ICEDISP
      IICEHDISP  = ICEHDISP
      IICEDDISP  = ICEDDISP
      IICEFDISP  = ICEFDISP
      PMOVE  = MAX ( 0. , PMOVE )
      PFMOVE = PMOVE
!
      BTBETA = MIN(MAX (1., BTBET), 2.)
      AAIRCMIN = ALOG(GRAV/AIRCMIN/SIG(1))/ALOG(XFR)+1 ! goes from phase speed C=g/sig to index
      AAIRGB = AIRGB
!
! Notes: Presently, if we select CICE0.ne.CICEN requires an obstruction
!     grid, that is initialized with zeros as default.
      IF ( FLAGTR .LT. 3 ) THEN
        IF (CICE0.NE.CICEN) THEN
          CICE0 = CICEN
          IF (STATUS=='(user def. values) :')  WRITE (NDSO,2961)
          END IF
        END IF
#ifdef W3_IC0
      IF ( CICE0.EQ.CICEN .AND. FLAGTR.GE.3 ) FLAGTR = FLAGTR - 2
#endif
      WRITE (NDSO,961) CICE0, CICEN
      WRITE (NDSO,8972) ICEWIND
      FICE0  = CICE0
! Variables for Space-Time Extremes
      STEXU = STDX
      IF ( STDY .LE. 0. ) THEN
        STDY = STDX
      END IF
      STEYU = STDY
      STEDU = STDT
      IF ( STDX .GT. 0 ) THEN
         WRITE (NDSO,1040) STDX
         WRITE (NDSO,1041) STDY
      ELSE
         WRITE (NDSO,1042)
      END IF
      IF ( STDT .GT. 0 ) THEN
         WRITE (NDSO,1043) STDT 
      ELSE
         WRITE (NDSO,1044)
      END IF
#ifdef W3_MGG
      WRITE (NDSO,962) PMOVE
#endif
!
#ifdef W3_SEED
      XSEED  =  MAX ( 1. , XSEED )
      WRITE (NDSO,964) XSEED
#endif
#ifdef W3_SCRIP
     WRITE (NDSO,963) GSHIFT
#endif
      WRITE (NDSO,1972) TRCKCMPR
      FACSD  = XSEED
#ifdef W3_RWND
 RWINDC = RWNDC 
#endif
#ifdef W3_WCOR
 WWCOR(1) = WCOR1 
 WWCOR(2) = WCOR2 
#endif
!
      XP     = MAX ( 1.E-6 , XP )
      XR     = MAX ( 1.E-6 , XR )
      XREL   = XR
      XFILT  = MAX ( 0. , XFILT )
      XFLT   = XFILT
      WRITE (NDSO,965) XP, XR, XFILT
      FACP   = XP / PI * 0.62E-3 * TPI**4 / GRAV**2
!
      IHMAX  = MAX ( 50, IHM )
      HSPMIN = MAX ( 0.0001 , HSPM )
      WSMULT = MAX ( 1. , WSM )
      WSCUT  = MIN ( 1.0001 , MAX ( 0. , WSC ) )
      FLCOMB = FLC
      NOSWLL = MAX ( 1 , NOSW )
      PTMETH = PTM  ! Partitioning method. Chris Bunney (Jan 2016)
      PTFCUT = PTFC ! Freq cutoff for partitiong method 5
      PMNAM2 = ""
      IF( PTMETH .EQ. 1 ) THEN
        PMNAME = "WW3 default"
      ELSE IF( PTMETH .EQ. 2 ) THEN
        PMNAME = "Watershedding plus wind cut-off"
      ELSE IF( PTMETH .EQ. 3 ) THEN
        PMNAME = "Watershedding only"
        WSCUT = 0.0 ! We don't want to classify by ws frac
        PMNAM2 = "WSC set to 0.0"
      ELSE IF( PTMETH .EQ. 4 ) THEN
        PMNAME = "Wind speed cut-off only"
        PMNAM2 = "WSC set to 0.0, NOSW set to 1"
        WSCUT = 0.0 ! We don't want to classify by ws frac
        NOSWLL = 1  ! Only ever one swell
      ELSE IF( PTMETH .EQ. 5 ) THEN
        WRITE(PMNAME, '("2-Band hi/low cutoff at ", F4.2,"Hz")') PTFCUT
        PMNAM2 = "WSC set to 0.0, NOSW set to 1"
        WSCUT = 0.0 ! We don't want to classify by ws frac
        NOSWLL = 1  ! Only ever one swell
      ELSE
        WRITE( NDSE, * )                                                &
          "*** Error - unknown partitioing method (PTM)! ***"
        CALL EXIT(1)
      ENDIF

        IF ( FLCOMB ) THEN
          J      = 1
        ELSE
          J      = 2
        END IF
      WRITE (NDSO,966) IHMAX, HSPMIN, WSMULT, WSCUT, YESXNO(J), NOSWLL
      WRITE (NDSO,5971) PMNAME
      IF( PMNAM2 .NE. "" ) WRITE (NDSO,5972) PMNAM2
!!    WRITE (NDSO,966) IHMAX, HSPMIN, WSMULT, WSCUT, YESXNO(J)
!
      FHMAX  = MAX ( 0.01 , FMICHE )
      J      = 2
#ifdef W3_MLIM
      J      = 1
#endif
      WRITE (NDSO,967) FHMAX, FHMAX/SQRT(2.), YESXNO(J)
      IF ( FHMAX.LT.0.50 .AND. J.EQ.1 ) WRITE (NDST,968)
!
      IF (TRIM(CALTYPE) .NE. 'standard' .AND.                           &
          TRIM(CALTYPE) .NE. '360_day'  .AND.                           &
          TRIM(CALTYPE) .NE. '365_day' ) GOTO 2003 
      WRITE (NDST,1973) CALTYPE
      WRITE (NDSO,*)
!
! 6.x Read values for FLD stress calculation
!
#ifdef W3_FLD1
      TAILTYPE = 0
      TAILLEV  = 0.006
      TAILT1 = 1.25
      TAILT2 = 3.00
#endif
#ifdef W3_FLD2
      TAILTYPE = 0
      TAILLEV  = 0.006
      TAILT1 = 1.25
      TAILT2 = 3.00
#endif
!
#ifdef W3_FLD1
      CALL READNL ( NDSS, 'FLD1', STATUS )
      TAILLEV  = MIN( MAX ( 0.0005 , TAILLEV ), 0.04)
      TAIL_LEV = TAILLEV
      TAIL_ID = TAILTYPE
      TAIL_TRAN1 = TAILT1
      TAIL_TRAN2 = TAILT2
#endif
#ifdef W3_FLD2
      CALL READNL ( NDSS, 'FLD2', STATUS )
      TAILLEV  = MIN( MAX ( 0.0005 , TAILLEV ), 0.04)
      TAIL_LEV = TAILLEV
      TAIL_ID = TAILTYPE
      TAIL_TRAN1 = TAILT1
      TAIL_TRAN2 = TAILT2
#endif
!
! 6.o End of namelist processing
!
      IF (FLGNML) THEN
        CLOSE (NDSS)
      ELSE
        CLOSE (NDSS,STATUS='DELETE')
      END IF
!
      IF ( FLNMLO ) THEN
        WRITE (NDSO,917)
#ifdef W3_FLX3
          WRITE (NDSO,2810) CDMAX*1.E3, CTYPE
#endif
#ifdef W3_FLX4
          WRITE (NDSO,2810) CDFAC
#endif
#ifdef W3_LN1
          WRITE (NDSO,2820) CLIN, RFPM, RFHF
#endif
#ifdef W3_ST1
          WRITE (NDSO,2920) CINP
#endif
        IF ( .NOT. FLSTB2 ) THEN
#ifdef W3_ST2
              WRITE (NDSO,2920) ZWND, SWELLF
#endif
        ELSE
#ifdef W3_STAB2
              WRITE (NDSO,2921) ZWND, SWELLF, STABSH, STABOF,  &
                                CNEG, CPOS, FNEG
#endif
          END IF
!
#ifdef W3_ST3
          WRITE (NDSO,2920) ZWND, ALPHA0, Z0MAX, BETAMAX, SINTHP, ZALP,   &
            SWELLF
#endif
#ifdef W3_ST4
          WRITE (NDSO,2920) ZWND, ALPHA0, Z0MAX, BETAMAX, SINTHP, ZALP,   &
            TAUWSHELTER, SWELLFPAR, SWELLF, SWELLF2, SWELLF3, SWELLF4, &
            SWELLF5, SWELLF6, SWELLF7, Z0RAT, SINBR
#endif
#ifdef W3_ST6
          WRITE (NDSO,2920) SINA0, SINWS, SINFC
#endif
#ifdef W3_NL1
          WRITE (NDSO,2922) LAMBDA, NLPROP, KDCONV, KDMIN,       &
                            SNLCS1, SNLCS2, SNLCS3
#endif
#ifdef W3_NL2
          WRITE (NDSO,2922) IQTYPE, TAILNL, NDEPTH
          IF ( IQTYPE .EQ. 3 ) THEN
              IF ( NDEPTH .EQ. 1 ) THEN
                  WRITE (NDSO,3923) DPTHNL(1)
                ELSE
                  WRITE (NDSO,4923) DPTHNL(1)
                END IF
              WRITE (NDSO,5923) DPTHNL(2:NDEPTH-1)
              WRITE (NDSO,6923) DPTHNL(NDEPTH)
            END IF
#endif
#ifdef W3_NL3
          WRITE (NDSO,2922) NQDEF, MSC, NSC, KDFD, KDFS
          IF ( NQDEF .EQ. 1 ) THEN
              WRITE (NDSO,3923) QPARMS(1:5)
            ELSE
              WRITE (NDSO,4923) QPARMS(1:5)
              DO J=2, NQDEF-1
                WRITE (NDSO,5923) QPARMS((J-1)*5+1:J*5)
                END DO
              WRITE (NDSO,6923) QPARMS((NQDEF-1)*5+1:NQDEF*5)
            END IF
#endif
#ifdef W3_NL4
          WRITE (NDSO,2922) INDTSA, ALTLP
#endif
#ifdef W3_NL5
          WRITE (NDSO,2922) QR5DPT, QR5OML, QI5DIS, QI5KEV, QI5IPL, QI5PMX
#endif
#ifdef W3_NLS
          WRITE (NDSO,8922) A34, FHFC, DNM, FC1, FC2, FC3
#endif
#ifdef W3_ST1
          WRITE (NDSO,2924) CDIS, APM
#endif
#ifdef W3_ST2
          WRITE (NDSO,2924) SDSA0, SDSA1, SDSA2, SDSB0, SDSB1, PHIMIN
#endif
#ifdef W3_ST3
          WRITE (NDSO,2924) SDSC1, WNMEANP, FXPM3, FXFM3, SDSDELTA1,  &
                            SDSDELTA2
#endif

#ifdef W3_ST4
          WRITE (NDSO,2924) SDSBCHOICE, SDSC2, SDSCUM, SDSC4,         &
                            SDSC5, SDSC6, &
                    WNMEANP, FXPM3, FXFM3, FXFMAGE,                   &
                    SDSBINT, SDSBCK, SDSABK, SDSPBK, SDSHCK,          &
                    SDSBR, SDSSTRAIN,  SDSSTRAINA,  SDSSTRAIN2,       &
                    SDSBT, SDSP, SDSISO, SDSCOS, SDSDTH, SDSBRF1,     &
                    SDSBRFDF, SDSBM0, SDSBM1, SDSBM2, SDSBM3, SDSBM4, &
                    SPMSS, SDKOF, SDSMWD, SDSFACMTF, SDSNMTF,SDSMWPOW,&
                    SDSCUMP, SDSNUW, WHITECAPWIDTH, WHITECAPDUR
#endif
#ifdef W3_ST6
          WRITE (NDSO,2924) SDSET, SDSA1, SDSA2, SDSP1, SDSP2
          WRITE (NDSO,2937) SWLB1, CSTB1
#endif
#ifdef W3_BT1
          WRITE (NDSO,2926) GAMMA
#endif
#ifdef W3_BT4
          WRITE (NDSO,2926) SEDMAPD50, SED_D50_UNIFORM,      &
                            RIPFAC1,RIPFAC2,RIPFAC3,RIPFAC4, SIGDEPTH, &
                            BOTROUGHMIN, BOTROUGHFAC
#endif
#ifdef W3_DB1
          IF ( BJFLAG ) THEN
              WRITE (NDSO,2928) BJALFA, BJGAM, '.TRUE.'
            ELSE
              WRITE (NDSO,2928) BJALFA, BJGAM, '.FALSE.'
            END IF
#endif
#ifdef W3_PR1
          WRITE (NDSO,2953) CFLTM
#endif
#ifdef W3_PR2
          WRITE (NDSO,2953) CFLTM, DTIME, LATMIN
#endif
#ifdef W3_SMC
          WRITE (NDSO,2954) CFLSM, DTIMS, Arctic, RFMAXD, UNO3, &
                      AVERG, LvSMC, NBISMC, ISHFT, JEQT, SEAWND 
#endif
#ifdef W3_PR3
          WRITE (NDSO,2953) CFLTM, WDTHCG, WDTHTH
#endif
!
        WRITE (NDSO,2956) UGBCCFL, UGOBCAUTO, UGOBCDEPTH,TRIM(UGOBCFILE), &
                          EXPFSN, EXPFSPSI, EXPFSFCT, IMPFSN, EXPTOTAL,&
                          IMPTOTAL, IMPREFRACTION, IMPFREQSHIFT,      &
                          IMPSOURCE, SETUP_APPLY_WLV,                 &
                          JGS_TERMINATE_MAXITER,                      &
                          JGS_TERMINATE_DIFFERENCE,                   &
                          JGS_TERMINATE_NORM,                         &
                          JGS_LIMITER,                                &
                          JGS_USE_JACOBI,                             &
                          JGS_BLOCK_GAUSS_SEIDEL,                     &
                          JGS_MAXITER,                                &
                          JGS_PMIN,                                   &
                          JGS_DIFF_THR,                               &
                          JGS_NORM_THR,                               &
                          JGS_NLEVEL,                                 &
                          JGS_SOURCE_NONLINEAR
!
        WRITE (NDSO,2976)    P2SF, I1P2SF, I2P2SF,                    &
                             US3D, I1US3D, I2US3D,                    & 
                             USSP, IUSSP,                             &
                             E3D, I1E3D, I2E3D,                       &
                             TH1MF, I1TH1M, I2TH1M,                   &
                             STH1MF, I1STH1M, I2STH1M,                &
                             TH2MF, I1TH2M, I2TH2M,                   &
                             STH2MF, I1STH2M, I2STH2M
!
#ifdef W3_REF1
         WRITE(NDSO,2986) REFCOAST, REFFREQ, REFSLOPE, REFMAP,  &
                    REFMAPD, REFSUBGRID , REFRMAX, REFFREQPOW,  &
                    REFICEBERG, REFCOSP_STRAIGHT, REFUNSTSOURCE
#endif
!
#ifdef W3_IG1
         WRITE(NDSO,2977) IGMETHOD, IGADDOUTP, IGSOURCE,         &
                    IGSTERMS, IGBCOVERWRITE, IGSWELLMAX,         &
                    IGMAXFREQ, IGSOURCEATBP, IGKDMIN,            &
                    IGFIXEDDEPTH, IGEMPIRICAL 
#endif
!
#ifdef W3_IC2
         WRITE(NDSO,2978) IC2DISPER, IC2TURB, IC2ROUGH,          &
                    IC2REYNOLDS,  IC2SMOOTH, IC2VISC, IC2TURBS,  &
                    IC2DMAX
#endif
!
#ifdef W3_IC3
         WRITE(NDSO,2979) IC3MAXTHK, IC3MAXCNC, IC2TURB,         &
                          IC2ROUGH,  IC2REYNOLDS,  IC2SMOOTH,    &
                          IC2VISC, IC2TURBS, IC3CHENG,           &
                          USECGICE, IC3HILIM, IC3KILIM,          &
                          IC3HICE, IC3VISC, IC3DENS, IC3ELAS
#endif
!
#ifdef W3_IC4
         WRITE(NDSO,NML=SIC4)
#endif
!
#ifdef W3_IC5
         WRITE(NDSO,2981) IC5MINIG, IC5MINWT, IC5MAXKRATIO,       &
                          IC5MAXKI, IC5MINHW, IC5MAXITER,         &
                          IC5RKICK, IC5KFILTER, IC5VEMOD
#endif
!
#ifdef W3_IS1
          WRITE (NDSO,2946) IS1C1, IS1C2
#endif
!
#ifdef W3_IS2
          WRITE (NDSO,948) ISC1, IS2BACKSCAT, IS2ISOSCAT, IS2BREAK,  &
                    IS2DUPDATE, IS2FLEXSTR, IS2DISP,  IS2DAMP, IS2FRAGILITY, IS2DMIN, IS2C2,   &
                    IS2C3, IS2CONC, IS2CREEPB, IS2CREEPC, IS2CREEPD,  &
                    IS2CREEPN, IS2BREAKE, IS2BREAKF, IS2WIM1, IS2ANDISB, &
                    IS2ANDISE, IS2ANDISD, IS2ANDISN
#endif
!
#ifdef W3_UOST
         WRITE (NDSO, 4502) ADJUSTL(TRIM(UOSTFILELOCAL)), ADJUSTL(TRIM(UOSTFILESHADOW)), &
                   UOSTFACTORLOCAL, UOSTFACTORSHADOW
#endif

!
        IF ( FLCOMB ) THEN
          WRITE (NDSO,2966) CICE0, CICEN, LICE, PMOVE, XSEED, FLAGTR, &
                                XP, XR, XFILT, IHMAX, HSPMIN, WSMULT, &
                                WSCUT, '.TRUE.', NOSWLL, FHMAX,       &
                                RWNDC, WCOR1, WCOR2, FACBERG, GSHIFT, &
                                STDX, STDY, STDT, ICEHMIN, ICEHFAC,   &
                                ICEHINIT, ICEDISP, ICEHDISP,          &
                                ICESLN, ICEWIND, ICESNL, ICESDS,      &
                                ICEDDISP,ICEFDISP, CALTYPE, TRCKCMPR, &
                                BTBETA
        ELSE
          WRITE (NDSO,2966) CICE0, CICEN, LICE, PMOVE, XSEED, FLAGTR, &
                                XP, XR, XFILT, IHMAX, HSPMIN, WSMULT, &
                                WSCUT, '.FALSE.', NOSWLL, FHMAX,      &
                                RWNDC, WCOR1, WCOR2, FACBERG, GSHIFT, &
                                STDX, STDY, STDT,  ICEHMIN, ICEHFAC,  &
                                ICEHINIT, ICEDISP, ICEHDISP,          &
                                ICESLN, ICEWIND, ICESNL, ICESDS,      &
                                ICEDDISP, ICEFDISP, CALTYPE, TRCKCMPR,&
                                BTBETA
          END IF
!
#ifdef W3_FLD1
         WRITE(NDSO,2987) TAIL_ID, TAIL_LEV, TAIL_TRAN1, TAIL_TRAN2
#endif
#ifdef W3_FLD2
         WRITE(NDSO,2987) TAIL_ID, TAIL_LEV, TAIL_TRAN1, TAIL_TRAN2
#endif
#ifdef W3_RTD
         WRITE(NDSO,4991) PLAT, PLON, UNROT
         WRITE(NDSO,4992) BPLAT, BPLON
#endif
!
        WRITE (NDSO,918)
        END IF
!
! 6.p Set various other values ...
! ... Tail in integration       --> scale factor for A to E conv
!
      FTE    = 0.25 * SIG(NK)      * DTH * SIG(NK)
      FTF    = 0.20                * DTH * SIG(NK)
      FTWN   = 0.20 * SQRT(GRAV)   * DTH * SIG(NK)
      FTTR   = FTF
      FTWL   = GRAV / 6. / SIG(NK) * DTH * SIG(NK)
#ifdef W3_ST3
      STXFTF      = 1/(FACHF-1.-WNMEANP*2)                       &
                           * SIG(NK)**(2+WNMEANP*2) * DTH
      STXFTFTAIL  = 1/(FACHF-1.-WNMEANPTAIL*2)                   &
                           * SIG(NK)**(2+WNMEANPTAIL*2) * DTH
      STXFTWN = 1/(FACHF-1.-WNMEANP*2) * SIG(NK)**(2)            &
                 * (SIG(NK)/SQRT(GRAV))**(WNMEANP*2)   * DTH
      SSTXFTF     = STXFTF
      SSTXFTFTAIL = STXFTFTAIL
      SSTXFTWN    = STXFTWN
#endif
!
#ifdef W3_ST4
      STXFTF      = 1/(FACHF-1.-WNMEANP*2)                       &
                           * SIG(NK)**(2+WNMEANP*2) * DTH
      STXFTFTAIL  = 1/(FACHF-1.-WNMEANPTAIL*2)                   &
                           * SIG(NK)**(2+WNMEANPTAIL*2) * DTH
      STXFTWN = 1/(FACHF-1.-WNMEANP*2) * SIG(NK)**(2)            &
                 * (SIG(NK)/SQRT(GRAV))**(WNMEANP*2)   * DTH
      SSTXFTF     = STXFTF
      SSTXFTFTAIL = STXFTFTAIL
      SSTXFTWN    = STXFTWN
#endif
!
! ... High frequency cut-off
!
      FXFM   = 2.5
#ifdef W3_ST6
       FXFM   = SIN6FC
#endif
      FXPM   = 4.0
      FXPM   = FXPM * GRAV / 28.
      FXFM   = FXFM * TPI
      XFC    = 3.0
#ifdef W3_ST2
      XFH    = 2.0
      XF1    = 1.75
      XF2    = 2.5
      XFT    = XF2
#endif
!
      FACTI1 = 1. / LOG(XFR)
      FACTI2 = 1. - LOG(TPI*FR1) * FACTI1
!
! Setting of FACHF moved to before !/NL2 set-up for consistency
!
#ifdef W3_NL2
      FACHF  = -TAILNL
#endif
      FACHFA = XFR**(-FACHF-2)
      FACHFE = XFR**(-FACHF)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 7.  Read and prepare the grid.
! 7.a Type of grid
!
      IF (FLGNML) THEN
        GSTRG=TRIM(NML_GRID%TYPE)
        IF (TRIM(NML_GRID%COORD).EQ.'SPHE') FLAGLL=.TRUE.
        IF (TRIM(NML_GRID%COORD).EQ.'CART') FLAGLL=.FALSE.
        CSTRG=TRIM(NML_GRID%CLOS)
      ELSE
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=2001,ERR=2002) GSTRG, FLAGLL, CSTRG
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
      END IF

      SELECT CASE (TRIM(GSTRG))
        CASE ('RECT')
          GTYPE = RLGTYPE
          WRITE (NDSO,3000) 'rectilinear'
        CASE ('CURV')
          GTYPE = CLGTYPE
          WRITE (NDSO,3000) 'curvilinear'
        CASE ('UNST')
          GTYPE = UNGTYPE
          WRITE (NDSO,3000) 'unstructured'
!!Li  Add SMC grid type option.  JGLi12Oct2020
        CASE ('SMCG')
          GTYPE = SMCTYPE
          WRITE (NDSO,3000) 'SMC Grid'
        CASE DEFAULT
          WRITE (NDSE,1007) TRIM(GSTRG)
          CALL EXTCDE ( 25 )
        END SELECT      
!
      IF ( FLAGLL ) THEN
          FACTOR = 1.
          WRITE (NDSO,3001) 'spherical'
        ELSE
          FACTOR = 1.E-3
          WRITE (NDSO,3001) 'Cartesian'
        END IF
!
!     Only process grid closure string for logically rectangular grids.
!     Closure setting for unstructured grids is NONE.
      ICLOSE = ICLOSE_NONE
      IF ( GTYPE.NE.UNGTYPE ) THEN
          SELECT CASE (TRIM(CSTRG))
            CASE ('NONE')
              ICLOSE = ICLOSE_NONE
              WRITE (NDSO,3002) 'none'
            CASE ('SMPL')
              ICLOSE = ICLOSE_SMPL
              WRITE (NDSO,3002) 'simple'
            CASE ('TRPL')
              WRITE (NDSE,'(/2A)') ' *** WARNING WW3_GRID: TRIPOLE ',  &
              'GRID CLOSURE IMPLEMENTATION IS INCOMPLETE ***'
              ICLOSE = ICLOSE_TRPL
              WRITE (NDSO,3002) 'tripole'
              IF ( GTYPE.EQ.RLGTYPE ) THEN
                  WRITE (NDSE,1009)
                  CALL EXTCDE ( 25 )
                END IF
            CASE DEFAULT
              ! Check for old style GLOBAL input
              SELECT CASE (TRIM(CSTRG))
                CASE ('T','t','.TRU','.tru')
                  ICLOSE = ICLOSE_SMPL
                  WRITE (NDSO,3002) 'simple'
                  WRITE (NDSE,1013)
                CASE ('F','f','.FAL','.fal')
                  ICLOSE = ICLOSE_NONE
                  WRITE (NDSO,3002) 'none'
                  WRITE (NDSE,1013)
                CASE DEFAULT
                  WRITE (NDSE,1012) TRIM(CSTRG)
                  CALL EXTCDE ( 25 )
                END SELECT
            END SELECT
          IF ( ICLOSE.NE.ICLOSE_NONE .AND. .NOT.FLAGLL ) THEN
              WRITE (NDSE,1008)
              CALL EXTCDE ( 25 )
            END IF
        END IF !GTYPE.NE.UNGTYPE
!
! 7.b Size of grid
!
      IF (FLGNML) THEN
        SELECT CASE ( GTYPE )
!!Li  SMCTYPE shares domain info with RLGTYPE.  JGLi12Oct2020
        CASE ( RLGTYPE, SMCTYPE )
          NX = NML_RECT%NX
          NY = NML_RECT%NY
          NX = MAX ( 3 , NX )
          NY = MAX ( 3 , NY )
          WRITE (NDSO,3003) NX, NY
        CASE ( CLGTYPE )
          NX = NML_CURV%NX
          NY = NML_CURV%NY
          NX = MAX ( 3 , NX )
          NY = MAX ( 3 , NY )
          WRITE (NDSO,3003) NX, NY
        CASE ( UNGTYPE )
          NY=1
        END SELECT
      ELSE
        IF ( GTYPE.NE.UNGTYPE) THEN 
          CALL NEXTLN ( COMSTR , NDSI , NDSE )
          READ (NDSI,*,END=2001,ERR=2002) NX, NY
          NX     = MAX ( 3 , NX )
          NY     = MAX ( 3 , NY )
          WRITE (NDSO,3003) NX, NY
        ELSE 
          NY =1 
        END IF
      END IF
!
! Propagation specific to unstructured grids
!
      DO_CHANGE_WLV=.FALSE.
      IF ( GTYPE.EQ.UNGTYPE) THEN 
        UNSTSCHEMES(:)=0
        IF (EXPFSN)   UNSTSCHEMES(1)=1
        IF (EXPFSPSI) UNSTSCHEMES(2)=1
        IF (EXPFSFCT) UNSTSCHEMES(3)=1
        IF (IMPFSN)   UNSTSCHEMES(4)=1
        UNSTSCHEME=-1
        DO IX=1,4
          IF (UNSTSCHEMES(IX).EQ.1) THEN 
            UNSTSCHEME=IX
            EXIT
          END IF
        END DO
 
        FSBCCFL = UGBCCFL
        SELECT CASE (UNSTSCHEME)
        CASE (1) 
          FSN = EXPFSN
          PNAME2 = 'N Explicit (Fluctuation Splitting) '
        CASE (2) 
          FSPSI = EXPFSPSI
          PNAME2 = 'PSI Explicit (Fluctuation Splitting)  '
        CASE (3) 
          FSFCT = EXPFSFCT
          PNAME2 = ' Flux Corrected Transport Explicit'
        CASE (4) 
          FSNIMP = IMPFSN 
          PNAME2 = 'N Implicit (Fluctuation Splitting) '
          END SELECT
!
        IF (SUM(UNSTSCHEMES).GT.1) WRITE(NDSO,1035)
        WRITE (NDSO,2951) PNAME2
        IF (IMPTOTAL) THEN
          FSTOTALIMP = IMPTOTAL
          PNAME2 = 'N Implicit (Fluctuation Splitting) for total implicit'
        END IF
        IF (EXPTOTAL) THEN
          FSTOTALEXP = EXPTOTAL 
          PNAME2 = 'N Explicit (Fluctuation Splitting) for one exchange explicit DC HPCF '
        END IF
        IF (IMPREFRACTION .and. IMPTOTAL .AND. FLCTH) THEN
          FSREFRACTION = .TRUE.
          PNAME2 = 'Refraction done implicitly'
          WRITE (NDSO,2951) PNAME2
        ELSE
          FSREFRACTION = .FALSE.
        END IF
        IF (IMPFREQSHIFT .and. IMPTOTAL .AND. FLCK) THEN
          FSFREQSHIFT = .TRUE.
          PNAME2 = 'Frequency shifting done implicitly'
          WRITE (NDSO,2951) PNAME2
        ELSE
          FSFREQSHIFT = .FALSE.
        END IF
        IF (IMPSOURCE .and. IMPTOTAL .AND. FLSOU) THEN
          FSSOURCE = .TRUE.
          PNAME2 = 'Source terms integrated implicitly'
          WRITE (NDSO,2951) PNAME2
        ELSE
          FSSOURCE = .FALSE.
        END IF
        IF (SETUP_APPLY_WLV) THEN
          DO_CHANGE_WLV = SETUP_APPLY_WLV
          PNAME2 = ' we change WLV'
          WRITE (NDSO,2952) PNAME2
        END IF
        SOLVERTHR_STP = SOLVERTHR_SETUP
        CRIT_DEP_STP  = CRIT_DEP_SETUP
      END IF

!
! 7.c Grid coordinates (branch here based on grid type)
!
      IF ( GTYPE.NE.UNGTYPE) ALLOCATE ( XGRDIN(NX,NY), YGRDIN(NX,NY) )
      SELECT CASE ( GTYPE )
!
! 7.c.1 Rectilinear grid
!
!!Li  SMC grid shares domain info with RLGTYPE.   JGLi12Oct2020 
        CASE ( RLGTYPE, SMCTYPE )
!
          IF (FLGNML) THEN
            SX = NML_RECT%SX 
            SY = NML_RECT%SY
            VSC = NML_RECT%SF
            X0 = NML_RECT%X0
            Y0 = NML_RECT%Y0
            VSC0 = NML_RECT%SF0
          ELSE
            CALL NEXTLN ( COMSTR , NDSI , NDSE )
            READ (NDSI,*,END=2001,ERR=2002) SX, SY, VSC
            CALL NEXTLN ( COMSTR , NDSI , NDSE )
            READ (NDSI,*,END=2001,ERR=2002) X0, Y0, VSC0
          END IF
!
          VSC    = MAX ( 1.E-7 , VSC )
          SX     = SX / VSC
          SY     = SY / VSC
          SX     = MAX ( 1.E-7 , SX )
          SY     = MAX ( 1.E-7 , SY )
          IF ( ICLOSE.EQ.ICLOSE_SMPL ) SX = 360. / REAL(NX)
!
          VSC0    = MAX ( 1.E-7 , VSC0 )
          X0     = X0 / VSC0
          Y0     = Y0 / VSC0
!
          IF ( FLAGLL ) THEN
              WRITE (NDSO,3004) FACTOR*SX, FACTOR*SY,         &
                     FACTOR*X0, FACTOR*(X0+REAL(NX-1)*SX),    &
                     FACTOR*Y0, FACTOR*(Y0+REAL(NY-1)*SY)
            ELSE
              WRITE (NDSO,3005) FACTOR*SX, FACTOR*SY,         &
                     FACTOR*X0, FACTOR*(X0+REAL(NX-1)*SX),    &
                     FACTOR*Y0, FACTOR*(Y0+REAL(NY-1)*SY)
            END IF
!
          DO IY=1, NY
            DO IX=1, NX
              XGRDIN(IX,IY) = X0 + REAL(IX-1)*SX
              YGRDIN(IX,IY) = Y0 + REAL(IY-1)*SY
              END DO
            END DO
!
! 7.c.2 Curvilinear grid
!
        CASE ( CLGTYPE )
!
! 7.c.2.a Process x-coordinates
!
          IF (FLGNML) THEN
            NDSG = NML_CURV%XCOORD%IDF
            VSC = NML_CURV%XCOORD%SF
            VOF = NML_CURV%XCOORD%OFF
            IDLA = NML_CURV%XCOORD%IDLA
            IDFM = NML_CURV%XCOORD%IDFM
            RFORM = TRIM(NML_CURV%XCOORD%FORMAT)
            FROM = TRIM(NML_CURV%XCOORD%FROM)
            FNAME = TRIM(NML_CURV%XCOORD%FILENAME)
          ELSE
            CALL NEXTLN ( COMSTR , NDSI , NDSE )
            READ (NDSI,*,END=2001,ERR=2002) NDSG, VSC, VOF, &
                                            IDLA, IDFM, RFORM, FROM, FNAME
          END IF
!
          IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
          IF (IDFM.LT.1 .OR. IDFM.GT.3) IDFM   = 1
!
          WRITE (NDSO,3006) NDSG, VSC, VOF, IDLA, IDFM
          IF (IDFM.EQ.2) WRITE (NDSO,3008) TRIM(RFORM)
          IF (FROM.EQ.'NAME' .AND. NDSG.NE.NDSI) &
              WRITE (NDSO,3009) TRIM(FNAME)
!
          IF ( NDSG .EQ. NDSI ) THEN
            IF ( IDFM .EQ. 3 ) THEN
              WRITE (NDSE,1004) NDSG
              CALL EXTCDE (23)
            ELSE
              IF (.NOT.FLGNML) THEN
              CALL NEXTLN ( COMSTR , NDSI , NDSE )
              END IF
            END IF
          ELSE
            IF ( IDFM .EQ. 3 ) THEN
              IF (FROM.EQ.'NAME') THEN
                OPEN (NDSG,FILE=TRIM(FNMPRE)//TRIM(FNAME),&
                      FORM='UNFORMATTED',                 &
                      STATUS='OLD',ERR=2000,IOSTAT=IERR)
              ELSE
                OPEN (NDSG,                               &
                      FORM='UNFORMATTED',                 &
                      STATUS='OLD',ERR=2000,IOSTAT=IERR)
              END IF
            ELSE
              IF (FROM.EQ.'NAME') THEN
                  OPEN (NDSG,FILE=TRIM(FNMPRE)//TRIM(FNAME),&
                  STATUS='OLD',ERR=2000,IOSTAT=IERR)
              ELSE
                  OPEN (NDSG,                               &
                        STATUS='OLD',ERR=2000,IOSTAT=IERR)
              END IF
            END IF !IDFM
          END IF !NDSG
!
          CALL INA2R ( XGRDIN, NX, NY, 1, NX, 1, NY, NDSG, NDST, NDSE, &
                       IDFM, RFORM, IDLA, VSC, VOF)
!
! 7.c.2.b Process y-coordinates
!
          IF (FLGNML) THEN
            NDSG = NML_CURV%YCOORD%IDF
            VSC = NML_CURV%YCOORD%SF
            VOF = NML_CURV%YCOORD%OFF
            IDLA = NML_CURV%YCOORD%IDLA
            IDFM = NML_CURV%YCOORD%IDFM
            RFORM = TRIM(NML_CURV%YCOORD%FORMAT)
            FROM = TRIM(NML_CURV%YCOORD%FROM)
            FNAME = TRIM(NML_CURV%YCOORD%FILENAME)
          ELSE
            CALL NEXTLN ( COMSTR , NDSI , NDSE )
            READ (NDSI,*,END=2001,ERR=2002) NDSG, VSC, VOF, &
                                            IDLA, IDFM, RFORM, FROM, FNAME
          END IF
!
          IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
          IF (IDFM.LT.1 .OR. IDFM.GT.3) IDFM   = 1
!
          WRITE (NDSO,3007) NDSG, VSC, VOF, IDLA, IDFM
          IF (IDFM.EQ.2) WRITE (NDSO,3008) TRIM(RFORM)
          IF (FROM.EQ.'NAME' .AND. NDSG.NE.NDSI) &
              WRITE (NDSO,3009) TRIM(FNAME)
!
          IF ( NDSG .EQ. NDSI ) THEN
            IF ( IDFM .EQ. 3 ) THEN
              WRITE (NDSE,1004) NDSG
              CALL EXTCDE (23)
            ELSE
              IF (.NOT.FLGNML) THEN
                CALL NEXTLN ( COMSTR , NDSI , NDSE )
              END IF
            END IF
          ELSE
            IF ( IDFM .EQ. 3 ) THEN
              IF (FROM.EQ.'NAME') THEN
                OPEN (NDSG,FILE=TRIM(FNMPRE)//TRIM(FNAME),&
                      FORM='UNFORMATTED',                 &
                      STATUS='OLD',ERR=2000,IOSTAT=IERR)
              ELSE
                OPEN (NDSG,                               &
                      FORM='UNFORMATTED',                 &
                      STATUS='OLD',ERR=2000,IOSTAT=IERR)
              END IF
            ELSE
              IF (FROM.EQ.'NAME') THEN
                  OPEN (NDSG,FILE=TRIM(FNMPRE)//TRIM(FNAME),&
                  STATUS='OLD',ERR=2000,IOSTAT=IERR)
              ELSE
                OPEN (NDSG,                               &
                      STATUS='OLD',ERR=2000,IOSTAT=IERR)
              END IF
            END IF !IDFM
          END IF !NDSG
!
          CALL INA2R ( YGRDIN, NX, NY, 1, NX, 1, NY, NDSG, NDST, NDSE, &
                       IDFM, RFORM, IDLA, VSC, VOF)
!
! 7.c.2.c Check for obvious errors in grid definition or input
!
! ....... Check for inverted grid (can result from wrong IDLA)
          IF ( (XGRDIN(2,1)-XGRDIN(1,1))*(YGRDIN(1,2)-YGRDIN(1,1)) .LT. &
               (YGRDIN(2,1)-YGRDIN(1,1))*(XGRDIN(1,2)-XGRDIN(1,1)) ) THEN
             WRITE (NDSE,1011) IDLA
!.........Notes: here, we are checking to make sure that the j axis is ~90 degrees
!................counter-clockwise from the i axis (the standard cartesian setup).
!................So, it is a check on the handedness of the grid.
!................We have confirmed for one case that a left-handed grid produces
!................errors in SCRIP. We have not confirmed that left-handed grids necessarily
!................produce errors in single-grid simulations, or that they necessarily
!................produce errors in all multi-grid simulations.
!................Note that transposing or flipping a grid will generally change the handedness.
             CALL EXTCDE (25)
          END IF
!
! 7.c.3 Unstructured grid
!
        CASE ( UNGTYPE )
!
          MAXX = 0.
          MAXY = 0.
          DXYMAX = 0.
          WRITE (NDSO,1150)

          IF (FLGNML) THEN
            ZLIM = NML_GRID%ZLIM
            DMIN = NML_GRID%DMIN
            NDSG = NML_UNST%IDF
            VSC = NML_UNST%SF
            IDLA = NML_UNST%IDLA
            IDFM = NML_UNST%IDFM
            RFORM = TRIM(NML_UNST%FORMAT)
            FROM = 'NAME'
            FNAME = TRIM(NML_UNST%FILENAME)
            UGOBCFILE = TRIM(NML_UNST%UGOBCFILE)
          END IF
      END SELECT !GTYPE
!
! 7.d Depth information for grid
!
      IF (FLGNML) THEN
        IF (GTYPE.NE.UNGTYPE) THEN
          ZLIM = NML_GRID%ZLIM
          DMIN = NML_GRID%DMIN
          NDSG = NML_DEPTH%IDF
          VSC = NML_DEPTH%SF
          IDLA = NML_DEPTH%IDLA
          IDFM = NML_DEPTH%IDFM
          RFORM = TRIM(NML_DEPTH%FORMAT)
          FROM = TRIM(NML_DEPTH%FROM)
          FNAME = TRIM(NML_DEPTH%FILENAME)
        END IF
      ELSE
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=2001,ERR=2002) ZLIM, DMIN, NDSG, VSC, IDLA,    &
                                        IDFM, RFORM, FROM, FNAME
      END IF
!
      DMIN    = MAX ( 1.E-3 , DMIN )
      IF (   ABS(VSC) .LT. 1.E-7  ) VSC    = 1.
      IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
      IF (IDFM.LT.1 .OR. IDFM.GT.3) IDFM   = 1
!
      WRITE (NDSO,972) NDSG, ZLIM, DMIN, VSC, IDLA, IDFM
      IF (IDFM.EQ.2) WRITE (NDSO,973) TRIM(RFORM)
      IF (FROM.EQ.'NAME' .AND. NDSG.NE.NDSI) &
          WRITE (NDSO,974) TRIM(FNAME)
!
! 7.e Read bottom depths
!
      IF ( GTYPE.NE.UNGTYPE ) THEN 
!
! Reading depths on structured grid 
!
        ALLOCATE ( ZBIN(NX,NY), OBSX(NX,NY), OBSY(NX,NY) )
!
!       Initialize subgrid obstructions with zeros.
        ZBIN(:,:)=0.
        OBSX(:,:)=0.
        OBSY(:,:)=0.

!Li Suspended for SMC grid, which uses depth stored in its cell array.
!Li               JGLi15Oct2014
        IF( GTYPE .NE. SMCTYPE ) THEN
!
          IF ( NDSG .EQ. NDSI ) THEN
              IF ( IDFM .EQ. 3 ) THEN
                  WRITE (NDSE,1004) NDSG
                  CALL EXTCDE (23)
                ELSE
                  CALL NEXTLN ( COMSTR , NDSI , NDSE )
                END IF
            ELSE  ! NDSG.NE.NDSI
              IF ( IDFM .EQ. 3 ) THEN
                  IF (FROM.EQ.'NAME') THEN
                      OPEN (NDSG,FILE=TRIM(FNMPRE)//TRIM(FNAME), &
                            FORM='UNFORMATTED',&
                            STATUS='OLD',ERR=2000,IOSTAT=IERR)
                    ELSE
                      OPEN (NDSG, FORM='UNFORMATTED',                &
                            STATUS='OLD',ERR=2000,IOSTAT=IERR)
                    END IF
                ELSE
                  IF (FROM.EQ.'NAME') THEN
                      OPEN (NDSG,FILE=TRIM(FNMPRE)//TRIM(FNAME),  &
                            STATUS='OLD',ERR=2000,IOSTAT=IERR)
                    ELSE
                      OPEN (NDSG,                                     &
                            STATUS='OLD',ERR=2000,IOSTAT=IERR)
                    END IF
                END IF
            END IF  !( NDSG .EQ. NDSI ) 
!
          CALL INA2R ( ZBIN, NX, NY, 1, NX, 1, NY, NDSG, NDST, NDSE,      &
                       IDFM, RFORM, IDLA, VSC, 0.0)
!
!Li     End of IF( GTYPE .NE. SMCTYPE ) block
        ENDIF
!
      ELSE
!
! Reading depths on unstructured grid (this also sets number of mesh points, NX)
!
        CALL READMSH(NDSG,FNAME)
        ALLOCATE(ZBIN(NX, NY),OBSX(NX,NY),OBSY(NX,NY))
        ZBIN(:,1) = VSC*XYB(:,3)
#ifdef W3_DEBUGSTP
        WRITE(740,*) 'VSC=', VSC
        WRITE(740,*) 'Printing ZBIN 1'
        DO IX=1,NX
          WRITE(740,*) 'IX/ZBIN=', IX, ZBIN(IX,1)
        END DO
#endif
!
! subgrid obstructions are not yet handled in unstructured grids
!
        OBSX(:,:)=0.
        OBSY(:,:)=0.

      END IF
!
! 7.f Set up temporary map
!
      ALLOCATE ( TMPSTA(NY,NX), TMPMAP(NY,NX) )
      TMPSTA = 0
!
#ifdef W3_DEBUGSTP
          WRITE(740,*) 'Printing ZBIN 2'
          DO IX=1,NX
            WRITE(740,*) 'IX/ZBIN=', IX, ZBIN(IX,1)
          END DO
#endif
      IF (GTYPE .EQ. UNGTYPE) THEN
        TMPSTA = 1
      ELSE
        DO IY=1, NY
          DO IX=1, NX
            IF ( ZBIN(IX,IY) .LE. ZLIM ) TMPSTA(IY,IX) = 1
          END DO
        END DO
      ENDIF
!
!Li   Suspended for SMC grid.  JGLi15Oct2014
      IF( GTYPE .NE. SMCTYPE ) THEN
!
! 7.g Subgrid information
!
      TRFLAG = FLAGTR
      IF ( TRFLAG.GT.6 .OR. TRFLAG.LT.0 ) TRFLAG = 0
!
      IF ( TRFLAG .EQ. 0 ) THEN
        WRITE (NDSO,976) 'Not available.'
      ELSE IF ( TRFLAG.EQ.1 .OR. TRFLAG.EQ.3 .OR. TRFLAG.EQ.5 ) THEN
        WRITE (NDSO,976) 'In between grid points.'
      ELSE
        WRITE (NDSO,976) 'At grid points.'
      END IF
!
      IF ( TRFLAG .NE. 0 ) THEN
!
! 7.g.1 Info from input file
!
        IF (FLGNML) THEN
          NDSTR = NML_OBST%IDF
          VSC = NML_OBST%SF
          IDLA = NML_OBST%IDLA
          IDFT = NML_OBST%IDFM
          RFORM = TRIM(NML_OBST%FORMAT)
          FROM = TRIM(NML_OBST%FROM)
          TNAME = TRIM(NML_OBST%FILENAME)
        ELSE
          CALL NEXTLN ( COMSTR , NDSI , NDSE )
          READ (NDSI,*,END=2001,ERR=2002) NDSTR, VSC, IDLA, IDFT, RFORM, &
                                          FROM, TNAME
        END IF
!
        IF (   ABS(VSC) .LT. 1.E-7  ) VSC    = 1.
        IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
        IF (IDFT.LT.1 .OR. IDFT.GT.3) IDFT   = 1
!
        WRITE (NDSO,977) NDSTR, VSC, IDLA, IDFT
        IF (IDFT.EQ.2) WRITE (NDSO,973) RFORM
        IF (FROM.EQ.'NAME' .AND. NDSG.NE.NDSTR) WRITE (NDSO,974) TNAME
!
! 7.g.2 Open file and check if necessary
!
        IF ( NDSTR .EQ. NDSI ) THEN
          IF ( IDFT .EQ. 3 ) THEN
            WRITE (NDSE,1004) NDSTR
            CALL EXTCDE (23)
          ELSE
            CALL NEXTLN ( COMSTR , NDSI , NDSE )
          END IF
        ELSE IF ( NDSTR .EQ. NDSG ) THEN
          IF ( ( IDFM.EQ.3 .AND. IDFT.NE.3 ) .OR.                 &
               ( IDFM.NE.3 .AND. IDFT.EQ.3 ) ) THEN
            WRITE (NDSE,1005) IDFM, IDFT
            CALL EXTCDE (24)
          END IF
        ELSE
          IF ( IDFT .EQ. 3 ) THEN
            IF (FROM.EQ.'NAME') THEN
              OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                    FORM='UNFORMATTED',STATUS='OLD',ERR=2000, &
                    IOSTAT=IERR)
            ELSE
              OPEN (NDSTR,           FORM='UNFORMATTED',      &
                    STATUS='OLD',ERR=2000,IOSTAT=IERR)
            END IF
          ELSE
            IF (FROM.EQ.'NAME') THEN
              OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                    STATUS='OLD',ERR=2000,IOSTAT=IERR)
            ELSE
              OPEN (NDSTR,                                    &
                    STATUS='OLD',ERR=2000,IOSTAT=IERR)
            END IF
          END IF
        END IF
!
! 7.g.3 Read the data
!
        CALL INA2R ( OBSX, NX, NY, 1, NX, 1, NY, NDSTR, NDST, NDSE, &
                     IDFT, RFORM, IDLA, VSC, 0.0)
!
        IF ( NDSTR .EQ. NDSI ) CALL NEXTLN ( COMSTR , NDSI , NDSE )
!
        CALL INA2R ( OBSY, NX, NY, 1, NX, 1, NY, NDSTR, NDST, NDSE, &
                     IDFT, RFORM, IDLA, VSC, 0.0)
!
! 7.g.4 Limit
!
        DO IX=1, NX
          DO IY=1, NY
            OBSX(IX,IY) = MAX( 0. , MIN(1.,OBSX(IX,IY)) )
            OBSY(IX,IY) = MAX( 0. , MIN(1.,OBSY(IX,IY)) )
          END DO
        END DO
!
        WRITE (NDSO,*)
!
      END IF ! TRFLAG
!
!Li     End of IF( GTYPE .NE. SMCTYPE ) block
      END IF
!
#ifdef W3_RTD
 ! 7.h Calculate rotation angles for configs with rotated pole
       PoLon = PLON
       PoLat = PLAT
       FLAGUNR = UNROT
 ! Default values PLON=-180, PLAT=90, UNROT=.FALSE. for standard lat-lon

       ALLOCATE( AnglDin(NX,NY) )
 ! For standard lat-lon the rotation angles are zero 
       IF ( PoLat == 90. ) THEN
         AnglDin = 0.
       ELSE
         ALLOCATE(StdLat(NX,NY), StdLon(NX,NY))

 !       Calculate rotation angles; (StdLon/Lat are returned, but not used)
 !       The regular grid X/YGRDIN are used as equatorial lon and lat
         CALL W3EQTOLL( YGRDIN, XGRDIN, StdLat, StdLon, AnglDin, &
                      PoLat, PoLon, NX*NY )

 !       Clean up
         DEALLOCATE( StdLat, StdLon )
         END IF
 !     Write out rotation information
       WRITE (NDSO,4203)   PoLat, PoLon
       WRITE (NDSO,4200) 
       WRITE (NDSO,4201)    (        IX,     IX=1,NX,NX/3)
       WRITE (NDSO,4202)    1,(AnglDin(IX, 1), IX=1,NX,NX/3)
       WRITE (NDSO,4202)   NY,(AnglDin(IX,NY), IX=1,NX,NX/3)
       IF ( FLAGUNR ) WRITE (NDSO,4204)
       WRITE (NDSO,*) ' '

#endif
!
#ifdef W3_SMC
 !! 7.i  Read SMC grid cell and face integer arrays.
       IF( GTYPE .EQ. SMCTYPE ) THEN

 !! Overwrite 2 parameters for SMC grid.  JGLi03Mar2021
       DTMS   = DTIMS 
       CTMAX  = CFLSM
#endif
!
#ifdef W3_SMC
       IF (FLGNML) THEN
         NDSTR = NML_SMC%MCELS%IDF
         IDLA = NML_SMC%MCELS%IDLA
         IDFM = NML_SMC%MCELS%IDFM
         RFORM = TRIM(NML_SMC%MCELS%FORMAT)
         TNAME = TRIM(NML_SMC%MCELS%FILENAME)
       ELSE  
         CALL NEXTLN ( COMSTR , NDSI , NDSE )
         READ (NDSI,*,END=2001,ERR=2002) NDSTR, IDLA, IDFM, RFORM, TNAME
       END IF
       OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                   FORM='FORMATTED',STATUS='OLD',ERR=2000)
       ALLOCATE (  NLvCelsk( 0:NRLv ) )
       READ (NDSTR,*) NLvCelsk
       NCel=NLvCelsk(0)
       NGLO=NCel
       WRITE (NDSO,4004)  NCel, NLvCelsk

       ALLOCATE (   IJKCelin( 5, NCel) )
       CALL INA2I ( IJKCelin, 5, NCel, 1, 5, 1, NCel, NDSTR, NDST, NDSE, &
                    IDFM, RFORM, IDLA, 1, 0)
       CLOSE(NDSTR)
 !!Li     Offset to change Equator index = 0 to regular grid index JEQT
       IJKCelin( 2, :) = IJKCelin( 2, :) + JEQT 
 !!Li     Offset to change i-index = 0 to regular grid index ISHFT
       IJKCelin( 1, :) = IJKCelin( 1, :) + ISHFT

       WRITE (NDSO,4005) TNAME
       WRITE (NDSO,4006)    1,(IJKCelin(ix,    1), ix=1,5)
       WRITE (NDSO,4006) NCel,(IJKCelin(ix, NCel), ix=1,5)
       WRITE (NDSO,*) ' '

       IF (FLGNML) THEN
         NDSTR = NML_SMC%ISIDE%IDF
         IDLA = NML_SMC%ISIDE%IDLA
         IDFM = NML_SMC%ISIDE%IDFM
         RFORM = TRIM(NML_SMC%ISIDE%FORMAT)
         TNAME = TRIM(NML_SMC%ISIDE%FILENAME)
       ELSE  
         CALL NEXTLN ( COMSTR , NDSI , NDSE )
         READ (NDSI,*,END=2001,ERR=2002) NDSTR, IDLA, IDFM, RFORM, TNAME
       END IF
       OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                   FORM='FORMATTED',STATUS='OLD',ERR=2000)
       ALLOCATE (  NLvUFcsk( 0:NRLv ) )
       READ (NDSTR,*)  NLvUFcsk
       NUFc = NLvUFcsk(0)
       NGUI = NUFc 
       WRITE (NDSO,4007)   NUFc, NLvUFcsk

       ALLOCATE (   IJKUFcin( 7, NUFc) )
       CALL INA2I ( IJKUFcin, 7, NUFc, 1, 7, 1, NUFc, NDSTR, NDST, NDSE, &
                    IDFM, RFORM, IDLA, 1, 0)
       CLOSE(NDSTR)
 !!Li     Offset to change Equator index = 0 to regular grid index
       IJKUFcin( 2, :) = IJKUFcin( 2, :) + JEQT 
       IJKUFcin( 1, :) = IJKUFcin( 1, :) + ISHFT

       WRITE (NDSO,4008) TNAME
       WRITE (NDSO,4009)    1,(IJKUFcin(ix,    1), ix=1,7)
       WRITE (NDSO,4009) NUFc,(IJKUFcin(ix, NUFc), ix=1,7)
       WRITE (NDSO,*) ' '

       IF (FLGNML) THEN
         NDSTR = NML_SMC%JSIDE%IDF
         IDLA = NML_SMC%JSIDE%IDLA
         IDFM = NML_SMC%JSIDE%IDFM
         RFORM = TRIM(NML_SMC%JSIDE%FORMAT)
         TNAME = TRIM(NML_SMC%JSIDE%FILENAME)
       ELSE  
         CALL NEXTLN ( COMSTR , NDSI , NDSE )
         READ (NDSI,*,END=2001,ERR=2002) NDSTR, IDLA, IDFM, RFORM, TNAME
       END IF
       OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                   FORM='FORMATTED',STATUS='OLD',ERR=2000)
       ALLOCATE (  NLvVFcsk( 0:NRLv ) )
       READ (NDSTR,*) NLvVFcsk
       NVFc= NLvVFcsk(0)
       NGVJ= NVFc
       WRITE (NDSO,4010)   NVFc, NLvVFcsk

       ALLOCATE (   IJKVFcin( 8, NVFc) )
       CALL INA2I ( IJKVFcin, 8, NVFc, 1, 8, 1, NVFc, NDSTR, NDST, NDSE, &
                    IDFM, RFORM, IDLA, 1, 0)
       CLOSE(NDSTR)
 !!Li     Offset to change Equator index = 0 to regular grid index
       IJKVFcin( 2, :) = IJKVFcin( 2, :) + JEQT 
       IJKVFcin( 1, :) = IJKVFcin( 1, :) + ISHFT

       WRITE (NDSO,4011) TNAME
       WRITE (NDSO,4012)    1,(IJKVFcin(ix,    1), ix=1,8)
       WRITE (NDSO,4012) NVFc,(IJKVFcin(ix, NVFc), ix=1,8)
       WRITE (NDSO,*) ' '

 !!Li  Subgrid obstruction for each SMCels.  JGLi15Oct2014
       IF (FLGNML) THEN
         NDSTR = NML_SMC%SUBTR%IDF
         IDLA = NML_SMC%SUBTR%IDLA
         IDFM = NML_SMC%SUBTR%IDFM
         RFORM = TRIM(NML_SMC%SUBTR%FORMAT)
         TNAME = TRIM(NML_SMC%SUBTR%FILENAME)
       ELSE  
         CALL NEXTLN ( COMSTR , NDSI , NDSE )
         READ (NDSI,*,END=2001,ERR=2002) NDSTR, IDLA, IDFM, RFORM, TNAME
       END IF
       OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                   FORM='FORMATTED',STATUS='OLD',ERR=2000)
       READ (NDSTR,*) NCObst, JObs
       WRITE (NDSO,4110)   NCObst, JObs

       ALLOCATE (   IJKObstr( JObs, NCObst) )
       CALL INA2I ( IJKObstr, JObs, NCObst, 1, JObs, 1, NCObst, NDSTR, NDST,  &
                    NDSE, IDFM, RFORM, IDLA, 1, 0)
       CLOSE(NDSTR)

       WRITE (NDSO,4111) TNAME
       WRITE (NDSO,4012)      1, (IJKObstr(ix,      1), ix=1,JObs)
       WRITE (NDSO,4012) NCObst, (IJKObstr(ix, NCObst), ix=1,JObs)
       WRITE (NDSO,*) ' '

 !!Li     Bounary cell sequential numbers are read only if NBISMC>0
       IF( NBISMC .GT. 0 ) THEN
       IF (FLGNML) THEN
         NDSTR = NML_SMC%BUNDY%IDF
         IDLA = NML_SMC%BUNDY%IDLA
         IDFM = NML_SMC%BUNDY%IDFM
         RFORM = TRIM(NML_SMC%BUNDY%FORMAT)
         TNAME = TRIM(NML_SMC%BUNDY%FILENAME)
       ELSE  
         CALL NEXTLN ( COMSTR , NDSI , NDSE )
         READ (NDSI,*,END=2001,ERR=2002) NDSTR, IDLA, IDFM, RFORM, TNAME
       END IF
       OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                   FORM='FORMATTED',STATUS='OLD',ERR=2000)
       ALLOCATE (  NBICelin( NBISMC )  )
       CALL INA2I ( NBICelin, 1, NBISMC, 1, 1, 1, NBISMC, NDSTR, NDST, &
                    NDSE, IDFM, RFORM, IDLA, 1, 0)
       CLOSE(NDSTR)

       WRITE (NDSO,4013) TNAME
       WRITE (NDSO,4014)      1, NBICelin(     1)
       WRITE (NDSO,4014) NBISMC, NBICelin(NBISMC)
       WRITE (NDSO,*) ' '
       ENDIF

#endif
!
#ifdef W3_SMC
 !! 7.j  Read Arctic grid cell and boundary cell integer arrays.  
       IF( ARCTC ) THEN    

       IF (FLGNML) THEN
         NDSTR = NML_SMC%MBARC%IDF
         IDLA = NML_SMC%MBARC%IDLA
         IDFM = NML_SMC%MBARC%IDFM
         RFORM = TRIM(NML_SMC%MBARC%FORMAT)
         TNAME = TRIM(NML_SMC%MBARC%FILENAME)
       ELSE  
         CALL NEXTLN ( COMSTR , NDSI , NDSE )
         READ (NDSI,*,END=2001,ERR=2002) NDSTR, IDLA, IDFM, RFORM, TNAME
       END IF
       OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                   FORM='FORMATTED',STATUS='OLD',ERR=2000)
       READ (NDSTR,*) NARC, NBGL, NBAC
       WRITE (NDSO,4015)  NARC, NBGL, NBAC

       ALLOCATE (   IJKCelAC( 5, NARC) )
       CALL INA2I ( IJKCelAC, 5, NARC, 1, 5, 1, NARC, NDSTR, NDST, NDSE, &
                    IDFM, RFORM, IDLA, 1, 0)
       CLOSE(NDSTR)
 !!Li     Offset to change Equator index = 0 to regular grid index JEQT
       IJKCelAC( 2, :) = IJKCelAC( 2, :) + JEQT 
       IJKCelAC( 1, :) = IJKCelAC( 1, :) + ISHFT

       WRITE (NDSO,4016) TNAME
       WRITE (NDSO,4006)    1,(IJKCelAC(ix,    1), ix=1,5)
       WRITE (NDSO,4006) NARC,(IJKCelAC(ix, NARC), ix=1,5)
       WRITE (NDSO,*) ' '

       IF (FLGNML) THEN
         NDSTR = NML_SMC%AISID%IDF
         IDLA = NML_SMC%AISID%IDLA
         IDFM = NML_SMC%AISID%IDFM
         RFORM = TRIM(NML_SMC%AISID%FORMAT)
         TNAME = TRIM(NML_SMC%AISID%FILENAME)
       ELSE  
         CALL NEXTLN ( COMSTR , NDSI , NDSE )
         READ (NDSI,*,END=2001,ERR=2002) NDSTR, IDLA, IDFM, RFORM, TNAME
       END IF
       OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                   FORM='FORMATTED',STATUS='OLD',ERR=2000)
       READ (NDSTR,*)  NAUI
       WRITE (NDSO,4017)   NAUI

       ALLOCATE (   IJKUFcAC( 7, NAUI) )
       CALL INA2I ( IJKUFcAC, 7, NAUI, 1, 7, 1, NAUI, NDSTR, NDST, NDSE, &
                    IDFM, RFORM, IDLA, 1, 0)
       CLOSE(NDSTR)
 !!Li     Offset to change Equator index = 0 to regular grid index
       IJKUFcAC( 2, :) = IJKUFcAC( 2, :) + JEQT 
       IJKUFcAC( 1, :) = IJKUFcAC( 1, :) + ISHFT
 !!Li     Offset Arctic cell sequential numbers by global cell number NGLO
       DO  IP=1, NAUI
         DO  IX=4,7
       IF( IJKUFcAC(IX,IP) > 0 ) IJKUFcAC(IX,IP) = IJKUFcAC(IX,IP) + NGLO
         ENDDO
       ENDDO

       WRITE (NDSO,4018) TNAME
       WRITE (NDSO,4009)    1,(IJKUFcAC(ix,    1), ix=1,7)
       WRITE (NDSO,4009) NAUI,(IJKUFcAC(ix, NAUI), ix=1,7)
       WRITE (NDSO,*) ' '

       IF (FLGNML) THEN
         NDSTR = NML_SMC%AJSID%IDF
         IDLA = NML_SMC%AJSID%IDLA
         IDFM = NML_SMC%AJSID%IDFM
         RFORM = TRIM(NML_SMC%AJSID%FORMAT)
         TNAME = TRIM(NML_SMC%AJSID%FILENAME)
       ELSE  
         CALL NEXTLN ( COMSTR , NDSI , NDSE )
         READ (NDSI,*,END=2001,ERR=2002) NDSTR, IDLA, IDFM, RFORM, TNAME
       END IF
       OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                   FORM='FORMATTED',STATUS='OLD',ERR=2000)
       READ (NDSTR,*) NAVJ 
       WRITE (NDSO,4019)   NAVJ

       ALLOCATE (   IJKVFcAC( 8, NAVJ) )
       CALL INA2I ( IJKVFcAC, 8, NAVJ, 1, 8, 1, NAVJ, NDSTR, NDST, NDSE, &
                    IDFM, RFORM, IDLA, 1, 0)
       CLOSE(NDSTR)
 !!Li     Offset to change Equator index = 0 to regular grid index
       IJKVFcAC( 2, :) = IJKVFcAC( 2, :) + JEQT 
       IJKVFcAC( 1, :) = IJKVFcAC( 1, :) + ISHFT
 !!Li     Offset Arctic cell sequential numbers by global cell number NGLO
       DO  IP=1, NAVJ
         DO  IY=4,7
       IF( IJKVFcAC(IY,IP) > 0 ) IJKVFcAC(IY,IP) = IJKVFcAC(IY,IP) + NGLO
         ENDDO
       ENDDO

       WRITE (NDSO,4020) TNAME
       WRITE (NDSO,4012)    1,(IJKVFcAC(ix,    1), ix=1,8)
       WRITE (NDSO,4012) NAVJ,(IJKVFcAC(ix, NAVJ), ix=1,8)
       WRITE (NDSO,*) ' '

 !!Li  Reset total cell and face numbers
       NCel = NGLO + NARC
       NUFc = NGUI + NAUI
       NVFc = NGVJ + NAVJ
 !!Li  Also append Arctic part into base level sub-loops
       NLvCelsk(NRLv)=NLvCelsk(NRLv)+NARC 
       NLvUFcsk(NRLv)=NLvUFcsk(NRLv)+NAUI 
       NLvVFcsk(NRLv)=NLvVFcsk(NRLv)+NAVJ 
 !!Li  Reset NBAC to total number of boundary cells.
       NBAC = NBGL + NBAC

       ENDIF  !! ARCTC section.

       ENDIF  !! GTYPE .EQ. SMCTYPE 
#endif
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 8.  Finalize status maps
! 8.a Defines open boundary conditions for UNST grids
!
       J = LEN_TRIM(UGOBCFILE)
       IF (GTYPE.EQ.UNGTYPE.AND.UGOBCFILE(:J).NE.'unset')  &
        CALL READMSHOBC(NDSG,UGOBCFILE,TMPSTA,UGOBCOK)
       IF ((GTYPE.EQ.UNGTYPE).AND.UGOBCAUTO.AND.(.NOT.UGOBCOK))  &
          CALL UG_GETOPENBOUNDARY(TMPSTA,ZBIN,UGOBCDEPTH)
#ifdef W3_DEBUGSTP
          WRITE(740,*) 'Printing ZBIN 4'
          DO IX=1,NX
            WRITE(740,*) 'IX/ZBIN=', IX, ZBIN(IX,1)
          END DO
#endif
!
! 8.b Determine where to get the data
!
      IF (FLGNML) THEN
        NDSTR = NML_MASK%IDF
        IDLA = NML_MASK%IDLA
        IDFT = NML_MASK%IDFM
        RFORM = TRIM(NML_MASK%FORMAT)
        FROM = TRIM(NML_MASK%FROM)
        TNAME = TRIM(NML_MASK%FILENAME)
        IF (TNAME.EQ.'unset' .OR. TNAME.EQ.'UNSET') FROM='PART'
      ELSE  
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=2001,ERR=2002) NDSTR, IDLA, IDFT, RFORM,     &
                                        FROM, TNAME
      END IF
!
! ... Data to be read in parts
!
#ifdef W3_DEBUGGRID
             WRITE(740+IAPROC,*) 'FROM=', TRIM(FROM)
#endif
      IF ( FROM .EQ. 'PART' ) THEN
!
! 8.b Update TMPSTA with input boundary data (ILOOP=1)
!                        and excluded points (ILOOP=2)
!
        IF ( ICLOSE .EQ. ICLOSE_TRPL ) THEN
          WRITE(NDSE,*)'PROGRAM W3GRID STATUS MAP CALCULATION IS '//   &
          'NOT TESTED FOR TRIPOLE GRIDS FOR CASE WHERE USER OPTS '//   &
          'TO READ DATA IN PARTS. STOPPING NOW (107).'
          CALL EXTCDE ( 107 )
        END IF
#ifdef W3_DEBUGGRID
       nbCase1=0
       nbCase2=0
       nbCase3=0
       nbCase4=0
       nbCase5=0
       nbCase6=0
       nbCase7=0
       nbCase8=0
#endif
        DO ILOOP=1, 2
!
          I = 1
          IF ( ILOOP .EQ. 1 ) THEN
            WRITE (NDSO,979) 'boundary points'
            NSTAT  = 2
          ELSE
            WRITE (NDSO,979) 'excluded points'
            NSTAT  = -1
          END IF
          FIRST  = .TRUE.
!
          DO
            IF (FLGNML) THEN
              ! inbound points
              IF (ILOOP.EQ.1) THEN
                IF (NML_INBND_COUNT%N_POINT.GT.0 .AND. I.LE.NML_INBND_COUNT%N_POINT) THEN 
                  IX = NML_INBND_POINT(I)%X_INDEX
                  IY = NML_INBND_POINT(I)%Y_INDEX
                  CONNCT = NML_INBND_POINT(I)%CONNECT
                  I=I+1
                ELSE
                  EXIT
                END IF
              ! excluded points
              ELSE IF (ILOOP.EQ.2) THEN
                IF (NML_EXCL_COUNT%N_POINT.GT.0 .AND. I.LE.NML_EXCL_COUNT%N_POINT) THEN 
                  IX = NML_EXCL_POINT(I)%X_INDEX
                  IY = NML_EXCL_POINT(I)%Y_INDEX
                  CONNCT = NML_EXCL_POINT(I)%CONNECT
                  I=I+1
                ELSE
                  EXIT
                END IF
              END IF
            ELSE
              CALL NEXTLN ( COMSTR , NDSI , NDSE )
              READ (NDSI,*,END=2001,ERR=2002) IX, IY, CONNCT
            END IF
#ifdef W3_DEBUGGRID
  		 WRITE(740+IAPROC,*) 'read IX=', IX
  		 WRITE(740+IAPROC,*) 'read IY=', IY
  		 WRITE(740+IAPROC,*) 'read CONNCT=', CONNCT
#endif

!
! ... Check if last point reached.
!
            IF (IX.EQ.0 .AND. IY.EQ.0) EXIT
!           
! ... Check if point in grid.
!
            IF (GTYPE.EQ.UNGTYPE.AND.(UGOBCAUTO.OR.UGOBCOK)) CYCLE
            IF (IX.LT.1 .OR. IX.GT.NX .OR.  IY.LT.1 .OR. IY.GT.NY) THEN
              WRITE (NDSO,981)
              WRITE (NDSO,*) '       ', IX, IY
              CYCLE
            END IF
!
! ... Check if intermediate points are to be added.
!
#ifdef W3_DEBUGGRID
  		 WRITE(740+IAPROC,*) 'CONNCT=', CONNCT
  		 WRITE(740+IAPROC,*) 'FIRST=', FIRST
#endif
            IF ( CONNCT .AND. .NOT.FIRST ) THEN
                IDX    = IX - IXO
                IDY    = IY - IYO
                IF ( IDX.EQ.0 .OR. IDY.EQ.0 .OR.                      &
                    ABS(IDX).EQ.ABS(IDY) ) THEN
                    NBA    = MAX ( MAX(ABS(IDX),ABS(IDY))-1 , 0 )
                    IF (IDX.NE.0) IDX = SIGN(1,IDX)
                    IF (IDY.NE.0) IDY = SIGN(1,IDY)
                    IX     = IXO
                    IY     = IYO
                    DO IBA=1, NBA
                      IX     = IX + IDX
                      IY     = IY + IDY
                      IF ( TMPSTA(IY,IX).EQ.1 .OR. J.EQ.2 ) THEN
                        TMPSTA(IY,IX) = NSTAT
                      ELSE 
                        WRITE(NDSO,*) 'WARNING: POINT (',IX,',',IY,  &
                                   ') CANNOT BE GIVEN THE STATUS ',NSTAT
                      END IF
                    END DO
                  IX     = IX + IDX
                  IY     = IY + IDY
                ELSE
                  WRITE (NDSO,982)
                  WRITE (NDSO,*) '       ', IX , IY
                  WRITE (NDSO,*) '       ', IXO, IYO
                END IF
              END IF
!
! ... Check if point itself is to be added
!
            IF ( TMPSTA(IY,IX).EQ.1 .OR. J.EQ.2 ) THEN
#ifdef W3_DEBUGGRID
                nbCase2=nbCase2+1
#endif
              TMPSTA(IY,IX) = NSTAT
            END IF
!
! ... Save data of previous point
!
            IXO    = IX
            IYO    = IY
            FIRST  = .FALSE.
!
! ... Branch back to read.
!
          END DO
!
! 8.c Final processing excluded points
!
          IF ( ILOOP .EQ. 2 ) THEN
!
            I = 1
            DO
              IF (FLGNML) THEN
                ! excluded bodies
                IF (NML_EXCL_COUNT%N_BODY.GT.0 .AND. I.LE.NML_EXCL_COUNT%N_BODY) THEN 
                  IX = NML_EXCL_BODY(I)%X_INDEX
                  IY = NML_EXCL_BODY(I)%Y_INDEX
                  I=I+1
                ELSE
                  EXIT
                END IF
              ELSE
                CALL NEXTLN ( COMSTR , NDSI , NDSE )
                READ (NDSI,*,END=2001,ERR=2002) IX, IY
              END IF
!
! ... Check if last point reached.
!
              IF (IX.EQ.0 .AND. IY.EQ.0) EXIT
!
! ... Check if point in grid.
!
              IF (IX.LT.1 .OR. IX.GT.NX .OR. IY.LT.1 .OR. IY.GT.NY) THEN
                WRITE (NDSO,981)
                WRITE (NDSO,*) '       ', IX, IY
                CYCLE
              END IF
!
! ... Check if point already excluded
!
              IF ( TMPSTA(IY,IX) .EQ. NSTAT ) THEN
                WRITE (NDSO,1981)
                WRITE (NDSO,*) '       ', IX, IY
                CYCLE
              END IF
!
! ... Search for points to exclude
!
              TMPMAP = TMPSTA
              J      = 1
              IX1    = IX
              IY1    = IY
!
              JJ     = TMPSTA(IY,IX)
#ifdef W3_DEBUGGRID
             nbCase3=nbCase3 + 1
#endif
              TMPSTA(IY,IX) = NSTAT
              DO
                NBT    = 0
                DO IX=MAX(1,IX1-J), MIN(IX1+J,NX)
                  DO IY=MAX(1,IY1-J), MIN(IY1+J,NY)
                    IF ( TMPSTA(IY,IX) .EQ. JJ ) THEN
                      IF (IX.GT.1) THEN
                        IF (TMPSTA(IY  ,IX-1).EQ.NSTAT           &
                            .AND. TMPMAP(IY  ,IX-1).EQ.JJ ) THEN
#ifdef W3_DEBUGGRID
               nbCase4=nbCase4 + 1
#endif
                          TMPSTA(IY,IX) = NSTAT
                        END IF
                      END IF
                      IF (IX.LT.NX) THEN
                        IF (TMPSTA(IY  ,IX+1).EQ.NSTAT           &
                            .AND. TMPMAP(IY  ,IX+1).EQ.JJ ) THEN
#ifdef W3_DEBUGGRID
               nbCase5=nbCase5 + 1
#endif
                          TMPSTA(IY,IX) = NSTAT
                        END IF
                      END IF
                      IF (IY.LT.NY) THEN
                        IF (TMPSTA(IY+1,IX  ).EQ.NSTAT           &
                            .AND. TMPMAP(IY+1,IX  ).EQ.JJ ) THEN
#ifdef W3_DEBUGGRID
  	          nbCase6=nbCase6 + 1
#endif
                          TMPSTA(IY,IX) = NSTAT
                        END IF
                      END IF
                      IF (IY.GT.1) THEN
                        IF (TMPSTA(IY-1,IX  ).EQ.NSTAT           &
                           .AND. TMPMAP(IY-1,IX  ).EQ.JJ ) THEN
#ifdef W3_DEBUGGRID
               nbCase7=nbCase7 + 1
#endif
                          TMPSTA(IY,IX) = NSTAT
                        END IF
                      END IF
                      IF (TMPSTA(IY,IX).EQ.NSTAT) NBT = NBT + 1
                    END IF
                  END DO
                END DO
!
                IF ( NBT .NE. 0 ) THEN
                  J = J + 1
                ELSE
                  EXIT
                END IF
              END DO
            END DO
!
! ... Outer boundary excluded points
!
            IF ( GTYPE.NE.UNGTYPE ) THEN    

              DO IX=1, NX
                IF ( TMPSTA( 1,IX) .EQ. 1 ) TMPSTA( 1,IX) = NSTAT
                IF ( TMPSTA(NY,IX) .EQ. 1 ) TMPSTA(NY,IX) = NSTAT
              END DO
!
              IF ( ICLOSE.EQ.ICLOSE_NONE ) THEN
                DO IY=2, NY-1
                  IF ( TMPSTA(IY, 1) .EQ. 1 ) TMPSTA(IY, 1) = NSTAT
                  IF ( TMPSTA(IY,NX) .EQ. 1 ) TMPSTA(IY,NX) = NSTAT
                END DO
              END IF

            END IF ! GTYPE
!
          END IF ! ILOOP .EQ. 2
!
! ... Branch back input / excluded points ( ILOOP in 8.b )
!
        END DO
#ifdef W3_DEBUGGRID
      WRITE(740+IAPROC,*) 'nbCase1=', nbCase1
      WRITE(740+IAPROC,*) 'nbCase2=', nbCase2
      WRITE(740+IAPROC,*) 'nbCase3=', nbCase3
      WRITE(740+IAPROC,*) 'nbCase4=', nbCase4
      WRITE(740+IAPROC,*) 'nbCase5=', nbCase5
      WRITE(740+IAPROC,*) 'nbCase6=', nbCase6
      WRITE(740+IAPROC,*) 'nbCase7=', nbCase7
      WRITE(740+IAPROC,*) 'nbCase8=', nbCase8
      nbTMPSTA0=0
      nbTMPSTA1=0
      nbTMPSTA2=0
      DO IX=1,NX
        DO IY=1,NY
          WRITE(740+IAPROC,*) 'IX/IY/TMPSTA=', IX, IY, TMPSTA(IY,IX)
          IF (TMPSTA(IY,IX) .eq. 0) nbTMPSTA0=nbTMPSTA0+1
          IF (TMPSTA(IY,IX) .eq. 1) nbTMPSTA1=nbTMPSTA1+1
          IF (TMPSTA(IY,IX) .eq. 2) nbTMPSTA2=nbTMPSTA2+1
        END DO
      END DO
      WRITE(740+IAPROC,*) 'nbTMPSTA0=', nbTMPSTA0
      WRITE(740+IAPROC,*) 'nbTMPSTA1=', nbTMPSTA1
      WRITE(740+IAPROC,*) 'nbTMPSTA2=', nbTMPSTA2
      FLUSH(740+IAPROC)
#endif
!
        ELSE ! FROM .EQ. PART
!
! 8.d Read the map from file instead
!
          NSTAT  = -1
          IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
          IF (IDFT.LT.1 .OR. IDFT.GT.3) IDFT   = 1

!!Li  Suspended for SMC grid though the file input line in  ww3_grid.inp 
!!Li  is kept to divert the program into this block.  JGLi15Oct2014 
!!Li
          IF( GTYPE .NE. SMCTYPE ) THEN
!!Li
!
          WRITE (NDSO,978) NDSTR, IDLA, IDFT
          IF (IDFT.EQ.2) WRITE (NDSO,973) RFORM
          IF (FROM.EQ.'NAME') WRITE (NDSO,974) TNAME
!
          IF ( NDSTR .EQ. NDSI ) THEN
              IF ( IDFT .EQ. 3 ) THEN
                  WRITE (NDSE,1004) NDSTR
                  CALL EXTCDE (23)
                ELSE
                  CALL NEXTLN ( COMSTR , NDSI , NDSE )
                END IF
            ELSE
              IF ( IDFT .EQ. 3 ) THEN
                  IF (FROM.EQ.'NAME') THEN
                      OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                            FORM='UNFORMATTED',STATUS='OLD',ERR=2000, &
                            IOSTAT=IERR)
                    ELSE
                      OPEN (NDSTR,           FORM='UNFORMATTED',      &
                            STATUS='OLD',ERR=2000,IOSTAT=IERR)
                    END IF
                ELSE
                  IF (FROM.EQ.'NAME') THEN
                      OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                            STATUS='OLD',ERR=2000,IOSTAT=IERR)
                    ELSE
                      OPEN (NDSTR,                                    &
                            STATUS='OLD',ERR=2000,IOSTAT=IERR)
                    END IF
                END IF
            END IF
!
          ALLOCATE ( READMP(NX,NY) )
          CALL INA2I ( READMP, NX, NY, 1, NX, 1, NY, NDSTR, NDST,    &
                       NDSE, IDFT, RFORM, IDLA, 1, 0 )
!
          IF ( ICLOSE.EQ.ICLOSE_NONE ) THEN
              DO IY=2, NY-1
                IF ( READMP( 1,IY) .EQ. 1 ) READMP( 1,IY) = 3
                IF ( READMP(NX,IY) .EQ. 1 ) READMP(NX,IY) = 3
                END DO
            END IF
!
          DO IX=1, NX
            IF ( READMP(IX, 1) .EQ. 1 ) READMP(IX, 1) = 3
            IF ( READMP(IX,NY) .EQ. 1 .AND. ICLOSE .NE. ICLOSE_TRPL)   &
               READMP(IX,NY) = 3
            END DO
!
          DO IY=1, NY
            DO IX=1, NX
              IF ( READMP(IX,IY) .EQ. 3 ) THEN
                  TMPSTA(IY,IX) = NSTAT
                ELSE
                  TMPSTA(IY,IX) = READMP(IX,IY)
                  ! force to dry the sea points over zlim
                  IF ( ZBIN(IX,IY) .GT. ZLIM ) TMPSTA(IY,IX) = 0
                END IF
              END DO
            END DO
          DEALLOCATE ( READMP )
!!Li
          ENDIF   !! GTYPE .NE. SMCTYPE
!
        END IF !FROM .NE. 'PART'
!
! 8.e Get NSEA and other counters
!
      NSEA   = 0
      NLAND  = 0
      NBI    = 0
      NBT    = 0
!
      DO IX=1, NX
        DO IY=1, NY
          IF ( TMPSTA(IY,IX) .GT. 0 ) NSEA   = NSEA + 1
          IF ( TMPSTA(IY,IX) .EQ. 0 ) NLAND  = NLAND + 1
          IF ( TMPSTA(IY,IX) .LT. 0 ) NBT    = NBT + 1
          IF ( TMPSTA(IY,IX) .EQ. 2 ) NBI    = NBI + 1
          END DO
        END DO
!
#ifdef W3_SMC
      IF( GTYPE .EQ. SMCTYPE ) THEN
 !Li   Moved before FLBPI is defined with NBI value.  JGLi05Jun2015
 !Li   Overwrite NSEA with NCel for SMC grid. 
        NSEA = NCel
 !Li   Use input NBI number for SMC grid because merged
 !Li   cells are over-counted by model.   
        NBI = NBISMC
 !Li   No land points are used in SMC grid.   JGLi26Feb2016 
        NLAND = 0
      ENDIF  !!  GTYPE .EQ. SMCTYPE 
#endif
!
      WRITE (NDSO,980)
      FLBPI  = NBI .GT. 0
      IF ( .NOT. FLBPI ) THEN
          WRITE (NDSO,985)
        ELSE
          WRITE (NDSO,986) NBI
#ifdef W3_O1
          IF ( FLAGLL ) THEN
              WRITE (NDSO, 987)
            ELSE
              WRITE (NDSO,1987)
            END IF
          IBI    = 1
          DO IY=1, NY
            DO IX=1, NX
              IF (GTYPE.NE.UNGTYPE) THEN
                X = FACTOR * ( XGRDIN(IX,IY) )
                Y = FACTOR * ( YGRDIN(IX,IY) )
              ELSE
                X = FACTOR * XYB(IX,1)
                Y = FACTOR * XYB(IX,2)
                END IF
            IF ( TMPSTA(IY,IX).EQ.2 ) THEN
                  IF ( FLAGLL ) THEN
                      WRITE (NDSO, 988) IBI, IX, IY, X, Y
                    ELSE
                      WRITE (NDSO,1988) IBI, IX, IY, X, Y
                    END IF
                  IBI    = IBI + 1
                END IF
              END DO
            END DO
#endif
        END IF
!
      WRITE (NDSO,1980)
      IF ( NBT .EQ. 0 ) THEN
          WRITE (NDSO,1985)
        ELSE
          WRITE (NDSO,1986) NBT
        END IF
!
! 8.f Set up all maps
!
        CALL W3DIMX ( 1, NX, NY, NSEA, NDSE, NDST  &
#ifdef W3_SMC
             , NCel, NUFc, NVFc, NRLv, NBSMC  &
             , NARC, NBAC, NSPEC              &
#endif
                  )
#ifdef W3_SMC
        WRITE (NDSO,4021)   NCel
#endif
!
! 8.g Activation of reflections and scattering 
      FFACBERG=FACBERG
#ifdef W3_REF1
      REFPARS(1)=REFCOAST
      REFPARS(2)=REFSUBGRID
      REFPARS(3)=REFUNSTSOURCE
      REFPARS(4)=REFICEBERG
      REFPARS(6)=REFFREQ
      REFPARS(7)=REFSLOPE
      REFPARS(8)=REFCOSP_STRAIGHT
      REFPARS(9)=REFRMAX
      REFPARS(10)=REFFREQPOW
      IF (GTYPE.EQ.UNGTYPE) REFPARS(2:5)=0.
      IF (REFMAP.EQ.0) THEN 
        REFLC(3,:)=REFPARS(7)
        END IF
#endif


      IF (GTYPE.NE.UNGTYPE) THEN
        DO IY=1, NY
          DO IX=1, NX
            XGRD(IY,IX) = XGRDIN(IX,IY)
            YGRD(IY,IX) = YGRDIN(IX,IY)
            END DO
          END DO
          DEALLOCATE ( XGRDIN, YGRDIN )   
          CALL W3GNTX ( 1, 6, 6 )  
      ELSE 
!
!FA:  This distinction  between structured and unstructured
! should be removed when XYB is replaced by XGRD and YGRD
!
        DO IX=1, NX
          XGRD(:,IX) = XYB(IX,1)
          YGRD(:,IX) = XYB(IX,2)
          END DO
        END IF   ! GTYPE
!      
#ifdef W3_SMC
 !!Li  Shelter MAPSTA LLG definition for SMC 
      IF( GTYPE .NE. SMCTYPE ) THEN
#endif
!
      MAPSTA = TMPSTA
      MAPFS  = 0
!
#ifdef W3_T
      ALLOCATE ( MAPOUT(NX,NY) )
      MAPOUT = 0
#endif
!
#ifdef W3_T
      IX3    = 1 + NX/60
      IY3    = 1 + NY/60
      CALL PRTBLK (NDST, NX, NY, NX, ZBIN, MAPOUT, 1, 0.,          &
                   1, NX, IX3, 1, NY, IY3, 'Zb', 'm')
#endif
!
#ifdef W3_DEBUGSTP
          WRITE(740,*) 'Printing ZBIN 5'
          DO IX=1,NX
            WRITE(740,*) 'IX/ZBIN=', IX, ZBIN(IX,1)
          END DO
#endif
      TRNX   = 0.
      TRNY   = 0.
!
      ISEA   = 0
      DO IY=1, NY
        DO IX=1, NX
          IF ( TMPSTA(IY,IX) .EQ. NSTAT ) THEN
              MAPSTA(IY,IX) = 0
              MAPST2(IY,IX) = 1
              TMPSTA(IY,IX) = 3
            ELSE
              MAPSTA(IY,IX) = TMPSTA(IY,IX)
              MAPST2(IY,IX) = 0
            END IF
          IF ( MAPSTA(IY,IX) .NE. 0 ) THEN
              ISEA           = ISEA + 1
              MAPFS (IY,IX)  = ISEA
              ZB(ISEA)       = ZBIN(IX,IY)
#ifdef W3_T
              MAPOUT(IX,IY)  = 1
#endif
              MAPSF(ISEA,1)  = IX
              MAPSF(ISEA,2)  = IY
              IF ( FLAGLL ) THEN
                  Y              = YGRD(IY,IX)
                  CLATS(ISEA)    = COS(Y*DERA)
                  CLATIS(ISEA)   = 1. / CLATS(ISEA)
                  CTHG0S(ISEA)   = - TAN(DERA*Y) / RADIUS
                ELSE
                  CLATS(ISEA)    = 1.
                  CLATIS(ISEA)   = 1.
                  CTHG0S(ISEA)   = 0.
                END IF
            END IF

!/ ------------------------------------------------------------------- /

! notes: Oct 22 2012: I moved the following "if-then" statement from 
! inside the  "IF ( MAPSTA(IY,IX) .NE. 0 )" statement to outside that
! statement. This is needed since later on, ATRNX is computed from 
! TRNX(ix-1) , TRNX(ix) etc. which causes boundary effects if the
! MAPSTA=0 values are set to TRNX=0

              IF ( TRFLAG .NE. 0 ) THEN
                  TRNX(IY,IX) = 1. - OBSX(IX,IY)
                  TRNY(IY,IX) = 1. - OBSY(IX,IY)
                END IF

          END DO
        END DO
#ifdef W3_DEBUGSTP
        DO ISEA=1,NSEA
          WRITE(740,*) 'ISEA,ZB=', ISEA, ZB(ISEA)
        END DO
        FLUSH(740)
#endif
!
#ifdef W3_SMC
 !!Li SMC grid definition of mapping arrays.
      ELSE 
#endif
!
#ifdef W3_SMC
 !!Li  Pass refined level cell and face counts to NLv*(NRLv)
       NLvCel(0)=0
       NLvUFc(0)=0
       NLvVFc(0)=0
       DO IP = 1, NRLv
          NLvCel(IP)=NLvCelsk(IP) + NLvCel(IP-1)
          NLvUFc(IP)=NLvUFcsk(IP) + NLvUFc(IP-1)
          NLvVFc(IP)=NLvVFcsk(IP) + NLvVFc(IP-1)
       ENDDO
       WRITE (NDSO,4022)   NLvCel
       WRITE (NDSO,4023)   NLvUFc
       WRITE (NDSO,4024)   NLvVFc

 !Li     Redefine MAPSF MAPFS MAPSTA MAPST2 CLATS and ZB for SMC Grid,
 !Li     using SMC grid cell array and assuming NSEA=NCel.
       MAPSTA = 0
       MAPST2 = 1
       MAPFS  = 0

 !Li     Pass input SMC arrays to newly declared grid arrays.
       WRITE (NDSO,4025)   NCel
       IJKCel(:, 1:NGLO)=IJKCelin(:, 1:NGLO)
       IJKUFc(:, 1:NGUI)=IJKUFcin(:, 1:NGUI)
       IJKVFc(:, 1:NGVJ)=IJKVFcin(:, 1:NGVJ)
 !Li     Append Arctic part
       IF( ARCTC ) THEN
       IJKCel(:, NGLO+1:NCel)=IJKCelAC(:, 1:NARC)
       IJKUFc(:, NGUI+1:NUFc)=IJKUFcAC(:, 1:NAUI)
       IJKVFc(:, NGVJ+1:NVFc)=IJKVFcAC(:, 1:NAVJ)
       ENDIF !! ARCTC

       WRITE (NDSO,4026) 
       WRITE (NDSO,4006)    1,(IJKCel(ix,  1), ix=1,5)
       JJ=NCel
       WRITE (NDSO,4006)   JJ,(IJKCel(ix, JJ), ix=1,5)
       WRITE (NDSO,*) ' '
       WRITE (NDSO,4027) 
       WRITE (NDSO,4009)    1,(IJKUFc(ix,  1), ix=1,7)
       JJ=NUFc
       WRITE (NDSO,4009)   JJ,(IJKUFc(ix, JJ), ix=1,7)
       WRITE (NDSO,*) ' '
       WRITE (NDSO,4028) 
       WRITE (NDSO,4012)    1,(IJKVFc(ix,  1), ix=1,8)
       JJ=NVFc
       WRITE (NDSO,4012)   JJ,(IJKVFc(ix, JJ), ix=1,8)
       WRITE (NDSO,*) ' '

 !Li    Boundary -9 to 0 cells for cell x-size 2**n
 !Li    Note the position indice for bounary cell are not used.
       IJKCel(1, -9:0)=0
 !Li    Use Equator Y index for boundary cells.  JGLi04Apr2011 
 !Li   IJKCel(2, -9:0)=0  
       IJKCel(2, -9:0)=JEQT
       IJKCel(3,    0)=1
       IJKCel(4,    0)=1
 !Li    Use minimum 10 m depth for boundary cells. 
 !Li    Y-size is restricted below base-cell value. 
 !Li    For refined boundary cells, its y-size is replaced with 
 !Li    the inner cell y-size for flux gradient. 
       IJKCel(5,    0)=10
       DO ip=1,9
          IJKCel(3,-ip)=IJKCel(3,-ip+1)*2
          IK=MIN(ip, NRLv-1) 
          IJKCel(4,-ip)=2**IK 
          IJKCel(5,-ip)=10
       ENDDO
       WRITE (NDSO,4029) 
       DO ip=0, -9, -1
       WRITE (NDSO,4030)  IJKCel(:,ip)
       ENDDO

       WRITE (NDSO,4031)   NCel
 !Li    Multi-resolution SMC grid requires rounding of x, y indices
 !Li    by a factor MRFct. 
       MRFct = 2**(NRLv - 1)
       WRITE (NDSO,4032)   MRFct

 !Li   Cosine for SMC uses refined latitude increment.
       SYMR = SY*DERA/FLOAT( MRFct )
 !Li   Reference y point for adjusted cell j=0 in radian.  JGLi16Feb2016
       YJ0R = ( Y0 - 0.5*SY )*DERA

       DO ISEA=1, NCel
 !Li   There is no polar cell row so it is mapped to last row.
        IF( ARCTC .AND. (ISEA .EQ. NCel) ) THEN 
             IX=1
             IY=NY
             IK=1
             JS=1
        ELSE
          IX=IJKCel(1,ISEA)/MRFct + 1
          IY=IJKCel(2,ISEA)/MRFct + 1
          IK=MAX(1, IJKCel(3,ISEA)/MRFct)
          JS=MAX(1, IJKCel(4,ISEA)/MRFct)
        ENDIF

 !      Check that IX, IY are in the bound of [1,NX] and [1,NY] respec.
        IF ((IX+IK-1 .GT. NX) .OR. (IX .LE. 0)) THEN
             WRITE (NDSE,1014) ISEA, IX, IX+IK-1, NX
             CALL EXTCDE(65)
        END IF
 
        IF ((IY+JS-1 .GT. NY) .OR. (IY .LE. 0)) THEN
             WRITE (NDSE,1015) ISEA, IY, IY+JS-1, NY
             CALL EXTCDE(65)
        END IF

 !Li  Minimum DMIN depth is used as well for SMC. 
          ZB(ISEA)= - MAX( DMIN, FLOAT( IJKCel(5, ISEA) ) )
          MAPFS(IY:IY+JS-1,IX:IX+IK-1)  = ISEA
         MAPSTA(IY:IY+JS-1,IX:IX+IK-1)  = 1
         MAPST2(IY:IY+JS-1,IX:IX+IK-1)  = 0
          MAPSF(ISEA,1)  = IX
          MAPSF(ISEA,2)  = IY
          MAPSF(ISEA,3)  = IY + (IX    -1)*NY
 
 !Li   New variable CLATS to hold cosine latitude at cell centre.
 !Li   Also added CLATIS and CTHG0S for version 4.08.
 !Li   Use adjusted j-index to calculate cell centre y from YJ0R.  
          Y = YJ0R + SYMR*( FLOAT(IJKCel(2,ISEA))+0.5*FLOAT(IJKCel(4,ISEA)) )
 !Li   Arctic polar cell does not need COS(LAT), set 1 row down.
          IF(Y .GE. HPI-0.1*SYMR) Y=HPI - SYMR*0.5*FLOAT( MRFct )
 
          CLATS(ISEA) = COS( Y )
          CLATIS(ISEA)= 1. / CLATS(ISEA)
          CTHG0S(ISEA)= - TAN( Y ) / RADIUS
 !!Li  Sub-grid obstruction is set zero beyond NCObst cells.
         IF(ISEA .GT. NCObst) THEN
           TRNMX=1.0 
           TRNMY=1.0
         ELSE 
 !!Li    Present obstruction is isotropic and in percentage.
           TRNMX=1.0 - IJKObstr(1,    ISEA)*0.01
           TRNMY=1.0 - IJKObstr(JObs, ISEA)*0.01
         ENDIF
           CTRNX(ISEA) = MAX(0.11, TRNMX)
           CTRNY(ISEA) = MAX(0.11, TRNMY)
       END DO
 !!Li    Transparency for boundary cells are 1.0   JGLi16Jan2012
           CTRNX(-9:0) = 1.0 
           CTRNY(-9:0) = 1.0 
 !!Li  Check range of MAPSF and MAPFS
       WRITE (NDSO,4033) MINVAL( MAPSF(:,1) ), MAXVAL( MAPSF(:,1) )
       WRITE (NDSO,4034) MINVAL( MAPSF(:,2) ), MAXVAL( MAPSF(:,2) )
       WRITE (NDSO,4035) MINVAL( MAPSF(:,3) ), MAXVAL( MAPSF(:,3) )
       WRITE (NDSO,4036) MINVAL( MAPFS(:,:) ), MAXVAL( MAPFS(:,:) )
 
 !Li   New variable CLATF to hold cosine latitude at cell V face.
       DO IP = 1, NVFC
 !        CLATF(IP) = COS( SYMR*FLOAT(IJKVFc(2,IP) - JEQT) )
 !Li   Use adjusted j-index to calculate cell face Y from YJ0R.
          CLATF(IP) = COS( SYMR*FLOAT(IJKVFc(2,IP)) + YJ0R )
       ENDDO
       IF(NBISMC .GT. 0) THEN
 !Li   Save input boundary SMC list to ISMCBP(NBSMC)
          ISMCBP(1:NBISMC) = NBICelin(1:NBISMC)
 !Li   Reset MAPSTA for boundary cells if any.  
       DO IP=1, NBISMC
          ISEA = NBICelin(IP)
          IX=IJKCel(1,ISEA)/MRFct + 1
          IY=IJKCel(2,ISEA)/MRFct + 1
          IK=MAX(1, IJKCel(3,ISEA)/MRFct)
          JS=MAX(1, IJKCel(4,ISEA)/MRFct)
          MAPSTA(IY:IY+JS-1,IX:IX+IK-1)  = 2
          MAPST2(IY:IY+JS-1,IX:IX+IK-1)  = 0
       ENDDO
       ENDIF
 
#endif
!
#ifdef W3_SMC
 !Li   Define rotation angle for Arctic cells.
       IF( ARCTC ) THEN
 
       PoLonAC = 179.999
       PoLatAC =   0.001
       ALLOCATE( XLONAC(NARC),YLATAC(NARC),ELONAC(NARC),ELATAC(NARC) )
       DO ISEA=NGLO+1, NCel
 !Li   There is no polar cell row so it is mapped to last row.
          IF(ISEA .EQ. NCel) THEN
             IX=1
             IY=NY
             IK=1
             JS=1
          ELSE
             IX=IJKCel(1,ISEA)/MRFct + 1
             IY=IJKCel(2,ISEA)/MRFct + 1
             IK=MAX(1, IJKCel(3,ISEA)/MRFct)
             JS=MAX(1, IJKCel(4,ISEA)/MRFct)
          ENDIF
          XLONAC(ISEA-NGLO)= X0 + REAL(IX-1+IK/2)*SX
          YLATAC(ISEA-NGLO)= Y0 + REAL(IY-1+JS/2)*SY
       ENDDO 

        CALL W3LLTOEQ ( YLATAC, XLONAC, ELATAC, ELONAC,   &
      &                 ANGARC, PoLatAC, PoLonAC, NARC  )

       WRITE (NDSO,4037)  NARC 
       WRITE (NDSO,4038) (ANGARC(ix), ix=1,NARC,NARC/8)

#endif
!
#ifdef W3_SMC
 !Li   Mapping Arctic boundary cells with inner model cells
       DO IP=1, NBAC 
             IX=IJKCel(1,IP+NGLO)
             IY=IJKCel(2,IP+NGLO)
          DO ISEA=1, NGLO 
             IF( (IX .EQ. IJKCel(1,ISEA)) .AND.      &
      &          (IY .EQ. IJKCel(2,ISEA)) ) THEN 
                 ICLBAC(IP) = ISEA
             ENDIF
          ENDDO
       ENDDO
       WRITE (NDSO,4039)  NBAC 
       WRITE (NDSO,4040) (ICLBAC(ix), ix=1,NBAC,NBAC/8)

 !Li   Redefine GCT term factor for Arctic part or the netative of 
 !Li   tangient of rotated latitude divided by radius. JGLi14Sep2015
       DO ISEA=NGLO+1, NCel-1
          CTHG0S(ISEA)= - TAN( ELATAC(ISEA-NGLO)*DERA ) / RADIUS
       ENDDO 
          CTHG0S(NCel)=0.0
       
       ENDIF  !! ARCTC section.
#endif
!
#ifdef W3_SMC
      ENDIF  !! (GTYPE .NE. SMCTYPE) ELSE SMCTYPE block. 
#endif
!
#ifdef W3_RTD
 !Li   Assign rotated grid angle for all sea points.  JGLi01Feb2016
       DO ISEA=1,NSEA
          IX = MAPSF(ISEA,1)
          IY = MAPSF(ISEA,2)
          AnglD(ISEA) = AnglDin(IX,IY)
       END DO
#endif
!
#ifdef W3_T
      CALL PRTBLK (NDST, NX, NY, NX, ZBIN, MAPOUT, 0, 0.,          &
                   1, NX, IX3, 1, NY, IY3, 'Sea points', 'm')
      DEALLOCATE ( MAPOUT )
#endif
!
      DO ISP=1, NSPEC+NTH
        MAPWN(ISP) = 1 + (ISP-1)/NTH
        MAPTH(ISP) = 1 + MOD(ISP-1,NTH)
        END DO
!
#ifdef W3_O2
      NMAP   = 1 + (NX-1)/NCOL
      WRITE (NDSO,1100) NMAP
      DO IMAP=1, NMAP
        IX0    = 1 + (IMAP-1)*NCOL
        IXN    = MIN ( NX , IMAP*NCOL )
        DO IY=NY,1,-1
          WRITE (NDSO,1101) (TMPSTA(IY,IX),IX=IX0,IXN)
          END DO
        WRITE (NDSO,*) ' '
        END DO
      WRITE (NDSO,1102)
#endif

#ifdef W3_O2a
      OPEN (NDSM,FILE=TRIM(FNMPRE)//'mask.ww3')
      DO IY=1, NY
        WRITE (NDSM,998) MIN(1,MAPSTA(IY,:))
        END DO
      CLOSE (NDSM)
#endif
!
#ifdef W3_O2b
      IF ( TRFLAG .GT. 0 ) THEN
          NMAPB  = 1 + (NX-1)/NCOL
          WRITE (NDSO,1103) 'X', NMAPB
          DO IMAPB=1, NMAPB
            IX0    = 1 + (IMAPB-1)*NCOL
            IXN    = MIN ( NX , IMAPB*NCOL )
            DO IY=NY,1,-1
              WRITE (NDSO,1101) (NINT(10.*OBSX(IX,IY)),IX=IX0,IXN)
              END DO
            WRITE (NDSO,*) ' '
            END DO
          WRITE (NDSO,1104)
          WRITE (NDSO,1103) 'Y', NMAPB
          DO IMAPB=1, NMAPB
            IX0    = 1 + (IMAPB-1)*NCOL
            IXN    = MIN ( NX , IMAPB*NCOL )
            DO IY=NY,1,-1
              WRITE (NDSO,1101) (NINT(10.*OBSY(IX,IY)),IX=IX0,IXN)
              END DO
            WRITE (NDSO,*) ' '
            END DO
          WRITE (NDSO,1104)
        END IF
#endif
!
#ifdef W3_O2c
      OPEN (NDSM,FILE=TRIM(FNMPRE)//'mapsta.ww3', RECL=2*NX*NY*50+1)
      DO IY=NY,1, -1
        DO IX=1,NX
          DO I=1,50
            WRITE (NDSM,1998,ADVANCE='NO') (TMPSTA(IY,IX))
           END DO
         END DO
        END DO
      CLOSE (NDSM)
#endif
!

#ifdef W3_IG1
       IGPARS(1)=IGMETHOD
       IGPARS(2)=IGADDOUTP
       IGPARS(3)=IGSOURCE
       IGPARS(4)=0
       IF (IGBCOVERWRITE) IGPARS(4)=IGPARS(4)+1
       IF (IGSWELLMAX) IGPARS(4)=IGPARS(4)+2
       IGPARS(5)=1
       DO IK=1,NK
         IF (SIG(IK)*TPIINV.LT.IGMAXFREQ) IGPARS(5)=IK
         END DO
       IGMINDEP=MINVAL(ZB*(-1.)-2)  ! -2 / +2 is there for water level changes
       IGMAXDEP=MAXVAL(ZB*(-1.)+2)
       IF (IGSOURCEATBP.EQ.1)  IGMINDEP=1.   ! should use true minimum depth ... 
       IGPARS(6)=1+NINT(LOG(MAX(IGMAXDEP,1.0)/MAX(IGMINDEP,1.0))/LOG(1.1))
       IGPARS(7)=MAX(IGMINDEP,1.0)
       IGPARS(8)=IGSOURCEATBP
       IGPARS(9)=IGKDMIN
       IGPARS(10)=IGFIXEDDEPTH
       IGPARS(11)=IGEMPIRICAL**2
       IGPARS(12)=IGSTERMS
#endif
!
#ifdef W3_IC2
       IC2PARS(:)=0.
       IF (IC2DISPER) IC2PARS(1)=1.
       IC2PARS(2)=IC2TURB
       IC2PARS(3)=IC2ROUGH
       IC2PARS(4)=IC2REYNOLDS
       IC2PARS(5)=IC2SMOOTH
       IC2PARS(6)=IC2VISC
       IC2PARS(7)=IC2TURBS
       IC2PARS(8)=IC2DMAX
#endif
!
#ifdef W3_IC3
       IC3PARS(:)=0.
       IC3PARS(1)=IC3MAXTHK
       IC3PARS(2)=IC2TURB
       IC3PARS(3)=IC2ROUGH
       IC3PARS(4)=IC2REYNOLDS
       IC3PARS(5)=IC2SMOOTH
       IC3PARS(6)=IC2VISC
       IC3PARS(7)=IC2TURBS
       IC3PARS(8)=IC3MAXCNC
       IF (IC3CHENG) IC3PARS(9)=1.0
       IC3PARS(10)=IC3HILIM
       IC3PARS(11)=IC3KILIM
       IF (USECGICE) IC3PARS(12)=1.0
       IC3PARS(13)=IC3HICE
       IC3PARS(14)=IC3VISC
       IC3PARS(15)=IC3DENS
       IC3PARS(16)=IC3ELAS
#endif
!
#ifdef W3_IC4
      IC4PARS(1)=IC4METHOD
      IC4_KI=IC4KI
      IC4_FC=IC4FC
#endif
!
#ifdef W3_IC5
       IC5PARS(:)=0.
       IC5PARS(1)=IC5MINIG
       IC5PARS(2)=IC5MINWT
       IC5PARS(3)=IC5MAXKRATIO
       IC5PARS(4)=IC5MAXKI
       IC5PARS(5)=IC5MINHW
       IC5PARS(6)=IC5MAXITER
       IC5PARS(7)=IC5RKICK
       IC5PARS(8)=IC5KFILTER
       IC5PARS(9)=IC5VEMOD
#endif
!
#ifdef W3_IS2
      IS2PARS(1) = ISC1
      IS2PARS(2) = IS2BACKSCAT
      IS2PARS(3)=0.
      IF (IS2BREAK) IS2PARS(3)=1. 
      IS2PARS(4)=IS2C2
      IS2PARS(5)=IS2C3
      IS2PARS(6)=0.
      IF (IS2DISP) IS2PARS(6)=1.
      IS2PARS(7)=IS2DAMP
      IS2PARS(8)=IS2FRAGILITY
      IS2PARS(9)=IS2DMIN 
      IS2PARS(10)=0.
      IF (IS2DUPDATE) IS2PARS(10)=1.
      IS2PARS(11)=IS2CONC
      IS2PARS(12)=ABS(IS2CREEPB)
      IS2PARS(13)=IS2CREEPC
      IS2PARS(14)=IS2CREEPD
      IS2PARS(15)=IS2CREEPN
      IS2PARS(16)=IS2BREAKE
      IS2PARS(17)=IS2BREAKF
      IS2PARS(18)=IS2WIM1
      IS2PARS(19)=IS2FLEXSTR
      IS2PARS(20)=0.
      IF (IS2ISOSCAT) IS2PARS(20)=1.
      IS2PARS(21)=IS2ANDISD
      IS2PARS(22)=IS2ANDISN
      IS2PARS(23)=0.
      IF (IS2ANDISB) IS2PARS(23)=1.
      IS2PARS(24)=IS2ANDISE
#endif
!
! 9.d Estimates shoreline direction for reflection
!     and shoreline treatment in general for UNST grids. 
! NB: this is updated with moving water levels in W3ULEV
! AR: this is not anymore needed and will be deleted ...
!
      IF (GTYPE.EQ.UNGTYPE) THEN 
        CALL SETUGIOBP 
#ifdef W3_REF1
      ELSE
        CALL W3SETREF
#endif
        END IF
#ifdef W3_REF1
! 
!  9.a Reads shoreline slope  (whith REF1 switch only)
!
      ALLOCATE ( REFD(NX,NY), REFD2(NX,NY), REFS(NX,NY) )
      IF (REFMAP.EQ.0) THEN 
        REFS(:,:)=1.
      ELSE 
!
!  9.b Info from input file
!
        IF (FLGNML) THEN
          NDSTR = NML_SLOPE%IDF
          VSC = NML_SLOPE%SF
          IDLA = NML_SLOPE%IDLA
          IDFT = NML_SLOPE%IDFM
          RFORM = TRIM(NML_SLOPE%FORMAT)
          FROM = TRIM(NML_SLOPE%FROM)
          TNAME = TRIM(NML_SLOPE%FILENAME)
        ELSE
          CALL NEXTLN ( COMSTR , NDSI , NDSE )
          READ (NDSI,*,END=2001,ERR=2002) NDSTR, VSC, IDLA, IDFT, RFORM, &
                                            FROM, TNAME
        END IF
!
        IF (   ABS(VSC) .LT. 1.E-7  ) VSC    = 1.
        IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
        IF (IDFT.LT.1 .OR. IDFT.GT.3) IDFT   = 1
!
        WRITE (NDSO,1977) NDSTR, VSC, IDLA, IDFT
        IF (IDFT.EQ.2) WRITE (NDSO,973) RFORM
        IF (FROM.EQ.'NAME' .AND. NDSG.NE.NDSTR) WRITE (NDSO,974) TNAME
!
! 9;c  Open file and check if necessary
!
        IF ( NDSTR .EQ. NDSI ) THEN
          IF ( IDFT .EQ. 3 ) THEN
            WRITE (NDSE,1004) NDSTR
            CALL EXTCDE (23)
          ELSE
            CALL NEXTLN ( COMSTR , NDSI , NDSE )
            END IF
        ELSE IF ( NDSTR .EQ. NDSG ) THEN
          IF ( ( IDFM.EQ.3 .AND. IDFT.NE.3 ) .OR.                 &
               ( IDFM.NE.3 .AND. IDFT.EQ.3 ) ) THEN
            WRITE (NDSE,1005) IDFM, IDFT
            CALL EXTCDE (24)
            END IF
          ELSE
            IF ( IDFT .EQ. 3 ) THEN
              IF (FROM.EQ.'NAME') THEN
                OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,                 &
                        FORM='UNFORMATTED',STATUS='OLD',ERR=2000, &
                        IOSTAT=IERR)
              ELSE
                OPEN (NDSTR,           FORM='UNFORMATTED',      &
                    STATUS='OLD',ERR=2000,IOSTAT=IERR)
                END IF
            ELSE
              IF (FROM.EQ.'NAME') THEN
                OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                    STATUS='OLD',ERR=2000,IOSTAT=IERR)
              ELSE
                OPEN (NDSTR,                                    &
                    STATUS='OLD',ERR=2000,IOSTAT=IERR)
                END IF   !end  of (FROM.EQ.'NAME')
              END IF     !end of ( IDFT .EQ. 3 )
            END IF       !end of ( NDSTR .EQ. NDSG )
!
! 9.d Read the data
!
!         CALL INA2R ( REFD, NX, NY, 1, NX, 1, NY, NDSTR, NDST, NDSE, &
!                       IDFM, RFORM, IDLA, VSC, 0.0)
!
          IF ( NDSTR .EQ. NDSI ) CALL NEXTLN ( COMSTR , NDSI , NDSE )
!
!         CALL INA2R ( REFD2, NX, NY, 1, NX, 1, NY, NDSTR, NDST, NDSE, &
!                       IDFM, RFORM, IDLA, VSC, 0.0)
          CALL INA2R ( REFS, NX, NY, 1, NX, 1, NY, NDSTR, NDST, NDSE, &
                       IDFM, RFORM, IDLA, VSC, 0.0)
          DO ISEA=1,NSEA
            IX = MAPSF(ISEA,1)
            IY = MAPSF(ISEA,2)
            REFLC(3,ISEA) = REFS(IX,IY)*REFMAP
            END DO
#endif
!
#ifdef W3_REF1
          NMAPB  = 1 + (NX-1)/NCOL
          WRITE (NDSO,1105) NMAPB
#endif
#ifdef W3_T
#ifdef W3_REF1
               WRITE(NDSO,*) 'Maximum slope for reflection:',MAXVAL(REFS*REFMAP)
#endif
#endif
!
#ifdef W3_REF1
          DO IMAPB=1, NMAPB
            IX0    = 1 + (IMAPB-1)*NCOL
            IXN    = MIN ( NX , IMAPB*NCOL )
#endif
#ifdef W3_T
#ifdef W3_REF1
            DO IY=NY,1,-1
              WRITE (NDSO,1101) (NINT(100.*REFS(IX,IY)*REFMAP),IX=IX0,IXN)
              END DO
#endif
#endif
#ifdef W3_REF1
            WRITE (NDSO,*) ' '
            END DO
          WRITE (NDSO,1106)
!
          WRITE (NDSO,*)
!
          END IF         !end of (REFMAP.EQ.0)
#endif
!
      DEALLOCATE ( ZBIN, TMPSTA, TMPMAP )
#ifdef W3_RTD
      DEALLOCATE ( AnglDin )
#endif
!
! 9.e Reads bottom information from file
!
#ifdef W3_BT4
      ALLOCATE ( SED_D50FILE(NX,NY))
      IF ( SEDMAPD50 ) THEN

!
!  9.e.1 Info from input file
!
          IF (FLGNML) THEN
            NDSTR = NML_SED%IDF
            VSC = NML_SED%SF
            IDLA = NML_SED%IDLA
            IDFT = NML_SED%IDFM
            RFORM = TRIM(NML_SED%FORMAT)
            FROM = TRIM(NML_SED%FROM)
            TNAME = TRIM(NML_SED%FILENAME)
          ELSE
            CALL NEXTLN ( COMSTR , NDSI , NDSE )
            READ (NDSI,*,END=2001,ERR=2002) NDSTR, VSC, IDLA, IDFT, RFORM, &
                                            FROM, TNAME
          END IF
!
          IF (   ABS(VSC) .LT. 1.E-7  ) THEN 
            VSC    = 1.
          ELSE 
! WARNING TO BE ADDED ... 
          END IF
          IF (IDLA.LT.1 .OR. IDLA.GT.4) IDLA   = 1
          IF (IDFT.LT.1 .OR. IDFT.GT.3) IDFT   = 1
!
          WRITE (NDSO,1978) NDSTR, VSC, IDLA, IDFT
          IF (IDFT.EQ.2) WRITE (NDSO,973) RFORM
          IF (FROM.EQ.'NAME' .AND. NDSG.NE.NDSTR) WRITE (NDSO,974) TNAME
!
! 9.e.2  Open file and check if necessary
!
          IF ( NDSTR .EQ. NDSI ) THEN
              IF ( IDFT .EQ. 3 ) THEN
                  WRITE (NDSE,1004) NDSTR
                  CALL EXTCDE (23)
                ELSE
                  CALL NEXTLN ( COMSTR , NDSI , NDSE )
                END IF
            ELSE IF ( NDSTR .EQ. NDSG ) THEN
              IF ( ( IDFM.EQ.3 .AND. IDFT.NE.3 ) .OR.                 &
                   ( IDFM.NE.3 .AND. IDFT.EQ.3 ) ) THEN
                  WRITE (NDSE,1005) IDFM, IDFT
                  CALL EXTCDE (24)
                END IF
            ELSE
              IF ( IDFT .EQ. 3 ) THEN
                  IF (FROM.EQ.'NAME') THEN
                      OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                            FORM='UNFORMATTED',STATUS='OLD',ERR=2000, &
                            IOSTAT=IERR)
                    ELSE
                      OPEN (NDSTR,           FORM='UNFORMATTED',      &
                            STATUS='OLD',ERR=2000,IOSTAT=IERR)
                    END IF
                ELSE
                  IF (FROM.EQ.'NAME') THEN
                      OPEN (NDSTR,FILE=TRIM(FNMPRE)//TNAME,             &
                            STATUS='OLD',ERR=2000,IOSTAT=IERR)
                    ELSE
                      OPEN (NDSTR,                                    &
                            STATUS='OLD',ERR=2000,IOSTAT=IERR)
                    END IF
                END IF
            END IF
!
! 9.e.3 Read the data
!
          CALL INA2R ( SED_D50FILE, NX, NY, 1, NX, 1, NY, NDSTR, NDST, NDSE, &
                       IDFM, RFORM, IDLA, VSC, VOF)
!
          IF ( NDSTR .EQ. NDSI ) CALL NEXTLN ( COMSTR , NDSI , NDSE )
!
          WRITE (NDSO,*) 'Min and Max values of grain sizes:',MINVAL(SED_D50FILE), MAXVAL(SED_D50FILE)
          WRITE (NDSO,*)
!
      ELSE 
        SED_D50FILE(:,:)=SED_D50_UNIFORM
        END IF
!
      DO IY=1, NY
        DO IX=1, NX
          ISEA = MAPFS (IY,IX)
          SED_D50(ISEA)       = SED_D50FILE(IX,IY)
          SED_D50(ISEA)       = MAX(SED_D50(ISEA),1E-5)  
   ! Critical Shields number, Soulsby, R.L. and R J S W Whitehouse
   ! Threshold of sed. motion in coastal environments, Proc. Pacific Coasts and
   ! ports, 1997 conference, Christchurch, p149-154, University of Cantebury, NZ
          SED_DSTAR=(GRAV*(SED_SG-1)/nu_water**2)**(0.333333)*SED_D50(ISEA)  
          SED_PSIC(ISEA)=0.3/(1+1.2*SED_DSTAR)+0.55*(1-exp(-0.02*SED_DSTAR)) 
#endif


#ifdef W3_BT4
          END DO
        END DO
#endif
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 10.  Prepare output boundary points.
!     ILOOP = 1 to count NFBPO and NBO
!     ILOOP = 2 to fill data arrays
!
      WRITE (NDSO,990)
      IF ( .NOT. FLGNML ) &
        OPEN (NDSS,FILE=TRIM(FNMPRE)//'ww3_grid.scratch',FORM='FORMATTED')
!
      DO ILOOP = 1, 2
!
        IF ( ILOOP.EQ.2 ) CALL W3DMO5 ( 1, NDST, NDSE, 2 )
!
        I = 1
        NBOTOT = 0
        NFBPO  = 0
        NBO(0) = 0
        NBO2(0)= 0
        FIRST  = .TRUE.
        IF ( .NOT. FLGNML ) THEN
          REWIND (NDSS)
          IF ( ILOOP .EQ. 1 ) THEN
            NDSI2 = NDSI
          ELSE
            NDSI2 = NDSS
          END IF
        END IF
!
        DO
          IF (FLGNML) THEN
            ! outbound lines
            IF (NML_OUTBND_COUNT%N_LINE.GT.0 .AND. I.LE.NML_OUTBND_COUNT%N_LINE) THEN 
              XO0 = NML_OUTBND_LINE(I)%X0
              YO0 = NML_OUTBND_LINE(I)%Y0
              DXO = NML_OUTBND_LINE(I)%DX
              DYO = NML_OUTBND_LINE(I)%DY
              NPO = NML_OUTBND_LINE(I)%NP
              I=I+1
            ELSE
              NPO=0
            END IF
          ELSE
            CALL NEXTLN ( COMSTR , NDSI2 , NDSE )
            READ (NDSI2,*,END=2001,ERR=2002) XO0, YO0, DXO, DYO, NPO
          END IF
!
          IF ( .NOT. FLGNML .AND. ILOOP .EQ. 1 ) THEN
            BACKSPACE (NDSI)
            READ (NDSI,'(A)') LINE
            WRITE (NDSS,'(A)') LINE
          END IF
!
! ... Check if new file to be used
!
          FIRST  = FIRST .OR. NPO.LE.0
          NPO    = ABS(NPO)
!
! ... Preparations for new output file including end check
!     and output for last output file
!
          IF ( FIRST ) THEN
!
              FIRST  = .FALSE.
!
#ifdef W3_RTD
              IF ( NPO.NE.0 ) THEN
 ! Destination pole lat, lon from namelist
                  bPolat = BPLAT(NFBPO+1)
                  bPolon = BPLON(NFBPO+1)
                END IF
 !          
#endif
              IF ( NFBPO.GE.1 .AND. ILOOP.EQ.2 ) THEN
                  WRITE (NDSO,991)  NFBPO, NBO(NFBPO) - NBO(NFBPO-1), &
                                          NBO2(NFBPO) - NBO2(NFBPO-1)
#ifdef W3_RTD
 ! Print dest. Pole lat/lon if either the dest or present grid is rotated 
                 IF ( BPLAT(NFBPO) < 90. .OR. Polat < 90. )      &
                     WRITE (NDSO,1991) BPLAT(NFBPO), BPLON(NFBPO)
 !
#endif
#ifdef W3_O1
                  IF ( NBO(NFBPO) - NBO(NFBPO-1) .EQ. 1 ) THEN
                      IF ( FLAGLL ) THEN
                          WRITE (NDSO,992)
                        ELSE
                          WRITE (NDSO,2992)
                        END IF
                    ELSE
                      IF ( FLAGLL ) THEN
                          WRITE (NDSO,1992)
                        ELSE
                          WRITE (NDSO,3992)
                        END IF
                    END IF
                  IP0    = NBO(NFBPO-1)+1
                  IPN    = NBO(NFBPO)
                  IPH    = IP0 + (IPN-IP0-1)/2
                  IPI    = IPH -IP0 + 1 + MOD(IPN-IP0+1,2)
                  DO IP=IP0, IPH
                    IF ( FLAGLL ) THEN
                        WRITE (NDSO,1993) IP-NBO(NFBPO-1),     &
                                          FACTOR*XBPO(IP),     &
                                          FACTOR*YBPO(IP),     &
                                          IP+IPI-NBO(NFBPO-1), &
                                          FACTOR*XBPO(IP+IPI), &
                                          FACTOR*YBPO(IP+IPI)
                      ELSE
                        WRITE (NDSO,3993) IP-NBO(NFBPO-1),     &
                                          FACTOR*XBPO(IP),     &
                                          FACTOR*YBPO(IP),     &
                                          IP+IPI-NBO(NFBPO-1), &
                                          FACTOR*XBPO(IP+IPI), &
                                          FACTOR*YBPO(IP+IPI)
                      END IF
                    END DO
                  IF ( MOD(IPN-IP0+1,2) .EQ. 1 ) THEN
                      IF ( FLAGLL ) THEN
                          WRITE (NDSO, 993) IPH+1-NBO(NFBPO-1), &
                                            FACTOR*XBPO(IPH+1), &
                                            FACTOR*YBPO(IPH+1)
                        ELSE
                          WRITE (NDSO,2993) IPH+1-NBO(NFBPO-1), &
                                            FACTOR*XBPO(IPH+1), &
                                            FACTOR*YBPO(IPH+1)
                        END IF
                    END IF
                  WRITE (NDSO,*)
#endif
                END IF
!
              IF ( NPO .EQ. 0 ) EXIT
!
              NFBPO  = NFBPO + 1
              IF ( NFBPO .GT. 9 ) THEN
                  WRITE (NDSE,1006)
                  CALL EXTCDE ( 50 )
                END IF
              NBO2(NFBPO) = NBO2(NFBPO-1)
              NBO(NFBPO) = NBOTOT
!
            END IF
!
! ... Loop over line segment - - - - - - - - - - - - - - - - - - - - -
!
#ifdef W3_RTD
 ! If either base or destination grid is rotated lat-lon
          IF ( allocated(BDYLON) .eqv. .TRUE. ) THEN
            deallocate( BDYLON, BDYLAT )
            IF ( bPolat < 90. .OR. Polat < 90. ) &
              deallocate( ELatbdy, ELonbdy, Anglbdy )
            END IF
          allocate( BDYLON(NPO), BDYLAT(NPO))
          IF ( bPolat < 90. .OR. Polat < 90. ) &
            allocate( ELatbdy(NPO), ELonbdy(NPO), Anglbdy(NPO) )
 !            
#endif
#ifdef W3_T
          WRITE (NDST,9090)
#endif
!
          DO IP=1, NPO
!
            XO     = XO0 + REAL(IP-1)*DXO
            YO     = YO0 + REAL(IP-1)*DYO
#ifdef W3_RTD
 !
 ! Boundary points are specified in coordinates of the destination grid
 !
 ! Collect the line segment points into arrays
            BDYLON(IP)     = XO
            BDYLAT(IP)     = YO
 ! Close the loop before calculating rotated lat-lon coordinates.
            END DO

 ! Create one or two sets of the segment points:
 ! 1. (BDYLAT, BDYLON) in standard lat-lon coordinates,
 ! 2. Also (ELatbdy, ELonbdy) in case the base grid is rotated

            IF ( bPolat < 90. ) THEN
              ! The destination grid is rotated (std->rot or rot->rot)
              ! Change BDYLAT, BDYLON to their standard lat-lon positions
              ! Let ELatbdy,ELonbdy contain the rotated lat-lon coordinates
              ELatbdy(:) = BDYLAT(:)
              ELonbdy(:) = BDYLON(:)
              CALL W3EQTOLL ( ELatbdy, ELonbdy, BDYLAT, BDYLON,     &
             &                Anglbdy, bPolat, bPolon, NPO )
 ! Let the standard longitudes BDYLON be within the range [-180.,180.[
 ! or [0., 360.[ depending on the grid pole
              IF ( Polon < -90. .OR. Polon > 90. ) THEN
                BDYLON(:) = MOD( BDYLON(:) + 180., 360. ) - 180.
              ELSE
                BDYLON(:) = MOD( BDYLON(:) + 360., 360. )
                END IF
              END IF ! bPolat < 90.
 ! From now, BDYLAT, BDYLON are defined in standard lat-lon coordinates
 !
            IF ( Polat < 90. ) THEN
              ! The base grid is rotated (rot->std or rot->rot)
              ! Find lat-lon in coordinates of the rotated base grid
              CALL W3LLTOEQ ( BDYLAT, BDYLON, ELatbdy, ELonbdy,     &
             &                Anglbdy, Polat, Polon, NPO )
              END IF
 !
 ! Take up again the loop over the line segment points
          DO IP=1, NPO
            IF ( Polat < 90. ) THEN
              ! The base grid is rotated (rot->std, rot->rot)
              ! (The std. lat-lon values BDYLAT, BDYLON go to YBPO, XBPO)
              XO = ELonbdy(IP)
              YO = ELatbdy(IP)
            ELSE
              ! The base grid is standard geographic (std->rot or std->std)
              XO = BDYLON(IP)
              YO = BDYLAT(IP)
              END IF
#endif
!   
! ... Compute bilinear remapping weights
!
            INGRID = W3GRMP( GSU, XO, YO, IXR, IYR, RD )
!
!           Change cell-corners from counter-clockwise to column-major order
            IX     = IXR(3);  IY     = IYR(3);  X     = RD(3);
            IXR(3) = IXR(4);  IYR(3) = IYR(4);  RD(3) = RD(4);
            IXR(4) = IX    ;  IYR(4) = IY    ;  RD(4) = X    ;
!
#ifdef W3_T
            WRITE (NDST,9091) FACTOR*XO, FACTOR*YO,                   &
                              (IXR(J), IYR(J), RD(J), J=1,4)
#endif
!
! ... Check if point in grid
!
            IF ( INGRID ) THEN
!
! ... Check if point not on land
!
              IF ( ( MAPSTA(IYR(1),IXR(1)).GT.0 .AND.                 &
                                        RD(1).GT.0.05 ) .OR.          &
                   ( MAPSTA(IYR(2),IXR(2)).GT.0 .AND.                 &
                                        RD(2).GT.0.05 ) .OR.          &
                   ( MAPSTA(IYR(3),IXR(3)).GT.0 .AND.                 &
                                        RD(3).GT.0.05 ) .OR.          &
                   ( MAPSTA(IYR(4),IXR(4)).GT.0 .AND.                 &
                                        RD(4).GT.0.05 ) ) THEN
!
! ... Check storage and store coordinates
!
                NBOTOT = NBOTOT + 1
                IF ( ILOOP .EQ. 1 ) CYCLE
!
#ifdef W3_RTD
 ! BDYLAT, BDYLON contain Y0, X0, which are remapped to standard lat/lon.
 ! BDYLAT, BDYLON are stored in the mod_def file.
                IF ( Polat < 90. ) THEN
                  XO = BDYLON(IP)
                  YO = BDYLAT(IP)
                  END IF
#endif
                XBPO(NBOTOT) = XO
                YBPO(NBOTOT) = YO
!
! ... Interpolation factors
!
                RDTOT = 0.
                DO J=1, 4
                  IF ( MAPSTA(IYR(J),IXR(J)).GT.0 .AND.               &
                                            RD(J).GT.0.05 ) THEN
                      RDBPO(NBOTOT,J) = RD(J)
                    ELSE
                      RDBPO(NBOTOT,J) = 0.
                    END IF
                    RDTOT = RDTOT + RDBPO(NBOTOT,J)
                  END DO
!
                DO J=1, 4
                  RDBPO(NBOTOT,J) = RDBPO(NBOTOT,J) / RDTOT
                  END DO
!
#ifdef W3_T
              WRITE (NDST,9092) RDTOT, (RDBPO(NBOTOT,J),J=1,4)
#endif
!
! ... Determine sea and interpolation point counters
!
                DO J=1, 4
                  ISEAI(J) = MAPFS(IYR(J),IXR(J))
                  END DO
!
                DO J=1, 4
                  IF ( ISEAI(J).EQ.0 .OR. RDBPO(NBOTOT,J).EQ. 0. ) THEN
                      IPBPO(NBOTOT,J) = 0
                    ELSE
                      FLNEW   = .TRUE.
                      DO IST=NBO2(NFBPO-1)+1, NBO2(NFBPO)
                        IF ( ISEAI(J) .EQ. ISBPO(IST) ) THEN
                            FLNEW  = .FALSE.
                            IPBPO(NBOTOT,J) = IST - NBO2(NFBPO-1)
                          END IF
                        END DO
                      IF ( FLNEW ) THEN
                          NBO2(NFBPO)        = NBO2(NFBPO) + 1
                          IPBPO(NBOTOT,J)    = NBO2(NFBPO) - NBO2(NFBPO-1)
                          ISBPO(NBO2(NFBPO)) = ISEAI(J)
                        END IF
                    END IF
                  END DO
!
#ifdef W3_T
                WRITE (NDST,9093) ISEAI, (IPBPO(NBOTOT,J),J=1,4)
#endif
!
! ... Error output
!
                ELSE
                  IF ( FLAGLL ) THEN
                      WRITE (NDSE,2995) FACTOR*XO, FACTOR*YO
                    ELSE
                      WRITE (NDSE,995) FACTOR*XO, FACTOR*YO
                    END IF
                END IF
              ELSE
                IF ( FLAGLL ) THEN
                    WRITE (NDSE,2994) FACTOR*XO, FACTOR*YO
                  ELSE
                    WRITE (NDSE,994) FACTOR*XO, FACTOR*YO
                  END IF
              END IF
!
            END DO
!
          NBO(NFBPO) = NBOTOT
!
! ... Branch back to read.
!
          END DO
!
! ... End of ILOOP loop
!
        END DO
!
      IF ( .NOT. FLGNML ) CLOSE ( NDSS, STATUS='DELETE' )
!
      FLBPO  = NBOTOT .GT. 0
      IF ( .NOT. FLBPO ) THEN
          WRITE (NDSO,996)
        ELSE
          WRITE (NDSO,997) NBOTOT, NBO2(NFBPO)
        END IF
!
#ifdef W3_T0
      WRITE (NDST,9095)
      DO IFILE=1, NFBPO
        DO IP=NBO2(IFILE-1)+1, NBO2(IFILE)
          WRITE (NDST,9096) IFILE, IP-NBO2(IFILE-1), ISBPO(IP)
          END DO
        END DO
#endif
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!10.  Write model definition file.
!
      WRITE (NDSO,999)
      CALL W3IOGR ( 'WRITE', NDSM )
!
      CLOSE (NDSM)
!
      GOTO 2222
!
! Escape locations read errors :
!
 2000 CONTINUE
      WRITE (NDSE,1000) IERR
      CALL EXTCDE ( 60 )
!
 2001 CONTINUE
      WRITE (NDSE,1001)
      CALL EXTCDE ( 61 )
!
 2002 CONTINUE
      WRITE (NDSE,1002) IERR
      CALL EXTCDE ( 62 )
!
 2003 CONTINUE
      WRITE (NDSE,1003)
      CALL EXTCDE ( 64 )
!
 2222 CONTINUE
      IF ( GTYPE .NE. UNGTYPE) THEN
          IF ( NX*NY .NE. NSEA ) THEN
              WRITE (NDSO,9997) NX, NY, NX*NY, NSEA,                       &
                                100.*REAL(NSEA)/REAL(NX*NY), NBI, NLAND, NBT
            ELSE
              WRITE (NDSO,9998) NX, NY, NX*NY, NSEA, NBI, NLAND, NBT
            END IF
        ELSE IF ( GTYPE .EQ. UNGTYPE ) THEN
          IF ( NX*NY .NE. NSEA ) THEN
              WRITE (NDSO,9997)  0,  0, NX*NY, NSEA,                       &
                                100.*REAL(NSEA)/REAL(NX*NY), NBI, NLAND, NBT
            ELSE
              WRITE (NDSO,9998)  0,  0, NX*NY, NSEA, NBI, NLAND, NBT
            END IF
        ENDIF ! GTYPE .EQ. UNGTYPE
              
     WRITE (NDSO,9999)  

#ifdef W3_SCRIP
      GRID1_UNITS='degrees' ! the other option is radians...we don't use this
      GRID1_NAME='src' ! this is not used, except for netcdf output
      CALL GET_SCRIP_INFO(1,                                            &    
     &   GRID1_CENTER_LON, GRID1_CENTER_LAT,                            &    
     &   GRID1_CORNER_LON, GRID1_CORNER_LAT, GRID1_MASK,                &    
     &   GRID1_DIMS, GRID1_SIZE, GRID1_CORNERS, GRID1_RANK)

      
#endif

#ifdef W3_SCRIP
     IF (GTYPE .EQ. UNGTYPE) THEN
       GRID1_RANK=1
       DEALLOCATE(GRID1_DIMS)
       ALLOCATE(GRID1_DIMS(GRID1_RANK))
       GRID1_DIMS(1) = GRID1_SIZE
     ENDIF
#endif

#ifdef W3_SCRIP
     DO I = 1,GRID1_SIZE
       IF (GRID1_CENTER_LON(I) < 0.0) THEN
         GRID1_CENTER_LON(I) = GRID1_CENTER_LON(I)+360.0
       ENDIF
       DO J = 1,GRID1_CORNERS
         IF (GRID1_CORNER_LON(J,I) < 0.0) THEN
           GRID1_CORNER_LON(J,I) = GRID1_CORNER_LON(J,I)+360.0 
         ENDIF
       ENDDO
     ENDDO
#endif

#ifdef W3_SCRIPNC
     IERR = NF90_CREATE(TRIM('scrip.nc'), NF90_NETCDF4, NCID)
     IERR = NF90_DEF_DIM(NCID, 'grid_size', GRID1_SIZE, grid_size_dimid)
     IERR = NF90_DEF_DIM(NCID, 'grid_corners', GRID1_CORNERS, grid_corners_dimid)
     IERR = NF90_DEF_DIM(NCID, 'grid_rank', GRID1_RANK, grid_rank_dimid)
#endif

#ifdef W3_SCRIPNC
     IERR = NF90_DEF_VAR(NCID, 'grid_center_lat', NF90_DOUBLE, &
                         (/grid_size_dimid/),grid_center_lat_varid)
     IERR = NF90_DEF_VAR(NCID, 'grid_center_lon', NF90_DOUBLE, &
                         (/grid_size_dimid/),grid_center_lon_varid)
     IERR = NF90_DEF_VAR(NCID, 'grid_corner_lat', NF90_DOUBLE, &
                         (/grid_corners_dimid,grid_size_dimid/), &
                         grid_corner_lat_varid)
     IERR = NF90_DEF_VAR(NCID, 'grid_corner_lon', NF90_DOUBLE, &
                         (/grid_corners_dimid,grid_size_dimid/), &
                         grid_corner_lon_varid)
     IERR = NF90_DEF_VAR(NCID, 'grid_imask', NF90_INT, &
                         (/grid_size_dimid/),grid_imask_varid)
     IERR = NF90_DEF_VAR(NCID, 'grid_dims', NF90_INT, &
                         (/grid_rank_dimid/),grid_dims_varid)
     IERR = NF90_ENDDEF(NCID)
#endif

#ifdef W3_SCRIP
     ALLOCATE(GRID1_IMASK(GRID1_DIMS(1)))
     GRID1_IMASK = 0
     DO I = 1,GRID1_DIMS(1)
       IF (GRID1_MASK(I)) THEN
         GRID1_IMASK(I) = 1
       ENDIF
     ENDDO
#endif

#ifdef W3_SCRIPNC
     IERR = NF90_PUT_ATT(NCID,grid_center_lat_varid,'units',GRID1_UNITS)
     IERR = NF90_PUT_ATT(NCID,grid_center_lon_varid,'units',GRID1_UNITS)
     IERR = NF90_PUT_ATT(NCID,grid_corner_lat_varid,'units',GRID1_UNITS)
     IERR = NF90_PUT_ATT(NCID,grid_corner_lon_varid,'units',GRID1_UNITS)
     IERR = NF90_PUT_ATT(NCID,grid_imask_varid,'units','unitless')
#endif

#ifdef W3_SCRIPNC
     IERR = NF90_PUT_VAR(NCID,grid_center_lat_varid,GRID1_CENTER_LAT)
     IERR = NF90_PUT_VAR(NCID,grid_center_lon_varid,GRID1_CENTER_LON)
     IERR = NF90_PUT_VAR(NCID,grid_corner_lat_varid,GRID1_CORNER_LAT)
     IERR = NF90_PUT_VAR(NCID,grid_corner_lon_varid,GRID1_CORNER_LON)
     IERR = NF90_PUT_VAR(NCID,grid_imask_varid,GRID1_IMASK)
     IERR = NF90_PUT_VAR(NCID,grid_dims_varid,GRID1_DIMS)
     IERR = NF90_CLOSE(NCID)
#endif
      

!
! Formats
!
  900 FORMAT (/15X,'    *** WAVEWATCH III Grid preprocessor ***    '/ &
               15X,'==============================================='/)
  901 FORMAT ( '  Comment character is ''',A,''''/)
  902 FORMAT ( '  Grid name : ',A/)
  903 FORMAT (/'  Spectral discretization : '/                        &
               ' --------------------------------------------------'/ &
               '       Number of directions        :',I4/             &
               '       Directional increment (deg.):',F6.1)
  904 FORMAT ( '       First direction       (deg.):',F6.1)
  905 FORMAT ( '       Number of frequencies       :',I4/             &
               '       Frequency range        (Hz) :',F9.4,'-',F6.4/  &
               '       Increment factor            :',F8.3/)
!
  910 FORMAT (/'  Model definition :'/                                &
               ' --------------------------------------------------')
  911 FORMAT ( '       Dry run (no calculations)   :  ',A/            &
               '       Propagation in X-direction  :  ',A/            &
               '       Propagation in Y-direction  :  ',A/            &
               '       Refraction                  :  ',A/            &
               '       Current-induced k-shift     :  ',A/            &
               '       Source term calc. and int.  :  ',A/)
  912 FORMAT (/'  Time steps : '/                                     &
               ' --------------------------------------------------'/ &
               '       Maximum global time step      (s) :',F8.2/     &
               '       Maximum CFL time step X-Y     (s) :',F8.2/     &
               '       Maximum CFL time step k-theta (s) :',F8.2/     &
               '       Minimum source term time step (s) :',F8.2/)
  913 FORMAT (/ '  WARNING, TIME STEP LESS THAN 1 s, NITER:',I8 /)
  915 FORMAT ( '  Preprocessing namelists ...')
  916 FORMAT ( '  Preprocessing namelists finished.'/)
  917 FORMAT (/'  Equivalent namelists ...'/)
  918 FORMAT (/'  Equivalent namelists finished.'/)
!
#ifdef W3_FLX1
  810 FORMAT (/'  Stresses (Wu 1980)'/                          &
        ' --------------------------------------------------'/)
#endif
#ifdef W3_FLX2
  810 FORMAT (/'  Stresses (T&C 96)'/                           &
        ' --------------------------------------------------'/)
#endif
#ifdef W3_FLX3
  810 FORMAT (/'  Stresses (T&C 96 capped) ',A/                 &
        ' --------------------------------------------------')
#endif
#ifdef W3_FLX4
  810 FORMAT (/'  Stresses (Hwang 2011) ',A/                     &
        ' --------------------------------------------------')
  811 FORMAT ( '       drag coefficient scaling    :',F8.2    /)
 2810 FORMAT ( '  &FLX4 CDFAC =',F6.3,' /')
#endif
#ifdef W3_FLX5
  810 FORMAT (/'  Direct use of stress from input'/                          &
        ' --------------------------------------------------'/)
#endif
#ifdef W3_FLX3
  811 FORMAT ( '       Max Cd * 10^3               :',F8.2/     &
               '       Cap type                    : ',A/)
 2810 FORMAT ( '  &FLX3 CDMAX =',F6.2,'E-3 , CTYPE = ',I1,' /')
#endif
!
#ifdef W3_LN0
  820 FORMAT (/'  Linear input not defined.'/)
#endif
#ifdef W3_SEED
  820 FORMAT (/'  Seeding as proxi for linear input.'/)
#endif
!
#ifdef W3_LN1
  820 FORMAT (/'  Linear input (C&M-R 82) ',A/                   &
        ' --------------------------------------------------')
  821 FORMAT ( '       CLIN                        :',f8.2/      &
               '       Factor for fPM in filter    :',F8.2/      &
               '       Factor for fh in filter     :',F8.2/)
 2820 FORMAT ( '  &SLN1 CLIN =',F6.1,', RFPM =',F6.2,            &
               ', RFHF =',F6.2,' /')
#endif
!
#ifdef W3_ST0
  920 FORMAT (/'  Wind input not defined.'/)
#endif
!
#ifdef W3_ST1
  920 FORMAT (/'  Wind input (WAM-3) ',A/                        &
        ' --------------------------------------------------')
  921 FORMAT ( '       Cinp                        :',E10.3/)
 2920 FORMAT ( '  &SIN1 CINP =',F7.3,' /')
#endif
!
#ifdef W3_ST2
  920 FORMAT (/'  Wind input (T&C 1996) ',A/                     &
        ' --------------------------------------------------')
  921 FORMAT ( '       Height of input wind (m)    :',F8.2/      &
               '       Factor negative swell       :',F9.3/)
#endif
#ifdef W3_STAB2
 1921 FORMAT ( '       Effective wind mean factor  :',F8.2/    &
               '       Stability par. offset       :',F9.3/    &
               '       Stab. correction            :',F9.3,F8.3/&
               '       Stab. correction stab. fac. :',F7.1,F9.1/)
#endif
#ifdef W3_ST2
 2920 FORMAT ( '  &SIN2 ZWND =',F5.1,', SWELLF =',F6.3,' /')
#endif
#ifdef W3_STAB2
 2921 FORMAT ( '  &SIN2 ZWND =',F5.1,', SWELLF =',F6.3,', STABSH =',  &
                        F6.3,', STABOF = ',E10.3,','/          &
               '        CNEG =',F7.3,', CPOS =',F7.3,', FNEG =',F7.1,' /')
#endif
!
#ifdef W3_ST3
  920 FORMAT (/'  Wind input (WAM 4+) ',A/                            &
        ' --------------------------------------------------')
  921 FORMAT ( '       minimum Charnock coeff.     :',F10.4/          &
               '       betamax                     :',F9.3/           &
               '       power of cos. in wind input :',F9.3/           &
               '       z0max                       :',F9.3/           &
               '       zalp                        :',F9.3/           &
               '       Height of input wind (m)    :',F8.2/           &
               '       swell attenuation factor    :',F9.3/ )
 2920 FORMAT ( '  &SIN3 ZWND =',F5.1,', ALPHA0 =',F8.5,', Z0MAX =',F8.5,', BETAMAX =', &
                  F8.5,','/                                           &
              '        SINTHP =',F8.5,', ZALP =',F8.5,','/            &
              '        SWELLF =',F8.5,'R /'/)                              
#endif
!
#ifdef W3_ST4
  920 FORMAT (/'  Wind input (WAM 4+) ',A/                            &
        ' --------------------------------------------------')
  921 FORMAT ( '       minimum Charnock coeff.     :',F10.4/          &
               '       betamax                     :',F9.3/           &
               '       power of cos. in wind input :',F9.3/           &
               '       z0max                       :',F9.3/           &
               '       zalp                        :',F9.3/           &
               '       Height of input wind (m)    :',F8.2/           &
               '       wind stress sheltering      :',F9.3/           &
               '       swell attenuation param.    :',I5/             &
               '       swell attenuation factor    :',F9.3/           &
               '       swell attenuation factor2   :',F9.3/           &
               '       swell attenuation factor3   :',F9.3/           &
               '       critical Reynolds number    :',F9.1/           &
               '       swell attenuation factor5   :',F9.3/           &
               '       swell attenuation factor6   :',F9.3/           &
               '       swell attenuation factor7   :',F14.3/          &
               '       ratio of z0 for orb. & mean :',F9.3/)
 2920 FORMAT ( '  &SIN4 ZWND =',F5.1,', ALPHA0 =',F8.5,', Z0MAX =',F8.5,', BETAMAX =', &
                  F8.5,','/                                           &
              '        SINTHP =',F8.5,', ZALP =',F8.5,', TAUWSHELTER =',F8.5,           &
              ', SWELLFPAR =',I2,','/                                 &
              '        SWELLF =',F8.5,', SWELLF2 =',F8.5,             &
              ', SWELLF3 =',F8.5,', SWELLF4 =',F9.1,','/              &
              '        SWELLF5 =',F8.5,', SWELLF6 =',F8.5,            &
              ', SWELLF7 =',F12.2,', Z0RAT =',F8.5,', SINBR =',F8.5,'  /')
#endif
!
#ifdef W3_ST6
  920 FORMAT (/'  Wind input (Donelan et al, 2006) ',A/       &
         ' --------------------------------------------------')
  921 FORMAT ( '  negative wind input active       :  ',A/    &
               '  attenuation factor               :  ',F6.2/ &
               '  wind speed scaling factor        :  ',F6.2/ &
               '  frequency cut-off factor         :  ',F6.2/)
 2920 FORMAT ( '  &SIN6 SINA0 =', F6.3, ', SINWS =', F6.2, ', SINFC =', F6.2, ' /')
#endif
!
#ifdef W3_NL0
  922 FORMAT (/'  Nonlinear interactions not defined.'/)
#endif
!
#ifdef W3_NL1
  922 FORMAT (/'  Nonlinear interactions (DIA) ',A/                   &
               ' --------------------------------------------------')
  923 FORMAT ( '       Lambda                      :',F8.2/      &
               '       Prop. constant              :',E10.3/     &
               '       kd conversion factor        :',F8.2/      &
               '       minimum kd                  :',F8.2/      &
               '       shallow water constants     :',F8.2,2F6.2/)
 2922 FORMAT ( '  &SNL1 LAMBDA =',F7.3,', NLPROP =',E10.3,       &
               ', KDCONV =',F7.3,', KDMIN =',F7.3,','/           &
               '        SNLCS1 =',F7.3,', SNLCS2 =',F7.3,        &
               ', SNLCS3 = ',F7.3,' /')
#endif
!
#ifdef W3_NL2
  922 FORMAT (/'  Nonlinear interactions (WRT) ',A/              &
               ' --------------------------------------------------')
  923 FORMAT ( '       Deep/shallow options        : ',A/        &
               '       Power of h-f tail           : ',F6.1)
 1923 FORMAT ( '       Number of depths used       : ',I4/       &
               '       Depths (m)                  :',5F7.1)
 2923 FORMAT ( '                                    ',5F7.1)
 2922 FORMAT ( '  &SNL2 IQTYPE =',I2,', TAILNL =',F5.1,',',      &
                      ' NDEPTH =',I3,' /')
 3923 FORMAT ( '  &SNL2 DEPTHS =',F9.2,' /')
 4923 FORMAT ( '  &ANL2 DEPTHS =',F9.2,' ,')
 5923 FORMAT ( '                ',F9.2,' ,')
 6923 FORMAT ( '                ',F9.2,' /')
#endif
!
#ifdef W3_NL3
  922 FORMAT (/'  Nonlinear interactions (GMD) ',A/              &
        ' --------------------------------------------------')
  923 FORMAT ( '       Powers in scaling functions : ',2F7.2/    &
               '       Nondimension filter depths  : ',2F7.2)
 1923 FORMAT ( '       Number of quad. definitions : ',I4)
 2923 FORMAT ( '             ',2F8.3,F6.1,2E12.4)
 2922 FORMAT ( '  &SNL3 NQDEF =',I3,', MSC =',F6.2,',  NSC =',   &
               F6.2,',  KDFD =',F6.2,',  KDFS =',F6.2,' /')
 3923 FORMAT ( '  &ANL3 QPARMS = ',2(F5.3,', '),F5.1,', ',E10.4, &
               ', ',E10.4,' /')
 4923 FORMAT ( '  &ANL3 QPARMS = ',2(F5.3,', '),F5.1,', ',E10.4, &
               ', ',E10.4,' ,')
 5923 FORMAT ( '                 ',2(F5.3,', '),F5.1,', ',E10.4, &
               ', ',E10.4,' ,')
 6923 FORMAT ( '                 ',2(F5.3,', '),F5.1,', ',E10.4, &
               ', ',E10.4,' /')
#endif
!
#ifdef W3_NL4
  922 FORMAT (/'  Nonlinear interactions (TSA) ',A/              &
        ' --------------------------------------------------')
  923 FORMAT ( '   Source term computation (1=TSA,0=FBI) : ',I2/ &
               '   Alternate loops (1=no,2=yes)          : ',I2/ &
               '   (To speed up computation) ')
 2922 FORMAT ( '  &SNL4 ITSA =',I2,', IALT =',I2 )
#endif
!
#ifdef W3_NL5
  922 FORMAT(/'  Nonlinear interactions (GKE) ',A/              &
        ' --------------------------------------------------')
  923 FORMAT ( '   Constant water depth (in meter) : ', F7.1/ &
               '   Quasi-resonant quartets cut-off : ', F8.2/ &
               '   Discretiz. of GKE (0:Con., 1:GS): ', I5/   &
               '   GKE (0: GS13-JFM, 1: J03-JPO)   : ', I5/   &
               '   Interp (0: nearest, 1: bilinear): ', I5/   &
               '   Mixing (0: no, N: N Tm, -1: b_T): ', I5/)
 2922 FORMAT ( '  &SNL5 NL5DPT =', F7.1, ', NL5OML =', F5.2, &
               ', NL5DIS =', I2.1, ', NL5KEV =', I2.1,       &
               ', NL5IPL =', I2.1, ', NL5PMX =', I5.1, ' /')
#endif
!
#ifdef W3_NLS
 9922 FORMAT (/'  HF filter based on Snl ',A/                    &
        ' --------------------------------------------------')
 9923 FORMAT ( '       a34 (lambda)                :',F9.3,F9.4/ &
               '       Prop. constant              :',E10.3/     &
               '       maximum relative change     :',F9.3/      &
               '       filter constants            :',F8.2,2F6.2/)
 8922 FORMAT ( '  &SNLS A34 =',F6.3,', FHFC =',E11.4,            &
               ',  DNM =',F6.3,','/'        FC1 =',F6.3,         &
               ',  FC2 =',F6.3,',  FC3 =',F6.3,' /')
#endif
!
#ifdef W3_ST0
  924 FORMAT (/'  Dissipation not defined.'/)
#endif
!
#ifdef W3_ST1
  924 FORMAT (/'  Dissipation (WAM-3) ',A/                       &
        ' --------------------------------------------------')
  925 FORMAT ( '       Cdis                        :',E10.3/     &
               '       Apm                         :',E10.3/)
 2924 FORMAT ( '  &SDS1 CDIS =',E12.4,', APM =',E11.4,' /')
#endif
!
#ifdef W3_ST2
  924 FORMAT (/'  Dissipation (T&C 1996) ',A/                    &
        ' --------------------------------------------------')
  925 FORMAT ( '       High-frequency constants    :',F8.2,E11.3,F6.2/ &
               '       Low-frequency constants     :',E11.3,F6.2/&
               '                                    ',E11.3,F6.2/&
               '       Minimum input peak freq. (-):',F10.4/     &
               '       Minimum PHI                 :',F10.4/)
 2924 FORMAT ( '  &SDS2 SDSA0 =',E10.3,', SDSA1 =',E10.3,', SDSA2 =', &
                        E10.3,', '/                              &
               '        SDSB0 =',E10.3,', SDSB1 =',E10.3,', ',   &
                       'PHIMIN =',E10.3,' /')
#endif
!
#ifdef W3_ST3
  924 FORMAT (/' Dissipation (WAM Cycle 4+) ',A/                 &
        ' --------------------------------------------------')
  925 FORMAT ( '       SDSC1                       :',1E11.3/    &
               '       Power of k in mean k        :',F8.2/      &
               '       weights of k and k^2        :',F9.3,F6.3/)  
 2924 FORMAT ( '  &SDS3 SDSC1 =',E12.4,', WNMEANP =',F4.2,       &
               ', FXPM3 =', F4.2,',FXFM3 =',F4.2,', '/           &  
               '        SDSDELTA1 =', F5.2,', SDSDELTA2 =',F5.2, &
               ' /')                               
#endif
!
#ifdef W3_ST4
  924 FORMAT (/' Dissipation (Ardhuin / Filipot / Romero ) ',A/          &
        ' --------------------------------------------------')
  925 FORMAT ( '       SDSC2, SDSBCK, SDSCUM       :',3E11.3/    &
               '       Power of k in mean k        :',F8.2/)      
#endif


#ifdef W3_ST4
 2924 FORMAT ( '  &SDS4 SDSBCHOICE = ',F3.1,                       &
               ', SDSC2 =',E12.4,', SDSCUM =',F6.2,', '/         &
               '        SDSC4 =',F6.2,', SDSC5 =',E12.4,         &
               ', SDSC6 =',E12.4,','/                            &
               '        WNMEANP =',F4.2,', FXPM3 =', F4.2,       &
               ', FXFM3 =',F4.1,', FXFMAGE =',F6.3, ', '/       &
               '        SDSBINT =',E12.4,', SDSBCK =',E12.4,     &
               ', SDSABK =',F6.3,', SDSPBK =',F6.3,', '/         &
               '        SDSHCK =',F5.2,', SDSBR = ',E12.4,       &
               ', SDSSTRAIN =',F5.1,', SDSSTRAINA =',F4.1,       &
               ', SDSSTRAIN2 =',F5.1,', '/                       &
               '        SDSBT =',F5.2,', SDSP =',F5.2,          &
               ', SDSISO =',I2, &
               ', SDSCOS =',F3.1,', SDSDTH =',F5.1,', '/         &
               '        SDSBRF1 = ',F5.2,', SDSBRFDF =',I2,', '/ &
               '        SDSBM0 = ',F5.2, ', SDSBM1 =',F5.2,      &
               ', SDSBM2 =',F5.2,', SDSBM3 =',F5.2,', SDSBM4 =', &
               F5.2,', '/,                                       &
               '        SPMSS = ',F5.2, ', SDKOF =',F5.2,        &
               ', SDSMWD =',F5.2,', SDSFACMTF =',F5.1,', '/      &
               '        SDSMWPOW =',F3.1,', SDSNMTF =', F5.2,    &
               ', SDSCUMP =', F3.1,', SDSNUW =', E8.3,', '/,     &
               '        WHITECAPWIDTH =',F5.2, ' WHITECAPDUR =',F5.2,' /')
#endif
!
#ifdef W3_ST6
  924 FORMAT (/'  Dissipation (Rogers et al. 2012) ',A/             &
               ' --------------------------------------------------')
  925 FORMAT ( '  normalise by threshold spectral density    :  ',A/&
               '  normalise by spectral density              :  ',A/&
               '  coefficient and exponent  for                 '/  &
               '   inherent breaking term a1, L as in (21)   : ',E9.3,I3/ &
               '   cumulative breaking term a2, M as in (22) : ',E9.3,I3/ &
               ' ')
 2924 FORMAT ( '  &SDS6 SDSET = ',L,', SDSA1 = ',E9.3,              &
               ', SDSA2 = ',E9.3,', SDSP1 = ',I2,', SDSP1 = ',      &
               I2,' /'                                              )

  937 FORMAT (/'  Swell dissipation ',A/                            &
               ' --------------------------------------------------')
  940 FORMAT ( '  subroutine W3SWL6 activated           : ',A/      &
               '   coefficient b1 ',A,                ' : ',E9.3/   )
 2937 FORMAT ( '  &SWL6 SWLB1 = ',E9.3,', CSTB1 = ',L,' /')
#endif
!
#ifdef W3_BT0
  926 FORMAT (/'  Bottom friction not defined.'/)
#endif
!
#ifdef W3_BT1
  926 FORMAT (/'  Bottom friction (JONSWAP) ',A/                 &
               ' --------------------------------------------------')
  927 FORMAT ( '       gamma                       :',F8.4/)
 2926 FORMAT ( '  &SBT1 GAMMA =',E12.4,' /')
#endif
!
#ifdef W3_BT4
  926 FORMAT (/'  Bottom friction  (SHOWEX)  ',A/                 &
               ' --------------------------------------------------')
  927 FORMAT ( '       SEDMAPD50, SED_D50_UNIFORM        :',L3,1X,F8.6/ &
               '       RIPFAC1,RIPFAC2,RIPFAC3,RIPFAC4   :',4F8.4/      &
               '       SIGDEPTH, BOTROUGHMIN, BOTROUGHFAC:',3F8.4/)          
 2926 FORMAT ( '  &SBT4 SEDMAPD50 =',L3,', SED_D50_UNIFORM =',F8.6,','/ &
               '        RIPFAC1 =',F8.4,', RIPFAC2 =',F8.4,      &
               ', RIPFAC3 =',F8.4,', RIPFAC4 =',F8.4,','/        &
               '        SIGDEPTH =',F8.4,', BOTROUGHMIN =',F8.4, & 
               ', BOTROUGHFAC =',F4.1,' /')
#endif
!
#ifdef W3_DB0
  928 FORMAT (/'  Surf breaking not defined.'/)
#endif
!
#ifdef W3_DB1
  928 FORMAT (/'  Surf breaking (B&J 1978) ',A/                  &
               ' --------------------------------------------------')
  929 FORMAT ( '       alpha                       :',F8.3/      &
               '       gamma                       :',F8.3)
 2928 FORMAT ( '  &SDB1 BJALFA =',F7.3,', BJGAM =',F7.3,         &
               ', BJFLAG = ',A,' /')
#endif
!
#ifdef W3_TR0
  930 FORMAT (/'  Triad interactions not defined.'/)
#endif
!
#ifdef W3_BS0
  932 FORMAT (/'  Bottom scattering not defined.'/)
#endif
#ifdef W3_BS1
  932 FORMAT (/'  Experimental bottom scattering (F. Ardhuin).'/)
#endif
!
#ifdef W3_IC1
  935 FORMAT (/'  Dissipation via ice parameters (SIC1).'&
           ,/' --------------------------------------------------')
#endif
!
#ifdef W3_IC2
  935 FORMAT (/'  Dissipation via ice parameters (SIC2).'&
           ,/' --------------------------------------------------')
#endif
!
#ifdef W3_IC3
  935 FORMAT (/'  Dissipation via ice parameters (SIC3).'&
           ,/' --------------------------------------------------')
#endif
!
#ifdef W3_IC4
  935 FORMAT (/'  Dissipation via ice parameters (SIC4).'&
           ,/' --------------------------------------------------')
#endif
!
#ifdef W3_IC5
  935 FORMAT (/'  Dissipation via ice parameters (SIC5).'&
           ,/' --------------------------------------------------')
#endif
!
#ifdef W3_IS0
 944 FORMAT  (/'  Ice scattering not defined.'/)
#endif
#ifdef W3_IS1
 945 FORMAT  (/'  Ice scattering ',A,/ &
              ' --------------------------------------------------')
 946 FORMAT  ('  Isotropic (linear function of ice concentration)'/&
              '        slope                      : ',E10.3/ &
              '        offset                     : ',E10.3)
 2946 FORMAT ( '  &SIS1 ISC1 =',E9.3,', ISC2 =',E9.3)
#endif
#ifdef W3_IS2
 947 FORMAT  (/'  Ice scattering ',A,/ &
              ' --------------------------------------------------')
 948 FORMAT  ('  IS2 Scattering ... '/&
              '        scattering coefficient       : ',E9.3/ &
              '        0: no back-scattering        : ',E9.3/ &
              '     TRUE: istropic back-scattering  : ',L3/   &
              '     TRUE: update of ICEDMAX         : ',L3/   &
              '     TRUE: keeps updated ICEDMAX     : ',L3/   &
              '        flexural strength            : ',E9.3/ &
              '     TRUE: uses Robinson-Palmer disp.: ',L3/   &
              '        attenuation                  : ',F5.2/ &
              '        fragility                    : ',F5.2/ &
              '        minimum floe size in meters  : ',F5.2/ &
              '        pack scattering coef 1       : ',F5.2/ &
              '        pack scattering coef 2       : ',F5.2/ &
              '        scaling by concentration     : ',F5.2/ &
              '        creep B coefficient          : ',E9.3/ &
              '        creep C coefficient          : ',F5.2/ &
              '        creep D coefficient          : ',F5.2/ &
              '        creep N power                : ',F5.2/ &
              '        elastic energy factor        : ',F5.2/ &
              '        factor for ice breakup       : ',F5.2/ &
              '        IS2WIM1                      : ',F5.2/ &
              '        anelastic dissipation        : ',L3/   &
              '        energy of activation         : ',F5.2/ &
              '        anelastic coefficient        : ',E11.3/ &
              '        anelastic exponent           : ',F5.2)
 2948 FORMAT ( '  &SIS2 ISC1 =',E9.3,', IS2BACKSCAT =',E9.3,   &
               ', IS2ISOSCAT =',L3,', IS2BREAK =',L3,          &
               ', IS2DUPDATE =',L3,','/                        &
               '       IS2FLEXSTR =',E11.3,', IS2DISP =',L3,   &
               ', IS2DAMP =',F3.1,       &
               ', IS2FRAGILITY =',F4.2,', IS2DMIN =',F5.2,','/ &
               '       IS2C2 =',F12.8,', IS2C3 =',F8.4,        &
               ', IS2CONC =',F5.1,', IS2CREEPB =',E11.3,','/   &
               '       IS2CREEPC =',F5.2,', IS2CREEPD =',F5.2, &
               ', IS2CREEPN =',F5.2,','/                       &
               '       IS2BREAKE =',F5.2,                      &
               ', IS2BREAKF =',F5.2,', IS2WIM1 =',F5.2,','/    &
               ', IS2ANDISB =',L3,', IS2ANDISE =',F5.2,        &
               ', IS2ANDISD =',E11.3,', IS2ANDISN=',F5.2, ' /')
#endif
#ifdef W3_UOST
 4500 FORMAT (/' Unresolved Obstacles Source Term (UOST) ',A,/ &
              ' --------------------------------------------------')
 4501 FORMAT ('    local alpha-beta file:  ',A,    &
              '    shadow alpha-beta file: ',A,/    &
              '    local calibration factor: ',F5.2,  &
              '    shadow calibration factor: ',F5.2)
 4502 FORMAT ('  &UOST UOSTFILELOCAL = ',A,', UOSTFILESHADOW = ',A,/ &
              '        UOSTFACTORLOCAL = ',F5.2', UOSTFACTORSHADOW = ',F5.2,' /')
#endif
!
  950 FORMAT (/'  Propagation scheme : '/                             &
               ' --------------------------------------------------')
  951 FORMAT ( '       Type of scheme (structured) :',1X,A)
 2951 FORMAT ( '       Type of scheme(unstructured):',1X,A)
 2952 FORMAT ( '             wave setup computation:',1X,A)
  952 FORMAT ( '                                    ',1X,A)
#ifdef W3_PR1
  953 FORMAT ( '       CFLmax depth refraction     :',F9.3/)
 2953 FORMAT ( '  &PRO1 CFLTM =',F5.2,' /')
#endif
!
#ifdef W3_PR2
  953 FORMAT ( '       CFLmax depth refraction     :',F9.3/      &
               '       Effective swell age     (h) : switched off'/   &
               '       Cut-off latitude    (degr.) :',F7.1/)
  954 FORMAT ( '       CFLmax depth refraction     :',F9.3/      &
               '       Effective swell age     (h) :',F8.2/      &
               '       Cut-off latitude    (degr.) :',F7.1/)
 2953 FORMAT ( '  &PRO2 CFLTM =',F5.2,', DTIME =',F8.0,          &
                        ', LATMIN =',F5.1,' /')
#endif
!
#ifdef W3_SMC
 1950 FORMAT (/'  SMC grid parameters : '/                            &
               ' --------------------------------------------------')
 1951 FORMAT ( '       Type of scheme (structured) :',1X,A)
 1953 FORMAT ( '       Max propagation CFL number  :',F9.3/      &
               '       Effective swell age     (h) :',F8.2/      &
               '       Maximum refraction  (degr.) :',F8.2/)
 2954 FORMAT ( '  &PSMC CFLSM  =',F5.2,', DTIMS  =', F9.1/     &
               '        Arctic =',L5,  ', RFMAXD =', F9.2/     & 
               '        UNO3   =',L5,  ', AVERG  =',L5/        &
               '        LvSMC  =',i5,  ', NBISMC =',i9/        &
               '        ISHFT  =',i5,  ', JEQT   =',i9/        &
               '        SEAWND =',L5,  '/')
#endif
!
#ifdef W3_PR3
  953 FORMAT ( '       CFLmax depth refraction     :',F9.3/      &
               '       Averaging area factor Cg    :',F8.2)
  954 FORMAT ( '       Averaging area factor theta :',F8.2)
  955 FORMAT ( '            **** Internal maximum .GE.',F6.2,' ****')
 2953 FORMAT ( '  &PRO3 CFLTM =',F5.2,                           &
                      ', WDTHCG = ',F4.2,', WDTHTH = ',F4.2,' /')
#endif
!
 2956 FORMAT ( '  &UNST UGBCCFL =',L3,', UGOBCAUTO =',L3,             &
               ', UGOBCDEPTH =', F8.3,', UGOBCFILE=',A,','/           &
               ',  EXPFSN =',L3,',EXPFSPSI =',L3,                     &
               ',  EXPFSFCT =', L3,',IMPFSN =',L3,',EXPTOTAL=',L3,    &
               ',  IMPTOTAL=',L3,',IMPREFRACTION=', L3,               &
               ',  IMPFREQSHIFT=', L3,', IMPSOURCE=', L3,             &
               ',  SETUP_APPLY_WLV=', L3,                             &
               ',  JGS_TERMINATE_MAXITER=', L3,                       &
               ',  JGS_TERMINATE_DIFFERENCE=', L3,                    &
               ',  JGS_TERMINATE_NORM=', L3,                          &
               ',  JGS_LIMITER=', L3,                                 &
               ',  JGS_USE_JACOBI=', L3,                              &
               ',  JGS_BLOCK_GAUSS_SEIDEL=', L3,                      &
               ',  JGS_MAXITER=', I5,                                 &
               ',  JGS_PMIN=', F8.3,                                  &
               ',  JGS_DIFF_THR=', F8.3,                              &
               ',  JGS_NORM_THR=', F8.3,                              &
               ',  JGS_NLEVEL=', I3,                                  &
               ',  JGS_SOURCE_NONLINEAR=', L3 / )
!
  960 FORMAT (/'  Miscellaneous ',A/                                   &
               ' --------------------------------------------------')
 2961 FORMAT ( ' *** WAVEWATCH-III WARNING IN W3GRID :'/               &
               '     CICE0.NE.CICEN requires FLAGTR>2'/                &
               '     Parameters corrected: CICE0 = CICEN'/)
 2962 FORMAT (/' *** WAVEWATCH-III WARNING IN W3GRID : User requests', &
         'CICE0=CICEN corresponding to discontinuous treatment of ',   &
         'ice, so we will change FLAGTR')
 2963 FORMAT (/' *** WAVEWATCH-III WARNING IN W3GRID :'/               &
               '     Ice physics used, so we will change FLAGTR.')
  961 FORMAT ( '       Ice concentration cut-offs  :',F8.2,F6.2)
#ifdef W3_MGG
  962 FORMAT  ( '       Moving grid GSE cor. power  :',F8.2)
#endif
#ifdef W3_SCRIP
  963 FORMAT( ' Grid offset for multi-grid w/SCRIP  : ',E11.3)
#endif
 1972 FORMAT ( '       Compression of track output  : ',L3)
#ifdef W3_SEED
  964 FORMAT ( '       Xseed in seeding algorithm  :',F8.2)
#endif
  965 FORMAT (/'    Dynamic source term integration scheme :'/        &
               '       Xp                      (-) :',F9.3/           &
               '       Xr                      (-) :',F9.3/           &
               '       Xfilt                   (-) :',F9.3)
  966 FORMAT (/'    Wave field partitioning :'/                       &
               '       Levels                  (-) :',I5/             &
               '       Minimum wave height     (m) :',F9.3/           &
               '       Wind area multiplier    (-) :',F9.3/           &
               '       Cut-off wind sea fract. (-) :',F9.3/           &
               '       Combine wind seas           :  ',A/            &
               '       Number of swells in fld out :',I5)
  967 FORMAT (/'    Miche-style limiting wave height :'/              &
               '       Hs,max/d factor         (-) :',F9.3/           &
               '       Hrms,max/d factor       (-) :',F9.3/           &
               '       Limiter activated           :  ',A)
  968 FORMAT ( '          *** FACTOR DANGEROUSLY LOW ***')
 1973 FORMAT (/'    Calendar type                  :  ',A)
!
#ifdef W3_REF1
  969 FORMAT (/'  Shoreline reflection     ',A/                  &
               ' --------------------------------------------------')
#endif
!
#ifdef W3_IG1
  970 FORMAT (/'  Second order and infragravity waves  ',A/      &
               ' --------------------------------------------------')
#endif
!
 5971 FORMAT ('       Partitioning method         :  ',A)
 5972 FORMAT ('       Namelist options overridden :  ',A)
!
#ifdef W3_IC2
  971 FORMAT (/'  Boundary layer below ice  ',A/      &
               ' --------------------------------------------------')
#endif
#ifdef W3_IC3
  971 FORMAT (/'  Visco-elastic ice layer   ',A/      &
               ' --------------------------------------------------')
#endif
#ifdef W3_IC4
  971 FORMAT (/'  Empirical wave-ice physics   ',A/      &
               ' --------------------------------------------------')
#endif
#ifdef W3_IC5
  971 FORMAT (/'  Effective medium ice model (SIC5) ',A/      &
               ' --------------------------------------------------')
 2971 FORMAT ( '       Min. Ice shear modulus G     : ', E10.1/, &
               '       Min. Wave period T           : ', F7.2/,  &
               '       Max. Wavenumber Ratio (Ko/Kr): ', E10.1/, &
               '       Max. Attenu. Rate (Ki)       : ', E10.1/, &
               '       Min. Water depth (d)         : ', F5.0/,  &
               '       Max. # of Newton Iter.       : ', F5.0/,  &
               '       Use Rand. Kick               : ', F5.0/,  &
               '       Excluded Imag. Corridor      : ', F9.4/,  &
               '       Selected ice model           : ', A/)
#endif
!
 8972 FORMAT ( '       Wind input reduction factor in presence of ', &
               /'         ice :',F6.2, &
               /'         (0.0==> no reduction and 1.0==> no wind', &
               /'         input with 100% ice cover)')
!
!
 4970 FORMAT (/'  Spectral output on full grid ',A/                   &
               ' --------------------------------------------------')
 4971 FORMAT ( '       Second order pressure at K=0:',3I4)
 4972 FORMAT ( '       Spectrum of Uss             :',3I4)
 4973 FORMAT ( '       Frequency spectrum          :',3I4)
 4974 FORMAT ( '       Partions of Uss             :',2I4)
 4975 FORMAT ( '       Partition wavenumber #',I2,'   : ',1F6.3)

!
 4980 FORMAT (/'  Coastal / iceberg reflection  ',A/                   &
               ' --------------------------------------------------')
 4981 FORMAT ( '       Coefficient for shorelines  :',F6.4)
 4989 FORMAT ( '          *** CURVLINEAR GRID: REFLECTION NOT IMPLEMENTED YET ***')
 2977 FORMAT ( '  &SIG1  IGMETHOD =',I2,', IGADDOUTP =',I2,', IGSOURCE =',I2, &
               ', IGSTERMS = ',I2,', IGBCOVERWRITE =', L3,','/        & 
               '        IGSWELLMAX =', L3,', IGMAXFREQ =',F6.4,       &
               ', IGSOURCEATBP = ',I2,', IGKDMIN = ',F6.4,','/        &
               '        IGFIXEDDEPTH = ',F6.2,', IGEMPIRICAL = ',F8.6,' /')
!
 2978 FORMAT ( '  &SIC2  IC2DISPER =',L3,', IC2TURB =',F6.2,          &
               ', IC2ROUGH  =',F10.6,','/                             &
               '        IC2REYNOLDS = ',F10.1,', IC2SMOOTH = ',F10.1, & 
               ', IC2VISC =',F6.3,','/                                &
               ',       IC2TURBS =',F8.2,', IC2DMAX =',F5.3,' /')
!
 2979 FORMAT ( '  &SIC3 IC3MAXTHK =',F6.2, ', IC3MAXCNC =',F6.2,','/  &
               '        IC2TURB =',F8.2,                              &
               ', IC2ROUGH  =',F7.3,','/                              &
               '        IC2REYNOLDS = ',F10.1,', IC2SMOOTH = ',F10.1, & 
               ', IC2VISC =',F10.3,','/                               &
               '        IC2TURBS =',F8.2,', IC3CHENG =',L3,           &
               ', USECGICE =',L3,', IC3HILIM = ',F6.2,','/            &
               '        IC3KILIM = ',E9.2,', IC3HICE = ',E9.2,        &
               ', IC3VISC = ',E9.2,','/                               &
               '        IC3DENS = ',E9.2,', IC3ELAS = ',E9.2,' /')
!
 2981 FORMAT ( '  &SIC5 IC5MINIG = ', E9.2, ', IC5MINWT = ', F5.2,    &
               ', IC5MAXKRATIO = ', E9.2, ','/                        &
               '        IC5MAXKI = ', E9.2, ', IC5MINHW = ', F4.0,    &
               ', IC5MAXITER = ', F4.0, ','/                          &
               '        IC5RKICK = ', F2.0, ', IC5KFILTER = ', F7.4,  &
               ', IC5VEMOD   = ', F4.0, ' /')
!
 2966 FORMAT ( '  &MISC CICE0 =',F6.3,', CICEN =',F6.3,               &
                     ', LICE = ',F8.1,', PMOVE =',F6.3,','/           &
               '        XSEED =',F6.3,', FLAGTR = ', I1,              &
                     ', XP =',F6.3,', XR =',F6.3,', XFILT =', F6.3 /  &
               '        IHM =',I5,', HSPM =',F6.3,', WSM =',F6.3,     &
                     ', WSC =',F6.3,', FLC = ',A/                     &
               '        NOSW =',I3,', FMICHE =',F6.3,', RWNDC =' ,    &
                        F6.3,', WCOR1 =',F6.2,', WCOR2 =',F6.2,','/   &
               '        FACBERG =',F4.1,', GSHIFT = ',E11.3,          &
                     ', STDX = ' ,F7.2,', STDY =',F7.2,','/           &
               '        STDT =', F8.2,                                &
                     ', ICEHMIN =',F5.2,', ICEHFAC =',F5.2,','/       &
               '        ICEHINIT =',F5.2,', ICEDISP =',L3,            &
                     ', ICEHDISP =',F5.2,','/                         &
               '        ICESLN = ',F6.2,', ICEWIND = ',F6.2,          &
                     ', ICESNL = ',F6.2,', ICESDS = ',F5.2,','/       &
               '        ICEDDISP = ',F5.2,', ICEFDISP = ',F5.2,       &
                     ', CALTYPE = ',A8,' , TRCKCMPR = ', L3,','/      &
               '        BTBET  = ', F6.2, ' /')
!
 2976 FORMAT ( '  &OUTS P2SF  =',I2,', I1P2SF =',I2,', I2P2SF =',I3,','/&    
               '        US3D  =',I2,', I1US3D =',I3,', I2US3D =',I3,','/&  
               '        USSP  =',I2,', IUSSP  =',I3,','/&
               '        E3D   =',I2,', I1E3D  =',I3,', I2E3D  =',I3,','/&  
               '        TH1MF =',I2,', I1TH1M =',I3,', I2TH1M =',I3,','/&  
               '        STH1MF=',I2,', I1STH1M=',I3,', I2STH1M=',I3,','/&  
               '        TH2MF =',I2,', I1TH2M =',I3,', I2TH2M =',I3,','/&  
               '        STH2MF=',I2,', I1STH2M=',I3,', I2STH2M=',I3,' /')
!
 2986 FORMAT ( '  &REF1 REFCOAST =',F5.2,', REFFREQ =',F5.2,', REFSLOPE =',F5.3, &
               ', REFMAP =',F4.1, ', REFMAPD =',F4.1, ', REFSUBGRID =',F5.2,','/ &   
               '        REFRMAX=',F5.2,', REFFREQPOW =',F5.2,                    &
               ', REFICEBERG =',F5.2,', REFCOSP_STRAIGHT =',F4.1,' /')
!
 2987 FORMAT ( '  &FLD TAIL_ID =',I1,' TAIL_LEV =',F5.4,' TAILT1 =',F5.3,&
               ' TAILT2 =',F5.3,' /')
#ifdef W3_RTD

 4991 FORMAT ( '  &ROTD PLAT =', F6.2,', PLON =', F7.2,', UNROT =',L3,' /')
 4992 FORMAT ( '  &ROTB BPLAT =',9(F6.1,",")/                        &
               '        BPLON =',9(F6.1,","),' /')
#endif
      
 3000 FORMAT (/'  The spatial grid: '/                                &
               ' --------------------------------------------------'/ &
              /'       Grid type                   : ',A)
 3001 FORMAT ( '       Coordinate system           : ',A)
 3002 FORMAT ( '       Index closure type          : ',A)
 3003 FORMAT ( '       Dimensions                  : ',I6,I8)
 3004 FORMAT (/'       Increments           (deg.) :',2F10.4/         &
               '       Longitude range      (deg.) :',2F10.4/         &
               '       Latitude range       (deg.) :',2F10.4)
 3005 FORMAT ( '       Increments             (km) :',2F8.2/          &
               '       X range                (km) :',2F8.2/          &
               '       Y range                (km) :',2F8.2)
 3006 FORMAT (/'       X-coordinate unit           :',I6/             &
               '       Scale factor                :',F10.4/           &
               '       Add offset                  :',E12.4/          &
               '       Layout indicator            :',I6/             &
               '       Format indicator            :',I6)
 3007 FORMAT (/'       Y-coordinate unit           :',I6/             &
               '       Scale factor                :',F10.4/           &
               '       Add offset                  :',E12.4/          &
               '       Layout indicator            :',I6/             &
               '       Format indicator            :',I6)
 3008 FORMAT ( '       Format                      : ',A)
 3009 FORMAT ( '       File name                   : ',A)
#ifdef W3_SMC
 4001 FORMAT ( '       SMC refined levels NRLv   = ',I8) 
 4002 FORMAT ( '       SMC Equator j shift no.   = ',I8) 
 4302 FORMAT ( '       SMC I-index shift number  = ',I8) 
 4003 FORMAT ( '       SMC input boundary  no.   = ',I8)
 4004 FORMAT ( '       SMC NCel   = ',6I9)  
 4005 FORMAT ( '       IJKCel(5,NCel) read from ', A)
 4006 FORMAT (6I8)
 4007 FORMAT ( '       SMC NUFc   = ',6I9)
 4008 FORMAT ( '       IJKUFc(7,NCel) read from ', A)
 4009 FORMAT (8I8)
 4010 FORMAT ( '       SMC NVFc   = ',6I9)
 4011 FORMAT ( '       IJKVFc(8,NCel) read from ', A)
 4110 FORMAT ( '       SMC NCObsr = ',6I9)
 4111 FORMAT ( '       IJKObstr(1,NCel) read from ', A)
 4012 FORMAT (9I8)
 4013 FORMAT ( '       NBICelin(NBISMC) read from ', A)
 4014 FORMAT (2I8)
 4015 FORMAT ( '       ARC NARC   = ',6I9)
 4016 FORMAT ( '       IJKCel(5,NARC) read from ', A)
 4017 FORMAT ( '       ARC NAUI   = ',6I9)
 4018 FORMAT ( '       IJKUFc(7,NAUI) read from ', A)
 4019 FORMAT ( '       ARC NAVJ   = ',6I9)
 4020 FORMAT ( '       IJKVFc(8,NAVJ) read from ', A)
 4021 FORMAT ( '       Varables by W3DIMX NCel = ',I9)
 4022 FORMAT ( '       Defined NLvCel ',6I9)
 4023 FORMAT ( '       Defined NLvUFc ',6I9)
 4024 FORMAT ( '       Defined NLvVFc ',6I9)
 4025 FORMAT ( '       Define IJKCel from -9 to ',I9)
 4026 FORMAT ( '       IJKCel(5,NCel) defined : ')
 4027 FORMAT ( '       IJKUFc(7,NUFc) defined : ')
 4028 FORMAT ( '       IJKVFc(8,NVFc) defined : ')
 4029 FORMAT ( '       Boundary cells IJKCel(:,-9:0) : ')
 4030 FORMAT (5I8)
 4031 FORMAT ( '       Define MAPSF ...    1 to ',I9)
 4032 FORMAT ( '       Multi-Resolution factor = ',I6)
 4033 FORMAT ( '       Range of MAPSF(:,1)    : ',2I9)
 4034 FORMAT ( '       Range of MAPSF(:,2)    : ',2I9)
 4035 FORMAT ( '       Range of MAPSF(:,3)    : ',2I9)
 4036 FORMAT ( '       Range of MAPFS(:,:)    : ',2I9)
 4037 FORMAT ( '       Arctic AngArc defined as ',I6)
 4038 FORMAT (9F8.2)
 4039 FORMAT ( '       Arctic ICLBAC defined as ',I6)
 4040 FORMAT (9I8)
#endif
#ifdef W3_RTD
 4200 FORMAT ( '       AnglDin(NX,NY) defn checks  : ')
 4201 FORMAT ( '       JY/IX',4I8)
 4202 FORMAT (I12,4F8.2)
 4203 FORMAT ( '       Rotated pole lat/lon (deg.) :  ',2F9.3)
 4204 FORMAT ( '       Output dirns and x-y vectors will be set to True North')
#endif
  972 FORMAT (/'       Bottom level unit           :',I6/             &
               '       Limiting depth          (m) :',F8.2/           &
               '       Minimum depth           (m) :',F8.2/           &
               '       Scale factor                :',F8.2/           &
               '       Layout indicator            :',I6/             &
               '       Format indicator            :',I6)
  973 FORMAT ( '       Format                      : ',A)
  974 FORMAT ( '       File name                   : ',A)
  976 FORMAT (/'       Sub-grid information        : ',A)
  977 FORMAT ( '       Obstructions unit           :',I6/             &
               '       Scale factor                :',F10.4/          &
               '       Layout indicator            :',I6/             &
               '       Format indicator            :',I6)
  978 FORMAT (/'       Mask information            : From file.'/     &
               '       Mask unit                   :',I6/             &
               '       Layout indicator            :',I6/             &
               '       Format indicator            :',I6)
 1977 FORMAT ( '       Shoreline slope             :',I6/             &
               '       Scale factor                :',F10.4/          &
               '       Layout indicator            :',I6/             &
               '       Format indicator            :',I6)
 1978 FORMAT ( '       Grain sizes                 :',I6/             &
               '       Scale factor                :',F10.4/          &
               '       Layout indicator            :',I6/             &
               '       Format indicator            :',I6)
!
  979 FORMAT ( '  Processing ',A)
  980 FORMAT (/'  Input boundary points : '/                          &
               ' --------------------------------------------------')
 1980 FORMAT (/'  Excluded points : '/                                &
               ' --------------------------------------------------')
  981 FORMAT ( '   *** POINT OUTSIDE GRID (SKIPPED), IX, IY =')
 1981 FORMAT ( '   *** POINT ALREADY EXCLUDED (SKIPPED), IX, IY =')
  982 FORMAT ( '   *** CANNOT CONNECT POINTS, IX, IY =')
  985 FORMAT ( '       No boundary points.'/)
  986 FORMAT ( '       Number of boundary points   :',I6/)
 1985 FORMAT ( '       No excluded points.'/)
 1986 FORMAT ( '       Number of excluded points   :',I6/)
  987 FORMAT ( '         Nr.|   IX  |   IY  |  Long.  |   Lat.  '/        &
               '       -----|-------|-------|---------|---------')
 1987 FORMAT ( '         Nr.|   IX  |   IY  |     X     |     Y     '/    &
               '       -----|-------|-------|-----------|-----------')
  988 FORMAT ( '       ',I4,2(' |',I6),2(' |',F8.2))
 1988 FORMAT ( '       ',I4,2(' |',I6),2(' |',F8.1,'E3'))
  989 FORMAT ( ' ')
!
  990 FORMAT (/'  Output boundary points : '/                         &
               ' --------------------------------------------------')
  991 FORMAT ( '       File nest',I1,'.ww3  Number of points  :',I6/  &
               '                       Number of spectra :',I6)
 1991 FORMAT ( '                       Dest. grid Polat:',F6.2,', Polon:',F8.2)
  992 FORMAT (/'         Nr.|  Long.  |   Lat.  '/               &
               '       -----|---------|---------')
 1992 FORMAT (/'         Nr.|  Long.  |   Lat.  ',               &
               '         Nr.|  Long.  |   Lat.  '/               &
               '       -----|---------|---------',               &
               '       -----|---------|---------')
  993 FORMAT ( '       ',I4,2(' |',F8.2))
 1993 FORMAT ( '       ',I4,2(' |',F8.2),                        &
              '        ',I4,2(' |',F8.2))
  994 FORMAT ( '   *** POINT OUTSIDE GRID (SKIPPED) : X,Y =',2F10.5)
  995 FORMAT ( '   *** POINT ON LAND      (SKIPPED) : X,Y =',2F10.5)
 2992 FORMAT (/'         Nr.|     X     |     Y     '/           &
               '       -----|-----------|-----------')
 3992 FORMAT (/'         Nr.|     X     |     Y     ',           &
                 '       Nr.|     X     |     Y     '/           &
               '       -----|-----------|-----------',           &
                 '     -----|-----------|-----------')
 2993 FORMAT ( '       ',I4,2(' |',F8.1,'E3'))
 3993 FORMAT ( '       ',I4,2(' |',F8.1,'E3'),                   &
                '      ',I4,2(' |',F8.1,'E3'))
 2994 FORMAT ( '   *** POINT OUTSIDE GRID (SKIPPED) : X,Y =',2(F8.1,'E3'))
 2995 FORMAT ( '   *** POINT ON LAND      (SKIPPED) : X,Y =',2(F8.1,'E3'))
  996 FORMAT ( '       No boundary points.'/)
  997 FORMAT ( '       Number of boundary points   :',I6/             &
               '       Number of spectra           :',I6/)
!
#ifdef W3_O2a
  998 FORMAT (50I2)
#endif
#ifdef W3_O2c
 1998 FORMAT (50I2)
#endif
!
  999 FORMAT (/'  Writing model definition file ...'/)
!
 1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID : '/               &
               '     ERROR IN OPENING INPUT FILE'/                    &
               '     IOSTAT =',I5/)
!
 1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID : '/               &
               '     PREMATURE END OF INPUT FILE'/)
!
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID : '/               &
               '     ERROR IN READING FROM INPUT FILE'/               &
               '     IOSTAT =',I5/)
!
 1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID : '/               &
               '     INVALID CALENDAR TYPE: SELECT ONE OF:',          &
               '     standard, 360_day, or 365_day '/)
!
 1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID : '/               &
               '     CANNOT READ UNFORMATTED (IDFM = 3) FROM UNIT',   &
               I4,' (ww3_grid.inp)'/)
!
 1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID : '/               &
               '     BOTTOM AND OBSTRUCTION DATA FROM SAME FILE '/    &
               '     BUT WITH INCOMPATIBLE FORMATS (',I1,',',I1,')'/)
!
 1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     TOO MANY NESTING OUTPUT FILES '/)
!
 1007 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
               '     ILLEGAL GRID TYPE:',A4)
!
 1008 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
               '     A CARTESIAN WITH CLOSURE IS NOT ALLOWED')
!
 1009 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
               '     A RECTILINEAR TRIPOLE GRID IS NOT ALLOWED')
!
 1010 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'//               &
               '     NO PROPAGATION + NO SOURCE TERMS = NO WAVE MODEL'// &
               '     ( USE DRY RUN FLAG TO TEMPORARILY SWITCH OFF ',  &
               'CALCULATIONS )'/)
!
 1011 FORMAT (/' *** WAVEWATCH-III WARNING IN W3GRID :'/              &
               '     LEFT-HANDED GRID -- POSSIBLE CAUSE IS WRONG '/   &
               '     IDLA:',I4,' . THIS MAY PRODUCE ERRORS '/         &
               '     (COMMENT THIS EXTCDE AT YOUR OWN RISK).')
!
 1012 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
               '     ILLEGAL GRID CLOSURE TYPE:',A4)
!
 1013 FORMAT (/' *** WAVEWATCH-III WARNING IN W3GRID :'/              &
               '     THE GLOBAL (LOGICAL) INPUT FLAG IS DEPRECATED'/  &
               '     AND REPLACED WITH A STRING INDICATING THE TYPE'/ &
               '     OF GRID INDEX CLOSURE (NONE, SMPL or TRPL).'/    &
               ' *** PLEASE UPDATE YOUR GRID INPUT FILE ACCORDINGLY ***'/)
!
#ifdef W3_SMC
 1014 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
               '   SMC CELL LONGITUDE RANGE OUTSIDE BASE GRID RANGE:'/&
               '   ISEA =', I6, '; IX =', I4, ':', I4,'; NX =', I4/)
#endif
!
#ifdef W3_SMC
 1015 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
               '   SMC CELL LATITUDE RANGE OUTSIDE BASE GRID RANGE: '/&
               '   ISEA =', I6, '; IY =', I4, ':', I4,'; NY =', I4/)
#endif
!
 1020 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRID :'/                &
               '     SOURCE TERMS REQUESTED BUT NOT SELECTED'/)
 1021 FORMAT (/' *** WAVEWATCH III WARNING IN W3GRID :'/              &
               '     SOURCE TERMS SELECTED BUT NOT REQUESTED'/)
 1022 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     ILLEGAL NUMBER OF !/LNn OR SEED SWITCHES :',I3)
 1023 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     ILLEGAL NUMBER OF !/STn SWITCHES :',I3)
 1024 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     ILLEGAL NUMBER OF !/NLn SWITCHES :',I3)
 1025 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     ILLEGAL NUMBER OF !/BTn SWITCHES :',I3)
 1026 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     ILLEGAL NUMBER OF !/DBn SWITCHES :',I3)
 1027 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     ILLEGAL NUMBER OF !/TRn SWITCHES :',I3)
 1028 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     ILLEGAL NUMBER OF !/BSn SWITCHES :',I3)
!
 1030 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     PROPAGATION REQUESTED BUT NO SCHEME SELECTED '/)
 1031 FORMAT (/' *** WAVEWATCH III WARNING IN W3GRID :'/              &
               '     NO PROPAGATION REQUESTED BUT SCHEME SELECTED '/)
 1032 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     NO PROPAGATION SCHEME SELECTED ( use !/PR0 ) '/)
 1033 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     MULTIPLE PROPAGATION SCHEMES SELECTED :',I3/     &
               '     CHECK !/PRn SWITCHES'/)
 1034 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     ILLEGAL NUMBER OF !/ICn SWITCHES :',I3)
 1035 FORMAT (/' *** WAVEWATCH III WARNING IN W3GRID :'/              &
               '     ONLY FIRST PROPAGATION SCHEME WILL BE USED: ')
 1036 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     ILLEGAL NUMBER OF !/ISn SWITCHES :',I3)
#ifdef W3_RTD
 1052 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     WITH NAMELIST VALUE PLAT == 90, PLON MUST BE -180'/ &
               '     AND UNROT MUST BE .FALSE.' )
#endif
!
#ifdef W3_RTD
 1053 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRID :'/                &
               '     WITH NAMELIST VALUE BPLAT == 90, BPLON MUST BE -180')
#endif
!
 1040 FORMAT ( '       Space-time extremes DX      :',F10.2)
 1041 FORMAT ( '       Space-time extremes DX      :',F10.2)
 1042 FORMAT ( '       Space-time extremes DX-Y set to default 1000 m')
 1043 FORMAT ( '       Space-time extremes Dt      :',F8.2)
 1044 FORMAT ( '       Space-time extremes Dt set to default 1200 s')
!
 1100 FORMAT (/'  Status map, printed in',I6,' part(s) '/             &
               ' -----------------------------------'/)
 1101 FORMAT (2X,180I2)
 1102 FORMAT ( '  Legend : '/                                         &
               ' -----------------------------'/                      &
               '    0 : Land point            '/                      &
               '    1 : Sea point             '/                      &
               '    2 : Active boundary point '/                      &
               '    3 : Excluded point        '/)
 1103 FORMAT (/'  Obstruction map ',A1,', printed in',I6,' part(s) '/ &
               ' ---------------------------------------------'/)
 1104 FORMAT ( '  Legend : '/                                         &
               ' --------------------------------'/                   &
               '    fraction of obstruction * 10 '/)

 1105 FORMAT (/'  Shoreline slope, printed in',I6,' part(s) '/ &
               ' ---------------------------------------------'/)
 1106 FORMAT ( '  Legend : '/                                         &
               ' --------------------------------'/                   &
               '   Slope * 100'/)


 1150 FORMAT (/'  Reading unstructured grid definition files ...'/)
!
 9997 FORMAT (/'  Summary grid statistics : '/                        &
               ' --------------------------------------------------'/ &
               '       Number of longitudes      :',I10/              &
               '       Number of latitudes       :',I10/              &
               '       Number of grid points     :',I10/              &
               '       Number of sea points      :',I10,' (',F4.1,'%)'/&
               '       Number of input b. points :',I10/              &
               '       Number of land points     :',I10/              &
               '       Number of excluded points :',I10/)
 9998 FORMAT (/'  Summary grid statistics : '/                        &
               ' --------------------------------------------------'/ &
               '       Number of longitudes      :',I10/              &
               '       Number of latitudes       :',I10/              &
               '       Number of grid points     :',I10/              &
               '       Number of sea points      :',I10,' (100%)'/    &
               '       Number of input b. points :',I10/              &
               '       Number of land points     :',I10/              &
               '       Number of excluded points :',I10/)
 9999 FORMAT (/'  End of program '/                                   &
               ' ========================================'/           &
               '         WAVEWATCH III Grid preprocessor '/)
!
#ifdef W3_T
 9090 FORMAT ( ' TEST W3GRID : OUTPUT BOUND. POINT DATA LINE SEG.')
 9091 FORMAT ( '             ',2F8.2,4(2I4,F7.2))
 9092 FORMAT ( '                      ',F7.2,2X,4F7.2)
 9093 FORMAT ( '                            ',4I7/                 &
               '                            ',4I7)
#endif
!
#ifdef W3_T0
 9095 FORMAT ( ' TEST W3GRID : OUTPUT BOUND. POINT SPEC DATA ')
 9096 FORMAT ( '            ',I3,2I8)
#endif

      END SUBROUTINE 
!/
!/ Internal function READNL ------------------------------------------ /
!/
!/ ------------------------------------------------------------------- /
      SUBROUTINE READNL ( NDS, NAME, STATUS )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Jun-2013 |
!/                  +-----------------------------------+
!/
!  1. Purpose :
!
!     Read namelist info from file if namelist is found in file.
!
!  2. Method :
!
!     Look for namelist with name NAME in unit NDS and read if found.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NDS     Int.   I   Data set number used for search.
!       NAME    C*4    I   Name of namelist.
!       STATUS  C*20   O   Status at end of routine,
!                            '(default values)  ' if no namelist found.
!                            '(user def. values)' if namelist read.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      EXTCDE    Subr. W3SERVMD Abort program as graceful as possible.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Program in which it is contained.
!
!  6. Error messages :
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: NDS
      CHARACTER, INTENT(IN)   :: NAME*4
      CHARACTER, INTENT(OUT)  :: STATUS*20
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: IERR, I, J
      CHARACTER               :: LINE*80
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'READNL')
#endif
!
      REWIND (NDS)
      STATUS  = '(default values) :  '
!
      DO
        READ (NDS,'(A)',END=800,ERR=800,IOSTAT=IERR) LINE
        DO I=1, 70 
          IF ( LINE(I:I) .NE. ' ' ) THEN
              IF ( LINE(I:I) .EQ. '&' ) THEN
                  IF ( LINE(I+1:I+4) .EQ. NAME ) THEN
                      BACKSPACE (NDS)
                      SELECT CASE(NAME)
#ifdef W3_FLD1
                        CASE('FLD1')
                          READ (NDS,NML=FLD1,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_FLD2
                        CASE('FLD2')
                          READ (NDS,NML=FLD2,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_FLX3
                        CASE('FLX3')
                          READ (NDS,NML=FLX3,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_FLX4
                        CASE('FLX4')
                          READ (NDS,NML=FLX4,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_LN1
                        CASE('SLN1')
                          READ (NDS,NML=SLN1,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_ST1
                        CASE('SIN1')
                          READ (NDS,NML=SIN1,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_ST2
                        CASE('SIN2')
                          READ (NDS,NML=SIN2,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_ST3
                        CASE('SIN3')
                          READ (NDS,NML=SIN3,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_ST4
                        CASE('SIN4')
                          READ (NDS,NML=SIN4,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_ST6
                        CASE('SIN6')
                          READ (NDS,NML=SIN6,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_NL1
                        CASE('SNL1')
                          READ (NDS,NML=SNL1,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_NL2
                        CASE('SNL2')
                          READ (NDS,NML=SNL2,END=801,ERR=802,IOSTAT=J)
                        CASE('ANL2')
                          IF ( NDEPTH .GT. 100 ) GOTO 804
                          DEPTHS(1:NDEPTH) = DPTHNL
                          READ (NDS,NML=ANL2,END=801,ERR=802,IOSTAT=J)
                          DPTHNL = DEPTHS(1:NDEPTH)
#endif
#ifdef W3_NL3
                        CASE('SNL3')
                          READ (NDS,NML=SNL3,END=801,ERR=802,IOSTAT=J)
                        CASE('ANL3')
                          IF ( NQDEF .GT. 100 ) GOTO 804
                          READ (NDS,NML=ANL3,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_NL4
                        CASE('SNL4')
                          READ (NDS,NML=SNL4,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_NL5
                        CASE('SNL5')
                          READ (NDS,NML=SNL5,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_NLS
                        CASE('SNLS')
                          READ (NDS,NML=SNLS,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_ST1
                        CASE('SDS1')
                          READ (NDS,NML=SDS1,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_ST2
                        CASE('SDS2')
                          READ (NDS,NML=SDS2,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_ST3
                        CASE('SDS3')
                          READ (NDS,NML=SDS3,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_ST4
                        CASE('SDS4')
                          READ (NDS,NML=SDS4,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_ST6
                        CASE('SDS6')
                          READ (NDS,NML=SDS6,END=801,ERR=802,IOSTAT=J)
                        CASE('SWL6')
                          READ (NDS,NML=SWL6,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_BT1
                        CASE('SBT1')
                          READ (NDS,NML=SBT1,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_BT4
                        CASE('SBT4')
                          READ (NDS,NML=SBT4,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_IS1
                        CASE('SIS1')
                          READ (NDS,NML=SIS1,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_IS2
                        CASE('SIS2')
                          READ (NDS,NML=SIS2,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_DB1
                        CASE('SDB1')
                          READ (NDS,NML=SDB1,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_UOST
                        CASE('UOST')
                          READ (NDS,NML=UOST,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_PR1
                        CASE('PRO1')
                          READ (NDS,NML=PRO1,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_PR2
                        CASE('PRO2')
                          READ (NDS,NML=PRO2,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_SMC
                        CASE('PSMC')
                          READ (NDS,NML=PSMC,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_PR3
                        CASE('PRO3')
                          READ (NDS,NML=PRO3,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_RTD
                        CASE('ROTD') 
                          READ (NDS,NML=ROTD,END=801,ERR=802,IOSTAT=J)
                        CASE('ROTB')
                          READ (NDS,NML=ROTB,END=801,ERR=802,IOSTAT=J) 
#endif
#ifdef W3_REF1
                       CASE('REF1') 
                         READ (NDS,NML=REF1,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_IG1
                        CASE('SIG1') 
                         READ (NDS,NML=SIG1,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_IC2
                        CASE('SIC2') 
                         READ (NDS,NML=SIC2,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_IC3
                        CASE('SIC3') 
                         READ (NDS,NML=SIC3,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_IC4
                        CASE('SIC4 ') 
                         READ (NDS,NML=SIC4,END=801,ERR=802,IOSTAT=J)
#endif
#ifdef W3_IC5
                        CASE('SIC5 ') 
                         READ (NDS,NML=SIC5,END=801,ERR=802,IOSTAT=J)
#endif
                        CASE('UNST')
                          READ (NDS,NML=UNST,END=801,ERR=802,IOSTAT=J)
                        CASE('OUTS')
                          READ (NDS,NML=OUTS,END=801,ERR=802,IOSTAT=J)
                        CASE('MISC')
                          READ (NDS,NML=MISC,END=801,ERR=802,IOSTAT=J)
                        CASE DEFAULT
                          GOTO 803
                        END SELECT
                      STATUS  = '(user def. values) :'
                      RETURN
                    END IF
                ELSE
                  EXIT
                END IF
            ENDIF
          END DO
        END DO
!
  800 CONTINUE
      RETURN
!
  801 CONTINUE
      WRITE (NDSE,1001) NAME
      CALL EXTCDE(1)
      RETURN
!
  802 CONTINUE
      WRITE (NDSE,1002) NAME, J
      CALL EXTCDE(2)
      RETURN
!
  803 CONTINUE
      WRITE (NDSE,1003) NAME
      CALL EXTCDE(3)
      RETURN
!
#ifdef W3_NL2
  804 CONTINUE
      WRITE (NDSE,1004) NDEPTH
      CALL EXTCDE(4)
      RETURN
#endif
!
#ifdef W3_NL3
  804 CONTINUE
      WRITE (NDSE,1004) NQDEF
      CALL EXTCDE(4)
      RETURN
#endif
!
! Formats
!
 1001 FORMAT (/' *** WAVEWATCH III ERROR IN READNL : '/          &
               '     PREMATURE END OF FILE IN READING ',A/)
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN READNL : '/          &
               '     ERROR IN READING ',A,'  IOSTAT =',I8/)
 1003 FORMAT (/' *** WAVEWATCH III ERROR IN READNL : '/          &
               '     NAMELIST NAME ',A,' NOT RECOGNIZED'/)
#ifdef W3_NL2
 1004 FORMAT (/' *** WAVEWATCH III ERROR IN READNL : '/          &
               '     TEMP DEPTH ARRAY TOO SMALL, .LE. ',I8/)
#endif
#ifdef W3_NL3
 1004 FORMAT (/' *** WAVEWATCH-III ERROR IN READNL : '/          &
               '     TEMP QPARMS ARRAY TOO SMALL, .LE. ',I8/)
#endif
!/
!/ End of READNL ----------------------------------------------------- /
!/
      END SUBROUTINE
!/
!/ End of W3GRID ----------------------------------------------------- /
!/
      END MODULE W3GRIDMD
