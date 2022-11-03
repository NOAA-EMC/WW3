!> @file w3psmcmd.F90
!> @brief Spherical Multiple-Cell (SMC) grid
!>
!> @author Jian-Guo Li
!> @date 23 Mar 2020

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!> @brief Spherical Multiple-Cell (SMC) grid routines.
!>
!> @details Bundles routines for SMC advection (UNO2) and diffusion
!>  schemes in single module, including great circile turning and
!>  refraction rotation schemes.
!>
!> @author Jian-Guo Li
!> @date 23 Mar 2020
!>
MODULE W3PSMCMD
  !/
  !/                  +------------------------------------+
  !/                  | Spherical Multiple-Cell (SMC) grid |
  !/                  | Adv, GCT, Rfr, Dif subroutines.    |
  !/                  |           Jian-Guo Li              |
  !/                  | First created:     8 Nov 2010      |
  !/                  | Last modified:    23 Mar 2020      |
  !/                  +------------------------------------+
  !/
  !/    08-Nov-2010 : Coding started by adapting w3pro2md.ftn.
  !/    18-Nov-2010 : Refraction and GCT by rotation and k-shift.
  !/    12-Apr-2011 : Restore x-advective flux for intermediate update.
  !/     3-Jun-2011 : New refraction formulation using Cg only.
  !/     8-Jun-2011 : Optimise classic refraction formulation.
  !/    16-Jun-2011 : Add refraction limter to gradient direction.
  !/     1-Jul-2011 : New refraction using Cg and gradient limiter.
  !/    28-Jul-2011 : Finalise with old refraction scheme and gradient limiter.
  !/     4-Nov-2011 : Separate x and y obstruction coefficients.
  !/     5-Jan-2012 : Update to multi-resolution SMC grid with sub-time-steps.
  !/     2-Feb-2012 : Separate single- and multi-resolution advection.
  !/     6-Mar-2012 : Tidy up code and minor adjustments, CLATF.
  !/    12-Mar-2012 : Remove net flux bug and optimise upstream code.
  !/    16-Jan-2013 : Adapted for Version 4.08, removing FACX/Y.
  !/    16-Sep-2013 : Add Arctic part for SMC grid in WW3 V4.11
  !/     3-Jan-2014 : Remove bug in SMCDHXY for AU/V as cell size.
  !/     7-Jan-2014 : Remove bug in SMCGtCrfr for K definition.
  !/    28-Jan-2014 : Move Arctic boundary condition update out.
  !/    18-Aug-2015 : New gradient, average and 3rd order advection subs.
  !/     3-Sep-2015 : UNO3 advection scheme by logical option FUNO3.
  !/    14-Sep-2015 : Modify DHDX/Y for Arctic part refraction term.
  !/     8-Aug-2017 : Update SMCGradn for 0 or 1 boundary conditions.
  !/     9-Jan-2018 : Parallelization by adding OpenMP directives.
  !/    19-Feb-2020 : Additions for OMP bit-reproducability (C.Bunney)
  !/    23-Mar-2020 : Add extra parenthese for single ATOMIC line update.
  !/    22-Oct-2020 : Two new subs for lat-lon points mapping to cells.
  !/
  !  1. Purpose :
  !
  !     Bundles routines for SMC advection (UNO2) and diffusion schemes in
  !     single module, including GCT and refraction rotation schemes.
  !
  !  2. Variables and types :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      TRNMIN    R.P.  Private   Minimum transparancy for local
  !     ----------------------------------------------------------------
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      W3PSMC    Subr. Public   Spatial propagation on SMC grid.
  !      W3KRTN    Subr. Public   Spectral modification by GCT and refraction.
  !      SMCxUNO2  Subr. Public   Irregular grid mid-flux on U-faces by UNO2.
  !      SMCyUNO2  Subr. Public   Irregular grid mid-flux on V-faces by UNO2.
  !      SMCxUNO2r Subr. Public   Regular grid mid-flux on U-faces by UNO2.
  !      SMCyUNO2r Subr. Public   Regular grid mid-flux on V-faces by UNO2.
  !      SMCkUNO2  Subr. Public   Shift in k-space due to refraction by UNO2.
  !      SMCxUNO3  Subr. Public   Irregular grid mid-flux on U-faces by UNO3.
  !      SMCyUNO3  Subr. Public   Irregular grid mid-flux on V-faces by UNO3.
  !      SMCxUNO3r Subr. Public   Regular grid 3rd order U-mid-flux by UNO3.
  !      SMCyUNO3r Subr. Public   Regular grid 3rd order V-mid-flux by UNO3.
  !      SMCGtCrfr Subr. Public   Refraction and GCT rotation in theta.
  !      SMCDHXY   Subr. Public   Evaluate depth gradient and refraction limiter.
  !      SMCGradn  Subr. Public   Evaluate local gradient for sea points.
  !      SMCAverg  Subr. Public   Numerical 1-2-1 average of sea point field.
  !      W3GATHSMC W3SCATSMC      Gather and scatter spectral components.
  !      W3SMCELL  Subr. Public   Calculate cell centre lat-lon for given ids.
  !      W3SMCGMP  Subr. Public   Map lat-lon points to SMC grid cells.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      STRACE    Subr. W3SERVMD Subroutine tracing.
  !      W3ACTURN  Subr. W3SERVMD Subroutine rotating action spectrum.
  !     ----------------------------------------------------------------
  !
  !  5. Remarks :
  !
  !  6. Switches :
  !
  !       !/MGP   Correct for motion of grid.
  !       !/S     Enable subroutine tracing.
  !       !/Tn    Enable test output.
  !
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  !/ Use omp_lib for OpenMP functions if switched on.   JGLi10Jan2018
  !$       USE omp_lib
  !/
  !/ Public variables
  !/
  PUBLIC
  !/
  !/ Private data !/
  REAL, PRIVATE, PARAMETER:: TRNMIN = 0.95 !< Minimum transparancy for local
  !/
CONTAINS

  !/ ------------------------------------------------------------------- /
  !> @brief Propagation in phyiscal space for a given spectral component
  !>
  !> @details Unstructured SMC grid, point-oriented face and cell loops.
  !>  UNO2 advection scheme and isotropic FTCS diffusion scheme
  !>
  !> @param[in]     ISP     Number of spectral bin (IK-1)*NTH+ITH
  !> @param[in]     DTG     Total time step.
  !> @param[inout]  VQ      Field to propagate.
  !>
  !> @author Jian-Guo Li
  !> @date 18 Apr 2018
  !>
  SUBROUTINE W3PSMC ( ISP, DTG, VQ )
    !/
    !/                  +------------------------------------+
    !/                  | Spherical Multiple-Cell (SMC) grid |
    !/                  | Advection and diffusion sub.       |
    !/                  | First created:   JG Li  8 Nov 2010 |
    !/                  | Last modified:   JG Li 18 Apr 2018 |
    !/                  +------------------------------------+
    !/
    !/    08-Nov-2010 : Origination.                JGLi    ( version 1.00 )
    !/    16-Dec-2010 : Check U/V CFL values.       JGLi    ( version 1.10 )
    !/    18-Mar-2011 : Check MPI communication.    JGLi    ( version 1.20 )
    !/    16-May-2011 : Tidy up diagnosis lines.    JGLi    ( version 1.30 )
    !/     4 Nov-2011 : Separate x and y obstruc.   JGLi    ( version 1.40 )
    !/     5 Jan-2012 : Multi-resolution SMC grid.  JGLi    ( version 1.50 )
    !/     2 Feb-2012 : Separate single multi adv.  JGLi    ( version 1.60 )
    !/     6 Mar-2012 : Minor adjustments of CLATF. JGLi    ( version 1.70 )
    !/    12 Feb-2012 : Remove net flux bug.        JGLi    ( version 1.80 )
    !/    16 Sep-2013 : Add Arctic part.            JGLi    ( version 2.00 )
    !/     3 Sep-2015 : Gradient, UNO3 and Average. JGLi    ( version 2.10 )
    !/    26 Feb-2016 : Update boundary spectra.    JGLi    ( version 2.20 )
    !/    23 Mar-2016 : Add current option.         JGLi    ( version 2.30 )
    !/    18 Apr-2018 : Refined sub-grid blocking.  JGLi    ( version 2.40 )
    !/
    !  1. Purpose :
    !
    !     Propagation in phyiscal space for a given spectral component.
    !
    !  2. Method :
    !
    !     Unstructured SMC grid, point-oriented face and cell loops.
    !     UNO2 advection scheme and isotropic FTCS diffusion scheme.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       ISP     Int.   I   Number of spectral bin (IK-1)*NTH+ITH
    !       FACX/Y  Real   I   Factor in propagation velocity (1 or 0 *DT/DX)
    !       DTG     Real   I   Total time step.
    !       MAPSTA  I.A.   I   Grid point status map.
    !       MAPFS   I.A.   I   Storage map.
    !       VQ      R.A.  I/O  Field to propagate.
    !     ----------------------------------------------------------------
    !
    !     Local variables.
    !     ----------------------------------------------------------------
    !       NTLOC   Int   Number of local time steps.
    !       DTLOC   Real  Local propagation time step.
    !       CGD     Real  Deep water group velocity.
    !       DSSD, DNND    Deep water diffusion coefficients.
    !       ULCFLX  R.A.  Local courant numbers in 'x' (norm. velocities)
    !       VLCFLY  R.A.  Id. in 'y'.
    !       CXTOT   R.A.  Propagation velocities in physical space.
    !       CYTOT   R.A.
    !       DFRR    Real  Relative frequency increment.
    !       DX0I    Real  Inverted grid incremenent in meters (longitude, eq.).
    !       DY0I    Real  Inverted grid incremenent in meters (latitude).
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
    !      W3WAVE    Subr. W3WAVEMD Wave model routine.
    !     ---------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     ---------------------------------------------
    !       1.  Preparations
    !         a Set constants
    !         b Initialize arrays
    !       2.  Prepare arrays
    !         a Velocities and 'Q'
    !         b diffusion coefficients
    !       3.  Loop over sub-steps
    !       ----------------------------------------
    !         a Propagate
    !         b Update boundary conditions
    !         c Diffusion correction
    !       ----------------------------------------
    !       4.  Store Q field in spectra
    !     ---------------------------------------------
    !
    !  9. Switches :
    !
    !       !/MGP   Correct for motion of grid.
    !
    !       !/TDYN  Dynamic increase of DTME
    !       !/DSS0  Disable diffusion in propagation direction
    !       !/XW0   Propagation diffusion only.
    !       !/XW1   Growth diffusion only.
    !
    !       !/S     Enable subroutine tracing.
    !
    !       !/T     Enable general test output.
    !       !/T1    Dump of input field and fluxes.
    !       !/T2    Dump of output field.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    !
    USE W3TIMEMD, ONLY: DSEC21
    !
    USE W3GDATMD, ONLY: NK, NTH, DTH, XFR, ESIN, ECOS, SIG, NX, NY,  &
         NSEA, SX, SY, MAPSF, FUNO3, FVERG,           &
         IJKCel, IJKUFc, IJKVFc, NCel, NUFc, NVFc,    &
         IJKCel3, IJKCel4,                            &
         IJKVFc5, IJKVFc6,IJKUFc5,IJKUFc6,            &
         NLvCel, NLvUFc, NLvVFc, NRLv, MRFct,         &
         DTCFL, CLATS, DTMS, CTRNX, CTRNY
    USE W3GDATMD, ONLY: NGLO, ANGARC, ARCTC
    USE W3WDATMD, ONLY: TIME
    USE W3ADATMD, ONLY: CG, WN, U10, CX, CY, ATRNX, ATRNY, ITIME
    !
    USE W3IDATMD, ONLY: FLCUR
    USE W3ODATMD, ONLY: NDSE, NDST, FLBPI, NBI, TBPI0, TBPIN,       &
         ISBPI, BBPI0, BBPIN
    !
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: ISP
    REAL,    INTENT(IN)     :: DTG
    REAL,    INTENT(INOUT)  :: VQ(NSEA)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: ITH, IK, NTLOC, ITLOC, ISEA, IXY,    &
         IY, IY0, IP, IBI, LvR
    INTEGER                 :: i, j, k, L, M, N, LL, MM, NN, LMN,   &
         iuf, juf, ivf, jvf, icl, jcl
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: CG0, CGA, CGN, CGX, CGY, FMR, RD1,   &
         RD2, CXMIN, CXMAX, CYMIN, CYMAX,     &
         CXC, CYC, DTLDX, DTLDY
    REAL                    :: DTLOC, CGCOS, CGSIN, FUTRN, FVTRN,   &
         DFRR, DX0I, DY0I, CGD, DSSD,         &
         DNND, DCELL, XWIND, TFAC, DSS, DNN
    REAL                    :: PCArea, ARCTH
    LOGICAL                 :: YFIRST
    !/
    !/ Automatic work arrays
    !
    REAL, Dimension(-9:NCel) ::  FCNt, AFCN, BCNt, UCFL, VCFL, CQ,  &
         CQA, CXTOT, CYTOT
    REAL, Dimension(   NUFc) ::  FUMD, FUDIFX, ULCFLX
    REAL, Dimension(   NVFc) ::  FVMD, FVDIFY, VLCFLY
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3PSMC')
#endif
    !
    ! 1.  Preparations --------------------------------------------------- *

    !!Li  Spectral bin direction and frequency indices
    ITH    = 1 + MOD(ISP-1,NTH)
    IK     = 1 + (ISP-1)/NTH

    !!Li  Maximum group speed for 1st and the transported frequency bin
    !!Li  A factor of 1.2 is added to account for the shallow water peak.
    CG0    = 0.6 * GRAV / SIG(1)
    CGA    = 0.6 * GRAV / SIG(IK)

    !!Li  Maximum group speed for given spectral bin. First bin speed is
    !!Li  used to avoid zero speed component.
    !     CGX    = ABS( CGA * ECOS(ITH) )
    !     CGY    = ABS( CGA * ESIN(ITH) )
    CGX    =  CGA * ECOS(ITH)
    CGY    =  CGA * ESIN(ITH)

    !!Li  Add maximum current components to maximum group components.
    IF ( FLCUR ) THEN
      CXMIN  = MINVAL ( CX(1:NSEA) )
      CXMAX  = MAXVAL ( CX(1:NSEA) )
      CYMIN  = MINVAL ( CY(1:NSEA) )
      CYMAX  = MAXVAL ( CY(1:NSEA) )
      IF ( ABS(CGX+CXMIN) .GT. ABS(CGX+CXMAX) ) THEN
        CGX    = CGX + CXMIN
      ELSE
        CGX    = CGX + CXMAX
      END IF
      IF ( ABS(CGY+CYMIN) .GT. ABS(CGY+CYMAX) ) THEN
        CGY    = CGY + CYMIN
      ELSE
        CGY    = CGY + CYMAX
      END IF
      CXC    = MAX ( ABS(CXMIN) , ABS(CXMAX) )
      CYC    = MAX ( ABS(CYMIN) , ABS(CYMAX) )
    ELSE
      CXC    = 0.
      CYC    = 0.
    END IF

    !!Li  Base-cell grid lenth at Equator (size-4 on SMC625 grid).
    DX0I  = 1.0/(SX * DERA * RADIUS)
    DY0I  = 1.0/(SY * DERA * RADIUS)

    !!Li  Miminum time step determined by Courant number < 0.8
    !!Li  Note, minimum x grid length is half the Equator value.
    !!Li  Minimum time step should not be less than sub w3init requirement,
    !!Li  where IAPPRO array is initialised for propagation parallization.
    CGN   = 0.9999 * MAX( ABS(CGX), ABS(CGY), CXC, CYC, 0.001*CG0 )
    DTLOC = DTCFL*CG0/CGN
    NTLOC  = 1 + INT(DTG/DTLOC - 0.001)
    DTLOC  = DTG / REAL(NTLOC)

    !!Li  Group speed component common factors, FACX=DTG*DX0I
    !!Li  FACX and FACY are evaluated here directly.  JGLi16Jan2013
    !     CGCOS   = FACX * ECOS(ITH) / REAL(NTLOC)
    !     CGSIN   = FACY * ESIN(ITH) / REAL(NTLOC)
    CGCOS   = ECOS(ITH)
    CGSIN   = ESIN(ITH)
    DTLDX   = DTLOC * DX0I
    DTLDY   = DTLOC * DY0I
    !
    YFIRST = MOD(ITIME,2) .EQ. 0
    !
    !Li   Homogenous diffusion Fourier number DNND and DSSD will be used.
    !Li   They have to be divided by base-cell size for size-1 stability.
    !Li   So they are equivalent to the Fourier number in size-1 cell at
    !Li   the sub-time step DTLOC/MRFct.
    IF ( DTMS .GT. 0. ) THEN
      DFRR   = XFR - 1.
      CGD    = 0.5 * GRAV / SIG(IK)
      DNN    = ((DTH*CGD)**2)*DTMS / 12.
      DNND   = DNN*DTLOC*(DX0I*DX0I)
      DSSD   = DNN*DTLOC*(DY0I*DY0I)
    ELSE
      DSSD = 0.0
      DNND = 0.0
    END IF
    !
    ! 1.b Initialize arrays
    !
#ifdef W3_T
    WRITE (NDST,9010)
#endif
    !
    ULCFLX = 0.
    VLCFLY = 0.

    !Li    Pass spectral element VQ to CQ and define size-1 cell CFL
#ifdef W3_OMPG
    !$OMP Parallel DO Private(ISEA)
#endif
    DO ISEA=1, NSEA
      !Li  Transported variable is divided by CG as in WW3.
      CQ(ISEA) = VQ(ISEA)/CG(IK,ISEA)
      !Li  Resetting NaNQ VQ to zero if any.   JGLi18Mar2013
      IF( .NOT. (CQ(ISEA) .EQ. CQ(ISEA)) )  CQ(ISEA) = 0.0
    END DO
#ifdef W3_OMPG
    !$OMP END Parallel DO
#endif

    !Li  Add current components if any to wave velocity.
    IF ( FLCUR ) THEN
#ifdef W3_OMPG
      !$OMP Parallel DO Private(ISEA)
#endif
      DO ISEA=1, NSEA
        CXTOT(ISEA) = (CGCOS * CG(IK,ISEA) + CX(ISEA))
        CYTOT(ISEA) = (CGSIN * CG(IK,ISEA) + CY(ISEA))
      ENDDO
#ifdef W3_OMPG
      !$OMP END Parallel DO
#endif
    ELSE
      !Li   No current case use group speed only.
#ifdef W3_OMPG
      !$OMP Parallel DO Private(ISEA)
#endif
      DO ISEA=1, NSEA
        CXTOT(ISEA) =  CGCOS * CG(IK,ISEA)
        CYTOT(ISEA) =  CGSIN * CG(IK,ISEA)
      END DO
#ifdef W3_OMPG
      !$OMP END Parallel DO
#endif
      !Li   End of IF( FLCUR ) block.
    ENDIF

    !Li   Arctic cell velocity components need to be rotated
    !Li   back to local east referenence system for propagation.
    IF( ARCTC ) THEN
      DO ISEA=NGLO+1, NSEA
        ARCTH = ANGARC(ISEA-NGLO)*DERA
        CXC = CXTOT(ISEA)*COS(ARCTH) + CYTOT(ISEA)*SIN(ARCTH)
        CYC = CYTOT(ISEA)*COS(ARCTH) - CXTOT(ISEA)*SIN(ARCTH)
        CXTOT(ISEA) = CXC
        CYTOT(ISEA) = CYC
      END DO
      !Li   Polar cell area factor for V-flux update
      PCArea = DY0I/(MRFct*PI*DX0I*FLOAT(IJKCel(4,NSEA)))
      !Li   V-component is reset to zero for Polar cell as direction
      !Li   is undefined there.
      CYTOT(NSEA) = 0.0
    ENDIF


    !Li     Convert velocity components into CFL factors.
#ifdef W3_OMPG
    !$OMP Parallel DO Private(ISEA)
#endif
    DO ISEA=1, NSEA
      UCFL(ISEA) = DTLDX*CXTOT(ISEA)/CLATS(ISEA)
      VCFL(ISEA) = DTLDY*CYTOT(ISEA)
    ENDDO
#ifdef W3_OMPG
    !$OMP END Parallel DO
#endif

    !Li  Initialise boundary cell CQ and Velocity values.
    CQ(-9:0)=0.0
    UCFL(-9:0)=0.0
    VCFL(-9:0)=0.0
    !
    ! 3.  Loop over frequency-dependent sub-steps -------------------------*
    !
    DO ITLOC=1, NTLOC
      !
      !     Initialise net flux arrays.
      FCNt(-9:NCel) = 0.0
      AFCN(-9:NCel) = 0.0
      BCNt(-9:NCel) = 0.0
      !
      !     Single-resolution SMC grid uses regular grid advection with
      !     partial blocking enabled when NRLv = 1
      IF ( NRLv .EQ. 1 ) THEN
        IF( FUNO3 ) THEN
          !  Use 3rd order UNO3 scheme.  JGLi20Aug2015
          CALL SMCxUNO3r(1, NUFc, CQ, UCFL, ULCFLX, DNND, FUMD, FUDIFX)
        ELSE
          !  Call SMCxUNO2 to calculate MFx value
          CALL SMCxUNO2r(1, NUFc, CQ, UCFL, ULCFLX, DNND, FUMD, FUDIFX)
        ENDIF

        !  Store conservative flux in FCNt advective one in AFCN
#ifdef W3_OMPG
        !$OMP Parallel DO Private(i, M, N, FUTRN)
#endif
        DO i=1, NUFc
          M=IJKUFc5(i)
          N=IJKUFc6(i)
          FUTRN = FUMD(i)*ULCFLX(i) - FUDIFX(i)

          !! Add sub-grid transparency for input flux update.  JGLi16May2011
          !! Transparency is also applied on diffusion flux.   JGLi12Mar2012
          !! Replace CRITICAL with ATOMIC.  JGLi15Jan2019
          !! !$OMP CRITICAL
          !! Remove boundary cell flux update or M N > 0.  JGLi28Mar2019
          IF( M > 0 ) THEN
            IF( (CTRNX(M)+CTRNX(N)) .GE. 1.96 )  THEN
#ifdef W3_OMPG
              !$OMP ATOMIC
#endif
              FCNt(M) = FCNt(M) - FUTRN
            ELSE IF( ULCFLX(i) .GE. 0.0 )  THEN
#ifdef W3_OMPG
              !$OMP ATOMIC
#endif
              FCNt(M) = FCNt(M) - FUTRN*CTRNX(M)
            ELSE
#ifdef W3_OMPG
              !$OMP ATOMIC
#endif
              FCNt(M) = FCNt(M) - FUTRN*CTRNX(N)*CTRNX(M)
            ENDIF
            !  Also divided by another cell length as UCFL is in basic unit.
#ifdef W3_OMPG
            !$OMP ATOMIC
#endif
            ! ChrisB: Re-arranged the RHS term below to make it
            ! valid for OMP ATMOIC directive.
            AFCN(M) = AFCN(M) - (FUMD(i)*UCFL(M) - FUDIFX(i))
          ENDIF

          IF( N > 0 ) THEN
            IF( (CTRNX(M)+CTRNX(N)) .GE. 1.96 )  THEN
#ifdef W3_OMPG
              !$OMP ATOMIC
#endif
              FCNt(N) = FCNt(N) + FUTRN
            ELSE IF( ULCFLX(i) .GE. 0.0 )  THEN
#ifdef W3_OMPG
              !$OMP ATOMIC
#endif
              FCNt(N) = FCNt(N) + FUTRN*CTRNX(M)*CTRNX(N)
            ELSE
#ifdef W3_OMPG
              !$OMP ATOMIC
#endif
              FCNt(N) = FCNt(N) + FUTRN*CTRNX(N)
            ENDIF
            !  Also divided by another cell length as UCFL is in basic unit.
#ifdef W3_OMPG
            !$OMP ATOMIC
#endif
            AFCN(N) = AFCN(N) + (FUMD(i)*UCFL(N) - FUDIFX(i))
          ENDIF
          !! !$OMP END CRITICAL

        ENDDO
#ifdef W3_OMPG
        !$OMP END Parallel DO
#endif

        !  Store conservative update in CQA and advective update in CQ
        !  The side length in MF value has to be cancelled with cell length
        !  Note ULCFLX has been divided by the cell size inside SMCxUNO2.
#ifdef W3_OMPG
        !$OMP Parallel DO Private(n)
#endif
        DO n=1, NSEA
          CQA(n)=CQ(n) + FCNt(n)/FLOAT(IJKCel3(n))
          CQ (n)=CQ(n) + AFCN(n)/FLOAT(IJKCel3(n))
        ENDDO
#ifdef W3_OMPG
        !$OMP END Parallel DO
#endif

        !  Call advection subs.
        IF( FUNO3 ) THEN
          !  Use 3rd order UNO3 scheme.  JGLi20Aug2015
          CALL SMCyUNO3r(1, NVFc, CQ, VCFL, VLCFLY, DSSD, FVMD, FVDIFY)
        ELSE
          !  Call SMCyUNO2 to calculate MFy value
          CALL SMCyUNO2r(1, NVFc, CQ, VCFL, VLCFLY, DSSD, FVMD, FVDIFY)
        ENDIF

#ifdef W3_OMPG
        !$OMP Parallel DO Private(j, M, N, FVTRN)
#endif
        DO j=1, NVFc
          M=IJKVFc5(j)
          N=IJKVFc6(j)
          FVTRN = FVMD(j)*VLCFLY(j) - FVDIFY(j)

          !! Add sub-grid transparency for input flux update.  JGLi16May2011
          !! Transparency is also applied on diffusion flux.   JGLi12Mar2012
          !! Replace CRITICAL with ATOMIC.  JGLi15Jan2019
          !! !$OMP CRITICAL
          !! Remove boundary cell flux update or M N > 0.  JGLi28Mar2019
          IF( M > 0 ) THEN
            IF( (CTRNY(M)+CTRNY(N)) .GE. 1.96 )  THEN
#ifdef W3_OMPG
              !$OMP ATOMIC
#endif
              BCNt(M) = BCNt(M) - FVTRN
            ELSE IF( VLCFLY(j) .GE. 0.0 )  THEN
#ifdef W3_OMPG
              !$OMP ATOMIC
#endif
              BCNt(M) = BCNt(M) - FVTRN*CTRNY(M)
            ELSE
#ifdef W3_OMPG
              !$OMP ATOMIC
#endif
              BCNt(M) = BCNt(M) - FVTRN*CTRNY(N)*CTRNY(M)
            ENDIF
          ENDIF
          IF( N > 0 ) THEN
            IF( (CTRNY(M)+CTRNY(N)) .GE. 1.96 )  THEN
#ifdef W3_OMPG
              !$OMP ATOMIC
#endif
              BCNt(N) = BCNt(N) + FVTRN
            ELSE IF( VLCFLY(j) .GE. 0.0 )  THEN
#ifdef W3_OMPG
              !$OMP ATOMIC
#endif
              BCNt(N) = BCNt(N) + FVTRN*CTRNY(M)*CTRNY(N)
            ELSE
#ifdef W3_OMPG
              !$OMP ATOMIC
#endif
              BCNt(N) = BCNt(N) + FVTRN*CTRNY(N)
            ENDIF
          ENDIF
          !! !$OMP END CRITICAL
        ENDDO
#ifdef W3_OMPG
        !$OMP END Parallel DO
#endif

        !  Store conservative update of CQA in CQ
        !  The v side length in MF value has to be cancelled with cell length
        !! One cosine factor is also needed to be divided for SMC grid
#ifdef W3_OMPG
        !$OMP Parallel DO Private(n)
#endif
        DO n=1, NSEA
          CQ(n)=CQA(n) + BCNt(n)/( CLATS(n)*FLOAT(IJKCel3(n)) )
        ENDDO
#ifdef W3_OMPG
        !$OMP END Parallel DO
#endif
        !   Polar cell needs a special area factor, one-level case.
        IF( ARCTC ) CQ(NSEA) = CQA(NSEA) + BCNt(NSEA)*PCArea

        !   End of single-resolution advection and diffusion.
      ELSE

        !     Multi-resolution SMC grid uses irregular grid advection scheme
        !     without partial blocking when NRLv > 1
        !
        ! 3.a    Multiresolution sub-steps depend on refined levels MRFct
        DO LMN = 1, MRFct
          !
          ! 3.b    Loop over all levels, starting from the finest level.
          DO LL= 1, NRLv

            !        Cell size of this level
            LvR=2**(LL - 1)
            FMR=FLOAT( LvR )
            !
            ! 3.c    Calculate this level only if size is factor of LMN
            IF( MOD(LMN, LvR) .EQ. 0 ) THEN
              !
              ! 3.d    Select cell and face ranges
              icl=NLvCel(LL-1)+1
              iuf=NLvUFc(LL-1)+1
              ivf=NLvVFc(LL-1)+1
              jcl=NLvCel(LL)
              juf=NLvUFc(LL)
              jvf=NLvVFc(LL)
              !
              !  Use 3rd order UNO3 scheme.  JGLi03Sep2015
              IF( FUNO3 ) THEN
                CALL SMCxUNO3(iuf, juf, CQ, UCFL, ULCFLX, DNND, FUMD, FUDIFX, FMR)
              ELSE
                !  Call SMCxUNO2 to calculate finest level (size-1) MFx value
                CALL SMCxUNO2(iuf, juf, CQ, UCFL, ULCFLX, DNND, FUMD, FUDIFX, FMR)
              ENDIF

              !  Store fineset level conservative flux in FCNt advective one in AFCN
#ifdef W3_OMPG
              !$OMP Parallel DO Private(i, L, M, FUTRN)
#endif
              DO i=iuf, juf
                L=IJKUFc5(i)
                M=IJKUFc6(i)
                FUTRN = FUMD(i)*ULCFLX(i) - FUDIFX(i)
                !! Replace CRITICAL with ATOMIC.  JGLi15Jan2019
                !! !$OMP CRITICAL
                !! Remove boundary cell flux update or L M > 0.  JGLi28Mar2019
                IF( L > 0 ) THEN
                  !! Add sub-grid blocking for refined cells.   JGLi18Apr2018
                  IF( (CTRNX(M)+CTRNX(L)) .GE. 1.96 )  THEN
#ifdef W3_OMPG
                    !$OMP ATOMIC
#endif
                    FCNt(L) = FCNt(L) - FUTRN
                  ELSE IF( ULCFLX(i) .GE. 0.0 ) THEN
#ifdef W3_OMPG
                    !$OMP ATOMIC
#endif
                    FCNt(L) = FCNt(L) - FUTRN*CTRNX(L)
                  ELSE
#ifdef W3_OMPG
                    !$OMP ATOMIC
#endif
                    FCNt(L) = FCNt(L) - FUTRN*CTRNX(L)*CTRNX(M)
                  ENDIF
#ifdef W3_OMPG
                  !$OMP ATOMIC
#endif
                  ! ChrisB: Re-arranged the RHS term below to make it
                  ! valid for OMP ATMOIC directive.
                  AFCN(L) = AFCN(L) - (FUMD(i)*UCFL(L)*FMR - FUDIFX(i))
                ENDIF
                IF( M > 0 ) THEN
                  !! Add sub-grid blocking for refined cells.   JGLi18Apr2018
                  IF( (CTRNX(M)+CTRNX(L)) .GE. 1.96 )  THEN
#ifdef W3_OMPG
                    !$OMP ATOMIC
#endif
                    FCNt(M) = FCNt(M) + FUTRN
                  ELSE IF( ULCFLX(i) .GE. 0.0 ) THEN
#ifdef W3_OMPG
                    !$OMP ATOMIC
#endif
                    FCNt(M) = FCNt(M) + FUTRN*CTRNX(M)*CTRNX(L)
                  ELSE
#ifdef W3_OMPG
                    !$OMP ATOMIC
#endif
                    FCNt(M) = FCNt(M) + FUTRN*CTRNX(M)
                  ENDIF
#ifdef W3_OMPG
                  !$OMP ATOMIC
#endif
                  AFCN(M) = AFCN(M) + (FUMD(i)*UCFL(M)*FMR - FUDIFX(i))
                ENDIF
                !! !$OMP END CRITICAL
              ENDDO
#ifdef W3_OMPG
              !$OMP END Parallel DO
#endif

              !  Store conservative update in CQA and advective update in CQ
              !  The side length in MF value has to be cancelled with cell y-length.
              !  Also divided by another cell x-size as UCFL is in size-1 unit.
#ifdef W3_OMPG
              !$OMP Parallel DO Private(n)
#endif
              DO n=icl, jcl
                CQA(n)=CQ(n) + FCNt(n)/FLOAT( IJKCel3(n)*IJKCel4(n) )
                CQ (n)=CQ(n) + AFCN(n)/FLOAT( IJKCel3(n)*IJKCel4(n) )
                FCNt(n)=0.0
                AFCN(n)=0.0
              ENDDO
#ifdef W3_OMPG
              !$OMP END Parallel DO
#endif
              !
              !  Use 3rd order UNO3 scheme.  JGLi03Sep2015
              IF( FUNO3 ) THEN
                CALL SMCyUNO3(ivf, jvf, CQ, VCFL, VLCFLY, DSSD, FVMD, FVDIFY, FMR)
              ELSE
                !  Call SMCyUNO2 to calculate MFy value
                CALL SMCyUNO2(ivf, jvf, CQ, VCFL, VLCFLY, DSSD, FVMD, FVDIFY, FMR)
              ENDIF
              !
              !  Store conservative flux in BCNt
#ifdef W3_OMPG
              !$OMP Parallel DO Private(j, L, M, FVTRN)
#endif
              DO j=ivf, jvf
                L=IJKVFc5(j)
                M=IJKVFc6(j)
                FVTRN = FVMD(j)*VLCFLY(j) - FVDIFY(j)
                !! Replace CRITICAL with ATOMIC.  JGLi15Jan2019
                !! !$OMP CRITICAL
                !! Remove boundary cell flux update or L M > 0.  JGLi28Mar2019
                IF( L > 0 ) THEN
                  !! Add sub-grid blocking for refined cells.   JGLi18Apr2018
                  IF( (CTRNY(M)+CTRNY(L)) .GE. 1.96 )  THEN
#ifdef W3_OMPG
                    !$OMP ATOMIC
#endif
                    BCNt(L) = BCNt(L) - FVTRN
                  ELSE IF( VLCFLY(j) .GE. 0.0 )  THEN
#ifdef W3_OMPG
                    !$OMP ATOMIC
#endif
                    BCNt(L) = BCNt(L) - FVTRN*CTRNY(L)
                  ELSE
#ifdef W3_OMPG
                    !$OMP ATOMIC
#endif
                    BCNt(L) = BCNt(L) - FVTRN*CTRNY(L)*CTRNY(M)
                  ENDIF
                ENDIF
                IF( M > 0 ) THEN
                  !! Add sub-grid blocking for refined cells.   JGLi18Apr2018
                  IF( (CTRNY(M)+CTRNY(L)) .GE. 1.96 )  THEN
#ifdef W3_OMPG
                    !$OMP ATOMIC
#endif
                    BCNt(M) = BCNt(M) + FVTRN
                  ELSE IF( VLCFLY(j) .GE. 0.0 )  THEN
#ifdef W3_OMPG
                    !$OMP ATOMIC
#endif
                    BCNt(M) = BCNt(M) + FVTRN*CTRNY(M)*CTRNY(L)
                  ELSE
#ifdef W3_OMPG
                    !$OMP ATOMIC
#endif
                    BCNt(M) = BCNt(M) + FVTRN*CTRNY(M)
                  ENDIF
                ENDIF
                !! !$OMP END CRITICAL
              ENDDO
#ifdef W3_OMPG
              !$OMP END Parallel DO
#endif

              !  Store conservative update of CQA in CQ
              !  The v side length in MF value has to be cancelled with x-size.
              !  Also divided by cell y-size as VCFL is in size-1 unit.
              !! One cosine factor is also needed to be divided for SMC grid.
#ifdef W3_OMPG
              !$OMP Parallel DO Private(n)
#endif
              DO n=icl, jcl
                CQ(n)=CQA(n) + BCNt(n)/( CLATS(n)*            &
                     &             FLOAT( IJKCel3(n)*IJKCel4(n) ) )
                BCNt(n)=0.0
              ENDDO
#ifdef W3_OMPG
              !$OMP END Parallel DO
#endif
              !Li  Polar cell needs a special area factor, multi-level case.
              IF( ARCTC .AND. jcl .EQ. NSEA ) THEN
                CQ(NSEA) = CQA(NSEA) + BCNt(NSEA)*PCArea
              ENDIF
              !
              !  End of refine level if block  MOD(LMN, LvR) .EQ. 0
            ENDIF

            !  End of refine level loop LL=1, NRLv
          ENDDO
          !!
          !!    END of multi-resolution sub-step loop LMN = 1, MRFct
        ENDDO

        !  End of multi-resolution advection ELSE block of NRLv > 1
      ENDIF

      !!   Update boundary spectra if any.  JGLi26Feb2016
      !
      IF ( FLBPI ) THEN
        RD1 = DSEC21(TBPI0, TIME)-DTG*REAL(NTLOC-ITLOC)/REAL(NTLOC)
        RD2 = DSEC21(TBPI0, TBPIN)
        IF ( RD2 .GT. 0.001 ) THEN
          RD2    = MIN(1.,MAX(0.,RD1/RD2))
          RD1    = 1. - RD2
        ELSE
          RD1    = 0.
          RD2    = 1.
        END IF
        DO IBI=1, NBI
          ISEA     = ISBPI(IBI)
          CQ(ISEA) = (RD1*BBPI0(ISP,IBI) + RD2*BBPIN(ISP,IBI))   &
               /CG(IK,ISEA)
        END DO
      ENDIF
      !
      !!    End of ITLOC DO
    ENDDO

    !  Average with 1-2-1 scheme.  JGLi20Aug2015
    IF(FVERG) CALL SMCAverg(CQ)

    !
    ! 4.  Store results in VQ in proper format --------------------------- *
    !
#ifdef W3_OMPG
    !$OMP Parallel DO Private(ISEA)
#endif
    DO ISEA=1, NSEA
      VQ(ISEA) =  MAX ( 0. , CQ(ISEA)*CG(IK,ISEA) )
    END DO
#ifdef W3_OMPG
    !$OMP END Parallel DO
#endif
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9001 FORMAT (' TEST W3PSMC : ISP, ITH, IK, COS-SIN :',I8,2I4,2F7.3)
9003 FORMAT (' TEST W3PSMC : NO DISPERSION CORRECTION ')
9010 FORMAT (' TEST W3PSMC : INITIALIZE ARRAYS')
9020 FORMAT (' TEST W3PSMC : CALCULATING LCFLX/Y AND DSS/NN (NSEA=', &
         I6,')')
#endif
#ifdef W3_T1
9021 FORMAT (1X,I6,2I5,E12.4,2f7.3)
#endif
#ifdef W3_T
9022 FORMAT (' TEST W3PSMC : CORRECTING FOR CURRENT')
9040 FORMAT (' TEST W3PSMC : FIELD AFTER PROP. (NSEA=',I6,')')
#endif
#ifdef W3_T2
9041 FORMAT (1X,I6,2I5,E12.4)
#endif
    !/
    !/ End of W3PSMC ----------------------------------------------------- /
    !/
  END SUBROUTINE W3PSMC
  !/
  !/ ------------------------------------------------------------------- /
  !> @brief Refraction and great-circle turning by spectral rotation
  !>
  !> @details Linear interpolation equivalent to 1st order upstream scheme
  !>  but without restriction on rotation angle.  However, refraction
  !>  is limited towards the depth gradient direction (< 90 degree).
  !>  Refraction induced spectral shift in the k-space will remain
  !>  to be advected using the UNO2 scheme.
  !>
  !> @param[in]    ISEA     Number of sea point
  !> @param[in]    FACTH    Factor in propagation velocity (th)
  !> @param[in]    FACK     Factor in propagation velocity (k)
  !> @param[in]    CTHG0    Factor in great circle refraction term
  !> @param[in]    CG       Local group velocities
  !> @param[in]    WN       Local wavenumbers
  !> @param[in]    DEPTH    Depth
  !> @param[in]    DDDX     Depth x-gradient
  !> @param[in]    DDDY     Depth y-gradient
  !> @param[in]    ALFLMT   Refraction limiter
  !> @param[in]    CX       Current x-component
  !> @param[in]    CY       Current y-component
  !> @param[in]    DCXDX    Current gradient (dCX/dX)
  !> @param[in]    DCXDY    Current gradient (dCX/dY)
  !> @param[in]    DCYDX    Current gradient (dCY/dX)
  !> @param[in]    DCYDY    Current gradient (dCY/dY)
  !> @param[in]    DCDX     Phase speed x-gradient
  !> @param[in]    DCDY     Phase speed y-gradient
  !> @param[inout] VA       Spectrum
  !>
  !> @author Jian-Guo Li
  !> @date 06-Jun-2018
  !>
  SUBROUTINE W3KRTN ( ISEA, FACTH, FACK, CTHG0, CG, WN, DEPTH,    &
       DDDX, DDDY, ALFLMT, CX, CY, DCXDX, DCXDY,   &
       DCYDX, DCYDY, DCDX, DCDY, VA )
    !/
    !/                  +------------------------------------+
    !/                  | Spherical Multiple-Cell (SMC) grid |
    !/                  | Refraction and great-cirle turning |
    !/                  |           Jian-Guo Li              |
    !/                  | First created:     8 Nov 2010      |
    !/                  | Last modified:    06-Jun-2018      |
    !/                  +------------------------------------+
    !/
    !/    08-Nov-2010 : Origination.                        ( version 1.00 )
    !/    10-Jun-2011 : New refraction formulation.         ( version 1.10 )
    !/    16-Jun-2011 : Add refraction limiter to gradient. ( version 1.20 )
    !/    21-Jul-2011 : Old refraction formula + limiter.   ( version 1.30 )
    !/    26-Jul-2011 : Tidy up refraction schemes.         ( version 1.40 )
    !/    28-Jul-2011 : Finalise with old refraction.       ( version 1.50 )
    !/    23-Mar-2016 : Add current option in refraction.   ( version 2.30 )
    !/    06-Jun-2018 : Add DEBUGDCXDX                      ( version 6.04 )
    !/
    !/
    !  1. Purpose :
    !
    !     Refraction and great-circle turning by spectral rotation.
    !
    !  2. Method :
    !
    !     Linear interpolation equivalent to 1st order upstream scheme
    !     but without restriction on rotation angle.  However, refraction
    !     is limited towards the depth gradient direction (< 90 degree).
    !     Refraction induced spectral shift in the k-space will remain
    !     to be advected using the UNO2 scheme.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       ISEA    Int.   I   Number of sea point.
    !       FACTH/K Real   I   Factor in propagation velocity.
    !       CTHG0   Real   I   Factor in great circle refracftion term.
    !       MAPxx2  I.A.   I   Propagation and storage maps.
    !       CG      R.A.   I   Local group velocities.
    !       WN      R.A.   I   Local wavenumbers.
    !       DEPTH   R.A.   I   Depth.
    !       DDDx    Real   I   Depth gradients.
    !       CX/Y    Real   I   Current components.
    !       DCxDx   Real   I   Current gradients.
    !       DCDx    Real   I   Phase speed gradients.
    !       VA      R.A.  I/O  Spectrum.
    !     ----------------------------------------------------------------
    !
    !     Local variables.
    !     ----------------------------------------------------------------
    !       DPH2K   R.A.  2*Depth*Wave_number_K
    !       SNH2K   R.A.  SINH(2*Depth*Wave_number_K)
    !       FDD, FDU, FGC, FCD, FCU
    !               R.A.  Directionally varying part of depth, current and
    !                     great-circle refraction terms and of consit.
    !                     of Ck term.
    !       CFLT-K  R.A.  Propagation velocities of local fluxes.
    !       DB      R.A.  Wavenumber band widths at cell centers.
    !       DM      R.A.  Wavenumber band widths between cell centers and
    !                     next cell center.
    !       Q       R.A.  Extracted spectrum
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !       SMCGtCrfr Refraction and GCT rotation in theta.
    !       SMCkUNO2  Refraction shift in k-space by UNO2.
    !       STRACE    Service routine.
    !
    !  5. Called by :
    !
    !       W3WAVE   Wave model routine.
    !
    !  6. Error messages :
    !
    !       None.
    !
    !  8. Structure :
    !
    !     -----------------------------------------------------------------
    !       1.  Preparations
    !         a Initialize arrays
    !         b Set constants and counters
    !       2.  Point  preparations
    !         a Calculate SNH2K
    !         b Extract spectrum
    !       3.  Refraction velocities
    !         a Filter level depth reffraction.
    !         b Depth refratcion velocity.
    !         c Current refraction velocity.
    !       4.  Wavenumber shift velocities
    !         a Prepare directional arrays
    !         b Calcuate velocity.
    !       5.  Propagate.
    !       6.  Store results.
    !     -----------------------------------------------------------------
    !
    !  9. Switches :
    !
    !       !/S     Enable subroutine tracing.
    !       !/T     Enable general test output.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, DSIP, ECOS, ESIN, &
         EC2, ESC, ES2, FLCTH, FLCK, CTMAX, DTH
    USE W3ADATMD, ONLY: ITIME
    USE W3IDATMD, ONLY: FLCUR
    USE W3ODATMD, ONLY: NDSE, NDST
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN) :: ISEA
#ifdef W3_S
    INTEGER, SAVE       :: IENT = 0
#endif
    REAL, INTENT(IN)    :: FACTH, FACK, CTHG0, CG(0:NK+1),      &
         WN(0:NK+1), DEPTH, DDDX, DDDY,       &
         ALFLMT(NTH), CX, CY, DCXDX, DCXDY,   &
         DCYDX, DCYDY, DCDX(0:NK+1), DCDY(0:NK+1)
    REAL, INTENT(INOUT) :: VA(NSPEC)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER    :: ITH, IK, ISP
    REAL       :: FGC, FKD, FKS, FRK(NK), FRG(NK), DDNorm(NTH),     &
         FKC(NTH), VQ(NSPEC), VCFLT(NSPEC), DEPTH30,       &
         DB(0:NK+1), DM(-1:NK+1), CFLK(NTH,0:NK),          &
         !Li   For new refraction scheme using Cg.  JGLi26Jul2011
         !                   DPH2K(0:NK+1), SNH2K(0:NK+1)
         !Li   For old refraction scheme using phase speed.  JGLi26Jul2011
         SIGSNH(0:NK+1)
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3KRTN')
#endif
    !
    ! 1.  Preparation for point ------------------------------------------ *
    !     Array with partial derivative of sigma versus depth
    !Li   Use of minimum depth 30 m for refraction factor.  JGLi12Feb2014
    DEPTH30=MAX(30.0, DEPTH)

    DO IK=0, NK+1
      !Li   For old refraction scheme using phase speed.  JGLi8Jun2011
      !        DPH2K(IK) = 2.0*WN(IK)*DEPTH
      !        SNH2K(IK) = SINH( DPH2K(IK) )
      !Li   For new refraction scheme using Cg.  JGLi3Jun2011
      !!        SIGSNH(IK) = SIG(IK)/SINH(2.0*WN(IK)*DEPTH)
      !!AC  Replacing SIGSINH with a delimiter to prevent the SINH value from
      !!AC  becoming significantly large. Right now set to a max around 1E21
      !         SIGSNH(IK) = SIG(IK)/SINH(MIN(2.0*WN(IK)*DEPTH,50.0))
      !Li   Refraction factor uses minimum depth of 30 m.  JGLi12Feb2014
      SIGSNH(IK) = SIG(IK)/SINH(MIN(2.0*WN(IK)*DEPTH30,50.0))
    END DO
    !
    ! 2.  Extract spectrum without mapping
    !
    VQ = VA

    ! 3.  Refraction velocities ------------------------------------------ *
    !
    !
    IF ( FLCTH ) THEN
      !
      ! 3.a Set slope filter for depth refraction
      !
      !Li   Lift theta-refraction limit and use new formulation.   25 Nov 2010
      !Li   FACTH  = DTG / DTH / REAL(NTLOC), CTHG0 = - TAN(DERA*Y) / RADIUS
      FGC    = FACTH * CTHG0
      !
      DO IK=1, NK
        !Li   New refraction formulation using Cg only.  JGLi3Jun2011
        !         FRK(IK) = FACTH*2.0*SIG(IK)*(1.-DEPTH*SIG(IK)*SIG(IK)/GRAV)  &
        !     &                                /(DPH2K(IK)+SNH2K(IK))
        !Li   Old refraction formulation using phase speed.  JGLi8Jun2011
        FRK(IK) = FACTH * SIGSNH(IK)
        !Li
        FRG(IK) = FGC * CG(IK)
      END DO
      !
      !Li   Current induced refraction stored in FKC.    JGLi30Mar2016
      IF ( FLCUR ) THEN
        DO ITH=1, NTH
          !Li   Put a CTMAX limit on current theta rotation.  JGLi02Mar2017
          !               FKC(ITH) = FACTH*( DCYDX*ES2(ITH) - DCXDY*EC2(ITH) +  &
          FGC      = FACTH*( DCYDX*ES2(ITH) - DCXDY*EC2(ITH) +  &
               (DCXDX - DCYDY)*ESC(ITH) )
          FKC(ITH) = SIGN( MIN(ABS(FGC), CTMAX), FGC )
        END DO
      ELSE
        FKC(:)=0.0
      END IF
      !
      ! 3.b Depth refraction and great-circle turning.
      !
      DO ITH=1, NTH
        DDNorm(ITH)=ESIN(ITH)*DDDX-ECOS(ITH)*DDDY
        DO IK=1, NK
          ISP = (IK-1)*NTH + ITH
          !Li   Apply depth gradient limited refraction, current and GCT term
          VCFLT(ISP)=FRG(IK)*ECOS(ITH) + FKC(ITH) +          &
               SIGN( MIN(ABS(FRK(IK)*DDNorm(ITH)), ALFLMT(ITH)),  &
               !Li   For new refraction scheme using Cg.  JGLi26Jul2011
               !                             FRK(IK)*DDNorm(ITH) )
               !Li   For old refraction scheme using phase speed.  JGLi26Jul2011
               DDNorm(ITH) )
        END DO
      END DO

    END IF
    !
    ! 4.  Wavenumber shift velocities due to current refraction ---------- *
    !
    IF ( FLCK ) THEN
      !
      ! 4.a Directionally dependent part
      !
      DO ITH=1, NTH
        !Li   Depth induced refraction is suspended as it is absorbed in
        !Li   the fixed frequency bin used for wave spectrum.  JGLi30Mar2016
        !           FKC(ITH) = ( ECOS(ITH)*DDDX + ESIN(ITH)*DDDY )
        FKC(ITH) = -DCXDX*EC2(ITH) -DCYDY*ES2(ITH)               &
             -(DCXDY + DCYDX)*ESC(ITH)
      END DO
      FKD = CX*DDDX + CY*DDDY
      !
      ! 4.b Band widths
      !
      !Li  Cell and side indices for k-dimension are arranged as
      !    Cell:    | -1 | 0 | 1 | 2 | ... | NK | NK+1 | NK+2 |
      !    Side:        -1   0   1   2 ...     NK     NK+1
      !Li  DSIP = SIG(K+1) - SIG(K), radian frequency increment
      !
      DO IK=0, NK
        DB(IK) = DSIP(IK) / CG(IK)
        DM(IK) = WN(IK+1) - WN(IK)
      END DO
      DB(NK+1) = DSIP(NK+1) / CG(NK+1)
      DM(NK+1) = DM(NK)
      DM(  -1) = DM( 0)

      ! 4.c Courant number of k-velocity without dividing by dk
      !!Li  FACK = DTG / REAL(NTLOC)
      !
      DO IK=0, NK
        !Li   For new refraction scheme using Cg.  JGLi3Jun2011
        !           FKS   = - FACK*WN(IK)*SIG(IK)/SNH2K(IK)
        !Li   Old refraction formulation using phase speed.  JGLi8Jun2011
        !           FKS   = - FACK*WN(IK)*SIGSNH(IK)
        !Li   Current induced k-shift.   JGLi30Mar2016
        FKS = MAX( 0.0, CG(IK)*WN(IK)-0.5*SIG(IK) )*FKD /    &
             ( DEPTH30*CG(IK) )
        DO ITH=1, NTH
          CFLK(ITH,IK) = FACK*( FKS + FKC(ITH)*WN(IK) )
        END DO
      END DO
      !Li   No CFL limiter is required here as it is applied in SMCkUNO2.
      !
    END IF
    !
    ! 5.  Propagate ------------------------------------------------------ *
    !
    IF ( MOD(ITIME,2) .EQ. 0 ) THEN
      IF ( FLCK ) THEN
        !!Li  Refraction caused k-space shift.
        CALL SMCkUNO2(CFLK, VQ, DB, DM)
      END IF
      IF ( FLCTH ) THEN
        !!Li  GCT and refraction by rotation in theta direction.
        CALL SMCGtCrfr(VCFLT, VQ)
      END IF
    ELSE
      IF ( FLCTH ) THEN
        !!Li  GCT and refraction by rotation in theta direction.
        CALL SMCGtCrfr(VCFLT, VQ)
      END IF
      IF ( FLCK )  THEN
        !!Li  Refraction caused k-space shift.
        CALL SMCkUNO2(CFLK, VQ, DB, DM)
      END IF
    END IF
    !
    ! 6.  Store reults --------------------------------------------------- *
    !
    VA = VQ
    !
    RETURN
    !
    !/ End of W3KRTN ----------------------------------------------------- /
    !/
  END SUBROUTINE W3KRTN


  !> @brief Calculate mid-flux values for x dimension
  !>
  !> @param[in]   NUA      Start number of U-face list.
  !> @param[in]   NUB      End number of U-face list.
  !> @param[in]   CF       Transported variable.
  !> @param[in]   UC       Veclocity U-component at cell centre.
  !> @param[out]  UFLX     Mid-flux U-component on U-face.
  !> @param[in]   AKDif    Diffusion coefficient.
  !> @param[out]  FU       Advection Mid-flux on U-face.
  !> @param[out]  FX       Diffusion Mid-flux on U-face.
  !> @param[in]   FTS      Timestep fraction for sub-timestep.
  !>
  !> @author Jian-Guo Li
  !> @date 03-Mar-2022
  !>
  ! Subroutine that calculate mid-flux values for x dimension
  SUBROUTINE SMCxUNO2(NUA, NUB, CF, UC, UFLX, AKDif, FU, FX, FTS)

    USE CONSTANTS
    USE W3GDATMD, ONLY: NCel, MRFct, NUFc, IJKCel, IJKUFc, CLATS, &
         IJKCel3, IJKCel4
    USE W3ODATMD, ONLY: NDSE, NDST

    IMPLICIT NONE
    INTEGER, INTENT( IN):: NUA, NUB
    REAL,    INTENT( IN):: CF(-9:NCel), UC(-9:NCel), AKDif, FTS
    REAL,    INTENT(Out):: UFLX(NUFc), FU(NUFc), FX(NUFc)
    !
    INTEGER ::  i, j, k, L, M, N, ij
    REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6, CNST8, CNST9

    !     Two layer of boundary cells are added to each boundary cell face
    !     with all boundary cell values CF(-9:0)=0.0.

    !     Diffusion Fourier no. at sub-time-step, proportional to face size,
    !     which is also equal to the sub-time-step factor FTS.
    CNST0=AKDif*FTS*FTS
    !     Uniform diffusion coefficient for all sizes.  JGLi24Feb2012
    !        CNST0=AKDif*MRFct*FTS

#ifdef W3_OMPG
    !$OMP Parallel Default(Shared), Private(i, ij, K, L, M, N),  &
    !$OMP& Private(CNST,CNST1,CNST2,CNST3,CNST4,CNST5,CNST6,CNST8,CNST9)
#endif

    !    Notice an extra side length L is multiplied to mid-flux to give correct
    !    proportion of flux into the cells.  This length will be removed by the
    !    cell length when the tracer concentration is updated.

#ifdef W3_OMPG
    !$OMP DO
#endif

    DO i=NUA, NUB

      !    Select Upstream, Central and Downstream cells
      K=IJKUFc(4,i)
      L=IJKUFc(5,i)
      M=IJKUFc(6,i)
      N=IJKUFc(7,i)

      !    Face bounding cell lengths and central gradient
      CNST2=FLOAT( IJKCel3(L) )
      CNST3=FLOAT( IJKCel3(M) )
      CNST5=(CF(M)-CF(L))/( CNST2 + CNST3 )

      !    Courant number in local size-1 cell, arithmetic average.
      CNST6=0.5*( UC(L)+UC(M) )*FTS
      UFLX(i) = CNST6

      !    Multi-resolution SMC grid requires flux multiplied by face factor.
      CNST8 = FLOAT( IJKUFc(3,i) )

      !    Diffusion factor in local size-1 cell, plus the cosine factors.
      !    2.0 factor to cancel that in gradient CNST5.  JGLi08Mar2012
      !    The maximum cell number is used to avoid the boundary cell number
      !    in selection of the cosine factor.
      ij= MAX(L, M)
      CNST9 = 2.0/( CLATS( ij )*CLATS( ij ) )

      !    For positive velocity case
      IF(CNST6 >= 0.0)  THEN

        !    Use central cell velocity for boundary flux.  JGLi06Apr2011
        IF( M .LE. 0) UFLX(i) = UC(L)*FTS

        !    Upstream cell length and gradient, depending on UFLX sign.
        CNST1=FLOAT( IJKCel3(K) )
        CNST4=(CF(L)-CF(K))/( CNST2 + CNST1 )

        !    Use minimum gradient all region.
        CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        !    Mid-flux value inside central cell
        FU(i)=(CF(L) + CNST*(CNST2 - UFLX(i)))*CNST8

        !    For negative velocity case
      ELSE

        !    Use central cell velocity for boundary flux.  JGLi06Apr2011
        IF( L .LE. 0) UFLX(i) = UC(M)*FTS

        !    Upstream cell length and gradient, depending on UFLX sign.
        CNST1=FLOAT( IJKCel3(N) )
        CNST4=(CF(N)-CF(M))/( CNST1 + CNST3 )

        !    Use minimum gradient outside monotonic region.
        CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        !    Mid-flux value inside central cell M
        FU(i)=(CF(M) - CNST*(CNST3+UFLX(i)))*CNST8

      ENDIF

      !    Diffusion flux by face gradient x DT
      FX(i)=CNST0*CNST5*CNST8*CNST9

    END DO

#ifdef W3_OMPG
    !$OMP END DO
    !$OMP END Parallel
#endif

    RETURN
  END SUBROUTINE SMCxUNO2


  !> @brief Calculate mid-flux values for y dimension
  !>
  !> @param[in]   NVA      Start number of V-face list.
  !> @param[in]   NVB      End number of V-face list.
  !> @param[in]   CF       Transported variable.
  !> @param[in]   VC       Veclocity V-component at cell centre.
  !> @param[out]  VFLY     Mid-flux V-component on V-face.
  !> @param[in]   AKDif    Diffusion coefficient.
  !> @param[out]  FV       Advection Mid-flux on V-face.
  !> @param[out]  FY       Diffusion Mid-flux on V-face.
  !> @param[in]   FTS      Timestep fraction for sub-timestep.
  !>
  !> @author Jian-Guo Li
  !> @date 03-Mar-2022
  !>
  SUBROUTINE SMCyUNO2(NVA, NVB, CF, VC, VFLY, AKDif, FV, FY, FTS)

    USE CONSTANTS
    USE W3GDATMD, ONLY: NCel, MRFct, NVFc, IJKCel, IJKVFc, CLATF, IJKCel4
    USE W3ODATMD, ONLY: NDSE, NDST

    IMPLICIT NONE
    INTEGER, INTENT( IN):: NVA, NVB
    REAL,    INTENT( IN):: CF(-9:NCel), VC(-9:NCel), AKDif, FTS
    REAL,    INTENT(Out):: VFLY(NVFc), FV(NVFc), FY(NVFc)
    INTEGER ::  i, j, k, L, M, N, ij
    REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6, CNST8

    !     Notice an extra side length L is multiplied to mid-flux to give correct
    !     proportion of flux into the cells.  This length will be removed by the
    !     cell length when the tracer concentration is updated.

    !     Diffusion Fourier no. at sub-time-step, proportional to face size,
    !     which is also equal to the sub-time-step factor FTS.
    !        CNST0=AKDif*FTS*FTS
    !     2.0 factor to cancel that in gradient CNST5.  JGLi08Mar2012
    CNST0=AKDif*FTS*FTS*2.0
    !     Uniform diffusion coefficient for all sizes.  JGLi24Feb2012
    !        CNST0=AKDif*MRFct*FTS

#ifdef W3_OMPG
    !$OMP Parallel Default(Shared), Private(j, K, L, M, N),  &
    !$OMP& Private(CNST,CNST1,CNST2,CNST3,CNST4,CNST5,CNST6,CNST8)
    !$OMP DO
#endif

    DO j=NVA, NVB

      !    Select Upstream, Central and Downstream cells
      K=IJKVFc(4,j)
      L=IJKVFc(5,j)
      M=IJKVFc(6,j)
      N=IJKVFc(7,j)

      !    Face bounding cell lengths and gradient
      CNST2=FLOAT( IJKCel4(L) )
      CNST3=FLOAT( IJKCel4(M) )
      CNST5=(CF(M)-CF(L))/( CNST2 + CNST3 )

      !    Courant number in local size-1 cell unit
      !    Multiply by multi-resolution time step factor  FTS
      CNST6=0.5*( VC(L)+VC(M) )*FTS
      VFLY(j) = CNST6

      !    Face size integer and cosine factor.
      !    CLATF is defined on V-face for SMC grid.  JGLi28Feb2012
      CNST8=CLATF(j)*FLOAT( IJKVFc(3,j) )

      !    For positive velocity case
      IF(CNST6 >= 0.0)  THEN

        !    Boundary cell y-size is set equal to central cell y-size
        !    as y-boundary cell sizes are not proportional to refined
        !    inner cells but constant of the base cell y-size, and
        !    Use central cell speed for face speed.  JGLi06Apr2011
        IF( M .LE. 0 ) THEN
          VFLY(j) = VC(L)*FTS
          CNST3   = CNST2
        ENDIF

        !    Upstream cell size and irregular grid gradient, depending on VFLY.
        CNST1=FLOAT( IJKCel4(K) )
        CNST4=(CF(L)-CF(K))/( CNST2 + CNST1 )

        !    Use minimum gradient outside monotonic region
        CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        !    Mid-flux value multiplied by face width and cosine factor
        FV(j)=( CF(L) + CNST*(CNST2 - VFLY(j)) )*CNST8

        !    For negative velocity case
      ELSE

        !    Set boundary cell y-size equal to central cell y-size and
        !    Use central cell speed for flux face speed.  JGLi06Apr2011
        IF( L .LE. 0 ) THEN
          VFLY(j) = VC(M)*FTS
          CNST2   = CNST3
        ENDIF

        !    Upstream cell size and gradient, depending on VFLY sign.
        !    Side gradients for central cell includs 0.5 factor.
        CNST1=FLOAT( IJKCel4(N) )
        CNST4=(CF(N)-CF(M))/( CNST1 + CNST3 )

        !    Use minimum gradient outside monotonic region
        CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        !    Mid-flux value multiplied by face width and cosine factor
        FV(j)=( CF(M) - CNST*(CNST3 + VFLY(j)) )*CNST8

      ENDIF

      !    Diffusion flux by face gradient x DT x face_width x cos(lat)
      !    Multiply by multi-resolution time step factor FTS
      FY(j)=CNST0*CNST5*CNST8

    END DO

#ifdef W3_OMPG
    !$OMP END DO
    !$OMP END Parallel
#endif

    RETURN
  END SUBROUTINE SMCyUNO2


  !> @brief Calculate mid-flux values for x dimension
  !>
  !> @param[in]   NUA      Start number of U-face list.
  !> @param[in]   NUB      End number of U-face list.
  !> @param[in]   CF       Transported variable.
  !> @param[in]   UC       Veclocity U-component at cell centre.
  !> @param[out]  UFLX     Mid-flux U-component on U-face.
  !> @param[in]   AKDif    Diffusion coefficient.
  !> @param[out]  FU       Advection Mid-flux on U-face.
  !> @param[out]  FX       Diffusion Mid-flux on U-face.
  !>
  !> @author Jian-Guo Li
  !> @date 03-Mar-2022
  !>
  SUBROUTINE SMCxUNO2r(NUA, NUB, CF, UC, UFLX, AKDif, FU, FX)

    USE CONSTANTS
    USE W3GDATMD, ONLY: NSEA, NY, NCel, NUFc, IJKCel, IJKUFc, CLATS
    USE W3GDATMD, ONLY: IJKCel3
    USE W3ODATMD, ONLY: NDSE, NDST

    IMPLICIT NONE
    INTEGER, INTENT( IN):: NUA, NUB
    REAL,    INTENT( IN):: CF(-9:NCel), UC(-9:NCel), AKDif
    REAL,    INTENT(Out):: UFLX(NUFc), FU(NUFc), FX(NUFc)
    !
    INTEGER ::  i, j, k, L, M, N, ij
    REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6

    !      Two layer of boundary cells are added to each boundary cell face
    !      with all boundary cell values CF(-9:0)=0.0.

    !      Notice an extra side length L is multiplied to mid-flux to give correct
    !      proportion of flux into the cells.  This length will be removed by the
    !      cell length when the tracer concentration is updated.

#ifdef W3_OMPG
    !$OMP Parallel Default(Shared), Private(i, ij, K, L, M, N),  &
    !$OMP& Private(CNST,CNST0,CNST1,CNST2,CNST3,CNST4,CNST5,CNST6)
    !$OMP DO
#endif

    DO i=NUA, NUB

      !    Select Upstream, Central and Downstream cells
      K=IJKUFc(4,i)
      L=IJKUFc(5,i)
      M=IJKUFc(6,i)
      N=IJKUFc(7,i)

      !    Face bounding cell lengths and gradient
      CNST2=FLOAT( IJKCel3(L) )
      CNST3=FLOAT( IJKCel3(M) )
      CNST5=(CF(M)-CF(L))

      !    Averaged Courant number for base-level cell face
      CNST6= 0.5*( UC(L)+UC(M) )
      UFLX(i) = CNST6

      !    Diffusion Fourier number in local cell size
      !    To avoid boundary cell number, use maximum of L and M.
      ij= MAX(L, M)
      CNST0 = 2.0/( CLATS(ij)*CLATS(ij) )

      !    For positive velocity case
      IF(CNST6 >= 0.0)  THEN

        !    Use central cell velocity for boundary flux.  JGLi06Apr2011
        IF( M .LE. 0) UFLX(i) = UC(L)

        !    Side gradient for upstream cell as regular grid.
        CNST4=(CF(L)-CF(K))

        !    Use minimum gradient all region with 0.5 factor
        CNST=Sign(0.5, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        !    Mid-flux value inside central cell
        FU(i)=(CF(L) + CNST*(1.0-UFLX(i)/CNST2))

        !    For negative velocity case
      ELSE

        !    Use central cell velocity for boundary flux.  JGLi06Apr2011
        IF( L .LE. 0) UFLX(i) = UC(M)

        !    Side gradient for upstream cell, depneding on UFLX sign.
        CNST4=(CF(N)-CF(M))

        !    Use minimum gradient outside monotonic region, include 0.5 factor
        CNST=Sign(0.5, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        !    Mid-flux value inside central cell M
        FU(i)=(CF(M) - CNST*(1.0+UFLX(i)/CNST3))

      ENDIF

      !    Diffusion flux by face gradient x DT
      FX(i)=AKDif*CNST0*CNST5/(CNST2 + CNST3)

    END DO

#ifdef W3_OMPG
    !$OMP END DO
    !$OMP END Parallel
#endif

    RETURN
  END SUBROUTINE SMCxUNO2r


  !> @brief Calculate mid-flux values for y dimension
  !>
  !> @param[in]   NVA      Start number of V-face list.
  !> @param[in]   NVB      End number of V-face list.
  !> @param[in]   CF       Transported variable.
  !> @param[in]   VC       Veclocity V-component at cell centre.
  !> @param[out]  VFLY     Mid-flux V-component on V-face.
  !> @param[in]   AKDif    Diffusion coefficient.
  !> @param[out]  FV       Advection Mid-flux on V-face.
  !> @param[out]  FY       Diffusion Mid-flux on V-face.
  !>
  !> @author Jian-Guo Li
  !> @date 03-Mar-2022
  !>
  SUBROUTINE SMCyUNO2r(NVA, NVB, CF, VC, VFLY, AKDif, FV, FY)

    USE CONSTANTS
    USE W3GDATMD, ONLY: NSEA, NY, NCel, NVFc, IJKCel, IJKVFc, CLATF
    USE W3ODATMD, ONLY: NDSE, NDST

    IMPLICIT NONE
    INTEGER, INTENT( IN):: NVA, NVB
    REAL,    INTENT( IN):: CF(-9:NCel), VC(-9:NCel), AKDif
    REAL,    INTENT(Out):: VFLY(NVFc), FV(NVFc), FY(NVFc)
    INTEGER ::  i, j, k, L, M, N, ij
    REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6, CNST8

    !     Notice an extra side length L is multiplied to mid-flux to give correct
    !     proportion of flux into the cells.  This length will be removed by the
    !     cell length when the tracer concentration is updated.

#ifdef W3_OMPG
    !$OMP Parallel Default(Shared), Private(j, K, L, M, N),  &
    !$OMP& Private(CNST,CNST4,CNST5,CNST6,CNST8)
    !$OMP DO
#endif

    DO j=NVA, NVB

      !    Select Upstream, Central and Downstream cells
      K=IJKVFc(4,j)
      L=IJKVFc(5,j)
      M=IJKVFc(6,j)
      N=IJKVFc(7,j)

      !    Central face gradient.
      CNST5=(CF(M)-CF(L))

      !    Courant number in basic cell unit as dy is constant
      CNST6=0.5*( VC(L)+VC(M) )
      VFLY(j) = CNST6

      !    Face size integer and cosine factor
      !    CLATF is defined on V-face for SMC grid.  JGLi28Feb2012
      CNST8=CLatF(j)*FLOAT( IJKVFc(3,j) )

      !    For positive velocity case
      IF(CNST6 >= 0.0)  THEN

        !    Use central cell speed for flux face speed.  JGLi06Apr2011
        IF( M .LE. 0 ) VFLY(j) = VC(L)

        !    Upstream face gradient, depending on VFLY sign.
        CNST4=(CF(L)-CF(K))

        !    Use minimum gradient, including 0.5 factor and central sign.
        CNST=Sign(0.5, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        !    Mid-flux value multiplied by face width and cosine factor
        FV(j)=( CF(L) + CNST*(1.0-VFLY(j)) )*CNST8

        !    For negative velocity case
      ELSE

        !    Use central cell speed for flux face speed.  JGLi06Apr2011
        IF( L .LE. 0 ) VFLY(j) = VC(M)

        !    Side gradients for upstream face, depending on VFLY sign.
        CNST4=(CF(N)-CF(M))

        !    Use minimum gradient, including 0.5 factor and central sign.
        CNST=Sign(0.5, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        !    Mid-flux value multiplied by face width and cosine factor
        FV(j)=( CF(M) - CNST*(1.0+VFLY(j)) )*CNST8

      ENDIF

      !    Diffusion flux by face gradient x DT x face_width x cos(lat)
      FY(j)=AKDif*CNST5*CNST8

    END DO

#ifdef W3_OMPG
    !$OMP END DO
    !$OMP END Parallel
#endif

    RETURN
  END SUBROUTINE SMCyUNO2r


  !> @brief Calculate mid-flux values for x dimension with UNO3 scheme
  !>
  !> @param[in]   NUA      Start number of U-face list.
  !> @param[in]   NUB      End number of U-face list.
  !> @param[in]   CF       Transported variable.
  !> @param[in]   UC       Veclocity U-component at cell centre.
  !> @param[out]  UFLX     Mid-flux U-component on U-face.
  !> @param[in]   AKDif    Diffusion coefficient.
  !> @param[out]  FU       Advection Mid-flux on U-face.
  !> @param[out]  FX       Diffusion Mid-flux on U-face.
  !> @param[in]   FTS      Timestep fraction for sub-timestep.
  !>
  !> @author Jian-Guo Li
  !> @date 03-Mar-2022
  !>
  SUBROUTINE SMCxUNO3(NUA, NUB, CF, UC, UFLX, AKDif, FU, FX, FTS)

    USE CONSTANTS
    USE W3GDATMD, ONLY: NCel, MRFct, NUFc, IJKCel, IJKUFc, CLATS
    USE W3GDATMD, ONLY: IJKCel3
    USE W3ODATMD, ONLY: NDSE, NDST

    IMPLICIT NONE
    INTEGER, INTENT( IN):: NUA, NUB
    REAL,    INTENT( IN):: CF(-9:NCel), UC(-9:NCel), AKDif, FTS
    REAL,    INTENT(Out):: UFLX(NUFc), FU(NUFc), FX(NUFc)
    !
    INTEGER ::  i, j, k, L, M, N, ij
    REAL    :: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6,  &
         CNST7, CNST8, CNST9

    !     Two layer of boundary cells are added to each boundary cell face
    !     with all boundary cell values CF(-9:0)=0.0.

    !     Diffusion Fourier no. at sub-time-step, proportional to face size,
    !     which is also equal to the sub-time-step factor FTS.
    !         CNST0=AKDif*FTS*FTS
    !     2.0 factor to cancel that in gradient CNST5.  JGLi03Sep2015
    CNST0=AKDif*FTS*FTS*2.0

    !     Notice an extra side length L is multiplied to mid-flux to give correct
    !     proportion of flux into the cells.  This length will be removed by the
    !     cell length when the tracer concentration is updated.

#ifdef W3_OMPG
    !$OMP Parallel Default(Shared), Private(i, ij, K, L, M, N),  &
    !$OMP& Private(CNST,CNST1,CNST2,CNST3,CNST4,CNST5,CNST6,CNST7,CNST8,CNST9)
    !$OMP DO
#endif

    DO i=NUA, NUB

      !    Select Upstream, Central and Downstream cells
      K=IJKUFc(4,i)
      L=IJKUFc(5,i)
      M=IJKUFc(6,i)
      N=IJKUFc(7,i)

      !    Face bounding cell lengths and central gradient
      CNST2=FLOAT( IJKCel3(L) )
      CNST3=FLOAT( IJKCel3(M) )
      CNST5=(CF(M)-CF(L))/( CNST2 + CNST3 )

      !    Courant number in local size-1 cell, arithmetic average.
      CNST6=0.5*( UC(L)+UC(M) )*FTS
      UFLX(i) = CNST6

      !    Multi-resolution SMC grid requires flux multiplied by face factor.
      CNST8 = FLOAT( IJKUFc(3,i) )

      !    Diffusion factor in local size-1 cell, plus the cosine factors.
      !    2.0 factor to cancel that in gradient CNST5.  JGLi08Mar2012
      !    The maximum cell number is used to avoid the boundary cell number
      !    in selection of the cosine factor.
      ij= MAX(L, M)

      !    For positive velocity case
      IF(CNST6 >= 0.0)  THEN

        !    Use central cell velocity for boundary flux.  JGLi06Apr2011
        IF( M .LE. 0) UFLX(i) = UC(L)*FTS

        !    Upstream cell length and gradient, depending on UFLX sign.
        CNST1=FLOAT( IJKCel3(K) )
        CNST4=(CF(L)-CF(K))/( CNST2 + CNST1 )

        !    Second order gradient
        CNST7 = CNST5 - CNST4
        CNST9 = 2.0/( CNST3+CNST2+CNST2+CNST1 )

        !    Use 3rd order scheme
        IF( Abs(CNST7) .LT. 0.6*CNST9*Abs(CF(M)-CF(K)) ) THEN
          CNST= CNST5 - ( CNST3+UFLX(i) )*CNST7*CNST9/1.5

          !    Use doubled UNO2 scheme
        ELSE IF( DBLE(CNST4)*DBLE(CNST5) .GT. 0.d0 ) THEN
          CNST=Sign(2.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        ELSE
          !    Use minimum gradient UNO2 scheme
          CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        ENDIF

        !    Mid-flux value inside central cell
        FU(i)=(CF(L) + CNST*(CNST2 - UFLX(i)))*CNST8

        !    For negative velocity case
      ELSE

        !    Use central cell velocity for boundary flux.  JGLi06Apr2011
        IF( L .LE. 0) UFLX(i) = UC(M)*FTS

        !    Upstream cell length and gradient, depending on UFLX sign.
        CNST1=FLOAT( IJKCel3(N) )
        CNST4=(CF(N)-CF(M))/( CNST1 + CNST3 )

        !    Second order gradient
        CNST7 = CNST4 - CNST5
        CNST9 = 2.0/( CNST2+CNST3+CNST3+CNST1 )

        !    Use 3rd order scheme
        IF( Abs(CNST7) .LT. 0.6*CNST9*Abs(CF(N)-CF(L)) ) THEN
          CNST= CNST5 + ( CNST2-UFLX(i) )*CNST7*CNST9/1.5

          !    Use doubled UNO2 scheme
        ELSE IF( DBLE(CNST4)*DBLE(CNST5) .GT. 0.d0 ) THEN
          CNST=Sign(2.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        ELSE
          !    Use minimum gradient UNO2 scheme.
          CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        ENDIF

        !    Mid-flux value inside central cell M
        FU(i)=(CF(M) - CNST*(CNST3+UFLX(i)))*CNST8

      ENDIF

      !    Diffusion flux by face gradient x DT
      FX(i)=CNST0*CNST5*CNST8/( CLATS( ij )*CLATS( ij ) )

    END DO

#ifdef W3_OMPG
    !$OMP END DO
    !$OMP END Parallel
#endif

    RETURN
  END SUBROUTINE SMCxUNO3


  !> @brief Calculate mid-flux values for y dimension with UNO3 scheme
  !>
  !>
  !> @param[in]   NVA      Start number of V-face list.
  !> @param[in]   NVB      End number of V-face list.
  !> @param[in]   CF       Transported variable.
  !> @param[in]   VC       Veclocity V-component at cell centre.
  !> @param[out]  VFLY     Mid-flux V-component on V-face.
  !> @param[in]   AKDif    Diffusion coefficient.
  !> @param[out]  FV       Advection Mid-flux on V-face.
  !> @param[out]  FY       Diffusion Mid-flux on V-face.
  !> @param[in]   FTS      Timestep fraction for sub-timestep.
  !>
  !> @author Jian-Guo Li
  !> @date 03-Mar-2022
  !>
  SUBROUTINE SMCyUNO3(NVA, NVB, CF, VC, VFLY, AKDif, FV, FY, FTS)

    USE CONSTANTS
    USE W3GDATMD, ONLY: NCel, MRFct, NVFc, IJKCel, IJKVFc, CLATF
    USE W3GDATMD, ONLY: IJKCel4
    USE W3ODATMD, ONLY: NDSE, NDST

    IMPLICIT NONE
    INTEGER, INTENT( IN):: NVA, NVB
    REAL,    INTENT( IN):: CF(-9:NCel), VC(-9:NCel), AKDif, FTS
    REAL,    INTENT(Out):: VFLY(NVFc), FV(NVFc), FY(NVFc)
    INTEGER ::  i, j, k, L, M, N, ij
    REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6,  &
         CNST7, CNST8, CNST9

    !     Notice an extra side length L is multiplied to mid-flux to give correct
    !     proportion of flux into the cells.  This length will be removed by the
    !     cell length when the tracer concentration is updated.

    !     Diffusion Fourier no. at sub-time-step, proportional to face size,
    !     which is also equal to the sub-time-step factor FTS.
    !        CNST0=AKDif*FTS*FTS
    !     2.0 factor to cancel that in gradient CNST5.  JGLi08Mar2012
    CNST0=AKDif*FTS*FTS*2.0
    !     Uniform diffusion coefficient for all sizes.  JGLi24Feb2012
    !        CNST0=AKDif*MRFct*FTS

#ifdef W3_OMPG
    !$OMP Parallel Default(Shared), Private(j, K, L, M, N),  &
    !$OMP& Private(CNST,CNST1,CNST2,CNST3,CNST4,CNST5,CNST6,CNST7,CNST8,CNST9)
    !$OMP DO
#endif

    DO j=NVA, NVB

      !    Select Upstream, Central and Downstream cells
      K=IJKVFc(4,j)
      L=IJKVFc(5,j)
      M=IJKVFc(6,j)
      N=IJKVFc(7,j)

      !    Face bounding cell lengths and gradient
      CNST2=FLOAT( IJKCel4(L) )
      CNST3=FLOAT( IJKCel4(M) )
      CNST5=(CF(M)-CF(L))/( CNST2 + CNST3 )

      !    Courant number in local size-1 cell unit
      !    Multiply by multi-resolution time step factor  FTS
      CNST6=0.5*( VC(L)+VC(M) )*FTS
      VFLY(j) = CNST6

      !    Face size integer and cosine factor.
      !    CLATF is defined on V-face for SMC grid.  JGLi28Feb2012
      CNST8=CLATF(j)*FLOAT( IJKVFc(3,j) )

      !    For positive velocity case
      IF(CNST6 >= 0.0)  THEN

        !    Boundary cell y-size is set equal to central cell y-size
        !    as y-boundary cell sizes are not proportional to refined
        !    inner cells but constant of the base cell y-size, and
        !    Use central cell speed for face speed.  JGLi06Apr2011
        IF( M .LE. 0 ) THEN
          VFLY(j) = VC(L)*FTS
          CNST3   = CNST2
        ENDIF

        !    Upstream cell size and irregular grid gradient, depending on VFLY.
        CNST1=FLOAT( IJKCel4(K) )
        CNST4=(CF(L)-CF(K))/( CNST2 + CNST1 )

        !    Second order gradient
        CNST7 = CNST5 - CNST4
        CNST9 = 2.0/( CNST3+CNST2+CNST2+CNST1 )

        !    Use 3rd order scheme
        IF( Abs(CNST7) .LT. 0.6*CNST9*Abs(CF(M)-CF(K)) ) THEN
          CNST= CNST5 - ( CNST3+VFLY(j) )*CNST7*CNST9/1.5

          !    Use doubled UNO2 scheme
        ELSE IF( DBLE(CNST4)*DBLE(CNST5) .GT. 0.d0 ) THEN
          CNST=Sign(2.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        ELSE

          !    Use minimum gradient outside monotonic region
          CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        ENDIF

        !    Mid-flux value multiplied by face width and cosine factor
        FV(j)=( CF(L) + CNST*(CNST2 - VFLY(j)) )*CNST8

        !    For negative velocity case
      ELSE

        !    Set boundary cell y-size equal to central cell y-size and
        !    Use central cell speed for flux face speed.  JGLi06Apr2011
        IF( L .LE. 0 ) THEN
          VFLY(j) = VC(M)*FTS
          CNST2   = CNST3
        ENDIF

        !    Upstream cell size and gradient, depending on VFLY sign.
        !    Side gradients for central cell includs 0.5 factor.
        CNST1=FLOAT( IJKCel4(N) )
        CNST4=(CF(N)-CF(M))/( CNST1 + CNST3 )

        !    Second order gradient
        CNST7 = CNST4 - CNST5
        CNST9 = 2.0/( CNST2+CNST3+CNST3+CNST1 )

        !    Use 3rd order scheme
        IF( Abs(CNST7) .LT. 0.6*CNST9*Abs(CF(N)-CF(L)) ) THEN
          CNST= CNST5 + ( CNST2-VFLY(j) )*CNST7*CNST9/1.5

          !    Use doubled UNO2 scheme
        ELSE IF( DBLE(CNST4)*DBLE(CNST5) .GT. 0.d0 ) THEN
          CNST=Sign(2.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        ELSE

          !    Use minimum gradient outside monotonic region
          CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

        ENDIF

        !    Mid-flux value multiplied by face width and cosine factor
        FV(j)=( CF(M) - CNST*(CNST3 + VFLY(j)) )*CNST8

      ENDIF

      !    Diffusion flux by face gradient x DT x face_width x cos(lat)
      !    Multiply by multi-resolution time step factor FTS
      FY(j)=CNST0*CNST5*CNST8

    END DO

#ifdef W3_OMPG
    !$OMP END DO
    !$OMP END Parallel
#endif

    RETURN
  END SUBROUTINE SMCyUNO3


  !> @brief Calculate mid-flux values for x dimension with UNO3
  !>
  !> @param[in]   NUA      Start number of U-face list.
  !> @param[in]   NUB      End number of U-face list.
  !> @param[in]   CF       Transported variable.
  !> @param[in]   UC       Veclocity U-component at cell centre.
  !> @param[out]  UFLX     Mid-flux U-component on U-face.
  !> @param[in]   AKDif    Diffusion coefficient.
  !> @param[out]  FU       Advection Mid-flux on U-face.
  !> @param[out]  FX       Diffusion Mid-flux on U-face.
  !>
  !> @author Jian-Guo Li
  !> @date 03-Mar-2022
  !>
  SUBROUTINE SMCxUNO3r(NUA, NUB, CF, UC, UFLX, AKDif, FU, FX)

    USE CONSTANTS
    USE W3GDATMD, ONLY: NSEA, NY, NCel, NUFc, IJKCel, IJKUFc, CLATS
    USE W3GDATMD, ONLY: IJKCel3
    USE W3ODATMD, ONLY: NDSE, NDST

    IMPLICIT NONE
    INTEGER, INTENT( IN):: NUA, NUB
    REAL,    INTENT( IN):: CF(-9:NCel), UC(-9:NCel), AKDif
    REAL,    INTENT(Out):: UFLX(NUFc), FU(NUFc), FX(NUFc)
    !
    INTEGER ::  i, j, k, L, M, N, ij
    REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6, &
         CNST7, CNST8, CNST9
    !    Two layer of boundary cells are added to each boundary cell face
    !    with all boundary cell values CF(-9:0)=0.0.

    !    Notice an extra side length L is multiplied to mid-flux to give correct
    !    proportion of flux into the cells.  This length will be removed by the
    !    cell length when the tracer concentration is updated.

#ifdef W3_OMPG
    !$OMP Parallel Default(Shared), Private(i, ij, K, L, M, N),  &
    !$OMP& Private(CNST,CNST0,CNST1,CNST2,CNST3,CNST4,CNST5,CNST6,CNST7,CNST8,CNST9)
    !$OMP DO
#endif

    DO i=NUA, NUB

      !    Select Upstream, Central and Downstream cells
      K=IJKUFc(4,i)
      L=IJKUFc(5,i)
      M=IJKUFc(6,i)
      N=IJKUFc(7,i)

      !    Face bounding cell lengths and gradient
      CNST2=FLOAT( IJKCel3(L) )
      CNST3=FLOAT( IJKCel3(M) )
      CNST5=(CF(M)-CF(L))

      !    Averaged Courant number for base-level cell face
      CNST6= 0.5*( UC(L)+UC(M) )
      UFLX(i) = CNST6

      !    Diffusion Fourier number in local cell size
      !    To avoid boundary cell number, use maximum of L and M.
      ij= MAX(L, M)
      CNST0 = 2.0/( CLATS(ij)*CLATS(ij) )

      !    For positive velocity case
      IF(CNST6 >= 0.0)  THEN

        !    Use central cell velocity for boundary flux.  JGLi06Apr2011
        IF( M .LE. 0) UFLX(i) = UC(L)

        !    Side gradient for upstream cell as regular grid.
        CNST4=(CF(L)-CF(K))
        CNST8=(CF(M)-CF(K))
        CNST9=(CNST5-CNST4)

        IF( Abs(CNST9) <= 0.6*Abs(CNST8) )  THEN
          !    Use 3rd order scheme in limited zone, note division by 2 grid sizes
          CNST=0.5*CNST5 - (1.0+UFLX(i)/CNST2)*CNST9/6.0
        ELSE IF( DBLE(CNST4)*DBLE(CNST5) .GT. 0.d0 ) THEN
          !    Use doubled minimum gradient in rest of monotonic region
          CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )
        ELSE
          !    Use minimum gradient all region with 0.5 factor
          CNST=Sign(0.5, CNST5)*min( Abs(CNST4), Abs(CNST5) )
        ENDIF
        !    Mid-flux value inside central cell
        FU(i)=(CF(L) + CNST*(1.0-UFLX(i)/CNST2))

        !    For negative velocity case
      ELSE

        !    Use central cell velocity for boundary flux.  JGLi06Apr2011
        IF( L .LE. 0) UFLX(i) = UC(M)

        !    Side gradient for upstream cell, depneding on UFLX sign.
        CNST4=(CF(N)-CF(M))
        CNST8=(CF(N)-CF(L))
        CNST9=(CNST4-CNST5)

        IF( Abs(CNST9) <= 0.6*Abs(CNST8) )  THEN
          !    Use 3rd order scheme in limited zone, note division by 2 grid sizes
          CNST=0.5*CNST5 + (1.0-UFLX(i)/CNST3)*CNST9/6.0
        ELSE IF( DBLE(CNST4)*DBLE(CNST5) .GT. 0.d0 ) THEN
          !    Use doubled minimum gradient in rest of monotonic region
          CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )
        ELSE
          !    Use minimum gradient outside monotonic region, include 0.5 factor
          CNST=Sign(0.5, CNST5)*min( Abs(CNST4), Abs(CNST5) )
        ENDIF

        !    Mid-flux value inside central cell M
        FU(i)=(CF(M) - CNST*(1.0+UFLX(i)/CNST3))

      ENDIF

      !    Diffusion flux by face gradient x DT
      FX(i)=AKDif*CNST0*CNST5/(CNST2 + CNST3)

    END DO

#ifdef W3_OMPG
    !$OMP END DO
    !$OMP END Parallel
#endif

    ! 999  PRINT*, ' Sub SMCxUNO3r ended.'

    RETURN
  END SUBROUTINE SMCxUNO3r


  !> @brief Calculate mid-flux values for y dimension with UNO3
  !>
  !> @param[in]   NVA      Start number of V-face list.
  !> @param[in]   NVB      End number of V-face list.
  !> @param[in]   CF       Transported variable.
  !> @param[in]   VC       Veclocity V-component at cell centre.
  !> @param[out]  VFLY     Mid-flux V-component on V-face.
  !> @param[in]   AKDif    Diffusion coefficient.
  !> @param[out]  FV       Advection Mid-flux on V-face.
  !> @param[out]  FY       Diffusion Mid-flux on V-face.
  !>
  !> @author Jian-Guo Li
  !> @date 03-Mar-2022
  !>
  SUBROUTINE SMCyUNO3r(NVA, NVB, CF, VC, VFLY, AKDif, FV, FY)

    USE CONSTANTS
    USE W3GDATMD, ONLY: NSEA, NY, NCel, NVFc, IJKCel, IJKVFc, CLATF
    USE W3ODATMD, ONLY: NDSE, NDST

    IMPLICIT NONE
    INTEGER, INTENT( IN):: NVA, NVB
    REAL,    INTENT( IN):: CF(-9:NCel), VC(-9:NCel), AKDif
    REAL,    INTENT(Out):: VFLY(NVFc), FV(NVFc), FY(NVFc)
    INTEGER ::  i, j, k, L, M, N, ij
    REAL    :: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6, &
         CNST7, CNST8, CNST9

    !     Notice an extra side length L is multiplied to mid-flux to give correct
    !     proportion of flux into the cells.  This length will be removed by the
    !     cell length when the tracer concentration is updated.

#ifdef W3_OMPG
    !$OMP Parallel Default(Shared), Private(j, K, L, M, N),  &
    !$OMP& Private(CNST,CNST4,CNST5,CNST6,CNST7,CNST8,CNST9)
    !$OMP DO
#endif

    DO j=NVA, NVB

      !    Select Upstream, Central and Downstream cells
      K=IJKVFc(4,j)
      L=IJKVFc(5,j)
      M=IJKVFc(6,j)
      N=IJKVFc(7,j)

      !    Central face gradient.
      CNST5=(CF(M)-CF(L))

      !    Courant number in basic cell unit as dy is constant
      CNST6=0.5*( VC(L)+VC(M) )
      VFLY(j) = CNST6

      !    Face size integer and cosine factor
      !    CLATF is defined on V-face for SMC grid.  JGLi28Feb2012
      CNST7=CLatF(j)*FLOAT( IJKVFc(3,j) )

      !    For positive velocity case
      IF(CNST6 >= 0.0)  THEN

        !    Use central cell speed for flux face speed.  JGLi06Apr2011
        IF( M .LE. 0 ) VFLY(j) = VC(L)

        !    Upstream face gradient, depending on VFLY sign.
        CNST4=(CF(L)-CF(K))

        !    Second gradient for 3rd scheme
        CNST8=(CF(M)-CF(K))
        CNST9=(CNST5-CNST4)

        IF( Abs(CNST9) <= 0.6*Abs(CNST8) )  THEN
          !    Use 3rd order scheme in limited zone, note division by 2 grid sizes
          CNST=0.5*CNST5-(1.0+VFLY(j))*CNST9/6.0
        ELSE IF( DBLE(CNST4)*DBLE(CNST5) .GT. 0.d0 ) THEN
          !    Use doubled minimum gradient in rest of monotonic region
          CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )
        ELSE
          !    Use minimum gradient, including 0.5 factor and central sign.
          CNST=Sign(0.5, CNST5)*min( Abs(CNST4), Abs(CNST5) )
        ENDIF

        !    Mid-flux value multiplied by face width and cosine factor
        FV(j)=( CF(L) + CNST*(1.0-VFLY(j)) )*CNST7

        !    For negative velocity case
      ELSE

        !    Use central cell speed for flux face speed.  JGLi06Apr2011
        IF( L .LE. 0 ) VFLY(j) = VC(M)

        !    Side gradients for upstream face, depending on VFLY sign.
        CNST4=(CF(N)-CF(M))

        !    Second gradient for 3rd scheme
        CNST8=(CF(N)-CF(L))
        CNST9=(CNST4-CNST5)

        IF( Abs(CNST9) <= 0.6*Abs(CNST8) )  THEN
          !    Use 3rd order scheme in limited zone, note division by 2 grid sizes
          CNST=0.5*CNST5+(1.0-VFLY(j))*CNST9/6.0
        ELSE IF( DBLE(CNST4)*DBLE(CNST5) .GT. 0.d0 ) THEN
          !    Use doubled minimum gradient in rest of monotonic region
          CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )
        ELSE
          !    Use minimum gradient, including 0.5 factor and central sign.
          CNST=Sign(0.5, CNST5)*min( Abs(CNST4), Abs(CNST5) )
        ENDIF
        !    Mid-flux value multiplied by face width and cosine factor
        FV(j)=( CF(M) - CNST*(1.0+VFLY(j)) )*CNST7

      ENDIF

      !    Diffusion flux by face gradient x DT x face_width x cos(lat)
      FY(j)=AKDif*CNST5*CNST7

    END DO

#ifdef W3_OMPG
    !$OMP END DO
    !$OMP END Parallel
#endif

    RETURN
  END SUBROUTINE SMCyUNO3r

  !
  !> @brief Evaluate local gradient for sea points.
  !>
  !> @details
  !>  Calculate cell centre gradient for any input variable.
  !>  Nemerical average is applied to size-changing faces and the gradients
  !>  are along the lat-lon local east-north directions.
  !>
  !>
  !> @param[in]   CVQ      Input cell values.
  !> @param[out]  GrdX     Gradient along x-axis.
  !> @param[out]  GrdY     Gradient along y-axis.
  !> @param[in]   L0r1     Zero or 1st-order boundary condiiton.
  !>
  !> @author Jian-Guo Li
  !> @date 08 Aug 2017
  !>
  ! Add optional zero-gradient bounday conditions.    JGLi08Aug2017
  !
  SUBROUTINE SMCGradn(CVQ, GrdX, GrdY, L0r1)

    USE CONSTANTS
    USE W3GDATMD, ONLY: NSEA,   NUFc,   NVFc,   MRFct,       &
         IJKCel, IJKUFc, IJKVFC, CLATS, SX, SY
    USE W3GDATMD, ONLY: ARCTC
    USE W3ODATMD, ONLY: NDSE, NDST

    IMPLICIT NONE
    !!    New boundary conditions depending on user defined L0r1.
    !!    L0r1 = 0 will set zero at land points while L0r1 > 0 invokes
    !!    the zero-gradient boundary condition.    JGLi08Aug2017
    REAL,    INTENT( IN)::  CVQ(NSEA)
    REAL,    INTENT(Out):: GrdX(NSEA), GrdY(NSEA)
    INTEGER, INTENT( IN):: L0r1
    !
    INTEGER :: I, J, K, L, M, N
    REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6
    REAL :: DX0I, DY0I

    !     Use a few working arrays
    REAL,  Dimension(-9:NSEA):: CVF, AUN, AVN

    !     Two layer of boundary cells are added to each boundary cell face
    !     with all boundary cell default values CVF(-9:0)= 0.0.
    CVF(-9:0)  = 0.0
    CVF(1:NSEA)=CVQ(1:NSEA)

    !!    Initialize arrays
    AUN = 0.
    AVN = 0.
    GrdX = 0.
    GrdY = 0.

    !!    Multi-resolution base-cell size defined by refined levels.
    !!    So the MRFct converts the base cell SX, SY into size-1 cell lenth.
    !!    Constant size-1 dy=DY0 and dx on Equator DX0, inverted.
    DX0I   = MRFct/ ( SX * DERA * RADIUS )
    DY0I   = MRFct/ ( SY * DERA * RADIUS )

#ifdef W3_OMPG
    !$OMP Parallel Default(Shared), Private(i, j, K, L, M, N),  &
    !$OMP& Private(CNST,CNST0,CNST1,CNST2,CNST3,CNST4,CNST5,CNST6)
    !$OMP DO
#endif

    !!    Calculate x-gradient by averaging U-face gradients.
    DO i=1, NUFc

      !     Select Upstream, Central and Downstream cells
      L=IJKUFc(5,i)
      M=IJKUFc(6,i)

      !!      For zero-gradient boundary conditions, simply skip boundary faces.
      IF( L0r1 .EQ. 0 .OR. (L > 0 .AND. M > 0) ) THEN

        !         Multi-resolution SMC grid requires flux multiplied by face factor.
        CNST1=FLOAT( IJKUFc(3,i) )

        !         Face bounding cell lengths and central gradient
        CNST2=FLOAT( IJKCel(3,L) )
        CNST3=FLOAT( IJKCel(3,M) )

        !         Side gradients over 2 cell lengths for central cell.
        !         Face size factor is also included for average.
        CNST5=CNST1*(CVF(M)-CVF(L))/(CNST2+CNST3)
#if defined W3_B4B && defined W3_OMPG
        CNST5=INT(CNST5 * 1.0e6)   ! CB: B4B
#endif

        !! Replace CRITICAL with ATOMIC.  JGLi15Jan2019
        !! !$OMP CRITICAL
        !    Store side gradient in two neighbouring cells
        !! Remove boundary cell flux update or L M > 0.  JGLi28Mar2019
        IF( L > 0 ) THEN
#ifdef W3_OMPG
          !$OMP ATOMIC
#endif
          AUN(L) = AUN(L) + CNST5
        ENDIF
        IF( M > 0 ) THEN
#ifdef W3_OMPG
          !$OMP ATOMIC
#endif
          AUN(M) = AUN(M) + CNST5
        ENDIF
        !! !$OMP END CRITICAL

      ENDIF
    END DO

#ifdef W3_OMPG
    !$OMP END DO
#endif

#if defined W3_B4B && defined W3_OMPG
    !$OMP SINGLE
    AUN = AUN / 1.0e6  ! CB B4B
    !$OMP END SINGLE
#endif

    !  Assign averaged side-gradient to GrdX, plus latitude factor
    !  Note averaging over 2 times of cell y-width factor but AUN
    !  has already been divied by two cell lengths.

#ifdef W3_OMPG
    !$OMP DO
#endif

    DO n=1, NSEA
      !       Cell y-size IJKCel(4,i) is used to cancel the face size-factor in AUN.
      !       Plus the actual physical length scale for size-1 cell.
      !       Note polar cell (if any) AUN = 0.0 as it has no U-face.
      GrdX(n)=DX0I*AUN(n)/( CLats(n)*IJKCel(4,n) )

    ENDDO

#ifdef W3_OMPG
    !$OMP END DO
    !$OMP DO
#endif

    !!    Calculate y-gradient by averaging V-face gradients.
    DO j=1, NVFc

      !       Select Central and Downstream cells
      L=IJKVFc(5,j)
      M=IJKVFc(6,j)

      !!      For zero-gradient boundary conditions, simply skip boundary faces.
      IF( L0r1 .EQ. 0 .OR. (L > 0 .AND. M > 0) ) THEN

        !       Face size is required for multi-resolution grid.
        CNST1=Real( IJKVFc(3,j) )

        !         Cell y-length of UCD cells
        CNST2=Real( IJKCel(4,L) )
        CNST3=Real( IJKCel(4,M) )

        !         Side gradients over 2 cell lengths for central cell.
        !         Face size factor is also included for average.
        CNST6=CNST1*(CVF(M)-CVF(L))/(CNST2+CNST3)
#if defined W3_B4B && defined W3_OMPG
        CNST6 = int(CNST6 * 1.0e6) ! CB B4B
#endif

        !! Replace CRITICAL with ATOMIC.  JGLi15Jan2019
        !! !$OMP CRITICAL
        !! Remove boundary cell flux update or L M > 0.  JGLi28Mar2019
        IF( L > 0 ) THEN
          !    Store side gradient in two neighbouring cells
#ifdef W3_OMPG
          !$OMP ATOMIC
#endif
          AVN(L) = AVN(L) + CNST6
        ENDIF
        IF( M > 0 ) THEN
#ifdef W3_OMPG
          !$OMP ATOMIC
#endif
          AVN(M) = AVN(M) + CNST6
        ENDIF
        !! !$OMP END CRITICAL

      ENDIF
    END DO

#ifdef W3_OMPG
    !$OMP END DO
#endif

#if defined W3_B4B && defined W3_OMPG
    !$OMP SINGLE
    AVN = AVN / 1.0e6  !CB B4B
    !$OMP END SINGLE
#endif

#ifdef W3_OMPG
    !$OMP DO
#endif

    !  Assign averaged side-gradient to GrdY.
    DO n=1, NSEA
      !  AV is divided by the cell x-size IJKCel(3,i) to cancel face
      !  size-factor, and physical y-distance of size-1 cell.
      GrdY(n)=DY0I*AVN(n)/Real( IJKCel(3,n) )

    END DO

#ifdef W3_OMPG
    !$OMP END DO
    !$OMP END Parallel
#endif

    !!Li  Y-gradient for polar cell in Arctic part is set to zero.
    IF( ARCTC ) GrdY(NSEA) = 0.0

    RETURN
  END SUBROUTINE SMCGradn


  !> @brief Average sea point values with a 1-2-1 scheme.
  !>
  !> @param[inout]  CVQ  Input field.
  !>
  !> @author Jian-Guo Li
  !> @date 15-Jan-2019
  !>
  SUBROUTINE SMCAverg(CVQ)

    USE CONSTANTS
    USE W3GDATMD, ONLY: NSEA,   NUFc,   NVFc,    &
         IJKCel, IJKUFc, IJKVFC,  &
         IJKUFc5, IJKUFc6
    USE W3GDATMD, ONLY: ARCTC
    USE W3ODATMD, ONLY: NDSE, NDST

    IMPLICIT NONE
    REAL, INTENT(INOUT) :: CVQ(-9:NSEA)
    !
    INTEGER :: I, J, K, L, M, N
    REAL :: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6

    !     Use a few working arrays
    REAL, Dimension(-9:NSEA) :: CVF, AUN, AVN

    !     Two layer of boundary cells are added to each boundary cell face
    !     with all boundary cell values stored in CVF(-9:0).
    CVF=CVQ

    !!    Initialize arrays
    AUN = 0.
    AVN = 0.

    !!Li  Save polar cell value if any.
    CNST0 = CVQ(NSEA)

#ifdef W3_OMPG
    !$OMP Parallel Default(Shared), Private(i, j, L, M, n),  &
    !$OMP& Private(CNST3,CNST4,CNST5,CNST6)
    !$OMP DO
#endif

    !!    Calculate x-gradient by averaging U-face gradients.
    DO i=1, NUFc

      !    Select Upstream, Central and Downstream cells
      L=IJKUFc5(i)
      M=IJKUFc6(i)

      !       Multi-resolution SMC grid requires flux multiplied by face factor.
      CNST5=Real( IJKUFc(3,i) )*(CVF(M)+CVF(L))
#ifdef W3_B4B
      !OMPG           CNST5=int(CNST5 * 1.0e6)
#endif

      !! Replace CRITICAL with ATOMIC.  JGLi15Jan2019
      !! !$OMP CRITICAL
      !    Store side gradient in two neighbouring cells
      !! Remove boundary cell flux update or L M > 0.  JGLi28Mar2019
      IF( L > 0 ) THEN
#ifdef W3_OMPG
        !$OMP ATOMIC
#endif
        AUN(L) = AUN(L) + CNST5
      ENDIF
      IF( M > 0 ) THEN
#ifdef W3_OMPG
        !$OMP ATOMIC
#endif
        AUN(M) = AUN(M) + CNST5
      ENDIF
      !! !$OMP END CRITICAL

    END DO

#ifdef W3_OMPG
    !$OMP END DO
#endif

#if defined W3_B4B && defined W3_OMPG
    !$OMP SINGLE
    AUN = AUN / 1.0e6  !CB B4B
    !$OMP END SINGLE
#endif

#ifdef W3_OMPG
    !$OMP DO
#endif

    !!    Calculate y-gradient by averaging V-face gradients.
    DO j=1, NVFc

      !       Select Central and Downstream cells
      L=IJKVFc(5,j)
      M=IJKVFc(6,j)

      !       Face size is required for multi-resolution grid.
      CNST6=Real( IJKVfc(3,j) )*(CVF(M)+CVF(L))
#if defined W3_B4B && defined W3_OMPG
      CNST6=INT(CNST6 * 1e6)
#endif

      !! Replace CRITICAL with ATOMIC.  JGLi15Jan2019
      !! !$OMP CRITICAL
      !    Store side gradient in two neighbouring cells
      !! Remove boundary cell flux update or L M > 0.  JGLi28Mar2019
      IF( L > 0 ) THEN
#ifdef W3_OMPG
        !$OMP ATOMIC
#endif
        AVN(L) = AVN(L) + CNST6
      ENDIF
      IF( M > 0 ) THEN
#ifdef W3_OMPG
        !$OMP ATOMIC
#endif
        AVN(M) = AVN(M) + CNST6
      ENDIF
      !! !$OMP END CRITICAL

    END DO

#ifdef W3_OMPG
    !$OMP END DO
#endif

#if defined W3_B4B && defined W3_OMPG
    !$OMP SINGLE
    AVN = AVN / 1.0e6  !CB B4B
    !$OMP END SINGLE
#endif

#ifdef W3_OMPG
    !$OMP DO
#endif

    !  Assign averaged value back to CVQ.
    DO n=1, NSEA

      CNST3=0.125/Real( IJKCel(3,n) )
      CNST4=0.125/Real( IJKCel(4,n) )
      !       AUN is divided by the cell y-size IJKCel(4,n) and AVN by
      !       the cell x-size IJKCel(3,n) to cancel face size factors.
      CVQ(n)= AUN(n)*CNST4 + AVN(n)*CNST3

    END DO

#ifdef W3_OMPG
    !$OMP END DO
    !$OMP END Parallel
#endif

    !!Li  Polar cell (if any) keep original value.
    IF( ARCTC ) CVQ(NSEA) = CNST0

    ! 999  PRINT*, ' Sub SMCAverg ended.'

    RETURN
  END SUBROUTINE SMCAverg


  !> @brief Calculate great circle turning (GCT) and refraction.
  !>
  !> @details
  !>  The refraction and GCT terms are equivalent to a single rotation by each
  !>  element and does not need to be calculated as advection.  A simple rotation
  !>  scheme similar to the 1st order upstream scheme but without any restriction
  !>  on the rotation angle or the CFL limit by an Eulerian advection scheme.
  !>
  !> @param[in]  CoRfr   Courant number for refraction and GCT rotation.
  !> @param[in]  SpeTHK  Wave spectrum to be rotated and output.
  !>
  !> @author Jian-Guo Li
  !> @date 12 Nov 2010
  !>
  SUBROUTINE SMCGtCrfr(CoRfr, SpeTHK)
    USE CONSTANTS
    USE W3GDATMD, ONLY: NK, NTH, DTH, CTMAX

    IMPLICIT NONE
    REAL, INTENT(IN)   ::  CoRfr(NTH, NK)
    REAL, INTENT(INOUT):: SpeTHK(NTH, NK)
    INTEGER ::  I, J, K, L, M, N
    REAL, Dimension(NTH):: SpeGCT, Spectr
    REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6

    !     Loop through NK spectral bins.
    DO n=1, NK

      !!      Asign cell spectrum to temporary variable Spcetr
      Spectr=SpeTHK(1:NTH,n)
      SpeGCT=0.0

      !!      Loop through NTH directional bins for each cell spectrum
      DO j=1, NTH

        !         GCT + refraction Courant number for this dirctional bin
        CNST6=CoRfr(j,n)

        !         Work out integer number of bins to be skipped.
        !         If K is great than NTH, full circle turning is removed.
        CNST5=ABS( CNST6 )
        K= MOD( INT(CNST5), NTH )

        !         Partitioning faraction of the spectral component
        CNST1=CNST5 - FLOAT( INT(CNST5) )
        CNST2=1.0 - CNST1

        !         For positive turning case
        IF(CNST6 > 0.0)  THEN

          !           Select the upstream and downstream bins to rotate in, wrap at end
          L=j+K
          M=j+K+1
          IF( L .GT. NTH ) L = L - NTH
          IF( M .GT. NTH ) M = M - NTH

          !!          Divide the j bin energy by fraction of CNST6 and store in SpeGCT
          SpeGCT(L)=SpeGCT(L)+Spectr(j)*CNST2
          SpeGCT(M)=SpeGCT(M)+Spectr(j)*CNST1

          !         For negative or no turning case
        ELSE

          !           Select the upstream and downstream bins to rotate in, wrap at end
          L=j-K
          M=j-K-1
          IF( L .LT. 1 ) L = L + NTH
          IF( M .LT. 1 ) M = M + NTH

          !!          Divide the bin energy by fraction of CNST6 and store in SpeGCT
          SpeGCT(L)=SpeGCT(L)+Spectr(j)*CNST2
          SpeGCT(M)=SpeGCT(M)+Spectr(j)*CNST1

        ENDIF

        !!      End of directional loop j
      END DO

      !!      Store GCT spectrum
      SpeTHK(1:NTH,n) = SpeGCT

      !!    End of cell loop n
    END DO

    ! 999  PRINT*, ' Sub SMCGtCrfr ended.'

    RETURN
  END SUBROUTINE SMCGtCrfr


  !>
  !> @brief Calculates refraction induced shift in k-space.
  !>
  !> @details
  !>  The term is equivalent to advection on an irregular k-space grid.
  !>  The UNO2 scheme on irregular grid is used for this term.
  !>
  !>  Cell and side indices for k-dimension are arranged as:
  !>  @verbatim
  !>    Cell:    | -1 | 0 | 1 | 2 | ... | NK | NK+1 | NK+2 |
  !>    Side:        -1   0   1   2 ...     NK     NK+1
  !>  @endverbatim
  !>  The wave action in k-space is extended at the high-wavenumber
  !>  (frequency) end by the (m+2)th negative power of frequency for
  !>  boundary conditions.  Outside low-wavenumber (frequncy) end, wave
  !>  action is assumed to be zero.
  !>
  !> @param[in]    CoRfr      Courant number for refraction k-shift.
  !> @param[inout] SpeTHK     Spectrum to be shifted and output.
  !> @param[in]    DKC        Wave number increment at k-bin centre.
  !> @param[in]    DKS        Wave number increment at k-bin edges.
  !>
  !>
  !> @author Jian-Guo Li
  !> @date 15 Nov 2010
  !
  ! Fix bug on CFL limiter and add positive filter.  JGLi28Jun2017
  !
  SUBROUTINE SMCkUNO2(CoRfr, SpeTHK, DKC, DKS)

    USE CONSTANTS
    USE W3GDATMD, ONLY: NK, NK2, NTH, DTH, XFR, CTMAX

    IMPLICIT NONE
    REAL, INTENT(IN)   ::  CoRfr(NTH, 0:NK), DKC(0:NK+1), DKS(-1:NK+1)
    REAL, INTENT(INOUT):: SpeTHK(NTH, NK)
    INTEGER ::  I, J, K, L, M, N
    REAL, Dimension(-1:NK+2):: SpeRfr, Spectr, SpeFlx
    REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6

    CNST=XFR**(-7)

    DO n=1, NTH

      !!      Asign cell spectrum to temporary variable Spcetr
      Spectr(-1)  =0.0
      Spectr( 0)  =0.0
      Spectr(1:NK)=SpeTHK(n,1:NK)
      Spectr(NK+1)=Spectr(NK  )*CNST
      Spectr(NK+2)=Spectr(NK+1)*CNST

      !!      Calculate k-space gradient for NK+2 faces by UNO2 scheme
      SpeRfr(-1)= 0.0
      DO j=-1, NK+1
        SpeRfr(j)=(Spectr(j+1)-Spectr(j))/DKS(j)
      ENDDO

      !!      Calculate k-space fluxes for NK+1 faces by UNO2 scheme
      DO j=0, NK

        !         Final refraction Courant number for this k-space face
        CNST6=CoRfr(n,j)
        !!        Note CoRfr is CFL for k but without dividing dk.

        !         For positive case
        IF(CNST6 > 0.0)  THEN

          CNST0 = MIN( CTMAX*DKC(j), CNST6 )
          SpeFlx(j) = CNST0*( Spectr(j) + SIGN(0.5, SpeRfr(j))*(DKC(j)-CNST0)  &
               *MIN( ABS(SpeRfr(j-1)), ABS(SpeRfr(j)) ) )

          !         For negative or no turning case
        ELSE

          CNST0 = MIN( CTMAX*DKC(j+1), -CNST6 )
          SpeFlx(j) = -CNST0*( Spectr(j+1) - SIGN(0.5, SpeRfr(j))*(DKC(j+1)-CNST0) &
               *MIN( ABS(SpeRfr(j+1)), ABS(SpeRfr(j)) ) )

        ENDIF

        !!      End of flux loop j
      END DO

      !!      Update spectrum for the given direction
      DO j=1, NK
        !         Final refraction Courant number for this k-space face
        !         SpeTHK(n, j) = Spectr(j) + (SpeFlx(j-1) - SpeFlx(j))/DKC(j)
        !         Add positive filter in case negative values.  JGLi27Jun2017
        SpeTHK(n, j) = MAX( 0.0, Spectr(j)+(SpeFlx(j-1)-SpeFlx(j))/DKC(j) )
      END DO

      !!    End of directional loop n
    END DO

    ! 999   PRINT*, ' Sub SMCkUNO2 ended.'

    RETURN
  END SUBROUTINE SMCkUNO2


  !> @brief Calculates water-depth gradient for refraction.
  !>
  !> @details
  !>  For consistency with the lat-lon grid, full grid DDDX, DDDY are
  !>  also assigned here.  DHDX, DHDY are used for refraction at present.
  !>  It has to be rotated to map-east system in the Arctic part.
  !>
  !> @author Jian-Guo Li
  !> @date 03-Mar-2022
  !>
  SUBROUTINE SMCDHXY
    USE CONSTANTS
    USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSTA, MAPFS, MRFct, IJKCel,  &
         CLATS, NTH, DTH, ESIN, ECOS, Refran, DMIN
    USE W3GDATMD, ONLY: NGLO, ANGARC, ARCTC
    USE W3ADATMD, ONLY: DW, DDDX, DDDY, DHDX, DHDY, DHLMT
    USE W3ODATMD, ONLY: NDSE, NDST

    IMPLICIT NONE

    INTEGER :: I, J, K, L, M, N
    REAL :: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6
    REAL, Dimension(NSEA) :: HCel, GrHx, GrHy
    !     REAL, Dimension(-9:NSEA) :: HCel

    !!    Assign water depth to HCel from DW integer values.
    !!    set half the minimum depth DMIN for negative cells.
    !     HCel(-9:0) = 0.5*DMIN
    HCel(1:NSEA)= DW(1:NSEA)

    !!    Reset shallow water depth with minimum depth
#ifdef W3_OMPG
    !$OMP Parallel DO Private(k)
#endif
    DO k=1, NSEA
      IF(DW(k) .LT. DMIN)  HCel(k)=DMIN
    ENDDO
#ifdef W3_OMPG
    !$OMP END Parallel DO
#endif

    !!    Initialize full grid gradient arrays
    DDDX = 0.
    DDDY = 0.

    !!    Use zero-gradient boundary condition or L0r1 > 0
    L = 1

    !!    Calculate sea point water depth gradient
    CALL SMCGradn(HCel, GrHx, GrHy, L)

    !!    Pass gradient values to DHDX, DHDY
    DHDX(1:NSEA) = GrHx
    DHDY(1:NSEA) = GrHy

    !!   Apply limiter to depth-gradient and copy to full grid.
#ifdef W3_OMPG
    !$OMP Parallel DO Private(i,j,k,m,n, CNST0, CNST1, CNST2)
#endif
    DO n=1,NSEA

      !  A limiter of gradient <= 0.1 is applied.
      IF( ABS( DHDX(n) ) .GT.  0.1) DHDX(n)=SIGN( 0.1, DHDX(n) )
      IF( ABS( DHDY(n) ) .GT.  0.1) DHDY(n)=SIGN( 0.1, DHDY(n) )

      !! Asign DHDX value to full grid variable DDDX
      i= IJKCel(1,n)/MRFct + 1
      j= IJKCel(2,n)/MRFct + 1
      k= MAX(1, IJKCel(3,n)/MRFct)
      m= MAX(1, IJKCel(4,n)/MRFct)
      DDDX(j:j+m-1,i:i+k-1)  = DHDX(n)
      DDDY(j:j+m-1,i:i+k-1)  = DHDY(n)

      !Li  Depth gradient in the Arctic part has to be rotated into
      !Li  the map-east system for calculation of refraction.
      IF( ARCTC .AND. n .GT. NGLO ) THEN
        CNST0 = ANGARC(n - NGLO)*DERA
        CNST1 = DHDX(n)*COS(CNST0) - DHDY(n)*SIN(CNST0)
        CNST2 = DHDX(n)*SIN(CNST0) + DHDY(n)*COS(CNST0)
        DHDX(n) = CNST1
        DHDY(n) = CNST2
      ENDIF

    END DO
#ifdef W3_OMPG
    !$OMP END Parallel DO
#endif

    !! Calculate the depth gradient limiter for refraction.
#ifdef W3_T
    L = 0 !CB - added T switch
#endif

#ifdef W3_OMPG
    !$OMP Parallel DO Private(i, n, CNST4, CNST6)
#endif
    DO n=1,NSEA

      !Li   Work out magnitude of depth gradient
      CNST4 = 1.0001*SQRT(DHDX(n)*DHDX(n) + DHDY(n)*DHDY(n))
      !
      !Li   Directional depedent depth gradient limiter.  JGLi16Jun2011
      IF ( CNST4 .GT. 1.0E-5 ) THEN

#if defined W3_T && defined W3_OMPG
        !$OMP ATOMIC Update   !CB - added T switch
#endif
        L = L + 1       !CB - added T switch
#if defined W3_T && defined W3_OMPG
        !$OMP END ATOMIC      !CB - added T switch
#endif

        DO i=1, NTH
          !Li       Refraction is done only when depth gradient is non-zero.
          !Li       Note ACOS returns value between [0, Pi), always positive.
          CNST6 = ACOS(-(DHDX(n)*ECOS(i)+DHDY(n)*ESIN(i))/CNST4 )
          !Li   User-defined refraction limiter added.   JGLi09Jan2012
          DHLMT(i,n)=MIN(Refran, 0.75*MIN(CNST6,ABS(PI-CNST6)))/DTH
        END DO
        !Li   Output some values for inspection.  JGLi22Jul2011
#ifdef W3_T
        IF( MOD(n, 1000) .EQ. 0 )   &
             WRITE(NDST,'(i8,18F5.1)' ) n, (DHLMT(i,n), i=1,18)
#endif

      ELSE
        DHLMT(:,n) = 0.0
      ENDIF

    ENDDO
#ifdef W3_OMPG
    !$OMP END Parallel DO
#endif

#ifdef W3_T
    WRITE(NDST,*) ' No. Refraction points =', L
#endif

#ifdef W3_T
999 PRINT*, ' Sub SMCDHXY ended.'
#endif

    RETURN
  END SUBROUTINE SMCDHXY


  !> @brief Calculates current velocity gradient for refraction.
  !>
  !> @details
  !>  For consistency with the lat-lon grid, full grid DCXDXY, DCYDXY are
  !>  assigned here.  They are rotated to map-east system in the Arctic part.
  !>
  !> @author Jian-Guo Li
  !> @date 23 Mar 2016
  !>
  SUBROUTINE SMCDCXY
    USE CONSTANTS
    USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSTA, MAPFS, MRFct, IJKCel
    USE W3GDATMD, ONLY: NGLO, ANGARC, ARCTC
    USE W3ADATMD, ONLY: CX, CY, DCXDX, DCXDY, DCYDX, DCYDY
    USE W3ODATMD, ONLY: NDSE, NDST

    IMPLICIT NONE

    INTEGER :: I, J, K, L, M, N
    REAL :: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6
    REAL, Dimension(NSEA) :: CXCY, GrHx, GrHy
    !     REAL, Dimension(-9:NSEA) :: CXCY

    !!    Assign current CX speed to CXCY and set negative cells.
    !     CXCY(-9:0) = 0.0
    !!    Use zero-gradient boundary condition or L0r1 > 0
    L = 1
    CXCY(1:NSEA)= CX(1:NSEA)

    !!   Initialize full grid gradient arrays
    DCXDX = 0.0
    DCXDY = 0.0

    !!    Calculate sea point water depth gradient
    CALL SMCGradn(CXCY, GrHx, GrHy, L)

    !!   Apply limiter to CX-gradient and copy to full grid.
#ifdef W3_OMPG
    !$OMP Parallel DO Private(i, j, k, m, n, CNST0, CNST1, CNST2)
#endif
    DO n=1,NSEA

      !       A limiter of gradient <= 0.01 is applied.
      IF( ABS( GrHx(n) ) .GT.  0.01) GrHx(n)=SIGN( 0.01, GrHx(n) )
      IF( ABS( GrHy(n) ) .GT.  0.01) GrHy(n)=SIGN( 0.01, GrHy(n) )

      !Li     Current gradient in the Arctic part has to be rotated into
      !Li     the map-east system for calculation of refraction.
      IF( ARCTC .AND. n .GT. NGLO ) THEN
        CNST0 = ANGARC(n - NGLO)*DERA
        CNST1 = GrHx(n)*COS(CNST0) - GrHy(n)*SIN(CNST0)
        CNST2 = GrHx(n)*SIN(CNST0) + GrHy(n)*COS(CNST0)
        GrHx(n) = CNST1
        GrHy(n) = CNST2
      ENDIF

      !! Asign CX gradients to full grid variable DCXDX/Y
      i= IJKCel(1,n)/MRFct + 1
      j= IJKCel(2,n)/MRFct + 1
      k= MAX(1, IJKCel(3,n)/MRFct)
      m= MAX(1, IJKCel(4,n)/MRFct)
      DCXDX(j:j+m-1,i:i+k-1)  = GrHx(n)
      DCXDY(j:j+m-1,i:i+k-1)  = GrHy(n)

    END DO
#ifdef W3_OMPG
    !$OMP END Parallel DO
#endif

    !!    Assign current CY speed to CXCY and set negative cells.
    !     CXCY(-9:0) = 0.0
    !!    Use zero-gradient boundary condition or L0r1 > 0
    L = 1
    CXCY(1:NSEA)= CY(1:NSEA)

    !!    Initialize full grid gradient arrays
    DCYDX = 0.0
    DCYDY = 0.0

    !!    Calculate sea point water depth gradient
    CALL SMCGradn(CXCY, GrHx, GrHy, L)

    !!    Apply limiter to CX-gradient and copy to full grid.
#ifdef W3_OMPG
    !$OMP Parallel DO Private(i, j, k, m, n, CNST0, CNST1, CNST2)
#endif
    DO n=1,NSEA

      !!      A limiter of gradient <= 0.1 is applied.
      IF( ABS( GrHx(n) ) .GT.  0.01) GrHx(n)=SIGN( 0.01, GrHx(n) )
      IF( ABS( GrHy(n) ) .GT.  0.01) GrHy(n)=SIGN( 0.01, GrHy(n) )

      !!      Current gradient in the Arctic part has to be rotated into
      !!      the map-east system for calculation of refraction.
      IF( ARCTC .AND. n .GT. NGLO ) THEN
        CNST0 = ANGARC(n - NGLO)*DERA
        CNST1 = GrHx(n)*COS(CNST0) - GrHy(n)*SIN(CNST0)
        CNST2 = GrHx(n)*SIN(CNST0) + GrHy(n)*COS(CNST0)
        GrHx(n) = CNST1
        GrHy(n) = CNST2
      ENDIF

      !!      Asign CX gradients to full grid variable DCXDX/Y
      i= IJKCel(1,n)/MRFct + 1
      j= IJKCel(2,n)/MRFct + 1
      k= MAX(1, IJKCel(3,n)/MRFct)
      m= MAX(1, IJKCel(4,n)/MRFct)
      DCYDX(j:j+m-1,i:i+k-1)  = GrHx(n)
      DCYDY(j:j+m-1,i:i+k-1)  = GrHy(n)

    END DO
#ifdef W3_OMPG
    !$OMP END Parallel DO
#endif

#ifdef W3_T
999 PRINT*, ' Sub SMCDCXY ended.'
#endif

    RETURN
  END SUBROUTINE SMCDCXY

  !/
  !/ ------------------------------------------------------------------- /
  !> @brief SMC version of W3GATH
  !>
  !> @details
  !>  Gather spectral bin information into a propagation field array.
  !>  Direct copy or communication calls (MPP version).
  !>
  !> @remarks
  !>  - The field is extracted but not converted.
  !>  - Array FIELD is not initialized.
  !>  - MPI version requires posing of send and receive calls in
  !>    W3WAVE to match local calls.
  !>  - MPI version does not require an MPI_TESTALL call for the
  !>    posted gather operation as MPI_WAITALL is mandatory to
  !>    reset persistent communication for next time step.
  !>  - MPI version allows only two new pre-fetch postings per
  !>    call to minimize chances to be slowed down by gathers that
  !>    are not yet needed, while maximizing the pre-loading
  !>    during the early (low-frequency) calls to the routine
  !>    where the amount of calculation needed for proagation is
  !>    the largest.
  !>
  !> @param[in]   ISPEC   Spectral bin considered
  !> @param[out]  FIELD   Full field to be propagated
  !>
  !> @author Jian-Guo Li
  !> @date 15 Mar 2011
  !/ ------------------------------------------------------------------- /
  !/
  SUBROUTINE W3GATHSMC ( ISPEC, FIELD )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH-III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         13-Jun-2006 |
    !/                  +-----------------------------------+
    !/
    !/    04-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
    !/    13-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/                  Major changes to logistics.
    !/    29-Dec-2004 : Multiple grid version.              ( version 3.06 )
    !/    13-Jun-2006 : Split STORE in G/SSTORE             ( version 3.09 )
    !/     9-Dec-2010 : Adapted for SMC grid propagtion. JGLi
    !/
    !  1. Purpose :
    !
    !     Gather spectral bin information into a propagation field array.
    !
    !  2. Method :
    !
    !     Direct copy or communication calls (MPP version).
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       ISPEC   Int.   I   Spectral bin considered.
    !       FIELD   R.A.   O   Full field to be propagated.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !
    !      MPI_STARTALL, MPI_WAITALL
    !                Subr. mpif.h   MPI persistent comm. routines (!/MPI).
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
    !     - The field is extracted but not converted.
    !     - Array FIELD is not initialized.
    !     - MPI version requires posing of send and receive calls in
    !       W3WAVE to match local calls.
    !     - MPI version does not require an MPI_TESTALL call for the
    !       posted gather operation as MPI_WAITALL is mandatory to
    !       reset persistent communication for next time step.
    !     - MPI version allows only two new pre-fetch postings per
    !       call to minimize chances to be slowed down by gathers that
    !       are not yet needed, while maximizing the pre-loading
    !       during the early (low-frequency) calls to the routine
    !       where the amount of calculation needed for proagation is
    !       the largest.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/SHRD  Switch for message passing method.
    !     !/MPI   Id.
    !
    !     !/S     Enable subroutine tracing.
    !     !/MPIT  MPI test output.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    USE W3GDATMD, ONLY: NSPEC, NX, NY, NSEA, NSEAL, NCel, MAPSF
    USE W3WDATMD, ONLY: A => VA
#ifdef W3_MPI
    USE W3ADATMD, ONLY: MPIBUF, BSTAT, IBFLOC, ISPLOC, BISPL, &
         NSPLOC, NRQSG2, IRQSG2, GSTORE
    USE W3ODATMD, ONLY: NDST, IAPROC, NAPROC
#endif
    !/
    IMPLICIT NONE
    !
#ifdef W3_MPI
    INCLUDE "mpif.h"
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: ISPEC
    REAL,    INTENT(OUT)    :: FIELD(NCel)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_SHRD
    INTEGER                 :: ISEA, IXY
#endif
#ifdef W3_MPI
    INTEGER                 :: STATUS(MPI_STATUS_SIZE,NSPEC),  &
         IOFF, IERR_MPI, JSEA, ISEA,     &
         IXY, IS0, IB0, NPST, J
#endif
#ifdef W3_S
    INTEGER, SAVE           :: IENT
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3GATH')
#endif
    !
    !      FIELD  = 0.
    !
    ! 1.  Shared memory version ------------------------------------------ /
    !
#ifdef W3_SHRD
    DO ISEA=1, NSEA
      FIELD(ISEA) = A(ISPEC,ISEA)
    END DO
    !
    RETURN
#endif
    !
    ! 2.  Distributed memory version ( MPI ) ----------------------------- /
    ! 2.a Update counters
    !
#ifdef W3_MPI
    ISPLOC = ISPLOC + 1
    IBFLOC = IBFLOC + 1
    IF ( IBFLOC .GT. MPIBUF ) IBFLOC = 1
    !
    ! 2.b Check status of present buffer
    ! 2.b.1 Scatter (send) still in progress, wait to end
    !
    IF ( BSTAT(IBFLOC) .EQ. 2 ) THEN
      IOFF =  1 + (BISPL(IBFLOC)-1)*NRQSG2
      IF ( NRQSG2 .GT. 0 ) CALL                              &
           MPI_WAITALL ( NRQSG2, IRQSG2(IOFF,2),             &
           STATUS, IERR_MPI )
      BSTAT(IBFLOC) = 0
    END IF
    !
    ! 2.b.2 Gather (recv) not yet posted, post now
    !
    IF ( BSTAT(IBFLOC) .EQ. 0 ) THEN
      BSTAT(IBFLOC) = 1
      BISPL(IBFLOC) = ISPLOC
      IOFF =  1 + (ISPLOC-1)*NRQSG2
      IF ( NRQSG2 .GT. 0 ) CALL                              &
           MPI_STARTALL ( NRQSG2, IRQSG2(IOFF,1), IERR_MPI )
    END IF
    !
    ! 2.c Put local spectral densities in store
    !
    DO JSEA=1, NSEAL
      GSTORE(IAPROC+(JSEA-1)*NAPROC,IBFLOC) = A(ISPEC,JSEA)
    END DO
    !
    ! 2.d Wait for remote spectral densities
    !
    IOFF =  1 + (BISPL(IBFLOC)-1)*NRQSG2
    IF ( NRQSG2 .GT. 0 ) CALL                                  &
         MPI_WAITALL ( NRQSG2, IRQSG2(IOFF,1), STATUS, IERR_MPI )
    !
    ! 2.e Convert storage array to field.
    !
    DO ISEA=1, NSEA
      FIELD(ISEA) = GSTORE(ISEA,IBFLOC)
    END DO
    !
    ! 2.f Pre-fetch data in available buffers
    !
    IS0    = ISPLOC
    IB0    = IBFLOC
    NPST   = 0
    DO J=1, MPIBUF-1
      IS0    = IS0 + 1
      IF ( IS0 .GT. NSPLOC ) EXIT
      IB0    = 1 + MOD(IB0,MPIBUF)
      IF ( BSTAT(IB0) .EQ. 0 ) THEN
        BSTAT(IB0) = 1
        BISPL(IB0) = IS0
        IOFF       = 1 + (IS0-1)*NRQSG2
        IF ( NRQSG2 .GT. 0 ) CALL                            &
             MPI_STARTALL ( NRQSG2, IRQSG2(IOFF,1), IERR_MPI )
        NPST       = NPST + 1
      END IF
      IF ( NPST .GE. 2 ) EXIT
    END DO
    RETURN
#endif
    !
    !/ End of W3GATHSMC ----------------------------------------------------- /
    !/
  END SUBROUTINE W3GATHSMC
  !
  !/ ------------------------------------------------------------------- /
  !> @brief SMC version of W3GATH
  !>
  !> @details
  !> 'Scatter' data back to spectral storage after propagation.
  !>  Direct copy or communication calls (MPP version).
  !>  See also W3GATH.
  !>
  !> @param[in]  ISPEC   Spectral bin considered
  !> @param[in]  MAPSTA  Status map for spatial grid
  !> @param[in]  FIELD   SMC grid field to be propagated
  !>
  !> @remarks
  !>  - The field is put back but not converted !
  !>  - MPI persistent communication calls initialize in W3MPII.
  !>  - See W3GATH and W3MPII for additional comments on data
  !>    buffering.
  !>
  !> @author Jian-Guo Li
  !> @date 16 Jan 2012
  !>
  SUBROUTINE W3SCATSMC ( ISPEC, MAPSTA, FIELD )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH-III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         13-Jun-2006 |
    !/                  +-----------------------------------+
    !/
    !/    04-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
    !/    13-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/                  Major changes to logistics.
    !/    28-Dec-2004 : Multiple grid version.              ( version 3.06 )
    !/    07-Sep-2005 : Updated boundary conditions.        ( version 3.08 )
    !/    13-Jun-2006 : Split STORE in G/SSTORE             ( version 3.09 )
    !/     9-Dec-2010 : Adapted for SMC grid propagtion.     JGLi09Dec2010
    !/    16-Jan-2012 : Remove MAPSTA checking for SMC grid. JGLi16Jan2012
    !/
    !/
    !  1. Purpose :
    !
    !     'Scatter' data back to spectral storage after propagation.
    !
    !  2. Method :
    !
    !     Direct copy or communication calls (MPP version).
    !     See also W3GATH.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       ISPEC   Int.   I   Spectral bin considered.
    !       MAPSTA  I.A.   I   Status map for spatial grid.
    !       FIELD   R.A.   I   SMC grid field to be propagated.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !
    !      MPI_STARTALL, MPI_WAITALL, MPI_TESTALL
    !                Subr. mpif.h   MPI persistent comm. routines (!/MPI).
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
    !     None.
    !
    !  7. Remarks :
    !
    !     - The field is put back but not converted !
    !     - MPI persistent communication calls initialize in W3MPII.
    !     - See W3GATH and W3MPII for additional comments on data
    !       buffering.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/SHRD  Switch for message passing method.
    !     !/MPI   Id.
    !
    !     !/S     Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    USE W3GDATMD, ONLY: NSPEC, NX, NY, NSEA, NCel, NSEAL, MAPSF
    USE W3WDATMD, ONLY: A => VA
#ifdef W3_MPI
    USE W3ADATMD, ONLY: MPIBUF, BSTAT, IBFLOC, ISPLOC, BISPL, &
         NSPLOC, NRQSG2, IRQSG2, SSTORE
    USE W3ODATMD, ONLY: IAPROC, NAPROC
#endif
    USE W3ODATMD, ONLY: NDST
    !/
    IMPLICIT NONE
    !
#ifdef W3_MPI
    INCLUDE "mpif.h"
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: ISPEC, MAPSTA(NY*NX)
    REAL,    INTENT(IN)     :: FIELD(NCel)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_SHRD
    INTEGER                 :: ISEA, IXY
#endif
#ifdef W3_MPI
    INTEGER                 :: ISEA, IXY, IOFF, IERR_MPI, J,   &
         STATUS(MPI_STATUS_SIZE,NSPEC),  &
         JSEA, IB0
#endif
#ifdef W3_S
    INTEGER, SAVE           :: IENT
#endif
#ifdef W3_MPI
    LOGICAL                 :: DONE
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3SCAT')
#endif
    !
    ! 1.  Shared memory version ------------------------------------------ *
    !
#ifdef W3_SHRD
    DO ISEA=1, NSEA
      IXY           = MAPSF(ISEA,3)
      IF ( MAPSTA(IXY) .GE. 1 ) A(ISPEC,ISEA) = FIELD(ISEA)
    END DO
    !
    RETURN
#endif
    !
    ! 2.  Distributed memory version ( MPI ) ----------------------------- *
    ! 2.a Initializations
    !
    ! 2.b Convert full grid to sea grid, active points only
    !
#ifdef W3_MPI
    DO ISEA=1, NSEA
      IXY    = MAPSF(ISEA,3)
      IF ( MAPSTA(IXY) .GE. 1 ) SSTORE(ISEA,IBFLOC) = FIELD(ISEA)
    END DO
    !
    ! 2.c Send spectral densities to appropriate remote
    !
    IOFF   = 1 + (ISPLOC-1)*NRQSG2
    IF ( NRQSG2 .GT. 0 ) CALL                                  &
         MPI_STARTALL ( NRQSG2, IRQSG2(IOFF,2), IERR_MPI )
    BSTAT(IBFLOC) = 2
    !
    ! 2.d Save locally stored results
    !
    DO JSEA=1, NSEAL
      !!Li   ISEA   = IAPROC+(JSEA-1)*NAPROC
      ISEA   = MIN( IAPROC+(JSEA-1)*NAPROC, NSEA )
      A(ISPEC,JSEA) = SSTORE(ISEA,IBFLOC)
    END DO
    !
    ! 2.e Check if any sends have finished
    !
    IB0    = IBFLOC
    !
    DO J=1, MPIBUF
      IB0    = 1 + MOD(IB0,MPIBUF)
      IF ( BSTAT(IB0) .EQ. 2 ) THEN
        IOFF   = 1 + (BISPL(IB0)-1)*NRQSG2
        IF ( NRQSG2 .GT. 0 ) THEN
          CALL MPI_TESTALL ( NRQSG2, IRQSG2(IOFF,2), DONE,  &
               STATUS, IERR_MPI )
        ELSE
          DONE   = .TRUE.
        END IF
        IF ( DONE .AND. NRQSG2.GT.0 ) CALL                   &
             MPI_WAITALL ( NRQSG2, IRQSG2(IOFF,2),       &
             STATUS, IERR_MPI )
        IF ( DONE ) THEN
          BSTAT(IB0) = 0
        END IF
      END IF
    END DO
#endif
    !
    ! 2.f Last component, finish message passing, reset buffer control
    !
#ifdef W3_MPI
    IF ( ISPLOC .EQ. NSPLOC ) THEN
      DO IB0=1, MPIBUF
        IF ( BSTAT(IB0) .EQ. 2 ) THEN
          IOFF   = 1 + (BISPL(IB0)-1)*NRQSG2
          IF ( NRQSG2 .GT. 0 ) CALL                        &
               MPI_WAITALL ( NRQSG2, IRQSG2(IOFF,2),       &
               STATUS, IERR_MPI )
          BSTAT(IB0) = 0
        END IF
      END DO
      ISPLOC = 0
      IBFLOC = 0
    END IF
    RETURN
#endif
    !
    ! Formats
    !
    !/
    !/ End of W3SCATSMC ----------------------------------------------------- /
    !/
  END SUBROUTINE W3SCATSMC
  !/
  !/ End of two new subs for SMC grid.    JGLi 15Mar2011
  !/

  !> @brief Calculate cell centre lat-lon for given ids.
  !>
  !> @details
  !>  Calculate the cell centre longitude and latitude in degree for a given
  !>  list of cell identity or sequential numbers in the IMOD sub-grid.
  !>
  !>  Regular grid SX, SY, X0, Y0 and SMC grid MRFct and IJKCel arrays
  !>  in W3GDATMD are used to work out SMC grid origin and increments.
  !>  Then given cell centre coordinates are calculated.  Longitude is
  !>  wrapped into [0, 360) range, latitude in in (-90, 90) range.
  !>  The polar cell centre is off the N-Pole to avoid singularity but
  !>  its centre values are not used for propagation schemes.
  !>
  !> @param[in]   IMOD   Model number to point to
  !> @param[in]   NC     Numcer of cells to be calculated
  !> @param[in]   IDCl   List of cell id or sequential numbers
  !> @param[out]  XLon   X-Longitude in degree of listed cells
  !> @param[out]  YLat   Y-Latitude in degree of listed cells
  !>
  !> @author Jian-Guo Li
  !> @date 19 Oct 2020
  !>
  SUBROUTINE W3SMCELL( IMOD, NC, IDCl, XLon, YLat )
    !! -------------------------------------------------------------------
    !!
    !!    Generated for WW3 Multi-grid boundary matching.  JGLi19Oct2020
    !!
    !! 1. Purpose:
    !
    !     Calculate the cell centre longitude and latitude in degree for a given
    !     list of cell identity or sequential numbers in the IMOD sub-grid.
    !
    !! 2. Method:
    !
    !     Regular grid SX, SY, X0, Y0 and SMC grid MRFct and IJKCel arrays
    !     in W3GDATMD are used to work out SMC grid origin and increments.
    !     Then given cell centre coordinates are calculated.  Longitude is
    !     wrapped into [0, 360) range, latitude in in (-90, 90) range.
    !     The polar cell centre is off the N-Pole to avoid singularity but
    !     its centre values are not used for propagation schemes.
    !
    !! 3. Parameters:
    !     ----------------------------------------------------------------
    !     IMOD    Int.   I   Model number to point to.
    !     NC      Int.   I   Numcer of cells to be calculated.
    !     IDCl    Int.   I   List of cell id or sequential numbers.
    !     XLon    Real   O   X-Longitude in degree of listed cells.
    !     YLat    Real   O   Y-Latitude in degree of listed cells.
    !     ----------------------------------------------------------------
    !
    !! 4. Subroutines used:
    !
    !     None
    !
    !! 5. Called by:
    !
    !     WMGLOW, W3IOPP, WMIOPP, WW3_GINT
    !
    !! 6. Error messages:
    !
    !     - Error checks on previous setting of variable.
    !
    !! 7. Remarks:
    !
    !! 8. Structure:
    !
    !! 9. Switches:
    !
    !     !/S     Enable subroutine tracing.
    !     !/T     Enable test output
    !
    ! 10. Source code:
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD
    USE W3SERVMD, ONLY: EXTCDE
    USE W3ODATMD, ONLY: NDSE, NDST
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    !/ ------------------------------------------------------------------- /
    ! Input/Output variables

    INTEGER, INTENT(IN)               :: IMOD, NC
    INTEGER, INTENT(in), Dimension(NC):: IDCl        ! Automatic array
    REAL   , INTENT(out),Dimension(NC):: XLon, YLat
    !/ ------------------------------------------------------------------- /
    ! Local variables.

    REAL       :: XI0, YJ0, DXG, DYG, DX1, DY1
    INTEGER    :: I1, I3, J2, J4, MRF, ij, ijp, NSEM
#ifdef W3_S
    INTEGER    ::  IENT = 0
    CALL STRACE (IENT, 'W3SMCELL')
#endif

    !! 1. Convert regular grid parameters into SMC grid origin and increments.
    DXG = GRIDS(IMOD)%SX
    DYG = GRIDS(IMOD)%SY
    XI0 = GRIDS(IMOD)%X0 - 0.5*DXG
    YJ0 = GRIDS(IMOD)%Y0 - 0.5*DYG
    MRF = GRIDS(IMOD)%MRFct
    DX1 = DXG/Real(MRF)
    DY1 = DYG/Real(MRF)
    NSEM = GRIDS(IMOD)%NSEA

    !! 2. Loop over listed cells and work out their centre coordinates.

#ifdef W3_OMPG
    !$OMP Parallel DO Private(ij, ijp, I1, J2, I3, J4 )
#endif
    DO ij = 1, NC
      ijp = IDCL(ij)
      !!Li  Return South Pole lon-lat values for any ids < 1 or > NSEA
      !!    so these out of range points will not be matched to any cell.
      IF( ijp < 1 .OR. ijp > NSEM ) THEN
        XLon(ij) = 0.0
        YLat(ij) = -90.0
      ELSE
        !!    Fetch cell array indexes from given sub-grid.
        I1=GRIDS(IMOD)%IJKCel(1, ijp)
        J2=GRIDS(IMOD)%IJKCel(2, ijp)
        I3=GRIDS(IMOD)%IJKCel(3, ijp)
        J4=GRIDS(IMOD)%IJKCel(4, ijp)

        !!    Calculate its cell centre lon-lat values.
        XLon(ij) = XI0 + ( FLOAT(I1) + 0.5*FLOAT(I3) )*DX1
        YLat(ij) = YJ0 + ( FLOAT(J2) + 0.5*FLOAT(J4) )*DY1
      ENDIF
    END DO
#ifdef W3_OMPG
    !$OMP END Parallel DO
#endif

    !! 3. Wrap negative logitudes into [0, 360) range.
    WHERE( XLon < 0.0 ) XLon = XLon + 360.0
    !
    RETURN
  END SUBROUTINE W3SMCELL
  !!

  !!
  !> @brief Map lat-lon points to SMC grid cells
  !>
  !> @details
  !>  Determine whether a list of points are inside the IMOD SMC sub-grid
  !>  and return the IMOD sub-grid cell indexes, if any.
  !>
  !>  Convert point XLon and YLat values into cell indices i, j.
  !>  Match with cell ranges (i,i+di) and (j,j+dj) to see i,j in
  !>  which cell.  Return the matched cell number.  Otherwise,
  !>  return an index of 0, or no matching cell found.
  !>
  !> @param[in]   IMOD    Model number to point to
  !> @param[in]   XLon    X-Longitude in degree of search points
  !> @param[in]   YLat    Y-Latitude  in degree of search points
  !> @param[in]   NC      Number of points to be searched
  !> @param[out]  IDCl    Model number to point to
  !>
  !> @author Jian-Guo Li
  !> @date 20 Oct 2020
  !>
  SUBROUTINE W3SMCGMP( IMOD, NC, XLon, YLat, IDCl )
    !! -------------------------------------------------------------------
    !!
    !!    Generated for WW3 Multi-grid boundary matching.  JGLi22Oct2020
    !!
    !! 1. Purpose:
    !
    !     Determine whether a list of points are inside the IMOD SMC sub-grid
    !     and return the IMOD sub-grid cell indexes, if any.
    !
    !! 2. Method:
    !
    !     Convert point XLon and YLat values into cell indices i, j.
    !     Match with cell ranges (i,i+di) and (j,j+dj) to see i,j in
    !     which cell.  Return the matched cell number.  Otherwide,
    !     return an index of 0, or no matching cell found.
    !
    !! 3. Parameters:
    !     ----------------------------------------------------------------
    !     IMOD    Int.   I   Model number to point to.
    !     XLon    Real   I   X-Longitude in degree of search points.
    !     YLat    Real   I   Y-Latitude  in degree of search points.
    !     NC      Int.   I   Number of points to be searched.
    !     IDCl    Int.   O   Model number to point to.
    !     ----------------------------------------------------------------
    !
    !! 4. Subroutines used:
    !
    !     None
    !
    !! 5. Called by:
    !
    !     WMGLOW, W3IOPP, WMIOPP, WW3_GINT
    !
    !! 6. Error messages:
    !
    !     - Error checks on previous setting of variable.
    !
    !! 7. Remarks:
    !
    !! 8. Structure:
    !
    !! 9. Switches:
    !
    !     !/S     Enable subroutine tracing.
    !     !/T     Enable test output
    !
    ! 10. Source code:
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD
    USE W3SERVMD, ONLY: EXTCDE
    USE W3ODATMD, ONLY: NDSE, NDST
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    !/ ------------------------------------------------------------------- /
    ! Iuput/Output variables
    INTEGER, INTENT(IN)                :: IMOD, NC
    REAL   , INTENT(in),  Dimension(NC):: XLon, YLat
    INTEGER, INTENT(out), Dimension(NC):: IDCl
    !/ ------------------------------------------------------------------- /
    ! Local variables
    INTEGER, Dimension(NC) :: IX1, JY1
    REAL       :: XI0, YJ0, DXG, DYG, DX1, DY1, XLow(NC)
    INTEGER    :: I1, I3, J2, J4, ij, ijp, MRF, NSEM, NFund
#ifdef W3_S
    INTEGER    ::  IENT = 0
    CALL STRACE (IENT, 'W3SMCGMP')
#endif

    !! 1. Convert XLon YLat into SMC grid indexes in present SMC grid.
    DXG = GRIDS(IMOD)%SX
    DYG = GRIDS(IMOD)%SY
    XI0 = GRIDS(IMOD)%X0 - 0.5*DXG
    YJ0 = GRIDS(IMOD)%Y0 - 0.5*DYG
    MRF = GRIDS(IMOD)%MRFct
    DX1 = DXG/Real(MRF)
    DY1 = DYG/Real(MRF)
    NSEM = GRIDS(IMOD)%NSEA

    !!  Wrap longitude so they are great than XI0.
    XLow = XLon
    WHERE( XLow < XI0 ) XLow = XLow + 360.0

    !!  Convert XLon and YLat into SMC indexes.
    IX1 = FLOOR( (XLow - XI0)/DX1 )
    JY1 = FLOOR( (YLat - YJ0)/DY1 )

    !!  Initialise IDCl to be all 0
    IDCl = 0

    !! 2. Loop over all cells until all input points are found.
    NFund = 0
    ij = 0
    DO WHILE( ij < NSEM .AND. NFund < NC )
      ij = ij + 1
      I1=GRIDS(IMOD)%IJKCel(1, ij)
      J2=GRIDS(IMOD)%IJKCel(2, ij)
      I3=GRIDS(IMOD)%IJKCel(3, ij)
      J4=GRIDS(IMOD)%IJKCel(4, ij)
      LPNBIS: DO ijp = 1, NC
        IF( IDCl(ijp) .EQ. 0 ) THEN
          !!  Check if IX1 and JY1 fall inside the cell i,j range.
          IF((IX1(ijp) .GE. I1) .AND. (IX1(ijp) .LT. I1+I3) .AND.  &
               (JY1(ijp) .GE. J2) .AND. (JY1(ijp) .LT. J2+J4)) THEN
            NFund = NFund + 1
            IDCl(ijp) = ij
            EXIT  LPNBIS
          ENDIF
        ENDIF
      END DO  LPNBIS
    END DO

    !!  If any IDCl element remians to be 0, it means no cell is found
    !!  covering this point.  So check IDCl(ij) > 0 to ensure in grid.

    RETURN

  END SUBROUTINE W3SMCGMP
  !!

  !/ End of module W3PSMCMD -------------------------------------------- /
  !/
END MODULE W3PSMCMD
