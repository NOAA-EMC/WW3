!/ ------------------------------------------------------------------- /
      MODULE W3STR2MD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           A. Roland (IT&E)        |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         29-May-2012 |
!/                  +-----------------------------------+
!/
!/    15-Jul-2005 : Origination.                        ( version 3.07 )
!/    23-Jun-2006 : Formatted for submitting code for   ( version 3.09 )
!/                  inclusion in WAVEWATCH III.
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/
!/    Copyright 2009 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     This peace of code computes the triad interaction term in the same way  
!     as done in the SWAN model. 
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
!      W3STR2    Subr. Public   User supplied triad interactions.
!      INSTR2    Subr. Public   Corresponding initialization routine.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Remarks : The approach is truncated version of the work of Elderberky.
!               In SWAN the wave spectra is treated as one-dimensional and 
!               only the transfer to the higher harmoics is taken into account
!               for this no justification is given and it has to be further investigated.
!               The approximation of Elderberky is for a flat bottom (actually bragg-0 resonance)
!               The biggest problem is that it is not conservative, which is the biggest limitation factor.
!               Moreover it is questionable if it was taken into account the in spectral wave models the 
!               freq. bandwidths are exponentially distributed in freq. space, which leads to the problem that
!               it is possible that some jacobian transformation is missing the derivation of hte discrete form, 
!               I am now looking into this and I hope that I can give some closure soon.  
!
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
      SUBROUTINE W3STR2 (A, CG, WN, DEPTH, IX, S, D)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           A. Roland               |
!/                  |                        FORTRAN 90 |
!/                  | Last update :          2-Feb-2014 |
!/                  +-----------------------------------+
!/
!/    15-Jul-2005 : Origination.                        ( version 3.07 )
!/    23-Jun-2006 : Formatted for submitting code for   ( version 3.09 )
!/                  inclusion in WAVEWATCH III.
!/
!  1. Purpose :
!
!     Slot for user-supplied triad interaction source term.
!
!  2. Method :
!
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
!        Check resolution
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
      USE W3GDATMD, ONLY: NK, NTH, NSPEC, DTH, SIG, DDEN, FTE, FTF, PPTRIAD
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
      REAL, INTENT(IN)        :: CG(NK), WN(NK), DEPTH, A(NSPEC)
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
      INTEGER, SAVE           :: IENT = 0
#endif
      INTEGER I1, I2, ID, IDDUM, IENT, II, IS, ISM, ISM1, ISMAX, &
              ISP, ISP1, ITH, IK
      REAL    AUX1, AUX2, BIPH, C0, CM, DEP, DEP_2, DEP_3, E0, EM, HS, &
              FT, RINT, SIGPICG, SINBPH, STRI, WISM, WISM1, WISP, & 
              WISP1, W0, WM, WN0, WNM, XIS, XISLN
      REAL, ALLOCATABLE :: E(:), SA(:,:)
      REAL              :: EB(NK), EBAND, EMEAN, SIGM01
!----- Temp (to be moved) -----
      REAL, ALLOCATABLE :: EF(:),SF(:)
      REAL    :: URSELL
!------------------------------
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3STR2')
#endif
!
! 0.  Initializations ------------------------------------------------ *
!
!     **********************************************************
!     ***    The initialization routine should include all   ***
!     *** initialization, including reading data from files. ***
!     **********************************************************
!
! 1.  .... ----------------------------------------------------------- *
!
!---- Compute SIGM01 (= 2pi/Tm01) for use in source term
!
! 1.  Integral over directions
!
      SIGM01 = 0.
      EMEAN  = 0.
!      FMEAN  = 0.

      DO IK=1, NK
        EB(IK) = 0.
        DO ITH=1, NTH
          EB(IK) = EB(IK) + A(ITH+(IK-1)*NTH)
        END DO
      END DO
!
! 2.  Integrate over wave numbers  
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
      SIGM01 = MAX ( 1.E-7 , SIGM01 ) / EMEAN

      IF (ABS(FACSCL-2.).GT.0.05) THEN
         FACRES = 10.**( LOG10(2.) / FLOAT(IRES) )
         SIGLOW   = SIG(NK) / ( FACRES**(FLOAT(NK-1) ) )
!        WRITE (*,*) 'CHECK RESOLUTION', IRES, FACSCL, FACRES, SIGLOW
        END IF

      HS = 4.*SQRT( MAX(0.,EMEAN) )
      URSELL = (GRAV*HS)/(2.*SQRT(2.)*SIGM01**2*DEPTH**2)
!---------------------------------------------

      DEP   = DEPTH
      DEP_2 = DEP**2
      DEP_3 = DEP**3
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

      ALLOCATE (E (1:NK))
      ALLOCATE (SA(1:NTH,1:NK+ISP1))
      E  = 0.
      SA = 0.

!
!     --- compute maximum frequency for which interactions are calculated
!
      ISMAX = 1
      DO IK = 1, NK
       IF ( SIG(IK) .LT. ( PPTRIAD(2) * SIGM01) ) THEN
          ISMAX = IK
        ENDIF
      ENDDO
      ISMAX = MAX ( ISMAX , ISP1 )
!
!     --- compute 3-wave interactions
!
      IF ( URSELL.GE.PPTRIAD(5) ) THEN
!
!       --- calculate biphase
!
        BIPH   = (0.5*PI)*(TANH(PPTRIAD(4)/URSELL)-1.)
        SINBPH = ABS( SIN(BIPH) )
!
        ALLOCATE (EF (1:NK))
        EF = 0.
        DO ITH = 1, NTH
!
!          --- initialize array with E(f) for the direction considered
!          --- (convert from N(k) to E(f) using proper Jacobian)
!
           DO IK = 1, NK
              E(IK) = A(ITH+(IK-1)*NTH) * TPI * SIG(IK) / CG(IK)
              EF(IK) = EF(IK) + E(IK)        
           END DO
!
           DO IK = 1, ISMAX

              E0  = E(IK)
              W0  = SIG(IK)
              WN0 = WN(IK)
              C0  = W0 / WN0

              IF ( IK.GT.-ISM1 ) THEN
                 EM  = WISM * E(IK+ISM1)   + WISM1 * E(IK+ISM)
                 WM  = WISM * SIG(IK+ISM1) + WISM1 * SIG(IK+ISM)
                 WNM = WISM * WN(IK+ISM1)  + WISM1 * WN(IK+ISM)
                 CM  = WM / WNM
              ELSE
                 EM  = 0.
                 WM  = 0.
                 WNM = 0.
                 CM  = 0.
              END IF

              AUX1 = WNM**2 * ( GRAV * DEP + 2.*CM**2 )
              AUX2 = WN0 * DEP * ( GRAV * DEP + &
                                   (2./15.) * GRAV * DEP_3 * WN0**2 - &
                                   (2./ 5.) * W0**2 * DEP_2 )
              RINT = AUX1 / AUX2
              FT = PPTRIAD(1) * C0 * CG(IK) * RINT**2 * SINBPH

              SA(ITH,IK) = MAX(0., FT * ( EM * EM - 2. * EM * E0 ))

           END DO
        END DO

        DEALLOCATE(EF)
!
!        ---  put source and diagonal terms together
!             (using Jacobian for S(f) -> S(k))
!
        ALLOCATE (SF (1:NK))
        SF = 0.
        DO IK = 1, NK
           SIGPICG = SIG(IK) * 2. * PI / CG(IK)
           DO ITH = 1, NTH
!             --- Source term
              S(ITH+(IK-1)*NTH) = 2.*( SA(ITH,IK) - &
                                    2.*(WISP  * SA(ITH,IK+ISP1) + &
                                        WISP1 * SA(ITH,IK+ISP )) ) / &
                                    SIGPICG
!             --- Functional derivative 
              SF(IK) = 2.*( SA(ITH,IK) - &
                         2.*(WISP  * SA(ITH,IK+ISP1) + &
                         WISP1 * SA(ITH,IK+ISP )) ) + SF(IK)
              D = 0.
           END DO
        END DO
        DEALLOCATE(SF)

      ELSE
        D = 0.
        S = 0.
      END IF

      DEALLOCATE(E,SA)

      RETURN
!/
!/ End of W3STR2 ----------------------------------------------------- /
!/
      END SUBROUTINE W3STR2
!/ ------------------------------------------------------------------- /
!/
      END MODULE W3STR1MD
