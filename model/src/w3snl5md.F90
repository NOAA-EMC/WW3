!> @file
!> @brief Interface module for GKE (resonant & quasi-resonant four-wave
!>        interactions).
!>
!> @author O. Gramstad
!> @author Q. Liu
!> @date   07-Jun-2021
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Interface module for GKE (resonant & quasi-resonant four-wave
!>        interactions).
!>
!> @author O. Gramstad
!> @author Q. Liu
!> @date   07-Jun-2021
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>
MODULE W3SNL5MD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           O. Gramstad             |
  !/                  |           Q. Liu                  |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         07-Jun-2021 |
  !/                  +-----------------------------------+
  !/
  !/    24-Sep-2013 : Origination.                        ( version 3.14 )
  !/    24-Sep-2013 : GKE for the regular wavenumbergrid  ( O. Gramstad  )
  !/                  (interpolation required)
  !/    02-Dec-2013 : GKE for WW3 logarithmic freq. grid  ( O. Gramstad  )
  !/                  (single grid point)
  !/    27-Feb-2019 : GKE for 2D applications.            ( version 7.13 )
  !/                                                      ( Q. Liu )
  !/    07-06-2021  : Merge into WW3 Github               ( version 7.13 )
  !/                                                      ( Q. Liu       )
  !/
  !  1. Purpose :
  !     Interface module for GKE (resonant & quasi-resonant four-wave
  !     interactions)
  !
  !  2. Variables and types :
  !
  !  3. Subroutines and functions :
  !
  !      Name         Type  Scope    Description
  !     -------------------------------------------------------------------
  !      W3SNL5       Subr. Public   Interface to gkeModule
  !      INSNL5       Subr. Public   Initialization routine
  !      CALC_WBTv2   Subr. Private  Calc. dominant wave breaking prob.
  !      INPOUT       Subr. Private  Point output
  !     -------------------------------------------------------------------
  !
  !  4. Future work: Dnl
  !/
  !/ ------------------------------------------------------------------- /
  IMPLICIT NONE
  !/
  ! Subrs.
  PUBLIC      :: W3SNL5, INSNL5
  PRIVATE     :: CALC_WBTv2, INPOUT
  ! Vars.
  PRIVATE     :: NSEL, PSEA, PNMS
  !/ ------------------------------------------------------------------- /
  ! Parameter list
  INTEGER                              :: NSEL
  INTEGER, ALLOCATABLE, SAVE           :: PSEA(:)
  CHARACTER(LEN=10), ALLOCATABLE, SAVE :: PNMS(:)
  !
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Interface to CalcQRSNL subroutine of the GKE module.
  !>
  !> @param[in]  A
  !> @param[in]  CG
  !> @param[in]  WN
  !> @param[in]  FMEAN
  !> @param[in]  T1ABS
  !> @param[in]  U10
  !> @param[in]  UDIR
  !> @param[in]  JSEA
  !> @param[out] S
  !> @param[out] D
  !> @param[out] KURT
  !>
  !> @author O. Gramstad
  !> @author Q. Liu
  !> @date   24-Apr-2019
  !>
  SUBROUTINE W3SNL5(A, CG, WN, FMEAN, T1ABS, U10, UDIR, JSEA, &
       S, D, KURT)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           O. Gramstad             |
    !/                  |           Q. Liu                  |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         24-Apr-2019 |
    !/                  +-----------------------------------+
    !/
    !/    24-Sep-2013 : Origination.                        ( version 3.14 )
    !/    24-Sep-2013 : GKE for resonant & quasi-resonant four-wave
    !/                  interactions                        ( O. Gramstad  )
    !/    27-Feb-2019 : Full implementation of GKE          ( version 7.13 )
    !/                                                      ( Q. Liu       )
    !/    21-Apr-2019 : Phase mixing option                 ( version 7.13 )
    !/                                                      ( Q. Liu       )
    !/    24-Apr-2019 : Phase mixing option (b_T)           ( version 7.13 )
    !/                                                      ( Q. Liu       )
    !/    02-May-2019 : Organize screen output & disable binary output
    !/                                                      ( version 7.13 )
    !/                                                      ( Q. Liu       )
    !/
    !/
    !  1. Purpose :
    !
    !     Interface to CalcQRSNL subr. of the GKE module. Please refer to
    !     -------------
    !     gkeModule.f90 for further details.
    !     -------------
    !
    !     ◆ Different times used in this module
    !
    !     |----o---------o----o--|-|--o-----o------o-----o------o----> (t)
    !     ^    ^         ^       ^ ^ T1ABS (absol. current time step)¹
    !     |    |         |       |
    !     |    |         |       v t0 (relat. time, previous time step)
    !     |    |<------->|
    !     |    | PM_IVAL (phase mixing interval, relat. time)
    !     |    |
    !     |    v PM_PREV (phase mixing, appear quasi-periodically)
    !     |              (relat. time)
    !     |
    !     v TBEG (absol. begining time, defined by ww3_shel.inp)
    !
    !     ¹ Because of using the dynamic integration scheme, T1ABS
    !       is related to, but not the same as, TIME in w3wdatmd.ftn
    !  2. Method :
    !
    !  3. Parameters :
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
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS, ONLY: GRAV, TPI
    USE W3GKEMD,   ONLY: CalcQRSNL, qr_depth
    USE W3GDATMD,  ONLY: NK, NTH, NSPEC, SIG, TH,            &
         GTYPE, RLGTYPE, CLGTYPE,            &
         QR5DPT, QI5NNZ, QI5PMX
    USE W3WDATMD,  ONLY: QI5TBEG, QR5TIM0, QR5CVK0, QC5INT0, &
         QR5TMIX
    USE W3ODATMD,  ONLY: FLOUT, NOPTS, TOSNL5, TOLAST, &
         IAPROC, NAPOUT, SCREEN
    USE W3PARALL,  ONLY: INIT_GET_ISEA
    USE W3TIMEMD,  ONLY: DSEC21
    USE W3SERVMD,  ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)        :: A(NTH, NK)         ! N(θ, k)
    REAL, INTENT(IN)        :: CG(NK)             ! Cg(k)
    REAL, INTENT(IN)        :: WN(NK)             ! WN(k)
    REAL, INTENT(IN)        :: FMEAN              ! 1/T_{0, -1}
    INTEGER, INTENT(IN)     :: T1ABS(2)           ! Absol. t1
    REAL, INTENT(IN)        :: U10                ! Wind velocity
    REAL, INTENT(IN)        :: UDIR               ! φ (in rad)
    INTEGER, INTENT(IN)     :: JSEA               ! Local sea point count
    REAL, INTENT(OUT)       :: S(NTH,NK),      &  ! Snl
         D(NTH,NK),      &  ! Dnl
         KURT               ! Kurtosis

    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    REAL, PARAMETER         :: BTLOW = 10., BTHGH = 500.
    REAL                    :: T0REL, T1REL, TDEL1, TDEL2
    REAL                    :: Cvk1(NSPEC), SNL(NSPEC), DNL(NSPEC)
    REAL                    :: Cvk0(NSPEC)
    COMPLEX                 :: INPQR0(QI5NNZ)
    INTEGER                 :: IK, ITH, ISPEC, ISEA, JLOC
    INTEGER, ALLOCATABLE    :: PDIFF(:)
    LOGICAL, SAVE           :: FSTOUT = .TRUE.
    REAL                    :: FACTOR(NK), A2(NK, NTH), S2(NK, NTH)
    REAL                    :: PM_PREV, PM_IVAL, PM_DELT
    REAL                    :: WBT, BTINV
    INTEGER                 :: IUNT
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif

    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3SNL5')
#endif
    !
    !/ ------------------------------------------------------------------- /
    ! Read in wave info. @ the previous time step t0
    ! Array initialization is done in w3wdat/w3setw (called by w3initmd)
    T0REL  = QR5TIM0(JSEA)              ! t0 (nsea)
    CVK0   = QR5CVK0(:, JSEA)           ! Cvk (ns, nsea) @ t0
    INPQR0 = QC5INT0(:, JSEA)           ! Inpqr (nnz, nsea) @ t0
    !
    ! Calc. Relative time for T1ABS (QI5TBEG as the reference)
    T1REL  = DSEC21(QI5TBEG, T1ABS)     ! in second
    !
    ! W3WAVEMD: IF ( IT.EQ.0 ) DTG = 1 → T1REL = -1 (the first step of
    ! integration; TIME = TCALC/TOFRST, DTG = 0 →  1, QI5TBEG = TIME - 1)
    IF(T1REL < 0.) T1REL = 0.
    !
    ! Three options for phase mixing
    IF (QI5PMX .EQ. 0) THEN
      ! 1) 0: no phase mixing
#ifdef W3_TS
      IF (IAPROC .EQ. NAPOUT)                                    &
           WRITE(SCREEN, '(A, 2(I10.8, I7.6), E12.3)')                &
           " ⊚ → [WW3 SNL₅] QI5TBEG, T1ABS, T1REL:",              &
           QI5TBEG, T1ABS, T1REL
#endif
    ELSE
#ifdef W3_TS
      IF (IAPROC .EQ. NAPOUT)                                    &
           WRITE(SCREEN, '(A, 2(I10.8, I7.6), E12.3)', ADVANCE='no')  &
           " ⊚ → [WW3 SNL₅] QI5TBEG, T1ABS, T1REL, T1REL[P]:",    &
           QI5TBEG, T1ABS, T1REL
#endif
      !
      ! Calc. Phase mixing interval
      IF (QI5PMX .GT. 0) THEN
        ! 2) N: mix phase by every N characteristic wave periods
        IF (ABS(FMEAN) < 1E-7) THEN      ! FMEAN may be 0.
          PM_IVAL = REAL(QI5PMX) * 1.  ! then, assume FMEAN = 1.
        ELSE
          PM_IVAL = REAL(QI5PMX) * (1. / FMEAN)
        END IF
        !
      ELSE IF (QI5PMX .LT. 0) THEN
        ! 3) < 0: mix phase based on dominant wave breaking probability bT
        ! Calc bT
        WBT   = CALC_WBTv2(A, CG, WN, QR5DPT, U10, UDIR) ! [0, 1.]
        ! Mix phase by every 1/bT periods
        ! Odin used bT < 1/15. (0.066) → BTLOW = 15 and PM_IVAL > 150 s
        BTINV = MAX(BTLOW, MIN(1./MAX(1E-6, WBT), BTHGH))
        IF (ABS(FMEAN) < 1E-7) THEN      ! FMEAN may be 0.
          PM_IVAL = BTINV * 1.  ! then, assume FMEAN = 1.
        ELSE
          PM_IVAL = BTINV * (1. / FMEAN)
        END IF
      END IF
      !
      ! Previous phase mixing time (relat. to TBEG)
      ! QR5TMIX has already been initialized in w3wdatmd as zero.
      PM_PREV = QR5TMIX(JSEA)
      ! Update t1 if necessary
      PM_DELT = T1REL - PM_PREV
      IF (PM_DELT .GE. PM_IVAL) THEN
        QR5TMIX(JSEA) = T1REL        ! relat. to TBEG → PM_PREV
        T1REL         = 0.
      ELSE
        T1REL         = PM_DELT
      END IF
#ifdef W3_TS
      IF (IAPROC .EQ. NAPOUT) THEN
        WRITE(SCREEN, '(F9.1)') T1REL
        IF (QI5PMX .LT. 0 ) WRITE(SCREEN, '(A, F6.3)') '↔ bT: ', WBT
      ENDIF
#endif
    END IF
    !
    ! Calc. Cvk1 from A (C(\bm{k}) = g N(k, θ) / k)
    DO IK = 1, NK
      DO ITH = 1, NTH
        ISPEC = ITH + (IK-1) * NTH
        Cvk1(ISPEC) = A(ITH, IK) / WN(IK) * GRAV
      END DO
    END DO
    !
    ! CalcQRSNL(nk, nth, sig, th, t0, t1, Cvk0, Cvk1, Inpqr0, Snl, Dnl, Kurt)
    ! Depth is needed for reading in kernels at the first run
    qr_depth = QR5DPT
    CALL CalcQRSNL(NK, NTH, SIG(1:NK), TH,     &
         T0REL, T1REL, CVK0, CVK1,   &
         INPQR0, SNL, DNL, KURT)
    !
    ! Tranform back from C(k) to N(k)
    ! TODO D(ITH, IK) (See NL2 for reference)
    D = 0.0
    DO IK = 1, NK
      DO ITH = 1, NTH
        ISPEC = ITH + (IK-1) * NTH
        S(ITH, IK) = SNL(ISPEC) * WN(IK) / GRAV
      END DO
    END DO
    !
    ! Store wave info. @ t1 → t0
    QR5TIM0(JSEA)    = T0REL
    QR5CVK0(:, JSEA) = CVK0
    QC5INT0(:, JSEA) = INPQR0
    !
    ! Point output (Snl term)
    ! First ouput action (Find nearest grid points & generate binary files)
    IF (FSTOUT) THEN
      CALL INPOUT
      FSTOUT = .FALSE.
      IF (IAPROC .EQ. NAPOUT) THEN
        WRITE(SCREEN, *)
        WRITE(SCREEN, '(A)')                          &
             ' ⊚ → [WW3 SNL₅] Point ouptut initialization'
        WRITE(SCREEN, '(A, I4)')                      &
             ' ⊚ → [WW3 SNL₅] # of valid points: ', NSEL
        WRITE(SCREEN, *)
      END IF
    END IF
    !
    ! Calc FACTOR used for Jacobian tranformation from N(k, θ) to E(f, θ)
    FACTOR = TPI / CG * SIG(1:NK)
    !
    ! Regular grid & curvilinear grid
    IF ( ((GTYPE .EQ. RLGTYPE) .OR. (GTYPE .EQ. CLGTYPE)) &
         .AND. FLOUT(2) .AND. NSEL .GT. 0) THEN
      TDEL1 = DSEC21(T1ABS, TOSNL5)
      TDEL2 = DSEC21(T1ABS, TOLAST(:, 2)) ! not really useful since
      ! TOSNL5 can never catch
      ! TOLAST
      ! Output time
      IF (ABS(TDEL1) < 1E-6 .OR. ABS(TDEL2) < 1E-6) THEN
        ! JSEA→ ISEA
        CALL INIT_GET_ISEA(ISEA, JSEA)
        ! Find the loc of ISEA at PSEA (nearest sea grid point)
        IF (ALLOCATED(PDIFF)) DEALLOCATE(PDIFF); ALLOCATE(PDIFF(NSEL))
        PDIFF = ABS(PSEA(1:NSEL) - ISEA)
        IF (ANY(PDIFF .EQ. 0)) THEN
          JLOC = MINLOC(PDIFF, 1)
#ifdef W3_TS
          IF (IAPROC .EQ. NAPOUT)                               &
               WRITE(SCREEN, '(3A, I10.8, I7.6)')                    &
               '✓ Point output for |', PNMS(JLOC), '| @', T1ABS
#endif

          ! N(θ, k) →  F(f, θ) & S(θ, k) →  S(f, θ)
          DO ITH = 1, NTH
            A2(:, ITH) = A(ITH, :) * FACTOR
            S2(:, ITH) = S(ITH, :) * FACTOR
          END DO
          ! NaN Check
          IF (HasNaN(NK, NTH, A2) .OR. HasNaN(NK, NTH, S2)) THEN
            IF (IAPROC .EQ. NAPOUT)                            &
                 WRITE(SCREEN, *) '★★★ Warning: find NaN in E(f, θ) &
                 & or Snl(f, θ) !'
          END IF
          ! unit no.
          IUNT = 500 + JLOC
          ! Store data (binary)
          !                 OPEN(IUNT, FILE='NL5_'//trim(PNMS(JLOC))//'_src.bin',  &
          !                      form='unformatted', convert=file_endian, ACCESS='stream',             &
          !                      STATUS='old', POSITION='append', ACTION='write')
          !                 WRITE(IUNT) T1ABS
          !                 WRITE(IUNT) KURT
          !                 WRITE(IUNT) A2
          !                 WRITE(IUNT) S2
          !                 CLOSE(IUNT)
          ! Store data (ascii)
          OPEN(IUNT, FILE='NL5_'//trim(PNMS(JLOC))//'_src.dat', &
               FORM='formatted', STATUS='old',                  &
               POSITION='append', ACTION='write')
          WRITE(IUNT, '(I10.8, I7.6)') T1ABS
          WRITE(IUNT, '(ES11.3)')      KURT
          WRITE(IUNT, 113) A2
          WRITE(IUNT, 113) S2
          CLOSE(IUNT)
          !
        END IF
      END IF
    END IF
    ! Format
113 FORMAT ((10ES11.3))
    !/
    !/ End of W3SNL5 ----------------------------------------------------- /
    !/
  END SUBROUTINE W3SNL5
  !/ ------------------------------------------------------------------- /

  !>
  !> @brief Initialization for the GKE module (Prepare wavenumber grid & kernel
  !>        coefficients).
  !>
  !> @author Q. Liu
  !> @date   27-Feb-2019
  !>
  SUBROUTINE INSNL5
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           Q. Liu                  |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         27-Feb-2019 |
    !/                  +-----------------------------------+
    !/
    !/    27-Feb-2019 : Origination.                        ( version 7.13 )
    !/                                                      ( Q. Liu )
    !/
    !  1. Purpose :
    !
    !     Initialization for the GKE module (Prepare wavenumber grid & kernel
    !     coefficients)
    !
    !  2. Method :
    !     See subrs. PrepKGrid & PrepKernelIO of gkeModule.f90
    !
    !  3. Parameters :
    !
    !  4. Subroutines used :
    !     ----------------------------------------------------------------
    !      Name          Type  Module      Description
    !     ----------------------------------------------------------------
    !      STRACE        Subr. W3SERVMD    Subroutine tracing.
    !      PrepKernelIO  Subr. gkeModule   KGrid & Kernel Coeff.
    !
    !  5. Called by :
    !     ----------------------------------------------------------------
    !      Name          Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3IOGR        Subr. W3IOGRMD Model definition file management.
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
    USE W3GKEMD,  ONLY: qr_depth, qr_oml, qi_disc, qi_kev, qi_nnz,  &
         qi_interp, PrepKernelIO
    USE W3GDATMD, ONLY: NK, NTH, SIG, TH,                           &
         QR5DPT, QR5OML, QI5DIS, QI5KEV, QI5NNZ,     &
         QI5IPL, QI5PMX
    USE W3ODATMD, ONLY: IAPROC, NAPOUT, SCREEN
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
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'INSNL5')
#endif
    !
    ! Set important parameters for GKE module (QR[I]5DPT/OML/DIS/KEV are
    ! defined in ww3_grid.inp, and QI5NNZ is not known yet)
    qr_depth = QR5DPT
    qr_oml   = QR5OML
    qi_disc  = QI5DIS
    qi_kev   = QI5KEV
    qi_interp= QI5IPL
    !
    ! Prepare (kx, ky) grid & kernel coefficients
    CALL PrepKernelIO(NK, NTH, SIG(1:NK), TH, 'WRITE')
    !
    ! Store qi_NNZ to QI5NNZ (which will be used to initialize the
    ! QC5INT0 array)
    QI5NNZ   = qi_nnz
    !
    ! Q. Liu (TODO)
    IF (IAPROC .EQ. NAPOUT) THEN
      WRITE(SCREEN, '(A, F6.1)') " ⊚ → [WW3 SNL₅]: water depth   : ", qr_depth
      WRITE(SCREEN, '(A, F7.2)') " ⊚ → [WW3 SNL₅]: ω λc cut off  : ", qr_oml
      WRITE(SCREEN, '(A, I4)'  ) " ⊚ → [WW3 SNL₅]: Discretiza.   : ", qi_disc
      WRITE(SCREEN, '(A, I4)'  ) " ⊚ → [WW3 SNL₅]: GKE version   : ", qi_kev
      WRITE(SCREEN, '(A, I12)' ) " ⊚ → [WW3 SNL₅]: # of quartets : ", qi_nnz
      WRITE(SCREEN, '(A, I4)'  ) " ⊚ → [WW3 SNL₅]: interpol.     : ", qi_interp
      WRITE(SCREEN, '(A, I4)'  ) " ⊚ → [WW3 SNL₅]: phase mixing  : ", QI5PMX
    END IF
    !/
    !/ End of INSNL5 ----------------------------------------------------- /
    !/
  END SUBROUTINE INSNL5
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Estimate the dominant wave breaking probability.
  !>
  !> @details Based on the empirical parameterization proposed by
  !>  Babanin et al. (2001).
  !>
  !> @param   A
  !> @param   CG
  !> @param   WN
  !> @param   DPT
  !> @param   U10
  !> @param   UDIR
  !> @returns CALC_WBTv2
  !>
  !> @author Q. Liu
  !> @date   24-Apr-2019
  !>
  FUNCTION CALC_WBTv2 (A, CG, WN, DPT, U10, UDIR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           Q. Liu                  |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         24-Apr-2019 |
    !/                  +-----------------------------------+
    !/
    !/    24-Aug-2018 : Origination. (w3iogomd.ftn)         ( version 6.06 )
    !/                  Used for output parameter b_T       ( Q. Liu       )
    !/
    !/    24-Apr-2019 : Simplified for NL5                  ( version 7.13 )
    !/                                                      ( Q. Liu       )
    !/
    !  1. Purpose :
    !
    !     Estimate the dominant wave breaking probability b_T based on
    !     the empirical parameterization proposed by Babanin et al. (2001).
    !     From their Fig. 12, we have
    !
    !         b_T = 85.1 * [(εp - 0.055) * (1 + H_s/d)]^2.33,
    !
    !     where ε is the significant steepness of the spectral peak, H_s is
    !     the significant wave height, d is the water depth.
    !
    !     For more details, please see
    !         Banner et al.  2000: JPO,      30,  3145 -  3160.
    !         Babanin et al. 2001: JGR, 106(C6), 11569 - 11676.
    !
    !     See subr. CALC_WBT in w3iogomd.ftn for more details.
    !
    !/ ------------------------------------------------------------------- /
    USE W3DISPMD, ONLY: WAVNU1
    USE W3GDATMD, ONLY: NK, NTH, SIG, ESIN, ECOS, DTH, DSII
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)     :: A(NTH, NK)       ! N(θ, k)
    REAL, INTENT(IN)     :: CG(NK)           ! Cg(k)
    REAL, INTENT(IN)     :: WN(NK)           ! WN(k)
    REAL, INTENT(IN)     :: DPT              ! water depth
    REAL, INTENT(IN)     :: U10              ! wind velocity
    REAL, INTENT(IN)     :: UDIR             ! wind dirc. (φ in rad)
    REAL                 :: CALC_WBTv2
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !
    REAL, PARAMETER      :: BETA  = 1.2
    !
    INTEGER              :: IK, ITH
    REAL                 :: SINU, COSU, TC, TFORCE
    REAL                 :: ESIG(NK) ! E(σ)
    REAL                 :: FACTOR, ET, HS, ETP, HSP, SIGP, KP, &
         CGP, WSTP, TWBT
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'CALC_WBTv2')
#endif
    !
    ! Wind info. is required to select wind sea partition from the wave
    ! spectrum.
    !
    ! Following Janssen et al. (1989) and Bidlot (2001), spectral components
    ! are considered to be subject to local wind forcing when
    !
    !          c / [U cos(θ - φ)] < β,
    !
    ! where c is the phase velocity c = σ/k, φ is the wind direction, U is
    ! the wind speed U10, (sometimes approximated by U10≅ 28 * ust), β is
    ! the constant forcing parameter with β∈ [1.0, 2.0]. By default, we use
    ! β = 1.2 (Bidlot 2001).
    !
    SINU  = SIN(UDIR)                    ! sinφ
    COSU  = COS(UDIR)                    ! cosφ
    !
    ESIG  = 0.                           ! E(σ)
    ET    = 0.                           ! ΣE(σ)δσ
    ETP   = 0.                           ! ΣE(σ)δσ at peak only
    !
    DO IK = 1, NK
      TC     = SIG(IK) / WN(IK)        ! phase velocity c=σ/k
      FACTOR = SIG(IK) / CG(IK)        ! σ / cg
      FACTOR = FACTOR * DTH            ! σ / cg * δθ
      !
      DO ITH = 1, NTH
        TFORCE = TC - U10 * (COSU*ECOS(ITH)+SINU*ESIN(ITH)) &
             * BETA

        IF (TFORCE .LT. 0.) THEN ! wind sea component
          ESIG(IK) = ESIG(IK) + A(ITH, IK) * FACTOR
        ENDIF
      ENDDO ! ITH
      !
    ENDDO ! IK
    !
    ! ESIG is E(σ) of the wind sea after filtration of any background swell.
    ! Now we need to get Hs & σp for the wind sea spectrum.
    ! Unlike w3iogomd.ftn, the tail energy is not added here.
    ET = SUM(ESIG * DSII)
    HS = 4. * SQRT(MAX(0., ET))
    !
    ! Get σp from E(σ)
    ! FPOPT = 0 in w3iogomd.ftn: fp defined by Young (1999, p. 239)
    SIGP = SUM(ESIG**4. * SIG(1:NK) * DSII) /  &
         MAX(1E-10, SUM(ESIG**4. * DSII))
    IF (ABS(SIGP) < 1E-7) SIGP = SIG(NK) ! σp = 0
    !
    ! kp from σp (linear dispersion)
    CALL WAVNU1 (SIGP, DPT, KP, CGP)
    !
    !                         { /1.3σp         }1/2
    ! peak wave height Hp = 4 { |      E(σ) dσ }
    !                         { /0.7σp         }
    !
    DO IK = 1, NK
      IF ( (SIG(IK) >= 0.7 * SIGP) .AND. &
           (SIG(IK) <= 1.3 * SIGP) ) THEN
        ETP = ETP + ESIG(IK) * DSII(IK)
      ENDIF
    ENDDO ! IK
    HSP  = 4. * SQRT(MAX(0., ETP))
    !
    ! significant steepness of the peak region εp
    !
    WSTP = 0.5 * KP * HSP
    !
    ! Dominant wave breaking b_T
    !
    TWBT = 85.1 * (MAX(0.0, WSTP - 0.055) * (1 + HS/DPT))**2.33
    TWBT = MIN(1.0, TWBT)
    !
    CALC_WBTv2 = TWBT

    RETURN
    !/
    !/ End of  CALC_WBTv2 ------------------------------------------------ /
    !/
  END FUNCTION CALC_WBTv2

  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Initialization for point output (Snl).
  !>
  !> @author Q. Liu
  !> @date   25-Mar-2019
  !>
  SUBROUTINE INPOUT
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           Q. Liu                  |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         25-Mar-2019 |
    !/                  +-----------------------------------+
    !/
    !/    24-Mar-2019 : Origination.                        ( version 7.13 )
    !/                                                      ( Q. Liu       )
    !/    27-Apr-2019 : Add the ascii option                ( Q. Liu       )
    !/
    !  1. Purpose :
    !
    !     Initialization for point output (Snl) [see also W3IOPP of w3iopomd]
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !  4. Subroutines used :
    !
    !  5. Called by :
    !     ----------------------------------------------------------------
    !      Name          Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3SNL5        Subr. W3SNL5MD S_{nl} GKE
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
    !/
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS, ONLY: TPI
    USE W3GDATMD,  ONLY: NK, NTH, SIG, TH, QR5DPT,         &
         FLAGLL, XGRD, YGRD, MAPSTA, MAPFS
    USE W3ODATMD,  ONLY: NOPTS, PTNME, PTLOC, IPTINT,      &
         IAPROC, NAPOUT, SCREEN
    USE W3SERVMD,  ONLY: DIST_SPHERE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/
    INTEGER           :: IXS(4), IYS(4), IX, IY, IPT, IS, &
         JLOC, JX, JY, ISEA, SMAP(4), IUNT
    REAL              :: PLON, PLAT, XLON, YLAT, DIST(4)
    !/ ------------------------------------------------------------------- /
    !/
    ! Initialize arrays
    IF (ALLOCATED(PSEA)) DEALLOCATE(PSEA); ALLOCATE(PSEA(NOPTS))
    IF (ALLOCATED(PNMS)) DEALLOCATE(PNMS); ALLOCATE(PNMS(NOPTS))
    !
    NSEL    = 0
    PSEA(:) = 0
    PNMS(:) = 'null'
    DIST(:) = -999.
    !
    DO IPT = 1, NOPTS
      ! Get lon & lat of this output point
      PLON   = PTLOC(1, IPT)
      PLAT   = PTLOC(2, IPT)
      ! Get four indices surrounding the output point
      IXS(:) = IPTINT(1, :, IPT)
      IYS(:) = IPTINT(2, :, IPT)
      DO IS = 1, 4
        ! Get lon & lat of four corner points
        IX   = IXS(IS)
        IY   = IYS(IS)
        XLON = XGRD(IY, IX)
        YLAT = YGRD(IY, IX)
        ! Grid point status
        IF (MAPSTA(IY, IX) .EQ. 0) CYCLE
        ! Calc dist.
        IF (FLAGLL) THEN
          DIST(IS) = DIST_SPHERE(PLON, PLAT, XLON, YLAT)
        ELSE
          DIST(IS) = SQRT((PLON - XLON)**2. + (PLAT - YLAT)**2.)
        END IF
      END DO
      ! A sea point filter: there must be at least one sea grid point around
      ! the selected output location. [maybe not necessary since IOPP already
      ! checked this criterion]
      !
      IF (ALL(DIST < 0.)) CYCLE
      ! Find the nearest sea grid point
      JLOC = MINLOC(DIST, 1, DIST >= 0.)
      JX   = IXS(JLOC)
      JY   = IYS(JLOC)
      ISEA = MAPFS(JY, JX)
      ! Basic check
#ifdef W3_TS
      IF (FLAGLL) THEN
        IF (IAPROC .EQ. NAPOUT)                                    &
             WRITE(SCREEN, "(A, 2F10.3, A, 2F10.3, A)")                 &
             '✗ (PLON, PLAT): (', PLON, PLAT, ') | (XGRD, YGRD): (',&
             XGRD(JY, JX), YGRD(JY, JX), ')'
      ELSE
        IF (IAPROC .EQ. NAPOUT)                                    &
             WRITE(SCREEN, "(A, 2E10.3, A, 2E10.3, A)")                 &
             '✗ (PLON, PLAT): (', PLON, PLAT, ') | (XGRD, YGRD): (',&
             XGRD(JY, JX), YGRD(JY, JX), ')'
      END IF
#endif
      ! Store ISEA
      NSEL = NSEL + 1
      PSEA(NSEL) = ISEA
      PNMS(NSEL) = PTNME(IPT)
      ! Store Unit (Open & Write Binary files)
      IUNT = 500 + NSEL
      ! Binary
      !         OPEN(IUNT, FILE='NL5_'//trim(PNMS(NSEL))//'_src.bin',       &
      !              form='unformatted', convert=file_endian, ACCESS='stream', STATUS='replace', &
      !              ACTION='write')
      !         WRITE(IUNT) PLON, PLAT
      !         WRITE(IUNT) XGRD(JY, JX), YGRD(JY, JX)
      !         WRITE(IUNT) QR5DPT
      !         WRITE(IUNT) NK, NTH
      !         WRITE(IUNT) SIG(1:NK)/TPI  ! f, θ
      !         WRITE(IUNT) TH
      !         CLOSE(IUNT)
      ! Ascii
      OPEN(IUNT, FILE='NL5_'//trim(PNMS(NSEL))//'_src.dat',       &
           FORM='formatted', STATUS='replace', ACTION='write')
      WRITE(IUNT, '(2ES11.3)') PLON, PLAT
      WRITE(IUNT, '(ES11.3)' ) QR5DPT
      WRITE(IUNT, '(2I5)')     NK, NTH
      WRITE(IUNT, 113) SIG(1:NK)/TPI  ! f, θ
      WRITE(IUNT, 113) TH
      CLOSE(IUNT)
      !
    END DO
    ! Format
113 FORMAT ((10ES11.3))
    !
  END SUBROUTINE INPOUT
  !/ ------------------------------------------------------------------- /

  !>
  !> @brief Check if the 2D array `ARR2D` contains NaN.
  !>
  !> @param   NK
  !> @param   NTH
  !> @param   ARR2D
  !> @returns HasNaN
  !>
  !> @author Q. Liu
  !> @date   25-Apr-2019
  !>
  FUNCTION HasNaN(NK, NTH, ARR2D)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           Q. Liu                  |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         25-Apr-2019 |
    !/                  +-----------------------------------+
    !/
    !/    24-Apr-2019 : Origination.                        ( version 7.13 )
    !/                                                      ( Q. Liu       )
    !/
    !  1. Purpose :
    !     Check if the 2D array `ARR2D` contains NaN (see also w3gsrumd.ftn)
    !/
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN)  :: NK, NTH        ! # OF FREQ. & DIRC.
    REAL, INTENT(IN)     :: ARR2D(NK, NTH)
    LOGICAL              :: HasNaN
    !/
    HasNaN = .TRUE.
    !
    IF ( ALL(ARR2D .GE. -HUGE(ARR2D(1, 1))) .AND.  &
         ALL(ARR2D .LE.  HUGE(ARR2D(1, 1))) ) THEN
      HasNaN = .FALSE.
    END IF
    !
    RETURN
    !/
  END FUNCTION HasNaN
  !/ ------------------------------------------------------------------- /
  !/
  !/ End of module W3SNL5MD -------------------------------------------- /
  !/
END MODULE W3SNL5MD
!/ ------------------------------------------------------------------- /
