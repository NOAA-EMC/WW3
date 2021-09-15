#include "w3macros.h"
!/ ------------------------------------------------------------------- /
module w3gkemd
!/ ------------------------------------------------------------------- /
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |          Odin Gramstad            |
!/                  |          Qingxiang Liu            |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         03-Jun-2021 |
!/                  +-----------------------------------+
!/
!/    26-May-2014 : Origination.                        ( version 3.14 )
!/                  Intially Dr. O. Gramstad implemented his GKE method
!/                  in WW3 v3.14 which worked well for the single-grid
!/                  point duration-limited test.
!/
!/    09-Nov-2018 : Fully implemented in WW3            ( version 7.13 )
!/                                                      ( Q. Liu       )
!/    16-Apr-2019 : Add save attribute explicitly       ( version 7.13 )
!/                                                      ( Q. Liu       )
!/    18-Apr-2019 : Add the bilinear interp. option     ( version 7.13 )
!/                                                      ( Q. Liu       )
!/    08-Jul-2019 : Use kind=8 for qi_nnz               ( Q. Liu       )
!/    01-Apr-2020 : Boundary conditions                 ( Q. Liu       )
!/    03-Jun-2021 : Merge into the WW3 Github           ( version 7.12 )
!/                                                      ( Q. Liu       )
!/
!  1. Purpose:
!     Calculate the (resonant & quasi/near-resonant) four-wave nonlinear
!     interaction term S_{nl} according to the generalized kinetic
!     equation (GKE) developed in Gramstad and Stiassnie (2013).
!
!     References:
!     Gramstad and Stiassnie (2013), JFM, 818, 280-303 (hereafter GS13)
!     Gramstad and Babanin (2016),    OD,  66, 509-526 (hereafter GB16)
!     Liu et al. (2021),             JFM, 910, A50     (hereafter LGB21)
!     &
!     Annenkov and Shrira (2006),    JFM, 561, 181-207 (*)
!     Annenkov and Shrira (2015),    JPO,  45, 807-812
!     Annenkov and Shrira (2018),    JFM, 844, 766-795 (*)
!     &
!     Shrira and Annenkov (2013),    Book Ch., 239-281
!     Annenkov and Shrira (2016),    Book Ch., 159-178
!
!     (*) Note that equations therein contain typos.
!
!  2. Subroutines and functions :
!
!     [Part 1]: Kernel Function
!
!     Calculate the kernel function T_{0, 1, 2, 3} for the Zakharov
!     Equation.
!
!     References:
!     Krasitskii (1994), JFM, 272, 1 - 20 (hereafter K94)
!     Janssen    (2009), JFM, 637, 1 - 44 (hereafter J09)
!     Mei et al. (2005), ch. 14, 882 - 884
!
!     Based on my own observation, Odin has closely followed the
!     equations presented in the appendix (A.1/2) of J09.
!
!     ----------------------------------------------------------------
!      Name                Type  Scope    Description
!     ----------------------------------------------------------------
!      QFunc               Func. Private  q = ω^2 / g
!      VpFunc              Func. Private  V^{(+)}_{1, 2, 3}
!      VmFunc              Func. Private  V^{(-)}_{1, 2, 3}
!      UFunc               Func. Private  U_{1, 2, 3, 4}
!      TFunc               Func. Public   T_{1, 2, 3, 4}
!     ----------------------------------------------------------------
!
!     [Part 2]: Find Quartets (total number & configurations)
!
!     References:
!     Annenkov and Shrira (2015),       JPO, 45, 807-812
!     Hasselmann and Hasselmann (1981), Exact-NL/DIA report
!     Hasselmann and Hasselmann (1985), JPO, 15, 1369-1377
!
!     ----------------------------------------------------------------
!      Name                Type  Scope    Description
!     ----------------------------------------------------------------
!      FindQuartetNumber   Subr. Private  Total No. of Quartets
!      FindQuartetConfig   Subr. Private  Config.   of Quartets
!
!      [Part 3]: Sparse matrix (storage, operation)
!
!      References:
!      Saad (1994) SPARSKIT: a basic tool kit for sparse matrix
!                            compuation (version 2)
!
!     ----------------------------------------------------------------
!      Name                Type  Scope    Description
!     ----------------------------------------------------------------
!      CooCsrInd           Subr. Private  COO to CSR format
!      ASymSmatTimVec      Subr. Private  (A±A^T)∙X, where (A±A^T) is an
!                                         (anti)symmetric sparse matrix
!
!      [Part 4]: GKE Integral (main subrs.)
!
!      References:
!      Gramstad and Stiassnie (2013),    JFM, 818, 280-303 (hereafter GS13)
!      Gramstad and Babanin (2016),       OD,  66, 509-526 (hereafter GB16)
!      Liu et al. (2021),                JFM, 910, A50     (hereafter LGB21)
!      Janssen (2003),                   JPO,  33, 863-884 (hereafter J03)
!      Janssen (2009),                   JFM, 637,   1- 44 (hereafter J09)
!      Annenkov and Shrira (2013),       JFM, 726, 517-546
!
!      Hasselmann and Hasselmann (1981), Exact-NL/DIA report
!      Hasselmann and Hasselmann (1985), JPO, 15, 1369-1377
!      van Vledder (2006),               CE,  53, 223-242
!      Tolman (2013),                    OM,  70,  11- 24
!
!     ----------------------------------------------------------------
!      Name                Type  Scope    Description
!     ----------------------------------------------------------------
!      PrepKGrid           Subr. Private  (σ, θ) to (kx, ky)
!      PrepKernelIO        Subr. Private  Read/Write Quartet Cfg file
!      BiInterpWT          Subr. Private  Calc. interp. weights
!      CalcQRSNL           Subr. Public   GKE Transfer Integral
!
!  3. Future work (TODO)
!   * The current version only works for a constant-depth application (
!     either deep or finite deep). Extension of this module to be
!     applicable to varying-depth cases may be pursued in the future.
!
!   * Dnl   -- diagonal term
!   * βnpqr -- nonlinear stokes correction
!/
!/ ------------------------------------------------------------------- /
!
! Public parameters
!
! * `qi_` denotes the variable is integer number
! * `qr_` ...                     real    ...
! * `qs_` ...                     string
! * `ql_` ...                     logical
! * `qc_` ...                     complex
!/
    implicit none
!/ ------------------------------------------------------------------- /
    public  :: PrepKernelIO, CalcQRSNL
!
    public  :: qr_depth, qr_oml, qi_disc, qi_kev, qi_nnz, qi_interp

    private :: QFunc, VpFunc, VmFunc, UFunc, TFunc,           &
               FindQuartetNumber, FindQuartetConfig,          &
               CooCsrInd, ASymSmatTimVec,                     &
               PrepKGrid, BiInterpWT
!
    private :: qs_ver, qi_lrb, qr_eps, qr_grav,               &
               qr_pi, qr_tpi, qr_dmax, qc_iu, qs_cfg,         &
               qr_kx, qr_ky, qr_dk, qr_om, qi_nrsm,           &
               qi_NN, qi_PP, qi_QQ, qi_RR,                    &
               qr_k4x, qr_k4y, qr_om4, qr_dom,                &
               qr_TKern, qr_TKurt,                            &
               qi_icCos, qi_irCsr, qr_sumQR, qr_sumNP,        &
               qi_bind, qr_bwgh, qr_wn1
!
    private :: qi_bound, qr_fpow, qr_bdry
!
!/ ------------------------------------------------------------------- /
    real                       :: qr_depth                    ! Real water depth d (m)

    real                       :: qr_oml                      ! λ cut off factor
                                                              ! λ ≤ 0 →  quartets far
                                                              ! from resonance will not
                                                              ! be excluded.

    integer                    :: qi_disc                     ! Discretization of GKE
                                                              ! 0: continuous (like Exact-NL, WRT)
                                                              ! 1: discrete   (see GS13)

    integer                    :: qi_kev                      ! Version of KE
                                                              ! 0: GKE
                                                              ! 1: KE from J03

    integer                    :: qi_interp                   ! Interp. option
                                                              ! 0: Nearest bin
                                                              ! 1: Bilinear Interp

    integer, parameter         :: qi_bound= 1                 ! Boundary conditions
                                                              ! 0: no bound
                                                              ! 1: tail extension

    real, parameter            :: qr_fpow= -5.                ! E(f) tail power law
!
    character(len=50), parameter                              &
                               :: qs_ver  = 'gkev0'           ! version number/str
    integer, parameter         :: qi_lrb  = 4                 ! 4 bytes
    real, parameter            :: qr_eps  = epsilon(100.0)    ! Smallest positive
                                                              ! value supported by the
                                                              ! compiler (e.g., gfortran
                                                              ! → 1.19E-7)

    real, parameter            :: qr_grav = 9.806             ! Gravational acc (m/s^2)
    real, parameter            :: qr_pi   = 3.141592653589793 ! π
    real, parameter            :: qr_tpi  = 2 * qr_pi         ! π * 2
    real, parameter            :: qr_dmax = 3000.0            ! Maximum allowed water
    complex, parameter         :: qc_iu   = (0.0, 1.0)        ! complex unit `i`
!
    character(len=100)         :: qs_cfg                      ! File name for quartet/kernel
!
    real, allocatable, save    :: qr_kx(:), qr_ky(:),   &     ! kx, ky (2D grid → 1D vector)
                                  qr_dk(:), qr_om(:)          ! Δ\vec{k}, ω,
!
    integer(kind=8)            :: qi_nnz                      ! # of quartets
    integer                    :: qi_nrsm                     ! # of rows of SMat
    integer, allocatable, save :: qi_NN(:), qi_PP(:),   &     ! Index for Quartets
                                  qi_QQ(:), qi_RR(:)
    real, allocatable, save    :: qr_k4x(:), qr_k4y(:), &     ! kx, ky, ω for 4th wave
                                  qr_om4(:)
    real, allocatable, save    :: qr_dom(:),            &     ! Δω
                                  qr_TKern(:),          &     ! Kernel `T`
                                  qr_TKurt(:)                 ! Kurtosis `T`
    integer, allocatable, save :: qi_icCos(:),          &     ! col index of CooCsr
                                  qi_irCsr(:)                 ! row begining index of
                                                              ! Csr sparse matrix
    real, allocatable, save    :: qr_sumQR(:),          &     ! Σ over Q, R
                                  qr_sumNP(:, :)              ! Σ over P
!
    integer, allocatable, save :: qi_bind(:, :)               ! Bilinear interp. (index and
    real, allocatable, save    :: qr_bwgh(:, :)               ! weight)
    real, allocatable, save    :: qr_bdry(:)                  ! Boundary weight
    real, allocatable, save    :: qr_wn1(:)                   ! wavenumber k(nk)
!/
!/ ------------------------------------------------------------------- /
    contains
!/ ------------------------------------------------------------------- /
!/ [Part 1]
!/
    function QFunc(kx, ky)
!/
!/    19-Dec-2011 : Origination.                        ( O. Gramstad )
!/
!/    09-Nov-2018 : Prepare WW3 distribution            ( Q. Liu      )
!/
!  1. Purpose: Define q = ω^2 / g (i.e., q in K94 & J09)
!
!  2. Method:
!     For wind-generated ocean surface waves, the dispersion relation
!     reads
!         ω^2 = g k tanh(kd),
!     where g is the gravtional acceleration, ω is the radian frequency,
!     d is the water depth. Hence,
!
!             / k = √(k_x**2. + k_y**2.) for deep-water
!         q = |
!             \ k tanh(kd)               for finite-deep water (e.g.,
!                                            d < 2500.)
!
!/
        implicit none
!
        real, intent(in) :: kx, ky   ! x, y components of wavenumber
                                     ! vector (kx, ky)
        real             :: QFunc    ! Returned function
!/
        QFunc = sqrt(kx*kx + ky*ky)  ! deep-water case (q = k)
!
! Odin used qr_dmax = 2500.
!
        if (qr_depth > qr_eps .and. qr_depth < qr_dmax) then
            QFunc = QFunc * tanh(QFunc * qr_depth)  ! finite-deep
        end if
!
        return
!/
    end function QFunc
!/
!/ ------------------------------------------------------------------- /
!/
    function VpFunc(k0x, k0y, k1x, k1y, k2x, k2y)
!/
!/    19-Dec-2011: Origination.                        ( O. Gramstad )
!/
!/    09-Nov-2018 : Prepare WW3 distribution            ( Q. Liu      )
!/
!/
!  1. Purpose:
!     Calculate the second-order coefficient V^{(+)}_{1, 2, 3} of J09,
!     which corresponds to U^{(3)}_{0, 1, 2} of K94.
!
!     ◆ V^{(+)}_{1, 2, 3} differs from U^{(3)}_{0, 1, 2} by a factor
!       of 1/2π --- this is because the wave spectrum F(k) used in K94
!       and J09 differ by a fator of (1/2π)^2.
!
!/
        implicit none
!
        real, intent(in) :: k0x, k0y, k1x, k1y, k2x, k2y ! 3 waves
        real             :: VpFunc                       ! V^{(+)}_{1, 2, 3}
!
        real             :: q0, q1, q2                   ! q for 3 waves
!/
! Call Q function here
        q0 = QFunc(k0x, k0y)
        q1 = QFunc(k1x, k1y)
        q2 = QFunc(k2x, k2y)
!
! Odin has ignored √g here because it will be absorbed/vanish when we
! calculate the kernel function T. I, however, included √g here for
! clarity.
! V^{(+)}_{1, 2, 3}
!
        VpFunc = sqrt(1.0/32.0) * (                                       &
            (k0x*k1x + k0y*k1y + q0*q1) * sqrt(sqrt(qr_grav*q2 / (q0*q1)))&
          + (k0x*k2x + k0y*k2y + q0*q2) * sqrt(sqrt(qr_grav*q1 / (q0*q2)))&
          + (k1x*k2x + k1y*k2y + q1*q2) * sqrt(sqrt(qr_grav*q0 / (q1*q2))))
!
        return
!/
    end function VpFunc
!/
!/ ------------------------------------------------------------------- /
!/
    function VmFunc(k0x, k0y, k1x, k1y, k2x, k2y)
!/
!/    19-Dec-2011 : Origination.                        ( O. Gramstad )
!/
!/    09-Nov-2018 : Prepare WW3 distribution            ( Q. Liu      )
!/
!  1. Purpose:
!     Calculate the second-order coefficient V^{(-)}_{1, 2, 3} of J09,
!     which corresponds to U^{(1)}_{0, 1, 2} of K94.
!
!     ◆ V^{(-)}_{1, 2, 3} differs from U^{(1)}_{0, 1, 2} by a factor
!       of 1/2π
!
!/
        implicit none
!
        real, intent(in) :: k0x, k0y, k1x, k1y, k2x, k2y ! 3 waves
        real             :: VmFunc                       ! V^{(-)}_{1, 2, 3}
!
        real             :: q0, q1, q2                   ! q for 3 waves
!/
! Call Q function here
        q0 = QFunc(k0x, k0y)
        q1 = QFunc(k1x, k1y)
        q2 = QFunc(k2x, k2y)
!
! V^{(-)}_{1, 2, 3}
!
        VmFunc = sqrt(1.0/32.0) * (                                       &
            (k0x*k1x + k0y*k1y - q0*q1) * sqrt(sqrt(qr_grav*q2 / (q0*q1)))&
          + (k0x*k2x + k0y*k2y - q0*q2) * sqrt(sqrt(qr_grav*q1 / (q0*q2)))&
          + (k1x*k2x + k1y*k2y + q1*q2) * sqrt(sqrt(qr_grav*q0 / (q1*q2))))
!
        return
!/
    end function VmFunc
!/
!/ ------------------------------------------------------------------- /
!/
    function UFunc(k0x, k0y, k1x, k1y, k2x, k2y, k3x, k3y)
!/
!/    19-Dec-2011 : Origination.                        ( O. Gramstad )
!/
!/    09-Nov-2018 : Prepare WW3 distribution            ( Q. Liu      )
!/
!  1. Purpose:
!     Calculate the intermediate quantity (i.e., U_{1, 2, 3, 4} in J09,
!     V_{0, 1, 2, 3} in K94) for the third-order coefficient (i.e.,
!     W^{(2)}_{1, 2, 3, 4} in J09, V^{(2)}_{0, 1, 2, 3} in K94).
!
!     ◆ U_{1, 2, 3, 4} differs from V_{0, 1, 2, 3} by a factor of
!       (1/2π)^2.
!
!/
        implicit none
!
        real, intent(in) :: k0x, k0y, k1x, k1y,       &
                            k2x, k2y, k3x, k3y        ! 4 waves
        real             :: UFunc                     ! U_{1, 2, 3, 4}
!
        real             :: q0, q1, q2, q3            ! q for 4 waves
!/
! Call Q function here
        q0 = QFunc(k0x, k0y)
        q1 = QFunc(k1x, k1y)
        q2 = QFunc(k2x, k2y)
        q3 = QFunc(k3x, k3y)
!
! U_{1, 2, 3, 4}
!
        UFunc = (1.0/16.0) * sqrt(sqrt(q2*q3 / (q0*q1))) *              &
             (2.0*((k0x*k0x + k0y*k0y) * q1 + (k1x*k1x + k1y*k1y) * q0)-&
              q0*q1*( QFunc(k0x+k2x, k0y+k2y) + QFunc(k1x+k2x, k1y+k2y)+&
                      QFunc(k0x+k3x, k0y+k3y) + QFunc(k1x+k3x, k1y+k3y) ))
!
        return
!/
    end function UFunc
!/
!/ ------------------------------------------------------------------- /
!/
    function TFunc(k0x, k0y, k1x, k1y, k2x, k2y, k3x, k3y)
!/
!/    19-Dec-2011 : Origination.                        ( O. Gramstad )
!/
!/    09-Nov-2018 : Prepare WW3 distribution            ( Q. Liu      )
!/
!  1. Purpose:
!     Calculate the Kernel function for the four-wave interaction, i.e.,
!     (T_{1, 2, 3, 4}, \widetilde{V}^{(2)}_{0, 1, 2, 3} in K94).
!     ◆ T from J09 and K94 differ by a factor of (1/2π)^2.
!
!     Odin's comment:
!     Kernel function for all combination that are not Stokes correction.
!     I.e. n0 != n2 and n0 != n3
!/
        implicit none
!
        real, intent(in) :: k0x, k0y, k1x, k1y, &
                            k2x, k2y, k3x, k3y        ! 4 waves
        real             :: TFunc                     ! T_{1, 2, 3, 4}
!
! Virtual-state interaction: two free waves generate a virtual state
! consisting of bound waves, which then decays into a different set of
! free waves (see J09)
!
        real             :: om0, om1, om2, om3, &     ! ω for 4 waves
                            om02,               &     ! ω_{0-2}
                            om13,               &     ! ω_{1-3}
                            om12,               &     ! ω_{1-2}
                            om03,               &     ! ω_{0-3}
                            om0p1,              &     ! ω_{0+1}
                            om2p3                     ! ω_{2+3}
!
        real             :: L14, L23, L56,      &
                            W                         ! W^{(2)}_{1, 2, 3, 4} in J09
                                                      ! or
                                                      ! V^{(2)}_{0, 1, 2, 3} in K94
!/
! Initilization
        om0p1 = 0.
        om2p3 = 0.
        W     = 0.
        L14   = 0.
        L23   = 0.
        L56   = 0.
!
! Get ω from q: q = ω^2 / g →  ω = √(qg)
! Odin has ignored √g here because it will be absorbed/vanish when we
! calculate the kernel function T (V / ω). I, however, included √g here for
! clarity.
!
! ω for four free waves
        om0 = sqrt(qr_grav * QFunc(k0x, k0y))
        om1 = sqrt(qr_grav * QFunc(k1x, k1y))
        om2 = sqrt(qr_grav * QFunc(k2x, k2y))
        om3 = sqrt(qr_grav * QFunc(k3x, k3y))
!
! ω for other combined waves
!
        om02 = sqrt(qr_grav * QFunc(k0x-k2x, k0y-k2y))
        om13 = sqrt(qr_grav * QFunc(k1x-k3x, k1y-k3y))
        om12 = sqrt(qr_grav * QFunc(k1x-k2x, k1y-k2y))
        om03 = sqrt(qr_grav * QFunc(k0x-k3x, k0y-k3y))
!
        if (abs(k0x+k1x) > qr_eps .or. abs(k0y+k1y) > qr_eps) then
!           k₀ + k₁ = k₂ + k₃ = 0., ω_{0+1} = 0.,
!           V^{(-)}_{0+1, 0, 1} ~ 1/ω_{0+1} = NaN, L56 = NaN
            om0p1 = sqrt(qr_grav * QFunc(k0x+k1x, k0y+k1y))
            om2p3 = sqrt(qr_grav * QFunc(k2x+k3x, k2y+k3y))
         end if
!
! W^{(2)}_{1, 2, 3, 4} [Call U function here] for direct interaction
!
        W = UFunc(-k0x, -k0y, -k1x, -k1y,  k2x,  k2y,  k3x,  k3y) +     &
            UFunc( k2x,  k2y,  k3x,  k3y, -k0x, -k0y, -k1x, -k1y) -     &
            UFunc( k2x,  k2y, -k1x, -k1y, -k0x, -k0y,  k3x,  k3y) -     &
            UFunc(-k0x, -k0y,  k2x,  k2y, -k1x, -k1y,  k3x,  k3y) -     &
            UFunc(-k0x, -k0y,  k3x,  k3y,  k2x,  k2y, -k1x, -k1y) -     &
            UFunc( k3x,  k3y, -k1x, -k1y,  k2x,  k2y, -k0x, -k0y)
!
! First & Fourth lines for virtual-state interaction in J09
!
        L14 = VmFunc(k0x, k0y, k2x, k2y, k0x-k2x, k0y-k2y) *            &
              VmFunc(k3x, k3y, k1x, k1y, k3x-k1x, k3y-k1y) *            &
              (1.0/(om2 + om02 - om0) + 1.0/(om1 + om13 - om3)) +       &
              VmFunc(k1x, k1y, k3x, k3y, k1x-k3x, k1y-k3y) *            &
              VmFunc(k2x, k2y, k0x, k0y, k2x-k0x, k2y-k0y) *            &
              (1.0/(om3 + om13 - om1) + 1.0/(om0 + om02 - om2))
!
! Second & Third lines for virtual-state interaction in J09
!
        L23 = VmFunc(k1x, k1y, k2x, k2y, k1x-k2x, k1y-k2y) *            &
              VmFunc(k3x, k3y, k0x, k0y, k3x-k0x, k3y-k0y) *            &
              (1.0/(om2 + om12 - om1) + 1.0/(om0 + om03 - om3)) +       &
              VmFunc(k0x, k0y, k3x, k3y, k0x-k3x, k0y-k3y) *            &
              VmFunc(k2x, k2y, k1x, k1y, k2x-k1x, k2y-k1y) *            &
              (1.0/(om3 + om03 - om0) + 1.0/(om1 + om12 - om2))
!
! Fifth & Sixth lines for virtual-state interaction in J09
!
         if (abs(k0x+k1x) > qr_eps .or. abs(k0y+k1y) > qr_eps) then
!           k₁ + k₂ = k₃ + k₄ = 0., ω_{1+2} = 0.,
!           V^{(-)}_{1+2, 1, 2} ~ 1/ω_{1+2} = NaN, L56 = NaN
            L56 = VmFunc(k0x+k1x, k0y+k1y, k0x, k0y, k1x, k1y) *        &
                  VmFunc(k2x+k3x, k2y+k3y, k2x, k2y, k3x, k3y) *        &
                  (1.0/(om0p1 - om0 - om1) + 1.0/(om2p3 - om2 - om3)) + &
                  VpFunc(-k0x-k1x, -k0y-k1y, k0x, k0y, k1x, k1y) *      &
                  VpFunc(-k2x-k3x, -k2y-k3y, k2x, k2y, k3x, k3y) *      &
                  (1.0/(om0p1 + om0 + om1) + 1.0/(om2p3 + om2 + om3))
         end if
!
! T_{1, 2, 3, 4}
!
        TFunc =  W - L14 - L23 - L56
!
        return
!/
    end function TFunc
!/
!/ ------------------------------------------------------------------- /
!/ [Part 2]
!/
    subroutine FindQuartetNumber(ns, kx, ky, om, oml, nnz)
!/
!/    19-Dec-2011 : Origination.                        ( O. Gramstad )
!/
!/    09-Nov-2018 : Prepare WW3 distribution            ( Q. Liu      )
!/    02-Apr-2020 : Boundary conditions (< kmin, > kmax)( Q. Liu      )
!/
!  1. Purpose:
!     Find the total number of quartets (resonant and quasi/near-resonant
!     four waves) satisfying the criteria below:
!
!     1) \vec{k₁} + \vec{k₂} = \vec{k₃} + \vec{k₄}
!
!     2) Δω = |ω₁ + ω₂ - ω₃ - ω₄| <= λc \min(ω₁, ω₂, ω₃, ω₄)
!        - that is, quartets far from the resonance is excluded for
!          saving the computational cost.
!
!     3) For a given 2D frequency-direction grid (k_i, θ_j, i = 1, ...,
!        NK, j = 1, ..., NTH) consisting of NS points (NS = NK * NTH),
!        we will first reshape the 2D spectral grid (k_i, θ_j)
!        into a 1D wavenumber vector (k_{xl}, k_{yl}, l = 1, ..., NS).
!        Afterwards, we should have
!        3.a) l₂ >= l₁
!        3.b) l₃ ≠  l₁, l₃ ≠ l₂ (otherwise the third and fourth wave
!                                components will be the same as the
!                                first and second)
!        3.c) l₄ >= l₃
!        3.d) l₄ ≠  l₁, l₄ ≠ l₂
!        3.e) k₄ >= k_{min}, k₄ <= k_{max}
!
!        Note that `l` here only denotes the index of a specific wave
!        component inside the 1D wavenumber vector array. For k₄, its
!        index l₄ is not exact, and is just approximated by the index
!        of its closest wave component.
!
!     4) If we store the located quartets in a 2D large sparse matrix,
!        which can be organized as
!               |K K K |
!               |∩ ∩ ∩ |
!               |3 q l₃| 1 2 . . . N 1 2 . . . N . . . . . . 1 2 . . . N
!               |4 r l₄| 1 1 1 1 1 1 2 2 2 2 2 2 . . . . . . N N N N N N
!               |∪ ∪ ∪ |
!        -------
!        K {1 2}
!        K {n p}
!        K {l₁l₂}
!        -------
!           1 1
!           2 1                                  (2,1,N,3) ✗⁴    ✗²(2,1,3,N)
!           . 1                           col > row  → ▲
!           . 1
!           . 1
!           N 1
!           1 2                                  (1,2,N,3) ✗³   [★ (1,2,3,N)]
!           2 2
!           . 2
!           . 2                        ⊚ ← row = l₁ + (l₂ - 1) * NS
!           . 2                        ↑
!           N 2                        col = l₃ + (l₄ - 1) * NS
!           . .
!           . .
!           . .
!           . .
!           . .
!           . .  (N,3,2,1) ✓⁴        ✓³(N,3,1,2)
!           1 N              ▼ ← col < row
!           2 N
!           . N  (3,N,2,1) ✓²        ☆ (3,N,1,2)
!           . N
!           . N
!           N N
!
!        where `N` shown above denotes `NS`, not `NK`, therefore the shape
!        of this large sparse matrix is (NS*NS, NS*NS).
!
!        Only quartets with col > row (highlighted by ▲ , i.e.,
!                           ---------
!        elements in the upper trianglar matrix) are selected because,
!        for example, ★ (1, 2, 3, NS) & ☆ (3, NS, 1, 2) essentially
!        refer to the same quartet.
!
!     To sum up, criteria 1) and 2) are kinetic, whereas 3) and 4) are
!     enforced to avoid duplicating quartets since
!     ★  (k₁, k₂, k₃, k₄)
!
!     & [symmetric]
!     ✗² (k₂, k₁, k₃, k₄) ← filterd by 3.a)
!     ✗³ (k₁, k₂, k₄, k₃) ← filterd by 3.c)
!     ✗⁴ (k₂, k₁, k₄, k₃) ← filterd by 3.a) and 3.c)
!
!     & [antisymmetric]
!     ☆  (k₃, k₄, k₁, k₂) ← filterd by 4)
!     ✓² (k₃, k₄, k₂, k₁)
!     ✓³ (k₄, k₃, k₁, k₂)
!     ✓⁴ (k₄, k₃, k₂, k₁)
!
!     are essentially the same.
!
!     ◆ criteria 3.b) and 3.d) exclude two quartets:
!        / k₁ = k₃, k₂ = k₄ (k₁, k₂, k₁, k₂)
!        \ k₁ = k₄, k₂ = k₃ (k₁, k₂, k₂, k₁)
!        →  singular points for the nonlinear transfer integral as
!           T_{1, 2, 1, 2} or T_{1, 2, 2, 1} ~ 1 / 0 = NaN
!
!       van Vledder (2006, p. 231) argued that the first quadruplet had
!       negligible contribution to the total transfer rate. Similarly,
!       for the symmetric reason, the contribution from the second
!       quadruplet is also very limited.
!
!     ◆ We should keep in mind that the Snl term for wave component
!       3 in ★ (i.e., k₃) and in ☆ (i.e., k₁) are the same.
!
!     ◆ Although the other 7 quartets are not counted here, their
!       contributions to the nonlinear transfer rates should not be
!       ignored as the interval of the 6D integration starts from
!       -∞ and ends at +∞ !
!
!     More details can be found in Appendix of LGB21.
!
!     See also references:
!     Hasselmann and Hasselmann (1981)  Exact-NL/DIA report
!     Hasselmann and Hasselmann (1985), JPO, 15, 1369 - 1377.
!     Annenkov and Shrira (2015),       JPO, 45, 807-812
!
!/
        implicit none
!
        integer, intent(in)  :: ns           ! length of 1D wavenumber
                                             ! vector, ns = nk * nth
        real, intent(in)     :: kx(ns),   &
                                ky(ns),   &  ! (kx, ky) components
                                om(ns)       ! ω or σ
        real, intent(in)     :: oml          ! cut-off value λc for the
                                             ! quasi-resonant criterion 2)
!
        integer(kind=8), intent(out)      &
                             :: nnz          ! total number of quartets
                                             ! i.e., nonzero values
                                             ! in the large-sparse matrix
                                             ! illustrated above
!
! Local parameters
        real                 :: k(ns)        ! scalar/mag k
        integer              :: i1, i2, i3, i4, row, col
        real                 :: k4x, k4y, k4, om4, kmin, kmax, dom
!/
! Scalar wavenumber (i.e., magnitude)
        k    = sqrt(kx*kx + ky*ky)
        kmin = minval(k)
        kmax = maxval(k)
!
! Boundary conditions: include k4 beyond kmin & kmax
        if (qi_interp .eq. 1 .and. qi_bound .eq. 1) then
            kmin = kmin / 9.  ! 1/3 fmax
            kmax = kmax * 9.  ! 3   fmax
        end if
!
! Start to find the quartets: \vec{k_j}, j = 1, 2, 3 are chosen at the
! grid points, and \vec_{k_4} is found by
!     \vec{k_4} = \vec{k_1} + \vec{k_2} - \vec{k_3}
!
        nnz = 0
!
        do i1 = 1, ns
!           criterion 3.a) ← starting from i1
            do i2 = i1, ns
                do i3 = 1, ns
!                   criterion 3.b)
                    if (i3 .ne. i1 .and. i3 .ne. i2) then
!                       criterion 1)
                        k4x = kx(i1) + kx(i2) - kx(i3)
                        k4y = ky(i1) + ky(i2) - ky(i3)
                        k4  = sqrt(k4x*k4x + k4y*k4y)
!
! wavenumber k4 falls outside the grid (criterion 3.e)
                        if (k4 >= kmin .and. k4 <= kmax) then
!                           ω = √(qg) & Δω
                            om4 = sqrt(qr_grav * QFunc(k4x, k4y))
                            dom = abs(om(i1) + om(i2) - om(i3) - om4) / &
                                  min(om(i1), om(i2), om(i3), om4)
!                           criterion 2)
                            if (oml <= qr_eps .or. dom <= oml) then
                                i4 = minloc((kx - k4x)*(kx - k4x) +     &
                                            (ky - k4y)*(ky - k4y), 1)
!                               criterion 3.d)
                                if (i4 .ne. i1 .and. i4 .ne. i2) then
!                                   criterion 3.c)
                                    if (i4 >= i3) then
                                        row = i1 + ns * (i2-1)
                                        col = i3 + ns * (i4-1)
!                                       criterion 4)
                                        if (col > row) then
                                            nnz = nnz + 1
                                        end if
                                    end if
                                end if
                            end if
                        end if
                    end if
                end do
            end do
#ifdef W3_TS
        write(*, *) '→ nnz = ', nnz
#endif
        end do
!/
    end subroutine FindQuartetNumber
!/
!/ ------------------------------------------------------------------- /
!/
    subroutine FindQuartetConfig(ns, kx, ky, om, oml, nnz,     &
                                 NN, PP, QQ, RR,               &
                                 k4x, k4y, om4)
!/
!/    19-Dec-2011 : Origination.                        ( O. Gramstad )
!/
!/    09-Nov-2018 : Prepare WW3 distribution            ( Q. Liu      )
!/    02-Apr-2020 : Boundary conditions (< kmin, > kmax)( Q. Liu      )
!/
!  1. Purpose:
!     Find all the quartets that we are interested in. Initially I thought
!     we may merge this subroutine and the subroutine above (i.e.,
!     FindQuartetNumber) in such a way that we first initialize a large
!     array like Quartet(HNum), where HNum is a huge integer (something
!     like 0.5*NS**4). But it quickly turned out this was a very naive
!     idea because for the wavenumber grid (k, θ) used by 3G spectral
!     wave models, in general NS~O(10^2-3), then NS^4~O(10^8-12). Thus,
!     HNum becomes really very very very huge, and then we may have
!     the integer/memory overflow problem.
!
!     Based on the above-mentioned, we must split the whole process:
!     1) find the total number of quartets with FindQuartetNumber, `nnz`
!     2) allocate arrays with the known `nnz`, and store the wavenumber
!        and ω for k₄
!
!     For more details, see the header of the subr. FindQuartetNumber.
!
!/
        implicit none
!
        integer, intent(in)  :: ns           ! length of 1D wavenumber
                                             ! vector, ns = nk * nth
        real, intent(in)     :: kx(ns),   &
                                ky(ns),   &  ! (kx, ky) components
                                om(ns)       ! ω or σ
        real, intent(in)     :: oml          ! cut-off value λc for the
                                             ! quasi-resonant criterion 2)
        integer(kind=8), intent(in)       &
                             :: nnz          ! total number of quartets
                                             ! returned from the subr.
                                             ! FindQuartetNumber
!
        integer, intent(out) :: NN(nnz),  &  ! index of k₁
                                PP(nnz),  &  !          k₂
                                QQ(nnz),  &  !          k₃
                                RR(nnz)      !          k₄ in the 1D
                                             ! wavenumber vector [1 - NS]
        real, intent(out)    :: k4x(nnz), &
                                k4y(nnz), &  ! x, y comp. of k₄
                                om4(nnz)     ! ω₄
!
! Local parameters
        real                 :: k(ns)        ! scalar/mag k
        integer              :: i1, i2, i3, i4, row, col, s
        real                 :: k4xT, k4yT, k4T, om4T, kmin, kmax, dom
!/
! Scalar wavenumber (i.e., magnitude)
        k    = sqrt(kx*kx + ky*ky)
        kmin = minval(k)
        kmax = maxval(k)
!
! Boundary conditions: include k4 beyond kmin & kmax
        if (qi_interp .eq. 1 .and. qi_bound .eq. 1) then
            kmin = kmin / 9.  ! 1/3 fmax
            kmax = kmax * 9.  ! 3   fmax
        end if
!
! Start to find the quartets: \vec{k_j}, j = 1, 2, 3 are chosen at the
! grid points, and \vec_{k_4} is found by
!     \vec{k_4} = \vec{k_1} + \vec{k_2} - \vec{k_3}
!
! s: count of quartets. This time the total number of quartets `nnz` is
! already known from `FindQuartetNumber`.
!       nnz = 0
        s = 0
!
        do i1 = 1, ns
!           criterion 3.a) ← starting from i1
            do i2 = i1, ns
                do i3 = 1, ns
!                   criterion 3.b)
                    if (i3 .ne. i1 .and. i3 .ne. i2) then
!                       criterion 1)
                        k4xT = kx(i1) + kx(i2) - kx(i3)
                        k4yT = ky(i1) + ky(i2) - ky(i3)
                        k4T  = sqrt(k4xT*k4xT + k4yT*k4yT)
!
! wavenumber k4 falls outside the grid (criterion 3.e)
                        if (k4T >= kmin .and. k4T <= kmax) then
!                           ω = √qg & Δω
                            om4T = sqrt(qr_grav * QFunc(k4xT, k4yT))
                            dom  = abs(om(i1) + om(i2) - om(i3) - om4T)/&
                                   min(om(i1), om(i2), om(i3), om4T)
!                           criterion 2)
                            if (oml <= qr_eps .or. dom <= oml) then
                                i4 = minloc((kx - k4xT)*(kx - k4xT) +   &
                                            (ky - k4yT)*(ky - k4yT), 1)
!                               criterion 3.d)
                                if (i4 .ne. i1 .and. i4 .ne. i2) then
!                                   criterion 3.c)
                                    if (i4 >= i3) then
                                        row = i1 + ns * (i2-1)
                                        col = i3 + ns * (i4-1)
!                                       criterion 4)
                                        if (col > row) then
!                                           nnz    = nnz + 1
                                            s      = s + 1 ! Find 1 quartet
!
                                            NN(s)  = i1    ! Store index
                                            PP(s)  = i2
                                            QQ(s)  = i3
                                            RR(s)  = i4
!
                                            k4x(s) = k4xT  ! k₄, ω₄
                                            k4y(s) = k4yT
                                            om4(s) = om4T
!
                                        end if
                                    end if
                                end if
                            end if
                        end if
                    end if
                end do
            end do
        end do
!
! Check consistency of s and nnz
        if (s .ne.  nnz) then
            write(*, 1001) 'FindQuartetConfig'
            call exit(1)
        end if
!
! Formats
 1001 FORMAT(/' *** GKE ERROR IN gkeModule : '/  &
              '     Subr. ', A, ': The number of Quartet Configs. does not match NNZ!'/)
!/
    end subroutine FindQuartetConfig
!/
!/ ------------------------------------------------------------------- /
!/ [Part 3]
!/
    subroutine CooCsrInd (nrow, nnz, ir, jc, ind_translate, iao)
!/
!/    12-Sep-2012 : Origination.                        ( version 3.14 )
!/                  Based on coocsr of SPARKIT          ( O. Gramstad  )
!/
!/    16-Nov-2018 : Prepare WW3 distribution            ( Q. Liu      )
!/
!  1. Purpose:
!     It becomes clear from subr. FindQuartetNumber & FindQuartetConfig
!     that we are faced with a problem of large sparse matrice when we
!     manipulate the huge set of quartets. By sparse matrix we mean
!     only a `relatively small number` of its matrix elements are nonzero.
!
!     For saving time or memory space, a sparse matrix is usually stored
!     in some compressed formats in the computer memory. Two among those
!     formats, COO & CSR are relevant here in our application:
!     1) The coordinate format (COO) --- the simplest storage scheme
!        For a given sparse matrix `A` (N, N) with NNZ nonzero elements,
!        the COO format consists of 3 arrays:
!        * a (nnz): real nonzero values of A in `any order`
!        * ir(nnz): row indices of these nonzero values
!        * jc(nnz): column indices
!
!     2) The Compressed Sparse Row format (CSR)
!        The CSR format is the basic format used in SPARSKIT, consisting
!        of three arrays as well
!        * a (nnz): real nonzero values of A stored row by row from row
!                   1 to row N
!        * jc(nnz): column indices in `any order`
!        * ia(N+1): the index of the first nonzero element at this
!                   corresponding row in the array a and jc, that is
!                   ia(i) provides the position in a & jc where the i-th
!                   row starts.
!
!     This subroutine converts the sparse matrix (nrow, nrow) in the COO
!     format, as represented by (ir, jc) to the CSR format, as represented
!     by (ind_translate, iao).
!
!     N.B.:
!     This subr. neither needs the real value array in the COO format,
!     nor returns the real value array in the CSR format. Alternatively,
!     it returns the tranformed index (ind_translate) from COO to CSR.
!     With such indices, we have
!     *) a_csr  = a_coo(ind_translate)
!     *) jc_csr = jc_coo(ind_translate)
!
!     References:
!     Youcef Saad, 1994, SPARSKIT: a basic tool kit for sparse matrix
!         compuation (version 2, `coocsr` therein)
!     See also Numerical Recipe in Fortran (ch. 2.7, p. 71)
!/
        implicit none
!
        integer, intent(in)  :: nrow               ! # of rows of sparse matrix
        integer(kind=8), intent(in)     &
                             :: nnz                ! # of nonzero elements
        integer, intent(in)  :: ir(nnz)            ! COO row
        integer, intent(in)  :: jc(nnz)            ! COO col
        integer, intent(out) :: ind_translate(nnz) ! indices from COO to CSR
        integer, intent(out) :: iao(nrow+1)        ! CSR iao
!
! Local parameters
        integer              :: i, j, k, k0, iad
!/
! Determine the number of non-zeros in each row (iao(i), i = 1, ..., nrow,
! will be the # of nonzero elements at the i-th row), whereas
! iao(nrow+1) = 0
        iao(1:nrow+1) = 0
        do k = 1, nnz
           iao(ir(k)) = iao(ir(k)) + 1 ! row by row
        end do
!
! Find the positions that correspond to the first value in each row.
! Now iao(i) is the position where the i-th row starts, and
! iao(nrow+1) = 1 + nnz
        k = 1
        do j = 1, nrow+1
           k0     = iao(j)  ! num_i, # of nonzero in this row
           iao(j) = k       ! starting pos
           k      = k + k0  ! k = Σnum_i, where i <= j
        end do
!
! Go through the structure once more. Fill in ind_translate
        do k = 1, nnz
           i = ir(k)        ! coo row
           j = jc(k)        ! coo col
!
! When i-th row is encountered by the first time, iad = iao(i) denotes
! the starting position for this row. Afterwards, iao(i) is added by 1
! when i-th row arrives every time. In the end, iao(i) records the
! starting position for the (i+1)-th row. However, the last element of
! iao remains unchanged, i.e., iao(nrow+1) = iao(nrow) = 1 + nnz
           iad                = iao(i)
           ind_translate(iad) = k
           iao(i)             = iad + 1
        end do
!
! Shift back IAO.
        do j = nrow, 1, -1
           iao(j+1) = iao(j)
        end do
        iao(1) = 1
!
        return
!/
    end subroutine CooCsrInd
!/
!/ ------------------------------------------------------------------- /
!/
    subroutine ASymSmatTimVec (n, a, ja, ia, x, y, Symb)
!/
!/    07-Sep-2012 : Origination.                        ( version 3.14 )
!/                  Based on amux & atmux of SPARKIT    ( O. Gramstad  )
!/
!/    16-Nov-2018 : Prepare WW3 distribution            ( Q. Liu      )
!/    19-Feb-2018 : Add `Symb` keyword                  ( Q. Liu      )
!/
! 1. Purpose:
!    --------> Symb = -1 (antisymmetric)
!    Calculate the dot product of an antisymmetric CSR sparse matrix
!    and a vector X.
!
!    An antisymmetric (skew-symmetric) matrix is a square matrix `B`
!    whose transpose equals to its negative, i.e.,
!        B^T = -B
!
!    ◆ Do not be confused by the name of this subr. The coming-in CSR
!      sparse matrix `a` is not symmetric or antisymmetric. In our case,
!      `a` is a upper triangular sparse matrix, and we are acturally
!      calculating the dot product of `a - a^T` and `x`, where
!      'a - a^T' is an antisymmetric matrix due to the symmetry of
!      four-wave nonlinear interactions (dN₁/dt = -dN₃/dt).
!
!    This operation is in essence the dot product of two common dense
!    matrix/vector, such as
!        M(n, 1)  = A(n, n) * X(n, 1)
!        or
!        M_{i, 1} = Σa(i, j)  * x(j, 1)
!
!    For the transposed array A^T,
!        N_{i, 1} = Σat(i, j) * x(j, 1)
!                 = Σ a(j, i) * x(j, 1)
!    Alternatively, we can exchange the index of i, j for easy
!    understanding:
!        N_{j, 1} = Σat(j, i) * x(i, 1)
!                 = Σ a(i, j) * x(i, 1)
!
!    Finally, Y = M - N = A * X - A^T * X
!
!    --------> Symb = 1 (Symmetric)
!    Same as above but for Y = M + N = A * X + A^T * X
!/
        implicit none
!
        integer, intent(in)   :: n           ! # of rows/cols
!
        real, intent(in)      :: a(:)        ! CSR a (nnz)
        integer, intent(in)   :: ja(:)       ! CSR ja(nnz)
        integer, intent(in)   :: ia(n+1)     ! CSR ia(n+1)
!
        real, intent(in)      :: x(n)        ! vector of the same length
        real, intent(out)     :: y(n)        ! return product y = B * x
        real, intent(in)      :: Symb        ! -1 for minus, 1 for plus
!
! Local parameters
        integer               :: i, k
        real                  :: t
!/
! Initilization
        y(1:n) = 0.0
!
        do i = 1, n
            t = 0.0
            do k = ia(i), ia(i+1)-1
!
! M_{i, 1} =  Σa(i, j) * x(j, 1)
                t        = t + a(k) * x(ja(k))
!
!±N_{j, 1} = ±Σa(i, j)  * x(i, 1)
                y(ja(k)) = y(ja(k)) + Symb * a(k)*x(i)
            end do
            y(i) = y(i) + t
        end do
! The final Y = M ± N = A * x ± A^T * x
!
        return
!/
    end subroutine ASymSmatTimVec
!/
!/ ------------------------------------------------------------------- /
!/ Part 4
!/
    subroutine PrepKGrid(nk, nth, dpt, sig, th)
!/
!/    04-Dec-2018 : Origination.                        ( Q. Liu      )
!/    04-Dec-2018 : Based on `z_cmpcg` & `z_wnumb` of serv_xnl4v5.f90
!/                                                      ( Q. Liu      )
!/    01-Apr-2019 : Add the option using WAVNU1         ( Q. Liu      )
!/
! 1. Purpose:
!    Compute wave number k for a given discrete frequency grid and water
!    depth based on the dispersion relation for the linear wave theory
!        ω^2 = gk tanh(kd)
!
!    ◆ In WW3, the radian frequency grid ω is invariant.
!
!    ◆ It is desired that the GKE module should be independent from WW3
!      as much as possible. So I decided not to directly obtain
!      `NK, NTH, SIG, WN, CG, DSII` from WW3
!
! 2. Method
!    ✓ dispopt = 0
!    Finite depth linear dispersion relation, using a Pade approximation
!    (Hunt, 1988) [see WRT serv_xnl4v5.f90]
!
!    ✓ dispopt = 1 for WAVNU1
!/
        USE W3DISPMD, ONLY: WAVNU1
!
        implicit none
!
        integer, intent(in)    :: nk          ! # of frequencies
        integer, intent(in)    :: nth         ! # of directions
        real, intent(in)       :: dpt         ! water depth (m)
        real, intent(in)       :: sig(nk)     ! radian frequency σ
        real, intent(in)       :: th(nth)     ! θ (rad) [equally spaced,
                                              ! but may start from non-zero
                                              ! value]
!
        integer, parameter     :: dispopt = 1 ! dispersion relation
!
        integer                :: ik, ith, jkth, ns
        real                   :: x, xx, y, omega
        real                   :: k, cg, dsii, angR, dth
        real                   :: esin(nth), ecos(nth)
!/
! Initialization
        ns     = nk * nth
! Allocation of qr_kx/ky/dk/om/wn1 was done in PrepKernelIO)
        qr_kx  = 0. ! ns
        qr_ky  = 0.
        qr_dk  = 0.
        qr_om  = 0.
        qr_wn1 = 0. ! nk
!
! Calc Δθ, cosθ, sinθ [θ is equally spaced]
        dth = qr_tpi / real(nth)
!
        do ith = 1, nth
            angR      = th(ith)
            esin(ith) = sin(angR)
            ecos(ith) = cos(angR)
!
            if (abs(esin(ith)) .lt. 1.E-5) then
                esin(ith) = 0.
                if (ecos(ith) .gt. 0.5) then
                    ecos(ith) = 1.      ! θ = 0.
                else
                    ecos(ith) = -1.     ! θ = π
                end if
            end if
!
            if (abs(ecos(ith)) .lt. 1.E-5) then
                ecos(ith) = 0.
                if (esin(ith) .gt. 0.5) then
                    esin(ith) = 1.     ! θ = π/2
                else
                    esin(ith) = -1.    ! θ = π * 3/2
                end if
            end if
        end do
!
        do ik = 1, nk
            if (dispopt .eq. 0) then
! Calc k & Cg (`z_cmpcg` & `z_wnumb` of serv_xnl4v5.f90)
                omega   = sig(ik)**2.0/qr_grav
                y       = omega*dpt
                xx      = y*(y+1.0/(1.0+y*(0.66667+y*(0.35550+y*(0.16084+y*(0.06320+y* &
                             (0.02174+y*(0.00654+y*(0.00171+y*(0.00039+y*0.00011))))))))))
                x       = sqrt(xx)
                k       = x/dpt
!
                if(dpt*k > 30.0) then
                    cg  = qr_grav/(2.0*sig(ik))
                else
                    cg  = sig(ik)/k*(0.5+dpt*k/sinh(2.0*dpt*k))
                end if
!
            else if (dispopt .eq. 1) then
! Calc k & cg (WAVNU1 from WW3)
                call WAVNU1(sig(ik), dpt, k, cg)
            end if
            qr_wn1(ik) = k ! Store k in qr_wn1 ('ll used for interp.)
#ifdef W3_TS
        write(*, *) 'σ, k, cg: ', sig(ik), k, cg
#endif
! Calc Δσ
            if (ik .eq. 1) then
                dsii = 0.5 * (sig(2) - sig(1))       ! first bin
            else if (ik .eq. nk) then
                dsii = 0.5 * (sig(nk) - sig(nk-1))   ! last bin
            else
                dsii = 0.5 * (sig(ik+1) - sig(ik-1)) ! interm. bin
            end if
! Calc Kx, Ky
            do ith = 1, nth
                jkth        = ith + (ik - 1) * nth
                qr_kx(jkth) = k * ecos(ith)
                qr_ky(jkth) = k * esin(ith)
! Calc Δ\vec{k} = k Δk Δθ = k Δσ/cg Δθ
                qr_dk(jkth) = k * dsii / cg * dth
                qr_om(jkth) = sig(ik)
            end do
        end do
#ifdef W3_TS
    write(*, *) 'qr_kx: ', qr_kx
    write(*, *) 'qr_ky: ', qr_ky
    write(*, *) 'qr_dk: ', qr_dk
    write(*, *) 'qr_om: ', qr_om
#endif
!
        return
!/
    end subroutine PrepKGrid
!/
!/ ------------------------------------------------------------------- /
!/
    subroutine PrepKernelIO(nk, nth, sig, th, act)
!/
!/    04-Dec-2018 : Origination                         ( Q. Liu      )
!/    04-Dec-2018 : Extracted from Odin's subr. `calcQRSNL`
!/                                                      ( Q. Liu      )
!/
! 1. Purpose:
!    Read & Write the pre-computed kernel coefficients `T` for a given
!    discrete wavenumber grid and water depth.
!
!    For a typical 2D finite-depth wave model application, the wavenumber
!    grid varies according to water depth. Consequently, the quartet
!    configuration and interactive kernel coefficients will change as
!    well.
!
!    Therefore, it seems extremely difficult to handle a 2D varied-depth
!    application as the total number of quartets (qi_nnz) and thus the
!    array size of `Inpqr0` vary [see CalcQRSNL]. Initializing a 2D
!    array `Inpqr0` with a fixed size of (qi_nnz, nsea) becomes impossible.
!
!    So currently we are limiting ourself to deep-water or constant
!    finite-deep cases.
!
!/
        implicit none
!
        integer, intent(in)          :: nk             ! # of frequencies
        integer, intent(in)          :: nth            ! # of directions
        real, intent(in)             :: sig(nk)        ! radian frequency (rad)
        real, intent(in)             :: th(nth)        ! θ (rad) [equally spaced,
                                                       ! but may start from non-zero
                                                       ! value]
        character(len=*), intent(in) :: act            ! 'read' or 'write'
!
! Local parameters
        integer                      :: ns, iq, i1, i3, icol
        integer(kind=8)              :: rpos           ! reading position
        integer, allocatable         :: irow_coo(:), & ! row of coo mat
                                        icooTcsr(:)    ! index for coo → csr
!/
! Initilization
        ns      = nk * nth
        qi_nrsm = ns * ns
! → Be very careful that the size of `qi_irCsr` is not qi_nnz !
        if (allocated(qi_irCsr)) deallocate(qi_irCsr); allocate(qi_irCsr(qi_nrsm+1))
        if (allocated(qr_sumQR)) deallocate(qr_sumQR); allocate(qr_sumQR(qi_nrsm))
        if (allocated(qr_sumNP)) deallocate(qr_sumNP); allocate(qr_sumNP(ns, ns))
! qr_dk/om
        if (allocated(qr_dk))    deallocate(qr_dk);    allocate(qr_dk(ns))
        if (allocated(qr_om))    deallocate(qr_om);    allocate(qr_om(ns))
!
! Determine water depth for the whole module, which will be used by
! `T` & `Q` func.
        qr_depth  = max(0., min(qr_depth, qr_dmax))
        qi_disc   = max(0,  min(qi_disc, 1))
        qi_kev    = max(0,  min(qi_kev, 1))
        qi_interp = max(0,  min(qi_interp, 1))
        if (qi_disc .eq. 1) qi_interp = 0
!
! Determine the name for the binary file which stores the quartet
! configuration and the corresponding kernel coefficient ['gkev?_d????.cfg]
! constant-depth or deep water
        write(qs_cfg, "(A, '_d', I4.4, '.cfg')") trim(qs_ver), int(qr_depth)
!
        if (trim(act) == 'WRITE') then
! Calc KGrid → [qr_kx/ky/dk/om/wn]
            if (allocated(qr_kx))     deallocate(qr_kx);    allocate(qr_kx(ns))
            if (allocated(qr_ky))     deallocate(qr_ky);    allocate(qr_ky(ns))
            if (allocated(qr_wn1))    deallocate(qr_wn1);   allocate(qr_wn1(nk))
            call PrepKGrid(nk, nth, qr_depth, sig, th)
! Find total # of quartets → [qi_nnz]
            call FindQuartetNumber(ns, qr_kx, qr_ky, qr_om, qr_oml, qi_nnz)
! Find Quartet Config. → [qi_NN/PP/QQ/RR & qr_k4x/k4y/om4]
            if (allocated(qi_NN))     deallocate(qi_NN);    allocate(qi_NN(qi_nnz))
            if (allocated(qi_PP))     deallocate(qi_PP);    allocate(qi_PP(qi_nnz))
            if (allocated(qi_QQ))     deallocate(qi_QQ);    allocate(qi_QQ(qi_nnz))
            if (allocated(qi_RR))     deallocate(qi_RR);    allocate(qi_RR(qi_nnz))
!
            if (allocated(qr_k4x))    deallocate(qr_k4x);   allocate(qr_k4x(qi_nnz))
            if (allocated(qr_k4y))    deallocate(qr_k4y);   allocate(qr_k4y(qi_nnz))
            if (allocated(qr_om4))    deallocate(qr_om4);   allocate(qr_om4(qi_nnz))
!
            call FindQuartetConfig(ns, qr_kx, qr_ky, qr_om, qr_oml, qi_nnz, &
                                   qi_NN, qi_PP, qi_QQ, qi_RR,              &
                                   qr_k4x, qr_k4y, qr_om4)
!
! Calc Kernel `T`
            if (allocated(qr_TKern))  deallocate(qr_TKern); allocate(qr_TKern(qi_nnz))
            if (allocated(qr_TKurt))  deallocate(qr_TKurt); allocate(qr_TKurt(qi_nnz))
            if (allocated(qr_dom))    deallocate(qr_dom);   allocate(qr_dom(qi_nnz))
!
            do iq = 1, qi_nnz
                qr_TKern(iq) = TFunc(qr_kx(qi_NN(iq)), qr_ky(qi_NN(iq)),&
                                     qr_kx(qi_PP(iq)), qr_ky(qi_PP(iq)),&
                                     qr_kx(qi_QQ(iq)), qr_ky(qi_QQ(iq)),&
                                     qr_k4x(iq)      , qr_k4y(iq)      )
            end do
! Calc Kernel coeff. for Kurtosis
            qr_TKurt = qr_TKern * sqrt(qr_om(qi_NN) * qr_om(qi_PP) * qr_om(qi_QQ) * qr_om4)
! Calc Δω (Remove very small Δω; Δω=0 →  resonant quartets)
            qr_dom = qr_om(qi_NN) + qr_om(qi_PP) - qr_om(qi_QQ) - qr_om4
! TODO: should we use double precision for qr_dom
! Note for GNU compiler, qr_eps~1.2E-7 (single prec.) & ~2.2E-16 (double).
! The values above are also true for the intel compiler.
! sin(Δωt) / Δω is very different for Δω = 0 and Δw~1E-7 when t is large.
            where(abs(qr_dom) < qr_eps) qr_dom = 0.0
!
! Calc interp. weight if necessary
            if (qi_interp .eq. 1) then
                if (allocated(qi_bind)) deallocate(qi_bind); allocate(qi_bind(4, qi_nnz))
                if (allocated(qr_bwgh)) deallocate(qr_bwgh); allocate(qr_bwgh(4, qi_nnz))
                if (qi_bound .eq. 1 ) then
                    if (allocated(qr_bdry)) deallocate(qr_bdry); allocate(qr_bdry(qi_nnz))
                end if
                call BiInterpWT(nk, nth, qr_wn1, th)
            end if
!
            deallocate(qr_kx, qr_ky)
            deallocate(qr_k4x, qr_k4y, qr_om4)
            if (qi_interp .eq. 1) deallocate(qr_wn1)
!
! Sparse matrix index conversion [icCos shared by two formats: COO & CSR]
            if (allocated(qi_icCos))  deallocate(qi_icCos); allocate(qi_icCos(qi_nnz))
            if (allocated(irow_coo))  deallocate(irow_coo); allocate(irow_coo(qi_nnz))
            if (allocated(icooTcsr))  deallocate(icooTcsr); allocate(icooTcsr(qi_nnz))
!
            irow_coo = qi_NN + (qi_PP - 1) * ns
            qi_icCos = qi_QQ + (qi_RR - 1) * ns
!
! FindQuartetConfig stores the quartet row by row in a discontinuous order,
! so we need keep icooTcsr & qi_irCsr
            call CooCsrInd(qi_nrsm, qi_nnz, irow_coo, qi_icCos, icooTcsr, qi_irCsr)
!
! Reorder index & arrays [coo → crs]
            qi_NN    = qi_NN(icooTcsr)     ! used for calc. action prod.
            qi_PP    = qi_PP(icooTcsr)
            qi_QQ    = qi_QQ(icooTcsr)
            qi_RR    = qi_RR(icooTcsr)
            qr_TKern = qr_TKern(icooTcsr)
            qr_TKurt = qr_TKurt(icooTcsr)
            qr_dom   = qr_dom(icooTcsr)    ! Δω
!
            if (qi_interp .eq. 1) then     ! bilinear interp. weight
                qi_bind = qi_bind(:, icooTcsr)
                qr_bwgh = qr_bwgh(:, icooTcsr)
                if (qi_bound .eq. 1) qr_bdry = qr_bdry(icooTcsr)
            end if
!
            qi_icCos = qi_icCos(icooTcsr)
            deallocate(irow_coo, icooTcsr)
!
! Construct the sum vectors [used for 6D integration]
! Σ over Q, R [qr_sumQR]
            qr_sumQR = 2.0
            do i3 = 1, ns
!              i3 == i4
                icol = i3 + (i3 - 1) * ns
                qr_sumQR(icol) = 1.0
            end do
! Σ over P [qr_sumNP]
            qr_sumNP = 1.0
            do i1 = 1, ns
!               i1 == i2
                qr_sumNP(i1, i1) = 0.5
            end do
!
! WRITE KGrid & Kernel into qs_cfg
            write(*, *) '[W] Writing |', trim(qs_cfg), '| ...'
            open(51, file=trim(qs_cfg), form='unformatted', &
                     access='stream', status='replace')
!
! It is not necessary to store `ns` since `ns = nk * nth`
            write(51) nk, nth, sig, th                  ! (f, θ) grid
            write(51) qr_depth, qr_oml, qi_disc,        &
                      qi_kev, qi_interp                 ! parameters
            write(51) qr_om, qr_dk
            write(51) qi_nnz
            write(51) qi_NN, qi_PP, qi_QQ, qi_RR
            write(51) qr_TKern, qr_TKurt, qr_dom
            write(51) qi_icCos, qi_irCsr
            write(51) qr_sumQR, qr_sumNP
!
            if (qi_interp .eq. 1) write(51) qi_bind, qr_bwgh
            if ( (qi_interp .eq. 1) .and. (qi_bound .eq. 1) ) &
                write(51) qr_bdry
            close(51)
! Screen Test
#ifdef W3_TS
        write(*, *) "[W] qr_depth: ", qr_depth
        write(*, *) "[W] qr_oml  : ", qr_oml
        write(*, *) "[W] qi_disc : ", qi_disc
        write(*, *) "[W] qi_kev  : ", qi_kev
        write(*, *) "[W] qr_om   : ", qr_om
        write(*, *) "[W] qr_dk   : ", qr_dk
        write(*, *) "[W] The total number of quartets is ", qi_nnz
        write(*, *) '[W] qi_NN   : ', qi_NN
        write(*, *) '[W] qi_PP   : ', qi_PP
        write(*, *) '[W] qi_QQ   : ', qi_QQ
        write(*, *) '[W] qi_RR   : ', qi_RR
        write(*, *) '[W] qr_TKern: ', qr_TKern
        write(*, *) '[W] qr_TKurt: ', qr_TKurt
        write(*, *) '[W] qr_dom  : ', qr_dom
        write(*, *) '[W] qi_icCos: ', qi_icCos
        write(*, *) '[W] qi_irCsr: ', qi_irCsr
        write(*, *) '[W] Σ_QR    : ', qr_sumQR(1: qi_nrsm: ns+1)
        write(*, *) '[W] Σ_P     : ', (qr_sumNP(iq, iq), iq = 1, ns)
#endif
!
        else if (trim(act) == 'READ') then
            write(*, *) '⊚ → [R] Reading |', trim(qs_cfg), '| ...'
            open(51, file=trim(qs_cfg), form='unformatted', &
                     access='stream', status='old')
! nk, nth, sig, th can be skipped by using pos
            rpos = 1_8 + qi_lrb * (2_8 + nk + nth)
            read(51, pos=rpos) qr_depth, qr_oml, qi_disc,   &
                               qi_kev, qi_interp
!
! read ω & Δ\vec{k}
            read(51) qr_om, qr_dk
! read total # of quartets
            read(51) qi_nnz
            write(*, *) "⊚ → [R] The total number of quartets is ", qi_nnz
            write(*, *)
! allocate arrays
            if (allocated(qi_NN))     deallocate(qi_NN);    allocate(qi_NN(qi_nnz))
            if (allocated(qi_PP))     deallocate(qi_PP);    allocate(qi_PP(qi_nnz))
            if (allocated(qi_QQ))     deallocate(qi_QQ);    allocate(qi_QQ(qi_nnz))
            if (allocated(qi_RR))     deallocate(qi_RR);    allocate(qi_RR(qi_nnz))
!
            if (allocated(qr_TKern))  deallocate(qr_TKern); allocate(qr_TKern(qi_nnz))
            if (allocated(qr_TKurt))  deallocate(qr_TKurt); allocate(qr_TKurt(qi_nnz))
            if (allocated(qr_dom))    deallocate(qr_dom);   allocate(qr_dom(qi_nnz))
!
            if (allocated(qi_icCos))  deallocate(qi_icCos); allocate(qi_icCos(qi_nnz))
!
            read(51) qi_NN, qi_PP, qi_QQ, qi_RR
            read(51) qr_TKern, qr_TKurt, qr_dom
            read(51) qi_icCos, qi_irCsr
            read(51) qr_sumQR, qr_sumNP
!
            if (qi_interp .eq. 1) then
                if (allocated(qi_bind)) deallocate(qi_bind); allocate(qi_bind(4, qi_nnz))
                if (allocated(qr_bwgh)) deallocate(qr_bwgh); allocate(qr_bwgh(4, qi_nnz))
                read(51) qi_bind, qr_bwgh
!
                if (qi_bound .eq. 1) then
                    if (allocated(qr_bdry)) deallocate(qr_bdry); allocate(qr_bdry(qi_nnz))
                    read(51) qr_bdry
                end if
!
            end if
!
            close(51)
! Screen Test
#ifdef W3_TS
        write(*, *) "[R] qr_depth: ", qr_depth
        write(*, *) "[R] qr_oml  : ", qr_oml
        write(*, *) "[R] qi_disc : ", qi_disc
        write(*, *) "[R] qi_kev  : ", qi_kev
        write(*, *) "[R] qr_om   : ", qr_om
        write(*, *) "[R] qr_dk   : ", qr_dk
        write(*, *) "[R] The total number of quartets is ", qi_nnz
        write(*, *) '[R] qi_NN   : ', qi_NN
        write(*, *) '[R] qi_PP   : ', qi_PP
        write(*, *) '[R] qi_QQ   : ', qi_QQ
        write(*, *) '[R] qi_RR   : ', qi_RR
        write(*, *) '[R] qr_TKern: ', qr_TKern
        write(*, *) '[R] qr_TKurt: ', qr_TKurt
        write(*, *) '[R] qr_dom  : ', qr_dom
        write(*, *) '[R] qi_icCos: ', qi_icCos
        write(*, *) '[R] qi_irCsr: ', qi_irCsr
        write(*, *) '[R] Σ_QR    : ', qr_sumQR(1: qi_nrsm: ns+1)
        write(*, *) '[R] Σ_P     : ', (qr_sumNP(iq, iq), iq = 1, ns)
#endif
        end if
!/
    end subroutine PrepKernelIO
!/
!/ ------------------------------------------------------------------- /
!/
    subroutine BiInterpWT(nk, nth, wn, th)
!/
!/    19-Apr-2019 : Origination                         ( Q. Liu      )
!/    19-Apr-2019 : Extracted from a few subrs. of mod_xnl4v5.f90
!/                                                      ( Q. Liu      )
!/    01-Apr-2020 : Boundary conditions                 ( Q. Liu      )
!/
! 1. Purpose:
!    Calculate weights for the bilinear interpolation.
!
! 2. Method:
!    See also Fig. 9 of van Vledder (2006, CE) and mod_xnl4v5.f90 (WRT).
!    [q_t13v4,  q_weight, q_makegrid
!/
        implicit none
!
        integer, intent(in) :: nk
        integer, intent(in) :: nth
        real, intent(in)    :: wn(nk)  ! k
        real, intent(in)    :: th(nth) ! θ
!
        integer             :: iq, jkU, jk4, jk4p, jth4T, jth4, jth4p
        real                :: dth, aRef, k4T, angR, &
                               r_jk, r_jth, delK, w_k4, w_th4
        real                :: kmin, kmax, k4R
        real                :: qr_kpow
!
! Initialization
        qi_bind = 0
        qr_bwgh = 0.
        if (qi_bound .eq. 1) qr_bdry = 1.
!
! Get power law for F(k) from qi_fpow for E(f)
!     E(f) df = F(k) dk →  F(k) ~ f^n * cg = k^{(n-1)/2}
!     N(k) = F(k) / ω ~ k^{n/2-1}
!     C(k) = N(k) / k ~ k^{n/2-1 -1}
        qr_kpow = qr_fpow / 2. - 2.

! Kmin & Kmax
        kmin = minval(wn)
        kmax = maxval(wn)
!
! In general, th(nth) in [0, 2π). Note however, it is not the case when
! the first directional bin defined in ww3_grid.inp (RTH0) is not zero.
        dth     = qr_tpi / real(nth)
        aRef    = th(1)
!
! qr_k4x(nnz), qr_k4y(nnz), wn(nk) are already available for use
        do iq = 1, qi_nnz
            k4R = sqrt(qr_k4x(iq)**2. + qr_k4y(iq)**2.)  ! k₄
            angR= atan2(qr_k4y(iq), qr_k4x(iq))          ! θ₄ [-π, π]
! Boundary
            if (qi_bound .eq. 1) then
                k4T = max(kmin, min(k4R, kmax))
            else
                k4T = k4R ! already bounded in [kmin, kmax]
            end if
!
! Layout of surrouding four (f, θ) grid points
!
!          (θ)↑
!             ↑ ₄             ₃
!       jth4+1 ▪ ----------- ▪
!              |             |
!              |      r_jth) |
!     w-       |---✗ (r_jk,  |
!     t|       |   |         |
!     h|       |₁  |         |₂
!     4-  jth4 ▪ ----------- ▪  → → (k)
!             jk4            jk4+1
!              |---|
!               wk4
!
! i) θ index (counted counterclockwisely)
            r_jth = (angR - aRef) / dth + 1.
            jth4T = floor(r_jth)              ! 'll be revised later
            w_th4 = r_jth - real(jth4T)       ! dirc. weight
!
            jth4  = mod(jth4T-1+nth, nth) + 1 ! wrap around 2π
            jth4p = mod(jth4T+nth, nth) + 1
!
! ii) k index (counted in an ascending order). Note, as required in
! FindQuartetConfig, k4T >= kmin & k4T <= kmax are already satisfied.
! Thus, the resulted jkU will be in [1, nk].
! Two special cases:
!           /  1,  k4T = kmin
!     jkU = |
!           \ NK,  k4T = kmax or k4T in (wn(nk-1), kmax)
!
            jkU = 1
            do while (k4T > wn(jkU))
                jkU  = jkU + 1
                if (jkU > nk) exit ! impossible in our case
            end do
!
            if (jkU .eq. 1) then           ! k4T = kmin
                r_jk = 1.
            else                           ! k4T in (kmin, kmax]
                delK = wn(jkU) - wn(jkU-1) ! Δk
                r_jk = real(jkU - 1.) + (k4T - wn(jkU-1)) / delK
            end if
! Parse r_jk
            jk4   = floor(r_jk)            ! in [1, nk]
            w_k4  = r_jk  - real(jk4)
            jk4p  = min(jk4+1, nk)         ! k4T = kmax ← min func.
!
! Store indices (in 1D vector; jkth = ith + (ik-1) * nth)
            qi_bind(1, iq) = jth4  + (jk4  - 1) * nth
            qi_bind(2, iq) = jth4  + (jk4p - 1) * nth
            qi_bind(3, iq) = jth4p + (jk4p - 1) * nth
            qi_bind(4, iq) = jth4p + (jk4  - 1) * nth
!
! Store weights
            qr_bwgh(1, iq) = (1. - w_k4) * (1. - w_th4)
            qr_bwgh(2, iq) = w_k4 * (1. - w_th4)
            qr_bwgh(3, iq) = w_k4 * w_th4
            qr_bwgh(4, iq) = (1. - w_k4) * w_th4
!
! Note that the qi_bind & qr_bwgh do not make full sense when
! k4 < kmin (k indices are not correct at all) or k4 > kmax (k index = NK)
! because we have capped k4T in between kmin and kmax.
! But no need to worry about this because
!     1) C(k) = 0. when k < kmin
!     2) C(k) = C(NK) * power decay when k > kmax
!
            if (qi_bound .eq. 1) then
                if (k4R < kmin) then
                    qr_bdry(iq) = 0.
                else if (k4R > kmax) then
                    qr_bdry(iq) = (k4R/kmax)**qr_kpow
                end if
            end if
!
        end do
!/
    end subroutine BiInterpWT
!/
!/ ------------------------------------------------------------------- /
!/
    subroutine CalcQRSNL(nk, nth, sig, th,         &
                         t0, t1, Cvk0, Cvk1,       &
                         Inpqr0, Snl, Dnl, Kurt)
!/
!/    09-Dec-2018 : Origination                         ( Q. Liu      )
!/    09-Dec-2018 : Extracted from Odin's subr. `calcQRSNL`
!/                                                      ( Q. Liu      )
!/    10-Jun-2019 : Include Janssen's KE properly       ( Q. Liu      )
!/    07-Jun-2021 : Switch off the cal. of kurtosis (!|KT|)
!/                                                      ( Q. Liu      )
!/
! 1. Purpose:
!    Calculate the nonlinear transfer rates for a given frequency
!    grid and given action density spectrum C(\vec{k}).
!
!    According to J09 and GS13, C(\vec{k}) is given by
!             /
!        m0 = | F(\vec{k}) d \vec{k}
!             /
!
!        F(\vec{k}) = ω C(\vec{k}) / g,
!
!    whereas the wave action density spectrum used in WW3 is given by
!        F(\vec{k}) d \vec{k} = F(k, θ)   dk dθ
!                             = N(k, θ) ω dk dθ
!
!    Thus, we have
!        C(\vec{k}) = N * g / k.
!
! 2. Method
!    See GS13 & GB16 for all the details.
!
!    ◆ t0, t1 here are time `relative` to the begining time of the
!      simulaiton, rather than the `absolute` time
!
!    ◆ Cvk0, Cvk1, Snl, Dnl shoud be organized/stored in the same way as
!      qr_kx, qr_ky, qr_dk, qr_om
!/
        implicit none
!
        integer, intent(in)        :: nk             ! # of frequencies
        integer, intent(in)        :: nth            ! # of directions
        real, intent(in)           :: sig(nk)        ! radian frequency (rad)
        real, intent(in)           :: th(nth)        ! θ (rad) [equally spaced,
                                                     ! but may start from non-zero
!
        real, intent(inout)        :: t0             ! previous time step
        real, intent(inout)        :: Cvk0(nk*nth)   ! Action density @ t0
        complex, intent(inout)     :: Inpqr0(:)      ! I(t) @ t0
!
        real, intent(in)           :: t1             ! current  time step
        real, intent(in)           :: Cvk1(nk*nth)   ! Action density @ t1
                                                     ! ... C(\vec{k})
!
        real, intent(out)          :: Snl(nk*nth)    ! Snl = dC/dt
        real, intent(out)          :: Dnl(nk*nth)    ! Dnl
        real, intent(out)          :: Kurt           ! Kurtosis
!
! Local parameters
!
        real                       :: DelT           ! Δt
        logical, save              :: FlRead = .true.
        integer                    :: num_I, ns
        real                       :: Dvk0(nk*nth),& ! Odin's discrete Cvk @ t0
                                      Dvk1(nk*nth)   ! ...     @ t1
        real, allocatable, save    :: Cvk0_R(:),   & ! C₄      @ t0
                                      Cvk1_R(:)      !         @ t1
        real, allocatable, save    :: Fnpqr0(:),   & ! C prod. @ t0
                                      Fnpqr1(:),   & ! C prod. @ t1
                                      Mnpqr (:)      ! δC_1/δt * δk_1
        complex, allocatable, save :: Inpqr1(:),   & ! I(t) @ t1
                                      ETau(:),     & ! exp(iΔωt)
                                      EDelT(:)       ! exp(iΔωΔt)
!
        real, allocatable, save    :: Mnp1D(:), Mnp2D(:, :)
        real                       :: SecM2          ! Second-order moment²
!/
!
! Initilization
        ns      = nk * nth
        qi_nrsm = ns * ns
!
! Only constant depth is allowed now. Accordingly, we only need a single
! binary config file which provides the wavenumber grid and kernel
! coefficients.
!
! Read quartets & kernel coefficients in
        if (FlRead) then
! Only read data once
            call PrepKernelIO(nk, nth, sig, th, 'READ')
            FlRead = .false.
!           write(*, *) "⊚ → [R] FLag for Reading Kernels becomes |", FlRead, "|"
! Allocate arrays
! ✓ A variable with the SAVE attribute retains its value and definition,
! association, and `allocation` status on exit from a procedure
!
            if (allocated(Fnpqr0)) deallocate(Fnpqr0); allocate(Fnpqr0(qi_nnz))
            if (allocated(Fnpqr1)) deallocate(Fnpqr1); allocate(Fnpqr1(qi_nnz))
            if (allocated(ETau  )) deallocate(ETau  ); allocate(ETau  (qi_nnz))
            if (allocated(EDelT )) deallocate(EDelT ); allocate(EDelT (qi_nnz))
            if (allocated(Mnpqr )) deallocate(Mnpqr ); allocate(Mnpqr (qi_nnz))
            if (allocated(Inpqr1)) deallocate(Inpqr1); allocate(Inpqr1(qi_nnz))
!
            if (allocated(Mnp1D))  deallocate(Mnp1D);  allocate(Mnp1D(qi_nrsm))
            if (allocated(Mnp2D))  deallocate(Mnp2D);  allocate(Mnp2D(ns, ns))
!
            if (qi_disc .eq. 0) then
                if (allocated(Cvk0_R)) deallocate(Cvk0_R); allocate(Cvk0_R(qi_nnz))
                if (allocated(Cvk1_R)) deallocate(Cvk1_R); allocate(Cvk1_R(qi_nnz))
            end if
!
        end if
!
! Screen output (check whether the kernel data are stored in memory)
#ifdef W3_TS
    write(*, *) "◆ qr_depth      :", qr_depth
    write(*, *) "◆ qr_oml        :", qr_oml
    write(*, *) "◆ qi_disc       :", qi_disc
    write(*, *) "◆ qi_kev        :", qi_kev
    write(*, *) "◆ qi_nnz        :", qi_nnz
    write(*, *) "◆ qi_NN(:10)    :", qi_NN(:10)
    write(*, *) "◆ qr_TKern(:10) :", qr_TKern(:10)
    write(*, *) "◆ qr_TKurt(:10) :", qr_TKurt(:10)
    write(*, *) "◆ qr_sumQR(:10) :", qr_sumQR(:10)
#endif
!
        num_I = size(Inpqr0)
        if (num_I .ne. qi_nnz) then
            write(*, 1001) 'CalcQRSNL'
            call exit(1)
        end if
!
! Start to calc. Snl term
        if (qi_disc == 0) then
! Define ΔC = dC/dt * Δk Δt, we have ΔC₁ = ΔC₂ = -ΔC₃ = -ΔC₄ (Δt can be
! removed by taking the unit time)
!
! Cvk0/1_R (bilinear interp. or nearest bin)
            if (qi_interp .eq. 0) then
                Cvk0_R = Cvk0(qi_RR)
                Cvk1_R = Cvk1(qi_RR)
!
            else if (qi_interp .eq. 1) then
                Cvk0_R = qr_bwgh(1, :) * Cvk0(qi_bind(1, :)) + &
                         qr_bwgh(2, :) * Cvk0(qi_bind(2, :)) + &
                         qr_bwgh(3, :) * Cvk0(qi_bind(3, :)) + &
                         qr_bwgh(4, :) * Cvk0(qi_bind(4, :))
!
                Cvk1_R = qr_bwgh(1, :) * Cvk1(qi_bind(1, :)) + &
                         qr_bwgh(2, :) * Cvk1(qi_bind(2, :)) + &
                         qr_bwgh(3, :) * Cvk1(qi_bind(3, :)) + &
                         qr_bwgh(4, :) * Cvk1(qi_bind(4, :))
!
                if (qi_bound .eq. 1) then
                    Cvk0_R = Cvk0_R * qr_bdry
                    Cvk1_R = Cvk1_R * qr_bdry
                end if
!
            end if
!
! F = [C₃ C₄ (C₁ + C₂) - C₁ C₂ (C₃ + C₄)] dk₂ dk₃ dk₄ ∙ dk₁
! dk₄ vanishes with the δ function
            Fnpqr0 = (Cvk0(qi_QQ) * Cvk0_R      * (  &
                      Cvk0(qi_NN) + Cvk0(qi_PP) ) -  &
                      Cvk0(qi_NN) * Cvk0(qi_PP) * (  &
                      Cvk0(qi_QQ) + Cvk0_R      )) * &
                     qr_dk(qi_NN) * qr_dk(qi_PP) * qr_dk(qi_QQ)
!
            Fnpqr1 = (Cvk1(qi_QQ) * Cvk1_R      * (  &
                      Cvk1(qi_NN) + Cvk1(qi_PP) ) -  &
                      Cvk1(qi_NN) * Cvk1(qi_PP) * (  &
                      Cvk1(qi_QQ) + Cvk1_R      )) * &
                     qr_dk(qi_NN) * qr_dk(qi_PP) * qr_dk(qi_QQ)
!
        else if (qi_disc == 1) then
! Used in GS13 & GB16
! F = [C₃dk₃ C₄dk₄ (C₁dk₁ + C₂dk₂) - C₁dk₁ C₂dk₂ (C₃dk₃ + C₄dk₄)]
! It seems the bilinear interpolation for this discretization approach
! is not very meaningful.
            Dvk0   = Cvk0 * qr_dk
            Fnpqr0 = Dvk0(qi_QQ) * Dvk0(qi_RR) * ( &
                     Dvk0(qi_NN) + Dvk0(qi_PP) ) - &
                     Dvk0(qi_NN) * Dvk0(qi_PP) * ( &
                     Dvk0(qi_QQ) + Dvk0(qi_RR) )
!
            Dvk1   = Cvk1 * qr_dk
            Fnpqr1 = Dvk1(qi_QQ) * Dvk1(qi_RR) * ( &
                     Dvk1(qi_NN) + Dvk1(qi_PP) ) - &
                     Dvk1(qi_NN) * Dvk1(qi_PP) * ( &
                     Dvk1(qi_QQ) + Dvk1(qi_RR) )
!
        end if
!|KT|! Calc m2 for Kurtosis estimation ((2.6) of Annekov & Shrira (2013))
!|KT|        SecM2 = sum(Cvk1 * qr_om * qr_dk) ** 2.
!
!       write(*, *) '.... Input args: t0, t1 :', t0, t1
        if (abs(t1) < qr_eps) then
! t1 = 0.0 [essentially I₁ = 0 → I₀ = 0]
            t0     = 0.0
            Cvk0   = Cvk1
            Inpqr0 = (0.0, 0.0)  ! \int_{0}^{0} dt  = 0
            Snl    = 0.0
            Dnl    = 0.0
            Kurt   = 0.0
        else
! t1 ≠ 0.0
            DelT   = t1 - t0
            if (DelT < 0.0) then
                write(*, 1002) 'CalcQRSNL'
                call exit(2)
            end if
            ETau   = exp(qc_iu * cmplx(qr_dom * t1))       ! exp(iΔωt)
            EDelT  = exp(qc_iu * cmplx(qr_dom * DelT))     ! exp(iΔωΔt)
!
! ◆ Calc. I₁: note here I₁ = I(t₁) dk₁ dk₂ dk₃ for both qi_disc = 0/1
            if (qi_kev .eq. 0) then
! GKE from GS13, GB16
                Inpqr1 = Inpqr0 + cmplx(0.5 * DelT) *  &
                         conjg(ETau)                *  &       ! exp(-iΔωt)
                         (cmplx(Fnpqr0) * EDelT + cmplx(Fnpqr1))
            else if (qi_kev .eq. 1) then
! KE from J03 (Fnpqr1 is taken outside the time integral; Fnpqr0 is not
! used in this case; and the real part of Inpqr1 is sin(Δωt)/Δω, and
! the imaginary part is [1 - cos(Δωt)] / Δω
! Approximation used before
!               Inpqr1 = Inpqr0 + cmplx(0.5 * DelT) *  &
!                        conjg(ETau) * (EDelT + 1)
!
                where (abs(qr_dom) < qr_eps)
! Δω = 0., sin(Δωt)/Δω ~ t, [1 - cos(Δωt)] / Δω ~ 0
                    Inpqr1 = cmplx(t1, 0.)
                elsewhere
! Δω ≠ 0., cacl. sin(Δωt)/Δω & [1 - cos(Δωt)] / Δω directly
! TODO: the sign of cos is not clear yet.
                    Inpqr1 = cmplx(sin(qr_dom * t1) / qr_dom,     &
                                   (1 - cos(qr_dom * t1)) / qr_dom)
                end where
            end if
! ◆ Snl [Tranfer Integal]
            if (qi_kev .eq. 0) then
! GKE from GS13, GB16
                Mnpqr  = 4.0 * (qr_TKern ** 2.) * real(ETau * Inpqr1)
            else if (qi_kev .eq. 1) then
! KE from J03
!               Mnpqr  = 4.0 * (qr_TKern ** 2.) * Fnpqr1 * real(ETau * Inpqr1)
                Mnpqr  = 4.0 * (qr_TKern ** 2.) * Fnpqr1 * real(Inpqr1)
            end if
! Calc. Σ over Q, R [Mnpqr is a upper triangular sparse matrix]
! dN₁/dt = - dN₃/dt →  anti-symmetric array operation
! Mnp1D  = (Mnpqr - Mnpqr^{T}) × S_{qr}
            call ASymSmatTimVec(qi_nrsm, Mnpqr, qi_icCos, qi_irCsr, qr_sumQR, Mnp1D, -1.0)
! Calc. Σ over P [Mnp2D is a upper triangular matrix]
! dN₁/dt = dN₂/dt →  symmetric array operation
! Snl    = {Σ (Mnp + Mnp^{T}) ⊙ S_{p}} / d\vec{k₁}
            Mnp2D  = reshape(Mnp1D, (/ns, ns/))
            Snl    = sum((Mnp2D + transpose(Mnp2D)) * qr_sumNP, 2) / qr_dk
! ◆ Conservation Check
#ifdef W3_TS
        write(*, '(A, E15.3)') '   ← {WW3 GKE } ΣSnl(k) * dk: ', sum(Snl * qr_dk)
#endif
!
! ◆ Dnl [Diagonal term]  <TODO>
!   i) it is easy to calculate Dnl for Janssen's KE (but we may
!      have to abandon the sparse array approach)
!  ii) it is challenging to get Dnl for GKE.
            Dnl  = 0.0
            Kurt = 0.0
!
!|KT|! ◆ Kurtosis
!|KT|            if (qi_kev .eq. 0) then
!|KT|! GKE from GS13, GB16
!|KT|                Mnpqr  = -3.0 / SecM2 * qr_TKurt * aimag(ETau * Inpqr1)
!|KT|            else if (qi_kev .eq. 1) then
!|KT|! KE from J03 (here the imaginary part becomes [1 - cos(Δωt)] / Δω
!|KT|!               Mnpqr  = -3.0 / SecM2 * qr_TKurt * Fnpqr1 * aimag(ETau * Inpqr1)
!|KT|                Mnpqr  = -3.0 / SecM2 * qr_TKurt * Fnpqr1 * aimag(Inpqr1)
!|KT|            end if
!|KT|! Calc. Σ over Q, R [Mnpqr is a upper triangular sparse matrix]
!|KT|! symmetric array operation Mnp1D  = (Mnpqr - Mnpqr^{T}) × S_{qr}
!|KT|            call ASymSmatTimVec(qi_nrsm, Mnpqr, qi_icCos, qi_irCsr, qr_sumQR, Mnp1D, 1.0)
!|KT|            Mnp2D  = reshape(Mnp1D, (/ns, ns/))
!|KT|            Kurt   = sum((Mnp2D + transpose(Mnp2D)) * qr_sumNP)
!
! I₁ → I₀ for next computation (time step)
            t0     = t1
            Cvk0   = Cvk1
            Inpqr0 = Inpqr1
        end if
!
!       write(*, *) '.... Output args: t0, t1 :', t0, t1
!
! Formats
 1001 FORMAT(/' *** GKE ERROR IN gkeModule : '/  &
              '     Subr. ', A, ': the stored total number of quartets &
                    & and the size of Inpqr0 do not match !'/)
 1002 FORMAT(/' *** GKE ERROR IN gkeModule : '/  &
              '     Subr. ', A, ': t0 ≤ t1 is not satisfied !'/)
!/
    end subroutine CalcQRSNL
!/
!/ ------------------------------------------------------------------- /
end module w3gkemd
!/ ------------------------------------------------------------------- /
