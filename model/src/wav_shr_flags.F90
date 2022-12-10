!> @file wav_shr_flags
!!
!> Shared flags matching compile time options
!!
!> @details Sets logical flags to according to compile time
!! options
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 07-01-2022
module wav_shr_flags

  implicit none
  public

#ifdef W3_DIST
  logical, parameter ::  w3_dist_flag = .true.            !< @public a flag for "W3_DIST"
#else
  logical, parameter ::  w3_dist_flag = .false.           !< @public a flag for "W3_DIST"
#endif
#ifdef W3_SHRD
  logical, parameter ::  w3_shrd_flag = .true.            !< @public a flag for "W3_SHRD"
#else
  logical, parameter ::  w3_shrd_flag = .false.           !< @public a flag for "W3_SHRD"
#endif
  !   debug/logging
#ifdef W3_DEBUG
  logical, parameter ::  w3_debug_flag = .true.           !< @public a flag for "W3_DEBUG"
#else
  logical, parameter ::  w3_debug_flag = .false.          !< @public a flag for "W3_DEBUG"
#endif
#ifdef W3_DEBUGGRID
  logical, parameter ::  w3_debuggrid_flag = .true.       !< @public a flag for "W3_DEBUGGRID"
#else
  logical, parameter ::  w3_debuggrid_flag = .false.      !< @public a flag for "W3_DEBUGGRID"
#endif

#ifdef W3_DEBUGSTP
  logical, parameter ::  w3_debugstp_flag = .true.        !< @public a flag for "W3_DEBUGSTP"
#else
  logical, parameter ::  w3_debugstp_flag = .false.       !< @public a flag for "W3_DEBUGSTP"
#endif

#ifdef W3_DEBUGFLS
  logical, parameter ::  w3_debugfls_flag = .true.        !< @public a flag for "W3_DEBUGFLS"
#else
  logical, parameter ::  w3_debugfls_flag = .false.       !< @public a flag for "W3_DEBUGFLS"
#endif

#ifdef W3_DEBUGCOH
  logical, parameter ::  w3_debugcoh_flag = .true.        !< @public a flag for "W3_DEBUGCOH"
#else
  logical, parameter ::  w3_debugcoh_flag = .false.       !< @public a flag for "W3_DEBUGCOH"
#endif

#ifdef W3_DEBUGIOBP
  logical, parameter ::  w3_debugiobp_flag = .true.       !< @public a flag for "W3_DEBUGIOBP"
#else
  logical, parameter ::  w3_debugiobp_flag = .false.      !< @public a flag for "W3_DEBUGIOBP"
#endif

#ifdef W3_DEBUGIOGR
  logical, parameter ::  w3_debugiogr_flag = .true.       !< @public a flag for "W3_DEBUGIOGR"
#else
  logical, parameter ::  w3_debugiogr_flag = .false.      !< @public a flag for "W3_DEBUGIOGR"
#endif

#ifdef W3_DEBUGIOBC
  logical, parameter ::  w3_debugiobc_flag = .true.       !< @public a flag for "W3_DEBUGIOBC"
#else
  logical, parameter ::  w3_debugiobc_flag = .false.      !< @public a flag for "W3_DEBUGIOBC"
#endif
#ifdef W3_DEBUGDCXDX
  logical, parameter ::  w3_debugdcxdx_flag = .true.      !< @public a flag for "W3_DEBUGDCXDX"
#else
  logical, parameter ::  w3_debugdcxdx_flag = .false.     !< @public a flag for "W3_DEBUGDCXDX"
#endif

#ifdef W3_DEBUSETIOBP
  logical, parameter ::  w3_debugsetiobp_flag = .true.    !< @public a flag for "W3_DEBUSETIOBP"
#else
  logical, parameter ::  w3_debugsetiobp_flag = .false.   !< @public a flag for "W3_DEBUSETIOBP"
#endif

#ifdef W3_DEBUSETUGIOBP
  logical, parameter ::  w3_debugsetugiobp_flag = .true.  !< @public a flag for "W3_DEBUSETUGIOBP"
#else
  logical, parameter ::  w3_debugsetugiobp_flag = .false. !< @public a flag for "W3_DEBUSETUGIOBP"
#endif

#ifdef W3_DEBUGSRC
  logical, parameter ::  w3_debugsrc_flag = .true.        !< @public a flag for "W3_DEBUGSRC"
#else
  logical, parameter ::  w3_debugsrc_flag = .false.       !< @public a flag for "W3_DEBUGSRC"
#endif

#ifdef W3_DEBUGINIT
  logical, parameter ::  w3_debuginit_flag = .true.       !< @public a flag for "W3_DEBUGINIT"
#else
  logical, parameter ::  w3_debuginit_flag = .false.      !< @public a flag for "W3_DEBUGINIT"
#endif

#ifdef W3_DEBUGRUN
  logical, parameter ::  w3_debugrun_flag = .true.        !< @public a flag for "W3_DEBUGRUN"
#else
  logical, parameter ::  w3_debugrun_flag = .false.       !< @public a flag for "W3_DEBUGRUN"
#endif

#ifdef W3_DEBUGIO
  logical, parameter ::  w3_debugio_flag = .true.         !< @public a flag for "W3_DEBUGIO"
#else
  logical, parameter ::  w3_debugio_flag = .false.        !< @public a flag for "W3_DEBUGIO"
#endif
#ifdef W3_DEBUGW3ULEV
  logical, parameter ::  w3_debugw3ulev_flag = .true.     !< @public a flag for "W3_DEBUGW3ULEV"
#else
  logical, parameter ::  w3_debugw3ulev_flag = .false.    !< @public a flag for "W3_DEBUGW3ULEV"
#endif

#ifdef W3_TIMINGS
  logical, parameter ::  w3_timings_flag = .true.         !< @public a flag for "W3_TIMINGS"
#else
  logical, parameter ::  w3_timings_flag = .false.        !< @public a flag for "W3_TIMINGS"
#endif
#ifdef W3_TS
  logical, parameter ::  w3_ts_flag = .true.              !< @public a flag for "W3_TS"
#else
  logical, parameter ::  w3_ts_flag = .false.             !< @public a flag for "W3_TS"
#endif

  !   propagation/gse

#ifdef W3_PR0
  logical, parameter ::  w3_pr0_flag = .true.             !< @public a flag for "W3_PR0"
#else
  logical, parameter ::  w3_pr0_flag = .false.            !< @public a flag for "W3_PR0"
#endif

#ifdef W3_PR1
  logical, parameter ::  w3_pr1_flag = .true.             !< @public a flag for "W3_PR1"
#else
  logical, parameter ::  w3_pr1_flag = .false.            !< @public a flag for "W3_PR1"
#endif

#ifdef W3_PR2
  logical, parameter ::  w3_pr2_flag = .true.             !< @public a flag for "W3_PR2"
#else
  logical, parameter ::  w3_pr2_flag = .false.            !< @public a flag for "W3_PR2"
#endif

#ifdef W3_PR3
  logical, parameter ::  w3_pr3_flag = .true.             !< @public a flag for "W3_PR3"
#else
  logical, parameter ::  w3_pr3_flag = .false.            !< @public a flag for "W3_PR3"
#endif

#ifdef W3_UNO
  logical, parameter ::  w3_uno_flag = .true.             !< @public a flag for "W3_UNO"
#else
  logical, parameter ::  w3_uno_flag = .false.            !< @public a flag for "W3_UNO"
#endif

#ifdef W3_UQ
  logical, parameter ::  w3_uq_flag = .true.              !< @public a flag for "W3_UQ"
#else
  logical, parameter ::  w3_uq_flag = .false.             !< @public a flag for "W3_UQ"
#endif

  !   flux scheme

#ifdef W3_FLX0
  logical, parameter ::  w3_flx0_flag = .true.            !< @public a flag for "W3_FLX0"
#else
  logical, parameter ::  w3_flx0_flag = .false.           !< @public a flag for "W3_FLX0"
#endif

#ifdef W3_FLX1
  logical, parameter ::  w3_flx1_flag = .true.            !< @public a flag for "W3_FLX1"
#else
  logical, parameter ::  w3_flx1_flag = .false.           !< @public a flag for "W3_FLX1"
#endif

#ifdef W3_FLX2
  logical, parameter ::  w3_flx2_flag = .true.            !< @public a flag for "W3_FLX2"
#else
  logical, parameter ::  w3_flx2_flag = .false.           !< @public a flag for "W3_FLX2"
#endif

#ifdef W3_FLX3
  logical, parameter ::  w3_flx3_flag = .true.            !< @public a flag for "W3_FLX3"
#else
  logical, parameter ::  w3_flx3_flag = .false.           !< @public a flag for "W3_FLX3"
#endif

#ifdef W3_FLX4
  logical, parameter ::  w3_flx4_flag = .true.            !< @public a flag for "W3_FLX4"
#else
  logical, parameter ::  w3_flx4_flag = .false.           !< @public a flag for "W3_FLX4"
#endif

#ifdef W3_FLX5
  logical, parameter ::  w3_flx5_flag = .true.            !< @public a flag for "W3_FLX5"
#else
  logical, parameter ::  w3_flx5_flag = .false.           !< @public a flag for "W3_FLX5"
#endif

  !   linear input

#ifdef W3_LN0
  logical, parameter ::  w3_ln0_flag = .true.             !< @public a flag for "W3_LN0"
#else
  logical, parameter ::  w3_ln0_flag = .false.            !< @public a flag for "W3_LN0"
#endif

#ifdef W3_SEED
  logical, parameter ::  w3_seed_flag = .true.            !< @public a flag for "W3_SEED"
#else
  logical, parameter ::  w3_seed_flag = .false.           !< @public a flag for "W3_SEED"
#endif

#ifdef W3_LN1
  logical, parameter ::  w3_ln1_flag = .true.             !< @public a flag for "W3_LN1"
#else
  logical, parameter ::  w3_ln1_flag = .false.            !< @public a flag for "W3_LN1"
#endif

  !   input/dissipation

#ifdef W3_ST0
  logical, parameter ::  w3_st0_flag = .true.             !< @public a flag for "W3_ST0"
#else
  logical, parameter ::  w3_st0_flag = .false.            !< @public a flag for "W3_ST0"
#endif

#ifdef W3_ST1
  logical, parameter ::  w3_st1_flag = .true.             !< @public a flag for "W3_ST1"
#else
  logical, parameter ::  w3_st1_flag = .false.            !< @public a flag for "W3_ST1"
#endif

#ifdef W3_ST2
  logical, parameter ::  w3_st2_flag = .true.             !< @public a flag for "W3_ST2"
#else
  logical, parameter ::  w3_st2_flag = .false.            !< @public a flag for "W3_ST2"
#endif

#ifdef W3_STAB0
  logical, parameter ::  w3_stab0_flag = .true.           !< @public a flag for "W3_STAB0"
#else
  logical, parameter ::  w3_stab0_flag = .false.          !< @public a flag for "W3_STAB0"
#endif

#ifdef W3_STAB2
  logical, parameter ::  w3_stab2_flag = .true.           !< @public a flag for "W3_STAB2"
#else
  logical, parameter ::  w3_stab2_flag = .false.          !< @public a flag for "W3_STAB2"
#endif

#ifdef W3_ST3
  logical, parameter ::  w3_st3_flag = .true.             !< @public a flag for "W3_ST3"
#else
  logical, parameter ::  w3_st3_flag = .false.            !< @public a flag for "W3_ST3"
#endif

#ifdef W3_STAB3
  logical, parameter ::  w3_stab3_flag = .true.           !< @public a flag for "W3_STAB3"
#else
  logical, parameter ::  w3_stab3_flag = .false.          !< @public a flag for "W3_STAB3"
#endif

#ifdef W3_ST4
  logical, parameter ::  w3_st4_flag = .true.             !< @public a flag for "W3_ST4"
#else
  logical, parameter ::  w3_st4_flag = .false.            !< @public a flag for "W3_ST4"
#endif

#ifdef W3_ST6
  logical, parameter ::  w3_st6_flag = .true.             !< @public a flag for "W3_ST6"
#else
  logical, parameter ::  w3_st6_flag = .false.            !< @public a flag for "W3_ST6"
#endif

  !   non-linear interaction

#ifdef W3_NL0
  logical, parameter ::  w3_nl0_flag = .true.             !< @public a flag for "W3_NL0"
#else
  logical, parameter ::  w3_nl0_flag = .false.            !< @public a flag for "W3_NL0"
#endif

#ifdef W3_NL1
  logical, parameter ::  w3_nl1_flag = .true.             !< @public a flag for "W3_NL1"
#else
  logical, parameter ::  w3_nl1_flag = .false.            !< @public a flag for "W3_NL1"
#endif

#ifdef W3_NL2
  logical, parameter ::  w3_nl2_flag = .true.             !< @public a flag for "W3_NL2"
#else
  logical, parameter ::  w3_nl2_flag = .false.            !< @public a flag for "W3_NL2"
#endif

#ifdef W3_NL3
  logical, parameter ::  w3_nl3_flag = .true.             !< @public a flag for "W3_NL3"
#else
  logical, parameter ::  w3_nl3_flag = .false.            !< @public a flag for "W3_NL3"
#endif

#ifdef W3_NL4
  logical, parameter ::  w3_nl4_flag = .true.             !< @public a flag for "W3_NL4"
#else
  logical, parameter ::  w3_nl4_flag = .false.            !< @public a flag for "W3_NL4"
#endif

#ifdef W3_NL5
  logical, parameter ::  w3_nl5_flag = .true.             !< @public a flag for "W3_NL5"
#else
  logical, parameter ::  w3_nl5_flag = .false.            !< @public a flag for "W3_NL5"
#endif

  !   bottom friction

#ifdef W3_BT0
  logical, parameter ::  w3_bt0_flag = .true.             !< @public a flag for "W3_BT0"
#else
  logical, parameter ::  w3_bt0_flag = .false.            !< @public a flag for "W3_BT0"
#endif

#ifdef W3_BT1
  logical, parameter ::  w3_bt1_flag = .true.             !< @public a flag for "W3_BT1"
#else
  logical, parameter ::  w3_bt1_flag = .false.            !< @public a flag for "W3_BT1"
#endif

#ifdef W3_B24
  logical, parameter ::  w3_b24_flag = .true.             !< @public a flag for "W3_B24"
#else
  logical, parameter ::  w3_b24_flag = .false.            !< @public a flag for "W3_B24"
#endif

#ifdef W3_BT8
  logical, parameter ::  w3_bt8_flag = .true.             !< @public a flag for "W3_BT8"
#else
  logical, parameter ::  w3_bt8_flag = .false.            !< @public a flag for "W3_BT8"
#endif

#ifdef W3_BT9
  logical, parameter ::  w3_bt9_flag = .true.             !< @public a flag for "W3_BT9"
#else
  logical, parameter ::  w3_bt9_flag = .false.            !< @public a flag for "W3_BT9"
#endif

  !   damping by sea ice

#ifdef W3_IC0
  logical, parameter ::  w3_ic0_flag = .true.             !< @public a flag for "W3_IC0"
#else
  logical, parameter ::  w3_ic0_flag = .false.            !< @public a flag for "W3_IC0"
#endif

#ifdef W3_IC1
  logical, parameter ::  w3_ic1_flag = .true.             !< @public a flag for "W3_IC1"
#else
  logical, parameter ::  w3_ic1_flag = .false.            !< @public a flag for "W3_IC1"
#endif

#ifdef W3_IC2
  logical, parameter ::  w3_ic2_flag = .true.             !< @public a flag for "W3_IC2"
#else
  logical, parameter ::  w3_ic2_flag = .false.            !< @public a flag for "W3_IC2"
#endif

#ifdef W3_IC3
  logical, parameter ::  w3_ic3_flag = .true.             !< @public a flag for "W3_IC3"
#else
  logical, parameter ::  w3_ic3_flag = .false.            !< @public a flag for "W3_IC3"
#endif

#ifdef W3_IC4
  logical, parameter ::  w3_ic4_flag = .true.             !< @public a flag for "W3_IC4"
#else
  logical, parameter ::  w3_ic4_flag = .false.            !< @public a flag for "W3_IC4"
#endif

#ifdef W3_IC5
  logical, parameter ::  w3_ic5_flag = .true.             !< @public a flag for "W3_IC5"
#else
  logical, parameter ::  w3_ic5_flag = .false.            !< @public a flag for "W3_IC5"
#endif

  !   scattering by seaice

#ifdef W3_IS0
  logical, parameter ::  w3_is0_flag = .true.             !< @public a flag for "W3_IS0"
#else
  logical, parameter ::  w3_is0_flag = .false.            !< @public a flag for "W3_IS0"
#endif

#ifdef W3_IS1
  logical, parameter ::  w3_is1_flag = .true.             !< @public a flag for "W3_IS1"
#else
  logical, parameter ::  w3_is1_flag = .false.            !< @public a flag for "W3_IS1"
#endif

#ifdef W3_IS2
  logical, parameter ::  w3_is2_flag = .true.             !< @public a flag for "W3_IS2"
#else
  logical, parameter ::  w3_is2_flag = .false.            !< @public a flag for "W3_IS2"
#endif

  !   reflection

#ifdef W3_REF0
  logical, parameter ::  w3_ref0_flag = .true.            !< @public a flag for "W3_REF0"
#else
  logical, parameter ::  w3_ref0_flag = .false.           !< @public a flag for "W3_REF0"
#endif

#ifdef W3_REF1
  logical, parameter ::  w3_ref1_flag = .true.            !< @public a flag for "W3_REF1"
#else
  logical, parameter ::  w3_ref1_flag = .false.           !< @public a flag for "W3_REF1"
#endif

  !   depth induced breaking

#ifdef W3_DB0
  logical, parameter ::  w3_db0_flag = .true.             !< @public a flag for "W3_DB0"
#else
  logical, parameter ::  w3_db0_flag = .false.            !< @public a flag for "W3_DB0"
#endif

#ifdef W3_DB1
  logical, parameter ::  w3_db1_flag = .true.             !< @public a flag for "W3_DB1"
#else
  logical, parameter ::  w3_db1_flag = .false.            !< @public a flag for "W3_DB1"
#endif

  !   tidal interaction

#ifdef W3_TR0
  logical, parameter ::  w3_tr0_flag = .true.             !< @public a flag for "W3_TR0"
#else
  logical, parameter ::  w3_tr0_flag = .false.            !< @public a flag for "W3_TR0"
#endif

#ifdef W3_TR1
  logical, parameter ::  w3_tr1_flag = .true.             !< @public a flag for "W3_TR1"
#else
  logical, parameter ::  w3_tr1_flag = .false.            !< @public a flag for "W3_TR1"
#endif

  !   bottom scattering

#ifdef W3_BS0
  logical, parameter ::  w3_bs0_flag = .true.             !< @public a flag for "W3_BS0"
#else
  logical, parameter ::  w3_bs0_flag = .false.            !< @public a flag for "W3_BS0"
#endif

#ifdef W3_BS1
  logical, parameter ::  w3_bs1_flag = .true.             !< @public a flag for "W3_BS1"
#else
  logical, parameter ::  w3_bs1_flag = .false.            !< @public a flag for "W3_BS1"
#endif

  !   wind interpolation in time

#ifdef W3_WNT0
  logical, parameter ::  w3_wnt0_flag = .true.            !< @public a flag for "W3_WNT0"
#else
  logical, parameter ::  w3_wnt0_flag = .false.           !< @public a flag for "W3_WNT0"
#endif

#ifdef W3_WNT1
  logical, parameter ::  w3_wnt1_flag = .true.            !< @public a flag for "W3_WNT1"
#else
  logical, parameter ::  w3_wnt1_flag = .false.           !< @public a flag for "W3_WNT1"
#endif

#ifdef W3_WNT2
  logical, parameter ::  w3_wnt2_flag = .true.            !< @public a flag for "W3_WNT2"
#else
  logical, parameter ::  w3_wnt2_flag = .false.           !< @public a flag for "W3_WNT2"
#endif

  !   wind interpolation in space

#ifdef W3_WNX0
  logical, parameter ::  w3_wnx0_flag = .true.            !< @public a flag for "W3_WNX0"
#else
  logical, parameter ::  w3_wnx0_flag = .false.           !< @public a flag for "W3_WNX0"
#endif

#ifdef W3_WNX1
  logical, parameter ::  w3_wnx1_flag = .true.            !< @public a flag for "W3_WNX1"
#else
  logical, parameter ::  w3_wnx1_flag = .false.           !< @public a flag for "W3_WNX1"
#endif

#ifdef W3_WNX2
  logical, parameter ::  w3_wnx2_flag = .true.            !< @public a flag for "W3_WNX2"
#else
  logical, parameter ::  w3_wnx2_flag = .false.           !< @public a flag for "W3_WNX2"
#endif

  !   current interpolation in time

#ifdef W3_CRT0
  logical, parameter ::  w3_crt0_flag = .true.            !< @public a flag for "W3_CRT0"
#else
  logical, parameter ::  w3_crt0_flag = .false.           !< @public a flag for "W3_CRT0"
#endif

#ifdef W3_CRT1
  logical, parameter ::  w3_crt1_flag = .true.            !< @public a flag for "W3_CRT1"
#else
  logical, parameter ::  w3_crt1_flag = .false.           !< @public a flag for "W3_CRT1"
#endif

#ifdef W3_CRT2
  logical, parameter ::  w3_crt2_flag = .true.            !< @public a flag for "W3_CRT2"
#else
  logical, parameter ::  w3_crt2_flag = .false.           !< @public a flag for "W3_CRT2"
#endif

  !   current interpolation in space

#ifdef W3_CRX0
  logical, parameter ::  w3_crx0_flag = .true.            !< @public a flag for "W3_CRX0"
#else
  logical, parameter ::  w3_crx0_flag = .false.           !< @public a flag for "W3_CRX0"
#endif

#ifdef W3_CRX1
  logical, parameter ::  w3_crx1_flag = .true.            !< @public a flag for "W3_CRX1"
#else
  logical, parameter ::  w3_crx1_flag = .false.           !< @public a flag for "W3_CRX1"
#endif

#ifdef W3_CRX2
  logical, parameter ::  w3_crx2_flag = .true.            !< @public a flag for "W3_CRX2"
#else
  logical, parameter ::  w3_crx2_flag = .false.           !< @public a flag for "W3_CRX2"
#endif

  !   grib

#ifdef W3_NOGRB
  logical, parameter ::  w3_nogrb_flag = .true.           !< @public a flag for "W3_NOGRB"
#else
  logical, parameter ::  w3_nogrb_flag = .false.          !< @public a flag for "W3_NOGRB"
#endif

#ifdef W3_NCEP1
  logical, parameter ::  w3_ncep1_flag = .true.           !< @public a flag for "W3_NCEP1"
#else
  logical, parameter ::  w3_ncep1_flag = .false.          !< @public a flag for "W3_NCEP1"
#endif

#ifdef W3_NCEP2
  logical, parameter ::  w3_ncep2_flag = .true.           !< @public a flag for "W3_NCEP2"
#else
  logical, parameter ::  w3_ncep2_flag = .false.          !< @public a flag for "W3_NCEP2"
#endif

  !   optional output

#ifdef W3_O0
  logical, parameter ::  w3_o0_flag = .true.              !< @public a flag for "W3_O0"
#else
  logical, parameter ::  w3_o0_flag = .false.             !< @public a flag for "W3_O0"
#endif

#ifdef W3_O1
  logical, parameter ::  w3_o1_flag = .true.              !< @public a flag for "W3_O1"
#else
  logical, parameter ::  w3_o1_flag = .false.             !< @public a flag for "W3_O1"
#endif

#ifdef W3_O2
  logical, parameter ::  w3_o2_flag = .true.              !< @public a flag for "W3_O2"
#else
  logical, parameter ::  w3_o2_flag = .false.             !< @public a flag for "W3_O2"
#endif

#ifdef W3_O3
  logical, parameter ::  w3_o3_flag = .true.              !< @public a flag for "W3_O3"
#else
  logical, parameter ::  w3_o3_flag = .false.             !< @public a flag for "W3_O3"
#endif

#ifdef W3_O4
  logical, parameter ::  w3_o4_flag = .true.              !< @public a flag for "W3_O4"
#else
  logical, parameter ::  w3_o4_flag = .false.             !< @public a flag for "W3_O4"
#endif

#ifdef W3_O5
  logical, parameter ::  w3_o5_flag = .true.              !< @public a flag for "W3_O5"
#else
  logical, parameter ::  w3_o5_flag = .false.             !< @public a flag for "W3_O5"
#endif

#ifdef W3_O6
  logical, parameter ::  w3_o6_flag = .true.              !< @public a flag for "W3_O6"
#else
  logical, parameter ::  w3_o6_flag = .false.             !< @public a flag for "W3_O6"
#endif

#ifdef W3_O7
  logical, parameter ::  w3_o7_flag = .true.              !< @public a flag for "W3_O7"
#else
  logical, parameter ::  w3_o7_flag = .false.             !< @public a flag for "W3_O7"
#endif

#ifdef W3_O8
  logical, parameter ::  w3_o8_flag = .true.              !< @public a flag for "W3_O8"
#else
  logical, parameter ::  w3_o8_flag = .false.             !< @public a flag for "W3_O8"
#endif

#ifdef W3_O9
  logical, parameter ::  w3_o9_flag = .true.              !< @public a flag for "W3_O9"
#else
  logical, parameter ::  w3_o9_flag = .false.             !< @public a flag for "W3_O9"
#endif

#ifdef W3_O10
  logical, parameter ::  w3_o10_flag = .true.             !< @public a flag for "W3_O10"
#else
  logical, parameter ::  w3_o10_flag = .false.            !< @public a flag for "W3_O10"
#endif

#ifdef W3_O11
  logical, parameter ::  w3_o11_flag = .true.             !< @public a flag for "W3_O11"
#else
  logical, parameter ::  w3_o11_flag = .false.            !< @public a flag for "W3_O11"
#endif

#ifdef W3_O12
  logical, parameter ::  w3_o12_flag = .true.             !< @public a flag for "W3_O12"
#else
  logical, parameter ::  w3_o12_flag = .false.            !< @public a flag for "W3_O12"
#endif

#ifdef W3_O13
  logical, parameter ::  w3_o13_flag = .true.             !< @public a flag for "W3_O13"
#else
  logical, parameter ::  w3_o13_flag = .false.            !< @public a flag for "W3_O13"
#endif

#ifdef W3_O14
  logical, parameter ::  w3_o14_flag = .true.             !< @public a flag for "W3_O14"
#else
  logical, parameter ::  w3_o14_flag = .false.            !< @public a flag for "W3_O14"
#endif

#ifdef W3_O15
  logical, parameter ::  w3_o15_flag = .true.             !< @public a flag for "W3_O15"
#else
  logical, parameter ::  w3_o15_flag = .false.            !< @public a flag for "W3_O15"
#endif

#ifdef W3_O16
  logical, parameter ::  w3_o16_flag = .true.             !< @public a flag for "W3_O16"
#else
  logical, parameter ::  w3_o16_flag = .false.            !< @public a flag for "W3_O16"
#endif

  !   threading

#ifdef W3_OMPG
  logical, parameter ::  w3_ompg_flag = .true.            !< @public a flag for "W3_OMPG"
#else
  logical, parameter ::  w3_ompg_flag = .false.           !< @public a flag for "W3_OMPG"
#endif

#ifdef W3_OMPH
  logical, parameter ::  w3_omph_flag = .true.            !< @public a flag for "W3_OMPH"
#else
  logical, parameter ::  w3_omph_flag = .false.           !< @public a flag for "W3_OMPH"
#endif

#ifdef W3_PDLIB
  logical, parameter ::  w3_pdlib_flag = .true.           !< @public a flag for "W3_PDLIB"
#else
  logical, parameter ::  w3_pdlib_flag = .false.          !< @public a flag for "W3_PDLIB"
#endif

#ifdef W3_B4B
  logical, parameter ::  w3_b4b_flag = .true.             !< @public a flag for "W3_B4B"
#else
  logical, parameter ::  w3_b4b_flag = .false.            !< @public a flag for "W3_B4B"
#endif

  !   moving grids

#ifdef W3_MGP
  logical, parameter ::  w3_mgp_flag = .true.             !< @public a flag for "W3_MGP"
#else
  logical, parameter ::  w3_mgp_flag = .false.            !< @public a flag for "W3_MGP"
#endif

#ifdef W3_MGW
  logical, parameter ::  w3_mgw_flag = .true.             !< @public a flag for "W3_MGW"
#else
  logical, parameter ::  w3_mgw_flag = .false.            !< @public a flag for "W3_MGW"
#endif

#ifdef W3_MGG
  logical, parameter ::  w3_mgg_flag = .true.             !< @public a flag for "W3_MGG"
#else
  logical, parameter ::  w3_mgg_flag = .false.            !< @public a flag for "W3_MGG"
#endif

  !   misc

#ifdef W3_COU
  logical, parameter ::  w3_cou_flag = .true.             !< @public a flag for "W3_COU"
#else
  logical, parameter ::  w3_cou_flag = .false.            !< @public a flag for "W3_COU"
#endif

#ifdef W3_DSS0
  logical, parameter ::  w3_dss0_flag = .true.            !< @public a flag for "W3_DSS0"
#else
  logical, parameter ::  w3_dss0_flag = .false.           !< @public a flag for "W3_DSS0"
#endif

#ifdef W3_FLD1
  logical, parameter ::  w3_fld1_flag = .true.            !< @public a flag for "W3_FLD1"
#else
  logical, parameter ::  w3_fld1_flag = .false.           !< @public a flag for "W3_FLD1"
#endif

#ifdef W3_FLD2
  logical, parameter ::  w3_fld2_flag = .true.            !< @public a flag for "W3_FLD2"
#else
  logical, parameter ::  w3_fld2_flag = .false.           !< @public a flag for "W3_FLD2"
#endif

#ifdef W3_IG1
  logical, parameter ::  w3_ig1_flag = .true.             !< @public a flag for "W3_IG1"
#else
  logical, parameter ::  w3_ig1_flag = .false.            !< @public a flag for "W3_IG1"
#endif

#ifdef W3_MLIM
  logical, parameter ::  w3_mlim_flag = .true.            !< @public a flag for "W3_MLIM"
#else
  logical, parameter ::  w3_mlim_flag = .false.           !< @public a flag for "W3_MLIM"
#endif

#ifdef W3_MPI
  logical, parameter ::  w3_mpi_flag = .true.             !< @public a flag for "W3_MPI"
#else
  logical, parameter ::  w3_mpi_flag = .false.            !< @public a flag for "W3_MPI"
#endif

#ifdef W3_MPIBDI
  logical, parameter ::  w3_mpibdi_flag = .true.          !< @public a flag for "W3_MPIBDI"
#else
  logical, parameter ::  w3_mpibdi_flag = .false.         !< @public a flag for "W3_MPIBDI"
#endif

#ifdef W3_MPIT
  logical, parameter ::  w3_mpit_flag = .true.            !< @public a flag for "W3_MPIT"
#else
  logical, parameter ::  w3_mpit_flag = .false.           !< @public a flag for "W3_MPIT"
#endif

#ifdef W3_MPRF
  logical, parameter ::  w3_mprf_flag = .true.            !< @public a flag for "W3_MPRF"
#else
  logical, parameter ::  w3_mprf_flag = .false.           !< @public a flag for "W3_MPRF"
#endif

#ifdef W3_NCO
  logical, parameter ::  w3_nco_flag = .true.             !< @public a flag for "W3_NCO"
#else
  logical, parameter ::  w3_nco_flag = .false.            !< @public a flag for "W3_NCO"
#endif

#ifdef W3_NLS
  logical, parameter ::  w3_nls_flag = .true.             !< @public a flag for "W3_NLS"
#else
  logical, parameter ::  w3_nls_flag = .false.            !< @public a flag for "W3_NLS"
#endif

#ifdef W3_NNT
  logical, parameter ::  w3_nnt_flag = .true.             !< @public a flag for "W3_NNT"
#else
  logical, parameter ::  w3_nnt_flag = .false.            !< @public a flag for "W3_NNT"
#endif

#ifdef W3_OASIS
  logical, parameter ::  w3_oasis_flag = .true.           !< @public a flag for "W3_OASIS"
#else
  logical, parameter ::  w3_oasis_flag = .false.          !< @public a flag for "W3_OASIS"
#endif

#ifdef W3_OASACM
  logical, parameter ::  w3_oasacm_flag = .true.          !< @public a flag for "W3_OASACM"
#else
  logical, parameter ::  w3_oasacm_flag = .false.         !< @public a flag for "W3_OASACM"
#endif

#ifdef W3_OASOCM
  logical, parameter ::  w3_oasocm_flag = .true.          !< @public a flag for "W3_OASOCM"
#else
  logical, parameter ::  w3_oasocm_flag = .false.         !< @public a flag for "W3_OASOCM"
#endif

#ifdef W3_OASICM
  logical, parameter ::  w3_oasicm_flag = .true.          !< @public a flag for "W3_OASICM"
#else
  logical, parameter ::  w3_oasicm_flag = .false.         !< @public a flag for "W3_OASICM"
#endif

#ifdef W3_REFRX
  logical, parameter ::  w3_refrx_flag = .true.           !< @public a flag for "W3_REFRX"
#else
  logical, parameter ::  w3_refrx_flag = .false.          !< @public a flag for "W3_REFRX"
#endif

#ifdef W3_REFT
  logical, parameter ::  w3_reft_flag = .true.            !< @public a flag for "W3_REFT"
#else
  logical, parameter ::  w3_reft_flag = .false.           !< @public a flag for "W3_REFT"
#endif

#ifdef W3_RTD
  logical, parameter ::  w3_rtd_flag = .true.             !< @public a flag for "W3_RTD"
#else
  logical, parameter ::  w3_rtd_flag = .false.            !< @public a flag for "W3_RTD"
#endif

#ifdef W3_RWND
  logical, parameter ::  w3_rwnd_flag = .true.            !< @public a flag for "W3_RWND"
#else
  logical, parameter ::  w3_rwnd_flag = .false.           !< @public a flag for "W3_RWND"
#endif

#ifdef W3_S
  logical, parameter ::  w3_s_flag = .true.               !< @public a flag for "W3_S"
#else
  logical, parameter ::  w3_s_flag = .false.              !< @public a flag for "W3_S"
#endif

#ifdef W3_SCRIP
  logical, parameter ::  w3_scrip_flag = .true.           !< @public a flag for "W3_SCRIP"
#else
  logical, parameter ::  w3_scrip_flag = .false.          !< @public a flag for "W3_SCRIP"
#endif

#ifdef W3_SCRIPNC
  logical, parameter ::  w3_scripnc_flag = .true.         !< @public a flag for "W3_SCRIPNC"
#else
  logical, parameter ::  w3_scripnc_flag = .false.        !< @public a flag for "W3_SCRIPNC"
#endif

#ifdef W3_SEC1
  logical, parameter ::  w3_sec1_flag = .true.            !< @public a flag for "W3_SEC1"
#else
  logical, parameter ::  w3_sec1_flag = .false.           !< @public a flag for "W3_SEC1"
#endif

#ifdef W3_SMC
  logical, parameter ::  w3_smc_flag = .true.             !< @public a flag for "W3_SMC"
#else
  logical, parameter ::  w3_smc_flag = .false.            !< @public a flag for "W3_SMC"
#endif

#ifdef W3_T
  logical, parameter ::  w3_t_flag = .true.               !< @public a flag for "W3_T"
#else
  logical, parameter ::  w3_t_flag = .false.              !< @public a flag for "W3_T"
#endif
#ifdef W3_T0
  logical, parameter ::  w3_t0_flag = .true.              !< @public a flag for "W3_T0"
#else
  logical, parameter ::  w3_t0_flag = .false.             !< @public a flag for "W3_T0"
#endif

#ifdef W3_T1
  logical, parameter ::  w3_t1_flag = .true.              !< @public a flag for "W3_T1"
#else
  logical, parameter ::  w3_t1_flag = .false.             !< @public a flag for "W3_T1"
#endif

#ifdef W3_T2
  logical, parameter ::  w3_t2_flag = .true.              !< @public a flag for "W3_T2"
#else
  logical, parameter ::  w3_t2_flag = .false.             !< @public a flag for "W3_T2"
#endif
#ifdef W3_T3
  logical, parameter ::  w3_t3_flag = .true.              !< @public a flag for "W3_T3"
#else
  logical, parameter ::  w3_t3_flag = .false.             !< @public a flag for "W3_T3"
#endif
#ifdef W3_T4
  logical, parameter ::  w3_t4_flag = .true.              !< @public a flag for "W3_T4"
#else
  logical, parameter ::  w3_t4_flag = .false.             !< @public a flag for "W3_T4"
#endif
#ifdef W3_T5
  logical, parameter ::  w3_t5_flag = .true.              !< @public a flag for "W3_T5"
#else
  logical, parameter ::  w3_t5_flag = .false.             !< @public a flag for "W3_T5"
#endif
#ifdef W3_T6
  logical, parameter ::  w3_t6_flag = .true.              !< @public a flag for "W3_T6"
#else
  logical, parameter ::  w3_t6_flag = .false.             !< @public a flag for "W3_T6"
#endif
#ifdef W3_T7
  logical, parameter ::  w3_t7_flag = .true.              !< @public a flag for "W3_T7"
#else
  logical, parameter ::  w3_t7_flag = .false.             !< @public a flag for "W3_T7"
#endif
#ifdef W3_T8
  logical, parameter ::  w3_t8_flag = .true.              !< @public a flag for "W3_T8"
#else
  logical, parameter ::  w3_t8_flag = .false.             !< @public a flag for "W3_T8"
#endif
#ifdef W3_T9
  logical, parameter ::  w3_t9_flag = .true.              !< @public a flag for "W3_T9"
#else
  logical, parameter ::  w3_t9_flag = .false.             !< @public a flag for "W3_T9"
#endif
#ifdef W3_T38
  logical, parameter ::  w3_t38_flag = .true.             !< @public a flag for "W3_T38"
#else
  logical, parameter ::  w3_t38_flag = .false.            !< @public a flag for "W3_T38"
#endif
#ifdef W3_TDYN
  logical, parameter ::  w3_tdyn_flag = .true.            !< @public a flag for "W3_TDYN"
#else
  logical, parameter ::  w3_tdyn_flag = .false.           !< @public a flag for "W3_TDYN"
#endif

#ifdef W3_TIDE
  logical, parameter ::  w3_tide_flag = .true.            !< @public a flag for "W3_TIDE"
#else
  logical, parameter ::  w3_tide_flag = .false.           !< @public a flag for "W3_TIDE"
#endif

#ifdef W3_TIDET
  logical, parameter ::  w3_tidet_flag = .true.           !< @public a flag for "W3_TIDET"
#else
  logical, parameter ::  w3_tidet_flag = .false.          !< @public a flag for "W3_TIDET"
#endif

#ifdef W3_TRKNC
  logical, parameter ::  w3_trknc_flag = .true.           !< @public a flag for "W3_TRKNC"
#else
  logical, parameter ::  w3_trknc_flag = .false.          !< @public a flag for "W3_TRKNC"
#endif

#ifdef W3_UOST
  logical, parameter ::  w3_uost_flag = .true.            !< @public a flag for "W3_UOST"
#else
  logical, parameter ::  w3_uost_flag = .false.           !< @public a flag for "W3_UOST"
#endif

#ifdef W3_WRST
  logical, parameter ::  w3_wrst_flag = .true.            !< @public a flag for "W3_WRST"
#else
  logical, parameter ::  w3_wrst_flag = .false.           !< @public a flag for "W3_WRST"
#endif

#ifdef W3_XW0
  logical, parameter ::  w3_xw0_flag = .true.             !< @public a flag for "W3_XW0"
#else
  logical, parameter ::  w3_xw0_flag = .false.            !< @public a flag for "W3_XW0"
#endif

#ifdef W3_XW1
  logical, parameter ::  w3_xw1_flag = .true.             !< @public a flag for "W3_XW1"
#else
  logical, parameter ::  w3_xw1_flag = .false.            !< @public a flag for "W3_XW1"
#endif

#ifdef W3_CESMCOUPLED
  logical, parameter :: w3_cesmcoupled_flag = .true.      !< @public a flag for "W3_CESMCOUPLED"
#else
  logical, parameter :: w3_cesmcoupled_flag = .false.     !< @public a flag for "W3_CESMCOUPLED"
#endif

#ifdef W3_UWM
  logical, parameter ::  w3_uwm_flag = .true.             !< @public a flag for "W3_UWM"
#else
  logical, parameter ::  w3_uwm_flag = .false.            !< @public a flag for "W3_UWM"
#endif

#ifdef W3_SBS
  logical, parameter ::  w3_sbs_flag = .true.             !< @public a flag for "W3_SBS"
#else
  logical, parameter ::  w3_sbs_flag = .false.            !< @public a flag for "W3_SBS"
#endif

#ifdef W3_BT4
  logical, parameter ::  w3_bt4_flag = .true.             !< @public a flag for "W3_BT4"
#else
  logical, parameter ::  w3_bt4_flag = .false.            !< @public a flag for "W3_BT4"
#endif

#ifdef W3_WCOR
  logical, parameter ::  w3_wcor_flag = .true.            !< @public a flag for "W3_WCOR"
#else
  logical, parameter ::  w3_wcor_flag = .false.           !< @public a flag for "W3_WCOR"
#endif

#ifdef W3_SETUP
  logical, parameter ::  w3_setup_flag = .true.           !< @public a flag for "W3_SETUP"
#else
  logical, parameter ::  w3_setup_flag = .false.          !< @public a flag for "W3_SETUP"
#endif

#ifdef W3_O2A
  logical, parameter ::  w3_O2a_flag = .true.             !< @public a flag for "W3_O2A"
#else
  logical, parameter ::  w3_O2a_flag = .false.            !< @public a flag for "W3_O2A"
#endif

#ifdef W3_O2B
  logical, parameter ::  w3_O2b_flag = .true.             !< @public a flag for "W3_O2B"
#else
  logical, parameter ::  w3_O2b_flag = .false.            !< @public a flag for "W3_O2B"
#endif
#ifdef W3_O2C
  logical, parameter ::  w3_O2c_flag = .true.             !< @public a flag for "W3_O2C"
#else
  logical, parameter ::  w3_O2c_flag = .false.            !< @public a flag for "W3_O2C"
#endif
#ifdef W3_O7A
  logical, parameter ::  w3_O7a_flag = .true.             !< @public a flag for "W3_O7A"
#else
  logical, parameter ::  w3_O7a_flag = .false.            !< @public a flag for "W3_O7A"
#endif
#ifdef W3_O7B
  logical, parameter ::  w3_O7b_flag = .true.             !< @public a flag for "W3_O7B"
#else
  logical, parameter ::  w3_O7b_flag = .false.            !< @public a flag for "W3_O7B"
#endif
#ifdef W3_01
  logical, parameter ::  w3_01_flag = .true.              !< @public a flag for "W3_01"
#else
  logical, parameter ::  w3_01_flag = .false.             !< @public a flag for "W3_01"
#endif

  interface print_logmsg
    module procedure print_logmsg_1line
    module procedure print_logmsg_2line
    module procedure print_logmsg_3line
    module procedure print_logmsg_4line
  end interface print_logmsg

contains

  !========================================================================
  !> Write a 1 line message if requested
  !!
  !> @details Writes a one line message
  !!
  !! @param[in]   unum               unit number
  !! @param[in]   msg1               one line message
  !! @param[in]   lwrite             logical to control message writing
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 06-01-2022
  subroutine print_logmsg_1line(unum, msg1, lwrite)

    integer         , intent(in)           :: unum
    character(len=*), intent(in)           :: msg1
    logical         , intent(in)           :: lwrite

    if (.not. lwrite) return

    write(unum,'(a)') trim(msg1)
    flush(unum)

  end subroutine print_logmsg_1line

  !========================================================================
  !> Write a 2 line message if requested
  !!
  !> @details Writes a two line message
  !!
  !! @param[in]   unum               unit number
  !! @param[in]   msg1               first line of message
  !! @param[in]   msg2               second line of message
  !! @param[in]   lwrite             logical to control message writing
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 06-01-2022
  subroutine print_logmsg_2line(unum, msg1, msg2, lwrite)

    integer         , intent(in)           :: unum
    character(len=*), intent(in)           :: msg1
    character(len=*), intent(in)           :: msg2
    logical         , intent(in)           :: lwrite

    if (.not. lwrite) return

    write(unum,'(a)') trim(msg1)
    write(unum,'(a)') trim(msg2)
    flush(unum)

  end subroutine print_logmsg_2line

  !========================================================================
  !> Write a 3 line message if requested
  !!
  !> @details Writes a three line message
  !!
  !! @param[in]   unum               unit number
  !! @param[in]   msg1               first line of message
  !! @param[in]   msg2               second line of message
  !! @param[in]   msg3               third line of message
  !! @param[in]   lwrite             logical to control message writing
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 06-01-2022
  subroutine print_logmsg_3line(unum, msg1, msg2, msg3, lwrite)

    integer         , intent(in)           :: unum
    character(len=*), intent(in)           :: msg1
    character(len=*), intent(in)           :: msg2
    character(len=*), intent(in)           :: msg3
    logical         , intent(in)           :: lwrite

    if (.not. lwrite) return

    write(unum,'(a)') trim(msg1)
    write(unum,'(a)') trim(msg2)
    write(unum,'(a)') trim(msg3)
    flush(unum)

  end subroutine print_logmsg_3line

  !========================================================================
  !> Write a 4 line message if requested
  !!
  !> @details Writes a four line message
  !!
  !! @param[in]   unum               unit number
  !! @param[in]   msg1               first line of message
  !! @param[in]   msg2               second line of message
  !! @param[in]   msg3               third line of message
  !! @param[in]   msg4               forth line of message
  !! @param[in]   lwrite             logical to control message writing
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 06-01-2022

  subroutine print_logmsg_4line(unum, msg1, msg2, msg3, msg4, lwrite)

    integer         , intent(in)           :: unum
    character(len=*), intent(in)           :: msg1
    character(len=*), intent(in)           :: msg2
    character(len=*), intent(in)           :: msg3
    character(len=*), intent(in)           :: msg4
    logical         , intent(in)           :: lwrite

    if (.not. lwrite) return

    write(unum,'(a)') trim(msg1)
    write(unum,'(a)') trim(msg2)
    write(unum,'(a)') trim(msg3)
    write(unum,'(a)') trim(msg4)
    flush(unum)

  end subroutine print_logmsg_4line
end module wav_shr_flags
