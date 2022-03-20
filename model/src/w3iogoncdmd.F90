#include "w3macros.h"

module W3IOGONCDMD

  USE W3GDATMD      , ONLY: NK, NX, NY, MAPSF, MAPSTA, NSEA
  USE W3ODATMD      , ONLY: NOSWLL, UNDEF
  use w3odatmd      , only : nds, iaproc, napout
  use wav_shr_mod   , only : dbug_flag
  USE NETCDF

  implicit none

  private

  public :: w3iogoncd

  ! used/reused in module
 
  integer :: isea, ierr, ncid, varid
  integer :: len_s, len_b, len_m, len_p, len_k

contains

!/ ------------------------------------------------------------------- /
  subroutine W3IOGONCD ()

    ! Write netcdf ww3 history output

    USE CONSTANTS
    USE W3WDATMD, ONLY: W3SETW, W3DIMW, TIME, WLV, ICE, ICEF, ICEH, BERG, UST, USTDIR, ASF, RHOAIR
    USE W3GDATMD, ONLY: E3DF, P2MSF, US3DF, USSPF, W3SETG
    USE W3ODATMD, ONLY: NOGRP, NGRPP, IDOUT, NDST, NDSE,  FLOGRD, NOSWLL, W3SETO
    USE W3ADATMD, ONLY: W3SETA, W3DIMA, W3XETA
    USE W3ADATMD, ONLY: AINIT, DW, UA, UD, AS, CX, CY, WN, TAUA, TAUADIR
    USE W3ADATMD, ONLY: HS, WLM, T02, T0M1, T01, FP0, THM, THS, THP0, WBT, WNMEAN
    USE W3ADATMD, ONLY: FP1, THP1, DTDYN
    USE W3ADATMD, ONLY: FCUT, ABA, ABD, UBA, UBD, SXX, SYY, SXY
    USE W3ADATMD, ONLY: PHS, PTP, PLP, PDIR, PSI, PWS, PWST, PNR
    USE W3ADATMD, ONLY: PTHP0, PQP, PPE, PGW, PSW, PTM1, PT1, PT2
    USE W3ADATMD, ONLY: PEP, USERO, TAUOX, TAUOY, TAUWIX, TAUWIY
    USE W3ADATMD, ONLY: PHIAW, PHIOC, TUSX, TUSY, PRMS, TPMS
    USE W3ADATMD, ONLY: USSX, USSY, MSSX, MSSY, MSSD, MSCX, MSCY
    USE W3ADATMD, ONLY: MSCD, QP, TAUWNX, TAUWNY, CHARN, TWS, BHD
    USE W3ADATMD, ONLY: PHIBBL, TAUBBL, WHITECAP, BEDFORMS, CGE, EF
    USE W3ADATMD, ONLY: CFLXYMAX, CFLTHMAX, CFLKMAX, P2SMS, US3D
    USE W3ADATMD, ONLY: TH1M, STH1M, TH2M, STH2M, HSIG, PHICE, TAUICE
    USE W3ADATMD, ONLY: STMAXE, STMAXD, HMAXE, HCMAXE, HMAXD, HCMAXD, USSP, TAUOCX, TAUOCY
#ifdef CESMCOUPLED
    USE W3ADATMD, ONLY: LANGMT
#endif
    use wav_grdout     , only: varatts, outvars
    use wav_shr_mod    , only: time_origin, calendar_name, elapsed_secs

!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
    INTEGER                  :: IGRD, I, J, IX, IY, IFI, IFJ
    integer    ,target       :: dimid3(3)
    integer    ,target       :: dimid4(4)
    integer    ,pointer      :: dimid(:)
    character(len=12)        :: vname
    CHARACTER(len=1024)      :: FNAME

    integer :: n, xtid, ytid, stid, btid, mtid, ptid, ktid, timid, varid
    logical :: s_axis = .false., b_axis = .false., m_axis = .false., p_axis = .false., k_axis = .false.

!/
!/ ------------------------------------------------------------------- /
!/
!
    IGRD   = 1
    CALL W3SETO ( IGRD, NDSE, NDST )
    CALL W3SETG ( IGRD, NDSE, NDST )
    CALL W3SETA ( IGRD, NDSE, NDST )  ! sets pointers into wadats in w3adatmd
    CALL W3XETA ( IGRD, NDSE, NDST )  ! sets pointers into wadats in w3adatmd
    CALL W3SETW ( IGRD, NDSE, NDST )  ! sets pointers into wdatas in w3wdatmd

    ! -------------------------------------------------------------
    ! Initialization
    ! -------------------------------------------------------------

    !TODO: remove flogrd dependence and use actual output variables
    ! this will remove dependence on list ordering/numbering
    DO ISEA=1, NSEA
       IF ( MAPSTA(MAPSF(ISEA,2),MAPSF(ISEA,1)) .LT. 0 ) THEN
          !
          IF ( FLOGRD( 2, 2) ) WLM   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 3) ) T02   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 4) ) T0M1  (ISEA) = UNDEF
          IF ( FLOGRD( 2, 5) ) T01   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 6) ) FP0   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 7) ) THM   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 8) ) THS   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 9) ) THP0  (ISEA) = UNDEF
                               UST   (ISEA) = UNDEF
                               USTDIR(ISEA) = UNDEF
          IF ( FLOGRD( 2,10) ) HSIG  (ISEA) = UNDEF
          IF ( FLOGRD( 2,11) ) STMAXE(ISEA) = UNDEF
          IF ( FLOGRD( 2,12) ) STMAXD(ISEA) = UNDEF
          IF ( FLOGRD( 2,13) ) HMAXE (ISEA) = UNDEF
          IF ( FLOGRD( 2,14) ) HCMAXE(ISEA) = UNDEF
          IF ( FLOGRD( 2,15) ) HMAXD (ISEA) = UNDEF
          IF ( FLOGRD( 2,16) ) HCMAXD(ISEA) = UNDEF
          IF ( FLOGRD( 2,17) ) WBT   (ISEA) = UNDEF
          !
          IF ( FLOGRD( 3, 1) ) EF   (ISEA,:) = UNDEF
          IF ( FLOGRD( 3, 2) ) TH1M (ISEA,:) = UNDEF
          IF ( FLOGRD( 3, 3) ) STH1M(ISEA,:) = UNDEF
          IF ( FLOGRD( 3, 4) ) TH2M (ISEA,:) = UNDEF
          IF ( FLOGRD( 3, 5) ) STH2M(ISEA,:) = UNDEF
          !
          IF ( FLOGRD( 4, 1) ) PHS (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 2) ) PTP (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 3) ) PLP (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 4) ) PDIR (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 5) ) PSI (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 6) ) PWS (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 7) ) PTHP0(ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 8) ) PQP (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 9) ) PPE(ISEA,:)  = UNDEF
          IF ( FLOGRD( 4,10) ) PGW(ISEA,:)  = UNDEF
          IF ( FLOGRD( 4,11) ) PSW (ISEA,:) = UNDEF
          IF ( FLOGRD( 4,12) ) PTM1(ISEA,:) = UNDEF
          IF ( FLOGRD( 4,13) ) PT1 (ISEA,:) = UNDEF
          IF ( FLOGRD( 4,14) ) PT2 (ISEA,:) = UNDEF
          IF ( FLOGRD( 4,15) ) PEP (ISEA,:) = UNDEF
          IF ( FLOGRD( 4,16) ) PWST(ISEA  ) = UNDEF
          IF ( FLOGRD( 4,17) ) PNR (ISEA  ) = UNDEF
          !
          IF ( FLOGRD( 5, 2) ) CHARN (ISEA) = UNDEF
          IF ( FLOGRD( 5, 3) ) CGE   (ISEA) = UNDEF
          IF ( FLOGRD( 5, 4) ) PHIAW (ISEA) = UNDEF
          IF ( FLOGRD( 5, 5) ) THEN
             TAUWIX(ISEA) = UNDEF
             TAUWIY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 5, 6) ) THEN
             TAUWNX(ISEA) = UNDEF
             TAUWNY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 5, 7) ) WHITECAP(ISEA,1) = UNDEF
          IF ( FLOGRD( 5, 8) ) WHITECAP(ISEA,2) = UNDEF
          IF ( FLOGRD( 5, 9) ) WHITECAP(ISEA,3) = UNDEF
          IF ( FLOGRD( 5,10) ) WHITECAP(ISEA,4) = UNDEF
          !
          IF ( FLOGRD( 6, 1) ) THEN
             SXX   (ISEA) = UNDEF
             SYY   (ISEA) = UNDEF
             SXY   (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 2) ) THEN
             TAUOX (ISEA) = UNDEF
             TAUOY (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 3) ) BHD(ISEA) = UNDEF
          IF ( FLOGRD( 6, 4) ) PHIOC (ISEA) = UNDEF
          IF ( FLOGRD( 6, 5) ) THEN
             TUSX  (ISEA) = UNDEF
             TUSY  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 6) ) THEN
             USSX  (ISEA) = UNDEF
             USSY  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 7) ) THEN
             PRMS  (ISEA) = UNDEF
             TPMS  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 8) ) US3D(ISEA,:) = UNDEF
          IF ( FLOGRD( 6, 9) ) P2SMS(ISEA,:) = UNDEF
          IF ( FLOGRD( 6, 10) ) TAUICE(ISEA,:) = UNDEF
          IF ( FLOGRD( 6, 11) ) PHICE(ISEA) = UNDEF
          IF ( FLOGRD( 6, 12) ) USSP(ISEA,:) = UNDEF
#ifdef CESMCOUPLED
          IF ( FLOGRD( 6, 14) ) LANGMT(ISEA) = UNDEF  !cesm specific
#endif
          !
          IF ( FLOGRD( 7, 1) ) THEN
             ABA   (ISEA) = UNDEF
             ABD   (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 7, 2) ) THEN
             UBA   (ISEA) = UNDEF
             UBD   (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 7, 3) ) BEDFORMS(ISEA,:) = UNDEF
          IF ( FLOGRD( 7, 4) ) PHIBBL(ISEA) = UNDEF
          IF ( FLOGRD( 7, 5) ) TAUBBL(ISEA,:) = UNDEF
          !
          IF ( FLOGRD( 8, 1) ) THEN
             MSSX  (ISEA) = UNDEF
             MSSY  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 8, 2) ) THEN
             MSCX  (ISEA) = UNDEF
             MSCY  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 8, 3) ) MSSD (ISEA) = UNDEF
          IF ( FLOGRD( 8, 4) ) MSCD (ISEA) = UNDEF
          IF ( FLOGRD( 8, 5) ) QP   (ISEA) = UNDEF
          !
          IF ( FLOGRD( 9, 1) ) DTDYN (ISEA) = UNDEF
          IF ( FLOGRD( 9, 2) ) FCUT  (ISEA) = UNDEF
          IF ( FLOGRD( 9, 3) ) CFLXYMAX(ISEA) = UNDEF
          IF ( FLOGRD( 9, 4) ) CFLTHMAX(ISEA) = UNDEF
          IF ( FLOGRD( 9, 5) ) CFLKMAX(ISEA) = UNDEF
          !
       END IF
       !
       IF ( MAPSTA(MAPSF(ISEA,2),MAPSF(ISEA,1)) == 2 ) THEN
          !
          IF ( FLOGRD( 5, 4) ) PHIAW (ISEA) = UNDEF
          IF ( FLOGRD( 5, 5) ) THEN
             TAUWIX(ISEA) = UNDEF
             TAUWIY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 5, 6) ) THEN
             TAUWNX(ISEA) = UNDEF
             TAUWNY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 5, 7) ) WHITECAP(ISEA,1) = UNDEF
          IF ( FLOGRD( 5, 8) ) WHITECAP(ISEA,2) = UNDEF
          IF ( FLOGRD( 5, 9) ) WHITECAP(ISEA,3) = UNDEF
          IF ( FLOGRD( 5,10) ) WHITECAP(ISEA,4) = UNDEF
          !
          IF ( FLOGRD( 6, 2) ) THEN
             TAUOX (ISEA) = UNDEF
             TAUOY (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 4) ) PHIOC (ISEA) = UNDEF
          !
          IF ( FLOGRD( 7, 3) ) BEDFORMS(ISEA,:) = UNDEF
          IF ( FLOGRD( 7, 4) ) PHIBBL(ISEA) = UNDEF
          IF ( FLOGRD( 7, 5) ) TAUBBL(ISEA,:) = UNDEF
       end IF
    END DO

    ! -------------------------------------------------------------
    ! Create the netcdf file
    ! -------------------------------------------------------------
    call hist_filename(fname)

    len_s = noswll + 1                  ! 0:noswll
    len_b = 3                           ! currently hardwired to 3 bedform variables
    len_m = P2MSF(3)-P2MSF(2) + 1       ! ?
    len_p = usspf(2)                    ! partitions
    len_k = e3df(3,1) - e3df(2,1) + 1   ! frequencies

    ! define the dimensions required for the requested gridded fields
    do n = 1,size(outvars)
       if (outvars(n)%validout) then
          if(scan(trim(outvars(n)%dims),'s') > 0)s_axis = .true.
          if(scan(trim(outvars(n)%dims),'b') > 0)b_axis = .true.
          if(scan(trim(outvars(n)%dims),'m') > 0)m_axis = .true.
          if(scan(trim(outvars(n)%dims),'p') > 0)p_axis = .true.
          if(scan(trim(outvars(n)%dims),'k') > 0)k_axis = .true.
       end if
     end do

    ierr = nf90_create(trim(fname), nf90_clobber, ncid)
    call handle_err(ierr, 'nf90_create')
    ierr = nf90_def_dim(ncid, 'nx', nx, xtid)
    ierr = nf90_def_dim(ncid, 'ny', ny, ytid)
    ierr = nf90_def_dim(ncid, 'time', nf90_unlimited, timid)

    if (s_axis) ierr = nf90_def_dim(ncid, 'noswll', len_s, stid)
    if (b_axis) ierr = nf90_def_dim(ncid, 'nb'    , len_b, btid)
    if (m_axis) ierr = nf90_def_dim(ncid, 'nm'    , len_m, mtid)
    if (p_axis) ierr = nf90_def_dim(ncid, 'np'    , len_p, ptid)
    if (k_axis) ierr = nf90_def_dim(ncid, 'freq'  , len_k, ktid)

    ! define time axis
    ierr = nf90_put_att(ncid, timid, 'units'   , trim(time_origin))
    ierr = nf90_put_att(ncid, timid, 'calendar', trim(calendar_name))

    ! define the variables
    dimid3(1:2) = (/xtid, ytid/)
    dimid4(1:2) = (/xtid, ytid/)
    do n = 1,size(outvars)
       if (scan(trim(outvars(n)%dims),'s') > 0) then
        dimid4(3:4) = (/stid, timid/)
        dimid => dimid4
       else if (scan(trim(outvars(n)%dims),'b') > 0) then
        dimid4(3:4) = (/btid, timid/)
        dimid => dimid4
       else if (scan(trim(outvars(n)%dims),'m') > 0) then
        dimid4(3:4) = (/mtid, timid/)
        dimid => dimid4
       else if (scan(trim(outvars(n)%dims),'p') > 0) then
        dimid4(3:4) = (/ptid, timid/)
        dimid => dimid4
       else if (scan(trim(outvars(n)%dims),'k') > 0) then
        dimid4(3:4) = (/ktid, timid/)
        dimid => dimid4
       else
        dimid3(3) = timid
        dimid => dimid3
       end if

       ierr = nf90_def_var(ncid, trim(outvars(n)%var_name), nf90_float, dimid, varid)
       call handle_err(ierr, 'define variable '//trim((outvars(n)%var_name)))
       ierr = nf90_put_att(ncid, varid, 'units'     , trim(outvars(n)%unit_name))
       ierr = nf90_put_att(ncid, varid, 'long_name' , trim(outvars(n)%long_name))
       ierr = nf90_put_att(ncid, varid, '_FillValue', undef)
     end do
       ierr = nf90_enddef(ncid)
       call handle_err(ierr, 'end variable definition')

     ! write the time axis
     ierr = nf90_put_var(ncid, timid, elapsed_secs)
     call handle_err(ierr, 'put time')
     ierr = nf90_close(ncid)

     !print *,'XXX thm ',minval(thm),maxval(thm)
     !print *,'XXX hsig ',minval(hsig),maxval(hsig)
     !print *,'XXX stmaxe ',minval(stmaxe),maxval(stmaxe)
     !print *,'XXX stmaxd ',minval(stmaxd),maxval(stmaxd)
     !print *,'XXX hmaxe ',minval(hmaxe),maxval(hmaxe)
     !print *,'XXX hcmaxe ',minval(hcmaxe),maxval(hcmaxe)
     !print *,'XXX hcmaxd ',minval(hcmaxd),maxval(hcmaxd)

     ! write the requested variables
     do n = 1,size(outvars)
      vname = trim(outvars(n)%var_name)

      ! Group 1
      if (vname .eq.      'DW') call write_var(trim(fname), vname, dw)
      if (vname .eq.      'CX') call write_var(trim(fname), vname, cx)
      if (vname .eq.      'CY') call write_var(trim(fname), vname, cy)
      if (vname .eq.     'UAX') call write_var(trim(fname), vname, ua, dir=cos(ud))
      if (vname .eq.     'UAY') call write_var(trim(fname), vname, ua, dir=sin(ud))
      if (vname .eq.      'AS') call write_var(trim(fname), vname, as)
      if (vname .eq.     'WLV') call write_var(trim(fname), vname, wlv)
      if (vname .eq.     'ICE') call write_var(trim(fname), vname, ice)
      if (vname .eq.    'BERG') call write_var(trim(fname), vname, berg)
      if (vname .eq.    'TAUX') call write_var(trim(fname), vname, taua, dir=cos(tauadir))
      if (vname .eq.    'TAUY') call write_var(trim(fname), vname, taua, dir=sin(tauadir))
      if (vname .eq.  'RHOAIR') call write_var(trim(fname), vname, rhoair)
      if (vname .eq.    'ICEH') call write_var(trim(fname), vname, iceh)
      if (vname .eq.    'ICEF') call write_var(trim(fname), vname, icef)

      ! Group 2
      if (vname .eq.     'HS') call write_var(trim(fname), vname, hs)
      if (vname .eq.    'WLM') call write_var(trim(fname), vname, wlm)
      if (vname .eq.    'T02') call write_var(trim(fname), vname, t02)
      if (vname .eq.   'T0M1') call write_var(trim(fname), vname, t0m1)
      if (vname .eq.    'T01') call write_var(trim(fname), vname, t01)
      if (vname .eq.    'FP0') call write_var(trim(fname), vname, fp0)
      if (vname .eq.    'THM') call write_var(trim(fname), vname, thm)
      if (vname .eq.    'THS') call write_var(trim(fname), vname, ths)
      if (vname .eq.   'THP0') call write_var(trim(fname), vname, thp0)
      if (vname .eq.   'HSIG') call write_var(trim(fname), vname, hsig)
      if (vname .eq. 'STMAXE') call write_var(trim(fname), vname, stmaxe)
      if (vname .eq. 'STMAXD') call write_var(trim(fname), vname, stmaxd)
      if (vname .eq.  'HMAXE') call write_var(trim(fname), vname, hmaxe)
      if (vname .eq. 'HCMAXE') call write_var(trim(fname), vname, hcmaxe)
      if (vname .eq.  'HMAXD') call write_var(trim(fname), vname, hmaxd)
      if (vname .eq. 'HCMAXD') call write_var(trim(fname), vname, hcmaxd)
      if (vname .eq.    'WBT') call write_var(trim(fname), vname, wbt)
      if (vname .eq. 'WNMEAN') call write_var(trim(fname), vname, wnmean)

      ! Group 3
      if(vname .eq.    'EF') call write_var_k(trim(fname), vname, ef(1:nsea,E3DF(2,1):E3DF(3,1)) )   ! freq axis
      if(vname .eq.  'TH1M') call write_var_k(trim(fname), vname, ef(1:nsea,E3DF(2,2):E3DF(3,2)) )
      if(vname .eq. 'STH1M') call write_var_k(trim(fname), vname, ef(1:nsea,E3DF(2,3):E3DF(3,3)) )
      if(vname .eq.  'TH2M') call write_var_k(trim(fname), vname, ef(1:nsea,E3DF(2,4):E3DF(3,4)) )
      if(vname .eq. 'STH2M') call write_var_k(trim(fname), vname, ef(1:nsea,E3DF(2,5):E3DF(3,5)) )
      !TODO: wn has reversed indices (1:nk, 1:nsea)

      ! Group 4
      if(vname .eq.   'PHS') call write_var_s(trim(fname), vname, phs)     ! noswll axis
      if(vname .eq.   'PTP') call write_var_s(trim(fname), vname, ptp)
      if(vname .eq.   'PLP') call write_var_s(trim(fname), vname, plp)
      if(vname .eq.  'PDIR') call write_var_s(trim(fname), vname, pdir)
      if(vname .eq.   'PSI') call write_var_s(trim(fname), vname, psi)
      if(vname .eq.   'PWS') call write_var_s(trim(fname), vname, pws)
      if(vname .eq.   'PDP') call write_var_s(trim(fname), vname, pthp0)
      if(vname .eq.   'PQP') call write_var_s(trim(fname), vname, pqp)
      if(vname .eq.   'PPE') call write_var_s(trim(fname), vname, ppe)
      if(vname .eq.   'PGW') call write_var_s(trim(fname), vname, pgw)
      if(vname .eq.   'PSW') call write_var_s(trim(fname), vname, psw)
      if(vname .eq.  'PTM1') call write_var_s(trim(fname), vname, ptm1)
      if(vname .eq.   'PT1') call write_var_s(trim(fname), vname, pt1)
      if(vname .eq.   'PT2') call write_var_s(trim(fname), vname, pt2)
      if(vname .eq.   'PEP') call write_var_s(trim(fname), vname, pep)
      if(vname .eq.  'PWST') call   write_var(trim(fname), vname, pwst)
      if(vname .eq.   'PNR') call   write_var(trim(fname), vname, pnr)

      ! Group 5
      if (vname .eq.   'USTX') call write_var(trim(fname), vname, ust*asf, dir=cos(ustdir), usemask='true')
      if (vname .eq.   'USTY') call write_var(trim(fname), vname, ust*asf, dir=sin(ustdir), usemask='true')
      if (vname .eq.    'CHA') call write_var(trim(fname), vname, charn)
      if (vname .eq.    'CGE') call write_var(trim(fname), vname, cge)
      if (vname .eq.  'PHIAW') call write_var(trim(fname), vname, phiaw)
      if (vname .eq. 'TAUWIX') call write_var(trim(fname), vname, tauwix)
      if (vname .eq. 'TAUWIY') call write_var(trim(fname), vname, tauwiy)
      if (vname .eq. 'TAUWNX') call write_var(trim(fname), vname, tauwnx)
      if (vname .eq. 'TAUWNY') call write_var(trim(fname), vname, tauwny)
      if (vname .eq.    'WCC') call write_var(trim(fname), vname, whitecap(:,1))
      if (vname .eq.    'WCF') call write_var(trim(fname), vname, whitecap(:,2))
      if (vname .eq.    'WCH') call write_var(trim(fname), vname, whitecap(:,3))
      if (vname .eq.    'WCM') call write_var(trim(fname), vname, whitecap(:,4))
      if (vname .eq.    'TWS') call write_var(trim(fname), vname, tws)

      ! Group 6
      if (vname .eq.     'SXX') call write_var(trim(fname), vname, sxx)
      if (vname .eq.     'SYY') call write_var(trim(fname), vname, syy)
      if (vname .eq.     'SXY') call write_var(trim(fname), vname, sxy)
      if (vname .eq.   'TAUOX') call write_var(trim(fname), vname, tauox)
      if (vname .eq.   'TAUOY') call write_var(trim(fname), vname, tauoy)
      if (vname .eq.     'BHD') call write_var(trim(fname), vname, bhd)
      if (vname .eq.   'PHIOC') call write_var(trim(fname), vname, phioc)
      if (vname .eq.    'TUSX') call write_var(trim(fname), vname, tusx)
      if (vname .eq.    'TUSY') call write_var(trim(fname), vname, tusy)
      if (vname .eq.    'USSX') call write_var(trim(fname), vname, ussx)
      if (vname .eq.    'USSY') call write_var(trim(fname), vname, ussy)
      if (vname .eq.    'PRMS') call write_var(trim(fname), vname, prms)
      if (vname .eq.    'TPMS') call write_var(trim(fname), vname, tpms)
      if (vname .eq.   'US3DX') call write_var_k(trim(fname), vname, us3d(1:nsea,   US3DF(2):US3DF(3)) )     !freq axis
      if (vname .eq.   'US3DY') call write_var_k(trim(fname), vname, us3d(1:nsea,NK+US3DF(2):NK+US3DF(3)) )  !freq axis
      if (vname .eq.   'P2SMS') call write_var_m(trim(fname), vname, p2sms(1:nsea,P2MSF(2):P2MSF(3)) )       !m axis
      if (vname .eq. 'TAUICEX') call write_var(trim(fname), vname, tauice(:,1))
      if (vname .eq. 'TAUICEY') call write_var(trim(fname), vname, tauice(:,2))
      if (vname .eq.   'PHICE') call write_var(trim(fname), vname, phice)
      if (vname .eq.   'USSPX') call write_var_p(trim(fname), vname, ussp(1:nsea,   1:USSPF(2)) )     ! partition axis
      if (vname .eq.   'USSPY') call write_var_p(trim(fname), vname, ussp(1:nsea,NK+1:NK+USSPF(2)) )  ! partition axis
      if (vname .eq.  'TAUOCX') call write_var(trim(fname), vname, tauocx)
      if (vname .eq.  'TAUOCY') call write_var(trim(fname), vname, tauocy)
#ifdef CESMCOUPLED
      if (vname .eq.  'LANGMT') call write_var(trim(fname), vname, langmt)
#endif
      ! Group 7
      if (vname .eq.     'ABAX') call write_var(trim(fname), vname, aba, cos(abd))
      if (vname .eq.     'ABAY') call write_var(trim(fname), vname, aba, sin(abd))
      if (vname .eq.     'UBAX') call write_var(trim(fname), vname, uba, cos(ubd))
      if (vname .eq.     'UBAY') call write_var(trim(fname), vname, uba, sin(ubd))
      if (vname .eq. 'Bedforms') call write_var_b(trim(fname), vname, bedforms)         ! bedform axis
      if (vname .eq.   'PHIBBL') call write_var(trim(fname), vname, phibbl)
      if (vname .eq.  'TAUBBLX') call write_var(trim(fname), vname, taubbl(:,1))
      if (vname .eq.  'TAUBBLY') call write_var(trim(fname), vname, taubbl(:,2))

      ! Group 8
      if (vname .eq.   'MSSX') call write_var(trim(fname), vname, mssx)
      if (vname .eq.   'MSSY') call write_var(trim(fname), vname, mssy)
      if (vname .eq.   'MSCX') call write_var(trim(fname), vname, mscx)
      if (vname .eq.   'MSCY') call write_var(trim(fname), vname, mscy)
      !TODO: remaining variables have inconsistency between shel_inp listing and iogo code

      ! Group 9
      if (vname .eq.    'DTDYN') call write_var(trim(fname), vname, dtdyn)
      if (vname .eq.     'FCUT') call write_var(trim(fname), vname, fcut)
      if (vname .eq. 'CFLXYMAX') call write_var(trim(fname), vname, cflxymax)
      if (vname .eq. 'CFLTHMAX') call write_var(trim(fname), vname, cflthmax)
      if (vname .eq.  'CFLKMAX') call write_var(trim(fname), vname, cflkmax)

      ! Group 10
     end do

    ! Flush the buffers for write
    call W3SETA ( IGRD, NDSE, NDST )

  end subroutine W3IOGONCD

!/ ------------------------------------------------------------------- /
  subroutine write_var(fname, vname, var, dir, usemask)
    ! write (nsea) array as (nx,ny)
    ! if dir is present, write x or y component of (nsea) array as (nx,ny)
    ! if mask is present and true, use mapsta=1 to mask values

    character(len=*),  intent(in)          :: fname
    character(len=*),  intent(in)          :: vname
    real            ,  intent(in)          :: var(nsea)
    real, optional  ,  intent(in)          :: dir(nsea)
    character(len=*), optional, intent(in) :: usemask

    ! local variables
    real, dimension(nx,ny) :: var2d
    logical                :: lmask

    lmask = .false.
    if (present(usemask)) then
     lmask = (trim(usemask) == "true")
    end if

    if (dbug_flag > 5 ) then
     write(nds(1),'(a)')' writing variable ' //trim(vname)//' to history file '//trim(fname)
    end if
    ! initialization
    !do isea = 1,nsea
    !   if (mapsta(mapsf(isea,2),mapsf(isea,1)) < 0) var(isea) = undef
    !end do

    var2d = undef
    do isea = 1,nsea
       if (present(dir)) then
          if (var(isea) .ne. undef) then
             if (lmask) then
                if (mapsta(mapsf(isea,2),mapsf(isea,1)) == 1) then
                   var2d(mapsf(isea,1),mapsf(isea,2)) = var(isea)*dir(isea)
                end if
             else
                var2d(mapsf(isea,1),mapsf(isea,2)) = var(isea)*dir(isea)
             end if
          end if
       else
         var2d(mapsf(isea,1),mapsf(isea,2)) = var(isea)
       end if
    end do

    ierr = nf90_open(trim(fname),   nf90_write,  ncid)
    call handle_err(ierr, 'open '//trim(fname)//' for writing')
    ierr = nf90_inq_varid(ncid,  trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    ierr = nf90_put_var(ncid, varid, var2d)
    call handle_err(ierr, 'put variable '//trim(vname))
    ierr = nf90_close(ncid)

  end subroutine write_var

!/ ------------------------------------------------------------------- /
  subroutine write_var_s(fname, vname, var)
    ! write (nsea,0:noswll) array as (nx,ny,0:noswll)

    character(len=*), intent(in) :: fname
    character(len=*), intent(in) :: vname
    real            , intent(in) :: var(nsea,0:noswll)

    ! local variables
    real, dimension(nx,ny,0:noswll) :: var3d

    if (dbug_flag > 5 ) then
     write(nds(1),'(a)')' writing variable ' //trim(vname)//' to history file '//trim(fname)
    end if
    ! initialization
    !do isea = 1,nsea
    !   if (mapsta(mapsf(isea,2),mapsf(isea,1)) < 0) var(isea,:) = undef
    !end do

    var3d = undef
    do isea = 1,nsea
       var3d(mapsf(isea,1),mapsf(isea,2),:) = var(isea,:)
    end do

    ierr = nf90_open(trim(fname),  nf90_write,  ncid)
    call handle_err(ierr, 'open '//trim(fname)//' for writing')
    ierr = nf90_inq_varid(ncid, trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    ierr = nf90_put_var(ncid, varid, var3d)
    call handle_err(ierr, 'put variable '//trim(vname))
    ierr = nf90_close(ncid)

  end subroutine write_var_s

!/ ------------------------------------------------------------------- /
  subroutine write_var_k(fname, vname, var)
    ! write (nsea,nk) array as (nx,ny,nk)

    character(len=*), intent(in) :: fname
    character(len=*), intent(in) :: vname
    real            , intent(in) :: var(nsea,len_k)
 
    ! local variables
    real, dimension(nx,ny,len_k) :: var3d

    if (dbug_flag > 5 ) then
     write(nds(1),'(a)')' writing variable ' //trim(vname)//' to history file '//trim(fname)
    end if
    ! initialization
    !do isea = 1,nsea
    !   if (mapsta(mapsf(isea,2),mapsf(isea,1)) < 0) var(isea,:) = undef
    !end do

    var3d = undef
    do isea = 1,nsea
       var3d(mapsf(isea,1),mapsf(isea,2),:) = var(isea,:)
    end do

    ierr = nf90_open(trim(fname),  nf90_write,  ncid)
    call handle_err(ierr, 'open '//trim(fname)//' for writing')
    ierr = nf90_inq_varid(ncid, trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    ierr = nf90_put_var(ncid, varid, var3d)
    call handle_err(ierr, 'put variable '//trim(vname))
    ierr = nf90_close(ncid)

  end subroutine write_var_k

!/ ------------------------------------------------------------------- /
  subroutine write_var_m(fname, vname, var)

    character(len=*), intent(in) :: fname
    character(len=*), intent(in) :: vname
    real, dimension(nsea,len_m), intent(in) :: var

    ! local variables
    real, dimension(nx,ny,len_m) :: var3d

    if (dbug_flag > 5 ) then
     write(nds(1),'(a)')' writing variable ' //trim(vname)//' to history file '//trim(fname)
    end if
    ! initialization
    !do isea = 1,nsea
    !   if (mapsta(mapsf(isea,2),mapsf(isea,1)) < 0) var(isea,:) = undef
    !end do

    var3d = undef
    do isea = 1,nsea
       var3d(mapsf(isea,1),mapsf(isea,2),:) = var(isea,:)
    end do

    ierr = nf90_open(trim(fname),  nf90_write,  ncid)
    call handle_err(ierr, 'open '//trim(fname)//' for writing')
    ierr = nf90_inq_varid(ncid, trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    ierr = nf90_put_var(ncid, varid, var3d)
    call handle_err(ierr, 'put variable '//trim(vname))
    ierr = nf90_close(ncid)

  end subroutine write_var_m

!/ ------------------------------------------------------------------- /
  subroutine write_var_p(fname, vname, var)
    ! write (nsea,npartitions) array as (nx,ny,npartitions)

    character(len=*), intent(in) :: fname
    character(len=*), intent(in) :: vname
    real            , intent(in) :: var(nsea,len_p)

    ! local variables
    real, dimension(nx,ny,len_p) :: var3d

    if (dbug_flag > 5 ) then
     write(nds(1),'(a)')' writing variable ' //trim(vname)//' to history file '//trim(fname)
    end if
    ! initialization
    !do isea = 1,nsea
    !   if (mapsta(mapsf(isea,2),mapsf(isea,1)) < 0) var(isea,:) = undef
    !end do

    var3d = undef
    do isea = 1,nsea
       var3d(mapsf(isea,1),mapsf(isea,2),:) = var(isea,:)
    end do

    ierr = nf90_open(trim(fname),  nf90_write,  ncid)
    call handle_err(ierr, 'open '//trim(fname)//' for writing')
    ierr = nf90_inq_varid(ncid, trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    ierr = nf90_put_var(ncid, varid, var3d)
    call handle_err(ierr, 'put variable '//trim(vname))
    ierr = nf90_close(ncid)

  end subroutine write_var_p

!/ ------------------------------------------------------------------- /
  subroutine write_var_b(fname, vname, var)
    ! write (nsea,3) array as (nx,ny,3)

    character(len=*), intent(in) :: fname
    character(len=*), intent(in) :: vname
    real            , intent(in) :: var(nsea,3)

    ! local variables
    real, dimension(nx,ny,3) :: var3d

    if (dbug_flag > 5 ) then
     write(nds(1),'(a)')' writing variable ' //trim(vname)//' to history file '//trim(fname)
    end if
    ! initialization
    !do isea = 1,nsea
    !   if (mapsta(mapsf(isea,2),mapsf(isea,1)) < 0) var(isea,:) = undef
    !end do

    var3d = undef
    do isea = 1,nsea
       var3d(mapsf(isea,1),mapsf(isea,2),:) = var(isea,:)
    end do

    ierr = nf90_open(trim(fname),  nf90_write,  ncid)
    call handle_err(ierr, 'open '//trim(fname)//' for writing')
    ierr = nf90_inq_varid(ncid, trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    ierr = nf90_put_var(ncid, varid, var3d)
    call handle_err(ierr, 'put variable '//trim(vname))
    ierr = nf90_close(ncid)

  end subroutine write_var_b

!/ ------------------------------------------------------------------- /
  subroutine hist_filename(fname)

    USE WAV_SHR_MOD    , ONLY : CASENAME, INST_SUFFIX
    USE W3WDATMD       , ONLY : TIME
    USE W3ODATMD       , ONLY : NDS, IAPROC, NAPOUT

    implicit none

    ! input/output variables
    character(len=*), intent(out) :: fname

    ! local variables
    integer :: yy,mm,dd,hh,mn,ss,totsec
    !----------------------------------------------
    
    yy =  time(1)/10000
    mm = (time(1)-yy*10000)/100
    dd = (time(1)-yy*10000-mm*100)
    hh = time(2)/10000
    mn = (time(2)-hh*10000)/100
    ss = (time(2)-hh*10000-mn*100)
    totsec = hh*3600+mn*60+ss

    if (len_trim(inst_suffix) > 0) then
       write(fname,'(a,i4.4,a,i2.2,a,i2.2,a,i5.5,a)') &
            trim(casename)//'.ww3'//trim(inst_suffix)//'.hi.',yy,'-',mm,'-',dd,'-',totsec,'.nc'
    else
       write(fname,'(a,i4.4,a,i2.2,a,i2.2,a,i5.5,a)') &
            trim(casename)//'.ww3.hi.',yy,'-',mm,'-',dd,'-',totsec,'.nc'
    endif

    if (iaproc == napout) then
        write(nds(1),'(a)') 'w3iogomdncd: writing history '//trim(fname)
     end if

   end subroutine hist_filename

!/ ------------------------------------------------------------------- /
  SUBROUTINE HANDLE_ERR(IERR,STRING)
    USE W3ODATMD, ONLY: NDSE
    USE W3SERVMD, ONLY: EXTCDE

    IMPLICIT NONE

    ! input/output variables
    integer         ,intent(in) :: ierr
    character(len=*),intent(in) :: string

    IF (IERR /= NF90_NOERR) then
         WRITE(NDSE,*) "*** WAVEWATCH III netCDF error: ",trim(string),':',trim(nf90_strerror(IERR))
         CALL EXTCDE ( 49 )
    END IF
  end SUBROUTINE HANDLE_ERR

end module W3IOGONCDMD
