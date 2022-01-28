#include "w3macros.h"

module W3IOGONCDMD

contains

!/ ------------------------------------------------------------------- /
  subroutine W3IOGONCD ()

    ! Write netcdf ww3 history output

    USE CONSTANTS
    USE W3WDATMD, ONLY: W3SETW, W3DIMW, TIME, WLV, ICE, ICEF, ICEH, BERG, UST, USTDIR, ASF
    USE W3GDATMD, ONLY: NX, NY, E3DF, MAPSF, MAPSTA, NSEA, W3SETG
    USE W3ODATMD, ONLY: NOGRP, NGRPP, IDOUT, UNDEF, NDST, NDSE,  FLOGRD, NOSWLL, W3SETO
    USE W3ADATMD, ONLY: LANGMT
    USE W3ADATMD, ONLY: W3SETA, W3DIMA, W3XETA
    USE W3ADATMD, ONLY: AINIT, DW, UA, UD, AS, CX, CY, WN
    USE W3ADATMD, ONLY: HS, WLM, T02, T0M1, T01, FP0, THM, THS, THP0, WBT
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
    USE W3ADATMD, ONLY: STMAXE, STMAXD, HMAXE, HCMAXE, HMAXD, HCMAXD, USSP
    USE NETCDF

    IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
    INTEGER                 :: IGRD, IERR, I, J, IX, IY, ISEA, IFI, IFJ
    REAL                    :: AUX1(NSEA), AUX2(NSEA), AUX3(NSEA), AUX4(NSEA)
    REAL                    :: AUXE(NSEA,0:NOSWLL), AUXEF(NSEA,E3DF(2,1):E3DF(3,1))
    REAL,    ALLOCATABLE    :: AUX2D1(:,:), AUX2D2(:,:), AUX2D3(:,:)
    REAL,    ALLOCATABLE    :: AUX3DEF(:,:,:), AUX3DE(:,:,:)
    LOGICAL                 :: WAUX1, WAUX2, WAUX3, WAUXE, WAUXEF
    INTEGER                 :: VARID1, VARID2, VARID3, VARIDE, NCLOOP
    CHARACTER(LEN=16)       :: FLDSTR1, FLDSTR2, FLDSTR3, FLDSTRE
    CHARACTER(LEN=16)       :: UNITSTR1, UNITSTR2, UNITSTR3, UNITSTRE
    CHARACTER(LEN=128)      :: LNSTR1, LNSTR2, LNSTR3, LNSTRE
    INTEGER                 :: EF_LEN
    INTEGER                 :: NCID, DIMID(4)
    CHARACTER(len=1024)     :: FNAME
    LOGICAL                 :: EXISTS
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
    ! Allocate fields needed for write
    ! -------------------------------------------------------------

    ALLOCATE ( AUX2D1(NX,NY), AUX2D2(NX,NY), AUX2D3(NX,NY), AUX3DE(NX,NY,0:NOSWLL) )
    ALLOCATE ( AUX3DEF(NX,NY,E3DF(2,1):E3DF(3,1)) )
    !
    ! -------------------------------------------------------------
    ! Create the netcdf file and return the ncid and dimid
    ! -------------------------------------------------------------
#ifdef CESMCOUPLED
    call cesm_hist_filename(fname)
#else
    ! fill this in for ufs
#endif

    ef_len = e3df(3,1) - e3df(2,1) + 1
    inquire(file=trim(fname),exist=exists)
    if (.not. exists) then
       ierr = nf90_create(trim(fname),nf90_clobber,ncid)
       call handle_err(ierr,'create')
       ierr = nf90_def_dim(ncid,'nx',nx,dimid(1))
       call handle_err(ierr,'def_dimid1')
       ierr = nf90_def_dim(ncid,'ny',ny,dimid(2))
       call handle_err(ierr,'def_dimid2')
       ierr = nf90_def_dim(ncid,'noswll',noswll+1,dimid(3))
       call handle_err(ierr,'def_dimid3')
       ierr = nf90_def_dim(ncid,'freq', ef_len, dimid(4)) !ef_len=25
       call handle_err(ierr,'def_dimid4')
    else
       ierr = nf90_open(trim(fname),nf90_write,ncid)
       call handle_err(ierr,'open')
       ierr = nf90_inq_dimid(ncid,'nx',dimid(1))
       call handle_err(ierr,'inq_dimid1')
       ierr = nf90_inq_dimid(ncid,'ny',dimid(2))
       call handle_err(ierr,'inq_dimid2')
       ierr = nf90_inq_dimid(ncid,'noswll',dimid(3))
       call handle_err(ierr,'inq_dimid3')
       ierr = nf90_inq_dimid(ncid,'freq',dimid(4)) !ef_len=25
       call handle_err(ierr,'inq_dimid4')
    endif

    ! -------------------------------------------------------------
    ! Initialization
    ! -------------------------------------------------------------

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
          IF ( FLOGRD( 6, 14) ) LANGMT(ISEA) = UNDEF  !cesm specific
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
    !
    ! -------------------------------------------------------------
    ! Actual output
    ! -------------------------------------------------------------
    !
    ! 1st loop step  define the netcdf variables and attributes
    ! 2nd loop step, write the variables

    NC_LOOP: do NCLOOP = 1,2
       if (NCLOOP == 1) then
          IERR = NF90_REDEF(NCID)
       else if (NCLOOP == 2) then
          IERR = NF90_ENDDEF(NCID)
       endif
       IFI_LOOP: do IFI=1, NOGRP
          IFJ_LOOP: do IFJ=1, NGRPP
             if ( FLOGRD(IFI,IFJ) ) then
                WAUX1 = .false.  ! vars with dims (nx,ny) shoved into AUX1
                WAUX2 = .false.  ! y-component of vars with dims (nx,ny) shoved into AUX2
                WAUX3 = .false.  ! unused
                WAUXE = .false.  ! wave height of partition vars with dims of NOSWLL, a mess
                WAUXEF = .false. ! for vars with dims of (Freq,nx,ny) shoved into AUXEF
                !
                !     Section 1)
                !
                if ( IFI .eq. 1 .and. IFJ .eq. 1 ) then
                   AUX1(1:NSEA) = DW(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'DW'
                   UNITSTR1 = 'm'
                   LNSTR1 = 'Water depth'  !CMB should use IDOUT here, see w3odatmd
                else if ( IFI .eq. 1 .and. IFJ .eq. 2 ) then
                   AUX1(1:NSEA) = CX(1:NSEA)
                   AUX2(1:NSEA) = CY(1:NSEA)
                   WAUX1 = .true.
                   WAUX2 = .true.
                   FLDSTR1 = 'CX'
                   FLDSTR2 = 'CY'
                   UNITSTR1 = 'm/s'
                   UNITSTR2 = 'm/s'
                   LNSTR1 = 'Mean current, x-component'
                   LNSTR2 = 'Mean current, y-component'
                else if ( IFI .eq. 1 .and. IFJ .eq. 3 ) then
                   do ISEA=1, NSEA
                      if (UA(ISEA) .ne.UNDEF) then
                         AUX1(ISEA) = UA(ISEA)*cos(UD(ISEA))
                         AUX2(ISEA) = UA(ISEA)*sin(UD(ISEA))
                      else
                         AUX1(ISEA) = UNDEF
                         AUX2(ISEA) = UNDEF
                      end if
                   end do
                   WAUX1 = .true.
                   WAUX2 = .true.
                   FLDSTR1 = 'UAX'
                   FLDSTR2 = 'UAY'
                   UNITSTR1 = 'm/s'
                   UNITSTR2 = 'm/s'
                   LNSTR1 = 'Mean wind, x-component'
                   LNSTR2 = 'Mean wind, y-component'
                else if ( IFI .eq. 1 .and. IFJ .eq. 4 ) then
                   AUX1(1:NSEA) = AS(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'AS'
                   UNITSTR1 = 'deg C'
                   LNSTR1 = 'Air-sea temperature difference'
                else if ( IFI .eq. 1 .and. IFJ .eq. 5 ) then
                   AUX1(1:NSEA) = WLV(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'WLV'
                   UNITSTR1 = 'm'
                   LNSTR1 = 'Water levels'
                else if ( IFI .eq. 1 .and. IFJ .eq. 6 ) then
                   AUX1(1:NSEA) = ICE(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'ICE'
                   UNITSTR1 = '1'
                   LNSTR1 = 'Ice coverage'
                else if ( IFI .eq. 1 .and. IFJ .eq. 7 ) then
                   AUX1(1:NSEA) = BERG(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'BERG'
                   UNITSTR1 = '1'
                   LNSTR1 = ''
                !
                !     Section 2)
                !
                else if ( IFI .eq. 2 .and. IFJ .eq. 1 ) then
                   AUX1(1:NSEA) = HS(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'HS'
                   UNITSTR1 = 'm'
                   LNSTR1 = 'Significant wave height'
                else if ( IFI .eq. 2 .and. IFJ .eq. 2 ) then
                   WAUX1 = .true.
                   FLDSTR1 = 'WLM'
                   UNITSTR1 = 'm'
                   LNSTR1 = 'Mean wave length'
                else if ( IFI .eq. 2 .and. IFJ .eq. 3 ) then
                   AUX1(1:NSEA) = T02(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'T02'
                   UNITSTR1 = 's'
                   LNSTR1 = 'Mean wave period'
                else if ( IFI .eq. 2 .and. IFJ .eq. 4 ) then
                   AUX1(1:NSEA) = T0M1(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'T0M1'
                   UNITSTR1 = 's'
                   LNSTR1 = 'Mean wave period'
                else if ( IFI .eq. 2 .and. IFJ .eq. 5 ) then
                   AUX1(1:NSEA) = T01(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'T01'
                   UNITSTR1 = 's'
                   LNSTR1 = 'Mean wave period'
                else if ( IFI .eq. 2 .and. IFJ .eq. 6 ) then
                   AUX1(1:NSEA) = FP0(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'FP0'
                   UNITSTR1 = 'Hz'
                   LNSTR1 = 'Peak frequency'
                else if ( IFI .eq. 2 .and. IFJ .eq. 7 ) then
                   AUX1(1:NSEA) = THM(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'THM'
                   UNITSTR1 = 'rad'
                   LNSTR1 = 'Mean wave direction'
                else if ( IFI .eq. 2 .and. IFJ .eq. 8 ) then
                   AUX1(1:NSEA) = THS(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'THS'
                   UNITSTR1 = 'rad'
                   LNSTR1 = 'Mean directional spread'
                else if ( IFI .eq. 2 .and. IFJ .eq. 9 ) then
                   AUX1(1:NSEA) = THP0(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'THP0'
                   UNITSTR1 = 'rad'
                   LNSTR1 = 'Peak direction'
                else if ( IFI .eq. 2 .and. IFJ .eq. 10 ) then
                   AUX1(1:NSEA) = HSIG(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'HSIG'
                   UNITSTR1 = '1'
                   LNSTR1 = ''
                else if ( IFI .eq. 2 .and. IFJ .eq. 11 ) then
                   AUX1(1:NSEA) = STMAXE(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'STMAXE'
                   UNITSTR1 = 'm'
                   LNSTR1 = 'Max surface elev STE'
                else if ( IFI .eq. 2 .and. IFJ .eq. 12 ) then
                   AUX1(1:NSEA) = STMAXD(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'STMAXD'
                   UNITSTR1 = 'm'
                   LNSTR1 = 'St Dev Max surface elev STE'
                else if ( IFI .eq. 2 .and. IFJ .eq. 13 ) then
                   AUX1(1:NSEA) = HMAXE(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'HMAXE'
                   UNITSTR1 = 'm'
                   LNSTR1 = 'Max wave height STE'
                else if ( IFI .eq. 2 .and. IFJ .eq. 14 ) then
                   AUX1(1:NSEA) = HCMAXE(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'HCMAXE'
                   UNITSTR1 = 'm'
                   LNSTR1 = 'Max wave height from crest STE'
                else if ( IFI .eq. 2 .and. IFJ .eq. 15 ) then
                   AUX1(1:NSEA) = HMAXD(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'HMAXD'
                   UNITSTR1 = 'm'
                   LNSTR1 = 'St Dev of MXC (STE)'
                else if ( IFI .eq. 2 .and. IFJ .eq. 16 ) then
                   AUX1(1:NSEA) = HCMAXD(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'HCMAXD'
                   UNITSTR1 = 'm'
                   LNSTR1 = 'St Dev of MXHC (STE)'
                else if ( IFI .eq. 2 .and. IFJ .eq. 17 ) then
                   AUX1(1:NSEA) = WBT(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'WBT'
                   UNITSTR1 = 'm'
                   LNSTR1 = 'Dominant wave breaking probability b'
                !
                ! Section 3)
                !
                else if ( IFI .eq. 3 .and. IFJ .eq. 1 ) then
                   AUXEF(1:NSEA,E3DF(2,1):E3DF(3,1)) = EF(1:NSEA,E3DF(2,1):E3DF(3,1))
                   WAUXEF = .true.
                   FLDSTRE = 'EF'
                   UNITSTRE = '1'
                   LNSTRE = '1D spectral density'
                !
                ! Section 4)
                !
                else if ( IFI .eq. 4 .and. IFJ .eq. 1 ) then
                   AUXE(1:NSEA,0:NOSWLL) = PHS(1:NSEA,0:NOSWLL)
                   WAUXE = .true.
                   FLDSTRE = 'PHS'
                   UNITSTRE = 'm'
                   LNSTRE = 'Wave height of partitions'
                else if ( IFI .eq. 4 .and. IFJ .eq. 2 ) then
                   AUXE(1:NSEA,0:NOSWLL) = PTP(1:NSEA,0:NOSWLL)
                   WAUXE = .true.
                   FLDSTRE = 'PTP'
                   UNITSTRE = 's'
                   LNSTRE = 'Peak wave period of partitions'
                else if ( IFI .eq. 4 .and. IFJ .eq. 3 ) then
                   AUXE(1:NSEA,0:NOSWLL) = PLP(1:NSEA,0:NOSWLL)
                   WAUXE = .true.
                   FLDSTRE = 'PLP'
                   UNITSTRE = 'm'
                   LNSTRE = 'Peak wave length of partitions'
                !
                !  Section 5)
                !
                else if ( IFI .eq. 5 .and. IFJ .eq. 1 ) then
                   do ISEA=1, NSEA
                      IX     = MAPSF(ISEA,1)
                      IY     = MAPSF(ISEA,2)
                      if ( MAPSTA(IY,IX) .eq. 1 ) then
                         AUX1(ISEA) = UST(ISEA) * ASF(ISEA) *        &
                              cos(USTDIR(ISEA))
                         AUX2(ISEA) = UST(ISEA) * ASF(ISEA) *        &
                              sin(USTDIR(ISEA))
                      else
                         AUX1(ISEA) = UNDEF
                         AUX2(ISEA) = UNDEF
                      end if
                   end do
                   WAUX1 = .true.
                   WAUX2 = .true.
                   FLDSTR1 = 'ASFX'
                   FLDSTR2 = 'ASFY'
                   UNITSTR1 = 'm/s'
                   UNITSTR2 = 'm/s'
                   LNSTR1 = 'Skin friction velocity, x-component'
                   LNSTR2 = 'Skin friction velocity, y-component'
                !
                !     Section 6)
                !
                else if ( IFI .eq. 6 .and. IFJ .eq. 6 ) then
                   AUX1(1:NSEA) = USSX(1:NSEA)
                   AUX2(1:NSEA) = USSY(1:NSEA)
                   WAUX1 = .true.
                   WAUX2 = .true.
                   FLDSTR1 = 'USSX'
                   FLDSTR2 = 'USSY'
                   UNITSTR1 = 'm/s'
                   UNITSTR2 = 'm/s'
                   LNSTR1 = 'Stokes drift at z=0'
                   LNSTR2 = 'Stokes drift at z=0'
                else if ( IFI .eq. 6 .and. IFJ .eq. 14 ) then
                   write(6,*)'DEBUG: nsea = ',nsea
                   write(6,*)'DEBUG: size(langmt) = ',size(langmt)
                   AUX1(1:NSEA) = LANGMT(1:NSEA)
                   WAUX1 = .true.
                   FLDSTR1 = 'LANGMT'
                   UNITSTR1 = ''
                   LNSTR1 = 'Turbulent Langmuir number (La_t)'
                !
                !     Section 7)
                !
                else if ( IFI .eq. 7 .and. IFJ .eq. 1 ) then
                   do ISEA=1, NSEA
                      if ( ABA(ISEA) .ne. UNDEF ) then
                         AUX1(ISEA) = ABA(ISEA)*cos(ABD(ISEA))
                         AUX2(ISEA) = ABA(ISEA)*sin(ABD(ISEA))
                      else
                         AUX1(ISEA) = UNDEF
                         AUX2(ISEA) = UNDEF
                      end if
                   end do
                   WAUX1 = .true.
                   WAUX2 = .true.
                   FLDSTR1 = 'ABAX'
                   FLDSTR2 = 'ABAY'
                   UNITSTR1 = 'm'
                   UNITSTR2 = 'm'
                   LNSTR1 = 'Near bottom rms wave excursion amplitude, x-component'
                   LNSTR2 = 'Near bottom rms wave excursion amplitude, y-component'
                else if ( IFI .eq. 7 .and. IFJ .eq. 2 ) then
                   do ISEA=1, NSEA
                      if ( UBA(ISEA) .ne. UNDEF ) then
                         AUX1(ISEA) = UBA(ISEA)*cos(UBD(ISEA))
                         AUX2(ISEA) = UBA(ISEA)*sin(UBD(ISEA))
                      else
                         AUX1(ISEA) = UNDEF
                         AUX2(ISEA) = UNDEF
                      end if
                   end do
                   WAUX1 = .true.
                   WAUX2 = .true.
                   FLDSTR1 = 'UBAX'
                   FLDSTR2 = 'UBAY'
                   UNITSTR1 = 'm/s'
                   UNITSTR2 = 'm/s'
                   LNSTR1 = 'Near bottom rms wave velocity, x-component'
                   LNSTR2 = 'Near bottom rms wave velocity, y-component'
                   !
                   !     Section 8)
                   !
                   !
                   !     Section 9)
                   !
                   !
                   !     Section 10)
                   !
                else if ( IFI .eq. 10 ) then
                   AUX1(1:NSEA) = USERO(1:NSEA,2)
                   WAUX1 = .true.
                   FLDSTR1 = 'USERO'
                   UNITSTR1 = '1'
                   LNSTR1 = 'User defined variable'
                end if

                ! netcdf history
                if (NCLOOP == 1) then
                   ! write(ndse,*) 'w3iogo NCLOOP=',NCLOOP, WAUX1, WAUX2, WAUX3,WAUXE,WAUXEF
                   !--- no error checking here in case file/vars exists already ---
                   if (WAUX1) then
                      ! write(ndse,*) ' w3iogo NCLOOP=1, WAUX1=T, FLDSTR1, VARID1', TRIM(FLDSTR1), VARID1
                      IERR = NF90_DEF_VAR(NCID,trim(FLDSTR1),NF90_FLOAT,DIMID(1:2),VARID1)
                      IERR = NF90_PUT_ATT(NCID,VARID1,"_FillValue",UNDEF)
                      IERR = NF90_PUT_ATT(NCID,VARID1,"units",UNITSTR1)
                      IERR = NF90_PUT_ATT(NCID,VARID1,"long_name",LNSTR1)
                   endif
                   if (WAUX2) then
                      ! write(ndse,*) ' w3iogo NCLOOP=1, WAUX2=T, FLDSTR2, VARID2', TRIM(FLDSTR2), VARID2
                      IERR = NF90_DEF_VAR(NCID,trim(FLDSTR2),NF90_FLOAT,DIMID(1:2),VARID2)
                      IERR = NF90_PUT_ATT(NCID,VARID2,"_FillValue",UNDEF)
                      IERR = NF90_PUT_ATT(NCID,VARID2,"units",UNITSTR2)
                      IERR = NF90_PUT_ATT(NCID,VARID2,"long_name",LNSTR2)
                   endif
                   if (WAUX3) then
                      IERR = NF90_DEF_VAR(NCID,trim(FLDSTR3),NF90_FLOAT,DIMID(1:2),VARID3)
                      IERR = NF90_PUT_ATT(NCID,VARID3,"_FillValue",UNDEF)
                      IERR = NF90_PUT_ATT(NCID,VARID3,"units",UNITSTR3)
                      IERR = NF90_PUT_ATT(NCID,VARID3,"long_name",LNSTR3)
                   endif
                   if (WAUXE) then
                      IERR = NF90_DEF_VAR(NCID,trim(FLDSTRE),NF90_FLOAT,DIMID(1:3),VARIDE)
                      IERR = NF90_PUT_ATT(NCID,VARIDE,"_FillValue",UNDEF)
                      IERR = NF90_PUT_ATT(NCID,VARIDE,"units",UNITSTRE)
                      IERR = NF90_PUT_ATT(NCID,VARIDE,"long_name",LNSTRE)
                   endif
                   if (WAUXEF) then
                      ! write(ndse,*) ' w3iogo NCLOOP=1, WAUXEF=T, FLDSTRE, VARIDE', TRIM(FLDSTRE), VARIDE
                      IERR = NF90_DEF_VAR(NCID,trim(FLDSTRE),NF90_FLOAT,(/DIMID(1),DIMID(2),DIMID(4)/),VARIDE)
                      IERR = NF90_PUT_ATT(NCID,VARIDE,"_FillValue",UNDEF)
                      IERR = NF90_PUT_ATT(NCID,VARIDE,"units",UNITSTRE)
                      IERR = NF90_PUT_ATT(NCID,VARIDE,"long_name",LNSTRE)
                   endif

                elseif (NCLOOP == 2) then
                   ! write(ndse,*) ' w3iogo write NCLOOP=',NCLOOP, WAUX1, WAUX2, WAUX3,WAUXE,WAUXEF
                   if (WAUX1) then
                      ! write(ndso,*) 'w3iogo write ',trim(fldstr1)
                      AUX2D1 = UNDEF
                      do ISEA=1, NSEA
                         AUX2D1(MAPSF(ISEA,1),MAPSF(ISEA,2)) = AUX1(ISEA)
                      enddo
                      IERR = NF90_INQ_VARID(NCID,trim(FLDSTR1),VARID1)
                      call HANDLE_ERR(IERR,'INQ_VARID_AUX2D1_'//trim(FLDSTR1))
                      IERR = NF90_PUT_VAR(NCID,VARID1,AUX2D1)
                      call HANDLE_ERR(IERR,'PUT_VAR_AUX2D1_'//trim(FLDSTR1))
                   endif
                   if (WAUX2) then
                      ! write(ndso,*) 'w3iogo write ',trim(fldstr2)
                      AUX2D2 = UNDEF
                      do ISEA=1, NSEA
                         AUX2D2(MAPSF(ISEA,1),MAPSF(ISEA,2)) = AUX2(ISEA)
                      enddo
                      IERR = NF90_INQ_VARID(NCID,trim(FLDSTR2),VARID2)
                      call HANDLE_ERR(IERR,'INQ_VARID_AUX2D2_'//trim(FLDSTR2))
                      IERR = NF90_PUT_VAR(NCID,VARID2,AUX2D2)
                      call HANDLE_ERR(IERR,'PUT_VAR_AUX2D2_'//trim(FLDSTR2))
                   endif
                   if (WAUX3) then
                      ! write(ndso,*) 'w3iogo write ',trim(fldstr3)
                      AUX2D3 = UNDEF
                      do ISEA=1, NSEA
                         AUX2D3(MAPSF(ISEA,1),MAPSF(ISEA,2)) = AUX3(ISEA)
                      enddo
                      IERR = NF90_INQ_VARID(NCID,trim(FLDSTR3),VARID3)
                      call HANDLE_ERR(IERR,'INQ_VARID_AUX2D3_'//trim(FLDSTR3))
                      IERR = NF90_PUT_VAR(NCID,VARID3,AUX2D3)
                      call HANDLE_ERR(IERR,'PUT_VAR_AUX2D3_'//trim(FLDSTR3))
                   endif
                   if (WAUXE) then
                      ! write(ndso,*) 'w3iogo write ',trim(fldstre)
                      AUX3DE = UNDEF
                      do ISEA=1, NSEA
                         AUX3DE(MAPSF(ISEA,1),MAPSF(ISEA,2),0:NOSWLL) = AUXE(ISEA,0:NOSWLL)
                      enddo
                      IERR = NF90_INQ_VARID(NCID,trim(FLDSTRE),VARIDE)
                      call HANDLE_ERR(IERR,'INQ_VARID_AUX2D1_'//trim(FLDSTRE))
                      IERR = NF90_PUT_VAR(NCID,VARIDE,AUX3DE)
                      call HANDLE_ERR(IERR,'PUT_VAR_AUX3DE_'//trim(FLDSTRE))
                   endif
                   if (WAUXEF) then
                      ! write(ndso,*) 'w3iogo write ',trim(fldstre)
                      AUX3DEF = UNDEF
                      do ISEA=1, NSEA
                         AUX3DEF(MAPSF(ISEA,1),MAPSF(ISEA,2),E3DF(2,1):E3DF(3,1)) = AUXEF(ISEA,E3DF(2,1):E3DF(3,1))
                      enddo
                      IERR = NF90_INQ_VARID(NCID,trim(FLDSTRE),VARIDE)
                      call HANDLE_ERR(IERR,'INQ_VARID_AUX2D1_'//trim(FLDSTRE))
                      IERR = NF90_PUT_VAR(NCID,VARIDE,AUX3DEF)
                      call HANDLE_ERR(IERR,'PUT_VAR_AUX3DE_'//trim(FLDSTRE))
                   endif
                endif !NC

             end if ! end of if ( FLOGRD(IFI,IFJ) ) 
          end do IFJ_LOOP
       end do IFI_LOOP
    end do NC_LOOP

    ierr = NF90_CLOSE(NCID)
    call handle_err(IERR,'CLOSE')
    deallocate(AUX2D1,AUX2D2,AUX2D3,AUX3DE,AUX3DEF)

    ! Flush the buffers for write
    call W3SETA ( IGRD, NDSE, NDST )

  end subroutine W3IOGONCD

!/ ------------------------------------------------------------------- /
#ifdef CESMCOUPLED
  subroutine cesm_hist_filename(fname)

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

   end subroutine cesm_hist_filename
#endif

!/ ------------------------------------------------------------------- /
  SUBROUTINE HANDLE_ERR(IERR,STRING)
    USE W3ODATMD, ONLY: NDSE 
    USE W3SERVMD, ONLY: EXTCDE
    USE NETCDF

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
