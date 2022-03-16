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
#ifdef CESMCOUPLED
    USE W3ADATMD, ONLY: LANGMT
#endif
    use wav_grdout_defs, only: varatts, outvars
    use wav_shr_mod    , only: time_origin, calendar_name, elapsed_secs

    USE NETCDF

    IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
    INTEGER                 :: IGRD, IERR, I, J, IX, IY, ISEA, IFI, IFJ
    !REAL                    :: AUX1(NSEA), AUX2(NSEA), AUX3(NSEA), AUX4(NSEA)
    !REAL                    :: AUXE(NSEA,0:NOSWLL), AUXEF(NSEA,E3DF(2,1):E3DF(3,1))
    !REAL,    ALLOCATABLE    :: AUX2D1(:,:), AUX2D2(:,:), AUX2D3(:,:)
    !REAL,    ALLOCATABLE    :: AUX3DEF(:,:,:), AUX3DE(:,:,:)
    !LOGICAL                 :: WAUX1, WAUX2, WAUX3, WAUXE, WAUXEF
    !INTEGER                 :: VARID, NCLOOP
    !CHARACTER(LEN=16)       :: FLDSTR1, FLDSTR2, FLDSTR3, FLDSTRE
    !CHARACTER(LEN=16)       :: UNITSTR1, UNITSTR2, UNITSTR3, UNITSTRE
    !CHARACTER(LEN=128)      :: LNSTR1, LNSTR2, LNSTR3, LNSTRE
    !INTEGER                 :: EF_LEN
    !INTEGER                 :: NCID, DIMID(5)
    integer    ,target       :: dimid3(3)
    integer    ,target       :: dimid4(4)
    integer    ,pointer      :: dimid(:)

    CHARACTER(len=1024)     :: FNAME
    LOGICAL                 :: EXISTS

    integer :: ncid, ierr, xtid, ytid, stid, btid, mtid, ptid, ktid, timid, varid
    integer :: i,j,n,nt,ii
    logical :: s_axis = .false., b_axis = .false., m_axis = .false., p_axis = .false., k_axis = .false.

    integer, parameter :: len_s = noswll + 1
    integer, parameter :: len_b = 3 ! currently hardwired to 3 bedform variables
    integer, parameter :: len_m = P2MSF(3)-P2MSF(2) + 1
    integer, parameter :: len_p = usspf(2)
    integer, parameter :: len_k = e3df(3,1) - e3df(2,1) + 1

    interface writevar
       module procedure write_var
       module procedure write_var_s
    end interface

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

    !ALLOCATE ( AUX2D1(NX,NY), AUX2D2(NX,NY), AUX2D3(NX,NY), AUX3DE(NX,NY,0:NOSWLL) )
    !ALLOCATE ( AUX3DEF(NX,NY,E3DF(2,1):E3DF(3,1)) )

    !ef_len = e3df(3,1) - e3df(2,1) + 1
    !inquire(file=trim(fname),exist=exists)
    !if (.not. exists) then
    !   ierr = nf90_create(trim(fname),nf90_clobber,ncid)
    !   call handle_err(ierr,'create')
    !   ierr = nf90_def_dim(ncid,'nx',nx,dimid(1))
    !   call handle_err(ierr,'def_dimid1')
    !   ierr = nf90_def_dim(ncid,'ny',ny,dimid(2))
    !   call handle_err(ierr,'def_dimid2')
    !   ierr = nf90_def_dim(ncid,'noswll',noswll+1,dimid(3))
    !   call handle_err(ierr,'def_dimid3')
    !   ierr = nf90_def_dim(ncid,'freq', ef_len, dimid(4)) !ef_len=25
    !   call handle_err(ierr,'def_dimid4')
    !   ierr = nf90_def_dim(ncid,'time', nf90_unlimited, dimid(5))
    !   call handle_err(ierr,'def_dimid5')
    !   ! define time axis
    !   ierr = nf90_def_var(ncid, 'time', nf90_double, (/dimid(5)/), varid)
    !   call handle_err(ierr,'def_timevar')
    !   ierr = nf90_put_att(ncid, varid, 'units', trim(time_origin))
    !   call handle_err(ierr,'def_time_units')
    !   ierr = nf90_put_att(ncid, varid, 'calendar', trim(calendar_name))
    !   call handle_err(ierr,'def_time_calendar')
    !else
    !   ierr = nf90_open(trim(fname),nf90_write,ncid)
    !   call handle_err(ierr,'open')
    !endif

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

    ! define the dimensions required for the requested gridded fields
    do n = 1,len(outvars)
       if (outvars(n)%validout) then
          if(scan(trim(outvars(n)%dims),'s') > 0)s_axis = .true.
          if(scan(trim(outvars(n)%dims),'b') > 0)b_axis = .true.
          if(scan(trim(outvars(n)%dims),'m') > 0)m_axis = .true.
          if(scan(trim(outvars(n)%dims),'p') > 0)p_axis = .true.
          if(scan(trim(outvars(n)%dims),'k') > 0)k_axis = .true.
       end if
     end do

    ierr = nf90_create(trim(fname),nf90_clobber,ncid)
    ierr = nf90_def_dim(ncid,'nx',nx,xtid)
    ierr = nf90_def_dim(ncid,'ny',ny,ytid)
    ierr = nf90_def_dim(ncid,'time', nf90_unlimited, timid)

    if (s_axis) ierr = nf90_def_dim(ncid,'noswll',len_s,stid)
    if (b_axis) ierr = nf90_def_dim(ncid,'nb'    ,len_b,btid)
    if (m_axis) ierr = nf90_def_dim(ncid,'nm'    ,len_m,mtid)
    if (p_axis) ierr = nf90_def_dim(ncid,'np'    ,len_p,ptid)
    if (k_axis) ierr = nf90_def_dim(ncid,'freq'  ,len_k,ktid)

    ! define time axis
    ierr = nf90_put_att(ncid, timid, 'units'   , trim(time_origin))
    ierr = nf90_put_att(ncid, timid, 'calendar', trim(calendar_name))

    ! define the variables
    dimid3(1:2) = (/xtid, ytid/)
    dimid4(1:2) = (/xtid, ytid/)
    do n = 1,len(outvars)
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
       ierr = nf90_put_att(ncid, varid, 'units'     , trim(outvars(n)%unit_name))
       ierr = nf90_put_att(ncid, varid, 'long_name' , trim(outvars(n)%long_name))
       ierr = nf90_put_att(ncid, varid, '_FillValue', undef)
     end do
       ierr = nf90_enddef(ncid)

     ! write the requested variables
     ierr = nf90_put_var(ncid, timid, elapsed_secs)

     do n = 1,len(outvars)
      vname = trim(outvars(n)%var_name)
      if(vname .eq.  'CX')call write_var(trim(fname), vname,  CX)
      if(vname .eq.  'CY')call write_var(trim(fname), vname,  CY)
      if(vname .eq. 'PHS')call write_var(trim(fname), vname, PHS)
     end do

    ! Flush the buffers for write
    call W3SETA ( IGRD, NDSE, NDST )

  end subroutine W3IOGONCD

!/ ------------------------------------------------------------------- /
  subroutine write_var(fname, vname, var)

    USE W3GDATMD, ONLY: MAPSF, NSEA
    USE W3ODATMD, ONLY: UNDEF

    character(len=*),      intent(in) :: fname
    character(len=*),      intent(in) :: vname
    real, dimension(nsea), intent(in) :: var

    ! local variables
    integer                :: i
    real, dimension(nx,ny) :: var2d

    var2d = undef
    do i = 1,nsea
     var2d(mapsf(isea,1),mapsf(isea,2)) = var(i)
    end do

    ierr = nf90_open(trim(fname),   nc_write,  ncid)
    ierr = nf90_inq_varid(ncid,  trim(vname), varid)
    ierr = nf90_put_varid(ncid, varid, var2d)
    ierr = nf90_close(ncid)

  end subroutine write_var

!/ ------------------------------------------------------------------- /
  subroutine write_var_s(fname, vname, var)

    USE W3GDATMD, ONLY: MAPSF, NSEA
    USE W3ODATMD, ONLY: NOSWLL, UNDEF

    character(len=*), intent(in) :: fname
    character(len=*), intent(in) :: vname
    real, dimension(nsea,0:noswll), intent(in) :: var

    ! local variables
    integer                :: i
    real, dimension(nx,ny,0:nswll) :: var3d

    var3d = undef
    do i = 1,nsea
     var3d(mapsf(isea,1),mapsf(isea,2),0:noswll) = var(i,0:nswll)
    end do

    ierr = nf90_open(trim(fname),  nc_write,  ncid)
    ierr = nf90_inq_varid(ncid, trim(vname), varid)
    ierr = nf90_put_varid(ncid, varid, var3d)
    ierr = nf90_close(ncid)

  end subroutine write_var_s

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
