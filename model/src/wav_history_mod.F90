!> @file wav_history_mod
!!
!> @brief Manage gridded model output as netCDF using PIO
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 01-05-2022
module wav_history_mod

  use constants         , only : rade
  use w3parall          , only : init_get_isea
  use w3gdatmd          , only : xgrd, ygrd
  use w3gdatmd          , only : nk, nx, ny, mapsf, mapsta, nsea
  use w3odatmd          , only : undef
  use w3adatmd          , only : mpi_comm_wave
  use wav_import_export , only : nseal_cpl
  use wav_pio_mod       , only : pio_iotype, pio_ioformat, wav_pio_subsystem
  use wav_pio_mod       , only : handle_err, wav_pio_initdecomp
  use pio
  use netcdf

  implicit none

  private

  public :: wav_history_init
  public :: write_history
  public :: varatts
  public :: outvars

  ! used/reused in module
  integer             :: isea, jsea, ix, iy, ierr

  real, allocatable, target :: var3ds(:,:)
  real, allocatable, target :: var3dm(:,:)
  real, allocatable, target :: var3dp(:,:)
  real, allocatable, target :: var3dk(:,:)

  ! output variable for (nx,ny,nz) fields
  real, pointer :: var3d(:,:)

  type(file_desc_t) :: pioid
  type(var_desc_t)  :: varid
  type(io_desc_t)   :: iodesc2d    !2d only
  type(io_desc_t)   :: iodesc2dint !2d only, integer
  type(io_desc_t)   :: iodesc3ds   !s-axis variables
  type(io_desc_t)   :: iodesc3dm   !m-axis variables
  type(io_desc_t)   :: iodesc3dp   !p-axis variables
  type(io_desc_t)   :: iodesc3dk   !k-axis variables

  ! variable attributes
  type :: varatts
    character(len= 5) :: tag
    character(len=10) :: var_name
    character(len=48) :: long_name
    character(len=10) :: unit_name
    character(len= 2) :: dims
    logical           :: validout
  end type varatts

  type(varatts), dimension(:), allocatable :: outvars

  !===============================================================================
contains
  !===============================================================================
  !> Write the requested list of fields using parallel netCDF via PIO
  !!
  !! @param[in]     timen    the timestamp for the file
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 08-26-2024
  subroutine write_history ( timen )

    use w3odatmd   , only : fnmpre
    use w3gdatmd   , only : filext, trigp, ntri, ungtype, gtype
    use w3servmd   , only : extcde
    use w3wdatmd   , only : wlv, ice, icef, iceh, berg, ust, ustdir, asf, rhoair
    use w3gdatmd   , only : e3df, p2msf, us3df, usspf
    use w3odatmd   , only : noswll
    use w3odatmd   , only : ndso, iaproc
    use w3adatmd   , only : dw, ua, ud, as, cx, cy, taua, tauadir
    use w3adatmd   , only : hs, wlm, t02, t0m1, t01, fp0, thm, ths, thp0, wbt, wnmean
    use w3adatmd   , only : dtdyn
    use w3adatmd   , only : fcut, aba, abd, uba, ubd, sxx, syy, sxy
    use w3adatmd   , only : phs, ptp, plp, pdir, psi, pws, pwst, pnr
    use w3adatmd   , only : pthp0, pqp, ppe, pgw, psw, ptm1, pt1, pt2
    use w3adatmd   , only : pep, tauox, tauoy, tauwix, tauwiy
    use w3adatmd   , only : phiaw, phioc, tusx, tusy, prms, tpms
    use w3adatmd   , only : ussx, ussy, mssx, mssy, mscx, mscy
    use w3adatmd   , only : tauwnx, tauwny, charn, tws, bhd
    use w3adatmd   , only : phibbl, taubbl, whitecap, bedforms, cge, ef
    use w3adatmd   , only : cflxymax, cflthmax, cflkmax, p2sms, us3d
    use w3adatmd   , only : hsig, phice, tauice
    use w3adatmd   , only : stmaxe, stmaxd, hmaxe, hcmaxe, hmaxd, hcmaxd, ussp, tauocx, tauocy
    use w3adatmd   , only : usshx, usshy

    use w3timemd   , only : set_user_timestring
    use w3odatmd   , only : time_origin, calendar_name, elapsed_secs
    use w3odatmd   , only : user_histfname
    !TODO: use unstr_mesh from wav_shr_mod; currently fails due to CI
    !use wav_shr_mod      , only : unstr_mesh

    integer, intent(in)   :: timen(2)

    ! local variables
    integer    ,target  :: dimid3(3)
    integer    ,target  :: dimid4(4)
    integer    ,pointer :: dimid(:)
    character(len=1024) :: fname
    character(len=12)   :: vname
    character(len=16)   :: user_timestring    !YYYY-MM-DD-SSSSS

    integer :: n, xtid, ytid, xeid, ztid, stid, mtid, ptid, ktid, timid, nmode
    integer :: len_s, len_m, len_p, len_k
    logical :: s_axis = .false., m_axis = .false., p_axis = .false., k_axis = .false.

    integer :: lmap(nseal_cpl)

    ! -------------------------------------------------------------
    ! create the netcdf file
    ! -------------------------------------------------------------

    ! native WW3 file naming
    if (len_trim(user_histfname) == 0) then
      write(fname,'(a,i8.8,a1,i6.6,a)')trim(fnmpre),timen(1),'.',timen(2),'.out_grd.ww3.nc'
    else
      call set_user_timestring(timen,user_timestring)
      fname = trim(user_histfname)//trim(user_timestring)//'.nc'
    end if

    pioid%fh = -1
    nmode = pio_clobber
    ! only applies to classic NETCDF files.
    if (pio_iotype == PIO_IOTYPE_NETCDF .or. pio_iotype == PIO_IOTYPE_PNETCDF) then
      nmode = ior(nmode,pio_ioformat)
    endif
    ierr = pio_createfile(wav_pio_subsystem, pioid, pio_iotype, trim(fname), nmode)
    call handle_err(ierr, 'pio_create')
    if (iaproc == 1) write(ndso,'(a)')' Writing history file '//trim(fname)

    len_s = noswll + 1                  ! 0:noswll
    len_m = p2msf(3)-p2msf(2) + 1       ! ?
    len_p = usspf(2)                    ! partitions
    len_k = e3df(3,1) - e3df(2,1) + 1   ! frequencies

    ! define the dimensions required for the requested gridded fields
    do n = 1,size(outvars)
      if (outvars(n)%validout) then
        if(trim(outvars(n)%dims) == 's')s_axis = .true.
        if(trim(outvars(n)%dims) == 'm')m_axis = .true.
        if(trim(outvars(n)%dims) == 'p')p_axis = .true.
        if(trim(outvars(n)%dims) == 'k')k_axis = .true.
      end if
    end do

    ! allocate arrays if needed
    if (s_axis) allocate(var3ds(1:nseal_cpl,len_s))
    if (m_axis) allocate(var3dm(1:nseal_cpl,len_m))
    if (p_axis) allocate(var3dp(1:nseal_cpl,len_p))
    if (k_axis) allocate(var3dk(1:nseal_cpl,len_k))

    ierr = pio_def_dim(pioid, 'nx', nx, xtid)
    ierr = pio_def_dim(pioid, 'ny', ny, ytid)
    ierr = pio_def_dim(pioid, 'time', PIO_UNLIMITED, timid)

    if (s_axis) ierr = pio_def_dim(pioid, 'noswll', len_s, stid)
    if (m_axis) ierr = pio_def_dim(pioid, 'nm'    , len_m, mtid)
    if (p_axis) ierr = pio_def_dim(pioid, 'np'    , len_p, ptid)
    if (k_axis) ierr = pio_def_dim(pioid, 'freq'  , len_k, ktid)
    if (gtype .eq. ungtype) then
      ierr = pio_def_dim(pioid, 'ne'  , ntri, xeid)
      ierr = pio_def_dim(pioid, 'nn'  ,    3, ztid)
    end if

    ! define the time variable
    ierr = pio_def_var(pioid, 'time', PIO_DOUBLE, (/timid/), varid)
    call handle_err(ierr,'def_timevar')
    ierr = pio_put_att(pioid, varid, 'units', trim(time_origin))
    call handle_err(ierr,'def_time_units')
    ierr = pio_put_att(pioid, varid, 'calendar', trim(calendar_name))
    call handle_err(ierr,'def_time_calendar')

    ! define the spatial axis variables (lat,lon)
    ierr = pio_def_var(pioid, 'lon', PIO_DOUBLE, (/xtid,ytid/), varid)
    call handle_err(ierr,'def_lonvar')
    ierr = pio_put_att(pioid, varid, 'units', 'degrees_east')
    ierr = pio_def_var(pioid, 'lat', PIO_DOUBLE, (/xtid,ytid/), varid)
    call handle_err(ierr,'def_latvar')
    ierr = pio_put_att(pioid, varid, 'units', 'degrees_north')

    ! add mapsta
    ierr = pio_def_var(pioid, 'mapsta', PIO_INT, (/xtid, ytid, timid/), varid)
    call handle_err(ierr, 'def_mapsta')
    ierr = pio_put_att(pioid, varid, 'units', 'unitless')
    ierr = pio_put_att(pioid, varid, 'long_name', 'map status')
    ierr = pio_put_att(pioid, varid, '_FillValue', nf90_fill_int)

    if (gtype .eq. ungtype) then
      ierr = pio_def_var(pioid, 'nconn', PIO_INT, (/ztid,xeid/), varid)
      call handle_err(ierr,'def_nodeconnections')
      ierr = pio_put_att(pioid, varid, 'units', 'unitless')
      ierr = pio_put_att(pioid, varid, 'long_name', 'node connectivity')
    end if

    ! define the variables
    dimid3(1:2) = (/xtid, ytid/)
    dimid4(1:2) = (/xtid, ytid/)
    do n = 1,size(outvars)
      if (trim(outvars(n)%dims) == 's') then
        dimid4(3:4) = (/stid, timid/)
        dimid => dimid4
      else if (trim(outvars(n)%dims) == 'm') then
        dimid4(3:4) = (/mtid, timid/)
        dimid => dimid4
      else if (trim(outvars(n)%dims) == 'p') then
        dimid4(3:4) = (/ptid, timid/)
        dimid => dimid4
      else if (trim(outvars(n)%dims) == 'k') then
        dimid4(3:4) = (/ktid, timid/)
        dimid => dimid4
      else
        dimid3(3) = timid
        dimid => dimid3
      end if

      ierr = pio_def_var(pioid, trim(outvars(n)%var_name), PIO_REAL, dimid, varid)
      call handle_err(ierr, 'define variable '//trim((outvars(n)%var_name)))
      ierr = pio_put_att(pioid, varid, 'units'     , trim(outvars(n)%unit_name))
      ierr = pio_put_att(pioid, varid, 'long_name' , trim(outvars(n)%long_name))
      ierr = pio_put_att(pioid, varid, '_FillValue', undef)
    end do
    ! end variable definitions
    ierr = pio_enddef(pioid)
    call handle_err(ierr, 'end variable definition')

    call wav_pio_initdecomp(iodesc2d)
    call wav_pio_initdecomp(iodesc2dint, use_int=.true.)
    if (s_axis)call wav_pio_initdecomp(len_s, iodesc3ds)
    if (m_axis)call wav_pio_initdecomp(len_m, iodesc3dm)
    if (p_axis)call wav_pio_initdecomp(len_p, iodesc3dp)
    if (k_axis)call wav_pio_initdecomp(len_k, iodesc3dk)

    ! write the time and spatial axis values (lat,lon,time)
    ierr = pio_inq_varid(pioid,  'lat', varid)
    call handle_err(ierr, 'inquire variable lat ')
    ierr = pio_put_var(pioid, varid, transpose(ygrd))
    call handle_err(ierr, 'put lat')

    ierr = pio_inq_varid(pioid,  'lon', varid)
    call handle_err(ierr, 'inquire variable lon ')
    ierr = pio_put_var(pioid, varid, transpose(xgrd))
    call handle_err(ierr, 'put lon')

    ierr = pio_inq_varid(pioid,  'time', varid)
    call handle_err(ierr, 'inquire variable time ')
    ierr = pio_put_var(pioid, varid, (/1/), real(elapsed_secs,8))
    call handle_err(ierr, 'put time')

    if (gtype .eq. ungtype) then
      ierr = pio_inq_varid(pioid,  'nconn', varid)
      call handle_err(ierr, 'inquire variable nconn ')
      ierr = pio_put_var(pioid, varid, trigp)
      call handle_err(ierr, 'put trigp')
    end if

    ! TODO: tried init decomp w/ use_int=.true. but getting garbage
    ! land values....sea values OK
    ! mapsta is global
    lmap(:) = 0
    do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      lmap(jsea) = mapsta(iy,ix)
    end do
    ierr = pio_inq_varid(pioid,  'mapsta', varid)
    call handle_err(ierr, 'inquire variable mapsta ')
    call pio_setframe(pioid, varid, int(1,kind=Pio_Offset_Kind))
    call pio_write_darray(pioid, varid, iodesc2dint, lmap, ierr)
    call handle_err(ierr, 'put variable mapsta')

    ! write the requested variables
    do n = 1,size(outvars)
       vname = trim(outvars(n)%var_name)
      if (trim(outvars(n)%dims) == 's') then
        var3d => var3ds
        ! Group 4
        if(vname .eq.      'PHS') call write_var3d(iodesc3ds, vname, phs      (1:nseal_cpl,0:noswll) )
        if(vname .eq.      'PTP') call write_var3d(iodesc3ds, vname, ptp      (1:nseal_cpl,0:noswll) )
        if(vname .eq.      'PLP') call write_var3d(iodesc3ds, vname, plp      (1:nseal_cpl,0:noswll) )
        if(vname .eq.     'PDIR') call write_var3d(iodesc3ds, vname, pdir     (1:nseal_cpl,0:noswll), fldir='true' )
        if(vname .eq.      'PSI') call write_var3d(iodesc3ds, vname, psi      (1:nseal_cpl,0:noswll), fldir='true' )
        if(vname .eq.      'PWS') call write_var3d(iodesc3ds, vname, pws      (1:nseal_cpl,0:noswll) )
        if(vname .eq.      'PDP') call write_var3d(iodesc3ds, vname, pthp0    (1:nseal_cpl,0:noswll), fldir='true' )
        if(vname .eq.      'PQP') call write_var3d(iodesc3ds, vname, pqp      (1:nseal_cpl,0:noswll) )
        if(vname .eq.      'PPE') call write_var3d(iodesc3ds, vname, ppe      (1:nseal_cpl,0:noswll) )
        if(vname .eq.      'PGW') call write_var3d(iodesc3ds, vname, pgw      (1:nseal_cpl,0:noswll) )
        if(vname .eq.      'PSW') call write_var3d(iodesc3ds, vname, psw      (1:nseal_cpl,0:noswll) )
        if(vname .eq.     'PTM1') call write_var3d(iodesc3ds, vname, ptm1     (1:nseal_cpl,0:noswll) )
        if(vname .eq.      'PT1') call write_var3d(iodesc3ds, vname, pt1      (1:nseal_cpl,0:noswll) )
        if(vname .eq.      'PT2') call write_var3d(iodesc3ds, vname, pt2      (1:nseal_cpl,0:noswll) )
        if(vname .eq.      'PEP') call write_var3d(iodesc3ds, vname, pep      (1:nseal_cpl,0:noswll) )

      else if (trim(outvars(n)%dims) == 'm') then                          ! m axis
        var3d => var3dm
        ! Group 6
        if (vname .eq.   'P2SMS') call write_var3d(iodesc3dm, vname, p2sms    (1:nseal_cpl,p2msf(2):p2msf(3)) )

      else if (trim(outvars(n)%dims) == 'p') then                          ! partition axis
        var3d => var3dp
        ! Group 6
        if (vname .eq.   'USSPX') call write_var3d(iodesc3dp, vname, ussp     (1:nseal_cpl,   1:usspf(2)) )
        if (vname .eq.   'USSPY') call write_var3d(iodesc3dp, vname, ussp     (1:nseal_cpl,nk+1:nk+usspf(2)) )

      else if (trim(outvars(n)%dims) == 'k') then                           ! freq axis
        var3d => var3dk
        ! Group 3
        if(vname .eq.       'EF') call write_var3d(iodesc3dk, vname, ef       (1:nseal_cpl,e3df(2,1):e3df(3,1)) )
        if(vname .eq.     'TH1M') call write_var3d(iodesc3dk, vname, ef       (1:nseal_cpl,e3df(2,2):e3df(3,2)) )
        if(vname .eq.    'STH1M') call write_var3d(iodesc3dk, vname, ef       (1:nseal_cpl,e3df(2,3):e3df(3,3)) )
        if(vname .eq.     'TH2M') call write_var3d(iodesc3dk, vname, ef       (1:nseal_cpl,e3df(2,4):e3df(3,4)) )
        if(vname .eq.    'STH2M') call write_var3d(iodesc3dk, vname, ef       (1:nseal_cpl,e3df(2,5):e3df(3,5)) )
        !TODO: wn has reversed indices (1:nk, 1:nseal_cpl)
        ! Group 6
        if (vname .eq.   'US3DX') call write_var3d(iodesc3dk, vname, us3d     (1:nseal_cpl,   us3df(2):us3df(3)) )
        if (vname .eq.   'US3DY') call write_var3d(iodesc3dk, vname, us3d     (1:nseal_cpl,nk+us3df(2):nk+us3df(3)) )

      else
        ! Group 1
        if (vname .eq.      'DW') call write_var2d(vname, dw       (1:nsea), init0='false', global='true')
        if (vname .eq.      'CX') call write_var2d(vname, cx       (1:nsea), init0='false', global='true')
        if (vname .eq.      'CY') call write_var2d(vname, cy       (1:nsea), init0='false', global='true')
        if (vname .eq.     'UAX') call write_var2d(vname, ua       (1:nsea), dir=cos(ud(1:nsea)), init0='false', global='true')
        if (vname .eq.     'UAY') call write_var2d(vname, ua       (1:nsea), dir=sin(ud(1:nsea)), init0='false', global='true')
        if (vname .eq.      'AS') call write_var2d(vname, as       (1:nsea), init0='false', global='true')
        if (vname .eq.     'WLV') call write_var2d(vname, wlv      (1:nsea), init0='false', global='true')
        if (vname .eq.     'ICE') call write_var2d(vname, ice      (1:nsea), init0='false', global='true')
        if (vname .eq.     'IBG') call write_var2d(vname, berg     (1:nsea), init0='false', global='true')
        if (vname .eq.    'TAUX') call write_var2d(vname, taua     (1:nsea), dir=cos(tauadir(1:nsea)), init0='false', global='true')
        if (vname .eq.    'TAUY') call write_var2d(vname, taua     (1:nsea), dir=sin(tauadir(1:nsea)), init0='false', global='true')
        if (vname .eq.  'RHOAIR') call write_var2d(vname, rhoair   (1:nsea), init0='false', global='true')
        if (vname .eq.    'ICEH') call write_var2d(vname, iceh     (1:nsea), init0='false', global='true')
        if (vname .eq.    'ICEF') call write_var2d(vname, icef     (1:nsea), init0='false', global='true')

        ! Group 2
        if (vname .eq.      'HS') call write_var2d(vname, hs       (1:nseal_cpl) )
        if (vname .eq.     'WLM') call write_var2d(vname, wlm      (1:nseal_cpl) )
        if (vname .eq.     'T02') call write_var2d(vname, t02      (1:nseal_cpl) )
        if (vname .eq.    'T0M1') call write_var2d(vname, t0m1     (1:nseal_cpl) )
        if (vname .eq.     'T01') call write_var2d(vname, t01      (1:nseal_cpl) )
        if (vname .eq.     'FP0') call write_var2d(vname, fp0      (1:nseal_cpl) )
        if (vname .eq.     'THM') call write_var2d(vname, thm      (1:nseal_cpl), fldir='true' )
        if (vname .eq.     'THS') call write_var2d(vname, ths      (1:nseal_cpl), fldir='true' )
        if (vname .eq.    'THP0') call write_var2d(vname, thp0     (1:nseal_cpl), fldir='true' )
        if (vname .eq.    'HSIG') call write_var2d(vname, hsig     (1:nseal_cpl) )
        if (vname .eq.  'STMAXE') call write_var2d(vname, stmaxe   (1:nseal_cpl) )
        if (vname .eq.  'STMAXD') call write_var2d(vname, stmaxd   (1:nseal_cpl) )
        if (vname .eq.   'HMAXE') call write_var2d(vname, hmaxe    (1:nseal_cpl) )
        if (vname .eq.  'HCMAXE') call write_var2d(vname, hcmaxe   (1:nseal_cpl) )
        if (vname .eq.   'HMAXD') call write_var2d(vname, hmaxd    (1:nseal_cpl) )
        if (vname .eq.  'HCMAXD') call write_var2d(vname, hcmaxd   (1:nseal_cpl) )
        if (vname .eq.     'WBT') call write_var2d(vname, wbt      (1:nseal_cpl) )
        if (vname .eq.  'WNMEAN') call write_var2d(vname, wnmean   (1:nseal_cpl), init0='false')

        ! Group 4
        if(vname .eq.     'PWST') call write_var2d(vname, pwst     (1:nseal_cpl) )
        if(vname .eq.      'PNR') call write_var2d(vname, pnr      (1:nseal_cpl) )

        ! Group 5
        if (vname .eq.    'USTX') call write_var2d(vname, ust      (1:nseal_cpl)*asf(1:nseal_cpl), dir=cos(ustdir(1:nseal_cpl)), usemask='true')
        if (vname .eq.    'USTY') call write_var2d(vname, ust      (1:nseal_cpl)*asf(1:nseal_cpl), dir=sin(ustdir(1:nseal_cpl)), usemask='true')
        if (vname .eq.   'CHARN') call write_var2d(vname, charn    (1:nseal_cpl) )
        if (vname .eq.     'CGE') call write_var2d(vname, cge      (1:nseal_cpl) )
        if (vname .eq.   'PHIAW') call write_var2d(vname, phiaw    (1:nseal_cpl),   init2='true')
        if (vname .eq.  'TAUWIX') call write_var2d(vname, tauwix   (1:nseal_cpl),   init2='true')
        if (vname .eq.  'TAUWIY') call write_var2d(vname, tauwiy   (1:nseal_cpl),   init2='true')
        if (vname .eq.  'TAUWNX') call write_var2d(vname, tauwnx   (1:nseal_cpl),   init2='true')
        if (vname .eq.  'TAUWNY') call write_var2d(vname, tauwny   (1:nseal_cpl),   init2='true')
        if (vname .eq.     'WCC') call write_var2d(vname, whitecap (1:nseal_cpl,1), init2='true')
        if (vname .eq.     'WCF') call write_var2d(vname, whitecap (1:nseal_cpl,2), init2='true')
        if (vname .eq.     'WCH') call write_var2d(vname, whitecap (1:nseal_cpl,3), init2='true')
        if (vname .eq.     'WCM') call write_var2d(vname, whitecap (1:nseal_cpl,4), init2='true')
        if (vname .eq.     'TWS') call write_var2d(vname, tws      (1:nseal_cpl) )

        ! Group 6
        if (vname .eq.     'SXX') call write_var2d(vname, sxx      (1:nseal_cpl) )
        if (vname .eq.     'SYY') call write_var2d(vname, syy      (1:nseal_cpl) )
        if (vname .eq.     'SXY') call write_var2d(vname, sxy      (1:nseal_cpl) )
        if (vname .eq.   'TAUOX') call write_var2d(vname, tauox    (1:nseal_cpl),   init2='true')
        if (vname .eq.   'TAUOY') call write_var2d(vname, tauoy    (1:nseal_cpl),   init2='true')
        if (vname .eq.     'BHD') call write_var2d(vname, bhd      (1:nseal_cpl) )
        if (vname .eq.   'PHIOC') call write_var2d(vname, phioc    (1:nseal_cpl),   init2='true')
        if (vname .eq.    'TUSX') call write_var2d(vname, tusx     (1:nseal_cpl) )
        if (vname .eq.    'TUSY') call write_var2d(vname, tusy     (1:nseal_cpl) )
        if (vname .eq.    'USSX') call write_var2d(vname, ussx     (1:nseal_cpl) )
        if (vname .eq.    'USSY') call write_var2d(vname, ussy     (1:nseal_cpl) )
        if (vname .eq.    'PRMS') call write_var2d(vname, prms     (1:nseal_cpl) )
        if (vname .eq.    'TPMS') call write_var2d(vname, tpms     (1:nseal_cpl) )
        if (vname .eq. 'TAUICEX') call write_var2d(vname, tauice   (1:nseal_cpl,1) )
        if (vname .eq. 'TAUICEY') call write_var2d(vname, tauice   (1:nseal_cpl,2) )
        if (vname .eq.   'PHICE') call write_var2d(vname, phice    (1:nseal_cpl) )
        if (vname .eq.  'TAUOCX') call write_var2d(vname, tauocx   (1:nseal_cpl) )
        if (vname .eq.  'TAUOCY') call write_var2d(vname, tauocy   (1:nseal_cpl) )
        if (vname .eq.   'USSHX') call write_var2d(vname, usshx    (1:nseal_cpl) )
        if (vname .eq.   'USSHY') call write_var2d(vname, usshy    (1:nseal_cpl) )
        ! Group 7
        if (vname .eq.    'ABAX') call write_var2d(vname, aba      (1:nseal_cpl), dir=cos(abd(1:nseal_cpl)) )
        if (vname .eq.    'ABAY') call write_var2d(vname, aba      (1:nseal_cpl), dir=sin(abd(1:nseal_cpl)) )
        if (vname .eq.    'UBAX') call write_var2d(vname, uba      (1:nseal_cpl), dir=cos(ubd(1:nseal_cpl)) )
        if (vname .eq.    'UBAY') call write_var2d(vname, uba      (1:nseal_cpl), dir=sin(ubd(1:nseal_cpl)) )
        if (vname .eq.     'BED') call write_var2d(vname, bedforms (1:nseal_cpl,1), init2='true')
        if (vname .eq. 'RIPPLEX') call write_var2d(vname, bedforms (1:nseal_cpl,2), init2='true')
        if (vname .eq. 'RIPPLEY') call write_var2d(vname, bedforms (1:nseal_cpl,3), init2='true')
        if (vname .eq.  'PHIBBL') call write_var2d(vname, phibbl   (1:nseal_cpl),   init2='true')
        if (vname .eq. 'TAUBBLX') call write_var2d(vname, taubbl   (1:nseal_cpl,1), init2='true')
        if (vname .eq. 'TAUBBLY') call write_var2d(vname, taubbl   (1:nseal_cpl,2), init2='true')

        ! Group 8
        if (vname .eq.    'MSSX') call write_var2d(vname, mssx     (1:nseal_cpl) )
        if (vname .eq.    'MSSY') call write_var2d(vname, mssy     (1:nseal_cpl) )
        if (vname .eq.    'MSCX') call write_var2d(vname, mscx     (1:nseal_cpl) )
        if (vname .eq.    'MSCY') call write_var2d(vname, mscy     (1:nseal_cpl) )
        !TODO: remaining variables have inconsistency between shel_inp listing and iogo code

        ! Group 9
        if (vname .eq.   'DTDYN') call write_var2d(vname, dtdyn    (1:nseal_cpl) )
        if (vname .eq.    'FCUT') call write_var2d(vname, fcut     (1:nseal_cpl) )
        if (vname .eq.'CFLXYMAX') call write_var2d(vname, cflxymax (1:nseal_cpl) )
        if (vname .eq.'CFLTHMAX') call write_var2d(vname, cflthmax (1:nseal_cpl) )
        if (vname .eq. 'CFLKMAX') call write_var2d(vname, cflkmax  (1:nseal_cpl) )

        ! Group 10
      end if
    end do

    if (s_axis) deallocate(var3ds)
    if (m_axis) deallocate(var3dm)
    if (p_axis) deallocate(var3dp)
    if (k_axis) deallocate(var3dk)

    call pio_freedecomp(pioid,iodesc2d)
    call pio_freedecomp(pioid,iodesc2dint)
    if (s_axis) call pio_freedecomp(pioid, iodesc3ds)
    if (m_axis) call pio_freedecomp(pioid, iodesc3dm)
    if (p_axis) call pio_freedecomp(pioid, iodesc3dp)
    if (k_axis) call pio_freedecomp(pioid, iodesc3dk)

    call pio_closefile(pioid)

  end subroutine write_history

  !===============================================================================
  !>  Write an array of (nseal) points as (nx,ny)
  !!
  !! @details  If dir is present, the written variable will represent either the X
  !! or Y component of the variable. If mask is present and true, use mapsta=1 to
  !! mask values. If init0 is present and false, do not initialize values for mapsta<0.
  !! This prevents group 1 variables being set undef over ice. If init2 is present and
  !! true, apply a second initialization where mapsta==2. If fldir is present and true
  !! then the directions will be converted to degrees. If global is present and true,
  !! write pe-local copy of global field
  !!
  !! @param[in]    vname      the variable name
  !! @param[in]    var        the variable array
  !! @param[in]    dir        the direction array, optional
  !! @param[in]    usemask    a flag for variable masking, optional
  !! @param[in]    init0      a flag for variable initialization, optional
  !! @param[in]    init2      a flag for a second initialization type, optional
  !! @param[in]    fldir      a flag for unit conversion for direction, optional
  !! @param[in]    global     a flag for a global variable, optional
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 08-26-2024
  subroutine write_var2d(vname, var, dir, usemask, init0, init2, fldir, global)

    character(len=*),           intent(in) :: vname
    real            ,           intent(in) :: var(:)
    real, optional  ,           intent(in) :: dir(:)
    character(len=*), optional, intent(in) :: usemask
    character(len=*), optional, intent(in) :: init0
    character(len=*), optional, intent(in) :: init2
    character(len=*), optional, intent(in) :: fldir
    character(len=*), optional, intent(in) :: global

    ! local variables
    real, dimension(nseal_cpl) :: varout
    logical                    :: lmask, linit0, linit2, lfldir, lglobal
    real                       :: varloc

    lmask = .false.
    if (present(usemask)) then
      lmask = (trim(usemask) == "true")
    end if
    linit0 = .true.
    if (present(init0)) then
      linit0 = (trim(init0) == "true")
    end if
    linit2 = .false.
    if (present(init2)) then
      linit2 = (trim(init2) == "true")
    end if
    lfldir = .false.
    if (present(fldir)) then
      lfldir = (trim(fldir) == "true")
    end if
    lglobal = .false.
    if (present(global)) then
      lglobal = (trim(global) == "true")
    end if

    varout = undef
    do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      if (lglobal) then
        varloc = var(isea)
      else
        varloc = var(jsea)
      end if

      if (linit0) then
        if (mapsta(mapsf(isea,2),mapsf(isea,1)) < 0) varloc = undef
      end if
      if (linit2) then
        if (mapsta(mapsf(isea,2),mapsf(isea,1)) == 2) varloc = undef
      end if

      if (lfldir) then
        if (varloc .ne. undef) then
          varloc = mod(630. - rade*varloc, 360.)
        end if
      end if
      if (present(dir)) then
        if (varloc .ne. undef) then
          if (lmask) then
            if (mapsta(mapsf(isea,2),mapsf(isea,1)) == 1) then
              if (lglobal) then
                varout(jsea) = varloc*dir(isea)
              else
                varout(jsea) = varloc*dir(jsea)
              end if
            end if
          else
            if (lglobal) then
              varout(jsea) = varloc*dir(isea)
            else
              varout(jsea) = varloc*dir(jsea)
            end if
          end if
        end if
      else
        varout(jsea) = varloc
      end if
    end do

    ierr = pio_inq_varid(pioid,  trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    call pio_setframe(pioid, varid, int(1,kind=Pio_Offset_Kind))
    call pio_write_darray(pioid, varid, iodesc2d, varout, ierr)
    call handle_err(ierr, 'put variable '//trim(vname))

  end subroutine write_var2d

  !===============================================================================
  !>  Write an array of (nseal,:) points as (nx,ny,:)
  !!
  !! @details If init2 is present and true, apply a second initialization to a
  !! subset of variables for where mapsta==2. If fldir is present and true then
  !! the directions will be converted to degrees.
  !!
  !! @param[in]   iodesc      the PIO decomposition handle
  !! @param[in]   vname       the variable name
  !! @param[in]   var         the variable array
  !! @param[in]   init2       a flag for a second initialization type, optional
  !! @param[in]   fldir       a flag for unit conversion for direction, optional
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 08-26-2024
  subroutine write_var3d(iodesc, vname, var, init2, fldir)

    type(io_desc_t),            intent(inout) :: iodesc
    character(len=*),           intent(in) :: vname
    real            ,           intent(in) :: var(:,:)
    character(len=*), optional, intent(in) :: init2
    character(len=*), optional, intent(in) :: fldir

    ! local variables
    real, allocatable, dimension(:) :: varloc
    logical                         :: linit2, lfldir
    integer                         :: lb, ub

    linit2 = .false.
    if (present(init2)) then
      linit2 = (trim(init2) == "true")
    end if
    lfldir = .false.
    if (present(fldir)) then
      lfldir = (trim(fldir) == "true")
    end if

    lb = lbound(var,2)
    ub = ubound(var,2)
    allocate(varloc(lb:ub))

    var3d = undef
    do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      ! initialization
      varloc(:) = var(jsea,:)
      if (mapsta(mapsf(isea,2),mapsf(isea,1)) < 0) varloc(:) = undef
      if (linit2) then
        if (mapsta(mapsf(isea,2),mapsf(isea,1)) == 2) varloc(:) = undef
      end if
      if (lfldir) then
        if (mapsta(mapsf(isea,2),mapsf(isea,1)) > 0 )  then
          varloc(:) = mod(630. - rade*varloc(:), 360.)
        end if
      end if
      var3d(jsea,:) = varloc(:)
    end do

    ierr = pio_inq_varid(pioid,  trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    call pio_setframe(pioid, varid, int(1,kind=PIO_OFFSET_KIND))
    call pio_write_darray(pioid, varid, iodesc, var3d, ierr)

    deallocate(varloc)
  end subroutine write_var3d

  !===============================================================================
  !> Scan through all possible fields to determine a list of requested variables
  !!
  !> @details Utilizes the list of variables specified via WW3's flout array
  !! to define the variables written to the file. The elements of the flout
  !! array are here referred to as "tags". Tags are not 1:1 with output fields.
  !! For example, the tag "CUR" means that the ocean currents, comprising two
  !! directional values are requested. They will both be requested via the single
  !! CUR tag. The tag "SXY" means that three components of radiation stresses
  !! are requested (XX,YY,XY).
  !!
  !! @param[in]   stdout            the logfile unit on the root_task
  !!
  !> @author Denise.Worthen@noaa.gov
  !> @date 09-19-2022
  subroutine wav_history_init(stdout)

    use w3gdatmd, only: e3df, p2msf, us3df, usspf
    use w3odatmd, only: iaproc, nogrp, ngrpp
    use w3iogomd, only: fldout
    use w3servmd, only: strsplit

    integer, intent(in) :: stdout

    ! local variables
    integer, parameter :: maxvars = 25  ! maximum number of variables/group
    type(varatts), dimension(nogrp,maxvars) :: gridoutdefs

    character(len=100)      :: inptags(100) = ''
    integer                 :: j,k,n,nout
    character(len= 12)      :: ttag

    ! obtain all possible output variable tags and attributes
    call define_fields(gridoutdefs)

    ! obtain the tags for the requested output variables
    call strsplit(fldout,inptags)

    ! determine which variables are tagged for output
    do k = 1,nogrp
      do j = 1,maxvars
        if (len_trim(gridoutdefs(k,j)%tag) > 0) then
          do n = 1,len(inptags)
            if (len_trim(inptags(n)) > 0) then
              if (trim(inptags(n)) == trim(gridoutdefs(k,j)%tag)) gridoutdefs(k,j)%validout = .true.
            end if
          end do
        end if
      end do
    end do

    ! remove requested variables which are only allocated if specific
    ! options are set in mod_def (see w3adatmd, '3D arrays')
    do k = 1,nogrp
      do j = 1,maxvars
        if (gridoutdefs(k,j)%validout) then
          ttag = trim(gridoutdefs(k,j)%tag)
          if (ttag == 'EF'    .and. e3df(1,1) == 0) gridoutdefs(k,j)%validout = .false.
          if (ttag == 'TH1M'  .and. e3df(1,2) == 0) gridoutdefs(k,j)%validout = .false.
          if (ttag == 'STH1M' .and. e3df(1,3) == 0) gridoutdefs(k,j)%validout = .false.
          if (ttag == 'TH2M'  .and. e3df(1,4) == 0) gridoutdefs(k,j)%validout = .false.
          if (ttag == 'STH2M' .and. e3df(1,5) == 0) gridoutdefs(k,j)%validout = .false.

          if (ttag == 'P2L' .and. p2msf(1) == 0) gridoutdefs(k,j)%validout = .false.
          if (ttag == 'USF' .and. us3df(1) == 0) gridoutdefs(k,j)%validout = .false.
          if (ttag == 'USP' .and. usspf(1) == 0) gridoutdefs(k,j)%validout = .false.
        end if
      end do
    end do

    ! determine number of output variables (not the same as the number of tags)
    n = 0
    do k = 1,nogrp
      do j = 1,maxvars
        if (gridoutdefs(k,j)%validout) n = n+1
      end do
    end do
    nout = n
    allocate(outvars(1:nout))

    ! subset variables requested
    n = 0
    do k = 1,nogrp
      do j = 1,maxvars
        if (gridoutdefs(k,j)%validout) then
          n = n+1
          outvars(n) = gridoutdefs(k,j)
        end if
      enddo
    end do

    ! check
    if ( iaproc == 1 ) then
      write(stdout,*)
      write(stdout,'(a)')' --------------------------------------------------'
      write(stdout,'(a)')'  Requested gridded output variables : '
      write(stdout,'(a)')' --------------------------------------------------'
      write(stdout,*)
      do n = 1,nout
        write(stdout,'(i5,2a12,a50)')n,'  '//trim(outvars(n)%tag), &
             '  '//trim(outvars(n)%var_name), &
             '  '//trim(outvars(n)%long_name)
      end do
      write(stdout,*)
      call flush (stdout)
    end if

  end subroutine wav_history_init

  !====================================================================================
  !> Define the available output fields and their attributes
  !!
  !> @author Denise.Worthen@noaa.gov
  !> @date 09-19-2022
  subroutine define_fields (gridoutdefs)

    type(varatts), dimension(:,:), intent(inout) :: gridoutdefs

    gridoutdefs(:,:)%tag = ""
    gridoutdefs(:,:)%var_name = ""
    gridoutdefs(:,:)%long_name = ""
    gridoutdefs(:,:)%unit_name = ""
    gridoutdefs(:,:)%dims = ""
    gridoutdefs(:,:)%validout = .false.

    !  1   Forcing Fields
    gridoutdefs(1,1:14) = [ &
         varatts( "DPT  ", "DW        ", "Water depth                                     ", "m         ", "  ", .false.) , &
         varatts( "CUR  ", "CX        ", "Mean current, x-component                       ", "m s-1     ", "  ", .false.) , &
         varatts( "CUR  ", "CY        ", "Mean current, y-component                       ", "m s-1     ", "  ", .false.) , &
         varatts( "WND  ", "UAX       ", "Mean wind, x-component                          ", "m s-1     ", "  ", .false.) , &
         varatts( "WND  ", "UAY       ", "Mean wind, y-component                          ", "m s-1     ", "  ", .false.) , &
         varatts( "AST  ", "AS        ", "Air-sea temperature difference                  ", "K         ", "  ", .false.) , &
         varatts( "WLV  ", "WLV       ", "Water levels                                    ", "m         ", "  ", .false.) , &
         varatts( "ICE  ", "ICE       ", "Ice coverage                                    ", "nd        ", "  ", .false.) , &
         varatts( "IBG  ", "BERG      ", "Iceberg-induced damping                         ", "km-1      ", "  ", .false.) , &
         varatts( "TAUA ", "TAUAX     ", "Atm momentum x                                  ", "Pa        ", "  ", .false.) , &
         varatts( "TAUA ", "TAUAY     ", "Atm momentum y                                  ", "Pa        ", "  ", .false.) , &
         varatts( "RHO  ", "RHOAIR    ", "Air density                                     ", "kg m-3    ", "  ", .false.) , &
         varatts( "IC1  ", "ICEH      ", "Ice thickness                                   ", "m         ", "  ", .false.) , &
         varatts( "IC5  ", "ICEF      ", "Ice floe diameter                               ", "m         ", "  ", .false.)   &
         ]

    !  2   Standard mean wave Parameters
    gridoutdefs(2,1:18) = [ &
         varatts( "HS   ", "HS        ", "Significant wave height                         ", "m         ", "  ", .false.) , &
         varatts( "LM   ", "WLM       ", "Mean wave length                                ", "m         ", "  ", .false.) , &
         varatts( "T02  ", "T02       ", "Mean wave period (Tm0,2)                        ", "s         ", "  ", .false.) , &
         varatts( "T0M1 ", "T0M1      ", "Mean wave period (Tm0,-1)                       ", "s         ", "  ", .false.) , &
         varatts( "T01  ", "T01       ", "Mean wave period (Tm0,1)                        ", "s         ", "  ", .false.) , &
         varatts( "FP   ", "FP0       ", "Peak frequency                                  ", "s-1       ", "  ", .false.) , &
         varatts( "DIR  ", "THM       ", "Mean wave direction                             ", "rad       ", "  ", .false.) , &
         varatts( "SPR  ", "THS       ", "Mean directional spread                         ", "rad       ", "  ", .false.) , &
         varatts( "DP   ", "THP0      ", "Peak direction                                  ", "rad       ", "  ", .false.) , &
         varatts( "HIG  ", "HSIG      ", "Infragravity height                             ", "m         ", "  ", .false.) , &
         varatts( "MXE  ", "STMAXE    ", "Max surface elev (STE)                          ", "m         ", "  ", .false.) , &
         varatts( "MXES ", "STMAXD    ", "St Dev Max surface elev (STE)                   ", "m         ", "  ", .false.) , &
         varatts( "MXH  ", "HMAXE     ", "Max wave height (S.)                            ", "m         ", "  ", .false.) , &
         varatts( "MXHC ", "HCMAXE    ", "Max wave height from crest (STE)                ", "m         ", "  ", .false.) , &
         varatts( "SDMH ", "HMAXD     ", "St Dev of MXC (STE)                             ", "m         ", "  ", .false.) , &
         varatts( "SDMHC", "HCMAXD    ", "St Dev of MXHC (STE)                            ", "m         ", "  ", .false.) , &
         varatts( "WBT  ", "WBT       ", "Dominant wave breaking probability (b_T)        ", "nd        ", "  ", .false.) , &
         varatts( "WNM  ", "WNMEAN    ", "Mean wave number                                ", "m-1       ", "  ", .false.)   &
         ]

    !  3   Spectral Parameters
    gridoutdefs(3,1:6) = [ &
         varatts( "EF   ", "EF        ", "1D spectral density                             ", "m2 s      ", "k ", .false.) , &
         varatts( "TH1M ", "TH1M      ", "Mean wave direction from a1,b2                  ", "deg       ", "k ", .false.) , &
         varatts( "STH1M", "STH1M     ", "Directional spreading from a1,b2                ", "deg       ", "k ", .false.) , &
         varatts( "TH2M ", "TH2M      ", "Mean wave direction from a2,b2                  ", "deg       ", "k ", .false.) , &
         varatts( "STH2M", "STH2M     ", "Directional spreading from a2,b2                ", "deg       ", "k ", .false.) , &
         !TODO: has reverse indices (nk,nsea)
         varatts( "WN   ", "WN        ", "Wavenumber array                                ", "m-1       ", "k ", .false.)   &
         ]

    !  4   Spectral Partition Parameters
    gridoutdefs(4,1:17) = [ &
         varatts( "PHS  ", "PHS       ", "Partitioned wave heights                        ", "m         ", "s ", .false.) , &
         varatts( "PTP  ", "PTP       ", "Partitioned peak period                         ", "s         ", "s ", .false.) , &
         varatts( "PLP  ", "PLP       ", "Partitioned peak wave length                    ", "m         ", "s ", .false.) , &
         varatts( "PDIR ", "PDIR      ", "Partitioned mean direction                      ", "deg       ", "s ", .false.) , &
         varatts( "PSPR ", "PSI       ", "Partitioned mean directional spread             ", "deg       ", "s ", .false.) , &
         varatts( "PWS  ", "PWS       ", "Partitioned wind sea fraction                   ", "nd        ", "s ", .false.) , &
         varatts( "PDP  ", "PTHP0     ", "Peak wave direction of partition                ", "deg       ", "s ", .false.) , &
         varatts( "PQP  ", "PQP       ", "Goda peakdedness parameter of partition         ", "nd        ", "s ", .false.) , &
         varatts( "PPE  ", "PPE       ", "JONSWAP peak enhancement factor of partition    ", "s-1       ", "s ", .false.) , &
         varatts( "PGW  ", "PGW       ", "Gaussian frequency width of partition           ", "nd        ", "s ", .false.) , &
         varatts( "PSW  ", "PSW       ", "Spectral width of partition                     ", "nd        ", "s ", .false.) , &
         varatts( "PTM10", "PTM1      ", "Mean wave period (m-1,0) of partition           ", "s         ", "s ", .false.) , &
         varatts( "PT01 ", "PT1       ", "Mean wave period (m0,1) of partition            ", "s         ", "s ", .false.) , &
         varatts( "PT02 ", "PT2       ", "Mean wave period (m0,2) of partition            ", "s         ", "s ", .false.) , &
         varatts( "PEP  ", "PEP       ", "Peak spectral density of partition              ", "m2 s rad-1", "s ", .false.) , &
         varatts( "TWS  ", "PWST      ", "Total wind sea fraction                         ", "nd        ", "  ", .false.) , &
         varatts( "PNR  ", "PNR       ", "Number of partitions                            ", "nd        ", "  ", .false.)   &
         ]

    !  5   Atmosphere-waves layer
    gridoutdefs(5,1:14) = [ &
         varatts( "UST  ", "USTX      ", "Friction velocity x                             ", "m s-1     ", "  ", .false.) , &
         varatts( "UST  ", "USTY      ", "Friction velocity y                             ", "m s-1     ", "  ", .false.) , &
         varatts( "CHA  ", "CHARN     ", "Charnock parameter                              ", "nd        ", "  ", .false.) , &
         varatts( "CGE  ", "CGE       ", "Energy flux                                     ", "kW m-1    ", "  ", .false.) , &
         varatts( "FAW  ", "PHIAW     ", "Air-sea energy flux                             ", "W m-2     ", "  ", .false.) , &
         varatts( "TAW  ", "TAUWIX    ", "Net wave-supported stress x                     ", "m2 s-2    ", "  ", .false.) , &
         varatts( "TAW  ", "TAUWIY    ", "Net wave-supported stress y                     ", "m2 s-2    ", "  ", .false.) , &
         varatts( "TWA  ", "TAUWNX    ", "Negative part of the wave-supported stress x    ", "m2 s-2    ", "  ", .false.) , &
         varatts( "TWA  ", "TAUWNY    ", "Negative part of the wave-supported stress y    ", "m2 s-2    ", "  ", .false.) , &
         varatts( "WCC  ", "WCC       ", "Whitecap coverage                               ", "nd        ", "  ", .false.) , &
         varatts( "WCF  ", "WCF       ", "Whitecap foam thickness                         ", "m         ", "  ", .false.) , &
         varatts( "WCH  ", "WCH       ", "Mean breaking wave heigh                        ", "m         ", "  ", .false.) , &
         varatts( "WCM  ", "WCM       ", "Whitecap moment                                 ", "nd        ", "  ", .false.) , &
         varatts( "FWS  ", "TWS       ", "Wind sea mean period                            ", "s         ", "  ", .false.)   &
         ]

    !  6   Wave-ocean layer
    gridoutdefs(6,1:25) = [ &
         varatts( "SXY  ", "SXX       ", "Radiation stresses xx                           ", "N m-1     ", "  ", .false.) , &
         varatts( "SXY  ", "SYY       ", "Radiation stresses yy                           ", "N m-1     ", "  ", .false.) , &
         varatts( "SXY  ", "SXY       ", "Radiation stresses xy                           ", "N m-1     ", "  ", .false.) , &
         varatts( "TWO  ", "TAUOX     ", "Wave to ocean momentum flux x                   ", "m2 s-2    ", "  ", .false.) , &
         varatts( "TWO  ", "TAUOY     ", "Wave to ocean momentum flux y                   ", "m2 s-2    ", "  ", .false.) , &
         varatts( "BHD  ", "BHD       ", "Bernoulli head (J term)                         ", "m2 s-2    ", "  ", .false.) , &
         varatts( "FOC  ", "PHIOC     ", "Wave to ocean energy flux                       ", "W m-2     ", "  ", .false.) , &
         varatts( "TUS  ", "TUSX      ", "Stokes transport x                              ", "m2 s-1    ", "  ", .false.) , &
         varatts( "TUS  ", "TUSY      ", "Stokes transport y                              ", "m2 s-1    ", "  ", .false.) , &
         varatts( "USS  ", "USSX      ", "Surface Stokes drift x                          ", "m s-1     ", "  ", .false.) , &
         varatts( "USS  ", "USSY      ", "Surface Stokes drift y                          ", "m s-1     ", "  ", .false.) , &
         varatts( "P2S  ", "PRMS      ", "Second-order sum pressure                       ", "m4        ", "  ", .false.) , &
         varatts( "P2S  ", "TPMS      ", "Second-order sum pressure                       ", "s-1       ", "  ", .false.) , &
         varatts( "USF  ", "US3DX     ", "Spectrum of surface Stokes drift x              ", "m s-1 Hz-1", "k ", .false.) , &
         varatts( "USF  ", "US3DY     ", "Spectrum of surface Stokes drift y              ", "m s-1 Hz-1", "k ", .false.) , &
         varatts( "P2L  ", "P2SMS     ", "Micro seism  source term                        ", "Pa2 m2 s  ", "m ", .false.) , &
         varatts( "TWI  ", "TAUICEX   ", "Wave to sea ice stress x                        ", "m2 s-2    ", "  ", .false.) , &
         varatts( "TWI  ", "TAUICEY   ", "Wave to sea ice stress y                        ", "m2 s-2    ", "  ", .false.) , &
         varatts( "FIC  ", "PHICE     ", "Wave to sea ice energy flux                     ", "W m-2     ", "  ", .false.) , &
         varatts( "USP  ", "USSPX     ", "Partitioned surface Stokes drift x              ", "m s-1     ", "p ", .false.) , &
         varatts( "USP  ", "USSPY     ", "Partitioned surface Stokes drift y              ", "m s-1     ", "p ", .false.) , &
         varatts( "TWC  ", "TAUOCX    ", "Total wave to ocean stress x                    ", "Pa        ", "  ", .false.) , &
         varatts( "TWC  ", "TAUOCY    ", "Total wave to ocean stress y                    ", "Pa        ", "  ", .false.) , &
         varatts( "USSH ", "USSHX     ", "Surface layer averaged Stokes drift x           ", "m s-1     ", "  ", .false.) , &
         varatts( "USSH ", "USSHY     ", "Surface layer averaged Stokes drift y           ", "m s-1     ", "  ", .false.)   &
         ]

    !  7   Wave-bottom layer
    gridoutdefs(7,1:10) = [ &
         varatts( "ABR  ", "ABAX      ", "Near bottom rms wave excursion amplitudes x     ", "m         ", "  ", .false.) , &
         varatts( "ABR  ", "ABAY      ", "Near bottom rms wave excursion amplitudes y     ", "m         ", "  ", .false.) , &
         varatts( "UBR  ", "UBAX      ", "Near bottom rms wave velocities x               ", "m s-1     ", "  ", .false.) , &
         varatts( "UBR  ", "UBAY      ", "Near bottom rms wave velocities y               ", "m s-1     ", "  ", .false.) , &
         varatts( "BED  ", "BED       ", "Bottom roughness                                ", "m         ", "  ", .false.) , &
         varatts( "BED  ", "RIPPLEX   ", "Sea bottom ripple wavelength x                  ", "m         ", "  ", .false.) , &
         varatts( "BED  ", "RIPPLEY   ", "Sea bottom ripple wavelength y                  ", "m         ", "  ", .false.) , &
         varatts( "FBB  ", "PHIBBL    ", "Energy flux due to bottom friction              ", "W m-2     ", "  ", .false.) , &
         varatts( "TBB  ", "TAUBBLX   ", "Momentum flux due to bottom friction x          ", "m2 s-2    ", "  ", .false.) , &
         varatts( "TBB  ", "TAUBBLY   ", "Momentum flux due to bottom friction y          ", "m2 s-2    ", "  ", .false.)   &
         ]

    !  8   Spectrum parameters
    gridoutdefs(8,1:9) = [ &
         varatts( "MSS  ", "MSSX      ", "Surface mean square slope x                     ", "nd        ", "  ", .false.) , &
         varatts( "MSS  ", "MSSY      ", "Surface mean square slope y                     ", "nd        ", "  ", .false.) , &
         varatts( "MSC  ", "MSCX      ", "Spectral level at high frequency tail x         ", "nd        ", "  ", .false.) , &
         varatts( "MSC  ", "MSCY      ", "Spectral level at high frequency tail y         ", "nd        ", "  ", .false.) , &
         varatts( "WL02 ", "WL02X     ", "East/X North/Y mean wavelength component        ", "nd        ", "  ", .false.) , &
         varatts( "WL02 ", "WL02Y     ", "East/X North/Y mean wavelength component        ", "nd        ", "  ", .false.) , &
         varatts( "AXT  ", "ALPXT     ", "Correl sea surface gradients (x,t)              ", "nd        ", "  ", .false.) , &
         varatts( "AYT  ", "ALPYT     ", "Correl sea surface gradients (y,t)              ", "nd        ", "  ", .false.) , &
         varatts( "AXY  ", "ALPXY     ", "Correl sea surface gradients (x,y)              ", "nd        ", "  ", .false.)   &
         ]

    !  9   Numerical diagnostics
    gridoutdefs(9,1:5) = [ &
         varatts( "DTD  ", "DTDYN     ", "Average time step in integration                ", "min       ", "  ", .false.) , &
         varatts( "FC   ", "FCUT      ", "Cut-off frequency                               ", "s-1       ", "  ", .false.) , &
         varatts( "CFX  ", "CFLXYMAX  ", "Max. CFL number for spatial advection           ", "nd        ", "  ", .false.) , &
         varatts( "CFD  ", "CFLTHMAX  ", "Max. CFL number for theta-advection             ", "nd        ", "  ", .false.) , &
         varatts( "CFK  ", "CFLKMAX   ", "Max. CFL number for k-advection                 ", "nd        ", "  ", .false.)  &
         ]

    !  10   User defined
    gridoutdefs(10,1:2) = [ &
         varatts( "U1   ", "U1        ", "User defined 1                                  ", "nd        ", "  ", .false.) , &
         varatts( "U2   ", "U2        ", "User defined 2                                  ", "nd        ", "  ", .false.)  &
         ]
  end subroutine define_fields
end module wav_history_mod
