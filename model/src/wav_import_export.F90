module wav_import_export

  use ESMF
  use NUOPC
  use NUOPC_Model
  use wav_kind_mod    , only : r8 => shr_kind_r8
  use wav_shr_methods , only : ymd2date
  use wav_shr_methods , only : chkerr

  implicit none
  private ! except

  public  :: advertise_fields
  public  :: realize_fields
  public  :: import_fields
  public  :: export_fields
  public  :: state_getfldptr

  private :: fldlist_add
  private :: fldlist_realize

  type fld_list_type
     character(len=128) :: stdname
     integer :: ungridded_lbound = 0
     integer :: ungridded_ubound = 0
  end type fld_list_type

  integer, parameter     :: fldsMax = 100
  integer                :: fldsToWav_num = 0
  integer                :: fldsFrWav_num = 0
  type (fld_list_type)   :: fldsToWav(fldsMax)
  type (fld_list_type)   :: fldsFrWav(fldsMax)

  integer     ,parameter :: dbug_flag = 0 ! internal debug level
  character(*),parameter :: u_FILE_u = &
       __FILE__

  ! TODO: generalize this
  logical, public :: wav_coupling_to_cice = .false.
  logical, public :: wav_coupling_to_mom  = .false.

  real(r8), parameter :: zero  = 0.0_r8

!===============================================================================
contains
!===============================================================================

  subroutine advertise_fields(importState, ExportState, flds_scalar_name, rc)

    ! input/output variables
    type(ESMF_State)               :: importState
    type(ESMF_State)               :: exportState
    character(len=*) , intent(in)  :: flds_scalar_name
    integer          , intent(out) :: rc

    ! local variables
    integer          :: n, num
    character(len=2) :: fvalue
    character(len=*), parameter :: subname='(wav_import_export:advertise_fields)'
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    !--------------------------------
    ! Advertise import fields
    !--------------------------------

    call fldlist_add(fldsToWav_num, fldsToWav, trim(flds_scalar_name))

    call fldlist_add(fldsToWav_num, fldsToWav, 'Sa_u'       )
    call fldlist_add(fldsToWav_num, fldsToWav, 'Sa_v'       )
    call fldlist_add(fldsToWav_num, fldsToWav, 'Sa_tbot'    )
    call fldlist_add(fldsToWav_num, fldsToWav, 'Si_ifrac'   )
    call fldlist_add(fldsToWav_num, fldsToWav, 'So_t'       )
    call fldlist_add(fldsToWav_num, fldsToWav, 'So_u'       )
    call fldlist_add(fldsToWav_num, fldsToWav, 'So_v'       )
    call fldlist_add(fldsToWav_num, fldsToWav, 'So_bldepth' )

    if (wav_coupling_to_cice) then
       call fldlist_add(fldsToWav_num, fldsToWav, 'Si_thick'   )
       call fldlist_add(fldsToWav_num, fldsToWav, 'Si_floediam')
    end if

    do n = 1,fldsToWav_num
       call NUOPC_Advertise(importState, standardName=fldsToWav(n)%stdname, &
            TransferOfferGeomObject='will provide', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    !--------------------------------
    ! Advertise export fields
    !--------------------------------

    call fldlist_add(fldsFrWav_num, fldsFrWav, trim(flds_scalar_name))

    call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_lamult' )
    call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_ustokes')
    call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_vstokes')
   !call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_hstokes')
    call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_pstokes_x', ungridded_lbound=1, ungridded_ubound=3)
    call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_pstokes_y', ungridded_lbound=1, ungridded_ubound=3)
    ! AA TODO: In the above fldlist_add calls, we are passing hardcoded ungridded_ubound values (3) because, USSPF(2)
    ! is not initialized yet. It is set during w3init which gets called at a later phase (realize). A permanent solution
    ! will be implemented soon based on receiving USSP and USSPF from the coupler instead of the mod_def file. This will
    ! also ensure compatibility with the ocean component since ocean will also receive these from the coupler.

    if (wav_coupling_to_cice) then
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'wav_tauice1')
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'wav_tauice2')
       do n = 1, 25
          write(fvalue,'(I2)') n
          call fldlist_add(fldsFrWav_num, fldsFrWav,'wave_elevation_spectrum'//trim(adjustl(fvalue)))
       enddo
    end if

    do n = 1,fldsFrWav_num
       call NUOPC_Advertise(exportState, standardName=fldsFrWav(n)%stdname, &
            TransferOfferGeomObject='will provide', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)

  end subroutine advertise_fields

!===============================================================================
  subroutine realize_fields(gcomp, mesh, flds_scalar_name, flds_scalar_num, rc)

    ! input/output variables
    type(ESMF_GridComp)            :: gcomp
    type(ESMF_Mesh)                :: mesh
    character(len=*) , intent(in)  :: flds_scalar_name
    integer          , intent(in)  :: flds_scalar_num
    integer          , intent(out) :: rc

    ! local variables
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    character(len=*), parameter :: subname='(wav_import_export:realize_fields)'
    integer :: ii
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call fldlist_realize( &
         state=ExportState, &
         fldList=fldsFrWav, &
         numflds=fldsFrWav_num, &
         flds_scalar_name=flds_scalar_name, &
         flds_scalar_num=flds_scalar_num, &
         tag=subname//':WW3Export',&
         mesh=mesh, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call fldlist_realize( &
         state=importState, &
         fldList=fldsToWav, &
         numflds=fldsToWav_num, &
         flds_scalar_name=flds_scalar_name, &
         flds_scalar_num=flds_scalar_num, &
         tag=subname//':WW3Import',&
         mesh=mesh, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)

  end subroutine realize_fields

!===============================================================================
  subroutine import_fields( gcomp, rc )

    !---------------------------------------------------------------------------
    ! Obtain the wave input from the mediator
    !---------------------------------------------------------------------------

    use w3gdatmd    , only: nseal, MAPSTA, MAPFS, MAPSF, NX, NY
    !use w3idatmd    , only: CX0, CY0, CXN, CYN, DT0, DTN, ICEI, HML, WLEV, INFLAGS1, &
    use w3idatmd    , only: CX0, CY0, CXN, CYN, DT0, DTN, ICEI, WLEV, INFLAGS1, &
                            ICEP1, ICEP5
    use w3idatmd    , only: TC0, TCN, TLN, TIN, TI1, TI5, TW0, TWN, WX0, WY0, WXN, WYN
    use w3odatmd    , only: naproc, iaproc, napout
    use w3wdatmd    , only: time
    
    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_State)  :: importState
    type(ESMF_VM)     :: vm
    type(ESMF_Clock)  :: clock
    type(ESMF_Time)   :: ETime
    type(ESMF_Field)  :: lfield
    integer           :: ymd, tod
    integer           :: yy, mm, dd, hh, ss
    real(r8)          :: so_u_global(nx*ny)
    real(r8)          :: so_v_global(nx*ny)
    real(r8)          :: so_t_global(nx*ny)
    real(r8)          :: so_bldepth_global(nx*ny)
    real(r8)          :: sa_u_global(nx*ny)
    real(r8)          :: sa_v_global(nx*ny)
    real(r8)          :: sa_tbot_global(nx*ny)
    real(r8)          :: si_ifrac_global(nx*ny)
    real(r8)          :: si_thick_global(nx*ny)
    real(r8)          :: si_floediam_global(nx*ny)
    real(r8)          :: temp_global(nx*ny)
    real(r8), pointer :: so_u(:)
    real(r8), pointer :: so_v(:)
    real(r8), pointer :: so_t(:)
    real(r8), pointer :: so_bldepth(:)
    real(r8), pointer :: sa_u(:)
    real(r8), pointer :: sa_v(:)
    real(r8), pointer :: sa_tbot(:)
    real(r8), pointer :: si_ifrac(:)
    real(r8), pointer :: si_thick(:)
    real(r8), pointer :: si_floediam(:)
    integer           :: n, ix, iy, isea
    real(r8)          :: def_value
    integer           :: time0(2) ! starting time of the run interval
    integer           :: timen(2) ! ending time of the run interval
    character(len=*), parameter :: subname='(wav_import_export:import_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Get import state, clock and vm

    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Determine time0 and timen - note have not advanced the model clock yet with nuopc

    call ESMF_ClockGetNextTime( clock, Etime, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet( ETime, yy=yy, mm=mm, dd=dd, s=tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ymd2date(yy, mm, dd, ymd)
    hh = tod/3600
    mm = (tod - (hh * 3600))/60
    ss = tod - (hh*3600) - (mm*60)
    timen(1) = ymd
    timen(2) = hh*10000 + mm*100 + ss

    call ESMF_ClockGet( clock, currTime=ETime, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet( ETime, yy=yy, mm=mm, dd=dd, s=tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ymd2date(yy, mm, dd, ymd)
    hh = tod/3600
    mm = (tod - (hh * 3600))/60
    ss = tod - (hh*3600) - (mm*60)
    time0(1) = ymd
    time0(2) = hh*10000 + mm*100 + ss
    time = time0


    ! Determine global data

    call state_getfldptr(importState, 'So_u', fldptr1d=so_u, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'So_v', fldptr1d=so_v, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'So_t', fldptr1d=so_t, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'So_bldepth', fldptr1d=so_bldepth, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sa_u', fldptr1d=sa_u, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sa_v', fldptr1d=sa_v, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sa_tbot', fldptr1d=sa_tbot, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Si_ifrac', fldptr1d=si_ifrac, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    so_u_global(:)       = 0._r8
    so_v_global(:)       = 0._r8
    so_t_global(:)       = 0._r8
    so_bldepth_global(:) = 0._r8
    sa_u_global          = 0._r8
    sa_v_global          = 0._r8
    sa_tbot_global       = 0._r8
    si_ifrac_global      = 0._r8
    do n = 1, nseal
       isea = iaproc + (n-1)*naproc
       ix = mapsf(isea,1)
       iy = mapsf(isea,2)
       so_u_global       (ix + (iy-1)*nx) = so_u(n)
       so_v_global       (ix + (iy-1)*nx) = so_v(n)
       so_t_global       (ix + (iy-1)*nx) = so_t(n)
       so_bldepth_global (ix + (iy-1)*nx) = so_bldepth(n)
       sa_u_global       (ix + (iy-1)*nx) = sa_u(n)
       sa_v_global       (ix + (iy-1)*nx) = sa_v(n)
       sa_tbot_global    (ix + (iy-1)*nx) = sa_tbot(n)
       si_ifrac_global   (ix + (iy-1)*nx) = si_ifrac(n)
    end do

    call ESMF_VMAllReduce(vm, sendData=so_u_global, recvData=temp_global, count=nx*ny, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    so_u_global(:) = temp_global(:)

    call ESMF_VMAllReduce(vm, sendData=so_v_global, recvData=temp_global, count=nx*ny, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    so_v_global(:) = temp_global(:)

    call ESMF_VMAllReduce(vm, sendData=so_t_global, recvData=temp_global, count=nx*ny, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    so_t_global(:) = temp_global(:)

    call ESMF_VMAllReduce(vm, sendData=so_bldepth_global, recvData=temp_global, count=nx*ny, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    so_bldepth_global(:) = temp_global(:)

    call ESMF_VMAllReduce(vm, sendData=sa_u_global, recvData=temp_global, count=nx*ny, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    sa_u_global(:) = temp_global(:)

    call ESMF_VMAllReduce(vm, sendData=sa_v_global, recvData=temp_global, count=nx*ny, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    sa_v_global(:) = temp_global(:)

    call ESMF_VMAllReduce(vm, sendData=sa_tbot_global, recvData=temp_global, count=nx*ny, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    sa_tbot_global(:) = temp_global(:)

    call ESMF_VMAllReduce(vm, sendData=si_ifrac_global, recvData=temp_global, count=nx*ny, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    si_ifrac_global(:) = temp_global(:)

    if (wav_coupling_to_cice) then
       call state_getfldptr(importState, 'Si_thick', fldptr1d=si_thick, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(importState, 'Si_floediam', fldptr1d=si_floediam, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       si_thick_global      = 0._r8
       si_floediam_global   = 0._r8

       do n = 1, nseal
          isea = iaproc + (n-1)*naproc
          ix = mapsf(isea,1)
          iy = mapsf(isea,2)
          si_thick_global   (ix + (iy-1)*nx) = si_thick(n)
          si_floediam_global(ix + (iy-1)*nx) = si_floediam(n)
       end do

       call ESMF_VMAllReduce(vm, sendData=si_thick_global, recvData=temp_global, count=nx*ny, reduceflag=ESMF_REDUCE_SUM, rc=rc)
       si_thick_global(:) = temp_global(:)
       call ESMF_VMAllReduce(vm, sendData=si_floediam_global, recvData=temp_global, count=nx*ny, reduceflag=ESMF_REDUCE_SUM, rc=rc)
       si_floediam_global(:) = temp_global(:)
    end if

    ! input fields associated with W3FLDG calls in ww3_shel.ftn
    ! these arrays are global, just fill the local cells for use later
    ! fill both the lower (0) and upper (N) bound data with the same values
    ! fill with special values as default, these should not be used in practice
    ! set time for input data to time0 and timen (shouldn't matter)

    !HK flags is now INFLAGS1  INFLAGS(1-5) is true
    def_value = 0.0
    if (INFLAGS1(1)) then
       TLN  = timen
       WLEV = def_value   ! ssh
    endif
    if (INFLAGS1(2)) then
       TC0  = time0       ! times for ocn current fields
       TCN  = timen
       CX0  = def_value   ! ocn u current
       CXN  = def_value
       CY0  = def_value   ! ocn v current
       CYN  = def_value
    endif
    if (INFLAGS1(3)) then
       TW0  = time0       ! times for atm wind/temp fields.
       TWN  = timen
       WX0  = def_value   ! atm u wind
       WXN  = def_value
       WY0  = def_value   ! atm v wind
       WYN  = def_value
       DT0  = def_value   ! air temp - ocn temp
       DTN  = def_value
    endif
    if (INFLAGS1(4)) then
       TIN  = timen       ! time for ice field
       ICEI = def_value   ! ice frac
    endif
    if (INFLAGS1(-7)) then
       TI1  = timen       ! time for ice field
       ICEP1 = def_value   ! ice thickness
    end if
    if (INFLAGS1(-3)) then
       TI5  = timen       ! time for ice field
       ICEP5 = def_value   ! ice floe size
    end if

    ! use these loops for global copy
    n = 0
    do iy = 1,NY
       do ix = 1,NX
          n = n + 1
          if (INFLAGS1(1)) then
             WLEV(ix,iy) = 0.0
          endif
          if (INFLAGS1(2)) then
             CX0(ix,iy)  = so_u_global(n) ! ocn u current
             CXN(ix,iy)  = so_u_global(n)
             CY0(ix,iy)  = so_v_global(n) ! ocn v current
             CYN(ix,iy)  = so_v_global(n)
          endif
          if (INFLAGS1(3)) then
             WX0(ix,iy)  = sa_u_global(n)  ! atm u wind
             WXN(ix,iy)  = sa_u_global(n)
             WY0(ix,iy)  = sa_v_global(n)  ! atm v wind
             WYN(ix,iy)  = sa_v_global(n)
             DT0(ix,iy)  = sa_tbot_global(n) - so_t_global(n)  ! air temp - ocn temp
             DTN(ix,iy)  = sa_tbot_global(n) - so_t_global(n)
          endif
          if (INFLAGS1(4)) then
             ICEI(ix,iy) = si_ifrac_global(n) ! ice frac
          endif
          if (INFLAGS1(-7)) then
             ICEP1(ix,iy) = si_thick_global(n) ! ice thickness
          end if
          if (INFLAGS1(-3)) then
             ICEP5(ix,iy) = si_floediam_global(n) ! ice floe diameter
          end if
          !if (INFLAGS1(5)) then
          !   HML(ix,iy) = max(so_bldepth_global(n), 5.) ! ocn mixing layer depth
          !endif
       enddo
    enddo
100 format(a,i6,2x,d21.14)


    call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)

  end subroutine import_fields

!====================================================================================
  subroutine export_fields (gcomp, rc)

    !---------------------------------------------------------------------------
    ! Create the export state
    !---------------------------------------------------------------------------

    use wav_kind_mod,   only : R8 => SHR_KIND_R8
    !use w3adatmd      , only : LAMULT, USSX, USSY, EF, TAUICE, USSP
    use w3adatmd      , only : USSX, USSY, EF, TAUICE, USSP
    use w3wdatmd      , only : va
    !HK trying to use extended arrays use w3adatmd      , only : XUSSX, XUSSY, XEF
    use w3odatmd      , only : naproc, iaproc
    use w3gdatmd      , only : nseal, MAPSTA, MAPFS, MAPSF, USSPF, NK
    use w3iogomd      , only : CALC_U3STOKES

    ! input/output/variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Local variables
    real(R8)          :: fillvalue = 1.0e30_R8                 ! special missing value
    type(ESMF_State)  :: exportState
    integer           :: n, jsea, isea, ix, iy, lsize, ib

    real(r8), pointer :: charno(:)
    real(r8), pointer :: wbcuru(:)
    real(r8), pointer :: wbcurv(:)
    real(r8), pointer :: wbcurp(:)
    real(r8), pointer :: sxxn(:)
    real(r8), pointer :: sxyn(:)
    real(r8), pointer :: syyn(:)
     
    real(r8), pointer :: sw_lamult(:)
    real(r8), pointer :: sw_ustokes(:)
    real(r8), pointer :: sw_vstokes(:)
    real(r8), pointer :: wav_tauice1(:)
    real(r8), pointer :: wav_tauice2(:)

    ! d2 is location, d1 is frequency  - 25 1d variables
    real(r8), pointer :: wave_elevation_spectrum1(:)
    real(r8), pointer :: wave_elevation_spectrum2(:)
    real(r8), pointer :: wave_elevation_spectrum3(:)
    real(r8), pointer :: wave_elevation_spectrum4(:)
    real(r8), pointer :: wave_elevation_spectrum5(:)
    real(r8), pointer :: wave_elevation_spectrum6(:)
    real(r8), pointer :: wave_elevation_spectrum7(:)
    real(r8), pointer :: wave_elevation_spectrum8(:)
    real(r8), pointer :: wave_elevation_spectrum9(:)
    real(r8), pointer :: wave_elevation_spectrum10(:)
    real(r8), pointer :: wave_elevation_spectrum11(:)
    real(r8), pointer :: wave_elevation_spectrum12(:)
    real(r8), pointer :: wave_elevation_spectrum13(:)
    real(r8), pointer :: wave_elevation_spectrum14(:)
    real(r8), pointer :: wave_elevation_spectrum15(:)
    real(r8), pointer :: wave_elevation_spectrum16(:)
    real(r8), pointer :: wave_elevation_spectrum17(:)
    real(r8), pointer :: wave_elevation_spectrum18(:)
    real(r8), pointer :: wave_elevation_spectrum19(:)
    real(r8), pointer :: wave_elevation_spectrum20(:)
    real(r8), pointer :: wave_elevation_spectrum21(:)
    real(r8), pointer :: wave_elevation_spectrum22(:)
    real(r8), pointer :: wave_elevation_spectrum23(:)
    real(r8), pointer :: wave_elevation_spectrum24(:)
    real(r8), pointer :: wave_elevation_spectrum25(:)

    real(r8), pointer :: sw_pstokes_x(:,:)
    real(r8), pointer :: sw_pstokes_y(:,:)

    character(len=*), parameter :: subname='(wav_import_export:export_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Get export state
    call NUOPC_ModelGet(gcomp, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! copy ww3 data to coupling datatype
    ! copy enhancement factor, uStokes, vStokes, and wave elevation spectrum  to coupler

    call state_getfldptr(exportState, 'Sw_lamult', fldptr1d=sw_lamult, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sw_ustokes', fldptr1d=sw_ustokes, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sw_vstokes', fldptr1d=sw_vstokes, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! copy enhancement factor, uStokes, vStokes and surface layer Langmuir number to coupler
    ! LAMULT calculated in w3iogomd.f90
    do jsea=1, nseal
       isea = iaproc + (jsea-1)*naproc
       ix  = MAPSF(ISEA,1)
       iy  = MAPSF(ISEA,2)
       if (MAPSTA(iy,ix) .eq. 1) then
          ! use hstokes to pass LaSL to POP
          !sw_lamult(jsea)  = LAMULT(jsea)
          sw_ustokes(jsea) = USSX(jsea)
          sw_vstokes(jsea) = USSY(jsea)
          !sw_hstokes(jsea) = LASLPJ(jsea)
       else
          sw_lamult(jsea)  = 1.
          sw_ustokes(jsea) = 0.
          sw_vstokes(jsea) = 0.
          !sw_hstokes(jsea) = 0.
       endif
    enddo

    ! ------------------------------------------------------------
    ! UFS: charno

    !call state_getfldptr(exportState, 'charno', fldptr1d=charno, rc=rc)
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return
    !call calc_charno(charno, rc=rc)
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! TODO: put this into a subroutine calc_charno
    ! use w3wdatmd, only :: va
    ! use w3adatmd, only :: cg, wn, u10, u10d, charn
    ! real    :: emean, fmean, fmean1, wnmean, amax, ustar, ustdr
    ! real    :: tauwx, tauwy, cd, z0, fmeanws, dlwmean
    ! logical :: llws(nspec)
    !if ( firstCall ) then
    !   do jsea = 1,nseal
    !      isea = iaproc + (jsea-1)*naproc
    !      llws(:) = .true.
    !      ustar = zero
    !      ustdr = zero
    !      call w3spr4( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea),   &
    !                   emean, fmean, fmean1, wnmean, amax,         &
    !                   u10(isea), u10d(isea), ustar, ustdr, tauwx, &
    !                   tauwy, cd, z0, charn(jsea), llws, fmeanws,  &
    !                   dlwmean )
    !      ! set pointer value in export state
    !      charno(jsea) = charn(jsea)
    !   enddo
    !   firstCall = .false.
    !end if

    call state_getfldptr(exportState, 'wbcuru', fldptr1d=wbcuru, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'wbcurv', fldptr1d=wbcurv, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'wbcurp', fldptr1d=wbcurp, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call CalcBotcur( va, wbcuru, wbcurv, wbcurp)

    call state_getfldptr(exportState, 'sxxn', fldptr1d=sxxn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'sxyn', fldptr1d=sxyn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'syyn', fldptr1d=syyn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call CalcRadstr2D( va, sxxn, sxyn, syyn)

    ! ------------------------------------------------------------


    ! Fill in the local land points with fill value
    ! HK TODO is this correct?
    lsize = size(sw_lamult)
    do n = nseal+1,lsize
       sw_lamult(n)  = fillvalue
       sw_ustokes(n) = fillvalue
       sw_vstokes(n) = fillvalue
       !sw_hstokes(n) = fillvalue
    end do

    if (wav_coupling_to_cice) then
       call state_getfldptr(exportState, 'wav_tauice1', fldptr1d=wav_tauice1, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wav_tauice2', fldptr1d=wav_tauice2, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! wave_eleveation_spectrum as 25 variables
       call state_getfldptr(exportState, 'wave_elevation_spectrum1', fldptr1d=wave_elevation_spectrum1, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum2', fldptr1d=wave_elevation_spectrum2, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum3', fldptr1d=wave_elevation_spectrum3, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum4', fldptr1d=wave_elevation_spectrum4, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum5', fldptr1d=wave_elevation_spectrum5, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum6', fldptr1d=wave_elevation_spectrum6, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum7', fldptr1d=wave_elevation_spectrum7, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum8', fldptr1d=wave_elevation_spectrum8, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum9', fldptr1d=wave_elevation_spectrum9, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum10', fldptr1d=wave_elevation_spectrum10, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum11', fldptr1d=wave_elevation_spectrum11, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum12', fldptr1d=wave_elevation_spectrum12, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum13', fldptr1d=wave_elevation_spectrum13, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum14', fldptr1d=wave_elevation_spectrum14, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum15', fldptr1d=wave_elevation_spectrum15, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum16', fldptr1d=wave_elevation_spectrum16, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum17', fldptr1d=wave_elevation_spectrum17, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum18', fldptr1d=wave_elevation_spectrum18, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum19', fldptr1d=wave_elevation_spectrum19, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum20', fldptr1d=wave_elevation_spectrum20, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum21', fldptr1d=wave_elevation_spectrum21, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum22', fldptr1d=wave_elevation_spectrum22, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum23', fldptr1d=wave_elevation_spectrum23, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum24', fldptr1d=wave_elevation_spectrum24, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wave_elevation_spectrum25', fldptr1d=wave_elevation_spectrum25, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       !HK initialize wave_elevation_spectrum
       wave_elevation_spectrum1 (:) = fillvalue
       wave_elevation_spectrum2 (:) = fillvalue
       wave_elevation_spectrum3 (:) = fillvalue
       wave_elevation_spectrum4 (:) = fillvalue
       wave_elevation_spectrum5 (:) = fillvalue
       wave_elevation_spectrum6 (:) = fillvalue
       wave_elevation_spectrum7 (:) = fillvalue
       wave_elevation_spectrum8 (:) = fillvalue
       wave_elevation_spectrum9 (:) = fillvalue
       wave_elevation_spectrum10(:) = fillvalue
       wave_elevation_spectrum11(:) = fillvalue
       wave_elevation_spectrum12(:) = fillvalue
       wave_elevation_spectrum13(:) = fillvalue
       wave_elevation_spectrum14(:) = fillvalue
       wave_elevation_spectrum15(:) = fillvalue
       wave_elevation_spectrum16(:) = fillvalue
       wave_elevation_spectrum17(:) = fillvalue
       wave_elevation_spectrum18(:) = fillvalue
       wave_elevation_spectrum19(:) = fillvalue
       wave_elevation_spectrum20(:) = fillvalue
       wave_elevation_spectrum21(:) = fillvalue
       wave_elevation_spectrum22(:) = fillvalue
       wave_elevation_spectrum23(:) = fillvalue
       wave_elevation_spectrum24(:) = fillvalue
       wave_elevation_spectrum25(:) = fillvalue

       !HK This translates the WW3 arrays to the coupler arrays
       do jsea=1, nseal                       !HK jsea is local
          isea = iaproc + (jsea-1)*naproc     !HK isea is global
          ix  = MAPSF(isea,1)  ! global ix
          iy  = MAPSF(isea,2)  ! global iy

          if (MAPSTA(iy,ix) .eq. 1) then  ! active sea point
             wav_tauice1(jsea) = TAUICE(jsea,1) ! tau ice is 2D
             wav_tauice2(jsea) = TAUICE(jsea,2) ! tau ice is 2D

             !HK if wave_elevation_spectrum is UNDEF  - needs ouput flag to be turned on
             ! wave_elevation_spectrum as 25 variables
             wave_elevation_spectrum1(jsea) = EF(jsea,1)
             wave_elevation_spectrum2(jsea) = EF(jsea,2)
             wave_elevation_spectrum3(jsea) = EF(jsea,3)
             wave_elevation_spectrum4(jsea) = EF(jsea,4)
             wave_elevation_spectrum5(jsea) = EF(jsea,5)
             wave_elevation_spectrum6(jsea) = EF(jsea,6)
             wave_elevation_spectrum7(jsea) = EF(jsea,7)
             wave_elevation_spectrum8(jsea) = EF(jsea,8)
             wave_elevation_spectrum9(jsea) = EF(jsea,9)
             wave_elevation_spectrum10(jsea) = EF(jsea,10)
             wave_elevation_spectrum11(jsea) = EF(jsea,11)
             wave_elevation_spectrum12(jsea) = EF(jsea,12)
             wave_elevation_spectrum13(jsea) = EF(jsea,13)
             wave_elevation_spectrum14(jsea) = EF(jsea,14)
             wave_elevation_spectrum15(jsea) = EF(jsea,15)
             wave_elevation_spectrum16(jsea) = EF(jsea,16)
             wave_elevation_spectrum17(jsea) = EF(jsea,18)
             wave_elevation_spectrum18(jsea) = EF(jsea,18)
             wave_elevation_spectrum19(jsea) = EF(jsea,19)
             wave_elevation_spectrum20(jsea) = EF(jsea,20)
             wave_elevation_spectrum21(jsea) = EF(jsea,21)
             wave_elevation_spectrum22(jsea) = EF(jsea,22)
             wave_elevation_spectrum23(jsea) = EF(jsea,23)
             wave_elevation_spectrum24(jsea) = EF(jsea,24)
             wave_elevation_spectrum25(jsea) = EF(jsea,25)
          else
             sw_lamult(jsea)  = 1.
             sw_ustokes(jsea) = 0.
             sw_vstokes(jsea) = 0.
             wav_tauice1(jsea) = 0.
             wav_tauice2(jsea) = 0.
             wave_elevation_spectrum1(jsea) = 0.
             wave_elevation_spectrum2(jsea) = 0.
             wave_elevation_spectrum3(jsea) = 0.
             wave_elevation_spectrum4(jsea) = 0.
             wave_elevation_spectrum5(jsea) = 0.
             wave_elevation_spectrum6(jsea) = 0.
             wave_elevation_spectrum7(jsea) = 0.
             wave_elevation_spectrum8(jsea) = 0.
             wave_elevation_spectrum9(jsea) = 0.
             wave_elevation_spectrum10(jsea) = 0.
             wave_elevation_spectrum11(jsea) = 0.
             wave_elevation_spectrum12(jsea) = 0.
             wave_elevation_spectrum13(jsea) = 0.
             wave_elevation_spectrum14(jsea) = 0.
             wave_elevation_spectrum15(jsea) = 0.
             wave_elevation_spectrum16(jsea) = 0.
             wave_elevation_spectrum17(jsea) = 0.
             wave_elevation_spectrum18(jsea) = 0.
             wave_elevation_spectrum19(jsea) = 0.
             wave_elevation_spectrum20(jsea) = 0.
             wave_elevation_spectrum21(jsea) = 0.
             wave_elevation_spectrum22(jsea) = 0.
             wave_elevation_spectrum23(jsea) = 0.
             wave_elevation_spectrum24(jsea) = 0.
             wave_elevation_spectrum25(jsea) = 0.
          endif
       enddo

       ! Fill in the local land points with fill value
       ! HK TODO is this correct?
       lsize = size(sw_lamult)
       do n = nseal+1,lsize
          wav_tauice1(n) = fillvalue
          wav_tauice2(n) = fillvalue
          wave_elevation_spectrum1 (jsea) = fillvalue
          wave_elevation_spectrum2 (jsea) = fillvalue
          wave_elevation_spectrum3 (jsea) = fillvalue
          wave_elevation_spectrum4 (jsea) = fillvalue
          wave_elevation_spectrum5 (jsea) = fillvalue
          wave_elevation_spectrum6 (jsea) = fillvalue
          wave_elevation_spectrum7 (jsea) = fillvalue
          wave_elevation_spectrum8 (jsea) = fillvalue
          wave_elevation_spectrum9 (jsea) = fillvalue
          wave_elevation_spectrum10(jsea) = fillvalue
          wave_elevation_spectrum11(jsea) = fillvalue
          wave_elevation_spectrum12(jsea) = fillvalue
          wave_elevation_spectrum13(jsea) = fillvalue
          wave_elevation_spectrum14(jsea) = fillvalue
          wave_elevation_spectrum15(jsea) = fillvalue
          wave_elevation_spectrum16(jsea) = fillvalue
          wave_elevation_spectrum17(jsea) = fillvalue
          wave_elevation_spectrum18(jsea) = fillvalue
          wave_elevation_spectrum19(jsea) = fillvalue
          wave_elevation_spectrum20(jsea) = fillvalue
          wave_elevation_spectrum21(jsea) = fillvalue
          wave_elevation_spectrum22(jsea) = fillvalue
          wave_elevation_spectrum23(jsea) = fillvalue
          wave_elevation_spectrum24(jsea) = fillvalue
          wave_elevation_spectrum25(jsea) = fillvalue
       end do
    end if

   call state_getfldptr(exportState, 'Sw_pstokes_x', fldptr2d=sw_pstokes_x, rc=rc)
   if (ChkErr(rc,__LINE__,u_FILE_u)) return

   call state_getfldptr(exportState, 'Sw_pstokes_y', fldptr2d=sw_pstokes_y, rc=rc)
   if (ChkErr(rc,__LINE__,u_FILE_u)) return

   sw_pstokes_x(:,:) = fillvalue
   sw_pstokes_y(:,:) = fillvalue
   if (USSPF(1) > 0) then ! Partitioned Stokes drift computation is turned on in mod_def file.
      !call CalcPStokes(rc)
      call CALC_U3STOKES(va, 2)
      do ib = 1, USSPF(2)
         do jsea = 1, nseal
            sw_pstokes_x(ib,jsea) = ussp(jsea,ib)
            sw_pstokes_y(ib,jsea) = ussp(jsea,nk+ib)
         enddo
      end do
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
   endif

  end subroutine export_fields

  !===============================================================================

  subroutine fldlist_add(num, fldlist, stdname, ungridded_lbound, ungridded_ubound)
    integer,                    intent(inout) :: num
    type(fld_list_type),        intent(inout) :: fldlist(:)
    character(len=*),           intent(in)    :: stdname
    integer, optional,          intent(in)    :: ungridded_lbound
    integer, optional,          intent(in)    :: ungridded_ubound

    ! local variables
    integer :: rc
    character(len=*), parameter :: subname='(wav_import_export:fldlist_add)'
    !-------------------------------------------------------------------------------

    ! Set up a list of field information

    num = num + 1
    if (num > fldsMax) then
       call ESMF_LogWrite(trim(subname)//": ERROR num > fldsMax "//trim(stdname), &
            ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__)
       return
    endif
    fldlist(num)%stdname = trim(stdname)
    if (present(ungridded_lbound) .and. present(ungridded_ubound)) then
       fldlist(num)%ungridded_lbound = ungridded_lbound
       fldlist(num)%ungridded_ubound = ungridded_ubound
    end if

  end subroutine fldlist_add

  !===============================================================================

  subroutine fldlist_realize(state, fldList, numflds, flds_scalar_name, flds_scalar_num, mesh, tag, rc)

    use NUOPC, only : NUOPC_IsConnected, NUOPC_Realize
    use ESMF , only : ESMF_MeshLoc_Element, ESMF_FieldCreate, ESMF_TYPEKIND_R8
    use ESMF , only : ESMF_MAXSTR, ESMF_Field, ESMF_State, ESMF_Mesh, ESMF_StateRemove
    use ESMF , only : ESMF_LogFoundError, ESMF_LOGMSG_INFO, ESMF_SUCCESS
    use ESMF , only : ESMF_LogWrite, ESMF_LOGMSG_ERROR, ESMF_LOGERR_PASSTHRU
    use ESMF , only : ESMF_VM

    ! input/output variables
    type(ESMF_State)          , intent(inout) :: state
    type(fld_list_type)       , intent(in)    :: fldList(:)
    integer                   , intent(in)    :: numflds
    character(len=*)          , intent(in)    :: flds_scalar_name
    integer                   , intent(in)    :: flds_scalar_num
    character(len=*)          , intent(in)    :: tag
    type(ESMF_Mesh)           , intent(in)    :: mesh
    integer                   , intent(inout) :: rc

    ! local variables
    integer                :: n
    type(ESMF_Field)       :: field
    character(len=80)      :: stdname
    character(len=*),parameter  :: subname='(wav_import_export:fldlist_realize)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    do n = 1, numflds
       stdname = fldList(n)%stdname
       if (NUOPC_IsConnected(state, fieldName=stdname)) then
          if (stdname == trim(flds_scalar_name)) then
             call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is connected on root pe", &
                  ESMF_LOGMSG_INFO)
             ! Create the scalar field
             call SetScalarField(field, flds_scalar_name, flds_scalar_num, rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
          else
             call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is connected using mesh", &
                  ESMF_LOGMSG_INFO)
             ! Create the field
             if (fldlist(n)%ungridded_lbound > 0 .and. fldlist(n)%ungridded_ubound > 0) then
                call ESMF_LogWrite(trim(subname)//trim(tag)//" Field = "//trim(stdname)//" has ungridded dimension", &
                     ESMF_LOGMSG_INFO)
                field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name=stdname, meshloc=ESMF_MESHLOC_ELEMENT, &
                     ungriddedLbound=(/fldlist(n)%ungridded_lbound/), &
                     ungriddedUbound=(/fldlist(n)%ungridded_ubound/), &
                     gridToFieldMap=(/2/), rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return
             else
                field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name=stdname, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return
             end if
          end if ! if not scalar field

          ! NOW call NUOPC_Realize
          call NUOPC_Realize(state, field=field, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       else
          if (stdname /= trim(flds_scalar_name)) then
             call ESMF_LogWrite(subname // trim(tag) // " Field = "// trim(stdname) // " is not connected.", &
                  ESMF_LOGMSG_INFO)
             call ESMF_StateRemove(state, (/stdname/), rc=rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
          end if
       end if
    end do

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine SetScalarField(field, flds_scalar_name, flds_scalar_num, rc)
      ! ----------------------------------------------
      ! create a field with scalar data on the root pe
      ! ----------------------------------------------
      use ESMF, only : ESMF_Field, ESMF_DistGrid, ESMF_Grid
      use ESMF, only : ESMF_DistGridCreate, ESMF_GridCreate, ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU
      use ESMF, only : ESMF_FieldCreate, ESMF_GridCreate, ESMF_TYPEKIND_R8

      type(ESMF_Field) , intent(inout) :: field
      character(len=*) , intent(in)    :: flds_scalar_name
      integer          , intent(in)    :: flds_scalar_num
      integer          , intent(inout) :: rc

      ! local variables
      type(ESMF_Distgrid) :: distgrid
      type(ESMF_Grid)     :: grid
      character(len=*), parameter :: subname='(wav_import_export:SetScalarField)'
      ! ----------------------------------------------

      rc = ESMF_SUCCESS

      ! create a DistGrid with a single index space element, which gets mapped onto DE 0.
      distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

      grid = ESMF_GridCreate(distgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

      field = ESMF_FieldCreate(name=trim(flds_scalar_name), grid=grid, typekind=ESMF_TYPEKIND_R8, &
           ungriddedLBound=(/1/), ungriddedUBound=(/flds_scalar_num/), gridToFieldMap=(/2/), rc=rc) ! num of scalar values
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    end subroutine SetScalarField

  end subroutine fldlist_realize

  !===============================================================================
  subroutine state_getfldptr(State, fldname, fldptr1d, fldptr2d, rc)
    ! ----------------------------------------------
    ! Get pointer to a state field
    ! ----------------------------------------------
    use ESMF , only : ESMF_State, ESMF_Field, ESMF_Mesh, ESMF_FieldStatus_Flag
    use ESMF , only : ESMF_StateGet, ESMF_FieldGet, ESMF_MeshGet
    use ESMF , only : ESMF_FIELDSTATUS_COMPLETE, ESMF_FAILURE

    ! input/output variables
    type(ESMF_State),  intent(in)    :: State
    character(len=*),  intent(in)    :: fldname
    real(R8), pointer, optional, intent(out)   :: fldptr1d(:)
    real(R8), pointer, optional, intent(out)   :: fldptr2d(:,:)
    integer,           intent(out)   :: rc

    ! local variables
    type(ESMF_FieldStatus_Flag) :: status
    type(ESMF_Field)            :: lfield
    type(ESMF_Mesh)             :: lmesh
    integer                     :: nnodes, nelements
    character(len=*), parameter :: subname='(wav_import_export:state_getfldptr)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO)
    end if

    call ESMF_StateGet(State, itemName=trim(fldname), field=lfield, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(lfield, status=status, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
       call ESMF_LogWrite(trim(subname)//": ERROR data not allocated ", ESMF_LOGMSG_INFO, rc=rc)
       rc = ESMF_FAILURE
       return
    else
       call ESMF_FieldGet(lfield, mesh=lmesh, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       call ESMF_MeshGet(lmesh, numOwnedNodes=nnodes, numOwnedElements=nelements, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       if (nnodes == 0 .and. nelements == 0) then
          call ESMF_LogWrite(trim(subname)//": no local nodes or elements ", ESMF_LOGMSG_INFO)
          rc = ESMF_FAILURE
          return
       end if

       if (present(fldptr1d)) then
         call ESMF_FieldGet(lfield, farrayPtr=fldptr1d, rc=rc)
       else ! 2D
         call ESMF_FieldGet(lfield, farrayPtr=fldptr2d, rc=rc)
       endif

       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif  ! status

    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)
    end if

  end subroutine state_getfldptr

  !===============================================================================
  subroutine CalcBotcur ( a, wbxn, wbyn, wbpn )
    ! Calculate wave-bottom currents for export

    use w3gdatmd,  only : nseal, nk, nth, sig, dmin, ecos, esin, dden, mapsf, mapsta, nspec
    use w3adatmd,  only : dw, cg, wn
    use w3odatmd,  only : naproc, iaproc
    use constants, only : tpi, grav

    ! input/output variables
    real, intent(in)            :: a(nth,nk,0:nseal) ! Input spectra (in par list to change shape)
    real(ESMF_KIND_R8), pointer :: wbxn(:)           ! 2D eastward-component export field pointer
    real(ESMF_KIND_R8), pointer :: wbyn(:)           ! 2D northward-component export field pointer
    real(ESMF_KIND_R8), pointer :: wbpn(:)           ! 2D period export field pointer

    ! local variables
    real(8), parameter   :: half  = 0.5_r8
    real(8), parameter   ::  one  = 1.0_r8
    real(8), parameter   ::  two  = 2.0_r8
    real(8), parameter   :: kdmin = 1e-7_r8
    real(8), parameter   :: kdmax = 18.0_r8
    integer              :: isea, jsea, ik, ith
    real(8)              :: depth
    real(8)              :: kd, fack, fkd, aka, akx, aky, abr, ubr, ubx, uby, dir
    real(8), allocatable :: sig2(:)
    !---------------------------------------------------------------------------

    allocate( sig2(1:nk) )
    sig2(1:nk) = sig(1:nk)**2

    wbxn(:) = zero
    wbyn(:) = zero
    wbpn(:) = zero

    jsea_loop: do jsea = 1,nseal
       isea = iaproc + (jsea-1)*naproc
       if ( dw(isea).le.zero ) cycle jsea_loop
       depth = max(dmin,dw(isea))
       abr = zero
       ubr = zero
       ubx = zero
       uby = zero
       ik_loop: do ik = 1,nk
          aka = zero
          akx = zero
          aky = zero
          ith_loop: do ith = 1,nth
             aka = aka + a(ith,ik,jsea)
             akx = akx + a(ith,ik,jsea)*ecos(ith)
             aky = aky + a(ith,ik,jsea)*esin(ith)
          enddo ith_loop
          fack = dden(ik)/cg(ik,isea)
          kd = max(kdmin,min(kdmax,wn(ik,isea)*depth))
          fkd = fack/sinh(kd)**2
          abr = abr + aka*fkd
          ubr = ubr + aka*sig2(ik)*fkd
          ubx = ubx + akx*sig2(ik)*fkd
          uby = uby + aky*sig2(ik)*fkd
       enddo ik_loop
       if ( abr.le.zero .or. ubr.le.zero ) cycle jsea_loop
       abr = sqrt(two*abr)
       ubr = sqrt(two*ubr)
       dir = atan2(uby,ubx)
       wbxn(jsea) = ubr*cos(dir)
       wbyn(jsea) = ubr*sin(dir)
       wbpn(jsea) = tpi*abr/ubr
    enddo jsea_loop

    deallocate( sig2 )

  end subroutine CalcBotcur

  !===============================================================================
  subroutine CalcRoughl ( wrln, rc)
    !  Calculate 2D wave roughness length for export

    use w3gdatmd,   only : nseal, nk, nth, sig, dmin, ecos, esin, dden, mapsf, mapsta, nspec
    use w3adatmd,   only : dw, cg, wn, charn, u10, u10d
    use w3wdatmd,   only : va, ust
    use w3odatmd,   only : naproc, iaproc
    use constants,  only : grav
    use w3src4md,   only : w3spr4

    ! input/output variables
    implicit none
    real(r8), pointer     :: wrln(:)
    integer , intent(out) :: rc

    ! local variables
    integer       :: isea, jsea, ix, iy
    real          :: emean, fmean, fmean1, wnmean, amax, ustar, ustdr
    real          :: tauwx, tauwy, cd, z0, fmeanws, dlwmean
    logical       :: llws(nspec)
    logical, save :: firstCall = .true.
    ! -------------------------------------------------------------------- /

    rc = ESMF_SUCCESS

    jsea_loop: do jsea = 1,nseal
       isea = iaproc + (jsea-1)*naproc
       ix = mapsf(isea,1)
       iy = mapsf(isea,2)
       if ( mapsta(iy,ix) == 1 ) then
          if ( firstCall ) then
             charn(jsea) = zero
             llws(:) = .true.
             ustar = zero
             ustdr = zero
             call w3spr4( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea),   &
                  emean, fmean, fmean1, wnmean, amax,         &
                  u10(isea), u10d(isea), ustar, ustdr, tauwx, &
                  tauwy, cd, z0, charn(jsea), llws, fmeanws,  &
                  dlwmean )
          endif !firstCall
          wrln(jsea) = charn(jsea)*ust(isea)**2/grav
       endif
    enddo jsea_loop

    firstCall = .false.

  end subroutine CalcRoughl

  subroutine CalcRadstr2D ( a, sxxn, sxyn, syyn )
    !     Calculate 2D radiation stresses for export

    use w3gdatmd,   only : nseal, nk, nth, sig, es2, esc, ec2, fte, dden
    use w3adatmd,   only : dw, cg, wn
    use w3odatmd,   only : naproc, iaproc
    use constants,  only : dwat, grav

#ifdef W3_PDLIB
    use yowNodepool, only: np, iplg
#endif
    !/ Parameter list
    implicit none

    real, intent(in)               :: a(nth,nk,0:nseal) ! Input spectra (in par list to change shape)
    real(ESMF_KIND_R8), pointer    :: sxxn(:)           ! 2D eastward-component export field
    real(ESMF_KIND_R8), pointer    :: sxyn(:)           ! 2D eastward-northward-component export field
    real(ESMF_KIND_R8), pointer    :: syyn(:)           ! 2D northward-component export field

    !/ Local parameters
    character(ESMF_MAXSTR) :: cname
    character(128)         :: msg
    real(8), parameter     :: half  = 0.5
    real(8), parameter     ::  one  = 1.0
    real(8), parameter     ::  two  = 2.0
    integer                :: isea, jsea, ik, ith
    real(8)                :: sxxs, sxys, syys
    real(8)                :: akxx, akxy, akyy, cgoc, facd, fack, facs
    ! -------------------------------------------------------------------- /

    facd = dwat*grav
#ifdef W3_PDLIB
    if ( LPDLIB == .FALSE. ) then
#endif
       jsea_loop: do jsea = 1,nseal
          isea = iaproc + (jsea-1)*naproc
          if ( dw(isea).le.zero ) cycle jsea_loop
          sxxs = zero
          sxys = zero
          syys = zero
          ik_loop: do ik = 1,nk
             akxx = zero
             akxy = zero
             akyy = zero
             cgoc = cg(ik,isea)*wn(ik,isea)/sig(ik)
             cgoc = min(one,max(half,cgoc))
             ith_loop: do ith = 1,nth
                akxx = akxx + (cgoc*(ec2(ith)+one)-half)*a(ith,ik,jsea)
                akxy = akxy + cgoc*esc(ith)*a(ith,ik,jsea)
                akyy = akyy + (cgoc*(es2(ith)+one)-half)*a(ith,ik,jsea)
             enddo ith_loop
             fack = dden(ik)/cg(ik,isea)
             sxxs = sxxs + akxx*fack
             sxys = sxys + akxy*fack
             syys = syys + akyy*fack
          enddo ik_loop
          facs = (one+fte/cg(nk,isea))*facd
          sxxn(jsea) = sxxs*facs
          sxyn(jsea) = sxys*facs
          syyn(jsea) = syys*facs
       enddo jsea_loop
#ifdef W3_PDLIB
    else
       jsea_loop2: do jsea = 1,np
          isea = iplg(jsea)
          sxxn(jsea) = sxx(jsea)
          sxyn(jsea) = sxy(jsea)
          syyn(jsea) = syy(jsea)
       enddo jsea_loop2
    endif
#endif

  end subroutine CalcRadstr2D
end module wav_import_export
