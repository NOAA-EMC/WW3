module wav_import_export

  use ESMF
  use NUOPC
  use NUOPC_Model
  use wav_kind_mod , only : r8 => shr_kind_r8, r4 => shr_kind_r4, i4 => shr_kind_i4
  use wav_kind_mod , only : CL => shr_kind_cl, CS => shr_kind_cs
  use wav_shr_mod  , only : ymd2date
  use wav_shr_mod  , only : chkerr
  use wav_shr_mod  , only : state_diagnose, state_reset
  use wav_shr_mod  , only : wav_coupling_to_cice, merge_import, dbug_flag
  use constants    , only : grav, tpi, dwat

  implicit none
  private ! except

  public  :: advertise_fields
  public  :: realize_fields
  public  :: import_fields
  public  :: export_fields
  public  :: state_getfldptr
  public  :: state_fldchk
  public  :: CalcRoughl

  private :: fldlist_add
  private :: fldlist_realize
  private :: set_importmask
  private :: check_globaldata
  private :: readfromfile

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

  real(r4), allocatable  :: import_mask(:) ! mask for valid import data
  real(r8), parameter    :: zero  = 0.0_r8

#ifdef CESMCOUPLED
  logical :: cesmcoupled = .true.
#else
  logical :: cesmcoupled = .false.
#endif

  character(*),parameter :: u_FILE_u = &
       __FILE__

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
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    !--------------------------------
    ! Advertise import fields
    !--------------------------------

   !call fldlist_add(fldsToWav_num, fldsToWav, 'So_h'       )
    call fldlist_add(fldsToWav_num, fldsToWav, 'Si_ifrac'   )
    call fldlist_add(fldsToWav_num, fldsToWav, 'So_u'       )
    call fldlist_add(fldsToWav_num, fldsToWav, 'So_v'       )
    call fldlist_add(fldsToWav_num, fldsToWav, 'So_t'       )
    call fldlist_add(fldsToWav_num, fldsToWav, 'Sa_tbot'    )
    if (cesmcoupled) then
       call fldlist_add(fldsToWav_num, fldsToWav, 'Sa_u'       )
       call fldlist_add(fldsToWav_num, fldsToWav, 'Sa_v'       )
       call fldlist_add(fldsToWav_num, fldsToWav, 'So_bldepth' )
    else
       call fldlist_add(fldsToWav_num, fldsToWav, 'Sa_u10m'    )
       call fldlist_add(fldsToWav_num, fldsToWav, 'Sa_v10m'    )
    end if

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
    if (cesmcoupled) then
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_lamult' )
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_ustokes')
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_vstokes')
       !call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_hstokes')
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_pstokes_x', ungridded_lbound=1, ungridded_ubound=3)
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_pstokes_y', ungridded_lbound=1, ungridded_ubound=3)
    else
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_z0')
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_ustokes1')
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_ustokes2')
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_ustokes3')
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_vstokes1')
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_vstokes2')
       call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_vstokes3')
    end if

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

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

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
    type(ESMF_State) :: importState
    type(ESMF_State) :: exportState
    character(len=*), parameter :: subname='(wav_import_export:realize_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

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

    call state_reset(ExportState, zero, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_reset(ImportState, zero, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
       call state_diagnose(exportState, 'after state_reset', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine realize_fields

  !===============================================================================
  subroutine import_fields( gcomp, time0, timen, rc )

    !---------------------------------------------------------------------------
    ! Obtain the wave input from the mediator
    !---------------------------------------------------------------------------

    use w3gdatmd    , only: nseal, MAPSTA, MAPFS, MAPSF, NX, NY
    use w3idatmd    , only: CX0, CY0, CXN, CYN, DT0, DTN, ICEI, WLEV, INFLAGS1, ICEP1, ICEP5
    use w3idatmd    , only: TC0, TCN, TLN, TIN, TI1, TI5, TW0, TWN, WX0, WY0, WXN, WYN
    use w3idatmd    , only: UX0, UY0, UXN, UYN, TU0, TUN
    use w3wdatmd    , only: time
#ifdef CESMCOUPLED
    use w3idatmd    , only: HML
#endif

    ! input/output variables
    type(ESMF_GridComp) , intent(inout) :: gcomp
    integer             , intent(in)    :: time0(2), timen(2)
    integer             , intent(out)   :: rc

    ! Local variables
    type(ESMF_State)        :: importState
    type(ESMF_VM)           :: vm
    type(ESMF_Clock)        :: clock
    integer                 :: n, ix, iy
    real(r4)                :: data_global(nx*ny)
    real(r4), allocatable   :: data_global2(:)
    real(r4)                :: def_value
    character(len=10)       :: uwnd
    character(len=10)       :: vwnd
    real(r4), allocatable   :: wxdata(:)      ! only needed if merge_import
    real(r4), allocatable   :: wydata(:)      ! only needed if merge_import
    character(len=CL)       :: msgString
    character(len=*), parameter :: subname='(wav_import_export:import_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    if (cesmcoupled) then
       uwnd = 'Sa_u'
       vwnd = 'Sa_v'
    else
       uwnd = 'Sa_u10m'
       vwnd = 'Sa_v10m'
    end if

    ! Get import state, clock and vm
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) then
       call state_diagnose(importState, 'at import ', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    ! input fields associated with W3FLDG calls in ww3_shel.ftn
    ! these arrays are global, just fill the local cells for use later
    ! fill both the lower (0) and upper (N) bound data with the same values
    ! fill with special values as default, these should not be used in practice
    ! set time for input data to time0 and timen (shouldn't matter)

    ! note that INFLAGS(1-5) is true for CESM, for UWM INFLAGS(2:4) is true
    def_value = 0.0_r4

    ! ---------------
    ! INFLAGS1(1)
    ! ---------------
    if (INFLAGS1(1)) then
       TLN  = timen
       n = 0
       do iy = 1,NY
          do ix = 1,NX
             n = n + 1
             WLEV(ix,iy) = 0.0
          end do
       end do
    endif
    ! ---------------
    ! INFLAGS1(2) - ocn current fields
    ! ---------------
    if (INFLAGS1(2)) then
       TC0  = time0       ! times for ocn current fields
       TCN  = timen

       CX0(:,:) = def_value   ! ocn u current
       CXN(:,:) = def_value
       if (state_fldchk(importState, 'So_u')) then
          call SetGlobalInput(importState, 'So_u', vm, data_global, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          n = 0
          do iy = 1,NY
             do ix = 1,NX
                n = n + 1
                CX0(ix,iy) = data_global(n)
                CXN(ix,iy) = data_global(n)
             end do
          end do
       end if

       CY0(:,:) = def_value   ! ocn v current
       CYN(:,:) = def_value
       if (state_fldchk(importState, 'So_v')) then
          call SetGlobalInput(importState, 'So_v', vm, data_global, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          n = 0
          do iy = 1,NY
             do ix = 1,NX
                n = n + 1
                CY0(ix,iy) = data_global(n)
                CYN(ix,iy) = data_global(n)
             end do
          end do
       end if
    end if

    ! ---------------
    ! INFLAGS1(3) - atm wind/temp fields
    ! ---------------
    if (INFLAGS1(3)) then
       TW0  = time0       ! times for atm wind/temp fields.
       TWN  = timen

       if (merge_import) then
          ! set mask using u-wind field if merge_import; all import fields
          ! will have same missing overlap region
          ! import_mask memory will be allocate in set_importmask
          call set_importmask(importState, clock, trim(uwnd), vm, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          allocate(wxdata(nx*ny))
          allocate(wydata(nx*ny))
          call readfromfile('WND', wxdata, wydata, time0, timen, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          if (dbug_flag > 10) then
             call check_globaldata(gcomp, 'wxdata', wxdata, rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
             call check_globaldata(gcomp, 'wydata', wydata, rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
             call check_globaldata(gcomp, 'import_mask', import_mask, rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
          end if
       end if

       ! atm u wind
       WX0(:,:) = def_value
       WXN(:,:) = def_value
       if (state_fldchk(importState, trim(uwnd))) then
          call SetGlobalInput(importState, trim(uwnd), vm, data_global, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          n = 0
          do iy = 1,NY
             do ix = 1,NX
                n = n + 1
                if (merge_import) then
                   WX0(ix,iy) = data_global(n)*import_mask(n) + (1.0_r4 - import_mask(n))*wxdata(n)
                   WXN(ix,iy) = data_global(n)*import_mask(n) + (1.0_r4 - import_mask(n))*wxdata(n)
                else
                   WX0(ix,iy) = data_global(n)
                   WXN(ix,iy) = data_global(n)
                end if
             end do
          end do
          if (dbug_flag > 10) then
             call check_globaldata(gcomp, 'wx0', wx0, rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
          end if
       end if

       ! atm v wind
       WY0(:,:) = def_value
       WYN(:,:) = def_value
       if (state_fldchk(importState, trim(vwnd))) then
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          call SetGlobalInput(importState, trim(vwnd), vm, data_global, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          n = 0
          do iy = 1,NY
             do ix = 1,NX
                n = n + 1
                if (merge_import) then
                   WY0(ix,iy)  = data_global(n)*import_mask(n) + (1.0_r4 - import_mask(n))*wydata(n)
                   WYN(ix,iy)  = data_global(n)*import_mask(n) + (1.0_r4 - import_mask(n))*wydata(n)
                else
                   WY0(ix,iy)  = data_global(n)
                   WYN(ix,iy)  = data_global(n)
                end if
             end do
          end do
          if (dbug_flag > 10) then
             call check_globaldata(gcomp, 'wy0', wy0, rc)
             if (ChkErr(rc,__LINE__,u_FILE_u)) return
          end if
       end if

       ! air temp - ocn temp
       DT0(:,:) = def_value
       DTN(:,:) = def_value
       if ((state_fldchk(importState, 'So_t')) .and. (state_fldchk(importState, 'Sa_tbot'))) then
          allocate(data_global2(nx*ny))
          call SetGlobalInput(importState, 'Sa_tbot', vm, data_global, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          call SetGlobalInput(importState, 'So_t', vm, data_global2, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          n = 0
          do iy = 1,NY
             do ix = 1,NX
                n = n + 1
                DT0(ix,iy)  = data_global(n) - data_global2(n)
                DTN(ix,iy)  = data_global(n) - data_global2(n)
             end do
          end do
          deallocate(data_global2)
       end if
       ! Deallocate memory for merge_import
       if (merge_import) then
          deallocate(wxdata)
          deallocate(wydata)
       end if
    end if

    ! ---------------
    ! INFLAGS1(4) - ice fraction field
    ! ---------------
    if (INFLAGS1(4)) then
       TIN       = timen       ! time for ice field
       ICEI(:,:) = def_value   ! ice frac
       if (state_fldchk(importState, 'Si_ifrac')) then
          call SetGlobalInput(importState, 'Si_ifrac', vm, data_global, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          n = 0
          do iy = 1,NY
             do ix = 1,NX
                n = n + 1
                ICEI(ix,iy) = data_global(n)
             end do
          end do
       end if
    end if
#ifdef CESMCOUPLED
    ! ---------------
    ! ocean boundary layer depth - always assume that this is being imported for CESM
    ! ---------------
    call SetGlobalInput(importState, 'So_bldepth', vm, data_global, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    n = 0
    do iy = 1,NY
       do ix = 1,NX
          n = n + 1
          HML(ix,iy) = max(data_global(n), 5.) ! ocn mixing layer depth
       end do
    end do
#endif
    ! ---------------
    ! INFLAGS1(5) - atm momentum fields
    ! ---------------
    if (INFLAGS1(5)) then
       TU0  = time0       ! times for atm momentum fields.
       TUN  = timen

       UX0(:,:) = def_value   ! atm u momentum
       UXN(:,:) = def_value
       if (state_fldchk(importState, 'Faxa_taux')) then
          call SetGlobalInput(importState, 'Faxa_taux', vm, data_global, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          n = 0
          do iy = 1,NY
             do ix = 1,NX
                n = n + 1
                UX0(ix,iy) = data_global(n)
                UXN(ix,iy) = data_global(n)
             end do
          end do
       end if

       UY0(:,:) = def_value   ! atm v momentum
       UYN(:,:) = def_value
       if (state_fldchk(importState, 'Faxa_tauy')) then
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          call SetGlobalInput(importState, 'Faxa_tauy', vm, data_global, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if
       n = 0
       do iy = 1,NY
          do ix = 1,NX
             n = n + 1
             UY0(ix,iy)  = data_global(n)
             UYN(ix,iy)  = data_global(n)
          end do
       end do
    end if
    ! ---------------
    ! INFLAGS1(-7)
    ! ---------------
    if (INFLAGS1(-7)) then
       TI1  = timen       ! time for ice field
       ICEP1(:,:) = def_value   ! ice thickness
       if (state_fldchk(importState, 'Si_thick')) then
          call SetGlobalInput(importState, 'Si_thick', vm, data_global, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          n = 0
          do iy = 1,NY
             do ix = 1,NX
                n = n + 1
                ICEP1(ix,iy) = data_global(n) ! ice thickness
             end do
          end do
       end if
    end if
    ! ---------------
    ! INFLAGS1(-3)
    ! ---------------
    if (INFLAGS1(-3)) then
       TI5  = timen        ! time for ice field
       ICEP5(:,:) = def_value   ! ice floe size
       if (state_fldchk(importState, 'Si_floediam')) then
          call SetGlobalInput(importState, 'Si_floediam', vm, data_global, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          n = 0
          do iy = 1,NY
             do ix = 1,NX
                n = n + 1
                ICEP5(ix,iy) = data_global(n) ! ice floe diameter
             end do
          end do
       end if
    end if

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine import_fields

  !====================================================================================
  subroutine export_fields (gcomp, rc)

    !---------------------------------------------------------------------------
    ! Create the export state
    !---------------------------------------------------------------------------

    use wav_kind_mod,   only : R8 => SHR_KIND_R8
    use w3adatmd      , only : USSX, USSY, EF, TAUICE, USSP
#ifdef CESMCOUPLED
    use w3adatmd      , only : LAMULT
#endif
    use w3wdatmd      , only : va
    use w3odatmd      , only : naproc, iaproc
    use w3gdatmd      , only : nseal, MAPSTA, MAPFS, MAPSF, USSPF, NK
    use w3iogomd      , only : CALC_U3STOKES

    ! input/output/variables
    type(ESMF_GridComp)            :: gcomp
    integer          , intent(out) :: rc

    ! Local variables
    real(R8)          :: fillvalue = 1.0e30_R8                 ! special missing value
    type(ESMF_State)  :: exportState
    integer           :: n, jsea, isea, ix, iy, lsize, ib

    real(r8), pointer :: z0rlen(:)
    real(r8), pointer :: charno(:)
    real(r8), pointer :: wbcuru(:)
    real(r8), pointer :: wbcurv(:)
    real(r8), pointer :: wbcurp(:)
   !real(r8), pointer :: uscurr(:)
   !real(r8), pointer :: vscurr(:)
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

    ! Partitioned stokes drift
    real(r8), pointer :: sw_pstokes_x(:,:) ! cesm
    real(r8), pointer :: sw_pstokes_y(:,:) ! cesm
    real(r8), pointer :: sw_ustokes1(:)    ! ufs
    real(r8), pointer :: sw_vstokes1(:)    ! ufs
    real(r8), pointer :: sw_ustokes2(:)    ! ufs
    real(r8), pointer :: sw_vstokes2(:)    ! ufs
    real(r8), pointer :: sw_ustokes3(:)    ! ufs
    real(r8), pointer :: sw_vstokes3(:)    ! ufs
    character(len=*), parameter :: subname='(wav_import_export:export_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    ! Get export state
    call NUOPC_ModelGet(gcomp, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

#ifdef CESMCOUPLED
    if (state_fldchk(exportState, 'Sw_lamult')) then
       call state_getfldptr(exportState, 'Sw_lamult', fldptr1d=sw_lamult, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       sw_lamult(:) = fillvalue
       do jsea=1, nseal
          isea = iaproc + (jsea-1)*naproc
          ix  = mapsf(isea,1)
          iy  = mapsf(isea,2)
          if (mapsta(iy,ix) == 1) then
             sw_lamult(jsea) = LAMULT(jsea)
          else
             sw_lamult(jsea)  = 1.
          endif
       enddo
    end if
#endif

    ! surface stokes drift
    if (state_fldchk(exportState, 'Sw_ustokes')) then
       call state_getfldptr(exportState, 'Sw_ustokes', fldptr1d=sw_ustokes, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       sw_ustokes(:) = fillvalue
       do jsea=1, nseal
          isea = iaproc + (jsea-1)*naproc
          ix  = mapsf(isea,1)
          iy  = mapsf(isea,2)
          if (mapsta(iy,ix) == 1) then
             sw_ustokes(jsea) = USSX(jsea)
          else
             sw_ustokes(jsea) = 0.
          endif
       enddo
    end if
    if (state_fldchk(exportState, 'Sw_vstokes')) then
       call state_getfldptr(exportState, 'Sw_vstokes', fldptr1d=sw_vstokes, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       sw_vstokes(:) = fillvalue
       do jsea=1, nseal
          isea = iaproc + (jsea-1)*naproc
          ix  = mapsf(isea,1)
          iy  = mapsf(isea,2)
          if (mapsta(iy,ix) == 1) then
             sw_vstokes(jsea) = USSY(jsea)
          else
             sw_vstokes(jsea) = 0.
          endif
       enddo
    end if

    if (state_fldchk(exportState, 'Sw_ch')) then
       call state_getfldptr(exportState, 'charno', fldptr1d=charno, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call CalcCharnk(charno)
    endif

    if (state_fldchk(exportState, 'Sw_z0')) then
       call state_getfldptr(exportState, 'Sw_z0', fldptr1d=z0rlen, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call CalcRoughl(z0rlen)
    endif

    !TODO: what is difference between uscurr/vscurr and sw_ustokes,sw_vstokes?
    ! uscurr has standard name eastward_stokes_drift_current
    ! vscurr has standard name northward_stokes_drift_current
    ! in fd_nems.yaml but this seems to be calculated a (:,:) value
    !if ( state_fldchk(exportState, 'uscurr') .and. &
    !     state_fldchk(exportState, 'vscurr')) then
    !   call state_getfldptr(exportState, 'uscurr', fldptr1d=uscurr, rc=rc)
    !   if (ChkErr(rc,__LINE__,u_FILE_u)) return
    !   call state_getfldptr(exportState, 'vscurr', fldptr1d=vscurr, rc=rc)
    !   if (ChkErr(rc,__LINE__,u_FILE_u)) return
    !   call CalcStokes3D( va, uscurr, vscurr )
    !endif

    if ( state_fldchk(exportState, 'wbcuru') .and. &
         state_fldchk(exportState, 'wbcurv') .and. &
         state_fldchk(exportState, 'wbcurp')) then
       call state_getfldptr(exportState, 'wbcuru', fldptr1d=wbcuru, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wbcurv', fldptr1d=wbcurv, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wbcurp', fldptr1d=wbcurp, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call CalcBotcur( va, wbcuru, wbcurv, wbcurp)
    end if

    if ( state_fldchk(exportState, 'wavsuu') .and. &
         state_fldchk(exportState, 'wavsuv') .and. &
         state_fldchk(exportState, 'wavsvv')) then
       call state_getfldptr(exportState, 'sxxn', fldptr1d=sxxn, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'sxyn', fldptr1d=sxyn, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'syyn', fldptr1d=syyn, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call CalcRadstr2D( va, sxxn, sxyn, syyn)
    end if

    if (wav_coupling_to_cice) then
       call state_getfldptr(exportState, 'wav_tauice1', fldptr1d=wav_tauice1, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'wav_tauice2', fldptr1d=wav_tauice2, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
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

       ! Initialize wave elevation spectrum
       wav_tauice1(:) = fillvalue
       wav_tauice2(:) = fillvalue
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

       do jsea=1, nseal                         ! jsea is local
          isea = iaproc + (jsea-1)*naproc       ! isea is global
          ix  = mapsf(isea,1)                   ! global ix
          iy  = mapsf(isea,2)                   ! global iy
          if (mapsta(iy,ix) .eq. 1) then        ! active sea point
             wav_tauice1(jsea) = TAUICE(jsea,1) ! tau ice is 2D
             wav_tauice2(jsea) = TAUICE(jsea,2) ! tau ice is 2D

             ! If wave_elevation_spectrum is UNDEF  - needs ouput flag to be turned on
             ! wave_elevation_spectrum as 25 variables
             wave_elevation_spectrum1(jsea)  = EF(jsea,1)
             wave_elevation_spectrum2(jsea)  = EF(jsea,2)
             wave_elevation_spectrum3(jsea)  = EF(jsea,3)
             wave_elevation_spectrum4(jsea)  = EF(jsea,4)
             wave_elevation_spectrum5(jsea)  = EF(jsea,5)
             wave_elevation_spectrum6(jsea)  = EF(jsea,6)
             wave_elevation_spectrum7(jsea)  = EF(jsea,7)
             wave_elevation_spectrum8(jsea)  = EF(jsea,8)
             wave_elevation_spectrum9(jsea)  = EF(jsea,9)
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
             wav_tauice1(jsea) = 0.
             wav_tauice2(jsea) = 0.
             wave_elevation_spectrum1(jsea)  = 0.
             wave_elevation_spectrum2(jsea)  = 0.
             wave_elevation_spectrum3(jsea)  = 0.
             wave_elevation_spectrum4(jsea)  = 0.
             wave_elevation_spectrum5(jsea)  = 0.
             wave_elevation_spectrum6(jsea)  = 0.
             wave_elevation_spectrum7(jsea)  = 0.
             wave_elevation_spectrum8(jsea)  = 0.
             wave_elevation_spectrum9(jsea)  = 0.
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
    end if

    if ( state_fldchk(exportState, 'Sw_pstokes_x') .and. &
         state_fldchk(exportState, 'Sw_pstokes_y') )then
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'Sw_pstokes_x', fldptr2d=sw_pstokes_x, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'Sw_pstokes_y', fldptr2d=sw_pstokes_y, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       sw_pstokes_x(:,:) = fillvalue
       sw_pstokes_y(:,:) = fillvalue
       if (USSPF(1) > 0) then ! Partitioned Stokes drift computation is turned on in mod_def file.
          call CALC_U3STOKES(va, 2)
          do ib = 1, USSPF(2)
             do jsea = 1, nseal
                sw_pstokes_x(ib,jsea) = ussp(jsea,ib)
                sw_pstokes_y(ib,jsea) = ussp(jsea,nk+ib)
             enddo
          end do
       end if
    endif

    if ( state_fldchk(exportState, 'Sw_ustokes1') .and. &
         state_fldchk(exportState, 'Sw_ustokes2') .and. &
         state_fldchk(exportState, 'Sw_ustokes3') .and. &
         state_fldchk(exportState, 'Sw_vstokes1') .and. &
         state_fldchk(exportState, 'Sw_vstokes2') .and. &
         state_fldchk(exportState, 'Sw_vstokes3') ) then

       call state_getfldptr(exportState, 'Sw_ustokes1', fldptr1d=sw_ustokes1, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'Sw_ustokes2', fldptr1d=sw_ustokes2, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'Sw_ustokes3', fldptr1d=sw_ustokes3, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'Sw_vstokes1', fldptr1d=sw_vstokes1, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'Sw_vstokes2', fldptr1d=sw_vstokes2, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(exportState, 'Sw_vstokes3', fldptr1d=sw_vstokes3, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       sw_ustokes1(:)= zero
       sw_vstokes1(:)= zero
       sw_ustokes2(:)= zero
       sw_vstokes2(:)= zero
       sw_ustokes3(:)= zero
       sw_vstokes3(:)= zero
       call CALC_U3STOKES(va, 2)
       do jsea = 1,nseal
          sw_ustokes1(jsea)=ussp(jsea,1)
          sw_vstokes1(jsea)=ussp(jsea,nk+1)
          sw_ustokes2(jsea)=ussp(jsea,2)
          sw_vstokes2(jsea)=ussp(jsea,nk+2)
          sw_ustokes3(jsea)=ussp(jsea,3)
          sw_vstokes3(jsea)=ussp(jsea,nk+3)
       end do
    end if

    if (dbug_flag > 5) then
       call state_diagnose(exportState, 'at export ', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine export_fields

  !===============================================================================

  subroutine fldlist_add(num, fldlist, stdname, ungridded_lbound, ungridded_ubound)
    integer,                    intent(inout) :: num
    type(fld_list_type),        intent(inout) :: fldlist(:)
    character(len=*),           intent(in)    :: stdname
    integer, optional,          intent(in)    :: ungridded_lbound
    integer, optional,          intent(in)    :: ungridded_ubound

    ! local variables
    character(len=*), parameter :: subname='(wav_import_export:fldlist_add)'
    !-------------------------------------------------------------------------------

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

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
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

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
    type(ESMF_State),            intent(in)    :: State
    character(len=*),            intent(in)    :: fldname
    real(R8), pointer, optional, intent(out)   :: fldptr1d(:)
    real(R8), pointer, optional, intent(out)   :: fldptr2d(:,:)
    integer,                     intent(out)   :: rc

    ! local variables
    type(ESMF_FieldStatus_Flag) :: status
    type(ESMF_Field)            :: lfield
    type(ESMF_Mesh)             :: lmesh
    integer                     :: nnodes, nelements
    character(len=*), parameter :: subname='(wav_import_export:state_getfldptr)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

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

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine state_getfldptr

  !===============================================================================
  logical function state_fldchk(State, fldname)
    ! ----------------------------------------------
    ! Determine if field is in state
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_State) , intent(in)  :: State
    character(len=*) , intent(in)  :: fldname

    ! local variables
    type(ESMF_StateItem_Flag) :: itemType
    ! ----------------------------------------------

    call ESMF_StateGet(State, trim(fldname), itemType)
    State_FldChk = (itemType /= ESMF_STATEITEM_NOTFOUND)

  end function state_fldchk

  !===============================================================================
  subroutine CalcCharnk ( chkn )

    ! Calculate Charnok for export

    use w3gdatmd,   only : nseal, nk, nth, sig, mapsf, mapsta, nspec
    use w3adatmd,   only : cg, wn, charn, u10, u10d
    use w3wdatmd,   only : va
    use w3odatmd,   only : naproc, iaproc
#ifdef W3_ST3
    use w3src3md,   only : w3spr3
#endif
#ifdef W3_ST4
    use w3src4md,   only : w3spr4
#endif

    ! input/output variables
    real(ESMF_KIND_R8), pointer :: chkn(:)  ! 2D Charnock export field pointer

    ! local variables
    real   , parameter :: zero  = 0.0
    integer            :: isea, jsea, ix, iy
    real               :: emean, fmean, fmean1, wnmean, amax, ustar, ustdr
    real               :: tauwx, tauwy, cd, z0, fmeanws, dlwmean
    logical            :: llws(nspec)
    logical, save      :: firstCall = .true.
    !----------------------------------------------------------------------

    !TODO: fix firstCall like for Roughl
    jsea_loop: do jsea = 1,nseal
       isea = iaproc + (jsea-1)*naproc
       if ( firstCall ) then
          charn(jsea) = zero
          llws(:) = .true.
          ustar = zero
          ustdr = zero
#ifdef W3_ST3
          call w3spr3( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea),   &
                       emean, fmean, fmean1, wnmean, amax,         &
                       u10(isea), u10d(isea), ustar, ustdr, tauwx, &
                       tauwy, cd, z0, charn(jsea), llws, fmeanws )
#endif
#ifdef W3_ST4
           call w3spr4( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea),  &
                       emean, fmean, fmean1, wnmean, amax,         &
                       u10(isea), u10d(isea), ustar, ustdr, tauwx, &
                       tauwy, cd, z0, charn(jsea), llws, fmeanws,  &
                       dlwmean )
#endif
       endif !firstCall
       chkn(jsea) = charn(jsea)
    enddo jsea_loop

    firstCall = .false.

  end subroutine CalcCharnk

  !===============================================================================
  subroutine CalcRoughl ( wrln)

    ! Calculate 2D wave roughness length for export

    use w3gdatmd,   only : nseal, nk, nth, sig, dmin, ecos, esin, dden, mapsf, mapsta, nspec
    use w3adatmd,   only : dw, cg, wn, charn, u10, u10d
    use w3wdatmd,   only : va, ust
    use w3odatmd,   only : naproc, iaproc
#ifdef W3_ST3
    use w3src3md,   only : w3spr3
#endif
#ifdef W3_ST4
    use w3src4md,   only : w3spr4
#endif
    use wav_shr_mod, only : runtype

    ! input/output variables
    real(r8), pointer :: wrln(:) ! 2D roughness length export field ponter

    ! local variables
    integer       :: isea, jsea, ix, iy
    real          :: emean, fmean, fmean1, wnmean, amax, ustar, ustdr
    real          :: tauwx, tauwy, cd, z0, fmeanws, dlwmean
    logical       :: llws(nspec)
    logical, save :: firstCall = .true.

    !----------------------------------------------------------------------

    jsea_loop: do jsea = 1,nseal
       isea = iaproc + (jsea-1)*naproc
       ix = mapsf(isea,1)
       iy = mapsf(isea,2)
       if ( firstCall ) then
          if(( runtype == 'initial'  .and.     mapsta(iy,ix)  == 1 ) .or. &
             ( runtype == 'continue' .and. abs(mapsta(iy,ix)) == 1 )) then
             charn(jsea) = zero
             llws(:) = .true.
             ustar = zero
             ustdr = zero
             tauwx = zero
             tauwy = zero
#ifdef W3_ST3
             call w3spr3( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea),   &
                          emean, fmean, fmean1, wnmean, amax,         &
                          u10(isea), u10d(isea), ustar, ustdr, tauwx, &
                          tauwy, cd, z0, charn(jsea), llws, fmeanws )
#endif
#ifdef W3_ST4
             call w3spr4( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea),   &
                          emean, fmean, fmean1, wnmean, amax,         &
                          u10(isea), u10d(isea), ustar, ustdr, tauwx, &
                          tauwy, cd, z0, charn(jsea), llws, fmeanws,  &
                          dlwmean )
#endif
          end if
       endif !firstCall
       wrln(jsea) = charn(jsea)*ust(isea)**2/grav
    enddo jsea_loop

    firstCall = .false.

  end subroutine CalcRoughl

  !===============================================================================
  subroutine CalcBotcur ( a, wbxn, wbyn, wbpn )

    ! Calculate wave-bottom currents for export

    use w3gdatmd,  only : nseal, nk, nth, sig, dmin, ecos, esin, dden, mapsf, mapsta, nspec
    use w3adatmd,  only : dw, cg, wn
    use w3odatmd,  only : naproc, iaproc

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
    !----------------------------------------------------------------------

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
  subroutine CalcRadstr2D ( a, sxxn, sxyn, syyn )

    ! Calculate 2D radiation stresses for export

    use w3gdatmd,   only : nseal, nk, nth, sig, es2, esc, ec2, fte, dden
    use w3adatmd,   only : dw, cg, wn
    use w3odatmd,   only : naproc, iaproc
#ifdef W3_PDLIB
    use yowNodepool, only: np, iplg
#endif

    ! input/output variables
    real, intent(in)               :: a(nth,nk,0:nseal) ! Input spectra (in par list to change shape)
    real(ESMF_KIND_R8), pointer    :: sxxn(:)           ! 2D eastward-component export field
    real(ESMF_KIND_R8), pointer    :: sxyn(:)           ! 2D eastward-northward-component export field
    real(ESMF_KIND_R8), pointer    :: syyn(:)           ! 2D northward-component export field

    ! local variables
    character(ESMF_MAXSTR) :: cname
    character(128)         :: msg
    real(8), parameter     :: half  = 0.5
    real(8), parameter     ::  one  = 1.0
    real(8), parameter     ::  two  = 2.0
    integer                :: isea, jsea, ik, ith
    real(8)                :: sxxs, sxys, syys
    real(8)                :: akxx, akxy, akyy, cgoc, facd, fack, facs
    !----------------------------------------------------------------------

    facd = dwat*grav
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

  end subroutine CalcRadstr2D

  !====================================================================================
  subroutine SetGlobalInput(importState, fldname, vm, global_output, rc)

    use w3gdatmd, only: nseal, mapsta, mapfs, mapsf, nx, ny
    use w3odatmd, only: naproc, iaproc

    ! input/output variables
    type(ESMF_State) , intent(in)  :: importState
    character(len=*) , intent(in)  :: fldname
    type(ESMF_VM)    , intent(in)  :: vm
    real(r4)         , intent(out) :: global_output(nx*ny)
    integer          , intent(out) :: rc

    ! local variables
    integer           :: n, isea, ix, iy
    real(r4)          :: global_input(nx*ny)
    real(r8), pointer :: dataptr(:)
    character(len=*), parameter :: subname = '(wav_import_export:setGlobalInput)'

    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    call state_getfldptr(importState, trim(fldname), fldptr1d=dataptr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    global_output(:) = 0._r4
    global_input(:) = 0._r4
    do n = 1, nseal
       isea = iaproc + (n-1)*naproc
       ix = mapsf(isea,1)
       iy = mapsf(isea,2)
       global_input(ix + (iy-1)*nx) = real(dataptr(n),4)
    end do
    call ESMF_VMAllReduce(vm, sendData=global_input, recvData=global_output, count=nx*ny, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine SetGlobalInput

  !====================================================================================
  subroutine set_importmask(importState, clock, fldname, vm, rc)

    use w3gdatmd, only: nseal, mapsta, mapfs, mapsf, nx, ny
    use w3odatmd, only: naproc, iaproc

    ! input/output variables
    type(ESMF_State) , intent(in)  :: importState
    type(ESMF_Clock) , intent(in)  :: clock
    character(len=*) , intent(in)  :: fldname
    type(ESMF_VM)    , intent(in)  :: vm
    integer          , intent(out) :: rc

    ! local variables
    type(ESMF_Time)         :: currTime, startTime
    type(ESMF_TimeInterval) :: timeStep
    logical                 :: firstCall, secondCall
    real(r4)                :: fillValue = 9.99e20
    integer                 :: isea, n, ix, iy
    real(r8), pointer       :: dataptr(:)
    real(r4)                :: mask_local(nx*ny)
    real(r4)                :: mask_global(nx*ny)
    character(len=CL)       :: msgString
    character(len=*), parameter :: subname = '(wav_import_export:set_importmask)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! set call flags
    if (startTime == currTime) then
     firstCall  = .true.
     secondCall = .false.
    elseif (currTime == startTime+timeStep) then
     firstCall  = .false.
     secondCall = .true.
    else
     firstCall  = .false.
     secondCall = .false.
    end if
    if (firstcall) then
       allocate(import_mask(nx*ny))
    end if

    ! return if not the first or second call, mask has already been set
    if (.not. firstCall .and. .not. secondCall) return

    ! no valid import at firstCall, use all data
    if (firstCall) then
       import_mask(:) = 0.0_r4
       call ESMF_ClockPrint(clock, options='currTime', preString='Setting initial import_mask at currTime : ', &
          unit=msgString, rc=rc)
       call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    end if

    ! set merge mask where import field has fillvalue due to non-overlapping model domains
    if (secondCall) then
       call ESMF_ClockPrint(clock, options='currTime', preString='Setting new import_mask at currTime : ', &
          unit=msgString, rc=rc)
       call state_getfldptr(importState, trim(fldname), fldptr1d=dataptr, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       import_mask(:) = 0.0_r4
       mask_local(:) = 1.0_r4
       do n = 1, nseal
          isea = iaproc + (n-1)*naproc
          ix = mapsf(isea,1)
          iy = mapsf(isea,2)
          if (real(dataptr(n),4) .ge. fillValue) then
             mask_local(ix + (iy-1)*nx) = 0.0_r4
          end if
       end do
       call ESMF_VMAllReduce(vm, sendData=mask_local, recvData=import_mask, count=nx*ny, reduceflag=ESMF_REDUCE_MIN, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine set_importmask

  !====================================================================================
  subroutine check_globaldata(gcomp, fldname, global_data, rc)

    use w3gdatmd, only: nseal, mapsta, mapfs, mapsf, nx, ny
    use w3odatmd, only: naproc, iaproc

    ! input/output variables
    type(ESMF_GridComp) , intent(inout) :: gcomp
    character(len=*)    , intent(in)    :: fldname
    real(r4)            , intent(in)    :: global_data(nx*ny)
    integer             , intent(out)   :: rc

    ! local variables
    type(ESMF_Clock)                :: clock
    type(ESMF_State)                :: importState
    type(ESMF_Time)                 :: currtime, nexttime
    type(ESMF_Field)                :: lfield
    type(ESMF_Field)                :: newfield
    type(ESMF_MeshLoc)              :: meshloc
    type(ESMF_Mesh)                 :: lmesh
    character(len=CS)               :: timestr
    character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
    integer                         :: fieldCount
    integer                         :: lrank
    integer                         :: yr,mon,day,sec    ! time units
    integer                         :: n, nn, isea, ix, iy
    real(r8), pointer               :: dataptr1d(:)
    real(r8)                        :: fillValue = 9.99e20
    character(len=*), parameter :: subname = '(wav_import_export:check_globaldata)'

    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    call ESMF_GridCompGet(gcomp, importState=importstate, clock=clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    ! use next time; the NUOPC clock is not updated until the end of the time interval
    call ESMF_ClockGetNextTime(clock, nextTime=nexttime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(nexttime,yy=yr, mm=mon, dd=day, s=sec, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(timestr,'(i4.4,a,i2.2,a,i2.2,a,i5.5)') yr,'-',mon,'-',day,'-',sec

    call ESMF_StateGet(importState, itemCount=fieldCount, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    allocate(lfieldNameList(fieldCount))
    call ESMF_StateGet(importState, itemNameList=lfieldNameList, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! assumes no scalar field is present in importState
    call ESMF_StateGet(importState, itemName=lfieldNameList(1), field=lfield, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    deallocate(lfieldNameList)

    call ESMF_FieldGet(lfield, mesh=lmesh, meshloc=meshloc, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    newfield = ESMF_FieldCreate(lmesh, ESMF_TYPEKIND_R8, meshloc=meshloc, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(newfield, farrayptr=dataPtr1d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    dataptr1d(:) = fillValue

    do n = 1, nseal
       isea = iaproc + (n-1)*naproc
       ix = mapsf(isea,1)
       iy = mapsf(isea,2)
       dataptr1d(n) = global_data(ix + (iy-1)*nx)
    end do

    call ESMF_FieldWrite(newfield, filename=trim(fldname)//'.'//trim(timestr)//'.nc', &
      variableName=trim(fldname), overwrite=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldDestroy(newfield, rc=rc, noGarbage=.true.)

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine check_globaldata
  !========================================================================
  subroutine readfromfile (idfld, wxdata, wydata, time0, timen, rc)

    use w3gdatmd, only: gtype, nx, ny
    use w3fldsmd, only: w3fldo, w3fldg

    ! input/output variables
    character(len=*) , intent(in)    :: idfld
    integer          , intent(in)    :: time0(2)
    integer          , intent(in)    :: timen(2)
    real(r4)         , intent(out)   :: wxdata(nx*ny)
    real(r4)         , intent(out)   :: wydata(nx*ny)
    integer, optional, intent(out)   :: rc

    ! local variables
    integer                     :: ierr, tw0l(2), twnl(2)
    integer                     :: ix, iy, n
    integer                     :: nxt, nyt, gtypet, filler(3), tideflag
    integer                     :: mdse = 6
    integer                     :: mdst = 10
    integer                     :: mdsf = 13   ! same as ndsf(3) in ww3_shel
    real                        :: wx0l(nx,ny), wy0l(nx,ny)
    real                        :: wxnl(nx,ny), wynl(nx,ny)
    real                        :: dt0l(nx,ny), dtnl(nx,ny)
    logical                     :: flagsc = .false.
    logical, save               :: firstCall = .true.
    character(len=13)           :: tsstr
    character(len=3)            :: tsfld, lstring
    character(CL)               :: logmsg
    character(len=*), parameter :: subname = '(wav_import_export:readfromfile)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    lstring = trim(idfld)
    if (firstCall) then
      ! open file
      call w3fldo('READ', lstring, mdsf, mdst, mdse, nx, ny, gtype, ierr)
      write(logmsg,*) "Opening "//lstring//", iostat = ", ierr
      call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_ERROR)
      if (ierr.ne.0) then
        write(logmsg,*) "Error in opening "//lstring//", iostat = ", ierr
        call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
      endif
      firstCall = .false.
    end if

    ! init variables
    wx0l = 0.0
    wy0l = 0.0
    dt0l = 0.0
    wxnl = 0.0
    wynl = 0.0
    dtnl = 0.0

    ! need to rewind to the begining of the file to access
    ! data of requested date correctly
    rewind(mdsf)

    ! read header information
    ! this was inside of w3fldo call but since we are opening file
    ! once and rewinding, the header need to be read
    read(mdsf, iostat=ierr) tsstr, tsfld, nxt, nyt, &
      gtypet, filler(1:2), tideflag

    ! read input
    call w3fldg('READ', lstring, mdsf, mdst, mdse, nx, ny, &
           nx, ny, time0, timen, tw0l, wx0l, wy0l, dt0l, twnl, &
           wxnl, wynl, dtnl, ierr, flagsc)

    n = 0
    wxdata(:) = 0.0_r4
    wydata(:) = 0.0_r4
    do iy = 1,NY
       do ix = 1,NX
          n = n + 1
          wxdata(n) = wx0l(ix,iy)
          wydata(n) = wy0l(ix,iy)
       end do
    end do

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine readfromfile
end module wav_import_export
