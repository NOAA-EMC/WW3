!> @file wav_import_export
!!
!> Manage the import/export state and fields
!!
!> @details Contains the public routines to advertise and realize
!! the import and export fields and the public routines to fill
!! the import and export fields within the ESMF States.
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 01-05-2022
module wav_import_export

  use ESMF
  use NUOPC
  use NUOPC_Model
  use wav_shr_flags
  use wav_kind_mod , only : r8 => shr_kind_r8, r4 => shr_kind_r4, i4 => shr_kind_i4
  use wav_kind_mod , only : CL => shr_kind_cl, CS => shr_kind_cs
  use wav_shr_mod  , only : ymd2date
  use wav_shr_mod  , only : chkerr
  use wav_shr_mod  , only : state_diagnose, state_reset, state_getfldptr, state_fldchk
  use wav_shr_mod  , only : wav_coupling_to_cice, nwav_elev_spectrum, merge_import, dbug_flag, multigrid, unstr_mesh
  use constants    , only : grav, tpi, dwat, dair
  use w3parall     , only : init_get_isea

  implicit none
  private ! except

  public  :: advertise_fields     !< @public create a list of fields and advertise them
  public  :: realize_fields       !< @public realize a list of advertised fields
  public  :: import_fields        !< @public fill WW3 fields using values in the import state
  public  :: export_fields        !< @public fill values in the export state using WW3 fields
  public  :: CalcRoughl           !< @public calculate the roughness length

  private :: fldlist_add          !< @private add a field name to a list of field names
  private :: fldlist_realize      !< @private realize a field in a list of field names
  private :: set_importmask       !< @private set the import mask when merge_import is true
  private :: check_globaldata     !< @private write values in a field to a netCDF file for debugging
  private :: readfromfile         !< @private read values from a file

  interface FillGlobalInput
    module procedure fillglobal_with_import
    module procedure fillglobal_with_merge_import
  end interface FillGlobalInput

  type fld_list_type                               !< @private a structure for the list of fields
    character(len=128) :: stdname                  !< a standard field name
    integer :: ungridded_lbound = 0                !< the ungridded dimension lower bound
    integer :: ungridded_ubound = 0                !< the ugridded dimension upper bound
  end type fld_list_type

  integer, parameter     :: fldsMax = 100           !< the maximum allowed number of fields in a state
  integer                :: fldsToWav_num = 0       !< initial value of the number of fields sent to the wave model
  integer                :: fldsFrWav_num = 0       !< initial value of the number of fields sent from the wave model
  type (fld_list_type)   :: fldsToWav(fldsMax)      !< a structure containing the list of fields to the wave model
  type (fld_list_type)   :: fldsFrWav(fldsMax)      !< a structure containing the list of fields from the wave model

  real(r4), allocatable  :: import_mask(:)          !< the mask for valid import data
  real(r8), parameter    :: zero  = 0.0_r8          !< a named constant

#ifdef W3_CESMCOUPLED
  logical :: cesmcoupled = .true.                   !< logical defining a CESM use case
#else
  logical :: cesmcoupled = .false.                  !< logical defining a non-CESM use case (UWM)
#endif
  integer, public    :: nseal_cpl                   !< the number of local sea points on a processor, exclusive
                                                    !! of the ghost points. For non-PDLIB cases, this is nseal
  character(*),parameter :: u_FILE_u = &            !< a character string for an ESMF log message
       __FILE__

  !===============================================================================
contains
  !===============================================================================
  !> Set up the list of exchanged field to be advertised
  !!
  !> @details Called by InitializAdvertise, a list of standard field names to or
  !! from the wave model is created and then advertised in either the import or
  !! export state. A field with name set by the configuration variable ScalarFieldName
  !! and size of ScalarFieldCount is added to the list of fields in the export state
  !! and is used by CMEPS to write mediator history and restart fields as 2D arrays
  !!
  !! @param       importState       an ESMF_State for the import
  !! @param       exportState       an ESMF_State for the export
  !! @param[in]   flds_scalar_name  the name of the scalar field
  !! @param[out]  rc                a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
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

    if (.not. unstr_mesh) then
      call fldlist_add(fldsFrWav_num, fldsFrWav, trim(flds_scalar_name))
    end if
    if (cesmcoupled) then
      call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_lamult' )
      call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_lasl' )
      call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_ustokes')
      call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_vstokes')
    else
      call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_z0')
    end if
    call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_pstokes_x', ungridded_lbound=1, ungridded_ubound=3)
    call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_pstokes_y', ungridded_lbound=1, ungridded_ubound=3)

    ! AA TODO: In the above fldlist_add calls, we are passing hardcoded ungridded_ubound values (3) because, USSPF(2)
    ! is not initialized yet. It is set during w3init which gets called at a later phase (realize). A permanent solution
    ! will be implemented soon based on receiving USSP and USSPF from the coupler instead of the mod_def file. This will
    ! also ensure compatibility with the ocean component since ocean will also receive these from the coupler.
    if (wav_coupling_to_cice) then
      call fldlist_add(fldsFrWav_num, fldsFrWav, 'Sw_elevation_spectrum', &
           ungridded_lbound=1, ungridded_ubound=nwav_elev_spectrum)
    end if

    do n = 1,fldsFrWav_num
      call NUOPC_Advertise(exportState, standardName=fldsFrWav(n)%stdname, &
           TransferOfferGeomObject='will provide', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine advertise_fields

  !===============================================================================
  !> Realize the advertised fields
  !!
  !> @details Called by InitializeRealize, realize the advertised fields on the mesh
  !! and set all initial values to zero
  !!
  !! @param       gcomp             an ESMF_GridComp object
  !! @param       mesh              an ESMF_Mesh object
  !! @param[in]   flds_scalar_name  the name of the scalar field
  !! @param[in]   flds_scalar_num   the number of scalar fields
  !! @param[out]  rc                a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
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
  !> Fill WW3 fields with values from the import state
  !!
  !> @details Called by ModelAdvance, a global field for each connected field is
  !! created in SetGlobalInput and used to fill the internal WW3 global variables in
  !! FillGlobalInput. Optionally, the  WW3 field can be created by merging with a
  !! provided field in cases where the WW3 model domain extends outside the source
  !! domain
  !!
  !! @param[inout]   gcomp   an ESMF_GridComp object
  !! @param[in]      time0   the starting time of ModelAdvance
  !! @param[in]      timen   the ending time of ModelAdvance
  !! @param[out]     rc      return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine import_fields( gcomp, time0, timen, rc )

    !---------------------------------------------------------------------------
    ! Obtain the wave input from the mediator
    !---------------------------------------------------------------------------

    use w3gdatmd    , only: nsea, nseal, MAPSTA, NX, NY, w3setg
    use w3idatmd    , only: CX0, CY0, CXN, CYN, DT0, DTN, ICEI, WLEV, INFLAGS1, ICEP1, ICEP5
    use w3idatmd    , only: TC0, TCN, TLN, TIN, TI1, TI5, TW0, TWN, WX0, WY0, WXN, WYN
    use w3idatmd    , only: UX0, UY0, UXN, UYN, TU0, TUN
    use w3idatmd    , only: tfn, w3seti
    use w3odatmd    , only: w3seto
    use w3wdatmd    , only: time, w3setw
#ifdef W3_CESMCOUPLED
    use w3idatmd    , only: HSL
#else
    use wmupdtmd    , only: wmupd2
    use wmmdatmd    , only: wmsetm
    use wmmdatmd    , only: mdse, mdst, nrgrd, inpmap
#ifdef W3_MPI
    use wmmdatmd    , only: mpi_comm_grd
#endif
#endif

    ! input/output variables
    type(ESMF_GridComp) , intent(inout) :: gcomp
    integer             , intent(in)    :: time0(2), timen(2)
    integer             , intent(out)   :: rc

    ! Local variables
    type(ESMF_State)        :: importState
    type(ESMF_VM)           :: vm
    type(ESMF_Clock)        :: clock
    real(r4)                :: global_data(nsea)
    real(r4), allocatable   :: global_data2(:)
    real(r4)                :: def_value
    character(len=10)       :: uwnd
    character(len=10)       :: vwnd
    integer                 :: imod, j, jmod
    integer                 :: mpi_comm_null = -1
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
    ! fill both the lower (0) and upper (N) bound data with the same values
    ! fill with special values as default, these should not be used in practice
    ! set time for input data to time0 and timen (shouldn't matter)

    def_value = 0.0_r4

#ifndef W3_CESMCOUPLED
    call w3setg ( 1, mdse, mdst )
    call w3seti ( 1, mdse, mdst )
#endif

    ! ---------------
    ! INFLAGS1(1)
    ! ---------------
    if (INFLAGS1(1)) then
      TLN  = timen

      WLEV(:,:) = def_value   ! water level
      if (state_fldchk(importState, 'So_h')) then
        call SetGlobalInput(importState, 'So_h', vm, global_data, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call FillGlobalInput(global_data, WLEV)
      end if
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
        call SetGlobalInput(importState, 'So_u', vm, global_data, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call FillGlobalInput(global_data, CX0)
        call FillGlobalInput(global_data, CXN)
      end if

      CY0(:,:) = def_value   ! ocn v current
      CYN(:,:) = def_value
      if (state_fldchk(importState, 'So_v')) then
        call SetGlobalInput(importState, 'So_v', vm, global_data, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call FillGlobalInput(global_data, CY0)
        call FillGlobalInput(global_data, CYN)
      end if
    end if

    ! ---------------
    ! INFLAGS1(3) - atm wind/temp fields
    ! ---------------
    if (INFLAGS1(3)) then
      TW0  = time0       ! times for atm wind/temp fields.
      TWN  = timen

      if (merge_import) then
        ! set mask using u-wind field if merge_import; assume all import fields
        ! will have same missing overlap region
        ! import_mask memory will be allocate in set_importmask
        call set_importmask(importState, clock, trim(uwnd), vm, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        allocate(wxdata(nsea))
        allocate(wydata(nsea))
        call readfromfile('WND', wxdata, wydata, time0, timen, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        if (dbug_flag > 10) then
          call check_globaldata(gcomp, 'wxdata', wxdata, nsea, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          call check_globaldata(gcomp, 'wydata', wydata, nsea, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          call check_globaldata(gcomp, 'import_mask', import_mask, nsea, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
        end if
      end if

      ! atm u wind
      WX0(:,:) = def_value
      WXN(:,:) = def_value
      if (state_fldchk(importState, trim(uwnd))) then
        call SetGlobalInput(importState, trim(uwnd), vm, global_data, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        if (merge_import) then
          call FillGlobalInput(global_data, import_mask, wxdata, WX0)
          call FillGlobalInput(global_data, import_mask, wxdata, WXN)
          if (dbug_flag > 10) then
            call check_globaldata(gcomp, 'wx0', wx0, nx*ny, rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return
          end if
        else
          call FillGlobalInput(global_data, WX0)
          call FillGlobalInput(global_data, WXN)
        end if
      end if

      ! atm v wind
      WY0(:,:) = def_value
      WYN(:,:) = def_value
      if (state_fldchk(importState, trim(vwnd))) then
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call SetGlobalInput(importState, trim(vwnd), vm, global_data, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        if (merge_import) then
          call FillGlobalInput(global_data, import_mask, wydata, WY0)
          call FillGlobalInput(global_data, import_mask, wydata, WYN)
          if (dbug_flag > 10) then
            call check_globaldata(gcomp, 'wy0', wy0, nx*ny, rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return
          end if
        else
          call FillGlobalInput(global_data, WY0)
          call FillGlobalInput(global_data, WYN)
        end if
      end if

      ! air temp - ocn temp
      DT0(:,:) = def_value
      DTN(:,:) = def_value
      if ((state_fldchk(importState, 'So_t')) .and. (state_fldchk(importState, 'Sa_tbot'))) then
        allocate(global_data2(nsea))
        call SetGlobalInput(importState, 'Sa_tbot', vm, global_data, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call SetGlobalInput(importState, 'So_t', vm, global_data2, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        ! So_tbot - So_t
        global_data = global_data - global_data2
        call FillGlobalInput(global_data, DT0)
        call FillGlobalInput(global_data, DTN)
        deallocate(global_data2)
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
        call SetGlobalInput(importState, 'Si_ifrac', vm, global_data, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call FillGlobalInput(global_data, ICEI)
      end if
    end if
#ifdef W3_CESMCOUPLED
    ! ---------------
    ! ocean boundary layer depth - always assume that this is being imported for CESM
    ! ---------------
    call SetGlobalInput(importState, 'So_bldepth', vm, global_data, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! ocn mixing layer depth
    global_data = max(global_data, 5.)*0.2
    call FillGlobalInput(global_data, HSL)
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
        call SetGlobalInput(importState, 'Faxa_taux', vm, global_data, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call FillGlobalInput(global_data, UX0)
        call FillGlobalInput(global_data, UXN)
      end if

      UY0(:,:) = def_value   ! atm v momentum
      UYN(:,:) = def_value
      if (state_fldchk(importState, 'Faxa_tauy')) then
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call SetGlobalInput(importState, 'Faxa_tauy', vm, global_data, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call FillGlobalInput(global_data, UY0)
        call FillGlobalInput(global_data, UYN)
      end if
    end if
    ! ---------------
    ! INFLAGS1(-7)
    ! ---------------
    if (INFLAGS1(-7)) then
      TI1  = timen       ! time for ice field
      ICEP1(:,:) = def_value   ! ice thickness
      if (state_fldchk(importState, 'Si_thick')) then
        call SetGlobalInput(importState, 'Si_thick', vm, global_data, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call FillGlobalInput(global_data, ICEP1)
      end if
    end if
    ! ---------------
    ! INFLAGS1(-3)
    ! ---------------
    if (INFLAGS1(-3)) then
      TI5  = timen        ! time for ice field
      ICEP5(:,:) = def_value   ! ice floe size
      if (state_fldchk(importState, 'Si_floediam')) then
        call SetGlobalInput(importState, 'Si_floediam', vm, global_data, rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call FillGlobalInput(global_data, ICEP5)
      end if
    end if

#ifndef W3_CESMCOUPLED
    if (multigrid) then
      do j = lbound(inflags1,1),ubound(inflags1,1)
        if (inflags1(j)) then
          do imod = 1,nrgrd
            tfn(:,j) = timen(:)
            call w3setg ( imod, mdse, mdst )
            call w3setw ( imod, mdse, mdst )
            call w3seti ( imod, mdse, mdst )
            call w3seto ( imod, mdse, mdst )
            call wmsetm ( imod, mdse, mdst )
#ifdef W3_MPI
            if ( mpi_comm_grd .eq. mpi_comm_null ) cycle
#endif
            !TODO: when is this active? jmod = -999
            jmod = inpmap(imod,j)
            if ( jmod.lt.0 .and. jmod.ne.-999 ) then
              call wmupd2( imod, j, jmod, rc )
              if (ChkErr(rc,__LINE__,u_FILE_u)) return
            endif
          end do
        end if
      end do
    end if
#endif
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine import_fields

  !===============================================================================
  !> Fill the export state with values from WW3 fields
  !!
  !> @details Called by ModelAdvance, fill or compute the values in the export state.
  !!
  !! @param          gcomp   an ESMF_GridComp object
  !! @param[out]     rc      return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine export_fields (gcomp, rc)

    !---------------------------------------------------------------------------
    ! Create the export state
    !---------------------------------------------------------------------------

    use wav_kind_mod,   only : R8 => SHR_KIND_R8
    use w3adatmd      , only : USSX, USSY, USSP
    use w3adatmd      , only : w3seta
    use w3idatmd      , only : w3seti
    use w3wdatmd      , only : va, w3setw
    use w3odatmd      , only : w3seto, naproc, iaproc
    use w3gdatmd      , only : nseal, mapsf, MAPSTA, USSPF, NK, w3setg
    use w3iogomd      , only : CALC_U3STOKES
#ifdef W3_CESMCOUPLED
    use w3wdatmd      , only : ASF, UST
    use w3adatmd      , only : USSHX, USSHY, UD, HS
    use w3idatmd      , only : HSL
#else
    use wmmdatmd      , only : mdse, mdst, wmsetm
#endif

    ! input/output/variables
    type(ESMF_GridComp)            :: gcomp
    integer          , intent(out) :: rc

    ! Local variables
#ifdef W3_CESMCOUPLED
    real(R8)          :: fillvalue = 1.0e30_R8                 ! special missing value
    real              :: sww, langmt, lasl, laslpj, alphal
#else
    real(R8)          :: fillvalue = zero                      ! special missing value
#endif
    type(ESMF_State)  :: exportState
    integer           :: n, jsea, isea, ix, iy, ib

    real(r8), pointer :: z0rlen(:)
    real(r8), pointer :: charno(:)
    real(r8), pointer :: wbcuru(:)
    real(r8), pointer :: wbcurv(:)
    real(r8), pointer :: wbcurp(:)
    real(r8), pointer :: sxxn(:)
    real(r8), pointer :: sxyn(:)
    real(r8), pointer :: syyn(:)

    real(r8), pointer :: sw_lamult(:)
    real(r8), pointer :: sw_lasl(:)
    real(r8), pointer :: sw_ustokes(:)
    real(r8), pointer :: sw_vstokes(:)

    ! d2 is location, d1 is frequency  - nwav_elev_spectrum frequencies will be used
    real(r8), pointer :: wave_elevation_spectrum(:,:)

    ! Partitioned stokes drift
    real(r8), pointer :: sw_pstokes_x(:,:)
    real(r8), pointer :: sw_pstokes_y(:,:)
    character(len=*), parameter :: subname='(wav_import_export:export_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    ! Get export state
    call NUOPC_ModelGet(gcomp, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

#ifndef W3_CESMCOUPLED
    call w3setg ( 1, mdse, mdst )
    call w3setw ( 1, mdse, mdst )
    call w3seta ( 1, mdse, mdst )
    call w3seti ( 1, mdse, mdst )
    call w3seto ( 1, mdse, mdst )
    if (multigrid) then
      call wmsetm ( 1, mdse, mdst )
    end if
#else
    if (state_fldchk(exportState, 'Sw_lamult')) then
      call state_getfldptr(exportState, 'Sw_lamult', sw_lamult, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      sw_lamult(:) = fillvalue
      do jsea=1, nseal_cpl
        call init_get_isea(isea, jsea)
        ix  = mapsf(isea,1)
        iy  = mapsf(isea,2)
        if (mapsta(iy,ix) == 1 .and. HS(jsea) > zero .and. &
            sqrt(USSX(jsea)**2+USSY(jsea)**2)>zero .and. sqrt(USSHX(jsea)**2+USSHY(jsea)**2)>zero ) then
           sww = atan2(USSHY(jsea),USSHX(jsea)) - UD(isea)
           alphal = atan( sin(sww) / (                                       &
                          2.5 * UST(isea)*ASF(isea)*sqrt(dair/dwat)          &
                        / max(1.e-14_r8, sqrt(USSX(jsea)**2+USSY(jsea)**2))     &
                        * log(max(1.0, abs(1.25*HSL(ix,iy)/HS(jsea))))       &
                        + cos(sww)   )                                       &
                        )
           lasl = sqrt(ust(isea) * asf(isea) * sqrt(dair/dwat) &
                                 / sqrt(usshx(jsea)**2 + usshy(jsea)**2 ))
           laslpj = lasl * sqrt(abs(cos(alphal)) &
               / abs(cos(sww-alphal)))
           sw_lamult(jsea) = min(5.0, abs(cos(alphal)) * &
                              sqrt(1.0+(1.5*laslpj)**(-2)+(5.4_r8*laslpj)**(-4)))
        else
          sw_lamult(jsea)  = 1.
        endif
      enddo
    end if
    if (state_fldchk(exportState, 'Sw_lasl')) then
      call state_getfldptr(exportState, 'Sw_lasl', sw_lasl, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      sw_lasl(:) = fillvalue
      do jsea=1, nseal
         isea = iaproc + (jsea-1)*naproc
         ix  = mapsf(isea,1)
         iy  = mapsf(isea,2)
         if (mapsta(iy,ix) == 1) then
            ! note: an arbitrary minimum value of 0.2 is set to avoid zero
            !       Langmuir number which may result from zero surface friction
            !       velocity but may cause unphysically strong Langmuir mixing
            sw_lasl(jsea) = max(0.2, sqrt(UST(isea)*ASF(isea)*sqrt(dair/dwat) &
                          / max(1.e-14, sqrt(USSHX(jsea)**2+USSHY(jsea)**2))))
         else
            sw_lasl(jsea)  = 1.e6
         endif
      enddo
    end if
#endif
    ! surface stokes drift
    if (state_fldchk(exportState, 'Sw_ustokes')) then
      call state_getfldptr(exportState, 'Sw_ustokes', sw_ustokes, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      sw_ustokes(:) = fillvalue
      do jsea=1, nseal_cpl
        call init_get_isea(isea, jsea)
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
      call state_getfldptr(exportState, 'Sw_vstokes', sw_vstokes, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      sw_vstokes(:) = fillvalue
      do jsea=1, nseal_cpl
        call init_get_isea(isea, jsea)
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
      call state_getfldptr(exportState, 'charno', charno, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call CalcCharnk(charno)
    endif

    if (state_fldchk(exportState, 'Sw_z0')) then
      call state_getfldptr(exportState, 'Sw_z0', z0rlen, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call CalcRoughl(z0rlen)
    endif

    if ( state_fldchk(exportState, 'wbcuru') .and. &
         state_fldchk(exportState, 'wbcurv') .and. &
         state_fldchk(exportState, 'wbcurp')) then
      call state_getfldptr(exportState, 'wbcuru', wbcuru, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call state_getfldptr(exportState, 'wbcurv', wbcurv, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call state_getfldptr(exportState, 'wbcurp', wbcurp, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call CalcBotcur( va, wbcuru, wbcurv, wbcurp)
    end if

    if ( state_fldchk(exportState, 'wavsuu') .and. &
         state_fldchk(exportState, 'wavsuv') .and. &
         state_fldchk(exportState, 'wavsvv')) then
      call state_getfldptr(exportState, 'sxxn', sxxn, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call state_getfldptr(exportState, 'sxyn', sxyn, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call state_getfldptr(exportState, 'syyn', syyn, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call CalcRadstr2D( va, sxxn, sxyn, syyn)
    end if
    if (wav_coupling_to_cice) then
      call state_getfldptr(exportState, 'Sw_elevation_spectrum', wave_elevation_spectrum, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      ! Initialize wave elevation spectrum
      wave_elevation_spectrum(:,:) = fillvalue
      call CalcEF(va, wave_elevation_spectrum)
    end if

    if ( state_fldchk(exportState, 'Sw_pstokes_x') .and. &
         state_fldchk(exportState, 'Sw_pstokes_y') )then
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call state_getfldptr(exportState, 'Sw_pstokes_x', sw_pstokes_x, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call state_getfldptr(exportState, 'Sw_pstokes_y', sw_pstokes_y, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      sw_pstokes_x(:,:) = fillvalue
      sw_pstokes_y(:,:) = fillvalue
      if (USSPF(1) > 0) then ! Partitioned Stokes drift computation is turned on in mod_def file.
        call CALC_U3STOKES(va, 2)
        do ib = 1, USSPF(2)
          do jsea = 1, nseal_cpl
            call init_get_isea(isea, jsea)
            ix  = mapsf(isea,1)
            iy  = mapsf(isea,2)
            sw_pstokes_x(ib,jsea) = ussp(jsea,ib)
            sw_pstokes_y(ib,jsea) = ussp(jsea,nk+ib)
          enddo
        end do
      end if
    endif

    if (dbug_flag > 5) then
      call state_diagnose(exportState, 'at export ', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine export_fields

  !===============================================================================
  !> Add a fieldname to a list of fields in a state
  !!
  !! @param[inout]    num                a counter for added fields
  !! @param[inout]    fldlist            a structure for the standard name and ungridded dims
  !! @param[in]       stdname            a standard field name
  !! @param[in]       ungridded_lbound   the lower bound of an ungridded dimension
  !! @param[in]       ungridded_ubound   the upper bound of an ungridded dimension
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
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
  !> Realize a list of fields in a state
  !!
  !> @details For a connected field in a State, create an ESMF_Field object of
  !! the required dimensionality on the ESMF_Mesh. Remove any unconnected fields from
  !! the State. For a scalar field, create a field of dimensionality (1:flds_scalar_num)
  !!
  !! @param[inout]   state               an ESMF_State object
  !! @param[in]      fldlist             a list of fields in the State
  !! @param[in]      numflds             the number of fields in the state
  !! @param[in]      flds_scalar_name    the name of the scalar field
  !! @param[in]      flds_scalar_num     the count of scalar fields
  !! @param[in]      tag                 a character string for logging
  !! @param[in]      mesh                an ESMF_Mesh object
  !! @param[inout]   rc                  a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine fldlist_realize(state, fldList, numflds, flds_scalar_name, flds_scalar_num, mesh, tag, rc)

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
    !> Create a field with scalar data on the root pe
    !!
    !! @param[inout]   field               an ESMF_Field
    !! @param[in]      flds_scalar_name    the scalar field name
    !! @param[in[      flds_scalar_num     the dimnsionality of the scalar field
    !! @param[inout]   rc                  a return code
    !!
    !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
    !> @date 01-05-2022
    subroutine SetScalarField(field, flds_scalar_name, flds_scalar_num, rc)
      ! ----------------------------------------------
      ! create a field with scalar data on the root pe
      ! ----------------------------------------------

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
  !> Calculate Charnok parameter for export
  !!
  !> @details TODO:
  !!
  !! @param[inout] chkn             a 1-D pointer to a field on a mesh
  !!
  !> @author T. J. Campbell, NRL
  !> @date 09-Aug-2017
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
    real(ESMF_KIND_R8), pointer :: chkn(:)  ! 1D Charnock export field pointer

    ! local variables
    integer            :: isea, jsea, ix, iy
    real               :: emean, fmean, fmean1, wnmean, amax, ustar, ustdr
    real               :: tauwx, tauwy, cd, z0, fmeanws, dlwmean
    logical            :: llws(nspec)
    logical, save      :: firstCall = .true.
    !----------------------------------------------------------------------

    !TODO: fix firstCall like for Roughl
    jsea_loop: do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      if ( firstCall ) then
        charn(jsea) = zero
        llws(:) = .true.
        ustar = zero
        ustdr = zero
#ifdef W3_ST3
        call w3spr3( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea), emean, fmean, fmean1, wnmean, &
             amax, u10(isea), u10d(isea), ustar, ustdr, tauwx, tauwy, cd, z0, charn(jsea),   &
             llws, fmeanws )
#endif
#ifdef W3_ST4
        call w3spr4( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea), emean, fmean, fmean1, wnmean, &
             amax, u10(isea), u10d(isea), ustar, ustdr, tauwx, tauwy, cd, z0, charn(jsea),   &
             llws, fmeanws, dlwmean )
#endif
      endif !firstCall
      chkn(jsea) = charn(jsea)
    enddo jsea_loop

    firstCall = .false.

  end subroutine CalcCharnk

  !===============================================================================
  !> Calculate wave roughness length for export
  !!
  !> @details TODO:
  !!
  !! @param[inout] wrln             a 1-D pointer to a field on a mesh
  !!
  !> @author T. J. Campbell, NRL
  !> @date 09-Aug-2017
  subroutine CalcRoughl ( wrln)

    ! Calculate wave roughness length for export

    use w3gdatmd,   only : nseal, nk, nth, sig, dmin, ecos, esin, dden, mapsf, mapsta, nspec
    use w3adatmd,   only : dw, cg, wn, charn, u10, u10d
    use w3wdatmd,   only : va, ust
    use w3odatmd,   only : naproc, iaproc, runtype
#ifdef W3_ST3
    use w3src3md,   only : w3spr3
#endif
#ifdef W3_ST4
    use w3src4md,   only : w3spr4
#endif

    ! input/output variables
    real(r8), pointer :: wrln(:) ! 1D roughness length export field ponter

    ! local variables
    integer       :: isea, jsea, ix, iy
    real          :: emean, fmean, fmean1, wnmean, amax, ustar, ustdr
    real          :: tauwx, tauwy, cd, z0, fmeanws, dlwmean
    logical       :: llws(nspec)
    logical, save :: firstCall = .true.

    !----------------------------------------------------------------------

    jsea_loop: do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
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
          call w3spr3( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea), emean, fmean, fmean1, wnmean, &
               amax, u10(isea), u10d(isea), ustar, ustdr, tauwx, tauwy, cd, z0, charn(jsea),   &
               llws, fmeanws )
#endif
#ifdef W3_ST4
          call w3spr4( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea), emean, fmean, fmean1, wnmean, &
               amax, u10(isea), u10d(isea), ustar, ustdr, tauwx, tauwy, cd, z0, charn(jsea),   &
               llws, fmeanws, dlwmean )
#endif
        end if
      endif !firstCall
      wrln(jsea) = charn(jsea)*ust(isea)**2/grav
    enddo jsea_loop

    firstCall = .false.

  end subroutine CalcRoughl

  !===============================================================================
  !> Calculate wave-bottom currents for export
  !!
  !> @details TODO:
  !!
  !! @param[in] a                    input spectra
  !! @param     wbxn                 a 1-D pointer to a field on a mesh
  !! @param     wbyn                 a 1-D pointer to a field on a mesh
  !! @param     wbpn                 a 1-D pointer to a field on a mesh
  !!
  !> @author T. J. Campbell, NRL
  !> @date 09-Aug-2017
  subroutine CalcBotcur ( a, wbxn, wbyn, wbpn )

    ! Calculate wave-bottom currents for export

    use w3gdatmd,  only : nseal, nk, nth, sig, dmin, ecos, esin, dden, mapsf, mapsta, nspec
    use w3adatmd,  only : dw, cg, wn
    use w3odatmd,  only : naproc, iaproc

    ! input/output variables
    real, intent(in)            :: a(nth,nk,0:nseal) ! Input spectra (in par list to change shape)
    real(ESMF_KIND_R8), pointer :: wbxn(:)           ! eastward-component export field pointer
    real(ESMF_KIND_R8), pointer :: wbyn(:)           ! northward-component export field pointer
    real(ESMF_KIND_R8), pointer :: wbpn(:)           ! period export field pointer

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

    jsea_loop: do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
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
  !> Calculate radiation stresses for export
  !!
  !> @details TODO:
  !!
  !! @param[in] a                    input spectra
  !! @param     sxxn                 a 1-D pointer to a field on a mesh
  !! @param     sxyn                 a 1-D pointer to a field on a mesh
  !! @param     syyn                 a 1-D pointer to a field on a mesh
  !!
  !> @author T. J. Campbell, NRL
  !> @date 09-Aug-2017
  subroutine CalcRadstr2D ( a, sxxn, sxyn, syyn )

    ! Calculate radiation stresses for export

    use w3gdatmd,   only : nseal, nk, nth, sig, es2, esc, ec2, fte, dden
    use w3adatmd,   only : dw, cg, wn
    use w3odatmd,   only : naproc, iaproc

    ! input/output variables
    real, intent(in)               :: a(nth,nk,0:nseal) ! Input spectra (in par list to change shape)
    real(ESMF_KIND_R8), pointer    :: sxxn(:)           ! eastward-component export field
    real(ESMF_KIND_R8), pointer    :: sxyn(:)           ! eastward-northward-component export field
    real(ESMF_KIND_R8), pointer    :: syyn(:)           ! northward-component export field

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
    jsea_loop: do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
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

  !===============================================================================
  !> Calculate wave elevation spectrum for export
  !!
  !> @details Calculates wave elevation spectrum independently of w3iogomd to ensure
  !! that EF field sent to sea ice component is updated at the coupling frequency
  !!
  !! @param[in]    a                         input spectra
  !! @param[inout] wave_elevation_spectrum   a 2-D pointer to a field on a mesh
  !!
  !> @author Denise.Worthen@noaa.gov
  !> @date 10-28-2022
  subroutine CalcEF (a, wave_elevation_spectrum)

    use constants, only : tpi
    use w3gdatmd,  only : nth, nk, nseal, mapsf, mapsta, dden, dsii
    use w3adatmd,  only : nsealm, cg
    use w3parall,  only : init_get_isea

    ! input/output variables
    real, intent(in)     :: a(nth,nk,0:nseal)
    real(r8), pointer    :: wave_elevation_spectrum(:,:)

    ! local variables
    real    :: ab(nseal)
    real    :: ebd, factor
    integer :: ik, ith, isea, jsea, ix, iy

    do ik = 1,nwav_elev_spectrum
      ab = 0.0
      do ith = 1, nth
        do jsea = 1,nseal_cpl
          ab(jsea) = ab(jsea) + a(ith,ik,jsea)
        end do
      end do

      do jsea = 1,nseal_cpl
        call init_get_isea(isea, jsea)
        ix  = mapsf(isea,1)                   ! global ix
        iy  = mapsf(isea,2)                   ! global iy
        if (mapsta(iy,ix) == 1) then          ! active sea point
          factor = dden(ik) / cg(ik,isea)
          ebd = ab(jsea) * factor
          ebd = ebd / dsii(ik)
          wave_elevation_spectrum(ik,jsea) = ebd * tpi
        else
          wave_elevation_spectrum(ik,jsea) = 0.
        end if
      end do
    end do

  end subroutine CalcEF

  !====================================================================================
  !> Create a global field across all PEs
  !!
  !> @details Distributes the global values of the named import state field to all PEs
  !! using a global reduce across all PEs.
  !!
  !! @param[in]    importstate        the import state
  !! @param[in]    fldname            the field name
  !! @param[in]    vm                 the ESMF VM object
  !! @param[out]   global_output      the global nsea values
  !! @param[out]   rc                 a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine SetGlobalInput(importState, fldname, vm, global_output, rc)

    use w3gdatmd, only: nsea, nseal, nx, ny
    use w3odatmd, only: naproc, iaproc

    ! input/output variables
    type(ESMF_State) , intent(in)  :: importState
    character(len=*) , intent(in)  :: fldname
    type(ESMF_VM)    , intent(in)  :: vm
    real(r4)         , intent(out) :: global_output(nsea)
    integer          , intent(out) :: rc

    ! local variables
    integer           :: jsea, isea, ix, iy
    real(r4)          :: global_input(nsea)
    real(r8), pointer :: dataptr(:)
    character(len=*), parameter :: subname = '(wav_import_export:setGlobalInput)'

    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    call state_getfldptr(importState, trim(fldname), dataptr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    global_output(:) = 0._r4
    global_input(:) = 0._r4
    do jsea = 1, nseal_cpl
      call init_get_isea(isea, jsea)
      global_input(isea) = real(dataptr(jsea),4)
    end do
    call ESMF_VMAllReduce(vm, sendData=global_input, recvData=global_output, count=nsea, reduceflag=ESMF_REDUCE_SUM, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine SetGlobalInput

  !====================================================================================
  !> Fill a global field with import state values at nsea points
  !!
  !> @details Fills a global field on all points from the values at all sea points
  !!
  !! @param[in]    global_data        values of a global field on nsea points
  !! @param[inout] globalfield        values of a global field on all points
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine fillglobal_with_import(global_data, globalfield)

    use w3gdatmd, only: nsea, mapsf, nx, ny

    real(r4), intent(in)    :: global_data(nsea)
    real(r4), intent(inout) :: globalfield(nx,ny)

    ! local variables
    integer           :: isea, ix, iy

    do isea = 1,nsea
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      globalfield(ix,iy) = global_data(isea)
    end do

  end subroutine fillglobal_with_import

  !====================================================================================
  !> Fill a global field by merging
  !!
  !> @details Merges the global import field values on sea points with values from a file
  !! using a provided mask
  !!
  !! @param[in]    global_data        values of a global field on nsea points
  !! @param[in]    global_mask        values of a global mask
  !! @param[in]    filedata           values of a global field from a file
  !! @param[inout] globalfield        values of a global field on all points
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine fillglobal_with_merge_import(global_data, global_mask, filedata, globalfield)

    use w3gdatmd, only: nsea, mapsf, nx, ny

    real(r4), intent(in)    :: global_data(nsea)
    real(r4), intent(in)    :: global_mask(nsea)
    real(r4), intent(in)    :: filedata(nsea)
    real(r4), intent(inout) :: globalfield(nx,ny)

    ! local variables
    integer           :: isea, ix, iy

    do isea = 1,nsea
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      globalfield(ix,iy) = global_data(isea)*global_mask(isea) + (1.0_r4 - global_mask(isea))*filedata(isea)
    end do

  end subroutine fillglobal_with_merge_import

  !====================================================================================
  !> Obtain the import mask used to merge a field from the import state with values from
  !! a file
  !!
  !> @details Set the import mask for merging an import state field with values from
  !! a file. The import mask is set 0 where the field from the import state has a value
  !! of fillValue due to non-overlapping model domains. The field values read from a
  !! file will be used to provide the values in these regions. The values of the import
  !! mask are set initially (on the first ModelAdvance) to be 0 everywhere. In this case
  !! there are no valid import state values and only the values read from the file are
  !! used. At the second ModelAdvance, the import state contains valid values and the
  !! import mask can be set according the regions where the import state contains the
  !! fillValue. The import mask is fixed in time after the second ModelAdvance.
  !!
  !! @param[in]    importState     an ESMF_State object for import fields
  !! @param[in]    clock           an ESMF_Clock object
  !! @param[in]    fldname         a field name
  !! @param[in]    vm              an ESMF_VM object
  !! @param[out]   rc              return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine set_importmask(importState, clock, fldname, vm, rc)

    use w3gdatmd, only: nsea, nseal, nx, ny
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
    integer                 :: isea, jsea, ix, iy
    real(r8), pointer       :: dataptr(:)
    real(r4)                :: mask_local(nsea)
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
      allocate(import_mask(nsea))
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
    ! import_mask will be 1 where valid import exists and 0 where no valid import exists
    if (secondCall) then
      call ESMF_ClockPrint(clock, options='currTime', preString='Setting new import_mask at currTime : ', &
           unit=msgString, rc=rc)
      call state_getfldptr(importState, trim(fldname), dataptr, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      import_mask(:) = 0.0_r4
      mask_local(:) = 1.0_r4
      do jsea = 1, nseal
        isea = iaproc + (jsea-1)*naproc
        if (real(dataptr(jsea),4) .ge. fillValue) then
          mask_local(isea) = 0.0_r4
        end if
      end do
      call ESMF_VMAllReduce(vm, sendData=mask_local, recvData=import_mask, count=nsea, reduceflag=ESMF_REDUCE_MIN, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine set_importmask

  !====================================================================================
  !> Write a netCDF file containing the global field values for debugging
  !!
  !> @details Write a time-stamped netCDF file containing the values of a global field,
  !! where the global_field is provided on either on all points or only nsea points. In
  !! either case, the field will be written to the file on the mesh.
  !!
  !! @param[in]    gcomp           an ESMF_GridComp object
  !! @param[in]    fldname         a field name
  !! @param[in]    global_data     a global field
  !! @param[in]    nvals           the dimension of global_data
  !! @param[out]   rc              return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine check_globaldata(gcomp, fldname, global_data, nvals, rc)

    use w3gdatmd, only: nseal, nsea, mapsf, nx, ny
    use w3odatmd, only: naproc, iaproc

    ! input/output variables
    type(ESMF_GridComp) , intent(inout) :: gcomp
    character(len=*)    , intent(in)    :: fldname
    integer             , intent(in)    :: nvals
    real(r4)            , intent(in)    :: global_data(nvals)
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
    integer                         :: jsea, isea, ix, iy
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

    !TODO: get working for unstr
    if (nvals .eq. nx*ny) then
      do jsea = 1, nseal
        isea = iaproc + (jsea-1)*naproc
        ix = mapsf(isea,1)
        iy = mapsf(isea,2)
        dataptr1d(jsea) = global_data(ix + (iy-1)*nx)
      end do
    else
      do jsea = 1,nseal
        isea = iaproc + (jsea-1)*naproc
        dataptr1d(jsea) = global_data(isea)
      end do
    end if

    call ESMF_FieldWrite(newfield, filename=trim(fldname)//'.'//trim(timestr)//'.nc', &
         variableName=trim(fldname), overwrite=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldDestroy(newfield, rc=rc, noGarbage=.true.)

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine check_globaldata

  !========================================================================
  !> Read input from a file
  !!
  !> @details Obtain values from a file to fill an import state within a
  !! non-overlapped region of the wave domain
  !!
  !! @param[in]  idfld                  a file name to read
  !! @param[in]  time0                  the initial time
  !! @param[in]  timen                  the ending time
  !! @param[out] wxdata                 a 1-D pointer to a zonal wind field
  !! @param[out] wydata                 a 1-D pointer to a meridional wind field
  !! @param[out] rc                     a return code
  !!
  !> @author U. Turuncoglu, NCAR
  !> @date 18-May-2021
  subroutine readfromfile (idfld, wxdata, wydata, time0, timen, rc)

    use w3gdatmd, only: nsea, mapsf, gtype, nx, ny
    use w3fldsmd, only: w3fldo, w3fldg

    ! input/output variables
    character(len=*) , intent(in)    :: idfld
    integer          , intent(in)    :: time0(2)
    integer          , intent(in)    :: timen(2)
    real(r4)         , intent(out)   :: wxdata(nsea)
    real(r4)         , intent(out)   :: wydata(nsea)
    integer, optional, intent(out)   :: rc

    ! local variables
    integer                     :: ierr, tw0l(2), twnl(2)
    integer                     :: ix, iy, isea
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
      call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO)
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
    read(mdsf, iostat=ierr) tsstr, tsfld, nxt, nyt, gtypet, filler(1:2), tideflag

    ! read input
    call w3fldg('READ', lstring, mdsf, mdst, mdse, nx, ny, nx, ny, time0, timen, tw0l, wx0l, wy0l, &
         dt0l, twnl, wxnl, wynl, dtnl, ierr, flagsc)

    wxdata(:) = 0.0_r4
    wydata(:) = 0.0_r4
    do isea = 1,nsea
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      wxdata(isea) = wx0l(ix,iy)
      wydata(isea) = wy0l(ix,iy)
    end do

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine readfromfile
end module wav_import_export
