!> @file wav_comp_nuopc
!!
!> A NUOPC interface for WAVEWATCH III using the CMEPS mediator
!!
!> @details This module contains the base functionality of a mesh-based
!! NUOPC cap for WW3. It contains the only public entry point, SetServices
!! which registers all of the user-provided subroutines accessed by the NUOPC
!! layer. These include the user-routines to advertise the standard names of the
!! import and export fields (InitializeAdvertise), initialize the Wave model and
!! and realize the required fields within the import and export States on an
!! ESMF Mesh (InitializeRealize), fill the export State with initial values
!! (DataInitialize), advance the model one timestep (ModelAdvance), manage the
!! component clock (ModelSetRunClock), and finalize the component model at the
!! (ModelFinalize).
!!
!! The module wav_import_export includes the public routines to advertise and
!! realize the import and export fields called during the InitializeAdvertise and
!! InitializRealize phases, respectively and to fill the import and export states
!! during the ModelAdvance phase.
!!
!! The module wav_shr_mod contains public routines to access basic ESMF functions
!! and reduce code duplication.
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 01-05-2022
module wav_comp_nuopc

  use ESMF
  use NUOPC                 , only : NUOPC_CompDerive, NUOPC_CompSetEntryPoint, NUOPC_CompSpecialize
  use NUOPC                 , only : NUOPC_CompFilterPhaseMap, NUOPC_IsUpdated, NUOPC_IsAtTime
  use NUOPC                 , only : NUOPC_CompAttributeGet, NUOPC_Advertise
  use NUOPC                 , only : NUOPC_SetAttribute, NUOPC_CompAttributeGet, NUOPC_CompAttributeSet
  use NUOPC_Model           , only : model_routine_SS           => SetServices
  use NUOPC_Model           , only : model_label_Advance        => label_Advance
  use NUOPC_Model           , only : model_label_DataInitialize => label_DataInitialize
  use NUOPC_Model           , only : model_label_SetRunClock    => label_SetRunClock
  use NUOPC_Model           , only : model_label_Finalize       => label_Finalize
  use NUOPC_Model           , only : NUOPC_ModelGet, SetVM
  use wav_kind_mod          , only : r8=>shr_kind_r8, i8=>shr_kind_i8, i4=>shr_kind_i4
  use wav_kind_mod          , only : cl=>shr_kind_cl, cs=>shr_kind_cs
  use wav_import_export     , only : advertise_fields, realize_fields, nseal_cpl
  use wav_shr_mod           , only : state_diagnose, state_getfldptr, state_fldchk
  use wav_shr_mod           , only : chkerr, state_setscalar, state_getscalar, alarmInit, ymd2date
  use wav_shr_mod           , only : wav_coupling_to_cice, nwav_elev_spectrum
  use wav_shr_mod           , only : merge_import, dbug_flag
  use w3odatmd              , only : nds, iaproc, napout
  use w3odatmd              , only : runtype, use_user_histname, user_histfname, use_user_restname, user_restfname
  use w3odatmd              , only : user_netcdf_grdout
  use w3odatmd              , only : time_origin, calendar_name, elapsed_secs
  use wav_shr_mod           , only : casename, multigrid, inst_suffix, inst_index, unstr_mesh
  use wav_wrapper_mod       , only : ufs_settimer, ufs_logtimer, ufs_file_setlogunit, wtime
#ifndef W3_CESMCOUPLED
  use wmwavemd              , only : wmwave
  use wmupdtmd              , only : wmupd2
  use wmmdatmd              , only : mdse, mdst, nrgrd, improc, nmproc, wmsetm, stime, etime
  use wmmdatmd              , only : nmpscr
  use w3updtmd              , only : w3uini
  use w3adatmd              , only : flcold, fliwnd
#endif
  use constants             , only : is_esmf_component

  implicit none
  private ! except

  public  :: SetServices
  public  :: SetVM
  private :: InitializeP0
  private :: InitializeAdvertise
  private :: InitializeRealize
  private :: ModelSetRunClock
  private :: ModelAdvance
  private :: ModelFinalize

  include "mpif.h"

  !--------------------------------------------------------------------------
  ! Private module data
  !--------------------------------------------------------------------------

  character(len=CL)       :: flds_scalar_name = ''         !< the default scalar field name
  integer                 :: flds_scalar_num = 0           !< the default number of scalar fields
  integer                 :: flds_scalar_index_nx = 0      !< the default size of the scalar field nx
  integer                 :: flds_scalar_index_ny = 0      !< the default size of the scalar field ny
  logical                 :: profile_memory = .false.      !< default logical to control use of ESMF
                                                           !! memory profiling

  logical                 :: root_task = .false.           !< logical to indicate root task
#ifdef W3_CESMCOUPLED
  logical :: cesmcoupled = .true.                          !< logical to indicate CESM use case
#else
  logical :: cesmcoupled = .false.                         !< logical to indicate non-CESM use case
#endif
  integer, allocatable :: tend(:,:)                        !< the ending time of ModelAdvance when
                                                           !! run with multigrid=true
  logical                 :: user_histalarm = .false.      !< logical flag for user to set history alarms
                                                           !! using ESMF. If history_option is present as config
                                                           !! option, user_histalarm will be true and will be
                                                           !! set using history_option, history_n and history_ymd
  logical                 :: user_restalarm = .false.      !< logical flag for user to set restart alarms
                                                           !! using ESMF. If restart_option is present as config
                                                           !! option, user_restalarm will be true and will be
                                                           !! set using restart_option, restart_n and restart_ymd
  integer :: ymd                                           !< current year-month-day
  integer :: tod                                           !< current time of day (sec)
  integer :: time0(2)                                      !< start time stored as yyyymmdd,hhmmss
  integer :: timen(2)                                      !< end time stored as yyyymmdd,hhmmss
  integer :: nu_timer                                      !< simple timer log, unused except by UFS
  logical :: runtimelog = .false.                          !< logical flag for writing runtime log files
  character(*), parameter :: modName =  "(wav_comp_nuopc)" !< the name of this module
  character(*), parameter :: u_FILE_u = &                  !< a character string for an ESMF log message
       __FILE__

  !===============================================================================
contains
  !===============================================================================
  !> The public entry point. The NUOPC SetService method registers all of the
  !! user-provided subroutines in the module with the NUOPC layer
  !!
  !! @param[in]   gcomp   an ESMF_GridComp object
  !! @param[out]  rc      return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    character(len=*),parameter  :: subname=trim(modName)//':(SetServices) '

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    ! the NUOPC gcomp component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         userRoutine=InitializeP0, phase=0, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! attach specializing method(s)

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_DataInitialize, &
         specRoutine=DataInitialize, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
         specRoutine=ModelAdvance, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_MethodRemove(gcomp, label=model_label_SetRunClock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetRunClock, &
         specRoutine=ModelSetRunClock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
         specRoutine=ModelFinalize, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine SetServices

  !===============================================================================
  !> Switch to IPDv01 by filtering all other phaseMap entries
  !!
  !> @details Called by NUOPC to set the version of the Initialize Phase Definition
  !! (IPD) to use.
  !!
  !! @param[in]   gcomp           an ESMF_GridComp object
  !! @param[in]   importState     an ESMF_State object for import fields
  !! @param[in]   exportState     an ESMF_State object for export fields
  !! @param[in]   clock           an ESMF_Clock object
  !! @param[out]  rc return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Switch to IPDv01 by filtering all other phaseMap entries

    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, acceptStringList=(/"IPDv01p"/), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine InitializeP0

  !===============================================================================
  !> Read configuration attributes and advertise the import/export fields

  !> @details Called by NUOPC to read configuration attributes and to advertise the
  !! import and export fields. The configuration attributes are used to control run
  !! time settings, such as ESMF memory profiling, additional debug logging, multigrid
  !! mode and character strings for specific use cases. A set of configuration attributes
  !! is also read to describe any scalar fields to be added to a state. For coupling
  !! with the wave model, only a scalar field for the dimensions of the wave model
  !! is required. The scalar field is added to the export state to communicate to the
  !! CMEPS mediator the domain dimensions of the wave model in order to write
  !! mediator history and restart files. The attribute ScalarFieldName sets the name
  !! of the scalar field in the export state, the ScalarFieldCount sets the
  !! dimensionality of the scalar field and the ScalarFieldIdxGridNX (NY) set the
  !! index of the NX or NY dimension in the scalar field.
  !!
  !! @param[in]    gcomp             an ESMF_GridComp object
  !! @param[in]    importState       an ESMF_State object for import fields
  !! @param[in]    exportState       an ESMF_State object for export fields
  !! @param[in]    clock             an ESMF_Clock object
  !! @param[out]   rc                return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)

    use wav_shr_flags, only : w3_pdlib_flag
    ! input/output arguments
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(len=CL) :: logmsg
    logical           :: isPresent, isSet
    character(len=CL) :: cvalue
    character(len=*), parameter :: subname=trim(modName)//':(InitializeAdvertise) '
    !-------------------------------------------------------------------------------

    call ufs_settimer(wtime)
    rc = ESMF_SUCCESS
    call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    !----------------------------------------------------------------------------
    ! advertise fields
    !----------------------------------------------------------------------------

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldName", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      flds_scalar_name = trim(cvalue)
      call ESMF_LogWrite(trim(subname)//' flds_scalar_name = '//trim(flds_scalar_name), ESMF_LOGMSG_INFO)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
      call ESMF_LogWrite(trim(subname)//'Need to set attribute ScalarFieldName',&
           ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
      rc = ESMF_FAILURE
      return
    endif

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldCount", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      read(cvalue, *) flds_scalar_num
      write(logmsg,*) flds_scalar_num
      call ESMF_LogWrite(trim(subname)//' flds_scalar_num = '//trim(logmsg), ESMF_LOGMSG_INFO)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
      call ESMF_LogWrite(trim(subname)//'Need to set attribute ScalarFieldCount',&
           ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
      rc = ESMF_FAILURE
      return
    endif

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldIdxGridNX", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      read(cvalue,*) flds_scalar_index_nx
      write(logmsg,*) flds_scalar_index_nx
      call ESMF_LogWrite(trim(subname)//' : flds_scalar_index_nx = '//trim(logmsg), ESMF_LOGMSG_INFO)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
      call ESMF_LogWrite(trim(subname)//'Need to set attribute ScalarFieldIdxGridNX',&
           ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
      rc = ESMF_FAILURE
      return
    endif

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldIdxGridNY", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      read(cvalue,*) flds_scalar_index_ny
      write(logmsg,*) flds_scalar_index_ny
      call ESMF_LogWrite(trim(subname)//' : flds_scalar_index_ny = '//trim(logmsg), ESMF_LOGMSG_INFO)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
      call ESMF_LogWrite(trim(subname)//'Need to set attribute ScalarFieldIdxGridNY',&
           ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
      rc = ESMF_FAILURE
      return
    endif

    call NUOPC_CompAttributeGet(gcomp, name="ProfileMemory", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      read(cvalue,*) profile_memory
      call ESMF_LogWrite(trim(subname)//': profile_memory = '//trim(cvalue), ESMF_LOGMSG_INFO)
    end if

    call NUOPC_CompAttributeGet(gcomp, name="merge_import", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      if (trim(cvalue) == '.true.') then
        merge_import = .true.
      end if
    end if
    if (merge_import) then
      if (w3_pdlib_flag) then
        call ESMF_LogWrite('Merge_import is not valid with PDLIB', ESMF_LOGMSG_INFO)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
    end if

    call NUOPC_CompAttributeGet(gcomp, name='dbug_flag', value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      read(cvalue,*) dbug_flag
    end if
    write(logmsg,'(A,i6)') trim(subname)//': Wave cap dbug_flag is ',dbug_flag
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO)

    ! Get casename
    call NUOPC_CompAttributeGet(gcomp, name="case_name", value=casename, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(logmsg,'(A)') trim(subname)//': Wave casename setting : '//trim(casename)
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO)

    ! Get component instance
    call NUOPC_CompAttributeGet(gcomp, name="inst_suffix", isPresent=isPresent, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    if (isPresent) then
      call NUOPC_CompAttributeGet(gcomp, name="inst_suffix", value=inst_suffix, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      cvalue = inst_suffix(2:)
      read(cvalue, *) inst_index
    else
      inst_suffix = ""
      inst_index=1
    endif

    ! Get Multigrid setting
    multigrid = .false.
    call NUOPC_CompAttributeGet(gcomp, name='multigrid', value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      multigrid=(trim(cvalue)=="true")
    end if
    write(logmsg,'(A,l)') trim(subname)//': Wave multigrid setting is ',multigrid
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO)

    ! Determine wave-ice coupling
    wav_coupling_to_cice = .false.
    call NUOPC_CompAttributeGet(gcomp, name='wav_coupling_to_cice', value=cvalue, isPresent=isPresent, &
         isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      wav_coupling_to_cice=(trim(cvalue)=="true")
    end if
    write(logmsg,'(A,l)') trim(subname)//': Wave wav_coupling_to_cice setting is ',wav_coupling_to_cice
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO)

    ! Determine Runtime logging
    call NUOPC_CompAttributeGet(gcomp, name="RunTimeLog", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) runtimelog=(trim(cvalue)=="true")
    write(logmsg,*) runtimelog
    call ESMF_LogWrite('WW3_cap:RunTimeLog = '//trim(logmsg), ESMF_LOGMSG_INFO)
    if (runtimelog) then
      call ufs_file_setLogUnit('./log.ww3.timer',nu_timer,runtimelog)
    end if
    call advertise_fields(importState, exportState, flds_scalar_name, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeAdvertise

  !========================================================================
  !> Realize the import and export fields.

  !> @details Called by NUOPC to realize the import and export fields
  !! for the wave model. After the wave model initializes, the global index
  !! for all sea points is retrieved using the WW3 mapsf array. A global index
  !! array is then constructed which contains both land and sea points, with
  !! the land points at the end of the array. An ESMF Distgrid object is created
  !! using this global index array. The distgrid is then transfered to the ESMF
  !! Mesh provided for the wave model domain. If the provided Mesh does not contain
  !! a grid mask, then the internal WW3 mask is transfered to the Mesh, otherwise
  !! the mask provided with the mesh file will be used. This mask is used by
  !! CMEPS to map to and from the wave model. Once the mesh has been created, the
  !! advertised fields are realized on the mesh.
  !!
  !! @param[in]    gcomp           an ESMF_GridComp object
  !! @param[in]    importState     an ESMF_State object for import fields
  !! @param[in]    exportState     an ESMF_State object for export fields
  !! @param[in]    clock           an ESMF_Clock object
  !! @param[out]   rc              return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)

    use w3odatmd     , only : w3nout, w3seto, naproc, iaproc, naperr, napout
    use w3timemd     , only : stme21
    use w3adatmd     , only : w3naux, w3seta
    use w3idatmd     , only : w3seti, w3ninp
    use w3gdatmd     , only : nk, nseal, nsea, nx, ny, mapsf, w3nmod, w3setg
    use w3gdatmd     , only : rlgtype, ungtype, gtype
    use w3wdatmd     , only : va, time, w3ndat, w3dimw, w3setw
    use w3parall     , only : init_get_isea
#ifndef W3_CESMCOUPLED
    use wminitmd     , only : wminit, wminitnml
    use wmunitmd     , only : wmuget, wmuset
#endif
    use wav_shel_inp , only : set_shel_io
    use wav_grdout   , only : wavinit_grdout
    use wav_shr_mod  , only : diagnose_mesh, write_meshdecomp
#ifdef W3_PDLIB
    use yowNodepool  , only : ng
#endif

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_DistGrid)            :: distGrid
    type(ESMF_Mesh)                :: Emesh
    type(ESMF_Array)               :: elemMaskArray
    type(ESMF_VM)                  :: vm
    type(ESMF_Time)                :: esmfTime, startTime, currTime, stopTime
    type(ESMF_TimeInterval)        :: TimeOffset
    type(ESMF_TimeInterval)        :: TimeStep
    type(ESMF_Calendar)            :: calendar
    character(CL)                  :: cvalue
    integer                        :: shrlogunit
    integer                        :: yy,mm,dd,hh,ss
    integer                        :: start_ymd         ! start date (yyyymmdd)
    integer                        :: start_tod         ! start time of day (sec)
    integer                        :: stop_ymd          ! stop date (yyyymmdd)
    integer                        :: stop_tod          ! stop time of day (sec)
    integer                        :: ix, iy
    character(CL)                  :: starttype
    integer                        :: ntrace(2)
    integer                        :: n, jsea,isea, ncnt
    integer                        :: nlnd, nlnd_global, nlnd_local
    integer                        :: my_lnd_start, my_lnd_end
    integer, allocatable, target   :: mask_global(:)
    integer, allocatable, target   :: mask_local(:)
    integer, allocatable           :: gindex_lnd(:)
    integer, allocatable           :: gindex_sea(:)
    integer, allocatable           :: gindex(:)
    integer(i4)                    :: maskmin
    integer(i4), pointer           :: meshmask(:)
    character(23)                  :: dtme21
    integer                        :: iam, mpi_comm
    character(ESMF_MAXSTR)         :: msgString
    character(ESMF_MAXSTR)         :: diro
    character(CL)                  :: logfile
    logical                        :: local
    integer                        :: imod, idsi, idso, idss, idst, idse
    integer                        :: mds(13) ! Note that nds is set to this in w3initmod
    integer                        :: stdout
    integer                        :: petcount
    real(r8)                       :: toff
    character(ESMF_MAXSTR)         :: preamb = './'
    character(ESMF_MAXSTR)         :: ifname = 'ww3_multi.inp'
    character(len=*), parameter    :: subname = '(wav_comp_nuopc:InitializeRealize)'
    ! -------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    call ufs_settimer(wtime)
    !--------------------------------------------------------------------
    ! Set up data structures
    !--------------------------------------------------------------------

    if (.not. multigrid) then
      call w3nmod ( 1, 6, 6 )
      call w3ndat (    6, 6 )
      call w3naux (    6, 6 )
      call w3nout (    6, 6 )
      call w3ninp (    6, 6 )

      call w3setg ( 1, 6, 6 )
      call w3setw ( 1, 6, 6 )
      call w3seta ( 1, 6, 6 )
      call w3seto ( 1, 6, 6 )
      call w3seti ( 1, 6, 6 )
    end if

    !----------------------------------------------------------------------------
    ! Generate local mpi comm
    !----------------------------------------------------------------------------

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, mpiCommunicator=mpi_comm, peCount=petcount, localPet=iam, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
#ifndef W3_CESMCOUPLED
    nmproc = petcount
#else
    naproc = petcount
#endif

    ! naproc,iproc, napout, naperr are not available until after wminit
#ifndef W3_CESMCOUPLED
    improc = iam + 1
    if (multigrid) then
      nmpscr = 1
      is_esmf_component = .true.
    else
      iaproc = iam + 1
      naproc = nmproc
      napout = 1
      naperr = 1
    end if
    if (improc == 1) root_task = .true.
#else
    iaproc = iam + 1
    napout = 1
    naperr = 1
    if (iaproc == napout) root_task = .true.
#endif

    !--------------------------------------------------------------------
    ! IO set-up
    !--------------------------------------------------------------------

    if (cesmcoupled) then
      shrlogunit = 6
      if ( root_task ) then
        call NUOPC_CompAttributeGet(gcomp, name="diro", value=diro, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
        call NUOPC_CompAttributeGet(gcomp, name="logfile", value=logfile, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
        open (newunit=stdout, file=trim(diro)//"/"//trim(logfile))
      else
        stdout = 6
      endif
    else
      stdout = 6
    end if

    if (.not. multigrid) call set_shel_io(stdout,mds,ntrace)

    if ( root_task ) then
      write(stdout,'(a)')'      *** WAVEWATCH III Program shell ***      '
      write(stdout,'(a)')'==============================================='
    end if

    !--------------------------------------------------------------------
    ! Initialize run type
    !--------------------------------------------------------------------

    call NUOPC_CompAttributeGet(gcomp, name='start_type', value=starttype, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if ( trim(starttype) == trim('startup')) then
      runtype = "initial"
    else if (trim(starttype) == trim('continue') ) then
      runtype = "continue"
    else if (trim(starttype) == trim('branch')) then
      runtype = "branch"
    end if
    if ( root_task ) then
      write(stdout,*) 'WW3 runtype is '//trim(runtype)
    end if
    call ESMF_LogWrite('WW3 runtype is '//trim(runtype), ESMF_LOGMSG_INFO)

    !--------------------------------------------------------------------
    ! Time initialization
    !--------------------------------------------------------------------

    ! TIME0 = from ESMF clock
    ! NOTE - are not setting TIMEN here

    if ( root_task ) then
      write(stdout,'(a)')'  Time interval : '
      write(stdout,'(a)')'--------------------------------------------------'
    end if

    call ESMF_ClockPrint(clock, options="startTime", preString="Model Start Time: ", &
         unit=msgString, rc=rc)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_ClockPrint(clock, options="currTime", preString="Model Current Time: ", &
         unit=msgString, rc=rc)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_ClockGet( clock, startTime=startTime, currTime=currTime, rc=rc)
    TimeOffset = currTime - startTime
    call ESMF_TimeIntervalGet(TimeOffset, h_r8=toff, rc=rc)
    write(msgstring,'(a,g14.7)')'TimeOffset: CurrTime - StartTime = ',toff
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    ! Initial run or restart run
    if ( runtype == "initial") then
      call ESMF_ClockGet( clock, startTime=esmfTime, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
#ifndef W3_CESMCOUPLED
      esmfTime = esmfTime + TimeOffset
#endif
    else
      call ESMF_ClockGet( clock, currTime=esmfTime, rc=rc )
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif
    ! Determine time attributes for history output
    call ESMF_TimeGet( esmfTime, timeString=time_origin, calendar=calendar, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    time_origin = 'seconds since '//time_origin(1:10)//' '//time_origin(12:19)
    !call ESMF_ClockGet(clock, calendar=calendar)
    if (calendar == ESMF_CALKIND_GREGORIAN) then
      calendar_name = 'standard'
    else if (calendar == ESMF_CALKIND_NOLEAP) then
      calendar_name = 'noleap'
    end if
    call ESMF_TimeGet( esmfTime, yy=yy, mm=mm, dd=dd, s=start_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ymd2date(yy, mm, dd, start_ymd)

    hh = start_tod/3600
    mm = (start_tod - (hh * 3600))/60
    ss = start_tod - (hh*3600) - (mm*60)

    time0(1) = start_ymd
    time0(2) = hh*10000 + mm*100 + ss

    call ESMF_ClockGet( clock, stopTime=stopTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet( stopTime, yy=yy, mm=mm, dd=dd, s=stop_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ymd2date(yy, mm, dd, stop_ymd)

    hh = stop_tod/3600
    mm = (stop_tod - (hh * 3600))/60
    ss = stop_tod - (hh*3600) - (mm*60)

    timen(1) = stop_ymd
    timen(2) = hh*10000 + mm*100 + ss

    call stme21 ( time0 , dtme21 )
    if ( root_task ) then
      write (stdout,'(a)')' Starting time : '//trim(dtme21)
      write (stdout,'(a,i8,2x,i8)') 'start_ymd, stop_ymd = ',start_ymd, stop_ymd
    end if
#ifndef W3_CESMCOUPLED
    stime = time0
    etime = timen
#endif

    !--------------------------------------------------------------------
    ! Wave model initialization
    !--------------------------------------------------------------------

#ifndef W3_CESMCOUPLED
    if (multigrid) then
      call ESMF_UtilIOUnitGet(idsi); open(unit=idsi, status='scratch')
      call ESMF_UtilIOUnitGet(idso); open(unit=idso, status='scratch')
      call ESMF_UtilIOUnitGet(idss); open(unit=idss, status='scratch')
      call ESMF_UtilIOUnitGet(idst); open(unit=idst, status='scratch')
      call ESMF_UtilIOUnitGet(idse); open(unit=idse, status='scratch')
      close(idsi); close(idso); close(idss); close(idst); close(idse)

      if ( trim(ifname) == 'ww3_multi.nml' ) then
        call wminitnml ( idsi, idso, idss, idst, idse, trim(ifname), &
             mpi_comm, preamb=preamb )
      else
        call wminit ( idsi, idso, idss, idst, idse, trim(ifname), &
             mpi_comm, preamb=preamb )
      endif

      allocate(tend(2,nrgrd))
      do imod = 1,nrgrd
        tend(1,imod) = etime(1)
        tend(2,imod) = etime(2)
      end do
      call ESMF_LogWrite(trim(subname)//' done = wminit', ESMF_LOGMSG_INFO)
    else
      call waveinit_ufs(gcomp, ntrace, mpi_comm, mds, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if
#else
    time = time0
    call ESMF_ClockGet( clock, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call waveinit_cesm(gcomp, ntrace, mpi_comm, mds, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
#endif

    ! call mpi_barrier ( mpi_comm, ierr )
    if ( root_task ) then
      inquire(unit=nds(1), name=logfile)
      print *,'WW3 log written to '//trim(logfile)
    end if

    if (wav_coupling_to_cice) then
      if (nwav_elev_spectrum .gt. nk) then
        call ESMF_LogWrite('nwav_elev_spectrum is greater than nk ', ESMF_LOGMSG_INFO)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
    end if

    !--------------------------------------------------------------------
    ! Intialize the list of requested output variables for netCDF output
    !--------------------------------------------------------------------

    if (user_netcdf_grdout) then
      call wavinit_grdout
    end if

    !--------------------------------------------------------------------
    ! Mesh initialization
    !--------------------------------------------------------------------

    if (gtype .eq. ungtype) then
      unstr_mesh = .true.
    else
      unstr_mesh = .false.
    end if

    ! Create a  global index array for sea points.
    !
    ! Note that nsea is the global number of sea points - and nseal is the local
    ! number of sea points. For the unstr mesh, the nsea points are on mesh nodes.
    ! We will use the gindex to set the element distgrid of a dual mesh. A dual mesh
    ! contains the mesh nodes at the center of each element. For the domain decomposition
    ! case (PDLIB), set a value of the local sea points on this processor minus the
    ! ghost points.
#ifdef W3_PDLIB
    nseal_cpl = nseal - ng
#else
    nseal_cpl = nseal
#endif
    allocate(gindex_sea(nseal_cpl))
    do jsea=1, nseal_cpl
      call init_get_isea(isea, jsea)
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      gindex_sea(jsea) = ix + (iy-1)*nx
    end do

    if (unstr_mesh) then
      ! create distGrid from global index array of sea points with no ghost points
      DistGrid = ESMF_DistGridCreate(arbSeqIndexList=gindex_sea, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      deallocate(gindex_sea)
    else
      ! create a global index array for non-sea (i.e. land points)
      allocate(mask_global(nx*ny), mask_local(nx*ny))
      mask_local(:) = 0
      mask_global(:) = 0
      do jsea=1, nseal_cpl
        call init_get_isea(isea, jsea)
        ix = mapsf(isea,1)
        iy = mapsf(isea,2)
        mask_local(ix + (iy-1)*nx) = 1
      end do
      call ESMF_VMAllReduce(vm, sendData=mask_local, recvData=mask_global, count=nx*ny, &
           reduceflag=ESMF_REDUCE_MAX, rc=rc)

      nlnd_global = nx*ny - nsea
      nlnd_local = nlnd_global / naproc
      my_lnd_start = nlnd_local*iam + min(iam, mod(nlnd_global, naproc)) + 1
      if (iam < mod(nlnd_global, naproc)) then
        nlnd_local = nlnd_local + 1
      end if
      my_lnd_end = my_lnd_start + nlnd_local - 1

      allocate(gindex_lnd(my_lnd_end - my_lnd_start + 1))
      ncnt = 0
      do n = 1,nx*ny
        if (mask_global(n) == 0) then ! this is a land point
          ncnt = ncnt + 1
          if (ncnt >= my_lnd_start .and. ncnt <= my_lnd_end) then
            gindex_lnd(ncnt - my_lnd_start + 1) = n
          end if
        end if
      end do
      deallocate(mask_global)
      deallocate(mask_local)

      ! create a global index that includes both sea and land - but put land at the end
      nlnd = (my_lnd_end - my_lnd_start + 1)
      allocate(gindex(nlnd + nseal_cpl))
      do ncnt = 1,nlnd + nseal
        if (ncnt <= nseal_cpl) then
          gindex(ncnt) = gindex_sea(ncnt)
        else
          gindex(ncnt) = gindex_lnd(ncnt-nseal_cpl)
        end if
      end do
      deallocate(gindex_sea)
      deallocate(gindex_lnd)

      ! create distGrid from global index array
      DistGrid = ESMF_DistGridCreate(arbSeqIndexList=gindex, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    ! get the mesh file name
    call NUOPC_CompAttributeGet(gcomp, name='mesh_wav', value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! read in the mesh with the above DistGrid
    EMesh = ESMF_MeshCreate(filename=trim(cvalue), fileformat=ESMF_FILEFORMAT_ESMFMESH, &
         elementDistgrid=Distgrid,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (dbug_flag > 5) then
      call diagnose_mesh(EMesh, size(gindex), 'EMesh', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (.not. unstr_mesh) then
      ! obtain the mesh mask and find the minimum value across all PEs
      call ESMF_MeshGet(EMesh, elementDistgrid=Distgrid, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call ESMF_DistGridGet(Distgrid, localDe=0, elementCount=ncnt, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      allocate(meshmask(ncnt))
      elemMaskArray = ESMF_ArrayCreate(Distgrid, farrayPtr=meshmask, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call ESMF_MeshGet(Emesh, elemMaskArray=elemMaskArray, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call ESMF_VMAllFullReduce(vm, sendData=meshmask, recvData=maskmin, count=ncnt, &
           reduceflag=ESMF_REDUCE_MIN, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      if (maskmin == 1) then
        ! replace mesh mask with internal mask
        meshmask(:) = 0
        meshmask(1:nseal_cpl) = 1
        call ESMF_MeshSet(mesh=EMesh, elementMask=meshmask, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
      end if

      if (dbug_flag > 5) then
        call ESMF_ArrayWrite(elemMaskArray, 'meshmask.nc', variableName = 'mask', &
             overwrite=.true., rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      end if
      deallocate(meshmask)
      deallocate(gindex)
    end if

    if (dbug_flag > 5) then
      call write_meshdecomp(Emesh, 'emesh', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    !--------------------------------------------------------------------
    ! Realize the actively coupled fields
    !--------------------------------------------------------------------
    call realize_fields(gcomp, mesh=Emesh, flds_scalar_name=flds_scalar_name, &
         flds_scalar_num=flds_scalar_num, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

#ifndef W3_CESMCOUPLED
    !TODO: when is this required?
    if (multigrid) then
      do imod = 1,nrgrd
        call w3setg ( imod, mdse, mdst )
        call w3setw ( imod, mdse, mdst )
        call w3seta ( imod, mdse, mdst )
        call w3seti ( imod, mdse, mdst )
        call w3seto ( imod, mdse, mdst )
        call wmsetm ( imod, mdse, mdst )
        local = iaproc .gt. 0 .and. iaproc .le. naproc
        if ( local .and. flcold .and. fliwnd ) call w3uini( va )
      enddo
    end if
#endif
    if (root_task) call ufs_logtimer(nu_timer,time,start_tod,'InitializeRealize time: ',runtimelog,wtime)

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeRealize

  !===============================================================================
  !> Initialize the field values in the export state
  !!
  !> @details Called by NUOPC to initialize the field values in the export state and
  !! the values for the scalar field which describes the wave model global domain
  !! size.
  !!
  !! @param        gcomp     an ESMF_GridComp object
  !! @param[out]   rc        return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine DataInitialize(gcomp, rc)

    use wav_import_export, only : calcRoughl
    use w3gdatmd         , only : nx, ny

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)  :: exportState
    real(r8), pointer :: z0rlen(:)
    real(r8), pointer :: sw_lamult(:)
    real(r8), pointer :: sw_ustokes(:)
    real(r8), pointer :: sw_vstokes(:)
    real(r8), pointer :: wave_elevation_spectrum(:,:)
    character(len=*),parameter :: subname = '(wav_comp_nuopc:DataInitialize)'
    ! -------------------------------------------------------------------

    !--------------------------------------------------------------------
    ! Create export state
    !--------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    call NUOPC_ModelGet(gcomp, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (state_fldchk(exportState, 'Sw_lamult')) then
      call state_getfldptr(exportState, 'Sw_lamult', sw_lamult, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      sw_lamult (:) = 1.
    endif
    if (state_fldchk(exportState, 'Sw_ustokes')) then
      call state_getfldptr(exportState, 'Sw_ustokes', sw_ustokes, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      sw_ustokes(:) = 0.
    endif
    if (state_fldchk(exportState, 'Sw_vstokes')) then
      call state_getfldptr(exportState, 'Sw_vstokes', sw_vstokes, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      sw_vstokes(:) = 0.
    endif
    if (state_fldchk(exportState, 'Sw_z0')) then
      call state_getfldptr(exportState, 'Sw_z0', z0rlen, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call CalcRoughl(z0rlen)
    endif
    if (wav_coupling_to_cice) then
      call state_getfldptr(exportState, 'wave_elevation_spectrum', wave_elevation_spectrum, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      wave_elevation_spectrum(:,:) = 0.
    endif

    if (.not. unstr_mesh) then
      ! Set global grid size scalars in export state
      call State_SetScalar(dble(nx), flds_scalar_index_nx, exportState, flds_scalar_name, flds_scalar_num, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call State_SetScalar(dble(ny), flds_scalar_index_ny, exportState, flds_scalar_name, flds_scalar_num, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if ( dbug_flag > 5) then
      call state_diagnose(exportState, 'at DataInitialize ', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine DataInitialize

  !=====================================================================
  !> Called by NUOPC to advance the model a single timestep
  !!
  !> @details At each model advance, the call to import_fields fills the
  !! import state with the updated values. If a history alarm is present
  !! and ringing, a logical to write a wave history file is set true. The
  !! wave model itself is then advanced during which a history file will
  !! be written via a call to w3iogonc in place of w3iogo. The export
  !! fields at the current model Advance are filled in export_fields
  !!
  !! @param        gcomp     an ESMF_GridComp object
  !! @param[out]   rc        return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine ModelAdvance(gcomp, rc)

    !------------------------
    ! Run WW3
    !------------------------

    use w3wavemd          , only : w3wave
    use w3wdatmd          , only : time, w3setw
    use wav_import_export , only : import_fields, export_fields
    use wav_shel_inp      , only : odat
    use w3odatmd          , only : rstwr, histwr

    ! arguments:
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState
    type(ESMF_State)        :: exportState
    type(ESMF_Clock)        :: clock
    type(ESMF_Alarm)        :: alarm
    type(ESMF_TimeInterval) :: timeStep, elapsedTime
    type(ESMF_Time)         :: currTime, nextTime, startTime, stopTime
    integer                 :: yy,mm,dd,hh,ss
    integer                 :: imod
    integer                 :: shrlogunit ! original log unit and level
    character(ESMF_MAXSTR)  :: msgString
    character(len=*),parameter :: subname = '(wav_comp_nuopc:ModelAdvance) '
    !-------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag  > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    !------------
    ! query the Component for its importState, exportState and clock
    !------------
    call ESMF_GridCompGet(gcomp, importState=importState, exportState=exportState, clock=clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockPrint(clock, options="currTime", preString="------>Advancing WAV from: ", &
         unit=msgString, rc=rc)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, timeStep=timeStep, rc=rc)
    call ESMF_TimePrint(currTime + timeStep, preString="--------------------------------> to: ", &
         unit=msgString, rc=rc)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    !------------
    ! Determine time info
    !------------
    call ESMF_ClockGet( clock, currTime=currTime, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet( currTime, yy=yy, mm=mm, dd=dd, s=tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ymd2date(yy, mm, dd, ymd)
    hh = tod/3600
    mm = (tod - (hh * 3600))/60
    ss = tod - (hh*3600) - (mm*60)
    time0(1) = ymd
    time0(2) = hh*10000 + mm*100 + ss
    if ( root_task ) then
      write(nds(1),'(a,3i4,i10)') 'ymd2date currTime wav_comp_nuopc hh,mm,ss,ymd', hh,mm,ss,ymd
    end if
    if (root_task) call ufs_logtimer(nu_timer,time,tod,'ModelAdvance time since last step: ',runtimelog,wtime)
    call ufs_settimer(wtime)

    ! use next time; the NUOPC clock is not updated
    ! until the end of the time interval
    call ESMF_ClockGetNextTime(clock, nextTime=nextTime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet( nextTime, yy=yy, mm=mm, dd=dd, s=tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    elapsedTime = nextTime - startTime
    call ESMF_TimeIntervalGet(elapsedTime, s_i8=elapsed_secs,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ymd2date(yy, mm, dd, ymd)
    hh = tod/3600
    mm = (tod - (hh * 3600))/60
    ss = tod - (hh*3600) - (mm*60)

    timen(1) = ymd
    timen(2) = hh*10000 + mm*100 + ss

    time = time0
#ifndef W3_CESMCOUPLED
    if (multigrid) then
      do imod = 1,nrgrd
        tend(1,imod) = timen(1)
        tend(2,imod) = timen(2)
      end do
    end if
#endif

    !------------
    ! Obtain import data from import state
    !------------
    call import_fields(gcomp, time0, timen, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------
    ! Run the wave model for the given interval
    !------------
    if(profile_memory) call ESMF_VMLogMemInfo("Entering WW3 Run : ")

    if (user_restalarm) then
      ! Determine if time to write ww3 restart files
      call ESMF_ClockGetAlarm(clock, alarmname='alarm_restart', alarm=alarm, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      if (ESMF_AlarmIsRinging(alarm, rc=rc)) then
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        rstwr = .true.
        call ESMF_AlarmRingerOff( alarm, rc=rc )
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        rstwr = .false.
      endif
    else
      rstwr = .false.
    end if

    if (user_histalarm) then
      ! Determine if time to write ww3 history files
      call ESMF_ClockGetAlarm(clock, alarmname='alarm_history', alarm=alarm, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      if (ESMF_AlarmIsRinging(alarm, rc=rc)) then
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        histwr = .true.
        call ESMF_AlarmRingerOff( alarm, rc=rc )
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
      else
        histwr = .false.
      endif
    else
      histwr = .false.
    end if
    if ( root_task ) then
      !  write(nds(1),*) 'wav_comp_nuopc time', time, timen
      !  write(nds(1),*) 'ww3 hist flag ', histwr, hh
    end if

    ! Advance the wave model
#ifndef W3_CESMCOUPLED
    if (multigrid) then
      call wmwave ( tend )
    else
      call w3wave ( 1, odat, timen )
    end if
#else
    call w3wave ( 1, odat, timen )
#endif
    if(profile_memory) call ESMF_VMLogMemInfo("Exiting  WW3 Run : ")

    !------------
    ! Create export state
    !------------

    call export_fields(gcomp, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)
    if (root_task) call ufs_logtimer(nu_timer,time,tod,'ModelAdvance time: ',runtimelog,wtime)
    call ufs_settimer(wtime)

  end subroutine ModelAdvance

  !===============================================================================
  !> Called by NUOPC to manage the model clock
  !!
  !! @param[in]    gcomp     an ESMF_GridComp object
  !! @param[out]   rc        return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine ModelSetRunClock(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)         :: mclock, dclock
    type(ESMF_Time)          :: mcurrtime, dcurrtime
    type(ESMF_Time)          :: mstoptime
    type(ESMF_Time)          :: mstarttime
    type(ESMF_TimeInterval)  :: mtimestep, dtimestep
    logical                  :: isPresent
    logical                  :: isSet
    character(len=256)       :: cvalue
    character(len=256)       :: restart_option ! Restart option units
    integer                  :: restart_n      ! Number until restart interval
    integer                  :: restart_ymd    ! Restart date (YYYYMMDD)
    type(ESMF_ALARM)         :: restart_alarm
    character(len=256)       :: stop_option    ! Stop option units
    integer                  :: stop_n         ! Number until stop interval
    integer                  :: stop_ymd       ! Stop date (YYYYMMDD)
    type(ESMF_ALARM)         :: stop_alarm
    character(len=256)       :: history_option ! History option units
    integer                  :: history_n      ! Number until history interval
    integer                  :: history_ymd    ! History date (YYYYMMDD)
    type(ESMF_ALARM)         :: history_alarm
    character(len=128)       :: name
    integer                  :: alarmcount
    character(len=*),parameter :: subname=trim(modName)//':(ModelSetRunClock) '

    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    ! query the Component for its clocks
    call NUOPC_ModelGet(gcomp, driverClock=dclock, modelClock=mclock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(dclock, currTime=dcurrtime, timeStep=dtimestep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(mclock, currTime=mcurrtime, timeStep=mtimestep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! force model clock currtime and timestep to match driver and set stoptime
    !--------------------------------

    mstoptime = mcurrtime + dtimestep
    call ESMF_ClockSet(mclock, currTime=dcurrtime, timeStep=dtimestep, stopTime=mstoptime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! set restart, stop and history alarms
    !--------------------------------

    call ESMF_ClockGetAlarmList(mclock, alarmlistflag=ESMF_ALARMLIST_ALL, alarmCount=alarmCount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (alarmCount == 0) then

      call ESMF_ClockGet(mclock, startTime=mStartTime,  rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_GridCompGet(gcomp, name=name, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call ESMF_LogWrite(trim(subname)//'setting alarms for ' // trim(name), ESMF_LOGMSG_INFO)

      !----------------
      ! Restart alarm
      !----------------
      call NUOPC_CompAttributeGet(gcomp, name="restart_option", isPresent=isPresent, isSet=isSet, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      if (isPresent .and. isSet) then
        call NUOPC_CompAttributeGet(gcomp, name="restart_option", value=restart_option, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        call NUOPC_CompAttributeGet(gcomp, name="restart_n", value=cvalue, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        read(cvalue,*) restart_n

        call NUOPC_CompAttributeGet(gcomp, name="restart_ymd", value=cvalue, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        read(cvalue,*) restart_ymd

        call alarmInit(mclock, restart_alarm, restart_option, &
             opt_n   = restart_n,           &
             opt_ymd = restart_ymd,         &
             RefTime = mCurrTime,           &
             alarmname = 'alarm_restart', rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        call ESMF_AlarmSet(restart_alarm, clock=mclock, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        user_restalarm = .true.
      else
        ! If attribute is not present - write restarts at native WW3 freq
        restart_option = 'none'
        restart_n = -999
        user_restalarm = .false.
      end if

      !----------------
      ! Stop alarm
      !----------------
      call NUOPC_CompAttributeGet(gcomp, name="stop_option", value=stop_option, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call NUOPC_CompAttributeGet(gcomp, name="stop_n", value=cvalue, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      read(cvalue,*) stop_n

      call NUOPC_CompAttributeGet(gcomp, name="stop_ymd", value=cvalue, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      read(cvalue,*) stop_ymd

      call alarmInit(mclock, stop_alarm, stop_option, &
           opt_n   = stop_n,           &
           opt_ymd = stop_ymd,         &
           RefTime = mCurrTime,       &
           alarmname = 'alarm_stop', rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_AlarmSet(stop_alarm, clock=mclock, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      !----------------
      ! History alarm
      !----------------
      call NUOPC_CompAttributeGet(gcomp, name="history_option", isPresent=isPresent, isSet=isSet, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      if (isPresent .and. isSet) then
        call NUOPC_CompAttributeGet(gcomp, name='history_option', value=history_option, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        call NUOPC_CompAttributeGet(gcomp, name="history_n", value=cvalue, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        read(cvalue,*) history_n

        call NUOPC_CompAttributeGet(gcomp, name="history_ymd", value=cvalue, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        read(cvalue,*) history_ymd

        call alarmInit(mclock, history_alarm, history_option, &
             opt_n   = history_n,           &
             opt_ymd = history_ymd,         &
             RefTime = mStartTime,          &
             alarmname = 'alarm_history', rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        call ESMF_AlarmSet(history_alarm, clock=mclock, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        user_histalarm = .true.
      else
        ! If attribute is not present - write history output at native WW3 frequency
        history_option = 'none'
        history_n = -999
        user_histalarm = .false.
      end if

    end if

    !--------------------------------
    ! Advance model clock to trigger alarms then reset model clock back to currtime
    !--------------------------------

    call ESMF_ClockAdvance(mclock,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockSet(mclock, currTime=dcurrtime, timeStep=dtimestep, stopTime=mstoptime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine ModelSetRunClock

  !===============================================================================
  !> Called by NUOPC at the end of the run to clean up.
  !!
  !! @param[in]    gcomp     an ESMF_GridComp object
  !! @param[out]   rc        return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine ModelFinalize(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(*), parameter :: F00   = "('(ww3_comp_nuopc) ',8a)"
    character(*), parameter :: F91   = "('(ww3_comp_nuopc) ',73('-'))"
    character(len=*),parameter  :: subname=trim(modName)//':(ModelFinalize) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    if ( root_task ) then
      write(nds(1),F91)
      write(nds(1),F00) 'WW3: end of main integration loop'
      write(nds(1),F91)
    end if

    call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)
    if(root_task) call ufs_logtimer(nu_timer,timen,tod,'ModelFinalize time: ',runtimelog,wtime)

  end subroutine ModelFinalize

  !===============================================================================
  !> Initialize the wave model for the CESM use case
  !!
  !> @details Calls public routine read_shel_config to read the ww3_shel.inp or
  !! ww3_shel.nml file. Calls w3init to initialize the wave model
  !!
  !! @param[in]    gcomp        an ESMF_GridComp object
  !! @param[in]    ntrace       unit numbers for trace
  !! @param[in]    mpi_comm     an mpi communicator
  !! @param[in]    mds          unit numbers
  !! @param[out]   rc           return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine waveinit_cesm(gcomp, ntrace, mpi_comm, mds, rc)

    ! Initialize ww3 for cesm (called from InitializeRealize)

    use w3initmd     , only : w3init
    use w3gdatmd     , only : dtcfl, dtcfli, dtmax, dtmin
    use w3idatmd     , only : inflags1, inflags2
    use w3odatmd     , only : initfile
    use wav_shr_mod  , only : casename
    use wav_shr_mod  , only : inst_index, inst_name, inst_suffix
    use wav_shr_mod  , only : wav_coupling_to_cice
    use wav_shel_inp , only : read_shel_config
    use wav_shel_inp , only : npts, odat, iprt, x, y, pnames, prtfrm
    use wav_shel_inp , only : flgrd, flgd, flgr2, flg2

    ! input/output variables
    type(ESMF_GridComp)   :: gcomp
    integer , intent(in)  :: ntrace(:)
    integer , intent(in)  :: mpi_comm
    integer , intent(in)  :: mds(:)
    integer , intent(out) :: rc

    ! local variables
    integer           :: ierr
    integer           :: unitn  ! namelist unit number
    integer           :: shrlogunit
    logical           :: isPresent, isSet
    real(r8)          :: dtmax_in  ! Maximum overall time step.
    real(r8)          :: dtmin_in  ! Minimum dynamic time step for source
    real(r8)          :: dtcfl_in  ! Maximum CFL time step X-Y propagation.
    real(r8)          :: dtcfli_in ! Maximum CFL time step X-Y propagation intra-spectral
    integer           :: stdout
    character(len=*), parameter    :: subname = '(wav_comp_nuopc:wavinit_cesm)'
    ! -------------------------------------------------------------------

    namelist /ww3_inparm/ initfile, dtcfl, dtcfli, dtmax, dtmin

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    inst_name = "WAV"//trim(inst_suffix)
    ! Read namelist (set initfile in w3odatmd)
    if ( root_task ) then
      open (newunit=unitn, file='wav_in'//trim(inst_suffix), status='old')
      read (unitn, ww3_inparm, iostat=ierr)
      if (ierr /= 0) then
        call ESMF_LogWrite(trim(subname)//' problem reading ww3_inparm namelist',&
             ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
        rc = ESMF_FAILURE
        return
      end if
      close (unitn)

      ! Write out input
      stdout = mds(1)
      write(stdout,*)
      write(stdout,'(a)')' --------------------------------------------------'
      write(stdout,'(a)')'  Initializations : '
      write(stdout,'(a)')' --------------------------------------------------'
      write(stdout,'(a)')' Case Name is '//trim(casename)
      write(stdout,'(a)') trim(subname)//' inst_name   = '//trim(inst_name)
      write(stdout,'(a)') trim(subname)//' inst_suffix = '//trim(inst_suffix)
      write(stdout,'(a,i4)') trim(subname)//' inst_index  = ',inst_index
      write(stdout,'(a)')' Read in ww3_inparm namelist from wav_in'//trim(inst_suffix)
      write(stdout,'(a)')' initfile = '//trim(initfile)
      write(stdout,'(a, 2x, f10.3)')' dtcfl    = ',dtcfl
      write(stdout,'(a, 2x, f10.3)')' dtcfli   = ',dtcfli
      write(stdout,'(a, 2x, f10.3)')' dtmax    = ',dtmax
      write(stdout,'(a, 2x, f10.3)')' dtmin    = ',dtmin
      write(stdout,*)
    end if

    ! ESMF does not have a broadcast for chars
    call mpi_bcast(initfile, len(initfile), MPI_CHARACTER, 0, mpi_comm, ierr)
    if (ierr /= MPI_SUCCESS) then
      call ESMF_LogWrite(trim(subname)//' error in mpi broadcast for initfile ', &
           ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
      rc = ESMF_FAILURE
      return
    end if
    call mpi_bcast(dtcfl, 1, MPI_INTEGER, 0, mpi_comm, ierr)
    if (ierr /= MPI_SUCCESS) then
      call ESMF_LogWrite(trim(subname)//' error in mpi broadcast for dtcfl ',&
           ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
      rc = ESMF_FAILURE
      return
    end if
    call mpi_bcast(dtcfli, 1, MPI_INTEGER, 0, mpi_comm, ierr)
    if (ierr /= MPI_SUCCESS) then
      call ESMF_LogWrite(trim(subname)//' error in mpi broadcast for dtcfli ',&
           ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
      rc = ESMF_FAILURE
      return
    end if
    call mpi_bcast(dtmax, 1, MPI_INTEGER, 0, mpi_comm, ierr)
    if (ierr /= MPI_SUCCESS) then
      call ESMF_LogWrite(trim(subname)//' error in mpi broadcast for dtmax ',&
           ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
      rc = ESMF_FAILURE
      return
    end if
    call mpi_bcast(dtmin, 1, MPI_INTEGER, 0, mpi_comm, ierr)
    if (ierr /= MPI_SUCCESS) then
      call ESMF_LogWrite(trim(subname)//' error in mpi broadcast for dtmax ',&
           ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
      rc = ESMF_FAILURE
      return
    end if
    dtmax_in  = dtmax
    dtcfl_in  = dtcfl
    dtcfli_in = dtcfli
    dtmin_in  = dtmin

    ! Read the namelist settings in ww3_shel.nml
    call ESMF_LogWrite(trim(subname)//' call read_shel_config', ESMF_LOGMSG_INFO)
    call read_shel_config(mpi_comm, mds, time0_overwrite=time0, timen_overwrite=timen)

    ! NOTE:  that wavice_coupling must be set BEFORE the call to advertise_fields
    ! So the current mechanism is to force the inflags1(-7) and inflags1(-3) be set to true
    ! if wavice coupling is active
    ! NOTE:
    ! inflags1(-7) = nml_input%forcing%ice_param1
    ! inflags1(-3) = nml_input%forcing%ice_param5

    ! Force inflags2 to be false - otherwise inflags2 will be set to inflags1 and answers will change
    ! Need to set this to .false. to avoid scaling of ice in section 4. of w3srcemed.
    ! inflags2(4) is true if ice concentration was ever read during this simulation
    ! Currently IC4 is used in cesm
    inflags2(:) = .false.
    if (wav_coupling_to_cice) then
      inflags2(4)  = .true. ! inflags2(4) is true if ice concentration was read during initialization
      inflags1(-7) = .true. ! ice thickness
      inflags2(-7) = .true. ! ice thickness
      inflags1(-3) = .true. ! ice floe size
      inflags2(-3) = .true. ! ice floe size
    else
      inflags1(-7) = .false. ! ice thickness
      inflags2(-7) = .false. ! ice thickness
      inflags1(-3) = .false. ! ice floe size
      inflags2(-3) = .false. ! ice floe size
    end if

    ! custom restart and history file names are used for CESM
    use_user_histname = .true.
    use_user_restname = .true.

    ! if runtype=initial, the initfile will be read in w3iorsmd
    if (len_trim(inst_suffix) > 0) then
      user_restfname = trim(casename)//'.ww3'//trim(inst_suffix)//'.r.'
      user_histfname = trim(casename)//'.ww3'//trim(inst_suffix)//'.hi.'
    else
      user_restfname = trim(casename)//'.ww3.r.'
      user_histfname = trim(casename)//'.ww3.hi.'
    endif

    ! netcdf gridded output is used for CESM
    user_netcdf_grdout = .true.
    ! restart and history alarms are set for CESM by default through config

    ! Read in initial/restart data and initialize the model
    ! ww3 read initialization occurs in w3iors (which is called by initmd in module w3initmd)
    ! ww3 always starts up from a 'restart' file type
    ! For a startup (including hybrid) or branch run the restart file is obtained from 'initfile'
    ! For a continue run, the restart filename upon read is created from the time(1:2) array
    ! flgr2 is flags for coupling output, not ready yet so keep .false.
    ! 1 is model number
    ! IsMulti does not appear to be used, setting to .false.

    call ESMF_LogWrite(trim(subname)//' call w3init', ESMF_LOGMSG_INFO)
    call w3init ( 1, .false., 'ww3', mds, ntrace, odat, flgrd, flgr2, flgd, flg2, &
         npts, x, y, pnames, iprt, prtfrm, mpi_comm )

    ! NOTE: these need to be set again AFTER w3init is run - since these values will be overwritten
    ! by the read of mod_def.ww3
    dtmax  = dtmax_in
    dtcfl  = dtcfl_in
    dtcfli = dtcfli_in
    dtmin  = dtmin_in

    if (dbug_flag  > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)
  end subroutine waveinit_cesm

  !===============================================================================
  !> Initialize the wave model for the UWM use case
  !!
  !> @details Calls public routine read_shel_config to read the ww3_shel.inp or
  !! ww3_shel.nml file. Calls w3init to initialize the wave model
  !!
  !! @param[in]    gcomp        an ESMF_GridComp object
  !! @param[in]    ntrace       unit numbers for trace
  !! @param[in]    mpi_comm     an mpi communicator
  !! @param[in]    mds          unit numbers
  !! @param[out]   rc           return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine waveinit_ufs( gcomp, ntrace, mpi_comm, mds, rc)

    ! Initialize ww3 for ufs (called from InitializeRealize)

    use w3odatmd     , only : fnmpre
    use w3gdatmd     , only : dtcfl, dtcfli, dtmax, dtmin
    use w3initmd     , only : w3init
    use wav_shel_inp , only : read_shel_config
    use wav_shel_inp , only : npts, odat, iprt, x, y, pnames, prtfrm
    use wav_shel_inp , only : flgrd, flgd, flgr2, flg2

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(in)  :: ntrace(:)
    integer, intent(in)  :: mpi_comm
    integer, intent(in)  :: mds(:)
    integer, intent(out) :: rc

    ! local variables
    character(len=CL) :: logmsg
    logical           :: isPresent, isSet
    character(len=CL) :: cvalue
    integer           :: dt_in(4)
    character(len=*), parameter :: subname = '(wav_comp_nuopc:wavinit_ufs)'
    ! -------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    ! restart and history alarms are optional for UFS and used via allcomp config settings
    call NUOPC_CompAttributeGet(gcomp, name='user_sets_histname', value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      use_user_histname=(trim(cvalue)=="true")
    end if
    write(logmsg,'(A,l)') trim(subname)//': Custom history names in use ',use_user_histname
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='user_sets_restname', value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      use_user_restname=(trim(cvalue)=="true")
    end if
    write(logmsg,'(A,l)') trim(subname)//': Custom restart names in use ',use_user_restname
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO)

    call NUOPC_CompAttributeGet(gcomp, name='gridded_netcdfout', value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      user_netcdf_grdout=(trim(cvalue)=="true")
    end if
    write(logmsg,'(A,l)') trim(subname)//': Gridded netcdf output is requested ',user_netcdf_grdout
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO)

    if (use_user_histname) then
      user_histfname = trim(casename)//'.ww3.hi.'
    end if
    if (use_user_restname) then
      user_restfname = trim(casename)//'.ww3.r.'
    end if

    fnmpre = './'

    call ESMF_LogWrite(trim(subname)//' call read_shel_config', ESMF_LOGMSG_INFO)
    call read_shel_config(mpi_comm, mds) ! time0_overwrite=time0, timen_overwrite=timen)

    call ESMF_LogWrite(trim(subname)//' call w3init', ESMF_LOGMSG_INFO)
    call w3init ( 1, .false., 'ww3', mds, ntrace, odat, flgrd, flgr2, flgd, flg2, &
         npts, x, y, pnames, iprt, prtfrm, mpi_comm )

    write(logmsg,'(A,4f10.2)') trim(subname)//': mod_def timesteps file  ',dtmax,dtcfl,dtcfli,dtmin
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO)
    call NUOPC_CompAttributeGet(gcomp, name='dt_in', isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
      call NUOPC_CompAttributeGet(gcomp, name='dt_in', value=cvalue, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      read(cvalue,*)dt_in
      dtmax  = real(dt_in(1),4)
      dtcfl  = real(dt_in(2),4)
      dtcfli = real(dt_in(3),4)
      dtmin  = real(dt_in(4),4)
      write(logmsg,'(A,4f10.2)') trim(subname)//': mod_def timesteps reset ',dtmax,dtcfl,dtcfli,dtmin
      call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO)
    end if
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)
  end subroutine waveinit_ufs

end module wav_comp_nuopc
