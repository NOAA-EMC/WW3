#include "macros.h"
!-------------------------------------------------------------------------------
! A test coupled application model component
!
! Author:
!   Tim Campbell
!   Naval Research Laboratory
!   November 2014
!-------------------------------------------------------------------------------

module MDL

  use ESMF
  use NUOPC
! use NUOPC_Model, parent_SetServices => SetServices
  use NUOPC_Model, only: parent_SetServices => SetServices, &
    label_DataInitialize, label_SetClock, label_Advance, label_Finalize
  use UTL

  implicit none

  private

  public SetServices

  character (*), parameter :: label_InternalState = 'InternalState'
  character (*), parameter :: inputAlarmName = 'InputAlarm'
  integer      , parameter :: maxFields = 25

  integer, parameter :: modTypeConstant = 0
  integer, parameter :: modTypeTendency = 1
  integer, parameter :: modTypeForecast = 2
  integer, parameter :: modTypeHindcast = 3

  type type_InternalStateStruct
    logical                         :: verbose
    integer                         :: modType
    type(ESMF_TimeInterval)         :: timeStep
    type(ESMF_TimeInterval)         :: inputInterval
    real(ESMF_KIND_RX)              :: dtRatio
    character(ESMF_MAXSTR)          :: dataDir
    integer                         :: numf
    logical                ,pointer :: isActive(:) => null()
    character(ESMF_MAXSTR) ,pointer :: sname(:) => null()
    character(6)           ,pointer :: fname(:) => null()
    type(ESMF_Field)       ,pointer :: field(:) => null()
    logical                         :: realizeAllExport
    integer(ESMF_KIND_I4)           :: numwt
    character(ESMF_MAXSTR) ,pointer :: wtnam(:) => null()
    integer(ESMF_KIND_I4)  ,pointer :: wtcnt(:) => null()
    real(ESMF_KIND_R8)     ,pointer :: wtime(:) => null()
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: msgString
    type(type_InternalState)      :: is
    integer                       :: lrc, stat

    rc = ESMF_SUCCESS

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! initialize timers
    is%wrap%numwt = 6
    allocate(is%wrap%wtnam(is%wrap%numwt), is%wrap%wtcnt(is%wrap%numwt), &
      is%wrap%wtime(is%wrap%numwt), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of wall timer memory failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    is%wrap%wtnam(1) = 'InitializeP0'
    is%wrap%wtnam(2) = 'InitializeP1'
    is%wrap%wtnam(3) = 'InitializeP3'
    is%wrap%wtnam(4) = 'DataInitialize'
    is%wrap%wtnam(5) = 'ModelAdvance'
    is%wrap%wtnam(6) = 'Finalize'
    is%wrap%wtcnt(:) = 0
    is%wrap%wtime(:) = 0d0

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, parent_SetServices, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set initialize phase 0 requires use of ESMF method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set entry points for initialize methods
    ! >= IPDv03 supports satisfying inter-model data dependencies and the transfer of ESMF
    ! Grid & Mesh objects between Model and/or Mediator components during initialization
    ! IPDv03p1: advertise import & export fields
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    ! IPDv03p2: unspecified by NUOPC -- not required
    ! IPDv03p3: realize import & export fields
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    ! IPDv03p4: relevant for TransferActionGeomObject=="accept"
    ! IPDv03p5: relevant for TransferActionGeomObject=="accept"
    ! IPDv03p6: check compatibility of fields connected status
    ! IPDv03p7: handle field data initialization

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=label_SetClock, &
         specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=label_DataInitialize, &
         specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, &
         specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=label_Finalize, &
         specRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    character(ESMF_MAXSTR)        :: verbosity
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer, parameter            :: it1=1, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    integer                       :: i

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! determine verbosity
    call NUOPC_CompAttributeGet(gcomp, name='Verbosity', value=verbosity, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (trim(verbosity)=='high') then
      is%wrap%verbose = .true.
    else
      is%wrap%verbose = .false.
    endif
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered InitializeP0', ESMF_LOGMSG_INFO)

    ! check supported component names
    select case (trim(cname))
    case ('ATM')
    case ('OCN')
    case ('WAV')
    case ('ICE')
    case default
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg='MDL: unsupported component name'//trim(cname))
      return ! bail out
    endselect

    ! switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving InitializeP0', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer, parameter            :: it1=2, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    type(ESMF_VM)                 :: vm
    type(ESMF_Config)             :: config
    character(ESMF_MAXSTR)        :: label
    character(ESMF_MAXSTR)        :: inpstr
    integer                       :: i, numf

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered InitializeP1', ESMF_LOGMSG_INFO)

    ! query Component for its config & vm
    call ESMF_GridCompGet(gcomp, config=config, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get realize all export flag
    label=trim(cname)//'_realize_all_export:'
    call ESMF_ConfigGetAttribute(config, is%wrap%realizeAllExport, &
      default=.false., label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get data directory
    label=trim(cname)//'_work_dir:'
    call ESMF_ConfigGetAttribute(config, is%wrap%dataDir, label=trim(label), default='.', rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! allocate field/data arrays
    allocate(is%wrap%isActive(maxFields), &
      is%wrap%sname(maxFields), is%wrap%fname(maxFields), &
      is%wrap%field(maxFields), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state data arrays failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    is%wrap%isActive = .false.

    ! set attributes for fields
    i = 0
    select case (trim(cname))
    case ('ATM')
      i = i+1
      is%wrap%sname(i) = 'air_pressure_at_sea_level'
      is%wrap%fname(i) = 'slpres' !'pres'
      i = i+1
      is%wrap%sname(i) = 'eastward_wind_at_10m_height'
      is%wrap%fname(i) = 'uutrue' !'wnd_utru'
      i = i+1
      is%wrap%sname(i) = 'northward_wind_at_10m_height'
      is%wrap%fname(i) = 'vvtrue' !'wnd_vtru'
      i = i+1
      is%wrap%sname(i) = 'magnitude_of_surface_downward_stress'
      is%wrap%fname(i) = 'wstres' !'wnd_strs'
      i = i+1
      is%wrap%sname(i) = 'air_temperature_at_2m_height'
      is%wrap%fname(i) = 'airtmp' !'air_temp'
      i = i+1
      is%wrap%sname(i) = 'relative_humidity_at_2m_height'
      is%wrap%fname(i) = 'relhum' !'rltv_hum'
      i = i+1
      is%wrap%sname(i) = 'surface_net_downward_shortwave_flux'
      is%wrap%fname(i) = 'solflx' !'sol_rad'
      i = i+1
      is%wrap%sname(i) = 'surface_net_downward_longwave_flux'
      is%wrap%fname(i) = 'lonflx' !'ir_flux'
    case ('OCN')
      i = i+1
      is%wrap%sname(i) = 'sea_surface_temperature'
      is%wrap%fname(i) = 'seatmp'
      i = i+1
      is%wrap%sname(i) = 'sea_surface_salinity'
      is%wrap%fname(i) = 'salint'
      i = i+1
      is%wrap%sname(i) = 'sea_surface_height_above_sea_level'
      is%wrap%fname(i) = 'seahgt'
      i = i+1
      is%wrap%sname(i) = 'surface_eastward_sea_water_velocity'
      is%wrap%fname(i) = 'uucurr'
      i = i+1
      is%wrap%sname(i) = 'surface_northward_sea_water_velocity'
      is%wrap%fname(i) = 'vvcurr'
    case ('WAV')
      i = i+1
      is%wrap%sname(i) = 'wave_induced_charnock_parameter'
      is%wrap%fname(i) = 'charno'
      i = i+1
      is%wrap%sname(i) = 'surface_total_wave_induced_stress'
      is%wrap%fname(i) = 'wvstrs'
      i = i+1
      is%wrap%sname(i) = 'surface_eastward_wave_induced_stress'
      is%wrap%fname(i) = 'wvstru'
      i = i+1
      is%wrap%sname(i) = 'surface_northward_wave_induced_stress'
      is%wrap%fname(i) = 'wvstrv'
      i = i+1
      is%wrap%sname(i) = 'eastward_stokes_drift_current'
      is%wrap%fname(i) = 'uscurr'
      i = i+1
      is%wrap%sname(i) = 'northward_stokes_drift_current'
      is%wrap%fname(i) = 'vscurr'
      i = i+1
      is%wrap%sname(i) = 'eastward_wave_bottom_current'
      is%wrap%fname(i) = 'wbcuru'
      i = i+1
      is%wrap%sname(i) = 'northward_wave_bottom_current'
      is%wrap%fname(i) = 'wbcurv'
      i = i+1
      is%wrap%sname(i) = 'wave_bottom_current_radian_frequency'
      is%wrap%fname(i) = 'wbcurf'
      i = i+1
      is%wrap%sname(i) = 'eastward_wave_radiation_stress_gradient'
      is%wrap%fname(i) = 'wavsgu'
      i = i+1
      is%wrap%sname(i) = 'northward_wave_radiation_stress_gradient'
      is%wrap%fname(i) = 'wavsgv'
    case ('ICE')
      i = i+1
      is%wrap%sname(i) = 'sea_ice_concentration'
      is%wrap%fname(i) = 'icecon'
    endselect
    numf = i
    is%wrap%numf = numf
    if (numf.gt.maxFields) then
      write(msgString,'(a,i3)') trim(cname)//': increase maxFields to ',numf
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
      return ! bail out
    endif

    ! advertise exportable fields
    do i=1,numf
      call NUOPC_Advertise(exportState, &
        StandardName=trim(is%wrap%sname(i)), name=trim(is%wrap%fname(i)), &
        TransferOfferGeomObject="will provide", rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    ! report advertised export fields
    write(msgString,'(a,i0,a)') trim(cname)// &
      ': List of advertised export fields(',numf,'):'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    write(msgString,'(a,a5,a,a10,a3,a)') trim(cname)// &
      ': ','index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    do i=1,numf
      write(msgString,'(a,i5,a,a10,a3,a)') trim(cname)// &
        ': ',i,' ',trim(is%wrap%fname(i)),' ',trim(is%wrap%sname(i))
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving InitializeP1', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer, parameter            :: it1=3, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    integer, parameter            :: localDE=0
    type(ESMF_VM)                 :: vm
    type(ESMF_Config)             :: config
    character(ESMF_MAXSTR)        :: label
    character(ESMF_MAXSTR)        :: inpstr
    type(ESMF_Time)               :: startTime
    integer                       :: i, n
    type(ESMF_ArraySpec)          :: arraySpec2d, arraySpec3d
    type(ESMF_Grid)               :: grid
    logical                       :: inc3d
    logical                       :: isConnected

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered InitializeP3', ESMF_LOGMSG_INFO)

    ! query Component for its config & vm
    call ESMF_GridCompGet(gcomp, config=config, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! flag connected export fields
    ! remove unconnected if not realize all export
    n = 0
    do i = 1,is%wrap%numf
      isConnected = NUOPC_IsConnected(exportState, is%wrap%fname(i), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      is%wrap%isActive(i) = isConnected .or. is%wrap%realizeAllExport
      if (is%wrap%isActive(i)) then
        n = n + 1
      else
        call ESMF_StateRemove(exportState, (/is%wrap%fname(i)/), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
    enddo

    ! report realized export fields
    write(msgString,'(a,i0,a)') trim(cname)// &
      ': List of realized export fields(',n,'):'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    write(msgString,'(a,a5,a,a10,a3,a)') trim(cname)// &
      ': ','index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    n = 0
    do i = 1,is%wrap%numf
      if (.not.is%wrap%isActive(i)) cycle
      n = n + 1
      write(msgString,'(a,i5,a,a10,a3,a)') trim(cname)// &
        ': ',n,' ',trim(is%wrap%fname(i)),' ',trim(is%wrap%sname(i))
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! if no active export fields, then skip the rest
    if (.not.any(is%wrap%isActive)) goto 1

    ! query Component for its config & vm
    call ESMF_GridCompGet(gcomp, config=config, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get time information from clock
    call ESMF_ClockGet(clock, startTime=startTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get coord and create grid
    !TODO

    ! create arraySpec
    call ESMF_ArraySpecSet(arraySpec2d, rank=2, typeKind=ESMF_TYPEKIND_RX, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ArraySpecSet(arraySpec3d, rank=3, typeKind=ESMF_TYPEKIND_RX, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! realize active export fields
    do i = 1,is%wrap%numf
      if (.not.is%wrap%isActive(i)) cycle
      is%wrap%field(i) = ESMF_FieldCreate(grid, arraySpec2d, &
        name=is%wrap%fname(i), indexFlag=ESMF_INDEX_GLOBAL, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call NUOPC_Realize(exportState, is%wrap%field(i), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving InitializeP3', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer, parameter            :: it1=4, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    integer, parameter            :: localDE=0
    integer                       :: localPet
    integer                       :: i

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! query the Component for PET info
    call ESMF_GridCompGet(gcomp, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! report
    write(msgString,'(a)') trim(cname)//': entered DataInitialize'
    if (verbose) call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    if (localPet.eq.0) write(*,'(///a)') trim(msgString)

    ! set export fields
    call SetExport(gcomp, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! report all import dependencies are satisfied
    write(msgString,'(a)') trim(cname)//': all inter-model data dependencies SATISFIED'
    if (verbose) call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    if (localPet.eq.0) write(*,'(a)') trim(msgString)

    ! set Updated Field Attribute to "true", indicating to the
    ! generic code to set the timestamp for these fields
    do i=1,is%wrap%numf
      if (.not.is%wrap%isActive(i)) cycle
      call NUOPC_SetAttribute(is%wrap%field(i), name="Updated", value="true", rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    ! set InitializeDataComplete Attribute to "true", indicating to the
    ! generic code that all inter-model data dependencies are satisfied
    call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! report
1   write(msgString,'(a)') trim(cname)//': leaving DataInitialize'
    if (verbose) call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    if (localPet.eq.0) write(*,'(/a)') trim(msgString)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer, parameter            :: it1=5, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    type(ESMF_Clock)              :: clock
    integer                       :: localPet

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered ModelAdvance', ESMF_LOGMSG_INFO)

    ! query the Component for PET info
    call ESMF_GridCompGet(gcomp, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the Component for its clock
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (localPet.eq.0) then
      write(*,'(///)')
      call ESMF_ClockPrint(clock, options='currTime', &
        preString='-->Advancing '//trim(cname)//' from: ')
      call ESMF_ClockPrint(clock, options='stopTime', &
        preString='-----------------> to: ')
    endif

    ! set export fields
    call SetExport(gcomp, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving ModelAdvance', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer, parameter            :: it1=6, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    integer                       :: i

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered Finalize', ESMF_LOGMSG_INFO)

    ! destroy fields
    do i = 1,is%wrap%numf
      if (.not.is%wrap%isActive(i)) cycle
      call ESMF_FieldDestroy(is%wrap%field(i),rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    ! deallocate field/data arrays
    deallocate(is%wrap%isActive, &
      is%wrap%sname, is%wrap%fname, &
      is%wrap%field, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of internal state data arrays failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

    ! print timing
    call PrintTimers(trim(cname), is%wrap%wtnam, is%wrap%wtcnt, is%wrap%wtime)

    ! deallocate timers
    if (associated(is%wrap%wtnam)) then
      deallocate(is%wrap%wtnam, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of wtnam array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif
    if (associated(is%wrap%wtcnt)) then
      deallocate(is%wrap%wtcnt, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of wtcnt array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif
    if (associated(is%wrap%wtime)) then
      deallocate(is%wrap%wtime, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of wtime array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of internal state memory failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving Finalize', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    type(ESMF_Config)             :: config
    character(ESMF_MAXSTR)        :: label
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: zeroti
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_TimeInterval)       :: inputInterval
    type(ESMF_Alarm)              :: inputAlarm
    real(ESMF_KIND_R8)            :: dtRatio
    integer(ESMF_KIND_I4)         :: time(3)
    logical                       :: isPresent

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! set zero time interval
    call ESMF_TimeIntervalSet(zeroti, s=0, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get clock and config
    call ESMF_GridCompGet(gcomp, clock=clock, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get driver time step (coupling interval)
    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get time step from config
    is%wrap%timeStep = timeStep
    label=trim(cname)//'_time_step:'
    call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=isPresent, rc=rc)
    if (isPresent.and.rc.eq.ESMF_SUCCESS) then
      call ESMF_ConfigGetAttribute(config, time, count=3, label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_TimeIntervalSet(is%wrap%timeStep, h=time(1), m=time(2), s=time(3), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endif
    if (mod(timeStep,is%wrap%timeStep) /= zeroti) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': '//trim(label)// &
        ' must be divisible into the driver time step')
      return ! bail out
    endif

    ! get data input interval from config
    is%wrap%inputInterval = timeStep
    label=trim(cname)//'_input_interval:'
    call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=isPresent, rc=rc)
    if (isPresent.and.rc.eq.ESMF_SUCCESS) then
      call ESMF_ConfigGetAttribute(config, time, count=3, label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_TimeIntervalSet(is%wrap%inputInterval, h=time(1), m=time(2), s=time(3), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endif
    if (mod(is%wrap%inputInterval,timeStep) /= zeroti) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': '//trim(label)// &
        ' must be a multiple of driver time step')
      return ! bail out
    endif

    ! setup alarm for reading input fields
    inputAlarm = ESMF_AlarmCreate(clock, ringInterval=is%wrap%inputInterval, &
      name=inputAlarmName, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! compute ratio of driver timeStep to model input interval
    dtRatio = timeStep / is%wrap%inputInterval
    is%wrap%dtRatio = real(dtRatio,ESMF_KIND_RX)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetExport(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    type(ESMF_VM)                 :: vm
    type(ESMF_Clock)              :: clock
    type(ESMF_Time)               :: startTime, currTime
    type(ESMF_Alarm)              :: inputAlarm
    integer                       :: i

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! if no active export fields, then return
    if (.not.any(is%wrap%isActive)) return

    ! query the Component for its clock & vm
    call ESMF_GridCompGet(gcomp, clock=clock, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get time information from clock
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get input alarm from clock
    call ESMF_ClockGetAlarm(clock, inputAlarmName, inputAlarm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! time for new input
    if (ESMF_AlarmIsRinging(inputAlarm)) then
      do i=1,is%wrap%numf
        ! skip if not active
        if (.not.is%wrap%isActive(i)) cycle
        ! read next field
        ! TODO
      enddo
      ! turn off alarm
      call ESMF_AlarmRingerOff(inputAlarm)
    endif

  end subroutine

end module
