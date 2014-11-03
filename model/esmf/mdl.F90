!-------------------------------------------------------------------------------
! A test Wavewatch III coupled application model component
!
! Author:
!   Tim Campbell
!   Naval Research Laboratory
!   March 2014
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! ESMF macros for logging
!-------------------------------------------------------------------------------
#define FILENAME "mdl.F90"
#define CONTEXT  line=__LINE__,file=FILENAME
#define PASSTHRU msg=ESMF_LOGERR_PASSTHRU,CONTEXT

!-------------------------------------------------------------------------------
! Define real kind for data passed through ESMF interface
!-------------------------------------------------------------------------------
#if defined(REAL8)
#define _ESMF_KIND_RX _ESMF_KIND_R8
#define ESMF_KIND_RX ESMF_KIND_R8
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R8
#else
#define _ESMF_KIND_RX _ESMF_KIND_R4
#define ESMF_KIND_RX ESMF_KIND_R4
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R4
#endif


module MDL

  use ESMF
  use NUOPC
  use NUOPC_Model, only: &
    model_routine_SS            => routine_SetServices, &
    model_type_IS               => type_InternalState, &
    model_label_IS              => label_InternalState, &
    model_label_DataInitialize  => label_DataInitialize, &
    model_label_SetClock        => label_SetClock, &
    model_label_Advance         => label_Advance

  implicit none

  private

  public SetServices

  character (*), parameter :: defaultVerbosity = 'low'
  character (*), parameter :: label_InternalState = 'InternalState'
  character (*), parameter :: inputAlarmName = 'InputAlarm'
  integer      , parameter :: maxFields = 15

  integer, parameter :: modTypeConstant = 0
  integer, parameter :: modTypeTendency = 1
  integer, parameter :: modTypeForecast = 2
  integer, parameter :: modTypeHindcast = 3

  type type_InternalStateStruct
    logical                         :: verbose
    integer                         :: modType
    type(ESMF_TimeInterval)         :: timeStep
    real(ESMF_KIND_RX)              :: dtRatio
    character(ESMF_MAXSTR)          :: dataDir
    integer                         :: numf
    logical                ,pointer :: isActive(:) => null()
    character(ESMF_MAXSTR) ,pointer :: sname(:) => null()
    character(6)           ,pointer :: fname(:) => null()
    type(ESMF_Field)       ,pointer :: field(:) => null()
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
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    character(ESMF_MAXSTR)        :: verbosity
    type(type_InternalState)      :: is
    integer                       :: localrc, stat

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! check supported component names
    select case (trim(cname))
    case ('ATM','OCN','ICE')
    case default
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg='MOD: unsupported component name'//trim(cname))
      return  ! bail out
    endselect

    ! determine verbosity
    call ESMF_AttributeGet(gcomp, name='Verbosity', value=verbosity, &
      defaultValue=defaultVerbosity, convention='NUOPC', purpose='General', rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (trim(verbosity)=='high') then
      verbose = .true.
    else
      verbose = .false.
    endif

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered SetServices', ESMF_LOGMSG_INFO)

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    is%wrap%verbose = verbose

    ! initialize timers
    is%wrap%numwt = 6
    allocate(is%wrap%wtnam(is%wrap%numwt), is%wrap%wtcnt(is%wrap%numwt), &
      is%wrap%wtime(is%wrap%numwt), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of wall timer memory failed.', &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    is%wrap%wtnam(1) = 'InitializeP0'
    is%wrap%wtnam(2) = 'InitializeP1'
    is%wrap%wtnam(3) = 'InitializeP2'
    is%wrap%wtnam(4) = 'DataInitialize'
    is%wrap%wtnam(5) = 'ModelAdvance'
    is%wrap%wtnam(6) = 'Finalize'
    is%wrap%wtcnt(:) = 0
    is%wrap%wtime(:) = 0d0

    ! the NUOPC model component will register the generic methods
    call model_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set entry points for initialize methods
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP2, phase=2, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set entry point for finalize method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, phase=1, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! attach specializing method(s)
    call ESMF_MethodAdd(gcomp, label=model_label_SetClock, &
         userRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_MethodAdd(gcomp, label=model_label_DataInitialize, &
         userRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_MethodAdd(gcomp, label=model_label_Advance, &
         userRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving SetServices', ESMF_LOGMSG_INFO)

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
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
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
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered InitializeP0', ESMF_LOGMSG_INFO)

    ! define required initialization phases
    ! * >= IPDv02 supports satisfying inter-model data dependencies
    !   during initialization
    ! * >= IPDv03 supports the transfer of ESMF Grid & Mesh objects
    !   between Model and/or Mediator components during initialization
    ! IPDv03p2: unspecified by NUOPC -- not required
    ! IPDv03p4: relevant for TransferActionGeomObject=="accept"
    ! IPDv03p5: relevant for TransferActionGeomObject=="accept"
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p1","IPDv03p3","IPDv03p6","IPDv03p7"/), rc=rc)
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
    integer                       :: localrc, stat
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

    ! get data directory
    label=trim(cname)//'_work_dir:'
    call ESMF_ConfigGetAttribute(config, is%wrap%dataDir, label=trim(label), &
      default='.', rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! allocate field/data arrays
    allocate(is%wrap%isActive(maxFields), &
      is%wrap%sname(maxFields), is%wrap%fname(maxFields), &
      is%wrap%field(maxFields), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state data arrays failed.', &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    is%wrap%isActive = .false.

    ! set attributes for fields
    i = 0
    select case (trim(cname))
    case ('ATM')
      i = i+1
      is%wrap%sname(i) = 'eastward_wind_at_10m_height'
      is%wrap%fname(i) = 'uutrue'
      i = i+1
      is%wrap%sname(i) = 'northward_wind_at_10m_height'
      is%wrap%fname(i) = 'vvtrue'
    case ('OCN')
      i = i+1
      is%wrap%sname(i) = 'sea_surface_height_above_sea_level'
      is%wrap%fname(i) = 'seahgt'
      i = i+1
      is%wrap%sname(i) = 'sea_surface_temperature'
      is%wrap%fname(i) = 'seatmp'
      i = i+1
      is%wrap%sname(i) = 'sea_surface_salinity'
      is%wrap%fname(i) = 'salint'
      i = i+1
      is%wrap%sname(i) = 'surface_eastward_sea_water_velocity'
      is%wrap%fname(i) = 'uucurr'
      i = i+1
      is%wrap%sname(i) = 'surface_northward_sea_water_velocity'
      is%wrap%fname(i) = 'vvcurr'
    case ('ICE')
!     i = i+1
!     is%wrap%sname(i) = 'sea_ice_concentration'
!     is%wrap%fname(i) = 'icecon'
    endselect
    numf = i
    is%wrap%numf = numf
    if (numf.gt.maxFields) then
      write(msgString,'(a,i3)') trim(cname)//': increase maxFields to ',numf
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
      return  ! bail out
    endif

    ! advertise exportable fields
    do i=1,numf
      call NUOPC_StateAdvertiseField(exportState, trim(is%wrap%sname(i)), &
        name=trim(is%wrap%fname(i)), rc=rc)
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

  subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
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
    type(ESMF_ArraySpec)          :: arraySpec2d
    type(ESMF_Grid)               :: grid

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
    call ESMF_LogWrite(trim(cname)//': entered InitializeP2', ESMF_LOGMSG_INFO)

    ! query Component for its config & vm
    call ESMF_GridCompGet(gcomp, config=config, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! flag connected export fields and remove unconnected
    n = 0
    do i = 1,is%wrap%numf
      is%wrap%isActive(i) = NUOPC_StateIsFieldConnected(exportState, &
        is%wrap%fname(i), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
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

    ! realize active export fields
    do i = 1,is%wrap%numf
      if (.not.is%wrap%isActive(i)) cycle
      is%wrap%field(i) = ESMF_FieldCreate(grid, arraySpec2d, &
        name=is%wrap%fname(i), indexFlag=ESMF_INDEX_GLOBAL, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call NUOPC_StateRealizeField(exportState, is%wrap%field(i), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving InitializeP2', ESMF_LOGMSG_INFO)

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
    integer                       :: localrc, stat
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

    ! set Updated Field Attribute to "true", indicating to the IPDv03p7
    ! generic code to set the timestamp for these fields
    do i=1,is%wrap%numf
      if (.not.is%wrap%isActive(i)) cycle
      call ESMF_AttributeSet(is%wrap%field(i), name="Updated", value="true", &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    ! set InitializeDataComplete Attribute to "true", indicating to the IPDv03p7
    ! generic code that all inter-model data dependencies are satisfied
    ! *** this is required since not all import fields are required for init ***
    call ESMF_AttributeSet(gcomp, name="InitializeDataComplete", value="true", &
      convention="NUOPC", purpose="General", rc=rc)
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
    integer                       :: localrc, stat
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
      call NUOPC_ClockPrintCurrTime(clock, &
        '-->Advancing '//trim(cname)//' from: ', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call NUOPC_ClockPrintStopTime(clock, &
        '-----------------> to: ', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
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

  subroutine Finalize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
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
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

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
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif
    if (associated(is%wrap%wtcnt)) then
      deallocate(is%wrap%wtcnt, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of wtcnt array failed.', &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif
    if (associated(is%wrap%wtime)) then
      deallocate(is%wrap%wtime, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of wtime array failed.', &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of internal state memory failed.', &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

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
    integer                       :: localrc, stat
    type(ESMF_Config)             :: config
    character(ESMF_MAXSTR)        :: label
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: zeroti
    type(ESMF_TimeInterval)       :: timeStep
    integer(ESMF_KIND_I4)         :: time(3)
    type(ESMF_Alarm)              :: inputAlarm
    real(ESMF_KIND_R8)            :: dtRatio

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

    ! get time step (data input interval) from config
    label=trim(cname)//'_time_step:'
    call ESMF_GridCompGet(gcomp, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ConfigGetAttribute(config, time, count=3, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_TimeIntervalSet(is%wrap%timeStep, h=time(1), m=time(2), s=time(3), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (mod(is%wrap%timeStep,timeStep) /= zeroti) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': '//trim(label)// &
        ' must be a multiple of driver time step')
      return  ! bail out
    endif

    ! setup alarm for reading input fields
    inputAlarm = ESMF_AlarmCreate(clock, ringInterval=is%wrap%timeStep, &
      name=inputAlarmName, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! compute ratio of driver timeStep to model timeStep
    dtRatio = timeStep / is%wrap%timeStep
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
    integer                       :: localrc, stat
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

  subroutine PrintTimers(cname, wtnam, wtcnt, wtime)
    character(*)          :: cname
    character(*)          :: wtnam(:)
    integer(ESMF_KIND_I4) :: wtcnt(:)
    real(ESMF_KIND_R8)    :: wtime(:)

    ! local variables
    character(ESMF_MAXSTR) :: msg
    integer(ESMF_KIND_I4)  :: k

    write(msg,1) trim(cname),'timer','count','time'
    call ESMF_LogWrite(TRIM(msg), ESMF_LOGMSG_INFO)
    do k=lbound(wtcnt,1),ubound(wtcnt,1)
      write(msg,2) trim(cname),trim(wtnam(k)),wtcnt(k),wtime(k)
      call ESMF_LogWrite(TRIM(msg), ESMF_LOGMSG_INFO)
    enddo

1   format(a,': wtime: ',a20,a10,a14)
2   format(a,': wtime: ',a20,i10,e14.6)

  end subroutine

end module
