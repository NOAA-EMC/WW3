!-------------------------------------------------------------------------------
! A test Wavewatch III coupled application driver component
!
! Author:
!   Tim Campbell
!   Naval Research Laboratory
!   March 2014
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! ESMF macros for logging
!-------------------------------------------------------------------------------
#define FILENAME "esm.F90"
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


module ESM

  use ESMF
  use NUOPC
  use NUOPC_Driver, only: &
    Driver_routine_SS             => routine_SetServices, &
    Driver_type_IS                => type_InternalState, &
    Driver_label_IS               => label_InternalState, &
    Driver_label_SetModelCount    => label_SetModelCount, &
    Driver_label_SetModelPetLists => label_SetModelPetLists, &
    Driver_label_SetModelServices => label_SetModelServices, &
    Driver_label_Finalize         => label_Finalize

  use FRONT_CON, only: cplSS => SetServices
  use FRONT_ATM, only: atmSS => SetServices
  use FRONT_OCN, only: ocnSS => SetServices
  use FRONT_WAV, only: wavSS => SetServices
  use FRONT_ICE, only: iceSS => SetServices

  implicit none
  save
  private

  public SetServices

  integer     , parameter :: maxModCount = 4
  logical     , parameter :: defaultVerbose = .false.
  logical     , parameter :: defaultModActive = .false.
  character(*), parameter :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
    logical      :: verbose
    integer      :: modCount=0
    integer      :: atm=0, ocn=0, wav=0, ice=0
    character(3) :: modName(0:maxModCount)
    logical      :: modActive(0:maxModCount)
    character(8) :: conName(0:maxModCount,0:maxModCount)
    logical      :: conActive(0:maxModCount,0:maxModCount)
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
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(Driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    logical     ,pointer               :: verbose
    integer     ,pointer               :: modCount
    integer     ,pointer               :: atm, ocn, wav, ice
    character(3),pointer               :: modName(:)
    logical     ,pointer               :: modActive(:)
    character(8),pointer               :: conName(:,:)
    logical     ,pointer               :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: localrc, stat
    integer                            :: i, j, k
    logical                            :: configIsPresent
    character(ESMF_MAXSTR)             :: label
    integer(ESMF_KIND_I4)              :: time(6)
    type(ESMF_Time)                    :: startTime
    type(ESMF_Time)                    :: stopTime
    type(ESMF_TimeInterval)            :: runDuration
    type(ESMF_TimeInterval)            :: timeStep
    type(ESMF_TimeInterval)            :: zeroTimeInterval
    type(ESMF_Clock)                   :: internalClock

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! check/get the config (config is required)
    call ESMF_GridCompGet(gcomp, configIsPresent=configIsPresent, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (configIsPresent) then
      call ESMF_GridCompGet(gcomp, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    else
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config')
      return  ! bail out
    endif

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    modName => is%wrap%modName
    modActive => is%wrap%modActive
    conName => is%wrap%conName
    conActive => is%wrap%conActive

    ! set model count, model index mapping, and model names
    modCount = modCount + 1
    atm = modCount
    modName(atm) = 'ATM'
    modCount = modCount + 1
    ocn = modCount
    modName(ocn) = 'OCN'
    modCount = modCount + 1
    wav = modCount
    modName(wav) = 'WAV'
    modCount = modCount + 1
    ice = modCount
    modName(ice) = 'ICE'

    ! report model indexing
    do i = 1,modCount
      write(msgString,'(a,i0)') trim(cname)//': '//modName(i)//' model index: ',i
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! process config for verbose
    label = 'verbose:'
    call ESMF_ConfigGetAttribute(config, verbose, default=defaultVerbose, &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! process config for modActive
    do i = 1,modCount
      label = modName(i)//'_active:'
      call ESMF_ConfigGetAttribute(config, modActive(i), default=defaultModActive, &
        label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        call ESMF_LogWrite(trim(cname)//': ESMF_ConfigGetAttribute: '// &
          trim(label), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      ! report active/inactive models
      if (modActive(i)) then
        write(msgString,'(a)') trim(cname)//': '//modName(i)//' is     active'
      else
        write(msgString,'(a)') trim(cname)//': '//modName(i)//' is not active'
      endif
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! set connector names
    do j = 1,modCount
    do i = 1,modCount
      conName(i,j) = modName(i)//'->'//modName(j)
    enddo
    enddo

    ! set active connectors
    conActive = .false.
    conActive(atm,wav) = modActive(atm).and.modActive(wav)
    conActive(ocn,wav) = modActive(ocn).and.modActive(wav)
    conActive(ice,wav) = modActive(ice).and.modActive(wav)
    conActive(wav,atm) = modActive(wav).and.modActive(atm)
    conActive(wav,ocn) = modActive(wav).and.modActive(ocn)
    conActive(wav,ice) = modActive(wav).and.modActive(ice)

    ! report active connections
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      write(msgString,'(a)') trim(cname)//': '//conName(i,j)//' connector is active'
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo
    enddo

    ! process config for required timeStep input
    label = 'time_step:'
    call ESMF_ConfigGetAttribute(config, time(4:6), count=3, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config input: '// &
        trim(label)//' hh mm ss')
      return  ! bail out
    endif
    write(msgString,'(a,3(a,i0))') trim(cname)//': '//trim(label),(' ',time(k),k=4,6)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_TimeIntervalSet(timeStep, h=time(4), m=time(5), s=time(6), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! process config for required startTime input
    label = 'start_time:'
    call ESMF_ConfigGetAttribute(config, time, count=6, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config input: '// &
        trim(label)//' YYYY MM DD hh mm ss')
      return  ! bail out
    endif
    write(msgString,'(a,6(a,i0))') trim(cname)//': '//trim(label),(' ',time(k),k=1,6)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_TimeSet(startTime, yy=time(1), mm=time(2), dd=time(3), &
      h=time(4), m=time(5), s=time(6), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! process config for required stopTime input
    label = 'stop_time:'
    call ESMF_ConfigGetAttribute(config, time, count=6, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config input: '// &
        trim(label)//' YYYY MM DD hh mm ss')
      return  ! bail out
    endif
    write(msgString,'(a,6(a,i0))') trim(cname)//': '//trim(label),(' ',time(k),k=1,6)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_TimeSet(stopTime, yy=time(1), mm=time(2), dd=time(3), &
      h=time(4), m=time(5), s=time(6), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! check that simulation time is multiple of timeStep
    runDuration = stopTime - startTime
    call ESMF_TimeIntervalSet(zeroTimeInterval, h=0, m=0, s=0, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (mod(runDuration,timeStep) .ne. zeroTimeInterval) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': run duration is not a multiple of timeStep')
      return  ! bail out
    endif

    ! NUOPC_Driver registers the generic methods
    call Driver_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! attach specializing method(s)
    call ESMF_MethodAdd(gcomp, label=Driver_label_SetModelCount, &
      userRoutine=SetModelCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_MethodAdd(gcomp, label=Driver_label_SetModelPetLists, &
      userRoutine=SetModelPetLists, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_MethodAdd(gcomp, label=Driver_label_SetModelServices, &
      userRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! create/set the driver clock
    internalClock = ESMF_ClockCreate(name=trim(cname)//'_clock', &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridCompSet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelCount(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(Driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    logical     ,pointer               :: verbose
    integer     ,pointer               :: modCount
    integer     ,pointer               :: atm, ocn, wav, ice
    character(3),pointer               :: modName(:)
    logical     ,pointer               :: modActive(:)
    character(8),pointer               :: conName(:,:)
    logical     ,pointer               :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the component for its config
    call ESMF_GridCompGet(gcomp, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, Driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    modName => is%wrap%modName
    modActive => is%wrap%modActive
    conName => is%wrap%conName
    conActive => is%wrap%conActive

    ! set the modelCount
    superIS%wrap%modelCount = modCount

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelPetLists(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(Driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    logical     ,pointer               :: verbose
    integer     ,pointer               :: modCount
    integer     ,pointer               :: atm, ocn, wav, ice
    character(3),pointer               :: modName(:)
    logical     ,pointer               :: modActive(:)
    character(8),pointer               :: conName(:,:)
    logical     ,pointer               :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: localrc, stat
    integer                            :: i, j
    integer                            :: k, l, m, n, p
    integer                            :: k1, k2
    integer                            :: modStart
    integer                            :: petCount, npet
    integer                            :: modPetCount(maxModCount)
    integer     , pointer              :: modPetList(:)
    character(ESMF_MAXSTR)             :: label
    character(ESMF_MAXSTR)             :: petLayoutOption

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the component for its config
    call ESMF_GridCompGet(gcomp, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, Driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    modName => is%wrap%modName
    modActive => is%wrap%modActive
    conName => is%wrap%conName
    conActive => is%wrap%conActive

    ! get the petCount
    call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! process config for pet_layout_option:
    label = 'pet_layout_option:'
    call ESMF_ConfigGetAttribute(config, petLayoutOption, default='sequential', &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_LogWrite(trim(cname)//': '//trim(label)//' '//trim(petLayoutOption), &
    ESMF_LOGMSG_INFO)

    ! set the model petLists based on petLayoutOption
    select case (trim(petLayoutOption))

    ! pet_layout_option: sequential
    !   * active models defined on pet_count pets
    !   * default is # PETs
    case ('sequential')
      label='pet_count:'
      call ESMF_ConfigGetAttribute(config, npet, label=trim(label), &
        default=petCount, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
          ' = sequential')
        return  ! bail out
      endif
      if (npet.lt.1.or.npet.gt.petCount) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': '//trim(label)//' must be > 0 and <= # PETs')
        return  ! bail out
      endif
      do i = 1,modCount
        if (.not.modActive(i)) cycle
        allocate(superIS%wrap%modelPetLists(i)%petList(npet), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg='Allocation of '//modName(i)//' PET list array failed.', &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        modPetList => superIS%wrap%modelPetLists(i)%petList
        do j = 1,npet
          modPetList(j) = j-1
        enddo
      enddo

    ! pet_layout_option: concurrent
    !   * active models defined on non-overlapping sets of PETs
    !   * requires <MOD>_pet_count input for active models
    !   * requires \sum(<MOD>_pet_count) <= petCount
    case ('concurrent')
      npet = 0
      modStart = 1
      do i = modStart,modCount
        if (.not.modActive(i)) cycle
        label=modName(i)//'_pet_count:'
        call ESMF_ConfigGetAttribute(config, modPetCount(i), label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
            ' = concurrent and '//modName(i)//' is active')
          return  ! bail out
        endif
        if (modPetCount(i).lt.1.or.modPetCount(i).ge.petCount) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' must be > 0 and < # PETs')
          return  ! bail out
        endif
        npet = npet + modPetCount(i)
      enddo
      if (npet.gt.petCount) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': pet_layout_option = concurrent requires'// &
          ' \sum(<MOD>_pet_count) <= # PETs for active models')
        return  ! bail out
      endif
      npet = 0
      do i = modStart,modCount
        if (.not.modActive(i)) cycle
        allocate(superIS%wrap%modelPetLists(i)%petList(modPetCount(i)), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg='Allocation of '//modName(i)//' PET list array failed.', &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        modPetList => superIS%wrap%modelPetLists(i)%petList
        do j = 1,modPetCount(i)
          modPetList(j) = npet
          npet = npet + 1
        enddo
        if (verbose) then
          write(msgString,'(a,i0)') trim(cname)//': '// &
            modName(i)//' PET count: ',modPetCount(i)
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          n = 10
          m = ceiling(real(modPetCount(i))/real(n))
          write(msgString,'(a)') trim(cname)//': '//modName(i)//' PET list:'
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          do l = 1,m
            k1 = min((l-1)*n+1,modPetCount(i))
            k2 = min((l-1)*n+n,modPetCount(i))
            write(msgString,'(a,100i7)') trim(cname)//': ', (modPetList(k),k=k1,k2)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          enddo
        endif
      enddo

    ! unsupported pet_layout_option:
    case default
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
       msg=trim(cname)//': pet_layout_option not supported: '//trim(petLayoutOption))
      return  ! bail out
    endselect

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(Driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    logical     ,pointer               :: verbose
    integer     ,pointer               :: modCount
    integer     ,pointer               :: atm, ocn, wav, ice
    character(3),pointer               :: modName(:)
    logical     ,pointer               :: modActive(:)
    character(8),pointer               :: conName(:,:)
    logical     ,pointer               :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: localrc, stat
    integer                            :: i, j
    type(ESMF_GridComp), pointer       :: modComp(:)
    type(ESMF_CplComp), pointer        :: conComp(:,:)
    type(NUOPC_RunSequence), pointer   :: runSeq(:)
    character(ESMF_MAXSTR)             :: verbosity

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the component for its config
    call ESMF_GridCompGet(gcomp, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, Driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for super internal state members
    modComp => superIS%wrap%modelComp
    conComp => superIS%wrap%connectorComp
    runSeq => superIS%wrap%runSeq

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    modName => is%wrap%modName
    modActive => is%wrap%modActive
    conName => is%wrap%conName
    conActive => is%wrap%conActive

    ! set verbosity for components
    if (verbose) then
      verbosity = 'high'
    else
      verbosity = 'low'
    endif

    ! set model component names and attributes for active models
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      ! set name
      call ESMF_GridCompSet(modComp(i), name=modName(i), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'ESMF_GridCompSet: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      ! set config
      call ESMF_GridCompSet(modComp(i), config=config, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'Set config: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      ! set verbosity
      call ESMF_AttributeSet(modComp(i), name='Verbosity', value=trim(verbosity), &
        convention='NUOPC', purpose='General', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'ESMF_AttributeSet: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! set connector component names and attributes for active connectors
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      ! set name
      call ESMF_CplCompSet(conComp(i,j), name=conName(i,j), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'ESMF_CplCompSet: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      ! set config
      call ESMF_CplCompSet(conComp(i,j), config=config, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'Set config: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      ! set verbosity
      call ESMF_AttributeSet(conComp(i,j), name='Verbosity', value=trim(verbosity), &
        convention='NUOPC', purpose='General', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'ESMF_AttributeSet: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo
    enddo

    ! SetServices for active models
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      select case (modName(i))
      case ('ATM')
        call ESMF_GridCompSetServices(modComp(i), atmSS, userRc=localrc, rc=rc)
      case ('OCN')
        call ESMF_GridCompSetServices(modComp(i), ocnSS, userRc=localrc, rc=rc)
      case ('WAV')
        call ESMF_GridCompSetServices(modComp(i), wavSS, userRc=localrc, rc=rc)
      case ('ICE')
        call ESMF_GridCompSetServices(modComp(i), iceSS, userRc=localrc, rc=rc)
      endselect
      if (ESMF_LogFoundError(rcToCheck=rc,      PASSTHRU, rcToReturn=rc) .or. &
          ESMF_LogFoundError(rcToCheck=localrc, PASSTHRU, rcToReturn=rc)) then
        write(msgString,'(a,1i2,a)') 'ESMF_GridCompSetServices: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! SetServices for active connectors
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      call ESMF_CplCompSetServices(conComp(i,j), cplSS, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc,      PASSTHRU, rcToReturn=rc) .or. &
          ESMF_LogFoundError(rcToCheck=localrc, PASSTHRU, rcToReturn=rc)) then
        write(msgString,'(a,2i2,a)') 'ESMF_CplCompSetServices: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo
    enddo

    ! override the default run sequence defined by the generic Driver
    ! notes: j = 0 indicates connector to driver; j < 0 indicates model run
    call NUOPC_RunSequenceDeallocate(runSeq, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_RunSequenceAdd(runSeq, 1, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    ! 1: connect active models to active models
    do j = 1,modCount
    do i = 1,modCount
      if (i.eq.j) cycle
      if (.not.conActive(i,j)) cycle
      call NUOPC_RunElementAdd(runSeq(1), i=i, j=j, phase=1, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo
    enddo
    ! 2: advance active models
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      call NUOPC_RunElementAdd(runSeq(1), i=i, j=-1, phase=1, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo
    call NUOPC_RunSequencePrint(runSeq(1))

  end subroutine

end module
