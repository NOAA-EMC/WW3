#include "macros.h"
#define FRONT_CON CON
#define FRONT_DUM DUM
!-------------------------------------------------------------------------------
! A test coupled application driver component
!
! Author:
!   Tim Campbell
!   Naval Research Laboratory
!   November 2014
!-------------------------------------------------------------------------------

module FRONT_ESM

  use ESMF
  use NUOPC
  use NUOPC_Driver, parent_SetServices => SetServices
  use UTL

  use  FRONT_CON, only: cplSS => SetServices
#ifdef FRONT_MED
  use  FRONT_MED, only: medSS => SetServices
#else
  use  FRONT_DUM, only: medSS => SetServices
#endif
#ifdef FRONT_ATM
  use  FRONT_ATM, only: atmSS => SetServices
#else
  use  FRONT_DUM, only: atmSS => SetServices
#endif
#ifdef FRONT_OCN
  use  FRONT_OCN, only: ocnSS => SetServices
#else
  use  FRONT_DUM, only: ocnSS => SetServices
#endif
#ifdef FRONT_WAV
  use  FRONT_WAV, only: wavSS => SetServices
#else
  use  FRONT_DUM, only: wavSS => SetServices
#endif
#ifdef FRONT_ICE
  use  FRONT_ICE, only: iceSS => SetServices
#else
  use  FRONT_DUM, only: iceSS => SetServices
#endif
#ifdef FRONT_LND
  use  FRONT_LND, only: lndSS => SetServices
#else
  use  FRONT_DUM, only: lndSS => SetServices
#endif

  implicit none
  save
  private

  public SetServices

  integer     , parameter :: maxModCount = 6
  logical     , parameter :: defaultVerbose = .false.
  logical     , parameter :: defaultModActive = .false.
  character(*), parameter :: label_InternalState = 'InternalState'

  type type_PL
    integer, pointer :: p(:)
  end type

  type type_InternalStateStruct
    logical      :: verbose
    integer      :: modCount=0
    integer      :: med=0, atm=0, ocn=0, wav=0, ice=0, lnd=0
    character(3) :: modName(0:maxModCount)
    logical      :: modActive(0:maxModCount)
    type(type_PL):: modPetList(0:maxModCount)
    character(10):: conName(0:maxModCount,0:maxModCount)
    logical      :: conActive(0:maxModCount,0:maxModCount)
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(type_InternalState)           :: is
    logical      ,pointer              :: verbose
    integer      ,pointer              :: modCount
    integer      ,pointer              :: med, atm, ocn, wav, ice, lnd
    character(3) ,pointer              :: modName(:)
    logical      ,pointer              :: modActive(:)
    type(type_PL),pointer              :: modPetList(:)
    character(10),pointer              :: conName(:,:)
    logical      ,pointer              :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: lrc, stat
    logical                            :: configIsPresent
    integer                            :: i, j, k

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(driver, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! config is required
    call ESMF_GridCompGet(driver, configIsPresent=configIsPresent, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (.not.configIsPresent) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config')
      return ! bail out
    endif

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    call ESMF_UserCompSetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    med => is%wrap%med
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    lnd => is%wrap%lnd
    modName => is%wrap%modName
    modActive => is%wrap%modActive
    modPetList => is%wrap%modPetList
    conName => is%wrap%conName
    conActive => is%wrap%conActive

    ! initialize
    modCount = 0
    med = 0
    atm = 0
    ocn = 0
    wav = 0
    ice = 0
    lnd = 0
    modActive = .false.
    conActive = .false.

    ! *** report compiled modules, set model count, model index mapping, and model names ***

    ! CON component
#ifdef FRONT_CON
    call ESMF_LogWrite(trim(cname)//': compiled with    CON  module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without CON  module', ESMF_LOGMSG_INFO)
#endif

    ! MED component
#ifdef FRONT_MED
    modCount     =  modCount + 1
    med          =  modCount
    modName(med) =  'MED'
    call ESMF_LogWrite(trim(cname)//': compiled with    MED  module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without MED  module', ESMF_LOGMSG_INFO)
#endif

    ! ATM component
#ifdef FRONT_ATM
    modCount     = modCount + 1
    atm          = modCount
    modName(atm) = 'ATM'
    call ESMF_LogWrite(trim(cname)//': compiled with    ATM  module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ATM  module', ESMF_LOGMSG_INFO)
#endif

    ! OCN component
#ifdef FRONT_OCN
    modCount     = modCount + 1
    ocn          = modCount
    modName(ocn) = 'OCN'
    call ESMF_LogWrite(trim(cname)//': compiled with    OCN  module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without OCN  module', ESMF_LOGMSG_INFO)
#endif

    ! WAV component
#ifdef FRONT_WAV
    modCount     = modCount + 1
    wav          = modCount
    modName(wav) = 'WAV'
    call ESMF_LogWrite(trim(cname)//': compiled with    WAV  module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without WAV  module', ESMF_LOGMSG_INFO)
#endif

    ! ICE component
#ifdef FRONT_ICE
    modCount     = modCount + 1
    ice          = modCount
    modName(ice) = 'ICE'
    call ESMF_LogWrite(trim(cname)//': compiled with    ICE  module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ICE  module', ESMF_LOGMSG_INFO)
#endif

    ! LND component
#ifdef FRONT_LND
    modCount     = modCount + 1
    lnd          = modCount
    modName(lnd) = 'LND'
    call ESMF_LogWrite(trim(cname)//': compiled with    LND  module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without LND  module', ESMF_LOGMSG_INFO)
#endif

    ! report model indexing
    do i = 1,modCount
      write(msgString,'(a,i0)') trim(cname)//': '//modName(i)//' model index: ',i
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! NUOPC_Driver registers the generic methods
    call NUOPC_CompDerive(driver, parent_SetServices, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(driver, specLabel=label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(driver, specLabel=label_Finalize, &
      specRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(type_InternalState)           :: is
    logical      ,pointer              :: verbose
    integer      ,pointer              :: modCount
    integer      ,pointer              :: med, atm, ocn, wav, ice, lnd
    character(3) ,pointer              :: modName(:)
    logical      ,pointer              :: modActive(:)
    type(type_PL),pointer              :: modPetList(:)
    character(10),pointer              :: conName(:,:)
    logical      ,pointer              :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: lrc, stat
    integer                            :: i, j, k
    character(ESMF_MAXSTR)             :: verbosity
    character(ESMF_MAXSTR)             :: label
    type(ESMF_GridComp)                :: modComp(maxModCount)
    type(ESMF_CplComp)                 :: conComp(maxModCount,maxModCount)
    integer(ESMF_KIND_I4)              :: time(6)
    type(ESMF_Time)                    :: startTime
    type(ESMF_Time)                    :: stopTime
    type(ESMF_TimeInterval)            :: runDuration
    type(ESMF_TimeInterval)            :: timeStep
    type(ESMF_TimeInterval)            :: zeroTimeInterval
    type(ESMF_Clock)                   :: internalClock

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(driver, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the component for its config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    med => is%wrap%med
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    lnd => is%wrap%lnd
    modName => is%wrap%modName
    modActive => is%wrap%modActive
    modPetList => is%wrap%modPetList
    conName => is%wrap%conName
    conActive => is%wrap%conActive

    ! process config for verbose
    label = 'verbose:'
    call ESMF_ConfigGetAttribute(config, verbose, default=defaultVerbose, &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (verbose) then
      verbosity = 'high'
    else
      verbosity = 'low'
    endif

    ! process config for modActive
    do i = 1,modCount
      label = modName(i)//'_active:'
      call ESMF_ConfigGetAttribute(config, modActive(i), default=defaultModActive, &
        label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        call ESMF_LogWrite(trim(cname)//': ESMF_ConfigGetAttribute: '// &
          trim(label), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      ! report active/inactive models
      if (modActive(i)) then
        write(msgString,'(a)') trim(cname)//': '//modName(i)//' is     active'
      else
        write(msgString,'(a)') trim(cname)//': '//modName(i)//' is not active'
      endif
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! set connector names (use same as NUOPC)
    do j = 1,modCount
    do i = 1,modCount
      conName(i,j) = modName(i)//'-TO-'//modName(j)
    enddo
    enddo

    ! set active connectors
    if (modActive(med)) then
      ! mediator is active
      ! * active model to mediator connections
      do i = med+1,modCount
        conActive(i,med) = modActive(i)
      enddo
      ! * mediator to active model connections
      do i = med+1,modCount
        conActive(med,i) = modActive(i)
      enddo
    else
      ! mediator is not active
      ! * active model to active model connections
      do j = med+1,modCount
      do i = med+1,modCount
        if (i.eq.j) cycle
        conActive(i,j) = modActive(i).and.modActive(j)
        conActive(j,i) = modActive(j).and.modActive(i)
      enddo
      enddo
    endif

    ! report active connections
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      write(msgString,'(a)') trim(cname)//': '//conName(i,j)//' connector is active'
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo
    enddo

    ! process config for pet lists
    call SetModelPetLists(driver, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! SetServices for active models
    ! The model setServices function reference must be compile-time valid
    ! so that CPP macros are not required here.
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      select case (modName(i))
      case ('MED')
        call NUOPC_DriverAddComp(driver, modName(i), medSS, &
          petList=modPetList(i)%p, comp=modComp(i), rc=rc)
      case ('ATM')
        call NUOPC_DriverAddComp(driver, modName(i), atmSS, &
          petList=modPetList(i)%p, comp=modComp(i), rc=rc)
      case ('OCN')
        call NUOPC_DriverAddComp(driver, modName(i), ocnSS, &
          petList=modPetList(i)%p, comp=modComp(i), rc=rc)
      case ('WAV')
        call NUOPC_DriverAddComp(driver, modName(i), wavSS, &
          petList=modPetList(i)%p, comp=modComp(i), rc=rc)
      case ('ICE')
        call NUOPC_DriverAddComp(driver, modName(i), iceSS, &
          petList=modPetList(i)%p, comp=modComp(i), rc=rc)
      case ('LND')
        call NUOPC_DriverAddComp(driver, modName(i), lndSS, &
          petList=modPetList(i)%p, comp=modComp(i), rc=rc)
      endselect
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'NUOPC_DriverAddComp: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
    enddo

    ! set component attributes for active models
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      ! set config
      call ESMF_GridCompSet(modComp(i), config=config, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'Set config: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      ! set verbosity
      call NUOPC_CompAttributeSet(modComp(i), name='Verbosity', value=trim(verbosity), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'NUOPC_CompAttributeSet: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
    enddo

    ! SetServices for active connectors
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      call NUOPC_DriverAddComp(driver, modName(i), modName(j), cplSS, comp=conComp(i,j), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'NUOPC_DriverAddComp: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
    enddo
    enddo

    ! set connector component attributes for active connectors
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      ! set config
      call ESMF_CplCompSet(conComp(i,j), config=config, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'Set Config: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      ! set verbosity
      call NUOPC_CompAttributeSet(conComp(i,j), name='Verbosity', value=trim(verbosity), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'NUOPC_CompAttributeSet: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
    enddo
    enddo

    ! process config for required timeStep input
    label = 'time_step:'
    call ESMF_ConfigGetAttribute(config, time(4:6), count=3, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config input: '// &
        trim(label)//' hh mm ss')
      return ! bail out
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
      return ! bail out
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
      return ! bail out
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
      return ! bail out
    endif

    ! create/set the driver clock
    internalClock = ESMF_ClockCreate(name=trim(cname)//'_clock', &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelPetLists(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(type_InternalState)           :: is
    logical      ,pointer              :: verbose
    integer      ,pointer              :: modCount
    integer      ,pointer              :: med, atm, ocn, wav, ice, lnd
    character(3) ,pointer              :: modName(:)
    logical      ,pointer              :: modActive(:)
    type(type_PL),pointer              :: modPetList(:)
    character(10),pointer              :: conName(:,:)
    logical      ,pointer              :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: lrc, stat
    integer                            :: i, j, k
    integer                            :: l, m, n, p
    integer                            :: k1, k2
    integer                            :: modStart
    integer                            :: petCount, npet
    integer                            :: modPetCount(maxModCount)
    character(ESMF_MAXSTR)             :: label
    character(ESMF_MAXSTR)             :: petLayoutOption
    logical                            :: isPresent
    integer     , allocatable          :: list(:), ncol(:)

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(driver, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the component for its config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    med => is%wrap%med
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    lnd => is%wrap%lnd
    modName => is%wrap%modName
    modActive => is%wrap%modActive
    modPetList => is%wrap%modPetList
    conName => is%wrap%conName
    conActive => is%wrap%conActive

    ! get the petCount
    call ESMF_GridCompGet(driver, petCount=petCount, rc=rc)
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
    !   * MED defined on pet_count pets
    !   * requires pet_count input
    case ('sequential')
      label='pet_count:'
      call ESMF_ConfigGetAttribute(config, npet, label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
          ' = sequential')
        return ! bail out
      endif
      if (npet.lt.1.or.npet.gt.petCount) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': '//trim(label)//' must be > 0 and <= # PETs')
        return ! bail out
      endif
      do i = 1,modCount
        if (.not.modActive(i)) cycle
        allocate(modPetList(i)%p(npet), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg='Allocation of '//modName(i)//' PET list array failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        do j = 1,npet
          modPetList(i)%p(j) = j-1
        enddo
      enddo

    ! pet_layout_option: concurrent
    !   * active models defined on non-overlapping sets of PETs
    !   * requires <MOD>_pet_count input for active models
    !   * MED_pet_count optional, default is MED defined on all PETs
    !   * requires \sum(<MOD>_pet_count) <= petCount
    case ('concurrent')
      modStart = 1
      if (modActive(med)) then
        label=modName(med)//'_pet_count:'
        call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=isPresent, rc=rc)
        if (.not.isPresent.or.rc.ne.ESMF_SUCCESS) then
          modPetCount(med) = petCount
          if (verbose) then
            write(msgString,'(a,i0)') trim(cname)//': '// &
              modName(med)//' PET count: ',modPetCount(med)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          endif
          allocate(modPetList(med)%p(petCount), stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg='Allocation of '//modName(med)//' PET list array failed.', &
            CONTEXT, rcToReturn=rc)) return ! bail out
          do j = 1,petCount
            modPetList(med)%p(j) = j-1
          enddo
          modStart = med+1
        endif
      endif
      npet = 0
      do i = modStart,modCount
        if (.not.modActive(i)) cycle
        label=modName(i)//'_pet_count:'
        call ESMF_ConfigGetAttribute(config, modPetCount(i), label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
            ' = concurrent and '//modName(i)//' is active')
          return ! bail out
        endif
        if (modPetCount(i).lt.1.or.modPetCount(i).ge.petCount) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' must be > 0 and < # PETs')
          return ! bail out
        endif
        npet = npet + modPetCount(i)
      enddo
      if (npet.gt.petCount) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': pet_layout_option = concurrent requires'// &
          ' \sum(<MOD>_pet_count) <= # PETs for active models')
        return ! bail out
      endif
      npet = 0
      do i = modStart,modCount
        if (.not.modActive(i)) cycle
        allocate(modPetList(i)%p(modPetCount(i)), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg='Allocation of '//modName(i)//' PET list array failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        do j = 1,modPetCount(i)
          modPetList(i)%p(j) = npet
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
            write(msgString,'(a,100i7)') trim(cname)//': ', (modPetList(i)%p(k),k=k1,k2)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          enddo
        endif
      enddo

    ! pet_layout_option: specified
    !   * active models defined on specified sets of PETs
    !   * requires <MOD>_pet_list input for active models
    !   * MED_pet_list optional, default is MED defined on all PETs
    !   * requires min(<MOD>_pet_list) >= 0 && max(<MOD>_pet_list) < petCount
    case ('specified')
      modStart = 1
      if (modActive(med)) then
        label=modName(med)//'_pet_list::'
        call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=isPresent, rc=rc)
        if (.not.isPresent.or.rc.ne.ESMF_SUCCESS) then
          modPetCount(med) = petCount
          if (verbose) then
            write(msgString,'(a,i0)') trim(cname)//': '// &
              modName(med)//' PET count: ',modPetCount(med)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          endif
          allocate(modPetList(med)%p(petCount), stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg='Allocation of '//modName(med)//' PET list array failed.', &
            CONTEXT, rcToReturn=rc)) return ! bail out
          do j = 1,petCount
            modPetList(i)%p(j) = j-1
          enddo
          modStart = med+1
        endif
      endif
      do i = modStart,modCount
        if (.not.modActive(i)) cycle
        label=modName(i)//'_pet_list::'
        call ESMF_ConfigGetDim(config, m, n, label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
            ' = specified and '//modName(i)//' is active')
          return ! bail out
        endif
        if (verbose) then
          write(msgString,'(a,i0)') trim(cname)//': '// &
            trim(label)//' table number of rows: ',m
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          write(msgString,'(a,i0)') trim(cname)//': '// &
            trim(label)//' table max number of columns: ',n
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
        endif
        allocate(list(n), ncol(m), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg='Allocation of '//modName(i)//' PET list table failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        do p = 1,2
          if (p.eq.2) then
            if (verbose) then
              write(msgString,'(a,i0)') trim(cname)//': '// &
                modName(i)//' PET count: ',modPetCount(i)
              call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
            endif
            if (modPetCount(i).lt.1.or.modPetCount(i).gt.petCount) then
              call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
                msg=trim(cname)//': '//modName(i)// &
                ' PET count must be > 0 and <= # PETs')
              return ! bail out
            endif
            allocate(modPetList(i)%p(modPetCount(i)), stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg='Allocation of '//modName(i)//' PET list array failed.', &
              CONTEXT, rcToReturn=rc)) return ! bail out
          endif
          call ESMF_ConfigFindLabel(config, trim(label), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) then
            call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
              msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
              ' = specified and '//modName(i)//' is active')
            return ! bail out
          endif
          modPetCount(i) = 0
          do l=1,m
            call ESMF_ConfigNextLine(config, rc=rc)
            if (ESMF_LogFoundError(rc, PASSTHRU)) then
              write(msgString,'(a,i0,a)') trim(cname)//': '//trim(label)// &
                ' next line ',l,' failed'
              call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
              return ! bail out
            endif
            if (p.eq.1) then
              ncol(l) = ESMF_ConfigGetLen(config, rc=rc)
              if (ESMF_LogFoundError(rc, PASSTHRU)) then
                write(msgString,'(a,i0,a)') trim(cname)//': '//trim(label)// &
                  ' get length ',l,' failed'
                call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
                return ! bail out
              endif
              modPetCount(i) = modPetCount(i) + ncol(l)
            else
              call ESMF_ConfigGetAttribute(config, list(1:ncol(l)), rc=rc)
              if (ESMF_LogFoundError(rc, PASSTHRU)) then
                write(msgString,'(a,i0,a)') trim(cname)//': '//trim(label)// &
                  ' get row ',l,' failed'
                call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
                return ! bail out
              endif
              modPetCount(i) = modPetCount(i) + ncol(l)
              modPetList(i)%p(modPetCount(i)-ncol(l)+1:modPetCount(i)) = list(1:ncol(l))
            endif
          enddo
        enddo
        deallocate(list, ncol, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg='Deallocation of '//modName(i)//' PET list table failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        if (verbose) then
          n = 10
          m = ceiling(real(modPetCount(i))/real(n))
          write(msgString,'(a)') trim(cname)//': '//modName(i)//' PET list:'
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          do l = 1,m
            k1 = min((l-1)*n+1,modPetCount(i))
            k2 = min((l-1)*n+n,modPetCount(i))
            write(msgString,'(a,100i7)') trim(cname)//': ', (modPetList(i)%p(k),k=k1,k2)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          enddo
        endif
        if (minval(modPetList(i)%p).lt.0.or.maxval(modPetList(i)%p).ge.petCount) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//modName(i)//' PET list ids must be > 0 and < # PETs')
          return ! bail out
        endif
        do j = 1,modPetCount(i)
          if (count(modPetList(i)%p.eq.modPetList(i)%p(j)).gt.1) then
            call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
              msg=trim(cname)//': '//modName(i)//' PET list has duplicate entries')
            return ! bail out
          endif
        enddo
      enddo

    ! unsupported pet_layout_option:
    case default
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
       msg=trim(cname)//': pet_layout_option not supported: '//trim(petLayoutOption))
      return ! bail out
    endselect

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(type_InternalState)           :: is
    logical      ,pointer              :: verbose
    integer      ,pointer              :: modCount
    integer      ,pointer              :: med, atm, ocn, wav, ice, lnd
    character(3) ,pointer              :: modName(:)
    logical      ,pointer              :: modActive(:)
    type(type_PL),pointer              :: modPetList(:)
    character(10),pointer              :: conName(:,:)
    logical      ,pointer              :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: lrc, stat
    integer                            :: i, j, k

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(driver, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the component for its config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    med => is%wrap%med
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    lnd => is%wrap%lnd
    modName => is%wrap%modName
    modActive => is%wrap%modActive
    modPetList => is%wrap%modPetList
    conName => is%wrap%conName
    conActive => is%wrap%conActive

    ! override the default run sequence defined by the generic Driver
    call NUOPC_DriverNewRunSequence(driver, slotCount=1, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (modActive(med)) then
      ! *** run sequence with mediator ***
      ! 1: connect active models to mediator
      do i = med+1,modCount
        if (.not.conActive(i,med)) cycle
        call NUOPC_DriverAddRunElement(driver, 1, modName(i), modName(med), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      enddo
      ! 2: advance mediator
      call NUOPC_DriverAddRunElement(driver, 1, modName(med), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      ! 3: connect mediator to active models
      do j = med+1,modCount
        if (.not.conActive(med,j)) cycle
        call NUOPC_DriverAddRunElement(driver, 1, modName(med), modName(j), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      enddo
      ! 4: advance active models
      do i = med+1,modCount
        if (.not.modActive(i)) cycle
        call NUOPC_DriverAddRunElement(driver, 1, modName(i), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      enddo
    else
      ! *** run sequence without mediator ***
      ! 1: connect active models to active models
      do j = 1,modCount
      do i = 1,modCount
        if (i.eq.j) cycle
        if (.not.conActive(i,j)) cycle
        call NUOPC_DriverAddRunElement(driver, 1, modName(i), modName(j), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      enddo
      enddo
      ! 2: advance active models
      do i = 1,modCount
        if (.not.modActive(i)) cycle
        call NUOPC_DriverAddRunElement(driver, 1, modName(i), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      enddo
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(type_InternalState)           :: is
    logical                            :: verbose
    integer      ,pointer              :: modCount
    integer      ,pointer              :: med, atm, ocn, wav, ice, lnd
    character(3) ,pointer              :: modName(:)
    logical      ,pointer              :: modActive(:)
    type(type_PL),pointer              :: modPetList(:)
    character(10),pointer              :: conName(:,:)
    logical      ,pointer              :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: lrc, stat
    integer                            :: i, j, k

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(driver, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose = is%wrap%verbose
    modCount => is%wrap%modCount
    med => is%wrap%med
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    lnd => is%wrap%lnd
    modName => is%wrap%modName
    modActive => is%wrap%modActive
    modPetList => is%wrap%modPetList
    conName => is%wrap%conName
    conActive => is%wrap%conActive

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered Finalize', ESMF_LOGMSG_INFO)

    ! clean up internal state
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      deallocate(modPetList(i)%p, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of '//modName(i)//' PET list array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    enddo

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving Finalize', ESMF_LOGMSG_INFO)

  end subroutine

end module
