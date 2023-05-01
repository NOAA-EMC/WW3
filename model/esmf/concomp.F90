#include "macros.h"
!-------------------------------------------------------------------------------
! A test coupled application connector component
!
! Author:
!   Tim Campbell
!   Naval Research Laboratory
!   November 2014
!-------------------------------------------------------------------------------

module CON

  use ESMF
  use NUOPC
! use NUOPC_Connector, parent_SetServices => SetServices
  use NUOPC_Connector, only: parent_SetServices => SetServices, &
    label_ComputeRouteHandle, label_ExecuteRouteHandle, &
    label_ReleaseRouteHandle, label_Finalize, &
    NUOPC_ConnectorGet, NUOPC_ConnectorSet
  use UTL

  implicit none

  private

  public SetServices

  character (*), parameter :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
    logical :: verbose
    type(ESMF_VM)                   :: vm
    integer                         :: cplCount
    character(ESMF_MAXSTR) ,pointer :: srcNames(:) => null()
    character(ESMF_MAXSTR) ,pointer :: dstNames(:) => null()
    type(ESMF_RouteHandle)          :: remapRH
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

  subroutine SetServices(ccomp, rc)
    type(ESMF_CplComp)  :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer                       :: i

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    call ESMF_UserCompSetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! initialize timers
    is%wrap%numwt = 5
    allocate(is%wrap%wtnam(is%wrap%numwt), is%wrap%wtcnt(is%wrap%numwt), &
      is%wrap%wtime(is%wrap%numwt), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of wall timer memory failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    is%wrap%wtnam(1) = 'InitializeP0'
    is%wrap%wtnam(2) = 'ComputeRH'
    is%wrap%wtnam(3) = 'ExecuteRH'
    is%wrap%wtnam(4) = 'ReleaseRH'
    is%wrap%wtnam(5) = 'Finalize'
    is%wrap%wtcnt(:) = 0
    is%wrap%wtime(:) = 0d0

    ! the NUOPC connector component will register the generic methods
    call NUOPC_CompDerive(ccomp, parent_SetServices, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set initialize phase 0 requires use of ESMF method
    call ESMF_CplCompSetEntryPoint(ccomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(ccomp, specLabel=label_ComputeRouteHandle, &
      specRoutine=ComputeRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(ccomp, specLabel=label_ExecuteRouteHandle, &
      specRoutine=ExecuteRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(ccomp, specLabel=label_ReleaseRouteHandle, &
      specRoutine=ReleaseRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(ccomp, specLabel=label_Finalize, &
      specRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(ccomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: ccomp
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
    character(ESMF_MAXSTR)        :: attrString

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! determine verbosity
    call NUOPC_CompAttributeGet(ccomp, name='Verbosity', value=verbosity, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (trim(verbosity)=='high') then
      is%wrap%verbose = .true.
    else
      is%wrap%verbose = .false.
    endif
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered InitializeP0', ESMF_LOGMSG_INFO)

    ! query Component for its vm
    call ESMF_CplCompGet(ccomp, vm=is%wrap%vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(ccomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! finish timing
1   call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving InitializeP0', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ComputeRH(ccomp, rc)
    type(ESMF_CplComp)   :: ccomp
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
    integer                       :: i
    character(ESMF_MAXSTR), pointer :: cplList(:)
    integer                       :: srcCount, dstCount
    type(ESMF_FieldBundle)        :: srcFields, dstFields
    type(ESMF_Config)             :: config
    character(ESMF_MAXSTR)        :: label

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered ComputeRH', ESMF_LOGMSG_INFO)

    ! query Component for its config
    call ESMF_CplCompGet(ccomp, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get size of couple list
    call NUOPC_CompAttributeGet(ccomp, name='CplList', itemCount=is%wrap%cplCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    write(msgString,'(a,i0,a)') trim(cname)// &
      ': List of coupled fields (',is%wrap%cplCount,'):'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    ! if no coupled fields, then return
    if (is%wrap%cplCount.eq.0) goto 1

    ! get field bundles from connecter internal state
    call NUOPC_ConnectorGet(ccomp, srcFields=srcFields, dstFields=dstFields, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! report the cplList and FieldBundle lists
    call ESMF_FieldBundleGet(srcFields, fieldCount=srcCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldBundleGet(dstFields, fieldCount=dstCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (is%wrap%cplCount.ne.srcCount .or. is%wrap%cplCount.ne.dstCount) then
      write(msgString,'(a)') trim(cname)// &
        ': cplList count does not agree with FieldBundle counts'
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msgString), rcToReturn=rc)
      return ! bail out
    endif
    write(msgString,'(a,a5,a,a10,a,a10,a3,a)') &
      trim(cname)//': ','index',' ','srcField',' ','dstField',' ','standardName'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    allocate(is%wrap%srcNames(is%wrap%cplCount), is%wrap%dstNames(is%wrap%cplCount), &
      cplList(is%wrap%cplCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of cplList() failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    call NUOPC_CompAttributeGet(ccomp, name='CplList', valueList=cplList, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldBundleGet(srcFields, fieldNameList=is%wrap%srcNames, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldBundleGet(dstFields, fieldNameList=is%wrap%dstNames, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    do i=1, is%wrap%cplCount
      write(msgString,'(a,i5,a,a10,a,a10,a3,a)') &
        trim(cname)//': ',i,' ',trim(is%wrap%srcNames(i)),' ', &
        trim(is%wrap%dstNames(i)),'   ',trim(cplList(i))
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo
    deallocate(cplList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of cplList() failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! store remap
    call ESMF_FieldBundleRedistStore(srcFields, dstFields, is%wrap%remapRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! finish timing
1   call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving ComputeRH', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ExecuteRH(ccomp, rc)
    type(ESMF_CplComp)   :: ccomp
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
    type(ESMF_FieldBundle)        :: srcFields, dstFields

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! if no coupled fields, then return
    if (is%wrap%cplCount.eq.0) return

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered ExecuteRH', ESMF_LOGMSG_INFO)

    ! get field bundles from connecter internal state
    call NUOPC_ConnectorGet(ccomp, srcFields=srcFields, dstFields=dstFields, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! apply remap
    call ESMF_FieldBundleRedist(srcFields, dstFields, is%wrap%remapRH, checkFlag=.false., rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! finish timing
1   call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving ExecuteRH', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ReleaseRH(ccomp, rc)
    type(ESMF_CplComp)   :: ccomp
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

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! if no coupled fields, then return
    if (is%wrap%cplCount.eq.0) return

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered ReleaseRH', ESMF_LOGMSG_INFO)

    ! release remap
    call ESMF_FieldBundleRedistRelease(is%wrap%remapRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! deallocate field name arrays
    if (associated(is%wrap%srcNames)) then
      deallocate(is%wrap%srcNames, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of srcNames array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif
    if (associated(is%wrap%dstNames)) then
      deallocate(is%wrap%dstNames, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of dstNames array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif

    ! finish timing
1   call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving ReleaseRH', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(ccomp, rc)
    type(ESMF_CplComp)   :: ccomp
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

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered Finalize', ESMF_LOGMSG_INFO)

    ! finish timing
1   call ESMF_VMWtime(wf1Time)
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
    if (associated(is%wrap)) then
      deallocate(is%wrap, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of internal state memory failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving Finalize', ESMF_LOGMSG_INFO)

  end subroutine

end module
