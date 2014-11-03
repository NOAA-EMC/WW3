!-------------------------------------------------------------------------------
! A test Wavewatch III coupled application connector component
!
! Author:
!   Tim Campbell
!   Naval Research Laboratory
!   March 2014
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! ESMF macros for logging
!-------------------------------------------------------------------------------
#define FILENAME "con.F90"
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


module CON

  use ESMF
  use NUOPC
  use NUOPC_Connector, only: &
    con_routine_SS      => routine_SetServices, &
    con_type_IS         => type_InternalState, &
    con_label_IS        => label_InternalState, &
    con_label_ComputeRH => label_ComputeRouteHandle, &
    con_label_ExecuteRH => label_ExecuteRouteHandle, &
    con_label_ReleaseRH => label_ReleaseRouteHandle
  
  implicit none
  
  private
  
  public SetServices

  character (*), parameter :: defaultVerbosity = 'low'
  character (*), parameter :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
    logical :: verbose
    integer                         :: cplCount
    character(ESMF_MAXSTR) ,pointer :: srcNames(:) => null()
    character(ESMF_MAXSTR) ,pointer :: dstNames(:) => null()
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
    logical                       :: verbose
    character(ESMF_MAXSTR)        :: verbosity
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer                       :: i

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! determine verbosity
    call ESMF_AttributeGet(ccomp, name='Verbosity', value=verbosity, &
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
    call ESMF_UserCompSetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    is%wrap%verbose = verbose

    ! initialize timers
    is%wrap%numwt = 3
    allocate(is%wrap%wtnam(is%wrap%numwt), is%wrap%wtcnt(is%wrap%numwt), &
      is%wrap%wtime(is%wrap%numwt), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of wall timer memory failed.', &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    is%wrap%wtnam(1) = 'ComputeRH'
    is%wrap%wtnam(2) = 'ExecuteRH'
    is%wrap%wtnam(3) = 'ReleaseRH'
    is%wrap%wtcnt(:) = 0
    is%wrap%wtime(:) = 0d0

    ! the NUOPC connector component will register the generic methods
    call con_routine_SS(ccomp, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! attach specializing method(s)
    call ESMF_MethodAdd(ccomp, label=con_label_ComputeRH, &
      userRoutine=ComputeRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_MethodAdd(ccomp, label=con_label_ExecuteRH, &
      userRoutine=ExecuteRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_MethodAdd(ccomp, label=con_label_ReleaseRH, &
      userRoutine=ReleaseRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving SetServices', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ComputeRH(ccomp, rc)
    type(ESMF_CplComp)  :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(con_type_IS)             :: superIS
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer, parameter            :: it1=1
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    integer                       :: i
    character(ESMF_MAXSTR), pointer :: cplList(:)
    integer                         :: srcCount, dstCount

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

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(ccomp, con_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered ComputeRH', ESMF_LOGMSG_INFO)

    ! get size of couple list
    call NUOPC_CplCompAttributeGet(ccomp, cplListSize=is%wrap%cplCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    write(msgString,'(a,i0,a)') trim(cname)// &
      ': List of coupled fields (',is%wrap%cplCount,'):'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    ! if no coupled fields, then return
    if (is%wrap%cplCount.eq.0) goto 1

    ! report the cplList and FieldBundle lists
    call ESMF_FieldBundleGet(superIS%wrap%srcFields, fieldCount=srcCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldBundleGet(superIS%wrap%dstFields, fieldCount=dstCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (is%wrap%cplCount.ne.srcCount .or. is%wrap%cplCount.ne.dstCount) then
      write(msgString,'(a)') trim(cname)// &
        ': cplList count does not agree with FieldBundle counts'
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msgString), rcToReturn=rc)
      return  ! bail out
    endif
    write(msgString,'(a,a5,a,a10,a,a10,a3,a)') &
      trim(cname)//': ','index',' ','srcField',' ','dstField',' ','standardName'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    allocate(is%wrap%srcNames(is%wrap%cplCount), is%wrap%dstNames(is%wrap%cplCount), &
      cplList(is%wrap%cplCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of cplList() failed.', &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    call NUOPC_CplCompAttributeGet(ccomp, cplList=cplList, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldBundleGet(superIS%wrap%srcFields, fieldNameList=is%wrap%srcNames, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldBundleGet(superIS%wrap%dstFields, fieldNameList=is%wrap%dstNames, rc=rc)
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
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! store remap
    call ESMF_FieldBundleRedistStore(superIS%wrap%srcFields, superIS%wrap%dstFields, &
      superIS%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving ComputeRH', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1
 
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ExecuteRH(ccomp, rc)
    type(ESMF_CplComp)  :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(con_type_IS)             :: superIS
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer, parameter            :: it1=2
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time

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

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(ccomp, con_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered ExecuteRH', ESMF_LOGMSG_INFO)

    ! apply remap
    call ESMF_FieldBundleRedist(superIS%wrap%srcFields, superIS%wrap%dstFields, &
      superIS%wrap%rh, checkFlag=.false., rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving ExecuteRH', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1
 
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ReleaseRH(ccomp, rc)
    type(ESMF_CplComp)  :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(con_type_IS)             :: superIS
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer, parameter            :: it1=3
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time

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

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(ccomp, con_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered ReleaseRH', ESMF_LOGMSG_INFO)

    ! release remap
    call ESMF_FieldBundleRedistRelease(superIS%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! deallocate field name arrays
    if (associated(is%wrap%srcNames)) then
      deallocate(is%wrap%srcNames, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of srcNames array failed.', &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif
    if (associated(is%wrap%dstNames)) then
      deallocate(is%wrap%dstNames, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of dstNames array failed.', &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif

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
    if (associated(is%wrap)) then
      deallocate(is%wrap, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of internal state memory failed.', &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving ReleaseRH', ESMF_LOGMSG_INFO)

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
