module wav_comp_nuopc

  !/ ------------------------------------------------------------------- /
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         29-May-2009 |
  !/                  +-----------------------------------+
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     A generic nuopc interface for WAVEWATCH III
  !     using input fields from CMEPS.
  !
  !  2. Method :
  !
  !     NUOPC component for the actual wave model (W3WAVE).
  !
  !  3. Parameters :
  !
  !     Local parameters.
  !     ----------------------------------------------------------------
  !       TIME0   I.A.  Starting time.
  !       TIMEN   I.A.  Ending time.
  !     ----------------------------------------------------------------
  !       NDS, NTRACE, ..., see W3WAVE
  !
  !  4. Subroutines used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      W3NMOD    Subr. W3GDATMD Set nummber of data structures
  !      W3SETG    Subr.   Id.    Point to data structure.
  !      W3NDAT    Subr. W3WDATMD Set nummber of data structures
  !      W3SETW    Subr.   Id.    Point to data structure.
  !      W3NMOD    Subr. W3ADATMD Set nummber of data structures
  !      W3NAUX    Subr.   Id.    Point to data structure.
  !      W3NOUT    Subr. W3ODATMD Set nummber of data structures
  !      W3SETO    Subr.   Id.    Point to data structure.
  !      W3NINP    Subr. W3IDATMD Set nummber of data structures
  !      W3SETI    Subr.   Id.    Point to data structure.
  !      STME21    Subr. W3TIMEMD Print date and time readable.
  !      W3INIT    Subr. W3INITMD Wave model initialization.
  !      W3WAVE    Subr. W3WAVEMD Wave model.
  !     ----------------------------------------------------------------
  !
  !  5. Called by :
  !
  !     NUOPC run sequence
  !
  !  6. Error messages :
  !
  !     - Checks on I-O.
  !     - Check on time interval.
  !
  !  7. Remarks :
  !
  !     - A rigourous input check is made in W3INIT.
  !
  !  8. Structure :
  !
  !     ----------------------------------------------------------------
  !
  !     wav_comp_init
  !
  !        0.   Set up data structures.                ( W3NMOD, etc. )
  !        1.   I-O setup.
  !          a  For shell.
  !          b  For WAVEWATCH III.
  !          c  Local parameters.
  !        2.   Define input fields
  !        3.   Set time frame.
  !        4.   Define output
  !          a  Loop over types, do
  !        +--------------------------------------------------------+
  !        | b    Process standard line                             |
  !        | c    If type 1: fields of mean wave parameters         |
  !        | d    If type 2: point output                           |
  !        | e    If type 3: track output                           |
  !        | f    If type 4: restart files                          |
  !        | g    If type 5: boundary output                        |
  !        | h    If type 6: separated wave fields                  |
  !        +--------------------------------------------------------+
  !        5.   Initialzations
  !
  !     wav_comp_run
  !
  !        7.   Run model for one time step with input from cmeps
  !             Return output to cmeps
  !             Do until end time is reached
  !        +--------------------------------------------------------+
  !        | a  Determine next time interval and input fields.      |
  !        |   1  Preparation                                       |
  !        |      Loop over input fields                            |
  !        | +------------------------------------------------------|
  !        | | 2  Check if update is needed                         |
  !        | | 4  Update next ending time                           |
  !        | +------------------------------------------------------|
  !        | b  Run wave model.                          ( W3WAVE ) |
  !        | d  Final output if needed.                  ( W3WAVE ) |
  !        | e  Check time                                          |
  !        +--------------------------------------------------------+
  !
  !     wav_comp_fin
  !
  !     ----------------------------------------------------------------
  !
  !  9. Switches :
  !
  ! 10. Source code :
  !
  !/ ------------------------------------------------------------------- /

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
  use wav_import_export     , only : advertise_fields, realize_fields
  use wav_shr_mod           , only : state_diagnose, state_getfldptr, state_fldchk
  use wav_shr_mod           , only : chkerr, state_setscalar, state_getscalar, alarmInit, ymd2date
  use wav_shr_mod           , only : runtype, merge_import, dbug_flag
  use w3odatmd              , only : nds, iaproc, napout
  use wav_shr_mod           , only : casename, multigrid, inst_suffix, inst_index
  use wav_shr_mod           , only : time_origin, calendar_name, elapsed_secs
#ifndef CESMCOUPLED
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

  public  :: SetServices, SetVM
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

  character(len=CL)       :: flds_scalar_name = ''
  integer                 :: flds_scalar_num = 0
  integer                 :: flds_scalar_index_nx = 0
  integer                 :: flds_scalar_index_ny = 0
  logical                 :: profile_memory = .false.

  logical                 :: histwr_is_active = .false. ! native WW3 grd output
  logical                 :: root_task = .false.
#ifdef CESMCOUPLED
  logical :: cesmcoupled = .true.
#else
  logical :: cesmcoupled = .false.
  integer, allocatable :: tend(:,:)
#endif

  integer     , parameter :: debug = 1
  character(*), parameter :: modName =  "(wav_comp_nuopc)"
  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

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

  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)

    use wav_shr_flags, only: initialize_flags

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
       call ESMF_LogWrite(trim(subname)//': profile_memory = '//trim(cvalue), ESMF_LOGMSG_INFO, rc=rc)
    end if

    call NUOPC_CompAttributeGet(gcomp, name="merge_import", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       if (trim(cvalue) == '.true.') then
          merge_import = .true.
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

    multigrid = .false.
    call NUOPC_CompAttributeGet(gcomp, name='multigrid', value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) multigrid=(trim(cvalue)=="true")
    write(logmsg,'(A,l)') trim(subname)//': Wave multigrid setting is ',multigrid
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO)

    call advertise_fields(importState, exportState, flds_scalar_name, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call initialize_flags()

    call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeAdvertise

  !========================================================================

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)

    use w3odatmd     , only : w3nout, w3seto, naproc, iaproc, naperr, napout
    use w3timemd     , only : stme21
    use w3adatmd     , only : w3naux, w3seta
    use w3idatmd     , only : w3seti, w3ninp
    use w3gdatmd     , only : nseal, nsea, nx, ny, mapsf, w3nmod, w3setg
    use w3wdatmd     , only : va, time, w3ndat, w3dimw, w3setw
#ifndef CESMCOUPLED
    use wminitmd     , only : wminit, wminitnml
    use wmunitmd     , only : wmuget, wmuset
#endif
    use wav_shel_inp , only : set_shel_io

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_DistGrid)            :: distGrid
    type(ESMF_Mesh)                :: Emesh, EmeshTemp
    type(ESMF_Array)               :: elemMaskArray
    type(ESMF_VM)                  :: vm
    type(ESMF_Time)                :: esmfTime, stopTime
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
    integer                        :: time0(2), ntrace(2)
    integer                        :: timen(2)
    integer                        :: i,j
    integer                        :: ierr
    integer                        :: n, jsea,isea, ncnt
    integer                        :: ntotal, nlnd
    integer                        :: nlnd_global, nlnd_local
    integer                        :: my_lnd_start, my_lnd_end
    integer, allocatable, target   :: mask_global(:)
    integer, allocatable, target   :: mask_local(:)
    integer, allocatable           :: gindex_lnd(:)
    integer, allocatable           :: gindex_sea(:)
    integer, allocatable           :: gindex(:)
    integer(i4)                    :: maskmin
    integer(i4), pointer           :: meshmask(:)
    logical                        :: isPresent, isSet
    character(23)                  :: dtme21
    integer                        :: iam, mpi_comm
    character(ESMF_MAXSTR)         :: msgString
    character(ESMF_MAXSTR)         :: diro
    character(ESMF_MAXSTR)         :: timestring
    character(CL)                  :: logfile
    logical                        :: local
    integer                        :: imod, idsi, idso, idss, idst, idse
    integer                        :: mds(13) ! Note that nds is set to this in w3initmod
    integer                        :: stdout
    integer                        :: petcount
    character(ESMF_MAXSTR)         :: preamb = './'
    character(ESMF_MAXSTR)         :: ifname = 'ww3_multi.inp'
    character(len=*), parameter    :: subname = '(wav_comp_nuopc:InitializeRealize)'
    ! -------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

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
#ifndef CESMCOUPLED
    nmproc = petcount
#else
    naproc = petcount
#endif

    ! naproc,iproc, napout, naperr are not available until after wminit
#ifndef CESMCOUPLED
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

    ! Initial run or restart run
    if ( runtype == "initial") then
       call ESMF_ClockGet( clock, startTime=esmfTime, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
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
#ifndef CESMCOUPLED
    stime = time0
    etime = timen
#endif

    !--------------------------------------------------------------------
    ! Wave model initialization
    !--------------------------------------------------------------------

#ifndef CESMCOUPLED
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

    !--------------------------------------------------------------------
    ! Mesh initialization
    !--------------------------------------------------------------------

    ! Note that nsea is the global number of sea points - and nseal is
    ! the local number of sea points

    ! create a  global index array for sea points
    allocate(gindex_sea(nseal))
    do jsea=1, nseal
       isea = iaproc + (jsea-1)*naproc
       ix = mapsf(isea,1)
       iy = mapsf(isea,2)
       gindex_sea(jsea) = ix + (iy-1)*nx
    end do

    ! create a global index array for non-sea (i.e. land points)
    allocate(mask_global(nx*ny), mask_local(nx*ny))
    mask_local(:) = 0
    mask_global(:) = 0
    do jsea=1, nseal
       isea = iaproc + (jsea-1)*naproc
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
    allocate(gindex(nlnd + nseal))
    do ncnt = 1,nlnd + nseal
       if (ncnt <= nseal) then
          gindex(ncnt) = gindex_sea(ncnt)
       else
          gindex(ncnt) = gindex_lnd(ncnt-nseal)
       end if
    end do
    deallocate(gindex_sea)
    deallocate(gindex_lnd)

    ! create distGrid from global index array
    DistGrid = ESMF_DistGridCreate(arbSeqIndexList=gindex, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! create the mesh
    call NUOPC_CompAttributeGet(gcomp, name='mesh_wav', value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! read in the mesh with an auto-generated distGrid
    EMeshTemp = ESMF_MeshCreate(filename=trim(cvalue), fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if ( root_task ) then
       write(stdout,*)'mesh file for domain is ',trim(cvalue)
    end if

    ! recreate the mesh using the above distGrid
    EMesh = ESMF_MeshCreate(EMeshTemp, elementDistgrid=Distgrid, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! obtain the mesh mask and find the minimum value across all PEs
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
       meshmask(1:nseal) = 1
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

    !--------------------------------------------------------------------
    ! Realize the actively coupled fields
    !--------------------------------------------------------------------
    call realize_fields(gcomp, mesh=Emesh, flds_scalar_name=flds_scalar_name, &
         flds_scalar_num=flds_scalar_num, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

#ifndef CESMCOUPLED
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

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeRealize

  !===============================================================================

  subroutine DataInitialize(gcomp, rc)

    use wav_import_export, only : calcRoughl
    use wav_shr_mod      , only : wav_coupling_to_cice
    use w3gdatmd         , only : nx, ny

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)  :: exportState
    integer           :: jsea
    real(r8), pointer :: z0rlen(:)
    real(r8), pointer :: sw_lamult(:)
    real(r8), pointer :: sw_ustokes(:)
    real(r8), pointer :: sw_vstokes(:)
    real(r8), pointer :: wav_tauice1(:)
    real(r8), pointer :: wav_tauice2(:)
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
       call state_getfldptr(exportState, 'Sw_z0', fldptr1d=z0rlen, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call CalcRoughl(z0rlen)
    endif

    if (wav_coupling_to_cice) then
      call state_getfldptr(exportState, 'wav_tauice1', wav_tauice1, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call state_getfldptr(exportState, 'wav_tauice2', wav_tauice2, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call state_getfldptr(exportState, 'wave_elevation_spectrum', fldptr2d=wave_elevation_spectrum, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

       wav_tauice1(:) = 0.
       wav_tauice2(:) = 0.
       wave_elevation_spectrum(:,:) = 0.
    endif

    ! Set global grid size scalars in export state
    call State_SetScalar(dble(nx), flds_scalar_index_nx, exportState, flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call State_SetScalar(dble(ny), flds_scalar_index_ny, exportState, flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if ( dbug_flag > 5) then
       call state_diagnose(exportState, 'at DataInitialize ', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine DataInitialize

  !=====================================================================

  subroutine ModelAdvance(gcomp, rc)

    !------------------------
    ! Run WW3
    !------------------------

    use w3wavemd          , only : w3wave
    use w3wdatmd          , only : time, w3setw
    use wav_import_export , only : import_fields, export_fields
    use wav_shel_inp      , only : odat
    use wav_shr_mod       , only : rstwr, histwr, outfreq ! only used by cesm

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
    integer                 :: ymd        ! current year-month-day
    integer                 :: tod        ! current time of day (sec)
    integer                 :: time0(2)
    integer                 :: timen(2)
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
#ifndef CESMCOUPLED
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

    if (cesmcoupled) then
       ! Determine if time to write cesm ww3 restart files
       ! rstwr is set in wav_shr_mod and used in w3wavmd to determine if restart should be written
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

    ! Determine if time to write ww3 history files
    ! history output is determined by the namelist variable outfreq
    ! histwr is set in wav_shr_mod and used in w3wavmd to determine if history should be written
    histwr = .false.
    if (outfreq .gt. 0) then
       ! output every outfreq hours if appropriate
       if( mod(hh, outfreq) == 0 ) then
          histwr = .true.
       endif
    endif
    if (.not. histwr) then
       if (histwr_is_active) then
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
       end if
       if ( root_task ) then
          !  write(nds(1),*) 'wav_comp_nuopc time', time, timen
          !  write(nds(1),*) 'ww3 hist flag ', histwr, outfreq, hh, mod(hh, outfreq)
       end if
    end if

    ! Advance the wave model
#ifndef CESMCOUPLED
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

  end subroutine ModelAdvance

  !===============================================================================

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
          histwr_is_active = .true.
       else
          ! If attribute is not present - write history native WW3 output if requested
          history_option = 'none'
          history_n = -999
          histwr_is_active = .false.
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

  end subroutine ModelFinalize

  !===============================================================================

  subroutine waveinit_cesm(gcomp, ntrace, mpi_comm, mds, rc)

    ! Initialize ww3 for cesm (called from InitializeRealize)

    use w3initmd     , only : w3init
    use w3gdatmd     , only : dtcfl, dtcfli, dtmax, dtmin
    use w3idatmd     , only : inflags1, inflags2
    use wav_shr_mod  , only : casename, initfile, outfreq
    use wav_shr_mod  , only : inst_index, inst_name, inst_suffix
    use wav_shel_inp , only : read_shel_inp
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
    character(len=CL) :: cvalue
    character(len=*), parameter    :: subname = '(wav_comp_nuopc:wavinit_cesm)'
    ! -------------------------------------------------------------------

    namelist /ww3_inparm/ initfile, outfreq, dtcfl, dtcfli, dtmax, dtmin

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    inst_name = "WAV"//trim(inst_suffix)
    ! Read namelist (set initfile in wav_shr_mod)
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
       write(stdout,'(a, 2x, i8)'   )' outfreq  = ',outfreq
       write(stdout,*)
    end if

    ! ESMF does not have a broadcast for chars
    call mpi_bcast(initfile, len_trim(initfile), MPI_CHARACTER, 0, mpi_comm, ierr)
    if (ierr /= MPI_SUCCESS) then
       call ESMF_LogWrite(trim(subname)//' error in mpi broadcast for initfile ', &
            ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
       rc = ESMF_FAILURE
       return
    end if
    call mpi_bcast(outfreq, 1, MPI_INTEGER, 0, mpi_comm, ierr)
    if (ierr /= MPI_SUCCESS) then
       call ESMF_LogWrite(trim(subname)//' error in mpi broadcast for outfreq ', &
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
    call ESMF_LogWrite(trim(subname)//' call read_shel_inp', ESMF_LOGMSG_INFO)
    call read_shel_inp(mpi_comm)

    ! Force inflags2 to be false - otherwise inflags2 will be set to inflags1 and answers will change
    ! Need to set this to .false. to avoide scaling of ice in section 4. of w3srcemed.
    ! inflags2(4) is true if ice concentration was ever read during this simulation
    ! for CESM we do not want ice concentration to be read in - 
    ! we do not want to have this occur for cesm
    ! Currently IC4 is used
    inflags2(:) = .false.
    if (wav_coupling_to_cice) then
       inflags2( 4) = .true. ! inflags2(4) is true if ice concentration was read during initialization
       ! TODO: these should be obtained by setting the following in ww3_shel.nml
       !   input%forcing%ice_param1 = 'T' ! ice thickness
       !   input%forcing%ice_param5 = 'T' ! ice floe size
    end if

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

  subroutine waveinit_ufs( gcomp, ntrace, mpi_comm, mds, rc)

    ! Initialize ww3 for ufs (called from InitializeRealize)

    use w3odatmd     , only : fnmpre
    use w3initmd     , only : w3init
    use wav_shr_mod  , only : outfreq
    use wav_shel_inp , only : read_shel_inp
    use wav_shel_inp , only : npts, odat, iprt, x, y, pnames, prtfrm
    use wav_shel_inp , only : flgrd, flgd, flgr2, flg2

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(in)  :: ntrace(:)
    integer, intent(in)  :: mpi_comm
    integer, intent(in)  :: mds(:)
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = '(wav_comp_nuopc:wavinit_ufs)'
    ! -------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    outfreq = 0
    fnmpre = './'

    call ESMF_LogWrite(trim(subname)//' call read_shel_inp', ESMF_LOGMSG_INFO)
    call read_shel_inp(mpi_comm)

    call ESMF_LogWrite(trim(subname)//' call w3init', ESMF_LOGMSG_INFO)
    call w3init ( 1, .false., 'ww3', mds, ntrace, odat, flgrd, flgr2, flgd, flg2, &
         npts, x, y, pnames, iprt, prtfrm, mpi_comm )

    if (dbug_flag > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)
  end subroutine waveinit_ufs

end module wav_comp_nuopc
