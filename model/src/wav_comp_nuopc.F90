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
  !       NHMAX   I.P.  Maximum number of homogeneous fields.
  !
  !       NDSO    Int.  General output unit number (shell only).
  !       NDSE    Int.  Error output unit number (shell only).
  !       NDST    Int.  Test output unit number (shell only).
  !       FLH     L.A.  Flags for homogeneous fields.
  !       NH      I.A.  Number of times for homogeneous fields.
  !       THO     I.A.  Times of homogeneous fields.
  !       TIME0   I.A.  Starting time.
  !       TIMEN   I.A.  Ending time.
  !     ----------------------------------------------------------------
  !
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
  !
  !      STME21    Subr. W3TIMEMD Print date and time readable.
  !      DSEC21    Func.   Id.    Difference between times.
  !      TICK21    Subr.   Id.    Increment time.
  !
  !      W3INIT    Subr. W3INITMD Wave model initialization.
  !      W3WAVE    Subr. W3WAVEMD Wave model.
  !     ----------------------------------------------------------------
  !
  !  5. Called by :
  !
  !     None, stand-alone program.
  !
  !  6. Error messages :
  !
  !     - Checks on I-O.
  !     - Check on time interval.
  !
  !  7. Remarks :
  !
  !     - A rigourous input check is made in W3INIT.
  !     - See W3WDAS for documentation on the set-up of the data
  !       assimilation.
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
  !        7.   Run model with input
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
  !        | c  If requested, data assimilation.         ( W3WDAS ) |
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
  !       !/SHRD  Switch for shared / distributed memory architecture.
  !       !/DIST  Id.
  !       !/MPI   Id.
  !
  !       !/LLG   Spherical grid.
  !       !/XYG   Cartesian grid.
  !       !/MGW   Moving grid wind correction.
  !       !/MGP   Moving grid propagation correction.
  !
  !       !/T     Enable test output.
  !       !/O7    Echo input homogeneous fields.
  !
  !       !/NCO   NCEP NCO modifications for operational implementation.
  !
  !       !/F90   Timer function included for F90.
  !
  ! 10. Source code :
  !
  !/ ------------------------------------------------------------------- /

  use w3gdatmd              , only : nseal, nsea, dtmax, dtcfl, dtmin, nx, ny, mapsf, w3nmod, w3setg
  use w3wdatmd              , only : time, w3ndat, w3dimw, w3setw
  use w3adatmd              , only : w3naux, w3seta
  use w3idatmd              , only : inflags1, inflags2, w3seti, w3ninp
  use w3idatmd              , only : TC0, CX0, CY0, TCN, CXN, CYN
  use w3idatmd              , only : TW0, WX0, WY0, DT0, TWN, WXN, WYN, DTN
  use w3idatmd              , only : TIN, ICEI, TLN, WLEV
  use w3odatmd              , only : w3nout, w3seto, naproc, iaproc, napout, naperr, nds !cmb
  use w3odatmd              , only : idout, fnmpre, iostyp, nogrp, ngrpp, noge
  use w3initmd              , only : w3init
  use w3wavemd              , only : w3wave
  use w3timemd              , only : stme21
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
  use wav_wrapper_mod       , only : t_startf, t_stopf, t_barrierf
  use wav_wrapper_mod       , only : shr_file_getlogunit, shr_file_setlogunit
  use wav_kind_mod          , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use wav_import_export     , only : advertise_fields, realize_fields, import_fields, export_fields
  use wav_import_export     , only : calcroughl, state_getfldptr, state_fldchk
  use wav_import_export     , only : wav_coupling_to_cice, wav_coupling_to_mom
  use wav_shr_methods       , only : chkerr, state_setscalar, state_getscalar, state_diagnose, alarmInit
  use wav_shr_methods       , only : set_component_logging, get_component_instance, log_clock_advance
  use wav_shr_methods       , only : ymd2date
  use w3gdatmd              , only : dtcfli
!TODO: need shared version
  use w3cesmmd              , only : casename, initfile, rstwr, runtype, histwr, outfreq
  use w3cesmmd              , only : inst_index, inst_name, inst_suffix
#ifdef CESMCOUPLED
  use shr_nl_mod            , only : shr_nl_find_group_name
  use shr_file_mod          , only : shr_file_getunit
  use shr_mpi_mod           , only : shr_mpi_bcast     ! TODO: remove
#endif

  implicit none
  private ! except

  public  :: SetServices
  private :: InitializeP0
  private :: InitializeAdvertise
  private :: InitializeRealize
  private :: ModelSetRunClock
  private :: ModelAdvance
  private :: ModelFinalize

  integer :: stdout
  include "mpif.h"

  !--------------------------------------------------------------------------
  ! Private module data
  !--------------------------------------------------------------------------

#ifdef CESMCOUPLED
  integer :: odat(35)
#else
  integer :: odat(40)
#endif
  character(len=CL)       :: flds_scalar_name = ''
  integer                 :: flds_scalar_num = 0
  integer                 :: flds_scalar_index_nx = 0
  integer                 :: flds_scalar_index_ny = 0
  integer                 :: flds_scalar_index_precip_factor = 0._r8
  logical                 :: profile_memory = .false.

  logical                 :: masterproc
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
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

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

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

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
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

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

    call advertise_fields(importState, exportState, flds_scalar_name, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeAdvertise

  !========================================================================

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)

    ! TODO: remove (used for debugging)
    use w3adatmd,   only : charn
    use w3wdatmd,   only : ust

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer, parameter             :: nhmax = 200
    real                           :: a(nhmax,4)
    type(ESMF_DistGrid)            :: distGrid
    type(ESMF_Mesh)                :: Emesh, EmeshTemp
    type(ESMF_VM)                  :: vm
    type(ESMF_Time)                :: ETime
    type(ESMF_TimeInterval)        :: TimeStep
    character(CL)                  :: cvalue
    integer                        :: shrlogunit
    integer                        :: yy,mm,dd,hh,ss
    integer                        :: dtime_sync        ! integer timestep size
    integer                        :: start_ymd         ! start date (yyyymmdd)
    integer                        :: start_tod         ! start time of day (sec)
    integer                        :: stop_ymd          ! stop date (yyyymmdd)
    integer                        :: stop_tod          ! stop time of day (sec)
    integer                        :: ix, iy
    character(CL)                  :: starttype
    integer                        :: unitn            ! namelist unit number
    integer                        :: ndso, ndse, nds(13), ntrace(2), time0(2)
    integer                        :: timen(2), nh(4), iprt(6)
    integer                        :: J0  ! CMB
    integer                        :: i,j,npts
    integer                        :: ierr
    real, allocatable              :: x(:), y(:)
    integer                        :: n, jsea,isea, ncnt
    integer                        :: ntotal, nlnd
    integer                        :: nlnd_global, nlnd_local
    integer                        :: my_lnd_start, my_lnd_end
    integer, allocatable, target   :: mask_global(:)
    integer, allocatable, target   :: mask_local(:)
    integer, allocatable           :: gindex_lnd(:)
    integer, allocatable           :: gindex_sea(:)
    integer, allocatable           :: gindex(:)
    logical                        :: prtfrm, flt
    logical                        :: flgrd(nogrp,ngrpp)  !flags for gridded output
    logical                        :: flgrd2(nogrp,ngrpp) !flags for coupling output
    logical                        :: flg(nogrp)          !flags for whole group?, probably eliminated now
    logical                        :: flg2(nogrp)         !flags for whole group?
    logical                        :: isPresent, isSet
    character(len=23)              :: dtme21
    integer                        :: iam, mpi_comm
    character(len=10), allocatable :: pnames(:)
    character(len=*),parameter :: subname = '(wav_comp_nuopc:InitializeRealize)'
    character(len=1024)            :: msgString
#ifdef CESMCOUPLED
    integer                        :: odat(35) !HK odat is 35
#else
    integer                        :: odat(40)
#endif
    integer :: lb,ub

    ! -------------------------------------------------------------------

    namelist /ww3_inparm/ initfile, outfreq

    !--------------------------------------------------------------------
    ! Set up data structures
    !--------------------------------------------------------------------

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

    !----------------------------------------------------------------------------
    ! Generate local mpi comm
    !----------------------------------------------------------------------------

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, mpiCommunicator=mpi_comm, peCount=naproc, localPet=iam, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    iaproc = iam + 1

!#ifdef CESMCOUPLED
    !--------------------------------------------------------------------
    ! IO set-up
    !--------------------------------------------------------------------

    ! 1.b For WAVEWATCH III (See W3INIT) ??? ask adrean if i am missing something
    !
    ! The following units are referenced in module w3initmd
    ! NDS(1) ! OUTPUT LOG: General output unit number ("log file") (NDS0)
    ! NDS(2) ! OUTPUT LOG: Error output unit number (NDSE)
    ! NDS(3) ! OUTPUT LOG: Test output unit number (NDST)
    ! NDS(4) ! OUTPUT LOG: Unit for 'direct' output (SCREEN)
    !
    ! NDS(5) ! INPUT: mod_def.ww3 file (model definition) unit number
    ! NDS(9) ! INPUT: unit for read in boundary conditions (based on FLBPI)
    !
    ! The following units are referenced in module w3wavemd for output
    ! NDS( 6) ! OUTPUT DATA: restart(N).ww3 file (model restart) unit number
    ! NDS( 7) ! unit for output for FLOUT(1) flag   out_grd grid unformmatted output
    ! NDS( 8) ! unit for output for FLOUT(2) flag   point unformmatted output
    ! etc through 13
    !
    !----------------------------------------------------------------------------
    ! determine instance information
    !----------------------------------------------------------------------------

    !call get_component_instance(gcomp, inst_suffix, inst_index, rc)
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return
    !inst_name = "WAV"//trim(inst_suffix)

    !----------------------------------------------------------------------------
    ! reset shr logging to my log file
    !----------------------------------------------------------------------------

    napout = 1
    naperr = 1

    if (iaproc == napout) then
       masterproc = .true.
    else
       masterproc = .false.
    end if

    !call set_component_logging(gcomp, masterproc, stdout, shrlogunit, rc)
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Identify available unit numbers
    ! Each ESMF_UtilIOUnitGet is followed by an OPEN statement for that
    ! unit so that subsequent ESMF_UtilIOUnitGet calls do not return the
    ! the same unit.  After getting all the available unit numbers, close
    ! the units since they will be opened within W3INIT.
    nds(1) = stdout
    nds(2) = stdout
    nds(3) = stdout
    nds(4) = stdout
    call ESMF_UtilIOUnitGet(nds(5)) ; open(unit=nds(5)  , status='scratch'); close(nds(5))
    call ESMF_UtilIOUnitGet(nds(6)) ; open(unit=nds(6)  , status='scratch'); close(nds(6))
    call ESMF_UtilIOUnitGet(nds(7)) ; open(unit=nds(7)  , status='scratch'); close(nds(7))
    call ESMF_UtilIOUnitGet(nds(8)) ; open(unit=nds(8)  , status='scratch'); close(nds(8))
    call ESMF_UtilIOUnitGet(nds(9)) ; open(unit=nds(9)  , status='scratch'); close(nds(9))
    call ESMF_UtilIOUnitGet(nds(10)); open(unit=nds(10) , status='scratch'); close(nds(10))
    call ESMF_UtilIOUnitGet(nds(11)); open(unit=nds(11) , status='scratch'); close(nds(11))
    call ESMF_UtilIOUnitGet(nds(12)); open(unit=nds(12) , status='scratch'); close(nds(12))
    call ESMF_UtilIOUnitGet(nds(13)); open(unit=nds(13) , status='scratch'); close(nds(13))
    ndso      =  stdout
    ndse      =  stdout
    ntrace(1) =  nds(3)
    ntrace(2) =  10

    ! Redirect share output to wav log
    call shr_file_getLogUnit (shrlogunit)
    call shr_file_setLogUnit (ndso)
!#endif
    if ( iaproc == napout ) write (ndso,900)

    !--------------------------------------------------------------------
    ! Initialize run type
    !--------------------------------------------------------------------

    call NUOPC_CompAttributeGet(gcomp, name='start_type', value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) starttype

    if (     trim(starttype) == trim('startup')) then
       runtype = "initial"
      if(masterproc) write(ndso,*) 'starttype: initial'
    else if (trim(starttype) == trim('continue') ) then
       runtype = "continue"
       if(masterproc)write(ndso,*) 'starttype: continue'
    else if (trim(starttype) == trim('branch')) then
       runtype = "branch"
       if(masterproc)write(ndso,*) 'starttype: branch'
    end if
#ifdef CESMCOUPLED
    if ( iaproc == napout) then
       write(ndso,*) trim(subname),' inst_name   = ',trim(inst_name)
       write(ndso,*) trim(subname),' inst_index  = ',inst_index
       write(ndso,*) trim(subname),' inst_suffix = ',trim(inst_suffix)
    endif
#else
    inst_index = 1
    inst_suffix = ''
    inst_name = ''
    casename = ''
#endif
    call ESMF_LogWrite('WW3 runtype is '//trim(runtype), ESMF_LOGMSG_INFO)

    !--------------------------------------------------------------------
    ! Define input fields inflags1 and inflags2 settings
    !--------------------------------------------------------------------

    !  fllev   inflags1(1)  flag for water level input.
    !  flcur   inflags1(2)  flag for current input.
    !  flwind  inflags1(3)  flag for wind input.
    !  flice   inflags1(4)  flag for ice input (ice fraction)
    !  flhml   inflags1(5)  flag for mixed layer depth input. ql, 150525 hk

    !  inflags1 array consolidating the above four flags, as well asfour additional data flags.
    !  inflags2 like inflags1 but does *not* get changed when model reads last record of ice.ww3

    !TODO: in current code, inflags1(5) is expected to be momentum, inflags1(6) is the air density
#ifdef CESMCOUPLED
    ! flags for passing variables from coupler to ww3, lev, curr, wind, ice and mixing layer depth on
    inflags1(:) = .false.
    inflags1(1:5) = .true.
#else
   !   FLLEV  => INPUTS(IMOD)%INFLAGS1(1)
   !   FLCUR  => INPUTS(IMOD)%INFLAGS1(2)
   !   FLWIND => INPUTS(IMOD)%INFLAGS1(3)
   !   FLICE  => INPUTS(IMOD)%INFLAGS1(4)
   !   FLTAUA => INPUTS(IMOD)%INFLAGS1(5)
   !   FLRHOA => INPUTS(IMOD)%INFLAGS1(6)

    inflags1(:) = .false.
    inflags1(1:5) = .true.
    !inflags1(3) = .true.       !sa_u10m,sa_v10m
    ! ? actually, I think even if we don't connect these fields, because of
    ! these fields were defined as expected coupling fields in the mod_def, 
    ! we have to pretend to turn them on, even if in the end they're not imported
    ! and are instead set to zero
    !inflags1(2) = .true.       !so_u, so_v
    !inflags1(4) = .true.       !si_ifrac
#endif

    if (wav_coupling_to_cice) then
       inflags1(-7) = .true. ! LR ice thickness
       inflags1(-3) = .true. ! LR ice floe size

       ! LR - I don't understand the difference between inflags1 and inflags2
       ! I am setting them both here to get thickness and floe size import to waves
       ! and to turn on attenuation
       inflags2(-7) = .true. ! LR thickness
       inflags2(-3) = .true. ! LR floe size

       inflags2( 4) = .true. ! LR ????
    end if

    !--------------------------------------------------------------------
    ! Set time frame
    !--------------------------------------------------------------------

    ! TIME0 = from ESMF clock
    ! NOTE - are not setting TIMEN here

    if ( iaproc == napout ) write (ndso,930)

    ! Initial run or restart run
    if ( runtype == "initial") then
       call ESMF_ClockGet( clock, startTime=ETime, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    else
       call ESMF_ClockGet( clock, currTime=ETime, rc=rc )
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif
    call ESMF_TimeGet( ETime, yy=yy, mm=mm, dd=dd, s=start_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ymd2date(yy, mm, dd, start_ymd)

    hh = start_tod/3600
    mm = (start_tod - (hh * 3600))/60
    ss = start_tod - (hh*3600) - (mm*60)

    time0(1) = start_ymd
    time0(2) = hh*10000 + mm*100 + ss

    call ESMF_ClockGet( clock, stopTime=ETime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet( ETime, yy=yy, mm=mm, dd=dd, s=stop_tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ymd2date(yy, mm, dd, stop_ymd)

    hh = stop_tod/3600
    mm = (stop_tod - (hh * 3600))/60
    ss = stop_tod - (hh*3600) - (mm*60)

    timen(1) = stop_ymd
    timen(2) = hh*10000 + mm*100 + ss

    call stme21 ( time0 , dtme21 )
    if ( iaproc .eq. napout ) write (ndso,931) dtme21
    if ( iaproc .eq. napout ) write (ndso,*) 'start_ymd, stop_ymd = ',start_ymd, stop_ymd
    time = time0

    !--------------------------------------------------------------------
    ! Define output type and fields
    !--------------------------------------------------------------------

    iostyp = 1        ! gridded field
    write (ndso,940) 'no dedicated output process, any file system '

    ! Actually will need a new restart flag - since all of the ODAT
    ! should be set to 0 - since they are initializated in w3initmd
    ! ODAT    I.A.   I   Output data, five parameters per output type
    !                          1 YYYMMDD for first output.
    !                          2 HHMMSS for first output.
    !                          3 Output interval in seconds.
    !                          4 YYYMMDD for last output.
    !                          5 HHMMSS for last output.
    !                     1-5  Data for OTYPE = 1; gridded fields.
    !                     6-10 Id.  for OTYPE = 2; point output.
    !                    11-15 Id.  for OTYPE = 3; track point output.
    !                    16-20 Id.  for OTYPE = 4; restart files. (CMB I do not think this is right)
    !                    21-25 Id.  for OTYPE = 5; boundary data.
    !                    26-30 Id.  for OTYPE = 6; ?
    !                    31-35 Id.  for OTYPE = 7; coupled fields
    ! FLGRD   L.A.   I   Flags for gridded output.
    ! NPT     Int.   I   Number of output points
    ! X/YPT   R.A.   I   Coordinates of output points.
    ! PNAMES  C.A.   I   Output point names.

    ! CMB changed odat so all 35 values are initialized here
    do j=1, 7
       odat(5*(j-1)+3) = 0
    end do

    ! get coupling interval
    call ESMF_ClockGet( clock, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeIntervalGet( timeStep, s=dtime_sync, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Hardwire gridded output for now
    ! first output time stamp is now read from file
    ! QL, 150525, 1-5 for history files, 16-20 for restart files
    !     150629, restart output interval is set to the total time of run
    !     150823, restart is taken over by rstwr
    !     160601, output interval is set to coupling interval, so that
    !             variables calculated in W3IOGO could be updated at
    !             every coupling interval
    ! CMB changed odat so all 35 values are set, only permitting one frequency controlled by histwr
    !do J =1,7
    !TODO: odat is now 40
    do J =1,8
       J0 = (j-1)*5
       odat(J0+1) = time(1)     ! YYYYMMDD for first output
       odat(J0+2) = time(2)     ! HHMMSS for first output
       odat(J0+3) = dtime_sync  ! output interval in sec ! changed by Adrean
       odat(J0+4) = 99990101    ! YYYYMMDD for last output
       odat(J0+5) = 0           ! HHMMSS for last output
    end do

    ! output index is now a in a 2D array
    ! IDOUT(NOGRP,NGRPP)
    !   NOGRP = number of output field groups
    !   NGRPP = Max num of parameters per output
    !   NOGE(NOGRP) = number of output group elements

    flgrd(:,:)  = .false.   ! gridded fields
    flgrd2(:,:) = .false.   ! coupled fields, w3init w3iog are not ready to deal with these yet
!TODO: These seem to be set in the namelist file on our side
#ifdef CESMCOUPLED
    ! 1) Forcing fields
    flgrd( 1, 1)  = .false. ! Water depth
    flgrd( 1, 2)  = .false. ! Current vel.
    flgrd( 1, 3)  = .true.  ! Wind speed
    flgrd( 1, 4)  = .false. ! Air-sea temp. dif.
    flgrd( 1, 5)  = .false. ! Water level
    flgrd( 1, 6)  = .true.  ! Ice concentration     !MV - this was changed by CC - do we want this?
    flgrd( 1, 7)  = .false. ! Iceberg damp coeffic

    ! 2) Standard mean wave parameters
    flgrd( 2, 1)  = .true.  ! Wave height
    flgrd( 2, 2)  = .false. ! Mean wave length
    flgrd( 2, 3)  = .true.  ! Mean wave period(+2)
    flgrd( 2, 4)  = .true.  ! Mean wave period(-1)
    flgrd( 2, 5)  = .true.  ! Mean wave period(+1)
    flgrd( 2, 6)  = .true.  ! Peak frequency        !MV - changed by CC - do we want this?
    flgrd( 2, 7)  = .true.  ! Mean wave dir. a1b1   !MV - changed by CC - do we want this?
    flgrd( 2, 8)  = .false. ! Mean dir. spr. a1b1
    flgrd( 2, 9)  = .false. ! Peak direction
    flgrd( 2, 10) = .false. ! Infragravity height
    flgrd( 2, 11) = .false. ! Space-Time Max E
    flgrd( 2, 12) = .false. ! Space-Time Max Std
    flgrd( 2, 13) = .false. ! Space-Time Hmax
    flgrd( 2, 14) = .false. ! Spc-Time Hmax^crest
    flgrd( 2, 15) = .false. ! STD Space-Time Hmax
    flgrd( 2, 16) = .false. ! STD ST Hmax^crest
    flgrd( 2, 17) = .false. ! Dominant wave bT

    ! 3) Frequency-dependent standard parameters
    !HK These were not set to true in the previous CESM version
    !HK whether the 1D Freq. Spectrum gets allocated is decided
    !HK in the grid_inp file
    !HK ~/ww3_toolbox/grids/grid_inp/ww3_grid.inp.ww3a
    !HK namelist section:   &OUTS E3D = 1 /
    flgrd( 3, 1)  = .true.  !1D Freq. Spectrum  !HK EF
    flgrd( 3, 2)  = .false. !Mean wave dir. a1b1
    flgrd( 3, 3)  = .false. !Mean dir. spr. a1b1
    flgrd( 3, 4)  = .false. !Mean wave dir. a2b2
    flgrd( 3, 5)  = .false. !Mean dir. spr. a2b2
    flgrd( 3, 6)  = .false. !Wavenumber array   '

    ! 4) Spectral Partitions parameters
    flgrd( 4, 1)  =  .false. !Part. wave height   '
    flgrd( 4, 2)  =  .false. !Part. peak period   '
    flgrd( 4, 3)  =  .false. !Part. peak wave len.'
    flgrd( 4, 4)  =  .false. !Part. mean direction'
    flgrd( 4, 5)  =  .false. !Part. dir. spread   '
    flgrd( 4, 6)  =  .false. !Part. wind sea frac.'
    flgrd( 4, 7)  =  .false. !Part. peak direction'
    flgrd( 4, 8)  =  .false. !Part. peakedness    '
    flgrd( 4, 9)  =  .false. !Part. peak enh. fac.'
    flgrd( 4,10)  =  .false. !Part. gaussian width'
    flgrd( 4,11)  =  .false. !Part. spectral width'
    flgrd( 4,12)  =  .false. !Part. mean per. (-1)'
    flgrd( 4,13)  =  .false. !Part. mean per. (+1)'
    flgrd( 4,14)  =  .false. !Part. mean per. (+2)'
    flgrd( 4,15)  =  .false. !Part. peak density  '
    flgrd( 4,16)  =  .false. !Total wind sea frac.'
    flgrd( 4,17)  =  .false. !Number of partitions'

    ! 5) Atmosphere-waves layer
    flgrd( 5, 1)  = .false. !Friction velocity   '
    flgrd( 5, 2)  = .false. !Charnock parameter  '
    flgrd( 5, 3)  = .false. !Energy flux         '
    flgrd( 5, 4)  = .false. !Wind-wave enrgy flux'
    flgrd( 5, 5)  = .false. !Wind-wave net mom. f'
    flgrd( 5, 6)  = .false. !Wind-wave neg.mom.f.'
    flgrd( 5, 7)  = .false. !Whitecap coverage   '
    flgrd( 5, 8)  = .false. !Whitecap mean thick.'
    flgrd( 5, 9)  = .false. !Mean breaking height'
    flgrd( 5,10)  = .false. !Dominant break prob '
    flgrd( 5,11)  = .false. !Breaker passage rate'

    ! 6) Wave-ocean layer
    flgrd( 6, 1)  = .false. !'Radiation stresses  '
    flgrd( 6, 2)  = .false. !'Wave-ocean mom. flux'
    flgrd( 6, 3)  = .false. !'wave ind p Bern Head'
    flgrd( 6, 4)  = .false. !'Wave-ocean TKE  flux'
    flgrd( 6, 5)  = .false. !'Stokes transport    '
    flgrd( 6, 6)  = .true. !'Stokes drift at z=0 '
    flgrd( 6, 7)  = .false. !'2nd order pressure  '
    flgrd( 6, 8)  = .false. !'Stokes drft spectrum'
    flgrd( 6, 9)  = .false. !'2nd ord press spectr'
    flgrd( 6,10)  = .false. !'Wave-ice mom. flux  '
    flgrd( 6,11)  = .false. !'Wave-ice energy flux'
    flgrd( 6,12)  = .false. !'Split Surface Stokes'
    !aa
    flgrd( 6,13)  = .true. ! Turbulent Langmuir number (La_t)

    ! 7) Wave-bottom layer
    flgrd( 7, 1)  = .false. !'Bottom rms ampl.    '
    flgrd( 7, 2)  = .false. !'Bottom rms velocity '
    flgrd( 7, 3)  = .false. !'Bedform parameters  '
    flgrd( 7, 4)  = .false. !'Energy diss. in WBBL'
    flgrd( 7, 5)  = .false. !'Moment. loss in WBBL'

    ! 8) Spectrum parameters
    flgrd( 8, 1)  = .false. !'Mean square slopes  '
    flgrd( 8, 2)  = .false. !'Phillips tail const'
    flgrd( 8, 3)  = .false. !'Slope direction     '
    flgrd( 8, 4)  = .false. !'Tail slope direction'
    flgrd( 8, 5)  = .false. !'Goda peakedness parm'

    ! 9) Numerical diagnostics
    flgrd( 9, 1)  = .false. !'Avg. time step.     '
    flgrd( 9, 2)  = .false. !'Cut-off freq.       '
    flgrd( 9, 3)  = .false. !'Maximum spatial CFL '
    flgrd( 9, 4)  = .false. !'Maximum angular CFL '
    flgrd( 9, 5)  = .false. !'Maximum k advect CFL'

    ! 10) is user defined
#endif

    !CMB document which fields to be output to first hist file in wav.log
    if ( iaproc .eq. napout ) then
       flt = .true.
       do i=1, nogrp
          do j=1, noge(i)
             if ( flgrd(i,j) ) then
                if ( flt ) then
                   write (ndso,1945) idout(i,j)
                   flt = .false.
                else
                   write (ndso,1946) idout(i,j)
                end if
             end if
          end do
       end do
       if ( flt ) write (ndso,1945) 'no fields defined'
    end if
    !--------------------------------------------------------------------
    ! Wave model initializations
    !--------------------------------------------------------------------

!#ifdef CESMCOUPLED
    ! Notes on ww3 initialization:
    ! ww3 read initialization occurs in w3iors (which is called by initmd)
    ! For a startup (including hybrid) or branch run the initial datafile is
    ! set in namelist input 'initfile'
    ! For a continue run - the initfile vluae is created from the time(1:2)
    ! array set below

    if ( iaproc .eq. napout ) write (ndso,950)
    if ( iaproc .eq. napout ) write (ndso,951) 'wave model ...'

    ! Read namelist (set initfile in w3cesmmd)
    if ( iaproc .eq. napout ) then
       write(ndso,*) 'Read in ww3_inparm namelist from wav_in'//trim(inst_suffix)
       open(newunit=unitn, file='wav_in'//trim(inst_suffix), status='old')
       !call shr_nl_find_group_name(unitn, 'ww3_inparm', status=ierr)
       if (ierr == 0) then
          read (unitn, ww3_inparm, iostat=ierr)
          if (ierr /= 0) then
             call ESMF_LogWrite(trim(subname)//' problem reading ww3_inparm namelist',&
                  ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
             rc = ESMF_FAILURE
             return
          end if
       end if
       close( unitn )
    end if
    !call shr_mpi_bcast(initfile, mpi_comm)
    !call shr_mpi_bcast(outfreq, mpi_comm)

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

    ! Set casename (in w3cesmmd)
    call NUOPC_CompAttributeGet(gcomp, name="case_name", value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(cvalue,*) casename
       call ESMF_LogWrite(trim(subname)//' case_name = '//trim(casename), ESMF_LOGMSG_INFO)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if
!#else
    ! TODO: UFS fills this in
!#endif

    ! Read in input data and initialize the model
    ! w3init calls w3iors which:
    ! - reads either the initfile if the run is startup or branch
    ! - constructs the filename from the casename variable and the time(:) array
    !   which is set above

    !HK npts, pnames are fpr point output
    npts = 0
    allocate ( x(1), y(1), pnames(1) )
    pnames(1) = ' '

    call ESMF_LogWrite(trim(subname)//' calling = w3init', ESMF_LOGMSG_INFO)
    !HK flgrd2 is flags for coupling output, not ready yet so keep .false.
    !HK 1 is model number
    !HK IsMulti does not appear to be used, setting to .true.
#ifdef CESMCOUPLED
    call w3init ( 1, .true., 'ww3', nds, ntrace, odat, flgrd, flgrd2, flg, flg2, &
         npts, x, y, pnames, iprt, prtfrm, mpi_comm )

    ! gx17
    !180.0000       180.0000       180.0000       15.00000
    dtmax  = 1800.0000 ! LR
    dtcfl  = 600.0000
    dtcfli = 1800.0000
    dtmin  = 1800.00000
#else
    call w3init ( 1, .false., 'ww3', nds, ntrace, odat, flgrd, flgrd2, flg, flg2, &
         npts, x, y, pnames, iprt, prtfrm, mpi_comm )
    ! TODO: do we need these, are they in eg ww3_grid.inp.glo_1deg_ori
    dtmax  = 1800.0000
    dtcfl  = 450.0000
    dtcfli = 900.0000
    dtmin  = 30.00000
#endif
    lb = lbound(ust,1); ub = ubound(ust,1)
    write(msgString,'(A,2i7,10g14.7)')'w3init ust ',lb,ub, ust((ub-lb)/2:9+(ub-lb)/2)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    lb = lbound(charn,1); ub = ubound(charn,1)
    write(msgString,'(A,2i7,10g14.7)')'w3init charn ',lb,ub, charn((ub-lb)/2:9+(ub-lb)/2)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    call ESMF_LogWrite(trim(subname)//' done = w3init', ESMF_LOGMSG_INFO)

    call mpi_barrier ( mpi_comm, ierr )

    !--------------------------------------------------------------------
    ! Mesh initialization
    !--------------------------------------------------------------------

    ! Note that nsea is the global number of sea points - and nseal is
    ! the local number of sea points

    !-------------
    ! create a  global index array for sea points
    !-------------
    allocate(gindex_sea(nseal))
    do jsea=1, nseal
       isea = iaproc + (jsea-1)*naproc
       ix = mapsf(isea,1)
       iy = mapsf(isea,2)
       gindex_sea(jsea) = ix + (iy-1)*nx
    end do

    !-------------
    ! create a global index array for non-sea (i.e. land points)
    !-------------
    allocate(mask_global(nx*ny), mask_local(nx*ny))
    mask_local(:) = 0
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
       if (mask_global(n) == 0) then ! this is a land pont
          ncnt = ncnt + 1
          if (ncnt >= my_lnd_start .and. ncnt <= my_lnd_end) then
             gindex_lnd(ncnt - my_lnd_start + 1) = n
          end if
       end if
    end do
    deallocate(mask_global)

    !-------------
    ! create a global index that includes both sea and land - but put land at the end
    !-------------
    nlnd = (my_lnd_end - my_lnd_start + 1)
    allocate(gindex(nlnd + nseal))
    do ncnt = 1,nlnd + nseal
       if (ncnt <= nseal) then
          gindex(ncnt) = gindex_sea(ncnt)
       else
          gindex(ncnt) = gindex_lnd(ncnt-nseal)
       end if
    end do

    !-------------
    ! create distGrid from global index array
    !-------------
    DistGrid = ESMF_DistGridCreate(arbSeqIndexList=gindex, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !-------------
    ! create the mesh
    !-------------
    call NUOPC_CompAttributeGet(gcomp, name='mesh_wav', value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! read in the mesh with an auto-generated distGrid
    EMeshTemp = ESMF_MeshCreate(filename=trim(cvalue), fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (masterproc) then
       write(stdout,*)'mesh file for domain is ',trim(cvalue)
    end if

    ! recreate the mesh using the above distGrid
    EMesh = ESMF_MeshCreate(EMeshTemp, elementDistgrid=Distgrid, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------------------------------------------
    ! Realize the actively coupled fields
    !--------------------------------------------------------------------
    call realize_fields(gcomp, mesh=Emesh, flds_scalar_name=flds_scalar_name, flds_scalar_num=flds_scalar_num, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------------------------------------------
    ! end redirection of share output to wav log
    !--------------------------------------------------------------------
    call shr_file_setlogunit (shrlogunit)


900 FORMAT (/15X,'      *** WAVEWATCH III Program shell ***      '/ &
         15X,'==============================================='/)
901 FORMAT ( '  Comment character is ''',A,''''/)

930 FORMAT (/'  Time interval : '/                                  &
         ' --------------------------------------------------')
931 FORMAT ( '       Starting time : ',A)

940 FORMAT (/'  Output requests : '/                                &
         ' --------------------------------------------------'/ &
         '       ',A)

950 FORMAT (/'  Initializations :'/                                 &
         ' --------------------------------------------------')
951 FORMAT ( '       ',A)
1945 FORMAT ( '            Fields   : ',A)
1946 FORMAT ( '                       ',A)

  end subroutine InitializeRealize

  !===============================================================================

  subroutine DataInitialize(gcomp, rc)

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
    character(len=*),parameter :: subname = '(wav_comp_nuopc:DataInitialize)'
    ! -------------------------------------------------------------------

    !--------------------------------------------------------------------
    ! Create export state
    !--------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' entered', ESMF_LOGMSG_INFO)

    call NUOPC_ModelGet(gcomp, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
#ifdef CESMCOUPLED
    call state_getfldptr(exportState, 'Sw_lamult', sw_lamult, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sw_ustokes', sw_ustokes, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sw_vstokes', sw_vstokes, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    sw_lamult (:) = 1.
    sw_ustokes(:) = 0.
    sw_vstokes(:) = 0.
#else
    call state_getfldptr(exportState, 'Sw_z0', fldptr1d=z0rlen, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call CalcRoughl(z0rlen)
#endif

    if (wav_coupling_to_cice) then
      call state_getfldptr(exportState, 'wav_tauice1', wav_tauice1, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call state_getfldptr(exportState, 'wav_tauice2', wav_tauice2, rc=rc)
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

      wav_tauice1              (:) = 0.
      wav_tauice2              (:) = 0.
      wave_elevation_spectrum1 (:) = 0.
      wave_elevation_spectrum2 (:) = 0.
      wave_elevation_spectrum3 (:) = 0.
      wave_elevation_spectrum4 (:) = 0.
      wave_elevation_spectrum5 (:) = 0.
      wave_elevation_spectrum6 (:) = 0.
      wave_elevation_spectrum7 (:) = 0.
      wave_elevation_spectrum8 (:) = 0.
      wave_elevation_spectrum9 (:) = 0.
      wave_elevation_spectrum10(:) = 0.
      wave_elevation_spectrum11(:) = 0.
      wave_elevation_spectrum12(:) = 0.
      wave_elevation_spectrum13(:) = 0.
      wave_elevation_spectrum14(:) = 0.
      wave_elevation_spectrum15(:) = 0.
      wave_elevation_spectrum16(:) = 0.
      wave_elevation_spectrum17(:) = 0.
      wave_elevation_spectrum18(:) = 0.
      wave_elevation_spectrum19(:) = 0.
      wave_elevation_spectrum20(:) = 0.
      wave_elevation_spectrum21(:) = 0.
      wave_elevation_spectrum22(:) = 0.
      wave_elevation_spectrum23(:) = 0.
      wave_elevation_spectrum24(:) = 0.
      wave_elevation_spectrum25(:) = 0.
    endif

    ! Set global grid size scalars in export state
    call State_SetScalar(dble(NX), flds_scalar_index_nx, exportState, &
         flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call State_SetScalar(dble(NY), flds_scalar_index_ny, exportState, &
         flds_scalar_name, flds_scalar_num, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call state_diagnose(exportState, 'at DataInitialize ', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine DataInitialize

  !=====================================================================

  subroutine ModelAdvance(gcomp, rc)

    !------------------------
    ! Run WW3
    !------------------------

    ! arguments:
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State) :: importState
    type(ESMF_State) :: exportState
    type(ESMF_Clock) :: clock
    type(ESMF_Alarm) :: alarm
    type(ESMF_TIME)  :: ETime

    type(ESMF_TimeInterval)    :: timeStep
    type(ESMF_Time)            :: currTime, startTime, stopTime

    integer          :: yy,mm,dd,hh,ss
    integer          :: ymd        ! current year-month-day
    integer          :: tod        ! current time of day (sec)
    integer          :: time0(2)
    integer          :: timen(2)
    logical          :: lerr
    integer          :: shrlogunit ! original log unit and level
    character(len=256)         :: msgString
    character(len=*),parameter :: subname = '(wav_comp_nuopc:ModelAdvance) '
    !-------------------------------------------------------
    rc = ESMF_SUCCESS
    call ESMF_LogWrite(trim(subname)// ': entered ModelAdvance', ESMF_LOGMSG_INFO)

    !------------
    ! Reset shr logging to my log file
    !------------
    call shr_file_getLogUnit (shrlogunit)
    call shr_file_setLogUnit (stdout)

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
    ! Determine if time to write restart
    !------------
    !TODO: how is CESM controlling restarts inside of WW3?
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

    !------------
    ! Determine time info
    !------------

    ! use current time for next time step the NUOPC clock is not updated
    ! until the end of the time interval
    call ESMF_ClockGetNextTime(clock, nextTime=ETime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet( ETime, yy=yy, mm=mm, dd=dd, s=tod, rc=rc )
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ymd2date(yy, mm, dd, ymd)
    hh = tod/3600
    mm = (tod - (hh * 3600))/60
    ss = tod - (hh*3600) - (mm*60)
    write(stdout,*) 'ymd2date wav_comp_nuopc hh,mm,ss,ymd', hh,mm,ss,ymd

    timen(1) = ymd
    timen(2) = hh*10000 + mm*100 + ss

    time0(1) = ymd
    time0(2) = hh*10000 + mm*100 + ss

    time = time0

    !------------
    ! Determine if time to write history
    !------------
    histwr = .false.
    if (outfreq .gt. 0) then
      if( mod(hh, outfreq) .eq. 0 ) then
       ! output every outfreq hours
       histwr = .true.
      endif
    endif
#ifdef CESMCOUPLED
    if (.not. histwr) then
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
#endif

    !    write(stdout,*) 'CMB wav_comp_nuopc time', time, timen
    !    write(stdout,*) 'ww3 hist flag ', histwr, outfreq, hh, mod(hh, outfreq)

    !------------
    ! Obtain import data from import state
    !------------
    call import_fields(gcomp, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------
    ! Run the wave model for the given interval
    !------------
    if(profile_memory) call ESMF_VMLogMemInfo("Entering WW3 Run : ")
#ifdef CESMCOUPLED
    call w3wave ( 1, timen )
#else
    call w3wave ( 1, odat, timen )
#endif
    if(profile_memory) call ESMF_VMLogMemInfo("Exiting  WW3 Run : ")

    !------------
    ! Create export state
    !------------

    call export_fields(gcomp, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------
    ! Reset shr logging to original values
    !------------

    call shr_file_setLogUnit (shrlogunit)

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
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

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
       call ESMF_LogWrite(subname//'setting alarms for ' // trim(name), ESMF_LOGMSG_INFO)

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
#ifdef CESMCOUPLED
       !TODO: Are these required? CMEPS writes history
       !----------------
       ! History alarm
       !----------------
       call NUOPC_CompAttributeGet(gcomp, name="history_option", value=history_option, rc=rc)
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
#endif
    end if

    !--------------------------------
    ! Advance model clock to trigger alarms then reset model clock back to currtime
    !--------------------------------

    call ESMF_ClockAdvance(mclock,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockSet(mclock, currTime=dcurrtime, timeStep=dtimestep, stopTime=mstoptime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

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

    !--------------------------------
    ! Finalize routine
    !--------------------------------

    print*, 'HK model finalize: ',  dtmax, dtcfl, dtcfli, dtmin
    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    if (masterproc) then
       write(stdout,F91)
       write(stdout,F00) 'WW3: end of main integration loop'
       write(stdout,F91)
    end if

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine ModelFinalize

  !===============================================================================

end module wav_comp_nuopc
