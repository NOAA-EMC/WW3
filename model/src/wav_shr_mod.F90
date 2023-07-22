!> @file wav_shr_mod
!!
!> Shared utility routines
!!
!> @details Contains public routines to execute repeated operations
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 01-05-2022
module wav_shr_mod

  use ESMF            , only : operator(<), operator(/=), operator(+)
  use ESMF            , only : operator(-), operator(*) , operator(>=)
  use ESMF            , only : operator(<=), operator(>), operator(==)
  use ESMF            , only : ESMF_LOGERR_PASSTHRU, ESMF_LogFoundError, ESMF_LOGMSG_ERROR, ESMF_MAXSTR
  use ESMF            , only : ESMF_SUCCESS, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_FAILURE
  use ESMF            , only : ESMF_State, ESMF_StateGet, ESMF_StateItem_Flag, ESMF_STATEITEM_NOTFOUND
  use ESMF            , only : ESMF_Field, ESMF_FieldGet
  use ESMF            , only : ESMF_GridComp, ESMF_GridCompGet, ESMF_GridCompSet
  use ESMF            , only : ESMF_GeomType_Flag, ESMF_FieldStatus_Flag
  use ESMF            , only : ESMF_Mesh, ESMF_MeshGet
  use ESMF            , only : ESMF_GEOMTYPE_MESH, ESMF_GEOMTYPE_GRID, ESMF_FIELDSTATUS_COMPLETE
  use ESMF            , only : ESMF_Clock, ESMF_ClockCreate, ESMF_ClockGet, ESMF_ClockSet
  use ESMF            , only : ESMF_ClockPrint, ESMF_ClockAdvance
  use ESMF            , only : ESMF_Alarm, ESMF_AlarmCreate, ESMF_AlarmGet, ESMF_AlarmSet
  use ESMF            , only : ESMF_Calendar, ESMF_CALKIND_NOLEAP, ESMF_CALKIND_GREGORIAN
  use ESMF            , only : ESMF_Time, ESMF_TimeGet, ESMF_TimeSet
  use ESMF            , only : ESMF_TimeInterval, ESMF_TimeIntervalSet, ESMF_TimeIntervalGet
  use ESMF            , only : ESMF_VM, ESMF_VMGet, ESMF_VMBroadcast, ESMF_VMGetCurrent
  use NUOPC           , only : NUOPC_CompAttributeGet
  use NUOPC_Model     , only : NUOPC_ModelGet
  use wav_kind_mod    , only : r8 => shr_kind_r8, i8 => shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs
  use wav_kind_mod    , only : i4 => shr_kind_i4

  implicit none
  private

  public  :: state_getscalar   !< @public obtain a scalar field from a state
  public  :: state_setscalar   !< @public set scalar data from state for a particular name
  public  :: state_reset       !< @public reset field values in a state
  public  :: state_getfldptr   !< @public obtain a pointer to a field in a state
  public  :: state_fldchk      !< @public check whether a field is in a state
  public  :: state_diagnose    !< @public print min,max,sum and size of a field in a state
  public  :: alarmInit         !< @public set up an alarm in a clock
  public  :: chkerr            !< @public check if an error was returned from and ESMF call
  public  :: ymd2date          !< @public convert  year,month,day to integer
  private :: timeInit          !< @public create an ESMF_Time object
  private :: field_getfldptr   !< @private obtain a pointer to a field
  public  :: diagnose_mesh     !< @public write out info about mesh
  public  :: write_meshdecomp  !< @public write the mesh decomposition to a file

  interface state_getfldptr
    module procedure state_getfldptr_1d
    module procedure state_getfldptr_2d
  end interface state_getfldptr

  ! used by both CESM and UFS
  logical            , public :: wav_coupling_to_cice = .false. !< @public flag to specify additional wave export
                                                                !! fields for coupling to CICE (TODO: generalize)
  integer, parameter , public :: nwav_elev_spectrum = 25        !< the size of the wave spectrum exported if coupling
                                                                !! waves to cice6
  integer            , public :: dbug_flag = 0                  !< @public flag used to produce additional output
  logical            , public :: unstr_mesh = .false.           !< @public flag to specify use of unstructured mesh
  character(len=256) , public :: casename = ''                  !< @public the name pre-prended to an output file

  ! Only used by cesm and optionally by uwm
  ! to construct the initial file and used in W3IORSMD
  ! if a run is a continue run, then casename is used to construct
  ! the restart filename in W3IORSMD
  integer            , public :: inst_index  !< @public number of current instance (ie 1)
  character(len=16)  , public :: inst_name   !< @public fullname of current instance (ie "wav_0001")
  character(len=16)  , public :: inst_suffix !< @public  char string associated with instance

  ! Only used by ufs
  logical            , public :: merge_import  = .false.  !< @public logical to specify whether import fields will
                                                          !! be merged with a field provided from a file
  logical            , public :: multigrid = .false.      !< @public logical to control whether wave model is run
                                                          !! as multigrid

  interface ymd2date
    module procedure ymd2date_int
    module procedure ymd2date_long
  end interface ymd2date

  ! Clock and alarm option
  character(len=*), private, parameter :: &
       optNONE           = "none"      , &             !< alarm option none
       optNever          = "never"     , &             !< alarm option never
       optNSteps         = "nsteps"    , &             !< alarm option nsteps
       optNStep          = "nstep"     , &             !< alarm option nstep
       optNSeconds       = "nseconds"  , &             !< alarm option nseconds
       optNSecond        = "nsecond"   , &             !< alarm option nsecond
       optNMinutes       = "nminutes"  , &             !< alarm option nminutes
       optNMinute        = "nminute"   , &             !< alarm option nminute
       optNHours         = "nhours"    , &             !< alarm option nhours
       optNHour          = "nhour"     , &             !< alarm option nhour
       optNDays          = "ndays"     , &             !< alarm option ndays
       optNDay           = "nday"      , &             !< alarm option nday
       optNMonths        = "nmonths"   , &             !< alarm option nmonths
       optNMonth         = "nmonth"    , &             !< alarm option nmonth
       optNYears         = "nyears"    , &             !< alarm option nyears
       optNYear          = "nyear"     , &             !< alarm option nyear
       optMonthly        = "monthly"   , &             !< alarm option monthly
       optYearly         = "yearly"    , &             !< alarm option yearly
       optDate           = "date"      , &             !< alarm option date
       optIfdays0        = "ifdays0"                   !< alarm option for number of days 0

  ! Module data
  character(len=*), parameter :: u_FILE_u = &          !< a character string for an ESMF log message
       __FILE__

  !===============================================================================
contains
  !===============================================================================
  !> Get properties of a mesh
  !!
  !! @param[in]    EMeshIn          an ESMF Mesh
  !! @param[in]    gindex_size      the length of the gindex
  !! @param[in]    mesh_name        a name to identify the mesh in the PET log
  !! @param[out]   rc               a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 09-12-2022
  subroutine diagnose_mesh(EMeshIn, gindex_size, mesh_name, rc)

    use ESMF          , only : ESMF_Mesh, ESMF_LOGMSG_Info

    ! input/output variables
    type(ESMF_Mesh) , intent(in)  :: EMeshIn
    integer         , intent(in)  :: gindex_size
    character(len=*), intent(in)  :: mesh_name
    integer         , intent(out) :: rc

    !local variables
    logical                :: elementCoordsIsPresent
    logical                :: elementDistGridIsPresent
    logical                :: nodalDistGridIsPresent
    logical                :: elementMaskIsPresent
    logical                :: nodeMaskIsPresent
    character(ESMF_MAXSTR) :: msgString

    integer                :: ncnt,ecnt,lb,ub
    integer                :: nowndn, nownde
    integer, allocatable   :: nids(:), eids(:), nowners(:)
    character(len=*),parameter :: subname = '(wav_shr_mod:mesh_diagnose) '
    !-------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag  > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    !The Mesh class is distributed by elements. This means that a node must be present on any PET that
    !contains an element associated with that node, but not on any other PET (a node can't be on a PET
    !without an element "home"). Since a node may be used by two or more elements located on different
    !PETS, a node may be duplicated on multiple PETs. When a node is duplicated in this manner, one and
    !only one of the PETs that contain the node must "own" the node. The user sets this ownership when they
    !define the nodes during Mesh creation. When a Field is created on a Mesh (i.e. on the Mesh nodes),
    !on each PET the Field is only created on the nodes which are owned by that PET. This means that the
    !size of the Field memory on the PET can be smaller than the number of nodes used to create the Mesh
    !on that PET.

    !The node id is a unique (across all PETs) integer attached to the particular node. It is used to
    !indicate which nodes are the same when connecting together pieces of the Mesh on different processors.
    !The node owner indicates which PET is in charge of the node

    !The element id is a unique (across all PETs) integer attached to the particular element. The element
    !connectivity indicates which nodes are to be connected together to form the element. The entries
    !in this list are NOT the global ids of the nodes, but are indices into the PET local lists of node info used
    !in the Mesh Create.

    call ESMF_MeshGet(EMeshIn, nodeCount=ncnt, elementCount=ecnt, &
         numOwnedElements=nownde, numOwnedNodes=nowndn, &
         elementCoordsIsPresent=elementCoordsIsPresent, &
         elementDistGridIsPresent=elementDistGridIsPresent,&
         nodalDistGridIsPresent=nodalDistGridIsPresent, &
         elementMaskIsPresent=elementMaskIsPresent, &
         nodeMaskIsPresent=nodeMaskIsPresent, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    write(msgString,'(5(a,i6))')trim(mesh_name)//' Info: Node Cnt = ',ncnt,' Elem Cnt = ',ecnt, &
         ' num Owned Elms = ',nownde,' num Owned Nodes = ',nowndn,&
         ' Gindex size = ',gindex_size
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    allocate(nids(ncnt))
    allocate(nowners(ncnt))
    allocate(eids(ecnt))

    if (elementDistGridIsPresent) call ESMF_LogWrite('element Distgrid is Present', ESMF_LOGMSG_INFO)
    if (nodalDistGridIsPresent) call ESMF_LogWrite('nodal Distgrid is Present', ESMF_LOGMSG_INFO)
    if (elementMaskIsPresent) call ESMF_LogWrite('element Mask is Present', ESMF_LOGMSG_INFO)
    if (nodeMaskIsPresent) call ESMF_LogWrite('node Mask is Present', ESMF_LOGMSG_INFO)
    if (elementCoordsIsPresent) call ESMF_LogWrite('element Coords is Present', ESMF_LOGMSG_INFO)

    call ESMF_MeshGet(EMeshIn, nodeIds=nids, nodeOwners=nowners, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    lb = lbound(nids,1); ub = ubound(nids,1)
    write(msgString,'(a,12i8)')trim(mesh_name)//' : NodeIds(lb:lb+9) = ',lb,ub,nids(lb:lb+9)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    write(msgString,'(a,12i8)')trim(mesh_name)//' : NodeOwners(lb:lb+9) = ',lb,ub,nowners(lb:lb+9)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    write(msgString,'(a,12i8)')trim(mesh_name)//' : NodeIds(ub-9:ub) = ',lb,ub,nids(ub-9:ub)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    write(msgString,'(a,12i8)')trim(mesh_name)//' : NodeOwners(ub-9:ub) = ',lb,ub,nowners(ub-9:ub)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    write(msgString,'(a,12i8)')trim(mesh_name)//' : NodeOwners min,max = ',minval(nowners),maxval(nowners)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    ! some methods not avail when using a dual mesh
    if (.not. unstr_mesh) then
      call ESMF_MeshGet(EMeshIn, elementIds=eids, rc=rc)
      lb = lbound(eids,1); ub = ubound(eids,1)
      write(msgString,'(a,12i8)')trim(mesh_name)//' : ElemIds(lb:lb+9) = ',lb,ub,eids(lb:lb+9)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
      write(msgString,'(a,12i8)')trim(mesh_name)//' : ElemIds(ub-9:ub) = ',lb,ub,eids(ub-9:ub)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    end if
    deallocate(nids)
    deallocate(eids)
    deallocate(nowners)

    if (dbug_flag  > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine diagnose_mesh
  !===============================================================================
  !> Write the mesh decomposition to a file
  !!
  !! @param[in]    EMeshIn          an ESMF Mesh
  !! @param[in]    mesh_name        a name to identify the mesh
  !! @param[out]   rc               a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 09-12-2022
  subroutine write_meshdecomp(EMeshIn, mesh_name, rc)

    use ESMF          , only : ESMF_Mesh, ESMF_DistGrid, ESMF_Field, ESMF_FieldBundle, ESMF_FieldBundleAdd
    use ESMF          , only : ESMF_DistGridGet, ESMF_FieldBundleCreate, ESMF_FieldCreate, ESMF_FieldBundleGet
    use ESMF          , only : ESMF_MESHLOC_ELEMENT, ESMF_TYPEKIND_R8, ESMF_TYPEKIND_I4, ESMF_LOGMSG_Info
    use ESMF          , only : ESMF_FieldBundleWrite, ESMF_FieldBundleDestroy

    use w3odatmd      , only : iaproc

    ! input/output variables
    type(ESMF_Mesh) , intent(in)  :: EMeshIn
    character(len=*), intent(in)  :: mesh_name
    integer         , intent(out) :: rc

    ! local variables
    type(ESMF_FieldBundle)         :: FBTemp
    type(ESMF_Field)               :: lfield
    type(ESMF_DistGrid)            :: distgrid
    type(ESMF_Field)               :: doffield
    character(len=6), dimension(4) :: lfieldlist
    integer                        :: i,ndims,nelements
    real(r8), pointer              :: fldptr1d(:)
    integer(i4), allocatable       :: dof(:)
    integer(i4), pointer           :: dofptr(:)
    real(r8), pointer              :: ownedElemCoords(:), ownedElemCoords_x(:), ownedElemCoords_y(:)
    character(len=*),parameter     :: subname = '(wav_shr_mod:write_meshdecomp) '
    !-------------------------------------------------------

    rc = ESMF_SUCCESS
    if (dbug_flag  > 5) call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    ! create a temporary FB to write the fields
    FBtemp = ESMF_FieldBundleCreate(rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_MeshGet(EMeshIn, spatialDim=ndims, numOwnedElements=nelements, elementDistgrid=distgrid, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    lfieldlist = (/'dof   ', 'coordx', 'coordy', 'decomp'/)
    ! index array
    doffield = ESMF_FieldCreate(EMeshIn, ESMF_TYPEKIND_I4, name=trim(lfieldlist(1)), &
         meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldBundleAdd(FBTemp, (/doffield/), rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    ! coords and decomp field
    do i = 2,size(lfieldlist)
      lfield = ESMF_FieldCreate(EMeshIn, ESMF_TYPEKIND_R8, name=trim(lfieldlist(i)), &
           meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call ESMF_FieldBundleAdd(FBTemp, (/lfield/), rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
    end do

    ! Set element coordinates
    allocate(ownedElemCoords(ndims*nelements))
    allocate(ownedElemCoords_x(ndims*nelements/2))
    allocate(ownedElemCoords_y(ndims*nelements/2))
    call ESMF_MeshGet(EmeshIn, ownedElemCoords=ownedElemCoords, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    ownedElemCoords_x(1:nelements) = ownedElemCoords(1::2)
    ownedElemCoords_y(1:nelements) = ownedElemCoords(2::2)
    allocate(dof(1:nelements))
    call ESMF_DistGridGet(distgrid, localDE=0, seqIndexList=dof, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldBundleGet(FBtemp, fieldName='dof', field=doffield, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(doffield, farrayPtr=dofptr, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    dofptr(:) = dof(:)

    call ESMF_FieldBundleGet(FBtemp, fieldName='coordx', field=lfield, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr1d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    fldptr1d(:) = ownedElemCoords_x(:)

    call ESMF_FieldBundleGet(FBtemp, fieldName='coordy', field=lfield, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr1d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    fldptr1d(:) = ownedElemCoords_y(:)

    call ESMF_FieldBundleGet(FBtemp, fieldName='decomp', field=lfield, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr1d, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    do i = 1,ndims*nelements/2
      fldptr1d(i) = iaproc
    end do
    call ESMF_FieldBundleWrite(FBtemp, filename=trim(mesh_name)//'.decomp.nc', overwrite=.true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    deallocate(ownedElemCoords)
    deallocate(ownedElemCoords_x)
    deallocate(ownedElemCoords_y)
    deallocate(dof)

    call ESMF_FieldBundleDestroy(FBtemp, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    if (dbug_flag  > 5) call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)
  end subroutine write_meshdecomp

  !===============================================================================
  !> Get scalar data from a state
  !!
  !> @details Obtain the field flds_scalar_name from a State and broadcast and
  !! it to all PEs
  !!
  !! @param[in]    State            an ESMF_State
  !! @param[in]    scalar_value     the value of the scalar
  !! @param[in]    scalar_id        the identity of the scalar
  !! @param[in]    flds_scalar_name the name of the scalar
  !! @param[in]    flds_scalar_num  the number of scalars
  !! @param[out]   rc               a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine state_getscalar(state, scalar_id, scalar_value, flds_scalar_name, flds_scalar_num, rc)

    ! ----------------------------------------------
    ! Get scalar data from State for a particular name and broadcast it to all other pets
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_State), intent(in)     :: state
    integer,          intent(in)     :: scalar_id
    real(r8),         intent(out)    :: scalar_value
    character(len=*), intent(in)     :: flds_scalar_name
    integer,          intent(in)     :: flds_scalar_num
    integer,          intent(inout)  :: rc

    ! local variables
    integer           :: mytask, ierr, len
    type(ESMF_VM)     :: vm
    type(ESMF_Field)  :: field
    real(r8), pointer :: farrayptr(:,:)
    real(r8)          :: tmp(1)
    character(len=*), parameter :: subname = ' (wav_shr_mod:state_getscalar) '
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, localPet=mytask, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_StateGet(State, itemName=trim(flds_scalar_name), field=field, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    if (mytask == 0) then
      call ESMF_FieldGet(field, farrayPtr = farrayptr, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      if (scalar_id < 0 .or. scalar_id > flds_scalar_num) then
        call ESMF_LogWrite(trim(subname)//": ERROR in scalar_id", ESMF_LOGMSG_INFO, line=__LINE__, file=u_FILE_u)
        rc = ESMF_FAILURE
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
      endif
      tmp(:) = farrayptr(scalar_id,:)
    endif
    call ESMF_VMBroadCast(vm, tmp, 1, 0, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    scalar_value = tmp(1)

  end subroutine state_getscalar

  !================================================================================
  !> Set scalar data into a state
  !!
  !! Called by fldlist_realize to set the required scalar data into a state. The
  !! scalar_value will be set into a field with name flds_scalar_name. The scalar_id
  !! identifies which dimension in the scalar field is given by the scalar_value. The
  !! number of scalars is used to ensure that the scalar_id is within the bounds of
  !! the scalar field
  !!
  !! @param[inout]   State            an ESMF_State
  !! @param[in]      scalar_value     the value of the scalar
  !! @param[in]      scalar_id        the identity of the scalar
  !! @param[in]      flds_scalar_name the name of the scalar
  !! @param[in]      flds_scalar_num  the number of scalars
  !! @param[inout]   rc               a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine state_setscalar(scalar_value, scalar_id, State, flds_scalar_name, flds_scalar_num,  rc)

    ! ----------------------------------------------
    ! Set scalar data from State for a particular name
    ! ----------------------------------------------

    ! input/output arguments
    real(r8),         intent(in)     :: scalar_value
    integer,          intent(in)     :: scalar_id
    type(ESMF_State), intent(inout)  :: State
    character(len=*), intent(in)     :: flds_scalar_name
    integer,          intent(in)     :: flds_scalar_num
    integer,          intent(inout)  :: rc

    ! local variables
    integer           :: mytask
    type(ESMF_Field)  :: lfield
    type(ESMF_VM)     :: vm
    real(r8), pointer :: farrayptr(:,:)
    character(len=*), parameter :: subname = ' (wav_shr_mod:state_setscalar) '
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, localPet=mytask, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_StateGet(State, itemName=trim(flds_scalar_name), field=lfield, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    if (mytask == 0) then
      call ESMF_FieldGet(lfield, farrayPtr = farrayptr, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      if (scalar_id < 0 .or. scalar_id > flds_scalar_num) then
        call ESMF_LogWrite(trim(subname)//": ERROR in scalar_id", ESMF_LOGMSG_INFO)
        rc = ESMF_FAILURE
        return
      endif
      farrayptr(scalar_id,1) = scalar_value
    endif

  end subroutine state_setscalar

  !===============================================================================
  !> Reset all fields in a state to a value
  !!
  !! @param[inout] State            an ESMF_State
  !! @param[in]    reset_value      the reset value
  !! @param[out]   rc               a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine state_reset(State, reset_value, rc)

    ! ----------------------------------------------
    ! Set all fields to value in State to value
    ! ----------------------------------------------

    ! intput/output variables
    type(ESMF_State) , intent(inout) :: State
    real(R8)         , intent(in)    :: reset_value
    integer          , intent(out)   :: rc

    ! local variables
    integer                             :: i,j,n
    type(ESMF_Field)                    :: lfield
    integer                             :: fieldCount
    integer                             :: lrank
    character(ESMF_MAXSTR), allocatable :: lfieldnamelist(:)
    real(R8), pointer                   :: fldptr1(:)
    real(R8), pointer                   :: fldptr2(:,:)
    character(len=*), parameter         :: subname = ' (wav_shr_mod:state_reset) '
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(lfieldnamelist(fieldCount))
    call ESMF_StateGet(State, itemNameList=lfieldnamelist, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return


    do n = 1, fieldCount
      call ESMF_StateGet(State, itemName=trim(lfieldnamelist(n)), field=lfield, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call field_getfldptr(lfield, fldptr1=fldptr1, fldptr2=fldptr2, rank=lrank, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      if (lrank == 0) then
        ! no local data
      elseif (lrank == 1) then
        fldptr1 = reset_value
      elseif (lrank == 2) then
        fldptr2 = reset_value
      else
        call ESMF_LogWrite(trim(subname)//": ERROR in rank "//trim(lfieldnamelist(n)), ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
      endif
    enddo

    deallocate(lfieldnamelist)

  end subroutine state_reset

  !===============================================================================
  !> Obtain a 1-D pointer to a field in a state
  !!
  !! @param[in]    State            an ESMF_State
  !! @param[in]    fldname          the name of an ESMF field
  !! @param[inout] fldptr           a 1-d pointer to an ESMF field
  !! @param[out]   rc               a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine state_getfldptr_1d(State, fldname, fldptr, rc)
    ! ----------------------------------------------
    ! Get pointer to a state field
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_State)              , intent(in)     :: State
    character(len=*)              , intent(in)     :: fldname
    real(R8)            , pointer , intent(inout)  :: fldptr(:)
    integer, optional             , intent(out)    :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    type(ESMF_FieldStatus_Flag) :: status
    character(len=*),parameter :: subname='(wav_import_export:state_getfldptr_1d)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_StateGet(State, itemName=trim(fldname), field=lfield, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(lfield, status=status, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_LogWrite(trim(subname)//": ERROR data not allocated ", ESMF_LOGMSG_ERROR)
      rc = ESMF_FAILURE
      return
    else
      call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine state_getfldptr_1d

  !===============================================================================
  !> Obtain a 2-D pointer to a field in a state
  !!
  !! @param[in]    State            an ESMF_State
  !! @param[in]    fldname          the name of an ESMF field
  !! @param[inout] fldptr           a 2-d pointer to an ESMF field
  !! @param[out]   rc               a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine state_getfldptr_2d(State, fldname, fldptr, rc)
    ! ----------------------------------------------
    ! Get pointer to a state field
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_State)    ,            intent(in)     :: State
    character(len=*)    ,            intent(in)     :: fldname
    real(R8)            , pointer  , intent(inout)  :: fldptr(:,:)
    integer             , optional , intent(out)    :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    type(ESMF_FieldStatus_Flag) :: status
    character(len=*),parameter :: subname='(wav_import_export:state_getfldptr_2d)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_StateGet(State, itemName=trim(fldname), field=lfield, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(lfield, status=status, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_LogWrite(trim(subname)//": ERROR data not allocated ", ESMF_LOGMSG_INFO, rc=rc)
      rc = ESMF_FAILURE
      return
    else
      call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if
  end subroutine state_getfldptr_2d

  !===============================================================================
  !> Return true if a field is in a state
  !!
  !! @param[in] State               an ESMF_State
  !! @param[in] fldname             the name of an ESMF field
  !! @return    state_fldchk        logical indicating a field is present in a state
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  logical function state_fldchk(State, fldname)
    ! ----------------------------------------------
    ! Determine if field is in state
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_State) , intent(in)  :: State
    character(len=*) , intent(in)  :: fldname

    ! local variables
    type(ESMF_StateItem_Flag) :: itemType
    ! ----------------------------------------------

    call ESMF_StateGet(State, trim(fldname), itemType)
    State_FldChk = (itemType /= ESMF_STATEITEM_NOTFOUND)

  end function state_fldchk

  !===============================================================================
  !> Print minimum, maximum, sum and size for a field in a state
  !!
  !! @param[in] State               an ESMF_State
  !! @param[in] string              a string for denoting the location of the call
  !! @param[out] rc                 a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine state_diagnose(State, string, rc)

    ! ----------------------------------------------
    ! Diagnose status of State
    ! ----------------------------------------------

    type(ESMF_State), intent(in)  :: state
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    ! local variables
    integer                         :: i,j,n
    type(ESMf_Field)                :: lfield
    integer                         :: fieldCount, lrank
    character(ESMF_MAXSTR) ,pointer :: lfieldnamelist(:)
    real(r8), pointer               :: dataPtr1d(:)
    real(r8), pointer               :: dataPtr2d(:,:)
    character(len=ESMF_MAXSTR)      :: msgString
    character(len=*), parameter     :: subname = ' (wav_shr_mod:state_diagnose) '
    ! ----------------------------------------------


    call ESMF_StateGet(state, itemCount=fieldCount, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    allocate(lfieldnamelist(fieldCount))

    call ESMF_StateGet(state, itemNameList=lfieldnamelist, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    do n = 1, fieldCount

      call ESMF_StateGet(state, itemName=lfieldnamelist(n), field=lfield, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return

      call field_getfldptr(lfield, fldptr1=dataPtr1d, fldptr2=dataPtr2d, rank=lrank, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return

      if (lrank == 0) then
        ! no local data
      elseif (lrank == 1) then
        if (size(dataPtr1d) > 0) then
          write(msgString,'(A,3g14.7,i8)') trim(string)//': '//trim(lfieldnamelist(n))//'  ', &
               minval(dataPtr1d), maxval(dataPtr1d), sum(dataPtr1d), size(dataPtr1d)
        else
          write(msgString,'(A,a)') trim(string)//': '//trim(lfieldnamelist(n))," no data"
        endif
      elseif (lrank == 2) then
        if (size(dataPtr2d) > 0) then
          write(msgString,'(A,3g14.7,i8)') trim(string)//': '//trim(lfieldnamelist(n))//'  ', &
               minval(dataPtr2d), maxval(dataPtr2d), sum(dataPtr2d), size(dataPtr2d)
        else
          write(msgString,'(A,a)') trim(string)//': '//trim(lfieldnamelist(n))," no data"
        endif
      else
        call ESMF_LogWrite(trim(subname)//": ERROR rank not supported ", ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
      endif
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    deallocate(lfieldnamelist)

  end subroutine state_diagnose

  !===============================================================================
  !> Obtain a 1 or 2-D pointer to a field
  !!
  !! @param[in]    field            an ESMF_Field
  !! @param[inout] fldptr1          a 1-d pointer to an ESMF field
  !! @param[inout] fldptr2          a 2-d pointer to an ESMF field
  !! @param[out]   rank             the field rank
  !! @param[in]    abort            an optional flag to override the default abort value
  !! @param[out]   rc               a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine field_getfldptr(field, fldptr1, fldptr2, rank, abort, rc)

    ! ----------------------------------------------
    ! for a field, determine rank and return fldptr1 or fldptr2
    ! abort is true by default and will abort if fldptr is not yet allocated in field
    ! rank returns 0, 1, or 2.  0 means fldptr not allocated and abort=false
    ! ----------------------------------------------

    ! input/output variables
    type(ESMF_Field)  , intent(in)              :: field
    real(r8), pointer , intent(inout), optional :: fldptr1(:)
    real(r8), pointer , intent(inout), optional :: fldptr2(:,:)
    integer           , intent(out)  , optional :: rank
    logical           , intent(in)   , optional :: abort
    integer           , intent(out)  , optional :: rc

    ! local variables
    type(ESMF_GeomType_Flag)    :: geomtype
    type(ESMF_FieldStatus_Flag) :: status
    type(ESMF_Mesh)             :: lmesh
    integer                     :: lrank, nnodes, nelements
    logical                     :: labort
    character(len=*), parameter :: subname = ' (wav_shr_mod:field_getfldptr) '
    ! ----------------------------------------------

    if (.not.present(rc)) then
      call ESMF_LogWrite(trim(subname)//": ERROR rc not present ", &
           ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
      rc = ESMF_FAILURE
      return
    endif

    rc = ESMF_SUCCESS

    labort = .true.
    if (present(abort)) then
      labort = abort
    endif
    lrank = -99

    call ESMF_FieldGet(field, status=status, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    if (status /= ESMF_FIELDSTATUS_COMPLETE) then
      lrank = 0
      if (labort) then
        call ESMF_LogWrite(trim(subname)//": ERROR data not allocated ", ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
      else
        call ESMF_LogWrite(trim(subname)//": WARNING data not allocated ", ESMF_LOGMSG_INFO)
      endif
    else

      call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return

      if (geomtype == ESMF_GEOMTYPE_GRID) then
        call ESMF_FieldGet(field, rank=lrank, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
      elseif (geomtype == ESMF_GEOMTYPE_MESH) then
        call ESMF_FieldGet(field, rank=lrank, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
        call ESMF_FieldGet(field, mesh=lmesh, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
        call ESMF_MeshGet(lmesh, numOwnedNodes=nnodes, numOwnedElements=nelements, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
        if (nnodes == 0 .and. nelements == 0) lrank = 0
      else
        call ESMF_LogWrite(trim(subname)//": ERROR geomtype not supported ", ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
      endif ! geomtype

      if (lrank == 0) then
        call ESMF_LogWrite(trim(subname)//": no local nodes or elements ", &
             ESMF_LOGMSG_INFO)
      elseif (lrank == 1) then
        if (.not.present(fldptr1)) then
          call ESMF_LogWrite(trim(subname)//": ERROR missing rank=1 array ", &
               ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
          rc = ESMF_FAILURE
          return
        endif
        call ESMF_FieldGet(field, farrayPtr=fldptr1, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
      elseif (lrank == 2) then
        if (.not.present(fldptr2)) then
          call ESMF_LogWrite(trim(subname)//": ERROR missing rank=2 array ", &
               ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
          rc = ESMF_FAILURE
          return
        endif
        call ESMF_FieldGet(field, farrayPtr=fldptr2, rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
      else
        call ESMF_LogWrite(trim(subname)//": ERROR in rank ", &
             ESMF_LOGMSG_ERROR, line=__LINE__, file=u_FILE_u)
        rc = ESMF_FAILURE
        return
      endif

    endif  ! status

    if (present(rank)) then
      rank = lrank
    endif

  end subroutine field_getfldptr

  !===============================================================================
  !> Set up an alarm in a clock
  !!
  !> @details Create an ESMF_Alarm according to the desired frequency, where the
  !! frequency is relative to a time frequency of seconds, days, hours etc.
  !!
  !! @param[inout]  clock           an ESMF_Clock
  !! @param[inout]  alarm           an ESMF_Alarm
  !! @param[in]     option          the alarm option (day,hour etc)
  !! @param[in]     opt_n           the alarm frequency
  !! @param[in]     opt_ymd         the YMD, required for alarm_option when option is
  !!                                date
  !! @param[in]     opt_tod         the time-of-day in seconds
  !! @param[in]     Reftime         initial guess of next alarm time
  !! @param[in]     alarmname       the alarm name
  !! @param[inout]  rc              a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine alarmInit( clock, alarm, option, &
       opt_n, opt_ymd, opt_tod, RefTime, alarmname, rc)

    ! Setup an alarm in a clock
    ! Notes: The ringtime sent to AlarmCreate MUST be the next alarm
    ! time.  If you send an arbitrary but proper ringtime from the
    ! past and the ring interval, the alarm will always go off on the
    ! next clock advance and this will cause serious problems.  Even
    ! if it makes sense to initialize an alarm with some reference
    ! time and the alarm interval, that reference time has to be
    ! advance forward to be >= the current time.  In the logic below
    ! we set an appropriate "NextAlarm" and then we make sure to
    ! advance it properly based on the ring interval.

    ! input/output variables
    type(ESMF_Clock)            , intent(inout) :: clock     ! clock
    type(ESMF_Alarm)            , intent(inout) :: alarm     ! alarm
    character(len=*)            , intent(in)    :: option    ! alarm option
    integer          , optional , intent(in)    :: opt_n     ! alarm freq
    integer          , optional , intent(in)    :: opt_ymd   ! alarm ymd
    integer          , optional , intent(in)    :: opt_tod   ! alarm tod (sec)
    type(ESMF_Time)  , optional , intent(in)    :: RefTime   ! ref time
    character(len=*) , optional , intent(in)    :: alarmname ! alarm name
    integer                     , intent(inout) :: rc        ! Return code

    ! local variables
    type(ESMF_Calendar)     :: cal                ! calendar
    integer                 :: lymd             ! local ymd
    integer                 :: ltod             ! local tod
    integer                 :: cyy,cmm,cdd,csec ! time info
    character(len=64)       :: lalarmname       ! local alarm name
    logical                 :: update_nextalarm ! update next alarm
    type(ESMF_Time)         :: CurrTime         ! Current Time
    type(ESMF_Time)         :: NextAlarm        ! Next restart alarm time
    type(ESMF_TimeInterval) :: AlarmInterval    ! Alarm interval
    integer                 :: sec

    character(len=*), parameter :: subname = ' (wav_shr_mod:set_alarmInit) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    lalarmname = 'alarm_unknown'
    if (present(alarmname)) lalarmname = trim(alarmname)
    ltod = 0
    if (present(opt_tod)) ltod = opt_tod
    lymd = -1
    if (present(opt_ymd)) lymd = opt_ymd

    call ESMF_ClockGet(clock, CurrTime=CurrTime, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeGet(CurrTime, yy=cyy, mm=cmm, dd=cdd, s=csec, rc=rc )
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! initial guess of next alarm, this will be updated below
    if (present(RefTime)) then
      NextAlarm = RefTime
    else
      NextAlarm = CurrTime
    endif

    ! Determine calendar
    call ESMF_ClockGet(clock, calendar=cal)

    ! Determine inputs for call to create alarm
    selectcase (trim(option))

    case (optNONE)
      call ESMF_TimeIntervalSet(AlarmInterval, yy=9999, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call ESMF_TimeSet( NextAlarm, yy=9999, mm=12, dd=1, s=0, calendar=cal, rc=rc )
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      update_nextalarm  = .false.

    case (optNever)
      call ESMF_TimeIntervalSet(AlarmInterval, yy=9999, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call ESMF_TimeSet( NextAlarm, yy=9999, mm=12, dd=1, s=0, calendar=cal, rc=rc )
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      update_nextalarm  = .false.

    case (optDate)
      if (.not. present(opt_ymd)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_ymd', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (lymd < 0 .or. ltod < 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//'opt_ymd, opt_tod invalid', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, yy=9999, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call timeInit(NextAlarm, lymd, cal, ltod, rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      update_nextalarm  = .false.

    case (optIfdays0)
      if (.not. present(opt_ymd)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_ymd', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0)  then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, mm=1, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call ESMF_TimeSet( NextAlarm, yy=cyy, mm=cmm, dd=opt_n, s=0, calendar=cal, rc=rc )
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      update_nextalarm  = .true.

    case (optNSteps)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_ClockGet(clock, TimeStep=AlarmInterval, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optNStep)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0)  then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_ClockGet(clock, TimeStep=AlarmInterval, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optNSeconds)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, s=1, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optNSecond)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, s=1, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optNMinutes)
      call ESMF_TimeIntervalSet(AlarmInterval, s=60, rc=rc)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optNMinute)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, s=60, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optNHours)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, s=3600, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optNHour)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, s=3600, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optNDays)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, d=1, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optNDay)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, d=1, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optNMonths)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, mm=1, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optNMonth)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, mm=1, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optMonthly)
      call ESMF_TimeIntervalSet(AlarmInterval, mm=1, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call ESMF_TimeSet( NextAlarm, yy=cyy, mm=cmm, dd=1, s=0, calendar=cal, rc=rc )
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      update_nextalarm  = .true.

    case (optNYears)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, yy=1, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optNYear)
      if (.not.present(opt_n)) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' requires opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      if (opt_n <= 0) then
        call ESMF_LogWrite(trim(subname)//trim(option)//' invalid opt_n', &
             ESMF_LOGMSG_INFO, rc=rc)
        rc = ESMF_FAILURE
      end if
      call ESMF_TimeIntervalSet(AlarmInterval, yy=1, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      AlarmInterval = AlarmInterval * opt_n
      update_nextalarm  = .true.

    case (optYearly)
      call ESMF_TimeIntervalSet(AlarmInterval, yy=1, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      call ESMF_TimeSet( NextAlarm, yy=cyy, mm=1, dd=1, s=0, calendar=cal, rc=rc )
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      update_nextalarm  = .true.

    case default
      call ESMF_LogWrite(trim(subname)//'unknown option '//trim(option), &
           ESMF_LOGMSG_INFO, rc=rc)
      rc = ESMF_FAILURE

    end select

    ! --------------------------------------------------------------------------------
    ! --- AlarmInterval and NextAlarm should be set ---
    ! --------------------------------------------------------------------------------

    ! --- advance Next Alarm so it won't ring on first timestep for
    ! --- most options above. go back one alarminterval just to be careful

    if (update_nextalarm) then
      NextAlarm = NextAlarm - AlarmInterval
      do while (NextAlarm <= CurrTime)
        NextAlarm = NextAlarm + AlarmInterval
      enddo
    endif

    alarm = ESMF_AlarmCreate( name=lalarmname, clock=clock, ringTime=NextAlarm, &
         ringInterval=AlarmInterval, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

  end subroutine alarmInit

  !===============================================================================
  !> Create an ESMF_Time object
  !!
  !> @details Create a ESMF_Time corresponding to a input time YYYYMMMDD and
  !! time of day in seconds
  !!
  !! @param[inout] Time             an ESMF_Time object
  !! @param[in]    ymd              year, month, day YYYYMMDD
  !! @param[in]    cal              an ESMF_Calendar
  !! @param[in]    tod              time of day in secons
  !! @param[out]   rc               a return code
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine timeInit( Time, ymd, cal, tod, rc)

    ! Create the ESMF_Time object corresponding to the given input time,
    ! given in YMD (Year Month Day) and TOD (Time-of-day) format.
    ! Set the time by an integer as YYYYMMDD and integer seconds in the day

    ! input/output parameters:
    type(ESMF_Time)     , intent(inout) :: Time ! ESMF time
    integer             , intent(in)    :: ymd  ! year, month, day YYYYMMDD
    type(ESMF_Calendar) , intent(in)    :: cal  ! ESMF calendar
    integer             , intent(in)    :: tod  ! time of day in seconds
    integer             , intent(out)   :: rc

    ! local variables
    integer :: year, mon, day ! year, month, day as integers
    integer :: tdate          ! temporary date
    integer :: date           ! coded-date (yyyymmdd)
    integer, parameter          :: SecPerDay = 86400 ! Seconds per day
    character(len=*), parameter :: subname = ' (wav_shr_mod:timeInit) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    if ( (ymd < 0) .or. (tod < 0) .or. (tod > SecPerDay) )then
      call ESMF_LogWrite(trim(subname)//'ERROR yymmdd is a negative number or time-of-day out of bounds', &
           ESMF_LOGMSG_INFO, rc=rc)
      rc = ESMF_FAILURE
    end if

    tdate = abs(date)
    year = int(tdate/10000)
    if (date < 0) year = -year
    mon = int( mod(tdate,10000)/  100)
    day = mod(tdate,  100)

    call ESMF_TimeSet( Time, yy=year, mm=mon, dd=day, s=tod, calendar=cal, rc=rc )
    if (chkerr(rc,__LINE__,u_FILE_u)) return

  end subroutine timeInit

  !===============================================================================
  !> Convert  year, month, day to integer*4 coded-date
  !!
  !! @param[in]   year              calendar year
  !! @param[in]   month             calendary month
  !! @param[in]   day               calendar day
  !! @param[out]  date              calendar date yyyymmmdd
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine ymd2date_int(year,month,day,date)
    ! Converts  year, month, day to coded-date

    ! input/output variables
    integer,intent(in ) :: year,month,day  ! calendar year,month,day
    integer,intent(out) :: date            ! coded (yyyymmdd) calendar date
    !---------------------------------------

    ! NOTE: this calendar has a year zero (but no day or month zero)
    date = abs(year)*10000 + month*100 + day  ! coded calendar date
    if (year < 0) date = -date
  end subroutine ymd2date_int

  !===============================================================================
  !> Converts  year, month, day to integer*8 coded-date
  !!
  !! @param[in]   year              calendar year
  !! @param[in]   month             calendary month
  !! @param[in]   day               calendar day
  !! @param[out]  date              calendar date yyyymmmdd
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine ymd2date_long(year,month,day,date)
    ! Converts  year, month, day to coded-date

    ! input/output variables
    integer    ,intent(in ) :: year,month,day  ! calendar year,month,day
    integer(I8),intent(out) :: date            ! coded ([yy]yyyymmdd) calendar date
    !---------------------------------------

    ! NOTE: this calendar has a year zero (but no day or month zero)
    date = abs(year)*10000_I8 + month*100 + day  ! coded calendar date
    if (year < 0) date = -date
  end subroutine ymd2date_long

  !===============================================================================
  !> Return a logical true if ESMF_LogFoundError detects an error
  !!
  !! @param[in]  rc                 return code
  !! @param[in]  line               source code line number
  !! @param[in]  file               user provided source file name
  !! @return     chkerr             logical indicating an error was found
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  logical function chkerr(rc, line, file)

    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=*), intent(in) :: file

    integer :: lrc

    chkerr = .false.
    lrc = rc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
      chkerr = .true.
    endif
  end function chkerr

end module wav_shr_mod
